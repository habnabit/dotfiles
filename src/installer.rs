use std::collections::HashSet;
use std::ffi::OsString;
use std::io::BufRead;
use std::io::ErrorKind::NotFound;
use std::os::unix::fs::symlink;
use std::path::{Path, PathBuf};
use std::{fs, io};

use anyhow::{Context, Result};

use super::errors::PromptErrors;
use super::term::confirm;

fn maybe_mkdir(for_file_at: &Path) -> Result<()> {
    if let Some(parent) = for_file_at.parent() {
        std::fs::create_dir_all(parent)
            .with_context(|| format!("whilst maybe_mkdir create_dir: {:#?}", for_file_at))?;
    }
    Ok(())
}

#[derive(Debug, Clone, Copy)]
struct InstallContext<'r> {
    dry_run: bool,
    source: &'r Path,
    target: &'r Path,
}

fn action_exists(i: InstallContext) -> Result<()> {
    tracing::info!(?i, "touching");
    maybe_mkdir(i.source)?;
    let _ = fs::OpenOptions::new()
        .append(true)
        .create(true)
        .open(i.source)
        .with_context(|| format!("whilst touching {:?}", i.source))?;
    Ok(())
}

fn action_install(i: InstallContext) -> Result<()> {
    tracing::info!(?i, "installing");
    match fs::symlink_metadata(i.target) {
        Err(e) if e.kind() == NotFound => (),
        Err(e) => Err(e).with_context(|| format!("whilst stat {:?}", i.target))?,
        Ok(m) => match m.file_type() {
            t if t.is_dir() => {
                anyhow::bail!(PromptErrors::InstallationError(format!(
                    "cowardly refusing to delete extant directory {:?}",
                    i.target
                )));
            },
            t if t.is_file() => {
                let prompt = format!("Delete extant file {:?}", i.target);
                if confirm(&prompt, true, "? ", true) {
                    fs::remove_file(i.target)?;
                } else {
                    anyhow::bail!(PromptErrors::InstallationError(format!(
                        "not deleting {:?}",
                        i.target
                    )));
                }
            },
            t if t.is_symlink() => fs::remove_file(i.target)?,
            t => {
                anyhow::bail!(PromptErrors::InstallationError(format!(
                    "can't figure out what {:?} is: {:#?}",
                    i.target, t
                )))
            },
        },
    }
    maybe_mkdir(i.target)?;
    symlink(i.source, i.target)
        .with_context(|| format!("whilst symlinking {:?} -> {:?}", i.source, i.target))?;
    Ok(())
}

fn find_files_to_assemble(source: &Path) -> Result<Vec<PathBuf>> {
    let mut files = fs::read_dir(source)
        .with_context(|| format!("whilst read_dir on {:?}", source))?
        .map(|r| r.map(|e| (None, None, e.path())))
        .collect::<std::result::Result<Vec<_>, _>>()
        .with_context(|| format!("whilst parsing read_dir on {:?}", source))?;
    for (number, letter, path) in &mut files {
        match path.file_name().and_then(|f| f.to_str()) {
            Some(f) => {
                if f.starts_with("_") {
                    continue;
                }
                if let Some(mut it) = f.split('-').next().map(|s| s.chars()) {
                    *letter = it.next_back();
                    *number = it.as_str().parse::<u64>().ok();
                }
            },
            _ => (),
        }
    }
    files.retain(|(n, l, _)| n.is_some() && l.is_some());
    files.sort();
    let mut seen = HashSet::with_capacity(files.len());
    let files = files
        .into_iter()
        .filter_map(|(n, _, p)| if seen.insert(n) { Some(p) } else { None })
        .collect();
    Ok(files)
}

fn assemble_files(i: InstallContext) -> Result<()> {
    let files = find_files_to_assemble(i.source).context("find_files_to_assemble")?;
    maybe_mkdir(i.target)?;
    let mut out = {
        let mut b = tempfile::Builder::new();
        b.prefix("_tmp");
        if i.dry_run {
            b.tempfile()
        } else {
            b.tempfile_in(i.target.parent().unwrap_or_else(|| unimplemented!()))
        }
        .with_context(|| format!("whilst making tempdir adjacent {:?}", i.target))?
    };
    for file in files {
        let mut file = fs::OpenOptions::new().read(true).open(file)?;
        io::copy(&mut file, &mut out)?;
        io::Write::write_all(&mut out, b"\n\n\n")?;
    }
    out.persist(i.target).map_err(|e| e.error)?;
    Ok(())
}

fn action_assemble(i: InstallContext) -> Result<()> {
    let mut source: PathBuf = i.source.into();
    let mut source_name: OsString = source
        .file_name()
        .unwrap_or_else(|| unimplemented!())
        .into();
    source_name.push(".d");
    source.set_file_name(source_name);
    tracing::info!(?i, "assembling");
    let assembled_file = source.join("_assembled");
    assemble_files(InstallContext {
        source: source.as_path(),
        target: assembled_file.as_path(),
        ..i
    })
    .with_context(|| format!("whilst assembling {:?}", assembled_file))?;
    action_install(InstallContext {
        source: assembled_file.as_path(),
        ..i
    })
    .with_context(|| format!("whilst installing {:?}", assembled_file))?;
    Ok(())
}

fn action_uninstall(i: InstallContext) -> Result<()> {
    tracing::info!(?i, "uninstalling");
    match fs::read_link(i.target) {
        Err(e) if e.kind() == NotFound => (),
        Err(e) => Err(e)?,
        Ok(link_target) if link_target == i.source => {
            fs::remove_file(i.target)?;
        },
        Ok(link_target) => anyhow::bail!(PromptErrors::InstallationError(format!(
            "cowardly refusing to delete weird link to {:?}",
            link_target
        ))),
    };
    Ok(())
}

static ACTIONS: &'static [(&'static str, fn(InstallContext) -> Result<()>)] = &[
    ("exists", action_exists),
    ("install", action_install),
    ("!install", action_uninstall),
    ("assemble", action_assemble),
];

pub fn install_from_manifest(dry_run: bool, manifest: &Path, target_dir: &Path) -> Result<()> {
    let manifest_dir = manifest.parent().unwrap_or_else(|| unimplemented!());
    let manifest_dir = manifest_dir
        .canonicalize()
        .with_context(|| format!("whilst canonicalizing {:?}", manifest_dir))?;
    let target_dir = target_dir
        .canonicalize()
        .with_context(|| format!("whilst canonicalizing {:?}", target_dir))?;
    let file = io::BufReader::new(
        fs::OpenOptions::new()
            .read(true)
            .open(manifest)
            .with_context(|| format!("whilst opening {:?}", manifest))?,
    );
    for line in file.lines() {
        let line = line?;
        let splut: Vec<&str> = line.split_whitespace().collect();
        let (name, source, target) = match splut.len() {
            2 => {
                let source = Path::new(splut[1]);
                let mut as_dotfile: OsString = ".".into();
                as_dotfile.push(source.file_name().unwrap_or_else(|| unimplemented!()));
                (
                    splut[0],
                    manifest_dir.join(source),
                    target_dir.join(as_dotfile),
                )
            },
            3 => {
                let source = Path::new(splut[1]);
                let dest = Path::new(splut[2]);
                (splut[0], manifest_dir.join(source), target_dir.join(dest))
            },
            _ => unimplemented!(),
        };
        let action = ACTIONS
            .iter()
            .filter_map(|(action_name, func)| {
                if &name == action_name {
                    Some(func)
                } else {
                    None
                }
            })
            .next()
            .unwrap_or_else(|| unimplemented!());
        let i = InstallContext {
            dry_run,
            source: source.as_path(),
            target: target.as_path(),
        };
        tracing::info!(action = ?name, ?source, ?target, ?i);
        action(i).with_context(|| format!("whilst evaluating {:?}", splut))?;
    }
    Ok(())
}
