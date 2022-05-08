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

fn action_exists(source: &Path, _: &Path) -> Result<()> {
    tracing::info!(?source, "touching");
    println!("Ensuring the existence of {:?}.", source);
    maybe_mkdir(source)?;
    let _ = fs::OpenOptions::new()
        .append(true)
        .create(true)
        .open(source)
        .with_context(|| format!("whilst touching {:?}", source))?;
    Ok(())
}

fn action_install(source: &Path, target: &Path) -> Result<()> {
    tracing::info!(?source, ?target, "installing");
    match fs::symlink_metadata(target) {
        Err(e) if e.kind() == NotFound => (),
        Err(e) => Err(e).with_context(|| format!("whilst stat {:?}", target))?,
        Ok(m) => match m.file_type() {
            t if t.is_dir() => {
                anyhow::bail!(PromptErrors::InstallationError(format!(
                    "cowardly refusing to delete extant directory {:?}",
                    target
                )));
            },
            t if t.is_file() => {
                let prompt = format!("Delete extant file {:?}", target);
                if confirm(&prompt, true, "? ", true) {
                    fs::remove_file(target)?;
                } else {
                    anyhow::bail!(PromptErrors::InstallationError(format!(
                        "not deleting {:?}",
                        target
                    )));
                }
            },
            t if t.is_symlink() => fs::remove_file(target)?,
            t => {
                anyhow::bail!(PromptErrors::InstallationError(format!(
                    "can't figure out what {:?} is: {:#?}",
                    target, t
                )))
            },
        },
    }
    maybe_mkdir(target)?;
    symlink(source, target)
        .with_context(|| format!("whilst symlinking {:?} -> {:?}", source, target))?;
    Ok(())
}

fn find_files_to_assemble(source: &Path) -> Result<Vec<PathBuf>> {
    let mut files = fs::read_dir(source)?
        .map(|r| r.map(|e| (None, None, e.path())))
        .collect::<std::result::Result<Vec<_>, _>>()?;
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

fn assemble_files(source: &Path, target: &Path) -> Result<()> {
    let files = find_files_to_assemble(source)?;
    maybe_mkdir(target)?;
    let out = tempfile::Builder::new()
        .prefix("_tmp")
        .tempfile_in(target.parent().unwrap_or_else(|| unimplemented!()));
    let mut out = out?;
    for file in files {
        let mut file = fs::OpenOptions::new().read(true).open(file)?;
        io::copy(&mut file, &mut out)?;
        io::Write::write_all(&mut out, b"\n\n\n")?;
    }
    out.persist(target).map_err(|e| e.error)?;
    Ok(())
}

fn action_assemble(source: &Path, target: &Path) -> Result<()> {
    let mut source: PathBuf = source.into();
    let mut source_name: OsString = source
        .file_name()
        .unwrap_or_else(|| unimplemented!())
        .into();
    source_name.push(".d");
    source.set_file_name(source_name);
    tracing::info!(?source, ?target, "assembling");
    let assembled_file = source.join("_assembled");
    assemble_files(&source, &assembled_file)?;
    action_install(&assembled_file, target)?;
    Ok(())
}

fn action_uninstall(source: &Path, target: &Path) -> Result<()> {
    tracing::info!(?target, "uninstalling");
    match fs::read_link(target) {
        Err(e) if e.kind() == NotFound => (),
        Err(e) => Err(e)?,
        Ok(link_target) => {
            if link_target == source {
                fs::remove_file(target)?;
            } else {
                anyhow::bail!(PromptErrors::InstallationError(format!(
                    "cowardly refusing to delete weird link to {:?}",
                    link_target
                )));
            }
        },
    }
    Ok(())
}

static ACTIONS: &'static [(&'static str, fn(&Path, &Path) -> Result<()>)] = &[
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
        tracing::info!(action = ?name, ?source, ?target);
        if !dry_run {
            action(&source, &target).with_context(|| format!("whilst evaluating {:?}", splut))?;
        }
    }
    Ok(())
}
