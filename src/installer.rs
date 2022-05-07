use std::collections::HashSet;
use std::ffi::OsString;
use std::io::ErrorKind::NotFound;
use std::os::unix::fs::symlink;
use std::{fs, io, path};

use super::errors::{PromptErrors, PromptResult as Result};
use super::term::confirm;

fn maybe_mkdir(for_file_at: &path::Path) -> Result<()> {
    if let Some(parent) = for_file_at.parent() {
        std::fs::create_dir_all(parent)?;
    }
    Ok(())
}

fn action_exists(source: &path::Path, _: &path::Path) -> Result<()> {
    println!("Ensuring the existence of {:?}.", source);
    maybe_mkdir(source)?;
    let _ = fs::OpenOptions::new()
        .append(true)
        .create(true)
        .open(source)?;
    Ok(())
}

fn action_install(source: &path::Path, target: &path::Path) -> Result<()> {
    println!("Installing {:?} to {:?}.", source, target);
    match fs::symlink_metadata(target) {
        Err(ref e) if e.kind() == NotFound => (),
        Err(e) => return Err(e.into()),
        Ok(m) => match m.file_type() {
            ref t if t.is_dir() => {
                return Err(PromptErrors::InstallationError(format!(
                    "cowardly refusing to delete extant directory {:?}",
                    target
                )))
            },
            ref t if t.is_file() => {
                let prompt = format!("Delete extant file {:?}", target);
                if confirm(&prompt, true, "? ", true) {
                    fs::remove_file(target)?;
                } else {
                    return Err(PromptErrors::InstallationError(format!(
                        "not deleting {:?}",
                        target
                    )));
                }
            },
            ref t if t.is_symlink() => fs::remove_file(target)?,
            _ => {
                return Err(PromptErrors::InstallationError(format!(
                    "can't figure out what {:?} is",
                    target
                )))
            },
        },
    }
    maybe_mkdir(target)?;
    symlink(source, target)?;
    Ok(())
}

fn find_files_to_assemble(source: &path::Path) -> Result<Vec<path::PathBuf>> {
    let mut files = fs::read_dir(source)?
        .map(|r| r.map(|e| (None, None, e.path())))
        .collect::<std::result::Result<Vec<_>, _>>()?;
    for &mut (ref mut number, ref mut letter, ref mut path) in &mut files {
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
    files.retain(|&(ref n, ref l, _)| n.is_some() && l.is_some());
    files.sort();
    let mut seen = HashSet::with_capacity(files.len());
    let files = files
        .into_iter()
        .filter_map(|(n, _, p)| if seen.insert(n) { Some(p) } else { None })
        .collect();
    Ok(files)
}

fn assemble_files(source: &path::Path, target: &path::Path) -> Result<()> {
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

fn action_assemble(source: &path::Path, target: &path::Path) -> Result<()> {
    let mut source: path::PathBuf = source.into();
    let mut source_name: OsString = source
        .file_name()
        .unwrap_or_else(|| unimplemented!())
        .into();
    source_name.push(".d");
    source.set_file_name(source_name);
    println!("Assembling {:?} to {:?}.", source, target);
    let assembled_file = source.join("_assembled");
    assemble_files(&source, &assembled_file)?;
    action_install(&assembled_file, target)?;
    Ok(())
}

fn action_uninstall(source: &path::Path, target: &path::Path) -> Result<()> {
    println!("Uninstalling {:?}.", target);
    match fs::read_link(target) {
        Err(ref e) if e.kind() == NotFound => (),
        Err(e) => Err(e)?,
        Ok(link_target) => {
            if link_target == source {
                fs::remove_file(target)?;
            } else {
                return Err(PromptErrors::InstallationError(format!(
                    "cowardly refusing to delete weird link to {:?}",
                    link_target
                )));
            }
        },
    }
    Ok(())
}

static ACTIONS: &'static [(&'static str, fn(&path::Path, &path::Path) -> Result<()>)] = &[
    ("exists", action_exists),
    ("install", action_install),
    ("!install", action_uninstall),
    ("assemble", action_assemble),
];

pub fn install_from_manifest(manifest: &path::Path, target_dir: &path::Path) -> Result<()> {
    let manifest_dir = manifest.parent().unwrap_or_else(|| unimplemented!());
    let manifest_dir = manifest_dir.canonicalize()?;
    let target_dir = target_dir.canonicalize()?;
    let file = io::BufReader::new(fs::OpenOptions::new().read(true).open(manifest)?);
    for line in io::BufRead::lines(file) {
        let line = line?;
        let splut: Vec<&str> = line.split_whitespace().collect();
        let (name, source, target) = match splut.len() {
            2 => {
                let source = path::Path::new(splut[1]);
                let mut as_dotfile: OsString = ".".into();
                as_dotfile.push(source.file_name().unwrap_or_else(|| unimplemented!()));
                (
                    splut[0],
                    manifest_dir.join(source),
                    target_dir.join(as_dotfile),
                )
            },
            3 => {
                let source = path::Path::new(splut[1]);
                let dest = path::Path::new(splut[2]);
                (splut[0], manifest_dir.join(source), target_dir.join(dest))
            },
            _ => unimplemented!(),
        };
        let action = ACTIONS
            .iter()
            .filter_map(|&(ref action_name, func)| {
                if &name == action_name {
                    Some(func)
                } else {
                    None
                }
            })
            .next()
            .unwrap_or_else(|| unimplemented!());
        action(&source, &target)?;
    }
    Ok(())
}
