use std::ffi::OsString;
use std::io::ErrorKind::NotFound;
use std::os::unix::fs::symlink;
use std::{fs, io, path};

use tempfile::NamedTempFileOptions;

use super::errors::{PromptErrors, PromptResult as Result};
use super::term::confirm;

fn action_exists(source: &path::Path, _: &path::Path) -> Result<()> {
    println!("Ensuring the existence of {:?}.", source);
    let _ = try!(fs::OpenOptions::new().append(true).create(true).open(source));
    Ok(())
}

fn action_install(source: &path::Path, target: &path::Path) -> Result<()> {
    println!("Installing {:?} to {:?}.", source, target);
    match fs::symlink_metadata(target) {
        Err(ref e) if e.kind() == NotFound => (),
        Err(e) => return Err(e.into()),
        Ok(m) => match m.file_type() {
            ref t if t.is_dir() => return Err(PromptErrors::InstallationError(
                format!("cowardly refusing to delete extant directory {:?}", target))),
            ref t if t.is_file() => {
                let prompt = format!("Delete extant file {:?}", target);
                if confirm(&prompt, true, "? ", true) {
                    try!(fs::remove_file(target));
                } else {
                    return Err(PromptErrors::InstallationError(
                        format!("not deleting {:?}", target)))
                }
            },
            ref t if t.is_symlink() => try!(fs::remove_file(target)),
            _ => return Err(PromptErrors::InstallationError(
                format!("can't figure out what {:?} is", target))),
        }
    }
    try!(symlink(source, target));
    Ok(())
}

fn assemble_files(source: &path::Path, target: &path::Path) -> Result<()> {
    let files = try!(fs::read_dir(source))
        .filter_map(|r| match r {
            Err(e) => Some(Err(e)),
            Ok(e) => {
                let p = e.path();
                if match p.file_name().and_then(|f| f.to_str()) {
                    Some(f) if !f.starts_with("_") => true,
                    _ => false,
                } {Some(Ok(p))} else {None}
            },
        })
        .collect();
    let mut files: Vec<path::PathBuf> = try!(files);
    files.sort_by_key(|p| {
        p.file_name()
            .and_then(|f| f.to_str())
            .and_then(|s| s.split('-').next())
            .and_then(|i| i.parse::<u64>().ok())
    });
    let out = NamedTempFileOptions::new()
        .prefix("_tmp")
        .create_in(target.parent().unwrap_or_else(|| unimplemented!()));
    let mut out = try!(out);
    for file in files {
        let mut file = try!(fs::OpenOptions::new().read(true).open(file));
        try!(io::copy(&mut file, &mut out));
        try!(io::Write::write_all(&mut out, b"\n\n\n"));
    }
    try!(out.persist(target).map_err(|e| e.error));
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
    try!(assemble_files(&source, &assembled_file));
    try!(action_install(&assembled_file, target));
    Ok(())
}

const ACTIONS: [(&'static str, fn(&path::Path, &path::Path) -> Result<()>); 3] = [
    ("exists", action_exists),
    ("install", action_install),
    ("assemble", action_assemble),
];

pub fn install_from_manifest(manifest: &path::Path, target_dir: &path::Path) -> Result<()> {
    let manifest_dir = manifest.parent().unwrap_or_else(|| unimplemented!());
    let file = io::BufReader::new(
        try!(fs::OpenOptions::new().read(true).open(manifest)));
    for line in io::BufRead::lines(file) {
        let line = try!(line);
        let splut: Vec<&str> = line.split_whitespace().collect();
        let (name, source, target) = match splut.len() {
            2 => {
                let source = path::Path::new(splut[1]);
                let mut as_dotfile: OsString = ".".into();
                as_dotfile.push(source.file_name().unwrap_or_else(|| unimplemented!()));
                (splut[0], manifest_dir.join(source), target_dir.join(as_dotfile))
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
            .filter_map(|&(ref action_name, func)| if &name == action_name {Some(func)} else {None})
            .next()
            .unwrap_or_else(|| unimplemented!());
        try!(action(&source, &target));
    }
    Ok(())
}
