use std::collections::BTreeMap;
use std::io::ErrorKind::{NotFound, PermissionDenied};
use std::io::{BufRead, BufReader, stdout, stderr};
use std::{env, path, process};

use super::errors::PromptResult as Result;
use super::utils::{IncrementalMap, format_counts, limited_foreach};
use super::plugins;

const GIT_INDEX_STATII: &'static str = "TMADRC";
const GIT_WORKING_STATII: &'static str = "TMD";
const HG_STATII: &'static str = "MAR?";
const STATUS_ORDER: &'static str = "UTMADRCtmd?";

fn tolower(c: char) -> char {
    c.to_lowercase().next().unwrap()
}


fn tally_counts_git(f: &mut BufRead) -> Result<(BTreeMap<char, usize>, bool)> {
    let mut counts = BTreeMap::new();
    let truncated = try!(limited_foreach(f.lines(), |line| {
        let line = try!(line);
        let chars: Vec<char> = line.chars().take(4).collect();
        if chars.len() < 4 || chars[2] != ' ' {
            return Ok(());
        }
        match (chars[0], chars[1]) {
            ('?', '?') => counts.increment('?'),
            ('U', _) |
            (_, 'U') |
            ('D', 'D') |
            ('A', 'A') => counts.increment('U'),
            (i, w) => {
                if GIT_INDEX_STATII.contains(i) {
                    counts.increment(i)
                }
                if GIT_WORKING_STATII.contains(w) {
                    counts.increment(tolower(w))
                }
            },
        };
        Ok(())
    }));
    Ok((counts, truncated))
}

fn tally_counts_hg(f: &mut BufRead) -> Result<(BTreeMap<char, usize>, bool)> {
    let mut counts = BTreeMap::new();
    let truncated = try!(limited_foreach(f.lines(), |line| {
        let line = try!(line);
        let chars: Vec<char> = line.chars().take(3).collect();
        if chars.len() < 3 || chars[1] != ' ' {
            return Ok(());
        }
        match chars[0] {
            c if HG_STATII.contains(c) => counts.increment(tolower(c)),
            _ => (),
        };
        Ok(())
    }));
    Ok((counts, truncated))
}

fn path_dev(path: &path::Path) -> Result<u64> {
    use std::os::unix::fs::MetadataExt;
    Ok(try!(path.metadata().map(|m| m.dev())))
}

const VC_DIRS: [(&'static str, Vc); 2] = [
    (".git", Vc::Git),
    (".hg", Vc::Hg),
];

fn test_vc_dir(path: &path::Path, plugins: Option<&mut plugins::PluginLoader>) -> Result<Option<(Vc, path::PathBuf)>> {
    for &(ref dirname, ref vc) in &VC_DIRS {
        let child = path.join(dirname);
        match child.metadata() {
            Ok(_) => return Ok(Some((vc.clone(), child))),
            Err(ref e) if match e.kind() {
                NotFound | PermissionDenied => true,
                _ => false,
            } => continue,
            Err(e) => return Err(e.into()),
        }
    }
    if let Some(r) = try!(plugins.map(|p| p.test_vc_dir(path)).unwrap_or(Ok(None))) {
        Ok(Some((Vc::Plugin(r), path.to_path_buf())))
    } else {
        Ok(None)
    }
}

fn find_vc_root(mut plugins: Option<&mut plugins::PluginLoader>) -> Result<Option<(Vc, path::PathBuf, path::PathBuf)>>
{
    let mut cur = try!(env::current_dir());
    let top_dev = try!(path_dev(cur.as_path()));
    loop {
        if let Some((vc, vc_dir)) = try!(test_vc_dir(cur.as_path(), plugins.as_mut().map(|p| &mut **p))) {
            return Ok(Some((vc, vc_dir, cur)));
        }
        cur = match cur.parent() {
            Some(p) if try!(path_dev(p)) == top_dev => p.to_path_buf(),
            _ => break,
        };
    }
    Ok(None)
}

#[derive(Clone)]
enum Vc {
    Git,
    Hg,
    Plugin(plugins::VcStatusResponse),
}

impl Vc {
    fn as_name(&self) -> &str {
        match self {
            &Vc::Git => "git",
            &Vc::Hg => "hg",
            &Vc::Plugin(ref s) => s.vc_name.as_str(),
        }
    }
}

struct VcLoc {
    vc: Vc,
    vc_dir: path::PathBuf,
    work_dir: path::PathBuf,
}

fn from_utf8(v: Vec<u8>) -> Result<String> {
    Ok(try!(String::from_utf8(v)))
}

impl VcLoc {
    fn from_current_dir(plugins: Option<&mut plugins::PluginLoader>) -> Result<Option<VcLoc>> {
        find_vc_root(plugins).map(|o| o.map(
            |(vc, vc_dir, work_dir)| VcLoc {vc: vc, vc_dir: vc_dir, work_dir: work_dir}))
    }

    fn command_setup(&self) -> process::Command {
        let mut cmd = match &self.vc {
            &Vc::Git => {
                let mut cmd = process::Command::new("git");
                cmd.env("GIT_DIR", self.vc_dir.as_path());
                cmd.env("GIT_WORK_TREE", self.work_dir.as_path());
                cmd
            },
            &Vc::Hg => {
                let mut cmd = process::Command::new("hg");
                cmd.arg("-R").arg(self.work_dir.as_path());
                cmd
            },
            &Vc::Plugin(_) => unreachable!(),
        };
        cmd.stdin(process::Stdio::null());
        cmd.stderr(process::Stdio::inherit());
        cmd
    }

    fn run_git_status(&self) -> Result<(BTreeMap<char, usize>, bool)> {
        let child = self.command_setup()
            .arg("status").arg("--porcelain")
            .stdout(process::Stdio::piped())
            .spawn();
        let mut child = try!(child);
        let ret = {
            let stdout = child.stdout.take().unwrap();
            try!(tally_counts_git(&mut BufReader::new(stdout)))
        };
        try!(child.wait());
        Ok(ret)
    }

    fn run_hg_status(&self) -> Result<(BTreeMap<char, usize>, bool)> {
        let child = self.command_setup()
            .arg("status")
            .stdout(process::Stdio::piped())
            .spawn();
        let mut child = try!(child);
        let ret = {
            let stdout = child.stdout.take().unwrap();
            try!(tally_counts_hg(&mut BufReader::new(stdout)))
        };
        try!(child.wait());
        Ok(ret)
    }

    fn get_counts(&self) -> Result<(BTreeMap<char, usize>, bool)> {
        match &self.vc {
            &Vc::Git => self.run_git_status(),
            &Vc::Hg => self.run_hg_status(),
            &Vc::Plugin(ref s) => Ok((s.file_counts.clone(), s.file_counts_truncated)),
        }
    }

    fn get_git_head_branch(&self) -> Result<Option<String>> {
        match &self.vc {
            &Vc::Git => (),
            _ => return Ok(None),
        }
        let output = self.command_setup()
            .arg("symbolic-ref").arg("HEAD")
            .stdout(process::Stdio::piped())
            .stderr(process::Stdio::null())
            .output();
        let output = try!(output);
        if output.status.success() {
            let git_ref = try!(from_utf8(output.stdout));
            Ok(Some(
                git_ref.as_str()
                    .trim_left_matches("refs/heads/")
                    .trim()
                    .to_string()))
        } else {
            Ok(None)
        }
    }

    fn get_git_branch(&self) -> Result<String> {
        if let Some(head) = try!(self.get_git_head_branch()) {
            return Ok(head);
        }
        let output = self.command_setup()
            .arg("show").arg("--pretty=%h").arg("-s")
            .stdout(process::Stdio::piped())
            .stderr(process::Stdio::null())
            .output();
        let output = try!(output);
        if output.status.success() {
            let sha = try!(from_utf8(output.stdout));
            return Ok(format!("detached {}", sha.as_str().trim()));
        }
        Ok("unknown".to_string())
    }

    fn get_hg_branch(&self) -> Result<String> {
        let output = self.command_setup()
            .arg("id").arg("-bn")
            .stdout(process::Stdio::piped())
            .stderr(process::Stdio::null())
            .output();
        let output = try!(output);
        if output.status.success() {
            let hg_out = try!(from_utf8(output.stdout));
            let words: Vec<&str> = hg_out.split_whitespace().collect();
            return Ok(format!("{}.{}", words[1], words[0]));
        }
        Ok("unknown".to_string())
    }

    fn get_branch(&self) -> Result<String> {
        match &self.vc {
            &Vc::Git => self.get_git_branch(),
            &Vc::Hg => self.get_hg_branch(),
            &Vc::Plugin(ref s) => Ok(s.branch.clone()),
        }
    }
}

pub fn vc_status(plugins: Option<&mut plugins::PluginLoader>) -> Result<String> {
    use std::fmt::Write;
    let vc_loc = match try!(VcLoc::from_current_dir(plugins)) {
        Some(v) => v,
        None => return Ok("".to_string()),
    };
    let (counts, truncated) = try!(vc_loc.get_counts());
    let mut ret = String::new();
    write!(ret, "{} {}", vc_loc.vc.as_name(), try!(vc_loc.get_branch())).unwrap();
    let counts = format_counts(STATUS_ORDER, &counts, truncated, false);
    if !counts.is_empty() {
        write!(ret, ": {}", counts).unwrap();
    }
    Ok(ret)
}

pub fn git_head_branch() -> Result<String> {
    use std::io::Write;
    match try!(try!(VcLoc::from_current_dir(None)).map_or(Ok(None), |v| v.get_git_head_branch())) {
        Some(head) => Ok(head),
        None => {
            try!(write!(stderr(), "no branch found for git HEAD\n"));
            process::exit(111)
        },
    }
}
