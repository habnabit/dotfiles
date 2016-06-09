extern crate clap;
extern crate sha1;

use std::collections::BTreeMap;
use std::io::*;
use std::{env, fs, path, process};

const GIT_INDEX_STATII: &'static str = "MADRC";
const GIT_WORKING_STATII: &'static str = "MD";
const HG_STATII: &'static str = "MAR?";
const STATUS_ORDER: &'static str = "UMADRCmd?";
const FILE_ORDER: &'static str = ".-/l?";
const ITERATION_LIMIT: usize = 1000;

fn tolower(c: char) -> char {
    c.to_lowercase().next().unwrap()
}

trait IncrementalMap<T> {
    fn increment(&mut self, key: T);
}

impl<T: Ord> IncrementalMap<T> for BTreeMap<T, usize> {
    fn increment(&mut self, key: T) {
        *self.entry(key).or_insert(0) += 1;
    }
}

fn limited_foreach<I, F>(iter: I, mut func: F) -> Result<bool>
    where I: IntoIterator,
          F: FnMut(I::Item) -> Result<()>,
{
    for (e, item) in iter.into_iter().enumerate() {
        if e >= ITERATION_LIMIT {
            return Ok(true);
        }
        try!(func(item));
    }
    Ok(false)
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
            (c, _) if GIT_INDEX_STATII.contains(c) => counts.increment(c),
            (_, c) if GIT_WORKING_STATII.contains(c) => counts.increment(tolower(c)),
            _ => (),
        };
        Ok(())
    }));
    Ok((counts, truncated))
}

fn run_git_status(mut base: process::Command) -> Result<(BTreeMap<char, usize>, bool)> {
    let child = base
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

fn run_hg_status(mut base: process::Command) -> Result<(BTreeMap<char, usize>, bool)> {
    let child = base
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

fn format_counts(order: &str, counts: &BTreeMap<char, usize>, truncated: bool, show_total: bool) -> Vec<u8> {
    let mut sorted: Vec<_> = counts
        .into_iter()
        .filter_map(|(&c, &count)| order.find(c).map(|pos| (pos, c, count)))
        .collect();
    sorted.sort();
    let mut ret = Vec::new();
    for &(_, c, count) in &sorted {
        write!(ret, "{}{} ", count, c).unwrap();
    }
    if show_total {
        let total = counts.values().fold(0, |acc, &x| acc + x);
        write!(ret, "≡{} ", total).unwrap();
    }
    if ret.pop().is_some() {
        if truncated {
            ret.extend("…".as_bytes());
        }
        ret.push(b'\n');
    }
    ret
}

fn count_files() -> Result<(BTreeMap<char, usize>, bool)> {
    let mut counts = BTreeMap::new();
    let truncated = try!(limited_foreach(try!(fs::read_dir(".")), |entry| {
        let entry = try!(entry);
        match entry.file_name().as_os_str().to_str() {
            Some(s) if s.starts_with(".") => counts.increment('.'),
            _ => {
                let ty = try!(entry.file_type());
                if ty.is_symlink() {
                    counts.increment('l');
                } else if ty.is_file() {
                    counts.increment('-');
                } else if ty.is_dir() {
                    counts.increment('/');
                } else {
                    counts.increment('?');
                }
            },
        };
        Ok(())
    }));
    Ok((counts, truncated))
}

fn path_dev(path: &path::Path) -> Result<u64> {
    use std::os::unix::fs::MetadataExt;
    path.metadata().map(|m| m.dev())
}

const VC_DIRS: [(&'static str, Vc); 2] = [
    (".git", Vc::Git),
    (".hg", Vc::Hg),
];

fn test_vc_dir(path: &path::Path) -> Result<Option<(Vc, path::PathBuf)>> {
    for &(ref dirname, vc) in &VC_DIRS {
        let child = path.join(dirname);
        use std::io::ErrorKind::*;
        let meta = match child.metadata() {
            Ok(m) => m,
            Err(ref e) if match e.kind() {
                NotFound | PermissionDenied => true,
                _ => false,
            } => continue,
            Err(e) => return Err(e),
        };
        if meta.is_dir() {
            return Ok(Some((vc, child)));
        }
    }
    Ok(None)
}

fn find_vc_root() -> Result<Option<(Vc, path::PathBuf, path::PathBuf)>> {
    let mut cur = try!(env::current_dir());
    let top_dev = try!(path_dev(cur.as_path()));
    loop {
        if let Some((vc, vc_dir)) = try!(test_vc_dir(cur.as_path())) {
            return Ok(Some((vc, vc_dir, cur)));
        }
        cur = match cur.parent() {
            Some(p) if try!(path_dev(p)) == top_dev => p.to_path_buf(),
            _ => break,
        };
    }
    Ok(None)
}

#[derive(Copy, Clone)]
enum Vc {
    Git,
    Hg,
}

impl Vc {
    fn as_name(&self) -> &'static str {
        match self {
            &Vc::Git => "git",
            &Vc::Hg => "hg",
        }
    }
}

struct VcLoc {
    vc: Vc,
    vc_dir: path::PathBuf,
    work_dir: path::PathBuf,
}

fn from_utf8(v: Vec<u8>) -> Result<String> {
    String::from_utf8(v)
        .map_err(|e| Error::new(ErrorKind::Other, e))
}

impl VcLoc {
    fn from_current_dir() -> Result<Option<VcLoc>> {
        find_vc_root().map(|o| o.map(
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
        };
        cmd.stdin(process::Stdio::null());
        cmd.stderr(process::Stdio::inherit());
        cmd
    }

    fn get_counts(&self) -> Result<(BTreeMap<char, usize>, bool)> {
        match &self.vc {
            &Vc::Git => run_git_status(self.command_setup()),
            &Vc::Hg => run_hg_status(self.command_setup()),
        }
    }

    fn get_git_branch(&self) -> Result<String> {
        let output = self.command_setup()
            .arg("symbolic-ref").arg("HEAD")
            .stdout(process::Stdio::piped())
            .stderr(process::Stdio::null())
            .output();
        let output = try!(output);
        if output.status.success() {
            let git_ref = try!(from_utf8(output.stdout));
            return Ok(
                git_ref.as_str()
                    .trim_left_matches("refs/heads/")
                    .trim()
                    .to_string());
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
        }
    }
}

fn vc_status() -> Result<()> {
    let vc_loc = match try!(VcLoc::from_current_dir()) {
        Some(v) => v,
        None => return Ok(()),
    };
    let (counts, truncated) = try!(vc_loc.get_counts());
    let mut stdout_ = stdout();
    try!(write!(stdout_, "{} {}", vc_loc.vc.as_name(), try!(vc_loc.get_branch())));
    let counts = format_counts(STATUS_ORDER, &counts, truncated, false);
    if !counts.is_empty() {
        try!(write!(stdout_, ": "));
        try!(stdout_.write_all(&counts[..]));
    } else {
        try!(write!(stdout_, "\n"));
    }
    Ok(())
}

fn file_count() -> Result<()> {
    let (counts, truncated) = try!(count_files());
    try!(stdout().write_all(&format_counts(FILE_ORDER, &counts, truncated, true)[..]));
    Ok(())
}

fn pick_color(choices: Vec<u8>, allow_all: bool) -> u8 {
    for c in choices {
        if allow_all || match c {
            22 ... 51 => true,
            58 ... 230 => true,
            _ => false,
        } {
            return c;
        }
    }
    248
}

fn colorhash(input: &[u8], allow_all: bool) -> Result<()> {
    let mut h = sha1::Sha1::new();
    h.update(input);
    h.update(&[b'\n']);
    try!(write!(stdout(), "{:03}", pick_color(h.digest(), allow_all)));
    Ok(())
}

fn main() {
    let matches = clap::App::new("hab-prompt-utils")
        .setting(clap::AppSettings::SubcommandRequired)
        .version("0.1")
        .author("Aaron Gallagher <_@habnab.it>")
        .about("Prompt utilities")
        .subcommand(
            clap::SubCommand::with_name("vc-status")
                .about("Write a line describing version control status, if possible")
        )
        .subcommand(
            clap::SubCommand::with_name("file-count")
                .about("Count the number of files in the current directory")
        )
        .subcommand(
            clap::SubCommand::with_name("colorhash")
                .about("Hash a value into a 256-color number")
                .arg(
                    clap::Arg::with_name("STRING")
                        .required(true))
                .arg(
                    clap::Arg::with_name("allow-all-colors")
                        .short("a")
                        .long("allow-all-colors")
                        .help("Don't filter out hard-to-read colors"))
        )
        .get_matches();
    if let Some(_) = matches.subcommand_matches("vc-status") {
        vc_status()
    } else if let Some(_) = matches.subcommand_matches("file-count") {
        file_count()
    } else if let Some(m) = matches.subcommand_matches("colorhash") {
        use std::os::unix::ffi::OsStrExt;
        colorhash(m.value_of_os("STRING").unwrap().as_bytes(),
                  m.is_present("allow-all-colors"))
    } else { return }.expect("failure running subcommand")
}
