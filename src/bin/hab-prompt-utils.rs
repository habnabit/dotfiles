#![cfg_attr(test, feature(plugin))]
#![cfg_attr(test, plugin(fnconcat))]

#[macro_use] extern crate clap;
extern crate sha1;

use std::collections::BTreeMap;
use std::io::{BufRead, BufReader, Error, ErrorKind, Result, stdout};
use std::{env, fmt, fs, path, process, time};

const GIT_INDEX_STATII: &'static str = "TMADRC";
const GIT_WORKING_STATII: &'static str = "TMD";
const HG_STATII: &'static str = "MAR?";
const STATUS_ORDER: &'static str = "UTMADRCtmd?";
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

fn format_counts(order: &str, counts: &BTreeMap<char, usize>, truncated: bool, show_total: bool) -> String {
    use std::fmt::Write;
    let mut sorted: Vec<_> = counts
        .into_iter()
        .filter_map(|(&c, &count)| order.find(c).map(|pos| (pos, c, count)))
        .collect();
    sorted.sort();
    let mut ret = String::new();
    for &(_, c, count) in &sorted {
        write!(ret, "{}{} ", count, c).unwrap();
    }
    if show_total {
        let total = counts.values().fold(0, |acc, &x| acc + x);
        write!(ret, "≡{} ", total).unwrap();
    }
    if ret.pop().is_some() && truncated {
        ret.push('…');
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

fn vc_status() -> Result<String> {
    use std::fmt::Write;
    let vc_loc = match try!(VcLoc::from_current_dir()) {
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

fn file_count() -> Result<String> {
    let (counts, truncated) = try!(count_files());
    Ok(format_counts(FILE_ORDER, &counts, truncated, true))
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

fn colorhash(input: &[u8], allow_all: bool) -> Result<String> {
    let mut h = sha1::Sha1::new();
    h.update(input);
    h.update(&[b'\n']);
    Ok(format!("{:03}", pick_color(h.digest(), allow_all)))
 }

fn actually_emit(s: String, no_newline: bool) -> Result<()> {
    use std::io::Write;
    let stdout_ = stdout();
    let mut stdout_locked = stdout_.lock();
    try!(stdout_locked.write_all(&s.as_bytes()[..]));
    if !no_newline {
        try!(stdout_locked.write_all(b"\n"));
    }
    Ok(())
}

fn main() {
    let matches = clap_app!
        (hab_utils =>
         (version: "0.1")
         (author: "Aaron Gallagher <_@habnab.it>")
         (about: "General utilities")
         (@setting SubcommandRequired)
         (@subcommand emit =>
          (about: "Emit a string")
          (@arg no_newline: -n "Don't emit a trailing newline")
          (@setting SubcommandRequired)
          (@subcommand vc_status =>
           (aliases: &["vc-status"])
           (about: "Write a line describing version control status, if possible")
          )
          (@subcommand file_count =>
           (aliases: &["file-count"])
           (about: "Count the number of files in the current directory")
          )
          (@subcommand color_hash =>
           (aliases: &["color-hash"])
           (about: "Hash a value into a 256-color number")
           (@arg STRING: +required)
           (@arg allow_all_colors: -a --allow-all-colors "Don't filter out hard-to-read colors")
          )
         )
        ).get_matches();
    if let Some(m) = matches.subcommand_matches("emit") {
        if let Some(_) = m.subcommand_matches("vc_status") {
            vc_status()
        } else if let Some(_) = m.subcommand_matches("file_count") {
            file_count()
        } else if let Some(m) = m.subcommand_matches("color_hash") {
            use std::os::unix::ffi::OsStrExt;
            colorhash(m.value_of_os("STRING").unwrap().as_bytes(),
                      m.is_present("allow_all_colors"))
        } else { return }.and_then(|s| actually_emit(s, m.is_present("no_newline")))
    } else { return }.expect("failure running subcommand")
}

fn float_precision(v: f64, sig_figs: u32) -> usize {
    let v = v.abs();
    if v == 0f64 {
        return 0;
    }
    let prec = (v.log10().floor() - (sig_figs as f64)) as isize + 1;
    if prec >= 0 {
        0
    } else {
        -prec as usize
    }
}

struct PrettyDuration(time::Duration, u32);

const TIME_UNITS: [(&'static str, u64); 3] = [
    ("d", 60 * 60 * 24),
    ("h", 60 * 60),
    ("m", 60),
];

impl fmt::Display for PrettyDuration {
    fn fmt(&self, f: &mut fmt::Formatter) -> std::result::Result<(), fmt::Error> {
        let mut written_first = false;
        let mut secs = self.0.as_secs();
        for &(ref unit, unit_secs) in &TIME_UNITS {
            let unit_count = secs / unit_secs;
            if unit_count == 0 {
                continue;
            }
            secs %= unit_secs;
            try!(write!(f, "{}{}{}", {if written_first {" "} else {""}}, unit_count, unit));
            written_first = true;
        }
        if written_first {
            if secs > 0 {
                try!(write!(f, " {}s", secs));
            }
        } else {
            let fsecs = secs as f64 + (self.0.subsec_nanos() as f64 / 1_000_000.);
            try!(write!(f, "{:.*}s", float_precision(fsecs, self.1), fsecs));
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use super::{PrettyDuration, float_precision};

    parametrize_test!{test_float_precision_formatting, [
        (v: f64, s: u32, r: &'static str),
        (100.,      2, "100"),
        ( 10.,      2, "10"),
        (  1.,      2, "1.0"),
        (  0.,      2, "0"),
        (  0.1,     2, "0.10"),
        (123.456,   2, "123"),
        ( 23.456,   2, "23"),
        (  3.456,   2, "3.5"),
        (  0.456,   2, "0.46"),
        (  0.056,   2, "0.056"),
        (  0.00078, 2, "0.00078"),

        (100.,      3, "100"),
        ( 10.,      3, "10.0"),
        (  1.,      3, "1.00"),
        (  0.,      3, "0"),
        (  0.1,     3, "0.100"),
        (123.456,   3, "123"),
        ( 23.456,   3, "23.5"),
        (  3.456,   3, "3.46"),
        (  0.456,   3, "0.456"),
        (  0.056,   3, "0.0560"),
        (  0.00078, 3, "0.000780"),
    ], {
        assert_eq!(format!("{:.*}", float_precision(v, s), v), r);
    }}

    parametrize_test!{test_pretty_duration, [
        (s: u64, n: u32, p: u32, r: &'static str),
        (0, 0, 2, "0s"),
        (1, 0, 2, "1.0s"),
        (1, 100_000, 2, "1.1s"),
        (60, 0, 2, "1m"),
        (60, 1000, 2, "1m"),
        (120, 0, 2, "2m"),
        (3600, 0, 2, "1h"),
        (3660, 0, 2, "1h 1m"),
        (86460, 0, 2, "1d 1m"),
        (93780, 0, 2, "1d 2h 3m"),
        (93784, 0, 2, "1d 2h 3m 4s"),
    ], {
        let duration = Duration::new(s, n);
        assert_eq!(format!("{}", PrettyDuration(duration, p)), r);
    }}
}
