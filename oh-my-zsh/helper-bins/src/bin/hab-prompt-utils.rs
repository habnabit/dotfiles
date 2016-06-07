extern crate clap;

use std::collections::BTreeMap;
use std::io::*;
use std::{fs, process};

const INDEX_STATII: &'static str = "MADRC";
const WORKING_STATII: &'static str = "MD";
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

fn tally_counts_git(f: &mut BufRead) -> Result<(BTreeMap<char, usize>, bool)> {
    let mut counts = BTreeMap::new();
    let mut truncated = false;
    {
        for (e, line) in f.lines().enumerate() {
            if e > ITERATION_LIMIT {
                truncated = true;
                break;
            }
            let line = try!(line);
            let chars: Vec<char> = line.chars().take(4).collect();
            if chars.len() < 4 || chars[2] != ' ' {
                continue;
            }
            match (chars[0], chars[1]) {
                ('?', '?') => counts.increment('?'),
                ('U', _) |
                (_, 'U') |
                ('D', 'D') |
                ('A', 'A') => counts.increment('U'),
                (c, _) if INDEX_STATII.contains(c) => counts.increment(c),
                (_, c) if WORKING_STATII.contains(c) => counts.increment(tolower(c)),
                _ => (),
            };
        }
    }
    Ok((counts, truncated))
}

fn run_git_status() -> Result<(BTreeMap<char, usize>, bool)> {
    let child = process::Command::new("git")
        .arg("status").arg("--porcelain")
        .stdin(process::Stdio::null())
        .stdout(process::Stdio::piped())
        .stderr(process::Stdio::inherit())
        .spawn();
    let mut child = try!(child);
    let ret = {
        let stdout = child.stdout.take().unwrap();
        try!(tally_counts_git(&mut BufReader::new(stdout)))
    };
    try!(child.wait());
    Ok(ret)
}

fn format_counts(order: &str, counts: &BTreeMap<char, usize>, truncated: bool) -> Vec<u8> {
    let mut sorted: Vec<_> = counts
        .into_iter()
        .filter_map(|(&c, &count)| order.find(c).map(|pos| (pos, c, count)))
        .collect();
    sorted.sort();
    let mut ret = Vec::new();
    for &(_, c, count) in &sorted {
        write!(ret, "{}{} ", count, c).unwrap();
    }
    if let Some(_) = ret.pop() {
        if truncated {
            ret.extend("…".as_bytes());
        }
        ret.push(b'\n');
    }
    ret
}

fn count_files() -> Result<(BTreeMap<char, usize>, bool)> {
    let mut counts = BTreeMap::new();
    let mut truncated = false;
    for (e, entry) in try!(fs::read_dir(".")).enumerate() {
        if e > ITERATION_LIMIT {
            truncated = true;
            break;
        }
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
        }
    }
    Ok((counts, truncated))
}

fn vc_status() -> Result<()> {
    let (counts, truncated) = try!(run_git_status());
    try!(stdout().write_all(&format_counts(STATUS_ORDER, &counts, truncated)[..]));
    Ok(())
}

fn file_count() -> Result<()> {
    let (counts, truncated) = try!(count_files());
    try!(stdout().write_all(&format_counts(FILE_ORDER, &counts, truncated)[..]));
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
        .get_matches();
    if let Some(_) = matches.subcommand_matches("vc-status") {
        vc_status().expect("couldn't run git");
    } else if let Some(_) = matches.subcommand_matches("file-count") {
        file_count().expect("couldn't count files");
    }
}
