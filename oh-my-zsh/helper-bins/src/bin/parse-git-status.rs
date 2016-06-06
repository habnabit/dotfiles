use std::collections::BTreeMap;
use std::io::*;
use std::process;

const INDEX_STATII: &'static str = "MADRC";
const WORKING_STATII: &'static str = "MD";
const STATUS_ORDER: &'static str = "UMADRCmd?";

fn tolower(c: char) -> char {
    c.to_lowercase().next().unwrap()
}

fn tally_counts_git(f: &mut BufRead) -> Result<BTreeMap<char, usize>> {
    let mut counts = BTreeMap::new();
    {
        let mut add = |c| {
            *counts.entry(c).or_insert(0) += 1;
        };
        for line in f.lines() {
            let line = try!(line);
            let chars: Vec<char> = line.chars().take(4).collect();
            if chars.len() < 4 || chars[2] != ' ' {
                continue;
            }
            match (chars[0], chars[1]) {
                ('?', '?') => add('?'),
                ('U', _) |
                (_, 'U') |
                ('D', 'D') |
                ('A', 'A') => add('U'),
                (c, _) if INDEX_STATII.contains(c) => add(c),
                (_, c) if WORKING_STATII.contains(c) => add(tolower(c)),
                _ => (),
            };
        }
    }
    Ok(counts)
}

fn run_git() -> Result<BTreeMap<char, usize>> {
    let child = process::Command::new("git")
        .arg("status").arg("--porcelain")
        .stdin(process::Stdio::null())
        .stdout(process::Stdio::piped())
        .stderr(process::Stdio::inherit())
        .spawn();
    let mut child = try!(child);
    let counts = try!(tally_counts_git(&mut BufReader::new(child.stdout.as_mut().unwrap())));
    try!(child.wait());
    Ok(counts)
}

fn format_counts(order: &str, counts: &BTreeMap<char, usize>) -> Vec<u8> {
    let mut sorted: Vec<_> = counts
        .into_iter()
        .filter_map(|(&c, &count)| order.find(c).map(|pos| (pos, c, count)))
        .collect();
    sorted.sort();
    let mut ret = Vec::new();
    for &(_, c, count) in &sorted {
        write!(ret, "{}{} ", count, c).unwrap();
    }
    if let Some(c) = ret.last_mut() {
        *c = b'\n';
    }
    ret
}

fn main() {
    let counts = run_git().expect("couldn't read counts");
    {
        let mut f = stdout();
        f.write_all(&format_counts(STATUS_ORDER, &counts)[..]).expect("couldn't write counts");
    }
}
