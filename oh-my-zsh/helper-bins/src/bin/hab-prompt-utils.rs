#[macro_use] extern crate clap;
extern crate helper_bins;
extern crate sha1;

use std::collections::BTreeMap;
use std::io::ErrorKind::{NotFound, PermissionDenied};
use std::io::{BufRead, BufReader, stdout, stderr};
use std::{env, fmt, fs, mem, path, process, time};

use helper_bins::duration::PrettyDuration;
use helper_bins::error::PromptResult as Result;
use helper_bins::{PluginServer, plugin};

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


struct Plugin {
    name: String,
    process: process::Child,
    stdin: process::ChildStdin,
    stdout: BufReader<process::ChildStdout>,
    meta: plugin::InitializeResponse,
    running: bool,
}

impl Plugin {
    fn new(path: path::PathBuf) -> Result<Plugin> {
        let process = process::Command::new(&path)
            .stdin(process::Stdio::piped())
            .stdout(process::Stdio::piped())
            .stderr(process::Stdio::inherit())
            .spawn();
        let mut process = try!(process);
        let name = path.to_string_lossy().into_owned();
        Ok(Plugin {
            name: name,
            stdin: process.stdin.take().unwrap(),
            stdout: BufReader::new(process.stdout.take().unwrap()),
            process: process,
            meta: Default::default(),
            running: true,
        })
    }

    fn reap(&mut self) {
        ((move || {
            self.running = false;
            try!(self.process.kill());
            try!(self.process.wait());
            Ok(())
        })() as Result<()>).expect("couldn't reap child")
    }

    fn issue_request(&mut self, req: &plugin::PluginRequest) -> Result<Option<plugin::PluginResponse>> {
        if !self.running {
            return Ok(None)
        }
        let resp = try!(PluginServer::new(&mut self.stdout, &mut self.stdin).issue_request(req));
        if resp.is_none() {
            self.reap()
        }
        Ok(resp)
    }

    fn initialize(&mut self) -> Result<()> {
        let req = plugin::InitializeRequest::default();
        if let Some(plugin::PluginResponse::Initialize(resp)) = try!(self.issue_request(&req.into())) {
            mem::replace(&mut self.meta, resp);
        }
        Ok(())
    }
}

impl Drop for Plugin {
    fn drop(&mut self) {
        self.reap()
    }
}

impl fmt::Debug for Plugin {
    fn fmt(&self, f: &mut fmt::Formatter) -> std::result::Result<(), fmt::Error> {
        write!(f, "Plugin({:?}, @{}, meta: {:?})", self.name, self.process.id(), self.meta)
    }
}


#[derive(Debug)]
struct PluginLoader {
    plugins: Vec<Plugin>,
}

impl PluginLoader {
    fn new() -> PluginLoader {
        PluginLoader {
            plugins: vec![],
        }
    }

    fn plugin_dir(&self) -> Option<path::PathBuf> {
        match env::var("HAB_PROMPT_PLUGIN_DIR") {
            Ok(d) => return Some(d.into()),
            _ => (),
        };
        let mut plugin_dir = match env::home_dir() {
            Some(h) => h,
            None => return None,
        };
        plugin_dir.extend(&[".config", "hab-prompt", "plugins"]);
        Some(plugin_dir)
    }

    fn load_plugins(&mut self) -> Result<()> {
        let plugin_dir = match self.plugin_dir() {
            Some(d) => d,
            None => return Ok(()),
        };
        for file in match plugin_dir.read_dir() {
            Ok(r) => r,
            Err(ref e) if e.kind() == NotFound => return Ok(()),
            Err(e) => return Err(e.into()),
        } {
            let file = try!(file).path();
            self.plugins.push(try!(Plugin::new(file)));
        }
        for plugin in &mut self.plugins {
            try!(plugin.initialize());
        }
        Ok(())
    }

    fn test_vc_dir(&mut self, path: &path::Path) -> Result<Option<plugin::VcStatusResponse>> {
        let mut req = plugin::VcStatusRequest::default();
        req.cwd = path.to_string_lossy().into_owned();
        let req = req.into();
        for plugin in &mut self.plugins {
            if !plugin.meta.handles_vc {
                continue
            }
            if let Some(plugin::PluginResponse::VcStatus(resp)) = try!(plugin.issue_request(&req)) {
                return Ok(Some(resp));
            }
        }
        Ok(None)
    }
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
    Ok(try!(path.metadata().map(|m| m.dev())))
}

const VC_DIRS: [(&'static str, Vc); 2] = [
    (".git", Vc::Git),
    (".hg", Vc::Hg),
];

fn test_vc_dir(path: &path::Path, plugins: Option<&mut PluginLoader>) -> Result<Option<(Vc, path::PathBuf)>> {
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

fn find_vc_root(mut plugins: Option<&mut PluginLoader>) -> Result<Option<(Vc, path::PathBuf, path::PathBuf)>> {
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
    Plugin(plugin::VcStatusResponse),
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
    fn from_current_dir(plugins: Option<&mut PluginLoader>) -> Result<Option<VcLoc>> {
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

fn vc_status(plugins: Option<&mut PluginLoader>) -> Result<String> {
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

fn git_head_branch() -> Result<String> {
    use std::io::Write;
    match try!(try!(VcLoc::from_current_dir(None)).map_or(Ok(None), |v| v.get_git_head_branch())) {
        Some(head) => Ok(head),
        None => {
            try!(write!(stderr(), "no branch found for git HEAD\n"));
            process::exit(111)
        },
    }
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

fn zsh_precmd_map(timers: Option<(time::Duration, time::Duration)>,
                  plugins: Option<&mut PluginLoader>) -> Result<BTreeMap<&'static str, String>> {
    let mut results = BTreeMap::new();
    if let Some((before, after)) = timers {
        let span = after - before;
        results.insert("duration", format!("{}", PrettyDuration(span)));
    } else {
        results.insert("duration", "—".to_string());
    }
    results.insert("vc", try!(vc_status(plugins)));
    results.insert("files", try!(file_count()));
    Ok(results)
}

fn zsh_precmd(timers: Option<(time::Duration, time::Duration)>,
              plugins: Option<&mut PluginLoader>) -> Result<()> {
    use std::io::Write;
    let map = try!(zsh_precmd_map(timers, plugins));
    let mut bytes: Vec<u8> = vec![];
    for (k, v) in &map {
        bytes.extend(k.as_bytes());
        bytes.push(0);
        bytes.extend(v.as_bytes());
        bytes.push(0);
    }
    bytes.pop();
    Ok(try!(stdout().write_all(&bytes[..])))
}

fn main() {
    let mut plugins = PluginLoader::new();
    plugins.load_plugins().expect("plugin failure");
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
          (@subcommand git_head_branch =>
           (aliases: &["git-head-branch"])
           (about: "Find the branch associated with the git HEAD")
          )
         )
         (@subcommand precmd =>
          (about: "Do everything the zsh precmd would need")
          (@arg TIMERS: ...)
         )
        ).get_matches();
    if let Some(m) = matches.subcommand_matches("emit") {
        if let Some(_) = m.subcommand_matches("vc_status") {
            vc_status(Some(&mut plugins))
        } else if let Some(_) = m.subcommand_matches("file_count") {
            file_count()
        } else if let Some(m) = m.subcommand_matches("color_hash") {
            use std::os::unix::ffi::OsStrExt;
            colorhash(m.value_of_os("STRING").unwrap().as_bytes(),
                      m.is_present("allow_all_colors"))
        } else if let Some(_) = m.subcommand_matches("git_head_branch") {
            git_head_branch()
        } else { return }.and_then(|s| actually_emit(s, m.is_present("no_newline")))
    } else if let Some(m) = matches.subcommand_matches("precmd") {
        let timers = values_t!(m, "TIMERS", u64).unwrap_or_else(|e| e.exit());
        let durations = if timers.len() == 4 {
            let before = time::Duration::new(timers[0], timers[1] as u32);
            let after = time::Duration::new(timers[2], timers[3] as u32);
            Some((before, after))
        } else { None };
        zsh_precmd(durations, Some(&mut plugins))
    } else { return }.expect("failure running subcommand")
}
