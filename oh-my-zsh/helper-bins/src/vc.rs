use std::collections::BTreeMap;
use std::io::ErrorKind::{NotFound, PermissionDenied};
use std::io::{BufRead, BufReader, stdout, stderr};
use std::{env, path, process};

use capnp;
use capnp::capability::Promise;
use futures::{Future, done, future};
use tokio_core::reactor::Handle;
use tokio_process::CommandExt;

use super::errors::{PromptErrors, PromptResult as Result};
use super::utils::{IncrementalMap, btree_of_counts, format_counts, limited_foreach};
use super::plugins::{PluginLoader, VcStatus};
use super::plugins_capnp::version_control_plugin;

const GIT_INDEX_STATII: &'static str = "TMADRC";
const GIT_WORKING_STATII: &'static str = "TMD";
const HG_STATII: &'static str = "MAR?";
const STATUS_ORDER: &'static str = "UTMADRCtmd?";

fn tolower(c: char) -> char {
    c.to_lowercase().next().unwrap()
}

struct GitRequest {
    handle: Handle,
    vc_dir: path::PathBuf,
    work_dir: path::PathBuf,
}

impl GitRequest {

    fn command_setup(&self) -> process::Command {
        let mut cmd = process::Command::new("git");
        cmd.env("GIT_DIR", self.vc_dir.as_path());
        cmd.env("GIT_WORK_TREE", self.work_dir.as_path());
        cmd.stdin(process::Stdio::null());
        cmd.stderr(process::Stdio::inherit());
        cmd
    }

    fn run_git_status(&self) -> Box<Future<Item=(), Error=PromptErrors>> {
        let child = self.command_setup()
            .arg("status").arg("--porcelain")
            .stdout(process::Stdio::piped())
            .spawn_async(&self.handle);
        done(child)
            .and_then(|mut c| {
                let stdout = BufReader::new(c.stdout().take().unwrap());
                ::tokio_core::io::read_until(stdout, b'\n', vec![])
                    .map(|r| {

                    })
                    .join(c)
            })
            .map(|_| ())
            .from_err::<(), PromptErrors>()
            .boxed()
    }

    fn get_git_head_branch(&self) -> Result<Option<String>> {
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

}

struct Git;

impl version_control_plugin::Server for Git {
    fn status(&mut self,
              params: version_control_plugin::StatusParams,
              mut results: version_control_plugin::StatusResults) -> Promise<(), capnp::Error>
    {
        Promise::ok(())
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

fn path_dev(path: &path::Path) -> Result<u64> {
    use std::os::unix::fs::MetadataExt;
    Ok(try!(path.metadata().map(|m| m.dev())))
}

const VC_DIRS: [(&'static str, Vc); 2] = [
    (".git", Vc::Git),
    (".hg", Vc::Hg),
];

fn vc_root_step<'a>(loader: &'a mut PluginLoader, top_dev: u64, cur: path::PathBuf) -> Box<Future<Item=Option<VcStatus>, Error=PromptErrors> + 'a>
{
    enum Step {
        Done(Result<Option<VcStatus>>),
        TryNext(path::PathBuf),
    }
    let ret = loader.test_vc_dir(&cur).map(move |o| {
        if let Some(s) = o {
            Step::Done(Ok(Some(s)))
        } else if let Some(p) = cur.parent() {
            match path_dev(p) {
                Ok(d) if d == top_dev => Step::TryNext(p.into()),
                Ok(_) => Step::Done(Ok(None)),
                Err(e) => Step::Done(Err(e.into())),
            }
        } else {
            Step::Done(Ok(None))
        }
    }).and_then(move |s| match s {
        Step::Done(r) => Box::new(future::done(r)),
        Step::TryNext(p) => vc_root_step(loader, top_dev, p),
    });
    Box::new(ret)
}

fn find_vc_root<'a>(loader: &'a mut PluginLoader) -> Box<Future<Item=Option<VcStatus>, Error=PromptErrors> + 'a>
{
    let (cwd, top_dev) = match env::current_dir() {
        Ok(cwd) => match path_dev(&cwd) {
            Ok(top_dev) => (cwd, top_dev),
            Err(e) => return Box::new(future::err(e.into())),
        },
        Err(e) => return Box::new(future::err(e.into())),
    };
    vc_root_step(loader, top_dev, cwd)
}

#[derive(Clone)]
enum Vc {
    Git,
    Hg,
    Plugin,
}

impl Vc {
    fn as_name(&self) -> &str {
        match self {
            &Vc::Git => "git",
            &Vc::Hg => "hg",
            &Vc::Plugin => "plugin",
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
            &Vc::Plugin => unreachable!(),
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
            &Vc::Plugin => unreachable!(),
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
            &Vc::Plugin => unreachable!(),
        }
    }
}

pub fn vc_status<'a>(loader: &'a mut PluginLoader) -> Box<Future<Item=String, Error=PromptErrors> + 'a> {
    let ret = find_vc_root(loader).map(|o| {
        let status = match o {
            Some(v) => v,
            None => return "".into(),
        };
        use std::fmt::Write;
        let mut ret = String::new();
        // XXX less unwrap
        let reader = status.results.get_root_as_reader();
        write!(ret, "{} {}", status.vc_name, reader.get_branch().unwrap()).unwrap();
        let counts: Result<String> = reader.get_file_counts().map_err(Into::into).and_then(|c| {
            let ret = format_counts(
                STATUS_ORDER, &try!(btree_of_counts(&c)), c.get_truncated(), false);
            Ok(ret)
        });
        let counts = counts.unwrap();
        if !counts.is_empty() {
            write!(ret, ": {}", counts).unwrap();
        }
        ret
    });
    Box::new(ret)
}

// pub fn git_head_branch() -> Result<String> {
//     use std::io::Write;
//     match try!(try!(VcLoc::from_current_dir()).map_or(Ok(None), |v| v.get_git_head_branch())) {
//         Some(head) => Ok(head),
//         None => {
//             try!(write!(stderr(), "no branch found for git HEAD\n"));
//             process::exit(111)
//         },
//     }
// }
