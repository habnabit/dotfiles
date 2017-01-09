use std::collections::BTreeMap;
use std::io::{BufRead, BufReader};
use std::{env, path, process};

use capnp;
use capnp::capability::Promise;
use futures::{Future, future};
use tokio_core::reactor::Handle;
use tokio_process::CommandExt;
use tokio_service::Service;

use super::errors::{PromptErrors, PromptResult as Result};
use super::utils::{IncrementalMap, btree_of_counts, format_counts};
use super::plugins::{TestVcDirService, VcStatus};
use super::plugins_capnp::{file_counts, version_control_plugin};

const GIT_INDEX_STATII: &'static str = "TMADRC";
const GIT_WORKING_STATII: &'static str = "TMD";
const HG_STATII: &'static str = "MAR?";
const STATUS_ORDER: &'static str = "UTMADRCtmd?";

fn tolower(c: char) -> char {
    c.to_lowercase().next().unwrap()
}

fn status_inner_loop<F: 'static, T: 'static>(reader: T, mut updater: F, buf: Vec<u8>, mut ret: BTreeMap<char, usize>) -> Box<Future<Item=BTreeMap<char, usize>, Error=PromptErrors>>
    where F: FnMut(&mut BTreeMap<char, usize>, &str),
          T: BufRead,
{
    let ret = ::tokio_core::io::read_until(reader, b'\n', buf)
        .map_err(Into::into)
        .and_then(move |(reader, mut buf)| -> Box<Future<Item=BTreeMap<char, usize>, Error=PromptErrors>> {
            match ::std::str::from_utf8(&buf) {
                Err(e) => return Box::new(future::err(e.into())),
                Ok("") => return Box::new(future::ok(ret)),
                Ok(line) => updater(&mut ret, line),
            }
            buf.clear();
            status_inner_loop(reader, updater, buf, ret)
        });
    Box::new(ret)
}

fn status_line_loop<F: 'static, T: 'static>(reader: T, updater: F) -> Box<Future<Item=BTreeMap<char, usize>, Error=PromptErrors>>
    where F: FnMut(&mut BTreeMap<char, usize>, &str),
          T: BufRead,
{
    status_inner_loop(reader, updater, vec![], BTreeMap::new())
}

fn is_directory_usable(p: &path::Path) -> bool {
    p.metadata().is_ok()
}

#[derive(Clone)]
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

    fn run_git_status(&self) -> Box<Future<Item=BTreeMap<char, usize>, Error=PromptErrors>> {
        let child = self.command_setup()
            .arg("status").arg("--porcelain")
            .stdout(process::Stdio::piped())
            .spawn_async(&self.handle);
        let ret = future::done(child)
            .map_err(Into::into)
            .and_then(|mut c| {
                let stdout = BufReader::new(c.stdout().take().unwrap());
                status_line_loop(stdout, update_counts_git).join(c.map_err(Into::into))
            })
            .map(|(counts, _)| counts);
        Box::new(ret)
    }

    fn get_git_head_branch(&self) -> Box<Future<Item=Option<String>, Error=PromptErrors>> {
        let child = self.command_setup()
            .arg("symbolic-ref").arg("HEAD")
            .stdout(process::Stdio::piped())
            .stderr(process::Stdio::null())
            .spawn_async(&self.handle);
        let ret = future::done(child)
            .and_then(|c| c.wait_with_output())
            .map_err(Into::into)
            .and_then(|output| {
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
            });
        Box::new(ret)
    }

    fn get_git_sha(&self) -> Box<Future<Item=String, Error=PromptErrors>> {
        let child = self.command_setup()
            .arg("show").arg("--pretty=%h").arg("-s")
            .stdout(process::Stdio::piped())
            .stderr(process::Stdio::null())
            .spawn_async(&self.handle);
        let ret = future::done(child)
            .and_then(|c| c.wait_with_output())
            .map_err(Into::into)
            .and_then(|output| {
                if output.status.success() {
                    let sha = try!(from_utf8(output.stdout));
                    return Ok(format!("detached {}", sha.as_str().trim()));
                }
                Ok("unknown".to_string())
            });
        Box::new(ret)
    }

    fn fill_all(self, mut results: version_control_plugin::StatusResults) -> Box<Future<Item=(), Error=PromptErrors>> {
        let status = self.run_git_status();
        let ret = self.get_git_head_branch()
            .and_then(move |o| -> Box<Future<Item=(Option<String>, String), Error=PromptErrors>> {
                match o {
                    Some(s) => Box::new(future::ok((Some(s.clone()), s))),
                    None => Box::new(self.get_git_sha().map(|s| (None, s))),
                }})
            .join(status)
            .map(move |((branch_opt, display), counts)| {
                let mut resp = results.get().init_status().init_some();
                if let Some(branch) = branch_opt {
                    resp.set_branch(&branch);
                }
                resp.set_display_branch(&display);
                write_counts(resp.init_counts(), &counts, false);
            });
        Box::new(ret)
    }

    fn fill_branch_only(self, mut results: version_control_plugin::StatusResults) -> Box<Future<Item=(), Error=PromptErrors>> {
        let ret = self.get_git_head_branch()
            .map(move |branch_opt| {
                let mut resp = results.get().init_status().init_some();
                if let Some(branch) = branch_opt {
                    resp.set_branch(&branch);
                }
            });
        Box::new(ret)
    }
}

pub struct Git(pub Handle);

impl version_control_plugin::Server for Git {
    fn status(&mut self,
              params: version_control_plugin::StatusParams,
              results: version_control_plugin::StatusResults) -> Promise<(), capnp::Error>
    {
        let handle = self.0.clone();
        let ret = future::lazy(move || {
            let params = try!(params.get());
            let work_dir: path::PathBuf = path::Path::new(try!(params.get_directory())).into();
            let mut vc_dir = work_dir.clone();
            vc_dir.push(".git");
            if !is_directory_usable(&vc_dir) {
                return Ok(None);
            }
            Ok(Some((params.get_branch_only(), GitRequest {
                handle: handle,
                work_dir: work_dir,
                vc_dir: vc_dir,
            })))
        }).and_then(move |o| match o {
            None => Box::new(future::ok(())),
            Some((true, req)) => req.fill_branch_only(results),
            Some((false, req)) => req.fill_all(results),
        }).map_err(|e: PromptErrors| ::capnp::Error::failed(format!("{:?}", e)));
        Promise::from_future(ret)
    }
}

#[derive(Clone)]
struct HgRequest {
    handle: Handle,
    vc_dir: path::PathBuf,
    work_dir: path::PathBuf,
}

impl HgRequest {
    fn command_setup(&self) -> process::Command {
        let mut cmd = process::Command::new("hg");
        cmd.arg("-R").arg(self.work_dir.as_path());
        cmd.stdin(process::Stdio::null());
        cmd.stderr(process::Stdio::inherit());
        cmd
    }

    fn run_hg_status(&self) -> Box<Future<Item=BTreeMap<char, usize>, Error=PromptErrors>> {
        let child = self.command_setup()
            .arg("status")
            .stdout(process::Stdio::piped())
            .spawn_async(&self.handle);
        let ret = future::done(child)
            .map_err(Into::into)
            .and_then(|mut c| {
                let stdout = BufReader::new(c.stdout().take().unwrap());
                status_line_loop(stdout, update_counts_hg).join(c.map_err(Into::into))
            })
            .map(|(counts, _)| counts);
        Box::new(ret)
    }

    fn get_hg_branch(&self) -> Box<Future<Item=String, Error=PromptErrors>> {
        let child = self.command_setup()
            .arg("id").arg("-bn")
            .stdout(process::Stdio::piped())
            .stderr(process::Stdio::null())
            .spawn_async(&self.handle);
        let ret = future::done(child)
            .and_then(|c| c.wait_with_output())
            .map_err(Into::into)
            .and_then(|output| {
                if output.status.success() {
                    let hg_out = try!(from_utf8(output.stdout));
                    let words: Vec<&str> = hg_out.split_whitespace().collect();
                    return Ok(format!("{}.{}", words[1], words[0]));
                }
                Ok("unknown".to_string())
            });
        Box::new(ret)
    }

}

pub struct Hg(pub Handle);

impl version_control_plugin::Server for Hg {
    fn status(&mut self,
              params: version_control_plugin::StatusParams,
              mut results: version_control_plugin::StatusResults) -> Promise<(), capnp::Error>
    {
        let handle = self.0.clone();
        let ret = future::lazy(move || {
            let params = try!(params.get());
            let work_dir: path::PathBuf = path::Path::new(try!(params.get_directory())).into();
            let mut vc_dir = work_dir.clone();
            vc_dir.push(".hg");
            Ok(HgRequest {
                handle: handle,
                work_dir: work_dir,
                vc_dir: vc_dir,
            })
        }).and_then(|req| {
            req.get_hg_branch().join(req.run_hg_status())
        }).map(move |(branch, counts)| {
            let mut resp = results.get().init_status().init_some();
            resp.set_display_branch(&branch);
            write_counts(resp.init_counts(), &counts, false);
        }).map_err(|e: PromptErrors| ::capnp::Error::failed(format!("{:?}", e)));
        Promise::from_future(ret)
    }
}

fn update_counts_git(counts: &mut BTreeMap<char, usize>, line: &str) {
    let chars: Vec<char> = line.chars().take(4).collect();
    if chars.len() < 4 || chars[2] != ' ' {
        return;
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
    }
}

fn update_counts_hg(counts: &mut BTreeMap<char, usize>, line: &str) {
    let chars: Vec<char> = line.chars().take(3).collect();
    if chars.len() < 3 || chars[1] != ' ' {
        return;
    }
    match chars[0] {
        c if HG_STATII.contains(c) => counts.increment(tolower(c)),
        _ => (),
    }
}

fn write_counts(mut counts_builder: file_counts::Builder, counts: &BTreeMap<char, usize>, truncated: bool) {
    counts_builder.set_truncated(truncated);
    let mut entries = counts_builder.init_entries(counts.len() as u32);
    let mut buf = String::new();
    for (e, (&k, &v)) in counts.into_iter().enumerate() {
        let mut entry = entries.borrow().get(e as u32);
        buf.clear();
        buf.push(k);
        entry.set_file_type(&buf);
        entry.set_count(v as u32);
    }
}

fn path_dev(path: &path::Path) -> Result<u64> {
    use std::os::unix::fs::MetadataExt;
    Ok(try!(path.metadata().map(|m| m.dev())))
}

fn vc_root_step(mut test_vc_dir: TestVcDirService, top_dev: u64, cur: path::PathBuf) -> Box<Future<Item=Option<VcStatus>, Error=PromptErrors>>
{
    enum Step {
        Done(Result<Option<VcStatus>>),
        TryNext(path::PathBuf),
    }
    let ret = test_vc_dir.call(cur.clone()).map(move |o| {
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
        Step::TryNext(p) => vc_root_step(test_vc_dir, top_dev, p),
    });
    Box::new(ret)
}

fn find_vc_root(test_vc_dir: TestVcDirService) -> Box<Future<Item=Option<VcStatus>, Error=PromptErrors>>
{
    let (cwd, top_dev) = match env::current_dir() {
        Ok(cwd) => match path_dev(&cwd) {
            Ok(top_dev) => (cwd, top_dev),
            Err(e) => return Box::new(future::err(e.into())),
        },
        Err(e) => return Box::new(future::err(e.into())),
    };
    vc_root_step(test_vc_dir, top_dev, cwd)
}

fn from_utf8(v: Vec<u8>) -> Result<String> {
    Ok(try!(String::from_utf8(v)))
}

pub fn vc_status(test_vc_dir: TestVcDirService) -> Box<Future<Item=String, Error=PromptErrors>> {
    let ret = find_vc_root(test_vc_dir).map(|o| {
        let status = match o {
            Some(v) => v,
            None => return "".into(),
        };
        use std::fmt::Write;
        let mut ret = String::new();
        // XXX less unwrap
        let reader = status.results.get_root_as_reader();
        write!(ret, "{} {}", status.vc_name, reader.get_display_branch().unwrap()).unwrap();
        let counts: Result<String> = reader.get_counts().map_err(Into::into).and_then(|c| {
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

pub fn git_head_branch(test_vc_dir: TestVcDirService) -> Box<Future<Item=String, Error=PromptErrors>> {
    // XXX query branch only
    let ret = find_vc_root(test_vc_dir).and_then(|o| {
        let status = match o {
            Some(v) => v,
            None => return Ok(None),
        };
        match try!(status.results.get_root_as_reader().get_branch()) {
            "" => Ok(None),
            s => Ok(Some(s.into())),
        }
    }).and_then(|o| match o {
        Some(head) => Ok(head),
        None => Err(PromptErrors::NoHead),
    });
    Box::new(ret)
}
