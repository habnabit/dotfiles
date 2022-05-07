use std::collections::BTreeMap;
use std::io::{BufRead, BufReader};
use std::path::Path;
use std::{env, path, process};

use async_trait::async_trait;

use super::errors::{PromptErrors, PromptResult as Result};
use super::utils::{format_counts, IncrementalMap};
use crate::plugins::{VcStatus, VcsPlugin};
use crate::utils::FileCounts;

const GIT_INDEX_STATII: &'static str = "TMADRC";
const GIT_WORKING_STATII: &'static str = "TMD";
const STATUS_ORDER: &'static str = "UTMADRCtmd?";

fn tolower(c: char) -> char {
    c.to_lowercase().next().unwrap()
}

async fn status_inner_loop<F: 'static, T: 'static>(
    reader: T, mut updater: F, buf: Vec<u8>, mut ret: FileCounts,
) -> Result<FileCounts>
where
    F: FnMut(&mut FileCounts, &str),
    T: BufRead,
{
    todo!()
    // let ret = ::tokio_core::io::read_until(reader, b'\n', buf)
    //     .map_err(Into::into)
    //     .and_then(
    //         move |(reader, mut buf)| -> BoxFuture<FileCounts> {
    //             match ::std::str::from_utf8(&buf) {
    //                 Err(e) => return Box::new(future::err(e.into())),
    //                 Ok("") => return Box::new(future::ok(ret)),
    //                 Ok(line) => updater(&mut ret, line),
    //             }
    //             buf.clear();
    //             status_inner_loop(reader, updater, buf, ret)
    //         },
    //     );
    // Box::new(ret)
}

async fn status_line_loop<F: 'static, T: 'static>(reader: T, updater: F) -> Result<FileCounts>
where
    F: FnMut(&mut FileCounts, &str),
    T: BufRead,
{
    status_inner_loop(reader, updater, vec![], Default::default()).await
}

fn is_directory_usable(p: &Path) -> bool {
    p.metadata().is_ok()
}

#[derive(Clone)]
struct GitRequest {
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

    async fn run_git_status(&self) -> Result<FileCounts> {
        let child = self
            .command_setup()
            .arg("status")
            .arg("--porcelain")
            .stdout(process::Stdio::piped());
        // .spawn_async();
        todo!()
        // let ret = future::done(child)
        //     .map_err(Into::into)
        //     .and_then(|mut c| {
        //         let stdout = BufReader::new(c.stdout().take().unwrap());
        //         status_line_loop(stdout, update_counts_git).join(c.map_err(Into::into))
        //     })
        //     .map(|(counts, _)| counts);
        // Box::new(ret)
    }

    async fn get_git_head_branch(&self) -> Result<Option<String>> {
        let child = self
            .command_setup()
            .arg("symbolic-ref")
            .arg("HEAD")
            .stdout(process::Stdio::piped())
            .stderr(process::Stdio::null());
        // .spawn_async();
        todo!()
        // let ret = future::done(child)
        //     .and_then(|c| c.wait_with_output())
        //     .map_err(Into::into)
        //     .and_then(|output| {
        //         if output.status.success() {
        //             let git_ref = from_utf8(output.stdout)?;
        //             Ok(Some(
        //                 git_ref
        //                     .as_str()
        //                     .trim_start_matches("refs/heads/")
        //                     .trim()
        //                     .to_string(),
        //             ))
        //         } else {
        //             Ok(None)
        //         }
        //     });
        // Box::new(ret)
    }

    async fn get_git_sha(&self) -> Result<String> {
        let child = self
            .command_setup()
            .arg("show")
            .arg("--pretty=%h")
            .arg("-s")
            .stdout(process::Stdio::piped())
            .stderr(process::Stdio::null());
        // .spawn_async();
        todo!()
        // let ret = future::done(child)
        //     .and_then(|c| c.wait_with_output())
        //     .map_err(Into::into)
        //     .and_then(|output| {
        //         if output.status.success() {
        //             let sha = from_utf8(output.stdout)?;
        //             return Ok(format!("detached {}", sha.as_str().trim()));
        //         }
        //         Ok("unknown".to_string())
        //     });
        // Box::new(ret)
    }

    async fn fill_all(self, mut results: ()) -> Result<()> {
        let status = self.run_git_status().await?;
        let branch_opt = match self.get_git_head_branch().await? {
            Some(s) => (Some(s.clone()), s),
            None => (None, self.get_git_sha().await?),
        };
        todo!()
        //     .join(status)
        //     .map(move |((branch_opt, display), counts)| {
        //         let mut resp = results.get().init_status().init_some();
        //         if let Some(branch) = branch_opt {
        //             resp.set_branch(&branch);
        //         }
        //         resp.set_display_branch(&display);
        //         write_counts(resp.init_counts(), &counts, false);
        //     });
        // Box::new(ret)
    }

    async fn fill_branch_only(self, mut results: ()) -> Result<()> {
        if let Some(branch) = self.get_git_head_branch().await? {}
        todo!()
        // let ret = .map(move |branch_opt| {
        //     let mut resp = results.get().init_status().init_some();
        //     if let Some(branch) = branch_opt {
        //         resp.set_branch(&branch);
        //     }
        // });
        // Box::new(ret)
    }
}

pub struct Git;

#[async_trait]
impl VcsPlugin for Git {
    async fn status(&self, dir: &Path, branch_only: bool) -> Result<Option<VcStatus>> {
        todo!()
    }
    // let handle = self.0.clone();
    // let ret = future::lazy(move || {
    //     let params = params.get()?;
    //     let work_dir: path::PathBuf = path::Path::new(params.get_directory()?).into();
    //     let mut vc_dir = work_dir.clone();
    //     vc_dir.push(".git");
    //     if !is_directory_usable(&vc_dir) {
    //         return Ok(None);
    //     }
    //     Ok(Some((params.get_branch_only(), GitRequest {
    //         handle: handle,
    //         work_dir: work_dir,
    //         vc_dir: vc_dir,
    //     })))
    // })
    // .and_then(move |o| match o {
    //     None => Box::new(future::ok(())),
    //     Some((true, req)) => req.fill_branch_only(results),
    //     Some((false, req)) => req.fill_all(results),
    // })
    // .map_err(|e: PromptErrors| ::capnp::Error::failed(format!("{:?}", e)));
    // Promise::from_future(ret)
}

#[derive(Clone)]
struct HgRequest {
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

    async fn run_hg_status(&self) -> Result<FileCounts> {
        let child = self
            .command_setup()
            .arg("status")
            .stdout(process::Stdio::piped());
        // .spawn_async();
        todo!()
        // let ret = future::done(child)
        //     .map_err(Into::into)
        //     .and_then(|mut c| {
        //         let stdout = BufReader::new(c.stdout().take().unwrap());
        //         status_line_loop(stdout, update_counts_hg).join(c.map_err(Into::into))
        //     })
        //     .map(|(counts, _)| counts);
        // Box::new(ret)
    }

    async fn get_hg_branch(&self) -> Result<String> {
        let child = self
            .command_setup()
            .arg("id")
            .arg("-bn")
            .stdout(process::Stdio::piped())
            .stderr(process::Stdio::null());
        // .spawn_async();
        todo!()
        // let ret = future::done(child)
        //     .and_then(|c| c.wait_with_output())
        //     .map_err(Into::into)
        //     .and_then(|output| {
        //         if output.status.success() {
        //             let hg_out = from_utf8(output.stdout)?;
        //             let words: Vec<&str> = hg_out.split_whitespace().collect();
        //             return Ok(format!("{}.{}", words[1], words[0]));
        //         }
        //         Ok("unknown".to_string())
        //     });
        // Box::new(ret)
    }

    async fn fill_all(self) -> Result<()> {
        Ok(())
        // let ret = self
        //     .get_hg_branch()
        //     .join(self.run_hg_status())
        //     .map(move |(branch, counts)| {
        //         let mut resp = results.get().init_status().init_some();
        //         resp.set_branch(&branch);
        //         resp.set_display_branch(&branch);
        //         write_counts(resp.init_counts(), &counts, false);
        //     });
        // Box::new(ret)
    }
}

pub struct Hg;

#[async_trait]
impl VcsPlugin for Hg {
    async fn status(&self, dir: &Path, branch_only: bool) -> Result<Option<VcStatus>> {
        todo!()
    }
    // fn status(&self) -> () {
    //     let handle = self.0.clone();
    //     let ret = future::lazy(move || {
    //         let params = params.get()?;
    //         let work_dir: path::PathBuf = path::Path::new(params.get_directory()?).into();
    //         let mut vc_dir = work_dir.clone();
    //         vc_dir.push(".hg");
    //         if !is_directory_usable(&vc_dir) {
    //             Ok(None)
    //         } else {
    //             Ok(Some(HgRequest {
    //                 handle: handle,
    //                 work_dir: work_dir,
    //                 vc_dir: vc_dir,
    //             }))
    //         }
    //     })
    //     .and_then(|o| {
    //         o.map(move |req| req.fill_all(results))
    //             .unwrap_or_else(|| Box::new(future::ok(())))
    //     })
    //     .map_err(|e: PromptErrors| ::capnp::Error::failed(format!("{:?}", e)));
    //     Promise::from_future(ret)
    // }
}

fn update_counts_git(counts: &mut FileCounts, line: &str) {
    let chars: Vec<char> = line.chars().take(4).collect();
    if chars.len() < 4 || chars[2] != ' ' {
        return;
    }
    match (chars[0], chars[1]) {
        ('?', '?') => counts.increment('?'),
        ('U', _) | (_, 'U') | ('D', 'D') | ('A', 'A') => counts.increment('U'),
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

fn update_counts_hg(counts: &mut FileCounts, line: &str) {
    let chars: Vec<char> = line.chars().take(3).collect();
    if chars.len() < 3 || chars[1] != ' ' {
        return;
    }
    match chars[0] {
        'M' | '?' => counts.increment(tolower(chars[0])),
        'A' => counts.increment('A'),
        'R' => counts.increment('d'),
        _ => (),
    }
}

fn path_dev(path: &Path) -> Result<u64> {
    use std::os::unix::fs::MetadataExt;
    Ok(path.metadata().map(|m| m.dev())?)
}

async fn find_vc_root(vcs: &dyn VcsPlugin) -> Result<Option<VcStatus>> {
    let cwd = env::current_dir()?;
    let top_dev = path_dev(&cwd)?;
    let mut cur: &Path = cwd.as_path();
    loop {
        if let Some(status) = vcs.status(cur, false).await? {
            return Ok(Some(status));
        } else if let Some(parent) = cur.parent() {
            if path_dev(parent)? == top_dev {
                cur = parent;
                continue;
            }
        }
        return Ok(None);
    }
}

fn from_utf8(v: Vec<u8>) -> Result<String> {
    Ok(String::from_utf8(v)?)
}

pub async fn vc_status(vcs: &dyn VcsPlugin) -> Result<String> {
    let status = match find_vc_root(vcs).await? {
        Some(v) => v,
        None => return Ok("".into()),
    };
    use std::fmt::Write;
    let mut ret = String::new();
    // XXX less unwrap
    write!(ret, "{} {}", status.vc_name, status.results.display_branch,).unwrap();
    let counts = format_counts(
        STATUS_ORDER,
        &status.results.counts,
        status.results.counts_truncated,
        false,
    );
    if !counts.is_empty() {
        write!(ret, ": {}", counts).unwrap();
    }
    Ok(ret)
}

pub async fn git_head_branch(vcs: &dyn VcsPlugin) -> Result<String> {
    // XXX query branch only
    todo!()
    // let ret = find_vc_root(test_vc_dir)
    //     .and_then(|o| {
    //         let status = match o {
    //             Some(v) => v,
    //             None => return Ok(None),
    //         };
    //         match status.results.get_root_as_reader().get_branch()? {
    //             "" => Ok(None),
    //             s => Ok(Some(s.into())),
    //         }
    //     })
    //     .and_then(|o| match o {
    //         Some(head) => Ok(head),
    //         None => Err(PromptErrors::NoHead),
    //     });
    // Box::new(ret)
}
