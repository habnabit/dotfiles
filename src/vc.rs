use std::path;
use std::path::{Path, PathBuf};
use std::process::Stdio;
use std::str::from_utf8;

use async_trait::async_trait;
use tokio::io::AsyncBufReadExt;
use tokio::process::Command;

use super::errors::PromptResult as Result;
use super::utils::IncrementalMap;
use crate::directories::FileCounts;
use crate::plugins::{VcsPlugin, VcsStatus};

pub const GIT_INDEX_STATII: &'static str = "TMADRC";
pub const GIT_WORKING_STATII: &'static str = "TMD";
pub const STATUS_ORDER: &'static str = "UTMADRCtmd?";

fn tolower(c: char) -> char {
    c.to_lowercase().next().unwrap()
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
    fn command_setup(&self) -> Command {
        let mut cmd = Command::new("git");
        cmd.env("GIT_DIR", self.vc_dir.as_path());
        cmd.env("GIT_WORK_TREE", self.work_dir.as_path());
        cmd.stdin(Stdio::null());
        cmd.stderr(Stdio::inherit());
        cmd
    }

    async fn run_git_status(&self) -> Result<FileCounts> {
        let mut child = self
            .command_setup()
            .arg("status")
            .arg("--porcelain")
            .stdout(Stdio::piped())
            .kill_on_drop(true)
            .spawn()?;
        let mut ret: FileCounts = Default::default();
        {
            let stdout = tokio::io::BufReader::new(child.stdout.take().unwrap());
            let mut lines = stdout.lines();
            while let Some(line) = lines.next_line().await? {
                update_counts_git(&mut ret, &line);
            }
        }
        let status_out = child.wait().await;
        tracing::info!(?status_out, "git done");
        Ok(ret)
    }

    async fn get_head_branch(&self) -> Result<Option<String>> {
        let child = self
            .command_setup()
            .arg("symbolic-ref")
            .arg("HEAD")
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .kill_on_drop(true)
            .spawn()?;
        let output = child.wait_with_output().await?;
        if output.status.success() {
            let git_ref = from_utf8(&output.stdout)?;
            Ok(Some(
                git_ref.trim_start_matches("refs/heads/").trim().to_string(),
            ))
        } else {
            Ok(None)
        }
    }

    async fn get_sha(&self) -> Result<String> {
        let child = self
            .command_setup()
            .arg("show")
            .arg("--pretty=%h")
            .arg("-s")
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .kill_on_drop(true)
            .spawn()?;
        let output = child.wait_with_output().await?;
        if output.status.success() {
            let sha = from_utf8(&output.stdout)?;
            return Ok(format!("detached {}", sha.trim()));
        }
        Ok("unknown".to_owned())
    }

    async fn get_branch_and_symbolic(&self) -> Result<(String, String)> {
        let branch = self.get_head_branch().await?;
        let symbolic = match &branch {
            Some(s) => s.clone(),
            None => self.get_sha().await?,
        };
        Ok((branch.unwrap_or_else(|| symbolic.clone()), symbolic))
    }

    async fn get_all(self) -> Result<VcsStatus> {
        let (counts, (branch, display_branch)) =
            futures_util::future::try_join(self.run_git_status(), self.get_branch_and_symbolic())
                .await?;
        Ok(VcsStatus {
            branch,
            display_branch,
            counts,
        })
    }

    async fn get_branch_only(self) -> Result<VcsStatus> {
        let mut ret: VcsStatus = Default::default();
        if let Some(branch) = self.get_head_branch().await? {
            ret.branch = branch;
        }
        Ok(ret)
    }
}

pub struct Git;

#[async_trait]
impl VcsPlugin for Git {
    fn name(&self) -> &'static str {
        "git"
    }

    async fn status(&self, work_dir: PathBuf, branch_only: bool) -> Result<Option<VcsStatus>> {
        let mut vc_dir = work_dir.clone();
        vc_dir.push(".git");
        if !is_directory_usable(&vc_dir) {
            return Ok(None);
        }
        let req = GitRequest { work_dir, vc_dir };
        Ok(Some(if branch_only {
            req.get_branch_only().await?
        } else {
            req.get_all().await?
        }))
    }
}

#[derive(Clone)]
struct HgRequest {
    work_dir: path::PathBuf,
}

impl HgRequest {
    fn command_setup(&self) -> Command {
        let mut cmd = Command::new("hg");
        cmd.arg("-R").arg(self.work_dir.as_path());
        cmd.stdin(Stdio::null());
        cmd.stderr(Stdio::inherit());
        cmd
    }

    async fn run_hg_status(&self) -> Result<FileCounts> {
        let mut child = self
            .command_setup()
            .arg("status")
            .stdout(Stdio::piped())
            .kill_on_drop(true)
            .spawn()?;
        let mut ret: FileCounts = Default::default();
        {
            let stdout = tokio::io::BufReader::new(child.stdout.take().unwrap());
            let mut lines = stdout.lines();
            while let Some(line) = lines.next_line().await? {
                update_counts_hg(&mut ret, &line);
            }
        }
        let status_out = child.wait().await;
        tracing::info!(?status_out, "hg done");
        Ok(ret)
    }

    async fn get_branch(&self) -> Result<String> {
        let child = self
            .command_setup()
            .arg("id")
            .arg("-bn")
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .kill_on_drop(true)
            .spawn()?;
        let output = child.wait_with_output().await?;
        if output.status.success() {
            let hg_out = from_utf8(&output.stdout)?;
            let words: Vec<&str> = hg_out.split_whitespace().collect();
            return Ok(format!("{}.{}", words[1], words[0]));
        }
        Ok("unknown".to_string())
    }
}

pub struct Hg;

#[async_trait]
impl VcsPlugin for Hg {
    fn name(&self) -> &'static str {
        "hg"
    }

    async fn status(&self, work_dir: PathBuf, _branch_only: bool) -> Result<Option<VcsStatus>> {
        let mut vc_dir = work_dir.clone();
        vc_dir.push(".hg");
        if !is_directory_usable(&vc_dir) {
            return Ok(None);
        }
        let req = HgRequest { work_dir };
        let (counts, branch) =
            futures_util::future::try_join(req.run_hg_status(), req.get_branch()).await?;
        Ok(Some(VcsStatus {
            display_branch: branch.clone(),
            branch,
            counts,
        }))
    }
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
