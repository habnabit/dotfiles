use std::path::{Path, PathBuf};

use async_trait::async_trait;

use super::errors::PromptResult as Result;
use crate::errors::PromptErrors;
use crate::utils::{format_counts, path_dev, FileCounts};
use crate::vc::STATUS_ORDER;

#[derive(Debug, Clone, Default)]
pub struct VcsStatus {
    pub branch: String,
    pub display_branch: String,
    pub counts: FileCounts,
    pub counts_truncated: bool,
}

#[derive(Debug, Clone)]
pub struct PluginVcsStatus {
    pub vc_name: String,
    pub inner: VcsStatus,
}

#[async_trait]
pub trait VcsPlugin {
    fn name(&self) -> &'static str;
    async fn status(&self, dir: PathBuf, branch_only: bool) -> Result<Option<VcsStatus>>;
}

pub type BuiltinVcsPlugin = &'static (dyn VcsPlugin + Sync + Send);

pub static VCS_GIT: BuiltinVcsPlugin = &crate::vc::Git;
pub static VCS_HG: BuiltinVcsPlugin = &crate::vc::Hg;

pub struct PluginLoader {
    plugins: Vec<BuiltinVcsPlugin>,
}

impl PluginLoader {
    pub fn new() -> PluginLoader {
        PluginLoader { plugins: vec![] }
    }

    pub fn load_builtin_plugins(mut self) -> Self {
        self.plugins.push(VCS_GIT);
        self.plugins.push(VCS_HG);
        self
    }

    pub async fn load_plugins(self) -> Result<Self> {
        // XXX: load some real plugins
        Ok(self)
    }
}

impl PluginLoader {
    async fn status(&self, dir: PathBuf, branch_only: bool) -> Result<Option<PluginVcsStatus>> {
        use futures_util::TryFutureExt;
        let mut set = tokio::task::JoinSet::new();
        for p in &self.plugins {
            set.spawn(p.status(dir.clone(), branch_only).map_ok(|s| (p.name(), s)));
        }
        let status = loop {
            match set.join_one().await? {
                Some(Ok((vc_name, Some(inner)))) => {
                    break PluginVcsStatus {
                        vc_name: vc_name.to_owned(),
                        inner,
                    }
                },
                Some(Ok((_, None))) => continue,
                Some(Err(e)) => return Err(e),
                None => return Ok(None),
            }
        };
        Ok(Some(status))
    }

    async fn find_vc_root(&self) -> Result<Option<PluginVcsStatus>> {
        let cwd = std::env::current_dir()?;
        let top_dev = path_dev(&cwd)?;
        let mut cur: &Path = cwd.as_path();
        loop {
            if let Some(status) = self.status(cur.to_owned(), false).await? {
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

    pub async fn vc_status(&self) -> Result<String> {
        let status = match self.find_vc_root().await? {
            Some(v) => v,
            None => return Ok("".into()),
        };
        use std::fmt::Write;
        let mut ret = String::new();
        // XXX less unwrap
        write!(ret, "{} {}", status.vc_name, status.inner.display_branch).unwrap();
        let counts = format_counts(
            STATUS_ORDER,
            &status.inner.counts,
            status.inner.counts_truncated,
            false,
        );
        if !counts.is_empty() {
            write!(ret, ": {}", counts).unwrap();
        }
        Ok(ret)
    }

    pub async fn git_head_branch(&self) -> Result<String> {
        let status = self.find_vc_root().await?;
        match status {
            Some(PluginVcsStatus {
                inner: VcsStatus { branch, .. },
                ..
            }) if !branch.is_empty() => Ok(branch),
            _ => Err(PromptErrors::NoHead),
        }
    }
}
