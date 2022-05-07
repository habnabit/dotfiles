use std::path;
use std::path::Path;

use async_trait::async_trait;

use super::errors::PromptResult as Result;
use crate::utils::FileCounts;

#[derive(Debug, Clone)]
pub struct VcsStatus {
    pub branch: String,
    pub display_branch: String,
    pub counts: FileCounts,
    pub counts_truncated: bool,
}

#[derive(Debug, Clone)]
pub struct VcStatus {
    pub vc_name: String,
    pub results: VcsStatus,
}

#[async_trait]
pub trait VcsPlugin {
    async fn status(&self, dir: &Path, branch_only: bool) -> Result<Option<VcStatus>>;
}

#[derive(Clone, Copy)]
pub struct BuiltinVcsPlugin {
    pub name: &'static str,
    pub vcs: &'static (dyn VcsPlugin + Sync + Send),
}

pub static VCS_GIT: BuiltinVcsPlugin = BuiltinVcsPlugin {
    name: "git",
    vcs: &crate::vc::Git,
};
pub static VCS_HG: BuiltinVcsPlugin = BuiltinVcsPlugin {
    name: "hg",
    vcs: &crate::vc::Hg,
};

pub struct PluginLoader {
    plugins: Vec<BuiltinVcsPlugin>,
}

impl PluginLoader {
    pub fn new() -> PluginLoader {
        PluginLoader { plugins: vec![] }
    }

    fn plugin_dir(&self) -> Option<path::PathBuf> {
        // XXX: real location searching
        todo!()
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

    pub async fn test_vc_dir(&self, path: &path::Path) -> Result<Option<VcStatus>> {
        todo!()
        // if self.plugins.is_empty() {
        //     return Box::new(future::ok(None));
        // }
        // let path = path.to_string_lossy();
        // let futures: Vec<_> = self
        //     .plugins
        //     .iter()
        //     .filter_map(vc_plugin)
        //     .map(|p| p.get_vc_name(&path, false))
        //     .collect();
        // Box::new(select_first_some((None, futures)))
    }
}

#[async_trait]
impl VcsPlugin for PluginLoader {
    async fn status(&self, dir: &Path, branch_only: bool) -> Result<Option<VcStatus>> {
        todo!()
    }
}
