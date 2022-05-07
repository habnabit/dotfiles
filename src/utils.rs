use std::collections::BTreeMap;
use std::path::Path;

use super::errors::PromptResult as Result;

pub const ITERATION_LIMIT: usize = 1000;

pub fn limited_foreach<I, F>(iter: I, mut func: F) -> Result<bool>
where
    I: IntoIterator,
    F: FnMut(I::Item) -> Result<()>,
{
    let mut limit = ITERATION_LIMIT;
    for item in iter {
        if limit == 0 {
            return Ok(true);
        }
        func(item)?;
        limit -= 1;
    }
    Ok(false)
}

pub trait IncrementalMap<T> {
    fn increment(&mut self, key: T);
}

impl<T: Ord> IncrementalMap<T> for BTreeMap<T, usize> {
    fn increment(&mut self, key: T) {
        *self.entry(key).or_insert(0) += 1;
    }
}

pub fn default_theme_seed() -> String {
    let mut ret = whoami::username();
    let hostname = whoami::hostname();
    if let Some(seg) = hostname.split('.').next() {
        ret.push('@');
        ret.push_str(seg);
    }
    ret
}

pub fn path_dev(path: &Path) -> Result<u64> {
    use std::os::unix::fs::MetadataExt;
    Ok(path.metadata().map(|m| m.dev())?)
}
