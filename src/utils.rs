use std::collections::BTreeMap;
use std::path::Path;

use super::errors::PromptResult as Result;

const ITERATION_LIMIT: usize = 1000;

pub fn limited_foreach<I, F>(iter: I, mut func: F) -> Result<bool>
where
    I: IntoIterator,
    F: FnMut(I::Item) -> Result<()>,
{
    for (e, item) in iter.into_iter().enumerate() {
        if e >= ITERATION_LIMIT {
            return Ok(true);
        }
        func(item)?;
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

pub fn from_utf8(v: Vec<u8>) -> Result<String> {
    Ok(String::from_utf8(v)?)
}

pub fn path_dev(path: &Path) -> Result<u64> {
    use std::os::unix::fs::MetadataExt;
    Ok(path.metadata().map(|m| m.dev())?)
}
