use std::collections::BTreeMap;
use std::path::Path;

use async_trait::async_trait;

use super::errors::PromptResult as Result;

#[repr(transparent)]
#[derive(Debug, Clone, Default)]
pub struct FileCounts(BTreeMap<char, usize>);

const ITERATION_LIMIT: usize = 1000;

pub fn format_counts(
    order: &str, counts: &FileCounts, truncated: bool, show_total: bool,
) -> String {
    use std::fmt::Write;
    let mut sorted: Vec<_> = counts
        .0
        .iter()
        .filter_map(|(&c, &count)| order.find(c).map(|pos| (pos, c, count)))
        .collect();
    sorted.sort();
    let mut ret = String::new();
    for &(_, c, count) in &sorted {
        write!(ret, "{}{} ", count, c).unwrap();
    }
    if show_total {
        let total = counts.0.values().fold(0, |acc, &x| acc + x);
        write!(ret, "≡{} ", total).unwrap();
    }
    if ret.pop().is_some() && truncated {
        ret.push('…');
    }
    ret
}

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

impl IncrementalMap<char> for FileCounts {
    fn increment(&mut self, key: char) {
        self.0.increment(key)
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
