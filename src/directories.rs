use std::collections::BTreeMap;
use std::fs;
use std::path::Path;

use super::errors::PromptResult as Result;
use super::utils::{limited_foreach, IncrementalMap};

#[derive(Debug, Clone, Default)]
pub struct FileCounts {
    pub map: BTreeMap<char, usize>,
    pub truncated: bool,
}

impl FileCounts {
    pub fn format_ordered(&self, order: &str, show_total: bool) -> String {
        use std::fmt::Write;
        let mut sorted: Vec<_> = self
            .map
            .iter()
            .filter_map(|(&c, &count)| order.find(c).map(|pos| (pos, c, count)))
            .collect();
        sorted.sort();
        let mut ret = String::new();
        for &(_, c, count) in &sorted {
            write!(ret, "{}{} ", count, c).unwrap();
        }
        if show_total {
            let total = self.map.values().fold(0, |acc, &x| acc + x);
            write!(ret, "≡{} ", total).unwrap();
        }
        if ret.pop().is_some() && self.truncated {
            ret.push('…');
        }
        ret
    }

    pub fn from_read_dir<P>(dir: P) -> Result<Self>
    where
        P: AsRef<Path>,
    {
        let mut map: BTreeMap<_, _> = Default::default();
        let truncated = limited_foreach(fs::read_dir(dir)?, |entry| {
            let entry = entry?;
            match entry.file_name().as_os_str().to_str() {
                Some(s) if s.starts_with(".") => map.increment('.'),
                _ => {
                    let ty = entry.file_type()?;
                    if ty.is_symlink() {
                        map.increment('l');
                    } else if ty.is_file() {
                        map.increment('-');
                    } else if ty.is_dir() {
                        map.increment('/');
                    } else {
                        map.increment('?');
                    }
                },
            };
            Ok(())
        })?;
        Ok(FileCounts { map, truncated })
    }
}

impl IncrementalMap<char> for FileCounts {
    fn increment(&mut self, key: char) {
        self.map.increment(key)
    }
}

const FILE_ORDER: &'static str = ".-/l?";

pub fn file_count() -> Result<String> {
    let counts = FileCounts::from_read_dir(".")?;
    Ok(counts.format_ordered(FILE_ORDER, true))
}
