use std::collections::BTreeMap;
use std::fs;

use super::errors::PromptResult as Result;
use super::utils::{format_counts, limited_foreach, IncrementalMap};

const FILE_ORDER: &'static str = ".-/l?";

fn count_files() -> Result<(BTreeMap<char, usize>, bool)> {
    let mut counts = BTreeMap::new();
    let truncated = try!(limited_foreach(try!(fs::read_dir(".")), |entry| {
        let entry = try!(entry);
        match entry.file_name().as_os_str().to_str() {
            Some(s) if s.starts_with(".") => counts.increment('.'),
            _ => {
                let ty = try!(entry.file_type());
                if ty.is_symlink() {
                    counts.increment('l');
                } else if ty.is_file() {
                    counts.increment('-');
                } else if ty.is_dir() {
                    counts.increment('/');
                } else {
                    counts.increment('?');
                }
            },
        };
        Ok(())
    }));
    Ok((counts, truncated))
}

pub fn file_count() -> Result<String> {
    let (counts, truncated) = try!(count_files());
    Ok(format_counts(FILE_ORDER, &counts, truncated, true))
}
