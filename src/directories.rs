use std::fs;

use super::errors::PromptResult as Result;
use super::utils::{format_counts, limited_foreach, IncrementalMap};
use crate::utils::FileCounts;

const FILE_ORDER: &'static str = ".-/l?";

fn count_files() -> Result<(FileCounts, bool)> {
    let mut counts = <FileCounts as Default>::default();
    let truncated = limited_foreach(fs::read_dir(".")?, |entry| {
        let entry = entry?;
        match entry.file_name().as_os_str().to_str() {
            Some(s) if s.starts_with(".") => counts.increment('.'),
            _ => {
                let ty = entry.file_type()?;
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
    })?;
    Ok((counts, truncated))
}

pub fn file_count() -> Result<String> {
    let (counts, truncated) = count_files()?;
    Ok(format_counts(FILE_ORDER, &counts, truncated, true))
}
