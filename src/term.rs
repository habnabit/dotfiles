// This module implements terminal related helpers.
// Copyright (c) 2015 by Shipeng Feng.
// Licensed under the BSD License, see LICENSE for more details.

use std::io::{self, Write};
use std::str;

fn build_prompt_text(
    text: &str, suffix: &str, show_default: bool, default: Option<&str>,
) -> String {
    let prompt_text: String;
    if default.is_some() && show_default {
        prompt_text = format!("{} [{}]", text, default.unwrap());
    } else {
        prompt_text = text.to_string();
    }
    prompt_text + suffix
}

fn get_prompt_input(prompt_text: &str) -> String {
    let mut stdout = io::stdout();
    stdout
        .write_all(prompt_text.as_bytes())
        .and_then(|_| stdout.flush())
        .expect("Failed to write prompt");
    let mut input = String::new();
    io::stdin()
        .read_line(&mut input)
        .expect("Failed to read line");
    let new_len = input.trim_end_matches("\n").len();
    input.truncate(new_len);
    input
}

/// Prompts for confirmation (yes/no question).
///
/// - `text` - the question to ask
/// - `default` - the default for the prompt
/// - `prompt_suffix` - a suffix that should be added to the prompt
/// - `show_default` - shows or hides the default value
///
pub fn confirm(text: &str, default: bool, prompt_suffix: &str, show_default: bool) -> bool {
    let default_string = match default {
        true => Some("Y/n"),
        false => Some("y/N"),
    };
    let prompt_text = build_prompt_text(text, prompt_suffix, show_default, default_string);

    loop {
        let prompt_input = get_prompt_input(&prompt_text).to_ascii_lowercase();
        match prompt_input.trim() {
            "y" | "yes" => {
                return true;
            },
            "n" | "no" => {
                return false;
            },
            "" => {
                return default;
            },
            _ => {
                println!("Error: invalid input");
            },
        }
    }
}
