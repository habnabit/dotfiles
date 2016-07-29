use std::string::FromUtf8Error;
use std::{error, fmt, io};

use serde_json;

#[derive(Debug)]
pub enum PromptErrors {
    Io(io::Error),
    Utf8(FromUtf8Error),
    Json(serde_json::Error),
    InvalidSshProxy(String),
}

impl error::Error for PromptErrors {
    fn description(&self) -> &str {
        use self::PromptErrors::*;
        match self {
            &Io(_) => "io error",
            &Utf8(_) => "utf8 decode error",
            &Json(_) => "json error",
            &InvalidSshProxy(_) => "invalid ssh-proxy host",
        }
    }
}

impl fmt::Display for PromptErrors {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{:?}", self)
    }
}

impl From<io::Error> for PromptErrors {
    fn from(e: io::Error) -> PromptErrors {
        PromptErrors::Io(e)
    }
}

impl From<serde_json::Error> for PromptErrors {
    fn from(e: serde_json::Error) -> PromptErrors {
        PromptErrors::Json(e)
    }
}

impl From<FromUtf8Error> for PromptErrors {
    fn from(e: FromUtf8Error) -> PromptErrors {
        PromptErrors::Utf8(e)
    }
}

pub type PromptResult<T> = Result<T, PromptErrors>;
