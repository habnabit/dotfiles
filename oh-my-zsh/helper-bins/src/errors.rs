use std::string::FromUtf8Error;
use std::{error, fmt, io};

#[derive(Debug)]
pub enum PromptErrors {
    Io(io::Error),
    Utf8(FromUtf8Error),
    Capnp,
    InvalidSshProxy(String),
    InstallationError(String),
}

impl error::Error for PromptErrors {
    fn description(&self) -> &str {
        use self::PromptErrors::*;
        match self {
            &Io(_) => "io error",
            &Utf8(_) => "utf8 decode error",
            &Capnp => "capnp error",
            &InvalidSshProxy(_) => "invalid ssh-proxy host",
            &InstallationError(_) => "couldn't install a file",
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

impl From<FromUtf8Error> for PromptErrors {
    fn from(e: FromUtf8Error) -> PromptErrors {
        PromptErrors::Utf8(e)
    }
}

impl From<::capnp::Error> for PromptErrors {
    fn from(_: ::capnp::Error) -> PromptErrors {
        PromptErrors::Capnp
    }
}

pub type PromptResult<T> = Result<T, PromptErrors>;
