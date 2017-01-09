use std::str::Utf8Error;
use std::{error, fmt, io};

#[derive(Debug)]
pub enum PromptErrors {
    Io(io::Error),
    Utf8(Utf8Error),
    Boxed(Box<error::Error>),
    InvalidSshProxy(String),
    InstallationError(String),
    NoHead,
}

impl error::Error for PromptErrors {
    fn description(&self) -> &str {
        use self::PromptErrors::*;
        match self {
            &Io(_) => "io error",
            &Utf8(_) => "utf8 decode error",
            &Boxed(_) => "upstream error",
            &InvalidSshProxy(_) => "invalid ssh-proxy host",
            &InstallationError(_) => "couldn't install a file",
            &NoHead => "no HEAD found",
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

impl From<Utf8Error> for PromptErrors {
    fn from(e: Utf8Error) -> PromptErrors {
        PromptErrors::Utf8(e)
    }
}

impl From<::std::string::FromUtf8Error> for PromptErrors {
    fn from(e: ::std::string::FromUtf8Error) -> PromptErrors {
        PromptErrors::Utf8(e.utf8_error())
    }
}

impl From<::futures::Canceled> for PromptErrors {
    fn from(e: ::futures::Canceled) -> PromptErrors {
        PromptErrors::Boxed(Box::new(e))
    }
}

impl From<::capnp::Error> for PromptErrors {
    fn from(e: ::capnp::Error) -> PromptErrors {
        PromptErrors::Boxed(Box::new(e))
    }
}

pub type PromptResult<T> = Result<T, PromptErrors>;
