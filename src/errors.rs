use thiserror::Error;

#[derive(Error, Debug)]
pub enum PromptErrors {
    #[error("std::io::Error: {0:?}")]
    Io(#[from] std::io::Error),
    #[error("std::fmt::Error: {0:?}")]
    Fmt(#[from] std::fmt::Error),
    #[error("std::string::FromUtf8Error: {0:?}")]
    FromUtf8Error(#[from] std::string::FromUtf8Error),
    #[error("std::str::Utf8Error: {0:?}")]
    Utf8Error(#[from] std::str::Utf8Error),
    #[error("tokio::task::JoinError: {0:?}")]
    JoinError(#[from] tokio::task::JoinError),

    #[error("invalid ssh-proxy host {0:?}")]
    InvalidSshProxy(String),
    #[error("couldn't install a file: {0}")]
    InstallationError(String),
    #[error("no HEAD found")]
    NoHead,
}

pub type PromptResult<T> = Result<T, PromptErrors>;
