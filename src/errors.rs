use thiserror::Error;

#[derive(Error, Debug)]
pub enum PromptErrors {
    #[error("invalid ssh-proxy host {0:?}")]
    InvalidSshProxy(String),
    #[error("couldn't install a file: {0}")]
    InstallationError(String),
    #[error("no HEAD found")]
    NoHead,
}
