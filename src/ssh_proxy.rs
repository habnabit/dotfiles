use std::borrow::Cow;
use std::ffi::OsStr;
use std::{env, process};

use regex;

use super::errors::{PromptErrors, PromptResult as Result};

enum SshProxyTarget<'a> {
    Jail(&'a str),
    Via(&'a str),
}

impl<'a> SshProxyTarget<'a> {
    fn with_ssh_args(self, host: &str, port: &str, mut cmd: process::Command) -> process::Command {
        use self::SshProxyTarget::*;
        match self {
            Jail(jail) => {
                cmd
                    .arg(host)
                    .arg("sudo").arg("jexec").arg(jail)
                    .arg("sshd").arg("-i");
            },
            Via(target) => {
                cmd
                    .arg("-W").arg(format!("{}:{}", target, port))
                    .arg(host);
            },
        }
        cmd
    }
}

lazy_static! {
    static ref SSH_PROXY_PATTERN: regex::Regex = regex::Regex::new(r"(?ix)
        \A(?:(?P<jail>[a-zA-Z0-9_-]+) \._jails
            |(?P<via> [a-zA-Z0-9.-]+) \._via
        )\.(?P<host>  [a-zA-Z0-9._-]+)\z
    ").unwrap();
}

fn parse_ssh_proxy_host(host: &str) -> Result<(SshProxyTarget, &str)> {
    let matches = match SSH_PROXY_PATTERN.captures(host) {
        Some(m) => m,
        None => return Err(PromptErrors::InvalidSshProxy(host.to_owned())),
    };
    let host = matches.name("host").unwrap();
    let target = if let Some(j) = matches.name("jail") { SshProxyTarget::Jail(j.as_str()) }
    else if let Some(v) = matches.name("via") { SshProxyTarget::Via(v.as_str()) }
    else { unreachable!() };
    Ok((target, host.as_str()))
}

pub fn ssh_proxy_command(host: &str, port: &str, args: Option<&mut dyn Iterator<Item=&OsStr>>) -> Result<process::Command>
{
    let (target, host) = try!(parse_ssh_proxy_host(host));
    let ssh_cmd = if let Ok(ssh) = env::var("SSH_PROXY_SSH") {
        Cow::Owned(ssh)
    } else {
        Cow::Borrowed("ssh")
    };
    let mut cmd = process::Command::new(&*ssh_cmd);
    cmd.arg("-enone");
    if let Some(args) = args {
        for arg in args {
            cmd.arg(arg);
        }
    }
    if let Ok(extra) = env::var("SSH_PROXY_EXTRA_ARGS") {
        for arg in extra.split_whitespace() {
            cmd.arg(arg);
        }
    }
    Ok(target.with_ssh_args(host, port, cmd))
}
