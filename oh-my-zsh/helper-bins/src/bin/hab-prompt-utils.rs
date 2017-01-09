#[macro_use] extern crate clap;
extern crate futures;
extern crate helper_bins;
extern crate tokio_core;

use std::collections::BTreeMap;
use std::io::stdout;
use std::{path, process, time};

use futures::Future;

use helper_bins::colors::colorhash;
use helper_bins::directories::file_count;
use helper_bins::durations::PrettyDuration;
use helper_bins::errors::{PromptErrors, PromptResult as Result};
use helper_bins::installer::install_from_manifest;
use helper_bins::plugins::{PluginLoader, TestVcDirService};
use helper_bins::ssh_proxy::ssh_proxy_command;
use helper_bins::vc::{git_head_branch, vc_status};

fn actually_emit(s: String, no_newline: bool) -> Result<()> {
    use std::io::Write;
    let stdout_ = stdout();
    let mut stdout_locked = stdout_.lock();
    try!(stdout_locked.write_all(&s.as_bytes()[..]));
    if !no_newline {
        try!(stdout_locked.write_all(b"\n"));
    }
    Ok(())
}

fn zsh_precmd_map(timers: Option<(time::Duration, time::Duration)>, test_vc_dir: TestVcDirService) -> Box<Future<Item=BTreeMap<&'static str, String>, Error=PromptErrors>>
{
    let ret = futures::lazy(move || {
        let mut results = BTreeMap::new();
        if let Some((before, after)) = timers {
            let span = after - before;
            results.insert("duration", format!("{}", PrettyDuration(span)));
        } else {
            results.insert("duration", "—".to_string());
        }
        results.insert("files", try!(file_count()));
        Ok(results)
    });
    let ret = ret
        .join(vc_status(test_vc_dir))
        .and_then(|(mut results, status)| {
            results.insert("vc", status);
            Ok(results)
        });
    Box::new(ret)
}

fn zsh_precmd(timers: Option<(time::Duration, time::Duration)>, test_vc_dir: TestVcDirService) -> Box<Future<Item=(), Error=PromptErrors>>
{
    let ret = zsh_precmd_map(timers, test_vc_dir)
        .and_then(|map| {
            let mut bytes: Vec<u8> = vec![];
            for (k, v) in &map {
                bytes.extend(k.as_bytes());
                bytes.push(0);
                bytes.extend(v.as_bytes());
                bytes.push(0);
            }
            bytes.pop();
            tokio_core::io::write_all(stdout(), bytes)
                .map_err(Into::into)
        })
        .map(|_| ());
    Box::new(ret)
}

fn run_in_loop<F, T>(func: F) -> Result<T>
    where F: FnOnce(TestVcDirService) -> Box<Future<Item=T, Error=PromptErrors>>
{
    let mut lp = try!(tokio_core::reactor::Core::new());
    let fut = PluginLoader::new(lp.handle())
        .load_builtin_plugins()
        .load_plugins()
        .and_then(|loader| func(loader.spawn()));
    lp.run(fut)
}

fn main() {
    let matches = clap_app!
        (hab_utils =>
         (version: "0.1")
         (author: "Aaron Gallagher <_@habnab.it>")
         (about: "General utilities")
         (@setting SubcommandRequired)
         (@subcommand emit =>
          (about: "Emit a string")
          (@arg no_newline: -n "Don't emit a trailing newline")
          (@setting SubcommandRequired)
          (@subcommand vc_status =>
           (aliases: &["vc-status"])
           (about: "Write a line describing version control status, if possible")
          )
          (@subcommand file_count =>
           (aliases: &["file-count"])
           (about: "Count the number of files in the current directory")
          )
          (@subcommand color_hash =>
           (aliases: &["color-hash"])
           (about: "Hash a value into a 256-color number")
           (@arg STRING: +required)
           (@arg allow_all_colors: -a --allow_all_colors "Don't filter out hard-to-read colors")
          )
          (@subcommand git_head_branch =>
           (aliases: &["git-head-branch"])
           (about: "Find the branch associated with the git HEAD")
          )
         )
         (@subcommand precmd =>
          (about: "Do everything the zsh precmd would need")
          (@arg TIMERS: ...)
         )
         (@subcommand ssh_proxy =>
          (aliases: &["ssh-proxy"])
          (about: "Proxy to a remote sshd in a standard-ish way")
          (@arg dry_run: -n --dry_run "Don't actually exec ssh")
          (@arg HOST: +required)
          (@arg PORT: +required)
          (@arg SSHARGS: ...)
         )
         (@subcommand install =>
          (about: "Install files according to a manifest")
          (@arg MANIFEST: +required)
          (@arg TARGET:)
         )
        ).get_matches();
    if let Some(m) = matches.subcommand_matches("emit") {
        if let Some(_) = m.subcommand_matches("vc_status") {
            run_in_loop(vc_status)
        } else if let Some(_) = m.subcommand_matches("file_count") {
            file_count()
        } else if let Some(m) = m.subcommand_matches("color_hash") {
            use std::os::unix::ffi::OsStrExt;
            colorhash(m.value_of_os("STRING").unwrap().as_bytes(),
                      m.is_present("allow_all_colors"))
        } else if let Some(_) = m.subcommand_matches("git_head_branch") {
            run_in_loop(git_head_branch)
        } else { return }.and_then(|s| actually_emit(s, m.is_present("no_newline")))
    } else if let Some(m) = matches.subcommand_matches("precmd") {
        let timers = values_t!(m, "TIMERS", u64).unwrap_or_else(|e| e.exit());
        let durations = if timers.len() == 4 {
            let before = time::Duration::new(timers[0], timers[1] as u32);
            let after = time::Duration::new(timers[2], timers[3] as u32);
            Some((before, after))
        } else { None };
        run_in_loop(move |test_vc_dir| zsh_precmd(durations, test_vc_dir))
    } else if let Some(m) = matches.subcommand_matches("ssh_proxy") {
        let host = m.value_of("HOST").unwrap();
        let port = m.value_of("PORT").unwrap();
        let mut args = m.values_of_os("SSHARGS");
        let args_iter = args.as_mut().map(|i| i as &mut Iterator<Item=_>);
        ssh_proxy_command(host, port, args_iter).and_then(|mut c| {
            if m.is_present("dry_run") {
                use std::io::Write;
                let stdout_ = stdout();
                try!(write!(stdout_.lock(), "would run: {:?}\n", c));
            } else {
                try!(c.status().and_then(|e| process::exit(e.code().unwrap_or(1))));
            }
            Ok(())
        })
    } else if let Some(m) = matches.subcommand_matches("install") {
        let manifest = path::Path::new(m.value_of("MANIFEST").unwrap());
        let target_dir = path::Path::new(m.value_of("TARGET").unwrap());
        install_from_manifest(manifest, target_dir)
    } else { return }.expect("failure running subcommand")
}
