use std::collections::BTreeMap;
use std::future::Future;
use std::io::stdout;
use std::{path, process, time};

use clap::{clap_app, values_t};
use helper_bins::colors::make_theme;
use helper_bins::directories::file_count;
use helper_bins::durations::PrettyDuration;
use helper_bins::errors::PromptResult as Result;
use helper_bins::installer::install_from_manifest;
use helper_bins::ssh_proxy::ssh_proxy_command;
use helper_bins::utils::{default_theme_seed, VcsPlugin};
use helper_bins::vc::{git_head_branch, vc_status, Git};
use hsl::HSL;
use serde_json::json;
use tokio::io::AsyncWriteExt;
use tokio::runtime::Handle;

fn actually_emit(s: String, no_newline: bool) -> Result<()> {
    use std::io::Write;
    let stdout_ = stdout();
    let mut stdout_locked = stdout_.lock();
    stdout_locked.write_all(&s.as_bytes()[..])?;
    if !no_newline {
        stdout_locked.write_all(b"\n")?;
    }
    Ok(())
}

async fn zsh_precmd_map(
    timers: Option<(time::Duration, time::Duration)>, vcs: &dyn VcsPlugin,
) -> Result<BTreeMap<&'static str, String>> {
    let mut results = BTreeMap::new();
    if let Some((before, after)) = timers {
        let span = after - before;
        results.insert("duration", format!("{}", PrettyDuration(span)));
    } else {
        results.insert("duration", "—".to_string());
    }
    results.insert("files", file_count()?);
    let status = vc_status(vcs).await?;
    results.insert("vc", status);
    Ok(results)
}

fn stringify_theme(
    theme: BTreeMap<&'static str, HSL>, format: Option<&str>, source: &str,
) -> Result<String> {
    Ok(match format {
        Some("zsh") => {
            let theme_ansi = theme
                .into_iter()
                .map(|(k, v)| (k, ansi_of_hsl(v)))
                .collect::<BTreeMap<_, _>>();
            zsh_map_string(&theme_ansi)
        },
        Some("json") => {
            let mut theme_map = theme
                .into_iter()
                .map(|(k, v)| (k.into(), json_of_hsl(v)))
                .collect::<serde_json::map::Map<String, _>>();
            theme_map.insert("_source".to_owned(), source.into());
            let theme_json: serde_json::Value = theme_map.into();
            serde_json::to_string(&theme_json).unwrap() // XXX
        },
        _ => {
            use std::fmt::Write;
            let mut out = String::new();
            write!(out, "generated for {:?}:\n", source)?;
            for (k, v) in theme {
                write!(out, "{}: {:?}\n", k, v)?;
            }
            out
        },
    })
}

fn ansi_of_hsl(color: HSL) -> String {
    let (r, g, b) = color.to_rgb();
    let ansi_color = ansi_term::Color::RGB(r, g, b);
    format!("{}", ansi_color.normal().prefix())
}

fn json_of_hsl(color: HSL) -> serde_json::Value {
    let (r, g, b) = color.to_rgb();
    json!({
        "h": color.h,
        "s": color.s,
        "l": color.l,
        "r": r,
        "g": g,
        "b": b,
    })
}

fn zsh_map_string(map: &BTreeMap<&'static str, String>) -> String {
    let mut ret: String = Default::default();
    for (k, v) in map {
        ret.push_str(k);
        ret.push('\0');
        ret.push_str(v);
        ret.push('\0');
    }
    ret.pop();
    ret
}

async fn zsh_precmd(
    timers: Option<(time::Duration, time::Duration)>, vcs: &dyn VcsPlugin,
) -> Result<()> {
    let map = zsh_precmd_map(timers, vcs).await?;
    let map_str = zsh_map_string(&map);
    tokio::io::stdout().write_all(map_str.as_bytes()).await?;
    Ok(())
}

fn run_in_loop<FN, Fut, T>(func: FN) -> T
where
    FN: FnOnce(&'static dyn VcsPlugin) -> Fut,
    Fut: Future<Output = T>,
{
    let handle = Handle::current();
    let fut = func(&Git);
    handle.block_on(fut)
}

#[tokio::main]
async fn main() {
    let () = Handle::current()
        .spawn_blocking(blocking_main)
        .await
        .unwrap_or_else(|err| panic!("join error? {:?}", err));
}

fn blocking_main() {
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
      (@subcommand git_head_branch =>
       (aliases: &["git-head-branch"])
       (about: "Find the branch associated with the git HEAD")
      )
     )
     (@subcommand precmd =>
      (about: "Do everything the zsh precmd would need")
      (@arg TIMERS: ...)
     )
     (@subcommand color_theme =>
      (aliases: &["color-theme"])
      (about: "Hash a value into a 24-bit color theme")
      (@arg format: -f --format +takes_value "Set the output format (zsh, json)")
      (@arg STRING:)
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
    )
    .get_matches();
    if let Some(m) = matches.subcommand_matches("emit") {
        if let Some(_) = m.subcommand_matches("vc_status") {
            run_in_loop(vc_status)
        } else if let Some(_) = m.subcommand_matches("file_count") {
            file_count()
        } else if let Some(_) = m.subcommand_matches("git_head_branch") {
            run_in_loop(git_head_branch)
        } else {
            return;
        }
        .and_then(|s| actually_emit(s, m.is_present("no_newline")))
    } else if let Some(m) = matches.subcommand_matches("precmd") {
        let timers = values_t!(m, "TIMERS", u64).unwrap_or_else(|e| e.exit());
        let durations = if timers.len() == 4 {
            let before = time::Duration::new(timers[0], timers[1] as u32);
            let after = time::Duration::new(timers[2], timers[3] as u32);
            Some((before, after))
        } else {
            None
        };
        run_in_loop(move |test_vc_dir| zsh_precmd(durations, test_vc_dir))
    } else if let Some(m) = matches.subcommand_matches("color_theme") {
        let the_string = m
            .value_of("STRING")
            .map(ToOwned::to_owned)
            .unwrap_or_else(default_theme_seed);
        let theme = make_theme(&the_string);
        stringify_theme(theme, m.value_of("format"), &the_string)
            .and_then(|s| actually_emit(s, true))
    } else if let Some(m) = matches.subcommand_matches("ssh_proxy") {
        let host = m.value_of("HOST").unwrap();
        let port = m.value_of("PORT").unwrap();
        let mut args = m.values_of_os("SSHARGS");
        let args_iter = args.as_mut().map(|i| i as &mut dyn Iterator<Item = _>);
        ssh_proxy_command(host, port, args_iter).and_then(|mut c| {
            if m.is_present("dry_run") {
                use std::io::Write;
                let stdout_ = stdout();
                write!(stdout_.lock(), "would run: {:?}\n", c)?;
            } else {
                c.status()
                    .and_then(|e| process::exit(e.code().unwrap_or(1)))?;
            }
            Ok(())
        })
    } else if let Some(m) = matches.subcommand_matches("install") {
        let manifest = path::Path::new(m.value_of("MANIFEST").unwrap());
        let target_dir = path::Path::new(m.value_of("TARGET").unwrap());
        install_from_manifest(manifest, target_dir)
    } else {
        return;
    }
    .expect("failure running subcommand")
}
