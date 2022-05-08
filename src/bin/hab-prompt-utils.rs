use std::collections::BTreeMap;
use std::ffi::OsString;
use std::io::stdout;
use std::path::PathBuf;
use std::time;

use anyhow::{Context, Result};
use clap::{ArgEnum, Args, Parser, Subcommand};
use helper_bins::colors::make_theme;
use helper_bins::directories::file_count;
use helper_bins::durations::PrettyDuration;
use helper_bins::installer::install_from_manifest;
use helper_bins::plugins::PluginLoader;
use helper_bins::ssh_proxy::ssh_proxy_command;
use helper_bins::utils::default_theme_seed;
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
    timers: Option<(time::Duration, time::Duration)>, plugins: &PluginLoader,
) -> Result<BTreeMap<&'static str, String>> {
    let mut results = BTreeMap::new();
    if let Some((before, after)) = timers {
        let span = after - before;
        results.insert("duration", format!("{}", PrettyDuration(span)));
    } else {
        results.insert("duration", "—".to_string());
    }
    results.insert("files", file_count()?);
    let status = plugins.vc_status().await?;
    results.insert("vc", status);
    Ok(results)
}

fn stringify_theme(
    theme: BTreeMap<&'static str, HSL>, format: OutputFormat, source: &str,
) -> Result<String> {
    use self::OutputFormat::*;
    Ok(match format {
        Zsh => {
            let theme_ansi = theme
                .into_iter()
                .map(|(k, v)| (k, ansi_of_hsl(v)))
                .collect::<BTreeMap<_, _>>();
            zsh_map_string(&theme_ansi)
        },
        Json => {
            let mut theme_map = theme
                .into_iter()
                .map(|(k, v)| (k.into(), json_of_hsl(v)))
                .collect::<serde_json::map::Map<String, _>>();
            theme_map.insert("_source".to_owned(), source.into());
            let theme_json: serde_json::Value = theme_map.into();
            serde_json::to_string(&theme_json).unwrap() // XXX
        },
        Lines => {
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
    timers: Option<(time::Duration, time::Duration)>, plugins: &PluginLoader,
) -> Result<()> {
    let map = zsh_precmd_map(timers, plugins).await?;
    let map_str = zsh_map_string(&map);
    tokio::io::stdout().write_all(map_str.as_bytes()).await?;
    Ok(())
}

#[tokio::main]
async fn main() -> Result<()> {
    let subscriber = tracing_subscriber::fmt()
        .with_max_level(tracing::Level::TRACE)
        .finish();

    tracing::subscriber::set_global_default(subscriber)?;

    std::panic::set_hook(Box::new(|panic| {
        if let Some(location) = panic.location() {
            tracing::error!(
                message = %panic,
                panic.file = location.file(),
                panic.line = location.line(),
                panic.column = location.column(),
            );
        } else {
            tracing::error!(message = %panic);
        }
    }));

    let plugins = PluginLoader::new()
        .load_builtin_plugins()
        .load_plugins()
        .await?;

    let () = Handle::current()
        .spawn_blocking(move || blocking_main(&plugins))
        .await??;
    Ok(())
}

#[derive(Debug, Clone, Copy, ArgEnum)]
enum OutputFormat {
    Lines,
    Zsh,
    Json,
}

#[derive(Debug, Parser)]
#[clap(author, version, about)]
struct App {
    #[clap(subcommand)]
    command: Commands,
}

#[derive(Debug, Subcommand)]
enum Commands {
    /// Emit some information
    Emit(EmitCommand),
    /// Do everything a zsh precmd would need
    Precmd(PrecmdCommand),
    /// Hash a value into a 24-bit color theme
    ColorTheme(ColorThemeCommand),
    /// Proxy to a remote sshd in a standard-ish way
    SshProxy(SshProxyCommand),
    /// Install files from a manifest
    Install(InstallCommand),
}

#[derive(Debug, Args)]
struct EmitCommand {
    #[clap(short, long)]
    /// Don't emit a trailing newline
    no_newline: bool,

    #[clap(subcommand)]
    which: EmitWhich,
}

#[derive(Debug, Subcommand)]
enum EmitWhich {
    /// Describe version control status, if possible
    VcStatus,
    /// Count the number of files in the current directory
    FileCount,
    /// Find the branch associated with the VCS tip (git HEAD)
    HeadBranch,
}

#[derive(Debug, Args)]
struct PrecmdCommand {
    /// zsh timer data
    timers: Vec<u64>,
}

#[derive(Debug, Args)]
struct ColorThemeCommand {
    #[clap(short, long, arg_enum, default_value = "lines")]
    /// Set the output format
    format: OutputFormat,

    /// The color theme's seed string
    seed: Option<String>,
}

#[derive(Debug, Args)]
struct SshProxyCommand {
    #[clap(short = 'n', long)]
    dry_run: bool,

    /// Proxy target host
    host: String,
    /// Proxy target port
    port: String,
    /// Additional ssh args
    #[clap(raw = true, last = false)]
    ssh_args: Vec<OsString>,
}

#[derive(Debug, Args)]
struct InstallCommand {
    #[clap(short = 'n', long)]
    dry_run: bool,

    /// Manifest file
    manifest: PathBuf,
    /// Target directory base
    target: Option<PathBuf>,
}

fn blocking_main(plugins: &PluginLoader) -> Result<()> {
    let hnd = Handle::current();
    let app = App::parse();
    tracing::info!(?app, "app parsed");
    match app.command {
        Commands::Emit(cmd) => {
            use self::EmitWhich::*;
            let out = match cmd.which {
                VcStatus => hnd.block_on(plugins.vc_status()),
                FileCount => file_count(),
                HeadBranch => hnd.block_on(plugins.git_head_branch()),
            }?;
            actually_emit(out, cmd.no_newline)?;
        },
        Commands::Precmd(cmd) => {
            let timers = &cmd.timers;
            let durations = if timers.len() == 4 {
                let before = time::Duration::new(timers[0], timers[1] as u32);
                let after = time::Duration::new(timers[2], timers[3] as u32);
                Some((before, after))
            } else {
                None
            };
            hnd.block_on(zsh_precmd(durations, plugins))?;
        },
        Commands::ColorTheme(cmd) => {
            let the_string = cmd.seed.unwrap_or_else(default_theme_seed);
            let theme = make_theme(&the_string);
            let out = stringify_theme(theme, cmd.format, &the_string)?;
            actually_emit(out, true)?;
        },
        Commands::SshProxy(cmd) => {
            let mut args_iter = cmd.ssh_args.iter().map(|s| s.as_os_str());
            let ssh_command = ssh_proxy_command(
                &cmd.host,
                &cmd.port,
                &mut args_iter as &mut dyn Iterator<Item = _>,
            )?;
            if cmd.dry_run {
                actually_emit(format!("would run: {:?}\n", ssh_command), true)?;
            }
        },
        Commands::Install(mut cmd) => {
            let target = cmd.target.get_or_insert_with(|| {
                cmd.dry_run = true;
                "/".into()
            });
            install_from_manifest(cmd.dry_run, &cmd.manifest, &*target)
                .with_context(|| format!("whilst installing: {:#?}", cmd))?;
        },
    }
    Ok(())
}
