extern crate capnpc;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    if std::env::var("PROFILE")? == "debug" {
        ::capnpc::CompilerCommand::new().file("plugins.capnp").run()?;
    }
    Ok(())
}
