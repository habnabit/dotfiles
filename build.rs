extern crate capnpc;

fn main() {
    ::capnpc::CompilerCommand::new().file("plugins.capnp").run().unwrap();
}
