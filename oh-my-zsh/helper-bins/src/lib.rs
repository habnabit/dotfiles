#![cfg_attr(test, feature(plugin))]
#![cfg_attr(test, plugin(fnconcat))]

#[macro_use] extern crate lazy_static;
extern crate ansi_term;
extern crate byteorder;
extern crate capnp;
extern crate capnp_rpc;
extern crate clap;
extern crate futures;
extern crate hsl;
extern crate rand;
extern crate regex;
extern crate sha1;
extern crate tempfile;
extern crate tokio_core;
extern crate tokio_process;
extern crate tokio_service;

pub mod plugins_capnp {
    include!(concat!(env!("OUT_DIR"), "/plugins_capnp.rs"));
}

mod term;
pub mod colors;
pub mod directories;
pub mod durations;
pub mod errors;
pub mod installer;
pub mod plugins;
pub mod ssh_proxy;
pub mod utils;
pub mod vc;
