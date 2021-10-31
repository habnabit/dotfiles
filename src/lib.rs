#![allow(unused_parens)]
#![cfg_attr(test, feature(plugin))]
#![cfg_attr(test, plugin(fnconcat))]

#[macro_use]
extern crate lazy_static;
extern crate ansi_term;
extern crate blake2;
extern crate byteorder;
extern crate capnp;
extern crate capnp_rpc;
extern crate clap;
extern crate dirs;
extern crate futures;
extern crate hsl;
extern crate rand;
extern crate rand_chacha;
extern crate regex;
extern crate tempfile;
extern crate tokio_core;
extern crate tokio_process;
extern crate tokio_service;
extern crate whoami;

#[cfg(debug_assertions)]
pub mod plugins_capnp {
    include!(concat!(env!("OUT_DIR"), "/plugins_capnp.rs"));
}

#[cfg(not(debug_assertions))]
pub mod _plugins_capnp;
#[cfg(not(debug_assertions))]
pub use _plugins_capnp as plugins_capnp;

pub mod colors;
pub mod directories;
pub mod durations;
pub mod errors;
pub mod installer;
pub mod plugins;
pub mod ssh_proxy;
mod term;
pub mod utils;
pub mod vc;
