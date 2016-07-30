#![cfg_attr(feature = "serde_macros", feature(custom_derive, plugin))]
#![cfg_attr(feature = "serde_macros", plugin(serde_macros))]
#![cfg_attr(test, feature(plugin))]
#![cfg_attr(test, plugin(fnconcat))]

extern crate byteorder;
#[macro_use]
extern crate clap;
#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate serde;
extern crate serde_json;
extern crate sha1;
extern crate tempfile;

mod plugin_types;
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
