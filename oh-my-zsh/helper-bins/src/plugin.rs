#[cfg(feature = "serde_macros")]
include!("plugin.rs.in");

#[cfg(not(feature = "serde_macros"))]
include!(concat!(env!("OUT_DIR"), "/plugin.rs"));
