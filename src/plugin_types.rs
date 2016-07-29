#[cfg(feature = "serde_macros")]
include!("plugin_types.rs.in");

#[cfg(not(feature = "serde_macros"))]
include!(concat!(env!("OUT_DIR"), "/plugin_types.rs"));
