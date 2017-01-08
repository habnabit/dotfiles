use std::io::{ErrorKind, BufRead, BufReader, Write, self};
use std::io::ErrorKind::NotFound;
use std::{env, fmt, mem, path, process};

use capnp;
use capnp::capability::Promise;
use capnp_rpc::RpcSystem;
use capnp_rpc::rpc_twoparty_capnp::Side;
use capnp_rpc::twoparty::VatNetwork;
use futures::{Future, future};
use tokio_core::reactor::Handle;
use tokio_process::CommandExt;

use super::plugins_capnp::{plugin, plugin_process, version_control_plugin};

// use byteorder::{ReadBytesExt, WriteBytesExt, LittleEndian};
// use serde::{Serialize, Deserialize};
// use serde_json;

use super::errors::{PromptErrors, PromptResult as Result};
use super::utils::OwnedMessage;
// pub use super::plugin_types::*;

// fn read_string32(reader: &mut BufRead) -> io::Result<Option<Vec<u8>>> {
//     let len = match reader.read_u32::<LittleEndian>() {
//         Ok(s) => s as usize,
//         Err(ref e) if e.kind() == ErrorKind::UnexpectedEof => return Ok(None),
//         Err(e) => return Err(e),
//     };
//     let mut buf = vec![0_u8; len];
//     try!(reader.read_exact(&mut buf[..]));
//     Ok(Some(buf))
// }

// fn write_string32(writer: &mut Write, bytes: &[u8]) -> io::Result<()> {
//     try!(writer.write_u32::<LittleEndian>(bytes.len() as u32));
//     try!(writer.write_all(bytes));
//     try!(writer.flush());
//     Ok(())
// }

// struct PluginIO<'a> {
//     input: &'a mut BufRead,
//     output: &'a mut Write,
// }

// impl<'a> PluginIO<'a> {
//     fn new(r: &'a mut BufRead, w: &'a mut Write) -> PluginIO<'a> {
//         PluginIO {
//             input: r,
//             output: w,
//         }
//     }

//     fn write_message<M: Serialize + fmt::Debug>(&mut self, m: &M) -> Result<()> {
//         let bytes = try!(serde_json::ser::to_vec(m));
//         try!(write_string32(self.output, &bytes[..]));
//         Ok(())
//     }

//     fn read_message<M: Deserialize>(&mut self) -> Result<Option<M>> {
//         let bytes_opt = try!(read_string32(self.input));
//         bytes_opt.map(|b| serde_json::de::from_slice(&b[..]))
//             .unwrap_or(Ok(None))
//             .map_err(Into::into)
//     }
// }

// pub struct PluginClient<'a>(PluginIO<'a>);

// impl<'a> PluginClient<'a> {
//     pub fn new(r: &'a mut BufRead, w: &'a mut Write) -> PluginClient<'a> {
//         PluginClient(PluginIO::new(r, w))
//     }

//     pub fn handle_request<F>(&mut self, f: &mut F) -> Result<bool>
//         where F: FnMut(PluginRequest) -> Result<PluginResponse>,
//     {
//         let request: PluginRequest = match try!(self.0.read_message()) {
//             Some(r) => r,
//             None => return Ok(false),
//         };
//         let response = try!(f(request));
//         try!(self.0.write_message(&response));
//         Ok(true)
//     }

//     pub fn serve_forever<F>(&mut self, mut f: F)
//         where F: FnMut(PluginRequest) -> Result<PluginResponse>,
//     {
//         loop {
//             match self.handle_request(&mut f) {
//                 Ok(true) => (),
//                 Ok(false) => break,
//                 Err(e) => {
//                     write!(io::stderr(), "error serving plugin request: {:?}\n", e).unwrap();
//                     process::exit(2);
//                 }
//             }
//         }
//     }
// }

// pub struct PluginServer<'a>(PluginIO<'a>);

// impl<'a> PluginServer<'a> {
//     pub fn new(r: &'a mut BufRead, w: &'a mut Write) -> PluginServer<'a> {
//         PluginServer(PluginIO::new(r, w))
//     }

//     pub fn issue_request(&mut self, req: &PluginRequest) -> Result<Option<PluginResponse>> {
//         try!(self.0.write_message(req));
//         self.0.read_message()
//     }
// }

// pub struct Plugin {
//     name: String,
//     process: process::Child,
//     stdin: process::ChildStdin,
//     stdout: BufReader<process::ChildStdout>,
//     meta: InitializeResponse,
//     running: bool,
// }

// impl Plugin {
//     fn new(path: path::PathBuf) -> Result<Plugin> {
//         let process = process::Command::new(&path)
//             .stdin(process::Stdio::piped())
//             .stdout(process::Stdio::piped())
//             .stderr(process::Stdio::inherit())
//             .spawn();
//         let mut process = try!(process);
//         let name = path.to_string_lossy().into_owned();
//         Ok(Plugin {
//             name: name,
//             stdin: process.stdin.take().unwrap(),
//             stdout: BufReader::new(process.stdout.take().unwrap()),
//             process: process,
//             meta: Default::default(),
//             running: true,
//         })
//     }

//     fn reap(&mut self) {
//         ((move || {
//             self.running = false;
//             try!(self.process.kill());
//             try!(self.process.wait());
//             Ok(())
//         })() as Result<()>).expect("couldn't reap child")
//     }

//     fn issue_request(&mut self, req: &PluginRequest) -> Result<Option<PluginResponse>> {
//         if !self.running {
//             return Ok(None)
//         }
//         let resp = try!(PluginServer::new(&mut self.stdout, &mut self.stdin).issue_request(req));
//         if resp.is_none() {
//             self.reap()
//         }
//         Ok(resp)
//     }

//     fn initialize(&mut self) -> Result<()> {
//         let req = InitializeRequest::default();
//         if let Some(PluginResponse::Initialize(resp)) = try!(self.issue_request(&req.into())) {
//             mem::replace(&mut self.meta, resp);
//         }
//         Ok(())
//     }
// }

// impl Drop for Plugin {
//     fn drop(&mut self) {
//         self.reap()
//     }
// }

// impl fmt::Debug for Plugin {
//     fn fmt(&self, f: &mut fmt::Formatter) -> ::std::result::Result<(), fmt::Error> {
//         write!(f, "Plugin({:?}, @{}, meta: {:?})", self.name, self.process.id(), self.meta)
//     }
// }

fn extract_plugins(resp: &::capnp::capability::Response<plugin_process::initialize_results::Owned>) -> Result<Vec<OwnedMessage<plugin::Owned>>> {
    try!(try!(resp.get()).get_plugins())
        .iter()
        .map(|p| OwnedMessage::new_default(p.clone()))
        .collect::<::std::result::Result<_, ::capnp::Error>>()
        .map_err(Into::into)
}

fn load_plugin(handle: Handle, path: &path::Path) -> Box<Future<Item=Vec<OwnedMessage<plugin::Owned>>, Error=PromptErrors>> {
    let child = process::Command::new(path)
        .stdin(process::Stdio::piped())
        .stdout(process::Stdio::piped())
        .stderr(process::Stdio::inherit())
        .spawn_async(&handle);
    let ret = future::done(child)
        .map_err(Into::into)
        .and_then(|mut c| {
            let network = VatNetwork::new(
                c.stdout().take().unwrap(),
                c.stdin().take().unwrap(),
                Side::Client,
                Default::default());
            let mut rpc = RpcSystem::new(Box::new(network), None);
            let plugin: plugin_process::Client = rpc.bootstrap(Side::Server);
            plugin.initialize_request().send().promise
                .map_err(Into::into)
                .and_then(|resp| {
                    future::done(extract_plugins(&resp))
                })
        });
    Box::new(ret)
}

fn vc_plugin(p: &OwnedMessage<plugin::Owned>) -> Option<plugin::version_control::Reader> {
    use super::plugins_capnp::plugin::Which::*;
    match p.get_root_as_reader().which() {
        Ok(VersionControl(p)) => Some(p),
        _ => None,
    }
}

pub struct VcStatus {
    pub vc_name: String,
    pub results: OwnedMessage<version_control_plugin::status_results::Owned>,
}

pub struct PluginLoader {
    plugins: Vec<OwnedMessage<plugin::Owned>>,
}

impl PluginLoader {
    pub fn new() -> PluginLoader {
        PluginLoader {
            plugins: vec![],
        }
    }

    pub fn loaded(handle: Handle) -> Box<Future<Item=Self, Error=PromptErrors>> {
        Self::new().load_plugins(handle)
    }

    fn plugin_dir(&self) -> Option<path::PathBuf> {
        if let Ok(d) = env::var("HAB_PROMPT_PLUGIN_DIR") {
            return Some(d.into());
        }
        let mut plugin_dir = match env::home_dir() {
            Some(h) => h,
            None => return None,
        };
        plugin_dir.extend(&[".config", "hab-prompt", "plugins"]);
        Some(plugin_dir)
    }

    pub fn load_plugins(mut self, handle: Handle) -> Box<Future<Item=Self, Error=PromptErrors>> {
        let plugin_dir = match self.plugin_dir() {
            Some(d) => d,
            None => return Box::new(future::ok(self)),
        };
        let futures = match plugin_dir.read_dir() {
            Ok(r) => r,
            Err(ref e) if e.kind() == NotFound => return Box::new(future::ok(self)),
            Err(e) => return Box::new(future::err(e.into())),
        }.map(move |file| {
            let handle_ = handle.clone();
            future::done(file)
                .map_err(Into::into)
                .and_then(move |d| load_plugin(handle_, &d.path()))
        });
        let ret = future::join_all(futures)
            .map(move |plugins| {
                self.plugins.extend(plugins.into_iter().flat_map(|v| v));
                self
            });
        Box::new(ret)
    }

    pub fn test_vc_dir(self, path: &path::Path) -> Box<Future<Item=(Self, Option<VcStatus>), Error=PromptErrors>>
    {
        let path = path.to_string_lossy();
        let futures: ::std::result::Result<Vec<_>, ::capnp::Error> = self.plugins.iter()
            .filter_map(vc_plugin)
            .map(|p| {
                let vc_name: String = try!(p.get_vc_name()).into();
                let mut req = try!(p.get_plugin()).status_request();
                req.get().set_directory(&path);
                Ok(req.send().promise.map(move |r| (r, vc_name)))
            })
            .collect();
        let f = future::done(futures)
            .and_then(future::select_ok)
            .map(|(i, _)| i)
            .and_then(|(r, vc_name)| {
                let results = OwnedMessage::new_default(r.get().unwrap()).unwrap();
                Ok((self, Some(VcStatus {
                    vc_name: vc_name,
                    results: results,
                })))
            })
            .map_err(Into::into);
        Box::new(f)
    }
}
