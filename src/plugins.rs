use std::io::ErrorKind::NotFound;
use std::{env, path, process};

use capnp_rpc::RpcSystem;
use capnp_rpc::rpc_twoparty_capnp::Side;
use capnp_rpc::twoparty::VatNetwork;
use futures::{Future, Sink, Stream, future};
use futures::sync::{mpsc, oneshot};
use tokio_core::reactor::Handle;
use tokio_process::CommandExt;
use tokio_service;

use super::plugins_capnp::{plugin, plugin_process, version_control_plugin};

use super::errors::{PromptErrors, PromptResult as Result};
use super::utils::OwnedMessage;
use super::vc::{Git, Hg};

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

fn builtin_vc_plugin<T: 'static>(name: &str, server: T) -> OwnedMessage<plugin::Owned>
    where T: version_control_plugin::Server,
{
    let mut ret = OwnedMessage::<plugin::Owned>::init_default();
    {
        let root = ret.get_root();
        let mut vc = root.init_version_control();
        vc.set_vc_name(name);
        vc.set_plugin(version_control_plugin::ToClient::new(server).from_server::<::capnp_rpc::Server>());
    }
    ret
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
    pub results: OwnedMessage<version_control_plugin::status::Owned>,
}

pub struct PluginLoader {
    handle: Handle,
    plugins: Vec<OwnedMessage<plugin::Owned>>,
}

impl PluginLoader {
    pub fn new(handle: Handle) -> PluginLoader {
        PluginLoader {
            handle: handle,
            plugins: vec![],
        }
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

    pub fn load_builtin_plugins(mut self) -> Self {
        self.plugins.push(builtin_vc_plugin("git", Git(self.handle.clone())));
        self.plugins.push(builtin_vc_plugin("hg", Hg(self.handle.clone())));
        self
    }

    pub fn load_plugins(mut self) -> Box<Future<Item=Self, Error=PromptErrors>> {
        let plugin_dir = match self.plugin_dir() {
            Some(d) => d,
            None => return Box::new(future::ok(self)),
        };
        let handle = self.handle.clone();
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

    pub fn test_vc_dir(&self, path: &path::Path) -> Box<Future<Item=Option<VcStatus>, Error=PromptErrors>>
    {
        if self.plugins.is_empty() {
            return Box::new(future::ok(None));
        }
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
                use super::plugins_capnp::option::Which;
                match try!(try!(try!(r.get()).get_status()).which()) {
                    Which::None(()) => Ok(None),
                    Which::Some(r) => {
                        let results = try!(OwnedMessage::new_default(try!(r)));
                        Ok(Some(VcStatus {
                            vc_name: vc_name,
                            results: results,
                        }))
                    },
                }
            })
            .map_err(Into::into);
        Box::new(f)
    }

    pub fn spawn(self) -> TestVcDirService {
        let (tx, rx) = mpsc::channel(self.plugins.len());
        let handle = self.handle.clone();
        let fut = rx.and_then(move |(req, resp_tx)| {
            TestVcDir::request(&self, req).map(move |resp| {
                (resp_tx as oneshot::Sender<Option<VcStatus>>).complete(resp);
            }).map_err(|_| ())
        }).for_each(|_| Ok(()));
        handle.spawn(fut);

        PluginRequestHandle {
            sender: tx,
        }

    }
}

pub trait PluginService {
    type Request: 'static;
    type Response: 'static;

    fn request(loader: &PluginLoader, req: Self::Request) -> Box<Future<Item=Self::Response, Error=PromptErrors>>;
}

pub enum TestVcDir {}
impl PluginService for TestVcDir {
    type Request = path::PathBuf;
    type Response = Option<VcStatus>;

    fn request(loader: &PluginLoader, req: path::PathBuf) -> Box<Future<Item=Option<VcStatus>, Error=PromptErrors>>
    {
        loader.test_vc_dir(&req)
    }
}

pub struct PluginRequestHandle<T: PluginService> {
    sender: mpsc::Sender<(T::Request, oneshot::Sender<T::Response>)>,
}

impl<T> Clone for PluginRequestHandle<T>
    where T: PluginService,
{
    fn clone(&self) -> PluginRequestHandle<T> {
        PluginRequestHandle {
            sender: self.sender.clone(),
        }
    }
}

impl<T> tokio_service::Service for PluginRequestHandle<T>
    where T: PluginService,
{
    type Request = T::Request;
    type Response = T::Response;
    type Error = PromptErrors;
    type Future = Box<Future<Item=T::Response, Error=PromptErrors>>;

    fn call(&mut self, req: T::Request) -> Box<Future<Item=T::Response, Error=PromptErrors>> {
        let req_tx = self.sender.clone();
        let (resp_tx, resp_rx) = oneshot::channel();
        let ret = req_tx.send((req, resp_tx))
            .map_err(|_| unimplemented!())
            .and_then(move |_| resp_rx.map_err(Into::into));
        Box::new(ret)
    }
}

pub type TestVcDirService = PluginRequestHandle<TestVcDir>;
