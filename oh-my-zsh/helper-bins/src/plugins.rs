use std::io::{ErrorKind, BufRead, BufReader, Write, self};
use std::io::ErrorKind::NotFound;
use std::{env, fmt, mem, path, process};

use byteorder::{ReadBytesExt, WriteBytesExt, LittleEndian};
use serde::{Serialize, Deserialize};
use serde_json;

use super::errors::PromptResult as Result;
pub use super::plugin_types::*;

fn read_string32(reader: &mut BufRead) -> io::Result<Option<Vec<u8>>> {
    let len = match reader.read_u32::<LittleEndian>() {
        Ok(s) => s as usize,
        Err(ref e) if e.kind() == ErrorKind::UnexpectedEof => return Ok(None),
        Err(e) => return Err(e),
    };
    let mut buf = vec![0_u8; len];
    try!(reader.read_exact(&mut buf[..]));
    Ok(Some(buf))
}

fn write_string32(writer: &mut Write, bytes: &[u8]) -> io::Result<()> {
    try!(writer.write_u32::<LittleEndian>(bytes.len() as u32));
    try!(writer.write_all(bytes));
    try!(writer.flush());
    Ok(())
}

struct PluginIO<'a> {
    input: &'a mut BufRead,
    output: &'a mut Write,
}

impl<'a> PluginIO<'a> {
    fn new(r: &'a mut BufRead, w: &'a mut Write) -> PluginIO<'a> {
        PluginIO {
            input: r,
            output: w,
        }
    }

    fn write_message<M: Serialize + fmt::Debug>(&mut self, m: &M) -> Result<()> {
        let bytes = try!(serde_json::ser::to_vec(m));
        try!(write_string32(self.output, &bytes[..]));
        Ok(())
    }

    fn read_message<M: Deserialize>(&mut self) -> Result<Option<M>> {
        let bytes_opt = try!(read_string32(self.input));
        bytes_opt.map(|b| serde_json::de::from_slice(&b[..]))
            .unwrap_or(Ok(None))
            .map_err(Into::into)
    }
}

pub struct PluginClient<'a>(PluginIO<'a>);

impl<'a> PluginClient<'a> {
    pub fn new(r: &'a mut BufRead, w: &'a mut Write) -> PluginClient<'a> {
        PluginClient(PluginIO::new(r, w))
    }

    pub fn handle_request<F>(&mut self, f: &mut F) -> Result<bool>
        where F: FnMut(PluginRequest) -> Result<PluginResponse>,
    {
        let request: PluginRequest = match try!(self.0.read_message()) {
            Some(r) => r,
            None => return Ok(false),
        };
        let response = try!(f(request));
        try!(self.0.write_message(&response));
        Ok(true)
    }

    pub fn serve_forever<F>(&mut self, mut f: F)
        where F: FnMut(PluginRequest) -> Result<PluginResponse>,
    {
        loop {
            match self.handle_request(&mut f) {
                Ok(true) => (),
                Ok(false) => break,
                Err(e) => {
                    write!(io::stderr(), "error serving plugin request: {:?}\n", e).unwrap();
                    process::exit(2);
                }
            }
        }
    }
}

pub struct PluginServer<'a>(PluginIO<'a>);

impl<'a> PluginServer<'a> {
    pub fn new(r: &'a mut BufRead, w: &'a mut Write) -> PluginServer<'a> {
        PluginServer(PluginIO::new(r, w))
    }

    pub fn issue_request(&mut self, req: &PluginRequest) -> Result<Option<PluginResponse>> {
        try!(self.0.write_message(req));
        self.0.read_message()
    }
}

pub struct Plugin {
    name: String,
    process: process::Child,
    stdin: process::ChildStdin,
    stdout: BufReader<process::ChildStdout>,
    meta: InitializeResponse,
    running: bool,
}

impl Plugin {
    fn new(path: path::PathBuf) -> Result<Plugin> {
        let process = process::Command::new(&path)
            .stdin(process::Stdio::piped())
            .stdout(process::Stdio::piped())
            .stderr(process::Stdio::inherit())
            .spawn();
        let mut process = try!(process);
        let name = path.to_string_lossy().into_owned();
        Ok(Plugin {
            name: name,
            stdin: process.stdin.take().unwrap(),
            stdout: BufReader::new(process.stdout.take().unwrap()),
            process: process,
            meta: Default::default(),
            running: true,
        })
    }

    fn reap(&mut self) {
        ((move || {
            self.running = false;
            try!(self.process.kill());
            try!(self.process.wait());
            Ok(())
        })() as Result<()>).expect("couldn't reap child")
    }

    fn issue_request(&mut self, req: &PluginRequest) -> Result<Option<PluginResponse>> {
        if !self.running {
            return Ok(None)
        }
        let resp = try!(PluginServer::new(&mut self.stdout, &mut self.stdin).issue_request(req));
        if resp.is_none() {
            self.reap()
        }
        Ok(resp)
    }

    fn initialize(&mut self) -> Result<()> {
        let req = InitializeRequest::default();
        if let Some(PluginResponse::Initialize(resp)) = try!(self.issue_request(&req.into())) {
            mem::replace(&mut self.meta, resp);
        }
        Ok(())
    }
}

impl Drop for Plugin {
    fn drop(&mut self) {
        self.reap()
    }
}

impl fmt::Debug for Plugin {
    fn fmt(&self, f: &mut fmt::Formatter) -> ::std::result::Result<(), fmt::Error> {
        write!(f, "Plugin({:?}, @{}, meta: {:?})", self.name, self.process.id(), self.meta)
    }
}


#[derive(Debug)]
pub struct PluginLoader {
    plugins: Vec<Plugin>,
}

impl PluginLoader {
    pub fn new() -> PluginLoader {
        PluginLoader {
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

    pub fn load_plugins(&mut self) -> Result<()> {
        let plugin_dir = match self.plugin_dir() {
            Some(d) => d,
            None => return Ok(()),
        };
        for file in match plugin_dir.read_dir() {
            Ok(r) => r,
            Err(ref e) if e.kind() == NotFound => return Ok(()),
            Err(e) => return Err(e.into()),
        } {
            let file = try!(file).path();
            self.plugins.push(try!(Plugin::new(file)));
        }
        for plugin in &mut self.plugins {
            try!(plugin.initialize());
        }
        Ok(())
    }

    pub fn test_vc_dir(&mut self, path: &path::Path) -> Result<Option<VcStatusResponse>> {
        let mut req = VcStatusRequest::default();
        req.cwd = path.to_string_lossy().into_owned();
        let req = req.into();
        for plugin in &mut self.plugins {
            if !plugin.meta.handles_vc {
                continue
            }
            if let Some(PluginResponse::VcStatus(resp)) = try!(plugin.issue_request(&req)) {
                return Ok(Some(resp));
            }
        }
        Ok(None)
    }
}
