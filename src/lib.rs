#![cfg_attr(feature = "serde_macros", feature(custom_derive, plugin))]
#![cfg_attr(feature = "serde_macros", plugin(serde_macros))]

extern crate byteorder;
extern crate serde;
extern crate serde_json;

use std::io::{BufRead, Write, self};

use byteorder::{ReadBytesExt, WriteBytesExt, LittleEndian};
use serde::{Serialize, Deserialize};

pub mod error;
pub mod plugin;

use self::error::PromptResult as Result;
use self::plugin::{PluginRequest, PluginResponse};

fn read_string32(reader: &mut BufRead) -> io::Result<Vec<u8>> {
    let len = try!(reader.read_u32::<LittleEndian>()) as usize;
    let mut buf = vec![0_u8; len];
    try!(reader.read_exact(&mut buf[..]));
    Ok(buf)
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

    fn write_message<M: Serialize + std::fmt::Debug>(&mut self, m: &M) -> Result<()> {
        let bytes = try!(serde_json::ser::to_vec(m));
        try!(write_string32(self.output, &bytes[..]));
        Ok(())
    }

    fn read_message<M: Deserialize>(&mut self) -> Result<M> {
        let bytes = try!(read_string32(self.input));
        Ok(try!(serde_json::de::from_slice(&bytes[..])))
    }
}

pub struct PluginClient<'a>(PluginIO<'a>);

impl<'a> PluginClient<'a> {
    pub fn new(r: &'a mut BufRead, w: &'a mut Write) -> PluginClient<'a> {
        PluginClient(PluginIO::new(r, w))
    }

    pub fn handle_request<F>(&mut self, f: &mut F) -> Result<()>
        where F: FnMut(PluginRequest) -> Result<PluginResponse>,
    {
        let request: PluginRequest = try!(self.0.read_message());
        let response = try!(f(request));
        try!(self.0.write_message(&response));
        Ok(())
    }

    pub fn serve_forever<F>(&mut self, mut f: F)
        where F: FnMut(PluginRequest) -> Result<PluginResponse>,
    {
        loop {
            match self.handle_request(&mut f) {
                Ok(()) => (),
                Err(e) => {
                    println!("error serving plugin request: {:?}", e);
                    std::process::exit(2);
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

    pub fn issue_request(&mut self, req: &PluginRequest) -> Result<PluginResponse> {
        try!(self.0.write_message(req));
        self.0.read_message()
    }
}
