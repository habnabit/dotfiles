use std::collections::BTreeMap;
use std::marker::PhantomData;

use futures::{Future, Stream, future, self};
use futures::sync::{mpsc, oneshot};
use tokio_core::reactor::Handle;
use tokio_service;

use super::errors::PromptResult as Result;
use super::plugins_capnp::file_counts;

const ITERATION_LIMIT: usize = 1000;

pub fn format_counts(order: &str, counts: &BTreeMap<char, usize>, truncated: bool, show_total: bool) -> String {
    use std::fmt::Write;
    let mut sorted: Vec<_> = counts
        .into_iter()
        .filter_map(|(&c, &count)| order.find(c).map(|pos| (pos, c, count)))
        .collect();
    sorted.sort();
    let mut ret = String::new();
    for &(_, c, count) in &sorted {
        write!(ret, "{}{} ", count, c).unwrap();
    }
    if show_total {
        let total = counts.values().fold(0, |acc, &x| acc + x);
        write!(ret, "≡{} ", total).unwrap();
    }
    if ret.pop().is_some() && truncated {
        ret.push('…');
    }
    ret
}

pub fn btree_of_counts(counts: &file_counts::Reader) -> Result<BTreeMap<char, usize>> {
    let mut btree = BTreeMap::new();
    for entry in try!(counts.get_entries()).iter() {
        let mut chars = try!(entry.get_file_type()).chars();
        // XXX error handling?
        let c = chars.next().unwrap();
        *btree.entry(c).or_insert(0) += entry.get_count() as usize;
        assert!(chars.next() == None);
    }
    Ok(btree)
}

pub fn limited_foreach<I, F>(iter: I, mut func: F) -> Result<bool>
    where I: IntoIterator,
          F: FnMut(I::Item) -> Result<()>,
{
    for (e, item) in iter.into_iter().enumerate() {
        if e >= ITERATION_LIMIT {
            return Ok(true);
        }
        try!(func(item));
    }
    Ok(false)
}

pub trait IncrementalMap<T> {
    fn increment(&mut self, key: T);
}

impl<T: Ord> IncrementalMap<T> for BTreeMap<T, usize> {
    fn increment(&mut self, key: T) {
        *self.entry(key).or_insert(0) += 1;
    }
}

pub struct OwnedMessage<T: for<'a> ::capnp::traits::Owned<'a>,
                        A: ::capnp::message::Allocator=::capnp::message::HeapAllocator>
{
    message: ::capnp::message::Builder<A>,
    phantom: ::std::marker::PhantomData<T>,
}

impl<T, A> OwnedMessage<T, A>
    where T: for<'a> ::capnp::traits::Owned<'a>,
          A: ::capnp::message::Allocator
{
    pub fn new<'b>(alloc: A, r: <T as ::capnp::traits::Owned<'b>>::Reader) -> ::std::result::Result<OwnedMessage<T, A>, ::capnp::Error> {
        let mut message = ::capnp::message::Builder::new(alloc);
        try!(message.set_root(r));
        Ok(OwnedMessage {
            message: message,
            phantom: ::std::marker::PhantomData,
        })
    }

    pub fn get_root_as_reader<'b>(&'b self) -> <T as ::capnp::traits::Owned<'b>>::Reader {
        self.message.get_root_as_reader().expect("???")
    }
}

impl<T> OwnedMessage<T, ::capnp::message::HeapAllocator>
    where T: for<'a> ::capnp::traits::Owned<'a>,
{
    pub fn new_default<'b>(r: <T as ::capnp::traits::Owned<'b>>::Reader) -> ::std::result::Result<OwnedMessage<T, ::capnp::message::HeapAllocator>, ::capnp::Error> {
        let mut message = ::capnp::message::Builder::new_default();
        try!(message.set_root(r));
        Ok(OwnedMessage {
            message: message,
            phantom: ::std::marker::PhantomData,
        })
    }
}

pub struct SerialService<I: 'static, O: 'static, E: 'static>
    where E: From<futures::Canceled>,
{
    sender: mpsc::UnboundedSender<(I, oneshot::Sender<O>)>,
    _phantom: PhantomData<E>,
}

impl<I: 'static, O: 'static, E: 'static> Clone for SerialService<I, O, E>
    where E: From<futures::Canceled>,
{
    fn clone(&self) -> SerialService<I, O, E> {
        SerialService {
            sender: self.sender.clone(),
            _phantom: PhantomData,
        }
    }
}

impl<I: 'static, O: 'static, E: 'static> SerialService<I, O, E>
    where E: From<futures::Canceled>,
{
    pub fn new<S: 'static, F: 'static>(handle: &Handle, state: S, mut func: F) -> Self
        where F: FnMut(S, I) -> Box<Future<Item=(S, O), Error=E>>,
    {
        let (tx, rx) = mpsc::unbounded();
        let fut = rx.fold(state, move |state, (i, resp_tx)| {
            func(state, i).map(move |(s, o)| {
                (resp_tx as oneshot::Sender<O>).complete(o);
                s
            }).map_err(|_| ())
        }).map(|_| ());
        handle.spawn(fut);

        SerialService {
            sender: tx,
            _phantom: PhantomData,
        }
    }
}

impl<I: 'static, O: 'static, E: 'static> tokio_service::Service for SerialService<I, O, E>
    where E: From<futures::Canceled>,
{
    type Request = I;
    type Response = O;
    type Error = E;
    type Future = Box<Future<Item=O, Error=E>>;

    fn call(&mut self, req: I) -> Box<Future<Item=O, Error=E>> {
        let mut req_tx = self.sender.clone();
        let (resp_tx, resp_rx) = oneshot::channel();
        req_tx.send((req, resp_tx)).unwrap();
        Box::new(resp_rx.map_err(Into::into))
    }
}
