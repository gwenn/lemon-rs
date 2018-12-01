#![feature(read_initializer)]

#[macro_use]
extern crate log;
extern crate memchr;
extern crate sqlite_dialect as dialect;
extern crate sqlite_parser as parser;

mod scan;
pub mod sql;

pub use scan::{ScanError, Scanner, Splitter};
