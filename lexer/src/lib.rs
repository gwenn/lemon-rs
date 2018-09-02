#![feature(read_initializer)]
#![feature(plugin)]
#![plugin(phf_macros)]

#[macro_use]
extern crate log;
extern crate memchr;
extern crate phf;

mod scan;
pub mod sql;

pub use scan::{ScanError, Scanner, Splitter};
