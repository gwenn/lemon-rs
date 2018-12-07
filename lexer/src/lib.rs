#![feature(read_initializer)]

extern crate dialect;
extern crate parser;

mod scan;
pub mod sql;

pub use crate::scan::{ScanError, Scanner, Splitter};
