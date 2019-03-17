//! Streaming SQLite tokenizer
#![feature(read_initializer)]

use dialect;

mod scan;
pub mod sql;

pub use crate::scan::{ScanError, Scanner, Splitter};
