//! Streaming SQLite tokenizer

mod scan;
pub mod sql;

pub use scan::{Input, InputStream, ScanError, Scanner, Splitter};
