//! Streaming SQLite tokenizer

mod scan;
pub mod sql;

#[cfg(feature = "buf_redux")]
pub use scan::InputStream;
pub use scan::{Input, ScanError, Scanner, Splitter};
