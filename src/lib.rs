//! SQLite3 syntax lexer and parser
#![warn(missing_docs)]
#![forbid(unsafe_code)]

pub use bumpalo::Bump;
pub use bumpalo::collections::Vec;
pub use fallible_iterator::FallibleIterator;

pub mod dialect;
// In Lemon, the tokenizer calls the parser.
pub mod lexer;
mod parser;
pub use parser::ast;
