//! SQLite3 syntax lexer and parser
#![warn(missing_docs)]
#![warn(clippy::large_stack_frames)]

pub mod dialect;
// In Lemon, the tokenizer calls the parser.
pub mod lexer;
mod parser;
pub use parser::ast;
