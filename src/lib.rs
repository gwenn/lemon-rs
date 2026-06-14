//! SQLite3 syntax lexer and parser
#![warn(missing_docs)]
//#![warn(clippy::large_stack_frames)] doesn't take into account opt-level / --release flag

pub use bumpalo::{collections::Vec, Bump};
pub use fallible_iterator::FallibleIterator;
pub mod dialect;
// In Lemon, the tokenizer calls the parser.
pub mod lexer;
mod parser;
pub use parser::ast;
