#![feature(read_initializer)]
pub mod dialect;
// In Lemon, the tokenizer calls the parser.
pub mod lexer;
mod parser;
