use sqlite_lexer as scan;

use crate::scan::sql::TokenType;
use crate::scan::sql::Tokenizer;
use crate::scan::Scanner;
use std::env;
use std::fs::File;
use std::i64;
use std::str;

/// Parse specified files and print all commands.
fn main() {
    let args = env::args();
    for arg in args.skip(1) {
        let f = File::open(arg.clone()).unwrap();
        let parser = Parser::new(f);
        loop {
            match parser.next() {
                Ok(None) => break,
                Err(err) => {
                    eprintln!("Err: {} in {}", err, arg);
                    break;
                }
                Ok(Some(cmd)) => {
                    println!("{}", cmd);
                }
            }
        }
    }
}
