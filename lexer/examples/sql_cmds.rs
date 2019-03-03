use fallible_iterator::FallibleIterator;
use std::env;
use std::fs::File;

use sqlite_lexer::sql::Parser;

/// Parse specified files and print all commands.
fn main() {
    let args = env::args();
    for arg in args.skip(1) {
        let f = File::open(arg.clone()).unwrap();
        let mut parser = Parser::new(f);
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
