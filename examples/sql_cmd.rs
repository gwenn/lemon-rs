use std::{
    env,
    io::{stdin, IsTerminal, Read},
};

use bumpalo::Bump;
use fallible_iterator::FallibleIterator;
use sqlite3_parser::lexer::sql::Parser;

/// Parse args.
// RUST_LOG=sqlite3Parser=debug
fn main() {
    env_logger::init();
    let args = env::args();
    if args.len() == 1 && !stdin().is_terminal() {
        let mut str = String::with_capacity(1024);
        stdin().read_to_string(&mut str).unwrap();
        parse(str);
    }
    for arg in args.skip(1) {
        parse(arg);
    }
}

fn parse(arg: String) {
    let bump = Bump::new();
    let mut parser = Parser::new(&bump, arg.as_bytes());
    loop {
        match parser.next() {
            Ok(None) => break,
            Err(err) => {
                eprintln!("Err: {err} in {arg}");
                break;
            }
            Ok(Some(cmd)) => {
                println!("{cmd}");
            }
        }
    }
}
