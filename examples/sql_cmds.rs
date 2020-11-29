use fallible_iterator::FallibleIterator;
use std::env;
use std::fs::File;
use std::panic;

use sqlite_parser::lexer::sql::Parser;
use sqlite_parser::lexer::InputStream;

/// Parse specified files and print all commands.
fn main() {
    env_logger::init();
    let args = env::args();
    for arg in args.skip(1) {
        println!("{}", arg);
        let result = panic::catch_unwind(|| {
            let f = File::open(arg.clone()).unwrap();
            let input = InputStream::new(f);
            let mut parser = Parser::new(input);
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
        });
        if let Err(e) = result {
            eprintln!("Panic: {:?} in {}", e, arg);
        }
    }
}
