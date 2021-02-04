use fallible_iterator::FallibleIterator;
use std::env;
use std::fs::File;
use std::panic;

use sqlite3_parser::lexer::sql::Parser;
use sqlite3_parser::lexer::InputStream;

/// Parse specified files and check all commands.
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
                        let input = cmd.to_string();
                        let mut checker = Parser::new(input.as_bytes());
                        match checker.next() {
                            Err(err) => {
                                eprintln!(
                                    "Check Err in {}:{}, {} in\n{}\n{:?}",
                                    arg,
                                    parser.line(),
                                    err,
                                    input,
                                    cmd
                                );
                            }
                            Ok(None) => {
                                eprintln!("Check Err in {}:{}, {:?}", arg, parser.line(), cmd);
                            }
                            Ok(Some(check)) => {
                                if cmd != check {
                                    eprintln!("{:?}\n<>\n{:?}", cmd, check);
                                }
                            }
                        }
                    }
                }
            }
        });
        if let Err(e) = result {
            eprintln!("Panic: {:?} in {}", e, arg);
        }
    }
}
