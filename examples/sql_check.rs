use bumpalo::Bump;
use fallible_iterator::FallibleIterator;
use std::env;
use std::fs::read;
use std::panic;

use sqlite3_parser::lexer::sql::Parser;

/// Parse specified files and check all commands.
fn main() {
    env_logger::init();
    let args = env::args();
    for arg in args.skip(1) {
        println!("{arg}");
        let result = panic::catch_unwind(|| {
            let input = read(arg.clone()).unwrap();
            let bump = Bump::new();
            let mut parser = Parser::new(&bump, &input);
            loop {
                match parser.next() {
                    Ok(None) => break,
                    Err(err) => {
                        eprintln!("Err: {err} in {arg}");
                        break;
                    }
                    Ok(Some(cmd)) => {
                        let input = cmd.to_string();
                        let mut checker = Parser::new(&bump, input.as_bytes());
                        match checker.next() {
                            Err(err) => {
                                eprintln!(
                                    "Check Err in {}:{}, {} in\n{}\n{:?}",
                                    arg,
                                    parser.position(),
                                    err,
                                    input,
                                    cmd
                                );
                            }
                            Ok(None) => {
                                eprintln!("Check Err in {}:{}, {:?}", arg, parser.position(), cmd);
                            }
                            Ok(Some(check)) => {
                                if cmd != check {
                                    eprintln!("{cmd:?}\n<>\n{check:?}");
                                }
                            }
                        }
                    }
                }
            }
        });
        if let Err(e) = result {
            eprintln!("Panic: {e:?} in {arg}");
        }
    }
}
