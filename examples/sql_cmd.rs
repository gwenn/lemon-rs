use env_logger;
use fallible_iterator::FallibleIterator;
use sqlite_parser::lexer::sql::Parser;

/// Parse a string.
// RUST_LOG=sqlite3Parser=debug
fn main() {
    env_logger::init();
    let arg = "PRAGMA parser_trace=ON;";
    let mut parser = Parser::new(arg.as_bytes());
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
