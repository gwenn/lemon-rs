use fallible_iterator::FallibleIterator;
use sqlite3_parser::lexer::sql::Parser;

// RUST_LOG=sqlite3Parser=debug
fn main() {
    env_logger::init();
    let mut parser = Parser::new(b"SELECT 1");
    loop {
        match parser.next() {
            Ok(None) => break,
            Err(err) => {
                eprintln!("Err: {err}");
                break;
            }
            Ok(Some(cmd)) => {
                println!("{cmd}");
            }
        }
    }
}
