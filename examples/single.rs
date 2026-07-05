use sqlite3_parser::{Bump, FallibleIterator as _, lexer::sql::Parser};

// RUST_LOG=sqlite3Parser=debug
fn main() {
    env_logger::init();
    let bump = Bump::new();
    let mut parser = Parser::new(&bump, b"SELECT 1");
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
