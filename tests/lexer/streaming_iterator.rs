use sqlite_parser::lexer::sql::Tokenizer;
use sqlite_parser::lexer::Scanner;

fn main() {
    let tokenizer = Tokenizer::new();
    let input = "PRAGMA parser_trace=ON;".as_bytes();
    let mut s = Scanner::new(input, tokenizer);
    let (token1, _) = s.scan().unwrap().unwrap();
    s.scan().unwrap().unwrap();
    assert!(b"PRAGMA".eq_ignore_ascii_case(token1));
}