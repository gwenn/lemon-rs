%token_type { i32 }

// The generated parser function takes a 4th argument as follows:
%extra_argument {ctx: Context}

%left PLUS MINUS.
%left DIVIDE TIMES.

%include {
#[macro_use]
extern crate log;
extern crate smallvec;

use std::io::{self, Write};
use log::{LogLevel, LogLevelFilter, LogMetadata, LogRecord, SetLoggerError};

pub struct Context {
    expr: Option<Expr>,
}

#[derive(Debug)]
pub enum Operator {
    Add,
    Substract,
    Multiply,
    Divide,
}

#[derive(Debug)]
pub enum Expr {
    Number(i32),
    Binary(Operator, Box<Expr>, Box<Expr>),
}
impl Expr {
    fn binary(op: Operator, lhs: Expr, rhs: Expr) -> Expr {
        Expr::Binary(op, Box::new(lhs), Box::new(rhs))
    }
}

fn main() {
    init_logger().is_ok();

    let r = Context { expr: None };
    let mut p = yyParser::new(r);
    p.Parse(TokenType::INTEGER as YYCODETYPE, 5);
    p.Parse(TokenType::PLUS as YYCODETYPE, 0);
    p.Parse(TokenType::INTEGER as YYCODETYPE, 10);
    p.Parse(TokenType::TIMES as YYCODETYPE, 0);
    p.Parse(TokenType::INTEGER as YYCODETYPE, 4);
    p.Parse(TokenType::EOF as YYCODETYPE, 0);
    p.ParseFinalize();
    let s = format!("{:?}", p.ctx.expr);
    assert_eq!(s, "Some(Binary(Add, Number(5), Binary(Multiply, Number(10), Number(4))))");

    let r = Context { expr: None };
    let mut p = yyParser::new(r);
    p.Parse(TokenType::INTEGER as YYCODETYPE, 15);
    p.Parse(TokenType::DIVIDE as YYCODETYPE, 0);
    p.Parse(TokenType::INTEGER as YYCODETYPE, 5);
    p.Parse(TokenType::EOF as YYCODETYPE, 0);
    p.ParseFinalize();
    let s = format!("{:?}", p.ctx.expr);
    assert_eq!(s, "Some(Binary(Divide, Number(15), Number(5)))");

    let r = Context { expr: None };
    let mut p = yyParser::new(r);
    p.Parse(TokenType::INTEGER as YYCODETYPE, 50);
    p.Parse(TokenType::PLUS as YYCODETYPE, 0);
    p.Parse(TokenType::INTEGER as YYCODETYPE, 125);
    p.Parse(TokenType::EOF as YYCODETYPE, 0);
    p.ParseFinalize();
    let s = format!("{:?}", p.ctx.expr);
    assert_eq!(s, "Some(Binary(Add, Number(50), Number(125)))");

    let r = Context { expr: None };
    let mut p = yyParser::new(r);
    p.Parse(TokenType::INTEGER as YYCODETYPE, 50);
    p.Parse(TokenType::TIMES as YYCODETYPE, 0);
    p.Parse(TokenType::INTEGER as YYCODETYPE, 125);
    p.Parse(TokenType::PLUS as YYCODETYPE, 0);
    p.Parse(TokenType::INTEGER as YYCODETYPE, 125);
    p.Parse(TokenType::EOF as YYCODETYPE, 0);
    p.ParseFinalize();
    let s = format!("{:?}", p.ctx.expr);
    assert_eq!(s, "Some(Binary(Add, Number(50), Number(125)))");
}

struct Logger;

impl log::Log for Logger {
    fn enabled(&self, metadata: &LogMetadata) -> bool {
        metadata.level() <= LogLevel::Debug
    }

    fn log(&self, record: &LogRecord) {
        if self.enabled(record.metadata()) {
            writeln!(io::stderr(), "{} - {}", record.level(), record.args()).unwrap();
        }
    }
}

fn init_logger() -> Result<(), SetLoggerError> {
    log::set_logger(|max_log_level| {
        max_log_level.set(LogLevelFilter::Debug);
        Box::new(Logger)
    })
}
}

%syntax_error {
    println!("near token {}: syntax error", yyminor);
}

program ::= expr(A). { self.ctx.expr = Some(A); }

%type expr { Expr }
expr(A) ::= expr(B) MINUS expr(C). { A = Expr::binary(Operator::Substract, B, C); }
expr(A) ::= expr(B) PLUS expr(C). { A = Expr::binary(Operator::Add, B, C); }
expr(A) ::= expr(B) TIMES expr(C). { A = Expr::binary(Operator::Multiply, B, C); }
expr(A) ::= expr(B) DIVIDE expr(C). { A = Expr::binary(Operator::Divide, B, C); }

expr(A) ::= INTEGER(B). { A = Expr::Number(B); }
