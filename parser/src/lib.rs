use dialect;

pub mod ast;
pub mod parse;

use ast::Stmt;

pub struct Context {
    stmt: Option<Stmt>,
}
