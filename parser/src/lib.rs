use dialect;

pub mod ast;
pub mod parse;

use ast::{Name, Stmt};

pub struct Context {
    stmt: Option<Stmt>,
    constraint_name: Option<Name>,
}

impl Context {
    pub fn constraint_name(&mut self) -> Option<Name> {
        self.constraint_name.take()
    }
}
