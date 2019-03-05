use dialect;

pub mod ast;
pub mod parse;

use ast::{Cmd, ExplainKind, Name, Stmt};

pub struct Context {
    explain: Option<ExplainKind>,
    stmt: Option<Stmt>,
    constraint_name: Option<Name>, // transient
    done: bool,
    error: Option<String>,
}

impl Context {
    pub fn new() -> Context {
        Context {
            explain: None,
            stmt: None,
            constraint_name: None,
            done: false,
            error: None,
        }
    }

    pub fn cmd(&mut self) -> Option<Cmd> {
        if let Some(stmt) = self.stmt.take() {
            match self.explain.take() {
                Some(ExplainKind::Explain) => Some(Cmd::Explain(stmt)),
                Some(ExplainKind::QueryPlan) => Some(Cmd::ExplainQueryPlan(stmt)),
                None => Some(Cmd::Stmt(stmt)),
            }
        } else {
            None
        }
    }

    fn constraint_name(&mut self) -> Option<Name> {
        self.constraint_name.take()
    }

    fn sqlite3_error_msg(&mut self, msg: &str) {
        // TODO
        eprintln!("{}", msg);
    }

    /// This routine is called after a single SQL statement has been parsed.
    fn sqlite3_finish_coding(&mut self) {
        self.done = true;
    }

    pub fn done(&self) -> bool {
        self.done
    }

    pub fn error(&mut self) -> Option<String> {
        self.error.take()
    }

    pub fn reset(&mut self) {
        self.explain = None;
        self.stmt = None;
        self.constraint_name = None;
        self.done = false;
        self.error = None;
    }
}
