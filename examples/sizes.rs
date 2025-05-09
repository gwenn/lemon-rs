use std::mem::size_of;

use sqlite3_parser::ast::{Cmd, Expr, Select, Stmt};
//use sqlite3_parser::parser::parse::YYMINORTYPE;

fn main() {
    println!("size_of(Cmd) => {}", size_of::<Cmd>());
    println!("size_of(Stmt) => {}", size_of::<Stmt>());
    println!("size_of(Select) => {}", size_of::<Select>());
    println!("size_of(Expr) => {}", size_of::<Expr>());
    //println!("size_of(YYMINORTYPE) => {}", size_of::<YYMINORTYPE>());
}
/*
size_of(Cmd) => 520
size_of(Stmt) => 512
size_of(Select) => 408
size_of(Expr) => 96
*/
