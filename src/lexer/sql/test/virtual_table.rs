use bumpalo::Bump;

use super::parse_cmd;
use crate::ast::{Cmd, Name, QualifiedName, Stmt};
use crate::lexer::sql::Error;

#[test]
fn vtab_args() -> Result<(), Error> {
    let sql = b"CREATE VIRTUAL TABLE mail USING fts3(
  subject VARCHAR(256) NOT NULL,
  body TEXT CHECK(length(body)<10240)
);";
    let b = Bump::new();
    let r = parse_cmd(sql, &b);
    let Cmd::Stmt(Stmt::CreateVirtualTable {
        tbl_name: QualifiedName {
            name: Name(tbl_name),
            ..
        },
        module_name: Name(module_name),
        args: Some(args),
        ..
    }) = r
    else {
        panic!("unexpected AST")
    };
    assert_eq!(tbl_name, "mail");
    assert_eq!(module_name, "fts3");
    assert_eq!(args.len(), 2);
    assert_eq!(args[0], "subject VARCHAR(256) NOT NULL");
    assert_eq!(args[1], "body TEXT CHECK(length(body)<10240)");
    Ok(())
}
