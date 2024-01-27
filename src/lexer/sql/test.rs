use fallible_iterator::FallibleIterator;

use super::{Error, Parser};
use crate::parser::ast::fmt::ToTokens;
use crate::parser::{
    ast::{Cmd, Name, ParameterInfo, QualifiedName, Stmt},
    ParserError,
};

#[test]
fn count_placeholders() -> Result<(), Error> {
    let mut parser = Parser::new(b"SELECT ? WHERE 1 = ?");
    let ast = parser.next()?.unwrap();
    let mut info = ParameterInfo::default();
    ast.to_tokens(&mut info).unwrap();
    assert_eq!(info.count, 2);
    Ok(())
}

#[test]
fn count_numbered_placeholders() -> Result<(), Error> {
    let mut parser = Parser::new(b"SELECT ?1 WHERE 1 = ?2 AND 0 = ?1");
    let ast = parser.next()?.unwrap();
    let mut info = ParameterInfo::default();
    ast.to_tokens(&mut info).unwrap();
    assert_eq!(info.count, 2);
    Ok(())
}

#[test]
fn count_unused_placeholders() -> Result<(), Error> {
    let mut parser = Parser::new(b"SELECT ?1 WHERE 1 = ?3");
    let ast = parser.next()?.unwrap();
    let mut info = ParameterInfo::default();
    ast.to_tokens(&mut info).unwrap();
    assert_eq!(info.count, 3);
    Ok(())
}

#[test]
fn count_named_placeholders() -> Result<(), Error> {
    let mut parser = Parser::new(b"SELECT :x, :y WHERE 1 = :y");
    let ast = parser.next()?.unwrap();
    let mut info = ParameterInfo::default();
    ast.to_tokens(&mut info).unwrap();
    assert_eq!(info.count, 2);
    assert_eq!(info.names.len(), 2);
    assert!(info.names.contains(":x"));
    assert!(info.names.contains(":y"));
    Ok(())
}

#[test]
fn duplicate_column() {
    let mut parser = Parser::new(b"CREATE TABLE t (x TEXT, x TEXT)");
    let r = parser.next();
    let Error::ParserError(ParserError::Custom(msg), _) = r.unwrap_err() else {
        panic!("unexpected error type")
    };
    assert!(msg.contains("duplicate column name"));
}

#[test]
fn create_table_without_column() {
    let mut parser = Parser::new(b"CREATE TABLE t ()");
    let r = parser.next();
    let Error::ParserError(
        ParserError::SyntaxError {
            token_type: "RP",
            found: None,
        },
        _,
    ) = r.unwrap_err()
    else {
        panic!("unexpected error type")
    };
}

#[test]
fn vtab_args() -> Result<(), Error> {
    let sql = r#"CREATE VIRTUAL TABLE mail USING fts3(
  subject VARCHAR(256) NOT NULL,
  body TEXT CHECK(length(body)<10240)
);"#;
    let mut parser = Parser::new(sql.as_bytes());
    let Cmd::Stmt(Stmt::CreateVirtualTable {
        tbl_name: QualifiedName {
            name: Name(tbl_name),
            ..
        },
        module_name: Name(module_name),
        args: Some(args),
        ..
    }) = parser.next()?.unwrap()
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

#[test]
fn only_semicolons_no_statements() {
    let sqls = ["", ";", ";;;"];
    for sql in sqls.iter() {
        let mut parser = Parser::new(sql.as_bytes());
        assert_eq!(parser.next().unwrap(), None);
    }
}

#[test]
fn extra_semicolons_between_statements() {
    let sqls = [
        "SELECT 1; SELECT 2",
        "SELECT 1; SELECT 2;",
        "; SELECT 1; SELECT 2",
        ";; SELECT 1;; SELECT 2;;",
    ];
    for sql in sqls.iter() {
        let mut parser = Parser::new(sql.as_bytes());
        assert!(matches!(
            parser.next().unwrap(),
            Some(Cmd::Stmt(Stmt::Select { .. }))
        ));
        assert!(matches!(
            parser.next().unwrap(),
            Some(Cmd::Stmt(Stmt::Select { .. }))
        ));
        assert_eq!(parser.next().unwrap(), None);
    }
}

#[test]
fn insert_mismatch_count() {
    expect_parser_err(
        b"INSERT INTO tbl (a, b) VALUES (1)",
        "1 values for 2 columns",
    );
}

#[test]
fn insert_default_values() {
    expect_parser_err(
        b"INSERT INTO tbl (a) DEFAULT VALUES",
        "0 values for 1 columns",
    );
}

#[test]
fn create_view_mismatch_count() {
    expect_parser_err(
        b"CREATE VIEW v (c1, c2) AS SELECT 1",
        "expected 2 columns for v but got 1",
    );
}

#[test]
fn create_view_duplicate_column_name() {
    expect_parser_err(
        b"CREATE VIEW v (c1, c1) AS SELECT 1, 2",
        "duplicate column name: c1",
    );
}

#[test]
fn create_table_without_rowid_missing_pk() {
    expect_parser_err(
        b"CREATE TABLE tbl (c1) WITHOUT ROWID",
        "PRIMARY KEY missing on table tbl",
    );
}

#[test]
fn create_strict_table_missing_datatype() {
    expect_parser_err(
        b"CREATE TABLE tbl (c1) STRICT",
        "missing datatype for tbl.c1",
    );
}

#[test]
fn create_strict_table_unknown_datatype() {
    expect_parser_err(
        b"CREATE TABLE tbl (c1 BOOL) STRICT",
        "unknown datatype for tbl.c1: \"BOOL\"",
    );
}

#[test]
fn create_strict_table_generated_column() {
    let mut parser = Parser::new(
        b"CREATE TABLE IF NOT EXISTS transactions (
      debit REAL,
      credit REAL,
      amount REAL GENERATED ALWAYS AS (ifnull(credit, 0.0) -ifnull(debit, 0.0))
  ) STRICT;
",
    );
    let r = parser.next();
    r.unwrap();
}

#[test]
fn selects_compound_mismatch_columns_count() {
    expect_parser_err(
        b"SELECT 1 UNION SELECT 1, 2",
        "SELECTs to the left and right of UNION do not have the same number of result columns",
    );
}

#[test]
fn delete_order_by_without_limit() {
    expect_parser_err(
        b"DELETE FROM test ORDER BY x",
        "ORDER BY without LIMIT on DELETE",
    );
}

#[test]
fn update_order_by_without_limit() {
    expect_parser_err(
        b"UPDATE test SET data = 1 ORDER BY data",
        "ORDER BY without LIMIT on UPDATE",
    );
}

#[test]
fn values_mismatch_columns_count() {
    expect_parser_err(
        b"INSERT INTO test VALUES (1), (1,2)",
        "all VALUES must have the same number of terms",
    );
}

#[test]
fn alter_add_column_primary_key() {
    expect_parser_err(
        b"ALTER TABLE test ADD COLUMN c PRIMARY KEY",
        "Cannot add a PRIMARY KEY column",
    );
}

#[test]
fn alter_add_column_unique() {
    expect_parser_err(
        b"ALTER TABLE test ADD COLUMN c UNIQUE",
        "Cannot add a UNIQUE column",
    );
}

#[test]
fn alter_rename_same() {
    expect_parser_err(
        b"ALTER TABLE test RENAME TO test",
        "there is already another table or index with this name: test",
    );
}

fn expect_parser_err(input: &[u8], error_msg: &str) {
    let mut parser = Parser::new(input);
    let r = parser.next();
    if let Error::ParserError(ParserError::Custom(ref msg), _) = r.unwrap_err() {
        assert_eq!(msg, error_msg);
    } else {
        panic!("unexpected error type")
    };
}
