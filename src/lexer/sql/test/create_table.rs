use bumpalo::Bump;

use super::{assert_string, expect_parser_err, expect_parser_err_msg, parse_cmd};
use crate::parser::ParserError;

#[test]
fn duplicate_column() {
    expect_parser_err_msg(
        b"CREATE TABLE t (x TEXT, x TEXT)",
        "duplicate column name: x",
    );
    expect_parser_err_msg(
        b"CREATE TABLE t (x TEXT, \"x\" TEXT)",
        "duplicate column name: \"x\"",
    );
    expect_parser_err_msg(
        b"CREATE TABLE t (x TEXT, `x` TEXT)",
        "duplicate column name: `x`",
    );
}

#[test]
fn create_table_without_column() {
    expect_parser_err(b"CREATE TABLE t ()", ParserError::SyntaxError(")".into()));
}

#[test]
fn auto_increment() {
    assert_string("CREATE TABLE t(x INTEGER PRIMARY KEY AUTOINCREMENT);");
    assert_string("CREATE TABLE t(x \"INTEGER\" PRIMARY KEY AUTOINCREMENT);");
    #[cfg(feature = "extra_checks")]
    expect_parser_err_msg(
        b"CREATE TABLE t (x TEXT PRIMARY KEY AUTOINCREMENT)",
        "AUTOINCREMENT is only allowed on an INTEGER PRIMARY KEY",
    );
}

#[test]
#[cfg(feature = "extra_checks")]
fn generated() {
    expect_parser_err_msg(
        b"CREATE TABLE x(a PRIMARY KEY AS ('id'))",
        "generated columns cannot be part of the PRIMARY KEY",
    );
    expect_parser_err_msg(
        b"CREATE TABLE x(a AS ('id') DEFAULT '')",
        "cannot use DEFAULT on a generated column",
    );
}

#[test]
#[cfg(feature = "extra_checks")]
fn more_than_one_pk() {
    expect_parser_err_msg(
        b"CREATE TABLE test (a,b, PRIMARY KEY(a), PRIMARY KEY(b))",
        "table has more than one primary key",
    );
    expect_parser_err_msg(
        b"CREATE TABLE test (a PRIMARY KEY, b PRIMARY KEY)",
        "table has more than one primary key",
    );
    expect_parser_err_msg(
        b"CREATE TABLE test (a PRIMARY KEY, b, PRIMARY KEY(a))",
        "table has more than one primary key",
    );
}

#[test]
#[cfg(feature = "extra_checks")]
fn create_table_without_rowid_missing_pk() {
    expect_parser_err_msg(
        b"CREATE TABLE t (c1) WITHOUT ROWID",
        "PRIMARY KEY missing on table t",
    );
}

#[test]
fn create_temporary_table_with_qualified_name() {
    #[cfg(feature = "extra_checks")]
    expect_parser_err_msg(
        b"CREATE TEMPORARY TABLE mem.x AS SELECT 1",
        "temporary table name must be unqualified",
    );
    assert_string("CREATE TEMP TABLE temp.x AS SELECT 1;");
}

#[test]
#[cfg(feature = "extra_checks")]
fn create_table_with_only_generated_column() {
    expect_parser_err_msg(
        b"CREATE TABLE test(data AS (1))",
        "must have at least one non-generated column",
    );
}

#[test]
#[cfg(feature = "extra_checks")]
fn create_strict_table_missing_datatype() {
    expect_parser_err_msg(b"CREATE TABLE t (c1) STRICT", "missing datatype for t.c1");
}

#[test]
fn create_strict_table_unknown_datatype() {
    #[cfg(feature = "extra_checks")]
    expect_parser_err_msg(
        b"CREATE TABLE t (c1 BOOL) STRICT",
        "unknown datatype for t.c1: \"BOOL\"",
    );
    #[cfg(feature = "extra_checks")]
    expect_parser_err_msg(
        b"CREATE TABLE t (c1 INT(10)) STRICT",
        "unknown datatype for t.c1: \"INT(...)\"",
    );
    assert_string("CREATE TABLE t(c1 \"INT\", c2 [TEXT], c3 `INTEGER`);");
}

#[test]
#[cfg(feature = "extra_checks")]
fn foreign_key_on_column() {
    expect_parser_err_msg(
        b"CREATE TABLE t(a REFERENCES o(a,b))",
        "foreign key on a should reference only one column of table o",
    );
}

#[test]
fn create_strict_table_generated_column() {
    let b = Bump::new();
    parse_cmd(
        b"CREATE TABLE IF NOT EXISTS transactions (
      debit REAL,
      credit REAL,
      amount REAL GENERATED ALWAYS AS (ifnull(credit, 0.0) -ifnull(debit, 0.0))
  ) STRICT;",
        &b,
    );
}

#[test]
fn unknown_table_option() {
    expect_parser_err_msg(b"CREATE TABLE t(x)o", "unknown table option: o");
    expect_parser_err_msg(b"CREATE TABLE t(x) WITHOUT o", "unknown table option: o");
}
