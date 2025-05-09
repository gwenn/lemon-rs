use fallible_iterator::FallibleIterator;

use super::{Error, Parser};
use crate::parser::ast::fmt::ToTokens;
use crate::parser::{
    ast::{Cmd, Name, ParameterInfo, QualifiedName, Stmt},
    ParserError,
};

#[test]
fn count_placeholders() {
    let ast = parse_cmd(b"SELECT ? WHERE 1 = ?");
    let mut info = ParameterInfo::default();
    ast.to_tokens(&mut info).unwrap();
    assert_eq!(info.count, 2);
}

#[test]
fn count_numbered_placeholders() {
    let ast = parse_cmd(b"SELECT ?1 WHERE 1 = ?2 AND 0 = ?1");
    let mut info = ParameterInfo::default();
    ast.to_tokens(&mut info).unwrap();
    assert_eq!(info.count, 2);
}

#[test]
fn count_unused_placeholders() {
    let ast = parse_cmd(b"SELECT ?1 WHERE 1 = ?3");
    let mut info = ParameterInfo::default();
    ast.to_tokens(&mut info).unwrap();
    assert_eq!(info.count, 3);
}

#[test]
fn count_named_placeholders() {
    let ast = parse_cmd(b"SELECT :x, :y WHERE 1 = :y");
    let mut info = ParameterInfo::default();
    ast.to_tokens(&mut info).unwrap();
    assert_eq!(info.count, 2);
    assert_eq!(info.names.len(), 2);
    assert!(info.names.contains(":x"));
    assert!(info.names.contains(":y"));
}

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
    parse_cmd(b"CREATE TABLE t (x INTEGER PRIMARY KEY AUTOINCREMENT)");
    parse_cmd(b"CREATE TABLE t (x \"INTEGER\" PRIMARY KEY AUTOINCREMENT)");
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
    )
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
fn has_explicit_nulls() {
    expect_parser_err_msg(
        b"CREATE TABLE x(a TEXT, PRIMARY KEY (a ASC NULLS FIRST))",
        "unsupported use of NULLS FIRST",
    );
    expect_parser_err_msg(
        b"CREATE TABLE x(a TEXT, UNIQUE (a ASC NULLS LAST))",
        "unsupported use of NULLS LAST",
    );
    expect_parser_err_msg(
        b"INSERT INTO x VALUES('v')
              ON CONFLICT (a DESC NULLS FIRST) DO UPDATE SET a = a+1",
        "unsupported use of NULLS FIRST",
    );
    expect_parser_err_msg(
        b"CREATE INDEX i ON x(a ASC NULLS LAST)",
        "unsupported use of NULLS LAST",
    )
}

#[test]
fn vtab_args() -> Result<(), Error> {
    let sql = b"CREATE VIRTUAL TABLE mail USING fts3(
  subject VARCHAR(256) NOT NULL,
  body TEXT CHECK(length(body)<10240)
);";
    let r = parse_cmd(sql);
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
    assert_eq!(tbl_name.as_ref(), "mail");
    assert_eq!(module_name.as_ref(), "fts3");
    assert_eq!(args.len(), 2);
    assert_eq!(args[0].as_ref(), "subject VARCHAR(256) NOT NULL");
    assert_eq!(args[1].as_ref(), "body TEXT CHECK(length(body)<10240)");
    Ok(())
}

#[test]
fn only_semicolons_no_statements() {
    let sqls = ["", ";", ";;;"];
    for sql in &sqls {
        let r = parse(sql.as_bytes());
        assert_eq!(r.unwrap(), None);
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
    for sql in &sqls {
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
fn extra_comments_between_statements() {
    let sqls = [
        "-- abc\nSELECT 1; --def\nSELECT 2 -- ghj",
        "/* abc */ SELECT 1; /* def */ SELECT 2; /* ghj */",
        "/* abc */; SELECT 1 /* def */; SELECT 2 /* ghj */",
        "/* abc */;; SELECT 1;/* def */; SELECT 2; /* ghj */; /* klm */",
    ];
    for sql in &sqls {
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
fn values() {
    parse_cmd(b"SELECT * FROM (VALUES (1))");
    parse_cmd(b"SELECT * FROM (VALUES (1), (2))");
    expect_parser_err(
        b"SELECT * FROM (VALUES (1), VALUES (2))",
        ParserError::SyntaxError("VALUES".into()),
    );
}

#[test]
fn having_without_group_by() {
    parse_cmd(b"SELECT count(*) FROM t2 HAVING count(*)>1");
}

#[test]
#[cfg(feature = "extra_checks")]
fn insert_mismatch_count() {
    expect_parser_err_msg(b"INSERT INTO t (a, b) VALUES (1)", "1 values for 2 columns");
}

#[test]
#[cfg(feature = "extra_checks")]
fn insert_default_values() {
    expect_parser_err_msg(
        b"INSERT INTO t (a) DEFAULT VALUES",
        "0 values for 1 columns",
    );
}

#[test]
#[cfg(feature = "extra_checks")]
fn create_view_mismatch_count() {
    expect_parser_err_msg(
        b"CREATE VIEW v (c1, c2) AS SELECT 1",
        "expected 2 columns for v but got 1",
    );
}

#[test]
#[cfg(feature = "extra_checks")]
fn create_view_duplicate_column_name() {
    expect_parser_err_msg(
        b"CREATE VIEW v (c1, c1) AS SELECT 1, 2",
        "duplicate column name: c1",
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
    parse_cmd(b"CREATE TEMPORARY TABLE temp.x AS SELECT 1");
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
#[cfg(feature = "extra_checks")]
fn create_strict_table_unknown_datatype() {
    expect_parser_err_msg(
        b"CREATE TABLE t (c1 BOOL) STRICT",
        "unknown datatype for t.c1: \"BOOL\"",
    );
    expect_parser_err_msg(
        b"CREATE TABLE t (c1 INT(10)) STRICT",
        "unknown datatype for t.c1: \"INT(...)\"",
    );
    parse_cmd(b"CREATE TABLE t(c1 \"INT\", c2 [TEXT], c3 `INTEGER`)");
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
    parse_cmd(
        b"CREATE TABLE IF NOT EXISTS transactions (
      debit REAL,
      credit REAL,
      amount REAL GENERATED ALWAYS AS (ifnull(credit, 0.0) -ifnull(debit, 0.0))
  ) STRICT;",
    );
}

#[test]
#[cfg(feature = "extra_checks")]
fn selects_compound_mismatch_columns_count() {
    expect_parser_err_msg(
        b"SELECT 1 UNION SELECT 1, 2",
        "SELECTs to the left and right of UNION do not have the same number of result columns",
    );
}

#[test]
#[cfg(feature = "extra_checks")]
fn delete_order_by_without_limit() {
    expect_parser_err_msg(
        b"DELETE FROM t ORDER BY x",
        "ORDER BY without LIMIT on DELETE",
    );
}

#[test]
#[cfg(feature = "extra_checks")]
fn update_order_by_without_limit() {
    expect_parser_err_msg(
        b"UPDATE t SET x = 1 ORDER BY x",
        "ORDER BY without LIMIT on UPDATE",
    );
}

#[test]
#[cfg(feature = "extra_checks")]
fn values_mismatch_columns_count() {
    expect_parser_err_msg(
        b"INSERT INTO t VALUES (1), (1,2)",
        "all VALUES must have the same number of terms",
    );
}

#[test]
fn column_specified_more_than_once() {
    expect_parser_err_msg(
        b"INSERT INTO t (n, n, m) VALUES (1, 0, 2)",
        "column \"n\" specified more than once",
    )
}

#[test]
#[cfg(feature = "extra_checks")]
fn alter_add_column_primary_key() {
    expect_parser_err_msg(
        b"ALTER TABLE t ADD COLUMN c PRIMARY KEY",
        "Cannot add a PRIMARY KEY column",
    );
}

#[test]
#[cfg(feature = "extra_checks")]
fn alter_add_column_unique() {
    expect_parser_err_msg(
        b"ALTER TABLE t ADD COLUMN c UNIQUE",
        "Cannot add a UNIQUE column",
    );
}

#[test]
#[cfg(feature = "extra_checks")]
fn alter_rename_same() {
    expect_parser_err_msg(
        b"ALTER TABLE t RENAME TO t",
        "there is already another table or index with this name: t",
    );
}

#[test]
fn natural_join_on() {
    expect_parser_err_msg(
        b"SELECT x FROM t NATURAL JOIN t USING (x)",
        "a NATURAL join may not have an ON or USING clause",
    );
    expect_parser_err_msg(
        b"SELECT x FROM t NATURAL JOIN t ON t.x = t.x",
        "a NATURAL join may not have an ON or USING clause",
    );
}

#[test]
fn missing_join_clause() {
    expect_parser_err_msg(
        b"SELECT a FROM tt ON b",
        "a JOIN clause is required before ON",
    );
}

#[test]
fn cast_without_typename() {
    parse_cmd(b"SELECT CAST(a AS ) FROM t");
}

#[test]
#[cfg(feature = "extra_checks")]
fn distinct_aggregates() {
    expect_parser_err_msg(
        b"SELECT count(DISTINCT) FROM t",
        "DISTINCT aggregates must have exactly one argument",
    );
    expect_parser_err_msg(
        b"SELECT count(DISTINCT a,b) FROM t",
        "DISTINCT aggregates must have exactly one argument",
    );
}

#[test]
#[cfg(feature = "extra_checks")]
fn cte_column_count() {
    expect_parser_err_msg(
        b"WITH i(x, y) AS ( VALUES(1) )
      SELECT * FROM i;",
        "table i has 1 values for 2 columns",
    )
}
#[test]
#[cfg(feature = "extra_checks")]
fn duplicate_cte() {
    expect_parser_err_msg(
        b"WITH i(x) AS (SELECT 1),
      i(y) AS (SELECT 2)
      SELECT * FROM i;",
        "duplicate WITH table name: i",
    )
}

#[test]
fn unknown_join_type() {
    expect_parser_err_msg(
        b"SELECT * FROM t1 INNER OUTER JOIN t2;",
        "unknown join type: INNER OUTER ",
    );
    expect_parser_err_msg(
        b"SELECT * FROM t1 LEFT BOGUS JOIN t2;",
        "unknown join type: BOGUS",
    )
}

#[test]
fn no_tables_specified() {
    #[cfg(feature = "extra_checks")]
    expect_parser_err_msg(b"SELECT *", "no tables specified");
    #[cfg(feature = "extra_checks")]
    expect_parser_err_msg(b"SELECT t.*", "no tables specified");
    #[cfg(feature = "extra_checks")]
    expect_parser_err_msg(b"SELECT count(*), *", "no tables specified");
    parse_cmd(b"SELECT count(*)");
}

#[test]
#[cfg(feature = "extra_checks")]
fn group_by_out_of_range() {
    expect_parser_err_msg(
        b"SELECT a, b FROM x GROUP BY 0",
        "GROUP BY term out of range - should be between 1 and 2",
    );
    expect_parser_err_msg(
        b"SELECT a, b FROM x GROUP BY 3",
        "GROUP BY term out of range - should be between 1 and 2",
    );
}

#[test]
#[cfg(feature = "extra_checks")]
fn order_by_out_of_range() {
    expect_parser_err_msg(
        b"SELECT a, b FROM x ORDER BY -1",
        "ORDER BY term out of range - should be between 1 and 2",
    );
    expect_parser_err_msg(
        b"SELECT a, b FROM x ORDER BY 0",
        "ORDER BY term out of range - should be between 1 and 2",
    );
    expect_parser_err_msg(
        b"SELECT a, b FROM x ORDER BY 3",
        "ORDER BY term out of range - should be between 1 and 2",
    );
}

#[test]
#[cfg(feature = "extra_checks")]
fn update_from_target() {
    expect_parser_err_msg(
        b"UPDATE x1 SET a=5 FROM x1",
        "target object/alias may not appear in FROM clause",
    );
    expect_parser_err_msg(
        b"UPDATE x1 SET a=5 FROM x2, x1",
        "target object/alias may not appear in FROM clause",
    );
}

#[test]
fn unknown_table_option() {
    expect_parser_err_msg(b"CREATE TABLE t(x)o", "unknown table option: o");
    expect_parser_err_msg(b"CREATE TABLE t(x) WITHOUT o", "unknown table option: o");
}

#[test]
fn qualified_table_name_within_triggers() {
    expect_parser_err_msg(
        b"CREATE TRIGGER tr1 AFTER INSERT ON t1 BEGIN
            DELETE FROM main.t2;
          END;",
        "qualified table names are not allowed on INSERT, UPDATE, and DELETE statements \
          within triggers",
    );
}

#[test]
fn indexed_by_clause_within_triggers() {
    expect_parser_err_msg(
        b"CREATE TRIGGER main.t16err5 AFTER INSERT ON tA BEGIN
            UPDATE t16 INDEXED BY t16a SET rowid=rowid+1 WHERE a=1;
          END;",
        "the INDEXED BY clause is not allowed on UPDATE or DELETE statements \
           within triggers",
    );
    expect_parser_err_msg(
        b"CREATE TRIGGER main.t16err6 AFTER INSERT ON tA BEGIN
            DELETE FROM t16 NOT INDEXED WHERE a=123;
          END;",
        "the NOT INDEXED clause is not allowed on UPDATE or DELETE statements \
         within triggers",
    );
}

#[test]
fn returning_within_trigger() {
    expect_parser_err_msg(b"CREATE TRIGGER t AFTER DELETE ON x BEGIN INSERT INTO x (a) VALUES ('x') RETURNING rowid; END;", "cannot use RETURNING in a trigger");
}

#[test]
fn reserved_name() {
    #[cfg(feature = "extra_checks")]
    expect_parser_err_msg(
        b"CREATE TABLE sqlite_x(a)",
        "object name reserved for internal use: sqlite_x",
    );
    #[cfg(feature = "extra_checks")]
    expect_parser_err_msg(
        b"CREATE VIEW sqlite_x(a) AS SELECT 1",
        "object name reserved for internal use: sqlite_x",
    );
    #[cfg(feature = "extra_checks")]
    expect_parser_err_msg(
        b"CREATE INDEX sqlite_x ON x(a)",
        "object name reserved for internal use: sqlite_x",
    );
    #[cfg(feature = "extra_checks")]
    expect_parser_err_msg(
        b"CREATE TRIGGER sqlite_x AFTER INSERT ON x BEGIN SELECT 1; END;",
        "object name reserved for internal use: sqlite_x",
    );
    parse_cmd(b"CREATE TABLE sqlite(a)");
    parse_cmd(b"CREATE INDEX \"\" ON t(a)");
}

fn expect_parser_err_msg(input: &[u8], error_msg: &str) {
    expect_parser_err(input, ParserError::Custom(error_msg.to_owned()))
}
fn expect_parser_err(input: &[u8], err: ParserError) {
    let r = parse(input);
    if let Error::ParserError(e, _) = r.unwrap_err() {
        assert_eq!(e, err);
    } else {
        panic!("unexpected error type")
    };
}
fn parse_cmd(input: &[u8]) -> Cmd {
    parse(input).unwrap().unwrap()
}
fn parse(input: &[u8]) -> Result<Option<Cmd>, Error> {
    let mut parser = Parser::new(input);
    parser.next()
}
