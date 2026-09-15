use std::assert_matches;

use bumpalo::Bump;
use fallible_iterator::FallibleIterator as _;

use super::{Error, Parser};
use crate::parser::ParserError;
use crate::parser::ast::{Cmd, Stmt};

mod alter;
mod create_table;
mod create_trigger;
mod create_view;
mod cte;
mod delete;
mod expr;
mod insert;
mod placeholders;
mod select;
mod update;
mod virtual_table;
mod window;

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
    );
}

#[test]
fn only_semicolons_no_statements() {
    let bump = Bump::new();
    let sqls = ["", ";", ";;;"];
    for sql in &sqls {
        let r = parse(sql.as_bytes(), &bump);
        assert_eq!(r.unwrap(), None);
    }
}

#[test]
fn extra_semicolons_between_statements() {
    let bump = Bump::new();
    let sqls = [
        "SELECT 1; SELECT 2",
        "SELECT 1; SELECT 2;",
        "; SELECT 1; SELECT 2",
        ";; SELECT 1;; SELECT 2;;",
    ];
    for sql in &sqls {
        let mut parser = Parser::new(&bump, sql.as_bytes());
        assert_matches!(parser.next().unwrap(), Some(Cmd::Stmt(Stmt::Select { .. })));
        assert_matches!(parser.next().unwrap(), Some(Cmd::Stmt(Stmt::Select { .. })));
        assert_eq!(parser.next().unwrap(), None);
    }
}

#[test]
fn extra_comments_between_statements() {
    let bump = Bump::new();
    let sqls = [
        "-- abc\nSELECT 1; --def\nSELECT 2 -- ghj",
        "/* abc */ SELECT 1; /* def */ SELECT 2; /* ghj */",
        "/* abc */; SELECT 1 /* def */; SELECT 2 /* ghj */",
        "/* abc */;; SELECT 1;/* def */; SELECT 2; /* ghj */; /* klm */",
    ];
    for sql in &sqls {
        let mut parser = Parser::new(&bump, sql.as_bytes());
        assert_matches!(parser.next().unwrap(), Some(Cmd::Stmt(Stmt::Select { .. })));
        assert_matches!(parser.next().unwrap(), Some(Cmd::Stmt(Stmt::Select { .. })));
        assert_eq!(parser.next().unwrap(), None);
    }
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
    assert_string("CREATE TABLE sqlite(a);");
    assert_string("CREATE INDEX \"\" ON t(a);");
}

#[track_caller]
fn expect_parser_err_msg(input: &[u8], error_msg: &str) {
    expect_parser_err(input, ParserError::Custom(error_msg.to_owned()));
}
#[track_caller]
fn expect_parser_err(input: &[u8], err: ParserError) {
    let b = Bump::new();
    let r = parse(input, &b);
    if let Error::ParserError(e, _) = r.unwrap_err() {
        assert_eq!(e, err);
    } else {
        panic!("unexpected error type")
    }
}
#[track_caller]
fn assert_string(input: &str) {
    let b = Bump::new();
    let cmd = parse_cmd(input.as_bytes(), &b);
    assert_eq!(cmd.to_string(), input);
}
fn parse_cmd<'bump>(input: &'bump [u8], b: &'bump Bump) -> Cmd<'bump> {
    parse(input, b).unwrap().unwrap()
}
fn parse<'bump>(input: &'bump [u8], b: &'bump Bump) -> Result<Option<Cmd<'bump>>, Error> {
    let mut parser = Parser::new(b, input);
    parser.next()
}
