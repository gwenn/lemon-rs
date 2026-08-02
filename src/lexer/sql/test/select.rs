use super::{assert_string, expect_parser_err, expect_parser_err_msg};
use crate::parser::ParserError;

#[test]
fn values() {
    assert_string("SELECT * FROM (VALUES(1));");
    assert_string("SELECT * FROM (VALUES(1),(2));");
    expect_parser_err(
        b"SELECT * FROM (VALUES (1), VALUES (2))",
        ParserError::SyntaxError("VALUES".into()),
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
fn unknown_join_type() {
    expect_parser_err_msg(
        b"SELECT * FROM t1 INNER OUTER JOIN t2;",
        "unknown join type: INNER OUTER ",
    );
    expect_parser_err_msg(
        b"SELECT * FROM t1 LEFT BOGUS JOIN t2;",
        "unknown join type: BOGUS",
    );
}

#[test]
fn no_tables_specified() {
    #[cfg(feature = "extra_checks")]
    expect_parser_err_msg(b"SELECT *", "no tables specified");
    #[cfg(feature = "extra_checks")]
    expect_parser_err_msg(b"SELECT t.*", "no tables specified");
    #[cfg(feature = "extra_checks")]
    expect_parser_err_msg(b"SELECT count(*), *", "no tables specified");
    assert_string("SELECT count(*);");
}

#[test]
fn having_without_group_by() {
    assert_string("SELECT count(*) FROM t2 HAVING count(*) > 1;");
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
