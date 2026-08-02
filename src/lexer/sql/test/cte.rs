use super::expect_parser_err_msg;

#[test]
#[cfg(feature = "extra_checks")]
fn cte_column_count() {
    expect_parser_err_msg(
        b"WITH i(x, y) AS ( VALUES(1) )
      SELECT * FROM i;",
        "table i has 1 values for 2 columns",
    );
}
#[test]
#[cfg(feature = "extra_checks")]
fn duplicate_cte() {
    expect_parser_err_msg(
        b"WITH i(x) AS (SELECT 1),
      i(y) AS (SELECT 2)
      SELECT * FROM i;",
        "duplicate WITH table name: i",
    );
}
