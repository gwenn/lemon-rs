use super::assert_string;
#[cfg(feature = "extra_checks")]
use super::expect_parser_err_msg;

#[test]
fn cast_without_typename() {
    assert_string("SELECT CAST(a AS) FROM t;");
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
