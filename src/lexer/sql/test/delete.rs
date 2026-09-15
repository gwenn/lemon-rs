use super::assert_string;
#[cfg(feature = "extra_checks")]
use super::expect_parser_err_msg;

#[test]
#[cfg(feature = "extra_checks")]
fn delete_order_by_without_limit() {
    expect_parser_err_msg(
        b"DELETE FROM t ORDER BY x",
        "ORDER BY without LIMIT on DELETE",
    );
}
#[test]
fn delete() {
    assert_string("DELETE FROM artist WHERE artistname = 'Frank Sinatra';");
}
