use super::expect_parser_err_msg;

#[test]
#[cfg(feature = "extra_checks")]
fn delete_order_by_without_limit() {
    expect_parser_err_msg(
        b"DELETE FROM t ORDER BY x",
        "ORDER BY without LIMIT on DELETE",
    );
}
