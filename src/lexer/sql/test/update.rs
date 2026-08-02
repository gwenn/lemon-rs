use super::expect_parser_err_msg;

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
#[cfg(feature = "extra_checks")]
fn update_order_by_without_limit() {
    expect_parser_err_msg(
        b"UPDATE t SET x = 1 ORDER BY x",
        "ORDER BY without LIMIT on UPDATE",
    );
}
