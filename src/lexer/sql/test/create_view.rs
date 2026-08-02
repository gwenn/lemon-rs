use super::expect_parser_err_msg;

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
