use super::expect_parser_err_msg;

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
    );
}
