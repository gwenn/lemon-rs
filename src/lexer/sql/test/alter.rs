use super::expect_parser_err_msg;

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
