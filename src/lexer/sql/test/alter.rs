use super::assert_string;
#[cfg(feature = "extra_checks")]
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

#[test]
fn alter_table() {
    assert_string("ALTER TABLE tab1 RENAME TO tab1_real;");
    assert_string("ALTER TABLE t1 ADD COLUMN b INTEGER COLLATE NOCASE;");
    assert_string("ALTER TABLE t1 ADD COLUMN c;");
    assert_string("ALTER TABLE t2 ADD COLUMN c REFERENCES t1(c);");
    assert_string("ALTER TABLE t1 ADD COLUMN c NOT NULL;"); // FIXME should fail
    assert_string("ALTER TABLE t2 DROP COLUMN c;");
    assert_string("ALTER TABLE t2 ALTER COLUMN b SET NOT NULL;");
    assert_string("ALTER TABLE t2 ALTER COLUMN x DROP NOT NULL;");
    assert_string("ALTER TABLE t2 ADD CONSTRAINT abc CHECK(aaa > b);");
    assert_string("ALTER TABLE t2 DROP CONSTRAINT abc;");
    assert_string("ALTER TABLE t1 RENAME COLUMN b TO d;");
}
