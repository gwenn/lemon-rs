use super::{assert_string, expect_parser_err_msg};

#[test]
fn qualified_table_name_within_triggers() {
    assert_string(
        "CREATE TRIGGER tr1 AFTER INSERT ON t1 BEGIN
DELETE FROM main.t2;
END;",
    );
}

#[test]
fn indexed_by_clause_within_triggers() {
    expect_parser_err_msg(
        b"CREATE TRIGGER main.t16err5 AFTER INSERT ON tA BEGIN
            UPDATE t16 INDEXED BY t16a SET rowid=rowid+1 WHERE a=1;
          END;",
        "the INDEXED BY clause is not allowed on UPDATE or DELETE statements within triggers",
    );
    expect_parser_err_msg(
        b"CREATE TRIGGER main.t16err6 AFTER INSERT ON tA BEGIN
            DELETE FROM t16 NOT INDEXED WHERE a=123;
          END;",
        "the NOT INDEXED clause is not allowed on UPDATE or DELETE statements within triggers",
    );
}

#[test]
fn returning_within_trigger() {
    expect_parser_err_msg(b"CREATE TRIGGER t AFTER DELETE ON x BEGIN INSERT INTO x (a) VALUES ('x') RETURNING rowid; END;", "cannot use RETURNING in a trigger");
}
