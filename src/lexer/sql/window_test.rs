use bumpalo::Bump;
use fallible_iterator::FallibleIterator as _;

use super::Parser;

#[test]
fn over() {
    assert_string("SELECT x, y, row_number() OVER (ORDER BY y) AS row_number FROM t0 ORDER BY x;");
}

#[test]
fn window() {
    assert_string(
        "SELECT x, y, row_number() OVER win1, rank() OVER win2 FROM t0 WINDOW win1 AS (ORDER BY y \
         RANGE BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW), win2 AS (PARTITION BY y ORDER BY x) \
         ORDER BY x;",
    );
}

#[test]
fn filter() {
    assert_string(
        "SELECT c, a, b, group_concat(b, '.') FILTER (WHERE c <> 'two') OVER (ORDER BY a) AS \
         group_concat FROM t1 ORDER BY a;",
    );
}

fn assert_string(input: &str) {
    let b = Bump::new();
    let mut parser = Parser::new(&b, input.as_bytes());
    let cmd = parser.next().unwrap().unwrap();
    assert_eq!(cmd.to_string(), input);
}
