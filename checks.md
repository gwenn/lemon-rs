# Extra consistency checks

- `ALTER TABLE ... RENAME TO ...` when old and new table names are the same => `Stmt::check`
- `ALTER TABLE ... ADD COLUMN ...` with new primary key / unique constraint => `Stmt::check`
- `CREATE TABLE ...`
  - with duplicated column name => `ColumnDefinition::add_column`
  - with STRICT option and invalid or missing column type(s) => `CreateTableBody::check`
  - WITHOUT ROWID and without primary key => `CreateTableBody::check`
- `CREATE VIEW ... (...) ...`
  - when view columns count does not match select columns count => `Stmt::check`
  - with duplicated columns (same name) => `Stmt::check`
- `DELETE FROM ... ORDER BY ...` with ORDER BY but without LIMIT => `Stmt::check`
- `INSERT INTO ... (...) ...` when columns count does not match select columns / values count => `Stmt::check`
- `INSERT INTO ... (...) DEFAULT VALUES` with columns and DEFAULT VALUES => `Stmt::check`
- `SELECT ... EXCEPT|INTERSECT|UNION SELECT ...` when all SELECT does not have the same number of result columns => `SelectBody::push`
- `NATURAL JOIN ...` with ON or USING clause => `FromClause::push`
- `UPDATE ... ORDER BY ...` with ORDER BY but without LIMIT => `Stmt::check`
- `VALUES (...), (...), ...` when all VALUES does not have the same number of terms => `OneSelect::push`
- `WITH ...` with duplicated table name => `CommonTableExpr::add_cte`

## TODO

### wrong error location

```
RUST_LOG=sqlite3Parser=debug cargo run --example sql_cmd "PRAGMA test=?"
[ERROR sqlite3Parser] near "Ok("")": syntax error
Err: near "": syntax error at (1, 14) in PRAGMA test=?
```
vs
```
sqlite> .parameter init
sqlite> .parameter set ?1 1
sqlite> PRAGMA test=?;
Parse error: near "?": syntax error
  PRAGMA test=?;
              ^--- error here
```

### `CREATE TABLE`

- [x] qualified (different of `temp`) temporary table
- [x] must have at least one non-generated column
- [ ] column constraint(s) checks

```sql
sqlite> CREATE TABLE t(a INTEGER PRIMARY KEY AUTOINCREMENT) WITHOUT ROWID;
Parse error: AUTOINCREMENT not allowed on WITHOUT ROWID tables
```

- [ ] table constraint(s) checks

```sql
sqlite> CREATE TABLE test (a, b, FOREIGN KEY (b) REFERENCES test(a,b));
Parse error: number of columns in foreign key does not match the number of columns in the referenced table
```

### `HAVING`

- [ ] HAVING clause on a non-aggregate query (`GroupBy::having`): grammar already prevents this case (grammar differs from SQLite official grammar).

```sql
sqlite> SELECT 1 as i HAVING i > 1;
Parse error: HAVING clause on a non-aggregate query
```

### `SELECT ...`

- [ ] no duplicated column name in `selcollist`/`Select::columns`

```sql
sqlite> SELECT 1 as i, 2 as i;
-- no error (idem for postgres)
```

### `SELECT ... ORDER BY ...`

- [ ] ORDER BY term does not match any column in the result set (`Select::order_by`)

```sql
sqlite> SELECT 1 as i ORDER BY j;
Parse error: no such column: j
  SELECT 1 as i ORDER BY j;
                         ^--- error here
```

### `WITH`

- [ ] no duplicated column name in `CommonTableExpr::IndexedColumn`

### DML

```sql
sqlite> UPDATE test SET n = 1, n = 0; -- pgsql KO
sqlite> SELECT * FROM test;
0|1
```
