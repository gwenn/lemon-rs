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

### `CREATE TABLE`
- [X] qualified (different of `temp`) temporary table
```sql
sqlite> ATTACH DATABASE ':memory:' AS mem;
sqlite> CREATE TEMPORARY TABLE mem.x AS SELECT 1;
Parse error: temporary table name must be unqualified
```
```sql
sqlite> CREATE TEMPORARY TABLE temp.x AS SELECT 1;
-- OK
```
- [X] must have at least one non-generated column
```sql
sqlite> CREATE TABLE test(data AS (1));
Parse error: must have at least one non-generated column
```
- [ ] column constraint(s) checks
- [ ] table constraint(s) checks

### `HAVING`
- [ ] HAVING clause on a non-aggregate query (`GroupBy::having`)

### `SELECT ... ORDER BY ...`
- [ ] ORDER BY term does not match any column in the result set (`Select::order_by`)

### `WITH`
- [ ] no duplicated column name in `CommonTableExpr#IndexedColumn`
