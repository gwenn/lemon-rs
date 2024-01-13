use crate::ast::*;
use std::fmt::{Display, Formatter};

impl Cmd {
    /// Statement accessor
    pub fn stmt(&self) -> &Stmt {
        match self {
            Cmd::Explain(stmt) => stmt,
            Cmd::ExplainQueryPlan(stmt) => stmt,
            Cmd::Stmt(stmt) => stmt,
        }
    }
    /// Like `sqlite3_column_count` but more limited
    pub fn column_count(&self) -> ColumnCount {
        match self {
            Cmd::Explain(_) => ColumnCount::Fixed(8),
            Cmd::ExplainQueryPlan(_) => ColumnCount::Fixed(4),
            Cmd::Stmt(stmt) => stmt.column_count(),
        }
    }
    /// Like `sqlite3_stmt_readonly`
    pub fn readonly(&self) -> bool {
        self.stmt().readonly()
    }
    /// check for extra rules
    pub fn check(&self) -> Result<(), ParserError> {
        self.stmt().check()
    }
}

/// Column count
pub enum ColumnCount {
    /// With `SELECT *` / EXPLAIN / PRAGMA
    Dynamic,
    ///
    Fixed(usize),
    /// No column
    None,
}

impl ColumnCount {
    fn incr(&mut self) {
        if let ColumnCount::Fixed(n) = self {
            *n += 1;
        }
    }
}

impl Stmt {
    /// Like `sqlite3_column_count` but more limited
    pub fn column_count(&self) -> ColumnCount {
        match self {
            Stmt::Delete {
                returning: Some(returning),
                ..
            } => column_count(returning),
            Stmt::Insert {
                returning: Some(returning),
                ..
            } => column_count(returning),
            Stmt::Pragma(..) => ColumnCount::Dynamic,
            Stmt::Select(s) => s.column_count(),
            Stmt::Update {
                returning: Some(returning),
                ..
            } => column_count(returning),
            _ => ColumnCount::None,
        }
    }

    /// Like `sqlite3_stmt_readonly`
    pub fn readonly(&self) -> bool {
        match self {
            Stmt::Attach { .. } => true,
            Stmt::Begin(..) => true,
            Stmt::Commit(..) => true,
            Stmt::Detach(..) => true,
            Stmt::Pragma(..) => true, // TODO check all
            Stmt::Reindex { .. } => true,
            Stmt::Release(..) => true,
            Stmt::Rollback { .. } => true,
            Stmt::Savepoint(..) => true,
            Stmt::Select(..) => true,
            _ => false,
        }
    }

    /// check for extra rules
    pub fn check(&self) -> Result<(), ParserError> {
        match self {
            Stmt::CreateTable { tbl_name, body, .. } => body.check(tbl_name),
            Stmt::CreateView {
                view_name,
                columns: Some(columns),
                select,
                ..
            } => {
                // SQLite3 engine renames duplicates:
                for (i, c) in columns.iter().enumerate() {
                    for o in &columns[i + 1..] {
                        if c.col_name == o.col_name {
                            return Err(ParserError::Custom(format!(
                                "duplicate column name: {}",
                                c.col_name,
                            )));
                        }
                    }
                }
                // SQLite3 engine raises this error later (not while parsing):
                match select.column_count() {
                    ColumnCount::Fixed(n) if n != columns.len() => {
                        Err(ParserError::Custom(format!(
                            "expected {} columns for {} but got {}",
                            columns.len(),
                            view_name,
                            n
                        )))
                    }
                    _ => Ok(()),
                }
            }
            Stmt::Insert {
                columns: Some(columns),
                body: InsertBody::Select(select, ..),
                ..
            } => {
                match select.body.select.column_count() {
                    ColumnCount::Fixed(n) if n != columns.len() => Err(ParserError::Custom(
                        format!("{} values for {} columns", n, columns.len()),
                    )),
                    _ => Ok(()),
                }
            }
            Stmt::Insert {
                columns: Some(columns),
                body: InsertBody::DefaultValues,
                ..
            } => Err(ParserError::Custom(format!(
                "0 values for {} columns",
                columns.len()
            ))),
            Stmt::Update {
                order_by: Some(_),
                limit: None,
                ..
            } => Err(ParserError::Custom(
                "ORDER BY without LIMIT on UPDATE".to_owned(),
            )),
            _ => Ok(()),
        }
    }
}

impl CreateTableBody {
    /// check for extra rules
    pub fn check(&self, tbl_name: &QualifiedName) -> Result<(), ParserError> {
        if let CreateTableBody::ColumnsAndConstraints {
            columns,
            constraints,
            options,
        } = self
        {
            // TODO columns.constraints : check no duplicate names
            // TODO constraints: check no duplicated names, use only valid column names
            if options.contains(TableOptions::STRICT) {
                for c in columns {
                    match &c.col_type {
                        Some(Type { name, .. }) => {
                            // The datatype must be one of following: INT INTEGER REAL TEXT BLOB ANY
                            if !(name.eq_ignore_ascii_case("INT")
                                || name.eq_ignore_ascii_case("INTEGER")
                                || name.eq_ignore_ascii_case("REAL")
                                || name.eq_ignore_ascii_case("TEXT")
                                || name.eq_ignore_ascii_case("BLOB")
                                || name.eq_ignore_ascii_case("ANY"))
                            {
                                return Err(ParserError::Custom(format!(
                                    "unknown datatype for {}.{}: \"{}\"",
                                    tbl_name, c.col_name, name
                                )));
                            }
                        }
                        _ => {
                            // Every column definition must specify a datatype for that column. The freedom to specify a column without a datatype is removed.
                            return Err(ParserError::Custom(format!(
                                "missing datatype for {}.{}",
                                tbl_name, c.col_name
                            )));
                        }
                    }
                }
            }
            if options.contains(TableOptions::WITHOUT_ROWID) && !self.has_primary_key() {
                return Err(ParserError::Custom(format!(
                    "PRIMARY KEY missing on table {}",
                    tbl_name,
                )));
            }
        }
        Ok(())
    }

    /// explicit primary key constraint ?
    pub fn has_primary_key(&self) -> bool {
        if let CreateTableBody::ColumnsAndConstraints {
            columns,
            constraints,
            ..
        } = self
        {
            for col in columns {
                for c in &col.constraints {
                    if let ColumnConstraint::PrimaryKey { .. } = c.constraint {
                        return true;
                    }
                }
            }
            if let Some(constraints) = constraints {
                for c in constraints {
                    if let TableConstraint::PrimaryKey { .. } = c.constraint {
                        return true;
                    }
                }
            }
        }
        false
    }
}

impl Select {
    /// Like `sqlite3_column_count` but more limited
    pub fn column_count(&self) -> ColumnCount {
        self.body.select.column_count()
    }
}

impl OneSelect {
    /// Like `sqlite3_column_count` but more limited
    pub fn column_count(&self) -> ColumnCount {
        match self {
            OneSelect::Select { columns, .. } => column_count(columns),
            OneSelect::Values(values) => {
                assert!(!values.is_empty()); // TODO Validate
                ColumnCount::Fixed(values[0].len())
            }
        }
    }

    pub fn push(values: &mut [Vec<Expr>], v: Vec<Expr>) -> Result<(), ParserError> {
        if values[0].len() != v.len() {
            return Err(ParserError::Custom(
                "all VALUES must have the same number of terms".to_owned(),
            ));
        }
        Ok(())
    }
}

impl Display for QualifiedName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.to_fmt(f)
    }
}

impl ResultColumn {
    fn column_count(&self) -> ColumnCount {
        match self {
            ResultColumn::Expr(..) => ColumnCount::Fixed(1),
            _ => ColumnCount::Dynamic,
        }
    }
}
fn column_count(cols: &[ResultColumn]) -> ColumnCount {
    assert!(!cols.is_empty());
    let mut count = ColumnCount::Fixed(0);
    for col in cols {
        match col.column_count() {
            ColumnCount::Fixed(_) => count.incr(),
            _ => return ColumnCount::Dynamic,
        }
    }
    count
}
