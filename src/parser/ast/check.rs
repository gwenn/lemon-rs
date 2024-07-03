//! Check for additional syntax error
use crate::ast::*;
use crate::custom_err;
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
    /// Like `sqlite3_stmt_isexplain`
    pub fn is_explain(&self) -> bool {
        matches!(self, Cmd::Explain(_) | Cmd::ExplainQueryPlan(_))
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
    /// With `SELECT *` / PRAGMA
    Dynamic,
    /// Constant count
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
            Stmt::AlterTable(old_name, AlterTableBody::RenameTo(new_name)) => {
                if *new_name == old_name.name {
                    return Err(custom_err!(
                        "there is already another table or index with this name: {}",
                        new_name
                    ));
                }
                Ok(())
            }
            Stmt::AlterTable(.., AlterTableBody::AddColumn(cd)) => {
                if cd.flags.contains(ColFlags::PRIMKEY) {
                    return Err(custom_err!("Cannot add a PRIMARY KEY column"));
                } else if cd.flags.contains(ColFlags::UNIQUE) {
                    return Err(custom_err!("Cannot add a UNIQUE column"));
                }
                Ok(())
            }
            Stmt::CreateIndex { idx_name, .. } => check_reserved_name(idx_name),
            Stmt::CreateTable {
                temporary,
                tbl_name,
                body,
                ..
            } => {
                check_reserved_name(tbl_name)?;
                if *temporary {
                    if let Some(ref db_name) = tbl_name.db_name {
                        if db_name != "TEMP" {
                            return Err(custom_err!("temporary table name must be unqualified"));
                        }
                    }
                }
                body.check(tbl_name)
            }
            Stmt::CreateTrigger { trigger_name, .. } => check_reserved_name(trigger_name),
            Stmt::CreateView {
                view_name,
                columns: Some(columns),
                select,
                ..
            } => {
                check_reserved_name(view_name)?;
                // SQLite3 engine renames duplicates:
                for (i, c) in columns.iter().enumerate() {
                    for o in &columns[i + 1..] {
                        if c.col_name == o.col_name {
                            return Err(custom_err!("duplicate column name: {}", c.col_name,));
                        }
                    }
                }
                // SQLite3 engine raises this error later (not while parsing):
                match select.column_count() {
                    ColumnCount::Fixed(n) if n != columns.len() => Err(custom_err!(
                        "expected {} columns for {} but got {}",
                        columns.len(),
                        view_name,
                        n
                    )),
                    _ => Ok(()),
                }
            }
            Stmt::Delete {
                order_by: Some(_),
                limit: None,
                ..
            } => Err(custom_err!("ORDER BY without LIMIT on DELETE")),
            Stmt::Insert {
                columns: Some(columns),
                body: InsertBody::Select(select, ..),
                ..
            } => match select.body.select.column_count() {
                ColumnCount::Fixed(n) if n != columns.len() => {
                    Err(custom_err!("{} values for {} columns", n, columns.len()))
                }
                _ => Ok(()),
            },
            Stmt::Insert {
                columns: Some(columns),
                body: InsertBody::DefaultValues,
                ..
            } => Err(custom_err!("0 values for {} columns", columns.len())),
            _ => Ok(()),
        }
    }
}

fn check_reserved_name(name: &QualifiedName) -> Result<(), ParserError> {
    if name.name.is_reserved() {
        return Err(custom_err!(
            "object name reserved for internal use: {}",
            name.name
        ));
    }
    Ok(())
}

impl CreateTableBody {
    /// check for extra rules
    pub fn check(&self, tbl_name: &QualifiedName) -> Result<(), ParserError> {
        if let CreateTableBody::ColumnsAndConstraints {
            columns,
            constraints,
            flags,
        } = self
        {
            let mut generated_count = 0;
            for c in columns.values() {
                if c.flags.intersects(ColFlags::GENERATED) {
                    generated_count += 1;
                }
            }
            if generated_count == columns.len() {
                return Err(custom_err!("must have at least one non-generated column"));
            }

            if flags.contains(TabFlags::Strict) {
                for c in columns.values() {
                    match &c.col_type {
                        Some(Type {
                            name,
                            size: Some(_),
                        }) => {
                            return Err(custom_err!(
                                "unknown datatype for {}.{}: \"{}(...)\"",
                                tbl_name,
                                c.col_name,
                                unquote(name).0,
                            ));
                        }
                        // FIXME unquote
                        Some(Type { name, .. }) => {
                            let name = unquote(name).0;
                            // The datatype must be one of following: INT INTEGER REAL TEXT BLOB ANY
                            if !(name.eq_ignore_ascii_case("INT")
                                || name.eq_ignore_ascii_case("INTEGER")
                                || name.eq_ignore_ascii_case("REAL")
                                || name.eq_ignore_ascii_case("TEXT")
                                || name.eq_ignore_ascii_case("BLOB")
                                || name.eq_ignore_ascii_case("ANY"))
                            {
                                return Err(custom_err!(
                                    "unknown datatype for {}.{}: \"{}\"",
                                    tbl_name,
                                    c.col_name,
                                    name
                                ));
                            }
                        }
                        _ => {
                            // Every column definition must specify a datatype for that column. The freedom to specify a column without a datatype is removed.
                            return Err(custom_err!(
                                "missing datatype for {}.{}",
                                tbl_name,
                                c.col_name
                            ));
                        }
                    }
                }
            }
            if flags.contains(TabFlags::WithoutRowid) && !flags.contains(TabFlags::HasPrimaryKey) {
                return Err(custom_err!("PRIMARY KEY missing on table {}", tbl_name,));
            }
        }
        Ok(())
    }
}

impl<'a> IntoIterator for &'a ColumnDefinition {
    type Item = &'a ColumnConstraint;
    type IntoIter = std::iter::Map<
        std::slice::Iter<'a, NamedColumnConstraint>,
        fn(&'a NamedColumnConstraint) -> &'a ColumnConstraint,
    >;

    fn into_iter(self) -> Self::IntoIter {
        self.constraints.iter().map(|nc| &nc.constraint)
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
    /// Check all VALUES have the same number of terms
    pub fn push(values: &mut Vec<Vec<Expr>>, v: Vec<Expr>) -> Result<(), ParserError> {
        if values[0].len() != v.len() {
            return Err(custom_err!("all VALUES must have the same number of terms"));
        }
        values.push(v);
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
