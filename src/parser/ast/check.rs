//! Check for additional syntax error
use crate::ast::*;
use crate::custom_err;

impl Cmd {
    /// Statement accessor
    pub fn stmt(&self) -> &Stmt {
        match self {
            Self::Explain(stmt) => stmt,
            Self::ExplainQueryPlan(stmt) => stmt,
            Self::Stmt(stmt) => stmt,
        }
    }
    /// Like `sqlite3_column_count` but more limited
    pub fn column_count(&self) -> ColumnCount {
        match self {
            Self::Explain(_) => ColumnCount::Fixed(8),
            Self::ExplainQueryPlan(_) => ColumnCount::Fixed(4),
            Self::Stmt(stmt) => stmt.column_count(),
        }
    }
    /// Like `sqlite3_stmt_isexplain`
    pub fn is_explain(&self) -> bool {
        matches!(self, Self::Explain(_) | Self::ExplainQueryPlan(_))
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
    // The default setting for SQLITE_MAX_COLUMN is 2000. You can change it at compile time to values as large as 32767.
    Fixed(u16),
    /// No column
    None,
}

impl ColumnCount {
    fn incr(&mut self) {
        if let Self::Fixed(n) = self {
            *n += 1;
        }
    }
}

impl Stmt {
    /// Like `sqlite3_column_count` but more limited
    pub fn column_count(&self) -> ColumnCount {
        match self {
            Self::Delete {
                returning: Some(returning),
                ..
            } => column_count(returning),
            Self::Insert {
                returning: Some(returning),
                ..
            } => column_count(returning),
            Self::Pragma(..) => ColumnCount::Dynamic,
            Self::Select(s) => s.column_count(),
            Self::Update {
                returning: Some(returning),
                ..
            } => column_count(returning),
            _ => ColumnCount::None,
        }
    }

    /// Like `sqlite3_stmt_readonly`
    pub fn readonly(&self) -> bool {
        match self {
            Self::Attach { .. } => true,
            Self::Begin(..) => true,
            Self::Commit(..) => true,
            Self::Detach(..) => true,
            Self::Pragma(..) => true, // TODO check all
            Self::Reindex { .. } => true,
            Self::Release(..) => true,
            Self::Rollback { .. } => true,
            Self::Savepoint(..) => true,
            Self::Select(..) => true,
            _ => false,
        }
    }

    /// check for extra rules
    pub fn check(&self) -> Result<(), ParserError> {
        match self {
            Self::AlterTable(old_name, AlterTableBody::RenameTo(new_name)) => {
                if *new_name == old_name.name {
                    return Err(custom_err!(
                        "there is already another table or index with this name: {}",
                        new_name
                    ));
                }
                Ok(())
            }
            Self::AlterTable(.., AlterTableBody::AddColumn(cd)) => {
                if cd.flags.contains(ColFlags::PRIMKEY) {
                    return Err(custom_err!("Cannot add a PRIMARY KEY column"));
                } else if cd.flags.contains(ColFlags::UNIQUE) {
                    return Err(custom_err!("Cannot add a UNIQUE column"));
                }
                Ok(())
            }
            Self::CreateIndex { idx_name, .. } => check_reserved_name(idx_name),
            Self::CreateTable {
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
            Self::CreateTrigger { trigger_name, .. } => check_reserved_name(trigger_name),
            Self::CreateView {
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
                    ColumnCount::Fixed(n) if n as usize != columns.len() => Err(custom_err!(
                        "expected {} columns for {} but got {}",
                        columns.len(),
                        view_name,
                        n
                    )),
                    _ => Ok(()),
                }
            }
            Self::Delete {
                order_by: Some(_),
                limit: None,
                ..
            } => Err(custom_err!("ORDER BY without LIMIT on DELETE")),
            Self::Insert {
                columns: Some(columns),
                body: InsertBody::Select(select, ..),
                ..
            } => match select.body.select.column_count() {
                ColumnCount::Fixed(n) if n as usize != columns.len() => {
                    Err(custom_err!("{} values for {} columns", n, columns.len()))
                }
                _ => Ok(()),
            },
            Self::Insert {
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
        if let Self::ColumnsAndConstraints {
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
            Self::Select { columns, .. } => column_count(columns),
            Self::Values(values) => {
                assert!(!values.is_empty()); // TODO Validate
                ColumnCount::Fixed(u16::try_from(values[0].len()).unwrap())
            }
        }
    }
}

impl ResultColumn {
    fn column_count(&self) -> ColumnCount {
        match self {
            Self::Expr(..) => ColumnCount::Fixed(1),
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
