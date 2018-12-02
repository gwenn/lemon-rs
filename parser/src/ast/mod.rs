//! Abstract Syntax Tree

use dialect::{is_identifier, is_keyword};
use std::fmt::{Display, Formatter, Result, Write};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Cmd {
    Explain(Stmt),
    ExplainQueryPlan(Stmt),
    Stmt(Stmt),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Stmt {
    // table name, body
    AlterTable(QualifiedName, AlterTableBody),
    // object name
    Analyze(Option<QualifiedName>),
    Attach {
        // TODO distinction between ATTACH and ATTACH DATABASE
        expr: Expr,
        db_name: Expr,
        key: Option<Expr>,
    },
    // tx type, tx name
    Begin(Option<TransactionType>, Option<Name>),
    // tx name
    Commit(Option<Name>), // TODO distinction between COMMIT and END
    CreateIndex {
        unique: bool,
        if_not_exists: bool,
        idx_name: QualifiedName,
        tbl_name: Name,
        columns: Vec<SortedColumn>,
        where_clause: Option<Expr>,
    },
    CreateTable {
        temporary: bool, // TODO distinction between TEMP and TEMPORARY
        if_not_exists: bool,
        tbl_name: QualifiedName,
        body: CreateTableBody,
    },
    CreateTrigger {
        temporary: bool,
        if_not_exists: bool,
        trigger_name: QualifiedName,
        time: Option<TriggerTime>,
        event: TriggerEvent,
        tbl_name: QualifiedName,
        for_each_row: bool,
        when_clause: Option<Expr>,
        commands: Vec<TriggerCmd>,
    },
    CreateView {
        temporary: bool,
        if_not_exists: bool,
        view_name: QualifiedName,
        columns: Option<Vec<IndexedColumn>>,
        select: Select,
    },
    CreateVirtualTable {
        if_not_exists: bool,
        tbl_name: QualifiedName,
        module_name: Name,
        args: Option<Vec<Expr>>, // TODO Validate Expr
    },
    Delete {
        with: Option<With>,
        tbl_name: QualifiedName,
        indexed: Option<Indexed>,
        where_clause: Option<Expr>,
        order_by: Option<Vec<SortedColumn>>,
        limit: Option<Limit>,
    },
    // db name
    Detach(Expr), // TODO distinction between DETACH and DETACH DATABASE
    DropIndex {
        if_exists: bool,
        idx_name: QualifiedName,
    },
    DropTable {
        if_exists: bool,
        tbl_name: QualifiedName,
    },
    DropTrigger {
        if_exists: bool,
        trigger_name: QualifiedName,
    },
    DropView {
        if_exists: bool,
        view_name: QualifiedName,
    },
    Insert {
        with: Option<With>,
        or_conflict: Option<ResolveType>, // TODO distinction between REPLACE and INSERT OR REPLACE
        tbl_name: QualifiedName,
        columns: Option<Vec<Name>>,
        body: InsertBody,
    },
    // pragma name, body
    Pragma(QualifiedName, Option<PragmaBody>),
    Reindex {
        obj_name: Option<QualifiedName>,
    },
    // savepoint name
    Release(Name), // TODO distinction between RELEASE and RELEASE SAVEPOINT
    Rollback {
        tx_name: Option<Name>,
        savepoint_name: Option<Name>, // TODO distinction between TO and TO SAVEPOINT
    },
    // savepoint name
    Savepoint(Name),
    Select(Select),
    Update {
        with: Option<With>,
        or_conflict: Option<ResolveType>,
        tbl_name: QualifiedName,
        indexed: Option<Indexed>,
        sets: Vec<Set>,
        where_clause: Option<Expr>,
        order_by: Option<Vec<SortedColumn>>,
        limit: Option<Limit>,
    },
    // database name
    Vacuum(Option<Name>),
}

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Stmt::AlterTable(tbl_name, body) => {
                f.write_str("ALTER TABLE ")?;
                tbl_name.fmt(f)?;
                f.write_char(' ')?;
                body.fmt(f)
            }
            Stmt::Analyze(obj_name) => {
                f.write_str("ANALYZE")?;
                if let Some(obj_name) = obj_name {
                    f.write_char(' ')?;
                    obj_name.fmt(f)?;
                }
                Ok(())
            }
            Stmt::Attach { expr, db_name, key } => {
                f.write_str("ATTACH ")?;
                expr.fmt(f)?;
                f.write_str(" AS ")?;
                db_name.fmt(f)?;
                if let Some(key) = key {
                    f.write_str(" KEY ")?;
                    key.fmt(f)?;
                }
                Ok(())
            }
            Stmt::Begin(tx_type, tx_name) => {
                f.write_str("BEGIN")?;
                if let Some(tx_type) = tx_type {
                    f.write_char(' ')?;
                    tx_type.fmt(f)?;
                }
                if let Some(tx_name) = tx_name {
                    f.write_str(" TRANSACTION ")?;
                    tx_name.fmt(f)?;
                }
                Ok(())
            }
            Stmt::Commit(tx_name) => {
                f.write_str("COMMIT ")?;
                if let Some(tx_name) = tx_name {
                    f.write_str(" TRANSACTION ")?;
                    tx_name.fmt(f)?;
                }
                Ok(())
            }
            Stmt::CreateIndex {
                unique,
                if_not_exists,
                idx_name,
                tbl_name,
                columns,
                where_clause,
            } => {
                unimplemented!()
            }
            Stmt::CreateTable {
                temporary,
                if_not_exists,
                tbl_name,
                body,
            } => {
                unimplemented!()
            }
            Stmt::CreateTrigger {
                temporary,
                if_not_exists,
                trigger_name,
                time,
                event,
                tbl_name,
                for_each_row,
                when_clause,
                commands,
            } => {
                unimplemented!()
            }
            Stmt::CreateView {
                temporary,
                if_not_exists,
                view_name,
                columns,
                select,
            } => {
                unimplemented!()
            }
            Stmt::CreateVirtualTable {
                if_not_exists,
                tbl_name,
                module_name,
                args,
            } => {
                unimplemented!()
            }
            Stmt::Delete {
                with,
                tbl_name,
                indexed,
                where_clause,
                order_by,
                limit,
            } => {
                if let Some(with) = with {
                    with.fmt(f)?;
                    f.write_char(' ')?;
                }
                f.write_str("DELETE FROM ")?;
                tbl_name.fmt(f)?;
                if let Some(indexed) = indexed {
                    f.write_char(' ')?;
                    indexed.fmt(f)?;
                }
                if let Some(where_clause) = where_clause {
                    f.write_str(" WHERE ")?;
                    where_clause.fmt(f)?;
                }
                if let Some(order_by) = order_by {
                    f.write_str(" ORDER BY ")?;
                    comma(order_by, f)?;
                }
                if let Some(limit) = limit {
                    f.write_char(' ')?;
                    limit.fmt(f)?;
                }
                Ok(())
            }
            Stmt::Detach(expr) => {
                f.write_str("DETACH ")?;
                expr.fmt(f)
            }
            Stmt::DropIndex {
                if_exists,
                idx_name,
            } => {
                f.write_str("DROP INDEX ")?;
                if *if_exists {
                    f.write_str("IF EXISTS ")?;
                }
                idx_name.fmt(f)
            }
            Stmt::DropTable {
                if_exists,
                tbl_name,
            } => {
                f.write_str("DROP TABLE ")?;
                if *if_exists {
                    f.write_str("IF EXISTS ")?;
                }
                tbl_name.fmt(f)
            }
            Stmt::DropTrigger {
                if_exists,
                trigger_name,
            } => {
                f.write_str("DROP TRIGGER ")?;
                if *if_exists {
                    f.write_str("IF EXISTS ")?;
                }
                trigger_name.fmt(f)
            }
            Stmt::DropView {
                if_exists,
                view_name,
            } => {
                f.write_str("DROP VIEW ")?;
                if *if_exists {
                    f.write_str("IF EXISTS ")?;
                }
                view_name.fmt(f)
            }
            Stmt::Insert {
                with,
                or_conflict,
                tbl_name,
                columns,
                body,
            } => {
                if let Some(with) = with {
                    with.fmt(f)?;
                    f.write_char(' ')?;
                }
                if let Some(ResolveType::Replace) = or_conflict {
                    f.write_str("REPLACE")?;
                } else {
                    f.write_str("INSERT")?;
                    if let Some(or_conflict) = or_conflict {
                        f.write_str(" OR ")?;
                        or_conflict.fmt(f)?;
                    }
                }
                f.write_str(" INTO ")?;
                tbl_name.fmt(f)?;
                if let Some(columns) = columns {
                    f.write_str(" (")?;
                    comma(columns, f)?;
                    f.write_char(')')?;
                }
                f.write_char(' ')?;
                body.fmt(f)
            }
            Stmt::Pragma(name, value) => {
                f.write_str("PRAGMA ")?;
                name.fmt(f)?;
                if let Some(value) = value {
                    f.write_char('(')?;
                    value.fmt(f)?;
                    f.write_char(')')?;
                }
                Ok(())
            }
            Stmt::Reindex { obj_name } => {
                f.write_str("REINDEX")?;
                if let Some(obj_name) = obj_name {
                    f.write_char(' ')?;
                    obj_name.fmt(f)?;
                }
                Ok(())
            }
            Stmt::Release(name) => {
                f.write_str("RELEASE ")?;
                name.fmt(f)
            }
            Stmt::Rollback {
                tx_name,
                savepoint_name,
            } => {
                f.write_str("ROLLBACK")?;
                if let Some(tx_name) = tx_name {
                    f.write_str(" TRANSACTION ")?;
                    tx_name.fmt(f)?;
                }
                if let Some(savepoint_name) = savepoint_name {
                    f.write_str(" TO ")?;
                    savepoint_name.fmt(f)?;
                }
                Ok(())
            }
            Stmt::Savepoint(name) => {
                f.write_str("SAVEPOINT ")?;
                name.fmt(f)
            }
            Stmt::Select(select) => select.fmt(f),
            Stmt::Update {
                with,
                or_conflict,
                tbl_name,
                indexed,
                sets,
                where_clause,
                order_by,
                limit,
            } => {
                if let Some(with) = with {
                    with.fmt(f)?;
                    f.write_char(' ')?;
                }
                f.write_str("UPDATE ")?;
                if let Some(or_conflict) = or_conflict {
                    f.write_str("OR ")?;
                    or_conflict.fmt(f)?;
                    f.write_char(' ')?;
                }
                tbl_name.fmt(f)?;
                if let Some(indexed) = indexed {
                    f.write_char(' ')?;
                    indexed.fmt(f)?;
                }
                f.write_str(" SET ")?;
                comma(sets, f)?;
                if let Some(where_clause) = where_clause {
                    f.write_str(" WHERE ")?;
                    where_clause.fmt(f)?;
                }
                if let Some(order_by) = order_by {
                    f.write_str(" ORDER BY ")?;
                    comma(order_by, f)?;
                }
                if let Some(limit) = limit {
                    f.write_char(' ')?;
                    limit.fmt(f)?;
                }
                Ok(())
            }
            Stmt::Vacuum(name) => {
                f.write_str("VACUUM")?;
                if let Some(ref name) = name {
                    f.write_char(' ')?;
                    name.fmt(f)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    Between {
        lhs: Box<Expr>,
        not: bool,
        start: Box<Expr>,
        end: Box<Expr>,
    },
    Binary(Box<Expr>, Operator, Box<Expr>),
    // CASE expression
    Case {
        base: Option<Box<Expr>>,
        when_then_pairs: Vec<(Box<Expr>, Box<Expr>)>,
        else_expr: Option<Box<Expr>>,
    },
    // CAST expression
    Cast {
        expr: Box<Expr>,
        type_name: Type,
    },
    // COLLATE expression
    Collate(Box<Expr>, Name),
    // schema-name.table-name.column-name
    DoublyQualified(Name, Name, Name),
    // EXISTS subquery
    Exists(Box<Select>),
    // call to a built-in function
    FunctionCall {
        name: Name,
        distinctness: Option<Distinctness>,
        args: Option<Vec<Box<Expr>>>,
    }, // TODO overClause
    // Function call expression with '*' as arg
    FunctionCallStar(Name), // TODO overClause
    // Identifier
    Id(Name),
    InList {
        lhs: Box<Expr>,
        not: bool,
        rhs: Option<Vec<Box<Expr>>>,
    },
    InSelect {
        lhs: Box<Expr>,
        not: bool,
        rhs: Box<Select>,
    },
    InTable {
        lhs: Box<Expr>,
        not: bool,
        rhs: QualifiedName,
        args: Option<Vec<Box<Expr>>>,
    },
    IsNull(Box<Expr>),
    Like {
        lhs: Box<Expr>,
        not: bool,
        op: LikeOperator,
        rhs: Box<Expr>,
        escape: Option<Box<Expr>>,
    },
    // Literal expression
    Literal(Literal),
    // "NOT NULL" or "NOTNULL"
    NotNull(Box<Expr>),
    // Parenthesized subexpression
    Parenthesized(Vec<Box<Expr>>),
    Qualified(Name, Name),
    // RAISE function call
    Raise(ResolveType, Option<String>),
    // Subquery expression
    Subquery(Box<Select>),
    // Unary expression
    Unary(UnaryOperator, Box<Expr>),
    // Parameters
    Variable(String),
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Expr::Between {
                lhs,
                not,
                start,
                end,
            } => {
                lhs.fmt(f)?;
                if *not {
                    f.write_str(" NOT")?;
                }
                f.write_str(" BETWEEN ")?;
                start.fmt(f)?;
                f.write_str(" AND ")?;
                end.fmt(f)
            }
            Expr::Binary(lhs, op, rhs) => {
                lhs.fmt(f)?;
                f.write_char(' ')?;
                op.fmt(f)?;
                f.write_char(' ')?;
                rhs.fmt(f)
            }
            Expr::Case {
                base,
                when_then_pairs,
                else_expr,
            } => {
                f.write_str("CASE")?;
                if let Some(ref base) = base {
                    f.write_char(' ')?;
                    base.fmt(f)?;
                }
                for (when, then) in when_then_pairs {
                    f.write_str(" WHEN ")?;
                    when.fmt(f)?;
                    f.write_str(" THEN ")?;
                    then.fmt(f)?;
                }
                if let Some(ref else_expr) = else_expr {
                    f.write_str(" ELSE ")?;
                    else_expr.fmt(f)?;
                }
                f.write_str(" END")
            }
            Expr::Cast { expr, type_name } => {
                f.write_str("CAST(")?;
                expr.fmt(f)?;
                f.write_str(" AS ")?;
                type_name.fmt(f)?;
                f.write_char(')')
            }
            Expr::Collate(expr, collation) => {
                expr.fmt(f)?;
                f.write_str(" COLLATE ")?;
                collation.fmt(f)
            }
            Expr::DoublyQualified(db_name, tbl_name, col_name) => {
                db_name.fmt(f)?;
                f.write_char('.')?;
                tbl_name.fmt(f)?;
                f.write_char('.')?;
                col_name.fmt(f)
            }
            Expr::Exists(subquery) => {
                f.write_str("EXISTS (")?;
                subquery.fmt(f)?;
                f.write_char(')')
            }
            Expr::FunctionCall {
                name,
                distinctness,
                args,
            } => {
                name.fmt(f)?;
                f.write_char('(')?;
                if let Some(distinctness) = distinctness {
                    distinctness.fmt(f)?;
                    f.write_char(' ')?;
                }
                if let Some(args) = args {
                    comma(args, f)?;
                }
                f.write_char(')')
            }
            Expr::FunctionCallStar(name) => {
                name.fmt(f)?;
                f.write_str("(*)")
            }
            Expr::Id(name) => name.fmt(f),
            Expr::InList { lhs, not, rhs } => {
                lhs.fmt(f)?;
                if *not {
                    f.write_str(" NOT")?;
                }
                f.write_str(" IN (")?;
                if let Some(rhs) = rhs {
                    comma(rhs, f)?;
                }
                f.write_char(')')
            }
            Expr::InSelect { lhs, not, rhs } => {
                lhs.fmt(f)?;
                if *not {
                    f.write_str(" NOT")?;
                }
                f.write_str(" IN (")?;
                rhs.fmt(f)?;
                f.write_char(')')
            }
            Expr::InTable {
                lhs,
                not,
                rhs,
                args,
            } => {
                lhs.fmt(f)?;
                if *not {
                    f.write_str(" NOT")?;
                }
                f.write_str(" IN ")?;
                rhs.fmt(f)?;
                if let Some(args) = args {
                    f.write_char('(')?;
                    comma(args, f)?;
                    f.write_char(')')?;
                }
                Ok(())
            }
            Expr::IsNull(sub_expr) => {
                sub_expr.fmt(f)?;
                f.write_str(" ISNULL")
            }
            Expr::Like {
                lhs,
                not,
                op,
                rhs,
                escape,
            } => {
                lhs.fmt(f)?;
                f.write_char(' ')?;
                if *not {
                    f.write_str("NOT ")?;
                }
                op.fmt(f)?;
                f.write_char(' ')?;
                rhs.fmt(f)?;
                if let Some(escape) = escape {
                    f.write_str(" ESCAPE ")?;
                    escape.fmt(f)?;
                }
                Ok(())
            }
            Expr::Literal(lit) => lit.fmt(f),
            Expr::NotNull(sub_expr) => {
                sub_expr.fmt(f)?;
                f.write_str(" NOTNULL")
            }
            Expr::Parenthesized(exprs) => {
                f.write_char('(')?;
                comma(exprs, f)?;
                f.write_char(')')
            }
            Expr::Qualified(qualifier, qualified) => {
                qualifier.fmt(f)?;
                f.write_char('.')?;
                qualified.fmt(f)
            }
            Expr::Raise(rt, err) => {
                f.write_str("RAISE(")?;
                rt.fmt(f)?;
                if let Some(err) = err {
                    f.write_str(", ")?;
                    single_quote(err, f)?;
                }
                f.write_char(')')
            }
            Expr::Subquery(query) => {
                f.write_char('(')?;
                query.fmt(f)?;
                f.write_char(')')
            }
            Expr::Unary(op, sub_expr) => {
                op.fmt(f)?;
                if let UnaryOperator::Not = op {
                    f.write_char(' ')?;
                } else if let Expr::Unary(_, _) = sub_expr.as_ref() {
                    f.write_char(' ')?;
                }
                sub_expr.fmt(f)
            }
            Expr::Variable(var) => match var.chars().next() {
                Some(c) if c == '$' || c == '@' || c == ':' => f.write_str(var),
                Some(_) => {
                    f.write_char('?')?;
                    f.write_str(var)
                }
                None => f.write_char('?'),
            },
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    Numeric(String),
    String(String),
    Blob(String),
    Null,
    CurrentDate,
    CurrentTime,
    CurrentTimestamp,
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Literal::Numeric(ref num) => f.write_str(num),
            Literal::String(ref str) => single_quote(str, f),
            Literal::Blob(ref blob) => {
                f.write_char('X')?;
                single_quote(blob, f)
            }
            Literal::Null => f.write_str("NULL"),
            Literal::CurrentDate => f.write_str("CURRENT_DATE"),
            Literal::CurrentTime => f.write_str("CURRENT_TIME"),
            Literal::CurrentTimestamp => f.write_str("CURRENT_TIMESTAMP"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum LikeOperator {
    Glob,
    Like,
    Match,
    Regexp,
}

impl Display for LikeOperator {
    fn fmt(&self, f: &mut Formatter) -> Result {
        f.write_str(match self {
            LikeOperator::Glob => "GLOB",
            LikeOperator::Like => "LIKE",
            LikeOperator::Match => "MATCH",
            LikeOperator::Regexp => "REGEXP",
        })
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Operator {
    Add,
    And,
    BitwiseAnd,
    BitwiseOr,
    Concat, // String concatenation (||)
    Equals, // = or ==
    Divide,
    Greater,
    GreaterEquals,
    Is,
    IsNot,
    LeftShift,
    Less,
    LessEquals,
    Modulus,
    Multiply,
    NotEquals, // != or <>
    Or,
    RightShift,
    Substract,
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter) -> Result {
        f.write_str(match self {
            Operator::Add => "+",
            Operator::And => "AND",
            Operator::BitwiseAnd => "&",
            Operator::BitwiseOr => "|",
            Operator::Concat => "||",
            Operator::Equals => "=",
            Operator::Divide => "/",
            Operator::Greater => ">",
            Operator::GreaterEquals => ">=",
            Operator::Is => "IS",
            Operator::IsNot => "IS NOT",
            Operator::LeftShift => "<<",
            Operator::Less => "<",
            Operator::LessEquals => "<=",
            Operator::Modulus => "%",
            Operator::Multiply => "*",
            Operator::NotEquals => "<>",
            Operator::Or => "OR",
            Operator::RightShift => ">>",
            Operator::Substract => "-",
        })
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum UnaryOperator {
    // bitwise negation (~)
    BitwiseNot,
    // negative-sign
    Negative,
    // "NOT"
    Not,
    // positive-sign
    Positive,
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut Formatter) -> Result {
        f.write_str(match self {
            UnaryOperator::BitwiseNot => "~",
            UnaryOperator::Negative => "-",
            UnaryOperator::Not => "NOT",
            UnaryOperator::Positive => "+",
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Select {
    pub with: Option<With>,
    pub body: SelectBody,
    pub order_by: Option<Vec<SortedColumn>>,
    pub limit: Option<Limit>,
}

impl Display for Select {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if let Some(ref with) = self.with {
            with.fmt(f)?;
            f.write_char(' ')?;
        }
        self.body.fmt(f)?;
        if let Some(ref order_by) = self.order_by {
            f.write_str(" ORDER BY ")?;
            comma(order_by, f)?;
        }
        if let Some(ref limit) = self.limit {
            f.write_char(' ')?;
            limit.fmt(f)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SelectBody {
    pub select: OneSelect,
    pub compounds: Option<Vec<CompoundSelect>>,
}

impl Display for SelectBody {
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.select.fmt(f)?;
        if let Some(ref compounds) = self.compounds {
            for compound in compounds {
                f.write_char(' ')?;
                compound.fmt(f)?;
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CompoundSelect {
    pub operator: CompoundOperator,
    pub select: OneSelect,
}

impl Display for CompoundSelect {
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.operator.fmt(f)?;
        f.write_char(' ')?;
        self.select.fmt(f)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum CompoundOperator {
    Union,
    UnionAll,
    Except,
    Intersect,
}

impl Display for CompoundOperator {
    fn fmt(&self, f: &mut Formatter) -> Result {
        f.write_str(match self {
            CompoundOperator::Union => "UNION",
            CompoundOperator::UnionAll => "UNION ALL",
            CompoundOperator::Except => "EXCEPT",
            CompoundOperator::Intersect => "INTERSECT",
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OneSelect {
    Select {
        distinctness: Option<Distinctness>,
        columns: Vec<ResultColumn>,
        from: Option<FromClause>,
        where_clause: Option<Expr>,
        group_by: Option<GroupBy>, // TODO windowClause
    },
    Values(Vec<Vec<Expr>>),
}

impl Display for OneSelect {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            OneSelect::Select {
                distinctness,
                columns,
                from,
                where_clause,
                group_by,
            } => {
                f.write_str("SELECT")?;
                if let Some(ref distinctness) = distinctness {
                    f.write_char(' ')?;
                    distinctness.fmt(f)?;
                }
                f.write_char(' ')?;
                comma(columns, f)?;
                if let Some(ref from) = from {
                    f.write_str(" FROM ")?;
                    from.fmt(f)?;
                }
                if let Some(ref where_clause) = where_clause {
                    f.write_str(" WHERE ")?;
                    where_clause.fmt(f)?;
                }
                if let Some(ref group_by) = group_by {
                    f.write_char(' ')?;
                    group_by.fmt(f)?;
                }
                Ok(())
            }
            OneSelect::Values(values) => {
                for (i, vals) in values.iter().enumerate() {
                    if i == 0 {
                        f.write_str("VALUES (")?;
                    } else {
                        f.write_str(", (")?;
                    }
                    comma(vals, f)?;
                    f.write_char(')')?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FromClause {
    pub select: Box<SelectTable>,
    pub joins: Option<Vec<JoinedSelectTable>>,
}

impl Display for FromClause {
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.select.fmt(f)?;
        if let Some(ref joins) = self.joins {
            for join in joins {
                f.write_char(' ')?;
                join.fmt(f)?;
            }
        }
        Ok(())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Distinctness {
    Distinct,
    All,
}

impl Display for Distinctness {
    fn fmt(&self, f: &mut Formatter) -> Result {
        f.write_str(match self {
            Distinctness::Distinct => "DISTINCT",
            Distinctness::All => "ALL",
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ResultColumn {
    Expr(Expr, Option<As>),
    Star,
    // table name
    TableStar(Name),
}

impl Display for ResultColumn {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            ResultColumn::Expr(expr, alias) => {
                expr.fmt(f)?;
                if let Some(alias) = alias {
                    f.write_char(' ')?;
                    alias.fmt(f)?;
                }
                Ok(())
            }
            ResultColumn::Star => f.write_char('*'),
            ResultColumn::TableStar(tbl_name) => {
                tbl_name.fmt(f)?;
                f.write_str(".*")
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum As {
    As(Name),
    Elided(Name),
}

impl Display for As {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            As::As(ref name) => {
                f.write_str("AS ")?;
                name.fmt(f)
            }
            As::Elided(ref name) => name.fmt(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct JoinedSelectTable {
    pub operator: JoinOperator,
    pub table: SelectTable,
    pub constraint: Option<JoinConstraint>,
}

impl Display for JoinedSelectTable {
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.operator.fmt(f)?;
        f.write_char(' ')?;
        self.table.fmt(f)?;
        if let Some(ref constraint) = self.constraint {
            f.write_char(' ')?;
            constraint.fmt(f)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SelectTable {
    Table(QualifiedName, Option<As>, Option<Indexed>),
    TableCall(QualifiedName, Option<Vec<Expr>>, Option<As>),
    Select(Select, Option<As>),
    Sub(FromClause, Option<As>),
}

impl Display for SelectTable {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            SelectTable::Table(name, alias, indexed) => {
                name.fmt(f)?;
                if let Some(alias) = alias {
                    f.write_char(' ')?;
                    alias.fmt(f)?;
                }
                if let Some(indexed) = indexed {
                    f.write_char(' ')?;
                    indexed.fmt(f)?;
                }
                Ok(())
            }
            SelectTable::TableCall(name, exprs, alias) => {
                name.fmt(f)?;
                f.write_char('(')?;
                if let Some(exprs) = exprs {
                    comma(exprs, f)?;
                }
                f.write_char(')')?;
                if let Some(alias) = alias {
                    f.write_char(' ')?;
                    alias.fmt(f)?;
                }
                Ok(())
            }
            SelectTable::Select(select, alias) => {
                f.write_char('(')?;
                select.fmt(f)?;
                f.write_char(')')?;
                if let Some(alias) = alias {
                    f.write_char(' ')?;
                    alias.fmt(f)?;
                }
                Ok(())
            }
            SelectTable::Sub(from, alias) => {
                f.write_char('(')?;
                from.fmt(f)?;
                f.write_char(')')?;
                if let Some(alias) = alias {
                    f.write_char(' ')?;
                    alias.fmt(f)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum JoinOperator {
    Comma,
    TypedJoin {
        natural: bool,
        join_type: Option<JoinType>,
    },
}

impl Display for JoinOperator {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            JoinOperator::Comma => f.write_char(','),
            JoinOperator::TypedJoin { natural, join_type } => {
                if *natural {
                    f.write_str(" NATURAL")?;
                }
                if let Some(ref join_type) = join_type {
                    f.write_char(' ')?;
                    join_type.fmt(f)?;
                }
                f.write_str(" JOIN")
            }
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum JoinType {
    Left,
    LeftOuter,
    Inner,
    Cross,
}

impl Display for JoinType {
    fn fmt(&self, f: &mut Formatter) -> Result {
        f.write_str(match self {
            JoinType::Left => "LEFT",
            JoinType::LeftOuter => "LEFT OUTER",
            JoinType::Inner => "INNER",
            JoinType::Cross => "CROSS",
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum JoinConstraint {
    On(Expr),
    // col names
    Using(Vec<Name>),
}

impl Display for JoinConstraint {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            JoinConstraint::On(expr) => {
                f.write_str("ON ")?;
                expr.fmt(f)
            }
            JoinConstraint::Using(col_names) => {
                f.write_str("USING (")?;
                comma(col_names, f)?;
                f.write_char(')')
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GroupBy {
    pub exprs: Vec<Expr>,
    pub having: Option<Expr>,
}

impl Display for GroupBy {
    fn fmt(&self, f: &mut Formatter) -> Result {
        f.write_str("GROUP BY ")?;
        comma(&self.exprs, f)?;
        if let Some(ref having) = self.having {
            f.write_str(" HAVING ")?;
            having.fmt(f)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Name(String); // TODO distinction between Name and "Name"/[Name]/`Name`

impl Display for Name {
    fn fmt(&self, f: &mut Formatter) -> Result {
        double_quote(&self.0, f)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct QualifiedName {
    pub db_name: Option<Name>,
    pub name: Name,
    pub alias: Option<Name>,
}

impl Display for QualifiedName {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if let Some(ref db_name) = self.db_name {
            db_name.fmt(f)?;
            f.write_char('.')?;
        }
        self.name.fmt(f)?;
        if let Some(ref alias) = self.alias {
            f.write_str(" AS ")?;
            alias.fmt(f)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AlterTableBody {
    // new table name
    RenameTo(Name),
    AddColumn(ColumnDefinition), // TODO distinction between ADD and ADD COLUMN
    RenameColumn { old: Name, new: Name },
}

impl Display for AlterTableBody {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            AlterTableBody::RenameTo(name) => {
                f.write_str("RENAME TO ")?;
                name.fmt(f)
            }
            AlterTableBody::AddColumn(def) => {
                f.write_str("ADD COLUMN ")?;
                def.fmt(f)
            }
            AlterTableBody::RenameColumn { old, new } => {
                f.write_str("RENAME ")?;
                old.fmt(f)?;
                f.write_str(" TO ")?;
                new.fmt(f)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CreateTableBody {
    ColumnsAndConstraints {
        columns: Vec<ColumnDefinition>,
        constraints: Option<Vec<NamedTableConstraint>>,
        without: bool,
    },
    AsSelect(Select),
}

impl Display for CreateTableBody {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            CreateTableBody::ColumnsAndConstraints {
                columns,
                constraints,
                without,
            } => {
                f.write_char('(')?;
                comma(columns, f)?;
                if let Some(constraints) = constraints {
                    f.write_str(", ")?;
                    comma(constraints, f)?;
                }
                f.write_char(')')?;
                if *without {
                    f.write_str(" WITHOUT ROWID")?;
                }
                Ok(())
            }
            CreateTableBody::AsSelect(select) => {
                f.write_str(" AS ")?;
                select.fmt(f)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ColumnDefinition {
    pub col_name: Name,
    pub col_type: Option<Type>,
    pub constraints: Vec<NamedColumnConstraint>,
}

impl Display for ColumnDefinition {
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.col_name.fmt(f)?;
        if let Some(ref col_type) = self.col_type {
            f.write_char(' ')?;
            col_type.fmt(f)?;
        }
        for constraint in self.constraints.iter() {
            f.write_char(' ')?;
            constraint.fmt(f)?;
        }
        Ok(())
    }
}

// TODO ColumnNameAndType

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NamedColumnConstraint {
    pub name: Option<Name>,
    pub constraint: ColumnConstraint,
}

impl Display for NamedColumnConstraint {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if let Some(ref name) = self.name {
            f.write_str("CONSTRAINT ")?;
            name.fmt(f)?;
            f.write_char(' ')?;
        }
        self.constraint.fmt(f)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ColumnConstraint {
    PrimaryKey {
        order: Option<SortOrder>,
        conflict_clause: Option<ResolveType>,
        auto_increment: bool,
    },
    NotNull {
        nullable: bool,
        conflict_clause: Option<ResolveType>,
    },
    Unique(Option<ResolveType>),
    Check(Expr),
    Default(Expr),
    Collate {
        collation_name: Name,
    },
    ForeignKey {
        clause: ForeignKeyClause,
        deref_clause: Option<DeferSubclause>,
    },
}

impl Display for ColumnConstraint {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            ColumnConstraint::PrimaryKey {
                order,
                conflict_clause,
                auto_increment,
            } => {
                f.write_str("PRIMARY KEY")?;
                if let Some(order) = order {
                    f.write_char(' ')?;
                    order.fmt(f)?;
                }
                if let Some(conflict_clause) = conflict_clause {
                    f.write_str(" ON CONFLICT ")?;
                    conflict_clause.fmt(f)?;
                }
                if *auto_increment {
                    f.write_str(" AUTOINCREMENT")?;
                }
                Ok(())
            }
            ColumnConstraint::NotNull {
                nullable,
                conflict_clause,
            } => {
                if !nullable {
                    f.write_str("NOT ")?;
                }
                f.write_str("NULL")?;
                if let Some(conflict_clause) = conflict_clause {
                    f.write_str(" ON CONFLICT ")?;
                    conflict_clause.fmt(f)?;
                }
                Ok(())
            }
            ColumnConstraint::Unique(conflict_clause) => {
                f.write_str("UNIQUE")?;
                if let Some(conflict_clause) = conflict_clause {
                    f.write_str(" ON CONFLICT ")?;
                    conflict_clause.fmt(f)?;
                }
                Ok(())
            }
            ColumnConstraint::Check(expr) => {
                f.write_str("CHECK (")?;
                expr.fmt(f)?;
                f.write_char(')')
            }
            ColumnConstraint::Default(expr) => {
                f.write_str("DEFAULT ")?;
                expr.fmt(f)
            }
            ColumnConstraint::Collate { collation_name } => {
                f.write_str("COLLATE ")?;
                collation_name.fmt(f)
            }
            ColumnConstraint::ForeignKey {
                clause,
                deref_clause,
            } => {
                f.write_str("REFERENCES ")?;
                clause.fmt(f)?;
                if let Some(deref_clause) = deref_clause {
                    f.write_char(' ')?;
                    deref_clause.fmt(f)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NamedTableConstraint {
    pub name: Option<Name>,
    pub constraint: TableConstraint,
}

impl Display for NamedTableConstraint {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if let Some(ref name) = self.name {
            f.write_str("CONSTRAINT ")?;
            name.fmt(f)?;
            f.write_char(' ')?;
        }
        self.constraint.fmt(f)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TableConstraint {
    PrimaryKey {
        columns: Vec<SortedColumn>,
        auto_increment: bool,
        conflict_clause: Option<ResolveType>,
    },
    Unique {
        columns: Vec<SortedColumn>,
        conflict_clause: Option<ResolveType>,
    },
    Check(Expr),
    ForeignKey {
        columns: Vec<IndexedColumn>,
        clause: ForeignKeyClause,
        deref_clause: Option<DeferSubclause>,
    },
}

impl Display for TableConstraint {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            TableConstraint::PrimaryKey {
                columns,
                auto_increment,
                conflict_clause,
            } => {
                f.write_str("PRIMARY KEY (")?;
                comma(columns, f)?;
                if *auto_increment {
                    f.write_str(" AUTOINCREMENT")?;
                }
                f.write_char(')')?;
                if let Some(conflict_clause) = conflict_clause {
                    f.write_str(" ON CONFLICT ")?;
                    conflict_clause.fmt(f)?;
                }
                Ok(())
            }
            TableConstraint::Unique {
                columns,
                conflict_clause,
            } => {
                f.write_str("UNIQUE (")?;
                comma(columns, f)?;
                f.write_char(')')?;
                if let Some(conflict_clause) = conflict_clause {
                    f.write_str(" ON CONFLICT ")?;
                    conflict_clause.fmt(f)?;
                }
                Ok(())
            }
            TableConstraint::Check(expr) => {
                f.write_str("CHECK (")?;
                expr.fmt(f)?;
                f.write_char(')')
            }
            TableConstraint::ForeignKey {
                columns,
                clause,
                deref_clause,
            } => {
                f.write_str("FOREIGN KEY ")?;
                f.write_char('(')?;
                comma(columns, f)?;
                f.write_char(')')?;
                f.write_str(" REFERENCES ")?;
                clause.fmt(f)?;
                if let Some(deref_clause) = deref_clause {
                    f.write_char(' ')?;
                    deref_clause.fmt(f)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum SortOrder {
    Asc,
    Desc,
}

impl Display for SortOrder {
    fn fmt(&self, f: &mut Formatter) -> Result {
        f.write_str(match self {
            SortOrder::Asc => "ASC",
            SortOrder::Desc => "DESC",
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ForeignKeyClause {
    pub tbl_name: Name,
    pub columns: Option<Vec<IndexedColumn>>,
    pub args: Vec<RefArg>,
}

impl Display for ForeignKeyClause {
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.tbl_name.fmt(f)?;
        f.write_str("DEFERRABLE")?;
        if let Some(ref columns) = self.columns {
            f.write_char('(')?;
            comma(columns, f)?;
            f.write_char(')')?;
        }
        for arg in self.args.iter() {
            f.write_char(' ')?;
            arg.fmt(f)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RefArg {
    OnDelete(RefAct),
    OnInsert(RefAct),
    OnUpdate(RefAct),
    Match(Name),
}

impl Display for RefArg {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            RefArg::OnDelete(ref action) => {
                f.write_str("ON DELETE ")?;
                action.fmt(f)
            }
            RefArg::OnInsert(ref action) => {
                f.write_str("ON INSERT ")?;
                action.fmt(f)
            }
            RefArg::OnUpdate(ref action) => {
                f.write_str("ON UPDATE ")?;
                action.fmt(f)
            }
            RefArg::Match(ref name) => {
                f.write_str("MATCH ")?;
                name.fmt(f)
            }
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum RefAct {
    SetNull,
    SetDefault,
    Cascade,
    Restrict,
    NoAction,
}

impl Display for RefAct {
    fn fmt(&self, f: &mut Formatter) -> Result {
        f.write_str(match self {
            RefAct::SetNull => "SET NULL",
            RefAct::SetDefault => "SET DEFAULT",
            RefAct::Cascade => "CASCADE",
            RefAct::Restrict => "RESTRICT",
            RefAct::NoAction => "NO ACTION",
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DeferSubclause {
    pub deferrable: bool,
    pub init_deferred: Option<InitDeferredPred>,
}

impl Display for DeferSubclause {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if !self.deferrable {
            f.write_str("NOT ")?;
        }
        f.write_str("DEFERRABLE")?;
        if let Some(init_deferred) = self.init_deferred {
            f.write_char(' ')?;
            init_deferred.fmt(f)?;
        }
        Ok(())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum InitDeferredPred {
    InitiallyDeferred,
    InitiallyImmediate, // default
}

impl Display for InitDeferredPred {
    fn fmt(&self, f: &mut Formatter) -> Result {
        f.write_str(match self {
            InitDeferredPred::InitiallyDeferred => "INITIALLY DEFERRED",
            InitDeferredPred::InitiallyImmediate => "INITIALLY IMMEDIATE",
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IndexedColumn {
    pub col_name: Name,
    pub collation_name: Option<Name>,
    pub order: Option<SortOrder>,
}

impl Display for IndexedColumn {
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.col_name.fmt(f)?;
        if let Some(ref collation_name) = self.collation_name {
            f.write_str(" COLLATE ")?;
            collation_name.fmt(f)?;
        }
        if let Some(order) = self.order {
            f.write_char(' ')?;
            order.fmt(f)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Indexed {
    // idx name
    IndexedBy(Name),
    NotIndexed,
}

impl Display for Indexed {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Indexed::IndexedBy(ref name) => {
                f.write_str("INDEXED BY ")?;
                name.fmt(f)
            }
            Indexed::NotIndexed => f.write_str("NOT INDEXED"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SortedColumn {
    pub expr: Expr,
    pub order: Option<SortOrder>,
}

impl Display for SortedColumn {
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.expr.fmt(f)?;
        if let Some(ref order) = self.order {
            f.write_char(' ')?;
            order.fmt(f)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Limit {
    pub expr: Expr,
    pub offset: Option<Expr>, // TODO distinction between LIMIT offset, count and LIMIT count OFFSET offset
}

impl Display for Limit {
    fn fmt(&self, f: &mut Formatter) -> Result {
        f.write_str("LIMIT ")?;
        self.expr.fmt(f)?;
        if let Some(ref offset) = self.offset {
            f.write_str(" OFFSET ")?;
            offset.fmt(f)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum InsertBody {
    Select(Select),
    DefaultValues,
}

impl Display for InsertBody {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            InsertBody::Select(select) => select.fmt(f),
            InsertBody::DefaultValues => f.write_str("DEFAULT VALUES"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Set {
    pub col_names: Vec<Name>,
    pub expr: Expr,
}

impl Display for Set {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if self.col_names.len() == 1 {
            comma(&self.col_names, f)?;
        } else {
            f.write_char('(')?;
            comma(&self.col_names, f)?;
            f.write_char(')')?;
        }
        f.write_str(" = ")?;
        self.expr.fmt(f)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PragmaBody {
    Equals(PragmaValue),
    Call(PragmaValue),
}

impl Display for PragmaBody {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            PragmaBody::Equals(value) => {
                f.write_str(" = ")?;
                value.fmt(f)
            }
            PragmaBody::Call(value) => {
                f.write_char('(')?;
                value.fmt(f)?;
                f.write_char(')')
            }
        }
    }
}

pub type PragmaValue = Expr; // TODO

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TriggerTime {
    Before, // default
    After,
    InsteadOf,
}

impl Display for TriggerTime {
    fn fmt(&self, f: &mut Formatter) -> Result {
        f.write_str(match self {
            TriggerTime::Before => "BEFORE",
            TriggerTime::After => "AFTER",
            TriggerTime::InsteadOf => "INSTEAD OF",
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TriggerEvent {
    Delete,
    Insert,
    Update,
    // col names
    UpdateOf(Vec<Name>),
}

impl Display for TriggerEvent {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            TriggerEvent::Delete => f.write_str("DELETE"),
            TriggerEvent::Insert => f.write_str("INSERT"),
            TriggerEvent::Update => f.write_str("UPDATE"),
            TriggerEvent::UpdateOf(ref col_names) => {
                f.write_str("UPDATE OF")?;
                for (i, name) in col_names.iter().enumerate() {
                    if i == 0 {
                        f.write_char(' ')?;
                    } else {
                        f.write_str(", ")?;
                    }
                    name.fmt(f)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TriggerCmd {
    Update {
        or_conflict: Option<ResolveType>,
        tbl_name: Name,
        sets: Vec<Set>,
        where_clause: Option<Expr>,
    },
    Insert {
        or_conflict: Option<ResolveType>,
        tbl_name: Name,
        col_names: Option<Vec<Name>>,
        select: Select, // FIXME upsert: Upsert
    },
    Delete {
        tbl_name: Name,
        where_clause: Option<Expr>,
    },
    Select(Select),
}

impl Display for TriggerCmd {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            TriggerCmd::Update {
                or_conflict,
                tbl_name,
                sets,
                where_clause,
            } => {
                f.write_str("UPDATE ")?;
                if let Some(or_conflict) = or_conflict {
                    f.write_str("OR ")?;
                    or_conflict.fmt(f)?;
                    f.write_char(' ')?;
                }
                tbl_name.fmt(f)?;
                f.write_str(" SET ")?;
                comma(sets, f)?;
                if let Some(where_clause) = where_clause {
                    f.write_str(" WHERE ")?;
                    where_clause.fmt(f)?;
                }
                Ok(())
            }
            TriggerCmd::Insert {
                or_conflict,
                tbl_name,
                col_names,
                select,
            } => {
                if let Some(ResolveType::Replace) = or_conflict {
                    f.write_str("REPLACE")?;
                } else {
                    f.write_str("INSERT")?;
                    if let Some(or_conflict) = or_conflict {
                        f.write_str(" OR ")?;
                        or_conflict.fmt(f)?;
                    }
                }
                f.write_str(" INTO ")?;
                tbl_name.fmt(f)?;
                if let Some(col_names) = col_names {
                    f.write_str(" (")?;
                    comma(col_names, f)?;
                    f.write_char(')')?;
                }
                f.write_char(' ')?;
                select.fmt(f)
            }
            TriggerCmd::Delete {
                tbl_name,
                where_clause,
            } => {
                f.write_str("DELETE FROM ")?;
                tbl_name.fmt(f)?;
                if let Some(where_clause) = where_clause {
                    f.write_str(" WHERE ")?;
                    where_clause.fmt(f)?;
                }
                Ok(())
            }
            TriggerCmd::Select(select) => select.fmt(f),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ResolveType {
    Rollback,
    Abort, // default
    Fail,
    Ignore,
    Replace,
}

impl Display for ResolveType {
    fn fmt(&self, f: &mut Formatter) -> Result {
        f.write_str(match self {
            ResolveType::Rollback => "ROLLBACK",
            ResolveType::Abort => "ABORT",
            ResolveType::Fail => "FAIL",
            ResolveType::Ignore => "IGNORE",
            ResolveType::Replace => "REPLACE",
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct With {
    pub recursive: bool,
    pub ctes: Vec<CommonTableExpr>,
}

impl Display for With {
    fn fmt(&self, f: &mut Formatter) -> Result {
        f.write_str("WITH ")?;
        if self.recursive {
            f.write_str("RECURSIVE ")?;
        }
        comma(&self.ctes, f)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CommonTableExpr {
    pub tbl_name: Name,
    pub columns: Option<Vec<IndexedColumn>>,
    pub select: Select,
}

impl Display for CommonTableExpr {
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.tbl_name.fmt(f)?;
        if let Some(ref columns) = self.columns {
            f.write_str(" (")?;
            comma(columns, f)?;
            f.write_char(')')?;
        }
        f.write_str(" AS (")?;
        self.select.fmt(f)?;
        f.write_char(')')
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Type {
    pub name: String, // TODO Validate
    pub size: Option<TypeSize>,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self.size {
            None => f.write_str(&self.name),
            Some(ref size) => {
                f.write_str(&self.name)?; // TODO check there is no forbidden chars
                f.write_char('(')?;
                size.fmt(f)?;
                f.write_char(')')
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeSize {
    MaxSize(Box<Expr>),
    TypeSize(Box<Expr>, Box<Expr>),
}

impl Display for TypeSize {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            TypeSize::MaxSize(size) => size.fmt(f),
            TypeSize::TypeSize(size1, size2) => {
                size1.fmt(f)?;
                f.write_str(", ")?;
                size2.fmt(f)
            }
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TransactionType {
    Deferred, // default
    Immediate,
    Exclusive,
}

impl Display for TransactionType {
    fn fmt(&self, f: &mut Formatter) -> Result {
        f.write_str(match self {
            TransactionType::Deferred => "DEFERRED",
            TransactionType::Immediate => "IMMEDIATE",
            TransactionType::Exclusive => "EXCLUSIVE",
        })
    }
}

fn comma<I>(items: I, f: &mut Formatter) -> Result
where
    I: IntoIterator,
    I::Item: Display,
{
    let iter = items.into_iter();
    for (i, item) in iter.enumerate() {
        if i != 0 {
            f.write_str(", ")?;
        }
        item.fmt(f)?;
    }
    Ok(())
}

fn double_quote(name: &str, f: &mut Formatter) -> Result {
    if name.is_empty() {
        return f.write_str("\"\"");
    }
    if is_identifier(name) {
        // identifier must be quoted when they match a keyword...
        if is_keyword(name) {
            f.write_char('`')?;
            f.write_str(name)?;
            return f.write_char('`');
        }
        return f.write_str(name);
    }
    f.write_char('"')?;
    for c in name.chars() {
        if c == '"' {
            f.write_char(c)?;
        }
        f.write_char(c)?;
    }
    f.write_char('"')
}

fn single_quote(name: &str, f: &mut Formatter) -> Result {
    f.write_char('\'')?;
    for c in name.chars() {
        if c == '\'' {
            f.write_char(c)?;
        }
        f.write_char(c)?;
    }
    f.write_char('\'')
}
