//! Abstract Syntax Tree

use crate::dialect::TokenType::{self, *};
use crate::dialect::{from_token, is_identifier, Token};
use crate::parser::parse::YYCODETYPE;
use std::fmt::{self, Display, Formatter, Write};
use std::result::Result;

struct FmtTokenStream<'a, 'b> {
    f: &'a mut Formatter<'b>,
    spaced: bool,
}
impl<'a, 'b> TokenStream for FmtTokenStream<'a, 'b> {
    type Error = fmt::Error;

    fn append(&mut self, ty: TokenType, value: Option<&str>) -> fmt::Result {
        if !self.spaced {
            self.f.write_char(' ')?;
            self.spaced = true;
        }
        if let Some(str) = ty.as_str() {
            self.f.write_str(str)?;
            self.spaced = ty == TK_LP; // str should not be whitespace
        }
        if let Some(str) = value {
            // trick for pretty-print
            self.spaced = self.spaced || str.bytes().all(|b| b.is_ascii_whitespace());
            /*if !self.spaced {
                self.f.write_char(' ')?;
            }*/
            self.f.write_str(str)
        } else {
            Ok(())
        }
    }
}

pub trait TokenStream {
    type Error;

    fn append(&mut self, ty: TokenType, value: Option<&str>) -> Result<(), Self::Error>;
}

pub trait ToTokens {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error>;

    fn to_fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut s = FmtTokenStream { f, spaced: true };
        self.to_tokens(&mut s)
    }
}

impl<T: ?Sized + ToTokens> ToTokens for &T {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        ToTokens::to_tokens(&**self, s)
    }
}
/* FIXME: does not work, find why
impl Display for dyn ToTokens {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut s = FmtTokenStream { f, spaced: true };
        match self.to_tokens(&mut s) {
            Err(_) => Err(fmt::Error),
            Ok(()) => Ok(()),
        }
    }
}
*/

// https://sqlite.org/syntax/sql-stmt.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Cmd {
    Explain(Stmt),
    ExplainQueryPlan(Stmt),
    Stmt(Stmt),
}

impl ToTokens for Cmd {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        match self {
            Cmd::Explain(stmt) => {
                s.append(TK_EXPLAIN, None)?;
                stmt.to_tokens(s)?;
            }
            Cmd::ExplainQueryPlan(stmt) => {
                s.append(TK_EXPLAIN, None)?;
                s.append(TK_QUERY, None)?;
                s.append(TK_PLAN, None)?;
                stmt.to_tokens(s)?;
            }
            Cmd::Stmt(stmt) => {
                stmt.to_tokens(s)?;
            }
        }
        s.append(TK_SEMI, None)
    }
}

impl Display for Cmd {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.to_fmt(f)
    }
}

pub(crate) enum ExplainKind {
    Explain,
    QueryPlan,
}

// https://sqlite.org/syntax/sql-stmt.html
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
        args: Option<String>,
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
        from: Option<FromClause>,
        where_clause: Option<Expr>,
        order_by: Option<Vec<SortedColumn>>,
        limit: Option<Limit>,
    },
    // database name, into expr
    Vacuum(Option<Name>, Option<Expr>),
}

impl ToTokens for Stmt {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        match self {
            Stmt::AlterTable(tbl_name, body) => {
                s.append(TK_ALTER, None)?;
                s.append(TK_TABLE, None)?;
                tbl_name.to_tokens(s)?;
                body.to_tokens(s)
            }
            Stmt::Analyze(obj_name) => {
                s.append(TK_ANALYZE, None)?;
                if let Some(obj_name) = obj_name {
                    obj_name.to_tokens(s)?;
                }
                Ok(())
            }
            Stmt::Attach { expr, db_name, key } => {
                s.append(TK_ATTACH, None)?;
                expr.to_tokens(s)?;
                s.append(TK_AS, None)?;
                db_name.to_tokens(s)?;
                if let Some(key) = key {
                    s.append(TK_KEY, None)?;
                    key.to_tokens(s)?;
                }
                Ok(())
            }
            Stmt::Begin(tx_type, tx_name) => {
                s.append(TK_BEGIN, None)?;
                if let Some(tx_type) = tx_type {
                    tx_type.to_tokens(s)?;
                }
                if let Some(tx_name) = tx_name {
                    s.append(TK_TRANSACTION, None)?;
                    tx_name.to_tokens(s)?;
                }
                Ok(())
            }
            Stmt::Commit(tx_name) => {
                s.append(TK_COMMIT, None)?;
                if let Some(tx_name) = tx_name {
                    s.append(TK_TRANSACTION, None)?;
                    tx_name.to_tokens(s)?;
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
                s.append(TK_CREATE, None)?;
                if *unique {
                    s.append(TK_UNIQUE, None)?;
                }
                s.append(TK_INDEX, None)?;
                if *if_not_exists {
                    s.append(TK_IF, None)?;
                    s.append(TK_NOT, None)?;
                    s.append(TK_EXISTS, None)?;
                }
                idx_name.to_tokens(s)?;
                s.append(TK_ON, None)?;
                tbl_name.to_tokens(s)?;
                s.append(TK_LP, None)?;
                comma_(columns, s)?;
                s.append(TK_RP, None)?;
                if let Some(where_clause) = where_clause {
                    s.append(TK_WHERE, None)?;
                    where_clause.to_tokens(s)?;
                }
                Ok(())
            }
            Stmt::CreateTable {
                temporary,
                if_not_exists,
                tbl_name,
                body,
            } => {
                s.append(TK_CREATE, None)?;
                if *temporary {
                    s.append(TK_TEMP, None)?;
                }
                s.append(TK_TABLE, None)?;
                if *if_not_exists {
                    s.append(TK_IF, None)?;
                    s.append(TK_NOT, None)?;
                    s.append(TK_EXISTS, None)?;
                }
                tbl_name.to_tokens(s)?;
                body.to_tokens(s)
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
                s.append(TK_CREATE, None)?;
                if *temporary {
                    s.append(TK_TEMP, None)?;
                }
                s.append(TK_TRIGGER, None)?;
                if *if_not_exists {
                    s.append(TK_IF, None)?;
                    s.append(TK_NOT, None)?;
                    s.append(TK_EXISTS, None)?;
                }
                trigger_name.to_tokens(s)?;
                if let Some(time) = time {
                    time.to_tokens(s)?;
                }
                event.to_tokens(s)?;
                s.append(TK_ON, None)?;
                tbl_name.to_tokens(s)?;
                if *for_each_row {
                    s.append(TK_FOR, None)?;
                    s.append(TK_EACH, None)?;
                    s.append(TK_ROW, None)?;
                }
                if let Some(when_clause) = when_clause {
                    s.append(TK_WHEN, None)?;
                    when_clause.to_tokens(s)?;
                }
                s.append(TK_BEGIN, Some("\n"))?;
                for (i, command) in commands.iter().enumerate() {
                    command.to_tokens(s)?;
                    s.append(TK_SEMI, Some("\n"))?;
                }
                s.append(TK_END, None)
            }
            Stmt::CreateView {
                temporary,
                if_not_exists,
                view_name,
                columns,
                select,
            } => {
                s.append(TK_CREATE, None)?;
                if *temporary {
                    s.append(TK_TEMP, None)?;
                }
                s.append(TK_VIEW, None)?;
                if *if_not_exists {
                    s.append(TK_IF, None)?;
                    s.append(TK_NOT, None)?;
                    s.append(TK_EXISTS, None)?;
                }
                view_name.to_tokens(s)?;
                if let Some(columns) = columns {
                    s.append(TK_LP, None)?;
                    comma_(columns, s)?;
                    s.append(TK_RP, None)?;
                }
                s.append(TK_AS, None)?;
                select.to_tokens(s)
            }
            Stmt::CreateVirtualTable {
                if_not_exists,
                tbl_name,
                module_name,
                args,
            } => {
                s.append(TK_CREATE, None)?;
                s.append(TK_VIRTUAL, None)?;
                s.append(TK_TABLE, None)?;
                if *if_not_exists {
                    s.append(TK_IF, None)?;
                    s.append(TK_NOT, None)?;
                    s.append(TK_EXISTS, None)?;
                }
                tbl_name.to_tokens(s)?;
                s.append(TK_USING, None)?;
                module_name.to_tokens(s)?;
                s.append(TK_LP, None)?;
                if let Some(args) = args {
                    s.append(TK_ANY, Some(args))?;
                }
                s.append(TK_RP, None)
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
                    with.to_tokens(s)?;
                }
                s.append(TK_DELETE, None)?;
                s.append(TK_FROM, None)?;
                tbl_name.to_tokens(s)?;
                if let Some(indexed) = indexed {
                    indexed.to_tokens(s)?;
                }
                if let Some(where_clause) = where_clause {
                    s.append(TK_WHERE, None)?;
                    where_clause.to_tokens(s)?;
                }
                if let Some(order_by) = order_by {
                    s.append(TK_ORDER, None)?;
                    s.append(TK_BY, None)?;
                    comma_(order_by, s)?;
                }
                if let Some(limit) = limit {
                    limit.to_tokens(s)?;
                }
                Ok(())
            }
            Stmt::Detach(expr) => {
                s.append(TK_DETACH, None)?;
                expr.to_tokens(s)
            }
            Stmt::DropIndex {
                if_exists,
                idx_name,
            } => {
                s.append(TK_DROP, None)?;
                s.append(TK_INDEX, None)?;
                if *if_exists {
                    s.append(TK_IF, None)?;
                    s.append(TK_EXISTS, None)?;
                }
                idx_name.to_tokens(s)
            }
            Stmt::DropTable {
                if_exists,
                tbl_name,
            } => {
                s.append(TK_DROP, None)?;
                s.append(TK_TABLE, None)?;
                if *if_exists {
                    s.append(TK_IF, None)?;
                    s.append(TK_EXISTS, None)?;
                }
                tbl_name.to_tokens(s)
            }
            Stmt::DropTrigger {
                if_exists,
                trigger_name,
            } => {
                s.append(TK_DROP, None)?;
                s.append(TK_TRIGGER, None)?;
                if *if_exists {
                    s.append(TK_IF, None)?;
                    s.append(TK_EXISTS, None)?;
                }
                trigger_name.to_tokens(s)
            }
            Stmt::DropView {
                if_exists,
                view_name,
            } => {
                s.append(TK_DROP, None)?;
                s.append(TK_VIEW, None)?;
                if *if_exists {
                    s.append(TK_IF, None)?;
                    s.append(TK_EXISTS, None)?;
                }
                view_name.to_tokens(s)
            }
            Stmt::Insert {
                with,
                or_conflict,
                tbl_name,
                columns,
                body,
            } => {
                if let Some(with) = with {
                    with.to_tokens(s)?;
                }
                if let Some(ResolveType::Replace) = or_conflict {
                    s.append(TK_REPLACE, None)?;
                } else {
                    s.append(TK_INSERT, None)?;
                    if let Some(or_conflict) = or_conflict {
                        s.append(TK_OR, None)?;
                        or_conflict.to_tokens(s)?;
                    }
                }
                s.append(TK_INTO, None)?;
                tbl_name.to_tokens(s)?;
                if let Some(columns) = columns {
                    s.append(TK_LP, None)?;
                    comma_(columns, s)?;
                    s.append(TK_RP, None)?;
                }
                body.to_tokens(s)
            }
            Stmt::Pragma(name, value) => {
                s.append(TK_PRAGMA, None)?;
                name.to_tokens(s)?;
                if let Some(value) = value {
                    value.to_tokens(s)?;
                }
                Ok(())
            }
            Stmt::Reindex { obj_name } => {
                s.append(TK_REINDEX, None)?;
                if let Some(obj_name) = obj_name {
                    obj_name.to_tokens(s)?;
                }
                Ok(())
            }
            Stmt::Release(name) => {
                s.append(TK_RELEASE, None)?;
                name.to_tokens(s)
            }
            Stmt::Rollback {
                tx_name,
                savepoint_name,
            } => {
                s.append(TK_ROLLBACK, None)?;
                if let Some(tx_name) = tx_name {
                    s.append(TK_TRANSACTION, None)?;
                    tx_name.to_tokens(s)?;
                }
                if let Some(savepoint_name) = savepoint_name {
                    s.append(TK_TO, None)?;
                    savepoint_name.to_tokens(s)?;
                }
                Ok(())
            }
            Stmt::Savepoint(name) => {
                s.append(TK_SAVEPOINT, None)?;
                name.to_tokens(s)
            }
            Stmt::Select(select) => select.to_tokens(s),
            Stmt::Update {
                with,
                or_conflict,
                tbl_name,
                indexed,
                sets,
                from,
                where_clause,
                order_by,
                limit,
            } => {
                if let Some(with) = with {
                    with.to_tokens(s)?;
                }
                s.append(TK_UPDATE, None)?;
                if let Some(or_conflict) = or_conflict {
                    s.append(TK_OR, None)?;
                    or_conflict.to_tokens(s)?;
                }
                tbl_name.to_tokens(s)?;
                if let Some(indexed) = indexed {
                    indexed.to_tokens(s)?;
                }
                s.append(TK_SET, None)?;
                comma_(sets, s)?;
                if let Some(from) = from {
                    s.append(TK_FROM, None)?;
                    from.to_tokens(s)?;
                }
                if let Some(where_clause) = where_clause {
                    s.append(TK_WHERE, None)?;
                    where_clause.to_tokens(s)?;
                }
                if let Some(order_by) = order_by {
                    s.append(TK_ORDER, None)?;
                    s.append(TK_BY, None)?;
                    comma_(order_by, s)?;
                }
                if let Some(limit) = limit {
                    limit.to_tokens(s)?;
                }
                Ok(())
            }
            Stmt::Vacuum(name, expr) => {
                s.append(TK_VACUUM, None)?;
                if let Some(ref name) = name {
                    name.to_tokens(s)?;
                }
                if let Some(ref expr) = expr {
                    s.append(TK_INTO, None)?;
                    expr.to_tokens(s)?;
                }
                Ok(())
            }
        }
    }
}

// https://sqlite.org/syntax/expr.html
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
        when_then_pairs: Vec<(Expr, Expr)>,
        else_expr: Option<Box<Expr>>,
    },
    // CAST expression
    Cast {
        expr: Box<Expr>,
        type_name: Type,
    },
    // COLLATE expression
    Collate(Box<Expr>, String),
    // schema-name.table-name.column-name
    DoublyQualified(Name, Name, Name),
    // EXISTS subquery
    Exists(Box<Select>),
    // call to a built-in function
    FunctionCall {
        name: Id,
        distinctness: Option<Distinctness>,
        args: Option<Vec<Expr>>,
        filter_over: Option<FunctionTail>,
    },
    // Function call expression with '*' as arg
    FunctionCallStar {
        name: Id,
        filter_over: Option<FunctionTail>,
    },
    // Identifier
    Id(Id),
    InList {
        lhs: Box<Expr>,
        not: bool,
        rhs: Option<Vec<Expr>>,
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
        args: Option<Vec<Expr>>,
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
    Name(Name),
    // "NOT NULL" or "NOTNULL"
    NotNull(Box<Expr>),
    // Parenthesized subexpression
    Parenthesized(Vec<Expr>),
    Qualified(Name, Name),
    // RAISE function call
    Raise(ResolveType, Option<Name>),
    // Subquery expression
    Subquery(Box<Select>),
    // Unary expression
    Unary(UnaryOperator, Box<Expr>),
    // Parameters
    Variable(String),
}

impl Expr {
    pub fn parenthesized(x: Expr) -> Expr {
        Expr::Parenthesized(vec![x])
    }
    pub fn id(xt: YYCODETYPE, x: Token) -> Expr {
        Expr::Id(Id::from_token(xt, x))
    }
    pub fn collate(x: Expr, ct: YYCODETYPE, c: Token) -> Expr {
        Expr::Collate(Box::new(x), from_token(ct, c))
    }
    pub fn cast(x: Expr, type_name: Type) -> Expr {
        Expr::Cast {
            expr: Box::new(x),
            type_name,
        }
    }
    pub fn binary(left: Expr, op: YYCODETYPE, right: Expr) -> Expr {
        Expr::Binary(Box::new(left), Operator::from(op), Box::new(right))
    }
    pub fn like(lhs: Expr, not: bool, op: LikeOperator, rhs: Expr, escape: Option<Expr>) -> Expr {
        Expr::Like {
            lhs: Box::new(lhs),
            not,
            op,
            rhs: Box::new(rhs),
            escape: escape.map(Box::new),
        }
    }
    pub fn not_null(x: Expr, op: YYCODETYPE) -> Expr {
        if op == TK_ISNULL as YYCODETYPE {
            Expr::IsNull(Box::new(x))
        } else if op == TK_NOTNULL as YYCODETYPE {
            Expr::NotNull(Box::new(x))
        } else {
            unreachable!()
        }
    }
    pub fn unary(op: UnaryOperator, x: Expr) -> Expr {
        Expr::Unary(op, Box::new(x))
    }
    pub fn between(lhs: Expr, not: bool, start: Expr, end: Expr) -> Expr {
        Expr::Between {
            lhs: Box::new(lhs),
            not,
            start: Box::new(start),
            end: Box::new(end),
        }
    }
    pub fn in_list(lhs: Expr, not: bool, rhs: Option<Vec<Expr>>) -> Expr {
        Expr::InList {
            lhs: Box::new(lhs),
            not,
            rhs,
        }
    }
    pub fn in_select(lhs: Expr, not: bool, rhs: Select) -> Expr {
        Expr::InSelect {
            lhs: Box::new(lhs),
            not,
            rhs: Box::new(rhs),
        }
    }
    pub fn in_table(lhs: Expr, not: bool, rhs: QualifiedName, args: Option<Vec<Expr>>) -> Expr {
        Expr::InTable {
            lhs: Box::new(lhs),
            not,
            rhs,
            args,
        }
    }
    pub fn sub_query(query: Select) -> Expr {
        Expr::Subquery(Box::new(query))
    }
}
impl ToTokens for Expr {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        match self {
            Expr::Between {
                lhs,
                not,
                start,
                end,
            } => {
                lhs.to_tokens(s)?;
                if *not {
                    s.append(TK_NOT, None)?;
                }
                s.append(TK_BETWEEN, None)?;
                start.to_tokens(s)?;
                s.append(TK_AND, None)?;
                end.to_tokens(s)
            }
            Expr::Binary(lhs, op, rhs) => {
                lhs.to_tokens(s)?;
                op.to_tokens(s)?;
                rhs.to_tokens(s)
            }
            Expr::Case {
                base,
                when_then_pairs,
                else_expr,
            } => {
                s.append(TK_CASE, None)?;
                if let Some(ref base) = base {
                    base.to_tokens(s)?;
                }
                for (when, then) in when_then_pairs {
                    s.append(TK_WHEN, None)?;
                    when.to_tokens(s)?;
                    s.append(TK_THEN, None)?;
                    then.to_tokens(s)?;
                }
                if let Some(ref else_expr) = else_expr {
                    s.append(TK_ELSE, None)?;
                    else_expr.to_tokens(s)?;
                }
                s.append(TK_END, None)
            }
            Expr::Cast { expr, type_name } => {
                s.append(TK_CAST, None)?;
                s.append(TK_LP, None)?;
                expr.to_tokens(s)?;
                s.append(TK_AS, None)?;
                type_name.to_tokens(s)?;
                s.append(TK_RP, None)
            }
            Expr::Collate(expr, collation) => {
                expr.to_tokens(s)?;
                s.append(TK_COLLATE, None)?;
                double_quote(collation, f)
            }
            Expr::DoublyQualified(db_name, tbl_name, col_name) => {
                db_name.to_tokens(s)?;
                s.append(TK_DOT, None)?;
                tbl_name.to_tokens(s)?;
                s.append(TK_DOT, None)?;
                col_name.to_tokens(s)
            }
            Expr::Exists(subquery) => {
                s.append(TK_EXISTS, None)?;
                s.append(TK_LP, None)?;
                subquery.to_tokens(s)?;
                s.append(TK_RP, None)
            }
            Expr::FunctionCall {
                name,
                distinctness,
                args,
                filter_over,
            } => {
                name.to_tokens(s)?;
                s.append(TK_LP, None)?;
                if let Some(distinctness) = distinctness {
                    distinctness.to_tokens(s)?;
                }
                if let Some(args) = args {
                    comma_(args, s)?;
                }
                s.append(TK_RP, None)?;
                if let Some(filter_over) = filter_over {
                    filter_over.to_tokens(s)?;
                }
                Ok(())
            }
            Expr::FunctionCallStar { name, filter_over } => {
                name.to_tokens(s)?;
                s.append(TK_LP, None)?;
                s.append(TK_STAR, None)?;
                s.append(TK_RP, None)?;
                if let Some(filter_over) = filter_over {
                    filter_over.to_tokens(s)?;
                }
                Ok(())
            }
            Expr::Id(id) => id.to_tokens(s),
            Expr::InList { lhs, not, rhs } => {
                lhs.to_tokens(s)?;
                if *not {
                    s.append(TK_NOT, None)?;
                }
                s.append(TK_IN, None)?;
                s.append(TK_LP, None)?;
                if let Some(rhs) = rhs {
                    comma_(rhs, s)?;
                }
                s.append(TK_RP, None)
            }
            Expr::InSelect { lhs, not, rhs } => {
                lhs.to_tokens(s)?;
                if *not {
                    s.append(TK_NOT, None)?;
                }
                s.append(TK_IN, None)?;
                s.append(TK_LP, None)?;
                rhs.to_tokens(s)?;
                s.append(TK_RP, None)
            }
            Expr::InTable {
                lhs,
                not,
                rhs,
                args,
            } => {
                lhs.to_tokens(s)?;
                if *not {
                    s.append(TK_NOT, None)?;
                }
                s.append(TK_IN, None)?;
                rhs.to_tokens(s)?;
                if let Some(args) = args {
                    s.append(TK_LP, None)?;
                    comma_(args, s)?;
                    s.append(TK_RP, None)?;
                }
                Ok(())
            }
            Expr::IsNull(sub_expr) => {
                sub_expr.to_tokens(s)?;
                s.append(TK_ISNULL, None)
            }
            Expr::Like {
                lhs,
                not,
                op,
                rhs,
                escape,
            } => {
                lhs.to_tokens(s)?;
                if *not {
                    s.append(TK_NOT, None)?;
                }
                op.to_tokens(s)?;
                rhs.to_tokens(s)?;
                if let Some(escape) = escape {
                    s.append(TK_ESCAPE, None)?;
                    escape.to_tokens(s)?;
                }
                Ok(())
            }
            Expr::Literal(lit) => lit.to_tokens(s),
            Expr::Name(name) => name.to_tokens(s),
            Expr::NotNull(sub_expr) => {
                sub_expr.to_tokens(s)?;
                s.append(TK_NOTNULL, None)
            }
            Expr::Parenthesized(exprs) => {
                s.append(TK_LP, None)?;
                comma_(exprs, s)?;
                s.append(TK_RP, None)
            }
            Expr::Qualified(qualifier, qualified) => {
                qualifier.to_tokens(s)?;
                s.append(TK_DOT, None)?;
                qualified.to_tokens(s)
            }
            Expr::Raise(rt, err) => {
                s.append(TK_RAISE, None)?;
                s.append(TK_LP, None)?;
                rt.to_tokens(s)?;
                if let Some(err) = err {
                    s.append(TK_COMMA, None)?;
                    err.to_tokens(s)?;
                }
                s.append(TK_RP, None)
            }
            Expr::Subquery(query) => {
                s.append(TK_LP, None)?;
                query.to_tokens(s)?;
                s.append(TK_RP, None)
            }
            Expr::Unary(op, sub_expr) => {
                op.to_tokens(s)?;
                if let UnaryOperator::Not = op {
                } else if let Expr::Unary(_, _) = sub_expr.as_ref() {
                }
                sub_expr.to_tokens(s)
            }
            Expr::Variable(var) => match var.chars().next() {
                Some(c) if c == '$' || c == '@' || c == '#' || c == ':' => s.append(TK_VARIABLE, Some(var)),
                Some(_) => {
                    s.append(TK_VARIABLE, Some(&("?".to_owned() + var)))
                }
                None => s.append(TK_VARIABLE, Some("?")),
            },
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.to_fmt(f)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    Numeric(String),
    // TODO Check that string is already quoted and correctly escaped
    String(String),
    // TODO Check that string is valid (only hexa)
    Blob(String),
    Keyword(String),
    Null,
    CurrentDate,
    CurrentTime,
    CurrentTimestamp,
}

impl Literal {
    pub fn from_ctime_kw(token: Token) -> Literal {
        if let Some(ref token) = token {
            if "CURRENT_DATE".eq_ignore_ascii_case(token) {
                Literal::CurrentDate
            } else if "CURRENT_TIME".eq_ignore_ascii_case(token) {
                Literal::CurrentTime
            } else if "CURRENT_TIMESTAMP".eq_ignore_ascii_case(token) {
                Literal::CurrentTimestamp
            } else {
                unreachable!()
            }
        } else {
            unreachable!()
        }
    }
}
impl ToTokens for Literal {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        unimplemented!()
    }
}
impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Numeric(ref num) => f.write_str(num),
            Literal::String(ref str) => single_quote(str, f),
            Literal::Blob(ref blob) => {
                f.write_char('X')?;
                f.write_char('\'')?;
                f.write_str(blob)?;
                f.write_char('\'')
            }
            Literal::Keyword(ref str) => f.write_str(str),
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

impl LikeOperator {
    pub fn from_token(token_type: YYCODETYPE, token: Token) -> LikeOperator {
        if token_type == TK_MATCH as YYCODETYPE {
            return LikeOperator::Match;
        } else if token_type == TK_LIKE_KW as YYCODETYPE {
            if let Some(ref token) = token {
                if "LIKE".eq_ignore_ascii_case(token) {
                    return LikeOperator::Like;
                } else if "GLOB".eq_ignore_ascii_case(token) {
                    return LikeOperator::Glob;
                } else if "REGEXP".eq_ignore_ascii_case(token) {
                    return LikeOperator::Regexp;
                }
            }
        }
        unreachable!()
    }
}
impl ToTokens for LikeOperator {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        unimplemented!()
    }
}
impl Display for LikeOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

impl From<YYCODETYPE> for Operator {
    fn from(token_type: YYCODETYPE) -> Operator {
        match token_type {
            x if x == TK_AND as YYCODETYPE => Operator::And,
            x if x == TK_OR as YYCODETYPE => Operator::Or,
            x if x == TK_LT as YYCODETYPE => Operator::Less,
            x if x == TK_GT as YYCODETYPE => Operator::Greater,
            x if x == TK_GE as YYCODETYPE => Operator::GreaterEquals,
            x if x == TK_LE as YYCODETYPE => Operator::LessEquals,
            x if x == TK_EQ as YYCODETYPE => Operator::Equals,
            x if x == TK_NE as YYCODETYPE => Operator::NotEquals,
            x if x == TK_BITAND as YYCODETYPE => Operator::BitwiseAnd,
            x if x == TK_BITOR as YYCODETYPE => Operator::BitwiseOr,
            x if x == TK_LSHIFT as YYCODETYPE => Operator::LeftShift,
            x if x == TK_RSHIFT as YYCODETYPE => Operator::RightShift,
            x if x == TK_PLUS as YYCODETYPE => Operator::Add,
            x if x == TK_MINUS as YYCODETYPE => Operator::Substract,
            x if x == TK_STAR as YYCODETYPE => Operator::Multiply,
            x if x == TK_SLASH as YYCODETYPE => Operator::Divide,
            x if x == TK_REM as YYCODETYPE => Operator::Modulus,
            x if x == TK_CONCAT as YYCODETYPE => Operator::Concat,
            x if x == TK_IS as YYCODETYPE => Operator::Is,
            x if x == TK_NOT as YYCODETYPE => Operator::IsNot,
            _ => unreachable!(),
        }
    }
}
impl ToTokens for Operator {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        unimplemented!()
    }
}
impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

impl From<YYCODETYPE> for UnaryOperator {
    fn from(token_type: YYCODETYPE) -> UnaryOperator {
        match token_type {
            x if x == TK_BITNOT as YYCODETYPE => UnaryOperator::BitwiseNot,
            x if x == TK_MINUS as YYCODETYPE => UnaryOperator::Negative,
            x if x == TK_NOT as YYCODETYPE => UnaryOperator::Not,
            x if x == TK_PLUS as YYCODETYPE => UnaryOperator::Positive,
            _ => unreachable!(),
        }
    }
}
impl ToTokens for UnaryOperator {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        unimplemented!()
    }
}
impl Display for UnaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            UnaryOperator::BitwiseNot => "~",
            UnaryOperator::Negative => "-",
            UnaryOperator::Not => "NOT",
            UnaryOperator::Positive => "+",
        })
    }
}

// https://sqlite.org/lang_select.html
// https://sqlite.org/syntax/factored-select-stmt.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Select {
    pub with: Option<With>,
    pub body: SelectBody,
    pub order_by: Option<Vec<SortedColumn>>,
    pub limit: Option<Limit>,
}
impl ToTokens for Select {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        unimplemented!()
    }
}
impl Display for Select {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

impl SelectBody {
    pub(crate) fn push(&mut self, cs: CompoundSelect) {
        if let Some(ref mut v) = self.compounds {
            v.push(cs);
        } else {
            self.compounds = Some(vec![cs]);
        }
    }
}

impl Display for SelectBody {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.operator.fmt(f)?;
        f.write_char(' ')?;
        self.select.fmt(f)
    }
}

// https://sqlite.org/syntax/compound-operator.html
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum CompoundOperator {
    Union,
    UnionAll,
    Except,
    Intersect,
}

impl Display for CompoundOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            CompoundOperator::Union => "UNION",
            CompoundOperator::UnionAll => "UNION ALL",
            CompoundOperator::Except => "EXCEPT",
            CompoundOperator::Intersect => "INTERSECT",
        })
    }
}

// https://sqlite.org/syntax/select-core.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OneSelect {
    Select {
        distinctness: Option<Distinctness>,
        columns: Vec<ResultColumn>,
        from: Option<FromClause>,
        where_clause: Option<Expr>,
        group_by: Option<GroupBy>,
        window_clause: Option<Vec<WindowDef>>,
    },
    Values(Vec<Vec<Expr>>),
}

impl Display for OneSelect {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            OneSelect::Select {
                distinctness,
                columns,
                from,
                where_clause,
                group_by,
                window_clause,
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
                if let Some(ref window_clause) = window_clause {
                    f.write_str(" WINDOW ")?;
                    comma(window_clause, f)?;
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

// https://sqlite.org/syntax/join-clause.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FromClause {
    pub select: Option<Box<SelectTable>>, // FIXME mandatory
    pub joins: Option<Vec<JoinedSelectTable>>,
    op: Option<JoinOperator>, // FIXME transient
}
impl ToTokens for FromClause {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        unimplemented!()
    }
}
impl FromClause {
    pub(crate) fn empty() -> FromClause {
        FromClause {
            select: None,
            joins: None,
            op: None,
        }
    }

    pub(crate) fn push(&mut self, table: SelectTable, jc: Option<JoinConstraint>) {
        let op = self.op.take();
        if let Some(op) = op {
            let jst = JoinedSelectTable {
                operator: op,
                table,
                constraint: jc,
            };
            if let Some(ref mut joins) = self.joins {
                joins.push(jst);
            } else {
                self.joins = Some(vec![jst]);
            }
        } else {
            debug_assert!(jc.is_none());
            debug_assert!(self.select.is_none());
            debug_assert!(self.joins.is_none());
            self.select = Some(Box::new(table));
        }
    }

    pub(crate) fn push_op(&mut self, op: JoinOperator) {
        self.op = Some(op);
    }
}

impl Display for FromClause {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.select.as_ref().unwrap().fmt(f)?;
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
impl ToTokens for Distinctness {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        unimplemented!()
    }
}
impl Display for Distinctness {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Distinctness::Distinct => "DISTINCT",
            Distinctness::All => "ALL",
        })
    }
}

// https://sqlite.org/syntax/result-column.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ResultColumn {
    Expr(Expr, Option<As>),
    Star,
    // table name
    TableStar(Name),
}

impl Display for ResultColumn {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
    Elided(Name), // FIXME Ids
}

impl Display for As {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            As::As(ref name) => {
                f.write_str("AS ")?;
                name.fmt(f)
            }
            As::Elided(ref name) => name.fmt(f),
        }
    }
}

// https://sqlite.org/syntax/join-clause.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct JoinedSelectTable {
    pub operator: JoinOperator,
    pub table: SelectTable,
    pub constraint: Option<JoinConstraint>,
}

impl Display for JoinedSelectTable {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

// https://sqlite.org/syntax/table-or-subquery.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SelectTable {
    Table(QualifiedName, Option<As>, Option<Indexed>),
    TableCall(QualifiedName, Option<Vec<Expr>>, Option<As>),
    Select(Select, Option<As>),
    Sub(FromClause, Option<As>),
}

impl Display for SelectTable {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

// https://sqlite.org/syntax/join-operator.html
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum JoinOperator {
    Comma,
    TypedJoin {
        natural: bool,
        join_type: Option<JoinType>,
    },
}

impl JoinOperator {
    pub(crate) fn from_single(token: Token) -> JoinOperator {
        if let Some(ref jt) = token {
            if "INNER".eq_ignore_ascii_case(jt) {
                JoinOperator::TypedJoin {
                    natural: false,
                    join_type: Some(JoinType::Inner),
                }
            } else if "LEFT".eq_ignore_ascii_case(jt) {
                JoinOperator::TypedJoin {
                    natural: false,
                    join_type: Some(JoinType::Left),
                }
            } else if "CROSS".eq_ignore_ascii_case(jt) {
                JoinOperator::TypedJoin {
                    natural: false,
                    join_type: Some(JoinType::Cross),
                }
            } else if "NATURAL".eq_ignore_ascii_case(jt) {
                JoinOperator::TypedJoin {
                    natural: true,
                    join_type: None,
                }
            } else {
                unreachable!() // FIXME do not panic
            }
        } else {
            unreachable!()
        }
    }
    pub(crate) fn from_couple(token: Token, name: Name) -> JoinOperator {
        if let Some(ref jt) = token {
            if "NATURAL".eq_ignore_ascii_case(jt) {
                let join_type = if "INNER".eq_ignore_ascii_case(&name.0) {
                    JoinType::Inner
                } else if "LEFT".eq_ignore_ascii_case(&name.0) {
                    JoinType::Left
                } else if "CROSS".eq_ignore_ascii_case(&name.0) {
                    JoinType::Cross
                } else {
                    unreachable!() // FIXME do not panic
                };
                JoinOperator::TypedJoin {
                    natural: true,
                    join_type: Some(join_type),
                }
            } else if "LEFT".eq_ignore_ascii_case(jt) && "OUTER".eq_ignore_ascii_case(&name.0) {
                JoinOperator::TypedJoin {
                    natural: false,
                    join_type: Some(JoinType::LeftOuter),
                }
            } else {
                unreachable!() // FIXME do not panic
            }
        } else {
            unreachable!()
        }
    }
    pub(crate) fn from_triple(token: Token, n1: Name, n2: Name) -> JoinOperator {
        if let Some(ref jt) = token {
            if "NATURAL".eq_ignore_ascii_case(jt)
                && "LEFT".eq_ignore_ascii_case(&n1.0)
                && "OUTER".eq_ignore_ascii_case(&n2.0)
            {
                JoinOperator::TypedJoin {
                    natural: true,
                    join_type: Some(JoinType::LeftOuter),
                }
            } else {
                unreachable!() // FIXME do not panic
            }
        } else {
            unreachable!()
        }
    }
}

impl Display for JoinOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

impl JoinConstraint {
    pub(crate) fn from(on: Option<Expr>, using: Option<Vec<Name>>) -> Option<JoinConstraint> {
        if let Some(expr) = on {
            debug_assert!(using.is_none());
            Some(JoinConstraint::On(expr))
        } else if let Some(names) = using {
            Some(JoinConstraint::Using(names))
        } else {
            None
        }
    }
}

impl Display for JoinConstraint {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("GROUP BY ")?;
        comma(&self.exprs, f)?;
        if let Some(ref having) = self.having {
            f.write_str(" HAVING ")?;
            having.fmt(f)?;
        }
        Ok(())
    }
}

/// identifier or one of several keywords or `INDEXED`
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Id(pub String);

impl Id {
    pub fn from_token(ty: YYCODETYPE, token: Token) -> Id {
        Id(from_token(ty, token))
    }
}
impl ToTokens for Id {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        unimplemented!()
    }
}
impl Display for Id {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        double_quote(&self.0, f)
    }
}

// TODO ids (identifier or string)

/// identifier or string or `CROSS` or `FULL` or `INNER` or `LEFT` or `NATURAL` or `OUTER` or `RIGHT`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Name(pub String); // TODO distinction between Name and "Name"/[Name]/`Name`

impl Name {
    pub fn from_token(ty: YYCODETYPE, token: Token) -> Name {
        Name(from_token(ty, token))
    }
}
impl ToTokens for Name {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        unimplemented!()
    }
}
impl Display for Name {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        double_quote(&self.0, f)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct QualifiedName {
    pub db_name: Option<Name>,
    pub name: Name,
    pub alias: Option<Name>, // FIXME restrict alias usage (fullname vs xfullname)
}

impl QualifiedName {
    pub fn single(name: Name) -> Self {
        QualifiedName {
            db_name: None,
            name,
            alias: None,
        }
    }
    pub fn fullname(db_name: Name, name: Name) -> Self {
        QualifiedName {
            db_name: Some(db_name),
            name,
            alias: None,
        }
    }
    pub fn xfullname(db_name: Name, name: Name, alias: Name) -> Self {
        QualifiedName {
            db_name: Some(db_name),
            name,
            alias: Some(alias),
        }
    }
    pub fn alias(name: Name, alias: Name) -> Self {
        QualifiedName {
            db_name: None,
            name,
            alias: Some(alias),
        }
    }
}
impl ToTokens for QualifiedName {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        unimplemented!()
    }
}
impl Display for QualifiedName {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

// https://sqlite.org/lang_altertable.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AlterTableBody {
    // new table name
    RenameTo(Name),
    AddColumn(ColumnDefinition), // TODO distinction between ADD and ADD COLUMN
    RenameColumn { old: Name, new: Name },
}
impl ToTokens for AlterTableBody {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        unimplemented!()
    }
}
impl Display for AlterTableBody {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

// https://sqlite.org/lang_createtable.html
// https://sqlite.org/syntax/create-table-stmt.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CreateTableBody {
    ColumnsAndConstraints {
        columns: Vec<ColumnDefinition>,
        constraints: Option<Vec<NamedTableConstraint>>,
        without: bool,
    },
    AsSelect(Select),
}
impl ToTokens for CreateTableBody {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        unimplemented!()
    }
}
impl Display for CreateTableBody {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

// https://sqlite.org/syntax/column-def.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ColumnDefinition {
    pub col_name: Name,
    pub col_type: Option<Type>,
    pub constraints: Vec<NamedColumnConstraint>,
}

impl Display for ColumnDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

// https://sqlite.org/syntax/column-constraint.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NamedColumnConstraint {
    pub name: Option<Name>,
    pub constraint: ColumnConstraint,
}

impl Display for NamedColumnConstraint {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(ref name) = self.name {
            f.write_str("CONSTRAINT ")?;
            name.fmt(f)?;
            f.write_char(' ')?;
        }
        self.constraint.fmt(f)
    }
}

// https://sqlite.org/syntax/column-constraint.html
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
    Defer(DeferSubclause), // FIXME
    Collate {
        collation_name: Name, // FIXME Ids
    },
    ForeignKey {
        clause: ForeignKeyClause,
        deref_clause: Option<DeferSubclause>,
    },
    Generated {
        expr: Expr,
        typ: Option<Id>,
    },
}

impl Display for ColumnConstraint {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
            ColumnConstraint::Defer(deref_clause) => deref_clause.fmt(f),
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
            ColumnConstraint::Generated { expr, typ } => {
                f.write_str("AS (")?;
                expr.fmt(f)?;
                f.write_char(')')?;
                if let Some(typ) = typ {
                    f.write_char(' ')?;
                    typ.fmt(f)?;
                }
                Ok(())
            }
        }
    }
}

// https://sqlite.org/syntax/table-constraint.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NamedTableConstraint {
    pub name: Option<Name>,
    pub constraint: TableConstraint,
}

impl Display for NamedTableConstraint {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(ref name) = self.name {
            f.write_str("CONSTRAINT ")?;
            name.fmt(f)?;
            f.write_char(' ')?;
        }
        self.constraint.fmt(f)
    }
}

// https://sqlite.org/syntax/table-constraint.html
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
                f.write_str("FOREIGN KEY (")?;
                comma(columns, f)?;
                f.write_str(") REFERENCES ")?;
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            SortOrder::Asc => "ASC",
            SortOrder::Desc => "DESC",
        })
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum NullsOrder {
    First,
    Last,
}

impl Display for NullsOrder {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            NullsOrder::First => "NULLS FIRST",
            NullsOrder::Last => "NULLS LAST",
        })
    }
}

// https://sqlite.org/syntax/foreign-key-clause.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ForeignKeyClause {
    pub tbl_name: Name,
    pub columns: Option<Vec<IndexedColumn>>,
    pub args: Vec<RefArg>,
}

impl Display for ForeignKeyClause {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.tbl_name.fmt(f)?;
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            InitDeferredPred::InitiallyDeferred => "INITIALLY DEFERRED",
            InitDeferredPred::InitiallyImmediate => "INITIALLY IMMEDIATE",
        })
    }
}

// https://sqlite.org/syntax/indexed-column.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IndexedColumn {
    pub col_name: Name,
    pub collation_name: Option<Name>, // FIXME Ids
    pub order: Option<SortOrder>,
}
impl ToTokens for IndexedColumn {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        unimplemented!()
    }
}
impl Display for IndexedColumn {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
impl ToTokens for Indexed {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        unimplemented!()
    }
}
impl Display for Indexed {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
    pub nulls: Option<NullsOrder>,
}
impl ToTokens for SortedColumn {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        unimplemented!()
    }
}
impl Display for SortedColumn {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.expr.fmt(f)?;
        if let Some(ref order) = self.order {
            f.write_char(' ')?;
            order.fmt(f)?;
        }
        if let Some(ref nulls) = self.nulls {
            f.write_char(' ')?;
            nulls.fmt(f)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Limit {
    pub expr: Expr,
    pub offset: Option<Expr>, // TODO distinction between LIMIT offset, count and LIMIT count OFFSET offset
}
impl ToTokens for Limit {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        unimplemented!()
    }
}
impl Display for Limit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("LIMIT ")?;
        self.expr.fmt(f)?;
        if let Some(ref offset) = self.offset {
            f.write_str(" OFFSET ")?;
            offset.fmt(f)?;
        }
        Ok(())
    }
}

// https://sqlite.org/lang_insert.html
// https://sqlite.org/syntax/insert-stmt.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum InsertBody {
    Select(Select, Option<Upsert>),
    DefaultValues,
}
impl ToTokens for InsertBody {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        unimplemented!()
    }
}
impl Display for InsertBody {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            InsertBody::Select(select, upsert) => {
                select.fmt(f)?;
                if let Some(upsert) = upsert {
                    f.write_char(' ')?;
                    upsert.fmt(f)?;
                }
                Ok(())
            }
            InsertBody::DefaultValues => f.write_str("DEFAULT VALUES"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Set {
    pub col_names: Vec<Name>,
    pub expr: Expr,
}
impl ToTokens for Set {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        unimplemented!()
    }
}
impl Display for Set {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

// https://sqlite.org/syntax/pragma-stmt.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PragmaBody {
    Equals(PragmaValue),
    Call(PragmaValue),
}
impl ToTokens for PragmaBody {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        unimplemented!()
    }
}
impl Display for PragmaBody {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

// https://sqlite.org/syntax/pragma-value.html
pub type PragmaValue = Expr; // TODO

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TriggerTime {
    Before, // default
    After,
    InsteadOf,
}
impl ToTokens for TriggerTime {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        unimplemented!()
    }
}
impl Display for TriggerTime {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
impl ToTokens for TriggerEvent {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        unimplemented!()
    }
}
impl Display for TriggerEvent {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TriggerEvent::Delete => f.write_str("DELETE"),
            TriggerEvent::Insert => f.write_str("INSERT"),
            TriggerEvent::Update => f.write_str("UPDATE"),
            TriggerEvent::UpdateOf(ref col_names) => {
                f.write_str("UPDATE OF ")?;
                comma(col_names, f)
            }
        }
    }
}

// https://sqlite.org/lang_createtrigger.html
// https://sqlite.org/syntax/create-trigger-stmt.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TriggerCmd {
    Update {
        or_conflict: Option<ResolveType>,
        tbl_name: Name,
        sets: Vec<Set>,
        from: Option<FromClause>,
        where_clause: Option<Expr>,
    },
    Insert {
        or_conflict: Option<ResolveType>,
        tbl_name: Name,
        col_names: Option<Vec<Name>>,
        select: Select,
        upsert: Option<Upsert>,
    },
    Delete {
        tbl_name: Name,
        where_clause: Option<Expr>,
    },
    Select(Select),
}
impl ToTokens for TriggerCmd {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        unimplemented!()
    }
}
impl Display for TriggerCmd {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TriggerCmd::Update {
                or_conflict,
                tbl_name,
                sets,
                from,
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
                if let Some(from) = from {
                    f.write_str(" FROM ")?;
                    from.fmt(f)?;
                }
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
                upsert,
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
                select.fmt(f)?;
                if let Some(upsert) = upsert {
                    f.write_char(' ')?;
                    upsert.fmt(f)?;
                }
                Ok(())
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
impl ToTokens for ResolveType {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        unimplemented!()
    }
}
impl Display for ResolveType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            ResolveType::Rollback => "ROLLBACK",
            ResolveType::Abort => "ABORT",
            ResolveType::Fail => "FAIL",
            ResolveType::Ignore => "IGNORE",
            ResolveType::Replace => "REPLACE",
        })
    }
}

// https://sqlite.org/lang_with.html
// https://sqlite.org/syntax/with-clause.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct With {
    pub recursive: bool,
    pub ctes: Vec<CommonTableExpr>,
}
impl ToTokens for With {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        unimplemented!()
    }
}
impl Display for With {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("WITH ")?;
        if self.recursive {
            f.write_str("RECURSIVE ")?;
        }
        comma(&self.ctes, f)
    }
}

// https://sqlite.org/syntax/common-table-expression.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CommonTableExpr {
    pub tbl_name: Name,
    pub columns: Option<Vec<IndexedColumn>>,
    pub select: Select,
}

impl Display for CommonTableExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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

// https://sqlite.org/syntax/type-name.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Type {
    pub name: String, // TODO Validate: Ids+
    pub size: Option<TypeSize>,
}
impl ToTokens for Type {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        unimplemented!()
    }
}
impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.size {
            None => double_quote(&self.name, f),
            Some(ref size) => {
                f.write_str(&self.name)?; // TODO check there is no forbidden chars
                f.write_char('(')?;
                size.fmt(f)?;
                f.write_char(')')
            }
        }
    }
}

// https://sqlite.org/syntax/type-name.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeSize {
    MaxSize(Box<Expr>),
    TypeSize(Box<Expr>, Box<Expr>),
}

impl Display for TypeSize {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
impl ToTokens for TransactionType {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        unimplemented!()
    }
}
impl Display for TransactionType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            TransactionType::Deferred => "DEFERRED",
            TransactionType::Immediate => "IMMEDIATE",
            TransactionType::Exclusive => "EXCLUSIVE",
        })
    }
}

// https://sqlite.org/lang_upsert.html
// https://sqlite.org/syntax/upsert-clause.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Upsert {
    pub index: Option<UpsertIndex>,
    pub do_clause: UpsertDo,
}

impl Display for Upsert {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("ON CONFLICT ")?;
        if let Some(ref index) = self.index {
            index.fmt(f)?;
            f.write_char(' ')?;
        }
        self.do_clause.fmt(f)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UpsertIndex {
    pub targets: Vec<SortedColumn>,
    pub where_clause: Option<Expr>,
}

impl Display for UpsertIndex {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_char('(')?;
        comma(&self.targets, f)?;
        f.write_char(')')?;
        if let Some(ref where_clause) = self.where_clause {
            f.write_str(" WHERE ")?;
            where_clause.fmt(f)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UpsertDo {
    Set {
        sets: Vec<Set>,
        where_clause: Option<Expr>,
    },
    Nothing,
}

impl Display for UpsertDo {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            UpsertDo::Set { sets, where_clause } => {
                f.write_str("DO UPDATE SET ")?;
                comma(sets, f)?;
                if let Some(where_clause) = where_clause {
                    f.write_str(" WHERE ")?;
                    where_clause.fmt(f)?;
                }
                Ok(())
            }
            UpsertDo::Nothing => f.write_str("DO NOTHING"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionTail {
    pub filter_clause: Option<Box<Expr>>,
    pub over_clause: Option<Box<Over>>,
}
impl ToTokens for FunctionTail {
    fn to_tokens<S: TokenStream>(&self, s: &mut S) -> Result<(), S::Error> {
        unimplemented!()
    }
}
impl Display for FunctionTail {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(ref filter_clause) = self.filter_clause {
            f.write_str(" FILTER (WHERE ")?;
            filter_clause.fmt(f)?;
            f.write_str(")")?;
        }
        if let Some(ref over_clause) = self.over_clause {
            f.write_str(" OVER ")?;
            over_clause.fmt(f)?;
        }
        Ok(())
    }
}

// https://sqlite.org/syntax/over-clause.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Over {
    Window(Window),
    Name(Name),
}

impl Display for Over {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Over::Window(ref window) => window.fmt(f),
            Over::Name(ref name) => name.fmt(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WindowDef {
    pub name: Name,
    pub window: Window,
}

impl Display for WindowDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.name.fmt(f)?;
        f.write_str(" AS ")?;
        self.window.fmt(f)
    }
}

// https://sqlite.org/syntax/window-defn.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Window {
    pub base: Option<Name>,
    pub partition_by: Option<Vec<Expr>>,
    pub order_by: Option<Vec<SortedColumn>>,
    pub frame_clause: Option<FrameClause>,
}

impl Display for Window {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_char('(')?;
        if let Some(ref base) = self.base {
            base.fmt(f)?;
            f.write_char(' ')?;
        }
        if let Some(ref partition_by) = self.partition_by {
            f.write_str("PARTITION BY ")?;
            comma(partition_by, f)?;
            f.write_char(' ')?;
        }
        if let Some(ref order_by) = self.order_by {
            f.write_str("ORDER BY ")?;
            comma(order_by, f)?;
            f.write_char(' ')?;
        }
        if let Some(ref frame_clause) = self.frame_clause {
            frame_clause.fmt(f)?;
        }
        f.write_char(')')
    }
}

// https://sqlite.org/syntax/frame-spec.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FrameClause {
    pub mode: FrameMode,
    pub start: FrameBound,
    pub end: Option<FrameBound>,
    pub exclude: Option<FrameExclude>,
}

impl Display for FrameClause {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.mode.fmt(f)?;
        if let Some(ref end) = self.end {
            f.write_str(" BETWEEN ")?;
            self.start.fmt(f)?;
            f.write_str(" AND ")?;
            end.fmt(f)?;
        } else {
            f.write_char(' ')?;
            self.start.fmt(f)?;
        }
        if let Some(ref exclude) = self.exclude {
            f.write_str(" EXCLUDE ")?;
            exclude.fmt(f)?;
        }
        Ok(())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum FrameMode {
    Groups,
    Range,
    Rows,
}

impl Display for FrameMode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            FrameMode::Groups => "GROUPS",
            FrameMode::Range => "RANGE",
            FrameMode::Rows => "ROWS",
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FrameBound {
    CurrentRow,
    Following(Expr),
    Preceding(Expr),
    UnboundedFollowing,
    UnboundedPreceding,
}

impl Display for FrameBound {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            FrameBound::CurrentRow => f.write_str("CURRENT ROW"),
            FrameBound::Following(value) => {
                value.fmt(f)?;
                f.write_str(" FOLLOWING")
            }
            FrameBound::Preceding(value) => {
                value.fmt(f)?;
                f.write_str(" PRECEDING")
            }
            FrameBound::UnboundedFollowing => f.write_str("UNBOUNDED FOLLOWING"),
            FrameBound::UnboundedPreceding => f.write_str("UNBOUNDED PRECEDING"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FrameExclude {
    NoOthers,
    CurrentRow,
    Group,
    Ties,
}

impl Display for FrameExclude {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            FrameExclude::NoOthers => f.write_str("NO OTHERS"),
            FrameExclude::CurrentRow => f.write_str("CURRENT ROW"),
            FrameExclude::Group => f.write_str("GROUP"),
            FrameExclude::Ties => f.write_str("TIES"),
        }
    }
}

fn comma<I>(items: I, f: &mut Formatter<'_>) -> fmt::Result
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
fn comma_<I, S: TokenStream>(items: I, s: &mut S) -> Result<(), S::Error>
where
    I: IntoIterator,
    I::Item: ToTokens,
{
    let iter = items.into_iter();
    for (i, item) in iter.enumerate() {
        if i != 0 {
            s.append(TK_COMMA, None)?;
        }
        item.to_tokens(s)?;
    }
    Ok(())
}

// TK_ID: [...] / `...` / "..." / some keywords / non keywords
fn double_quote(name: &str, f: &mut Formatter<'_>) -> fmt::Result {
    if name.is_empty() {
        return f.write_str("\"\"");
    }
    if is_identifier(name) {
        // identifier must be quoted when they match a keyword...
        /*if is_keyword(name) {
            f.write_char('`')?;
            f.write_str(name)?;
            return f.write_char('`');
        }*/
        return f.write_str(name);
    }
    /*f.write_char('"')?;
    for c in name.chars() {
        if c == '"' {
            f.write_char(c)?;
        }
        f.write_char(c)?;
    }
    f.write_char('"')*/
    f.write_str(name)
}

// TK_STRING
fn single_quote(name: &str, f: &mut Formatter<'_>) -> fmt::Result {
    /*f.write_char('\'')?;
    for c in name.chars() {
        if c == '\'' {
            f.write_char(c)?;
        }
        f.write_char(c)?;
    }
    f.write_char('\'')*/
    f.write_str(name)
}
