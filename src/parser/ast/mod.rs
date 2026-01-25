//! Abstract Syntax Tree
#[cfg(feature = "extra_checks")]
pub mod check;
pub mod fmt;

use std::num::ParseIntError;
use std::ops::Deref;
use std::str::{self, Bytes, FromStr};

use bumpalo::{
    boxed::Box,
    collections::{String, Vec},
    Bump,
};
use indexmap::{IndexMap, IndexSet};

#[cfg(feature = "extra_checks")]
use check::ColumnCount;
use fmt::TokenStream;

use crate::custom_err;
use crate::dialect::TokenType::{self, *};
use crate::dialect::{from_token, is_identifier, Token};
use crate::parser::{parse::YYCODETYPE, ParserError};

/// `?` or `$` Prepared statement arg placeholder(s)
#[derive(Default)]
pub struct ParameterInfo {
    /// Number of SQL parameters in a prepared statement, like `sqlite3_bind_parameter_count`
    pub count: u16,
    /// Parameter name(s) if any
    pub names: IndexSet<std::string::String>,
}

// https://sqlite.org/lang_expr.html#parameters
impl TokenStream for ParameterInfo {
    type Error = ParseIntError;

    fn append(&mut self, ty: TokenType, value: Option<&str>) -> Result<(), Self::Error> {
        if ty == TK_VARIABLE {
            if let Some(variable) = value {
                if variable == "?" {
                    self.count = self.count.saturating_add(1);
                } else if variable.as_bytes()[0] == b'?' {
                    let n = u16::from_str(&variable[1..])?;
                    if n > self.count {
                        self.count = n;
                    }
                } else if self.names.insert(variable.to_owned()) {
                    self.count = self.count.saturating_add(1);
                }
            }
        }
        Ok(())
    }
}

/// Statement or Explain statement
// https://sqlite.org/syntax/sql-stmt.html
#[derive(Debug, PartialEq, Eq)]
pub enum Cmd<'bump> {
    /// `EXPLAIN` statement
    Explain(Stmt<'bump>),
    /// `EXPLAIN QUERY PLAN` statement
    ExplainQueryPlan(Stmt<'bump>),
    /// statement
    Stmt(Stmt<'bump>),
}

pub(crate) enum ExplainKind {
    Explain,
    QueryPlan,
}

/// SQL statement
// https://sqlite.org/syntax/sql-stmt.html
#[derive(Debug, PartialEq, Eq)]
pub enum Stmt<'bump> {
    /// `ALTER TABLE`: table name, body
    AlterTable(QualifiedName<'bump>, AlterTableBody<'bump>),
    /// `ANALYSE`: object name
    Analyze(Option<QualifiedName<'bump>>),
    /// `ATTACH DATABASE`
    Attach {
        /// filename
        // TODO distinction between ATTACH and ATTACH DATABASE
        expr: Expr<'bump>,
        /// schema name
        db_name: Expr<'bump>,
        /// password
        key: Option<Expr<'bump>>,
    },
    /// `BEGIN`: tx type, tx name
    Begin(Option<TransactionType>, Option<Name<'bump>>),
    /// `COMMIT`/`END`: tx name
    Commit(Option<Name<'bump>>), // TODO distinction between COMMIT and END
    /// `CREATE INDEX`
    CreateIndex {
        /// `UNIQUE`
        unique: bool,
        /// `IF NOT EXISTS`
        if_not_exists: bool,
        /// index name
        idx_name: QualifiedName<'bump>,
        /// table name
        tbl_name: Name<'bump>,
        /// indexed columns or expressions
        columns: Vec<'bump, SortedColumn<'bump>>,
        /// partial index
        where_clause: Option<Expr<'bump>>,
    },
    /// `CREATE TABLE`
    CreateTable {
        /// `TEMPORARY`
        temporary: bool, // TODO distinction between TEMP and TEMPORARY
        /// `IF NOT EXISTS`
        if_not_exists: bool,
        /// table name
        tbl_name: QualifiedName<'bump>,
        /// table body
        body: CreateTableBody<'bump>,
    },
    /// `CREATE TRIGGER`
    CreateTrigger {
        /// `TEMPORARY`
        temporary: bool,
        /// `IF NOT EXISTS`
        if_not_exists: bool,
        /// trigger name
        trigger_name: QualifiedName<'bump>,
        /// `BEFORE`/`AFTER`/`INSTEAD OF`
        time: Option<TriggerTime>,
        /// `DELETE`/`INSERT`/`UPDATE`
        event: TriggerEvent<'bump>,
        /// table name
        tbl_name: QualifiedName<'bump>,
        /// `FOR EACH ROW`
        for_each_row: bool,
        /// `WHEN`
        when_clause: Option<Expr<'bump>>,
        /// statements
        commands: Vec<'bump, TriggerCmd<'bump>>,
    },
    /// `CREATE VIEW`
    CreateView {
        /// `TEMPORARY`
        temporary: bool,
        /// `IF NOT EXISTS`
        if_not_exists: bool,
        /// view name
        view_name: QualifiedName<'bump>,
        /// columns
        columns: Option<Vec<'bump, IndexedColumn<'bump>>>, // TODO check no duplicate directly
        /// query
        select: Box<'bump, Select<'bump>>,
    },
    /// `CREATE VIRTUAL TABLE`
    CreateVirtualTable {
        /// `IF NOT EXISTS`
        if_not_exists: bool,
        /// table name
        tbl_name: QualifiedName<'bump>,
        /// module
        module_name: Name<'bump>,
        /// args
        args: Option<Vec<'bump, String<'bump>>>,
    },
    /// `DELETE`
    Delete {
        /// CTE
        with: Option<With<'bump>>, // TODO check usages in where_clause
        /// `FROM` table name
        tbl_name: QualifiedName<'bump>,
        /// `INDEXED`
        indexed: Option<Indexed<'bump>>,
        /// `WHERE` clause
        where_clause: Option<Expr<'bump>>,
        /// `RETURNING`
        returning: Option<Vec<'bump, ResultColumn<'bump>>>,
        /// `ORDER BY`
        order_by: Option<Vec<'bump, SortedColumn<'bump>>>,
        /// `LIMIT`
        limit: Option<Limit<'bump>>,
    },
    /// `DETACH DATABASE`: db name
    Detach(Expr<'bump>), // TODO distinction between DETACH and DETACH DATABASE
    /// `DROP INDEX`
    DropIndex {
        /// `IF EXISTS`
        if_exists: bool,
        /// index name
        idx_name: QualifiedName<'bump>,
    },
    /// `DROP TABLE`
    DropTable {
        /// `IF EXISTS`
        if_exists: bool,
        /// table name
        tbl_name: QualifiedName<'bump>,
    },
    /// `DROP TRIGGER`
    DropTrigger {
        /// `IF EXISTS`
        if_exists: bool,
        /// trigger name
        trigger_name: QualifiedName<'bump>,
    },
    /// `DROP VIEW`
    DropView {
        /// `IF EXISTS`
        if_exists: bool,
        /// view name
        view_name: QualifiedName<'bump>,
    },
    /// `INSERT`
    Insert {
        /// CTE
        with: Option<With<'bump>>, // TODO check usages in body
        /// `OR`
        or_conflict: Option<ResolveType>, // TODO distinction between REPLACE and INSERT OR REPLACE
        /// table name
        tbl_name: QualifiedName<'bump>,
        /// `COLUMNS`
        columns: Option<DistinctNames<'bump>>,
        /// `VALUES` or `SELECT`
        body: InsertBody<'bump>,
        /// `RETURNING`
        returning: Option<Vec<'bump, ResultColumn<'bump>>>,
    },
    /// `PRAGMA`: pragma name, body
    Pragma(QualifiedName<'bump>, Option<PragmaBody<'bump>>),
    /// `REINDEX`
    Reindex {
        /// collation or index or table name
        obj_name: Option<QualifiedName<'bump>>,
    },
    /// `RELEASE`: savepoint name
    Release(Name<'bump>), // TODO distinction between RELEASE and RELEASE SAVEPOINT
    /// `ROLLBACK`
    Rollback {
        /// transaction name
        tx_name: Option<Name<'bump>>,
        /// savepoint name
        savepoint_name: Option<Name<'bump>>, // TODO distinction between TO and TO SAVEPOINT
    },
    /// `SAVEPOINT`: savepoint name
    Savepoint(Name<'bump>),
    /// `SELECT`
    Select(Box<'bump, Select<'bump>>),
    /// `UPDATE`
    Update {
        /// CTE
        with: Option<With<'bump>>, // TODO check usages in where_clause
        /// `OR`
        or_conflict: Option<ResolveType>,
        /// table name
        tbl_name: QualifiedName<'bump>,
        /// `INDEXED`
        indexed: Option<Indexed<'bump>>,
        /// `SET` assignments
        sets: Vec<'bump, Set<'bump>>, // FIXME unique
        /// `FROM`
        from: Option<FromClause<'bump>>,
        /// `WHERE` clause
        where_clause: Option<Expr<'bump>>,
        /// `RETURNING`
        returning: Option<Vec<'bump, ResultColumn<'bump>>>,
        /// `ORDER BY`
        order_by: Option<Vec<'bump, SortedColumn<'bump>>>,
        /// `LIMIT`
        limit: Option<Limit<'bump>>,
    },
    /// `VACUUM`: database name, into expr
    Vacuum(Option<Name<'bump>>, Option<Expr<'bump>>),
}

impl<'bump> Stmt<'bump> {
    /// CREATE INDEX constructor
    pub fn create_index(
        unique: bool,
        if_not_exists: bool,
        idx_name: QualifiedName<'bump>,
        tbl_name: Name<'bump>,
        columns: Vec<'bump, SortedColumn<'bump>>,
        where_clause: Option<Expr<'bump>>,
    ) -> Result<Self, ParserError> {
        has_explicit_nulls(&columns)?;
        Ok(Self::CreateIndex {
            unique,
            if_not_exists,
            idx_name,
            tbl_name,
            columns,
            where_clause,
        })
    }
    /// UPDATE constructor
    #[allow(clippy::too_many_arguments)]
    pub fn update(
        with: Option<With<'bump>>,
        or_conflict: Option<ResolveType>,
        tbl_name: QualifiedName<'bump>,
        indexed: Option<Indexed<'bump>>,
        sets: Vec<'bump, Set<'bump>>, // FIXME unique
        from: Option<FromClause<'bump>>,
        where_clause: Option<Expr<'bump>>,
        returning: Option<Vec<'bump, ResultColumn<'bump>>>,
        order_by: Option<Vec<'bump, SortedColumn<'bump>>>,
        limit: Option<Limit<'bump>>,
    ) -> Result<Self, ParserError> {
        #[cfg(feature = "extra_checks")]
        if let Some(FromClause {
            select: Some(ref select),
            ref joins,
            ..
        }) = from
        {
            if matches!(select.as_ref(),
                SelectTable::Table(qn, _, _) | SelectTable::TableCall(qn, _, _)
                    if *qn == tbl_name)
                || joins.as_ref().is_some_and(|js| js.iter().any(|j|
                    matches!(j.table, SelectTable::Table(ref qn, _, _) | SelectTable::TableCall(ref qn, _, _)
                    if *qn == tbl_name)))
            {
                return Err(custom_err!(
                    "target object/alias may not appear in FROM clause",
                ));
            }
        }
        #[cfg(feature = "extra_checks")]
        if order_by.is_some() && limit.is_none() {
            return Err(custom_err!("ORDER BY without LIMIT on UPDATE"));
        }
        Ok(Self::Update {
            with,
            or_conflict,
            tbl_name,
            indexed,
            sets,
            from,
            where_clause,
            returning,
            order_by,
            limit,
        })
    }
}

/// SQL expression
// https://sqlite.org/syntax/expr.html
#[derive(Debug, PartialEq, Eq)]
pub enum Expr<'bump> {
    /// `BETWEEN`
    Between {
        /// expression
        lhs: Box<'bump, Expr<'bump>>,
        /// `NOT`
        not: bool,
        /// start
        start: Box<'bump, Expr<'bump>>,
        /// end
        end: Box<'bump, Expr<'bump>>,
    },
    /// binary expression
    Binary(Box<'bump, Expr<'bump>>, Operator, Box<'bump, Expr<'bump>>),
    /// `CASE` expression
    Case {
        /// operand
        base: Option<Box<'bump, Expr<'bump>>>,
        /// `WHEN` condition `THEN` result
        when_then_pairs: Vec<'bump, (Expr<'bump>, Expr<'bump>)>,
        /// `ELSE` result
        else_expr: Option<Box<'bump, Expr<'bump>>>,
    },
    /// CAST expression
    Cast {
        /// expression
        expr: Box<'bump, Expr<'bump>>,
        /// `AS` type name
        type_name: Option<Type<'bump>>,
    },
    /// `COLLATE`: expression
    Collate(Box<'bump, Expr<'bump>>, String<'bump>),
    /// schema-name.table-name.column-name
    DoublyQualified(Name<'bump>, Name<'bump>, Name<'bump>),
    /// `EXISTS` subquery
    Exists(Box<'bump, Select<'bump>>),
    /// call to a built-in function
    FunctionCall {
        /// function name
        name: Id<'bump>,
        /// `DISTINCT`
        distinctness: Option<Distinctness>,
        /// arguments
        args: Option<Vec<'bump, Expr<'bump>>>,
        /// `ORDER BY` or `WITHIN GROUP`
        order_by: Option<FunctionCallOrder<'bump>>,
        /// `FILTER`
        filter_over: Option<FunctionTail<'bump>>,
    },
    /// Function call expression with '*' as arg
    FunctionCallStar {
        /// function name
        name: Id<'bump>,
        /// `FILTER`
        filter_over: Option<FunctionTail<'bump>>,
    },
    /// Identifier
    Id(Id<'bump>),
    /// `IN`
    InList {
        /// expression
        lhs: Box<'bump, Expr<'bump>>,
        /// `NOT`
        not: bool,
        /// values
        rhs: Option<Vec<'bump, Expr<'bump>>>,
    },
    /// `IN` subselect
    InSelect {
        /// expression
        lhs: Box<'bump, Expr<'bump>>,
        /// `NOT`
        not: bool,
        /// subquery
        rhs: Box<'bump, Select<'bump>>,
    },
    /// `IN` table name / function
    InTable {
        /// expression
        lhs: Box<'bump, Expr<'bump>>,
        /// `NOT`
        not: bool,
        /// table name
        rhs: QualifiedName<'bump>,
        /// table function arguments
        args: Option<Vec<'bump, Expr<'bump>>>,
    },
    /// `IS NULL`
    IsNull(Box<'bump, Expr<'bump>>),
    /// `LIKE`
    Like {
        /// expression
        lhs: Box<'bump, Expr<'bump>>,
        /// `NOT`
        not: bool,
        /// operator
        op: LikeOperator,
        /// pattern
        rhs: Box<'bump, Expr<'bump>>,
        /// `ESCAPE` char
        escape: Option<Box<'bump, Expr<'bump>>>,
    },
    /// Literal expression
    Literal(Literal<'bump>),
    /// Name
    Name(Name<'bump>),
    /// `NOT NULL` or `NOTNULL`
    NotNull(Box<'bump, Expr<'bump>>),
    /// Parenthesized subexpression
    Parenthesized(Vec<'bump, Expr<'bump>>),
    /// Qualified name
    Qualified(Name<'bump>, Name<'bump>),
    /// `RAISE` function call
    Raise(ResolveType, Option<Box<'bump, Expr<'bump>>>),
    /// Subquery expression
    Subquery(Box<'bump, Select<'bump>>),
    /// Unary expression
    Unary(UnaryOperator, Box<'bump, Expr<'bump>>),
    /// Parameters
    Variable(Box<'bump, str>),
}

/// Function call order
#[derive(Debug, PartialEq, Eq)]
pub enum FunctionCallOrder<'bump> {
    /// `ORDER BY cols`
    SortList(Vec<'bump, SortedColumn<'bump>>),
    /// `WITHIN GROUP (ORDER BY expr)`
    #[cfg(feature = "SQLITE_ENABLE_ORDERED_SET_AGGREGATES")]
    WithinGroup(&'bump Expr<'bump>),
}

impl<'bump> FunctionCallOrder<'bump> {
    /// Constructor
    #[cfg(feature = "SQLITE_ENABLE_ORDERED_SET_AGGREGATES")]
    pub fn within_group(expr: &'bump Expr<'bump>) -> Self {
        Self::WithinGroup(expr)
    }
}

impl<'bump> Expr<'bump> {
    /// Constructor
    pub fn parenthesized(x: Self, b: &'bump Bump) -> Self {
        Self::Parenthesized(bumpalo::vec![in b; x])
    }
    /// Constructor
    pub fn id(xt: YYCODETYPE, x: Token, b: &'bump Bump) -> Self {
        Self::Id(Id::from_token(xt, x, b))
    }
    /// Constructor
    pub fn collate(x: Self, ct: YYCODETYPE, c: Token, b: &'bump Bump) -> Self {
        Self::Collate(Box::new_in(x, b), from_token(ct, c, b))
    }
    /// Constructor
    pub fn cast(x: Self, type_name: Option<Type<'bump>>, b: &'bump Bump) -> Self {
        Self::Cast {
            expr: Box::new_in(x, b),
            type_name,
        }
    }
    /// Constructor
    pub fn binary(left: Self, op: YYCODETYPE, right: Self, b: &'bump Bump) -> Self {
        Self::Binary(
            Box::new_in(left, b),
            Operator::from(op),
            Box::new_in(right, b),
        )
    }
    /// Constructor
    pub fn ptr(left: Self, op: Token, right: Self, b: &'bump Bump) -> Self {
        let mut ptr = Operator::ArrowRight;
        if op.1 == b"->>" {
            ptr = Operator::ArrowRightShift;
        }
        Self::Binary(Box::new_in(left, b), ptr, Box::new_in(right, b))
    }
    /// Constructor
    pub fn like(
        lhs: Self,
        not: bool,
        op: LikeOperator,
        rhs: Self,
        escape: Option<Self>,
        b: &'bump Bump,
    ) -> Self {
        Self::Like {
            lhs: Box::new_in(lhs, b),
            not,
            op,
            rhs: Box::new_in(rhs, b),
            escape: escape.map(|e| Box::new_in(e, b)),
        }
    }
    /// Constructor
    pub fn not_null(x: Self, op: YYCODETYPE, b: &'bump Bump) -> Self {
        if op == TK_ISNULL as YYCODETYPE {
            Self::IsNull(Box::new_in(x, b))
        } else if op == TK_NOTNULL as YYCODETYPE {
            Self::NotNull(Box::new_in(x, b))
        } else {
            unreachable!()
        }
    }
    /// Constructor
    pub fn unary(op: UnaryOperator, x: Self, b: &'bump Bump) -> Self {
        Self::Unary(op, Box::new_in(x, b))
    }
    /// Constructor
    pub fn between(lhs: Self, not: bool, start: Self, end: Self, b: &'bump Bump) -> Self {
        Self::Between {
            lhs: Box::new_in(lhs, b),
            not,
            start: Box::new_in(start, b),
            end: Box::new_in(end, b),
        }
    }
    /// Constructor
    pub fn in_list(lhs: Self, not: bool, rhs: Option<Vec<'bump, Self>>, b: &'bump Bump) -> Self {
        Self::InList {
            lhs: Box::new_in(lhs, b),
            not,
            rhs,
        }
    }
    /// Constructor
    pub fn in_select(lhs: Self, not: bool, rhs: Select<'bump>, b: &'bump Bump) -> Self {
        Self::InSelect {
            lhs: Box::new_in(lhs, b),
            not,
            rhs: Box::new_in(rhs, b),
        }
    }
    /// Constructor
    pub fn in_table(
        lhs: Self,
        not: bool,
        rhs: QualifiedName<'bump>,
        args: Option<Vec<'bump, Self>>,
        b: &'bump Bump,
    ) -> Self {
        Self::InTable {
            lhs: Box::new_in(lhs, b),
            not,
            rhs,
            args,
        }
    }
    /// Constructor
    pub fn sub_query(query: Select<'bump>, b: &'bump Bump) -> Self {
        Self::Subquery(Box::new_in(query, b))
    }
    /// Constructor
    pub fn function_call(
        xt: YYCODETYPE,
        x: Token,
        distinctness: Option<Distinctness>,
        args: Option<Vec<'bump, Self>>,
        order_by: Option<FunctionCallOrder<'bump>>,
        filter_over: Option<FunctionTail<'bump>>,
        b: &'bump Bump,
    ) -> Result<Self, ParserError> {
        #[cfg(feature = "extra_checks")]
        if let Some(Distinctness::Distinct) = distinctness {
            if args.as_ref().map_or(0, Vec::len) != 1 {
                return Err(custom_err!(
                    "DISTINCT aggregates must have exactly one argument"
                ));
            }
        }
        Ok(Self::FunctionCall {
            name: Id::from_token(xt, x, b),
            distinctness,
            args,
            order_by,
            filter_over,
        })
    }

    /// Check if an expression is an integer
    pub fn is_integer(&self) -> Option<i64> {
        if let Self::Literal(Literal::Numeric(s)) = self {
            i64::from_str(s).ok()
        } else if let Self::Unary(UnaryOperator::Positive, e) = self {
            e.is_integer()
        } else if let Self::Unary(UnaryOperator::Negative, e) = self {
            e.is_integer().map(i64::saturating_neg)
        } else {
            None
        }
    }
    #[cfg(feature = "extra_checks")]
    fn check_range(&self, term: &str, mx: u16) -> Result<(), ParserError> {
        if let Some(i) = self.is_integer() {
            if i < 1 || i > mx as i64 {
                return Err(custom_err!(
                    "{} BY term out of range - should be between 1 and {}",
                    term,
                    mx
                ));
            }
        }
        Ok(())
    }
}

/// SQL literal
#[derive(Debug, PartialEq, Eq)]
pub enum Literal<'bump> {
    /// Number
    Numeric(Box<'bump, str>),
    /// String
    // TODO Check that string is already quoted and correctly escaped
    String(Box<'bump, str>),
    /// BLOB
    // TODO Check that string is valid (only hexa)
    Blob(Box<'bump, str>),
    /// Keyword
    Keyword(Box<'bump, str>),
    /// `NULL`
    Null,
    /// `CURRENT_DATE`
    CurrentDate,
    /// `CURRENT_TIME`
    CurrentTime,
    /// `CURRENT_TIMESTAMP`
    CurrentTimestamp,
}

impl<'bump> Literal<'bump> {
    /// Constructor
    pub fn from_ctime_kw(token: Token) -> Self {
        if b"CURRENT_DATE".eq_ignore_ascii_case(token.1) {
            Self::CurrentDate
        } else if b"CURRENT_TIME".eq_ignore_ascii_case(token.1) {
            Self::CurrentTime
        } else if b"CURRENT_TIMESTAMP".eq_ignore_ascii_case(token.1) {
            Self::CurrentTimestamp
        } else {
            unreachable!()
        }
    }
}

/// Textual comparison operator in an expression
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum LikeOperator {
    /// `GLOB`
    Glob,
    /// `LIKE`
    Like,
    /// `MATCH`
    Match,
    /// `REGEXP`
    Regexp,
}

impl LikeOperator {
    /// Constructor
    pub fn from_token(token_type: YYCODETYPE, token: Token) -> Self {
        if token_type == TK_MATCH as YYCODETYPE {
            return Self::Match;
        } else if token_type == TK_LIKE_KW as YYCODETYPE {
            let token = token.1;
            if b"LIKE".eq_ignore_ascii_case(token) {
                return Self::Like;
            } else if b"GLOB".eq_ignore_ascii_case(token) {
                return Self::Glob;
            } else if b"REGEXP".eq_ignore_ascii_case(token) {
                return Self::Regexp;
            }
        }
        unreachable!()
    }
}

/// SQL operators
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Operator {
    /// `+`
    Add,
    /// `AND`
    And,
    /// `->`
    ArrowRight,
    /// `->>`
    ArrowRightShift,
    /// `&`
    BitwiseAnd,
    /// `|`
    BitwiseOr,
    /// String concatenation (`||`)
    Concat,
    /// `=` or `==`
    Equals,
    /// `/`
    Divide,
    /// `>`
    Greater,
    /// `>=`
    GreaterEquals,
    /// `IS`
    Is,
    /// `IS NOT`
    IsNot,
    /// `<<`
    LeftShift,
    /// `<`
    Less,
    /// `<=`
    LessEquals,
    /// `%`
    Modulus,
    /// `*`
    Multiply,
    /// `!=` or `<>`
    NotEquals,
    /// `OR`
    Or,
    /// `>>`
    RightShift,
    /// `-`
    Subtract,
}

impl From<YYCODETYPE> for Operator {
    fn from(token_type: YYCODETYPE) -> Self {
        match token_type {
            x if x == TK_AND as YYCODETYPE => Self::And,
            x if x == TK_OR as YYCODETYPE => Self::Or,
            x if x == TK_LT as YYCODETYPE => Self::Less,
            x if x == TK_GT as YYCODETYPE => Self::Greater,
            x if x == TK_GE as YYCODETYPE => Self::GreaterEquals,
            x if x == TK_LE as YYCODETYPE => Self::LessEquals,
            x if x == TK_EQ as YYCODETYPE => Self::Equals,
            x if x == TK_NE as YYCODETYPE => Self::NotEquals,
            x if x == TK_BITAND as YYCODETYPE => Self::BitwiseAnd,
            x if x == TK_BITOR as YYCODETYPE => Self::BitwiseOr,
            x if x == TK_LSHIFT as YYCODETYPE => Self::LeftShift,
            x if x == TK_RSHIFT as YYCODETYPE => Self::RightShift,
            x if x == TK_PLUS as YYCODETYPE => Self::Add,
            x if x == TK_MINUS as YYCODETYPE => Self::Subtract,
            x if x == TK_STAR as YYCODETYPE => Self::Multiply,
            x if x == TK_SLASH as YYCODETYPE => Self::Divide,
            x if x == TK_REM as YYCODETYPE => Self::Modulus,
            x if x == TK_CONCAT as YYCODETYPE => Self::Concat,
            x if x == TK_IS as YYCODETYPE => Self::Is,
            x if x == TK_NOT as YYCODETYPE => Self::IsNot,
            _ => unreachable!(),
        }
    }
}

/// Unary operators
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum UnaryOperator {
    /// bitwise negation (`~`)
    BitwiseNot,
    /// negative-sign
    Negative,
    /// `NOT`
    Not,
    /// positive-sign
    Positive,
}

impl From<YYCODETYPE> for UnaryOperator {
    fn from(token_type: YYCODETYPE) -> Self {
        match token_type {
            x if x == TK_BITNOT as YYCODETYPE => Self::BitwiseNot,
            x if x == TK_MINUS as YYCODETYPE => Self::Negative,
            x if x == TK_NOT as YYCODETYPE => Self::Not,
            x if x == TK_PLUS as YYCODETYPE => Self::Positive,
            _ => unreachable!(),
        }
    }
}

/// `SELECT` statement
// https://sqlite.org/lang_select.html
// https://sqlite.org/syntax/factored-select-stmt.html
#[derive(Debug, PartialEq, Eq)]
pub struct Select<'bump> {
    /// CTE
    pub with: Option<With<'bump>>, // TODO check usages in body
    /// body
    pub body: SelectBody<'bump>,
    /// `ORDER BY`
    pub order_by: Option<Vec<'bump, SortedColumn<'bump>>>, // TODO: ORDER BY term does not match any column in the result set
    /// `LIMIT`
    pub limit: Option<Limit<'bump>>,
}

impl<'bump> Select<'bump> {
    /// Constructor
    pub fn new(
        with: Option<With<'bump>>,
        body: SelectBody<'bump>,
        order_by: Option<Vec<'bump, SortedColumn<'bump>>>,
        limit: Option<Limit<'bump>>,
    ) -> Result<Self, ParserError> {
        let select = Self {
            with,
            body,
            order_by,
            limit,
        };
        #[cfg(feature = "extra_checks")]
        if let Self {
            order_by: Some(ref scs),
            ..
        } = select
        {
            if let ColumnCount::Fixed(n) = select.column_count() {
                for sc in scs {
                    sc.expr.check_range("ORDER", n)?;
                }
            }
        }
        Ok(select)
    }
}

/// `SELECT` body
#[derive(Debug, PartialEq, Eq)]
pub struct SelectBody<'bump> {
    /// first select
    pub select: OneSelect<'bump>,
    /// compounds
    pub compounds: Option<Vec<'bump, CompoundSelect<'bump>>>,
}

impl<'bump> SelectBody<'bump> {
    pub(crate) fn push(
        &mut self,
        cs: CompoundSelect<'bump>,
        b: &'bump Bump,
    ) -> Result<(), ParserError> {
        #[cfg(feature = "extra_checks")]
        if let ColumnCount::Fixed(n) = self.select.column_count() {
            if let ColumnCount::Fixed(m) = cs.select.column_count() {
                if n != m {
                    return Err(custom_err!(
                        "SELECTs to the left and right of {} do not have the same number of result columns",
                        cs.operator
                    ));
                }
            }
        }
        if let Some(ref mut v) = self.compounds {
            v.push(cs);
        } else {
            self.compounds = Some(bumpalo::vec![in b; cs]);
        }
        Ok(())
    }
}

/// Compound select
#[derive(Debug, PartialEq, Eq)]
pub struct CompoundSelect<'bump> {
    /// operator
    pub operator: CompoundOperator,
    /// select
    pub select: OneSelect<'bump>,
}

/// Compound operators
// https://sqlite.org/syntax/compound-operator.html
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum CompoundOperator {
    /// `UNION`
    Union,
    /// `UNION ALL`
    UnionAll,
    /// `EXCEPT`
    Except,
    /// `INTERSECT`
    Intersect,
}

/// `SELECT` core
// https://sqlite.org/syntax/select-core.html
#[derive(Debug, PartialEq, Eq)]
pub enum OneSelect<'bump> {
    /// `SELECT`
    Select {
        /// `DISTINCT`
        distinctness: Option<Distinctness>,
        /// columns
        columns: Vec<'bump, ResultColumn<'bump>>,
        /// `FROM` clause
        from: Option<FromClause<'bump>>,
        /// `WHERE` clause
        where_clause: Option<Box<'bump, Expr<'bump>>>,
        /// `GROUP BY`
        group_by: Option<Vec<'bump, Expr<'bump>>>,
        /// `HAVING`
        having: Option<Box<'bump, Expr<'bump>>>, // TODO: HAVING clause on a non-aggregate query
        /// `WINDOW` definition
        window_clause: Option<Vec<'bump, WindowDef<'bump>>>,
    },
    /// `VALUES`
    Values(Vec<'bump, Vec<'bump, Expr<'bump>>>),
}

impl<'bump> OneSelect<'bump> {
    /// Constructor
    pub fn new(
        distinctness: Option<Distinctness>,
        columns: Vec<'bump, ResultColumn<'bump>>,
        from: Option<FromClause<'bump>>,
        where_clause: Option<Expr<'bump>>,
        group_by: Option<Vec<'bump, Expr<'bump>>>,
        having: Option<Expr<'bump>>,
        window_clause: Option<Vec<'bump, WindowDef<'bump>>>,
        b: &'bump Bump,
    ) -> Result<Self, ParserError> {
        #[cfg(feature = "extra_checks")]
        if from.is_none()
            && columns
                .iter()
                .any(|rc| matches!(rc, ResultColumn::Star | ResultColumn::TableStar(_)))
        {
            return Err(custom_err!("no tables specified"));
        }
        let select = Self::Select {
            distinctness,
            columns,
            from,
            where_clause: where_clause.map(|wc| Box::new_in(wc, b)),
            group_by,
            having: having.map(|h| Box::new_in(h, b)),
            window_clause,
        };
        #[cfg(feature = "extra_checks")]
        if let Self::Select {
            group_by: Some(ref gb),
            ..
        } = select
        {
            if let ColumnCount::Fixed(n) = select.column_count() {
                for expr in gb {
                    expr.check_range("GROUP", n)?;
                }
            }
        }
        Ok(select)
    }
    /// Check all VALUES have the same number of terms
    pub fn push(
        values: &mut Vec<'bump, Vec<'bump, Expr<'bump>>>,
        v: Vec<'bump, Expr<'bump>>,
    ) -> Result<(), ParserError> {
        #[cfg(feature = "extra_checks")]
        if values[0].len() != v.len() {
            return Err(custom_err!("all VALUES must have the same number of terms"));
        }
        values.push(v);
        Ok(())
    }
}

/// `SELECT` ... `FROM` clause
// https://sqlite.org/syntax/join-clause.html
#[derive(Debug, PartialEq, Eq)]
pub struct FromClause<'bump> {
    /// table
    pub select: Option<Box<'bump, SelectTable<'bump>>>, // FIXME mandatory
    /// `JOIN`ed tabled
    pub joins: Option<Vec<'bump, JoinedSelectTable<'bump>>>,
    op: Option<JoinOperator>, // FIXME transient
}
impl<'bump> FromClause<'bump> {
    pub(crate) fn empty() -> Self {
        Self {
            select: None,
            joins: None,
            op: None,
        }
    }

    pub(crate) fn push(
        &mut self,
        table: SelectTable<'bump>,
        jc: Option<JoinConstraint<'bump>>,
        b: &'bump Bump,
    ) -> Result<(), ParserError> {
        let op = self.op.take();
        if let Some(op) = op {
            let jst = JoinedSelectTable {
                operator: op,
                table,
                constraint: jc,
            };
            if jst.operator.is_natural() && jst.constraint.is_some() {
                return Err(custom_err!(
                    "a NATURAL join may not have an ON or USING clause"
                ));
            }
            if let Some(ref mut joins) = self.joins {
                joins.push(jst);
            } else {
                self.joins = Some(bumpalo::vec![in b; jst]);
            }
        } else {
            if jc.is_some() {
                return Err(custom_err!("a JOIN clause is required before ON"));
            }
            debug_assert!(self.select.is_none());
            debug_assert!(self.joins.is_none());
            self.select = Some(Box::new_in(table, b));
        }
        Ok(())
    }

    pub(crate) fn push_op(&mut self, op: JoinOperator) {
        self.op = Some(op);
    }
}

/// `SELECT` distinctness
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Distinctness {
    /// `DISTINCT`
    Distinct,
    /// `ALL`
    All,
}

/// `SELECT` or `RETURNING` result column
// https://sqlite.org/syntax/result-column.html
#[derive(Debug, PartialEq, Eq)]
pub enum ResultColumn<'bump> {
    /// expression
    Expr(Expr<'bump>, Option<As<'bump>>),
    /// `*`
    Star,
    /// table name.`*`
    TableStar(Name<'bump>),
}

/// Alias
#[derive(Debug, PartialEq, Eq)]
pub enum As<'bump> {
    /// `AS`
    As(Name<'bump>),
    /// no `AS`
    Elided(Name<'bump>), // FIXME Ids
}

/// `JOIN` clause
// https://sqlite.org/syntax/join-clause.html
#[derive(Debug, PartialEq, Eq)]
pub struct JoinedSelectTable<'bump> {
    /// operator
    pub operator: JoinOperator,
    /// table
    pub table: SelectTable<'bump>,
    /// constraint
    pub constraint: Option<JoinConstraint<'bump>>,
}

/// Table or subquery
// https://sqlite.org/syntax/table-or-subquery.html
#[derive(Debug, PartialEq, Eq)]
pub enum SelectTable<'bump> {
    /// table
    Table(
        QualifiedName<'bump>,
        Option<As<'bump>>,
        Option<Indexed<'bump>>,
    ),
    /// table function call
    TableCall(
        QualifiedName<'bump>,
        Option<Vec<'bump, Expr<'bump>>>,
        Option<As<'bump>>,
    ),
    /// `SELECT` subquery
    Select(Box<'bump, Select<'bump>>, Option<As<'bump>>),
    /// subquery
    Sub(FromClause<'bump>, Option<As<'bump>>),
}

/// Join operators
// https://sqlite.org/syntax/join-operator.html
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum JoinOperator {
    /// `,`
    Comma,
    /// `JOIN`
    TypedJoin(Option<JoinType>),
}

impl JoinOperator {
    pub(crate) fn from(
        token: Token,
        n1: Option<Name>,
        n2: Option<Name>,
    ) -> Result<Self, ParserError> {
        Ok({
            let mut jt = JoinType::try_from(token.1)?;
            for n in [&n1, &n2].into_iter().flatten() {
                jt |= JoinType::try_from(n.0.as_str().as_bytes())?;
            }
            if (jt & (JoinType::INNER | JoinType::OUTER)) == (JoinType::INNER | JoinType::OUTER)
                || (jt & (JoinType::OUTER | JoinType::LEFT | JoinType::RIGHT)) == JoinType::OUTER
            {
                return Err(custom_err!(
                    "unknown join type: {} {} {}",
                    str::from_utf8(token.1).unwrap_or("invalid utf8"),
                    n1.as_ref().map_or("", |n| n.0.as_ref()),
                    n2.as_ref().map_or("", |n| n.0.as_ref())
                ));
            }
            Self::TypedJoin(Some(jt))
        })
    }
    fn is_natural(&self) -> bool {
        match self {
            Self::TypedJoin(Some(jt)) => jt.contains(JoinType::NATURAL),
            _ => false,
        }
    }
}

// https://github.com/sqlite/sqlite/blob/80511f32f7e71062026edd471913ef0455563964/src/select.c#L197-L257
bitflags::bitflags! {
    /// `JOIN` types
    #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
    pub struct JoinType: u8 {
        /// `INNER`
        const INNER   = 0x01;
        /// `CROSS` => INNER|CROSS
        const CROSS   = 0x02;
        /// `NATURAL`
        const NATURAL = 0x04;
        /// `LEFT` => LEFT|OUTER
        const LEFT    = 0x08;
        /// `RIGHT` => RIGHT|OUTER
        const RIGHT   = 0x10;
        /// `OUTER`
        const OUTER   = 0x20;
    }
}

impl TryFrom<&[u8]> for JoinType {
    type Error = ParserError;
    fn try_from(s: &[u8]) -> Result<Self, ParserError> {
        if b"CROSS".eq_ignore_ascii_case(s) {
            Ok(Self::INNER | Self::CROSS)
        } else if b"FULL".eq_ignore_ascii_case(s) {
            Ok(Self::LEFT | Self::RIGHT | Self::OUTER)
        } else if b"INNER".eq_ignore_ascii_case(s) {
            Ok(Self::INNER)
        } else if b"LEFT".eq_ignore_ascii_case(s) {
            Ok(Self::LEFT | Self::OUTER)
        } else if b"NATURAL".eq_ignore_ascii_case(s) {
            Ok(Self::NATURAL)
        } else if b"RIGHT".eq_ignore_ascii_case(s) {
            Ok(Self::RIGHT | Self::OUTER)
        } else if b"OUTER".eq_ignore_ascii_case(s) {
            Ok(Self::OUTER)
        } else {
            Err(custom_err!(
                "unknown join type: {}",
                str::from_utf8(s).unwrap_or("invalid utf8")
            ))
        }
    }
}

/// `JOIN` constraint
#[derive(Debug, PartialEq, Eq)]
pub enum JoinConstraint<'bump> {
    /// `ON`
    On(Expr<'bump>),
    /// `USING`: col names
    Using(DistinctNames<'bump>),
}

/// identifier or one of several keywords or `INDEXED`
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Id<'bump>(pub String<'bump>);

impl<'bump> Id<'bump> {
    /// Constructor
    pub fn from_token(ty: YYCODETYPE, token: Token, b: &'bump Bump) -> Self {
        Self(from_token(ty, token, b))
    }
}

// TODO ids (identifier or string)

/// identifier or string or `CROSS` or `FULL` or `INNER` or `LEFT` or `NATURAL` or `OUTER` or `RIGHT`.
#[derive(Clone, Debug, Eq)]
pub struct Name<'bump>(pub String<'bump>); // TODO distinction between Name and "Name"/[Name]/`Name`

pub(crate) fn unquote(s: &str) -> (&str, u8) {
    if s.is_empty() {
        return (s, 0);
    }
    let bytes = s.as_bytes();
    let mut quote = bytes[0];
    if quote != b'"' && quote != b'`' && quote != b'\'' && quote != b'[' {
        return (s, 0);
    } else if quote == b'[' {
        quote = b']';
    }
    debug_assert!(bytes.len() > 1);
    debug_assert_eq!(quote, bytes[bytes.len() - 1]);
    let sub = &s[1..bytes.len() - 1];
    if quote == b']' || sub.len() < 2 {
        (sub, 0)
    } else {
        (sub, quote)
    }
}

impl<'bump> Name<'bump> {
    /// Constructor
    pub fn from_token(ty: YYCODETYPE, token: Token, b: &'bump Bump) -> Self {
        Self(from_token(ty, token, b))
    }

    fn as_bytes(&self) -> QuotedIterator<'_> {
        let (sub, quote) = unquote(self.0.as_ref());
        QuotedIterator(sub.bytes(), quote)
    }
    #[cfg(feature = "extra_checks")]
    fn is_reserved(&self) -> bool {
        let bytes = self.as_bytes();
        let reserved = QuotedIterator("sqlite_".bytes(), 0);
        bytes.zip(reserved).fold(0u8, |acc, (b1, b2)| {
            acc + if b1.eq_ignore_ascii_case(&b2) { 1 } else { 0 }
        }) == 7u8
    }
}

struct QuotedIterator<'s>(Bytes<'s>, u8);
impl Iterator for QuotedIterator<'_> {
    type Item = u8;

    fn next(&mut self) -> Option<u8> {
        match self.0.next() {
            x @ Some(b) => {
                if b == self.1 && self.0.next() != Some(self.1) {
                    panic!("Malformed string literal: {:?}", self.0);
                }
                x
            }
            x => x,
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        if self.1 == 0 {
            return self.0.size_hint();
        }
        (0, None)
    }
}

fn eq_ignore_case_and_quote(mut it: QuotedIterator<'_>, mut other: QuotedIterator<'_>) -> bool {
    loop {
        match (it.next(), other.next()) {
            (Some(b1), Some(b2)) => {
                if !b1.eq_ignore_ascii_case(&b2) {
                    return false;
                }
            }
            (None, None) => break,
            _ => return false,
        }
    }
    true
}

/// Ignore case and quote
impl<'bump> std::hash::Hash for Name<'bump> {
    fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
        self.as_bytes()
            .for_each(|b| hasher.write_u8(b.to_ascii_lowercase()));
    }
}
/// Ignore case and quote
impl<'bump> PartialEq for Name<'bump> {
    fn eq(&self, other: &Self) -> bool {
        eq_ignore_case_and_quote(self.as_bytes(), other.as_bytes())
    }
}
/// Ignore case and quote
impl<'bump> PartialEq<str> for Name<'bump> {
    fn eq(&self, other: &str) -> bool {
        eq_ignore_case_and_quote(self.as_bytes(), QuotedIterator(other.bytes(), 0u8))
    }
}
/// Ignore case and quote
impl<'bump> PartialEq<&str> for Name<'bump> {
    fn eq(&self, other: &&str) -> bool {
        eq_ignore_case_and_quote(self.as_bytes(), QuotedIterator(other.bytes(), 0u8))
    }
}

/// Qualified name
#[derive(Debug, PartialEq, Eq)]
pub struct QualifiedName<'bump> {
    /// schema
    pub db_name: Option<Name<'bump>>,
    /// object name
    pub name: Name<'bump>,
    /// alias
    pub alias: Option<Name<'bump>>, // FIXME restrict alias usage (fullname vs xfullname)
}

impl<'bump> QualifiedName<'bump> {
    /// Constructor
    pub fn single(name: Name<'bump>) -> Self {
        Self {
            db_name: None,
            name,
            alias: None,
        }
    }
    /// Constructor
    pub fn fullname(db_name: Name<'bump>, name: Name<'bump>) -> Self {
        Self {
            db_name: Some(db_name),
            name,
            alias: None,
        }
    }
    /// Constructor
    pub fn xfullname(db_name: Name<'bump>, name: Name<'bump>, alias: Name<'bump>) -> Self {
        Self {
            db_name: Some(db_name),
            name,
            alias: Some(alias),
        }
    }
    /// Constructor
    pub fn alias(name: Name<'bump>, alias: Name<'bump>) -> Self {
        Self {
            db_name: None,
            name,
            alias: Some(alias),
        }
    }
}

/// Ordered set of distinct column names
#[derive(Debug, PartialEq, Eq)]
pub struct DistinctNames<'bump>(IndexSet<Name<'bump>>);

impl<'bump> DistinctNames<'bump> {
    /// Initialize
    pub fn new(name: Name<'bump>) -> Self {
        let mut dn = Self(IndexSet::new());
        dn.0.insert(name);
        dn
    }
    /// Single column name
    pub fn single(name: Name<'bump>) -> Self {
        let mut dn = Self(IndexSet::with_capacity(1));
        dn.0.insert(name);
        dn
    }
    /// Push a distinct name or fail
    pub fn insert(&mut self, name: Name<'bump>) -> Result<(), ParserError> {
        if self.0.contains(&name) {
            return Err(custom_err!("column \"{}\" specified more than once", name));
        }
        self.0.insert(name);
        Ok(())
    }
}
impl<'bump> Deref for DistinctNames<'bump> {
    type Target = IndexSet<Name<'bump>>;

    fn deref(&self) -> &IndexSet<Name<'bump>> {
        &self.0
    }
}

/// `ALTER TABLE` body
// https://sqlite.org/lang_altertable.html
#[derive(Debug, PartialEq, Eq)]
pub enum AlterTableBody<'bump> {
    /// `RENAME TO`: new table name
    RenameTo(Name<'bump>),
    /// `ADD COLUMN`
    AddColumn(ColumnDefinition<'bump>), // TODO distinction between ADD and ADD COLUMN
    /// `ALTER COLUMN _ DROP NOT NULL`
    DropColumnNotNull(Name<'bump>), // TODO distinction between ALTER and ALTER COLUMN
    /// `ALTER COLUMN _ SET NOT NULL`
    SetColumnNotNull(Name<'bump>, Option<ResolveType>), // TODO distinction between ALTER and ALTER COLUMN
    /// `RENAME COLUMN`
    RenameColumn {
        /// old name
        old: Name<'bump>,
        /// new name
        new: Name<'bump>,
    },
    /// `DROP COLUMN`
    DropColumn(Name<'bump>), // TODO distinction between DROP and DROP COLUMN
    /// `ADD CONSTRAINT`
    AddConstraint(NamedTableConstraint<'bump>), // TODO only CHECK constraint supported
    /// `DROP CONSTRAINT`
    DropConstraint(Name<'bump>),
}

bitflags::bitflags! {
    /// `CREATE TABLE` flags
    #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
    pub struct TabFlags: u32 {
        //const TF_Readonly = 0x00000001; // Read-only system table
        /// Has one or more hidden columns
        const HasHidden = 0x00000002;
        /// Table has a primary key
        const HasPrimaryKey = 0x00000004;
        /// Integer primary key is autoincrement
        const Autoincrement = 0x00000008;
        //const TF_HasStat1 = 0x00000010; // nRowLogEst set from sqlite_stat1
        /// Has one or more VIRTUAL columns
        const HasVirtual = 0x00000020;
        /// Has one or more STORED columns
        const HasStored = 0x00000040;
        /// Combo: HasVirtual + HasStored
        const HasGenerated = 0x00000060;
        /// No rowid. PRIMARY KEY is the key
        const WithoutRowid = 0x00000080;
        //const TF_MaybeReanalyze = 0x00000100; // Maybe run ANALYZE on this table
        // No user-visible "rowid" column
        //const NoVisibleRowid = 0x00000200;
        // Out-of-Order hidden columns
        //const OOOHidden = 0x00000400;
        /// Contains NOT NULL constraints
        const HasNotNull = 0x00000800;
        //const Shadow = 0x00001000; // True for a shadow table
        //const TF_HasStat4 = 0x00002000; // STAT4 info available for this table
        //const Ephemeral = 0x00004000; // An ephemeral table
        //const TF_Eponymous = 0x00008000; // An eponymous virtual table
        /// STRICT mode
        const Strict = 0x00010000;
    }
}

/// `CREATE TABLE` body
// https://sqlite.org/lang_createtable.html
// https://sqlite.org/syntax/create-table-stmt.html
#[derive(Debug, PartialEq, Eq)]
pub enum CreateTableBody<'bump> {
    /// columns and constraints
    ColumnsAndConstraints {
        /// table column definitions
        columns: IndexMap<Name<'bump>, ColumnDefinition<'bump>>,
        /// table constraints
        constraints: Option<Vec<'bump, NamedTableConstraint<'bump>>>,
        /// table flags
        flags: TabFlags,
    },
    /// `AS` select
    AsSelect(Box<'bump, Select<'bump>>),
}

impl<'bump> CreateTableBody<'bump> {
    /// Constructor
    pub fn columns_and_constraints(
        columns: IndexMap<Name<'bump>, ColumnDefinition<'bump>>,
        constraints: Option<Vec<'bump, NamedTableConstraint<'bump>>>,
        mut flags: TabFlags,
    ) -> Result<Self, ParserError> {
        for col in columns.values() {
            if col.flags.contains(ColFlags::PRIMKEY) {
                flags |= TabFlags::HasPrimaryKey;
            }
        }
        if let Some(ref constraints) = constraints {
            for c in constraints {
                if let NamedTableConstraint {
                    constraint: TableConstraint::PrimaryKey { .. },
                    ..
                } = c
                {
                    if flags.contains(TabFlags::HasPrimaryKey) {
                        // FIXME table name
                        #[cfg(feature = "extra_checks")]
                        return Err(custom_err!("table has more than one primary key"));
                    } else {
                        flags |= TabFlags::HasPrimaryKey;
                    }
                }
            }
        }
        Ok(Self::ColumnsAndConstraints {
            columns,
            constraints,
            flags,
        })
    }
}

bitflags::bitflags! {
    /// Column definition flags
    #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
    pub struct ColFlags: u16 {
        /// Column is part of the primary key
        const PRIMKEY = 0x0001;
        // A hidden column in a virtual table
        //const HIDDEN = 0x0002;
        /// Type name follows column name
        const HASTYPE = 0x0004;
        /// Column def contains "UNIQUE" or "PK"
        const UNIQUE = 0x0008;
        //const SORTERREF =  0x0010;   /* Use sorter-refs with this column */
        /// GENERATED ALWAYS AS ... VIRTUAL
        const VIRTUAL = 0x0020;
        /// GENERATED ALWAYS AS ... STORED
        const STORED = 0x0040;
        //const NOTAVAIL = 0x0080;   /* STORED column not yet calculated */
        //const BUSY = 0x0100; /* Blocks recursion on GENERATED columns */
        /// Has collating sequence name in zCnName
        const HASCOLL = 0x0200;
        //const NOEXPAND = 0x0400;   /* Omit this column when expanding "*" */
        /// Combo: STORED, VIRTUAL
        const GENERATED = Self::STORED.bits() | Self::VIRTUAL.bits();
        // Combo: HIDDEN, STORED, VIRTUAL
        //const NOINSERT = Self::HIDDEN.bits() | Self::STORED.bits() | Self::VIRTUAL.bits();
    }
}

/// Table column definition
// https://sqlite.org/syntax/column-def.html
#[derive(Debug, PartialEq, Eq)]
pub struct ColumnDefinition<'bump> {
    /// column name
    pub col_name: Name<'bump>,
    /// column type
    pub col_type: Option<Type<'bump>>,
    /// column constraints
    pub constraints: Vec<'bump, NamedColumnConstraint<'bump>>,
    /// column flags
    pub flags: ColFlags,
}

impl<'bump> ColumnDefinition<'bump> {
    /// Constructor
    pub fn new(
        col_name: Name<'bump>,
        mut col_type: Option<Type<'bump>>,
        constraints: Vec<'bump, NamedColumnConstraint<'bump>>,
        b: &'bump Bump,
    ) -> Result<Self, ParserError> {
        let mut flags = ColFlags::empty();
        #[allow(unused_variables)]
        let mut default = false;
        for constraint in &constraints {
            match &constraint.constraint {
                #[allow(unused_assignments)]
                ColumnConstraint::Default(..) => {
                    default = true;
                }
                ColumnConstraint::Collate { .. } => {
                    flags |= ColFlags::HASCOLL;
                }
                ColumnConstraint::Generated { typ, .. } => {
                    flags |= ColFlags::VIRTUAL;
                    if let Some(id) = typ {
                        if id.0.eq_ignore_ascii_case("STORED") {
                            flags |= ColFlags::STORED;
                        }
                    }
                }
                #[cfg(feature = "extra_checks")]
                ColumnConstraint::ForeignKey {
                    clause:
                        ForeignKeyClause {
                            tbl_name, columns, ..
                        },
                    ..
                } => {
                    // The child table may reference the primary key of the parent without specifying the primary key column
                    if columns.as_ref().map_or(0, Vec::len) > 1 {
                        return Err(custom_err!(
                            "foreign key on {} should reference only one column of table {}",
                            col_name,
                            tbl_name
                        ));
                    }
                }
                #[allow(unused_variables)]
                ColumnConstraint::PrimaryKey { auto_increment, .. } => {
                    #[cfg(feature = "extra_checks")]
                    if *auto_increment
                        && col_type.as_ref().is_none_or(|t| {
                            !unquote(t.name.as_str()).0.eq_ignore_ascii_case("INTEGER")
                        })
                    {
                        return Err(custom_err!(
                            "AUTOINCREMENT is only allowed on an INTEGER PRIMARY KEY"
                        ));
                    }
                    flags |= ColFlags::PRIMKEY | ColFlags::UNIQUE;
                }
                ColumnConstraint::Unique(..) => {
                    flags |= ColFlags::UNIQUE;
                }
                _ => {}
            }
        }
        #[cfg(feature = "extra_checks")]
        if flags.contains(ColFlags::PRIMKEY) && flags.intersects(ColFlags::GENERATED) {
            return Err(custom_err!(
                "generated columns cannot be part of the PRIMARY KEY"
            ));
        } else if default && flags.intersects(ColFlags::GENERATED) {
            return Err(custom_err!("cannot use DEFAULT on a generated column"));
        }
        if flags.intersects(ColFlags::GENERATED) {
            // https://github.com/sqlite/sqlite/blob/e452bf40a14aca57fd9047b330dff282f3e4bbcc/src/build.c#L1511-L1514
            if let Some(ref mut col_type) = col_type {
                let mut split = col_type.name.split_ascii_whitespace();
                if split
                    .next_back()
                    .is_some_and(|s| s.eq_ignore_ascii_case("ALWAYS"))
                    && split
                        .next_back()
                        .is_some_and(|s| s.eq_ignore_ascii_case("GENERATED"))
                {
                    // str_split_whitespace_remainder
                    let mut new_type = String::new_in(b);
                    for (i, item) in split.enumerate() {
                        if i > 0 {
                            new_type.push(' ');
                        }
                        new_type.push_str(item);
                    }
                    col_type.name = new_type;
                }
            }
        }
        if col_type.as_ref().is_some_and(|t| !t.name.is_empty()) {
            flags |= ColFlags::HASTYPE;
        }
        Ok(Self {
            col_name,
            col_type,
            constraints,
            flags,
        })
    }
    /// Collector
    pub fn add_column(
        columns: &mut IndexMap<Name<'bump>, Self>,
        cd: Self,
    ) -> Result<(), ParserError> {
        let col_name = &cd.col_name;
        if columns.contains_key(col_name) {
            return Err(custom_err!("duplicate column name: {}", col_name));
        } else if cd.flags.contains(ColFlags::PRIMKEY)
            && columns
                .values()
                .any(|c| c.flags.contains(ColFlags::PRIMKEY))
        {
            #[cfg(feature = "extra_checks")]
            return Err(custom_err!("table has more than one primary key")); // FIXME table name
        }
        columns.insert(col_name.clone(), cd);
        Ok(())
    }
}

/// Named column constraint
// https://sqlite.org/syntax/column-constraint.html
#[derive(Debug, PartialEq, Eq)]
pub struct NamedColumnConstraint<'bump> {
    /// constraint name
    pub name: Option<Name<'bump>>,
    /// constraint
    pub constraint: ColumnConstraint<'bump>,
}

/// Column constraint
// https://sqlite.org/syntax/column-constraint.html
#[derive(Debug, PartialEq, Eq)]
pub enum ColumnConstraint<'bump> {
    /// `PRIMARY KEY`
    PrimaryKey {
        /// `ASC` / `DESC`
        order: Option<SortOrder>,
        /// `ON CONFLICT` clause
        conflict_clause: Option<ResolveType>,
        /// `AUTOINCREMENT`
        auto_increment: bool,
    },
    /// `NULL`
    NotNull {
        /// `NOT`
        nullable: bool,
        /// `ON CONFLICT` clause
        conflict_clause: Option<ResolveType>,
    },
    /// `UNIQUE`
    Unique(Option<ResolveType>),
    /// `CHECK`
    Check(Expr<'bump>),
    /// `DEFAULT`
    Default(Expr<'bump>),
    /// `DEFERRABLE`
    Defer(DeferSubclause), // FIXME
    /// `COLLATE`
    Collate {
        /// collation name
        collation_name: Name<'bump>, // FIXME Ids
    },
    /// `REFERENCES` foreign-key clause
    ForeignKey {
        /// clause
        clause: ForeignKeyClause<'bump>,
        /// `DEFERRABLE`
        defer_clause: Option<DeferSubclause>,
    },
    /// `GENERATED`
    Generated {
        /// expression
        expr: Expr<'bump>,
        /// `STORED` / `VIRTUAL`
        typ: Option<Id<'bump>>,
    },
}

/// Named table constraint
// https://sqlite.org/syntax/table-constraint.html
#[derive(Debug, PartialEq, Eq)]
pub struct NamedTableConstraint<'bump> {
    /// constraint name
    pub name: Option<Name<'bump>>,
    /// constraint
    pub constraint: TableConstraint<'bump>,
}

/// Table constraint
// https://sqlite.org/syntax/table-constraint.html
#[derive(Debug, PartialEq, Eq)]
pub enum TableConstraint<'bump> {
    /// `PRIMARY KEY`
    PrimaryKey {
        /// columns
        columns: Vec<'bump, SortedColumn<'bump>>,
        /// `AUTOINCREMENT`
        auto_increment: bool,
        /// `ON CONFLICT` clause
        conflict_clause: Option<ResolveType>,
    },
    /// `UNIQUE`
    Unique {
        /// columns
        columns: Vec<'bump, SortedColumn<'bump>>,
        /// `ON CONFLICT` clause
        conflict_clause: Option<ResolveType>,
    },
    /// `CHECK`
    Check(Expr<'bump>, Option<ResolveType>),
    /// `FOREIGN KEY`
    ForeignKey {
        /// columns
        columns: Vec<'bump, IndexedColumn<'bump>>,
        /// `REFERENCES`
        clause: ForeignKeyClause<'bump>,
        /// `DEFERRABLE`
        defer_clause: Option<DeferSubclause>,
    },
}

impl<'bump> TableConstraint<'bump> {
    /// PK constructor
    pub fn primary_key(
        columns: Vec<'bump, SortedColumn<'bump>>,
        auto_increment: bool,
        conflict_clause: Option<ResolveType>,
    ) -> Result<Self, ParserError> {
        has_expression(&columns)?;
        has_explicit_nulls(&columns)?;
        Ok(Self::PrimaryKey {
            columns,
            auto_increment,
            conflict_clause,
        })
    }
    /// UNIQUE constructor
    pub fn unique(
        columns: Vec<'bump, SortedColumn<'bump>>,
        conflict_clause: Option<ResolveType>,
    ) -> Result<Self, ParserError> {
        has_expression(&columns)?;
        has_explicit_nulls(&columns)?;
        Ok(Self::Unique {
            columns,
            conflict_clause,
        })
    }
}

/// Sort orders
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum SortOrder {
    /// `ASC`
    Asc,
    /// `DESC`
    Desc,
}

/// `NULLS FIRST` or `NULLS LAST`
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum NullsOrder {
    /// `NULLS FIRST`
    First,
    /// `NULLS LAST`
    Last,
}

/// `REFERENCES` clause
// https://sqlite.org/syntax/foreign-key-clause.html
#[derive(Debug, PartialEq, Eq)]
pub struct ForeignKeyClause<'bump> {
    /// foreign table name
    pub tbl_name: Name<'bump>,
    /// foreign table columns
    pub columns: Option<Vec<'bump, IndexedColumn<'bump>>>,
    /// referential action(s) / deferrable option(s)
    pub args: Vec<'bump, RefArg<'bump>>,
}

/// foreign-key reference args
#[derive(Debug, PartialEq, Eq)]
pub enum RefArg<'bump> {
    /// `ON DELETE`
    OnDelete(RefAct),
    /// `ON INSERT`
    OnInsert(RefAct),
    /// `ON UPDATE`
    OnUpdate(RefAct),
    /// `MATCH`
    Match(Name<'bump>),
}

/// foreign-key reference actions
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum RefAct {
    /// `SET NULL`
    SetNull,
    /// `SET DEFAULT`
    SetDefault,
    /// `CASCADE`
    Cascade,
    /// `RESTRICT`
    Restrict,
    /// `NO ACTION`
    NoAction,
}

/// foreign-key defer clause
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DeferSubclause {
    /// `DEFERRABLE`
    pub deferrable: bool,
    /// `INITIALLY` `DEFERRED` / `IMMEDIATE`
    pub init_deferred: Option<InitDeferredPred>,
}

/// `INITIALLY` `DEFERRED` / `IMMEDIATE`
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum InitDeferredPred {
    /// `INITIALLY DEFERRED`
    InitiallyDeferred,
    /// `INITIALLY IMMEDIATE`
    InitiallyImmediate, // default
}

/// Indexed column
// https://sqlite.org/syntax/indexed-column.html
#[derive(Debug, PartialEq, Eq)]
pub struct IndexedColumn<'bump> {
    /// column name
    pub col_name: Name<'bump>,
    /// `COLLATE`
    pub collation_name: Option<Name<'bump>>, // FIXME Ids
    /// `ORDER BY`
    pub order: Option<SortOrder>,
}

/// `INDEXED BY` / `NOT INDEXED`
#[derive(Debug, PartialEq, Eq)]
pub enum Indexed<'bump> {
    /// `INDEXED BY`: idx name
    IndexedBy(Name<'bump>),
    /// `NOT INDEXED`
    NotIndexed,
}

/// Sorted column
#[derive(Debug, PartialEq, Eq)]
pub struct SortedColumn<'bump> {
    /// expression
    pub expr: Expr<'bump>,
    /// `ASC` / `DESC`
    pub order: Option<SortOrder>,
    /// `NULLS FIRST` / `NULLS LAST`
    pub nulls: Option<NullsOrder>,
}

fn has_expression(columns: &Vec<SortedColumn>) -> Result<(), ParserError> {
    for _column in columns {
        if false {
            return Err(custom_err!(
                "expressions prohibited in PRIMARY KEY and UNIQUE constraints"
            ));
        }
    }
    Ok(())
}
#[allow(unused_variables)]
fn has_explicit_nulls(columns: &[SortedColumn]) -> Result<(), ParserError> {
    #[cfg(feature = "extra_checks")]
    for column in columns {
        if let Some(ref nulls) = column.nulls {
            return Err(custom_err!(
                "unsupported use of NULLS {}",
                if *nulls == NullsOrder::First {
                    "FIRST"
                } else {
                    "LAST"
                }
            ));
        }
    }
    Ok(())
}

/// `LIMIT`
#[derive(Debug, PartialEq, Eq)]
pub struct Limit<'bump> {
    /// count
    pub expr: Expr<'bump>,
    /// `OFFSET`
    pub offset: Option<Expr<'bump>>, // TODO distinction between LIMIT offset, count and LIMIT count OFFSET offset
}

/// `INSERT` body
// https://sqlite.org/lang_insert.html
// https://sqlite.org/syntax/insert-stmt.html
#[derive(Debug, PartialEq, Eq)]
pub enum InsertBody<'bump> {
    /// `SELECT` or `VALUES`
    Select(Box<'bump, Select<'bump>>, Option<Box<'bump, Upsert<'bump>>>),
    /// `DEFAULT VALUES`
    DefaultValues,
}

/// `UPDATE ... SET`
#[derive(Debug, PartialEq, Eq)]
pub struct Set<'bump> {
    /// column name(s)
    pub col_names: DistinctNames<'bump>,
    /// expression
    pub expr: Expr<'bump>,
}

/// `PRAGMA` body
// https://sqlite.org/syntax/pragma-stmt.html
#[derive(Debug, PartialEq, Eq)]
pub enum PragmaBody<'bump> {
    /// `=`
    Equals(PragmaValue<'bump>),
    /// function call
    Call(PragmaValue<'bump>),
}

/// `PRAGMA` value
// https://sqlite.org/syntax/pragma-value.html
pub type PragmaValue<'bump> = Expr<'bump>; // TODO

/// `CREATE TRIGGER` time
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TriggerTime {
    /// `BEFORE`
    Before, // default
    /// `AFTER`
    After,
    /// `INSTEAD OF`
    InsteadOf,
}

/// `CREATE TRIGGER` event
#[derive(Debug, PartialEq, Eq)]
pub enum TriggerEvent<'bump> {
    /// `DELETE`
    Delete,
    /// `INSERT`
    Insert,
    /// `UPDATE`
    Update,
    /// `UPDATE OF`: col names
    UpdateOf(DistinctNames<'bump>),
}

/// `CREATE TRIGGER` command
// https://sqlite.org/lang_createtrigger.html
// https://sqlite.org/syntax/create-trigger-stmt.html
#[derive(Debug, PartialEq, Eq)]
pub enum TriggerCmd<'bump> {
    /// `UPDATE`
    Update {
        /// `OR`
        or_conflict: Option<ResolveType>,
        /// table name
        tbl_name: QualifiedName<'bump>,
        /// `SET` assignments
        sets: Vec<'bump, Set<'bump>>, // FIXME unique
        /// `FROM`
        from: Option<FromClause<'bump>>,
        /// `WHERE` clause
        where_clause: Option<Expr<'bump>>,
    },
    /// `INSERT`
    Insert {
        /// `OR`
        or_conflict: Option<ResolveType>,
        /// table name
        tbl_name: QualifiedName<'bump>,
        /// `COLUMNS`
        col_names: Option<DistinctNames<'bump>>,
        /// `SELECT` or `VALUES`
        select: Box<'bump, Select<'bump>>,
        /// `ON CONFLICT` clause
        upsert: Option<Box<'bump, Upsert<'bump>>>,
    },
    /// `DELETE`
    Delete {
        /// table name
        tbl_name: QualifiedName<'bump>,
        /// `WHERE` clause
        where_clause: Option<Expr<'bump>>,
    },
    /// `SELECT`
    Select(Box<'bump, Select<'bump>>),
}

/// Conflict resolution types
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ResolveType {
    /// `ROLLBACK`
    Rollback,
    /// `ABORT`
    Abort, // default
    /// `FAIL`
    Fail,
    /// `IGNORE`
    Ignore,
    /// `REPLACE`
    Replace,
}

/// `WITH` clause
// https://sqlite.org/lang_with.html
// https://sqlite.org/syntax/with-clause.html
#[derive(Debug, PartialEq, Eq)]
pub struct With<'bump> {
    /// `RECURSIVE`
    pub recursive: bool,
    /// CTEs
    pub ctes: Vec<'bump, CommonTableExpr<'bump>>,
}

/// CTE materialization
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Materialized {
    /// No hint
    Any,
    /// `MATERIALIZED`
    Yes,
    /// `NOT MATERIALIZED`
    No,
}

/// CTE
// https://sqlite.org/syntax/common-table-expression.html
#[derive(Debug, PartialEq, Eq)]
pub struct CommonTableExpr<'bump> {
    /// table name
    pub tbl_name: Name<'bump>,
    /// table columns
    pub columns: Option<Vec<'bump, IndexedColumn<'bump>>>, // TODO: check no duplicate (eidlist_opt)
    /// `MATERIALIZED`
    pub materialized: Materialized,
    /// query
    pub select: Box<'bump, Select<'bump>>,
}

impl<'bump> CommonTableExpr<'bump> {
    /// Constructor
    pub fn new(
        tbl_name: Name<'bump>,
        columns: Option<Vec<'bump, IndexedColumn<'bump>>>,
        materialized: Materialized,
        select: Select<'bump>,
        b: &'bump Bump,
    ) -> Result<Self, ParserError> {
        #[cfg(feature = "extra_checks")]
        if let Some(ref columns) = columns {
            if let check::ColumnCount::Fixed(cc) = select.column_count() {
                if cc as usize != columns.len() {
                    return Err(custom_err!(
                        "table {} has {} values for {} columns",
                        tbl_name,
                        cc,
                        columns.len()
                    ));
                }
            }
        }
        Ok(Self {
            tbl_name,
            columns,
            materialized,
            select: Box::new_in(select, b),
        })
    }
    /// Constructor
    pub fn add_cte(ctes: &mut Vec<Self>, cte: Self) -> Result<(), ParserError> {
        #[cfg(feature = "extra_checks")]
        if ctes.iter().any(|c| c.tbl_name == cte.tbl_name) {
            return Err(custom_err!("duplicate WITH table name: {}", cte.tbl_name));
        }
        ctes.push(cte);
        Ok(())
    }
}

/// Column type
// https://sqlite.org/syntax/type-name.html
#[derive(Debug, PartialEq, Eq)]
pub struct Type<'bump> {
    /// type name
    pub name: String<'bump>, // TODO Validate: Ids+
    /// type size
    pub size: Option<TypeSize<'bump>>,
}

/// Column type size limit(s)
// https://sqlite.org/syntax/type-name.html
#[derive(Debug, PartialEq, Eq)]
pub enum TypeSize<'bump> {
    /// maximum size
    MaxSize(Box<'bump, Expr<'bump>>),
    /// precision
    TypeSize(Box<'bump, Expr<'bump>>, Box<'bump, Expr<'bump>>),
}

/// Transaction types
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TransactionType {
    /// `DEFERRED`
    Deferred, // default
    /// `IMMEDIATE`
    Immediate,
    /// `EXCLUSIVE`
    Exclusive,
}

/// Upsert clause
// https://sqlite.org/lang_upsert.html
// https://sqlite.org/syntax/upsert-clause.html
#[derive(Debug, PartialEq, Eq)]
pub struct Upsert<'bump> {
    /// conflict targets
    pub index: Option<UpsertIndex<'bump>>,
    /// `DO` clause
    pub do_clause: UpsertDo<'bump>,
    /// next upsert
    pub next: Option<Box<'bump, Upsert<'bump>>>,
}

/// Upsert conflict targets
#[derive(Debug, PartialEq, Eq)]
pub struct UpsertIndex<'bump> {
    /// columns
    pub targets: Vec<'bump, SortedColumn<'bump>>,
    /// `WHERE` clause
    pub where_clause: Option<Expr<'bump>>,
}

impl<'bump> UpsertIndex<'bump> {
    /// constructor
    pub fn new(
        targets: Vec<'bump, SortedColumn<'bump>>,
        where_clause: Option<Expr<'bump>>,
    ) -> Result<Self, ParserError> {
        has_explicit_nulls(&targets)?;
        Ok(Self {
            targets,
            where_clause,
        })
    }
}

/// Upsert `DO` action
#[derive(Debug, PartialEq, Eq)]
pub enum UpsertDo<'bump> {
    /// `SET`
    Set {
        /// assignments
        sets: Vec<'bump, Set<'bump>>,
        /// `WHERE` clause
        where_clause: Option<Expr<'bump>>,
    },
    /// `NOTHING`
    Nothing,
}

/// Function call tail
#[derive(Debug, PartialEq, Eq)]
pub struct FunctionTail<'bump> {
    /// `FILTER` clause
    pub filter_clause: Option<Box<'bump, Expr<'bump>>>,
    /// `OVER` clause
    pub over_clause: Option<Box<'bump, Over<'bump>>>,
}

/// Function call `OVER` clause
// https://sqlite.org/syntax/over-clause.html
#[derive(Debug, PartialEq, Eq)]
pub enum Over<'bump> {
    /// Window definition
    Window(Box<'bump, Window<'bump>>),
    /// Window name
    Name(Name<'bump>),
}

/// `OVER` window definition
#[derive(Debug, PartialEq, Eq)]
pub struct WindowDef<'bump> {
    /// window name
    pub name: Name<'bump>,
    /// window definition
    pub window: Window<'bump>,
}

/// Window definition
// https://sqlite.org/syntax/window-defn.html
#[derive(Debug, PartialEq, Eq)]
pub struct Window<'bump> {
    /// base window name
    pub base: Option<Name<'bump>>,
    /// `PARTITION BY`
    pub partition_by: Option<Vec<'bump, Expr<'bump>>>,
    /// `ORDER BY`
    pub order_by: Option<Vec<'bump, SortedColumn<'bump>>>,
    /// frame spec
    pub frame_clause: Option<FrameClause<'bump>>,
}

/// Frame specification
// https://sqlite.org/syntax/frame-spec.html
#[derive(Debug, PartialEq, Eq)]
pub struct FrameClause<'bump> {
    /// unit
    pub mode: FrameMode,
    /// start bound
    pub start: FrameBound<'bump>,
    /// end bound
    pub end: Option<FrameBound<'bump>>,
    /// `EXCLUDE`
    pub exclude: Option<FrameExclude>,
}

/// Frame modes
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum FrameMode {
    /// `GROUPS`
    Groups,
    /// `RANGE`
    Range,
    /// `ROWS`
    Rows,
}

/// Frame bounds
#[derive(Debug, PartialEq, Eq)]
pub enum FrameBound<'bump> {
    /// `CURRENT ROW`
    CurrentRow,
    /// `FOLLOWING`
    Following(Expr<'bump>),
    /// `PRECEDING`
    Preceding(Expr<'bump>),
    /// `UNBOUNDED FOLLOWING`
    UnboundedFollowing,
    /// `UNBOUNDED PRECEDING`
    UnboundedPreceding,
}

/// Frame exclusions
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FrameExclude {
    /// `NO OTHERS`
    NoOthers,
    /// `CURRENT ROW`
    CurrentRow,
    /// `GROUP`
    Group,
    /// `TIES`
    Ties,
}

#[cfg(test)]
mod test {
    use super::Name;

    #[test]
    fn test_dequote() {
        let b = bumpalo::Bump::new();
        assert_eq!(name("x", &b), "x");
        assert_eq!(name("`x`", &b), "x");
        assert_eq!(name("`x``y`", &b), "x`y");
        assert_eq!(name(r#""x""#, &b), "x");
        assert_eq!(name(r#""x""y""#, &b), "x\"y");
        assert_eq!(name("[x]", &b), "x");
    }

    fn name<'bump>(s: &'static str, b: &'bump bumpalo::Bump) -> Name<'bump> {
        Name(bumpalo::collections::String::from_str_in(s, b))
    }
}
