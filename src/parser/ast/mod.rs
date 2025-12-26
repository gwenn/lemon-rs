//! Abstract Syntax Tree
#[cfg(feature = "extra_checks")]
pub mod check;
pub mod fmt;

use std::num::ParseIntError;
use std::ops::Deref;
use std::str::{self, Bytes, FromStr};

#[cfg(feature = "extra_checks")]
use check::ColumnCount;
use fmt::TokenStream;
use indexmap::{IndexMap, IndexSet};

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
    pub names: IndexSet<String>,
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Cmd {
    /// `EXPLAIN` statement
    Explain(Stmt),
    /// `EXPLAIN QUERY PLAN` statement
    ExplainQueryPlan(Stmt),
    /// statement
    Stmt(Stmt),
}

pub(crate) enum ExplainKind {
    Explain,
    QueryPlan,
}

/// SQL statement
// https://sqlite.org/syntax/sql-stmt.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Stmt {
    /// `ALTER TABLE`: table name, body
    AlterTable(QualifiedName, AlterTableBody),
    /// `ANALYSE`: object name
    Analyze(Option<QualifiedName>),
    /// `ATTACH DATABASE`
    Attach {
        /// filename
        // TODO distinction between ATTACH and ATTACH DATABASE
        expr: Expr,
        /// schema name
        db_name: Expr,
        /// password
        key: Option<Expr>,
    },
    /// `BEGIN`: tx type, tx name
    Begin(Option<TransactionType>, Option<Name>),
    /// `COMMIT`/`END`: tx name
    Commit(Option<Name>), // TODO distinction between COMMIT and END
    /// `CREATE INDEX`
    CreateIndex {
        /// `UNIQUE`
        unique: bool,
        /// `IF NOT EXISTS`
        if_not_exists: bool,
        /// index name
        idx_name: QualifiedName,
        /// table name
        tbl_name: Name,
        /// indexed columns or expressions
        columns: Vec<SortedColumn>,
        /// partial index
        where_clause: Option<Expr>,
    },
    /// `CREATE TABLE`
    CreateTable {
        /// `TEMPORARY`
        temporary: bool, // TODO distinction between TEMP and TEMPORARY
        /// `IF NOT EXISTS`
        if_not_exists: bool,
        /// table name
        tbl_name: QualifiedName,
        /// table body
        body: CreateTableBody,
    },
    /// `CREATE TRIGGER`
    CreateTrigger {
        /// `TEMPORARY`
        temporary: bool,
        /// `IF NOT EXISTS`
        if_not_exists: bool,
        /// trigger name
        trigger_name: QualifiedName,
        /// `BEFORE`/`AFTER`/`INSTEAD OF`
        time: Option<TriggerTime>,
        /// `DELETE`/`INSERT`/`UPDATE`
        event: TriggerEvent,
        /// table name
        tbl_name: QualifiedName,
        /// `FOR EACH ROW`
        for_each_row: bool,
        /// `WHEN`
        when_clause: Option<Expr>,
        /// statements
        commands: Vec<TriggerCmd>,
    },
    /// `CREATE VIEW`
    CreateView {
        /// `TEMPORARY`
        temporary: bool,
        /// `IF NOT EXISTS`
        if_not_exists: bool,
        /// view name
        view_name: QualifiedName,
        /// columns
        columns: Option<Vec<IndexedColumn>>, // TODO check no duplicate directly
        /// query
        select: Box<Select>,
    },
    /// `CREATE VIRTUAL TABLE`
    CreateVirtualTable {
        /// `IF NOT EXISTS`
        if_not_exists: bool,
        /// table name
        tbl_name: QualifiedName,
        /// module
        module_name: Name,
        /// args
        args: Option<Vec<Box<str>>>,
    },
    /// `DELETE`
    Delete {
        /// CTE
        with: Option<With>, // TODO check usages in where_clause
        /// `FROM` table name
        tbl_name: QualifiedName,
        /// `INDEXED`
        indexed: Option<Indexed>,
        /// `WHERE` clause
        where_clause: Option<Expr>,
        /// `RETURNING`
        returning: Option<Vec<ResultColumn>>,
        /// `ORDER BY`
        order_by: Option<Vec<SortedColumn>>,
        /// `LIMIT`
        limit: Option<Box<Limit>>,
    },
    /// `DETACH DATABASE`: db name
    Detach(Expr), // TODO distinction between DETACH and DETACH DATABASE
    /// `DROP INDEX`
    DropIndex {
        /// `IF EXISTS`
        if_exists: bool,
        /// index name
        idx_name: QualifiedName,
    },
    /// `DROP TABLE`
    DropTable {
        /// `IF EXISTS`
        if_exists: bool,
        /// table name
        tbl_name: QualifiedName,
    },
    /// `DROP TRIGGER`
    DropTrigger {
        /// `IF EXISTS`
        if_exists: bool,
        /// trigger name
        trigger_name: QualifiedName,
    },
    /// `DROP VIEW`
    DropView {
        /// `IF EXISTS`
        if_exists: bool,
        /// view name
        view_name: QualifiedName,
    },
    /// `INSERT`
    Insert {
        /// CTE
        with: Option<With>, // TODO check usages in body
        /// `OR`
        or_conflict: Option<ResolveType>, // TODO distinction between REPLACE and INSERT OR REPLACE
        /// table name
        tbl_name: QualifiedName,
        /// `COLUMNS`
        columns: Option<DistinctNames>,
        /// `VALUES` or `SELECT`
        body: InsertBody,
        /// `RETURNING`
        returning: Option<Vec<ResultColumn>>,
    },
    /// `PRAGMA`: pragma name, body
    Pragma(QualifiedName, Option<PragmaBody>),
    /// `REINDEX`
    Reindex {
        /// collation or index or table name
        obj_name: Option<QualifiedName>,
    },
    /// `RELEASE`: savepoint name
    Release(Name), // TODO distinction between RELEASE and RELEASE SAVEPOINT
    /// `ROLLBACK`
    Rollback {
        /// transaction name
        tx_name: Option<Name>,
        /// savepoint name
        savepoint_name: Option<Name>, // TODO distinction between TO and TO SAVEPOINT
    },
    /// `SAVEPOINT`: savepoint name
    Savepoint(Name),
    /// `SELECT`
    Select(Box<Select>),
    /// `UPDATE`
    Update {
        /// CTE
        with: Option<With>, // TODO check usages in where_clause
        /// `OR`
        or_conflict: Option<ResolveType>,
        /// table name
        tbl_name: QualifiedName,
        /// `INDEXED`
        indexed: Option<Indexed>,
        /// `SET` assignments
        sets: Vec<Set>, // FIXME unique
        /// `FROM`
        from: Option<FromClause>,
        /// `WHERE` clause
        where_clause: Option<Expr>,
        /// `RETURNING`
        returning: Option<Vec<ResultColumn>>,
        /// `ORDER BY`
        order_by: Option<Vec<SortedColumn>>,
        /// `LIMIT`
        limit: Option<Box<Limit>>,
    },
    /// `VACUUM`: database name, into expr
    Vacuum(Option<Name>, Option<Expr>),
}

impl Stmt {
    /// CREATE INDEX constructor
    pub fn create_index(
        unique: bool,
        if_not_exists: bool,
        idx_name: QualifiedName,
        tbl_name: Name,
        columns: Vec<SortedColumn>,
        where_clause: Option<Expr>,
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
        with: Option<With>,
        or_conflict: Option<ResolveType>,
        tbl_name: QualifiedName,
        indexed: Option<Indexed>,
        sets: Vec<Set>, // FIXME unique
        from: Option<FromClause>,
        where_clause: Option<Expr>,
        returning: Option<Vec<ResultColumn>>,
        order_by: Option<Vec<SortedColumn>>,
        limit: Option<Limit>,
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr {
    /// `BETWEEN`
    Between {
        /// expression
        lhs: Box<Expr>,
        /// `NOT`
        not: bool,
        /// start
        start: Box<Expr>,
        /// end
        end: Box<Expr>,
    },
    /// binary expression
    Binary(Box<Expr>, Operator, Box<Expr>),
    /// `CASE` expression
    Case {
        /// operand
        base: Option<Box<Expr>>,
        /// `WHEN` condition `THEN` result
        when_then_pairs: Vec<(Expr, Expr)>,
        /// `ELSE` result
        else_expr: Option<Box<Expr>>,
    },
    /// CAST expression
    Cast {
        /// expression
        expr: Box<Expr>,
        /// `AS` type name
        type_name: Option<Type>,
    },
    /// `COLLATE`: expression
    Collate(Box<Expr>, Box<str>),
    /// schema-name.table-name.column-name
    DoublyQualified(Name, Name, Name),
    /// `EXISTS` subquery
    Exists(Box<Select>),
    /// call to a built-in function
    FunctionCall {
        /// function name
        name: Id,
        /// `DISTINCT`
        distinctness: Option<Distinctness>,
        /// arguments
        args: Option<Vec<Expr>>,
        /// `ORDER BY` or `WITHIN GROUP`
        order_by: Option<FunctionCallOrder>,
        /// `FILTER`
        filter_over: Option<FunctionTail>,
    },
    /// Function call expression with '*' as arg
    FunctionCallStar {
        /// function name
        name: Id,
        /// `FILTER`
        filter_over: Option<FunctionTail>,
    },
    /// Identifier
    Id(Id),
    /// `IN`
    InList {
        /// expression
        lhs: Box<Expr>,
        /// `NOT`
        not: bool,
        /// values
        rhs: Option<Vec<Expr>>,
    },
    /// `IN` subselect
    InSelect {
        /// expression
        lhs: Box<Expr>,
        /// `NOT`
        not: bool,
        /// subquery
        rhs: Box<Select>,
    },
    /// `IN` table name / function
    InTable {
        /// expression
        lhs: Box<Expr>,
        /// `NOT`
        not: bool,
        /// table name
        rhs: QualifiedName,
        /// table function arguments
        args: Option<Vec<Expr>>,
    },
    /// `IS NULL`
    IsNull(Box<Expr>),
    /// `LIKE`
    Like {
        /// expression
        lhs: Box<Expr>,
        /// `NOT`
        not: bool,
        /// operator
        op: LikeOperator,
        /// pattern
        rhs: Box<Expr>,
        /// `ESCAPE` char
        escape: Option<Box<Expr>>,
    },
    /// Literal expression
    Literal(Literal),
    /// Name
    Name(Name),
    /// `NOT NULL` or `NOTNULL`
    NotNull(Box<Expr>),
    /// Parenthesized subexpression
    Parenthesized(Vec<Expr>),
    /// Qualified name
    Qualified(Name, Name),
    /// `RAISE` function call
    Raise(ResolveType, Option<Box<Expr>>),
    /// Subquery expression
    Subquery(Box<Select>),
    /// Unary expression
    Unary(UnaryOperator, Box<Expr>),
    /// Parameters
    Variable(Box<str>),
}

/// Function call order
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FunctionCallOrder {
    /// `ORDER BY cols`
    SortList(Vec<SortedColumn>),
    /// `WITHIN GROUP (ORDER BY expr)`
    #[cfg(feature = "SQLITE_ENABLE_ORDERED_SET_AGGREGATES")]
    WithinGroup(Box<Expr>),
}

impl FunctionCallOrder {
    /// Constructor
    #[cfg(feature = "SQLITE_ENABLE_ORDERED_SET_AGGREGATES")]
    pub fn within_group(expr: Expr) -> Self {
        Self::WithinGroup(Box::new(expr))
    }
}

impl Expr {
    /// Constructor
    pub fn parenthesized(x: Self) -> Self {
        Self::Parenthesized(vec![x])
    }
    /// Constructor
    pub fn id(xt: YYCODETYPE, x: Token) -> Self {
        Self::Id(Id::from_token(xt, x))
    }
    /// Constructor
    pub fn collate(x: Self, ct: YYCODETYPE, c: Token) -> Self {
        Self::Collate(Box::new(x), from_token(ct, c))
    }
    /// Constructor
    pub fn cast(x: Self, type_name: Option<Type>) -> Self {
        Self::Cast {
            expr: Box::new(x),
            type_name,
        }
    }
    /// Constructor
    pub fn binary(left: Self, op: YYCODETYPE, right: Self) -> Self {
        Self::Binary(Box::new(left), Operator::from(op), Box::new(right))
    }
    /// Constructor
    pub fn ptr(left: Self, op: Token, right: Self) -> Self {
        let mut ptr = Operator::ArrowRight;
        if op.1 == b"->>" {
            ptr = Operator::ArrowRightShift;
        }
        Self::Binary(Box::new(left), ptr, Box::new(right))
    }
    /// Constructor
    pub fn like(lhs: Self, not: bool, op: LikeOperator, rhs: Self, escape: Option<Self>) -> Self {
        Self::Like {
            lhs: Box::new(lhs),
            not,
            op,
            rhs: Box::new(rhs),
            escape: escape.map(Box::new),
        }
    }
    /// Constructor
    pub fn not_null(x: Self, op: YYCODETYPE) -> Self {
        if op == TK_ISNULL as YYCODETYPE {
            Self::IsNull(Box::new(x))
        } else if op == TK_NOTNULL as YYCODETYPE {
            Self::NotNull(Box::new(x))
        } else {
            unreachable!()
        }
    }
    /// Constructor
    pub fn unary(op: UnaryOperator, x: Self) -> Self {
        Self::Unary(op, Box::new(x))
    }
    /// Constructor
    pub fn between(lhs: Self, not: bool, start: Self, end: Self) -> Self {
        Self::Between {
            lhs: Box::new(lhs),
            not,
            start: Box::new(start),
            end: Box::new(end),
        }
    }
    /// Constructor
    pub fn in_list(lhs: Self, not: bool, rhs: Option<Vec<Self>>) -> Self {
        Self::InList {
            lhs: Box::new(lhs),
            not,
            rhs,
        }
    }
    /// Constructor
    pub fn in_select(lhs: Self, not: bool, rhs: Select) -> Self {
        Self::InSelect {
            lhs: Box::new(lhs),
            not,
            rhs: Box::new(rhs),
        }
    }
    /// Constructor
    pub fn in_table(lhs: Self, not: bool, rhs: QualifiedName, args: Option<Vec<Self>>) -> Self {
        Self::InTable {
            lhs: Box::new(lhs),
            not,
            rhs,
            args,
        }
    }
    /// Constructor
    pub fn sub_query(query: Select) -> Self {
        Self::Subquery(Box::new(query))
    }
    /// Constructor
    pub fn function_call(
        xt: YYCODETYPE,
        x: Token,
        distinctness: Option<Distinctness>,
        args: Option<Vec<Self>>,
        order_by: Option<FunctionCallOrder>,
        filter_over: Option<FunctionTail>,
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
            name: Id::from_token(xt, x),
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    /// Number
    Numeric(Box<str>),
    /// String
    // TODO Check that string is already quoted and correctly escaped
    String(Box<str>),
    /// BLOB
    // TODO Check that string is valid (only hexa)
    Blob(Box<str>),
    /// Keyword
    Keyword(Box<str>),
    /// `NULL`
    Null,
    /// `CURRENT_DATE`
    CurrentDate,
    /// `CURRENT_TIME`
    CurrentTime,
    /// `CURRENT_TIMESTAMP`
    CurrentTimestamp,
}

impl Literal {
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Select {
    /// CTE
    pub with: Option<With>, // TODO check usages in body
    /// body
    pub body: SelectBody,
    /// `ORDER BY`
    pub order_by: Option<Vec<SortedColumn>>, // TODO: ORDER BY term does not match any column in the result set
    /// `LIMIT`
    pub limit: Option<Box<Limit>>,
}

impl Select {
    /// Constructor
    pub fn new(
        with: Option<With>,
        body: SelectBody,
        order_by: Option<Vec<SortedColumn>>,
        limit: Option<Limit>,
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SelectBody {
    /// first select
    pub select: OneSelect,
    /// compounds
    pub compounds: Option<Vec<CompoundSelect>>,
}

impl SelectBody {
    pub(crate) fn push(&mut self, cs: CompoundSelect) -> Result<(), ParserError> {
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
            self.compounds = Some(vec![cs]);
        }
        Ok(())
    }
}

/// Compound select
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CompoundSelect {
    /// operator
    pub operator: CompoundOperator,
    /// select
    pub select: OneSelect,
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OneSelect {
    /// `SELECT`
    Select {
        /// `DISTINCT`
        distinctness: Option<Distinctness>,
        /// columns
        columns: Vec<ResultColumn>,
        /// `FROM` clause
        from: Option<FromClause>,
        /// `WHERE` clause
        where_clause: Option<Box<Expr>>,
        /// `GROUP BY`
        group_by: Option<Vec<Expr>>,
        /// `HAVING`
        having: Option<Box<Expr>>, // TODO: HAVING clause on a non-aggregate query
        /// `WINDOW` definition
        window_clause: Option<Vec<WindowDef>>,
    },
    /// `VALUES`
    Values(Vec<Vec<Expr>>),
}

impl OneSelect {
    /// Constructor
    pub fn new(
        distinctness: Option<Distinctness>,
        columns: Vec<ResultColumn>,
        from: Option<FromClause>,
        where_clause: Option<Expr>,
        group_by: Option<Vec<Expr>>,
        having: Option<Expr>,
        window_clause: Option<Vec<WindowDef>>,
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
            where_clause: where_clause.map(Box::new),
            group_by,
            having: having.map(Box::new),
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
    pub fn push(values: &mut Vec<Vec<Expr>>, v: Vec<Expr>) -> Result<(), ParserError> {
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FromClause {
    /// table
    pub select: Option<Box<SelectTable>>, // FIXME mandatory
    /// `JOIN`ed tabled
    pub joins: Option<Vec<JoinedSelectTable>>,
    op: Option<JoinOperator>, // FIXME transient
}
impl FromClause {
    pub(crate) fn empty() -> Self {
        Self {
            select: None,
            joins: None,
            op: None,
        }
    }

    pub(crate) fn push(
        &mut self,
        table: SelectTable,
        jc: Option<JoinConstraint>,
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
                self.joins = Some(vec![jst]);
            }
        } else {
            if jc.is_some() {
                return Err(custom_err!("a JOIN clause is required before ON"));
            }
            debug_assert!(self.select.is_none());
            debug_assert!(self.joins.is_none());
            self.select = Some(Box::new(table));
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ResultColumn {
    /// expression
    Expr(Expr, Option<As>),
    /// `*`
    Star,
    /// table name.`*`
    TableStar(Name),
}

/// Alias
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum As {
    /// `AS`
    As(Name),
    /// no `AS`
    Elided(Name), // FIXME Ids
}

/// `JOIN` clause
// https://sqlite.org/syntax/join-clause.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct JoinedSelectTable {
    /// operator
    pub operator: JoinOperator,
    /// table
    pub table: SelectTable,
    /// constraint
    pub constraint: Option<JoinConstraint>,
}

/// Table or subquery
// https://sqlite.org/syntax/table-or-subquery.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SelectTable {
    /// table
    Table(QualifiedName, Option<As>, Option<Indexed>),
    /// table function call
    TableCall(QualifiedName, Option<Vec<Expr>>, Option<As>),
    /// `SELECT` subquery
    Select(Box<Select>, Option<As>),
    /// subquery
    Sub(FromClause, Option<As>),
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
                jt |= JoinType::try_from(n.0.as_ref().as_bytes())?;
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum JoinConstraint {
    /// `ON`
    On(Expr),
    /// `USING`: col names
    Using(DistinctNames),
}

/// identifier or one of several keywords or `INDEXED`
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Id(pub Box<str>);

impl Id {
    /// Constructor
    pub fn from_token(ty: YYCODETYPE, token: Token) -> Self {
        Self(from_token(ty, token))
    }
}

// TODO ids (identifier or string)

/// identifier or string or `CROSS` or `FULL` or `INNER` or `LEFT` or `NATURAL` or `OUTER` or `RIGHT`.
#[derive(Clone, Debug, Eq)]
pub struct Name(pub Box<str>); // TODO distinction between Name and "Name"/[Name]/`Name`

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

impl Name {
    /// Constructor
    pub fn from_token(ty: YYCODETYPE, token: Token) -> Self {
        Self(from_token(ty, token))
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
impl std::hash::Hash for Name {
    fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
        self.as_bytes()
            .for_each(|b| hasher.write_u8(b.to_ascii_lowercase()));
    }
}
/// Ignore case and quote
impl PartialEq for Name {
    fn eq(&self, other: &Self) -> bool {
        eq_ignore_case_and_quote(self.as_bytes(), other.as_bytes())
    }
}
/// Ignore case and quote
impl PartialEq<str> for Name {
    fn eq(&self, other: &str) -> bool {
        eq_ignore_case_and_quote(self.as_bytes(), QuotedIterator(other.bytes(), 0u8))
    }
}
/// Ignore case and quote
impl PartialEq<&str> for Name {
    fn eq(&self, other: &&str) -> bool {
        eq_ignore_case_and_quote(self.as_bytes(), QuotedIterator(other.bytes(), 0u8))
    }
}

/// Qualified name
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct QualifiedName {
    /// schema
    pub db_name: Option<Name>,
    /// object name
    pub name: Name,
    /// alias
    pub alias: Option<Name>, // FIXME restrict alias usage (fullname vs xfullname)
}

impl QualifiedName {
    /// Constructor
    pub fn single(name: Name) -> Self {
        Self {
            db_name: None,
            name,
            alias: None,
        }
    }
    /// Constructor
    pub fn fullname(db_name: Name, name: Name) -> Self {
        Self {
            db_name: Some(db_name),
            name,
            alias: None,
        }
    }
    /// Constructor
    pub fn xfullname(db_name: Name, name: Name, alias: Name) -> Self {
        Self {
            db_name: Some(db_name),
            name,
            alias: Some(alias),
        }
    }
    /// Constructor
    pub fn alias(name: Name, alias: Name) -> Self {
        Self {
            db_name: None,
            name,
            alias: Some(alias),
        }
    }
}

/// Ordered set of distinct column names
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DistinctNames(IndexSet<Name>);

impl DistinctNames {
    /// Initialize
    pub fn new(name: Name) -> Self {
        let mut dn = Self(IndexSet::new());
        dn.0.insert(name);
        dn
    }
    /// Single column name
    pub fn single(name: Name) -> Self {
        let mut dn = Self(IndexSet::with_capacity(1));
        dn.0.insert(name);
        dn
    }
    /// Push a distinct name or fail
    pub fn insert(&mut self, name: Name) -> Result<(), ParserError> {
        if self.0.contains(&name) {
            return Err(custom_err!("column \"{}\" specified more than once", name));
        }
        self.0.insert(name);
        Ok(())
    }
}
impl Deref for DistinctNames {
    type Target = IndexSet<Name>;

    fn deref(&self) -> &IndexSet<Name> {
        &self.0
    }
}

/// `ALTER TABLE` body
// https://sqlite.org/lang_altertable.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AlterTableBody {
    /// `RENAME TO`: new table name
    RenameTo(Name),
    /// `ADD COLUMN`
    AddColumn(ColumnDefinition), // TODO distinction between ADD and ADD COLUMN
    /// `RENAME COLUMN`
    RenameColumn {
        /// old name
        old: Name,
        /// new name
        new: Name,
    },
    /// `DROP COLUMN`
    DropColumn(Name), // TODO distinction between DROP and DROP COLUMN
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CreateTableBody {
    /// columns and constraints
    ColumnsAndConstraints {
        /// table column definitions
        columns: IndexMap<Name, ColumnDefinition>,
        /// table constraints
        constraints: Option<Vec<NamedTableConstraint>>,
        /// table flags
        flags: TabFlags,
    },
    /// `AS` select
    AsSelect(Box<Select>),
}

impl CreateTableBody {
    /// Constructor
    pub fn columns_and_constraints(
        columns: IndexMap<Name, ColumnDefinition>,
        constraints: Option<Vec<NamedTableConstraint>>,
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ColumnDefinition {
    /// column name
    pub col_name: Name,
    /// column type
    pub col_type: Option<Type>,
    /// column constraints
    pub constraints: Vec<NamedColumnConstraint>,
    /// column flags
    pub flags: ColFlags,
}

impl ColumnDefinition {
    /// Constructor
    pub fn new(
        col_name: Name,
        mut col_type: Option<Type>,
        constraints: Vec<NamedColumnConstraint>,
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
                    let new_type: Vec<&str> = split.collect();
                    col_type.name = new_type.join(" ");
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
    pub fn add_column(columns: &mut IndexMap<Name, Self>, cd: Self) -> Result<(), ParserError> {
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NamedColumnConstraint {
    /// constraint name
    pub name: Option<Name>,
    /// constraint
    pub constraint: ColumnConstraint,
}

/// Column constraint
// https://sqlite.org/syntax/column-constraint.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ColumnConstraint {
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
    Check(Expr),
    /// `DEFAULT`
    Default(Expr),
    /// `DEFERRABLE`
    Defer(DeferSubclause), // FIXME
    /// `COLLATE`
    Collate {
        /// collation name
        collation_name: Name, // FIXME Ids
    },
    /// `REFERENCES` foreign-key clause
    ForeignKey {
        /// clause
        clause: ForeignKeyClause,
        /// `DEFERRABLE`
        defer_clause: Option<DeferSubclause>,
    },
    /// `GENERATED`
    Generated {
        /// expression
        expr: Expr,
        /// `STORED` / `VIRTUAL`
        typ: Option<Id>,
    },
}

/// Named table constraint
// https://sqlite.org/syntax/table-constraint.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NamedTableConstraint {
    /// constraint name
    pub name: Option<Name>,
    /// constraint
    pub constraint: TableConstraint,
}

/// Table constraint
// https://sqlite.org/syntax/table-constraint.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TableConstraint {
    /// `PRIMARY KEY`
    PrimaryKey {
        /// columns
        columns: Vec<SortedColumn>,
        /// `AUTOINCREMENT`
        auto_increment: bool,
        /// `ON CONFLICT` clause
        conflict_clause: Option<ResolveType>,
    },
    /// `UNIQUE`
    Unique {
        /// columns
        columns: Vec<SortedColumn>,
        /// `ON CONFLICT` clause
        conflict_clause: Option<ResolveType>,
    },
    /// `CHECK`
    Check(Expr),
    /// `FOREIGN KEY`
    ForeignKey {
        /// columns
        columns: Vec<IndexedColumn>,
        /// `REFERENCES`
        clause: ForeignKeyClause,
        /// `DEFERRABLE`
        defer_clause: Option<DeferSubclause>,
    },
}

impl TableConstraint {
    /// PK constructor
    pub fn primary_key(
        columns: Vec<SortedColumn>,
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
        columns: Vec<SortedColumn>,
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ForeignKeyClause {
    /// foreign table name
    pub tbl_name: Name,
    /// foreign table columns
    pub columns: Option<Vec<IndexedColumn>>,
    /// referential action(s) / deferrable option(s)
    pub args: Vec<RefArg>,
}

/// foreign-key reference args
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RefArg {
    /// `ON DELETE`
    OnDelete(RefAct),
    /// `ON INSERT`
    OnInsert(RefAct),
    /// `ON UPDATE`
    OnUpdate(RefAct),
    /// `MATCH`
    Match(Name),
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IndexedColumn {
    /// column name
    pub col_name: Name,
    /// `COLLATE`
    pub collation_name: Option<Name>, // FIXME Ids
    /// `ORDER BY`
    pub order: Option<SortOrder>,
}

/// `INDEXED BY` / `NOT INDEXED`
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Indexed {
    /// `INDEXED BY`: idx name
    IndexedBy(Name),
    /// `NOT INDEXED`
    NotIndexed,
}

/// Sorted column
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SortedColumn {
    /// expression
    pub expr: Expr,
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Limit {
    /// count
    pub expr: Expr,
    /// `OFFSET`
    pub offset: Option<Expr>, // TODO distinction between LIMIT offset, count and LIMIT count OFFSET offset
}

/// `INSERT` body
// https://sqlite.org/lang_insert.html
// https://sqlite.org/syntax/insert-stmt.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum InsertBody {
    /// `SELECT` or `VALUES`
    Select(Box<Select>, Option<Box<Upsert>>),
    /// `DEFAULT VALUES`
    DefaultValues,
}

/// `UPDATE ... SET`
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Set {
    /// column name(s)
    pub col_names: DistinctNames,
    /// expression
    pub expr: Expr,
}

/// `PRAGMA` body
// https://sqlite.org/syntax/pragma-stmt.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PragmaBody {
    /// `=`
    Equals(PragmaValue),
    /// function call
    Call(PragmaValue),
}

/// `PRAGMA` value
// https://sqlite.org/syntax/pragma-value.html
pub type PragmaValue = Expr; // TODO

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
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TriggerEvent {
    /// `DELETE`
    Delete,
    /// `INSERT`
    Insert,
    /// `UPDATE`
    Update,
    /// `UPDATE OF`: col names
    UpdateOf(DistinctNames),
}

/// `CREATE TRIGGER` command
// https://sqlite.org/lang_createtrigger.html
// https://sqlite.org/syntax/create-trigger-stmt.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TriggerCmd {
    /// `UPDATE`
    Update {
        /// `OR`
        or_conflict: Option<ResolveType>,
        /// table name
        tbl_name: Name,
        /// `SET` assignments
        sets: Vec<Set>, // FIXME unique
        /// `FROM`
        from: Option<FromClause>,
        /// `WHERE` clause
        where_clause: Option<Expr>,
    },
    /// `INSERT`
    Insert {
        /// `OR`
        or_conflict: Option<ResolveType>,
        /// table name
        tbl_name: Name,
        /// `COLUMNS`
        col_names: Option<DistinctNames>,
        /// `SELECT` or `VALUES`
        select: Box<Select>,
        /// `ON CONFLICT` clause
        upsert: Option<Box<Upsert>>,
    },
    /// `DELETE`
    Delete {
        /// table name
        tbl_name: Name,
        /// `WHERE` clause
        where_clause: Option<Expr>,
    },
    /// `SELECT`
    Select(Box<Select>),
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct With {
    /// `RECURSIVE`
    pub recursive: bool,
    /// CTEs
    pub ctes: Vec<CommonTableExpr>,
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CommonTableExpr {
    /// table name
    pub tbl_name: Name,
    /// table columns
    pub columns: Option<Vec<IndexedColumn>>, // TODO: check no duplicate (eidlist_opt)
    /// `MATERIALIZED`
    pub materialized: Materialized,
    /// query
    pub select: Box<Select>,
}

impl CommonTableExpr {
    /// Constructor
    pub fn new(
        tbl_name: Name,
        columns: Option<Vec<IndexedColumn>>,
        materialized: Materialized,
        select: Select,
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
            select: Box::new(select),
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Type {
    /// type name
    pub name: String, // TODO Validate: Ids+
    /// type size
    pub size: Option<TypeSize>,
}

/// Column type size limit(s)
// https://sqlite.org/syntax/type-name.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeSize {
    /// maximum size
    MaxSize(Box<Expr>),
    /// precision
    TypeSize(Box<Expr>, Box<Expr>),
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Upsert {
    /// conflict targets
    pub index: Option<UpsertIndex>,
    /// `DO` clause
    pub do_clause: UpsertDo,
    /// next upsert
    pub next: Option<Box<Upsert>>,
}

/// Upsert conflict targets
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UpsertIndex {
    /// columns
    pub targets: Vec<SortedColumn>,
    /// `WHERE` clause
    pub where_clause: Option<Expr>,
}

impl UpsertIndex {
    /// constructor
    pub fn new(
        targets: Vec<SortedColumn>,
        where_clause: Option<Expr>,
    ) -> Result<Self, ParserError> {
        has_explicit_nulls(&targets)?;
        Ok(Self {
            targets,
            where_clause,
        })
    }
}

/// Upsert `DO` action
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UpsertDo {
    /// `SET`
    Set {
        /// assignments
        sets: Vec<Set>,
        /// `WHERE` clause
        where_clause: Option<Expr>,
    },
    /// `NOTHING`
    Nothing,
}

/// Function call tail
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionTail {
    /// `FILTER` clause
    pub filter_clause: Option<Box<Expr>>,
    /// `OVER` clause
    pub over_clause: Option<Box<Over>>,
}

/// Function call `OVER` clause
// https://sqlite.org/syntax/over-clause.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Over {
    /// Window definition
    Window(Box<Window>),
    /// Window name
    Name(Name),
}

/// `OVER` window definition
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WindowDef {
    /// window name
    pub name: Name,
    /// window definition
    pub window: Window,
}

/// Window definition
// https://sqlite.org/syntax/window-defn.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Window {
    /// base window name
    pub base: Option<Name>,
    /// `PARTITION BY`
    pub partition_by: Option<Vec<Expr>>,
    /// `ORDER BY`
    pub order_by: Option<Vec<SortedColumn>>,
    /// frame spec
    pub frame_clause: Option<FrameClause>,
}

/// Frame specification
// https://sqlite.org/syntax/frame-spec.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FrameClause {
    /// unit
    pub mode: FrameMode,
    /// start bound
    pub start: FrameBound,
    /// end bound
    pub end: Option<FrameBound>,
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
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FrameBound {
    /// `CURRENT ROW`
    CurrentRow,
    /// `FOLLOWING`
    Following(Expr),
    /// `PRECEDING`
    Preceding(Expr),
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
        assert_eq!(name("x"), "x");
        assert_eq!(name("`x`"), "x");
        assert_eq!(name("`x``y`"), "x`y");
        assert_eq!(name(r#""x""#), "x");
        assert_eq!(name(r#""x""y""#), "x\"y");
        assert_eq!(name("[x]"), "x");
    }

    fn name(s: &'static str) -> Name {
        Name(s.into())
    }
}
