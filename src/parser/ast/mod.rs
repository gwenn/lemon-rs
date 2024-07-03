//! Abstract Syntax Tree

pub mod check;
pub mod fmt;

use std::num::ParseIntError;
use std::ops::Deref;
use std::str::{Bytes, FromStr};

use fmt::{ToTokens, TokenStream};
use indexmap::{IndexMap, IndexSet};

use crate::custom_err;
use crate::dialect::TokenType::{self, *};
use crate::dialect::{from_token, is_identifier, Token};
use crate::parser::{parse::YYCODETYPE, ParserError};

/// `?` or `$` Prepared statement arg placeholder(s)
#[derive(Default)]
pub struct ParameterInfo {
    /// Number of SQL parameters in a prepared statement, like `sqlite3_bind_parameter_count`
    pub count: u32,
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
                    let n = u32::from_str(&variable[1..])?;
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
        columns: Option<Vec<IndexedColumn>>,
        /// query
        select: Select,
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
        args: Option<Vec<String>>, // TODO smol str
    },
    /// `DELETE`
    Delete {
        /// CTE
        with: Option<With>,
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
        limit: Option<Limit>,
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
        with: Option<With>,
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
    Select(Select),
    /// `UPDATE`
    Update {
        /// CTE
        with: Option<With>,
        /// `OR`
        or_conflict: Option<ResolveType>,
        /// table name
        tbl_name: QualifiedName,
        /// `INDEXED`
        indexed: Option<Indexed>,
        /// `SET` assigments
        sets: Vec<Set>,
        /// `FROM`
        from: Option<FromClause>,
        /// `WHERE` clause
        where_clause: Option<Expr>,
        /// `RETURNING`
        returning: Option<Vec<ResultColumn>>,
        /// `ORDER BY`
        order_by: Option<Vec<SortedColumn>>,
        /// `LIMIT`
        limit: Option<Limit>,
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
    ) -> Result<Stmt, ParserError> {
        has_explicit_nulls(&columns)?;
        Ok(Stmt::CreateIndex {
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
    ) -> Result<Stmt, ParserError> {
        if let Some(FromClause {
            select: Some(ref select),
            ref joins,
            ..
        }) = from
        {
            if matches!(select.as_ref(),
                SelectTable::Table(qn, _, _) | SelectTable::TableCall(qn, _, _)
                    if *qn == tbl_name)
                || joins.as_ref().map_or(false, |js| js.iter().any(|j|
                    matches!(j.table, SelectTable::Table(ref qn, _, _) | SelectTable::TableCall(ref qn, _, _)
                    if *qn == tbl_name)))
            {
                return Err(custom_err!(
                    "target object/alias may not appear in FROM clause",
                ));
            }
        }
        if order_by.is_some() && limit.is_none() {
            return Err(custom_err!("ORDER BY without LIMIT on UPDATE"));
        }
        Ok(Stmt::Update {
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
    Collate(Box<Expr>, String),
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
        /// `ORDER BY`
        order_by: Option<Vec<SortedColumn>>,
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
    Raise(ResolveType, Option<Name>),
    /// Subquery expression
    Subquery(Box<Select>),
    /// Unary expression
    Unary(UnaryOperator, Box<Expr>),
    /// Parameters
    Variable(String),
}

impl Expr {
    /// Constructor
    pub fn parenthesized(x: Expr) -> Expr {
        Expr::Parenthesized(vec![x])
    }
    /// Constructor
    pub fn id(xt: YYCODETYPE, x: Token) -> Expr {
        Expr::Id(Id::from_token(xt, x))
    }
    /// Constructor
    pub fn collate(x: Expr, ct: YYCODETYPE, c: Token) -> Expr {
        Expr::Collate(Box::new(x), from_token(ct, c))
    }
    /// Constructor
    pub fn cast(x: Expr, type_name: Option<Type>) -> Expr {
        Expr::Cast {
            expr: Box::new(x),
            type_name,
        }
    }
    /// Constructor
    pub fn binary(left: Expr, op: YYCODETYPE, right: Expr) -> Expr {
        Expr::Binary(Box::new(left), Operator::from(op), Box::new(right))
    }
    /// Constructor
    pub fn ptr(left: Expr, op: Token, right: Expr) -> Expr {
        let mut ptr = Operator::ArrowRight;
        if let Some(ref op) = op.1 {
            if op == "->>" {
                ptr = Operator::ArrowRightShift;
            }
        }
        Expr::Binary(Box::new(left), ptr, Box::new(right))
    }
    /// Constructor
    pub fn like(lhs: Expr, not: bool, op: LikeOperator, rhs: Expr, escape: Option<Expr>) -> Expr {
        Expr::Like {
            lhs: Box::new(lhs),
            not,
            op,
            rhs: Box::new(rhs),
            escape: escape.map(Box::new),
        }
    }
    /// Constructor
    pub fn not_null(x: Expr, op: YYCODETYPE) -> Expr {
        if op == TK_ISNULL as YYCODETYPE {
            Expr::IsNull(Box::new(x))
        } else if op == TK_NOTNULL as YYCODETYPE {
            Expr::NotNull(Box::new(x))
        } else {
            unreachable!()
        }
    }
    /// Constructor
    pub fn unary(op: UnaryOperator, x: Expr) -> Expr {
        Expr::Unary(op, Box::new(x))
    }
    /// Constructor
    pub fn between(lhs: Expr, not: bool, start: Expr, end: Expr) -> Expr {
        Expr::Between {
            lhs: Box::new(lhs),
            not,
            start: Box::new(start),
            end: Box::new(end),
        }
    }
    /// Constructor
    pub fn in_list(lhs: Expr, not: bool, rhs: Option<Vec<Expr>>) -> Expr {
        Expr::InList {
            lhs: Box::new(lhs),
            not,
            rhs,
        }
    }
    /// Constructor
    pub fn in_select(lhs: Expr, not: bool, rhs: Select) -> Expr {
        Expr::InSelect {
            lhs: Box::new(lhs),
            not,
            rhs: Box::new(rhs),
        }
    }
    /// Constructor
    pub fn in_table(lhs: Expr, not: bool, rhs: QualifiedName, args: Option<Vec<Expr>>) -> Expr {
        Expr::InTable {
            lhs: Box::new(lhs),
            not,
            rhs,
            args,
        }
    }
    /// Constructor
    pub fn sub_query(query: Select) -> Expr {
        Expr::Subquery(Box::new(query))
    }
    /// Constructor
    pub fn function_call(
        xt: YYCODETYPE,
        x: Token,
        distinctness: Option<Distinctness>,
        args: Option<Vec<Expr>>,
        order_by: Option<Vec<SortedColumn>>,
        filter_over: Option<FunctionTail>,
    ) -> Result<Expr, ParserError> {
        if let Some(Distinctness::Distinct) = distinctness {
            if args.as_ref().map_or(0, |v| v.len()) != 1 {
                return Err(custom_err!(
                    "DISTINCT aggregates must have exactly one argument"
                ));
            }
        }
        Ok(Expr::FunctionCall {
            name: Id::from_token(xt, x),
            distinctness,
            args,
            order_by,
            filter_over,
        })
    }
}

/// SQL literal
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    /// Number
    Numeric(String),
    /// String
    // TODO Check that string is already quoted and correctly escaped
    String(String),
    /// BLOB
    // TODO Check that string is valid (only hexa)
    Blob(String),
    /// Keyword
    Keyword(String),
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
    pub fn from_ctime_kw(token: Token) -> Literal {
        if let Some(ref token) = token.1 {
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
    pub fn from_token(token_type: YYCODETYPE, token: Token) -> LikeOperator {
        if token_type == TK_MATCH as YYCODETYPE {
            return LikeOperator::Match;
        } else if token_type == TK_LIKE_KW as YYCODETYPE {
            if let Some(ref token) = token.1 {
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

/// `SELECT` statement
// https://sqlite.org/lang_select.html
// https://sqlite.org/syntax/factored-select-stmt.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Select {
    /// CTE
    pub with: Option<With>,
    /// body
    pub body: SelectBody,
    /// `ORDER BY`
    pub order_by: Option<Vec<SortedColumn>>, // ORDER BY term does not match any column in the result set
    /// `LIMIT`
    pub limit: Option<Limit>,
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
        use crate::ast::check::ColumnCount;
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
        where_clause: Option<Expr>,
        /// `GROUP BY`
        group_by: Option<GroupBy>,
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
        group_by: Option<GroupBy>,
        window_clause: Option<Vec<WindowDef>>,
    ) -> Result<OneSelect, ParserError> {
        if from.is_none()
            && columns
                .iter()
                .any(|rc| matches!(rc, ResultColumn::Star | ResultColumn::TableStar(_)))
        {
            return Err(custom_err!("no tables specified"));
        }
        Ok(OneSelect::Select {
            distinctness,
            columns,
            from,
            where_clause,
            group_by,
            window_clause,
        })
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
    pub(crate) fn empty() -> FromClause {
        FromClause {
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
    Select(Select, Option<As>),
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
    ) -> Result<JoinOperator, ParserError> {
        Ok(if let Some(ref t) = token.1 {
            let mut jt = JoinType::try_from(t.as_ref())?;
            for n in [&n1, &n2].into_iter().flatten() {
                jt |= JoinType::try_from(n.0.as_ref())?;
            }
            if (jt & (JoinType::INNER | JoinType::OUTER)) == (JoinType::INNER | JoinType::OUTER)
                || (jt & (JoinType::OUTER | JoinType::LEFT | JoinType::RIGHT)) == JoinType::OUTER
            {
                return Err(custom_err!(
                    "unknown join type: {} {} {}",
                    t,
                    n1.as_ref().map_or("", |n| n.0.as_str()),
                    n2.as_ref().map_or("", |n| n.0.as_str())
                ));
            }
            JoinOperator::TypedJoin(Some(jt))
        } else {
            unreachable!()
        })
    }
    fn is_natural(&self) -> bool {
        match self {
            JoinOperator::TypedJoin(Some(jt)) => jt.contains(JoinType::NATURAL),
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

impl TryFrom<&str> for JoinType {
    type Error = ParserError;
    fn try_from(s: &str) -> Result<JoinType, ParserError> {
        if "CROSS".eq_ignore_ascii_case(s) {
            Ok(JoinType::INNER | JoinType::CROSS)
        } else if "FULL".eq_ignore_ascii_case(s) {
            Ok(JoinType::LEFT | JoinType::RIGHT | JoinType::OUTER)
        } else if "INNER".eq_ignore_ascii_case(s) {
            Ok(JoinType::INNER)
        } else if "LEFT".eq_ignore_ascii_case(s) {
            Ok(JoinType::LEFT | JoinType::OUTER)
        } else if "NATURAL".eq_ignore_ascii_case(s) {
            Ok(JoinType::NATURAL)
        } else if "RIGHT".eq_ignore_ascii_case(s) {
            Ok(JoinType::RIGHT | JoinType::OUTER)
        } else if "OUTER".eq_ignore_ascii_case(s) {
            Ok(JoinType::OUTER)
        } else {
            Err(custom_err!("unknown join type: {}", s))
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

/// `GROUP BY`
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GroupBy {
    /// expressions
    pub exprs: Vec<Expr>,
    /// `HAVING`
    pub having: Option<Expr>, // HAVING clause on a non-aggregate query
}

/// identifier or one of several keywords or `INDEXED`
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Id(pub String);

impl Id {
    /// Constructor
    pub fn from_token(ty: YYCODETYPE, token: Token) -> Id {
        Id(from_token(ty, token))
    }
}

// TODO ids (identifier or string)

/// identifier or string or `CROSS` or `FULL` or `INNER` or `LEFT` or `NATURAL` or `OUTER` or `RIGHT`.
#[derive(Clone, Debug, Eq)]
pub struct Name(pub String); // TODO distinction between Name and "Name"/[Name]/`Name`

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
    pub fn from_token(ty: YYCODETYPE, token: Token) -> Name {
        Name(from_token(ty, token))
    }

    fn as_bytes(&self) -> QuotedIterator<'_> {
        let (sub, quote) = unquote(self.0.as_str());
        QuotedIterator(sub.bytes(), quote)
    }

    fn is_reserved(&self) -> bool {
        let bytes = self.as_bytes();
        let reserved = QuotedIterator("sqlite_".bytes(), 0);
        bytes.zip(reserved).fold(0u8, |acc, (b1, b2)| {
            acc + if b1.eq_ignore_ascii_case(&b2) { 1 } else { 0 }
        }) == 7u8
    }
}

struct QuotedIterator<'s>(Bytes<'s>, u8);
impl<'s> Iterator for QuotedIterator<'s> {
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
    #[inline(always)]
    fn eq(&self, other: &Name) -> bool {
        eq_ignore_case_and_quote(self.as_bytes(), other.as_bytes())
    }
}
/// Ignore case and quote
impl PartialEq<str> for Name {
    #[inline(always)]
    fn eq(&self, other: &str) -> bool {
        eq_ignore_case_and_quote(self.as_bytes(), QuotedIterator(other.bytes(), 0u8))
    }
}
/// Ignore case and quote
impl PartialEq<&str> for Name {
    #[inline(always)]
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
        QualifiedName {
            db_name: None,
            name,
            alias: None,
        }
    }
    /// Constructor
    pub fn fullname(db_name: Name, name: Name) -> Self {
        QualifiedName {
            db_name: Some(db_name),
            name,
            alias: None,
        }
    }
    /// Constructor
    pub fn xfullname(db_name: Name, name: Name, alias: Name) -> Self {
        QualifiedName {
            db_name: Some(db_name),
            name,
            alias: Some(alias),
        }
    }
    /// Constructor
    pub fn alias(name: Name, alias: Name) -> Self {
        QualifiedName {
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
    pub fn new(name: Name) -> DistinctNames {
        let mut dn = DistinctNames(IndexSet::new());
        dn.0.insert(name);
        dn
    }
    /// Single column name
    pub fn single(name: Name) -> DistinctNames {
        let mut dn = DistinctNames(IndexSet::with_capacity(1));
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
    AsSelect(Select),
}

impl CreateTableBody {
    /// Constructor
    pub fn columns_and_constraints(
        columns: IndexMap<Name, ColumnDefinition>,
        constraints: Option<Vec<NamedTableConstraint>>,
        mut flags: TabFlags,
    ) -> Result<CreateTableBody, ParserError> {
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
                        return Err(custom_err!("table has more than one primary key"));
                    } else {
                        flags |= TabFlags::HasPrimaryKey;
                    }
                }
            }
        }
        Ok(CreateTableBody::ColumnsAndConstraints {
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
    ) -> Result<ColumnDefinition, ParserError> {
        let mut flags = ColFlags::empty();
        let mut default = false;
        for constraint in &constraints {
            match &constraint.constraint {
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
                ColumnConstraint::ForeignKey {
                    clause:
                        ForeignKeyClause {
                            tbl_name, columns, ..
                        },
                    ..
                } => {
                    // The child table may reference the primary key of the parent without specifying the primary key column
                    if columns.as_ref().map_or(0, |v| v.len()) > 1 {
                        return Err(custom_err!(
                            "foreign key on {} should reference only one column of table {}",
                            col_name,
                            tbl_name
                        ));
                    }
                }
                ColumnConstraint::PrimaryKey { auto_increment, .. } => {
                    if *auto_increment
                        && col_type.as_ref().map_or(true, |t| {
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
                    .map_or(false, |s| s.eq_ignore_ascii_case("ALWAYS"))
                    && split
                        .next_back()
                        .map_or(false, |s| s.eq_ignore_ascii_case("GENERATED"))
                {
                    // str_split_whitespace_remainder
                    let new_type: Vec<&str> = split.collect();
                    col_type.name = new_type.join(" ");
                }
            }
        }
        if col_type.as_ref().map_or(false, |t| !t.name.is_empty()) {
            flags |= ColFlags::HASTYPE;
        }
        Ok(ColumnDefinition {
            col_name,
            col_type,
            constraints,
            flags,
        })
    }
    /// Collector
    pub fn add_column(
        columns: &mut IndexMap<Name, ColumnDefinition>,
        cd: ColumnDefinition,
    ) -> Result<(), ParserError> {
        let col_name = &cd.col_name;
        if columns.contains_key(col_name) {
            return Err(custom_err!("duplicate column name: {}", col_name));
        } else if cd.flags.contains(ColFlags::PRIMKEY)
            && columns
                .values()
                .any(|c| c.flags.contains(ColFlags::PRIMKEY))
        {
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
        deref_clause: Option<DeferSubclause>,
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
        deref_clause: Option<DeferSubclause>,
    },
}

impl TableConstraint {
    /// PK constructor
    pub fn primary_key(
        columns: Vec<SortedColumn>,
        auto_increment: bool,
        conflict_clause: Option<ResolveType>,
    ) -> Result<TableConstraint, ParserError> {
        has_explicit_nulls(&columns)?;
        Ok(TableConstraint::PrimaryKey {
            columns,
            auto_increment,
            conflict_clause,
        })
    }
    /// UNIQUE constructor
    pub fn unique(
        columns: Vec<SortedColumn>,
        conflict_clause: Option<ResolveType>,
    ) -> Result<TableConstraint, ParserError> {
        has_explicit_nulls(&columns)?;
        Ok(TableConstraint::Unique {
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

fn has_explicit_nulls(columns: &Vec<SortedColumn>) -> Result<(), ParserError> {
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
    Select(Select, Option<Upsert>),
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
        /// `SET` assigments
        sets: Vec<Set>,
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
        select: Select,
        /// `ON CONLICT` clause
        upsert: Option<Upsert>,
        /// `RETURNING`
        returning: Option<Vec<ResultColumn>>,
    },
    /// `DELETE`
    Delete {
        /// table name
        tbl_name: Name,
        /// `WHERE` clause
        where_clause: Option<Expr>,
    },
    /// `SELECT`
    Select(Select),
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
    pub columns: Option<Vec<IndexedColumn>>, // check no duplicate
    /// `MATERIALIZED`
    pub materialized: Materialized,
    /// query
    pub select: Select,
}

impl CommonTableExpr {
    /// Constructor
    pub fn new(
        tbl_name: Name,
        columns: Option<Vec<IndexedColumn>>,
        materialized: Materialized,
        select: Select,
    ) -> Result<CommonTableExpr, ParserError> {
        if let Some(ref columns) = columns {
            if let check::ColumnCount::Fixed(cc) = select.column_count() {
                if cc != columns.len() {
                    return Err(custom_err!(
                        "table {} has {} values for {} columns",
                        tbl_name,
                        cc,
                        columns.len()
                    ));
                }
            }
        }
        Ok(CommonTableExpr {
            tbl_name,
            columns,
            materialized,
            select,
        })
    }
    /// Constructor
    pub fn add_cte(
        ctes: &mut Vec<CommonTableExpr>,
        cte: CommonTableExpr,
    ) -> Result<(), ParserError> {
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
    ) -> Result<UpsertIndex, ParserError> {
        has_explicit_nulls(&targets)?;
        Ok(UpsertIndex {
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
    /// Window defintion
    Window(Window),
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
        Name(s.to_owned())
    }
}
