//! Abstract Syntax Tree

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
        name: String,
        distinctness: Option<Distinctness>,
        args: Option<Vec<Box<Expr>>>,
    },
    // Function call expression with '*' as arg
    FunctionCallStar(String),
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
    Isnull(Box<Expr>),
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    Numeric(String),
    String(String),
    Blob(String),
    Null,
    CurrentTime,
    CurrentDate,
    CurrentTimestamp,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum LikeOperator {
    Glob,
    Like,
    Match,
    Regexp,
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
    Multiply,
    Modulus,
    NotEquals, // != or <>
    Or,
    RightShift,
    Substract,
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Select {
    pub with: Option<With>,
    pub body: SelectBody,
    pub order_by: Option<Vec<SortedColumn>>,
    pub limit: Option<Limit>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SelectBody {
    pub select: OneSelect,
    pub compounds: Option<Vec<CompoundSelect>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CompoundSelect {
    pub operator: CompoundOperator,
    pub select: OneSelect,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum CompoundOperator {
    Union,
    UnionAll,
    Except,
    Intersect,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum OneSelect {
    Select {
        distinctness: Option<Distinctness>,
        columns: Vec<ResultColumn>,
        from: Option<FromClause>,
        where_clause: Option<Expr>,
        group_by: Option<GroupBy>,
    },
    Values(Vec<Vec<Expr>>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FromClause {
    pub select: Box<SelectTable>,
    pub joins: Option<Vec<JoinedSelectTable>>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Distinctness {
    Distinct,
    All,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ResultColumn {
    Expr(Expr, Option<As>),
    Star,
    // table name
    TableStar(Name),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum As {
    As(Name),
    Elided(String),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct JoinedSelectTable {
    pub operator: JoinOperator,
    pub table: SelectTable,
    pub constraint: Option<JoinConstraint>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SelectTable {
    Table(QualifiedName, Option<As>, Option<Indexed>),
    TableCall(QualifiedName, Option<Vec<Expr>>, Option<As>),
    Select(Select, Option<As>),
    Sub(FromClause, Option<As>),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum JoinOperator {
    Comma,
    TypedJoin {
        natural: bool,
        join_type: Option<JoinType>,
    },
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum JoinType {
    Left,
    LeftOuter,
    Inner,
    Cross,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum JoinConstraint {
    On(Expr),
    // col names
    Using(Vec<Name>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GroupBy {
    pub exprs: Vec<Expr>,
    pub having: Option<Expr>,
}

pub type Name = String; // TODO distinction between Name and "Name"/[Name]/`Name`

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct QualifiedName {
    pub db_name: Option<Name>,
    pub name: Name,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AlterTableBody {
    // new table name
    RenameTo(Name),
    AddColumn(ColumnDefinition), // TODO distinction between ADD and ADD COLUMN
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ColumnDefinition {
    pub col_name: Name,
    pub col_type: Option<Type>,
    pub constraints: Vec<NamedColumnConstraint>,
}

// TODO ColumnNameAndType

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NamedColumnConstraint {
    pub name: Option<Name>,
    pub constraint: ColumnConstraint,
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
    Default(DefaultValue),
    Collate {
        collation_name: String,
    },
    ForeignKey {
        clause: ForeignKeyClause,
        deref_clause: Option<DeferSubclause>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NamedTableConstraint {
    pub name: Option<Name>,
    pub constraint: TableConstraint,
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum SortOrder {
    Asc,
    Desc,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DefaultValue {
    Expr(Expr), // TODO
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ForeignKeyClause {
    pub tbl_name: Name,
    pub columns: Option<Vec<IndexedColumn>>,
    pub args: Vec<RefArg>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RefArg {
    OnDelete(RefAct),
    OnInsert(RefAct),
    OnUpdate(RefAct),
    Match(Name),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum RefAct {
    SetNull,
    SetDefault,
    Cascade,
    Restrict,
    NoAction,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DeferSubclause {
    pub deferrable: bool,
    pub init_deferred: Option<InitDeferredPred>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum InitDeferredPred {
    InitiallyDeferred,
    InitiallyImmediate, // default
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IndexedColumn {
    pub col_name: Name,
    pub collation_name: Option<String>,
    pub order: Option<SortOrder>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Indexed {
    // idx name
    IndexedBy(Name),
    NotIndexed,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SortedColumn {
    pub expr: Expr,
    pub order: Option<SortOrder>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Limit {
    pub expr: Expr,
    pub offset: Option<Expr>, // TODO distinction between LIMIT offset, count and LIMIT count OFFSET offset
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum InsertBody {
    Select(Select),
    DefaultValues,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Set {
    pub col_names: Vec<Name>,
    pub expr: Expr,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PragmaBody {
    Equals(PragmaValue),
    Call(PragmaValue),
}

pub type PragmaValue = String; // TODO

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TriggerTime {
    Before, // default
    After,
    InsteadOf,
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
                    double_quote(f, name)?;
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CommonTableExpr {
    pub tbl_name: Name,
    pub columns: Option<Vec<IndexedColumn>>,
    pub select: Select,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Type {
    pub name: String, // TODO Validate
    pub size: Option<TypeSize>,
}

/*
impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self.size {
            None => f.write_str(&self.name),
            Some(ref size) => write!(f, "{}({})", double_quote(self.name), size),
        }
    }
}
*/

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeSize {
    MaxSize(Box<Expr>),
    TypeSize(Box<Expr>, Box<Expr>),
}

/*
impl Display for TypeSize {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            TypeSize::MaxSize(size) => write!(f, "{}", size),
            TypeSize::TypeSize(size1, size2) => write!(f, "{}, {}", size1, size2),
        }
    }
}
*/

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

fn double_quote(f: &mut Formatter, name: &str) -> Result {
    if name.is_empty() {
        return f.write_str("\"\"");
    }
    unimplemented!()
}
