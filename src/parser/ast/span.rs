//! Input source location
use std::cmp;

use crate::ast::*;
use crate::dialect::Token;

/// Input source location
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    /// byte offset in input
    pub start: usize,
    /// byte offset in input
    pub end: usize,
}

impl Span {
    const EMPTY: Span = Self::empty();
    /// No location
    pub const fn empty() -> Span {
        Span { start: 0, end: 0 }
    }
    /// Fuse two spans
    pub fn union(&self, other: &Span) -> Span {
        match (self, other) {
            (&Span::EMPTY, _) => *other,
            (_, &Span::EMPTY) => *self,
            _ => Span {
                start: cmp::min(self.start, other.start),
                end: cmp::max(self.end, other.end),
            },
        }
    }
    /// Fuse two spans
    pub fn union_opt(&self, other: &Option<Span>) -> Span {
        match other {
            Some(other) => self.union(other),
            None => *self,
        }
    }
}

impl<'i> From<&Token<'i>> for Span {
    fn from(value: &Token<'i>) -> Self {
        Span {
            start: value.0,
            end: value.2,
        }
    }
}

/// Trait for AST nodes that have a source location information.
pub trait Spanned {
    /// accessor
    fn span(&self) -> Span;
}
impl<S> Spanned for &S
where
    S: ?Sized + Spanned,
{
    fn span(&self) -> Span {
        <S as Spanned>::span(self)
    }
}
impl<S> Spanned for Option<S>
where
    S: Spanned,
{
    #[inline]
    fn span(&self) -> Span {
        match *self {
            Some(ref s) => s.span(),
            None => Span::EMPTY,
        }
    }
}
impl<S> Spanned for Box<S>
where
    S: ?Sized + Spanned,
{
    fn span(&self) -> Span {
        <S as Spanned>::span(self)
    }
}

impl<S> Spanned for Vec<S>
where
    S: Spanned,
{
    fn span(&self) -> Span {
        self.first().span().union(&self.last().span())
    }
}

impl Spanned for Expr {
    fn span(&self) -> Span {
        match self {
            Expr::Between {
                lhs,
                not: _,
                start: _,
                end,
            } => lhs.span().union(&end.span()),
            Expr::Binary(lhs, _, rhs) => lhs.span().union(&rhs.span()),
            // FIXME CASE ... END <- CASE and END spans
            Expr::Case {
                base,
                when_then_pairs,
                else_expr,
            } => base
                .span()
                .union_opt(&when_then_pairs.last().map(|p| p.1.span()))
                .union(&else_expr.span()),
            Expr::Cast { expr, type_name } => todo!(),
            Expr::Collate(expr, _) => todo!(),
            Expr::DoublyQualified(name, _, name2) => name.span().union(&name2.span()),
            Expr::Exists(select) => todo!(),
            Expr::FunctionCall {
                name,
                distinctness,
                args,
                order_by,
                filter_over,
            } => todo!(),
            // FIXME name(*) [filter_over] <- right parenthesis span
            Expr::FunctionCallStar { name, filter_over } => name.span().union(&filter_over.span()),
            Expr::Id(id) => id.span(),
            // FIXME lhs [NOT] IN (rhs) <- right parenthesis span
            Expr::InList { lhs, not: _, rhs } => lhs.span().union(&rhs.span()),
            // FIXME lhs [NOT] IN (rhs) <- right parenthesis span
            Expr::InSelect { lhs, not: _, rhs } => todo!(),
            // FIXME lhs ... (args) <- optional right parenthesis span
            Expr::InTable {
                lhs,
                not: _,
                rhs,
                args,
            } => lhs.span().union(&rhs.span()).union(&args.span()),
            // FIXME expr IS NULL <- NULL span
            Expr::IsNull(expr) => expr.span(),
            Expr::Like {
                lhs,
                not: _,
                op: _,
                rhs,
                escape,
            } => lhs.span().union(&rhs.span()).union(&escape.span()),
            Expr::Literal(lit) => lit.span(),
            Expr::Name(name) => name.span(),
            // FIXME expr NOT NULL <- NULL span
            Expr::NotNull(expr) => expr.span(),
            // FIXME (exprs) <- left and right parentheses spans
            Expr::Parenthesized(exprs) => exprs.span(),
            Expr::Qualified(qualifier, qualified) => qualifier.span().union(&qualified.span()),
            Expr::Raise(resolve_type, expr) => todo!(),
            Expr::Subquery(select) => todo!(),
            Expr::Unary(unary_operator, expr) => todo!(),
            Expr::Variable(var) => todo!(),
        }
    }
}

impl Spanned for Literal {
    fn span(&self) -> Span {
        match self {
            Literal::Numeric(_, span) => *span,
            Literal::String(_, span) => *span,
            Literal::Blob(_, span) => *span,
            Literal::Keyword(_, span) => *span,
            Literal::Null(span) => *span,
            Literal::CurrentDate(span) => *span,
            Literal::CurrentTime(span) => *span,
            Literal::CurrentTimestamp(span) => *span,
        }
    }
}

impl Spanned for Select {
    fn span(&self) -> Span {
        todo!()
    }
}

impl Spanned for FromClause {
    fn span(&self) -> Span {
        self.select.span().union(&self.joins.span())
    }
}

impl Spanned for ResultColumn {
    fn span(&self) -> Span {
        match self {
            ResultColumn::Expr(expr, alias) => expr.span().union(&alias.span()),
            // FIXME
            ResultColumn::Star => Span::EMPTY,
            // FIXME tbl_name.* <- * span
            ResultColumn::TableStar(tbl_name) => tbl_name.span(),
        }
    }
}

impl Spanned for As {
    fn span(&self) -> Span {
        match self {
            // FIXME AS name <- AS span
            As::As(name) => name.span(),
            As::Elided(name) => name.span(),
        }
    }
}

impl Spanned for JoinedSelectTable {
    fn span(&self) -> Span {
        self.operator
            .span()
            .union(&self.table.span())
            .union(&self.constraint.span())
    }
}

impl Spanned for SelectTable {
    fn span(&self) -> Span {
        match self {
            SelectTable::Table(name, alias, indexed) => {
                name.span().union(&alias.span()).union(&indexed.span())
            }
            SelectTable::TableCall(name, exprs, alias) => {
                name.span().union(&exprs.span()).union(&alias.span())
            }
            // FIXME (select) alias <- left parenthesis (and right parentheis if no alias) spans
            SelectTable::Select(select, alias) => select.span().union(&alias.span()),
            // FIXME (from) alias <- left parenthesis (and right parentheis if no alias) spans
            SelectTable::Sub(from, alias) => from.span().union(&alias.span()),
        }
    }
}

impl Spanned for JoinOperator {
    fn span(&self) -> Span {
        match self {
            JoinOperator::Comma(span) => *span,
            JoinOperator::TypedJoin(_, span) => *span,
        }
    }
}

impl Spanned for JoinConstraint {
    fn span(&self) -> Span {
        match self {
            // FIXME ON expr <- ON span
            JoinConstraint::On(expr) => expr.span(),
            // FIXME USING (col_names) <- USING and right parenthesis spans
            JoinConstraint::Using(col_names) => col_names.span(),
        }
    }
}

impl Spanned for Id {
    fn span(&self) -> Span {
        self.1
    }
}
impl Spanned for Name {
    fn span(&self) -> Span {
        self.1
    }
}
impl Spanned for QualifiedName {
    fn span(&self) -> Span {
        self.db_name
            .span()
            .union(&self.name.span())
            .union(&self.alias.span())
    }
}

impl Spanned for DistinctNames {
    fn span(&self) -> Span {
        self.0.first().span().union(&self.0.last().span())
    }
}

impl Spanned for SortOrder {
    fn span(&self) -> Span {
        // FIXME
        Span::EMPTY
    }
}

impl Spanned for NullsOrder {
    fn span(&self) -> Span {
        // FIXME
        Span::EMPTY
    }
}

impl Spanned for ForeignKeyClause {
    fn span(&self) -> Span {
        self.tbl_name.span().union(&self.args.span())
    }
}

impl Spanned for RefArg {
    fn span(&self) -> Span {
        // FIXME
        Span::EMPTY
    }
}

impl Spanned for IndexedColumn {
    fn span(&self) -> Span {
        self.col_name
            .span()
            .union(&self.collation_name.span())
            .union(&self.order.span())
    }
}

impl Spanned for Indexed {
    fn span(&self) -> Span {
        match self {
            // FIXME INDEXED BY name <- INDEXED span
            Indexed::IndexedBy(name) => name.span(),
            // FIXME
            Indexed::NotIndexed => Span::EMPTY,
        }
    }
}

impl Spanned for SortedColumn {
    fn span(&self) -> Span {
        self.expr
            .span()
            .union(&self.order.span())
            .union(&self.nulls.span())
    }
}

impl Spanned for Limit {
    fn span(&self) -> Span {
        // FIXME LIMIT expr OFFSET offset <- LIMIT span
        self.expr.span().union(&self.offset.span())
    }
}

impl Spanned for Set {
    fn span(&self) -> Span {
        // FIXME (col_names) = expr <- optional left parenthesis span
        self.col_names.span().union(&self.expr.span())
    }
}

impl Spanned for PragmaBody {
    fn span(&self) -> Span {
        match self {
            // FIXME = expr <- equal sign span
            PragmaBody::Equals(expr) => expr.span(),
            // FIXME (expr) <- parentheses spans
            PragmaBody::Call(expr) => expr.span(),
        }
    }
}

impl Spanned for With {
    fn span(&self) -> Span {
        // FIXME WITH [RECURSIVE] ctes <- WITH span
        self.ctes.span()
    }
}

impl Spanned for CommonTableExpr {
    fn span(&self) -> Span {
        // FIXME tbl_name ... (select) <- right parenthesis span
        self.tbl_name.span().union(&self.select.span())
    }
}

impl Spanned for TypeSize {
    fn span(&self) -> Span {
        match self {
            TypeSize::MaxSize(size) => size.span(),
            TypeSize::TypeSize(size1, size2) => size1.span().union(&size2.span()),
        }
    }
}

impl Spanned for Upsert {
    fn span(&self) -> Span {
        // FIXME ON CONFLICT index .. <- ON span
        self.index.span().union(&self.next.span())
    }
}

impl Spanned for UpsertIndex {
    fn span(&self) -> Span {
        // FIXME (targets) [WHERE clause] <- left (and optional right) parentheses spans
        self.targets.span().union(&self.where_clause.span())
    }
}

impl Spanned for UpsertDo {
    fn span(&self) -> Span {
        match self {
            // FIXME DO UPDATE SET sets ... <- DO span
            UpsertDo::Set { sets, where_clause } => sets.span().union(&where_clause.span()),
            // FIXME DO NOTHING <- spans
            UpsertDo::Nothing => Span::EMPTY,
        }
    }
}

impl Spanned for FunctionTail {
    fn span(&self) -> Span {
        // FIXME FILTER (WHERE filter_clause) OVER over_clause <- FILTER or OVER span
        self.filter_clause.span().union(&self.over_clause.span())
    }
}

impl Spanned for Over {
    fn span(&self) -> Span {
        match self {
            Over::Window(window) => window.span(),
            Over::Name(name) => name.span(),
        }
    }
}

impl Spanned for Window {
    fn span(&self) -> Span {
        todo!()
    }
}
