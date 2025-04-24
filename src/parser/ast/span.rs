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
            Expr::FunctionCallStar { name, filter_over } => name.span().union(&filter_over.span()),
            Expr::Id(id) => id.span(),
            Expr::InList { lhs, not: _, rhs } => lhs.span().union(&rhs.span()),
            Expr::InSelect { lhs, not: _, rhs } => todo!(),
            Expr::InTable {
                lhs,
                not: _,
                rhs,
                args,
            } => lhs.span().union(&rhs.span()).union(&args.span()),
            Expr::IsNull(expr) => expr.span(),
            Expr::Like {
                lhs,
                not: _,
                op: _,
                rhs,
                escape,
            } => lhs.span().union(&rhs.span()).union(&escape.span()),
            Expr::Literal(literal) => literal.span(),
            Expr::Name(name) => name.span(),
            Expr::NotNull(expr) => todo!(),
            Expr::Parenthesized(exprs) => todo!(),
            Expr::Qualified(qualifier, qualified) => qualifier.span().union(&qualified.span()),
            Expr::Raise(resolve_type, expr) => todo!(),
            Expr::Subquery(select) => todo!(),
            Expr::Unary(unary_operator, expr) => todo!(),
            Expr::Variable(_) => todo!(),
        }
    }
}

impl Spanned for As {
    fn span(&self) -> Span {
        match self {
            As::As(name) => name.span(),
            As::Elided(name) => name.span(),
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
            JoinConstraint::On(expr) => expr.span(),
            JoinConstraint::Using(distinct_names) => distinct_names.span(),
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

impl Spanned for Set {
    fn span(&self) -> Span {
        self.col_names.span().union(&self.expr.span())
    }
}

impl Spanned for FunctionTail {
    fn span(&self) -> Span {
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
