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
