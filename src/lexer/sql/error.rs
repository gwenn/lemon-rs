use std::error;
use std::fmt;
use std::io;

use crate::lexer::scan::{Pos, ScanError};
use crate::parser::ParserError;

/// SQL lexer and parser errors
#[non_exhaustive]
#[derive(Debug)]
pub enum Error {
    /// I/O Error
    Io(io::Error),
    /// Lexer error
    UnrecognizedToken(Option<Pos>),
    /// Missing quote or double-quote or backtick
    UnterminatedLiteral(Option<Pos>),
    /// Missing `]`
    UnterminatedBracket(Option<Pos>),
    /// Missing `*/`
    UnterminatedBlockComment(Option<Pos>),
    /// Invalid parameter name
    BadVariableName(Option<Pos>),
    /// Invalid number format
    BadNumber(Option<Pos>),
    /// Invalid or missing sign after `!`
    ExpectedEqualsSign(Option<Pos>),
    /// BLOB literals are string literals containing hexadecimal data and preceded by a single "x" or "X" character.
    MalformedBlobLiteral(Option<Pos>),
    /// Hexadecimal integer literals follow the C-language notation of "0x" or "0X" followed by hexadecimal digits.
    MalformedHexInteger(Option<Pos>),
    /// Grammar error
    ParserError(ParserError, Option<Pos>),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io(ref err) => err.fmt(f),
            Self::UnrecognizedToken(pos) => write!(f, "unrecognized token at {pos:?}"),
            Self::UnterminatedLiteral(pos) => {
                write!(f, "non-terminated literal at {pos:?}")
            }
            Self::UnterminatedBracket(pos) => {
                write!(f, "non-terminated bracket at {pos:?}")
            }
            Self::UnterminatedBlockComment(pos) => {
                write!(f, "non-terminated block comment at {pos:?}")
            }
            Self::BadVariableName(pos) => write!(f, "bad variable name at {pos:?}"),
            Self::BadNumber(pos) => write!(f, "bad number at {pos:?}"),
            Self::ExpectedEqualsSign(pos) => write!(f, "expected = sign at {pos:?}"),
            Self::MalformedBlobLiteral(pos) => {
                write!(f, "malformed blob literal at {pos:?}")
            }
            Self::MalformedHexInteger(pos) => {
                write!(f, "malformed hex integer at {pos:?}")
            }
            Self::ParserError(ref msg, Some(pos)) => write!(f, "{msg} at {pos}"),
            Self::ParserError(ref msg, _) => write!(f, "{msg}"),
        }
    }
}

impl error::Error for Error {}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Self::Io(err)
    }
}

impl From<ParserError> for Error {
    fn from(err: ParserError) -> Self {
        Self::ParserError(err, None)
    }
}

impl ScanError for Error {
    fn position(&mut self, p: Pos) {
        match *self {
            Self::Io(_) => {}
            Self::UnrecognizedToken(ref mut pos) => *pos = Some(p),
            Self::UnterminatedLiteral(ref mut pos) => *pos = Some(p),
            Self::UnterminatedBracket(ref mut pos) => *pos = Some(p),
            Self::UnterminatedBlockComment(ref mut pos) => *pos = Some(p),
            Self::BadVariableName(ref mut pos) => *pos = Some(p),
            Self::BadNumber(ref mut pos) => *pos = Some(p),
            Self::ExpectedEqualsSign(ref mut pos) => *pos = Some(p),
            Self::MalformedBlobLiteral(ref mut pos) => *pos = Some(p),
            Self::MalformedHexInteger(ref mut pos) => *pos = Some(p),
            Self::ParserError(_, ref mut pos) => *pos = Some(p),
        }
    }
}
