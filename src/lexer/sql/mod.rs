//! Adaptation/port of [`SQLite` tokenizer](http://www.sqlite.org/src/artifact?ci=trunk&filename=src/tokenize.c)
pub use fallible_iterator::FallibleIterator;
use memchr::memchr;
use std::collections::VecDeque;
use std::io::Read;
use std::result::Result;

pub use crate::dialect::TokenType;
use crate::dialect::{
    from_bytes, is_identifier_continue, is_identifier_start, keyword_token, MAX_KEYWORD_LEN,
};
use crate::parser::ast::Cmd;
use crate::parser::parse::{yyParser, YYCODETYPE};
use crate::parser::Context;

mod error;

use crate::lexer::scan::ScanError;
pub use crate::lexer::scan::Splitter;
use crate::lexer::Scanner;
pub use error::Error;

// TODO Extract scanning stuff and move this into the parser crate
// to make possible to use the tokenizer without depending on the parser...

pub struct Parser<R: Read> {
    scanner: Scanner<R, Tokenizer>,
    parser: yyParser,
    buffer: Vec<u8>,
    lookahead: VecDeque<(TokenType, String)>,
}

impl<R: Read> Parser<R> {
    pub fn new(input: R) -> Parser<R> {
        let lexer = Tokenizer::new();
        let scanner = super::Scanner::new(input, lexer);
        let ctx = Context::new();
        let parser = yyParser::new(ctx);
        let buffer = Vec::new();
        let lookahead = VecDeque::new();
        Parser {
            scanner,
            parser,
            buffer,
            lookahead,
        }
    }

    pub fn reset(&mut self, input: R) {
        self.scanner.reset(input);
    }

    pub fn line(&self) -> u64 {
        self.scanner.line()
    }
    pub fn column(&self) -> usize {
        self.scanner.column()
    }

    /*
     ** Return the id of the next token in input.
     */
    fn get_token(&mut self, i: usize) -> Result<TokenType, Error> {
        let mut t = if let Some((token_type, _)) = self.lookahead.get(i) {
            *token_type
        } else {
            let (value, token_type) = match self.scanner.scan()? {
                None => {
                    return Ok(TokenType::TK_EOF);
                }
                Some(tuple) => tuple,
            };
            self.lookahead.push_back((token_type, from_bytes(value)));
            token_type
        };
        if t == TokenType::TK_ID
            || t == TokenType::TK_STRING
            || t == TokenType::TK_JOIN_KW
            || t == TokenType::TK_WINDOW
            || t == TokenType::TK_OVER
            || yyParser::parse_fallback(t as YYCODETYPE) == TokenType::TK_ID as YYCODETYPE
        {
            t = TokenType::TK_ID;
        }
        Ok(t)
    }

    /*
     ** The following three functions are called immediately after the tokenizer
     ** reads the keywords WINDOW, OVER and FILTER, respectively, to determine
     ** whether the token should be treated as a keyword or an SQL identifier.
     ** This cannot be handled by the usual lemon %fallback method, due to
     ** the ambiguity in some constructions. e.g.
     **
     **   SELECT sum(x) OVER ...
     **
     ** In the above, "OVER" might be a keyword, or it might be an alias for the
     ** sum(x) expression. If a "%fallback ID OVER" directive were added to
     ** grammar, then SQLite would always treat "OVER" as an alias, making it
     ** impossible to call a window-function without a FILTER clause.
     **
     ** WINDOW is treated as a keyword if:
     **
     **   * the following token is an identifier, or a keyword that can fallback
     **     to being an identifier, and
     **   * the token after than one is TK_AS.
     **
     ** OVER is a keyword if:
     **
     **   * the previous token was TK_RP, and
     **   * the next token is either TK_LP or an identifier.
     **
     ** FILTER is a keyword if:
     **
     **   * the previous token was TK_RP, and
     **   * the next token is TK_LP.
     */
    fn analyze_window_keyword(&mut self) -> Result<TokenType, Error> {
        let t = self.get_token(0)?;
        if t != TokenType::TK_ID {
            return Ok(TokenType::TK_ID);
        };
        let t = self.get_token(1)?;
        if t != TokenType::TK_AS {
            return Ok(TokenType::TK_ID);
        };
        Ok(TokenType::TK_WINDOW)
    }
    fn analyze_over_keyword(&mut self, last_token: TokenType) -> Result<TokenType, Error> {
        if last_token == TokenType::TK_RP {
            let t = self.get_token(0)?;
            if t == TokenType::TK_LP || t == TokenType::TK_ID {
                return Ok(TokenType::TK_OVER);
            }
        }
        Ok(TokenType::TK_ID)
    }
    fn analyze_filter_keyword(&mut self, last_token: TokenType) -> Result<TokenType, Error> {
        if last_token == TokenType::TK_RP && self.get_token(1)? == TokenType::TK_LP {
            return Ok(TokenType::TK_FILTER);
        }
        Ok(TokenType::TK_ID)
    }
}

impl<R: Read> FallibleIterator for Parser<R> {
    type Item = Cmd;
    type Error = Error;

    fn next(&mut self) -> Result<Option<Cmd>, Error> {
        //print!("line: {}, column: {}: ", self.scanner.line(), self.scanner.column());
        self.parser.ctx.reset();
        let mut last_token_parsed = TokenType::TK_EOF;
        let mut eof = false;
        loop {
            let lookahead = self.lookahead.pop_front();
            let (value, mut token_type) = if let Some((token_type, ref value)) = lookahead {
                (value.as_bytes(), token_type)
            } else {
                match self.scanner.scan()? {
                    None => {
                        eof = true;
                        break;
                    }
                    Some(tuple) => tuple,
                }
            };
            let token = if token_type >= TokenType::TK_WINDOW {
                debug_assert!(
                    token_type == TokenType::TK_OVER
                        || token_type == TokenType::TK_FILTER
                        || token_type == TokenType::TK_WINDOW
                );
                self.buffer.extend_from_slice(value);

                if token_type == TokenType::TK_WINDOW {
                    token_type = self.analyze_window_keyword()?;
                } else if token_type == TokenType::TK_OVER {
                    token_type = self.analyze_over_keyword(last_token_parsed)?;
                } else if token_type == TokenType::TK_FILTER {
                    token_type = self.analyze_filter_keyword(last_token_parsed)?;
                }
                let token = token_type.to_token(self.buffer.as_slice());
                self.buffer.clear();
                token
            } else {
                token_type.to_token(value)
            };
            //print!("({:?}, {:?})", token_type, token);
            self.parser.sqlite3Parser(token_type, token);
            last_token_parsed = token_type;
            if self.parser.ctx.done() {
                //println!();
                break;
            }
        }
        self.lookahead.clear();
        if last_token_parsed == TokenType::TK_EOF {
            return Ok(None); // empty input
        }
        /* Upon reaching the end of input, call the parser two more times
        with tokens TK_SEMI and 0, in that order. */
        if eof && self.parser.ctx.is_ok() {
            if last_token_parsed != TokenType::TK_SEMI {
                self.parser.sqlite3Parser(TokenType::TK_SEMI, None);
            }
            self.parser.sqlite3Parser(TokenType::TK_EOF, None);
        }
        self.parser.sqlite3ParserFinalize();
        if let Some(msg) = self.parser.ctx.error() {
            let mut err = Error::SyntaxError(msg, None);
            err.position(self.scanner.line(), self.scanner.column());
            return Err(err);
        }
        let cmd = self.parser.ctx.cmd();
        assert_ne!(cmd, None);
        Ok(cmd)
    }
}

pub type Token<'input> = (&'input [u8], TokenType);

#[derive(Default)]
pub struct Tokenizer {
    uppercase_buffer: [u8; MAX_KEYWORD_LEN],
}

impl Tokenizer {
    pub fn new() -> Tokenizer {
        Tokenizer {
            uppercase_buffer: [0; MAX_KEYWORD_LEN],
        }
    }
}

impl Splitter for Tokenizer {
    type Error = Error;
    type TokenType = TokenType;

    fn split<'input>(
        &mut self,
        data: &'input mut [u8],
        eof: bool,
    ) -> Result<(Option<Token<'input>>, usize), Error> {
        if eof && data.is_empty() {
            return Ok((None, 0));
        }
        if data[0].is_ascii_whitespace() {
            // eat as much space as possible
            return Ok((
                None,
                match data.iter().skip(1).position(|&b| !b.is_ascii_whitespace()) {
                    Some(i) => i + 1,
                    _ => data.len(),
                },
            ));
        }
        match data[0] {
            b'-' => {
                if let Some(b) = data.get(1) {
                    if *b == b'-' {
                        // eat comment
                        if let Some(i) = memchr(b'\n', data) {
                            return Ok((None, i + 1));
                        } else if eof {
                            return Ok((None, data.len()));
                        } // else ask more data until '\n'
                    } else {
                        return Ok((Some((&data[..1], TokenType::TK_MINUS)), 1));
                    }
                } else if eof {
                    return Ok((Some((&data[..1], TokenType::TK_MINUS)), 1));
                } // else ask more data
            }
            b'(' => return Ok((Some((&data[..1], TokenType::TK_LP)), 1)),
            b')' => return Ok((Some((&data[..1], TokenType::TK_RP)), 1)),
            b';' => return Ok((Some((&data[..1], TokenType::TK_SEMI)), 1)),
            b'+' => return Ok((Some((&data[..1], TokenType::TK_PLUS)), 1)),
            b'*' => return Ok((Some((&data[..1], TokenType::TK_STAR)), 1)),
            b'/' => {
                if let Some(b) = data.get(1) {
                    if *b == b'*' {
                        // eat comment
                        let mut pb = 0;
                        let mut end = None;
                        for (i, b) in data.iter().enumerate().skip(2) {
                            if *b == b'/' && pb == b'*' {
                                end = Some(i);
                                break;
                            }
                            pb = *b;
                        }
                        if let Some(i) = end {
                            return Ok((None, i + 1));
                        } else if eof {
                            return Err(Error::UnterminatedBlockComment(None));
                        } // else ask more data until '*/'
                    } else {
                        return Ok((Some((&data[..1], TokenType::TK_SLASH)), 1));
                    }
                } else if eof {
                    return Ok((Some((&data[..1], TokenType::TK_SLASH)), 1));
                }
            }
            b'%' => return Ok((Some((&data[..1], TokenType::TK_REM)), 1)),
            b'=' => {
                if let Some(b) = data.get(1) {
                    return Ok(if *b == b'=' {
                        (Some((&data[..2], TokenType::TK_EQ)), 2)
                    } else {
                        (Some((&data[..1], TokenType::TK_EQ)), 1)
                    });
                } else if eof {
                    return Ok((Some((&data[..1], TokenType::TK_EQ)), 1));
                } // else ask more data to fuse '==' or not
            }
            b'<' => {
                if let Some(b) = data.get(1) {
                    return Ok(match *b {
                        b'=' => (Some((&data[..2], TokenType::TK_LE)), 2),
                        b'>' => (Some((&data[..2], TokenType::TK_NE)), 2),
                        b'<' => (Some((&data[..2], TokenType::TK_LSHIFT)), 2),
                        _ => (Some((&data[..1], TokenType::TK_LT)), 1),
                    });
                } else if eof {
                    return Ok((Some((&data[..1], TokenType::TK_LT)), 1));
                } // else ask more data
            }
            b'>' => {
                if let Some(b) = data.get(1) {
                    return Ok(match *b {
                        b'=' => (Some((&data[..2], TokenType::TK_GE)), 2),
                        b'>' => (Some((&data[..2], TokenType::TK_RSHIFT)), 2),
                        _ => (Some((&data[..1], TokenType::TK_GT)), 1),
                    });
                } else if eof {
                    return Ok((Some((&data[..1], TokenType::TK_GT)), 1));
                } // else ask more data
            }
            b'!' => {
                if let Some(b) = data.get(1) {
                    return if *b == b'=' {
                        Ok((Some((&data[..2], TokenType::TK_NE)), 2))
                    } else {
                        Err(Error::ExpectedEqualsSign(None))
                    };
                } else if eof {
                    return Err(Error::ExpectedEqualsSign(None));
                } // else ask more data
            }
            b'|' => {
                if let Some(b) = data.get(1) {
                    return Ok(if *b == b'|' {
                        (Some((&data[..2], TokenType::TK_CONCAT)), 2)
                    } else {
                        (Some((&data[..1], TokenType::TK_BITOR)), 1)
                    });
                } else if eof {
                    return Ok((Some((&data[..1], TokenType::TK_BITOR)), 1));
                } // else ask more data
            }
            b',' => return Ok((Some((&data[..1], TokenType::TK_COMMA)), 1)),
            b'&' => return Ok((Some((&data[..1], TokenType::TK_BITAND)), 1)),
            b'~' => return Ok((Some((&data[..1], TokenType::TK_BITNOT)), 1)),
            quote @ b'`' | quote @ b'\'' | quote @ b'"' => return literal(data, eof, quote),
            b'.' => {
                if let Some(b) = data.get(1) {
                    if b.is_ascii_digit() {
                        return fractional_part(data, eof, 0);
                    } else if eof {
                        return Ok((Some((&data[..1], TokenType::TK_DOT)), 1));
                    }
                } else if eof {
                    return Ok((Some((&data[..1], TokenType::TK_DOT)), 1));
                } // else ask more data
            }
            b'0'..=b'9' => return number(data, eof),
            b'[' => {
                if let Some(i) = memchr(b']', data) {
                    // do not include the '['/']' in the token
                    return Ok((Some((&data[1..i], TokenType::TK_ID)), i + 1));
                } else if eof {
                    return Err(Error::UnterminatedBracket(None));
                } // else ask more data until ']'
            }
            b'?' => {
                match data.iter().skip(1).position(|&b| !b.is_ascii_digit()) {
                    Some(i) => {
                        // do not include the '?' in the token
                        return Ok((Some((&data[1..=i], TokenType::TK_VARIABLE)), i + 1));
                    }
                    None if eof => {
                        return Ok((Some((&data[1..], TokenType::TK_VARIABLE)), data.len()));
                    }
                    _ => {
                        // else ask more data
                    }
                };
            }
            b'$' | b'@' | b'#' | b':' => {
                match data
                    .iter()
                    .skip(1)
                    .position(|&b| !is_identifier_continue(b))
                {
                    Some(0) => return Err(Error::BadVariableName(None)),
                    Some(i) => {
                        // '$' is included as part of the name
                        return Ok((Some((&data[..=i], TokenType::TK_VARIABLE)), i + 1));
                    }
                    None if eof => {
                        if data.len() == 1 {
                            return Err(Error::BadVariableName(None));
                        }
                        return Ok((Some((data, TokenType::TK_VARIABLE)), data.len()));
                    }
                    _ => {
                        // else ask more data
                    }
                };
            }
            b if is_identifier_start(b) => {
                return if b == b'x' || b == b'X' {
                    if let Some(&b'\'') = data.get(1) {
                        blob_literal(data, eof)
                    } else {
                        self.identifierish(data, eof)
                    }
                } else {
                    self.identifierish(data, eof)
                }
            }
            _ => return Err(Error::UnrecognizedToken(None)),
        };
        // Request more data.
        Ok((None, 0))
    }
}

fn literal(data: &mut [u8], eof: bool, quote: u8) -> Result<(Option<Token<'_>>, usize), Error> {
    debug_assert_eq!(data[0], quote);
    let tt = if quote == b'\'' {
        TokenType::TK_STRING
    } else {
        TokenType::TK_ID
    };
    let mut pb = 0;
    let mut end = None;
    let mut escaped_quotes = false;
    // data[0] == quote => skip(1)
    for (i, b) in data.iter().enumerate().skip(1) {
        if *b == quote {
            if pb == quote {
                // escaped quote
                pb = 0;
                escaped_quotes = true;
                continue;
            }
        } else if pb == quote {
            end = Some(i);
            break;
        }
        pb = *b;
    }
    if end.is_some() || (eof && pb == quote) {
        let i = match end {
            Some(i) => i,
            _ => data.len(),
        };
        // do not include the quote in the token
        return Ok((
            Some((
                if escaped_quotes {
                    unescape_quotes(&mut data[1..i - 1], quote)
                } else {
                    &data[1..i - 1]
                },
                tt,
            )),
            i,
        ));
    } else if eof {
        return Err(Error::UnterminatedLiteral(None));
    }
    // else ask more data until closing quote
    Ok((None, 0))
}

fn unescape_quotes(data: &mut [u8], quote: u8) -> &[u8] {
    let mut i = 0;
    let mut j = 0;
    while i < data.len() {
        data[j] = data[i];
        if data[i] == quote {
            i += 1;
        }
        i += 1;
        j += 1;
    }
    &data[..j]
}

fn blob_literal(data: &[u8], eof: bool) -> Result<(Option<Token<'_>>, usize), Error> {
    debug_assert!(data[0] == b'x' || data[0] == b'X');
    debug_assert_eq!(data[1], b'\'');
    if let Some((i, b)) = data
        .iter()
        .enumerate()
        .skip(2)
        .find(|&(_, &b)| !b.is_ascii_hexdigit())
    {
        if *b != b'\'' || i % 2 != 0 {
            return Err(Error::MalformedBlobLiteral(None));
        }
        return Ok((Some((&data[2..i], TokenType::TK_BLOB)), i + 1));
    } else if eof {
        return Err(Error::MalformedBlobLiteral(None));
    }
    // else ask more data
    Ok((None, 0))
}

fn number(data: &[u8], eof: bool) -> Result<(Option<Token<'_>>, usize), Error> {
    debug_assert!(data[0].is_ascii_digit());
    if data[0] == b'0' {
        if let Some(b) = data.get(1) {
            if *b == b'x' || *b == b'X' {
                return hex_integer(data, eof);
            }
        } else if eof {
            return Ok((Some((data, TokenType::TK_INTEGER)), data.len()));
        } else {
            // ask more data
            return Ok((None, 0));
        }
    }
    if let Some((i, b)) = data
        .iter()
        .enumerate()
        .skip(1)
        .find(|&(_, &b)| !b.is_ascii_digit())
    {
        if *b == b'.' {
            return fractional_part(data, eof, i);
        } else if *b == b'e' || *b == b'E' {
            return exponential_part(data, eof, i);
        } else if is_identifier_start(*b) {
            return Err(Error::BadNumber(None));
        }
        return Ok((Some((&data[..i], TokenType::TK_INTEGER)), i));
    } else if eof {
        return Ok((Some((data, TokenType::TK_INTEGER)), data.len()));
    }
    // else ask more data
    Ok((None, 0))
}

fn hex_integer(data: &[u8], eof: bool) -> Result<(Option<Token<'_>>, usize), Error> {
    debug_assert_eq!(data[0], b'0');
    debug_assert!(data[1] == b'x' || data[1] == b'X');
    if let Some((i, b)) = data
        .iter()
        .enumerate()
        .skip(2)
        .find(|&(_, &b)| !b.is_ascii_hexdigit())
    {
        // Must not be empty (Ox is invalid)
        if i == 2 || is_identifier_start(*b) {
            return Err(Error::MalformedHexInteger(None));
        }
        return Ok((Some((&data[..i], TokenType::TK_INTEGER)), i));
    } else if eof {
        // Must not be empty (Ox is invalid)
        if data.len() == 2 {
            return Err(Error::MalformedHexInteger(None));
        }
        return Ok((Some((data, TokenType::TK_INTEGER)), data.len()));
    }
    // else ask more data
    Ok((None, 0))
}

fn fractional_part(data: &[u8], eof: bool, i: usize) -> Result<(Option<Token<'_>>, usize), Error> {
    debug_assert_eq!(data[i], b'.');
    if let Some((i, b)) = data
        .iter()
        .enumerate()
        .skip(i + 1)
        .find(|&(_, &b)| !b.is_ascii_digit())
    {
        if *b == b'e' || *b == b'E' {
            return exponential_part(data, eof, i);
        } else if is_identifier_start(*b) {
            return Err(Error::BadNumber(None));
        }
        return Ok((Some((&data[..i], TokenType::TK_FLOAT)), i));
    } else if eof {
        return Ok((Some((data, TokenType::TK_FLOAT)), data.len()));
    }
    // else ask more data
    Ok((None, 0))
}

fn exponential_part(data: &[u8], eof: bool, i: usize) -> Result<(Option<Token<'_>>, usize), Error> {
    debug_assert!(data[i] == b'e' || data[i] == b'E');
    // data[i] == 'e'|'E'
    if let Some(b) = data.get(i + 1) {
        let i = if *b == b'+' || *b == b'-' { i + 1 } else { i };
        if let Some((i, b)) = data
            .iter()
            .enumerate()
            .skip(i + 1)
            .find(|&(_, &b)| !b.is_ascii_digit())
        {
            if is_identifier_start(*b) {
                return Err(Error::BadNumber(None));
            }
            return Ok((Some((&data[..i], TokenType::TK_FLOAT)), i));
        } else if eof {
            if data.len() == i + 1 {
                return Err(Error::BadNumber(None));
            }
            return Ok((Some((data, TokenType::TK_FLOAT)), data.len()));
        }
    } else if eof {
        return Err(Error::BadNumber(None));
    }
    // else ask more data
    Ok((None, 0))
}

impl Tokenizer {
    fn identifierish<'input>(
        &mut self,
        data: &'input [u8],
        eof: bool,
    ) -> Result<(Option<Token<'input>>, usize), Error> {
        debug_assert!(is_identifier_start(data[0]));
        // data[0] is_identifier_start => skip(1)
        let end = data
            .iter()
            .skip(1)
            .position(|&b| !is_identifier_continue(b));
        if end.is_some() || eof {
            let i = match end {
                Some(i) => i + 1,
                _ => data.len(),
            };
            let word = &data[..i];
            let tt = if word.len() >= 2 && word.len() <= MAX_KEYWORD_LEN && word.is_ascii() {
                let upcase = if word.iter().all(|b| b.is_ascii_uppercase()) {
                    word
                } else {
                    let buffer = &mut self.uppercase_buffer[..word.len()];
                    buffer.copy_from_slice(word);
                    buffer.make_ascii_uppercase();
                    buffer
                };
                keyword_token(upcase).unwrap_or(TokenType::TK_ID)
            } else {
                TokenType::TK_ID
            };
            return Ok((Some((word, tt)), i));
        }
        // else ask more data
        Ok((None, 0))
    }
}
