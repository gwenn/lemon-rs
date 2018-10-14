//! Adaptation/port of [`SQLite` tokenizer](http://www.sqlite.org/src/artifact?ci=trunk&filename=src/tokenize.c)
use std::result::Result;

use memchr::memchr;
pub use parser::token::TokenType;
use phf;

mod error;

pub use scan::Splitter;
pub use sql::error::Error;

pub fn parse(sql: &str) -> Result<Option<()>, Error> {
    let lexer = Tokenizer::new();
    let mut s = super::Scanner::new(sql.as_bytes(), lexer);
    //let ctx = ;
    //let mut parser = ;
    let mut last_token_parsed = TokenType::TK_EOF;
    let mut eof = false;
    loop {
        let (token, mut token_type) = match s.scan() {
            Err(e) => {
                return Err(e);
            }
            Ok(None) => {
                eof = true;
                break;
            }
            Ok(Some(tuple)) => tuple,
        };
        if token_type >= TokenType::TK_WINDOW {
            debug_assert!(
                token_type == TokenType::TK_OVER
                    || token_type == TokenType::TK_FILTER
                    || token_type == TokenType::TK_WINDOW
            );
        }
        if token_type == TokenType::TK_WINDOW {
            token_type = analyze_window_keyword();
        } else if token_type == TokenType::TK_OVER {
            token_type = analyze_over_keyword(last_token_parsed);
        } else if token_type == TokenType::TK_FILTER {
            token_type = analyze_filter_keyword(last_token_parsed);
        }
        //parser.sqlite3Parser(token_type, token);
        last_token_parsed = token_type;
        if
        /*ctx.done()*/
        true {
            break;
        }
    }
    if last_token_parsed == TokenType::TK_EOF {
        return Ok(None); // empty input
    }
    /* Upon reaching the end of input, call the parser two more times
    okens TK_SEMI and 0, in that order. */
    if eof {
        if last_token_parsed != TokenType::TK_SEMI {
            //parser.sqlite3Parser(TokenType::TK_SEMI, &";");
        }
        //parser.sqlite3Parser(TokenType::TK_EOF, &"");
    }
    //parser.sqlite3ParserFinalize();
    //assert ctx.stmt != null;
    Ok(Some(()))
}

/*
** Return the id of the next token in input.
*/
fn get_token() -> TokenType {
    let mut t = TokenType::TK_ID; /* Token type to return */
    /*while t == TokenType::TK_SPACE {
        // FIXME
    }*/
    if t == TokenType::TK_ID
        || t == TokenType::TK_STRING
        || t == TokenType::TK_JOIN_KW
        || t == TokenType::TK_WINDOW
        || t == TokenType::TK_OVER
    // FIXME || parser::sqlite3ParserFallback(t) == TokenType::TK_ID
    {
        t = TokenType::TK_ID;
    }
    t
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
fn analyze_window_keyword() -> TokenType {
    let t = get_token();
    if t != TokenType::TK_ID {
        return TokenType::TK_ID;
    };
    let t = get_token();
    if t != TokenType::TK_AS {
        return TokenType::TK_ID;
    };
    TokenType::TK_WINDOW
}
fn analyze_over_keyword(last_token: TokenType) -> TokenType {
    if last_token == TokenType::TK_RP {
        let t = get_token();
        if t == TokenType::TK_LP || t == TokenType::TK_ID {
            return TokenType::TK_OVER;
        }
    }
    TokenType::TK_ID
}
fn analyze_filter_keyword(last_token: TokenType) -> TokenType {
    if last_token == TokenType::TK_RP && get_token() == TokenType::TK_LP {
        return TokenType::TK_FILTER;
    }
    TokenType::TK_ID
}

static KEYWORDS: phf::Map<&[u8], TokenType> = phf_map! {
    b"ABORT" => TokenType::TK_ABORT,
    b"ACTION" => TokenType::TK_ACTION,
    b"ADD" => TokenType::TK_ADD,
    b"AFTER" => TokenType::TK_AFTER,
    b"ALL" => TokenType::TK_ALL,
    b"ALTER" => TokenType::TK_ALTER,
    b"ANALYZE" => TokenType::TK_ANALYZE,
    b"AND" => TokenType::TK_AND,
    b"AS" => TokenType::TK_AS,
    b"ASC" => TokenType::TK_ASC,
    b"ATTACH" => TokenType::TK_ATTACH,
    b"AUTOINCREMENT" => TokenType::TK_AUTOINCR,
    b"BEFORE" => TokenType::TK_BEFORE,
    b"BEGIN" => TokenType::TK_BEGIN,
    b"BETWEEN" => TokenType::TK_BETWEEN,
    b"BY" => TokenType::TK_BY,
    b"CASCADE" => TokenType::TK_CASCADE,
    b"CASE" => TokenType::TK_CASE,
    b"CAST" => TokenType::TK_CAST,
    b"CHECK" => TokenType::TK_CHECK,
    b"COLLATE" => TokenType::TK_COLLATE,
    b"COLUMN" => TokenType::TK_COLUMNKW,
    b"COMMIT" => TokenType::TK_COMMIT,
    b"CONFLICT" => TokenType::TK_CONFLICT,
    b"CONSTRAINT" => TokenType::TK_CONSTRAINT,
    b"CREATE" => TokenType::TK_CREATE,
    b"CROSS" => TokenType::TK_JOIN_KW,
    b"CURRENT" => TokenType::TK_CURRENT,
    b"CURRENT_DATE" => TokenType::TK_CTIME_KW,
    b"CURRENT_TIME" => TokenType::TK_CTIME_KW,
    b"CURRENT_TIMESTAMP" => TokenType::TK_CTIME_KW,
    b"DATABASE" => TokenType::TK_DATABASE,
    b"DEFAULT" => TokenType::TK_DEFAULT,
    b"DEFERRABLE" => TokenType::TK_DEFERRABLE,
    b"DEFERRED" => TokenType::TK_DEFERRED,
    b"DELETE" => TokenType::TK_DELETE,
    b"DESC" => TokenType::TK_DESC,
    b"DETACH" => TokenType::TK_DETACH,
    b"DISTINCT" => TokenType::TK_DISTINCT,
    b"DO" => TokenType::TK_DO,
    b"DROP" => TokenType::TK_DROP,
    b"EACH" => TokenType::TK_EACH,
    b"ELSE" => TokenType::TK_ELSE,
    b"END" => TokenType::TK_END,
    b"ESCAPE" => TokenType::TK_ESCAPE,
    b"EXCEPT" => TokenType::TK_EXCEPT,
    b"EXCLUSIVE" => TokenType::TK_EXCLUSIVE,
    b"EXISTS" => TokenType::TK_EXISTS,
    b"EXPLAIN" => TokenType::TK_EXPLAIN,
    b"FAIL" => TokenType::TK_FAIL,
    b"FILTER" => TokenType::TK_FILTER,
    b"FOLLOWING" => TokenType::TK_FOLLOWING,
    b"FOR" => TokenType::TK_FOR,
    b"FOREIGN" => TokenType::TK_FOREIGN,
    b"FROM" => TokenType::TK_FROM,
    b"FULL" => TokenType::TK_JOIN_KW,
    b"GLOB" => TokenType::TK_LIKE_KW,
    b"GROUP" => TokenType::TK_GROUP,
    b"HAVING" => TokenType::TK_HAVING,
    b"IF" => TokenType::TK_IF,
    b"IGNORE" => TokenType::TK_IGNORE,
    b"IMMEDIATE" => TokenType::TK_IMMEDIATE,
    b"IN" => TokenType::TK_IN,
    b"INDEX" => TokenType::TK_INDEX,
    b"INDEXED" => TokenType::TK_INDEXED,
    b"INITIALLY" => TokenType::TK_INITIALLY,
    b"INNER" => TokenType::TK_JOIN_KW,
    b"INSERT" => TokenType::TK_INSERT,
    b"INSTEAD" => TokenType::TK_INSTEAD,
    b"INTERSECT" => TokenType::TK_INTERSECT,
    b"INTO" => TokenType::TK_INTO,
    b"IS" => TokenType::TK_IS,
    b"ISNULL" => TokenType::TK_ISNULL,
    b"JOIN" => TokenType::TK_JOIN,
    b"KEY" => TokenType::TK_KEY,
    b"LEFT" => TokenType::TK_JOIN_KW,
    b"LIKE" => TokenType::TK_LIKE_KW,
    b"LIMIT" => TokenType::TK_LIMIT,
    b"MATCH" => TokenType::TK_MATCH,
    b"NATURAL" => TokenType::TK_JOIN_KW,
    b"NO" => TokenType::TK_NO,
    b"NOT" => TokenType::TK_NOT,
    b"NOTHING" => TokenType::TK_NOTHING,
    b"NOTNULL" => TokenType::TK_NOTNULL,
    b"NULL" => TokenType::TK_NULL,
    b"OF" => TokenType::TK_OF,
    b"OFFSET" => TokenType::TK_OFFSET,
    b"ON" => TokenType::TK_ON,
    b"OR" => TokenType::TK_OR,
    b"ORDER" => TokenType::TK_ORDER,
    b"OUTER" => TokenType::TK_JOIN_KW,
    b"OVER" => TokenType::TK_OVER,
    b"PARTITION" => TokenType::TK_PARTITION,
    b"PLAN" => TokenType::TK_PLAN,
    b"PRAGMA" => TokenType::TK_PRAGMA,
    b"PRECEDING" => TokenType::TK_PRECEDING,
    b"PRIMARY" => TokenType::TK_PRIMARY,
    b"QUERY" => TokenType::TK_QUERY,
    b"RAISE" => TokenType::TK_RAISE,
    b"RANGE" => TokenType::TK_RANGE,
    b"RECURSIVE" => TokenType::TK_RECURSIVE,
    b"REFERENCES" => TokenType::TK_REFERENCES,
    b"REGEXP" => TokenType::TK_LIKE_KW,
    b"REINDEX" => TokenType::TK_REINDEX,
    b"RELEASE" => TokenType::TK_RELEASE,
    b"RENAME" => TokenType::TK_RENAME,
    b"REPLACE" => TokenType::TK_REPLACE,
    b"RESTRICT" => TokenType::TK_RESTRICT,
    b"RIGHT" => TokenType::TK_JOIN_KW,
    b"ROLLBACK" => TokenType::TK_ROLLBACK,
    b"ROW" => TokenType::TK_ROW,
    b"ROWS" => TokenType::TK_ROWS,
    b"SAVEPOINT" => TokenType::TK_SAVEPOINT,
    b"SELECT" => TokenType::TK_SELECT,
    b"SET" => TokenType::TK_SET,
    b"TABLE" => TokenType::TK_TABLE,
    b"TEMP" => TokenType::TK_TEMP,
    b"TEMPORARY" => TokenType::TK_TEMP,
    b"THEN" => TokenType::TK_THEN,
    b"TO" => TokenType::TK_TO,
    b"TRANSACTION" => TokenType::TK_TRANSACTION,
    b"TRIGGER" => TokenType::TK_TRIGGER,
    b"UNBOUNDED" => TokenType::TK_UNBOUNDED,
    b"UNION" => TokenType::TK_UNION,
    b"UNIQUE" => TokenType::TK_UNIQUE,
    b"UPDATE" => TokenType::TK_UPDATE,
    b"USING" => TokenType::TK_USING,
    b"VACUUM" => TokenType::TK_VACUUM,
    b"VALUES" => TokenType::TK_VALUES,
    b"VIEW" => TokenType::TK_VIEW,
    b"VIRTUAL" => TokenType::TK_VIRTUAL,
    b"WHEN" => TokenType::TK_WHEN,
    b"WHERE" => TokenType::TK_WHERE,
    b"WINDOW" => TokenType::TK_WINDOW,
    b"WITH" => TokenType::TK_WITH,
    b"WITHOUT" => TokenType::TK_WITHOUT
};
const MAX_KEYWORD_LEN: usize = 17;

pub type Token<'input> = (&'input [u8], TokenType);

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
                }
            } /* else ask more data */
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
                }
            } /* else ask more data to fuse '==' or not */
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
                }
            } /* else ask more data */
            b'>' => {
                if let Some(b) = data.get(1) {
                    return Ok(match *b {
                        b'=' => (Some((&data[..2], TokenType::TK_GE)), 2),
                        b'>' => (Some((&data[..2], TokenType::TK_RSHIFT)), 2),
                        _ => (Some((&data[..1], TokenType::TK_GT)), 1),
                    });
                } else if eof {
                    return Ok((Some((&data[..1], TokenType::TK_GT)), 1));
                }
            } /* else ask more data */
            b'!' => {
                if let Some(b) = data.get(1) {
                    if *b == b'=' {
                        return Ok((Some((&data[..2], TokenType::TK_NE)), 2));
                    } else {
                        return Err(Error::ExpectedEqualsSign(None));
                    }
                } else if eof {
                    return Err(Error::ExpectedEqualsSign(None));
                }
            } /* else ask more data */
            b'|' => {
                if let Some(b) = data.get(1) {
                    return Ok(if *b == b'|' {
                        (Some((&data[..2], TokenType::TK_CONCAT)), 2)
                    } else {
                        (Some((&data[..1], TokenType::TK_BITOR)), 1)
                    });
                } else if eof {
                    return Ok((Some((&data[..1], TokenType::TK_BITOR)), 1));
                }
            } /* else ask more data */
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
                }
            } /* else ask more data */
            b'0'...b'9' => return number(data, eof),
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
                        return Ok((Some((&data[1..], TokenType::TK_VARIABLE)), data.len()))
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
                if b == b'x' || b == b'X' {
                    if let Some(&b'\'') = data.get(1) {
                        return blob_literal(data, eof);
                    } else {
                        return self.identifierish(data, eof);
                    }
                } else {
                    return self.identifierish(data, eof);
                }
            }
            _ => return Err(Error::UnrecognizedToken(None)),
        };
        // Request more data.
        Ok((None, 0))
    }
}

fn literal<'input>(
    data: &'input mut [u8],
    eof: bool,
    quote: u8,
) -> Result<(Option<Token<'input>>, usize), Error> {
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

fn blob_literal<'input>(
    data: &'input [u8],
    eof: bool,
) -> Result<(Option<Token<'input>>, usize), Error> {
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

fn number<'input>(data: &'input [u8], eof: bool) -> Result<(Option<Token<'input>>, usize), Error> {
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

fn hex_integer<'input>(
    data: &'input [u8],
    eof: bool,
) -> Result<(Option<Token<'input>>, usize), Error> {
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

fn fractional_part<'input>(
    data: &'input [u8],
    eof: bool,
    i: usize,
) -> Result<(Option<Token<'input>>, usize), Error> {
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

fn exponential_part<'input>(
    data: &'input [u8],
    eof: bool,
    i: usize,
) -> Result<(Option<Token<'input>>, usize), Error> {
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
                    let mut buffer = &mut self.uppercase_buffer[..word.len()];
                    buffer.copy_from_slice(word);
                    buffer.make_ascii_uppercase();
                    buffer
                };
                // https://github.com/rust-lang/rust/issues/28853
                let b = unsafe { ::std::mem::transmute::<_, &'static [u8]>(upcase) };
                KEYWORDS.get(&b).cloned().unwrap_or(TokenType::TK_ID)
            } else {
                TokenType::TK_ID
            };
            return Ok((Some((word, tt)), i));
        }
        // else ask more data
        Ok((None, 0))
    }
}

fn is_identifier_start(b: u8) -> bool {
    (b >= b'A' && b <= b'Z') || b == b'_' || (b >= b'a' && b <= b'z') || b > b'\x7F'
}

fn is_identifier_continue(b: u8) -> bool {
    b == b'$'
        || (b >= b'0' && b <= b'9')
        || (b >= b'A' && b <= b'Z')
        || b == b'_'
        || (b >= b'a' && b <= b'z')
        || b > b'\x7F'
}
