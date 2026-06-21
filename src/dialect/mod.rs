//! SQLite dialect

use bumpalo::Bump;
use std::fmt::Formatter;
use std::str;

mod token;
pub use token::TokenType;

/// Token value (lexeme)
#[derive(Clone, Copy)]
pub struct Token<'i>(pub usize, pub &'i [u8], pub usize);

pub(crate) fn sentinel(start: usize) -> Token<'static> {
    Token(start, b"", start)
}

impl Token<'_> {
    /// Access token value
    pub fn unwrap(self, b: &Bump) -> &str {
        from_bytes(self.1, b)
    }
}

impl std::fmt::Debug for Token<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Token").field(&self.1).finish()
    }
}

impl TokenType {
    // TODO try Cow<&'static, str> (Borrowed<&'static str> for keyword and Owned<String> for below),
    // => Syntax error on keyword will be better
    // => `from_token` will become unnecessary
    pub(crate) fn to_token(self, start: usize, value: &[u8], end: usize) -> Token<'_> {
        Token(start, value, end)
    }
}

pub(crate) fn from_bytes<'bump>(bytes: &[u8], b: &'bump Bump) -> &'bump str {
    b.alloc_str(str::from_utf8(bytes).unwrap()) // FIXME error handling
}

pub(crate) const MAX_KEYWORD_LEN: usize = 17;

/// Check if `word` is a keyword
pub fn keyword_token(word: &[u8]) -> Option<TokenType> {
    hashify::tiny_map_ignore_case! {
        word,
        b"ABORT" => TokenType::TK_ABORT,
        b"ACTION" => TokenType::TK_ACTION,
        b"ADD" => TokenType::TK_ADD,
        b"AFTER" => TokenType::TK_AFTER,
        b"ALL" => TokenType::TK_ALL,
        b"ALTER" => TokenType::TK_ALTER,
        b"ALWAYS" => TokenType::TK_ALWAYS,
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
        b"EXCLUDE" => TokenType::TK_EXCLUDE,
        b"EXCLUSIVE" => TokenType::TK_EXCLUSIVE,
        b"EXISTS" => TokenType::TK_EXISTS,
        b"EXPLAIN" => TokenType::TK_EXPLAIN,
        b"FAIL" => TokenType::TK_FAIL,
        b"FILTER" => TokenType::TK_FILTER,
        b"FIRST" => TokenType::TK_FIRST,
        b"FOLLOWING" => TokenType::TK_FOLLOWING,
        b"FOR" => TokenType::TK_FOR,
        b"FOREIGN" => TokenType::TK_FOREIGN,
        b"FROM" => TokenType::TK_FROM,
        b"FULL" => TokenType::TK_JOIN_KW,
        b"GENERATED" => TokenType::TK_GENERATED,
        b"GLOB" => TokenType::TK_LIKE_KW,
        b"GROUP" => TokenType::TK_GROUP,
        b"GROUPS" => TokenType::TK_GROUPS,
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
        b"LAST" => TokenType::TK_LAST,
        b"LEFT" => TokenType::TK_JOIN_KW,
        b"LIKE" => TokenType::TK_LIKE_KW,
        b"LIMIT" => TokenType::TK_LIMIT,
        b"MATCH" => TokenType::TK_MATCH,
        b"MATERIALIZED" => TokenType::TK_MATERIALIZED,
        b"NATURAL" => TokenType::TK_JOIN_KW,
        b"NO" => TokenType::TK_NO,
        b"NOT" => TokenType::TK_NOT,
        b"NOTHING" => TokenType::TK_NOTHING,
        b"NOTNULL" => TokenType::TK_NOTNULL,
        b"NULL" => TokenType::TK_NULL,
        b"NULLS" => TokenType::TK_NULLS,
        b"OF" => TokenType::TK_OF,
        b"OFFSET" => TokenType::TK_OFFSET,
        b"ON" => TokenType::TK_ON,
        b"OR" => TokenType::TK_OR,
        b"ORDER" => TokenType::TK_ORDER,
        b"OTHERS" => TokenType::TK_OTHERS,
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
        b"RETURNING" => TokenType::TK_RETURNING,
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
        b"TIES" => TokenType::TK_TIES,
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
        b"WITHOUT" => TokenType::TK_WITHOUT,
    }
}

pub(crate) fn is_identifier(name: &str) -> bool {
    if name.is_empty() {
        return false;
    }
    let bytes = name.as_bytes();
    is_identifier_start(bytes[0])
        && (bytes.len() == 1 || bytes[1..].iter().all(|b| is_identifier_continue(*b)))
}

pub(crate) fn is_identifier_start(b: u8) -> bool {
    b.is_ascii_uppercase() || b == b'_' || b.is_ascii_lowercase() || b > b'\x7F'
}

pub(crate) fn is_identifier_continue(b: u8) -> bool {
    b == b'$'
        || b.is_ascii_digit()
        || b.is_ascii_uppercase()
        || b == b'_'
        || b.is_ascii_lowercase()
        || b > b'\x7F'
}

// keyword may become an identifier
// see %fallback in parse.y
pub(crate) fn from_token<'bump>(_ty: u16, value: Token, b: &'bump Bump) -> &'bump str {
    from_bytes(value.1, b)
}

impl TokenType {
    /// Return the associated string (mainly for testing)
    pub const fn as_str(&self) -> Option<&'static str> {
        use TokenType::*;
        match self {
            TK_ABORT => Some("ABORT"),
            TK_ACTION => Some("ACTION"),
            TK_ADD => Some("ADD"),
            TK_AFTER => Some("AFTER"),
            TK_ALL => Some("ALL"),
            TK_ALTER => Some("ALTER"),
            TK_ANALYZE => Some("ANALYZE"),
            TK_ALWAYS => Some("ALWAYS"),
            TK_AND => Some("AND"),
            TK_AS => Some("AS"),
            TK_ASC => Some("ASC"),
            TK_ATTACH => Some("ATTACH"),
            TK_AUTOINCR => Some("AUTOINCREMENT"),
            TK_BEFORE => Some("BEFORE"),
            TK_BEGIN => Some("BEGIN"),
            TK_BETWEEN => Some("BETWEEN"),
            TK_BY => Some("BY"),
            TK_CASCADE => Some("CASCADE"),
            TK_CASE => Some("CASE"),
            TK_CAST => Some("CAST"),
            TK_CHECK => Some("CHECK"),
            TK_COLLATE => Some("COLLATE"),
            TK_COLUMNKW => Some("COLUMN"),
            TK_COMMIT => Some("COMMIT"),
            TK_CONFLICT => Some("CONFLICT"),
            TK_CONSTRAINT => Some("CONSTRAINT"),
            TK_CREATE => Some("CREATE"),
            TK_CURRENT => Some("CURRENT"),
            TK_DATABASE => Some("DATABASE"),
            TK_DEFAULT => Some("DEFAULT"),
            TK_DEFERRABLE => Some("DEFERRABLE"),
            TK_DEFERRED => Some("DEFERRED"),
            TK_DELETE => Some("DELETE"),
            TK_DESC => Some("DESC"),
            TK_DETACH => Some("DETACH"),
            TK_DISTINCT => Some("DISTINCT"),
            TK_DO => Some("DO"),
            TK_DROP => Some("DROP"),
            TK_EACH => Some("EACH"),
            TK_ELSE => Some("ELSE"),
            TK_END => Some("END"),
            TK_ESCAPE => Some("ESCAPE"),
            TK_EXCEPT => Some("EXCEPT"),
            TK_EXCLUDE => Some("EXCLUDE"),
            TK_EXCLUSIVE => Some("EXCLUSIVE"),
            TK_EXISTS => Some("EXISTS"),
            TK_EXPLAIN => Some("EXPLAIN"),
            TK_FAIL => Some("FAIL"),
            TK_FILTER => Some("FILTER"),
            TK_FIRST => Some("FIRST"),
            TK_FOLLOWING => Some("FOLLOWING"),
            TK_FOR => Some("FOR"),
            TK_FOREIGN => Some("FOREIGN"),
            TK_FROM => Some("FROM"),
            TK_GENERATED => Some("GENERATED"),
            TK_GROUP => Some("GROUP"),
            TK_GROUPS => Some("GROUPS"),
            TK_HAVING => Some("HAVING"),
            TK_IF => Some("IF"),
            TK_IGNORE => Some("IGNORE"),
            TK_IMMEDIATE => Some("IMMEDIATE"),
            TK_IN => Some("IN"),
            TK_INDEX => Some("INDEX"),
            TK_INDEXED => Some("INDEXED"),
            TK_INITIALLY => Some("INITIALLY"),
            TK_INSERT => Some("INSERT"),
            TK_INSTEAD => Some("INSTEAD"),
            TK_INTERSECT => Some("INTERSECT"),
            TK_INTO => Some("INTO"),
            TK_IS => Some("IS"),
            TK_ISNULL => Some("ISNULL"),
            TK_JOIN => Some("JOIN"),
            TK_KEY => Some("KEY"),
            TK_LAST => Some("LAST"),
            TK_LIMIT => Some("LIMIT"),
            TK_MATCH => Some("MATCH"),
            TK_MATERIALIZED => Some("MATERIALIZED"),
            TK_NO => Some("NO"),
            TK_NOT => Some("NOT"),
            TK_NOTHING => Some("NOTHING"),
            TK_NOTNULL => Some("NOTNULL"),
            TK_NULL => Some("NULL"),
            TK_NULLS => Some("NULLS"),
            TK_OF => Some("OF"),
            TK_OFFSET => Some("OFFSET"),
            TK_ON => Some("ON"),
            TK_OR => Some("OR"),
            TK_ORDER => Some("ORDER"),
            TK_OTHERS => Some("OTHERS"),
            TK_OVER => Some("OVER"),
            TK_PARTITION => Some("PARTITION"),
            TK_PLAN => Some("PLAN"),
            TK_PRAGMA => Some("PRAGMA"),
            TK_PRECEDING => Some("PRECEDING"),
            TK_PRIMARY => Some("PRIMARY"),
            TK_QUERY => Some("QUERY"),
            TK_RAISE => Some("RAISE"),
            TK_RANGE => Some("RANGE"),
            TK_RECURSIVE => Some("RECURSIVE"),
            TK_REFERENCES => Some("REFERENCES"),
            TK_REINDEX => Some("REINDEX"),
            TK_RELEASE => Some("RELEASE"),
            TK_RENAME => Some("RENAME"),
            TK_REPLACE => Some("REPLACE"),
            TK_RETURNING => Some("RETURNING"),
            TK_RESTRICT => Some("RESTRICT"),
            TK_ROLLBACK => Some("ROLLBACK"),
            TK_ROW => Some("ROW"),
            TK_ROWS => Some("ROWS"),
            TK_SAVEPOINT => Some("SAVEPOINT"),
            TK_SELECT => Some("SELECT"),
            TK_SET => Some("SET"),
            TK_TABLE => Some("TABLE"),
            TK_TEMP => Some("TEMP"), // or TEMPORARY
            TK_TIES => Some("TIES"),
            TK_THEN => Some("THEN"),
            TK_TO => Some("TO"),
            TK_TRANSACTION => Some("TRANSACTION"),
            TK_TRIGGER => Some("TRIGGER"),
            TK_UNBOUNDED => Some("UNBOUNDED"),
            TK_UNION => Some("UNION"),
            TK_UNIQUE => Some("UNIQUE"),
            TK_UPDATE => Some("UPDATE"),
            TK_USING => Some("USING"),
            TK_VACUUM => Some("VACUUM"),
            TK_VALUES => Some("VALUES"),
            TK_VIEW => Some("VIEW"),
            TK_VIRTUAL => Some("VIRTUAL"),
            TK_WHEN => Some("WHEN"),
            TK_WHERE => Some("WHERE"),
            TK_WINDOW => Some("WINDOW"),
            TK_WITH => Some("WITH"),
            #[cfg(feature = "SQLITE_ENABLE_ORDERED_SET_AGGREGATES")]
            TK_WITHIN => Some("WITHIN"),
            TK_WITHOUT => Some("WITHOUT"),
            TK_BITAND => Some("&"),
            TK_BITNOT => Some("~"),
            TK_BITOR => Some("|"),
            TK_COMMA => Some(","),
            TK_CONCAT => Some("||"),
            TK_DOT => Some("."),
            TK_EQ => Some("="), // or ==
            TK_GT => Some(">"),
            TK_GE => Some(">="),
            TK_LP => Some("("),
            TK_LSHIFT => Some("<<"),
            TK_LE => Some("<="),
            TK_LT => Some("<"),
            TK_MINUS => Some("-"),
            TK_NE => Some("<>"), // or !=
            TK_PLUS => Some("+"),
            TK_REM => Some("%"),
            TK_RP => Some(")"),
            TK_RSHIFT => Some(">>"),
            TK_SEMI => Some(";"),
            TK_SLASH => Some("/"),
            TK_STAR => Some("*"),
            _ => None,
        }
    }
}
