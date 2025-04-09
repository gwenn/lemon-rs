//! All terminal symbols.

/// Token classes
// Generated by lemon (parse.h).
// Renamed manually.
// To be keep in sync.
#[non_exhaustive]
#[allow(non_camel_case_types, missing_docs)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd)]
#[repr(u16)]
pub enum TokenType {
    TK_EOF = 0,
    TK_SEMI = 1,
    TK_EXPLAIN = 2,
    TK_QUERY = 3,
    TK_PLAN = 4,
    TK_BEGIN = 5,
    TK_TRANSACTION = 6,
    TK_DEFERRED = 7,
    TK_IMMEDIATE = 8,
    TK_EXCLUSIVE = 9,
    TK_COMMIT = 10,
    TK_END = 11,
    TK_ROLLBACK = 12,
    TK_SAVEPOINT = 13,
    TK_RELEASE = 14,
    TK_TO = 15,
    TK_TABLE = 16,
    TK_CREATE = 17,
    TK_IF = 18,
    TK_NOT = 19,
    TK_EXISTS = 20,
    TK_TEMP = 21,
    TK_LP = 22,
    TK_RP = 23,
    TK_AS = 24,
    TK_COMMA = 25,
    TK_WITHOUT = 26,
    TK_ABORT = 27,
    TK_ACTION = 28,
    TK_AFTER = 29,
    TK_ANALYZE = 30,
    TK_ASC = 31,
    TK_ATTACH = 32,
    TK_BEFORE = 33,
    TK_BY = 34,
    TK_CASCADE = 35,
    TK_CAST = 36,
    TK_CONFLICT = 37,
    TK_DATABASE = 38,
    TK_DESC = 39,
    TK_DETACH = 40,
    TK_EACH = 41,
    TK_FAIL = 42,
    TK_OR = 43,
    TK_AND = 44,
    TK_IS = 45,
    TK_ISNOT = 46,
    TK_MATCH = 47,
    TK_LIKE_KW = 48,
    TK_BETWEEN = 49,
    TK_IN = 50,
    TK_ISNULL = 51,
    TK_NOTNULL = 52,
    TK_NE = 53,
    TK_EQ = 54,
    TK_GT = 55,
    TK_LE = 56,
    TK_LT = 57,
    TK_GE = 58,
    TK_ESCAPE = 59,
    TK_ID = 60,
    TK_COLUMNKW = 61,
    TK_DO = 62,
    TK_FOR = 63,
    TK_IGNORE = 64,
    TK_INITIALLY = 65,
    TK_INSTEAD = 66,
    TK_NO = 67,
    TK_KEY = 68,
    TK_OF = 69,
    TK_OFFSET = 70,
    TK_PRAGMA = 71,
    TK_RAISE = 72,
    TK_RECURSIVE = 73,
    TK_REPLACE = 74,
    TK_RESTRICT = 75,
    TK_ROW = 76,
    TK_ROWS = 77,
    TK_TRIGGER = 78,
    TK_VACUUM = 79,
    TK_VIEW = 80,
    TK_VIRTUAL = 81,
    TK_WITH = 82,
    TK_NULLS = 83,
    TK_FIRST = 84,
    TK_LAST = 85,
    TK_CURRENT = 86,
    TK_FOLLOWING = 87,
    TK_PARTITION = 88,
    TK_PRECEDING = 89,
    TK_RANGE = 90,
    TK_UNBOUNDED = 91,
    TK_EXCLUDE = 92,
    TK_GROUPS = 93,
    TK_OTHERS = 94,
    TK_TIES = 95,
    TK_GENERATED = 96,
    TK_ALWAYS = 97,
    TK_MATERIALIZED = 98,
    TK_REINDEX = 99,
    TK_RENAME = 100,
    TK_CTIME_KW = 101,
    TK_ANY = 102,
    TK_BITAND = 103,
    TK_BITOR = 104,
    TK_LSHIFT = 105,
    TK_RSHIFT = 106,
    TK_PLUS = 107,
    TK_MINUS = 108,
    TK_STAR = 109,
    TK_SLASH = 110,
    TK_REM = 111,
    TK_CONCAT = 112,
    TK_PTR = 113,
    TK_COLLATE = 114,
    TK_BITNOT = 115,
    TK_ON = 116,
    TK_INDEXED = 117,
    TK_STRING = 118,
    TK_JOIN_KW = 119,
    TK_CONSTRAINT = 120,
    TK_DEFAULT = 121,
    TK_NULL = 122,
    TK_PRIMARY = 123,
    TK_UNIQUE = 124,
    TK_CHECK = 125,
    TK_REFERENCES = 126,
    TK_AUTOINCR = 127,
    TK_INSERT = 128,
    TK_DELETE = 129,
    TK_UPDATE = 130,
    TK_SET = 131,
    TK_DEFERRABLE = 132,
    TK_FOREIGN = 133,
    TK_DROP = 134,
    TK_UNION = 135,
    TK_ALL = 136,
    TK_EXCEPT = 137,
    TK_INTERSECT = 138,
    TK_SELECT = 139,
    TK_VALUES = 140,
    TK_DISTINCT = 141,
    TK_DOT = 142,
    TK_FROM = 143,
    TK_JOIN = 144,
    TK_USING = 145,
    TK_ORDER = 146,
    TK_GROUP = 147,
    TK_HAVING = 148,
    TK_LIMIT = 149,
    TK_WHERE = 150,
    TK_RETURNING = 151,
    TK_INTO = 152,
    TK_NOTHING = 153,
    TK_BLOB = 154,
    TK_FLOAT = 155,
    TK_INTEGER = 156,
    TK_VARIABLE = 157,
    TK_CASE = 158,
    TK_WHEN = 159,
    TK_THEN = 160,
    TK_ELSE = 161,
    TK_INDEX = 162,
    TK_ALTER = 163,
    TK_ADD = 164,
    TK_WINDOW = 165,
    TK_OVER = 166,
    TK_FILTER = 167,
    #[cfg(feature = "SQLITE_ENABLE_ORDERED_SET_AGGREGATES")]
    TK_WITHIN = 168,
}
