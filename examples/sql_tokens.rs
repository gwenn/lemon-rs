use sqlite_parser::lexer::sql::{TokenType, Tokenizer};
use sqlite_parser::lexer::Scanner;

use std::env;
use std::fs::File;
use std::i64;
use std::str;

/// Tokenize specified files (and do some checks)
fn main() {
    let args = env::args();
    for arg in args.skip(1) {
        let f = File::open(arg.clone()).unwrap();
        let tokenizer = Tokenizer::new();
        let mut s = Scanner::new(f, tokenizer);
        loop {
            match s.scan() {
                Ok(None) => break,
                Err(err) => {
                    //eprintln!("{} at line: {}, column: {}", err, s.line(), s.column());
                    eprintln!("Err: {} in {}", err, arg);
                    break;
                }
                Ok(Some((token, token_type))) => match token_type {
                    TokenType::TK_ABORT => debug_assert!(b"ABORT".eq_ignore_ascii_case(token)),
                    TokenType::TK_ACTION => debug_assert!(b"ACTION".eq_ignore_ascii_case(token)),
                    TokenType::TK_ADD => debug_assert!(b"ADD".eq_ignore_ascii_case(token)),
                    TokenType::TK_AFTER => debug_assert!(b"AFTER".eq_ignore_ascii_case(token)),
                    TokenType::TK_ALL => debug_assert!(b"ALL".eq_ignore_ascii_case(token)),
                    TokenType::TK_ALTER => debug_assert!(b"ALTER".eq_ignore_ascii_case(token)),
                    TokenType::TK_ANALYZE => debug_assert!(b"ANALYZE".eq_ignore_ascii_case(token)),
                    TokenType::TK_ALWAYS => debug_assert!(b"ALWAYS".eq_ignore_ascii_case(token)),
                    TokenType::TK_AND => debug_assert!(b"AND".eq_ignore_ascii_case(token)),
                    TokenType::TK_ANY => debug_assert!(b"ANY".eq_ignore_ascii_case(token)),
                    TokenType::TK_AS => debug_assert!(b"AS".eq_ignore_ascii_case(token)),
                    TokenType::TK_ASC => debug_assert!(b"ASC".eq_ignore_ascii_case(token)),
                    TokenType::TK_ATTACH => debug_assert!(b"ATTACH".eq_ignore_ascii_case(token)),
                    TokenType::TK_AUTOINCR => {
                        debug_assert!(b"AUTOINCREMENT".eq_ignore_ascii_case(token))
                    }
                    TokenType::TK_BEFORE => debug_assert!(b"BEFORE".eq_ignore_ascii_case(token)),
                    TokenType::TK_BEGIN => debug_assert!(b"BEGIN".eq_ignore_ascii_case(token)),
                    TokenType::TK_BETWEEN => debug_assert!(b"BETWEEN".eq_ignore_ascii_case(token)),
                    TokenType::TK_BY => debug_assert!(b"BY".eq_ignore_ascii_case(token)),
                    TokenType::TK_CASCADE => debug_assert!(b"CASCADE".eq_ignore_ascii_case(token)),
                    TokenType::TK_CASE => debug_assert!(b"CASE".eq_ignore_ascii_case(token)),
                    TokenType::TK_CAST => debug_assert!(b"CAST".eq_ignore_ascii_case(token)),
                    TokenType::TK_CHECK => debug_assert!(b"CHECK".eq_ignore_ascii_case(token)),
                    TokenType::TK_COLLATE => debug_assert!(b"COLLATE".eq_ignore_ascii_case(token)),
                    TokenType::TK_COLUMNKW => debug_assert!(b"COLUMN".eq_ignore_ascii_case(token)),
                    TokenType::TK_COMMIT => debug_assert!(b"COMMIT".eq_ignore_ascii_case(token)),
                    TokenType::TK_CONFLICT => {
                        debug_assert!(b"CONFLICT".eq_ignore_ascii_case(token))
                    }
                    TokenType::TK_CONSTRAINT => {
                        debug_assert!(b"CONSTRAINT".eq_ignore_ascii_case(token))
                    }
                    TokenType::TK_CREATE => debug_assert!(b"CREATE".eq_ignore_ascii_case(token)),
                    TokenType::TK_CURRENT => debug_assert!(b"CURRENT".eq_ignore_ascii_case(token)),
                    TokenType::TK_DATABASE => {
                        debug_assert!(b"DATABASE".eq_ignore_ascii_case(token))
                    }
                    TokenType::TK_DEFAULT => debug_assert!(b"DEFAULT".eq_ignore_ascii_case(token)),
                    TokenType::TK_DEFERRABLE => {
                        debug_assert!(b"DEFERRABLE".eq_ignore_ascii_case(token))
                    }
                    TokenType::TK_DEFERRED => {
                        debug_assert!(b"DEFERRED".eq_ignore_ascii_case(token))
                    }
                    TokenType::TK_DELETE => debug_assert!(b"DELETE".eq_ignore_ascii_case(token)),
                    TokenType::TK_DESC => debug_assert!(b"DESC".eq_ignore_ascii_case(token)),
                    TokenType::TK_DETACH => debug_assert!(b"DETACH".eq_ignore_ascii_case(token)),
                    TokenType::TK_DISTINCT => {
                        debug_assert!(b"DISTINCT".eq_ignore_ascii_case(token))
                    }
                    TokenType::TK_DO => debug_assert!(b"DO".eq_ignore_ascii_case(token)),
                    TokenType::TK_DROP => debug_assert!(b"DROP".eq_ignore_ascii_case(token)),
                    TokenType::TK_EACH => debug_assert!(b"EACH".eq_ignore_ascii_case(token)),
                    TokenType::TK_ELSE => debug_assert!(b"ELSE".eq_ignore_ascii_case(token)),
                    TokenType::TK_END => debug_assert!(b"END".eq_ignore_ascii_case(token)),
                    TokenType::TK_ESCAPE => debug_assert!(b"ESCAPE".eq_ignore_ascii_case(token)),
                    TokenType::TK_EXCEPT => debug_assert!(b"EXCEPT".eq_ignore_ascii_case(token)),
                    TokenType::TK_EXCLUDE => debug_assert!(b"EXCLUDE".eq_ignore_ascii_case(token)),
                    TokenType::TK_EXCLUSIVE => {
                        debug_assert!(b"EXCLUSIVE".eq_ignore_ascii_case(token))
                    }
                    TokenType::TK_EXISTS => debug_assert!(b"EXISTS".eq_ignore_ascii_case(token)),
                    TokenType::TK_EXPLAIN => debug_assert!(b"EXPLAIN".eq_ignore_ascii_case(token)),
                    TokenType::TK_FAIL => debug_assert!(b"FAIL".eq_ignore_ascii_case(token)),
                    TokenType::TK_FILTER => debug_assert!(b"FILTER".eq_ignore_ascii_case(token)),
                    TokenType::TK_FIRST => debug_assert!(b"FIRST".eq_ignore_ascii_case(token)),
                    TokenType::TK_FOLLOWING => {
                        debug_assert!(b"FOLLOWING".eq_ignore_ascii_case(token))
                    }
                    TokenType::TK_FOR => debug_assert!(b"FOR".eq_ignore_ascii_case(token)),
                    TokenType::TK_FOREIGN => debug_assert!(b"FOREIGN".eq_ignore_ascii_case(token)),
                    TokenType::TK_FROM => debug_assert!(b"FROM".eq_ignore_ascii_case(token)),
                    TokenType::TK_GENERATED => {
                        debug_assert!(b"GENERATED".eq_ignore_ascii_case(token))
                    }
                    TokenType::TK_GROUP => debug_assert!(b"GROUP".eq_ignore_ascii_case(token)),
                    TokenType::TK_GROUPS => debug_assert!(b"GROUPS".eq_ignore_ascii_case(token)),
                    TokenType::TK_HAVING => debug_assert!(b"HAVING".eq_ignore_ascii_case(token)),
                    TokenType::TK_IF => debug_assert!(b"IF".eq_ignore_ascii_case(token)),
                    TokenType::TK_IGNORE => debug_assert!(b"IGNORE".eq_ignore_ascii_case(token)),
                    TokenType::TK_IMMEDIATE => {
                        debug_assert!(b"IMMEDIATE".eq_ignore_ascii_case(token))
                    }
                    TokenType::TK_IN => debug_assert!(b"IN".eq_ignore_ascii_case(token)),
                    TokenType::TK_INDEX => debug_assert!(b"INDEX".eq_ignore_ascii_case(token)),
                    TokenType::TK_INDEXED => debug_assert!(b"INDEXED".eq_ignore_ascii_case(token)),
                    TokenType::TK_INITIALLY => {
                        debug_assert!(b"INITIALLY".eq_ignore_ascii_case(token))
                    }
                    TokenType::TK_INSERT => debug_assert!(b"INSERT".eq_ignore_ascii_case(token)),
                    TokenType::TK_INSTEAD => debug_assert!(b"INSTEAD".eq_ignore_ascii_case(token)),
                    TokenType::TK_INTERSECT => {
                        debug_assert!(b"INTERSECT".eq_ignore_ascii_case(token))
                    }
                    TokenType::TK_INTO => debug_assert!(b"INTO".eq_ignore_ascii_case(token)),
                    TokenType::TK_IS => debug_assert!(b"IS".eq_ignore_ascii_case(token)),
                    TokenType::TK_ISNULL => debug_assert!(b"ISNULL".eq_ignore_ascii_case(token)),
                    TokenType::TK_JOIN => debug_assert!(b"JOIN".eq_ignore_ascii_case(token)),
                    TokenType::TK_KEY => debug_assert!(b"KEY".eq_ignore_ascii_case(token)),
                    TokenType::TK_LAST => debug_assert!(b"LAST".eq_ignore_ascii_case(token)),
                    TokenType::TK_LIMIT => debug_assert!(b"LIMIT".eq_ignore_ascii_case(token)),
                    TokenType::TK_MATCH => debug_assert!(b"MATCH".eq_ignore_ascii_case(token)),
                    TokenType::TK_NO => debug_assert!(b"NO".eq_ignore_ascii_case(token)),
                    TokenType::TK_NOT => debug_assert!(b"NOT".eq_ignore_ascii_case(token)),
                    TokenType::TK_NOTHING => debug_assert!(b"NOTHING".eq_ignore_ascii_case(token)),
                    TokenType::TK_NOTNULL => debug_assert!(b"NOTNULL".eq_ignore_ascii_case(token)),
                    TokenType::TK_NULL => debug_assert!(b"NULL".eq_ignore_ascii_case(token)),
                    TokenType::TK_NULLS => debug_assert!(b"NULLS".eq_ignore_ascii_case(token)),
                    TokenType::TK_OF => debug_assert!(b"OF".eq_ignore_ascii_case(token)),
                    TokenType::TK_OFFSET => debug_assert!(b"OFFSET".eq_ignore_ascii_case(token)),
                    TokenType::TK_ON => debug_assert!(b"ON".eq_ignore_ascii_case(token)),
                    TokenType::TK_OR => debug_assert!(b"OR".eq_ignore_ascii_case(token)),
                    TokenType::TK_ORDER => debug_assert!(b"ORDER".eq_ignore_ascii_case(token)),
                    TokenType::TK_OTHERS => debug_assert!(b"OTHERS".eq_ignore_ascii_case(token)),
                    TokenType::TK_OVER => debug_assert!(b"OVER".eq_ignore_ascii_case(token)),
                    TokenType::TK_PARTITION => {
                        debug_assert!(b"PARTITION".eq_ignore_ascii_case(token))
                    }
                    TokenType::TK_PLAN => debug_assert!(b"PLAN".eq_ignore_ascii_case(token)),
                    TokenType::TK_PRAGMA => debug_assert!(b"PRAGMA".eq_ignore_ascii_case(token)),
                    TokenType::TK_PRECEDING => {
                        debug_assert!(b"PRECEDING".eq_ignore_ascii_case(token))
                    }
                    TokenType::TK_PRIMARY => debug_assert!(b"PRIMARY".eq_ignore_ascii_case(token)),
                    TokenType::TK_QUERY => debug_assert!(b"QUERY".eq_ignore_ascii_case(token)),
                    TokenType::TK_RAISE => debug_assert!(b"RAISE".eq_ignore_ascii_case(token)),
                    TokenType::TK_RANGE => debug_assert!(b"RANGE".eq_ignore_ascii_case(token)),
                    TokenType::TK_RECURSIVE => {
                        debug_assert!(b"RECURSIVE".eq_ignore_ascii_case(token))
                    }
                    TokenType::TK_REFERENCES => {
                        debug_assert!(b"REFERENCES".eq_ignore_ascii_case(token))
                    }
                    TokenType::TK_REINDEX => debug_assert!(b"REINDEX".eq_ignore_ascii_case(token)),
                    TokenType::TK_RELEASE => debug_assert!(b"RELEASE".eq_ignore_ascii_case(token)),
                    TokenType::TK_RENAME => debug_assert!(b"RENAME".eq_ignore_ascii_case(token)),
                    TokenType::TK_REPLACE => debug_assert!(b"REPLACE".eq_ignore_ascii_case(token)),
                    TokenType::TK_RESTRICT => {
                        debug_assert!(b"RESTRICT".eq_ignore_ascii_case(token))
                    }
                    TokenType::TK_ROLLBACK => {
                        debug_assert!(b"ROLLBACK".eq_ignore_ascii_case(token))
                    }
                    TokenType::TK_ROW => debug_assert!(b"ROW".eq_ignore_ascii_case(token)),
                    TokenType::TK_ROWS => debug_assert!(b"ROWS".eq_ignore_ascii_case(token)),
                    TokenType::TK_SAVEPOINT => {
                        debug_assert!(b"SAVEPOINT".eq_ignore_ascii_case(token))
                    }
                    TokenType::TK_SELECT => debug_assert!(b"SELECT".eq_ignore_ascii_case(token)),
                    TokenType::TK_SET => debug_assert!(b"SET".eq_ignore_ascii_case(token)),
                    TokenType::TK_TABLE => debug_assert!(b"TABLE".eq_ignore_ascii_case(token)),
                    TokenType::TK_TEMP => debug_assert!(
                        b"TEMP".eq_ignore_ascii_case(token)
                            || b"TEMPORARY".eq_ignore_ascii_case(token)
                    ),
                    TokenType::TK_TIES => debug_assert!(b"TIES".eq_ignore_ascii_case(token)),
                    TokenType::TK_THEN => debug_assert!(b"THEN".eq_ignore_ascii_case(token)),
                    TokenType::TK_TO => debug_assert!(b"TO".eq_ignore_ascii_case(token)),
                    TokenType::TK_TRANSACTION => {
                        debug_assert!(b"TRANSACTION".eq_ignore_ascii_case(token))
                    }
                    TokenType::TK_TRIGGER => debug_assert!(b"TRIGGER".eq_ignore_ascii_case(token)),
                    TokenType::TK_UNBOUNDED => {
                        debug_assert!(b"UNBOUNDED".eq_ignore_ascii_case(token))
                    }
                    TokenType::TK_UNION => debug_assert!(b"UNION".eq_ignore_ascii_case(token)),
                    TokenType::TK_UNIQUE => debug_assert!(b"UNIQUE".eq_ignore_ascii_case(token)),
                    TokenType::TK_UPDATE => debug_assert!(b"UPDATE".eq_ignore_ascii_case(token)),
                    TokenType::TK_USING => debug_assert!(b"USING".eq_ignore_ascii_case(token)),
                    TokenType::TK_VACUUM => debug_assert!(b"VACUUM".eq_ignore_ascii_case(token)),
                    TokenType::TK_VALUES => debug_assert!(b"VALUES".eq_ignore_ascii_case(token)),
                    TokenType::TK_VIEW => debug_assert!(b"VIEW".eq_ignore_ascii_case(token)),
                    TokenType::TK_VIRTUAL => debug_assert!(b"VIRTUAL".eq_ignore_ascii_case(token)),
                    TokenType::TK_WHEN => debug_assert!(b"WHEN".eq_ignore_ascii_case(token)),
                    TokenType::TK_WHERE => debug_assert!(b"WHERE".eq_ignore_ascii_case(token)),
                    TokenType::TK_WINDOW => debug_assert!(b"WINDOW".eq_ignore_ascii_case(token)),
                    TokenType::TK_WITH => debug_assert!(b"WITH".eq_ignore_ascii_case(token)),
                    TokenType::TK_WITHOUT => debug_assert!(b"WITHOUT".eq_ignore_ascii_case(token)),
                    TokenType::TK_BITAND => debug_assert_eq!(b"&", token),
                    TokenType::TK_BITNOT => debug_assert_eq!(b"~", token),
                    TokenType::TK_BITOR => debug_assert_eq!(b"|", token),
                    TokenType::TK_COMMA => debug_assert_eq!(b",", token),
                    TokenType::TK_CONCAT => debug_assert_eq!(b"||", token),
                    TokenType::TK_DOT => debug_assert_eq!(b".", token),
                    TokenType::TK_EQ => debug_assert!(b"=" == token || b"==" == token),
                    TokenType::TK_GT => debug_assert_eq!(b">", token),
                    TokenType::TK_GE => debug_assert_eq!(b">=", token),
                    TokenType::TK_LP => debug_assert_eq!(b"(", token),
                    TokenType::TK_LSHIFT => debug_assert_eq!(b"<<", token),
                    TokenType::TK_LE => debug_assert_eq!(b"<=", token),
                    TokenType::TK_LT => debug_assert_eq!(b"<", token),
                    TokenType::TK_MINUS => debug_assert_eq!(b"-", token),
                    TokenType::TK_NE => debug_assert!(b"<>" == token || b"!=" == token),
                    TokenType::TK_PLUS => debug_assert_eq!(b"+", token),
                    TokenType::TK_REM => debug_assert_eq!(b"%", token),
                    TokenType::TK_RP => debug_assert_eq!(b")", token),
                    TokenType::TK_RSHIFT => debug_assert_eq!(b">>", token),
                    TokenType::TK_SEMI => debug_assert_eq!(b";", token),
                    TokenType::TK_SLASH => debug_assert_eq!(b"/", token),
                    TokenType::TK_STAR => debug_assert_eq!(b"*", token),
                    //TokenType::TK_STRING => debug_assert!(),
                    //TokenType::TK_ID => debug_assert!(),
                    //TokenType::TK_VARIABLE => debug_assert!(),
                    TokenType::TK_BLOB => debug_assert!(
                        token.len() % 2 == 0 && token.iter().all(|b| b.is_ascii_hexdigit())
                    ),
                    TokenType::TK_INTEGER => {
                        if token.len() > 2
                            && token[0] == b'0'
                            && (token[1] == b'x' || token[1] == b'X')
                        {
                            if let Err(err) =
                                i64::from_str_radix(str::from_utf8(&token[2..]).unwrap(), 16)
                            {
                                eprintln!("Err: {} in {}", err, arg);
                            }
                        } else {
                            /*let raw = str::from_utf8(token).unwrap();
                            let res = raw.parse::<i64>();
                            if res.is_err() {
                                eprintln!("Err: {} in {}", res.unwrap_err(), arg);
                            }*/
                            debug_assert!(token.iter().all(|b| b.is_ascii_digit()))
                        }
                    }
                    TokenType::TK_FLOAT => {
                        debug_assert!(str::from_utf8(token).unwrap().parse::<f64>().is_ok())
                    }
                    TokenType::TK_CTIME_KW => debug_assert!(
                        b"CURRENT_DATE".eq_ignore_ascii_case(token)
                            || b"CURRENT_TIME".eq_ignore_ascii_case(token)
                            || b"CURRENT_TIMESTAMP".eq_ignore_ascii_case(token)
                    ),
                    TokenType::TK_JOIN_KW => debug_assert!(
                        b"CROSS".eq_ignore_ascii_case(token)
                            || b"FULL".eq_ignore_ascii_case(token)
                            || b"INNER".eq_ignore_ascii_case(token)
                            || b"LEFT".eq_ignore_ascii_case(token)
                            || b"NATURAL".eq_ignore_ascii_case(token)
                            || b"OUTER".eq_ignore_ascii_case(token)
                            || b"RIGHT".eq_ignore_ascii_case(token)
                    ),
                    TokenType::TK_LIKE_KW => debug_assert!(
                        b"GLOB".eq_ignore_ascii_case(token)
                            || b"LIKE".eq_ignore_ascii_case(token)
                            || b"REGEXP".eq_ignore_ascii_case(token)
                    ),
                    _ => {
                        println!("'{}', {:?}", str::from_utf8(token).unwrap(), token_type);
                    }
                },
            }
        }
    }
}
