extern crate scan_rs as scan;

use scan::sql::TokenType;
use scan::sql::Tokenizer;
use scan::Scanner;
use std::env;
use std::fs::File;
use std::i64;
use std::str;

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
                    TokenType::Abort => debug_assert!(b"ABORT".eq_ignore_ascii_case(token)),
                    TokenType::Action => debug_assert!(b"ACTION".eq_ignore_ascii_case(token)),
                    TokenType::Add => debug_assert!(b"ADD".eq_ignore_ascii_case(token)),
                    TokenType::After => debug_assert!(b"AFTER".eq_ignore_ascii_case(token)),
                    TokenType::All => debug_assert!(b"ALL".eq_ignore_ascii_case(token)),
                    TokenType::Alter => debug_assert!(b"ALTER".eq_ignore_ascii_case(token)),
                    TokenType::Analyze => debug_assert!(b"ANALYZE".eq_ignore_ascii_case(token)),
                    TokenType::And => debug_assert!(b"AND".eq_ignore_ascii_case(token)),
                    TokenType::As => debug_assert!(b"AS".eq_ignore_ascii_case(token)),
                    TokenType::Asc => debug_assert!(b"ASC".eq_ignore_ascii_case(token)),
                    TokenType::Attach => debug_assert!(b"ATTACH".eq_ignore_ascii_case(token)),
                    TokenType::Autoincr => {
                        debug_assert!(b"AUTOINCREMENT".eq_ignore_ascii_case(token))
                    }
                    TokenType::Before => debug_assert!(b"BEFORE".eq_ignore_ascii_case(token)),
                    TokenType::Begin => debug_assert!(b"BEGIN".eq_ignore_ascii_case(token)),
                    TokenType::Between => debug_assert!(b"BETWEEN".eq_ignore_ascii_case(token)),
                    TokenType::By => debug_assert!(b"BY".eq_ignore_ascii_case(token)),
                    TokenType::Cascade => debug_assert!(b"CASCADE".eq_ignore_ascii_case(token)),
                    TokenType::Case => debug_assert!(b"CASE".eq_ignore_ascii_case(token)),
                    TokenType::Cast => debug_assert!(b"CAST".eq_ignore_ascii_case(token)),
                    TokenType::Check => debug_assert!(b"CHECK".eq_ignore_ascii_case(token)),
                    TokenType::Collate => debug_assert!(b"COLLATE".eq_ignore_ascii_case(token)),
                    TokenType::ColumnKw => debug_assert!(b"COLUMN".eq_ignore_ascii_case(token)),
                    TokenType::Commit => debug_assert!(b"COMMIT".eq_ignore_ascii_case(token)),
                    TokenType::Conflict => debug_assert!(b"CONFLICT".eq_ignore_ascii_case(token)),
                    TokenType::Constraint => {
                        debug_assert!(b"CONSTRAINT".eq_ignore_ascii_case(token))
                    }
                    TokenType::Create => debug_assert!(b"CREATE".eq_ignore_ascii_case(token)),
                    TokenType::Cross => debug_assert!(b"CROSS".eq_ignore_ascii_case(token)),
                    TokenType::Current => debug_assert!(b"CURRENT".eq_ignore_ascii_case(token)),
                    TokenType::CurrentDate => {
                        debug_assert!(b"CURRENT_DATE".eq_ignore_ascii_case(token))
                    }
                    TokenType::CurrentTime => {
                        debug_assert!(b"CURRENT_TIME".eq_ignore_ascii_case(token))
                    }
                    TokenType::CurrentTimestamp => {
                        debug_assert!(b"CURRENT_TIMESTAMP".eq_ignore_ascii_case(token))
                    }
                    TokenType::Database => debug_assert!(b"DATABASE".eq_ignore_ascii_case(token)),
                    TokenType::Default => debug_assert!(b"DEFAULT".eq_ignore_ascii_case(token)),
                    TokenType::Deferrable => {
                        debug_assert!(b"DEFERRABLE".eq_ignore_ascii_case(token))
                    }
                    TokenType::Deferred => debug_assert!(b"DEFERRED".eq_ignore_ascii_case(token)),
                    TokenType::Delete => debug_assert!(b"DELETE".eq_ignore_ascii_case(token)),
                    TokenType::Desc => debug_assert!(b"DESC".eq_ignore_ascii_case(token)),
                    TokenType::Detach => debug_assert!(b"DETACH".eq_ignore_ascii_case(token)),
                    TokenType::Distinct => debug_assert!(b"DISTINCT".eq_ignore_ascii_case(token)),
                    TokenType::Do => debug_assert!(b"DO".eq_ignore_ascii_case(token)),
                    TokenType::Drop => debug_assert!(b"DROP".eq_ignore_ascii_case(token)),
                    TokenType::Each => debug_assert!(b"EACH".eq_ignore_ascii_case(token)),
                    TokenType::Else => debug_assert!(b"ELSE".eq_ignore_ascii_case(token)),
                    TokenType::End => debug_assert!(b"END".eq_ignore_ascii_case(token)),
                    TokenType::Escape => debug_assert!(b"ESCAPE".eq_ignore_ascii_case(token)),
                    TokenType::Except => debug_assert!(b"EXCEPT".eq_ignore_ascii_case(token)),
                    TokenType::Exclusive => debug_assert!(b"EXCLUSIVE".eq_ignore_ascii_case(token)),
                    TokenType::Exists => debug_assert!(b"EXISTS".eq_ignore_ascii_case(token)),
                    TokenType::Explain => debug_assert!(b"EXPLAIN".eq_ignore_ascii_case(token)),
                    TokenType::Fail => debug_assert!(b"FAIL".eq_ignore_ascii_case(token)),
                    TokenType::Filter => debug_assert!(b"FILTER".eq_ignore_ascii_case(token)),
                    TokenType::Following => debug_assert!(b"FOLLOWING".eq_ignore_ascii_case(token)),
                    TokenType::For => debug_assert!(b"FOR".eq_ignore_ascii_case(token)),
                    TokenType::Foreign => debug_assert!(b"FOREIGN".eq_ignore_ascii_case(token)),
                    TokenType::From => debug_assert!(b"FROM".eq_ignore_ascii_case(token)),
                    TokenType::Full => debug_assert!(b"FULL".eq_ignore_ascii_case(token)),
                    TokenType::Glob => debug_assert!(b"GLOB".eq_ignore_ascii_case(token)),
                    TokenType::Group => debug_assert!(b"GROUP".eq_ignore_ascii_case(token)),
                    TokenType::Having => debug_assert!(b"HAVING".eq_ignore_ascii_case(token)),
                    TokenType::If => debug_assert!(b"IF".eq_ignore_ascii_case(token)),
                    TokenType::Ignore => debug_assert!(b"IGNORE".eq_ignore_ascii_case(token)),
                    TokenType::Immediate => debug_assert!(b"IMMEDIATE".eq_ignore_ascii_case(token)),
                    TokenType::In => debug_assert!(b"IN".eq_ignore_ascii_case(token)),
                    TokenType::Index => debug_assert!(b"INDEX".eq_ignore_ascii_case(token)),
                    TokenType::Indexed => debug_assert!(b"INDEXED".eq_ignore_ascii_case(token)),
                    TokenType::Initially => debug_assert!(b"INITIALLY".eq_ignore_ascii_case(token)),
                    TokenType::Inner => debug_assert!(b"INNER".eq_ignore_ascii_case(token)),
                    TokenType::Insert => debug_assert!(b"INSERT".eq_ignore_ascii_case(token)),
                    TokenType::Instead => debug_assert!(b"INSTEAD".eq_ignore_ascii_case(token)),
                    TokenType::Intersect => debug_assert!(b"INTERSECT".eq_ignore_ascii_case(token)),
                    TokenType::Into => debug_assert!(b"INTO".eq_ignore_ascii_case(token)),
                    TokenType::Is => debug_assert!(b"IS".eq_ignore_ascii_case(token)),
                    TokenType::IsNull => debug_assert!(b"ISNULL".eq_ignore_ascii_case(token)),
                    TokenType::Join => debug_assert!(b"JOIN".eq_ignore_ascii_case(token)),
                    TokenType::Key => debug_assert!(b"KEY".eq_ignore_ascii_case(token)),
                    TokenType::Left => debug_assert!(b"LEFT".eq_ignore_ascii_case(token)),
                    TokenType::Like => debug_assert!(b"LIKE".eq_ignore_ascii_case(token)),
                    TokenType::Limit => debug_assert!(b"LIMIT".eq_ignore_ascii_case(token)),
                    TokenType::Match => debug_assert!(b"MATCH".eq_ignore_ascii_case(token)),
                    TokenType::Natural => debug_assert!(b"NATURAL".eq_ignore_ascii_case(token)),
                    TokenType::No => debug_assert!(b"NO".eq_ignore_ascii_case(token)),
                    TokenType::Not => debug_assert!(b"NOT".eq_ignore_ascii_case(token)),
                    TokenType::Nothing => debug_assert!(b"NOTHING".eq_ignore_ascii_case(token)),
                    TokenType::NotNull => debug_assert!(b"NOTNULL".eq_ignore_ascii_case(token)),
                    TokenType::Null => debug_assert!(b"NULL".eq_ignore_ascii_case(token)),
                    TokenType::Of => debug_assert!(b"OF".eq_ignore_ascii_case(token)),
                    TokenType::Offset => debug_assert!(b"OFFSET".eq_ignore_ascii_case(token)),
                    TokenType::On => debug_assert!(b"ON".eq_ignore_ascii_case(token)),
                    TokenType::Or => debug_assert!(b"OR".eq_ignore_ascii_case(token)),
                    TokenType::Order => debug_assert!(b"ORDER".eq_ignore_ascii_case(token)),
                    TokenType::Outer => debug_assert!(b"OUTER".eq_ignore_ascii_case(token)),
                    TokenType::Over => debug_assert!(b"OVER".eq_ignore_ascii_case(token)),
                    TokenType::Partition => debug_assert!(b"PARTITION".eq_ignore_ascii_case(token)),
                    TokenType::Plan => debug_assert!(b"PLAN".eq_ignore_ascii_case(token)),
                    TokenType::Pragma => debug_assert!(b"PRAGMA".eq_ignore_ascii_case(token)),
                    TokenType::Preceding => debug_assert!(b"PRECEDING".eq_ignore_ascii_case(token)),
                    TokenType::Primary => debug_assert!(b"PRIMARY".eq_ignore_ascii_case(token)),
                    TokenType::Query => debug_assert!(b"QUERY".eq_ignore_ascii_case(token)),
                    TokenType::Raise => debug_assert!(b"RAISE".eq_ignore_ascii_case(token)),
                    TokenType::Range => debug_assert!(b"RANGE".eq_ignore_ascii_case(token)),
                    TokenType::Recursive => debug_assert!(b"RECURSIVE".eq_ignore_ascii_case(token)),
                    TokenType::References => {
                        debug_assert!(b"REFERENCES".eq_ignore_ascii_case(token))
                    }
                    TokenType::Regexp => debug_assert!(b"REGEXP".eq_ignore_ascii_case(token)),
                    TokenType::Reindex => debug_assert!(b"REINDEX".eq_ignore_ascii_case(token)),
                    TokenType::Release => debug_assert!(b"RELEASE".eq_ignore_ascii_case(token)),
                    TokenType::Rename => debug_assert!(b"RENAME".eq_ignore_ascii_case(token)),
                    TokenType::Replace => debug_assert!(b"REPLACE".eq_ignore_ascii_case(token)),
                    TokenType::Restrict => debug_assert!(b"RESTRICT".eq_ignore_ascii_case(token)),
                    TokenType::Right => debug_assert!(b"RIGHT".eq_ignore_ascii_case(token)),
                    TokenType::Rollback => debug_assert!(b"ROLLBACK".eq_ignore_ascii_case(token)),
                    TokenType::Row => debug_assert!(b"ROW".eq_ignore_ascii_case(token)),
                    TokenType::Rows => debug_assert!(b"ROWS".eq_ignore_ascii_case(token)),
                    TokenType::Savepoint => debug_assert!(b"SAVEPOINT".eq_ignore_ascii_case(token)),
                    TokenType::Select => debug_assert!(b"SELECT".eq_ignore_ascii_case(token)),
                    TokenType::Set => debug_assert!(b"SET".eq_ignore_ascii_case(token)),
                    TokenType::Table => debug_assert!(b"TABLE".eq_ignore_ascii_case(token)),
                    //TokenType::Temp => debug_assert!(b"TEMP".eq_ignore_ascii_case(token)),
                    TokenType::Temp => debug_assert!(
                        b"TEMP".eq_ignore_ascii_case(token)
                            || b"TEMPORARY".eq_ignore_ascii_case(token)
                    ),
                    TokenType::Then => debug_assert!(b"THEN".eq_ignore_ascii_case(token)),
                    TokenType::To => debug_assert!(b"TO".eq_ignore_ascii_case(token)),
                    TokenType::Transaction => {
                        debug_assert!(b"TRANSACTION".eq_ignore_ascii_case(token))
                    }
                    TokenType::Trigger => debug_assert!(b"TRIGGER".eq_ignore_ascii_case(token)),
                    TokenType::Unbounded => debug_assert!(b"UNBOUNDED".eq_ignore_ascii_case(token)),
                    TokenType::Union => debug_assert!(b"UNION".eq_ignore_ascii_case(token)),
                    TokenType::Unique => debug_assert!(b"UNIQUE".eq_ignore_ascii_case(token)),
                    TokenType::Update => debug_assert!(b"UPDATE".eq_ignore_ascii_case(token)),
                    TokenType::Using => debug_assert!(b"USING".eq_ignore_ascii_case(token)),
                    TokenType::Vacuum => debug_assert!(b"VACUUM".eq_ignore_ascii_case(token)),
                    TokenType::Values => debug_assert!(b"VALUES".eq_ignore_ascii_case(token)),
                    TokenType::View => debug_assert!(b"VIEW".eq_ignore_ascii_case(token)),
                    TokenType::Virtual => debug_assert!(b"VIRTUAL".eq_ignore_ascii_case(token)),
                    TokenType::When => debug_assert!(b"WHEN".eq_ignore_ascii_case(token)),
                    TokenType::Where => debug_assert!(b"WHERE".eq_ignore_ascii_case(token)),
                    TokenType::Window => debug_assert!(b"WINDOW".eq_ignore_ascii_case(token)),
                    TokenType::With => debug_assert!(b"WITH".eq_ignore_ascii_case(token)),
                    TokenType::Without => debug_assert!(b"WITHOUT".eq_ignore_ascii_case(token)),
                    TokenType::BitAnd => debug_assert_eq!(b"&", token),
                    TokenType::BitNot => debug_assert_eq!(b"~", token),
                    TokenType::BitOr => debug_assert_eq!(b"|", token),
                    TokenType::Comma => debug_assert_eq!(b",", token),
                    TokenType::Concat => debug_assert_eq!(b"||", token),
                    TokenType::Dot => debug_assert_eq!(b".", token),
                    TokenType::Equals => debug_assert!(b"=" == token || b"==" == token),
                    TokenType::GreaterThan => debug_assert_eq!(b">", token),
                    TokenType::GreaterEquals => debug_assert_eq!(b">=", token),
                    TokenType::LeftParen => debug_assert_eq!(b"(", token),
                    TokenType::LeftShift => debug_assert_eq!(b"<<", token),
                    TokenType::LessEquals => debug_assert_eq!(b"<=", token),
                    TokenType::LessThan => debug_assert_eq!(b"<", token),
                    TokenType::Minus => debug_assert_eq!(b"-", token),
                    TokenType::NotEquals => debug_assert!(b"<>" == token || b"!=" == token),
                    TokenType::Plus => debug_assert_eq!(b"+", token),
                    TokenType::Reminder => debug_assert_eq!(b"%", token),
                    TokenType::RightParen => debug_assert_eq!(b")", token),
                    TokenType::RightShift => debug_assert_eq!(b">>", token),
                    TokenType::Semi => debug_assert_eq!(b";", token),
                    TokenType::Slash => debug_assert_eq!(b"/", token),
                    TokenType::Star => debug_assert_eq!(b"*", token),
                    //TokenType::StringLiteral => debug_assert!(),
                    //TokenType::Id => debug_assert!(),
                    //TokenType::Variable => debug_assert!(),
                    TokenType::Blob => debug_assert!(
                        token.len() % 2 == 0 && token.iter().all(|b| b.is_ascii_hexdigit())
                    ),
                    TokenType::Integer => if token.len() > 2
                        && token[0] == b'0'
                        && (token[1] == b'x' || token[1] == b'X')
                    {
                        if let Err(err) = i64::from_str_radix(str::from_utf8(&token[2..]).unwrap(), 16) {
                            eprintln!("Err: {} in {}", err, arg);
                        }
                    } else {
                        /*let raw = str::from_utf8(token).unwrap();
                        let res = raw.parse::<i64>();
                        if res.is_err() {
                            eprintln!("Err: {} in {}", res.unwrap_err(), arg);
                        }*/
                        debug_assert!(token.iter().all(|b| b.is_ascii_digit()))
                    },
                    TokenType::Float => {
                        debug_assert!(str::from_utf8(token).unwrap().parse::<f64>().is_ok())
                    }
                    _ => {
                        println!("'{}', {:?}", str::from_utf8(token).unwrap(), token_type);
                    }
                },
            }
        }
    }
}
