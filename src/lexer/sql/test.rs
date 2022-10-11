use fallible_iterator::FallibleIterator;

use super::Parser;
use crate::parser::ast::{ParameterInfo, ToTokens};

#[test]
fn count_placeholders() {
    let sql = "SELECT ? WHERE 1 = ?";
    let mut parser = Parser::new(sql.as_bytes());
    let ast = parser.next().unwrap().unwrap();
    let mut info = ParameterInfo::default();
    ast.to_tokens(&mut info).unwrap();
    assert_eq!(info.count, 2);
}

#[test]
fn count_numbered_placeholders() {
    let sql = "SELECT ?1 WHERE 1 = ?2 AND 0 = ?1";
    let mut parser = Parser::new(sql.as_bytes());
    let ast = parser.next().unwrap().unwrap();
    let mut info = ParameterInfo::default();
    ast.to_tokens(&mut info).unwrap();
    assert_eq!(info.count, 2);
}

#[test]
fn count_unused_placeholders() {
    let sql = "SELECT ?1 WHERE 1 = ?3";
    let mut parser = Parser::new(sql.as_bytes());
    let ast = parser.next().unwrap().unwrap();
    let mut info = ParameterInfo::default();
    ast.to_tokens(&mut info).unwrap();
    assert_eq!(info.count, 3);
}

#[test]
fn count_named_placeholders() {
    let sql = "SELECT :x, :y WHERE 1 = :y";
    let mut parser = Parser::new(sql.as_bytes());
    let ast = parser.next().unwrap().unwrap();
    let mut info = ParameterInfo::default();
    ast.to_tokens(&mut info).unwrap();
    assert_eq!(info.count, 2);
    assert_eq!(info.names.len(), 2);
    assert!(info.names.contains(":x"));
    assert!(info.names.contains(":y"));
}
