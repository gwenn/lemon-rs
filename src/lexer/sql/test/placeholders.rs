use bumpalo::Bump;

use super::parse_cmd;
use crate::ast::ParameterInfo;
use crate::ast::fmt::ToTokens as _;

#[test]
fn count_placeholders() {
    let b = Bump::new();
    let ast = parse_cmd(b"SELECT ? WHERE 1 = ?", &b);
    let mut info = ParameterInfo::default();
    ast.to_tokens(&mut info).unwrap();
    assert_eq!(info.count, 2);
}

#[test]
fn count_numbered_placeholders() {
    let b = Bump::new();
    let ast = parse_cmd(b"SELECT ?1 WHERE 1 = ?2 AND 0 = ?1", &b);
    let mut info = ParameterInfo::default();
    ast.to_tokens(&mut info).unwrap();
    assert_eq!(info.count, 2);
}

#[test]
fn count_unused_placeholders() {
    let b = Bump::new();
    let ast = parse_cmd(b"SELECT ?1 WHERE 1 = ?3", &b);
    let mut info = ParameterInfo::default();
    ast.to_tokens(&mut info).unwrap();
    assert_eq!(info.count, 3);
}

#[test]
fn count_named_placeholders() {
    let b = Bump::new();
    let ast = parse_cmd(b"SELECT :x, :y WHERE 1 = :y", &b);
    let mut info = ParameterInfo::default();
    ast.to_tokens(&mut info).unwrap();
    assert_eq!(info.count, 2);
    assert_eq!(info.names.len(), 2);
    assert!(info.names.contains(":x"));
    assert!(info.names.contains(":y"));
}
