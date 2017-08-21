
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error {
    pub code: ErrorCode,
    pub line: usize,
    pub column: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ErrorCode {
    UnrecognizedToken,
    UnterminatedLiteral,
    UnterminatedBracket,
    UnterminatedBlockComment,
    BadVariableName,
    BadNumber,
    ExpectedEqualsSign,
    MalformedBlobLiteral,
    MalformedHexInteger,
}
