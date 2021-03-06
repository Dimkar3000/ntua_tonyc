/// # TokenKind
/// - it represents the different tokens that the input is split into.
///
/// - ## Error
///   if the text parsed is not valid for the language the parser returns Error
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    Addition,
    Assignement,
    CChar,
    CString,
    Colon,
    Comma,
    Comment,
    Division,
    Empty,
    Equal,
    Error,
    Great,
    GreatOrEqual,
    Hash,
    INT,
    KAnd,
    KBool,
    KChar,
    KDecl,
    KDef,
    KElse,
    KElseif,
    KEnd,
    KExit,
    KFalse,
    KFor,
    KHead,
    KIf,
    KInt,
    KList,
    KMod,
    KNew,
    KNil,
    KNilQ,
    KNot,
    KOr,
    KRef,
    KReturn,
    KSkip,
    KTail,
    KTrue,
    LBracket,
    LParenthesis,
    Less,
    LessOrEqual,
    Multiplication,
    Name,
    NewLine,
    NotEqual,
    NotStarted,
    RBracket,
    RParenthesis,
    Semicolon,
    Space,
    Subtraction,
}

use std::fmt;
impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{:?}", self)
    }
}
