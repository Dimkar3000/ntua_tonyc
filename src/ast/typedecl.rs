use crate::error::Error;
use crate::parser::*;
use std::fmt::Display;
/// Typedecl is the type of a single block of code.
#[derive(Debug, Clone)]
pub enum TypeDecl {
    /// a function that returns nothing has this type
    Void,
    /// the type of the nill keyword which is a empty list of any type
    Nil,
    /// signed integer of 16-bit
    Int,
    /// single character in ascii table. The size is 8-bit
    Char,
    /// boolean type: **true** or **false**
    Bool,
    /// array of contingues data on the heap. The Type of the data is the sub-type
    Array(Box<TypeDecl>),
    /// linked list data. Leaves on the head and the type of the data is the sub-type
    List(Box<TypeDecl>),
}

impl Display for TypeDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            TypeDecl::Int => write!(f, "int"),
            TypeDecl::Void => write!(f, "void"),
            TypeDecl::Nil => write!(f, "nil"),
            TypeDecl::Char => write!(f, "char"),
            TypeDecl::Bool => write!(f, "bool"),
            TypeDecl::Array(t) => write!(f, "array[{}]", t),
            TypeDecl::List(t) => write!(f, "list[{}]", t),
        }
    }
}

/// PartialEq for Typedecl cannot be derived because the nill type match any list type and void doen't match anytning but void.
/// Instead we provide a manual implementation
impl PartialEq for TypeDecl {
    fn eq(&self, r: &TypeDecl) -> bool {
        match (self, r) {
            (TypeDecl::Bool, TypeDecl::Bool)
            | (TypeDecl::Char, TypeDecl::Char)
            | (TypeDecl::Int, TypeDecl::Int) => true,
            (TypeDecl::Array(t1), TypeDecl::Array(t2)) => t1 == t2,
            (TypeDecl::List(t1), TypeDecl::List(t2)) => t1 == t2,
            (TypeDecl::List(_), TypeDecl::Nil) => true,
            (TypeDecl::Nil, TypeDecl::Nil) => true,
            (TypeDecl::Void, TypeDecl::Void) => true,
            _ => false,
        }
    }
}
impl TypeDecl {
    pub fn generate(parser: &mut Parser) -> Result<Self, Error> {
        let mut t = match parser.read_token().get_kind() {
            TokenKind::KInt => TypeDecl::Int,
            TokenKind::KChar => TypeDecl::Char,
            TokenKind::KBool => TypeDecl::Bool,
            TokenKind::KList => match parser.advance_token().get_kind() {
                TokenKind::LBracket => {
                    parser.advance_token();
                    let sub = TypeDecl::generate(parser);
                    match sub {
                        Ok(e) => TypeDecl::List(Box::new(e)),
                        Err(e) => return Err(e),
                    }
                }
                _ => {
                    return Err(Error::with_message(
                        parser.column,
                        parser.line,
                        "Expected Left bracket after keyword \"List\"",
                        "Ast",
                    ))
                }
            },
            TokenKind::Error => match parser.read_token().extra {
                TokenExtra::Error(e) => {
                    return Err(Error::with_message(
                        parser.column,
                        parser.line,
                        &format!("Parser: {}", e),
                        "Ast",
                    ))
                }

                _ => unreachable!("Token kind and extra should be consistent"),
            },
            e => {
                return Err(Error::with_message(
                    parser.column,
                    parser.line,
                    &format!("Wrong type passed: \"{:?}\"", e),
                    "Ast",
                ))
            }
        };
        //Handle array syntax
        loop {
            let token = parser.advance_token();
            match token.get_kind() {
                TokenKind::LBracket => {
                    if parser.advance_token().get_kind() == TokenKind::RBracket {
                        t = TypeDecl::Array(Box::new(t));
                    } else {
                        return Err(Error::with_message(
                            parser.column,
                            parser.line,
                            "Array definition missing Right Bracket",
                            "Ast",
                        ));
                    }
                }
                _ => break,
            }
        }
        // parser.get_token();
        Ok(t)
    }

    // This version won't throw an error on missing right bracket. Used for 'new int[5]' type of statements
    pub fn generate_partial(parser: &mut Parser) -> Result<TypeDecl, Error> {
        let mut t = match parser.read_token().get_kind() {
            TokenKind::KInt => TypeDecl::Int,
            TokenKind::KChar => TypeDecl::Char,
            TokenKind::KBool => TypeDecl::Bool,
            TokenKind::KList => match parser.advance_token().get_kind() {
                TokenKind::LBracket => {
                    parser.advance_token();
                    let sub = TypeDecl::generate(parser);
                    match sub {
                        Ok(e) => TypeDecl::List(Box::new(e)),
                        Err(e) => return Err(e),
                    }
                }
                _ => {
                    return Err(Error::with_message(
                        parser.column,
                        parser.line,
                        "Expected Left bracket after keyword \"List\"",
                        "Ast",
                    ))
                }
            },
            TokenKind::Error => match parser.read_token().extra {
                TokenExtra::Error(e) => {
                    return Err(Error::with_message(
                        parser.column,
                        parser.line,
                        &format!("Parser: {}", e),
                        "Ast",
                    ))
                }

                _ => unreachable!("Token kind and extra should be consistent"),
            },
            e => {
                return Err(Error::with_message(
                    parser.column,
                    parser.line,
                    &format!("Wrong type passed: \"{:?}\"", e),
                    "Ast",
                ))
            }
        };
        //Handle array syntax
        loop {
            let token = parser.advance_token();
            match token.get_kind() {
                TokenKind::LBracket => {
                    if parser.advance_token().get_kind() == TokenKind::RBracket {
                        t = TypeDecl::Array(Box::new(t));
                    } else {
                        return Ok(TypeDecl::Array(Box::new(t)));
                    }
                }
                _ => break,
            }
        }
        // parser.get_token();
        Err(Error::with_message(parser.column, parser.line, "partial Type should end on open LBracket because an expression of its size is next. ONLY USED for memory initialization of arrays","Ast"))
    }
}
