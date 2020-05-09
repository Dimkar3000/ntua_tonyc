use crate::error::Error;
use crate::parser::*;
use std::fmt::Display;

fn get_token<'a>(tokens: &'a [Token], index: &mut usize) -> Result<&'a Token, Error> {
    match tokens.get(*index) {
        Some(k) => Ok(k),
        None => Err(Error::with_message(
            tokens[*index - 1].column,
            tokens[*index - 1].line,
            "tried to get token but failed",
            "Typedecl",
        )),
    }
}

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
    pub fn generate(tokens: &[Token], index: &mut usize) -> Result<Self, Error> {
        let mut current_token = get_token(tokens, index)?;
        let mut t = match current_token.kind {
            TokenKind::KInt => TypeDecl::Int,
            TokenKind::KChar => TypeDecl::Char,
            TokenKind::KBool => TypeDecl::Bool,
            TokenKind::KList => {
                *index += 1;
                current_token = get_token(tokens, index)?;
                match current_token.kind {
                    TokenKind::LBracket => {
                        *index += 1;
                        let sub = TypeDecl::generate(tokens, index);
                        match sub {
                            Ok(e) => TypeDecl::List(Box::new(e)),
                            Err(e) => return Err(e),
                        }
                    }
                    _ => {
                        return Err(Error::with_message(
                            current_token.column,
                            current_token.line,
                            "Expected Left bracket after keyword \"List\"",
                            "Ast",
                        ))
                    }
                }
            }
            TokenKind::Error => match &current_token.extra {
                TokenExtra::Error(e) => {
                    return Err(Error::with_message(
                        current_token.column,
                        current_token.line,
                        &format!("Parser: {}", e),
                        "Ast",
                    ))
                }

                _ => unreachable!("Token kind and extra should be consistent"),
            },
            e => {
                return Err(Error::with_message(
                    current_token.column,
                    current_token.line,
                    &format!("Wrong type passed: \"{}\"", e),
                    "Ast",
                ))
            }
        };
        //Handle array syntax
        loop {
            *index += 1;
            current_token = get_token(tokens, index)?;
            match current_token.kind {
                TokenKind::LBracket => {
                    *index += 1;
                    current_token = get_token(tokens, index)?;
                    if current_token.kind == TokenKind::RBracket {
                        t = TypeDecl::Array(Box::new(t));
                    } else {
                        return Err(Error::with_message(
                            current_token.column,
                            current_token.line,
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
    pub fn generate_partial(tokens: &[Token], index: &mut usize) -> Result<TypeDecl, Error> {
        let start = *index;
        let mut current_token = &tokens[start];
        let mut t = match current_token.kind {
            TokenKind::KInt => TypeDecl::Int,
            TokenKind::KChar => TypeDecl::Char,
            TokenKind::KBool => TypeDecl::Bool,
            TokenKind::KList => {
                *index += 1;
                current_token = get_token(tokens, index)?;
                match current_token.kind {
                    TokenKind::LBracket => {
                        *index += 1;
                        let sub = TypeDecl::generate(tokens, index);
                        match sub {
                            Ok(e) => TypeDecl::List(Box::new(e)),
                            Err(e) => return Err(e),
                        }
                    }
                    _ => {
                        return Err(Error::with_message(
                            current_token.column,
                            current_token.line,
                            "Expected Left bracket after keyword \"List\"",
                            "Ast",
                        ))
                    }
                }
            }
            TokenKind::Error => match &current_token.extra {
                TokenExtra::Error(e) => {
                    return Err(Error::with_message(
                        current_token.column,
                        current_token.line,
                        &format!("Parser: {}", e),
                        "Ast",
                    ))
                }

                _ => unreachable!("Token kind and extra should be consistent"),
            },
            e => {
                return Err(Error::with_message(
                    current_token.column,
                    current_token.line,
                    &format!("Wrong type passed: \"{}\"", e),
                    "Ast",
                ))
            }
        };
        //Handle array syntax
        loop {
            *index += 1;
            current_token = get_token(tokens, index)?;
            match current_token.kind {
                TokenKind::LBracket => {
                    *index += 1;
                    current_token = get_token(tokens, index)?;
                    if current_token.kind == TokenKind::RBracket {
                        t = TypeDecl::Array(Box::new(t));
                    } else {
                        return Ok(TypeDecl::Array(Box::new(t)));
                    }
                }
                _ => break,
            }
        }
        // parser.get_token();
        Err(Error::with_message(current_token.column, current_token.line, "partial Type should end on open LBracket because an expression of its size is next. ONLY USED for memory initialization of arrays","Ast"))
    }
}
