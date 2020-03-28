use crate::parser::Token;
// use crate::allocator::{Allocation, BumpAllocator};
use crate::parser::{Parser, TokenExtra, TokenKind};
// use std::ops::Deref;
use std::rc::Rc;
#[derive(Debug)]
pub struct AstError {
    sub_error: Option<Rc<AstError>>,
    message: String,
    col: usize,
    line: usize,
}

impl<'a> AstError {
    pub fn with_message(col: usize, line: usize, m: &'a str) -> Self {
        AstError {
            sub_error: None,
            message: m.to_owned(),
            col,
            line,
        }
    }

    pub fn extend(self, m: &'a str) -> Self {
        let line = self.line;
        let col = self.col;
        AstError {
            sub_error: Some(Rc::new(self)),
            message: m.to_owned(),
            line,
            col,
        }
    }
}

#[derive(Debug, Clone)]
pub enum VarType {
    Int,
    Char,
    Bool,
    Array(Box<VarType>),
    List(Box<VarType>),
}

#[derive(Debug, Clone)]
pub struct VarDef {
    name: String,
    var_type: VarType,
}

impl VarDef {
    fn new(name: &str, var_type: VarType) -> Self {
        VarDef {
            name: name.to_string(),
            var_type,
        }
    }
}

pub struct AstRoot {
    // allocator: BumpAllocator,
    pub parser: Parser,
}

impl AstRoot {
    pub fn new<T: Into<String>>(stream: T) -> Self {
        AstRoot {
            // allocator: BumpAllocator::new(),
            parser: Parser::new(stream.into()),
        }
    }

    fn read_token(&self) -> Token {
        self.parser.read_token()
    }

    // Todo(dimkar): check when can we blow the stack
    // Move the parser to the next token
    pub fn var_type(&mut self) -> Result<VarType, AstError> {
        let mut t = match self.read_token().get_kind() {
            TokenKind::KInt => VarType::Int,
            TokenKind::KChar => VarType::Char,
            TokenKind::KBool => VarType::Bool,
            TokenKind::KList => match self.parser.get_token().get_kind() {
                TokenKind::LBracket => {
                    self.parser.get_token();
                    let sub = self.var_type();
                    match sub {
                        Ok(e) => VarType::List(Box::new(e)),
                        Err(e) => return Err(e),
                    }
                    
                },
                _ => {
                    return Err(AstError::with_message(
                        self.parser.column,
                        self.parser.line,
                        "Ast: Expected Left bracket after keyword \"List\"",
                    ))
                }
            },
            TokenKind::Error => match self.read_token().extra {
                TokenExtra::Error(e) => {
                    return Err(AstError::with_message(
                        self.parser.column,
                        self.parser.line,
                        &mut format!("Parser: {}", e),
                    ))
                }

                _ => unreachable!("Token kind and extra should be consistent"),
            },
            _ => unreachable!("var_type expects the token to be correct"),
        };
        //Handle array syntax
        loop {
            let token = self.parser.get_token();
            match token.get_kind() {
                TokenKind::LBracket => {
                    if self.parser.get_token().get_kind() == TokenKind::RBracket {
                        t = VarType::Array(Box::new(t));
                    } else {
                        return Err(AstError::with_message(
                            self.parser.column,
                            self.parser.line,
                            "Ast: Array definition missing Right Bracket",
                        ));
                    }
                }
                _ => break,
            }
        }
        // self.parser.get_token();
        Ok(t)
    }

    // Expects the token to be Int, Char, Bool
    pub fn var_def(&mut self) -> Result<Vec<VarDef>, AstError> {
        let mut results = Vec::new();
        let kind = &self.read_token().get_kind();
        // sanity check
        assert!(
            kind == &TokenKind::KInt
                || kind == &TokenKind::KChar
                || kind == &TokenKind::KBool
                || kind == &TokenKind::KList
        );
        match self.var_type() {

            Ok(t) => loop {
                // println!("{:?}", self.read_token().get_kind());
                match self.read_token().get_kind() {
                    TokenKind::Name => match self.parser.read_token().extra {
                        TokenExtra::Name(name) => {
                            results.push(VarDef::new(&name, t.clone()));
                            if self.parser.get_token().get_kind() != TokenKind::Comma {
                                break;
                            }
                            self.parser.get_token();
                        }
                        _ => unreachable!("both kind and extra should be name"),
                    },
                    e => unreachable!("both kind and extra should be name: {:?}", e),
                }
            },
            Err(e) => return Err(e.extend("Ast: variable definition failed")),
            _ => unimplemented!("not now"),
        };
        Ok(results)
    }
}
