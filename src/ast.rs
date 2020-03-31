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
pub enum TypeDecl {
    Int,
    Char,
    Bool,
    Array(Box<TypeDecl>),
    List(Box<TypeDecl>),
}

#[derive(Debug, Clone)]
pub struct VarDef {
    name: String,
    var_type: TypeDecl,
}

#[derive(Debug, Clone)]
pub struct FormalDecl {
    is_ref: bool,
    def: VarDef,
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
    rtype: Option<TypeDecl>,
    name: String,
    arguments: Vec<FormalDecl>,
}

impl VarDef {
    fn new(name: &str, var_type: TypeDecl) -> Self {
        VarDef {
            name: name.to_string(),
            var_type,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Atomic {
    Name(String),
    CString(String),
    Accessor(Box<Atomic>, Box<Expr>),
    FuncCall(String, Vec<Expr>),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Atomic(Atomic),
    CInt(i32),
    CChar(char),
    CBool(bool),
    Parenthesis(Box<Expr>),

    Unary(TokenKind, Box<Expr>),
    Binary(TokenKind, Box<Expr>, Box<Expr>),

    Negation(Box<Expr>),
    Comparison(TokenKind, Box<Expr>, Box<Expr>),
    NilCheck(Box<Expr>),

    CNil, // expty list of any type
    NewArray(TypeDecl, Box<Expr>),
    Hash(Box<Expr>, Box<Expr>), // list creation head # tail
    Head(Box<Expr>),
    Tail(Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Exit,
    Return(String),
    If(String),
    For(String),
    Skip,
    Assign(String),
    Call(String),
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
    pub fn var_type(&mut self) -> Result<TypeDecl, AstError> {
        let mut t = match self.read_token().get_kind() {
            TokenKind::KInt => TypeDecl::Int,
            TokenKind::KChar => TypeDecl::Char,
            TokenKind::KBool => TypeDecl::Bool,
            TokenKind::KList => match self.parser.get_token().get_kind() {
                TokenKind::LBracket => {
                    self.parser.get_token();
                    let sub = self.var_type();
                    match sub {
                        Ok(e) => TypeDecl::List(Box::new(e)),
                        Err(e) => return Err(e),
                    }
                }
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
            e => {
                return Err(AstError::with_message(
                    self.parser.column,
                    self.parser.line,
                    &format!("Wrong type passed: \"{:?}\"", e),
                ))
            }
        };
        //Handle array syntax
        loop {
            let token = self.parser.get_token();
            match token.get_kind() {
                TokenKind::LBracket => {
                    if self.parser.get_token().get_kind() == TokenKind::RBracket {
                        t = TypeDecl::Array(Box::new(t));
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
                    e => {
                        return Err(AstError::with_message(
                            self.parser.column,
                            self.parser.line,
                            &format!("Ast: Expected name definition, found \"{:?}\"", e),
                        ))
                    }
                }
            },
            Err(e) => return Err(e.extend("Ast: variable definition failed")),
        };
        Ok(results)
    }

    pub fn atom(&mut self) -> Result<Atomic, AstError> {
        let base = match self.read_token() {
            Token {
                kind: TokenKind::Name,
                extra: TokenExtra::Name(n),
                ..
            } => match self.parser.get_token().kind {
                TokenKind::LParenthesis => {
                    let mut b = Vec::new();
                    loop {
                        let tmp = match self.expr() {
                            Ok(k) => k,
                            Err(e) => return Err(e.extend("Ast: Function Arguments failed")),
                        };
                        b.push(tmp);
                        match self.read_token().get_kind() {
                            TokenKind::Comma => (),
                            _ => break,
                        }
                    }
                    Atomic::FuncCall(n, b)
                }
                _ => {
                    self.parser.back();
                    Atomic::Name(n)
                }
            },
            Token {
                kind: TokenKind::CString,
                extra: TokenExtra::CString(s),
                ..
            } => Atomic::CString(s),

            e => {
                return Err(AstError::with_message(
                    self.parser.column,
                    self.parser.line,
                    &format!("Ast: atomic failed to consume token: {:?}", e),
                ))
            }
        };

        match self.parser.get_token().get_kind() {
            TokenKind::LBracket => {
                let b = match self.expr() {
                    Ok(k) => k,
                    Err(e) => {
                        return Err(
                            e.extend("Ast:Failed to parse bracket content")
                        )
                    }
                };
                match self.read_token().get_kind() {
                    TokenKind::RBracket => {
                        return Ok(Atomic::Accessor(Box::new(base), Box::new(b)))
                    }
                    e => {
                        return Err(AstError::with_message(
                            self.parser.column,
                            self.parser.line,
                            &format!("Ast; right bracket missing: {:?}", e),
                        ))
                    }
                }
            }
            _ => return Ok(base),
        };
    }

    pub fn expr(&mut self) -> Result<Expr, AstError> {
        // match self.parser.get_token().get_kind() {}
        unimplemented!()
    }

    pub fn func_decl(&mut self) -> Result<FuncDecl, AstError> {
        match self.parser.read_token().get_kind() {
            TokenKind::KDecl => self.parser.get_token(),
            e => {
                return Err(AstError::with_message(
                    self.parser.column,
                    self.parser.line,
                    &format!("Ast: failed to find function decl: {:?}", e),
                ))
            }
        };
        // println!("s1: {:?}",self.parser.get_token().get_kind());

        let rtype = match self.var_type() {
            Ok(k) => Some(k),
            Err(_) => None,
        };

        // println!("s2: {:?}",self.parser.get_token().get_kind());
        let name = match self.parser.read_token() {
            Token {
                kind: TokenKind::Name,
                extra: TokenExtra::Name(n),
                col: _,
                line: _,
            } => n.to_string(),
            e => {
                return Err(AstError::with_message(
                    self.parser.column,
                    self.parser.line,
                    &format!("Ast: function declaration missing name: {:?}", e),
                ))
            }
        };
        self.parser.get_token();
        // println!("s3: {:?}",self.parser.get_token().get_kind());
        match self.formal_def() {
            Ok(k) => Ok(FuncDecl {
                rtype,
                name,
                arguments: k,
            }),
            e => Err(AstError::with_message(
                self.parser.column,
                self.parser.line,
                &format!("Ast: function declaration missing arguments: {:?}", e),
            )),
        }
    }

    pub fn stmt_decl(&mut self) -> Result<Vec<Stmt>, AstError> {
        let mut results = Vec::new();
        match self.read_token().get_kind() {
            TokenKind::KExit => results.push(Stmt::Exit),
            TokenKind::KSkip => results.push(Stmt::Skip),
            TokenKind::KFor => {
                let start = self.parser.index;
                while self.parser.get_token().get_kind() != TokenKind::KEnd {}
                results.push(Stmt::For(
                    self.parser.stream[start..self.parser.index].to_owned(),
                ))
            }
            TokenKind::KIf => {
                let start = self.parser.index;
                while self.parser.get_token().get_kind() != TokenKind::KEnd {}
                results.push(Stmt::If(
                    self.parser.stream[start..self.parser.index].to_owned(),
                ))
            }
            e => {
                return Err(AstError::with_message(
                    self.parser.column,
                    self.parser.line,
                    &format!("Ast failed to parse statement: {:?}", e),
                ))
            }
        };

        Ok(results)
    }

    pub fn formal_def(&mut self) -> Result<Vec<FormalDecl>, AstError> {
        match self.parser.read_token().get_kind() {
            TokenKind::LParenthesis => self.parser.get_token(),
            e => {
                return Err(AstError::with_message(
                    self.parser.column,
                    self.parser.line,
                    &format!(
                        "Ast: formal definitions start with parenthesis but I was given: {:?}",
                        e
                    ),
                ))
            }
        };
        // println!("{:?}", self.parser.read_token().get_kind());
        let mut results = Vec::new();
        loop {
            let is_ref = match self.parser.read_token().get_kind() {
                TokenKind::KRef => {
                    self.parser.get_token();
                    true
                }
                _ => false,
            };

            let defs = match self.var_def() {
                Ok(v) => v,
                Err(e) => return Err(e.extend("Ast: Inside formal")),
            };
            let i = defs.iter().map(|x| FormalDecl {
                is_ref,
                def: VarDef::new(&x.name, x.var_type.clone()),
            });
            results.extend(i);
            // println!("{:?}", self.parser.read_token().get_kind());
            match self.parser.read_token().get_kind() {
                TokenKind::Semicolon => self.parser.get_token(),
                TokenKind::RParenthesis => {
                    println!("here");
                    self.parser.get_token();
                    break;
                }
                e => {
                    return Err(AstError::with_message(
                        self.parser.column,
                        self.parser.line,
                        &format!("unxpected token: {:?}", e),
                    ))
                }
            };
            // println!("{:?}", self.parser.read_token().get_kind());
        }
        self.parser.get_token();
        Ok(results)
    }
}
