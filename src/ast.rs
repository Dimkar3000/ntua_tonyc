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
    // body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct FuncDef {
    pub rtype: Option<TypeDecl>,
    pub name: String,
    pub arguments: Vec<FormalDecl>,
    pub decls: Vec<FuncDecl>,
    pub defs: Vec<FuncDef>,
    pub vars: Vec<VarDef>,
    pub stmts: Vec<Stmt>,
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
    CInt(i16),
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

type ElseIfBlock = Vec<(Expr, Vec<Stmt>)>;
#[derive(Debug, Clone)]
pub enum Stmt {
    Exit,
    Return(Box<Expr>),
    If {
        condition: Expr,
        stmts: Vec<Stmt>,
        elseif: ElseIfBlock,
        elseblock: Vec<Stmt>,
    },
    For(Vec<Stmt>, Expr, Vec<Stmt>, Vec<Stmt>), // Note: First 2 vecs should contain only simple vec
    Skip,
    Assign(String, Expr),
    Call(String, Vec<Expr>),
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

    // This version won't throw an error on missing right bracket. Used for 'new int[5]' type of statements
    pub fn partial_var_type(&mut self) -> Result<TypeDecl, AstError> {
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
                        return Ok(TypeDecl::Array(Box::new(t)));
                    }
                }
                _ => break,
            }
        }
        // self.parser.get_token();
        Err(AstError::with_message(self.parser.column, self.parser.line, "Ast: partial Type should end on open LBracket because an expression of its size is next. ONLY USED for memory initialization of arrays"))
    }

    // Expects the token to be Int, Char, Bool
    pub fn var_def(&mut self) -> Result<Vec<VarDef>, AstError> {
        let mut results = Vec::new();
        let kind = &self.read_token().get_kind();
        // sanity check
        println!("{} {},{:?},{:?}",self.parser.column,self.parser.line,self.read_token().get_kind(),self.read_token().extra);
        if kind == &TokenKind::RParenthesis {
            return Ok(Vec::new());
        }
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
        // println!("token: {:?}", self.read_token().get_kind());
        let base = match self.read_token() {
            Token {
                kind: TokenKind::Name,
                extra: TokenExtra::Name(n),
                ..
            } => match self.parser.get_token().kind {
                TokenKind::LParenthesis => {
                    // println!("here: {:?}", self.read_token());
                    let mut b = Vec::new();
                    loop {
                        self.parser.get_token();
                        println!("token: {:?}", self.read_token().get_kind());
                        if self.read_token().get_kind() == TokenKind::RParenthesis {
                            break;
                        }
                        let tmp = match self.expr() {
                            Ok(k) => k,
                            Err(e) => return Err(e.extend("Ast: Function Arguments failed")),
                        };
                        b.push(tmp);
                        println!("b = {:?}", b);
                        match self.read_token().get_kind() {
                            TokenKind::Comma => (),
                            TokenKind::RParenthesis => break,
                            e => {
                                return Err(AstError::with_message(
                                    self.parser.column,
                                    self.parser.line,
                                    &format!(
                                        "Ast: Expected RParenthesis or comma, but got {:?}, {:?}",
                                        e,
                                        self.read_token()
                                    ),
                                ))
                            }
                        }
                    }
                    self.parser.get_token();
                    // self.parser.get_token();
                    Atomic::FuncCall(n, b)
                }
                _ => {
                    // self.parser.back();
                    Atomic::Name(n)
                }
            },
            Token {
                kind: TokenKind::CString,
                extra: TokenExtra::CString(s),
                ..
            } => {
                self.parser.get_token();
                Atomic::CString(s)
                
            },

            e => {
                return Err(AstError::with_message(
                    self.parser.column,
                    self.parser.line,
                    &format!("Ast: atomic failed to consume token: {:?}", e),
                ))
            }
        };
        // println!("here: {:?}", base);
        // println!("token: {:?}", self.read_token().get_kind());
        match self.parser.read_token().get_kind() {
            TokenKind::LBracket => {
                self.parser.get_token();
                let b = match self.expr() {
                    Ok(k) => k,
                    Err(e) => return Err(e.extend("Ast:Failed to parse bracket content")),
                };
                match self.read_token().get_kind() {
                    TokenKind::RBracket => {
                        self.parser.get_token();
                        return Ok(Atomic::Accessor(Box::new(base), Box::new(b)));
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
        let left = match self.read_token().get_kind() {
            TokenKind::Name | TokenKind::CString => Expr::Atomic(self.atom()?),
            TokenKind::KTrue => {
                self.parser.get_token();
                Expr::CBool(true)
            }
            TokenKind::KFalse => {
                self.parser.get_token();
                Expr::CBool(false)
            }
            TokenKind::KNil => {
                self.parser.get_token();
                Expr::CNil
            }
            TokenKind::CChar => {
                if let TokenExtra::Cchar(n) = self.read_token().extra {
                    self.parser.get_token();
                    Expr::CChar(n)
                } else {
                    return Err(AstError::with_message(
                        self.parser.column,
                        self.parser.line,
                        "Parser: Token wan CChar but extra wan't",
                    ));
                }
            }
            TokenKind::INT => {
                if let TokenExtra::Number(n) = self.read_token().extra {
                    self.parser.get_token();
                    Expr::CInt(n)
                } else {
                    return Err(AstError::with_message(
                        self.parser.column,
                        self.parser.line,
                        "Parser: Token wan Int but extra wan't Number",
                    ));
                }
            },
            TokenKind::KNot => {
                self.parser.get_token();
                Expr::Negation( Box::new(self.expr()?))
            },
            TokenKind::LParenthesis => {
                self.parser.get_token();
                let c = self.expr()?;
                match self.read_token().get_kind() {
                    TokenKind::RParenthesis => {
                        self.parser.get_token();
                        Expr::Parenthesis(Box::new(c))
                    }
                    e => return Err(AstError::with_message(self.parser.column, self.parser.line, &format!("Ast: Expected RParenthesis but got: {:?}",e))), 
                }

            }
            TokenKind::Addition | TokenKind::Subtraction => {
                let a = self.read_token().get_kind();
                self.parser.get_token();
                Expr::Unary(a, Box::new(self.expr()?))
            }
            TokenKind::KNew => {
                self.parser.get_token();
                let t = self.partial_var_type()?;
                // self.parser.get_token();
                let exp = self.expr()?;
                match self.read_token().get_kind() {
                    TokenKind::RBracket => {
                        self.parser.get_token();
                        return Ok(Expr::NewArray(t, Box::new(exp)));
                    }
                    e => {
                        return Err(AstError::with_message(
                            self.parser.column,
                            self.parser.line,
                            &format!("Ast: Expected RBracket but got: {:?}", e),
                        ))
                    }
                }
            }
            TokenKind::KHead => match self.parser.get_token().get_kind() {
                TokenKind::LParenthesis => {
                    self.parser.get_token();
                    let exp = self.expr()?;
                    match self.read_token().get_kind() {
                        TokenKind::RParenthesis => {
                            self.parser.get_token();
                            Expr::Head(Box::new(exp))
                        }
                        e => {
                            return Err(AstError::with_message(
                                self.parser.column,
                                self.parser.line,
                                &format!(
                                    "Ast missing right paranthesis of head, instead got: {:?}",
                                    e
                                ),
                            ))
                        }
                    }
                }
                e => {
                    return Err(AstError::with_message(
                        self.parser.column,
                        self.parser.line,
                        &format!("Ast missing left paranthesis of head, instead got: {:?}", e),
                    ))
                }
            },
            TokenKind::KTail => match self.parser.get_token().get_kind() {
                TokenKind::LParenthesis => {
                    self.parser.get_token();
                    let exp = self.expr()?;
                    match self.read_token().get_kind() {
                        TokenKind::RParenthesis => {
                            self.parser.get_token();
                            Expr::Tail(Box::new(exp))
                        }
                        e => {
                            return Err(AstError::with_message(
                                self.parser.column,
                                self.parser.line,
                                &format!(
                                    "Ast missing right paranthesis of tail, instead got: {:?}",
                                    e
                                ),
                            ))
                        }
                    }
                }
                e => {
                    return Err(AstError::with_message(
                        self.parser.column,
                        self.parser.line,
                        &format!("Ast missing left paranthesis of tail, instead got: {:?}", e),
                    ))
                }
            },
            TokenKind::KNilQ => match self.parser.get_token().get_kind() {
                TokenKind::LParenthesis => {
                    self.parser.get_token();
                    let exp = self.expr()?;
                    match self.read_token().get_kind() {
                        TokenKind::RParenthesis => {
                            self.parser.get_token();
                            Expr::NilCheck(Box::new(exp))
                        }
                        e => {
                            return Err(AstError::with_message(
                                self.parser.column,
                                self.parser.line,
                                &format!(
                                    "Ast missing right paranthesis of nil?, instead got: {:?}",
                                    e
                                ),
                            ))
                        }
                    }
                }
                e => {
                    return Err(AstError::with_message(
                        self.parser.column,
                        self.parser.line,
                        &format!("Ast missing left paranthesis of nil?, instead got: {:?}", e),
                    ))
                }
            },

            e => {
                return Err(AstError::with_message(
                    self.parser.column,
                    self.parser.line,
                    &format!("Ast: unexpected token given to expr: {:?}", e),
                ))
            }
            // match self.atom() {
            //     Ok(k) => Ok(Expr::Atomic(k)),
            //     Err()
            // },
        };

        // Binary Operations
        // println!("{:?}", self.read_token().get_kind());
        match self.read_token().get_kind() {
            TokenKind::Addition
            | TokenKind::Subtraction
            | TokenKind::Multiplication
            | TokenKind::Division
            | TokenKind::KMod => {
                let a = self.read_token().get_kind();
                self.parser.get_token();
                let right = self.expr()?;
                Ok(Expr::Binary(a, Box::new(left), Box::new(right)))
            }
            TokenKind::Hash => {
                self.parser.get_token();
                let right = self.expr()?;
                Ok(Expr::Hash(Box::new(left), Box::new(right)))
            }
            TokenKind::Equal
            | TokenKind::NotEqual
            | TokenKind::Great
            | TokenKind::GreatOrEqual
            | TokenKind::Less
            | TokenKind::LessOrEqual
            | TokenKind::KAnd
            | TokenKind::KOr => {
                let a = self.read_token().get_kind();
                self.parser.get_token();
                let right = self.expr()?;
                Ok(Expr::Comparison(a, Box::new(left), Box::new(right)))
            }
            _ => Ok(left),
        }
    }

    pub fn func_decl(&mut self) -> Result<FuncDecl, AstError> {
        match self.parser.read_token().get_kind() {
            TokenKind::KDecl => {
                let t = match self.parser.get_token().get_kind() {
                    TokenKind::Name => None,
                    TokenKind::KList | TokenKind::KInt | TokenKind::KChar | TokenKind::KBool => {
                        Some(self.var_type()?)
                    }
                    e => {
                        return Err(AstError::with_message(
                            self.parser.column,
                            self.parser.line,
                            &format!("Ast: Wrong token in Function definition: {:?}", e),
                        ))
                    }
                };
                let name = match self.read_token() {
                    Token {
                        kind: TokenKind::Name,
                        extra: TokenExtra::Name(n),
                        ..
                    } => {
                        self.parser.get_token();
                        n
                    }
                    e => {
                        return Err(AstError::with_message(
                            self.parser.column,
                            self.parser.line,
                            &format!("Ast: Wrong token in Function definition: {:?}", e),
                        ))
                    }
                };
                if self.read_token().get_kind() != TokenKind::LParenthesis {
                    return Err(AstError::with_message(
                        self.parser.column,
                        self.parser.line,
                        &format!("Ast: Expected LParenthesis next to function name"),
                    ));
                }

                let args = self.formal_def()?;
                // println!("here: {:?}", self.read_token().get_kind());
                return Ok(FuncDecl {
                    rtype: t,
                    name,
                    arguments: args,
                });
            }
            e => {
                return Err(AstError::with_message(
                    self.parser.column,
                    self.parser.line,
                    &format!("Ast: failed to find function decl: {:?}", e),
                ))
            }
        };
        // println!("s1: {:?}",self.parser.get_token().get_kind());
    }

    pub fn func_def(&mut self) -> Result<FuncDef, AstError> {
        match self.parser.read_token().get_kind() {
            TokenKind::KDef => {
                let t = match self.parser.get_token().get_kind() {
                    TokenKind::Name => None,
                    TokenKind::KList | TokenKind::KInt | TokenKind::KChar | TokenKind::KBool => {
                        Some(self.var_type()?)
                    }
                    e => {
                        return Err(AstError::with_message(
                            self.parser.column,
                            self.parser.line,
                            &format!("Ast: Wrong token in Function definition: {:?}", e),
                        ))
                    }
                };
                let name = match self.read_token() {
                    Token {
                        kind: TokenKind::Name,
                        extra: TokenExtra::Name(n),
                        ..
                    } => {
                        self.parser.get_token();
                        n
                    }
                    e => {
                        return Err(AstError::with_message(
                            self.parser.column,
                            self.parser.line,
                            &format!("Ast: Wrong token in Function definition: {:?}", e),
                        ))
                    }
                };
                if self.read_token().get_kind() != TokenKind::LParenthesis {
                    return Err(AstError::with_message(
                        self.parser.column,
                        self.parser.line,
                        &format!("Ast: Expected LParenthesis next to function name"),
                    ));
                }

                let args = self.formal_def()?;
                let mut defs = Vec::new();
                let mut decls = Vec::new();
                let mut vars = Vec::new();
                let mut stmts = Vec::new();

                loop {
                    match self.read_token().get_kind() {
                        TokenKind::KDef => defs.push(self.func_def()?),
                        TokenKind::KDecl => decls.push(self.func_decl()?),
                        TokenKind::KInt
                        | TokenKind::KBool
                        | TokenKind::KList
                        | TokenKind::KChar => vars.extend(self.var_def()?),
                        _ => break,
                    }
                }
                while self.read_token().get_kind() != TokenKind::KEnd {
                    stmts.push(self.stmt()?);
                }
                self.parser.get_token();
                Ok(FuncDef{
                    rtype: t,
                    name: name,
                    arguments: args,
                    decls: decls,
                    defs: defs,
                    vars: vars,
                    stmts: stmts,
                   
                })
            }
            e => {
                return Err(AstError::with_message(
                    self.parser.column,
                    self.parser.line,
                    &format!("Ast: failed to find function decl: {:?}", e),
                ))
            }
        }
        // println!("s1: {:?}", self.parser.get_token().get_kind());
    }

    pub fn stmt(&mut self) -> Result<Stmt, AstError> {
        let t = self.read_token();
        // self.parser.get_token();
        match t.get_kind() {
            TokenKind::Name => {
                let atom = self.atom()?;
                match self.read_token().get_kind() {
                    TokenKind::Assignement => {
                        self.parser.get_token();
                        let expr = self.expr()?;
                        match t.extra {
                            TokenExtra::Name(s) =>
                            Ok(Stmt::Assign(s,expr)),
                            e => unreachable!("should never reach: {:?}",e),
                        }
                    },
                    _ => {
                        match atom {
                            Atomic::FuncCall(n,a) => Ok(Stmt::Call(n,a)),
                            e => return Err(AstError::with_message(self.parser.column, self.parser.line, &format!("An atomic expression is not a stmt unless it is a call, here it was: {:?}",e)))   
                        }
                    }
                }
            }
            TokenKind::KSkip => {
                self.parser.get_token();
                Ok(Stmt::Skip)
            }
            TokenKind::KExit => {
                self.parser.get_token();
                Ok(Stmt::Exit)
            }
            TokenKind::KReturn => {
                self.parser.get_token();
                Ok(Stmt::Return(Box::new(self.expr()?)))
            }
            TokenKind::KFor => {
                self.parser.get_token();
                let mut first_simples = Vec::new();
                loop {
                    let s = self.stmt()?;
                    match s {
                        Stmt::Skip | Stmt::Assign(..) | Stmt::Call(..) => (),
                        e => {
                            return Err(AstError::with_message(
                                self.parser.column,
                                self.parser.line,
                                &format!("Ast: Expected simple Statement but got: {:?}", e),
                            ))
                        }
                    }
                    first_simples.push(s);
                    match self.read_token().get_kind() {
                        TokenKind::Comma => continue,
                        TokenKind::Semicolon => break,
                        e => {
                            return Err(AstError::with_message(
                                self.parser.column,
                                self.parser.line,
                                &format!("Ast: Expected \",\" or \";\" but got: {:?}", e),
                            ))
                        }
                    }
                }
                self.parser.get_token();
                let cond = self.expr()?;
                match self.read_token().get_kind() {
                    TokenKind::Semicolon => self.parser.get_token(),
                    e => {
                        return Err(AstError::with_message(
                            self.parser.column,
                            self.parser.line,
                            &format!("Ast: Expected simple Statement but got: {:?}", e),
                        ))
                    }
                };
                let mut second_simples = Vec::new();
                loop {
                    let s = self.stmt()?;
                    match s {
                        Stmt::Skip | Stmt::Assign(..) | Stmt::Call(..) => (),
                        e => {
                            return Err(AstError::with_message(
                                self.parser.column,
                                self.parser.line,
                                &format!("Ast: Expected simple Statement but got: {:?}", e),
                            ))
                        }
                    }
                    second_simples.push(s);
                    match self.read_token().get_kind() {
                        TokenKind::Comma => continue,
                        TokenKind::Colon => break,
                        e => {
                            return Err(AstError::with_message(
                                self.parser.column,
                                self.parser.line,
                                &format!("Ast: Expected \",\" or \":\" but got: {:?}", e),
                            ))
                        }
                    }
                }
                self.parser.get_token();
                let mut body = Vec::new();
                while self.read_token().get_kind() != TokenKind::KEnd {
                    body.push(self.stmt()?);
                }
                self.parser.get_token();
                Ok(Stmt::For(first_simples, cond, second_simples, body))
            }
            TokenKind::KIf => {
                self.parser.get_token();
                let cond = self.expr()?;
                let mut elseifs = ElseIfBlock::new();
                let mut elseblock = Vec::new();
                let mut stmts = Vec::new();
                // println!("here: {:?}", self.read_token().get_kind());
                match self.read_token().get_kind() {
                    TokenKind::Colon => (),
                    c => {
                        return Err(AstError::with_message(
                            self.parser.column,
                            self.parser.line,
                            &format!("Expect Colon after If block's expression but got: {:?}", c),
                        ))
                    }
                }
                self.parser.get_token();
                //if block
                // println!("if: {:?}", self.read_token().get_kind());
                while self.read_token().get_kind() != TokenKind::KElseif
                    && self.read_token().get_kind() != TokenKind::KElse
                    && self.read_token().get_kind() != TokenKind::KEnd
                {
                    stmts.push(self.stmt()?);
                }
                // self.parser.get_token();
                // println!("elseif: {:?}", self.read_token().get_kind());
                while self.read_token().get_kind() == TokenKind::KElseif {
                    self.parser.get_token();
                    let cond = self.expr()?;
                    match self.read_token().get_kind() {
                        TokenKind::Colon => (),
                        c => {
                            return Err(AstError::with_message(
                                self.parser.column,
                                self.parser.line,
                                &format!(
                                    "Expect Colon after ElseIf block's expression but got: {:?}",
                                    c
                                ),
                            ))
                        }
                    }
                    self.parser.get_token();
                    let mut local_stmts = Vec::new();
                    //if block
                    while self.read_token().get_kind() != TokenKind::KElseif
                        && self.read_token().get_kind() != TokenKind::KElse
                        && self.read_token().get_kind() != TokenKind::KEnd
                    {
                        local_stmts.push(self.stmt()?);
                    }
                    
                    elseifs.push((cond, local_stmts));
                }
                if self.read_token().get_kind() == TokenKind::KElse {
                    self.parser.get_token();
                    match self.read_token().get_kind() {
                        TokenKind::Colon => (),
                        c => {
                            return Err(AstError::with_message(
                                self.parser.column,
                                self.parser.line,
                                &format!("Expect Colon after Else keyword but got: {:?}", c),
                            ))
                        }
                    }
                    self.parser.get_token();
                    while self.read_token().get_kind() != TokenKind::KEnd {
                        elseblock.push(self.stmt()?);
                    }
                }
                self.parser.get_token();
                
                Ok(Stmt::If {
                    condition: cond,
                    stmts: stmts,
                    elseif: elseifs,
                    elseblock: elseblock,
                })
            }
            e => {
                return Err(AstError::with_message(
                    self.parser.column,
                    self.parser.line,
                    &format!(
                        "Ast failed to parse statement: {:?} with extra; {:?}",
                        e,
                        self.read_token()
                    ),
                ))
            }
        }
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
                    // println!("here");
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
