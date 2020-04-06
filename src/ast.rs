use crate::parser::Token;
use crate::parser::{Parser, TokenExtra, TokenKind};
use crate::symbol_table::SymbolTable;
// use std::collections::HashMap;
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

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDecl {
    Void,
    Nil,
    Int,
    Char,
    Bool,
    Array(Rc<TypeDecl>),
    List(Rc<TypeDecl>),
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
    Name(TypeDecl, String),
    CString(String),
    Accessor(Rc<Atomic>, Rc<Expr>),
    FuncCall(TypeDecl, String, Vec<Expr>),
}

impl Atomic {
    pub fn get_type(&self) -> TypeDecl {
        match self {
            Atomic::Name(t, _) => t.clone(),
            Atomic::CString(_) => TypeDecl::Array(Rc::new(TypeDecl::Char)),
            Atomic::FuncCall(t, ..) => t.clone(),
            Atomic::Accessor(base, _) => base.get_type(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Atomic(TypeDecl, Atomic),

    CChar(char),
    // Parenthesis(TypeDecl, Rc<Expr>),
    CInt(i16),
    Unary(TokenKind, Rc<Expr>),
    Binary(TokenKind, Rc<Expr>, Rc<Expr>),

    CBool(bool),
    Negation(Rc<Expr>),
    Comparison(TokenKind, Rc<Expr>, Rc<Expr>),
    NilCheck(Rc<Expr>),

    CNil, // expty list of any type
    NewArray(TypeDecl, Rc<Expr>),
    Hash(TypeDecl, Rc<Expr>, Rc<Expr>), // list creation head # tail
    Head(TypeDecl, Rc<Expr>),
    Tail(TypeDecl, Rc<Expr>),
}

impl Expr {
    pub fn get_type(&self) -> TypeDecl {
        match self {
            Expr::Atomic(t, _) => t.clone(),
            Expr::CInt(_) => TypeDecl::Int,
            Expr::CChar(_) => TypeDecl::Char,
            Expr::CBool(_) => TypeDecl::Bool,
            Expr::Unary(..) => TypeDecl::Int,
            Expr::Binary(..) => TypeDecl::Int,
            Expr::Negation(_) => TypeDecl::Bool,
            Expr::Comparison(..) => TypeDecl::Bool,
            Expr::NilCheck(_) => TypeDecl::Bool,
            Expr::CNil => TypeDecl::Nil, // expty list of any type
            Expr::NewArray(t, _) => t.clone(),
            Expr::Hash(t, _, _) => t.clone(), // list creation head # tail
            Expr::Head(t, _) => t.clone(),
            Expr::Tail(t, _) => t.clone(),
        }
    }
}

type ElseIfBlock = Vec<(Expr, Vec<Stmt>)>;

#[derive(Debug, Clone)]
pub enum Stmt {
    Exit,
    Return(Rc<Expr>),
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
    pub symbol_table: SymbolTable,
}

impl AstRoot {
    pub fn new<T: Into<String>>(stream: T) -> Self {
        let mut result = AstRoot {
            // allocator: BumpAllocator::new(),
            parser: Parser::new(stream.into()),
            // current_block_name: Scope{name:"root".to_owned()},
            symbol_table: SymbolTable::new(),
        };

        result.symbol_table.insert("puti", TypeDecl::Void).unwrap();
        result.symbol_table.insert("putb", TypeDecl::Void).unwrap();
        result.symbol_table.insert("putc", TypeDecl::Void).unwrap();
        result.symbol_table.insert("puts", TypeDecl::Void).unwrap();

        result.symbol_table.insert("geti", TypeDecl::Void).unwrap();
        result.symbol_table.insert("getb", TypeDecl::Void).unwrap();
        result.symbol_table.insert("getc", TypeDecl::Void).unwrap();
        result.symbol_table.insert("gets", TypeDecl::Void).unwrap();

        result
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
                        Ok(e) => TypeDecl::List(Rc::new(e)),
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
                        &format!("Parser: {}", e),
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
                        t = TypeDecl::Array(Rc::new(t));
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
                        Ok(e) => TypeDecl::List(Rc::new(e)),
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
                        &format!("Parser: {}", e),
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
                        t = TypeDecl::Array(Rc::new(t));
                    } else {
                        return Ok(TypeDecl::Array(Rc::new(t)));
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
                match self.read_token().get_kind() {
                    TokenKind::Name => match self.parser.read_token().extra {
                        TokenExtra::Name(name) => {
                            results.push(VarDef::new(&name, t.clone()));
                            match self.symbol_table.insert(name, t.clone()) {
                                Ok(_) => (),
                                Err(e) => {
                                    return Err(AstError::with_message(
                                        self.parser.column,
                                        self.parser.line,
                                        &e,
                                    ))
                                }
                            }
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
                    let mut args = Vec::new();
                    loop {
                        self.parser.get_token();
                        if self.read_token().get_kind() == TokenKind::RParenthesis {
                            break;
                        }
                        let tmp = match self.expr() {
                            Ok(k) => k,
                            Err(e) => return Err(e.extend("Ast: Function Arguments failed")),
                        };
                        args.push(tmp);
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
                    let d = n.to_string();
                    match self.symbol_table.lookup(n) {
                        Some(i) => Atomic::FuncCall(i.ctype.clone(), d, args),
                        None => {
                            return Err(AstError::with_message(
                                self.parser.column,
                                self.parser.line,
                                &format!("Ast: Function name not defined: {}", d),
                            ))
                        }
                    }
                }
                _ => {
                    // self.parser.back();
                    match self.symbol_table.lookup(&n) {
                        Some(i) => Atomic::Name(i.ctype.clone(), n),
                        None => {
                            return Err(AstError::with_message(
                                self.parser.column,
                                self.parser.line,
                                &format!("Ast: Variable name not defined: {}", n),
                            ))
                        }
                    }
                }
            },
            Token {
                kind: TokenKind::CString,
                extra: TokenExtra::CString(s),
                ..
            } => {
                self.parser.get_token();
                Atomic::CString(s)
            }

            e => {
                return Err(AstError::with_message(
                    self.parser.column,
                    self.parser.line,
                    &format!("Ast: atomic failed to consume token: {:?}", e),
                ))
            }
        };
        match self.parser.read_token().get_kind() {
            TokenKind::LBracket => {
                self.parser.get_token();
                let b = match self.expr() {
                    Ok(k) => k,
                    Err(e) => return Err(e.extend("Ast:Failed to parse bracket content")),
                };
                match b {
                    Expr::CInt(_) | Expr::Unary(..) | Expr::Binary(..) => (),
                    Expr::Atomic(TypeDecl::Int, _) => (),
                    e => {
                        return Err(AstError::with_message(
                            self.parser.column,
                            self.parser.line,
                            &format!(
                                "Ast: expression inside bracket should reduce to intiger, but {:?}",
                                e
                            ),
                        ))
                    }
                }
                match self.read_token().get_kind() {
                    TokenKind::RBracket => {
                        self.parser.get_token();
                        let r = Atomic::Accessor(Rc::new(base), Rc::new(b));
                        // match self.symbol_table.insert(name, r.get_type()) {
                        //     Ok(()) => (),
                        //     Err(e) => return Err(AstError::with_message(self.parser.column, self.parser.line, &e)),
                        // }
                        Ok(r)
                    }
                    e => Err(AstError::with_message(
                        self.parser.column,
                        self.parser.line,
                        &format!("Ast; right bracket missing: {:?}", e),
                    )),
                }
            }
            _ => {
                // match self.symbol_table.insert(name, base.get_type()) {
                //     Ok(()) => (),
                //     Err(e) => return Err(AstError::with_message(self.parser.column, self.parser.line, &e)),
                // }
                Ok(base)
            }
        }
    }

    pub fn expr(&mut self) -> Result<Expr, AstError> {
        let left = match self.read_token().get_kind() {
            TokenKind::Name | TokenKind::CString => {
                let a = self.atom()?;
                Expr::Atomic(a.get_type(),a)
            },
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
                use std::ops::Deref;
                self.parser.get_token();
                let a = self.expr()?;
                match a {
                    Expr::CBool(a) => Expr::CBool(!a), // !true or !false
                    Expr::Comparison(..)
                    | Expr::NilCheck(..)
                    | Expr::Atomic(TypeDecl::Bool,_) => Expr::Negation(Rc::new(a)),
                    Expr::Negation(e) => e.deref().clone(), // not not true => true

                    e => return Err(AstError::with_message(self.parser.column, self.parser.line, &format!("Ast: \"not\" should be followed by a boolean expression, but instead got: {:?}",e))),                    
                }
            },
            // Paranthesis reduces to the Expression inside it 
            TokenKind::LParenthesis => {
                self.parser.get_token();
                let c = self.expr()?;
                match self.read_token().get_kind() {
                    TokenKind::RParenthesis => {
                        self.parser.get_token();
                        c
                    }
                    e => return Err(AstError::with_message(self.parser.column, self.parser.line, &format!("Ast: Expected RParenthesis but got: {:?}",e))), 
                }

            }
            TokenKind::Addition => {
                self.parser.get_token();
                let c = self.expr()?;
                match c {
                    Expr::CInt(e) => Expr::CInt(e),
                    Expr::Binary(..) => Expr::Unary(TokenKind::Addition,Rc::new(c)),
                    e => return Err(AstError::with_message(self.parser.column, self.parser.line, &format!("Ast: \"+\" should be followed by a numeric expression, but instead: {:?}",e))),
                }
            },
            TokenKind::Subtraction => {
                self.parser.get_token();
                let c = self.expr()?;
                match c {
                    Expr::CInt(e) => Expr::CInt(-e),
                    Expr::Binary(..) => Expr::Unary(TokenKind::Subtraction,Rc::new(c)),
                    e => return Err(AstError::with_message(self.parser.column, self.parser.line, &format!("Ast: \"+\" should be followed by a numeric expression, but: {:?}",e))),
                }
            },
            TokenKind::KNew => {
                self.parser.get_token();
                let t = self.partial_var_type()?;
                // self.parser.get_token();
                let exp = self.expr()?;
                match  exp.get_type() {
                    TypeDecl::Int => (),
                    e => return Err(AstError::with_message(self.parser.column, self.parser.line, &format!("Ast: new syntaxt required integer inside the arguments, but: {:?}",e))),
                };
                match self.read_token().get_kind() {
                    TokenKind::RBracket => {
                        self.parser.get_token();
                        return Ok(Expr::NewArray(t, Rc::new(exp)));
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
                    let ctype = match exp.clone() {
                        Expr::Atomic(TypeDecl::List(t),_) => t.as_ref().clone(),
                        Expr::Hash(t,..) => t,
                        Expr::Tail(TypeDecl::List(t),..) => t.as_ref().clone(),
                        e => return Err(AstError::with_message(self.parser.column, self.parser.line, &format!("Ast: Head function expects List of something, but: {:?}",e))),

                    };
                    match self.read_token().get_kind() {
                        TokenKind::RParenthesis => {
                            self.parser.get_token();
                            Expr::Head(ctype, Rc::new(exp))
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
                    let ctype = match exp.clone() {
                        Expr::Atomic(TypeDecl::List(t),_) => TypeDecl::List(t),
                        Expr::Hash(t,..) => t,
                        Expr::Tail(t,..) => t,
                        e => return Err(AstError::with_message(self.parser.column, self.parser.line, &format!("Ast: Tail function expects List of something, but: {:?}",e))),

                    };
                    match self.read_token().get_kind() {
                        TokenKind::RParenthesis => {
                            self.parser.get_token();
                            Expr::Tail(ctype,Rc::new(exp))
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
                    match exp {
                        Expr::Atomic(TypeDecl::List(_),_) => (),
                        Expr::CNil => (),
                        Expr::Hash(..) => (),
                        Expr::Tail(..) => (),
                        e => return Err(AstError::with_message(self.parser.column, self.parser.line, &format!("Ast: Tail function expects List of something, but: {:?}",e))),
                    };

                    match self.read_token().get_kind() {
                        TokenKind::RParenthesis => {
                            self.parser.get_token();
                            Expr::NilCheck(Rc::new(exp))
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
        match self.read_token().get_kind() {
            TokenKind::Addition
            | TokenKind::Subtraction
            | TokenKind::Multiplication
            | TokenKind::Division
            | TokenKind::KMod => {
                let a = self.read_token().get_kind();
                self.parser.get_token();
                let right = self.expr()?;
                match (left.get_type(),right.get_type()) {
                    (TypeDecl::Int,TypeDecl::Int) => (),
                    e => return Err(AstError::with_message(
                        self.parser.column,
                        self.parser.line,
                        &format!("Ast: Binary operations expect both left and right to integers expressions, but: (left, right) = {:?}", e),
                    )),
                }
                Ok(Expr::Binary(a, Rc::new(left), Rc::new(right)))
            }
            TokenKind::Hash => {
                self.parser.get_token();
                let right = self.expr()?;
                let ctype = match (left.get_type(), right.get_type()) {
                    (t, TypeDecl::List(e)) if t == *e => TypeDecl::List(e),
                    (t, TypeDecl::Void) => TypeDecl::List(Rc::new(t)),

                    e => return Err(AstError::with_message(
                        self.parser.column,
                        self.parser.line,
                        &format!("Ast: Hash operations expect left to be t and right list[t], but: (left, right) = {:?}", e),
                    )),

                };
                Ok(Expr::Hash(ctype, Rc::new(left), Rc::new(right)))
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
                match (left.get_type(),right.get_type()) {
                    (TypeDecl::Bool, TypeDecl::Bool) => (),
                    (TypeDecl::Int, TypeDecl::Int) => (),
                    (TypeDecl::Char, TypeDecl::Char) => (),
                    e =>  return Err(AstError::with_message(
                        self.parser.column,
                        self.parser.line,
                        &format!("Ast: Comparison operations expect left and right to both be the same type, but: (left, right) = {:?}", e),
                    )),
                }
                Ok(Expr::Comparison(a, Rc::new(left), Rc::new(right)))
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
                        "Ast: Expected LParenthesis next to function name",
                    ));
                }

                let args = self.formal_def()?;
                let a = match t.clone() {
                    Some(i) => self.symbol_table.insert(&name, i),
                    None => self.symbol_table.insert(&name, TypeDecl::Void),
                };
                match a {
                    Ok(()) => (),
                    Err(e) => {
                        return Err(AstError::with_message(
                            self.parser.column,
                            self.parser.line,
                            &e,
                        ))
                    }
                }
                Ok(FuncDecl {
                    rtype: t,
                    name,
                    arguments: args,
                })
            }
            e => Err(AstError::with_message(
                self.parser.column,
                self.parser.line,
                &format!("Ast: failed to find function decl: {:?}", e),
            )),
        }
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

                // let old_name = self.current_block_name.to_owned();
                // self.current_block_name = format!("{}::{}", self.current_block_name, name);
                self.symbol_table.open_scope(&name);
                if self.read_token().get_kind() != TokenKind::LParenthesis {
                    return Err(AstError::with_message(
                        self.parser.column,
                        self.parser.line,
                        "Ast: Expected LParenthesis next to function name",
                    ));
                }

                let args = self.formal_def()?;
                let a = match t.clone() {
                    Some(i) => self.symbol_table.insert(&name, i),
                    None => self.symbol_table.insert(&name, TypeDecl::Void),
                };

                match a {
                    Ok(()) => (),
                    Err(e) => {
                        return Err(AstError::with_message(
                            self.parser.column,
                            self.parser.line,
                            &e,
                        ))
                    }
                }

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
                // self.parser.get_token();
                // self.current_block_name = old_name;
                self.symbol_table.close_scope();
                let a = match t.clone() {
                    Some(i) => self.symbol_table.insert(&name, i),
                    None => self.symbol_table.insert(&name, TypeDecl::Void),
                };

                match a {
                    Ok(()) => (),
                    Err(e) => {
                        return Err(AstError::with_message(
                            self.parser.column,
                            self.parser.line,
                            &e,
                        ))
                    }
                }
                Ok(FuncDef {
                    rtype: t,
                    name,
                    arguments: args,
                    decls,
                    defs,
                    vars,
                    stmts,
                })
            }
            e => Err(AstError::with_message(
                self.parser.column,
                self.parser.line,
                &format!("Ast: failed to find function decl: {:?}", e),
            )),
        }
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
                            Atomic::FuncCall(_,n,a) => Ok(Stmt::Call(n,a)),
                            e => Err(AstError::with_message(self.parser.column, self.parser.line, &format!("An atomic expression is not a stmt unless it is a call, here it was: {:?}",e)))   
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
                Ok(Stmt::Return(Rc::new(self.expr()?)))
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
                match cond {
                    Expr::Comparison(..) | Expr::Negation(..) | Expr::NilCheck(..) => (),
                    e => return Err(AstError::with_message(self.parser.column, self.parser.line, &format!("Ast: condition of For statement should reduce to bool but instead got: {:?}",e))),
                }
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
                match cond {
                    Expr::Comparison(..) | Expr::Negation(..) | Expr::NilCheck(..) => (),
                    e => return Err(AstError::with_message(self.parser.column, self.parser.line, &format!("Ast: condition of If statement should reduce to bool but instead got: {:?}",e))),
                }
                let mut elseifs = ElseIfBlock::new();
                let mut elseblock = Vec::new();
                let mut stmts = Vec::new();
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
                while self.read_token().get_kind() != TokenKind::KElseif
                    && self.read_token().get_kind() != TokenKind::KElse
                    && self.read_token().get_kind() != TokenKind::KEnd
                {
                    stmts.push(self.stmt()?);
                }
                // self.parser.get_token();
                while self.read_token().get_kind() == TokenKind::KElseif {
                    self.parser.get_token();
                    let cond = self.expr()?;
                    match cond {
                        Expr::Comparison(..) | Expr::Negation(..) | Expr::NilCheck(..) => (),
                        e => return Err(AstError::with_message(self.parser.column, self.parser.line, &format!("Ast: condition of elif statement should reduce to bool but instead got: {:?}",e))),
                    }
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
                    stmts,
                    elseif: elseifs,
                    elseblock,
                })
            }
            e => Err(AstError::with_message(
                self.parser.column,
                self.parser.line,
                &format!(
                    "Ast failed to parse statement: {:?} with extra; {:?}",
                    e,
                    self.read_token()
                ),
            )),
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
            match self.parser.read_token().get_kind() {
                TokenKind::Semicolon => self.parser.get_token(),
                TokenKind::RParenthesis => {
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
        }
        self.parser.get_token();
        Ok(results)
    }
}
