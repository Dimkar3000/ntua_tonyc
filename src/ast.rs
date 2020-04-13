use crate::parser::Token;
use crate::parser::{Parser, TokenExtra, TokenKind};
use crate::symbol_table::SymbolTable;

#[derive(Debug)]
pub struct AstError {
    sub_error: Option<Box<AstError>>,
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
            sub_error: Some(Box::new(self)),
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
    Name(TypeDecl, String),
    CString(String),
    Accessor(Box<Atomic>, Box<Expr>),
    FuncCall(TypeDecl, String, Vec<Expr>),
}

impl Atomic {
    pub fn get_type(&self) -> TypeDecl {
        match self {
            Atomic::Name(t, _) => t.clone(),
            Atomic::CString(_) => TypeDecl::Array(Box::new(TypeDecl::Char)),
            Atomic::FuncCall(t, ..) => t.clone(),
            Atomic::Accessor(base, _) => base.get_type(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Atomic(TypeDecl, Atomic),

    CChar(char),
    // Parenthesis(TypeDecl, Box<Expr>),
    CInt(i16),
    Unary(TokenKind, Option<Box<Expr>>),
    Binary(TokenKind, Option<Box<Expr>>, Option<Box<Expr>>),

    CBool(bool),
    // Unary(TokenKind::Subtraction,Option<Box<Expr>>),
    Comparison(TokenKind, Option<Box<Expr>>, Option<Box<Expr>>),
    Logical(TokenKind, Option<Box<Expr>>, Option<Box<Expr>>),
    Negation(Option<Box<Expr>>),
    NilCheck(Box<Expr>),

    CNil, // expty list of any type
    NewArray(TypeDecl, Box<Expr>),
    Hash(TypeDecl, Box<Expr>, Box<Expr>), // list creation head # tail
    Head(TypeDecl, Box<Expr>),
    Tail(TypeDecl, Box<Expr>),
}

impl Expr {
    pub fn is_valid(&self) -> bool {
        match self {
            Expr::Atomic(_, _) => true,
            Expr::CInt(_) => true,
            Expr::CChar(_) => true,
            Expr::CBool(_) => true,
            Expr::Unary(_, e) => e.is_some() && e.as_ref().unwrap().is_valid(),
            Expr::Binary(_, a, b) => {
                a.is_some()
                    && a.as_ref().unwrap().is_valid()
                    && b.is_some()
                    && b.as_ref().unwrap().is_valid()
            }
            // Expr::Unary(TokenKind::Subtraction,e) => e.is_some() && e.as_ref().unwrap().is_valid(),
            Expr::Logical(_, a, b) => {
                a.is_some()
                    && a.as_ref().unwrap().get_type() == TypeDecl::Bool
                    && b.is_some()
                    && b.as_ref().unwrap().get_type() == TypeDecl::Bool
                    && a.as_ref().unwrap().is_valid()
                    && b.as_ref().unwrap().is_valid()
            }
            Expr::Negation(a) => {
                a.is_some()
                    && a.as_ref().unwrap().get_type() == TypeDecl::Bool
                    && a.as_ref().unwrap().is_valid()
            }
            Expr::Comparison(_, a, b) => {
                a.is_some()
                    && b.is_some()
                    && a.as_ref().unwrap().get_type() == b.as_ref().unwrap().get_type()
                    && a.as_ref().unwrap().is_valid()
                    && b.as_ref().unwrap().is_valid()
            }
            Expr::NilCheck(a) => a.is_valid(),
            Expr::CNil => true, // expty list of any type
            Expr::NewArray(_, s) => s.is_valid(),
            Expr::Hash(_, a, b) => a.is_valid() && b.is_valid(), // list creation head # tail
            Expr::Head(_, b) => b.is_valid(),
            Expr::Tail(_, b) => b.is_valid(),
        }
    }

    pub fn get_type(&self) -> TypeDecl {
        match self {
            Expr::Atomic(t, _) => t.clone(),
            Expr::CInt(_) => TypeDecl::Int,
            Expr::CChar(_) => TypeDecl::Char,
            Expr::CBool(_) => TypeDecl::Bool,
            Expr::Unary(..) => TypeDecl::Int,
            Expr::Binary(..) => TypeDecl::Int,
            Expr::Logical(..) => TypeDecl::Bool,
            Expr::Negation(..) => TypeDecl::Bool,
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
    pub symbol_table: SymbolTable<TypeDecl>,
}

impl Expr {
    pub fn bx(self) -> Box<Self> {
        Box::new(self)
    }
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

        result.symbol_table.insert("geti", TypeDecl::Int).unwrap();
        result.symbol_table.insert("getb", TypeDecl::Bool).unwrap();
        result.symbol_table.insert("getc", TypeDecl::Char).unwrap();
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
                            self.parser.get_token();
                            break;
                        }
                        let tmp = match self.expr(true) {
                            Ok(k) => k,
                            Err(e) => return Err(e.extend("Ast: Function Arguments failed")),
                        };

                        args.push(tmp);
                        match self.read_token().get_kind() {
                            TokenKind::Comma => (),
                            // TokenKind::RParenthesis => break,
                            e => {
                                if self.parser.previous.get_kind() == TokenKind::RParenthesis {
                                    // self.parser.get_token();
                                    break;
                                }
                                return Err(AstError::with_message(
                                    self.parser.column,
                                    self.parser.line,
                                    &format!(
                                        "Ast: Expected RParenthesis or comma, but got {:?}, {:?}",
                                        e,
                                        self.read_token()
                                    ),
                                ));
                            }
                        }
                    }
                    // self.parser.get_token();
                    let d = n.to_string();
                    match self.symbol_table.lookup(n) {
                        Some(i) => Atomic::FuncCall(i.data.clone(), d, args),
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
                        Some(i) => Atomic::Name(i.data.clone(), n),
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
                let b = match self.expr(false) {
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
                        let r = Atomic::Accessor(Box::new(base), Box::new(b));
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

    pub fn simple_expr(&mut self) -> Result<Expr, AstError> {
        match self.read_token().get_kind() {
            TokenKind::Name | TokenKind::CString => {
                let a = self.atom()?;
                Ok(Expr::Atomic(a.get_type(), a))
            }
            TokenKind::KTrue => {
                self.parser.get_token();
                Ok(Expr::CBool(true))
            }
            TokenKind::KFalse => {
                self.parser.get_token();
                Ok(Expr::CBool(false))
            }
            TokenKind::KNil => {
                self.parser.get_token();
                Ok(Expr::CNil)
            }
            TokenKind::CChar => {
                if let TokenExtra::Cchar(n) = self.read_token().extra {
                    self.parser.get_token();
                    Ok(Expr::CChar(n))
                } else {
                    return Err(AstError::with_message(
                        self.parser.column,
                        self.parser.line,
                        "Parser: Token wan CChar but extra wan't",
                    ));
                }
            }
            TokenKind::INT => {
                if let TokenExtra::Int(n) = self.read_token().extra {
                    self.parser.get_token();
                    Ok(Expr::CInt(n))
                } else {
                    return Err(AstError::with_message(
                        self.parser.column,
                        self.parser.line,
                        "Parser: Token wan Int but extra wan't Number",
                    ));
                }
            }
            e => Err(AstError::with_message(
                self.parser.column,
                self.parser.line,
                &format!(
                    "Ast: simple_expr() was called with incorrect token: {:?}",
                    e
                ),
            )),
        }
    }

    pub fn error<T, S: AsRef<str>>(&self, message: S) -> Result<T, AstError> {
        Err(AstError::with_message(
            self.parser.column,
            self.parser.line,
            message.as_ref(),
        ))
    }

    pub fn match_expr(&self, left: Option<Box<Expr>>, right: Expr) -> Result<Expr, Expr> {
        if left.is_none() {
            return Ok(right);
        }
        let left = left.unwrap();
        // high operator of binary and low in proriority
        let high = |t| {
            return t == TokenKind::Multiplication
                || t == TokenKind::Division
                || t == TokenKind::KMod;
        };
        let low = |t| return t == TokenKind::Addition || t == TokenKind::Subtraction;
        let is_logical = |e| match e {
            Expr::Logical(..) => true,
            _ => false,
        };
        // I expect right to either be a simple empression like a number or an empty operation
        match ((*left).clone(), right) {
            // Bool is valid on the left only when followed by a comparison operator
            (Expr::Unary(t, None), a) if a.get_type() == TypeDecl::Int => {
                Ok(Expr::Unary(t, Some(a.bx())))
            }
            (Expr::Binary(t, Some(a), b), Expr::Unary(t0, c)) => {
                match self.match_expr(b, Expr::Unary(t0, c)) {
                    Ok(k) => Ok(Expr::Binary(t, Some(a), Some(k.bx()))),
                    Err(_) => Err(*left),
                }
            }

            (Expr::Binary(t0, Some(a), b), Expr::Binary(t, c, d)) if low(t0) && high(t) => {
                match self.match_expr(b, Expr::Binary(t, c, d)) {
                    Ok(k) => Ok(Expr::Binary(t0, Some(a), Some(k.bx()))),
                    Err(_) => Err(*left),
                }
            }
            (a, Expr::Binary(t, None, None)) if a.get_type() == TypeDecl::Int => {
                Ok(Expr::Binary(t, Some(a.bx()), None))
            }

            (Expr::Binary(t, Some(a), b), Expr::CInt(n)) => match self.match_expr(b, Expr::CInt(n))
            {
                Ok(k) => Ok(Expr::Binary(t, Some(a), Some(k.bx()))),
                Err(_) => Err(*left),
            },
            (Expr::Binary(t0, Some(a), Some(b)), Expr::Binary(t, None, None)) => Ok(Expr::Binary(
                t,
                Some(Expr::Binary(t0, Some(a), Some(b)).bx()),
                None,
            )),

            (Expr::Negation(a), b) if !is_logical(b.clone()) => match self.match_expr(a, b) {
                Ok(k) => Ok(Expr::Negation(Some(k.bx()))),
                Err(_) => Err(*left),
            },
            (Expr::Logical(TokenKind::KOr, a, b), Expr::Logical(TokenKind::KAnd, None, None)) => {
                match self.match_expr(b, Expr::Logical(TokenKind::KAnd, None, None)) {
                    Ok(k) => Ok(Expr::Logical(TokenKind::KOr, a, Some(k.bx()))),
                    Err(_) => Err(*left),
                }
            }
            (a, Expr::Logical(t, None, None)) if a.get_type() == TypeDecl::Bool => {
                Ok(Expr::Logical(t, Some(a.bx()), None))
            }
            // (Expr::Logical(TokenKind::KOr, a,b), Expr::Logical(TokenKind::KAnd, None,None)) => match self.match_expr(b, Expr::Logical(TokenKind::KAnd, None,None)) {
            //     Ok(k)=> Ok(Expr::Logical(TokenKind::KOr, a,Some(k.bx()))),
            //     Err(_) => Err(*left),
            // },
            (Expr::Logical(t, Some(a), None), Expr::Negation(None)) => {
                Ok(Expr::Logical(t, Some(a), Some(Expr::Negation(None).bx())))
            }
            (Expr::Logical(t, Some(a), b), c) => match self.match_expr(b, c) {
                Ok(k) => Ok(Expr::Logical(t, Some(a), Some(k.bx()))),
                Err(_) => Err(*left),
            },

            (e, Expr::Comparison(t, None, None)) => Ok(Expr::Comparison(t, Some(e.bx()), None)),
            (Expr::Comparison(t, Some(a), None), e) if a.get_type() == e.get_type() => {
                Ok(Expr::Comparison(t, Some(a), Some(e.bx())))
            }
            (Expr::Comparison(t, Some(a), b), Expr::Binary(t0, c, d)) => {
                match self.match_expr(b, Expr::Binary(t0, c, d)) {
                    Ok(k) => Ok(Expr::Comparison(t, Some(a), Some(k.bx()))),
                    Err(_) => Err(*left),
                }
            }
            (Expr::Comparison(t, Some(a), b), Expr::Unary(t0, c)) => {
                match self.match_expr(b, Expr::Unary(t0, c)) {
                    Ok(k) => Ok(Expr::Comparison(t, Some(a), Some(k.bx()))),
                    Err(_) => Err(*left),
                }
            }
            // Ok(Expr::Comparison(t, Some(a),Some(Expr::Binary(t0,Some(b), None).bx()))),
            (Expr::Comparison(t, Some(a), b), Expr::CInt(n)) => {
                match self.match_expr(b, Expr::CInt(n)) {
                    Ok(k) => Ok(Expr::Comparison(t, Some(a), Some(k.bx()))),
                    Err(_) => Err(*left),
                }
            }
            _ => Err(*left),
        }
    }
    pub fn expr(&mut self, is_paranthesis: bool) -> Result<Expr, AstError> {
        let mut result: Option<Box<Expr>> = None;
        // let current_error;
        loop {
            let token = self.read_token();
            let right = match token.get_kind() {
                TokenKind::KTrue => {
                    self.parser.get_token();
                    Expr::CBool(true)
                }
                TokenKind::KFalse => {
                    self.parser.get_token();
                    Expr::CBool(false)
                }
                TokenKind::Addition | TokenKind::Subtraction => {
                    let token = self.read_token().get_kind();
                    self.parser.get_token();
                    if result.is_none() {
                        Expr::Unary(token, None)
                    } else {
                        Expr::Binary(token, None, None)
                    }
                }
                TokenKind::Name | TokenKind::CString => {
                    let atom = self.atom()?;

                    Expr::Atomic(atom.get_type(), atom)
                }
                TokenKind::CChar => {
                    self.parser.get_token();
                    match token.extra {
                        TokenExtra::Cchar(c) => Expr::CChar(c),
                        _ => unreachable!("char"),
                    }
                }
                TokenKind::INT => {
                    self.parser.get_token();
                    match token.extra {
                        TokenExtra::Int(n) => Expr::CInt(n),
                        _ => unreachable!("int"),
                    }
                }
                TokenKind::Multiplication | TokenKind::Division | TokenKind::KMod => {
                    let kind = token.get_kind();
                    self.parser.get_token();
                    Expr::Binary(kind, None, None)
                }
                TokenKind::KAnd | TokenKind::KOr => {
                    let kind = token.get_kind();
                    self.parser.get_token();
                    Expr::Logical(kind, None, None)
                }
                TokenKind::Equal
                | TokenKind::NotEqual
                | TokenKind::Less
                | TokenKind::LessOrEqual
                | TokenKind::Great
                | TokenKind::GreatOrEqual => {
                    let kind = token.get_kind();
                    self.parser.get_token();
                    Expr::Comparison(kind, None, None)
                }
                TokenKind::KNot => {
                    self.parser.get_token();
                    Expr::Negation(None)
                }
                TokenKind::LParenthesis => {
                    self.parser.get_token();
                    let r = self.expr(true)?;
                    match self.match_expr(result, r) {
                        Ok(k) => {
                            result = Some(k.bx());
                            continue;
                        }
                        Err(e) => {
                            result = Some(e.bx());
                            break;
                        }
                    }
                }
                TokenKind::RParenthesis => {
                    if result.is_none() {
                        return self.error("RParenthesis without anything");
                    }
                    let r = result.unwrap();
                    if !r.is_valid() {
                        return self
                            .error(format!("Invalid Expression inside parenthesis: {:?}", r));
                    }
                    self.parser.get_token();
                    return Ok(*r);
                }
                TokenKind::Empty => {
                    if result.is_some() {
                        let r = result.unwrap();
                        if r.is_valid() {
                            return Ok(*r);
                        } else {
                            return self.error("invalid expression with empty");
                        }
                    } else {
                        return self.error("invalid expression with empty None");
                    }
                }
                TokenKind::KNil => {
                    if result.is_none() {
                        self.parser.get_token();
                        Expr::CNil
                    } else {
                        return self.error(format!("nil shouldn't follow {:?}", result.unwrap()));
                    }
                }
                TokenKind::KNilQ => {
                    if self.parser.get_token().get_kind() == TokenKind::LParenthesis {
                        // self.parser.get_token();
                        let s = self.expr(false)?;
                        match s {
                            Expr::CNil => return Ok(Expr::CBool(true)),
                            _ => (),
                        }
                        match s.get_type() {
                            TypeDecl::List(_) => Expr::NilCheck(s.bx()),
                            e => return self.error(format!("nil shouldn't follow {:?} h", e)),
                        }
                    } else {
                        return self.error("nil? should be followed by left parenthesis");
                    }
                }
                TokenKind::KHead => {
                    if self.parser.get_token().get_kind() == TokenKind::LParenthesis {
                        // self.parser.get_token();
                        let s = self.expr(false)?;
                        match s {
                            Expr::CNil => return self.error("cannot get the head of empty list"),
                            _ => (),
                        }
                        match s.get_type() {
                            TypeDecl::List(t) => Expr::Head(*t, s.bx()),
                            e => return self.error(format!("nil shouldn't follow {:?} h", e)),
                        }
                    } else {
                        return self.error("nil? should be followed by left parenthesis");
                    }
                }
                TokenKind::KTail => {
                    if self.parser.get_token().get_kind() == TokenKind::LParenthesis {
                        // self.parser.get_token();
                        let s = self.expr(false)?;
                        match s {
                            Expr::CNil => return self.error("cannot get the tail of empty list"),
                            _ => (),
                        }
                        match s.get_type() {
                            TypeDecl::List(t) => Expr::Tail(TypeDecl::List(t), s.bx()),
                            e => return self.error(format!("nil shouldn't follow {:?} h", e)),
                        }
                    } else {
                        return self.error("nil? should be followed by left parenthesis");
                    }
                }
                TokenKind::KNew => {
                    self.parser.get_token();
                    let ctype = self.partial_var_type()?;
                    let s = self.expr(is_paranthesis)?;
                    if self.read_token().get_kind() == TokenKind::RBracket
                        && s.get_type() == TypeDecl::Int
                    {
                        self.parser.get_token();
                        return Ok(Expr::NewArray(ctype, s.bx()));
                    } else {
                        return self.error("new array definition mistake");
                    }
                }
                TokenKind::Hash => {
                    if result.is_some() {
                        let head = result.unwrap();
                        if head.is_valid() {
                            self.parser.get_token();
                            let tail = self.expr(false)?;
                            match tail.get_type() {
                                TypeDecl::Nil => {
                                    return Ok(Expr::Hash(
                                        TypeDecl::List(Box::new(head.get_type())),
                                        head.bx(),
                                        tail.bx(),
                                    ))
                                }

                                TypeDecl::List(t) if Box::new(head.get_type()) == t => {
                                    return Ok(Expr::Hash(TypeDecl::List(t), head.bx(), tail.bx()))
                                }

                                _ => {
                                    return self.error(format!(
                                        "Invalid hash expration: {:?} # {:?}",
                                        head, tail
                                    ))
                                }
                            };
                        // return Ok(Expr::Hash(r.get_type(), Some(r.bx()),Some(t.bx())));
                        } else {
                            return self.error("invalid expression with empty");
                        }
                    } else {
                        return self.error("invalid expression with empty None");
                    }
                }
                e if result.is_none() => return self.error(format!("unexpected token: {:?}", e)),
                _ => break,
            };
            match self.match_expr(result, right) {
                Ok(k) => {
                    result = Some(k.bx());
                }
                Err(p) => {
                    result = Some(p.bx());
                    break;
                }
            }
        }
        if result.is_some() {
            let r = result.unwrap();
            if r.is_valid() {
                Ok(*r)
            } else {
                self.error("Ast: Invalid expression")
            }
        } else {
            self.error("failed to parse expression")
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
                            &format!("Ast Def: {}", e),
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
                let _ = self
                    .symbol_table
                    .insert(&name, t.clone().unwrap_or(TypeDecl::Void));
                self.symbol_table.open_scope(&name);
                if self.read_token().get_kind() != TokenKind::LParenthesis {
                    return Err(AstError::with_message(
                        self.parser.column,
                        self.parser.line,
                        "Ast: Expected LParenthesis next to function name",
                    ));
                }

                let args = self.formal_def()?;
                match self.read_token().get_kind() {
                    TokenKind::Colon => (),
                    e => {
                        return Err(AstError::with_message(
                            self.parser.column,
                            self.parser.line,
                            &format!("Ast: Function Definition end with colon, but got: {:?}", e),
                        ))
                    }
                };
                self.parser.get_token();

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
                            &format!("Ast Def: {}", e),
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
                // self.parser.get_token();
                while self.read_token().get_kind() != TokenKind::KEnd {
                    stmts.push(self.stmt()?);
                }
                self.parser.get_token();
                // self.parser.get_token();
                // self.current_block_name = old_name;
                self.symbol_table.close_scope();
                // let a = match t.clone() {
                //     Some(i) => self.symbol_table.insert(&name, i),
                //     None => self.symbol_table.insert(&name, TypeDecl::Void),
                // };

                // match a {
                //     Ok(()) => (),
                //     Err(e) => {
                //         return Err(AstError::with_message(
                //             self.parser.column,
                //             self.parser.line,

                //             &format!("Ast Def: {}",e),
                //         ))
                //     }
                // }
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
                        let expr = self.expr(false)?;
                        if atom.get_type() != expr.get_type() {
                            return Err(AstError::with_message(self.parser.column, self.parser.line, &format!("Ast: Assigment requires that the variable is the same type as the expression, but {:?}:={:?}",atom.get_type(),expr.get_type())));
                        }
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
                Ok(Stmt::Return(Box::new(self.expr(false)?)))
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
                        TokenKind::Comma => {
                            self.parser.get_token();
                            continue;
                        }
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
                let cond = self.expr(false)?;
                if cond.get_type() != TypeDecl::Bool {
                    return Err(AstError::with_message(self.parser.column, self.parser.line, &format!("Ast: condition of For statement should reduce to bool but instead got: {:?}",cond)));
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
                        TokenKind::Comma => {
                            self.parser.get_token();
                            continue;
                        }
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
                let cond = self.expr(false)?;
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
                    let cond = self.expr(false)?;
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
        if self.read_token().get_kind() == TokenKind::RParenthesis {
            self.parser.get_token();
            return Ok(Vec::new());
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
        Ok(results)
    }
}
