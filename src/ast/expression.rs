use crate::ast::atomic::Atomic;
use crate::ast::TypeDecl;
use crate::error::Error;
use crate::parser::TokenKind;
use crate::parser::*;
use crate::symbol_table::SymbolTable;
use std::fmt::Display;

/// a basic coding block that represents an expression that return data of any type
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Atomic(TypeDecl, Atomic),

    CChar(char),
    CInt(i16),
    Unary(TokenKind, Option<Box<Expr>>),
    Binary(TokenKind, Option<Box<Expr>>, Option<Box<Expr>>),

    CBool(bool),
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
impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        let to_symbol = |&t| {
            match t {
                TokenKind::Addition => "+",
                TokenKind::Subtraction => "-",
                TokenKind::Multiplication => "*",
                TokenKind::Division => "/",
                TokenKind::KMod => "%",
                TokenKind::KOr => "||",
                TokenKind::KAnd => "&&",
                TokenKind::LessOrEqual => "<=",
                TokenKind::Less => "<",
                TokenKind::Equal => "=",
                TokenKind::NotEqual => "<>",
                TokenKind::Great => ">",
                TokenKind::GreatOrEqual => ">=",
                _ => "?",
            }
            .to_owned()
        };
        write!(f, "(expr ").unwrap();
        match self {
            Expr::Atomic(_, _) => write!(f, "atomic"),
            Expr::CChar(c) => write!(f, "{}", c),
            Expr::CInt(n) => write!(f, "{}", n),
            Expr::Unary(t, Some(e)) => write!(f, "{} {}", to_symbol(t), e),
            Expr::Binary(t, Some(a), Some(b)) => write!(f, "{} {} {}", a, to_symbol(t), b),
            Expr::CBool(b) => write!(f, "{}", b),
            Expr::Comparison(t, Some(a), Some(b)) => write!(f, "({} {} {})", a, to_symbol(t), b),
            Expr::Logical(t, Some(a), Some(b)) => write!(f, "({} {} {})", a, to_symbol(t), b),
            Expr::Negation(Some(a)) => write!(f, "(not {})", a),
            Expr::NilCheck(a) => write!(f, "nil? {}", a),
            Expr::CNil => write!(f, "nil"),
            Expr::NewArray(t, a) => write!(f, "new {:?}[{}]", t, a),
            Expr::Hash(_, a, b) => write!(f, "({}#{})", a, b), // list creation head # tai => write!(f,"{}")l
            Expr::Head(_, a) => write!(f, "head({})", a),
            Expr::Tail(_, a) => write!(f, "tail({})", a),
            e => panic!("tryed to print invalid expression: {:?}", e),
        }
        .unwrap();
        write!(f, ")")
    }
}

impl Expr {
    fn bx(self) -> Box<Self> {
        Box::new(self)
    }
    pub fn is_valid(&self) -> bool {
        match self {
            Expr::Atomic(_, _) => true,
            Expr::CInt(_) => true,
            Expr::CChar(_) => true,
            Expr::CBool(_) => true,
            Expr::Unary(_, b) => b.is_some() && b.as_ref().unwrap().is_valid(),
            Expr::Binary(_, a, b) => {
                a.is_some()
                    && a.as_ref().unwrap().is_valid()
                    && b.is_some()
                    && b.as_ref().unwrap().is_valid()
            }
            Expr::Logical(_, a, b) => {
                a.is_some()
                    && a.as_ref().unwrap().is_valid()
                    && b.is_some()
                    && b.as_ref().unwrap().is_valid()
            }
            Expr::Negation(b) => b.is_some() && b.as_ref().unwrap().is_valid(),
            Expr::Comparison(_, a, b) => {
                a.is_some()
                    && a.as_ref().unwrap().is_valid()
                    && b.is_some()
                    && b.as_ref().unwrap().is_valid()
            }
            Expr::NilCheck(a) => a.is_valid(),
            Expr::CNil => true,
            Expr::NewArray(_, s) => s.is_valid(),
            Expr::Hash(_, a, b) => a.is_valid() && b.is_valid(),
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

    /// Generic Expression trie matching
    ///
    /// this function takes 2 expressions and tries to apply the rgiht expression to the left.
    /// In case of failure it returns the left Expression
    pub fn match_expr(left: Option<Box<Expr>>, right: Expr) -> Result<Expr, Expr> {
        if left.is_none() {
            return Ok(right);
        }
        let left = left.unwrap();
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
        // All the cases possible some more
        match ((*left).clone(), right) {
            // Bool is valid on the left only when followed by a comparison operator
            (
                Expr::Comparison(t, Some(a), None),
                Expr::Binary(TokenKind::Subtraction, None, None),
            ) => Ok(Expr::Comparison(
                t,
                Some(a),
                Some(Expr::Unary(TokenKind::Subtraction, None).bx()),
            )),
            (Expr::Logical(t, Some(a), None), Expr::Binary(TokenKind::Subtraction, None, None)) => {
                Ok(Expr::Logical(
                    t,
                    Some(a),
                    Some(Expr::Unary(TokenKind::Subtraction, None).bx()),
                ))
            }
            (Expr::Unary(t, None), a) if a.get_type() == TypeDecl::Int => {
                Ok(Expr::Unary(t, Some(a.bx())))
            }
            (a, Expr::Binary(t, None, None)) if a.get_type() == TypeDecl::Int => {
                Ok(Expr::Binary(t, Some(a.bx()), None))
            }
            (Expr::Binary(t, Some(a), b), Expr::Unary(t0, c)) => {
                match Expr::match_expr(b, Expr::Unary(t0, c)) {
                    Ok(k) => Ok(Expr::Binary(t, Some(a), Some(k.bx()))),
                    Err(_) => Err(*left),
                }
            }
            (Expr::Binary(t0, Some(a), None), b) if b.get_type() == TypeDecl::Int => {
                Ok(Expr::Binary(t0, Some(a), Some(b.bx())))
            }

            (Expr::Binary(t0, Some(a), b), Expr::Binary(t, c, d)) if low(t0) && high(t) => {
                match Expr::match_expr(b, Expr::Binary(t, c, d)) {
                    Ok(k) => Ok(Expr::Binary(t0, Some(a), Some(k.bx()))),
                    Err(_) => Err(*left),
                }
            }

            (Expr::Binary(t, Some(a), b), Expr::CInt(n)) => {
                match Expr::match_expr(b, Expr::CInt(n)) {
                    Ok(k) => Ok(Expr::Binary(t, Some(a), Some(k.bx()))),
                    Err(_) => Err(*left),
                }
            }
            (Expr::Binary(t0, Some(a), Some(b)), Expr::Binary(t, None, None)) => Ok(Expr::Binary(
                t,
                Some(Expr::Binary(t0, Some(a), Some(b)).bx()),
                None,
            )),

            (Expr::Negation(a), b) if !is_logical(b.clone()) => match Expr::match_expr(a, b) {
                Ok(k) => Ok(Expr::Negation(Some(k.bx()))),
                Err(_) => Err(*left),
            },
            (Expr::Logical(TokenKind::KOr, a, b), Expr::Logical(TokenKind::KAnd, None, None)) => {
                match Expr::match_expr(b, Expr::Logical(TokenKind::KAnd, None, None)) {
                    Ok(k) => Ok(Expr::Logical(TokenKind::KOr, a, Some(k.bx()))),
                    Err(_) => Err(*left),
                }
            }
            (a, Expr::Logical(t, None, None)) if a.get_type() == TypeDecl::Bool => {
                Ok(Expr::Logical(t, Some(a.bx()), None))
            }
            (Expr::Logical(t, Some(a), None), Expr::Negation(None)) => {
                Ok(Expr::Logical(t, Some(a), Some(Expr::Negation(None).bx())))
            }
            (Expr::Logical(t, Some(a), Some(b)), Expr::Comparison(t0, None, None)) => Ok(
                Expr::Logical(t, Some(a), Some(Expr::Comparison(t0, Some(b), None).bx())),
            ),
            (Expr::Logical(t, Some(a), b), c) => match Expr::match_expr(b, c) {
                Ok(k) => Ok(Expr::Logical(t, Some(a), Some(k.bx()))),
                Err(_) => Err(*left),
            },
            (e, Expr::Comparison(t, None, None)) => Ok(Expr::Comparison(t, Some(e.bx()), None)),
            (Expr::Comparison(t, Some(a), None), e) if a.get_type() == e.get_type() => {
                Ok(Expr::Comparison(t, Some(a), Some(e.bx())))
            }
            (Expr::Comparison(t, Some(a), b), Expr::Binary(t0, c, d)) => {
                match Expr::match_expr(b, Expr::Binary(t0, c, d)) {
                    Ok(k) => Ok(Expr::Comparison(t, Some(a), Some(k.bx()))),
                    Err(_) => Err(*left),
                }
            }
            (Expr::Comparison(t, Some(a), b), Expr::Unary(t0, c)) => {
                match Expr::match_expr(b, Expr::Unary(t0, c)) {
                    Ok(k) => Ok(Expr::Comparison(t, Some(a), Some(k.bx()))),
                    Err(_) => Err(*left),
                }
            }
            (Expr::Comparison(t, Some(a), b), Expr::CInt(n)) => {
                match Expr::match_expr(b, Expr::CInt(n)) {
                    Ok(k) => Ok(Expr::Comparison(t, Some(a), Some(k.bx()))),
                    Err(_) => Err(*left),
                }
            }
            _ => Err(*left),
        }
    }
    fn reduce(expr: Expr) -> Expr {
        match expr {
            Expr::Unary(t, Some(e)) => {
                let l = Expr::reduce(*e);
                match (t, &l) {
                    (TokenKind::Addition, &Expr::CInt(n)) => Expr::CInt(n),
                    (TokenKind::Subtraction, &Expr::CInt(n)) => Expr::CInt(-n),
                    _ => Expr::Unary(t, Some(l.bx())),
                }
            }
            Expr::Binary(t, Some(a), Some(b)) => {
                let x = Expr::reduce(*a);
                let y = Expr::reduce(*b);
                match (&t, &x, &y) {
                    (TokenKind::Addition, &Expr::CInt(f), &Expr::CInt(s)) => {
                        Expr::CInt(f.wrapping_add(s))
                    }
                    (TokenKind::Subtraction, &Expr::CInt(f), &Expr::CInt(s)) => {
                        Expr::CInt(f.wrapping_sub(s))
                    }
                    (TokenKind::Multiplication, &Expr::CInt(f), &Expr::CInt(s)) => {
                        Expr::CInt(f.wrapping_mul(s))
                    }
                    (TokenKind::Division, &Expr::CInt(f), &Expr::CInt(s)) => {
                        Expr::CInt(f.wrapping_div(s))
                    }
                    (TokenKind::KMod, &Expr::CInt(f), &Expr::CInt(s)) => {
                        Expr::CInt(f.wrapping_rem(s))
                    }
                    _ => Expr::Binary(t, Some(x.bx()), Some(y.bx())),
                }
            }
            Expr::Negation(Some(a)) => {
                let x = Expr::reduce(*a);
                match &x {
                    Expr::CBool(t) => Expr::CBool(!t),
                    _ => Expr::Negation(Some(x.bx())),
                }
            }
            Expr::Comparison(t, Some(a), Some(b)) => {
                let x = Expr::reduce(*a);
                let y = Expr::reduce(*b);
                match (&t, &x, &y) {
                    (TokenKind::Less, &Expr::CBool(f), &Expr::CBool(s)) => Expr::CBool(f < s),
                    (TokenKind::LessOrEqual, &Expr::CBool(f), &Expr::CBool(s)) => {
                        Expr::CBool(f <= s)
                    }
                    (TokenKind::Equal, &Expr::CBool(f), &Expr::CBool(s)) => Expr::CBool(f == s),
                    (TokenKind::NotEqual, &Expr::CBool(f), &Expr::CBool(s)) => Expr::CBool(f != s),
                    (TokenKind::Great, &Expr::CBool(f), &Expr::CBool(s)) => Expr::CBool(f > s),
                    (TokenKind::GreatOrEqual, &Expr::CBool(f), &Expr::CBool(s)) => {
                        Expr::CBool(f >= s)
                    }

                    (TokenKind::Less, &Expr::CInt(f), &Expr::CInt(s)) => Expr::CBool(f < s),
                    (TokenKind::LessOrEqual, &Expr::CInt(f), &Expr::CInt(s)) => Expr::CBool(f <= s),
                    (TokenKind::Equal, &Expr::CInt(f), &Expr::CInt(s)) => Expr::CBool(f == s),
                    (TokenKind::NotEqual, &Expr::CInt(f), &Expr::CInt(s)) => Expr::CBool(f != s),
                    (TokenKind::Great, &Expr::CInt(f), &Expr::CInt(s)) => Expr::CBool(f > s),
                    (TokenKind::GreatOrEqual, &Expr::CInt(f), &Expr::CInt(s)) => {
                        Expr::CBool(f >= s)
                    }

                    (TokenKind::Less, &Expr::CChar(f), &Expr::CChar(s)) => Expr::CBool(f < s),
                    (TokenKind::LessOrEqual, &Expr::CChar(f), &Expr::CChar(s)) => {
                        Expr::CBool(f <= s)
                    }
                    (TokenKind::Equal, &Expr::CChar(f), &Expr::CChar(s)) => Expr::CBool(f == s),
                    (TokenKind::NotEqual, &Expr::CChar(f), &Expr::CChar(s)) => Expr::CBool(f != s),
                    (TokenKind::Great, &Expr::CChar(f), &Expr::CChar(s)) => Expr::CBool(f > s),
                    (TokenKind::GreatOrEqual, &Expr::CChar(f), &Expr::CChar(s)) => {
                        Expr::CBool(f >= s)
                    }

                    _ => Expr::Comparison(t, Some(x.bx()), Some(y.bx())),
                }
            }
            Expr::Logical(t, Some(a), Some(b)) => {
                let x = Expr::reduce(*a);
                let y = Expr::reduce(*b);
                match (&t, &x, &y) {
                    (TokenKind::KAnd, &Expr::CBool(f), &Expr::CBool(s)) => Expr::CBool(f && s),
                    (TokenKind::KOr, &Expr::CBool(f), &Expr::CBool(s)) => Expr::CBool(f || s),
                    _ => Expr::Comparison(t, Some(x.bx()), Some(y.bx())),
                }
            }

            // Incase the expression is not reducable the just return it
            e => e,
        }
    }

    /// Generate a new Expression from a parser and a symbol_table
    pub fn generate(
        parser: &mut Parser,
        symbol_table: &mut SymbolTable<TypeDecl>,
        is_paranthesis: bool,
    ) -> Result<Expr, Error> {
        let mut result: Option<Box<Expr>> = None;
        // let current_error;
        loop {
            let token = parser.read_token();
            let right = match token.get_kind() {
                TokenKind::KTrue => {
                    parser.advance_token();
                    Expr::CBool(true)
                }
                TokenKind::KFalse => {
                    parser.advance_token();
                    Expr::CBool(false)
                }
                TokenKind::Addition | TokenKind::Subtraction => {
                    let token = parser.read_token().get_kind();
                    parser.advance_token();
                    if result.is_none() {
                        Expr::Unary(token, None)
                    } else {
                        Expr::Binary(token, None, None)
                    }
                }
                TokenKind::Name => {
                    let n = parser.read_token().get_name().unwrap();
                    let t = symbol_table.lookup(n);
                    if t.is_some() && t.unwrap() == &TypeDecl::Void {
                        // function that returns void
                        break;
                    } else {
                        if result.is_some() && result.as_ref().unwrap().is_valid() {
                            break;
                        }
                        let atom = Atomic::generate(parser, symbol_table)?;

                        Expr::Atomic(atom.get_type(), atom)
                    }
                }
                TokenKind::CString => {
                    let atom = Atomic::generate(parser, symbol_table)?;
                    Expr::Atomic(atom.get_type(), atom)
                }
                TokenKind::CChar => {
                    let c = Expr::CChar(token.get_cchar().unwrap());
                    parser.advance_token();
                    c
                }
                TokenKind::INT => {
                    let n = Expr::CInt(token.get_int().unwrap());
                    parser.advance_token();
                    n
                }
                TokenKind::Multiplication | TokenKind::Division | TokenKind::KMod => {
                    let kind = token.get_kind();
                    parser.advance_token();
                    Expr::Binary(kind, None, None)
                }
                TokenKind::KAnd | TokenKind::KOr => {
                    let kind = token.get_kind();
                    parser.advance_token();
                    Expr::Logical(kind, None, None)
                }
                TokenKind::Equal
                | TokenKind::NotEqual
                | TokenKind::Less
                | TokenKind::LessOrEqual
                | TokenKind::Great
                | TokenKind::GreatOrEqual => {
                    let kind = token.get_kind();
                    parser.advance_token();
                    Expr::Comparison(kind, None, None)
                }
                TokenKind::KNot => {
                    parser.advance_token();
                    Expr::Negation(None)
                }
                TokenKind::LParenthesis => {
                    parser.advance_token();
                    let r = Expr::generate(parser, symbol_table, true)?;
                    match Expr::match_expr(result, r) {
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
                TokenKind::RBracket => {
                    if result.is_none() {
                        return Err(Error::with_message(
                            parser.column,
                            parser.line,
                            "Rbracket without anything",
                            "Expr",
                        ));
                    }
                    let r = result.unwrap();
                    if !r.is_valid() {
                        return Err(Error::with_message(
                            parser.column,
                            parser.line,
                            &format!("Invalid Expression followed by RBracket: {:?}", r),
                            "Expr",
                        ));
                    }
                    return Ok(*r);
                }
                TokenKind::RParenthesis => {
                    if result.is_none() {
                        return Err(Error::with_message(
                            parser.column,
                            parser.line,
                            "RParenthesis without anything",
                            "Expr",
                        ));
                    }
                    let r = result.unwrap();
                    if !r.is_valid() || !is_paranthesis {
                        return Err(Error::with_message(
                            parser.column,
                            parser.line,
                            &format!("Invalid Expression inside parenthesis: {:?}", r),
                            "Expr",
                        ));
                    }
                    parser.advance_token();
                    let a = Expr::reduce(*r);
                    return Ok(a);
                }
                TokenKind::Empty => {
                    if result.is_some() {
                        let r = result.unwrap();
                        if r.is_valid() {
                            let a = Expr::reduce(*r);
                            return Ok(a);
                        } else {
                            return Err(Error::with_message(
                                parser.column,
                                parser.line,
                                "invalid expression with empty",
                                "Expr",
                            ));
                        }
                    } else {
                        return Err(Error::with_message(
                            parser.column,
                            parser.line,
                            "invalid expression with empty None",
                            "Expr",
                        ));
                    }
                }
                TokenKind::KNil => {
                    if result.is_none() {
                        parser.advance_token();
                        Expr::CNil
                    } else {
                        return Err(Error::with_message(
                            parser.column,
                            parser.line,
                            &format!("nil shouldn't follow {:?}", result.unwrap()),
                            "Expr",
                        ));
                    }
                }
                TokenKind::KNilQ => {
                    if parser.advance_token().get_kind() == TokenKind::LParenthesis {
                        // parser.get_token();
                        let s = Expr::generate(parser, symbol_table, false)?;
                        match s {
                            Expr::CNil => return Ok(Expr::CBool(true)),
                            _ => (),
                        }
                        match s.get_type() {
                            TypeDecl::List(_) => Expr::NilCheck(s.bx()),
                            e => {
                                return Err(Error::with_message(
                                    parser.column,
                                    parser.line,
                                    &format!("nil shouldn't follow {:?} h", e),
                                    "Expr",
                                ))
                            }
                        }
                    } else {
                        return Err(Error::with_message(
                            parser.column,
                            parser.line,
                            "nil? should be followed by left parenthesis",
                            "Expr",
                        ));
                    }
                }
                TokenKind::KHead => {
                    if parser.advance_token().get_kind() == TokenKind::LParenthesis {
                        // parser.get_token();
                        let s = Expr::generate(parser, symbol_table, false)?;
                        match s {
                            Expr::CNil => {
                                return Err(Error::with_message(
                                    parser.column,
                                    parser.line,
                                    "cannot get the head of empty list",
                                    "Expr",
                                ))
                            }
                            _ => (),
                        }
                        match s.get_type() {
                            TypeDecl::List(t) => Expr::Head(*t, s.bx()),
                            e => {
                                return Err(Error::with_message(
                                    parser.column,
                                    parser.line,
                                    &format!("nil shouldn't follow {:?} h", e),
                                    "Expr",
                                ))
                            }
                        }
                    } else {
                        return Err(Error::with_message(
                            parser.column,
                            parser.line,
                            "nil? should be followed by left parenthesis",
                            "Expr",
                        ));
                    }
                }
                TokenKind::KTail => {
                    if parser.advance_token().get_kind() == TokenKind::LParenthesis {
                        // parser.get_token();
                        let s = Expr::generate(parser, symbol_table, false)?;
                        match s {
                            Expr::CNil => {
                                return Err(Error::with_message(
                                    parser.column,
                                    parser.line,
                                    "cannot get the tail of empty list",
                                    "Expr",
                                ))
                            }
                            _ => (),
                        }
                        match s.get_type() {
                            TypeDecl::List(t) => Expr::Tail(TypeDecl::List(t), s.bx()),
                            e => {
                                return Err(Error::with_message(
                                    parser.column,
                                    parser.line,
                                    &format!("nil shouldn't follow {:?} h", e),
                                    "Expr",
                                ))
                            }
                        }
                    } else {
                        return Err(Error::with_message(
                            parser.column,
                            parser.line,
                            "nil? should be followed by left parenthesis",
                            "Expr",
                        ));
                    }
                }
                TokenKind::KNew => {
                    parser.advance_token();
                    let ctype = TypeDecl::generate_partial(parser)?;
                    let s = Expr::generate(parser, symbol_table, is_paranthesis)?;
                    if parser.read_token().get_kind() == TokenKind::RBracket
                        && s.get_type() == TypeDecl::Int
                    {
                        parser.advance_token();
                        return Ok(Expr::NewArray(ctype, s.bx()));
                    } else {
                        return Err(Error::with_message(
                            parser.column,
                            parser.line,
                            "new array definition mistake",
                            "Expr",
                        ));
                    }
                }
                TokenKind::Hash => {
                    if result.is_some() {
                        let head = result.unwrap();
                        if head.is_valid() {
                            parser.advance_token();
                            let tail = Expr::generate(parser, symbol_table, false)?;
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
                                    return Err(Error::with_message(
                                        parser.column,
                                        parser.line,
                                        &format!("Invalid hash expration: {:?} # {:?}", head, tail),
                                        "Expr",
                                    ))
                                }
                            };
                        // return Ok(Expr::Hash(r.get_type(), Some(r.bx()),Some(t.bx())));
                        } else {
                            return Err(Error::with_message(
                                parser.column,
                                parser.line,
                                "invalid expression with empty",
                                "Expr",
                            ));
                        }
                    } else {
                        return Err(Error::with_message(
                            parser.column,
                            parser.line,
                            "invalid expression with empty None",
                            "Expr",
                        ));
                    }
                }
                e if result.is_none() => {
                    return Err(Error::with_message(
                        parser.column,
                        parser.line,
                        &format!("unexpected token: {:?}", e),
                        "Expr",
                    ))
                }
                _ => break,
            };

            match Expr::match_expr(result, right) {
                Ok(k) => {
                    result = Some(k.bx());
                }
                Err(p) => {
                    result = Some(p.bx());
                    parser.back();
                    break;
                }
            }
        }
        if result.is_some() {
            let r = result.unwrap();
            if r.is_valid() {
                let a = Expr::reduce(*r);
                Ok(a)
            } else {
                Err(Error::with_message(
                    parser.column,
                    parser.line,
                    "Invalid expression",
                    "Expr",
                ))
            }
        } else {
            Err(Error::with_message(
                parser.column,
                parser.line,
                "failed to parse expression",
                "Expr",
            ))
        }
    }
}
