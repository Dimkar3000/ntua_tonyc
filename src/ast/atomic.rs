use crate::ast::Expr;
use crate::ast::TypeDecl;
use crate::error::Error;
use crate::parser::{Parser, Token, TokenExtra, TokenKind};
use crate::symbol_table::SymbolTable;

#[derive(Debug, Clone)]
pub enum Atomic {
    Name(TypeDecl, String),
    CString(String),
    Accessor(Box<Atomic>, Box<Expr>),
    FuncCall(TypeDecl, String, Vec<Expr>),
}

impl Atomic {
    pub fn get_name(&self) -> String {
        match self {
            Atomic::Name(_, s) => s.clone(),
            Atomic::CString(s) => s.clone(),
            Atomic::Accessor(a, _) => a.get_name(),
            Atomic::FuncCall(_, s, _) => s.clone(),
        }
    }
    pub fn get_type(&self) -> TypeDecl {
        match self {
            Atomic::Name(t, _) => t.clone(),
            Atomic::CString(_) => TypeDecl::Array(Box::new(TypeDecl::Char)),
            Atomic::FuncCall(t, ..) => t.clone(),
            Atomic::Accessor(base, _) => {
                if let TypeDecl::Array(t) = base.get_type() {
                    *t
                } else {
                    unreachable!()
                }
            }
        }
    }

    pub fn generate(
        parser: &mut Parser,
        symbol_table: &mut SymbolTable<TypeDecl>,
    ) -> Result<Atomic, Error> {
        let base = match parser.read_token() {
            Token {
                kind: TokenKind::Name,
                extra: TokenExtra::Name(n),
                ..
            } => {
                let name = n.to_string();
                match parser.advance_token().kind {
                    TokenKind::LParenthesis => {
                        let mut args = Vec::new();
                        loop {
                            parser.advance_token();

                            if parser.read_token().get_kind() == TokenKind::RParenthesis {
                                parser.advance_token();
                                break;
                            }
                            let exp = Expr::generate(parser, symbol_table, true);
                            let tmp = match exp {
                                Ok(k) => k,
                                Err(e) => return Err(e.extend("Function Arguments failed", "Ast")),
                            };

                            args.push(tmp);
                            match parser.read_token().get_kind() {
                                TokenKind::Comma => (),
                                // TokenKind::RParenthesis => break,
                                e => {
                                    if parser.previous_token().get_kind() == TokenKind::RParenthesis
                                    {
                                        // parser.get_token();
                                        break;
                                    }
                                    return Err(Error::with_message(
                                        parser.column,
                                        parser.line,
                                        &format!(
                                            "Expected RParenthesis or comma, but got {:?}, {:?}",
                                            e,
                                            parser.read_token()
                                        ),
                                        "Ast",
                                    ));
                                }
                            }
                        }
                        // parser.get_token();
                        match symbol_table.lookup(&name) {
                            Some(i) => Atomic::FuncCall(i.clone(), name, args),
                            None => {
                                return Err(Error::with_message(
                                    parser.column,
                                    parser.line,
                                    &format!("Function name not defined: {}", name),
                                    "Ast",
                                ))
                            }
                        }
                    }
                    _ => {
                        // parser.back();
                        match symbol_table.lookup(&name) {
                            Some(i) => Atomic::Name(i.clone(), name),
                            None => {
                                return Err(Error::with_message(
                                    parser.column,
                                    parser.line,
                                    &format!("Variable name not defined: {}", name),
                                    "Ast",
                                ))
                            }
                        }
                    }
                }
            }
            Token {
                kind: TokenKind::CString,
                extra: TokenExtra::CString(s),
                ..
            } => {
                parser.advance_token();
                Atomic::CString(s)
            }

            e => {
                return Err(Error::with_message(
                    parser.column,
                    parser.line,
                    &format!("atomic failed to consume token: {:?}", e),
                    "Ast",
                ))
            }
        };
        match parser.read_token().get_kind() {
            TokenKind::LBracket => {
                parser.advance_token();
                let b = match Expr::generate(parser, symbol_table, false) {
                    Ok(k) => k,
                    Err(e) => return Err(e.extend("Ast:Failed to parse bracket content", "Ast")),
                };
                match b {
                    Expr::CInt(_) | Expr::Unary(..) | Expr::Binary(..) => (),
                    Expr::Atomic(TypeDecl::Int, _) => (),
                    e => {
                        return Err(Error::with_message(
                            parser.column,
                            parser.line,
                            &format!(
                                "expression inside bracket should reduce to intiger, but {:?}",
                                e
                            ),
                            "Ast",
                        ))
                    }
                }
                match parser.read_token().get_kind() {
                    TokenKind::RBracket => {
                        parser.advance_token();
                        let r = Atomic::Accessor(Box::new(base), Box::new(b));
                        // match symbol_table.insert(name, r.get_type()) {
                        //     Ok(()) => (),
                        //     Err(e) => return Err(Error::with_message(parser.column, parser.line, &e)),
                        // }
                        Ok(r)
                    }
                    e => Err(Error::with_message(
                        parser.column,
                        parser.line,
                        &format!("Ast; right bracket missing: {:?}", e),
                        "Ast",
                    )),
                }
            }
            _ => {
                // match symbol_table.insert(name, base.get_type()) {
                //     Ok(()) => (),
                //     Err(e) => return Err(Error::with_message(parser.column, parser.line, &e)),
                // }
                Ok(base)
            }
        }
    }
}
