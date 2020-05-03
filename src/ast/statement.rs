use crate::ast::typedecl::TypeDecl;
use crate::ast::{Atomic, Expr};
use crate::error::Error;
use crate::parser::*;
use crate::symbol_table::SymbolTable;
use std::fmt::Display;
#[derive(Debug, Clone)]
pub enum Stmt {
    Exit,
    Return(Box<Expr>),
    If {
        condition: Expr,
        stmts: Vec<Stmt>,
        elseif: Vec<(Expr, Vec<Stmt>)>,
        elseblock: Vec<Stmt>,
    },
    For(Vec<Stmt>, Expr, Vec<Stmt>, Vec<Stmt>), // Note: First 2 vecs should contain only simple vec
    Skip,
    Assign(Atomic, Expr),
    Call(String, Vec<Expr>),
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Stmt::For(pre, cond, post, body) => {
                write!(f, "for ").unwrap();
                for i in pre {
                    write!(f, "{},", i).unwrap();
                }
                write!(f, ";{};", cond).unwrap();
                for i in post {
                    write!(f, "{},", i).unwrap();
                }
                write!(f, ": ").unwrap();
                for i in body {
                    write!(f, "{}\t", i).unwrap();
                }
                write!(f, "end")
            }
            Stmt::Exit => write!(f, "exit"),
            Stmt::Return(e) => write!(f, "return {}", e),
            Stmt::Skip => write!(f, "skip"),
            Stmt::Assign(a, b) => write!(f, "{} := {}", a, b),
            Stmt::Call(name, args) => {
                write!(f, "{}(", name).unwrap();
                for i in args {
                    write!(f, "{},", i).unwrap();
                }
                write!(f, ")")
            }
            Stmt::If {
                condition: e,
                stmts: s,
                elseif: elses,
                elseblock: elseb,
            } => {
                write!(f, "if {}: ", e).unwrap();
                for i in s {
                    write!(f, "{}\t", i).unwrap();
                }
                for i in elses {
                    write!(f, "elsif {}: ", i.0).unwrap();
                    for j in &i.1 {
                        write!(f, "{}\t", j).unwrap();
                    }
                }
                if !elseb.is_empty() {
                    write!(f, "else: ").unwrap();
                    for i in elseb {
                        write!(f, "{}\t", i).unwrap();
                    }
                }
                write!(f, "end")
            }
        }
    }
}

impl Stmt {
    pub fn generate(
        parser: &mut Parser,
        symbol_table: &mut SymbolTable<TypeDecl>,
    ) -> Result<Stmt, Error> {
        let t = parser.read_token();
        match t.get_kind() {
            TokenKind::Name => {
                let atom = Atomic::generate(parser, symbol_table)?;
                match parser.read_token().get_kind() {
                    TokenKind::Assignement => {
                        parser.advance_token();

                        let expr = Expr::generate(parser, symbol_table, false)?;
                        if atom.get_type() != expr.get_type()  {
                            return Err(Error::with_message(parser.column, parser.line, &format!("Assigment requires that the variable is the same type as the expression, but {:?}:={:?}",atom.get_type(),expr.get_type()),"Ast"));
                        }
                        Ok(Stmt::Assign(atom,expr))
                    },
                    _ => {
                        match atom {
                            Atomic::FuncCall(_,n,a) => Ok(Stmt::Call(n,a)),
                            e => Err(Error::with_message(parser.column, parser.line, &format!("An atomic expression is not a stmt unless it is a call, here it was: {:?}",e),"Ast"))   
                        }
                    }
                }
            }
            TokenKind::KSkip => {
                parser.advance_token();
                Ok(Stmt::Skip)
            }
            TokenKind::KExit => {
                parser.advance_token();
                Ok(Stmt::Exit)
            }
            TokenKind::KReturn => {
                parser.advance_token();
                Ok(Stmt::Return(Box::new(Expr::generate(
                    parser,
                    symbol_table,
                    false,
                )?)))
            }
            TokenKind::KFor => {
                parser.advance_token();
                let mut first_simples = Vec::new();
                loop {
                    let s = Stmt::generate(parser, symbol_table)?;
                    match s {
                        Stmt::Skip | Stmt::Assign(..) | Stmt::Call(..) => (),
                        e => {
                            return Err(Error::with_message(
                                parser.column,
                                parser.line,
                                &format!("Expected simple Statement but got: {:?}", e),
                                "Ast",
                            ))
                        }
                    }
                    first_simples.push(s);
                    match parser.read_token().get_kind() {
                        TokenKind::Comma => {
                            parser.advance_token();
                            continue;
                        }
                        TokenKind::Semicolon => break,
                        e => {
                            return Err(Error::with_message(
                                parser.column,
                                parser.line,
                                &format!("Expected \",\" or \";\" but got: {:?}", e),
                                "Ast",
                            ))
                        }
                    }
                }
                parser.advance_token();
                let cond = Expr::generate(parser, symbol_table, false)?;
                if cond.get_type() != TypeDecl::Bool {
                    return Err(Error::with_message(parser.column, parser.line, &format!("condition of For statement should reduce to bool but instead got: {:?}",cond),"Ast"));
                }
                match parser.read_token().get_kind() {
                    TokenKind::Semicolon => parser.advance_token(),
                    e => {
                        return Err(Error::with_message(
                            parser.column,
                            parser.line,
                            &format!("Expected simple Statement but got: {:?}", e),
                            "Ast",
                        ))
                    }
                };
                let mut second_simples = Vec::new();
                loop {
                    let s = Stmt::generate(parser, symbol_table)?;
                    match s {
                        Stmt::Skip | Stmt::Assign(..) | Stmt::Call(..) => (),
                        e => {
                            return Err(Error::with_message(
                                parser.column,
                                parser.line,
                                &format!("Expected simple Statement but got: {:?}", e),
                                "Ast",
                            ))
                        }
                    }
                    second_simples.push(s);
                    match parser.read_token().get_kind() {
                        TokenKind::Comma => {
                            parser.advance_token();
                            continue;
                        }
                        TokenKind::Colon => break,
                        e => {
                            return Err(Error::with_message(
                                parser.column,
                                parser.line,
                                &format!("Expected \",\" or \":\" but got: {:?}", e),
                                "Ast",
                            ))
                        }
                    }
                }
                parser.advance_token();
                let mut body = Vec::new();
                while parser.read_token().get_kind() != TokenKind::KEnd {
                    body.push(Stmt::generate(parser, symbol_table)?);
                }
                parser.advance_token();

                Ok(Stmt::For(first_simples, cond, second_simples, body))
            }
            TokenKind::KIf => {
                parser.advance_token();
                let cond = Expr::generate(parser, symbol_table, false)?;
                match cond {
                    Expr::CBool(..)
                    | Expr::Atomic(TypeDecl::Bool, _)
                    | Expr::Comparison(..)
                    | Expr::Logical(..)
                    | Expr::Negation(..)
                    | Expr::NilCheck(..) => (),
                    e => {
                        return Err(Error::with_message(
                            parser.column,
                            parser.line,
                            &format!(
                            "condition of If statement should reduce to bool but instead got: {:?}",
                            e
                        ),
                            "Ast",
                        ))
                    }
                }
                let mut elseifs = Vec::new();
                let mut elseblock = Vec::new();
                let mut stmts = Vec::new();
                match parser.read_token().get_kind() {
                    TokenKind::Colon => (),
                    c => {
                        return Err(Error::with_message(
                            parser.column,
                            parser.line,
                            &format!("Expect Colon after If block's expression but got: {:?}", c),
                            "Ast",
                        ))
                    }
                }
                parser.advance_token();
                //if block
                while parser.read_token().get_kind() != TokenKind::KElseif
                    && parser.read_token().get_kind() != TokenKind::KElse
                    && parser.read_token().get_kind() != TokenKind::KEnd
                {
                    stmts.push(Stmt::generate(parser, symbol_table)?);
                }
                // parser.get_token();
                while parser.read_token().get_kind() == TokenKind::KElseif {
                    parser.advance_token();
                    let cond = Expr::generate(parser, symbol_table, false)?;
                    match cond {
                        Expr::Comparison(..) | Expr::Negation(..) | Expr::NilCheck(..) | Expr::CBool(..) | Expr::Logical(..) | Expr::Atomic(..) => () ,
                        e => return Err(Error::with_message(parser.column, parser.line, &format!("condition of elsif statement should reduce to bool but instead got: {:?}",e),"Ast")),
                    }
                    match parser.read_token().get_kind() {
                        TokenKind::Colon => (),
                        c => {
                            return Err(Error::with_message(
                                parser.column,
                                parser.line,
                                &format!(
                                    "Expect Colon after ElseIf block's expression but got: {:?}",
                                    c
                                ),
                                "Ast",
                            ))
                        }
                    }
                    parser.advance_token();
                    let mut local_stmts = Vec::new();
                    //if block
                    while parser.read_token().get_kind() != TokenKind::KElseif
                        && parser.read_token().get_kind() != TokenKind::KElse
                        && parser.read_token().get_kind() != TokenKind::KEnd
                    {
                        local_stmts.push(Stmt::generate(parser, symbol_table)?);
                    }
                    elseifs.push((cond, local_stmts));
                }
                if parser.read_token().get_kind() == TokenKind::KElse {
                    parser.advance_token();
                    match parser.read_token().get_kind() {
                        TokenKind::Colon => (),
                        c => {
                            return Err(Error::with_message(
                                parser.column,
                                parser.line,
                                &format!("Expect Colon after Else keyword but got: {:?}", c),
                                "Ast",
                            ))
                        }
                    }
                    parser.advance_token();

                    while parser.read_token().get_kind() != TokenKind::KEnd {
                        elseblock.push(Stmt::generate(parser, symbol_table)?);
                    }
                }
                parser.advance_token();
                Ok(Stmt::If {
                    condition: cond,
                    stmts,
                    elseif: elseifs,
                    elseblock,
                })
            }
            e => Err(Error::with_message(
                parser.column,
                parser.line,
                &format!(
                    "Ast failed to parse statement: {:?} with extra; {:?}, pre:{:?}",
                    e,
                    parser.read_token(),
                    parser.previous_token()
                ),
                "Ast",
            )),
        }
    }
}
