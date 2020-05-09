use crate::ast::typedecl::TypeDecl;
use crate::ast::var_def::VarDef;
use crate::ast::{Atomic, Expr};
use crate::error::Error;
use crate::parser::*;
use crate::symbol_table::SymbolTable;
use std::fmt::Display;

fn get_token<'a>(tokens: &'a [Token], index: &mut usize) -> Result<&'a Token, Error> {
    match tokens.get(*index) {
        Some(k) => Ok(k),
        None => Err(Error::with_message(
            tokens[*index - 1].column,
            tokens[*index - 1].line,
            "tried to get token but failed",
            "Stmt",
        )),
    }
}

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
        tokens: &[Token],
        index: &mut usize,
        symbol_table: &mut SymbolTable<TypeDecl>,
        ctx_table: &mut Vec<VarDef>,
    ) -> Result<Stmt, Error> {
        let start = *index;
        let mut current_token = &tokens[start];
        match current_token.kind {
            TokenKind::Name => {
                let atom = Atomic::generate(tokens, index, symbol_table, ctx_table)?;
                current_token = get_token(tokens, index)?;
                match current_token.kind {
                    TokenKind::Assignement => {
                        *index += 1;
                        let expr = Expr::generate(tokens,index, symbol_table, false,false,ctx_table)?;
                        current_token = get_token(tokens, index)?;
                        if atom.get_type() != expr.get_type()  {
                            return Err(Error::with_message(current_token.column, current_token.line, &format!("Assigment requires that the variable is the same type as the expression, but {}:={}",atom.get_type(),expr.get_type()),"Ast"));
                        }
                        Ok(Stmt::Assign(atom,expr))
                    },
                    _ => {
                        match atom {
                            Atomic::FuncCall(TypeDecl::Void,n,a) => Ok(Stmt::Call(n,a)),
                            Atomic::FuncCall(_,n,_) => Err(Error::with_message(current_token.column, current_token.line, &format!("Function {} doesn't return void",n),"Ast")),  
                            e => Err(Error::with_message(current_token.column, current_token.line, &format!("An atomic expression is not a stmt unless it is a call that returns void, here it was: {}",e),"Ast"))   
                        }
                    }
                }
            }
            TokenKind::KSkip => {
                *index += 1;
                Ok(Stmt::Skip)
            }
            TokenKind::KExit => {
                *index += 1;
                Ok(Stmt::Exit)
            }
            TokenKind::KReturn => {
                *index += 1;
                Ok(Stmt::Return(Box::new(Expr::generate(
                    tokens,
                    index,
                    symbol_table,
                    false,
                    false,
                    ctx_table,
                )?)))
            }
            TokenKind::KFor => {
                *index += 1;
                let mut first_simples = Vec::new();
                loop {
                    let s = Stmt::generate(tokens, index, symbol_table, ctx_table)?;

                    current_token = get_token(tokens, index)?;
                    match s {
                        Stmt::Skip | Stmt::Assign(..) | Stmt::Call(..) => (),
                        e => {
                            return Err(Error::with_message(
                                current_token.column,
                                current_token.line,
                                &format!("Expected simple Statement but got: {}", e),
                                "Ast",
                            ))
                        }
                    }
                    first_simples.push(s);
                    match current_token.kind {
                        TokenKind::Comma => {
                            *index += 1;
                            continue;
                        }
                        TokenKind::Semicolon => break,
                        e => {
                            return Err(Error::with_message(
                                current_token.column,
                                current_token.line,
                                &format!("Expected \",\" or \";\" but got: {}", e),
                                "Ast",
                            ))
                        }
                    }
                }
                *index += 1;
                let cond = Expr::generate(tokens, index, symbol_table, false, false, ctx_table)?;
                current_token = get_token(tokens, index)?;
                if cond.get_type() != TypeDecl::Bool {
                    return Err(Error::with_message(
                        current_token.column,
                        current_token.line,
                        &format!(
                            "condition of For statement should reduce to bool but instead got: {}",
                            cond
                        ),
                        "Ast",
                    ));
                }
                match current_token.kind {
                    TokenKind::Semicolon => *index += 1,
                    e => {
                        return Err(Error::with_message(
                            current_token.column,
                            current_token.line,
                            &format!("Expected simple Statement but got: {}", e),
                            "Ast",
                        ))
                    }
                };
                let mut second_simples = Vec::new();
                loop {
                    let s = Stmt::generate(tokens, index, symbol_table, ctx_table)?;
                    current_token = get_token(tokens, index)?;
                    match s {
                        Stmt::Skip | Stmt::Assign(..) | Stmt::Call(..) => (),
                        e => {
                            return Err(Error::with_message(
                                current_token.column,
                                current_token.line,
                                &format!("Expected simple Statement but got: {}", e),
                                "Ast",
                            ))
                        }
                    }
                    second_simples.push(s);
                    match current_token.kind {
                        TokenKind::Comma => {
                            *index += 1;
                            continue;
                        }
                        TokenKind::Colon => break,
                        e => {
                            return Err(Error::with_message(
                                current_token.column,
                                current_token.line,
                                &format!("Expected \",\" or \":\" but got: {}", e),
                                "Ast",
                            ))
                        }
                    }
                }
                let mut body = Vec::new();
                *index += 1;
                current_token = get_token(tokens, index)?;
                while current_token.kind != TokenKind::KEnd {
                    body.push(Stmt::generate(tokens, index, symbol_table, ctx_table)?);
                    current_token = get_token(tokens, index)?;
                }
                *index += 1;

                Ok(Stmt::For(first_simples, cond, second_simples, body))
            }
            TokenKind::KIf => {
                *index += 1;
                let cond = Expr::generate(tokens, index, symbol_table, false, false, ctx_table)?;
                current_token = get_token(tokens, index)?;
                match cond {
                    Expr::CBool(..)
                    | Expr::Atomic(TypeDecl::Bool, _)
                    | Expr::Comparison(..)
                    | Expr::Logical(..)
                    | Expr::Negation(..)
                    | Expr::NilCheck(..) => (),
                    e => {
                        return Err(Error::with_message(
                            current_token.column,
                            current_token.line,
                            &format!(
                            "condition of If statement should reduce to bool but instead got: {}",
                            e
                        ),
                            "Ast",
                        ))
                    }
                }
                let mut elseifs = Vec::new();
                let mut elseblock = Vec::new();
                let mut stmts = Vec::new();
                match current_token.kind {
                    TokenKind::Colon => (),
                    c => {
                        return Err(Error::with_message(
                            current_token.column,
                            current_token.line,
                            &format!("Expect Colon after If block's expression but got: {}", c),
                            "Ast",
                        ))
                    }
                }
                *index += 1;
                current_token = get_token(tokens, index)?;
                //if block
                while current_token.kind != TokenKind::KElseif
                    && current_token.kind != TokenKind::KElse
                    && current_token.kind != TokenKind::KEnd
                {
                    stmts.push(Stmt::generate(tokens, index, symbol_table, ctx_table)?);
                    current_token = get_token(tokens, index)?;
                }
                // parser.get_token();
                while current_token.kind == TokenKind::KElseif {
                    *index += 1;
                    let cond =
                        Expr::generate(tokens, index, symbol_table, false, false, ctx_table)?;
                    current_token = get_token(tokens, index)?;
                    match cond {
                        Expr::Comparison(..) | Expr::Negation(..) | Expr::NilCheck(..) | Expr::CBool(..) | Expr::Logical(..) | Expr::Atomic(..) => () ,
                        e => return Err(Error::with_message(current_token.column, current_token.line, &format!("condition of elsif statement should reduce to bool but instead got: {}",e),"Ast")),
                    }
                    match current_token.kind {
                        TokenKind::Colon => (),
                        c => {
                            return Err(Error::with_message(
                                current_token.column,
                                current_token.line,
                                &format!(
                                    "Expect Colon after ElseIf block's expression but got: {}",
                                    c
                                ),
                                "Ast",
                            ))
                        }
                    }
                    *index += 1;
                    current_token = get_token(tokens, index)?;
                    let mut local_stmts = Vec::new();
                    //if block
                    while current_token.kind != TokenKind::KElseif
                        && current_token.kind != TokenKind::KElse
                        && current_token.kind != TokenKind::KEnd
                    {
                        local_stmts.push(Stmt::generate(tokens, index, symbol_table, ctx_table)?);
                        current_token = get_token(tokens, index)?;
                    }
                    elseifs.push((cond, local_stmts));
                }
                if current_token.kind == TokenKind::KElse {
                    *index += 1;
                    current_token = get_token(tokens, index)?;
                    match current_token.kind {
                        TokenKind::Colon => (),
                        c => {
                            return Err(Error::with_message(
                                current_token.column,
                                current_token.line,
                                &format!("Expect Colon after Else keyword but got: {}", c),
                                "Ast",
                            ))
                        }
                    }
                    *index += 1;
                    current_token = get_token(tokens, index)?;

                    while current_token.kind != TokenKind::KEnd {
                        elseblock.push(Stmt::generate(tokens, index, symbol_table, ctx_table)?);
                        current_token = get_token(tokens, index)?;
                    }
                }
                *index += 1;
                Ok(Stmt::If {
                    condition: cond,
                    stmts,
                    elseif: elseifs,
                    elseblock,
                })
            }
            e => Err(Error::with_message(
                current_token.column,
                current_token.line,
                &format!(
                    "Ast failed to parse statement: {} with token {}",
                    e, current_token
                ),
                "Ast",
            )),
        }
    }
}
