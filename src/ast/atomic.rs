use crate::ast::var_def::VarDef;
use crate::ast::Expr;
use crate::ast::TypeDecl;
use crate::error::Error;
use crate::parser::{Token, TokenExtra, TokenKind};
use crate::symbol_table::SymbolTable;
use std::fmt::Display;

fn get_token<'a>(tokens: &'a [Token], index: &mut usize) -> Result<&'a Token, Error> {
    match tokens.get(*index) {
        Some(k) => Ok(k),
        None => Err(Error::with_message(
            tokens[*index - 1].column,
            tokens[*index - 1].line,
            "tried to get token but failed",
            "Ast",
        )),
    }
}

/// Atomic is an code block that can stand on it's own.
#[derive(Debug, Clone)]
pub enum Atomic {
    /// Variable
    Name(TypeDecl, String),
    /// Constant String
    CString(String),
    /// Accessing an array element
    Accessor(Box<Atomic>, Box<Expr>),
    /// Calling a function
    FuncCall(TypeDecl, String, Vec<Expr>),
}

impl PartialEq for Atomic {
    fn eq(&self, _: &Atomic) -> bool {
        false
    }
}

impl Display for Atomic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Atomic::Name(_, n) => write!(f, "(atom {})", n),
            Atomic::CString(s) => write!(f, "(atom {})", s),
            Atomic::Accessor(a, e) => write!(f, "(atom {}[{}])", a, e),
            Atomic::FuncCall(_, n, exp) => {
                write!(f, "(atom {}(", n).unwrap();
                exp.iter().fold(true, |first, elem| {
                    if !first {
                        write!(f, ", ").unwrap();
                    }
                    write!(f, "{}", elem).unwrap();
                    false
                });
                write!(f, "))")
            }
        }
    }
}

impl Atomic {
    /// Extract the name of atom, used to find function names during code generation
    pub fn get_name(&self) -> String {
        match self {
            Atomic::Name(_, s) => s.clone(),
            Atomic::CString(s) => s.clone(),
            Atomic::Accessor(a, _) => a.get_name(),
            Atomic::FuncCall(_, s, _) => s.clone(),
        }
    }
    /// Get the type of the atom
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

    /// Generate a new atom by consuming tokens from the parser and checking with the symbol table
    pub fn generate(
        tokens: &[Token],
        index: &mut usize,
        symbol_table: &mut SymbolTable<TypeDecl>,
        ctx_table: &mut Vec<VarDef>,
    ) -> Result<Atomic, Error> {
        let mut current_token = get_token(tokens, index)?;
        let base = match current_token {
            Token {
                kind: TokenKind::Name,
                extra: TokenExtra::Name(n),
                ..
            } => {
                let name = n.to_string();
                *index += 1;
                current_token = get_token(tokens, index)?;
                match current_token.kind {
                    TokenKind::LParenthesis => {
                        let mut args = Vec::new();
                        loop {
                            *index += 1;
                            current_token = get_token(tokens, index)?;
                            if current_token.kind == TokenKind::RParenthesis {
                                *index += 1;
                                current_token = get_token(tokens, index)?;
                                break;
                            }
                            let exp =
                                Expr::generate(tokens, index, symbol_table, true, false, ctx_table);
                            current_token = get_token(tokens, index)?;
                            let tmp = match exp {
                                Ok(k) => k,
                                Err(e) => return Err(e.extend("Function Arguments failed", "Ast")),
                            };
                            args.push(tmp);
                            match current_token.kind {
                                TokenKind::Comma => (),
                                // TokenKind::RParenthesis => break,
                                e => {
                                    if tokens[*index - 1].kind == TokenKind::RParenthesis {
                                        // parser.get_token();
                                        break;
                                    }
                                    return Err(Error::with_message(
                                        current_token.column,
                                        current_token.line,
                                        &format!(
                                            "Expected RParenthesis or comma, but got {}, {}",
                                            e, current_token
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
                                    current_token.column,
                                    current_token.line,
                                    &format!("Function name not defined: {}", name),
                                    "Ast",
                                ))
                            }
                        }
                    }
                    _ => {
                        // parser.back();
                        match symbol_table.lookup(&name) {
                            Some(i) => {
                                if !ctx_table.iter().any(|x| x.name == name) {
                                    ctx_table.push(VarDef {
                                        name: name.clone(),
                                        var_type: i.clone(),
                                    });
                                }
                                Atomic::Name(i.clone(), name)
                            }
                            None => {
                                return Err(Error::with_message(
                                    current_token.column,
                                    current_token.line,
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
                *index += 1;
                Atomic::CString(s.to_string())
            }

            e => {
                return Err(Error::with_message(
                    e.column,
                    e.line,
                    &format!("atomic failed to consume token: {}", e),
                    "Ast",
                ))
            }
        };
        current_token = get_token(tokens, index)?;
        match current_token.kind {
            TokenKind::LBracket => {
                *index += 1;
                let b = match Expr::generate(tokens, index, symbol_table, false, false, ctx_table) {
                    Ok(k) => k,
                    Err(e) => return Err(e.extend("Failed to parse bracket content", "Ast")),
                };
                current_token = get_token(tokens, index)?;
                match b {
                    Expr::CInt(_) | Expr::Unary(..) | Expr::Binary(..) => (),
                    Expr::Atomic(TypeDecl::Int, _) => (),
                    e => {
                        return Err(Error::with_message(
                            current_token.column,
                            current_token.line,
                            &format!(
                                "expression inside bracket should reduce to intiger, but {}",
                                e
                            ),
                            "Ast",
                        ))
                    }
                }
                match current_token.kind {
                    TokenKind::RBracket => {
                        *index += 1;
                        let r = Atomic::Accessor(Box::new(base), Box::new(b));
                        Ok(r)
                    }
                    e => Err(Error::with_message(
                        current_token.column,
                        current_token.line,
                        &format!("Ast; right bracket missing: {}", e),
                        "Ast",
                    )),
                }
            }
            _ => Ok(base),
        }
    }
}
