use crate::ast::{Stmt, TypeDecl, VarDef};
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
            "Function",
        )),
    }
}

#[derive(Debug, Clone)]
pub struct FormalDecl {
    pub is_ref: bool,
    pub def: VarDef,
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub rtype: TypeDecl,
    pub name: String,
    pub ctx: Vec<VarDef>, // Shadow arguments to support functions using parent state
    pub arguments: Vec<FormalDecl>,
    // body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct FuncDef {
    pub header: FuncDecl,
    pub decls: Vec<FuncDecl>,
    pub defs: Vec<FuncDef>,
    pub vars: Vec<VarDef>,
    pub stmts: Vec<Stmt>,
}

impl Display for FuncDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "decl {}", self.name)
    }
}

impl Display for FuncDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        writeln!(f, "def {}:", self.header.name).unwrap();
        for i in &self.decls {
            writeln!(f, "\t\t{}", i).unwrap();
        }
        writeln!(f, "\tdefs:").unwrap();
        for i in &self.defs {
            writeln!(f, "\t\t{}", i).unwrap();
        }
        writeln!(f, "\tdefs:").unwrap();
        for i in &self.vars {
            writeln!(f, "\t\t{}", i).unwrap();
        }
        writeln!(f, "\tdefs:").unwrap();
        for i in &self.stmts {
            writeln!(f, "\t\t{}", i).unwrap();
        }

        write!(f, "end")
    }
}

impl FuncDecl {
    pub fn generate(
        tokens: &[Token],
        index: &mut usize,
        symbol_table: &mut SymbolTable<TypeDecl>,
    ) -> Result<FuncDecl, Error> {
        let start = *index;
        match tokens[start].kind {
            TokenKind::KDecl => {
                *index += 1;
                let mut current_token = get_token(tokens, index)?;
                let t = match current_token.kind {
                    TokenKind::Name => TypeDecl::Void,
                    TokenKind::KList | TokenKind::KInt | TokenKind::KChar | TokenKind::KBool => {
                        TypeDecl::generate(tokens, index)?
                    }
                    e => {
                        return Err(Error::with_message(
                            current_token.column,
                            current_token.line,
                            &format!("Wrong token in Function decleration: {}", e),
                            "Ast",
                        ))
                    }
                };
                current_token = get_token(tokens, index)?;

                let name = match current_token {
                    Token {
                        kind: TokenKind::Name,
                        extra: TokenExtra::Name(n),
                        ..
                    } => {
                        let na = n.to_string();
                        *index += 1;
                        current_token = get_token(tokens, index)?;
                        na
                    }
                    e => {
                        return Err(Error::with_message(
                            current_token.column,
                            current_token.line,
                            &format!("Wrong token in Function decleration: {}", e),
                            "Ast",
                        ))
                    }
                };
                if current_token.kind != TokenKind::LParenthesis {
                    return Err(Error::with_message(
                        current_token.column,
                        current_token.line,
                        "Expected LParenthesis next to function name",
                        "Ast",
                    ));
                }
                let a = symbol_table.insert(&name, t.clone());
                symbol_table.open_scope(&name);
                let args = FormalDecl::generate(tokens, index, symbol_table)?;
                current_token = get_token(tokens, index)?;

                match a {
                    Ok(()) => (),
                    Err(e) => {
                        return Err(Error::with_message(
                            current_token.column,
                            current_token.line,
                            &format!("Ast Def: {}", e),
                            "Ast",
                        ))
                    }
                }
                symbol_table.close_scope();
                Ok(FuncDecl {
                    rtype: t,
                    name,
                    ctx: Vec::new(),
                    arguments: args,
                })
            }
            e => Err(Error::with_message(
                tokens[start].column,
                tokens[start].line,
                &format!("failed to find function decl: {}", e),
                "Ast",
            )),
        }
    }
}

impl FuncDef {
    pub fn generate(
        tokens: &[Token],
        index: &mut usize,
        symbol_table: &mut SymbolTable<TypeDecl>,
    ) -> Result<FuncDef, Error> {
        let start = *index;
        match tokens[start].kind {
            TokenKind::KDef => {
                *index += 1;
                let mut current_token = get_token(tokens, index)?;
                let t = match current_token.kind {
                    TokenKind::Name => TypeDecl::Void,
                    TokenKind::KList | TokenKind::KInt | TokenKind::KChar | TokenKind::KBool => {
                        TypeDecl::generate(tokens, index)?
                    }
                    e => {
                        return Err(Error::with_message(
                            current_token.column,
                            current_token.line,
                            &format!("Wrong token in Function definition 1: {}", e),
                            "Ast",
                        ))
                    }
                };
                current_token = get_token(tokens, index)?;
                let name = match current_token {
                    Token {
                        kind: TokenKind::Name,
                        extra: TokenExtra::Name(n),
                        ..
                    } => {
                        let na = n.to_string();
                        *index += 1;
                        na
                    }
                    e => {
                        return Err(Error::with_message(
                            current_token.column,
                            current_token.line,
                            &format!("Wrong token in Function definition 2: {}", e),
                            "Ast",
                        ))
                    }
                };
                current_token = get_token(tokens, index)?;
                let _ = symbol_table.insert(&name, t.clone());
                symbol_table.open_scope(&name);
                if current_token.kind != TokenKind::LParenthesis {
                    return Err(Error::with_message(
                        current_token.column,
                        current_token.line,
                        "Expected LParenthesis next to function name",
                        "Ast",
                    ));
                }

                let args = FormalDecl::generate(tokens, index, symbol_table)?;
                current_token = get_token(tokens, index)?;
                match current_token.kind {
                    TokenKind::Colon => (),
                    e => {
                        return Err(Error::with_message(
                            current_token.column,
                            current_token.line,
                            &format!("Function Definition end with colon, but got: {}", e),
                            "Ast",
                        ))
                    }
                };
                *index += 1;
                current_token = get_token(tokens, index)?;
                let a = symbol_table.insert(&name, t.clone());

                match a {
                    Ok(()) => (),
                    Err(e) => {
                        return Err(Error::with_message(
                            current_token.column,
                            current_token.line,
                            &format!("Ast Def: {}", e),
                            "Ast",
                        ))
                    }
                }

                let mut defs = Vec::new();
                let mut decls = Vec::new();
                let mut vars = Vec::new();
                let mut stmts = Vec::new();
                loop {
                    match current_token.kind {
                        TokenKind::KDef => {
                            defs.push(FuncDef::generate(tokens, index, symbol_table)?)
                        }
                        TokenKind::KDecl => {
                            decls.push(FuncDecl::generate(tokens, index, symbol_table)?)
                        }
                        TokenKind::KInt
                        | TokenKind::KBool
                        | TokenKind::KList
                        | TokenKind::KChar => {
                            vars.extend(VarDef::generate(tokens, index, symbol_table)?)
                        }

                        _ => break,
                    }
                    current_token = get_token(tokens, index)?;
                }
                let mut ctx = Vec::new();
                while current_token.kind != TokenKind::KEnd {
                    let stmt = Stmt::generate(tokens, index, symbol_table, &mut ctx)?;
                    match &stmt {
                        Stmt::Exit if t != TypeDecl::Void => {
                            return Err(Error::with_message(
                                tokens[*index].column,
                                tokens[*index].line,
                                "Exit statement if not valid in function that returns a value",
                                "Ast",
                            ))
                        }
                        Stmt::Return(..) if t == TypeDecl::Void => {
                            return Err(Error::with_message(
                                tokens[*index].column,
                                tokens[*index].line,
                                "Return statement if not valid in function that does not return a value",
                                "Ast",
                            ))
                        }

                        _ => (),
                    }
                    stmts.push(stmt);
                    current_token = get_token(tokens, index)?;
                }
                // remove from ctx all variables from the same scope

                for arg in &args {
                    let pos = ctx.iter().position(|x| x.name == arg.def.name);
                    if let Some(pos) = pos {
                        ctx.remove(pos);
                    }
                }
                for var in &vars {
                    let pos = ctx.iter().position(|x| x.name == var.name);
                    if let Some(pos) = pos {
                        ctx.remove(pos);
                    }
                }

                for i in &defs {
                    for j in &i.header.ctx {
                        if ctx.iter().all(|x| x.name != j.name)
                            && vars.iter().all(|x| x.name != j.name)
                        {
                            ctx.push(j.clone());
                        }
                    }
                }

                // println!("ctx of {}: {:?}", name, ctx);
                *index += 1;
                symbol_table.close_scope();
                Ok(FuncDef {
                    header: FuncDecl {
                        rtype: t,
                        ctx,
                        name,
                        arguments: args,
                    },
                    decls,
                    defs,
                    vars,
                    stmts,
                })
            }
            _ => Err(Error::with_message(
                tokens[start].column,
                tokens[start].line,
                &format!("failed to find function def: {}", tokens[start]),
                "Ast",
            )),
        }
    }
}
impl FormalDecl {
    pub fn generate(
        tokens: &[Token],
        index: &mut usize,
        symbol_table: &mut SymbolTable<TypeDecl>,
    ) -> Result<Vec<FormalDecl>, Error> {
        let start = *index;
        match tokens[start].kind {
            TokenKind::LParenthesis => *index += 1,
            e => {
                return Err(Error::with_message(
                    tokens[start].column,
                    tokens[start].line,
                    &format!(
                        "formal definitions start with parenthesis but I was given: {}",
                        e
                    ),
                    "Ast",
                ))
            }
        };
        let mut current_token = get_token(tokens, index)?;
        if current_token.kind == TokenKind::RParenthesis {
            *index += 1;
            return Ok(Vec::new());
        };
        let mut results = Vec::new();
        loop {
            current_token = get_token(tokens, index)?;
            let is_ref = match current_token.kind {
                TokenKind::KRef => {
                    *index += 1;
                    true
                }
                _ => false,
            };

            let defs = match VarDef::generate(tokens, index, symbol_table) {
                Ok(v) => v,
                Err(e) => return Err(e.extend("Inside formal", "Ast")),
            };
            let i = defs.iter().map(|x| FormalDecl {
                is_ref,
                def: VarDef::new(&x.name, x.var_type.clone()),
            });
            results.extend(i);

            current_token = get_token(tokens, index)?;
            match current_token.kind {
                TokenKind::Semicolon => *index += 1,
                TokenKind::RParenthesis => {
                    *index += 1;
                    break;
                }
                e => {
                    return Err(Error::with_message(
                        current_token.column,
                        current_token.line,
                        &format!("unxpected token: {}", e),
                        "Ast",
                    ))
                }
            };
        }
        Ok(results)
    }
}
