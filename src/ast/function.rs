use crate::ast::{Stmt, TypeDecl, VarDef};
use crate::error::Error;
use crate::parser::*;
use crate::symbol_table::SymbolTable;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct FormalDecl {
    pub is_ref: bool,
    pub def: VarDef,
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub rtype: TypeDecl,
    pub name: String,
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
        parser: &mut Parser,
        symbol_table: &mut SymbolTable<TypeDecl>,
    ) -> Result<FuncDecl, Error> {
        match parser.read_token().get_kind() {
            TokenKind::KDecl => {
                let t = match parser.advance_token().get_kind() {
                    TokenKind::Name => TypeDecl::Void,
                    TokenKind::KList | TokenKind::KInt | TokenKind::KChar | TokenKind::KBool => {
                        TypeDecl::generate(parser)?
                    }
                    e => {
                        return Err(Error::with_message(
                            parser.column,
                            parser.line,
                            &format!("Wrong token in Function definition: {:?}", e),
                            "Ast",
                        ))
                    }
                };
                let name = match parser.read_token() {
                    Token {
                        kind: TokenKind::Name,
                        extra: TokenExtra::Name(n),
                        ..
                    } => {
                        let na = n.to_string();
                        parser.advance_token();
                        na
                    }
                    e => {
                        return Err(Error::with_message(
                            parser.column,
                            parser.line,
                            &format!("Wrong token in Function definition: {:?}", e),
                            "Ast",
                        ))
                    }
                };
                if parser.read_token().get_kind() != TokenKind::LParenthesis {
                    return Err(Error::with_message(
                        parser.column,
                        parser.line,
                        "Expected LParenthesis next to function name",
                        "Ast",
                    ));
                }
                let args = FormalDecl::generate(parser, symbol_table)?;
                let a = symbol_table.insert(&name, t.clone());
                match a {
                    Ok(()) => (),
                    Err(e) => {
                        return Err(Error::with_message(
                            parser.column,
                            parser.line,
                            &format!("Ast Def: {}", e),
                            "Ast",
                        ))
                    }
                }
                Ok(FuncDecl {
                    rtype: t,
                    name: name.to_owned(),
                    arguments: args,
                })
            }
            e => Err(Error::with_message(
                parser.column,
                parser.line,
                &format!("failed to find function decl: {:?}", e),
                "Ast",
            )),
        }
    }
}

impl FuncDef {
    pub fn generate(
        parser: &mut Parser,
        symbol_table: &mut SymbolTable<TypeDecl>,
    ) -> Result<FuncDef, Error> {
        match parser.read_token().get_kind() {
            TokenKind::KDef => {
                let t = match parser.advance_token().get_kind() {
                    TokenKind::Name => TypeDecl::Void,
                    TokenKind::KList | TokenKind::KInt | TokenKind::KChar | TokenKind::KBool => {
                        TypeDecl::generate(parser)?
                    }
                    e => {
                        return Err(Error::with_message(
                            parser.column,
                            parser.line,
                            &format!("Wrong token in Function definition: {:?}", e),
                            "Ast",
                        ))
                    }
                };
                let name = match parser.read_token() {
                    Token {
                        kind: TokenKind::Name,
                        extra: TokenExtra::Name(n),
                        ..
                    } => {
                        let na = n.to_string();
                        parser.advance_token();
                        na
                    }
                    e => {
                        return Err(Error::with_message(
                            parser.column,
                            parser.line,
                            &format!("Wrong token in Function definition: {:?}", e),
                            "Ast",
                        ))
                    }
                };
                // println!("{:?}", symbol_table);
                let _ = symbol_table.insert(&name, t.clone());
                symbol_table.open_scope(&name);
                if parser.read_token().get_kind() != TokenKind::LParenthesis {
                    return Err(Error::with_message(
                        parser.column,
                        parser.line,
                        "Expected LParenthesis next to function name",
                        "Ast",
                    ));
                }

                let args = FormalDecl::generate(parser, symbol_table)?;
                match parser.read_token().get_kind() {
                    TokenKind::Colon => (),
                    e => {
                        return Err(Error::with_message(
                            parser.column,
                            parser.line,
                            &format!("Function Definition end with colon, but got: {:?}", e),
                            "Ast",
                        ))
                    }
                };
                parser.advance_token();

                let a = symbol_table.insert(&name, t.clone());

                match a {
                    Ok(()) => (),
                    Err(e) => {
                        return Err(Error::with_message(
                            parser.column,
                            parser.line,
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
                    match parser.read_token().get_kind() {
                        TokenKind::KDef => defs.push(FuncDef::generate(parser, symbol_table)?),
                        TokenKind::KDecl => decls.push(FuncDecl::generate(parser, symbol_table)?),
                        TokenKind::KInt
                        | TokenKind::KBool
                        | TokenKind::KList
                        | TokenKind::KChar => vars.extend(VarDef::generate(parser, symbol_table)?),

                        _ => break,
                    }
                }
                while parser.read_token().get_kind() != TokenKind::KEnd {
                    stmts.push(Stmt::generate(parser, symbol_table)?);
                }
                parser.advance_token();
                symbol_table.close_scope();
                Ok(FuncDef {
                    header: FuncDecl {
                        rtype: t,
                        name: name.to_owned(),
                        arguments: args,
                    },
                    decls,
                    defs,
                    vars,
                    stmts,
                })
            }
            _ => Err(Error::with_message(
                parser.column,
                parser.line,
                &format!("failed to find function decl: {}", parser.read_token()),
                "Ast",
            )),
        }
    }
}
impl FormalDecl {
    pub fn generate(
        parser: &mut Parser,
        symbol_table: &mut SymbolTable<TypeDecl>,
    ) -> Result<Vec<FormalDecl>, Error> {
        match parser.read_token().get_kind() {
            TokenKind::LParenthesis => parser.advance_token(),
            e => {
                return Err(Error::with_message(
                    parser.column,
                    parser.line,
                    &format!(
                        "formal definitions start with parenthesis but I was given: {:?}",
                        e
                    ),
                    "Ast",
                ))
            }
        };
        if parser.read_token().get_kind() == TokenKind::RParenthesis {
            parser.advance_token();
            return Ok(Vec::new());
        };
        let mut results = Vec::new();
        loop {
            let is_ref = match parser.read_token().get_kind() {
                TokenKind::KRef => {
                    parser.advance_token();
                    true
                }
                _ => false,
            };

            let defs = match VarDef::generate(parser, symbol_table) {
                Ok(v) => v,
                Err(e) => return Err(e.extend("Inside formal", "Ast")),
            };
            let i = defs.iter().map(|x| FormalDecl {
                is_ref,
                def: VarDef::new(&x.name, x.var_type.clone()),
            });
            results.extend(i);
            match parser.read_token().get_kind() {
                TokenKind::Semicolon => parser.advance_token(),
                TokenKind::RParenthesis => {
                    parser.advance_token();
                    break;
                }
                e => {
                    return Err(Error::with_message(
                        parser.column,
                        parser.line,
                        &format!("unxpected token: {:?}", e),
                        "Ast",
                    ))
                }
            };
        }
        Ok(results)
    }
}
