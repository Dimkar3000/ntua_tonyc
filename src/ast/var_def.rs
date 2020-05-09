use crate::ast::TypeDecl;
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
            "Vardef",
        )),
    }
}
#[derive(Debug, Clone)]
pub struct VarDef {
    pub name: String,
    pub var_type: TypeDecl,
}

impl Display for VarDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "{} {}", self.var_type, self.name)
    }
}

impl VarDef {
    pub fn new(name: &str, var_type: TypeDecl) -> Self {
        VarDef {
            name: name.to_string(),
            var_type,
        }
    }

    pub fn generate(
        tokens: &[Token],
        index: &mut usize,
        symbol_table: &mut SymbolTable<TypeDecl>,
    ) -> Result<Vec<VarDef>, Error> {
        let mut results = Vec::new();
        let starting_token = get_token(tokens, index)?;
        if starting_token.kind == TokenKind::RParenthesis {
            return Ok(Vec::new());
        }
        if !(starting_token.kind == TokenKind::KInt
            || starting_token.kind == TokenKind::KChar
            || starting_token.kind == TokenKind::KBool
            || starting_token.kind == TokenKind::KList)
        {
            return Err(Error::with_message(
                starting_token.column,
                starting_token.line,
                &format!("unsopported token kind: {}", starting_token.kind),
                "Vardef",
            ));
        }
        match TypeDecl::generate(tokens, index) {
            Ok(t) => loop {
                let mut current_token = &tokens[*index];
                match current_token.kind {
                    TokenKind::Name => {
                        let name = current_token.get_name().unwrap();
                        results.push(VarDef::new(&name, t.clone()));
                        match symbol_table.insert(name, t.clone()) {
                            Ok(_) => (),
                            Err(e) => {
                                return Err(Error::with_message(
                                    current_token.column,
                                    current_token.line,
                                    &e,
                                    "Ast",
                                ))
                            }
                        }
                        *index += 1;
                        current_token = &tokens[*index];
                        if current_token.kind != TokenKind::Comma {
                            break;
                        }
                        *index += 1;
                    }
                    e => {
                        return Err(Error::with_message(
                            current_token.column,
                            current_token.line,
                            &format!("Expected name definition, found \"{}\"", e),
                            "Ast",
                        ))
                    }
                }
            },
            Err(e) => return Err(e.extend("variable definition failed", "Ast")),
        };

        Ok(results)
    }
}
