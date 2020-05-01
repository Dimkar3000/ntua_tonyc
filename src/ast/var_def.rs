use crate::ast::TypeDecl;
use crate::error::Error;
use crate::parser::*;
use crate::symbol_table::SymbolTable;
use std::fmt::Display;

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
        parser: &mut Parser,
        symbol_table: &mut SymbolTable<TypeDecl>,
    ) -> Result<Vec<VarDef>, Error> {
        let mut results = Vec::new();
        let kind = &parser.read_token().get_kind();
        if kind == &TokenKind::RParenthesis {
            return Ok(Vec::new());
        }
        assert!(
            kind == &TokenKind::KInt
                || kind == &TokenKind::KChar
                || kind == &TokenKind::KBool
                || kind == &TokenKind::KList
        );
        match TypeDecl::generate(parser) {
            Ok(t) => loop {
                match parser.read_token().get_kind() {
                    TokenKind::Name => {
                        let name = parser.read_token().get_name().unwrap();
                        results.push(VarDef::new(&name, t.clone()));
                        match symbol_table.insert(name, t.clone()) {
                            Ok(_) => (),
                            Err(e) => {
                                return Err(Error::with_message(
                                    parser.column,
                                    parser.line,
                                    &e,
                                    "Ast",
                                ))
                            }
                        }
                        if parser.advance_token().get_kind() != TokenKind::Comma {
                            break;
                        }
                        parser.advance_token();
                    }
                    e => {
                        return Err(Error::with_message(
                            parser.column,
                            parser.line,
                            &format!("Expected name definition, found \"{:?}\"", e),
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
