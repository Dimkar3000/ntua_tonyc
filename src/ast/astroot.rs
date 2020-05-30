use crate::ast::*;
use crate::error::Error;
use crate::parser::{Parser, Token};
use crate::symbol_table::SymbolTable;

pub struct AstRoot {
    pub tokens: Vec<Token>,
    pub index: usize,
    pub symbol_table: SymbolTable<TypeDecl>,
}

impl AstRoot {
    pub fn new(stream: &str) -> Self {
        let mut p = Parser::new(stream);
        let mut result = AstRoot {
            index: 0,
            tokens: p.produce(),
            symbol_table: SymbolTable::new(),
        };
        result.symbol_table.insert("abs", TypeDecl::Int).unwrap();
        result.symbol_table.insert("ord", TypeDecl::Int).unwrap();
        result.symbol_table.insert("chr", TypeDecl::Char).unwrap();
        result.symbol_table.insert("strlen", TypeDecl::Int).unwrap();
        result.symbol_table.insert("strcmp", TypeDecl::Int).unwrap();
        result
            .symbol_table
            .insert("strcpy", TypeDecl::Void)
            .unwrap();
        result
            .symbol_table
            .insert("strcat", TypeDecl::Void)
            .unwrap();

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

    pub fn generate(&mut self) -> Result<FuncDef, Error> {
        if self.tokens.is_empty() {
            return Err(Error::with_message(0, 0, "No tokens supplied", "Ast"));
        }
        FuncDef::generate(&self.tokens, &mut 0, &mut self.symbol_table)
    }
}
