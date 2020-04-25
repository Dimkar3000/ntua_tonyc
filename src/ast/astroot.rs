use crate::ast::*;
use crate::error::Error;
use crate::parser::Parser;
use crate::symbol_table::SymbolTable;

pub struct AstRoot<'a> {
    pub parser: Parser<'a>,
    pub symbol_table: SymbolTable<TypeDecl>,
}

impl<'a> AstRoot<'a> {
    pub fn new(stream: &'a str) -> Self {
        let mut result = AstRoot {
            // allocator: BumpAllocator::new(),
            parser: Parser::new(stream),
            // current_block_name: Scope{name:"root".to_owned()},
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

    pub fn error<T, S: AsRef<str>>(&self, message: S) -> Result<T, Error> {
        Err(Error::with_message(
            self.parser.column,
            self.parser.line,
            message.as_ref(),
            "Ast",
        ))
    }

    pub fn generate(&mut self) -> Result<FuncDef, Error> {
        self.parser.advance_token();
        FuncDef::generate(&mut self.parser, &mut self.symbol_table)
    }
}
