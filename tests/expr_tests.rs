use libtonyc::ast::*;
use libtonyc::parser::Parser;
use libtonyc::symbol_table::SymbolTable;

#[test]
fn test_words() {
    let mut parser = Parser::new(r#"-(5*(2+2))"#);
    let mut symbol_table = SymbolTable::new();
    parser.advance_token();
    println!(
        "{}",
        Expr::generate(&mut parser, &mut symbol_table, false).unwrap()
    );
    assert!(false)
}
