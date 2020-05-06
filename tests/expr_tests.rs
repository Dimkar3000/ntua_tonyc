use libtonyc::ast::*;
use libtonyc::parser::Parser;
use libtonyc::symbol_table::SymbolTable;

fn expr_test(stream: &str, expr: Expr) {
    let mut parser = Parser::new(stream);
    let mut symbol_table = SymbolTable::new();
    parser.advance_token();
    assert_eq!(
        Expr::generate(&mut parser, &mut symbol_table, false).unwrap(),
        expr,
    );
}

#[test]
fn test_words() {
    use libtonyc::parser::TokenKind;
    // expr_test("-(5*(2+2))", Expr::CInt(-20));
    // expr_test("-(5*(2/2))", Expr::CInt(-5));
    // expr_test("-(5*(7 mod 4))", Expr::CInt(-15));
    // expr_test("5*(2+2) - (3 mod 4)", Expr::CInt(17));
    // expr_test("5*(2+2) - (3 mod 4) = 17", Expr::CBool(true));
    // expr_test("5*(2+2) - (3 mod 4) = 17 and 'a' > 'b'", Expr::CBool(false));
    // expr_test("5*(2+2) - (3 mod 4) <> 17", Expr::CBool(false));
    expr_test(
        "'a' = 'a'",
        Expr::Comparison(
            TokenKind::Equal,
            Some(Box::new(Expr::CChar('a'))),
            Some(Box::new(Expr::CChar('a'))),
        ),
    );
    expr_test(
        "'a' <> 'a'",
        Expr::Comparison(
            TokenKind::NotEqual,
            Some(Box::new(Expr::CChar('a'))),
            Some(Box::new(Expr::CChar('a'))),
        ),
    );
    expr_test(
        "'b' > 'a'",
        Expr::Comparison(
            TokenKind::Great,
            Some(Box::new(Expr::CChar('b'))),
            Some(Box::new(Expr::CChar('a'))),
        ),
    );
    expr_test(
        "'a' >= 'a'",
        Expr::Comparison(
            TokenKind::GreatOrEqual,
            Some(Box::new(Expr::CChar('a'))),
            Some(Box::new(Expr::CChar('a'))),
        ),
    );
    expr_test(
        "'a' < 'c'",
        Expr::Comparison(
            TokenKind::Less,
            Some(Box::new(Expr::CChar('a'))),
            Some(Box::new(Expr::CChar('c'))),
        ),
    );
    expr_test(
        "1 = 1",
        Expr::Comparison(
            TokenKind::Equal,
            Some(Box::new(Expr::CInt(1))),
            Some(Box::new(Expr::CInt(1))),
        ),
    );
    expr_test(
        "1 > 0",
        Expr::Comparison(
            TokenKind::Great,
            Some(Box::new(Expr::CInt(1))),
            Some(Box::new(Expr::CInt(0))),
        ),
    );
    expr_test(
        "-2 < -1",
        Expr::Comparison(
            TokenKind::Less,
            Some(Box::new(Expr::CInt(-2))),
            Some(Box::new(Expr::CInt(-1))),
        ),
    );
    expr_test(
        "2 > 1 or -1 <> 1",
        Expr::Logical(
            TokenKind::KOr,
            Some(Box::new(Expr::Comparison(
                TokenKind::Great,
                Some(Box::new(Expr::CInt(2))),
                Some(Box::new(Expr::CInt(1))),
            ))),
            Some(Box::new(Expr::Comparison(
                TokenKind::NotEqual,
                Some(Box::new(Expr::CInt(-1))),
                Some(Box::new(Expr::CInt(1))),
            ))),
        ),
    );
}
