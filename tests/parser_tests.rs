use libtonyc::parser::{Parser, TokenKind};

#[test]
fn test_words() {
    let mut parser = Parser::new(
        r#""abc" "Route66" "Helloworld!\n"
        "Name:\t\"DouglasAdams\"\nValue:\t42\n"
        "\n\t\r\0\\\'\"\x01"     
        "#,
    );
    assert_eq!(TokenKind::CString, parser.advance_token().kind);
    assert_eq!("abc", parser.read_token().get_cstring().unwrap());
    println!("abc");
    assert_eq!(TokenKind::CString, parser.advance_token().kind);
    assert_eq!("Route66", parser.read_token().get_cstring().unwrap());
    assert_eq!(TokenKind::CString, parser.advance_token().kind);
    assert_eq!("Helloworld!\n", parser.read_token().get_cstring().unwrap());
    assert_eq!(TokenKind::CString, parser.advance_token().kind);
    assert_eq!(
        "Name:\t\"DouglasAdams\"\nValue:\t42\n",
        parser.read_token().get_cstring().unwrap()
    );
    assert_eq!(TokenKind::CString, parser.advance_token().kind);
    assert_eq!(
        "\n\t\r\0\\\'\"\u{0001}",
        parser.read_token().get_cstring().unwrap()
    );
}
#[test]
fn test_all_symbols() {
    let mut parser = Parser::new(
        r#"and bool char decl def else elsif
    end exit false for head if int
    list mod new nil nil? not or
    ref return skip tail true 
    0 42 1284 00200 
    'a' '1' '\n' '\\' '\x1d'
    "#,
    );
    assert_eq!(TokenKind::NotStarted, parser.read_token().kind);
    assert_eq!(TokenKind::KAnd, parser.advance_token().kind);
    assert_eq!(TokenKind::KBool, parser.advance_token().kind);
    assert_eq!(TokenKind::KChar, parser.advance_token().kind);
    assert_eq!(TokenKind::KDecl, parser.advance_token().kind);
    assert_eq!(TokenKind::KDef, parser.advance_token().kind);
    assert_eq!(TokenKind::KElse, parser.advance_token().kind);
    assert_eq!(TokenKind::KElseif, parser.advance_token().kind);
    assert_eq!(TokenKind::KEnd, parser.advance_token().kind);
    assert_eq!(TokenKind::KExit, parser.advance_token().kind);
    assert_eq!(TokenKind::KFalse, parser.advance_token().kind);
    assert_eq!(TokenKind::KFor, parser.advance_token().kind);
    assert_eq!(TokenKind::KHead, parser.advance_token().kind);
    assert_eq!(TokenKind::KIf, parser.advance_token().kind);
    assert_eq!(TokenKind::KInt, parser.advance_token().kind);
    assert_eq!(TokenKind::KList, parser.advance_token().kind);
    assert_eq!(TokenKind::KMod, parser.advance_token().kind);
    assert_eq!(TokenKind::KNew, parser.advance_token().kind);
    assert_eq!(TokenKind::KNil, parser.advance_token().kind);
    assert_eq!(TokenKind::KNilQ, parser.advance_token().kind);
    assert_eq!(TokenKind::KNot, parser.advance_token().kind);
    assert_eq!(TokenKind::KOr, parser.advance_token().kind);
    assert_eq!(TokenKind::KRef, parser.advance_token().kind);
    assert_eq!(TokenKind::KReturn, parser.advance_token().kind);
    assert_eq!(TokenKind::KSkip, parser.advance_token().kind);
    assert_eq!(TokenKind::KTail, parser.advance_token().kind);
    assert_eq!(TokenKind::KTrue, parser.advance_token().kind);

    //     0 42 1284 00200
    assert_eq!(TokenKind::INT, parser.advance_token().kind);
    assert_eq!(0, parser.read_token().get_int().unwrap());
    assert_eq!(TokenKind::INT, parser.advance_token().kind);
    assert_eq!(42, parser.read_token().get_int().unwrap());
    assert_eq!(TokenKind::INT, parser.advance_token().kind);
    assert_eq!(1284, parser.read_token().get_int().unwrap());
    assert_eq!(TokenKind::INT, parser.advance_token().kind);
    assert_eq!(200, parser.read_token().get_int().unwrap());

    //  'a' '1' '\n' '\\' '\x1d'
    assert_eq!(TokenKind::CChar, parser.advance_token().kind);
    assert_eq!('a', parser.read_token().get_cchar().unwrap());
    assert_eq!(TokenKind::CChar, parser.advance_token().kind);
    assert_eq!('1', parser.read_token().get_cchar().unwrap());
    assert_eq!(TokenKind::CChar, parser.advance_token().kind);
    assert_eq!('\n', parser.read_token().get_cchar().unwrap());
    assert_eq!(TokenKind::CChar, parser.advance_token().kind);
    assert_eq!('\\', parser.read_token().get_cchar().unwrap());
    assert_eq!(TokenKind::CChar, parser.advance_token().kind);
    assert_eq!(0x1d as char, parser.read_token().get_cchar().unwrap());
    // \"abc\" \"Route66\" \"Helloworld!\\n\"
    // \"Name:\\t\\\"DouglasAdams\\\"\\nValue:\\t42\\n\"
}
