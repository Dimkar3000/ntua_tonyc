// use libtonyc::parser::{Parser, TokenKind};

// #[test]
// fn test_words() {
//     let mut parser = Parser::new(
//         r#""abc" "Route66" "Helloworld!\n"
//         "Name:\t\"DouglasAdams\"\nValue:\t42\n"
//         "\n\t\r\0\\\'\"\x01"
//         "#,
//     );
//     let tokens = parser.produce();
//     assert_eq!(TokenKind::CString, tokens[0].kind);
//     assert_eq!("abc", tokens[0].get_cstring().unwrap());
//     assert_eq!(TokenKind::CString, tokens[1].kind);
//     assert_eq!("Route66", tokens[1].get_cstring().unwrap());
//     assert_eq!(TokenKind::CString, tokens[2].kind);
//     assert_eq!("Helloworld!\n", tokens[2].get_cstring().unwrap());
//     assert_eq!(TokenKind::CString, tokens[3].kind);
//     assert_eq!(
//         "Name:\t\"DouglasAdams\"\nValue:\t42\n",
//         tokens[3].get_cstring().unwrap()
//     );
//     assert_eq!(TokenKind::CString, tokens[4].kind);
//     assert_eq!("\n\t\r\0\\\'\"\u{0001}", tokens[4].get_cstring().unwrap());
// }
// #[test]
// fn test_all_symbols() {
//     let mut parser = Parser::new(
//         r#"and bool char decl def else elsif
//     end exit false for head if int
//     list mod new nil nil? not or
//     ref return skip tail true
//     0 42 1284 00200
//     'a' '1' '\n' '\\' '\x1d'
//     "#,
//     );
//     let d = parser.produce();
//     let t = vec![
//         TokenKind::KAnd,
//         TokenKind::KBool,
//         TokenKind::KChar,
//         TokenKind::KDecl,
//         TokenKind::KDef,
//         TokenKind::KElse,
//         TokenKind::KElseif,
//         TokenKind::KEnd,
//         TokenKind::KExit,
//         TokenKind::KFalse,
//         TokenKind::KFor,
//         TokenKind::KHead,
//         TokenKind::KIf,
//         TokenKind::KInt,
//         TokenKind::KList,
//         TokenKind::KMod,
//         TokenKind::KNew,
//         TokenKind::KNil,
//         TokenKind::KNilQ,
//         TokenKind::KNot,
//         TokenKind::KOr,
//         TokenKind::KRef,
//         TokenKind::KReturn,
//         TokenKind::KSkip,
//         TokenKind::KTail,
//         TokenKind::KTrue,
//     ];
//     for i in 0..t.len() {
//         assert_eq!(d[i].kind, t[i]);
//     }

//     let base = t.len();
//     //     0 42 1284 00200
//     assert_eq!(TokenKind::INT, d[base].kind);
//     assert_eq!(0, d[base].get_int().unwrap());
//     assert_eq!(TokenKind::INT, d[base + 1].kind);
//     assert_eq!(42, d[base + 1].get_int().unwrap());
//     assert_eq!(TokenKind::INT, d[base + 2].kind);
//     assert_eq!(1284, d[base + 2].get_int().unwrap());
//     assert_eq!(TokenKind::INT, d[base + 3].kind);
//     assert_eq!(200, d[base + 3].get_int().unwrap());

//     //  'a' '1' '\n' '\\' '\x1d'
//     assert_eq!(TokenKind::CChar, d[base + 4].kind);
//     assert_eq!('a', d[base + 4].get_cchar().unwrap());
//     assert_eq!(TokenKind::CChar, d[base + 5].kind);
//     assert_eq!('1', d[base + 5].get_cchar().unwrap());
//     assert_eq!(TokenKind::CChar, d[base + 6].kind);
//     assert_eq!('\n', d[base + 6].get_cchar().unwrap());
//     assert_eq!(TokenKind::CChar, d[base + 7].kind);
//     assert_eq!('\\', d[base + 7].get_cchar().unwrap());
//     assert_eq!(TokenKind::CChar, d[base + 8].kind);
//     assert_eq!(0x1d as char, d[base + 8].get_cchar().unwrap());
//     // \"abc\" \"Route66\" \"Helloworld!\\n\"
//     // \"Name:\\t\\\"DouglasAdams\\\"\\nValue:\\t42\\n\"
// }
