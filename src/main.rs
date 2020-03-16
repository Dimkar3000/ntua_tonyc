mod parser;
use parser::{Token, Parser};

fn test_string(s: &str) {
    println!("Test on: {:?}", s);
    let mut parser = Parser::new(s);
    while parser.token != Token::Empty {
        println!("Token: {:?}", parser.token);
        parser.next_token();
    }
}

fn test_parser() {
    test_string("int i");
    test_string("int x, y, z");
    test_string("char[] s");
    test_string("char[] s%Testconemt\nint i");
    test_string("5 + 4");
    test_string("< <> <= >>= ");
    test_string("( )[], ; ::=");
    test_string("def p3 (int a, b; ref char c)");
    test_string("'a' '\n' '\\x65' ");
    test_string("\"LONG STATIC STRING\" + 5");
    test_string("and bool char decl def else elsif end exit false for head if int list mod new nil nil? not or ref return skip tail true");
    // println!("t = {:?}");
    // println!("t = {:?}");
    // println!("t = {:?}");
}

fn main() {
    test_parser();
}
