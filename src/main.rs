//#![feature(global_allocator, allocator_api, heap_api)]

mod allocator;
mod ast;
mod intinsics;
mod parser;

use allocator::{Allocation, BumpAllocator};
use parser::{Parser, Token, TokenKind};

fn test_string(s: &str) {
    println!("Test on:\n {}", s);
    let mut parser = Parser::new(s);
    loop {
        let token = parser.get_token();
        println!("Token: {:?}", token);
    
        if token.get_kind() == TokenKind::Empty
        {
            break
        }
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
    test_string(" and <*aodvmakpdvmadvdavdmba adddp  34 \n\n\n\t*>  or");
    test_string("\"test\"");
    // Backtracking test
    let mut p = Parser::new("5 + 4");
    assert_eq!(p.read_token().get_kind(), TokenKind::NotStarted);
    p.next_token();
    assert_eq!(p.read_token().get_kind(), TokenKind::INT);
}

fn test_alloc() {
    let mut a = BumpAllocator::new();
    let c = a.alloc("item: T");
    println!("here: {}", c);
}
use ast::AstRoot;
fn main() {
    test_parser();
    let stream = std::fs::read_to_string("./files/examples/hanoi.t").unwrap();
    test_string(&stream);

    test_alloc();
    // println!("{:?}", fs::read_dir(".").unwrap().collect::<Vec<_>>());
    // let mut ast = Ast::new(&stream);
    // ast.generate();
    // ast.print_tree();
    println!("test: list[int][][] i, y");
    let mut ast = AstRoot::new("list[int][][] i, y");
    ast.parser.next_token();
    let c= ast.var_def();
    println!("{:?}", c); 
    
}
