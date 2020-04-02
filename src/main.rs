//#![feature(global_allocator, allocator_api, heap_api)]

mod allocator;
mod ast;
mod intinsics;
mod parser;


use crate::ast::AstError;
use crate::ast::Atomic;
use crate::ast::Expr;
use crate::ast::FormalDecl;
use crate::ast::FuncDecl;
use crate::ast::Stmt;
use crate::ast::VarDef;
use allocator::{Allocation, BumpAllocator};
use parser::{Parser, Token, TokenKind};

fn test_string_parser(s: &str) {
    println!("Test on:\n {}", s);
    let mut parser = Parser::new(s);
    loop {
        let token = parser.get_token();
        println!("Token: {:?}", token);

        if token.get_kind() == TokenKind::Empty {
            break;
        }
    }
}

fn test_parser() {
    test_string_parser("int i");
    test_string_parser("int x, y, z");
    test_string_parser("char[] s");
    test_string_parser("char[] s%Testconemt\nint i");
    test_string_parser("5 + 4");
    test_string_parser("< <> <= >>= ");
    test_string_parser("( )[], ; ::=");
    test_string_parser("def p3 (int a, b; ref char c)");
    test_string_parser("'a' '\n' '\\x65' ");
    test_string_parser("\"LONG STATIC STRING\" + 5");
    test_string_parser("and bool char decl def else elsif end exit false for head if int list mod new nil nil? not or ref return skip tail true");
    test_string_parser(" and <*aodvmakpdvmadvdavdmba adddp  34 \n\n\n\t*>  or");
    test_string_parser("\"test\"");
    // Backtracking test
    let mut p = Parser::new("5 + 4");
    assert_eq!(p.read_token().get_kind(), TokenKind::NotStarted);
    p.next_token();
    assert_eq!(p.read_token().get_kind(), TokenKind::INT);
    let stream = std::fs::read_to_string("./files/examples/hanoi.t").unwrap();
    test_string_parser(&stream);
}

fn test_alloc() {
    let mut a = BumpAllocator::new();
    let c = a.alloc("item: T");
    println!("here: {}", c);
}

use ast::AstRoot;
fn test_string_var_def(stream: &str) -> Result<Vec<VarDef>, AstError> {
    println!("ast test: {}", stream);
    let mut ast = AstRoot::new(stream);
    ast.parser.next_token();
    let c = ast.var_def();
    println!("{:?}", c);
    c
}

fn test_string_formal_def(stream: &str) -> Result<Vec<FormalDecl>, AstError> {
    println!("ast test: {}", stream);
    let mut ast = AstRoot::new(stream);
    ast.parser.next_token();
    let c = ast.formal_def();
    println!("{:?}", c);
    c
}

fn test_string_func_decl(stream: &str) -> Result<FuncDecl, AstError> {
    println!("ast test: {}", stream);
    let mut ast = AstRoot::new(stream);
    ast.parser.next_token();
    let c = ast.func_decl();
    println!("{:?}", c);
    c
}

fn test_string_stmt(stream: &str) -> Result<Stmt, AstError> {
    println!("ast test: {}", stream);
    let mut ast = AstRoot::new(stream);
    ast.parser.next_token();
    let c = ast.stmt();
    println!("{:?}", c);
    c
}

fn test_string_atomic(stream: &str) -> Result<Atomic, AstError> {
    println!("ast test: {}", stream);
    let mut ast = AstRoot::new(stream);
    ast.parser.next_token();
    let c = ast.atom();
    println!("{:?}", c);
    c
}

fn test_string_expr(stream: &str) -> Result<Expr, AstError> {
    println!("ast test: {}", stream);
    let mut ast = AstRoot::new(stream);
    ast.parser.next_token();
    let c = ast.expr();
    println!("{:?}", c);
    c
}


fn test_ast() {
    let mut a = test_string_var_def("list[int][][] i, y <*test*>");
    assert!(a.is_ok());
    a = test_string_var_def("list[int][][] 5i, y <*test*>");
    assert!(a.is_err());
    // Doesnlt recognise y, this is correct
    a = test_string_var_def("list[int][][] i y <*test*>");
    assert!(a.unwrap().len() == 1);
    a = test_string_var_def("list[i][][] i y <*test*>");
    assert!(a.is_err());

    let mut a = test_string_formal_def("(ref int a, b; char c)");
    assert!(a.is_ok());
    a = test_string_formal_def("(ref int a, b char c)");
    assert!(a.is_err());
    a = test_string_formal_def("(ref int a, b; char c:");
    assert!(a.is_err());

    let mut a = test_string_func_decl("decl int f1(ref int a, b; char c)");
    assert!(a.is_ok());
    a = test_string_func_decl("decl f1(ref int a, b; char c)");
    assert!(a.is_ok());
    a = test_string_func_decl("decl int 3f2(ref int a, b; char c)");
    assert!(a.is_err());
    a = test_string_func_decl("decl int f3 (ref int a, b; char c");
    assert!(a.is_err());

    let mut a = test_string_stmt("exit");
    assert!(a.is_ok());
    a = test_string_stmt("if rings >= 1:\nhanoi(rings-1, source, auxiliary, target)\nmove(source, target)\nhanoi(rings-1, auxiliary, target, source)\nend");
    assert!(a.is_ok());
    
    a = test_string_stmt("if rings >= 1:\nhanoi(rings-1, source, auxiliary, target)\nelif 4 > 5 : \nmove(source, target)\nelif 3 > 2 : \nhanoi(rings-1, auxiliary, target, source)\nelse: makakas(i)\nend");
    assert!(a.is_ok());
    
    a = test_string_stmt("if rings >= 1:\na:=5\nhanoi(rings-1, source, auxiliary, target)\nmove(source, target)\nhanoi(rings-1, auxiliary, target, source)\nend");
    assert!(a.is_ok());

    a = test_string_stmt("for number := 6; number <= limit; number := number + 6:\na := 5\nend");
    assert!(a.is_ok());

    
    a = test_string_stmt("a := 5");
    assert!(a.is_ok());

    let mut a = test_string_atomic("a");
    assert!(a.is_ok());
    a = test_string_atomic("\"exit\"");
    assert!(a.is_ok());
    a = test_string_atomic("exit");
    assert!(a.is_err());
    a = test_string_atomic("a[5]");
    assert!(a.is_ok());
    a = test_string_atomic("a()");
    assert!(a.is_ok());
    a = test_string_atomic("a[");
    assert!(a.is_err());
    a = test_string_atomic("a(");
    assert!(a.is_err());

    let mut a = test_string_expr("1 + 5 + 2");
    assert!(a.is_ok());
    a = test_string_expr("a + 5 + 2");
    assert!(a.is_ok());
    a = test_string_expr("+ 5 + 2");
    assert!(a.is_ok());
    a = test_string_expr("a + 5 +");
    assert!(a.is_err());
    
    a = test_string_expr("4 > 5");
    assert!(a.is_ok());
    a = test_string_expr("new list[int[]][5]");
    assert!(a.is_ok());
}

fn main() {
    // test_parser();
    // test_alloc();
    // test_ast();
    let stream = std::fs::read_to_string("./files/examples/hanoi.t").unwrap();
    let mut ast = AstRoot::new(stream);
    ast.parser.get_token();
    let a = ast.func_def();
    println!("\n\n{:#?}",a);

    // println!("{:?}", fs::read_dir(".").unwrap().collect::<Vec<_>>());
    // let mut ast = Ast::new(&stream);
    // ast.generate();
    // ast.print_tree();
}
