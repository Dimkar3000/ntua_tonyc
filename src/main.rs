//#![feature(global_allocator, allocator_api, heap_api)]
// use std::fs::{DirEntry,read_dir};
use libtonyc::ast::AstRoot;
use libtonyc::ast::Expr;
use libtonyc::parser::TokenKind;
use std::fs::File;
use std::io::prelude::*;
pub fn print_expr(expr: Option<Box<Expr>>) -> String {
    if expr.is_none() {
        return "none".to_owned();
    }
    let expr = *expr.unwrap();
    let to_symbol = |t| {
        match t {
            TokenKind::Addition => "+",
            TokenKind::Subtraction => "-",
            TokenKind::Multiplication => "*",
            TokenKind::Division => "/",
            TokenKind::KMod => "%",
            TokenKind::KOr => "||",
            TokenKind::KAnd => "&&",
            TokenKind::LessOrEqual => "<=",
            TokenKind::Less => "<",
            TokenKind::Equal => "=",
            TokenKind::NotEqual => "<>",
            TokenKind::Great => ">",
            TokenKind::GreatOrEqual => ">=",

            _ => "?",
        }
        .to_owned()
    };
    match expr {
        Expr::Atomic(_, _) => format!("atomic"),

        Expr::CChar(c) => format!("{}", c),
        Expr::CInt(n) => format!("{}", n),
        Expr::Unary(t, e) => format!("({} {})", to_symbol(t), print_expr(e)),
        Expr::Binary(t, a, b) => format!("({} {} {})", print_expr(a), to_symbol(t), print_expr(b)),
        Expr::CBool(b) => format!("{}", b),
        Expr::Comparison(t, a, b) => {
            format!("({} {} {})", print_expr(a), to_symbol(t), print_expr(b))
        }
        Expr::Logical(t, a, b) => format!("({} {} {})", print_expr(a), to_symbol(t), print_expr(b)),
        Expr::Negation(a) => format!("(not {})", print_expr(a)),
        Expr::NilCheck(a) => format!("nil? {}", print_expr(Some(a))),
        Expr::CNil => format!("nil"),
        Expr::NewArray(t, a) => format!("new {:?}[{}]", t, print_expr(Some(a))),
        Expr::Hash(_, a, b) => format!("({}#{})", print_expr(Some(a)), print_expr(Some(b))), // list creation head # tai => format!("{}")l
        Expr::Head(_, a) => format!("head({})", print_expr(Some(a))),
        Expr::Tail(_, a) => format!("tail({})", print_expr(Some(a))),
    }
}
fn main() {
    let mut file = File::open("./files/examples/ok/for_block.t").unwrap();
    let mut stream = String::new();
    file.read_to_string(&mut stream).unwrap();
    let mut a = AstRoot::new(stream);
    a.parser.get_token();
    let tree = a.func_def();
    println!("{:#?}", tree);

    // let mut a = AstRoot::new("1 > 3 * (- 3) + 2");
    // let mut a = AstRoot::new("not 1>3 * 3 mod (-3) and 'a' = 'b' or (-1) > 2");
    // let stream = "true or false and false or true or not 1>2+3*(-3) and (true or not false)";
    // let stream = "tail(1#nil)";
    // let mut a = AstRoot::new(stream);
    // a.parser.get_token();
    // let e = a.expr(false);
    // // println!("here");
    // if e.is_ok() {
    //     // println!("here1");
    //     println!("Test: {}", stream);
    //     println!("{}", print_expr(Some(e.unwrap().bx())));
    // } else {
    //     // println!("here2");
    //     println!("{:?}", e);
    // }
}
