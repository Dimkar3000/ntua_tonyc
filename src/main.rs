//#![feature(global_allocator, allocator_api, heap_api)]
// use std::fs::{DirEntry,read_dir};
use libtonyc::ast::AstRoot;
use std::fs::File;
use std::io::prelude::*;


fn main() {
    let mut file = File::open("./files/examples/ok/syntax_if_elif_el.t").unwrap();
    let mut stream = String::new();
    file.read_to_string(&mut stream).unwrap();
    let mut a = AstRoot::new(stream);
    println!("{:?}", a.parser.get_token());
    let tree = a.func_def();
    println!("{:?}", tree);
}
