// extern crate ntua_tonyc;
use libtonyc::ast::AstRoot;
use std::ffi::OsStr;
use std::fs::File;
use std::fs::{read_dir, DirEntry};
use std::io::prelude::*;
use std::path::Path;

fn test_dir(dir: &Path, cb: &dyn Fn(&DirEntry)) -> std::io::Result<()> {
    if dir.is_dir() {
        for entry in read_dir(dir)? {
            let entry = entry?;
            cb(&entry);
        }
    } else {
        println!("not a folder: {:?}", dir);
    }
    Ok(())
}

#[test]
fn test_ok() {
    let stream = std::fs::read_to_string("./files/examples/ok/hanoi.t").unwrap();
    let mut ast = AstRoot::new(stream);
    ast.parser.get_token();
    test_dir(Path::new("./files/examples/ok/"), &|x| {
        let path = x.path();
        if path.is_file() && path.extension() == Some(OsStr::new("t")) {
            println!("\nTesting file: {:?}", path);
            let mut file = File::open(path).unwrap();
            let mut stream = String::new();
            file.read_to_string(&mut stream).unwrap();
            let mut a = AstRoot::new(stream);
            a.parser.next_token();
            let tree = a.func_def();
            println!("{:?}",tree);

            assert!(tree.is_ok());
            println!("Passed");
        }
    })
    .unwrap();
}

#[test]
fn test_error() {
    test_dir(Path::new("./files/examples/error/"), &|x| {
        let path = x.path();
        if path.is_file() && path.extension() == Some(OsStr::new("t")) {
            println!("\nTesting file: {:?}", path);
            let mut file = File::open(path).unwrap();
            let mut stream = String::new();
            file.read_to_string(&mut stream).unwrap();
            let mut a = AstRoot::new(stream);
            a.parser.next_token();
            let tree = a.func_def();
            // println!("{:?}",tree);
            // std::io::stdout().flush();
            assert!(tree.is_err());
        }
    })
    .unwrap();
}
