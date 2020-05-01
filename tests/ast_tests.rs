use libtonyc::ast::{AstRoot, FuncDef};
use libtonyc::error::Error;

fn read_file(path: &str) -> Result<FuncDef, Error> {
    use std::fs::File;
    use std::io::prelude::*;

    let mut file = File::open(path).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    let mut ast = AstRoot::new(&contents);
    ast.generate()
}

#[test]
fn test_ok() {
    use std::fs;
    for entry in fs::read_dir("./files/examples/ok/").unwrap() {
        println!("Testing: {:?}", entry);
        let entry = entry.unwrap().path();
        let p = entry.to_str().unwrap();
        let result = read_file(p);
        println!("{:?}", result);
        assert!(result.is_ok());
    }
}

#[test]
fn test_error() {
    use std::fs;
    for entry in fs::read_dir("./files/examples/error/").unwrap() {
        println!("Testing: {:?}", entry);
        let entry = entry.unwrap().path();
        let p = entry.to_str().unwrap();
        let result = read_file(p);
        println!("{:?}", result);
        assert!(result.is_err());
    }
}
