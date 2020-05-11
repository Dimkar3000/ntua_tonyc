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
#[ignore]
fn test_ok() {
    use std::fs;
    for entry in fs::read_dir("./files/examples/ok/").unwrap() {
        let entry = entry.unwrap().path();
        let entry = entry.as_path();
        if entry.is_dir()
            || entry.extension().is_none()
            || (entry.extension().is_some() && entry.extension().unwrap() != "tony")
        {
            continue;
        }
        println!("Testing: {:?}", entry);
        let p = entry.to_str().unwrap();
        let result = read_file(p);
        println!("{:?}", result);
        assert!(result.is_ok());
    }
}

#[test]
#[ignore]
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
