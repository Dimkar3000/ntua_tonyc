use inkwell::context::Context;
use libtonyc::ast::*;
use libtonyc::codegen::CodeGen;
use std::error::Error;
use std::io::prelude::*;
use std::path::Path;
use std::process::Command;

fn read_file(path: &str) -> AstRoot {
    use std::fs::File;
    use std::io::prelude::*;

    let mut file = File::open(path).unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    AstRoot::new(&contents)
}

#[test]
fn rust_test_suite() {
    use std::fs;
    for entry in fs::read_dir("./files/examples/ok/no_blocking").unwrap() {
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
        let mut ast = read_file(p);
        let context = Context::create();
        let module = context.create_module("main");
        let mut codegen = CodeGen::new(&context, module);
        assert!(codegen.is_ok());
        let mut codegen = codegen.unwrap();
        let path = Path::new(p);
        let a = codegen.compile(&mut ast, false, false, false, &path);
        assert_eq!(a.ok(), Some(()));
        let ext = if cfg!(windows) { "exe" } else { "" };
        let result = Command::new(path.with_extension(ext))
            .output()
            .expect(&format!(
                "failed to execute: {:?}",
                Path::new(p).with_extension(ext)
            ));
        assert_eq!(result.status.code(), Some(0));
        println!("Passed");
    }
}
