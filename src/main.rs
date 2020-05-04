use inkwell::context::Context;
use libtonyc::ast::*;
use libtonyc::codegen::CodeGen;
use std::error::Error;

use std::fs::File;

use std::io::prelude::*;
use std::path::Path;

#[macro_use]
extern crate clap;

use clap::{App, Arg};

fn main() -> Result<(), Box<dyn Error>> {
    let matches = App::new("tonyc")
        .version("1.0")
        .author(crate_authors!("\n"))
        .about("Compiler for the programming language tony")
        .arg(
            Arg::with_name("opt")
                .short('Ο')
                .about("Enable Optimizations")
                .takes_value(false),
        )
        .arg(
            Arg::with_name("INPUT")
                .takes_value(true)
                .about("The path to *.tony file")
                .required_unless("final")
                .required_unless("intermidiate")
                .validator(|f| {
                    if f.ends_with(".tony") {
                        Ok(())
                    } else {
                        Err("the file extension should be *.tony")
                    }
                }),
        )
        .arg(
            Arg::with_name("intermidiate")
                .short('i')
                .about("import from stdin, export intermidiate code to stdout")
                .takes_value(false)
                .conflicts_with("final")
                .conflicts_with("INPUT"),
        )
        .arg(
            Arg::with_name("final")
                .short('f')
                .about("import from stdin, export final code to stdout")
                .takes_value(false)
                .conflicts_with("intermidiate")
                .conflicts_with("INPUT"),
        )
        .arg(
            Arg::with_name("optimize")
                .short('O')
                .about("enable optimizations")
                .takes_value(false),
        )
        .get_matches();

    let context = Context::create();
    let module = context.create_module("main");
    let mut codegen = CodeGen::new(&context, module)?;
    // Read Input
    let mut data = Vec::new();
    if matches.is_present("intermidiate") || matches.is_present("final") {
        std::io::stdin().read_to_end(&mut data)?;
    } else {
        let filename = matches.value_of("INPUT").unwrap();
        File::open(filename)?.read_to_end(&mut data)?;
    }
    let source = String::from_utf8(data)?;
    let p = Path::new(matches.value_of("INPUT").unwrap_or("./main"));
    // Compile code
    let mut ast = AstRoot::new(&source);
    let a = codegen.compile(
        &mut ast,
        matches.is_present("intermidiate"),
        matches.is_present("final"),
        matches.is_present("optimize"),
        &p,
    );
    if let Err(a) = a {
        return Err(Box::new(a));
    }
    Ok(())
}
