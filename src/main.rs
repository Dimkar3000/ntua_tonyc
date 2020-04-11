// use libtonyc::ast::AstRoot;
// use libtonyc::ast::Expr;
// use libtonyc::parser::TokenKind;
// use std::fs::File;
// use std::io::prelude::*;
// pub fn print_expr(expr: Option<Box<Expr>>) -> String {
//     if expr.is_none() {
//         return "none".to_owned();
//     }
//     let expr = *expr.unwrap();
//     let to_symbol = |t| {
//         match t {
//             TokenKind::Addition => "+",
//             TokenKind::Subtraction => "-",
//             TokenKind::Multiplication => "*",
//             TokenKind::Division => "/",
//             TokenKind::KMod => "%",
//             TokenKind::KOr => "||",
//             TokenKind::KAnd => "&&",
//             TokenKind::LessOrEqual => "<=",
//             TokenKind::Less => "<",
//             TokenKind::Equal => "=",
//             TokenKind::NotEqual => "<>",
//             TokenKind::Great => ">",
//             TokenKind::GreatOrEqual => ">=",
//
//             _ => "?",
//         }
//         .to_owned()
//     };
//     match expr {
//         Expr::Atomic(_, _) => format!("atomic"),
//
//         Expr::CChar(c) => format!("{}", c),
//         Expr::CInt(n) => format!("{}", n),
//         Expr::Unary(t, e) => format!("({} {})", to_symbol(t), print_expr(e)),
//         Expr::Binary(t, a, b) => format!("({} {} {})", print_expr(a), to_symbol(t), print_expr(b)),
//         Expr::CBool(b) => format!("{}", b),
//         Expr::Comparison(t, a, b) => {
//             format!("({} {} {})", print_expr(a), to_symbol(t), print_expr(b))
//         }
//         Expr::Logical(t, a, b) => format!("({} {} {})", print_expr(a), to_symbol(t), print_expr(b)),
//         Expr::Negation(a) => format!("(not {})", print_expr(a)),
//         Expr::NilCheck(a) => format!("nil? {}", print_expr(Some(a))),
//         Expr::CNil => format!("nil"),
//         Expr::NewArray(t, a) => format!("new {:?}[{}]", t, print_expr(Some(a))),
//         Expr::Hash(_, a, b) => format!("({}#{})", print_expr(Some(a)), print_expr(Some(b))), // list creation head # tai => format!("{}")l
//         Expr::Head(_, a) => format!("head({})", print_expr(Some(a))),
//         Expr::Tail(_, a) => format!("tail({})", print_expr(Some(a))),
//     }
// }
// fn main() {
//     let mut file = File::open("./files/examples/ok/for_block.t").unwrap();
//     let mut stream = String::new();
//     file.read_to_string(&mut stream).unwrap();
//     let mut a = AstRoot::new(stream);
//     a.parser.get_token();
//     let tree = a.func_def();
//     println!("{:#?}", tree);
//
//     // let mut a = AstRoot::new("1 > 3 * (- 3) + 2");
//     // let mut a = AstRoot::new("not 1>3 * 3 mod (-3) and 'a' = 'b' or (-1) > 2");
//     // let stream = "true or false and false or true or not 1>2+3*(-3) and (true or not false)";
//     // let stream = "tail(1#nil)";
//     // let mut a = AstRoot::new(stream);
//     // a.parser.get_token();
//     // let e = a.expr(false);
//     // // println!("here");
//     // if e.is_ok() {
//     //     // println!("here1");
//     //     println!("Test: {}", stream);
//     //     println!("{}", print_expr(Some(e.unwrap().bx())));
//     // } else {
//     //     // println!("here2");
//     //     println!("{:?}", e);
//     // }
// }

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::{Linkage, Module};
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
};
use inkwell::OptimizationLevel;
use std::error::Error;

/// Convenience type alias for the `sum` function.
///
/// Calling this is innately `unsafe` because there's no guarantee it doesn't
/// do `unsafe` operations internally.
type SumFunc = unsafe extern "C" fn(u64, u64, u64) -> u64;
type MainFunc = unsafe extern "C" fn() -> u64;
struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    fn jit_compile_sum(&self) -> Option<JitFunction<SumFunc>> {
        let i64_type = self.context.i64_type();
        let fn_type = i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false);
        let function = self.module.add_function("sum", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        let x = function.get_nth_param(0)?.into_int_value();
        let y = function.get_nth_param(1)?.into_int_value();
        let z = function.get_nth_param(2)?.into_int_value();

        let sum = self.builder.build_int_add(x, y, "sum");
        let sum = self.builder.build_int_add(sum, z, "sum");

        self.builder.build_return(Some(&sum));
        // println!("{:?}", );

        unsafe { self.execution_engine.get_function("sum").ok() }
    }
    fn compile_expr(&self, expr: Expr) -> Option<()> {
        match expr {
            Expr::Binary(TokenKind::Addition, Some(a), Some(b)) => {
                let a = match *a {
                    Expr::CInt(n) => n,
                    _ => return None,
                };
                let b = match *b {
                    Expr::CInt(n) => n,
                    _ => return None,
                };
                let i16_type = self.context.i64_type();
                let f = i16_type.fn_type(&[], false);
                let function = self.module.add_function("main", f, Some(Linkage::Common));
                println!("{:?}", function);
                let basic_block = self.context.append_basic_block(function, "entry");
                self.builder.position_at_end(basic_block);
                let x = i16_type.const_int(a as u64, false);
                let y = i16_type.const_int(b as u64, false);

                println!("{:?},{:?}", x, y);
                let sum2 = self.builder.build_int_add(x, y, "s");
                println!("sum = {:?}", sum2);
                self.builder.build_return(Some(&sum2));
                Some(())
            }
            _ => None,
        }
    }
}
use inkwell::DLLStorageClass::Export;
use libtonyc::ast::Expr;
use libtonyc::parser::TokenKind;
use std::fmt::Debug;
use std::path::Path;

fn main() -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let module = context.create_module("main");
    let execution_engine = module.create_execution_engine()?;
    let codegen = CodeGen {
        context: &context,
        module,
        builder: context.create_builder(),
        execution_engine,
    };

    // let sum = codegen
    //     .jit_compile_sum()
    //     .ok_or("Unable to JIT compile `sum`")?;
    let expr = Expr::Binary(
        TokenKind::Addition,
        Some(Expr::CInt(1).bx()),
        Some(Expr::CInt(2).bx()),
    );
    codegen.compile_expr(expr);

    Target::initialize_native(&InitializationConfig::default())
        .expect("Failed to initialize native target");
    let opt = OptimizationLevel::Aggressive;
    let reloc = RelocMode::Default;
    let model = CodeModel::Default;
    let path = Path::new("./target/main.asm");
    let target = Target::from_name("x86-64").unwrap();
    println!("{:?}", TargetMachine::get_default_triple());
    let target_machine = target
        .create_target_machine(
            &TargetMachine::get_default_triple(),
            "x86-64",
            "+avx2",
            opt,
            reloc,
            model,
        )
        .unwrap();

    target_machine.write_to_file(&codegen.module, FileType::Object, path);
    // codegen.module.write_bitcode_to_path(path);
    Ok(())
}
