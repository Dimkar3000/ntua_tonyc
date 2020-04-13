// use libtonyc::ast::AstRoot;
// use libtonyc::ast::Expr;
// use libtonyc::parser::TokenKind;
// use std::fs::File;
// use std::io::prelude::*;
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
use inkwell::{AddressSpace, IntPredicate, OptimizationLevel};
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
    fn compile_expr(&self, expr: Expr) -> AnyValueEnum {
        match expr {
            Expr::CInt(n) => self.context.i16_type().const_int(n as u64, true).into(),

            Expr::CBool(b) => self.context.bool_type().const_int(b as u64, false).into(),
            Expr::CChar(c) => self.context.i8_type().const_int(c as u64, false).into(),
            Expr::CNil => AnyValueEnum::ArrayValue(self.context.i8_type().const_array(&[])),
            Expr::Negation(Some(a)) => {
                let a = self.compile_expr(*a);
                self.builder.build_not(a.into_int_value(), "not").into()
            }
            Expr::Logical(t, Some(a), Some(b)) => {
                let a = self.compile_expr(*a);
                let b = self.compile_expr(*b);
                match t {
                    TokenKind::KAnd => self
                        .builder
                        .build_and(a.into_int_value(), b.into_int_value(), "and")
                        .as_any_value_enum(),
                    TokenKind::KOr => self
                        .builder
                        .build_or(a.into_int_value(), b.into_int_value(), "and")
                        .as_any_value_enum(),
                    e => unreachable!("Logical operator should never have this token: {:?}", e),
                }
            }
            Expr::Comparison(t, Some(a), Some(b)) => {
                let a = self.compile_expr(*a);
                let b = self.compile_expr(*b);
                let op = match t {
                    TokenKind::Equal => IntPredicate::EQ,
                    TokenKind::NotEqual => IntPredicate::NE,
                    TokenKind::Less => IntPredicate::SLT,
                    TokenKind::LessOrEqual => IntPredicate::SLE,
                    TokenKind::Great => IntPredicate::SGT,
                    TokenKind::GreatOrEqual => IntPredicate::SGE,
                    e => unreachable!("Comparison should never have this token: {:?}", e),
                };
                self.builder
                    .build_int_compare(op, a.into_int_value(), b.into_int_value(), "eq")
                    .as_any_value_enum()
            }
            Expr::Binary(t, Some(a), Some(b)) => {
                let x = self.compile_expr(*a).into_int_value();
                let y = self.compile_expr(*b).into_int_value();
                match t {
                    TokenKind::Addition => self.builder.build_int_add(x, y, "s"),
                    TokenKind::Subtraction => self.builder.build_int_sub(x, y, "s"),
                    TokenKind::Multiplication => self.builder.build_int_mul(x, y, "s"),
                    TokenKind::Division => self.builder.build_int_signed_div(x, y, "s"),
                    TokenKind::KMod => self.builder.build_int_unsigned_rem(x, y, "s"),
                    e => unreachable!("Binary operation should never have this token: {:?}", t),
                }
                .as_any_value_enum()
            }
            e => unreachable!("unimplemented Expression: {:?}", e),
        }
    }
}
// use core::num::dec2flt::rawfp::encode_normal;
use inkwell::attributes::Attribute;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::types::{AnyType, AnyTypeEnum, BasicType, IntType};
use inkwell::values::{AnyValue, AnyValueEnum, BasicValue, BasicValueEnum, IntValue};
use inkwell::DLLStorageClass::Export;
use libtonyc::ast::Atomic;
use libtonyc::ast::{AstRoot, Expr};
use libtonyc::parser::TokenKind;
use libtonyc::parser::TokenKind::{Comma, KFalse};
use std::any::Any;
use std::convert::TryInto;
use std::fmt::Debug;
use std::hint::unreachable_unchecked;
use std::path::Path;
use std::process::{exit, Command};

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

    let i16_type = codegen.context.i16_type();
    let f = i16_type.fn_type(&[], false);
    let function = codegen.module.add_function("main", f, None);
    let basic_block = codegen.context.append_basic_block(function, "entry");
    codegen.builder.position_at_end(basic_block);

    // Test: 1 + 2 * 9 - (4/2) % 3
    let mut ast = AstRoot::new("1+2");
    ast.parser.get_token();
    let mut expr = ast.expr(false).unwrap_or_else(|e| {
        println!("{:?}", e);

        exit(1)
    });
    println!("{:?}", print_expr(Some(Box::new(expr.clone()))));
    // let printf_type = codegen.context.i32_type().fn_type(
    //     &[codegen.context.i8_type().ptr_type(AddressSpace::Generic).],
    //     true,
    // );
    //
    // let func_printf = codegen
    //     .module
    //     .add_function("printf", printf_type, Some(Linkage::External));
    // let format = codegen.builder.build_global_string_ptr("hello", "format");
    // let vargs = codegen.builder.build_va_arg(
    //     codegen.context.i16_type().as_pointer_value(),
    //     codegen.context.i16_type(),
    //     "vargs",
    // );
    // println!("va: {:?}", vargs);
    // func_printf.set_call_conventions(0);
    // codegen
    //     .builder
    //     .build_call(func_printf, &[format, vargs], "callPrintf");
    // let atr = printf.get_at
    // codegen.builder.build_call(
    //     printf,
    //     &[
    //         // format.as_basic_value_enum(),
    //         // sum.into_int_value().as_basic_value_enum(),
    //     ],
    //     "putChat",
    // );
    // let puti = codegen.module.add_global(t, None, "puti");
    // codegen
    //     .builder
    //     .build_call(puti, &[sum.into_int_value().as_basic_value_enum()], "puti");

    // let m = codegen
    //     .context
    //     .create_module_from_ir(MemoryBuffer::create_from_file(Path::new("lib.ll")).unwrap())
    //     .unwrap();
    let putct = codegen
        .context
        .void_type()
        .fn_type(&[codegen.context.i8_type().into()], false);
    println!("{:?}", putct);
    let putc = codegen
        .module
        .add_function("puti8", putct, Some(Linkage::External));
    println!("{:?}", putc);

    let putbt = codegen
        .context
        .void_type()
        .fn_type(&[codegen.context.bool_type().into()], false);
    let putb = codegen
        .module
        .add_function("putb", putbt, Some(Linkage::External));

    let putit = codegen
        .context
        .void_type()
        .fn_type(&[codegen.context.i16_type().as_basic_type_enum()], false);
    let puti = codegen
        .module
        .add_function("puti", putit, Some(Linkage::External));

    let getit = codegen.context.i16_type().fn_type(&[], false);
    let geti = codegen
        .module
        .add_function("geti", getit, Some(Linkage::External));
    let getbt = codegen.context.bool_type().fn_type(&[], false);
    let getb = codegen
        .module
        .add_function("getb", getbt, Some(Linkage::External));
    let b_ret = codegen.builder.build_call(getb, &[], "callb");
    codegen.builder.build_call(
        putb,
        &[b_ret.try_as_basic_value().left().unwrap().into()],
        "ptuaid",
    );

    let ret_val = codegen.builder.build_call(geti, &[], "callgeti");
    codegen.builder.build_call(
        puti,
        &[ret_val.try_as_basic_value().left().unwrap()],
        "putical",
    );

    codegen.builder.build_call(
        putc,
        &[codegen
            .context
            .i8_type()
            .const_int('\n' as u64, false)
            .into()],
        "call_putcs",
    );
    codegen.builder.build_call(
        putb,
        &[codegen
            .context
            .bool_type()
            .const_int(0 as u64, false)
            .into()],
        "putib",
    );

    let s = unsafe {
        codegen
            .builder
            .build_global_string(&"\nHello World\n", "str")
    }
    .as_pointer_value();
    println!("s: {:?}", s);

    let putst = codegen.context.void_type().fn_type(
        &[codegen
            .context
            .i8_type()
            .ptr_type(AddressSpace::Generic)
            .into()],
        false,
    );
    let puts = codegen
        .module
        .add_function("putstring", putst, Some(Linkage::External));
    codegen.builder.build_call(
        puts,
        &[
            s.const_cast(codegen.context.i8_type().ptr_type(AddressSpace::Generic))
                .into(),
        ],
        "putcall",
    );

    let getst = codegen.context.void_type().fn_type(
        &[
            context.i16_type().into(),
            context.i8_type().ptr_type(AddressSpace::Generic).into(),
        ],
        false,
    );
    let gets = codegen
        .module
        .add_function("gets", getst, Some(Linkage::External));
    println!("gets: {:?}", gets);
    let getct = codegen.context.i8_type().fn_type(&[], false);
    let getc = codegen
        .module
        .add_function("getcchar", getct, Some(Linkage::External));

    let p = codegen.context.i8_type();
    let n = codegen.context.i16_type().const_int(10, false);
    let p1 = codegen.builder.build_array_malloc(p, n, "sss").unwrap();
    println!("p1: {:?}", p1);
    codegen
        .builder
        .build_call(gets, &[n.into(), p1.into()], "asda");
    codegen.builder.build_call(puts, &[p1.into()], "asdads");
    let c = codegen.builder.build_call(getc, &[], "asddas");
    codegen.builder.build_call(
        putc,
        &[c.try_as_basic_value().left().unwrap().into()],
        "asdasd",
    );
    let sum = codegen.compile_expr(expr);
    // let sum = codegen.context.i16_type().const_int(44, true);
    println!("sum {:?}", sum);

    codegen
        .builder
        .build_call(puti, &[sum.into_int_value().into()], "print_sum");
    codegen.builder.build_return(Some(
        &sum.into_int_value().const_cast(i16_type, false), // .const_add(i16_type.const_int(5 as u64, false)),
    ));

    // let m = codegen
    //     .context
    //     .create_module_from_ir(MemoryBuffer::create_from_file(Path::new("lib.ll")).unwrap())
    //     .unwrap();
    // let puti = unsafe {
    //     m.create_jit_execution_engine(OptimizationLevel::Default)?
    //         .get_function::<fn(i16)>("puti")
    //         .unwrap()
    // };
    // unsafe {
    //     puti.call(5i16);
    // }
    // println!("{:?}", unsafe { ee.call() });
    // codegen.module.print_to_file("test.ir");

    Target::initialize_native(&InitializationConfig::default())
        .expect("Failed to initialize native target");
    let opt = OptimizationLevel::Aggressive;
    let reloc = RelocMode::Default;
    let model = CodeModel::Default;
    let path = Path::new("./target/main.o");
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
    target_machine.write_to_file(&codegen.module, FileType::Object, path)?;
    // build std 
    let s = Command::new("clang++")
            .args(&["-c", "-o","target/libtonystd.o", "libtonystd.c"])
            .output()
            .expect("failed to execute process");
    println!("building std: {:?}", s);
    let r = Command::new("clang++").args(&[path.to_str().unwrap(), "target/libtonystd.o"]).output().expect("failed to link");
    println!("linking std: {:?}", r);

    Ok(())
}
