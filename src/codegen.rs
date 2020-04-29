use crate::ast::Expr;
use crate::parser::TokenKind;

use std::collections::HashMap;
use std::path::Path;
use std::process::Command;
use std::result::Result;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::IntPredicate;

use inkwell::module::Linkage;
use inkwell::passes::*;
use inkwell::targets::*;
use inkwell::types::*;
use inkwell::values::*;
use inkwell::AddressSpace;
use inkwell::OptimizationLevel;

use crate::ast::*;

pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub global_funcs: HashMap<String, FunctionValue<'ctx>>,
    pub variable_list: Vec<(String, PointerValue<'ctx>)>,
}

impl<'ctx> CodeGen<'ctx> {
    fn typdecl_str(&self, t: &TypeDecl) -> String {
        match t {
            TypeDecl::Char => "c".to_owned(),
            TypeDecl::Nil => "n".to_owned(),
            TypeDecl::Bool => "b".to_owned(),
            TypeDecl::Int => "i".to_owned(),
            TypeDecl::List(tmp) => format!("list_{}", self.typdecl_str(tmp)),
            TypeDecl::Array(tmp) => format!("array_{}", self.typdecl_str(tmp)),
            e => panic!("called typdecl_str with: {:?}", e),
        }
    }

    fn typed_size_of(&self, ctype: &BasicTypeEnum<'ctx>) -> Option<IntValue<'ctx>> {
        match &ctype {
            BasicTypeEnum::ArrayType(v) => v.size_of(),
            BasicTypeEnum::IntType(v) => Some(v.size_of()),
            BasicTypeEnum::FloatType(v) => Some(v.size_of()),
            BasicTypeEnum::PointerType(v) => Some(v.size_of()),
            BasicTypeEnum::StructType(v) => v.size_of(),
            BasicTypeEnum::VectorType(v) => v.size_of(),
        }
    }

    fn typedecl_to_type(&self, t: &TypeDecl) -> BasicTypeEnum<'ctx> {
        match t {
            TypeDecl::Bool => self.context.bool_type().into(),
            TypeDecl::Char => self.context.i8_type().into(),
            TypeDecl::Int => self.context.i16_type().into(),
            TypeDecl::Array(t) => self
                .typedecl_to_type(&*t)
                .ptr_type(AddressSpace::Generic)
                .into(),
            TypeDecl::Nil => self.context.opaque_struct_type("nill").into(),
            TypeDecl::List(t) => {
                let type_name = format!("list_{}", self.typdecl_str(t));
                match self.module.get_type(&type_name) {
                    Some(k) => k,
                    None => {
                        let data = self.typedecl_to_type(t);
                        let node = self.context.opaque_struct_type(&type_name);
                        let is_opaque = node.set_body(
                            &[
                                data,
                                node.ptr_type(AddressSpace::Generic).into(),
                                self.context.bool_type().into(),
                            ],
                            true,
                        );
                        assert!(is_opaque);
                        node.into()
                    }
                }
            }
            e => panic!("called typedecl_to_type with {:?}", e),
        }
    }

    fn funcdef_to_function(&mut self, func: &FuncDef) -> FunctionValue<'ctx> {
        let mut args = Vec::new();
        for i in &func.header.arguments {
            let t = self.typedecl_to_type(&i.def.var_type);
            if i.is_ref {
                args.push(t.ptr_type(AddressSpace::Generic).into());
            } else {
                args.push(t);
            }
        }
        let fun_type;
        if func.header.rtype == TypeDecl::Void {
            fun_type = self.context.void_type().fn_type(&args, false);
        } else {
            fun_type = self
                .typedecl_to_type(&func.header.rtype)
                .fn_type(&args, false);
        }
        match self.module.get_function(&func.header.name) {
            Some(k) => k,
            None => {
                let r = self.module.add_function(&func.header.name, fun_type, None);
                self.store_function(func.header.name.clone(), r);
                r
            }
        }
    }

    fn funcdecl_to_function(&mut self, func: &FuncDecl) -> FunctionValue<'ctx> {
        let mut args = Vec::new();
        for i in &func.arguments {
            let t = self.typedecl_to_type(&i.def.var_type);
            if i.is_ref {
                args.push(t.ptr_type(AddressSpace::Generic).into());
            } else {
                args.push(t);
            }
        }
        let fun_type;
        if func.rtype == TypeDecl::Void {
            fun_type = self.context.void_type().fn_type(&args, false);
        } else {
            fun_type = self.typedecl_to_type(&func.rtype).fn_type(&args, false);
        }
        match self.module.get_function(&func.name) {
            Some(k) => k,
            None => {
                let r = self.module.add_function(&func.name, fun_type, None);
                self.store_function(func.name.clone(), r);
                r
            }
        }
    }

    fn find_pointer(&self, name: &String) -> Option<PointerValue<'ctx>> {
        self.variable_list
            .iter()
            .rev()
            .find_map(|x| if &x.0 == name { Some(x.1) } else { None })
    }

    fn store_function(&mut self, name: String, f: FunctionValue<'ctx>) {
        match self.global_funcs.insert(name, f) {
            Some(_) => (),
            None => (),
        }
    }
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, module: Module<'ctx>) -> Self {
        let mut std = HashMap::new();

        let gc_malloct = context
            .i8_type()
            .ptr_type(AddressSpace::Generic)
            .fn_type(&[context.i64_type().into()], false);
        let gc_malloc = module.add_function("GC_malloc", gc_malloct, Some(Linkage::External));
        std.insert("GC_malloc".to_owned(), gc_malloc);

        let strlent = context.i16_type().fn_type(
            &[context.i8_type().ptr_type(AddressSpace::Generic).into()],
            false,
        );
        let strlen = module.add_function("strlen", strlent, Some(Linkage::External));
        std.insert("strlen".to_owned(), strlen);

        let putit = context
            .void_type()
            .fn_type(&[context.i16_type().as_basic_type_enum()], false);
        let puti = module.add_function("puti", putit, Some(Linkage::External));
        std.insert("puti".to_owned(), puti);

        // Function: decl putb (bool b)
        let putbt = context
            .void_type()
            .fn_type(&[context.bool_type().into()], false);
        let putb = module.add_function("putb", putbt, Some(Linkage::External));
        std.insert("putb".to_owned(), putb);

        // Function: decl putc (char c)
        let putct = context
            .void_type()
            .fn_type(&[context.i8_type().into()], false);
        let putc = module.add_function("puti8", putct, Some(Linkage::External));
        std.insert("putc".to_owned(), putc);

        // Warning(dimkar): This Function has a different name in the c code "putstring", just to void colission with c++ function names
        // Function: decl puts (char[] s)
        let putst = context.void_type().fn_type(
            &[context.i8_type().ptr_type(AddressSpace::Generic).into()],
            false,
        );
        let puts = module.add_function("putstring", putst, Some(Linkage::External));
        std.insert("puts".to_owned(), puts);
        module.add_function("puts", putst, Some(Linkage::External));

        // Function: decl int geti ()
        let getit = context.i16_type().fn_type(&[], false);
        let geti = module.add_function("geti", getit, Some(Linkage::External));
        std.insert("geti".to_owned(), geti);

        // Function: decl bool getb ()
        let getbt = context.bool_type().fn_type(&[], false);
        let getb = module.add_function("getb", getbt, Some(Linkage::External));
        std.insert("getb".to_owned(), getb);

        // Function: decl gets (int n, char[] s)
        let getst = context.void_type().fn_type(
            &[
                context.i16_type().into(),
                context.i8_type().ptr_type(AddressSpace::Generic).into(),
            ],
            false,
        );
        let gets = module.add_function("gets", getst, Some(Linkage::External));
        std.insert("gets".to_owned(), gets);

        // Function: decl char getc ()
        let getct = context.i8_type().fn_type(&[], false);
        let getc = module.add_function("getcchar", getct, Some(Linkage::External));
        std.insert("getc".to_owned(), getc);

        CodeGen {
            context,
            module: module,
            builder: context.create_builder(),
            global_funcs: std,
            variable_list: Vec::new(),
        }
    }

    pub fn compile<'a>(&mut self, ast: &'a mut AstRoot<'a>) -> Result<(), String> {
        // Build Ast
        let main = ast.generate();
        if main.is_err() {
            return Err(format!("{}", main.unwrap_err()));
        }
        let main = main.unwrap();
        if main.header.arguments.len() != 0 || main.header.rtype != TypeDecl::Void {
            return Err("Top level Function shouldn't have argument or a return type".to_owned());
        }

        // Create Code
        self.compile_func(&main);

        // Clean global function table
        self.global_funcs.clear();
        self.global_funcs.shrink_to_fit();
        if let Err(e) = self.module.verify() {
            eprintln!("verify error: {}", e);
        }
        // Create FPM
        let fpm = PassManager::create(&self.module);

        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_gvn_pass();
        fpm.add_cfg_simplification_pass();
        fpm.add_basic_alias_analysis_pass();
        fpm.add_promote_memory_to_register_pass();
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();

        fpm.initialize();
        // self.module.print_to_file("test.ll");
        // Build Object file
        Target::initialize_native(&InitializationConfig::default())
            .expect("Failed to initialize native target");
        let opt = OptimizationLevel::Aggressive;
        let reloc = RelocMode::Default;
        let model = CodeModel::Default;
        let path = Path::new("./target/main.o");
        let target = Target::from_name("x86-64").unwrap();
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
        self.module.print_to_file("./test.ll").unwrap();
        match target_machine.write_to_file(&self.module, FileType::Object, path) {
            Ok(()) => (),
            Err(e) => eprintln!("{}", e),
        };

        /*****************
            Extra steps
        *****************/
        // Build STD
        let s = Command::new("clang++")
            .args(&["-c", "-O3", "-o", "target/libtonystd.o", "libtonystd.cpp"])
            .output()
            .expect("failed");
        if !s.status.success() {
            let message = String::from_utf8(s.stderr).unwrap();
            print!("Compiling std failed");
            return Err(message);
        }
        print!("compiling {}\n", s.status);
        // Linking STD
        let r = Command::new("clang++")
            .args(&[
                "-O3",
                path.to_str().unwrap(),
                "target/libtonystd.o",
                "llvm/gc-8.0.4/build/Release/gcmt-lib.lib",
            ])
            .output()
            .expect("failed to link");

        if !r.status.success() {
            let message = String::from_utf8(r.stderr).unwrap();
            print!("Linking std failed");
            return Err(message);
        }
        print!("linking {}\n", s.status);
        Ok(())
    }

    // Sub compilations

    // Func
    fn compile_func(&mut self, func: &FuncDef) {
        // Function Signature
        let function = self.funcdef_to_function(&func);
        // function.set_gc("simongc");
        let entry_block = self
            .context
            .append_basic_block(function, &format!("{}_entry", func.header.name));
        self.builder.position_at_end(entry_block);

        // Function arguments
        for i in 0..func.header.arguments.len() {
            let t = function.get_nth_param(i as u32);
            match t {
                Some(p) => {
                    if func.header.arguments[i].is_ref && p.is_pointer_value() {
                        self.variable_list.push((
                            func.header.arguments[i].def.name.clone(),
                            p.into_pointer_value(),
                        ));
                    } else {
                        let ptr = self
                            .builder
                            .build_alloca(p.get_type(), &func.header.arguments[i].def.name);
                        self.builder.build_store(ptr, p);
                        self.variable_list
                            .push((func.header.arguments[i].def.name.clone(), ptr));
                    }
                }
                None => {
                    panic!("Function should always have as many arguments as in the Func of ast")
                }
            }
        }
        // Create llvm function for all defs and decls
        for i in &func.defs {
            let f = self.funcdef_to_function(&i);
            self.store_function(i.header.name.clone(), f);
        }

        for i in &func.decls {
            let f = self.funcdecl_to_function(&i);
            self.store_function(i.name.clone(), f);
        }

        // Variables
        for i in &func.vars {
            let t = self.typedecl_to_type(&i.var_type);
            let p = match &i.var_type {
                TypeDecl::List(_) => self.create_typed_nil(&t, true).into_pointer_value(),
                _ => self
                    .builder
                    .build_alloca(t, &format!("{}_{}", func.header.name, i.name)),
            };
            self.variable_list.push((i.name.clone(), p));
        }

        // Statements
        self.compile_stmts(func, &entry_block);

        // Cleanup

        // Remove the variable pointers for this function
        for _ in 0..func.header.arguments.len() {
            self.variable_list.pop();
        }
        for _ in 0..func.vars.len() {
            self.variable_list.pop();
        }

        // Compile Function definitions
        for i in &func.defs {
            self.compile_func(i);
        }
    }
    fn compile_stmt(
        &mut self,
        stmt: &Stmt,
        func: &FuncDef,
        block: &BasicBlock<'ctx>,
        exit_block: &BasicBlock<'ctx>,
        exited: &mut bool,
    ) -> bool {
        match stmt {
            Stmt::Exit => {
                self.builder.build_unconditional_branch(*exit_block);
                *exited = true;
            }
            Stmt::Return(exp) => {
                match &**exp {
                    Expr::Atomic(_, a) => {
                        // the result is a variable to load
                        let result = self.get_atom(&a);
                        let r = self.builder.build_load(result, "return");
                        self.builder.build_return(Some(&r));
                        return true;
                    }
                    _ => {
                        let result = self.compile_exp(exp, false);
                        self.builder.build_return(Some(&result));
                        return true;
                    }
                };
            }
            Stmt::If {
                condition: cond,
                stmts: main_stmt,
                elseif: elseifs,
                elseblock: else_stmts,
            } => {
                let condition = self.compile_exp(cond, false);
                assert!(condition.is_int_value());
                let mut then_block = self
                    .context
                    .insert_basic_block_after(*block, &format!("{:?}_if", block.get_name()));
                let mut else_block = self
                    .context
                    .insert_basic_block_after(then_block, &format!("{:?}_else", block.get_name()));
                let if_exit_block = self.context.insert_basic_block_after(
                    then_block,
                    &format!("{:?}_ifexit", block.get_name()),
                );
                self.builder.build_conditional_branch(
                    condition.into_int_value(),
                    then_block,
                    else_block,
                );
                // then block
                self.builder.position_at_end(then_block);
                let mut contains_return = false;
                let mut exited = false;
                for i in main_stmt {
                    if self.compile_stmt(i, func, &then_block, &exit_block, &mut exited) {
                        assert!(!contains_return);
                        contains_return = true;
                    }
                }
                if !contains_return && !exited {
                    self.builder.build_unconditional_branch(if_exit_block);
                }
                self.builder.position_at_end(else_block);
                for i in 0..elseifs.len() {
                    then_block = self
                        .context
                        .insert_basic_block_after(else_block, &format!("else_ifthen{}", i));
                    else_block = self
                        .context
                        .insert_basic_block_after(else_block, &format!("else_ifelse{}", i));
                    let condition = self.compile_exp(&elseifs[i].0, false);
                    assert!(condition.is_int_value());
                    self.builder.build_conditional_branch(
                        condition.into_int_value(),
                        then_block,
                        else_block,
                    );
                    // elsifs
                    self.builder.position_at_end(then_block);
                    let mut contains_return = false;
                    let mut exited = false;
                    for i in &elseifs[i].1 {
                        if self.compile_stmt(&i, func, &then_block, &exit_block, &mut exited) {
                            assert!(!contains_return);
                            contains_return = true;
                        }
                    }
                    if !contains_return && !exited {
                        self.builder.build_unconditional_branch(if_exit_block);
                    }
                    self.builder.position_at_end(else_block);
                }
                // else block
                let mut contains_return = false;
                let mut exited = false;
                for i in else_stmts {
                    if self.compile_stmt(i, func, &else_block, &exit_block, &mut exited) {
                        assert!(!contains_return);
                        contains_return = true;
                    }
                }
                if !contains_return && !exited {
                    self.builder.build_unconditional_branch(if_exit_block);
                }
                self.builder.position_at_end(if_exit_block);
            }
            Stmt::For(pre_stmts, cond, post_stmts, main_stmts) => {
                let mut exited = false;
                for i in pre_stmts {
                    self.compile_stmt(&i, func, block, &exit_block, &mut exited);
                }
                let for_header = self.context.insert_basic_block_after(*block, "for_header");
                let for_body = self
                    .context
                    .insert_basic_block_after(for_header, "for_body");
                let for_exit = self.context.insert_basic_block_after(for_body, "for_exit");
                self.builder.build_unconditional_branch(for_header);
                self.builder.position_at_end(for_header);
                let condition = self.compile_exp(cond, false);
                assert!(condition.is_int_value());
                self.builder.build_conditional_branch(
                    condition.into_int_value(),
                    for_body,
                    for_exit,
                );

                // Body
                let mut exited = false;
                let mut returned = false;
                self.builder.position_at_end(for_body);
                for i in main_stmts {
                    if self.compile_stmt(i, func, &for_body, &for_exit, &mut exited) {
                        assert!(!returned);
                        returned = true;
                    }
                }
                for i in post_stmts {
                    if self.compile_stmt(i, func, &for_body, &for_exit, &mut exited) {
                        assert!(!returned);
                        returned = true;
                    }
                }
                if !returned && !exited {
                    self.builder.build_unconditional_branch(for_header);
                }
                self.builder.position_at_end(for_exit);
            }
            Stmt::Skip => eprintln!("skip statement found, test what happended"),
            Stmt::Call(name, args) => {
                let function = *self.global_funcs.get(name).unwrap();
                let mut v = Vec::new();
                for i in 0..args.len() {
                    let mut is_ref = false;
                    for j in &func.defs {
                        if &j.header.name == name && j.header.arguments[i].is_ref {
                            is_ref = true;
                            break;
                        }
                    }
                    if !is_ref {
                        for j in &func.decls {
                            if &j.name == name && j.arguments[i].is_ref {
                                is_ref = true;
                                break;
                            }
                        }
                    }
                    let r = self.compile_exp(&args[i], is_ref);
                    v.push(r);
                }
                self.builder
                    .build_call(function, &v, &format!("call_{}", name));
            }
            Stmt::Assign(atom, exp) => {
                let ptr = self.get_atom(atom);
                let value = self.compile_exp(exp, false);
                if value.is_pointer_value()
                    && value.into_pointer_value().is_null()
                    && value.into_pointer_value().is_const()
                {
                    let t = atom.get_type();
                    let ctype = self.typedecl_to_type(&t);
                    // Nil handilng for now skip
                    let v = self.create_typed_nil(&ctype, false);
                    self.builder.build_store(ptr, v);
                } else {
                    self.builder.build_store(ptr, value);
                }
            }
        }
        false
    }

    // Stmt
    // Note: I pass the FuncDef some I can look up Function Definition for call statements.
    //       I need to do that to understand how to pass the arguments( by ref or by value).
    fn compile_stmts(&mut self, func: &FuncDef, block: &BasicBlock<'ctx>) {
        let mut exited = false;
        let mut returned = false;
        for i in 0..func.stmts.len() {
            let stmt = &func.stmts[i];
            // Note: not sure what happend when you put an exit inside a function bloxk
            //       probably ast souldn't allow it
            if self.compile_stmt(stmt, func, block, block, &mut exited) {
                assert!(!returned);
                returned = true;
            }
        }
        assert!(!exited);
        if !returned {
            self.builder.build_return(None);
        }
    }

    // Note: Atoms return pointers to values
    pub fn get_atom(&mut self, atom: &Atomic) -> PointerValue<'ctx> {
        match atom {
            Atomic::CString(s) => {
                let g = match self.module.get_global(s) {
                    Some(v) => v.as_basic_value_enum().into_pointer_value(),
                    None => self
                        .builder
                        .build_global_string_ptr(s, &format!("ptr_{:?}", s))
                        .as_basic_value_enum()
                        .into_pointer_value(),
                };
                let p = self.builder.build_alloca(g.get_type(), "tmp");
                self.builder.build_store(p, g);
                p
            }
            Atomic::Accessor(atom, exp) => {
                let ptr = self.get_atom(atom);
                let index = self.compile_exp(exp, false);
                let data = self.builder.build_load(ptr, "store_data");
                unsafe {
                    self.builder.build_in_bounds_gep(
                        data.into_pointer_value(),
                        &[index.into_int_value()],
                        "gep",
                    )
                }
            }
            Atomic::FuncCall(_, name, args) => {
                let function = self.global_funcs[&atom.get_name()];
                let mut v = Vec::new();
                for i in 0..args.len() {
                    let r = self.compile_exp(&args[i], false);
                    if r.is_pointer_value()
                        && r.into_pointer_value().is_null()
                        && r.into_pointer_value().is_const()
                    {
                        // handling nill
                        let arg = function.get_nth_param(i as u32).unwrap();
                        let r0 = self.create_typed_nil(&arg.get_type(), false);
                        v.push(r0.into());
                    } else {
                        v.push(r);
                    }
                }
                // println!("function: {:?}", function);
                // println!("args: {:?}", v);
                let result = self
                    .builder
                    .build_call(function, &v, &format!("call_{}", name))
                    .try_as_basic_value()
                    .left()
                    .unwrap();
                let r = self.builder.build_alloca(result.get_type(), "tmp");
                self.builder.build_store(r, result);
                r
            }
            Atomic::Name(_, name) => match self.find_pointer(name) {
                Some(k) => k,
                None => panic!("variable name not found"),
            },
        }
    }
    fn create_typed_nil(
        &mut self,
        ctype: &BasicTypeEnum<'ctx>,
        is_ref: bool,
    ) -> BasicValueEnum<'ctx> {
        let gc_malloc = self.global_funcs.get("GC_malloc").unwrap();
        let size = ctype.into_struct_type().size_of().unwrap().into();
        let data = self.builder.build_call(*gc_malloc, &[size], "GC_malloc");
        let new_tail = self
            .builder
            .build_bitcast(
                data.try_as_basic_value().left().unwrap(),
                ctype.into_struct_type().ptr_type(AddressSpace::Generic),
                "cast_gc",
            )
            .into_pointer_value();

        let flag = unsafe {
            self.builder.build_in_bounds_gep(
                new_tail,
                &[
                    self.context.i32_type().const_int(0, false),
                    self.context.i32_type().const_int(2, false),
                ],
                "flag",
            )
        };
        self.builder
            .build_store(flag, self.context.bool_type().const_int(1, false));
        if is_ref {
            new_tail.into()
        } else {
            self.builder.build_load(new_tail, "d")
        }
    }

    fn compile_exp(&mut self, exp: &Expr, is_ref: bool) -> BasicValueEnum<'ctx> {
        match exp {
            Expr::Hash(t, left, right) => {
                let ctype = self.typedecl_to_type(t);
                let data = self.compile_exp(left, false);
                let tail_data = self.compile_exp(right, false);
                let tail = if tail_data.get_type().is_pointer_type()
                    && tail_data.into_pointer_value().is_null()
                    && tail_data.into_pointer_value().is_const()
                {
                    self.create_typed_nil(&ctype, true)
                } else {
                    // Copy data to new ptr
                    let gc_malloc = self.global_funcs.get("GC_malloc").unwrap();
                    let size = self.typed_size_of(&ctype).unwrap();
                    let data = self
                        .builder
                        .build_call(*gc_malloc, &[size.into()], "GC_malloc");
                    let p = self
                        .builder
                        .build_bitcast(
                            data.try_as_basic_value().left().unwrap(),
                            ctype.into_struct_type().ptr_type(AddressSpace::Generic),
                            "cast_gc",
                        )
                        .into_pointer_value();

                    self.builder.build_store(p, tail_data);
                    p.into()
                };
                let gc_malloc = self.global_funcs.get("GC_malloc").unwrap();
                let size = self.typed_size_of(&ctype).unwrap();
                let gc_data = self
                    .builder
                    .build_call(*gc_malloc, &[size.into()], "GC_malloc");
                let head = self
                    .builder
                    .build_bitcast(
                        gc_data.try_as_basic_value().left().unwrap(),
                        ctype.into_struct_type().ptr_type(AddressSpace::Generic),
                        "cast_gc",
                    )
                    .into_pointer_value();
                let data_ptr = unsafe {
                    self.builder.build_in_bounds_gep(
                        head,
                        &[
                            self.context.i32_type().const_int(0, false),
                            self.context.i32_type().const_int(0, false),
                        ],
                        "data",
                    )
                };
                self.builder.build_store(data_ptr, data);

                //tail
                let tail_ptr = unsafe {
                    self.builder.build_in_bounds_gep(
                        head,
                        &[
                            self.context.i32_type().const_int(0, false),
                            self.context.i32_type().const_int(1, false),
                        ],
                        "data",
                    )
                };
                self.builder.build_store(tail_ptr, tail);

                //flag
                let is_null_ptr = unsafe {
                    self.builder.build_in_bounds_gep(
                        head,
                        &[
                            self.context.i32_type().const_int(0, false),
                            self.context.i32_type().const_int(2, false),
                        ],
                        "data",
                    )
                };
                self.builder
                    .build_store(is_null_ptr, self.context.bool_type().const_int(0, false));
                if is_ref {
                    head.into()
                } else {
                    self.builder.build_load(head, "deref").into()
                }
            }
            Expr::NilCheck(x) => {
                let s = self.compile_exp(x, true);
                let flag = unsafe {
                    self.builder.build_in_bounds_gep(
                        s.into_pointer_value(),
                        &[
                            self.context.i32_type().const_int(0, false),
                            self.context.i32_type().const_int(2, false),
                        ],
                        "flag",
                    )
                };
                let v = self.builder.build_load(flag, "load_f");
                self.builder
                    .build_int_compare(
                        IntPredicate::EQ,
                        v.into_int_value(),
                        self.context.bool_type().const_int(1, false),
                        "check",
                    )
                    .as_basic_value_enum()
            }
            Expr::Tail(_, exp) => {
                let s = self.compile_exp(exp, true);
                let t = unsafe {
                    self.builder.build_in_bounds_gep(
                        s.into_pointer_value(),
                        &[
                            self.context.i32_type().const_int(0, false),
                            self.context.i32_type().const_int(1, false),
                        ],
                        "data",
                    )
                };
                let ptr = self.builder.build_load(t, "tail");
                if is_ref {
                    ptr.into()
                } else {
                    self.builder
                        .build_load(ptr.into_pointer_value(), "data")
                        .into()
                }
            }
            Expr::Head(_t, exp) => {
                let list = self.compile_exp(exp, true);
                assert!(list.is_pointer_value());
                let element = unsafe {
                    self.builder
                        .build_struct_gep(list.into_pointer_value().into(), 0, "head_ptr")
                };
                self.builder.build_load(element, "head")
            }
            Expr::CNil => self
                .context
                .opaque_struct_type("nil")
                .ptr_type(AddressSpace::Generic)
                .const_null()
                .into(),
            Expr::CBool(b) => self
                .context
                .bool_type()
                .const_int(if *b { 1 } else { 0 }, false)
                .into(),
            Expr::CInt(n) => self.context.i16_type().const_int(*n as u64, true).into(),
            Expr::CChar(c) => self.context.i8_type().const_int(*c as u64, false).into(),
            Expr::Atomic(t, at) => {
                let ptr = self.get_atom(at);
                if is_ref {
                    return ptr.into();
                }
                let result = self
                    .builder
                    .build_load(ptr, &format!("{:?}_ptr_{}", t, at.get_name()));
                result
            }
            Expr::NewArray(t, exp) => {
                let t = self.typedecl_to_type(&*t);
                let size = self.compile_exp(exp, false);
                assert!(size.is_int_value());
                let size = self.builder.build_int_cast(
                    size.into_int_value(),
                    self.context.i64_type(),
                    "upcast",
                );
                let t_size = self.typed_size_of(&t).unwrap();
                let total_size = self.builder.build_int_add(size, t_size, "array_size");
                let gc_malloc = self.global_funcs.get("GC_malloc").unwrap();
                let data = self
                    .builder
                    .build_call(*gc_malloc, &[total_size.into()], "GC_malloc");
                self.builder
                    .build_bitcast(data.try_as_basic_value().left().unwrap(), t, "cast_gc")
            }
            Expr::Binary(t, x, y) => {
                let x = self
                    .compile_exp(x.as_ref().unwrap(), false)
                    .into_int_value();
                let y = self
                    .compile_exp(y.as_ref().unwrap(), false)
                    .into_int_value();
                match t {
                    TokenKind::Addition => BasicValueEnum::IntValue(self.builder.build_int_add(
                        x,
                        y,
                        &format!("op_{:?}", t),
                    )),
                    TokenKind::Subtraction => BasicValueEnum::IntValue(self.builder.build_int_sub(
                        x,
                        y,
                        &format!("op_{:?}", t),
                    )),
                    TokenKind::Multiplication => BasicValueEnum::IntValue(
                        self.builder.build_int_mul(x, y, &format!("op_{:?}", t)),
                    ),
                    TokenKind::Division => BasicValueEnum::IntValue(
                        self.builder
                            .build_int_signed_div(x, y, &format!("op_{:?}", t)),
                    ),
                    TokenKind::KMod => BasicValueEnum::IntValue(self.builder.build_int_signed_rem(
                        x,
                        y,
                        &format!("op_{:?}", t),
                    )),
                    e => unreachable!("binary operation with token {:?}", e),
                }
            }
            Expr::Logical(t, x, y) => {
                let x = self
                    .compile_exp(x.as_ref().unwrap(), false)
                    .into_int_value();
                let y = self
                    .compile_exp(y.as_ref().unwrap(), false)
                    .into_int_value();
                if t == &TokenKind::KAnd {
                    BasicValueEnum::IntValue(self.builder.build_and(x, y, &format!("op_and")))
                } else if t == &TokenKind::KOr {
                    BasicValueEnum::IntValue(self.builder.build_or(x, y, &format!("op_or")))
                } else {
                    unreachable!("logical expression with token: {:?}", t)
                }
            }
            Expr::Negation(Some(t)) => {
                let r = self.compile_exp(t, false);
                self.builder.build_not(r.into_int_value(), "not").into()
            }
            Expr::Comparison(t, x, y) => {
                let predicate = match t {
                    TokenKind::Equal => IntPredicate::EQ,
                    TokenKind::NotEqual => IntPredicate::NE,
                    TokenKind::Great => IntPredicate::SGT,
                    TokenKind::GreatOrEqual => IntPredicate::SGE,
                    TokenKind::Less => IntPredicate::SLT,
                    TokenKind::LessOrEqual => IntPredicate::SLE,
                    e => panic!("Comparison with token: {:?}", e),
                };
                let x = self
                    .compile_exp(x.as_ref().unwrap(), false)
                    .into_int_value();
                let y = self
                    .compile_exp(y.as_ref().unwrap(), false)
                    .into_int_value();
                BasicValueEnum::IntValue(self.builder.build_int_compare(
                    predicate,
                    x,
                    y,
                    &format!("op_{:?}", t),
                ))
            }

            e => panic!("unhandle Expression: {:?}", e),
        }
    }
}
