use crate::ast::Expr;
use crate::error::Error;
use crate::parser::TokenKind;
use crate::symbol_table::SymbolTable;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::IntPredicate;

use std::collections::HashMap;
use std::path::Path;
use std::process::Command;
use std::result::Result;
use std::time::Instant;
use std::time::SystemTime;

use crate::ast::*;
use inkwell::attributes::AttributeLoc;
use inkwell::module::Linkage;
use inkwell::passes::*;
use inkwell::targets::*;
use inkwell::types::*;
use inkwell::values::*;
use inkwell::AddressSpace;
use inkwell::OptimizationLevel;

// this should be unique enough
fn random_sub_str() -> String {
    let now = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap();
    now.as_nanos().to_string()
}

pub struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub function_table: SymbolTable<FunctionValue<'ctx>>,
    pub ctx_mapping: HashMap<String, Vec<VarDef>>,
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
            e => panic!("called typdecl_str with: {}", e),
        }
    }

    fn typed_size_of(&self, ctype: &BasicTypeEnum<'ctx>) -> Option<IntValue<'ctx>> {
        match &ctype {
            BasicTypeEnum::ArrayType(v) => v.size_of(),
            BasicTypeEnum::IntType(v) => v.size_of(),
            BasicTypeEnum::FloatType(v) => v.size_of(),
            BasicTypeEnum::PointerType(v) => match &v.get_element_type() {
                AnyTypeEnum::ArrayType(v) => v.size_of(),
                AnyTypeEnum::IntType(v) => v.size_of(),
                AnyTypeEnum::FloatType(v) => v.size_of(),
                AnyTypeEnum::PointerType(v) => v.size_of(),
                AnyTypeEnum::StructType(v) => v.size_of(),
                AnyTypeEnum::VectorType(v) => v.size_of(),
                AnyTypeEnum::VoidType(e) => unreachable!("a pointer to what? : {:?}", e),
                AnyTypeEnum::FunctionType(e) => {
                    unreachable!("function pointer not used in this language: {:?}", e)
                }
            },
            BasicTypeEnum::StructType(v) => v.size_of(),
            BasicTypeEnum::VectorType(v) => v.size_of(),
        }
    }

    fn context_to_type(&self, list: &[VarDef]) -> BasicTypeEnum<'ctx> {
        let fields: Vec<_> = list
            .iter()
            .map(|x| {
                self.typedecl_to_type(&x.var_type)
                    .ptr_type(AddressSpace::Generic)
                    .as_basic_type_enum()
            })
            .collect();
        self.context.struct_type(&fields, true).into()
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
                match self.module.get_struct_type(&type_name) {
                    Some(k) => k.ptr_type(AddressSpace::Generic).into(),
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
                        node.ptr_type(AddressSpace::Generic).into()
                    }
                }
            }
            e => panic!("called typedecl_to_type with {}", e),
        }
    }

    fn store_function(
        &mut self,
        name: &str,
        ftype: FunctionType<'ctx>,
        context: &Context,
        ref_list: Vec<bool>,
    ) -> Result<FunctionValue<'ctx>, String> {
        if self.function_table.in_current_scope("name") {
            Err("function with the same name already exists".to_owned())
        } else if self.function_table.lookup(name).is_some() {
            let lname = format!("{}_{}", name, random_sub_str());
            let f = self.module.add_function(&lname, ftype, None);
            for i in 0..ref_list.len() {
                f.add_attribute(
                    AttributeLoc::Param(i as u32),
                    context.create_string_attribute(
                        "is_ref",
                        if ref_list[i] { "true" } else { "false" },
                    ),
                )
            }
            self.function_table.insert(name, f)?;
            Ok(f)
        } else {
            let f = self.module.add_function(name, ftype, None);
            for i in 0..ref_list.len() {
                f.add_attribute(
                    AttributeLoc::Param(i as u32),
                    context.create_string_attribute(
                        "is_ref",
                        if ref_list[i] { "true" } else { "false" },
                    ),
                )
            }
            self.function_table.insert(name, f)?;
            Ok(f)
        }
    }

    fn funcdecl_to_function_type(&mut self, func: &FuncDecl) -> FunctionType<'ctx> {
        let mut args = Vec::new();
        for i in &func.arguments {
            let t = self.typedecl_to_type(&i.def.var_type);
            if i.is_ref {
                args.push(t.ptr_type(AddressSpace::Generic).into());
            } else {
                args.push(t);
            }
        }
        // Handle optional context
        if !func.ctx.is_empty() {
            let r = self
                .context_to_type(&func.ctx)
                .into_struct_type()
                .ptr_type(AddressSpace::Generic)
                .into();
            args.insert(0, r);
        }

        if func.rtype == TypeDecl::Void {
            self.context.void_type().fn_type(&args, false)
        } else {
            self.typedecl_to_type(&func.rtype).fn_type(&args, false)
        }
    }

    fn func_to_function_type(&mut self, func: &FuncDef) -> FunctionType<'ctx> {
        self.funcdecl_to_function_type(&func.header)
    }
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, module: Module<'ctx>) -> Result<Self, Error> {
        let mut std = SymbolTable::new();
        std.open_scope("root");
        let gc_malloct = context
            .i8_type()
            .ptr_type(AddressSpace::Generic)
            .fn_type(&[context.i64_type().into()], false);
        let gc_malloc = module.add_function("GC_malloc", gc_malloct, Some(Linkage::External));
        std.insert("GC_malloc".to_owned(), gc_malloc)?;
        let strlent = context.i16_type().fn_type(
            &[context.i8_type().ptr_type(AddressSpace::Generic).into()],
            false,
        );
        let strlen = module.add_function("strlen", strlent, Some(Linkage::External));
        std.insert("strlen".to_owned(), strlen)?;

        let putit = context
            .void_type()
            .fn_type(&[context.i16_type().as_basic_type_enum()], false);
        let puti = module.add_function("puti", putit, Some(Linkage::External));
        std.insert("puti".to_owned(), puti)?;

        // Function: decl putb (bool b)
        let putbt = context
            .void_type()
            .fn_type(&[context.bool_type().into()], false);
        let putb = module.add_function("putb", putbt, Some(Linkage::External));
        std.insert("putb".to_owned(), putb)?;

        // Function: decl putc (char c)
        let putct = context
            .void_type()
            .fn_type(&[context.i8_type().into()], false);
        let putc = module.add_function("puti8", putct, Some(Linkage::External));
        std.insert("putc".to_owned(), putc)?;

        // Warning(dimkar): This Function has a different name in the c code "putstring", just to void colission with c++ function names
        // Function: decl puts (char[] s)
        let putst = context.void_type().fn_type(
            &[context.i8_type().ptr_type(AddressSpace::Generic).into()],
            false,
        );
        let puts = module.add_function("putstring", putst, Some(Linkage::External));
        std.insert("puts".to_owned(), puts)?;

        // Function: decl int geti ()
        let getit = context.i16_type().fn_type(&[], false);
        let geti = module.add_function("geti", getit, Some(Linkage::External));
        std.insert("geti".to_owned(), geti)?;

        // Function: decl bool getb ()
        let getbt = context.bool_type().fn_type(&[], false);
        let getb = module.add_function("getb", getbt, Some(Linkage::External));
        std.insert("getb".to_owned(), getb)?;

        // Function: decl gets (int n, char[] s)
        let getst = context.void_type().fn_type(
            &[
                context.i16_type().into(),
                context.i8_type().ptr_type(AddressSpace::Generic).into(),
            ],
            false,
        );
        let gets = module.add_function("gets", getst, Some(Linkage::External));
        std.insert("gets".to_owned(), gets)?;

        // Function: decl char getc ()
        let getct = context.i8_type().fn_type(&[], false);
        let getc = module.add_function("getcchar", getct, Some(Linkage::External));
        std.insert("getc".to_owned(), getc)?;

        Ok(CodeGen {
            context,
            module,
            builder: context.create_builder(),
            function_table: std,
            ctx_mapping: HashMap::new(),
        })
    }

    pub fn compile(
        &mut self,
        ast: &mut AstRoot,
        output_intermidiate: bool,
        output_final: bool,
        optimize: bool,
        filename: &Path,
    ) -> Result<(), Error> {
        // Build Ast
        let now = Instant::now();
        let main = ast.generate();
        if let Err(e) = main {
            return Err(e);
        }
        println!("Ast generation: {}", now.elapsed().as_secs_f64());

        let now = Instant::now();
        let mut main = main.unwrap();
        if !main.header.arguments.is_empty() || main.header.rtype != TypeDecl::Void {
            return Err(Error::with_message(
                0,
                0,
                "Top level Function shouldn't have argument or a return type",
                "Codegen",
            ));
        }

        // Create wrapper
        let f = self
            .module
            .add_function("main", self.context.i32_type().fn_type(&[], false), None);
        let entry_block = self.context.append_basic_block(f, "main_entry");

        //
        // self.function_table.insert("main", f).unwrap();
        if main.header.name == "main" {
            main.header.name = "main_".to_owned();
        };
        // Create Code
        self.compile_func(self.context, &main)?;

        self.builder.position_at_end(entry_block);
        // // Init GC
        // let gc_init = self.function_table.lookup("GC_INIT");
        // self.builder.build_call(*gc_init.unwrap(), &[], "init_gc");
        // Fill wrapper
        let callee = self.module.get_function(&main.header.name).unwrap();
        self.builder.build_call(callee, &[], "call_entry");
        self.builder
            .build_return(Some(&self.context.i32_type().const_int(0, false)));
        // Clean global function table
        if let Err(e) = self.module.verify() {
            use colored::*;
            let m: String = e.to_str().unwrap().replace("\n", "\n\t\t ");
            eprintln!("{}: {}", "verifier warning".to_owned().yellow(), m)
        }
        println!("Code generation: {:.5} secs", now.elapsed().as_secs_f64());
        let now = Instant::now();
        // Create FPM
        // Build Object file
        let interm_path = filename.with_extension("imm");
        let final_path = filename.with_extension("asm");
        if optimize {
            let pass_manager = PassManager::create(());

            pass_manager.add_argument_promotion_pass();
            pass_manager.add_constant_merge_pass();
            pass_manager.add_dead_arg_elimination_pass();
            pass_manager.add_function_attrs_pass();
            pass_manager.add_function_inlining_pass();
            pass_manager.add_always_inliner_pass();
            pass_manager.add_global_dce_pass();
            pass_manager.add_global_optimizer_pass();
            pass_manager.add_ip_constant_propagation_pass();
            pass_manager.add_prune_eh_pass();
            pass_manager.add_ipsccp_pass();
            pass_manager.add_internalize_pass(true);
            pass_manager.add_strip_dead_prototypes_pass();
            pass_manager.add_strip_symbol_pass();
            pass_manager.add_loop_vectorize_pass();
            pass_manager.add_slp_vectorize_pass();
            pass_manager.add_aggressive_dce_pass();
            pass_manager.add_alignment_from_assumptions_pass();
            pass_manager.add_cfg_simplification_pass();
            pass_manager.add_dead_store_elimination_pass();
            pass_manager.add_scalarizer_pass();
            pass_manager.add_merged_load_store_motion_pass();
            pass_manager.add_gvn_pass();
            pass_manager.add_ind_var_simplify_pass();
            pass_manager.add_instruction_combining_pass();
            pass_manager.add_jump_threading_pass();
            pass_manager.add_licm_pass();
            pass_manager.add_loop_deletion_pass();
            pass_manager.add_loop_idiom_pass();
            pass_manager.add_loop_rotate_pass();
            pass_manager.add_loop_reroll_pass();
            pass_manager.add_loop_unroll_pass();
            pass_manager.add_loop_unswitch_pass();
            pass_manager.add_memcpy_optimize_pass();
            pass_manager.add_partially_inline_lib_calls_pass();
            pass_manager.add_lower_switch_pass();
            pass_manager.add_promote_memory_to_register_pass();
            pass_manager.add_reassociate_pass();
            pass_manager.add_sccp_pass();
            pass_manager.add_scalar_repl_aggregates_pass();
            pass_manager.add_scalar_repl_aggregates_pass_ssa();
            pass_manager.add_scalar_repl_aggregates_pass_with_threshold(1);
            pass_manager.add_simplify_lib_calls_pass();
            pass_manager.add_tail_call_elimination_pass();
            pass_manager.add_constant_propagation_pass();
            pass_manager.add_demote_memory_to_register_pass();
            pass_manager.add_verifier_pass();
            pass_manager.add_correlated_value_propagation_pass();
            pass_manager.add_early_cse_pass();
            pass_manager.add_lower_expect_intrinsic_pass();
            pass_manager.add_type_based_alias_analysis_pass();
            pass_manager.add_scoped_no_alias_aa_pass();
            pass_manager.add_basic_alias_analysis_pass();
            let time = Instant::now();
            // Spend half a sec to optimize code
            while pass_manager.run_on(&self.module) {
                if time.elapsed().as_secs_f32() > 0.5 {
                    break;
                }
            }
        }

        Target::initialize_x86(&InitializationConfig::default());
        let opt = if optimize {
            OptimizationLevel::Aggressive
        } else {
            OptimizationLevel::Default
        };
        let target_machine = Target::from_name("x86-64")
            .unwrap()
            .create_target_machine(
                &TargetMachine::get_default_triple(),
                TargetMachine::get_host_cpu_name()
                    .to_str()
                    .expect("failed to get host cpu name"),
                TargetMachine::get_host_cpu_features()
                    .to_str()
                    .expect("failed to get host cpu features"),
                opt,
                RelocMode::Default,
                CodeModel::Default,
            )
            .unwrap();
        if output_intermidiate {
            let b = self.module.print_to_string();
            let s = b.to_str().unwrap();
            print!("{}", s);
        } else if output_final {
            let s = target_machine
                .write_to_memory_buffer(&self.module, FileType::Assembly)
                .expect("failed to create final code");
            let m = s.as_slice();
            use std::io::Write;
            let stdout = std::io::stdout();
            let mut handle = stdout.lock();

            handle.write_all(m).expect("failed to write to stdout");
        } else {
            match target_machine.write_to_file(&self.module, FileType::Assembly, &final_path) {
                Ok(()) => (),
                Err(e) => panic!("writing intemidiate file error: {}", e),
            };
            match target_machine.write_to_file(
                &self.module,
                FileType::Object,
                &final_path.with_extension("o"),
            ) {
                Ok(()) => (),
                Err(e) => panic!("writing intemidiate file error: {}", e),
            };
            match self.module.print_to_file(&interm_path) {
                Ok(()) => (),
                Err(e) => panic!("writing intemidiate file error: {}", e),
            };
        }
        println!("LLVM Compiling: {:.5} secs", now.elapsed().as_secs_f64());
        if output_final || output_intermidiate {
            return Ok(());
        }
        /*****************
            Extra steps
        *****************/

        let now = Instant::now();
        let r = if cfg!(windows) {
            let name = "clink.bat";
            Command::new(name)
                .args(&[
                    final_path.with_extension("o").to_str().unwrap(),
                    final_path.with_extension("exe").to_str().unwrap(),
                ])
                .output()
                .expect("failed to link")
        } else {
            let name = "sh";
            Command::new(name)
                .args(&[
                    "./clink.sh",
                    final_path.with_extension("o").to_str().unwrap(),
                    final_path.with_extension("").to_str().unwrap(),
                ])
                .output()
                .expect("failed to link")
        };
        if !r.status.success() {
            let message = String::from_utf8(r.stderr).unwrap();
            return Err(Error::with_message(0, 0, &message, "Codegen"));
        }
        println!("Linking: {:.5} secs", now.elapsed().as_secs_f64());
        Ok(())
    }

    // Sub compilations

    // Func
    fn compile_func(&mut self, context: &Context, func: &FuncDef) -> Result<(), String> {
        // Function Signature
        let ctype = self.func_to_function_type(&func);
        let function = if self.function_table.in_current_scope(&func.header.name) {
            *self.function_table.lookup(&func.header.name).unwrap()
        } else {
            let ref_list = func.header.arguments.iter().map(|x| x.is_ref).collect();
            self.store_function(&func.header.name, ctype, context, ref_list)?
        };
        self.function_table.open_scope(&func.header.name);
        let mut var_list: HashMap<String, PointerValue> = HashMap::new();

        let entry_block = self
            .context
            .append_basic_block(function, &format!("{}_entry", func.header.name));
        self.builder.position_at_end(entry_block);

        // The context will always be the first arguments
        let mut base = 0;
        if !func.header.ctx.is_empty() {
            base = 1;
            let t = function.get_first_param().unwrap();
            for i in 0..func.header.ctx.len() {
                let ii = self
                    .builder
                    .build_struct_gep(t.into_pointer_value(), i as u32, "tmp2")
                    .unwrap();
                let r = self.builder.build_load(ii, &format!("arg_{}", i));
                var_list.insert(func.header.ctx[i].name.clone(), r.into_pointer_value());
            }
        }
        // Function arguments
        for i in 0..func.header.arguments.len() {
            let t = function.get_nth_param((i + base) as u32);
            match t {
                Some(p) => {
                    if func.header.arguments[i].is_ref && p.is_pointer_value() {
                        var_list.insert(
                            func.header.arguments[i].def.name.clone(),
                            p.into_pointer_value(),
                        );
                    } else {
                        let ptr = self
                            .builder
                            .build_alloca(p.get_type(), &func.header.arguments[i].def.name);
                        self.builder.build_store(ptr, p);
                        var_list.insert(func.header.arguments[i].def.name.clone(), ptr);
                    }
                }
                None => {
                    panic!("Function should always have as many arguments as in the Func of ast")
                }
            }
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
            var_list.insert(i.name.clone(), p);
        }
        self.ctx_mapping
            .insert(func.header.name.clone(), func.header.ctx.clone());

        // Create llvm function for all defs and decls
        for i in &func.defs {
            let ref_list = i.header.arguments.iter().map(|x| x.is_ref).collect();
            let ftype = self.func_to_function_type(i);
            self.store_function(&i.header.name, ftype, context, ref_list)?;
        }

        for i in &func.decls {
            if !self.function_table.in_current_scope(&i.name) {
                let ref_list = i.arguments.iter().map(|x| x.is_ref).collect();
                let ftype = self.funcdecl_to_function_type(i);
                self.store_function(&i.name, ftype, context, ref_list)?;
            }
        }
        // Compile Function definitions
        for i in &func.defs {
            self.compile_func(context, i)?;
        }
        // Statements
        self.builder.position_at_end(entry_block);
        self.compile_stmts(func, entry_block, &var_list);

        self.function_table.close_scope();
        Ok(())
    }

    #[allow(clippy::cognitive_complexity)]
    fn compile_stmt(
        &mut self,
        stmt: &Stmt,
        func: &FuncDef,
        block: BasicBlock<'ctx>,
        exit_block: BasicBlock<'ctx>,
        exited: &mut bool,
        var_list: &HashMap<String, PointerValue<'ctx>>,
    ) -> bool {
        match stmt {
            Stmt::Exit => {
                self.builder.build_return(None);
                *exited = true;
            }
            Stmt::Return(exp) => {
                match &**exp {
                    Expr::Atomic(_, a) => {
                        // the result is a variable to load
                        let result = self.get_atom(&a, var_list);
                        let r = self.builder.build_load(result, "return");
                        self.builder.build_return(Some(&r));
                        return true;
                    }
                    _ => {
                        let result = self.compile_exp(exp, false, var_list);
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
                let condition = self.compile_exp(cond, false, var_list);
                assert!(condition.is_int_value());
                let mut then_block = self.context.insert_basic_block_after(
                    block,
                    &format!("{}_if", block.get_name().to_str().unwrap()),
                );
                let mut else_block = self.context.insert_basic_block_after(
                    then_block,
                    &format!("{}_else", block.get_name().to_str().unwrap()),
                );
                let if_exit_block = self.context.insert_basic_block_after(
                    then_block,
                    &format!("{}_ifexit", block.get_name().to_str().unwrap()),
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
                    if self.compile_stmt(i, func, then_block, exit_block, &mut exited, var_list) {
                        assert!(!contains_return);
                        contains_return = true;
                    }
                }
                if !contains_return && !exited {
                    self.builder.build_unconditional_branch(if_exit_block);
                }
                self.builder.position_at_end(else_block);
                for (i, elseif) in elseifs.iter().enumerate() {
                    then_block = self
                        .context
                        .insert_basic_block_after(else_block, &format!("else_ifthen{}", i));
                    else_block = self
                        .context
                        .insert_basic_block_after(else_block, &format!("else_ifelse{}", i));
                    let condition = self.compile_exp(&elseif.0, false, var_list);
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
                    for i in &elseif.1 {
                        if self.compile_stmt(
                            &i,
                            func,
                            then_block,
                            exit_block,
                            &mut exited,
                            var_list,
                        ) {
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
                    if self.compile_stmt(i, func, else_block, exit_block, &mut exited, var_list) {
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
                    self.compile_stmt(&i, func, block, exit_block, &mut exited, var_list);
                }
                let for_header = self.context.insert_basic_block_after(block, "for_header");
                let for_body = self
                    .context
                    .insert_basic_block_after(for_header, "for_body");
                let for_exit = self.context.insert_basic_block_after(for_body, "for_exit");
                self.builder.build_unconditional_branch(for_header);
                self.builder.position_at_end(for_header);
                let condition = self.compile_exp(cond, false, var_list);
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
                    if self.compile_stmt(i, func, for_body, for_exit, &mut exited, var_list) {
                        assert!(!returned);
                        returned = true;
                    }
                }
                for i in post_stmts {
                    if self.compile_stmt(i, func, for_body, for_exit, &mut exited, var_list) {
                        assert!(!returned);
                        returned = true;
                    }
                }
                if !returned && !exited {
                    self.builder.build_unconditional_branch(for_header);
                }
                self.builder.position_at_end(for_exit);
            }
            Stmt::Skip => (/* Skip does nothing so no code is generated*/),
            Stmt::Call(name, args) => {
                let function = *self.function_table.lookup(name).unwrap();
                let mut v = Vec::new();
                for (i, arg) in args.iter().enumerate() {
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
                    let r = self.compile_exp(&arg, is_ref, var_list);
                    v.push(r);
                }
                let mut my_ctx = func.defs.iter().find_map(|x| {
                    if &x.header.name == name {
                        Some(x.header.ctx.clone())
                    } else {
                        None
                    }
                });
                if my_ctx.is_none() {
                    my_ctx = func.decls.iter().find_map(|x| {
                        if &x.name == name {
                            Some(x.ctx.clone())
                        } else {
                            None
                        }
                    });
                }
                if my_ctx.is_some() {
                    let my_ctx = my_ctx.unwrap();
                    if !my_ctx.is_empty() {
                        // we have context
                        let ctype = self.context_to_type(&my_ctx);
                        let p = self.builder.build_alloca(ctype, "ctx");
                        for i in 0..my_ctx.len() {
                            let d = var_list.get(&my_ctx[i].name).unwrap();
                            let t = self.builder.build_struct_gep(
                                p,
                                i as u32,
                                &format!("tmp_{}_ptr", my_ctx[i].name),
                            );
                            self.builder.build_store(t.unwrap(), *d);
                        }
                        v.insert(0, p.into());
                    }
                }
                self.builder
                    .build_call(function, &v, &format!("call_{}", name));
            }
            Stmt::Assign(atom, exp) => {
                let ptr = self.get_atom(atom, &var_list);
                let value = self.compile_exp(exp, false, var_list);
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
    fn compile_stmts(
        &mut self,
        func: &FuncDef,
        block: BasicBlock<'ctx>,
        var_list: &HashMap<String, PointerValue<'ctx>>,
    ) {
        let mut exited = false;
        let mut returned = false;
        for i in 0..func.stmts.len() {
            let stmt = &func.stmts[i];
            // Note: not sure what happend when you put an exit inside a function bloxk
            //       probably ast souldn't allow it
            if self.compile_stmt(stmt, func, block, block, &mut exited, var_list) {
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
    pub fn get_atom(
        &mut self,
        atom: &Atomic,
        var_list: &HashMap<String, PointerValue<'ctx>>,
    ) -> PointerValue<'ctx> {
        match atom {
            Atomic::CString(s) => {
                let g = match self.module.get_global(s) {
                    Some(v) => v.as_basic_value_enum().into_pointer_value(),
                    None => self
                        .builder
                        .build_global_string_ptr(s, &format!("ptr_{}", s))
                        .as_basic_value_enum()
                        .into_pointer_value(),
                };
                let p = self.builder.build_alloca(g.get_type(), "tmp");
                self.builder.build_store(p, g);
                p
            }
            Atomic::Accessor(atom, exp) => {
                let ptr = self.get_atom(atom, var_list);
                let index = self.compile_exp(exp, false, var_list);
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
                let function = *self
                    .function_table
                    .lookup(&atom.get_name())
                    .expect("function call failed to find function");
                let mut v = Vec::new();
                for (i, arg) in args.iter().enumerate() {
                    let is_ref = match function
                        .get_string_attribute(AttributeLoc::Param(i as u32), "is_ref")
                    {
                        Some(v) => v.get_string_value().to_str().unwrap_or("false") == "true",
                        None => false,
                    };
                    let r = self.compile_exp(&arg, is_ref, var_list);
                    if r.is_pointer_value()
                        && r.into_pointer_value().is_null()
                        && r.into_pointer_value().is_const()
                    {
                        // handling nill
                        let arg = function.get_nth_param(i as u32).unwrap();
                        let r0 = self.create_typed_nil(&arg.get_type(), false);
                        v.push(r0);
                    } else {
                        v.push(r);
                    }
                }
                let my_ctx = self.ctx_mapping.get(name);
                if my_ctx.is_some() {
                    let my_ctx = my_ctx.unwrap();
                    if !my_ctx.is_empty() {
                        // we have context
                        let ctype = self.context_to_type(my_ctx);
                        let p = self.builder.build_alloca(ctype, "ctx");
                        for i in 0..my_ctx.len() {
                            let d = var_list.get(&my_ctx[i].name).unwrap();
                            let t = self.builder.build_struct_gep(
                                p,
                                i as u32,
                                &format!("tmp_{}_ptr", my_ctx[i].name),
                            );
                            self.builder.build_store(t.unwrap(), *d);
                        }
                        v.insert(0, p.into());
                    }
                }
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
            Atomic::Name(_, name) => match var_list.get(name) {
                Some(k) => *k,
                None => panic!("variable name not found: {}", name),
            },
        }
    }
    fn create_typed_nil(
        &mut self,
        ctype: &BasicTypeEnum<'ctx>,
        is_ref: bool,
    ) -> BasicValueEnum<'ctx> {
        let gc_malloc = self.module.get_function("GC_malloc").unwrap();
        let size = ctype
            .into_pointer_type()
            .get_element_type()
            .into_struct_type()
            .size_of()
            .unwrap()
            .into();
        let data = self.builder.build_call(gc_malloc, &[size], "gc_malloc_d");
        let new_tail = self
            .builder
            .build_bitcast(
                data.try_as_basic_value().left().unwrap(),
                ctype.into_pointer_type(),
                "cast_gc",
            )
            .into_pointer_value();

        let flag = self.builder.build_struct_gep(new_tail, 2, "flag").unwrap();
        self.builder
            .build_store(flag, self.context.bool_type().const_int(1, false));
        if is_ref {
            let p = self
                .builder
                .build_alloca(new_tail.get_type(), "ref_nil")
                .into();
            self.builder.build_store(p, new_tail);
            p.into()
        } else {
            new_tail.into()
        }
    }

    fn compile_exp(
        &mut self,
        exp: &Expr,
        is_ref: bool,
        var_list: &HashMap<String, PointerValue<'ctx>>,
    ) -> BasicValueEnum<'ctx> {
        match exp {
            Expr::Hash(t, left, right) => {
                let ctype = self.typedecl_to_type(t);
                let data = self.compile_exp(left, false, var_list);
                let tail_data = self.compile_exp(right, false, var_list);

                let tail = if tail_data.get_type().is_pointer_type()
                    && tail_data.into_pointer_value().is_null()
                    && tail_data.into_pointer_value().is_const()
                {
                    self.create_typed_nil(&ctype, true)
                } else {
                    // Copy data to new ptr
                    let gc_malloc = self.module.get_function("GC_malloc").unwrap();
                    let size = self.typed_size_of(&ctype).unwrap();
                    let data = self
                        .builder
                        .build_call(gc_malloc, &[size.into()], "gc_malloc_d");
                    let p = self
                        .builder
                        .build_bitcast(
                            data.try_as_basic_value().left().unwrap(),
                            ctype.into_pointer_type(),
                            "cast_gc",
                        )
                        .into_pointer_value();
                    let tail_data = self
                        .builder
                        .build_load(tail_data.into_pointer_value(), "ll");
                    self.builder.build_store(p, tail_data);
                    p.into()
                };
                let gc_malloc = self.module.get_function("GC_malloc").unwrap();
                let size = self.typed_size_of(&ctype).unwrap();
                let gc_data = self
                    .builder
                    .build_call(gc_malloc, &[size.into()], "gc_malloc_d");
                let head = self
                    .builder
                    .build_bitcast(
                        gc_data.try_as_basic_value().left().unwrap(),
                        ctype.into_pointer_type(),
                        "cast_gc",
                    )
                    .into_pointer_value();
                let data_ptr = self.builder.build_struct_gep(head, 0, "data").unwrap();
                self.builder.build_store(data_ptr, data);
                let flag = self
                    .builder
                    .build_struct_gep(head, 2, "flag")
                    .unwrap();
                self.builder
                    .build_store(flag, self.context.bool_type().const_int(0, false));

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
                    let size = self
                        .typed_size_of(&head.get_type().ptr_type(AddressSpace::Generic).into())
                        .unwrap();
                    let gc_data = self
                        .builder
                        .build_call(gc_malloc, &[size.into()], "gc_malloc_d");
                    let ptr = self.builder.build_bitcast(
                        gc_data.try_as_basic_value().left().unwrap(),
                        head.get_type().ptr_type(AddressSpace::Generic),
                        "cast_gc",
                    );
                    self.builder.build_store(ptr.into_pointer_value(), head);
                    ptr
                } else {
                    head.into()
                }
            }
            Expr::NilCheck(x) => {
                let s = self.compile_exp(x, true, var_list);
                let s = self.builder.build_load(s.into_pointer_value(), "tmtnilq");
                let flag = self
                    .builder
                    .build_struct_gep(s.into_pointer_value(), 2, "flag")
                    .unwrap();
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
                let s = self.compile_exp(exp, true, var_list);
                let s = self.builder.build_load(s.into_pointer_value(), "tmtnilq");
                let t = self
                    .builder
                    .build_struct_gep(s.into_pointer_value(), 1, "data")
                    .unwrap();
                let ptr = self.builder.build_load(t, "tail");
                if is_ref {
                    let gc_malloc = self.module.get_function("GC_malloc").unwrap();
                    let size = self
                        .typed_size_of(&ptr.get_type().ptr_type(AddressSpace::Generic).into())
                        .unwrap();
                    let gc_data = self
                        .builder
                        .build_call(gc_malloc, &[size.into()], "gc_malloc_d");
                    let n = self.builder.build_bitcast(
                        gc_data.try_as_basic_value().left().unwrap(),
                        ptr.get_type().ptr_type(AddressSpace::Generic),
                        "cast_gc",
                    );
                    self.builder.build_store(n.into_pointer_value(), ptr);
                    n
                } else {
                    ptr.into()
                }
            }
            Expr::Head(_t, exp) => {
                let list = self.compile_exp(exp, true, var_list);
                assert!(list.is_pointer_value());
                let list = self
                    .builder
                    .build_load(list.into_pointer_value(), "head_tmt");
                assert!(list.is_pointer_value());
                let element = self
                    .builder
                    .build_struct_gep(list.into_pointer_value(), 0, "head_ptr")
                    .unwrap();
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
                let ptr = self.get_atom(at, var_list);
                if is_ref {
                    return ptr.into();
                }
                self.builder
                    .build_load(ptr, &format!("{}_ptr_{}", t, at.get_name()))
            }
            Expr::NewArray(t, exp) => {
                let t = self.typedecl_to_type(&*t);
                let size = self.compile_exp(exp, false, var_list);
                assert!(size.is_int_value());
                let size = self.builder.build_int_cast(
                    size.into_int_value(),
                    self.context.i64_type(),
                    "upcast",
                );
                let t_size = self.typed_size_of(&t).unwrap();
                let total_size = self.builder.build_int_add(size, t_size, "array_size");
                let gc_malloc = self.module.get_function("GC_malloc").unwrap();
                let data =
                    self.builder
                        .build_call(gc_malloc, &[total_size.into()], "gc_malloc_new");
                self.builder
                    .build_bitcast(data.try_as_basic_value().left().unwrap(), t, "cast_gc")
            }
            Expr::Binary(t, x, y) => {
                let x = self
                    .compile_exp(x.as_ref().unwrap(), false, var_list)
                    .into_int_value();
                let y = self
                    .compile_exp(y.as_ref().unwrap(), false, var_list)
                    .into_int_value();
                match t {
                    TokenKind::Addition => BasicValueEnum::IntValue(self.builder.build_int_add(
                        x,
                        y,
                        &format!("op_{}", t),
                    )),
                    TokenKind::Subtraction => BasicValueEnum::IntValue(self.builder.build_int_sub(
                        x,
                        y,
                        &format!("op_{}", t),
                    )),
                    TokenKind::Multiplication => BasicValueEnum::IntValue(
                        self.builder.build_int_mul(x, y, &format!("op_{}", t)),
                    ),
                    TokenKind::Division => BasicValueEnum::IntValue(
                        self.builder
                            .build_int_signed_div(x, y, &format!("op_{}", t)),
                    ),
                    TokenKind::KMod => BasicValueEnum::IntValue(self.builder.build_int_signed_rem(
                        x,
                        y,
                        &format!("op_{}", t),
                    )),
                    e => unreachable!("binary operation with token {}", e),
                }
            }
            Expr::Logical(t, x, y) => {
                let x = self
                    .compile_exp(x.as_ref().unwrap(), false, var_list)
                    .into_int_value();
                let y = self
                    .compile_exp(y.as_ref().unwrap(), false, var_list)
                    .into_int_value();
                if t == &TokenKind::KAnd {
                    BasicValueEnum::IntValue(self.builder.build_and(x, y, "op_and"))
                } else if t == &TokenKind::KOr {
                    BasicValueEnum::IntValue(self.builder.build_or(x, y, "op_or"))
                } else {
                    unreachable!("logical expression with token: {}", t)
                }
            }
            Expr::Negation(Some(t)) => {
                let r = self.compile_exp(t, false, var_list);
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
                    e => panic!("Comparison with token: {:?}", (e, x, y)),
                };
                let x = self
                    .compile_exp(x.as_ref().unwrap(), false, var_list)
                    .into_int_value();
                let y = self
                    .compile_exp(y.as_ref().unwrap(), false, var_list)
                    .into_int_value();
                BasicValueEnum::IntValue(self.builder.build_int_compare(
                    predicate,
                    x,
                    y,
                    &format!("op_{}", t),
                ))
            }

            e => panic!("unhandle Expression: {}", e),
        }
    }
}
