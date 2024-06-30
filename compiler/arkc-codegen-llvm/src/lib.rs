// mod top_level;
mod util;

use arkc_frontend::compilation::Compilation;
use arkc_hir::hir::{self, HirId};
use arkc_hir::ty::{self, PrimitiveType, Type};
use inkwell::types::{BasicTypeEnum, StructType};
use parser::ast;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::Arc;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::targets::{CodeModel, FileType, RelocMode, TargetTriple};
use inkwell::targets::{InitializationConfig, Target, TargetMachine};
use inkwell::types::BasicType;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, InstructionOpcode};
use inkwell::{AddressSpace, OptimizationLevel};

use crate::hir::File;

pub fn codegen(hir: &hir::File) -> Result<(), ()> {
    let context = Context::create();
    let builder = context.create_builder();

    let mut codegen = CodegenContext::new(&context, &hir);

    let target_triple = TargetMachine::get_default_triple();
    codegen.module.set_triple(&target_triple);

    if codegen.codegen_hir(&hir, &builder).is_err() {
        panic!("LLVM codegen failed");
    }

    match codegen.module.verify() {
        Ok(_) => (),
        Err(e) => {
            codegen.module.print_to_stderr();

            println!("Error: Bug in the generated IR:\n\n{}", e.to_string());

            return Err(());
        }
    }

    // if !config.no_optimize {
    //     codegen_ctx.optimize();
    // }

    //if config.show_ir {
    codegen.module.print_to_stderr();
    //}

    if !codegen.module.write_bitcode_to_path(Path::new("module.bc")) {
        panic!("Cannot write ir");
    }

    codegen.optimize('0');

    let module_name = String::from("test");
    let binary_name = util::binary_name(&module_name);
    codegen.output(module_name, &binary_name, &target_triple, &codegen.module);

    Ok(())
}

/// The (code) CodegenContext provides all the needed context for generating LLVM IR
/// while walking the HIR.
pub struct CodegenContext<'a> {
    pub context: &'a Context,
    pub hir: &'a hir::File,
    pub module: Module<'a>,
    //pub scopes: Scopes<HirId, BasicValueEnum<'a>>,
    pub cur_func: Option<FunctionValue<'a>>,
    pub struct_types: HashMap<HirId, StructType<'a>>,
}

impl<'a> CodegenContext<'a> {
    pub fn new(context: &'a Context, hir: &'a hir::File) -> Self {
        let module = context.create_module("test");

        Self {
            context,
            module,
            hir,
            // scopes: Scopes::new(),
            struct_types: HashMap::new(),
            cur_func: None,
        }
    }

    /// Output the current module to a file and link with gcc.
    fn output(
        &self,
        module_name: String,
        binary_filename: &str,
        target_triple: &TargetTriple,
        module: &Module,
    ) {
        // generate the bitcode to a .bc file
        let path = Path::new(&module_name).with_extension("o");
        let target = Target::from_triple(target_triple).unwrap();
        let target_machine = target
            .create_target_machine(
                target_triple,
                "",
                "",
                OptimizationLevel::None,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .unwrap();

        target_machine
            .write_to_file(module, FileType::Object, &path)
            .unwrap();

        #[cfg(debug_assertions)]
        let rt_lib_path = "target/debug/";

        #[cfg(not(debug_assertions))]
        let rt_lib_path = "target/release/";

        // call gcc to compile the bitcode to a binary
        util::link(
            path.to_string_lossy().as_ref(),
            binary_filename,
            rt_lib_path,
        );
    }

    /// Optimize the current inkwell::Module.
    /// optimization_argument is one of '0', '1', '2', '3', 's', or 'z'
    fn optimize(&self, optimization_argument: char) {
        let config = InitializationConfig::default();
        Target::initialize_native(&config).unwrap();
        let pass_manager_builder = PassManagerBuilder::create();

        let optimization_level = to_optimization_level(optimization_argument);
        let size_level = to_size_level(optimization_argument);
        pass_manager_builder.set_optimization_level(optimization_level);
        pass_manager_builder.set_size_level(size_level);

        let pass_manager = PassManager::create(());
        pass_manager_builder.populate_module_pass_manager(&pass_manager);
        pass_manager.run_on(&self.module);

        // TODO: It seems this method was removed; re-evaluate if inlining is still done
        // Do LTO optimizations afterward mosty for function inlining
        // let link_time_optimizations = PassManager::create(());
        // pass_manager_builder.populate_lto_pass_manager(&link_time_optimizations, false, true);
        // link_time_optimizations.run_on(&self.module);
    }

    pub fn codegen_hir(&mut self, root: &'a hir::File, builder: &'a Builder) -> Result<(), ()> {
        for item in &root.elements {
            match &item.kind {
                hir::ElemKind::Extern(p) => todo!(),
                hir::ElemKind::Signature(_p) => todo!(),
                hir::ElemKind::Function(f) => self.codegen_fn_declaration(f, builder)?,
                hir::ElemKind::Struct(s) => {}
            }
        }

        for body in root.bodies.values() {
            self.codegen_fn_body(body, builder)?;
        }

        Ok(())
    }

    pub fn codegen_fn_body(
        &mut self,
        fn_body: &'a hir::FnBody,
        builder: &'a Builder,
    ) -> Result<(), ()> {
        let func = self.hir.get_fn(&fn_body.fn_id).unwrap();

        if let Some(func) = self.module.get_function(&func.get_name()) {
            self.cur_func = Some(func);

            // let f_decl = self.hir.get_fn(&fn_body.fn_id).unwrap();

            /*for (i, arg) in f_decl.arguments.iter().enumerate() {
                let param = f.get_nth_param(i.try_into().unwrap()).unwrap();

                param.set_name(&arg.name.name);

                self.scopes.add(arg.name.hir_id.clone(), param);
            }*/

            let (last, _) = self.codegen_body(&fn_body.body, "entry", builder)?;

            builder.build_return(Some(&last));
        } else {
            panic!("Cannot find function {:?}", func.get_name());
        }

        Ok(())
    }

    pub fn codegen_body(
        &mut self,
        body: &'a hir::Block,
        name: &str,
        builder: &'a Builder,
    ) -> Result<(BasicValueEnum<'a>, BasicBlock<'a>), ()> {
        let basic_block = self
            .context
            .append_basic_block(self.cur_func.unwrap(), name);

        builder.position_at_end(basic_block);

        for stmt in body.stmts.iter() {
            self.codegen_stmt(stmt, builder)?;
        }

        let return_expr = self.codegen_expression(body.expr.as_ref().unwrap(), builder)?;

        //let first_return_idx = body
        //    .stmts
        //    .iter()
        //    .position(|s| s.is_return())
        //    .unwrap_or(body.stmts.len());

        //let stmts = body.stmts.iter().take(first_return_idx + 1);

        //let stmt = stmts
        //    .map(|stmt| self.codegen_stmt(stmt, builder))
        //    .last()
        //    .unwrap()?;

        Ok((return_expr, basic_block))
    }

    pub fn codegen_struct_field(
        &mut self,
        field: &'a hir::StructField,
        builder: &'a Builder,
    ) -> Result<BasicTypeEnum<'a>, ()> {
        println!("{:#?}", self.hir);
        let field_type = self.hir.node_types.get(&field.hir_id).unwrap();
        self.codegen_type(field_type, builder)
    }

    pub fn codegen_struct(
        &mut self,
        stru: &'a hir::Struct,
        builder: &'a Builder,
    ) -> Result<StructType<'a>, ()> {
        let mut field_types = vec![];

        for field in stru.fields.iter() {
            let v = self.codegen_struct_field(field, builder)?;
            field_types.push(v);
        }

        let f32_type = self.context.f32_type();
        let struct_type = self.context.opaque_struct_type("struct");
        struct_type.set_body(&field_types, false);

        self.struct_types.insert(stru.hir_id, struct_type);

        Ok(struct_type)
    }

    pub fn codegen_stmt(
        &mut self,
        stmt: &'a hir::Statement,
        builder: &'a Builder,
    ) -> Result<BasicValueEnum<'a>, ()> {
        println!("Processing stms: {:?}", stmt);

        Ok(match &*stmt.kind {
            hir::StatementKind::Let(l) => self.codegen_let(l, builder)?,
            hir::StatementKind::Expr(e) => self.codegen_expression(e, builder)?,
        })
    }

    pub fn codegen_expression(
        &mut self,
        expr: &'a hir::Expr,
        builder: &'a Builder,
    ) -> Result<BasicValueEnum<'a>, ()> {
        Ok(match *expr.kind {
            hir::ExprKind::Struct(ref expr_stru) => {
                let ty = self
                    .hir
                    .node_types
                    .get(&expr.hir_id)
                    .expect("typechecking to provide");

                match ty {
                    Type::Struct(struct_id) => {
                        let llvm_struct_ty = self.struct_types.get(struct_id).unwrap().clone();
                        let mut values = vec![];

                        // TODO: make sure ordering is correct
                        for field in expr_stru.fields.iter() {
                            let value = self.codegen_expression(&field.value, builder)?;
                            values.push(value)
                        }

                        let val = llvm_struct_ty.const_named_struct(&values);
                        let struct_ptr =
                            builder.build_alloca(llvm_struct_ty, "struct_ptr").unwrap();

                        for (i, def) in expr_stru.fields.iter().enumerate() {
                            let inner_ptr = builder
                                .build_struct_gep(
                                    llvm_struct_ty,
                                    struct_ptr,
                                    i as u32,
                                    "struct_inner",
                                )
                                .unwrap();

                            builder.build_store(inner_ptr, values[i]).unwrap();
                        }

                        let val = struct_ptr.as_basic_value_enum();
                        val
                    }
                    _ => unreachable!(),
                }
            }
            hir::ExprKind::Lit(ref lit) => self.codegen_literal(lit, builder)?,
            hir::ExprKind::Return(ref expr) => self.codegen_return(expr, builder)?,
            hir::ExprKind::Block(_) => todo!(),
            hir::ExprKind::Dot(_) => todo!(),
            hir::ExprKind::Call(_) => todo!(),
            hir::ExprKind::Ident(ref ident) => {
                println!("{:?}", ident);
                self.context.bool_type().const_zero().into()
            }
        })
    }

    fn codegen_return(
        &mut self,
        expr: &'a Option<hir::Expr>,
        builder: &'a Builder,
    ) -> Result<BasicValueEnum<'a>, ()> {
        Ok(if let Some(expr) = expr {
            let val = self.codegen_expression(expr, builder)?;

            // This is disabled because this is handled in lower_body
            //builder.build_return(Some(&val));

            val
        } else {
            // Hardcoded unit value
            //builder.build_return(None);

            let i1 = self.context.bool_type();
            i1.const_zero().into()
        })
    }

    pub fn codegen_fn_declaration(
        &mut self,
        func: &hir::FnDeclaration,
        builder: &'a Builder,
    ) -> Result<(), ()> {
        let name = func.get_name();

        if self.module.get_function(&name).is_some() {
            panic!()
        }

        let func_ty = self.hir.node_types.get(&func.hir_id).unwrap();

        if let Type::Function(func_ty) = func_ty {
            let return_ty = func_ty.output.clone().unwrap();

            let fn_type = if let Type::Primitive(PrimitiveType::Unit) = *return_ty {
                self.context.bool_type().fn_type(&vec![], false)
            } else {
                self.codegen_type(&return_ty, builder)?
                    .fn_type(&vec![], false)
            };

            let fn_value = self.module.add_function(&name, fn_type, None);
        }

        Ok(())
    }

    fn codegen_type(
        &mut self,
        ty: &ty::Type,
        builder: &'a Builder,
    ) -> Result<BasicTypeEnum<'a>, ()> {
        let ty: BasicTypeEnum<'a> = match ty {
            Type::Primitive(PrimitiveType::Int32) => self.context.i32_type().into(),
            Type::Primitive(PrimitiveType::Int64) => self.context.i64_type().into(),
            Type::Primitive(PrimitiveType::Float64) => self.context.f64_type().into(),
            Type::Primitive(PrimitiveType::Bool) => self.context.bool_type().into(),
            Type::Primitive(PrimitiveType::Unit) => self.context.bool_type().into(),
            Type::Primitive(PrimitiveType::Char) => self.context.i8_type().into(),
            Type::Function(_) => todo!(),
            Type::Struct(struct_id) => {
                let ty = if let Some(ty) = self.struct_types.get(struct_id) {
                    *ty
                } else {
                    let stru = self.hir.get_struct(struct_id).unwrap();
                    self.codegen_struct(stru, builder)?
                };

                ty.ptr_type(AddressSpace::default()).into()
            }
            _ => unimplemented!("Codegen: Cannot codegen type {:#?}", ty),
        };

        Ok(ty)
    }

    /*fn codegen_pattern() {
        match pattern {
            hir::PatternKind::Ident(ref ident) => {
                let var_id = *self.analysis.map_vars.get(ident.id).unwrap();
                let var = self.analysis.vars.get_var(var_id);

                if !var.ty.is_unit() {
                    match var.location {
                        //VarLocation::Context(..) => {
                            // Nothing to do here.
                        //}
                        VarLocation::Stack => {
                            let var_reg = self.alloc_var(bty);
                            self.var_registers.insert(var_id, var_reg);
                        }
                    }
                }
            }
        }
    }*/

    fn codegen_let(
        &mut self,
        l: &'a hir::LetBinding,
        builder: &'a Builder,
    ) -> Result<BasicValueEnum<'a>, ()> {
        if let Some(ref expr) = l.expr {
            let value = self.codegen_expression(expr, builder)?;

            // Allocate space for the variable
            self.hir
                .node_types
                .get(&expr.hir_id)
                .and_then(|t| {
                    (t.is_primitive()/*&& !t.is_array() && !t.is_string()*/).then(|| {
                        let ptr = builder
                            .build_alloca(value.get_type(), format!("_{}", l.hir_id.0).as_str())
                            .unwrap();

                        // TODO: self.scopes.add(id.get_hir_id(), ptr.as_basic_value_enum());

                        ptr
                    })
                })
                .map(|ptr| {
                    builder.build_store(ptr, value);
                    ptr.as_basic_value_enum()
                })
                .or(Some(value))
                .unwrap();

            //let _ = builder.build_store(var_alloca, value).unwrap();
        }

        Ok(self.context.bool_type().const_zero().into())
    }

    fn codegen_literal(
        &self,
        lit: &hir::Literal,
        builder: &Builder,
    ) -> Result<BasicValueEnum<'a>, ()> {
        Ok(match lit.kind {
            hir::LiteralKind::Int(_) => {
                let value = *self.hir.int_literals.get(lit.hir_id).unwrap();
                let i64_type = self.context.i64_type();
                i64_type.const_int(value.try_into().unwrap(), false).into()
            }
            hir::LiteralKind::Bool(b) => {
                let bool_type = self.context.bool_type();
                bool_type.const_int(b.try_into().unwrap(), false).into()
            }
            hir::LiteralKind::Char(c) => {
                let char_type = self.context.i8_type();
                char_type.const_int(c as u64, false).into()
            }
            hir::LiteralKind::Float(_) => todo!(),
            hir::LiteralKind::String(_) => todo!(),
            hir::LiteralKind::Unit => todo!(),
        })
    }
}

/// The (code) Generator provides all the needed context for generating LLVM IR
/// while walking the Ast.
#[derive(Debug)]
pub struct Generator<'context> {
    context: &'context Context,
    module: Module<'context>,
    builder: Builder<'context>,
}

/// Codegen the given Ast, producing a binary file at the given path.
/*pub fn run(module_name: String, entry: FnDefinitionId, compilation: &Compilation) {
    let context = Context::create();
    let module = context.create_module(&module_name);

    let target_triple = TargetMachine::get_default_triple();
    module.set_triple(&target_triple);

    let mut codegen = Generator {
        context: &context,
        module,
        builder: context.create_builder(),
        //definitions: HashMap::new(),
        //auto_derefs: HashSet::new(),
        //current_function_info: None,
        //current_definition_name: None,
    };

    //let entry_fn = compilation.func(entry);

    // Codegen main, and all functions reachable from it
    //codegen.codegen_main(entry_fn);

    codegen.optimize('0');

    // --show-ir: Dump the LLVM-IR of the generated module to stderr.
    // Useful to debug codegen
    //if args.emit == Some(EmitTarget::Ir) {
    codegen.module.print_to_file("./test.ir").unwrap();
    //print!("{}", llvm_ir.to_str().unwrap());
    //}

    let binary_name = util::binary_name(&module_name);
    codegen.output(module_name, &binary_name, &target_triple, &codegen.module)
}*/

fn path_to_module_name(path: &Path) -> String {
    path.with_extension("").to_string_lossy().into()
}

fn to_optimization_level(opt_level: char) -> OptimizationLevel {
    match opt_level {
        '1' => OptimizationLevel::Less,
        '2' => OptimizationLevel::Default,
        '3' => OptimizationLevel::Aggressive,
        _ => OptimizationLevel::None,
    }
}

fn to_size_level(optimization_argument: char) -> u32 {
    match optimization_argument {
        's' => 1,
        'z' => 2,
        _ => 0,
    }
}

impl<'g> Generator<'g> {
    fn codegen_main(&mut self, hir: &hir::FnDeclaration) {
        let void_type = self.context.void_type();
        let i32_type = self.context.i32_type();

        let fn_type = void_type.fn_type(&[], false);
        let function = self.module.add_function("main", fn_type, None);

        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        let value = {
            let rt_print_type = void_type.fn_type(&[], false);
            let rt_print = self.module.add_function(
                "native_print_invoke",
                rt_print_type,
                Some(Linkage::External),
            );

            self.builder
                .build_direct_call(rt_print, &[], "call")
                .expect("Could not build call")
        };

        let success = i32_type.const_int(0, true);
        self.build_return(success.into());

        /*let i32_type = self.context.i32_type();
        let main_type = i32_type.fn_type(&[], false);
        let function = self
            .module
            .add_function("main", main_type, Some(Linkage::External));
        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        ast.codegen(self);

        let success = i32_type.const_int(0, true);
        self.build_return(success.into());
        */
    }

    /// Optimize the current inkwell::Module.
    /// optimization_argument is one of '0', '1', '2', '3', 's', or 'z'
    fn optimize(&self, optimization_argument: char) {
        let config = InitializationConfig::default();
        Target::initialize_native(&config).unwrap();
        let pass_manager_builder = PassManagerBuilder::create();

        let optimization_level = to_optimization_level(optimization_argument);
        let size_level = to_size_level(optimization_argument);
        pass_manager_builder.set_optimization_level(optimization_level);
        pass_manager_builder.set_size_level(size_level);

        let pass_manager = PassManager::create(());
        pass_manager_builder.populate_module_pass_manager(&pass_manager);
        pass_manager.run_on(&self.module);

        // TODO: It seems this method was removed; re-evaluate if inlining is still done
        // Do LTO optimizations afterward mosty for function inlining
        // let link_time_optimizations = PassManager::create(());
        // pass_manager_builder.populate_lto_pass_manager(&link_time_optimizations, false, true);
        // link_time_optimizations.run_on(&self.module);
    }

    /// Output the current module to a file and link with gcc.
    fn output(
        &self,
        module_name: String,
        binary_filename: &str,
        target_triple: &TargetTriple,
        module: &Module,
    ) {
        // generate the bitcode to a .bc file
        let path = Path::new(&module_name).with_extension("o");
        let target = Target::from_triple(target_triple).unwrap();
        let target_machine = target
            .create_target_machine(
                target_triple,
                "",
                "",
                OptimizationLevel::None,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .unwrap();

        target_machine
            .write_to_file(module, FileType::Object, &path)
            .unwrap();

        #[cfg(debug_assertions)]
        let rt_lib_path = "target/debug/";

        #[cfg(not(debug_assertions))]
        let rt_lib_path = "target/release/";

        // call gcc to compile the bitcode to a binary
        util::link(
            path.to_string_lossy().as_ref(),
            binary_filename,
            rt_lib_path,
        );
    }

    /// Return the llvm block we're currently inserting into
    fn current_block(&self) -> BasicBlock<'g> {
        self.builder.get_insert_block().unwrap()
    }

    /// Does the given llvm instruction terminate its BasicBlock?
    /// This currently only checks for cases that can actually occur
    /// while codegening an arbitrary Ast node.
    fn current_instruction_is_block_terminator(&self) -> bool {
        let instruction = self.current_block().get_last_instruction();
        matches!(
            instruction.map(|instruction| instruction.get_opcode()),
            Some(InstructionOpcode::Return | InstructionOpcode::Unreachable)
        )
    }

    fn build_return(&mut self, return_value: BasicValueEnum<'g>) {
        if !self.current_instruction_is_block_terminator() {
            self.builder
                .build_return(Some(&return_value))
                .expect("Could not create return");
        }
    }
}

/*
trait CodeGen<'g> {
    fn codegen(&self, generator: &mut Generator<'g>) -> BasicValueEnum<'g>;
}

impl<'g> CodeGen<'g> for ast::Function {
    fn codegen(&self, generator: &mut Generator<'g>) -> BasicValueEnum<'g> {
        match self.block {
            Some(body) => {
                let caller_block = generator.current_block();
                let name = generator
                    .current_definition_name
                    .take()
                    .unwrap_or_else(|| "lambda".into());

                let (function, function_value) = generator.function(&name, &self.return_type);

                // Bind each parameter node to the nth parameter of `function`
                for (i, parameter) in self.params.iter().enumerate() {
                    let value = expect_opt!(
                        function.get_nth_param(i as u32),
                        "Could not get parameter {} of function {}",
                        i,
                        self
                    );

                    generator.definitions.insert(parameter.id, value);
                }

                let return_value = body.codegen(generator);

                generator.build_return(return_value);
                generator.builder.position_at_end(caller_block);

                function_value
            }
            None => todo!(),
        }
    }
}

impl<'g> CodeGen<'g> for ast::ExprReturnType {
    fn codegen(&self, generator: &mut Generator<'g>) -> BasicValueEnum<'g> {
        match self.expr.as_ref() {
            Some(ret_val) => {
                let value = ret_val.codegen(generator);
                generator
                    .builder
                    .build_return(Some(&value))
                    .expect("Could not create an llvm return");
                value
            }
            None => unreachable!(),
        }
    }
}

impl<'g> CodeGen<'g> for ast::ExprData {
    fn codegen(&self, generator: &mut Generator<'g>) -> BasicValueEnum<'g> {
        match self {
            ast::ExprData::Un(_) => todo!(),
            ast::ExprData::Bin(_, _, _) => todo!(),
            ast::ExprData::Lit(_) => todo!(),
            ast::ExprData::Ident(_) => todo!(),
            ast::ExprData::Call(_) => todo!(),
            ast::ExprData::Path(_) => todo!(),
            ast::ExprData::Dot(_) => todo!(),
            ast::ExprData::Block(_) => todo!(),
            ast::ExprData::Paren(_) => todo!(),
            ast::ExprData::Return(expr) => expr.codegen(generator),
            ast::ExprData::Error { id, span } => todo!(),
        }
    }
}

/*impl<'g> CodeGen<'g> for hir::Lambda {
    fn codegen(&self, generator: &mut Generator<'g>) -> BasicValueEnum<'g> {
        let caller_block = generator.current_block();
        let name = generator.current_definition_name.take().unwrap_or_else(|| "lambda".into());

        let (function, function_value) = generator.function(&name, &self.typ);

        // Bind each parameter node to the nth parameter of `function`
        for (i, parameter) in self.args.iter().enumerate() {
            let value =
                expect_opt!(function.get_nth_param(i as u32), "Could not get parameter {} of function {}", i, self);
            generator.definitions.insert(parameter.definition_id, value);
        }

        let return_value = self.body.codegen(generator);

        generator.build_return(return_value);
        generator.builder.position_at_end(caller_block);

        function_value
    }
}*/
*/
