mod top_level;

use parser::ast;
use std::path::Path;
use std::sync::Arc;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::targets::{CodeModel, FileType, RelocMode, TargetTriple};
use inkwell::targets::{InitializationConfig, Target, TargetMachine};
use inkwell::values::{BasicValueEnum, InstructionOpcode};
use inkwell::OptimizationLevel;

/// The (code) Generator provides all the needed context for generating LLVM IR
/// while walking the Ast.
#[derive(Debug)]
pub struct Generator<'context> {
    context: &'context Context,
    module: Module<'context>,
    builder: Builder<'context>,
}

/// Codegen the given Ast, producing a binary file at the given path.
pub fn run(module_name: String, ast: Arc<ast::File>) {
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

    // Codegen main, and all functions reachable from it
    codegen.codegen_main(&ast);

    codegen.optimize('0');

    // --show-ir: Dump the LLVM-IR of the generated module to stderr.
    // Useful to debug codegen
    //if args.emit == Some(EmitTarget::Ir) {
    let llvm_ir = codegen.module.print_to_string();
    print!("{}", llvm_ir);
    return;
    //}
}

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
    fn codegen_main(&mut self, ast: &ast::File) {
        /*for element in ast.elements.iter() {
            match element.as_ref() {
                ast::ElemData::Function(f) => f.codegen(self),
                _ => todo!(),
            };
        }*/

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