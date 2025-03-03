mod scopes;
mod util;

use arkc_hir::{
    hir::{self, BinOp, Block, ExprKind, ExprStruct, HirId},
    ty::{self, PrimitiveType, Type},
};
use scopes::Scopes;
use std::collections::HashMap;
use wasm_encoder::{
    BlockType, CodeSection, ComponentValType, ConstExpr, DataSection, ExportKind, ExportSection,
    Function, FunctionSection, GlobalSection, GlobalType, IndirectNameMap, Instruction,
    MemorySection, MemoryType, Module, NameMap, NameSection, PrimitiveValType, TypeSection,
    ValType,
};
use wasmparser::{Parser, Payload};

pub fn codegen(hir: &hir::File) -> Result<(), ()> {
    let mut codegen = CodegenContext::new(&hir);

    // Encode the name section.
    codegen.name_section.module("module");

    codegen.memory_section.memory(MemoryType {
        minimum: 1,
        maximum: None,
        memory64: false,
        shared: false,
        page_size_log2: None,
    });

    codegen.global_section.global(
        GlobalType {
            val_type: ValType::I32,
            mutable: false,
            shared: false,
        },
        &ConstExpr::i32_const(0),
    );
    codegen.global_section.global(
        GlobalType {
            val_type: ValType::I32,
            mutable: false,
            shared: false,
        },
        &ConstExpr::i32_const(1024),
    );

    //let offset = ConstExpr::i32_const(0);
    //let segment_data = b"hello";
    //data.active(0, &offset, segment_data.iter().copied());

    /*let mut offset = 0x0;

    for s in compilation.literals.iter() {
        let str_value = compilation.interner.lookup(*s);
        data_section.active(
            0,
            &ConstExpr::i32_const(offset as i32),
            str_value.as_bytes().iter().copied(),
        );
        //TODO: and here truncation
        let s_len = str_value.len() as u32;
        generation_context
            .literal_offsets
            .insert(*s, (offset, s_len));
        offset += s_len;
    }*/

    // Encode the export section.
    codegen.export_section.export("main", ExportKind::Func, 0);
    codegen
        .export_section
        .export("memory", ExportKind::Memory, 0);
    codegen
        .export_section
        .export("__data_end", ExportKind::Global, 0);
    codegen
        .export_section
        .export("__heap_base", ExportKind::Global, 1);

    codegen.codegen_hir(hir)?;

    let mut module = Module::new();
    module.section(&codegen.name_section);
    module.section(&codegen.type_section);
    // module.section(&import);
    module.section(&codegen.function_section);
    // module.section(&table);
    module.section(&codegen.memory_section);
    module.section(&codegen.global_section);
    module.section(&codegen.export_section);
    // module.section(&start);
    // module.section(&elements);
    module.section(&codegen.code_section);
    module.section(&codegen.data_section);
    // module.section(&data_count);

    let module_name = String::from("test");
    let binary_name = util::binary_name(&module_name);

    // Extract the encoded Wasm bytes for this module.
    let wasm_bytes = module.finish();

    // Write the WebAssembly binary to a file
    if let Err(e) = std::fs::write(&binary_name, &wasm_bytes) {
        eprintln!("Failed to write Wasm binary: {}", e);
    } else {
        println!("WebAssembly binary has been written to {}", binary_name);
    }

    Ok(())
}

pub struct Builder;

/// The (code) CodegenContext provides all the needed context for generating LLVM IR
/// while walking the HIR.
pub struct CodegenContext<'a> {
    pub hir: &'a hir::File,
    pub fn_types: HashMap<HirId, u32>,
    pub vars: Scopes<hir::VarId, u32>,
    pub cur_func_data: Option<hir::AnalysisData>,
    pub cur_func_locals: Option<Vec<ValType>>,
    pub cur_func_instructions: Option<Vec<Instruction<'a>>>,
    pub cur_func_local_names: Option<NameMap>,
    // wasm sections
    pub data_section: DataSection,
    pub name_section: NameSection,
    pub export_section: ExportSection,
    pub memory_section: MemorySection,
    pub function_section: FunctionSection,
    pub code_section: CodeSection,
    pub global_section: GlobalSection,
    pub type_section: TypeSection,
    // names
    pub function_names: NameMap,
    pub local_names: IndirectNameMap,
    pub type_names: NameMap,
    pub global_names: NameMap,
}

impl<'a> CodegenContext<'a> {
    pub fn new(hir: &'a hir::File) -> Self {
        Self {
            hir,
            fn_types: HashMap::new(),
            vars: Scopes::new(),
            cur_func_data: None,
            cur_func_locals: None,
            cur_func_instructions: None,
            cur_func_local_names: None,
            data_section: DataSection::new(),
            name_section: NameSection::new(),
            export_section: ExportSection::new(),
            memory_section: MemorySection::new(),
            function_section: FunctionSection::new(),
            type_section: TypeSection::new(),
            code_section: CodeSection::new(),
            global_section: GlobalSection::new(),
            function_names: NameMap::new(),
            local_names: IndirectNameMap::new(),
            type_names: NameMap::new(),
            global_names: NameMap::new(),
        }
    }

    pub fn load_corelib() {
        let wasm = std::fs::read("input.wat").unwrap();
    }

    pub fn push_instruction(&mut self, instruction: Instruction<'a>) {
        let instructions = self.cur_func_instructions.as_mut().unwrap();
        instructions.push(instruction);
    }

    pub fn add_local(&mut self, val_ty: ValType) -> u32 {
        let locals = self.cur_func_locals.as_mut().unwrap();
        let idx = locals.len();
        locals.push(val_ty);
        return idx as u32;
    }

    pub fn get_type(&self, hir_id: &HirId) -> &ty::Type {
        self.hir
            .node_types
            .get(hir_id)
            .expect("typechecking to provide")
    }

    pub fn codegen_hir(&mut self, root: &'a hir::File) -> Result<(), ()> {
        for item in &root.elements {
            match &item.kind {
                hir::ElemKind::Extern(_) => todo!(),
                hir::ElemKind::Signature(_) => todo!(),
                hir::ElemKind::Function(f) => self.codegen_fn_declaration(f)?,
                hir::ElemKind::Struct(_) => {}
            }
        }

        for body in root.bodies.values() {
            self.codegen_fn_body(body)?;
        }

        self.global_names.append(0, "data_end");
        self.global_names.append(1, "heap_base");

        self.name_section.functions(&self.function_names);
        self.name_section.locals(&self.local_names);
        //self.name_section.labels(&self.label_names);
        self.name_section.types(&self.type_names);
        self.name_section.globals(&self.global_names);
        //self.name_section.data(&self.data_names);

        Ok(())
    }

    pub fn codegen_fn_body(&mut self, fn_body: &'a hir::FnBody) -> Result<(), ()> {
        let func = self.hir.get_fn(&fn_body.fn_id).unwrap();
        let fn_analisis = self.hir.get_fn_analisis(&fn_body.fn_id);

        if let Some(fn_type_idx) = self.fn_types.get(&func.hir_id).cloned() {
            //self.cur_func = Some(func);
            self.cur_func_data = Some(fn_analisis.unwrap().clone());
            self.cur_func_local_names = Some(NameMap::new());
            self.cur_func_locals = Some(vec![]);
            self.cur_func_instructions = Some(vec![]);
            self.vars.push();

            self.codegen_body(&fn_body.body)?;

            match (
                self.cur_func_locals.clone(),
                &self.cur_func_local_names,
                &self.cur_func_instructions,
            ) {
                (Some(locals), Some(local_names), Some(instructions)) => {
                    self.function_section.function(fn_type_idx);

                    let func_idx = self.function_section.len() - 1;
                    let func_name = func.get_name();
                    self.function_names.append(func_idx, func_name.as_str());

                    println!("locals={:?}", locals);
                    let mut func = Function::new_with_locals_types(locals);

                    for instruction in instructions {
                        func.instruction(instruction);
                    }

                    func.instruction(&Instruction::End); // End function

                    self.local_names.append(func_idx, local_names);
                    self.code_section.function(&func);
                }
                _ => unreachable!(),
            }

            self.vars.pop();
            self.cur_func_data = None;
            self.cur_func_local_names = None;
            self.cur_func_locals = None;
            self.cur_func_instructions = None;
        } else {
            panic!("Cannot find function {:?}", func.get_name());
        }

        Ok(())
    }

    pub fn codegen_body(&mut self, body: &'a hir::Block) -> Result<Option<Instruction>, ()> {
        let first_return_idx = body
            .stmts
            .iter()
            .position(|s| s.is_return())
            .unwrap_or(body.stmts.len());

        let stmts = body.stmts.iter().take(first_return_idx + 1);

        let mut last = None;
        for stmt in stmts {
            last = self.codegen_stmt(stmt)?;
        }

        Ok(None)
    }

    fn codegen_struct_expression(
        &mut self,
        struct_expr: &ExprStruct,
    ) -> Result<Option<Instruction>, ()> {
        Ok(None)
    }

    fn codegen_block(&mut self, block: &'a Block) -> Result<Option<Instruction>, ()> {
        // TODO: support non empty blocks
        self.push_instruction(Instruction::Block(BlockType::Empty));

        for stmt in &block.stmts {
            self.codegen_stmt(stmt)?;
        }

        self.push_instruction(Instruction::End);

        Ok(None)
    }

    fn codegen_bin(
        &mut self,
        binop: &'a BinOp,
        lhs: &'a hir::Expr,
        rhs: &'a hir::Expr,
    ) -> Result<Option<Instruction>, ()> {
        self.codegen_expression(lhs)?;
        self.codegen_expression(rhs)?;

        let (wasm_type, _) = match self.get_type(&lhs.hir_id) {
            ty::Type::Primitive(primitive) => match primitive {
                ty::PrimitiveType::Int64 => (ValType::I64, true),
                ty::PrimitiveType::Int32 => (ValType::I32, true),
                ty::PrimitiveType::Float64 => (ValType::F64, true),
                _ => unreachable!(),
            },
            _ => todo!("operator overloads"),
        };

        match binop.kind {
            hir::BinOpKind::Add => {
                match wasm_type {
                    ValType::I64 => self.push_instruction(Instruction::I64Add),
                    ValType::I32 => self.push_instruction(Instruction::I32Add),
                    ValType::F64 => self.push_instruction(Instruction::F64Add),
                    ValType::F32 => self.push_instruction(Instruction::F32Add),
                    _ => unreachable!(),
                };
            }
            hir::BinOpKind::Assign => todo!(),
            hir::BinOpKind::Sub => {
                match wasm_type {
                    ValType::I64 => self.push_instruction(Instruction::I64Sub),
                    ValType::I32 => self.push_instruction(Instruction::I32Sub),
                    ValType::F64 => self.push_instruction(Instruction::F64Sub),
                    ValType::F32 => self.push_instruction(Instruction::F32Sub),
                    _ => unreachable!(),
                };
            }
            hir::BinOpKind::Mul => todo!(),
            hir::BinOpKind::Div => todo!(),
            hir::BinOpKind::Mod => todo!(),
            hir::BinOpKind::Or => todo!(),
            hir::BinOpKind::And => todo!(),
            hir::BinOpKind::BitOr => todo!(),
            hir::BinOpKind::BitAnd => todo!(),
            hir::BinOpKind::BitXor => todo!(),
            hir::BinOpKind::ShiftL => todo!(),
            hir::BinOpKind::ArithShiftR => todo!(),
            hir::BinOpKind::LogicalShiftR => todo!(),
        };

        Ok(None)
    }

    pub fn codegen_expression(&mut self, expr: &'a hir::Expr) -> Result<Option<Instruction>, ()> {
        Ok(match *expr.kind {
            hir::ExprKind::Struct(ref s) => self.codegen_struct_expression(s)?,
            hir::ExprKind::Lit(ref lit) => self.codegen_literal(lit)?,
            hir::ExprKind::Return(ref expr) => self.codegen_return(expr)?,
            hir::ExprKind::Call(ref call) => self.codegen_call(call)?,
            hir::ExprKind::Block(ref block) => self.codegen_block(block)?,
            hir::ExprKind::Bin(ref op, ref lhs, ref rhs) => self.codegen_bin(op, lhs, rhs)?,
            hir::ExprKind::Ident(ref ident) => self.codegen_identifier(ident)?,
            hir::ExprKind::Dot(_) => todo!(),
            hir::ExprKind::Path(_) => todo!(),
        })
    }

    fn codegen_identifier(&mut self, id: &'a hir::Identifier) -> Result<Option<Instruction>, ()> {
        let analysis = self.cur_func_data.as_ref().unwrap();
        let ident_type = analysis.idents.get(id.hir_id).unwrap();

        match ident_type {
            hir::IdentType::Var(var_id) => {
                let var = analysis.vars.get_var(*var_id);
                let var_type = &var.ty;

                let wasm_val = match self.vars.get(*var_id) {
                    None => {
                        println!("var not assigned: {:#?} {:#?}", id, var_id);
                        return Err(());
                    }
                    Some(val) => val,
                };

                // Dereference primitives only
                if var_type.is_primitive() {
                    //let wasm_ty = self.codegen_type(&var_type.clone())?;
                    self.push_instruction(Instruction::LocalGet(wasm_val));
                }
            }
        }

        Ok(None)
    }

    fn codegen_literal(&mut self, lit: &'a hir::Literal) -> Result<Option<Instruction>, ()> {
        let analysis = self.cur_func_data.as_ref().unwrap();

        match lit.kind {
            hir::LiteralKind::Int(_) => {
                let primitive_ty = self.get_type(&lit.hir_id).as_primitive().ok_or(())?;
                let value = *analysis.int_literals.get(lit.hir_id).unwrap();

                match primitive_ty {
                    PrimitiveType::Int64 => {
                        self.push_instruction(Instruction::I64Const(value));
                    }
                    PrimitiveType::Int32 => {
                        self.push_instruction(Instruction::I32Const(value.try_into().unwrap()));
                    }
                    _ => unreachable!(),
                }
            }
            hir::LiteralKind::Bool(boolean) => {
                self.push_instruction(Instruction::I32Const(boolean.try_into().unwrap()));
            }
            hir::LiteralKind::Char(c) => self.push_instruction(Instruction::I32Const(c as i32)),
            hir::LiteralKind::Float(_) => todo!(),
            hir::LiteralKind::String(_) => todo!(),
            hir::LiteralKind::Unit => todo!(),
        };

        Ok(None)
    }

    fn codegen_call(&mut self, call: &'a hir::FnCall) -> Result<Option<Instruction>, ()> {
        for arg in &call.args {
            self.codegen_expression(&arg.expr)?;
        }

        match *call.callee.kind {
            hir::ExprKind::Ident(ref ident) => {
                //ident.name
            }
            _ => unimplemented!(),
        };

        self.push_instruction(Instruction::Call(0));

        Ok(None)
    }

    fn codegen_return(&mut self, expr: &'a Option<hir::Expr>) -> Result<Option<Instruction>, ()> {
        Ok(if let Some(expr) = expr {
            self.codegen_expression(expr)?;

            // This is disabled because this is handled in lower_body
            // builder.build_return(Some(&val));
            //self.push_instruction(Instruction::Return);
            None
        } else {
            // Hardcoded unit value
            // builder.build_return(None);
            None
        })
    }

    pub fn codegen_stmt(&mut self, stmt: &'a hir::Statement) -> Result<Option<Instruction>, ()> {
        Ok(match &*stmt.kind {
            hir::StatementKind::Let(l) => self.codegen_let(l)?,
            hir::StatementKind::Expr(e) => match e.kind.as_ref() {
                ExprKind::Lit(_) | ExprKind::Struct(_) | ExprKind::Bin(_, _, _) => None, // empty expression like: 0;
                _ => self.codegen_expression(e)?,
            },
        })
    }

    fn codegen_let(&mut self, let_bind: &'a hir::LetBinding) -> Result<Option<Instruction>, ()> {
        match let_bind.pattern {
            hir::PatternKind::Ident(ref id) => {
                let expr = let_bind.expr.as_ref().unwrap();
                self.codegen_expression(expr)?;

                let analysis = self.cur_func_data.as_ref().unwrap();
                let var_id = *analysis.map_vars.get(id.hir_id).unwrap();

                let existing_value = self.vars.get(var_id);
                println!("var_id={:?}, {:?}", var_id, existing_value);

                let val = match existing_value {
                    Some(val) => Some(val),
                    None => {
                        let var_type = &analysis.vars.get_var(var_id).ty;

                        let wasm_type = match var_type
                            .as_primitive()
                            .expect("load non primitives not supported yet")
                        {
                            PrimitiveType::Bool => ValType::I32,
                            PrimitiveType::Int32 => ValType::I32,
                            PrimitiveType::Int64 => ValType::I64,
                            PrimitiveType::Float32 => ValType::F32,
                            PrimitiveType::Float64 => ValType::F64,
                            _ => unreachable!(),
                        };

                        // self.scopes.add(id.hir_id, ptr.as_basic_value_enum());
                        let local_idx = self.add_local(wasm_type);
                        self.cur_func_local_names.as_mut().unwrap().append(
                            local_idx,
                            format!("{}<{:?}>", id.name.to_string(), wasm_type).as_str(),
                        );

                        Some(local_idx)
                    }
                };

                if let Some(idx) = val {
                    self.push_instruction(Instruction::LocalSet(idx));
                    self.vars.add(var_id, idx);
                }
            }
            _ => unreachable!(),
        }

        Ok(None)
    }

    pub fn codegen_fn_declaration(&mut self, func: &'a hir::FnDeclaration) -> Result<(), ()> {
        let name = func.get_name();

        let func_ty = self.hir.node_types.get(&func.hir_id).unwrap();

        if let Type::Function(func_ty) = func_ty {
            let return_ty = func_ty.output.clone().unwrap();

            if let Type::Primitive(PrimitiveType::Unit) = *return_ty {
                todo!("unit type")
            } else {
                let wasm_type = self.codegen_type(&return_ty)?;

                // Encode the type section.
                let params = vec![];
                let results = wasm_type.1;
                self.type_section.ty().function(params, results);

                let type_idx = self.type_section.len() - 1;
                self.fn_types.insert(func.hir_id, type_idx);
                self.type_names.append(type_idx, name.as_str());
            };
        } else {
            return Err(());
        }

        Ok(())
    }

    fn codegen_struct(
        &mut self,
        stru: &'a hir::Struct,
    ) -> Result<(ComponentValType, Vec<ValType>), ()> {
        Ok((
            ComponentValType::Primitive(PrimitiveValType::S32),
            vec![ValType::I32],
        ))
    }

    fn codegen_type(&mut self, ty: &ty::Type) -> Result<(ComponentValType, Vec<ValType>), ()> {
        let ty = match ty {
            Type::Primitive(PrimitiveType::Int32) => (
                ComponentValType::Primitive(PrimitiveValType::S32),
                vec![ValType::I32],
            ),
            Type::Primitive(PrimitiveType::Int64) => (
                ComponentValType::Primitive(PrimitiveValType::S64),
                vec![ValType::I64],
            ),
            Type::Primitive(PrimitiveType::Float64) => (
                ComponentValType::Primitive(PrimitiveValType::F64),
                vec![ValType::F64],
            ),
            Type::Primitive(PrimitiveType::Bool) => (
                ComponentValType::Primitive(PrimitiveValType::Bool),
                vec![ValType::I32],
            ),
            Type::Primitive(PrimitiveType::Unit) => (
                ComponentValType::Primitive(PrimitiveValType::Bool),
                vec![ValType::I32],
            ),
            Type::Primitive(PrimitiveType::Char) => (
                ComponentValType::Primitive(PrimitiveValType::Char),
                vec![ValType::I32],
            ),
            Type::Function(_) => (ComponentValType::Type(0), vec![ValType::I32]),
            Type::Struct(id) => {
                let stru = self.hir.get_struct(id).unwrap();
                self.codegen_struct(stru)?
            }
            _ => unimplemented!("Codegen: Cannot codegen type {:#?}", ty),
        };

        Ok(ty)
    }
}
