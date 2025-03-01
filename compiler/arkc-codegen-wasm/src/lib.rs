mod scopes;
mod util;

use arkc_hir::{
    hir::{self, HirId},
    ty::{self, PrimitiveType, Type},
};
use std::{collections::HashMap, path::Path};
use wasm_encoder::{
    CodeSection, ComponentValType, ConstExpr, DataSection, ExportKind, ExportSection, Function,
    FunctionSection, GlobalSection, GlobalType, MemorySection, MemoryType, Module, NameMap,
    NameSection, PrimitiveValType, TypeSection, ValType,
};

pub fn codegen(hir: &hir::File) -> Result<(), ()> {
    let mut codegen = CodegenContext::new(&hir);

    // Encode the name section.
    codegen.name_section.module("module");

    // Encode the type section.
    let params = vec![ValType::I32, ValType::I32];
    let results = vec![ValType::I32];
    codegen.type_section.ty().function(params, results);

    // Encode the function section.
    let type_index = 0;
    codegen.function_section.function(type_index);

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

    // Encode the code section.
    let locals = vec![];
    let mut f = Function::new(locals);
    f.instructions().local_get(0).local_get(1).i32_add().end();
    codegen.code_section.function(&f);

    let _ = codegen.codegen_hir(hir);

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
    pub type_names: NameMap,
    pub global_names: NameMap,
}

impl<'a> CodegenContext<'a> {
    pub fn new(hir: &'a hir::File) -> Self {
        Self {
            hir,
            fn_types: HashMap::new(),
            data_section: DataSection::new(),
            name_section: NameSection::new(),
            export_section: ExportSection::new(),
            memory_section: MemorySection::new(),
            function_section: FunctionSection::new(),
            type_section: TypeSection::new(),
            code_section: CodeSection::new(),
            global_section: GlobalSection::new(),
            function_names: NameMap::new(),
            type_names: NameMap::new(),
            global_names: NameMap::new(),
        }
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

        self.function_names.append(0, "dummy");
        self.name_section.functions(&self.function_names);

        self.global_names.append(0, "data_end");
        self.global_names.append(1, "heap_base");
        self.name_section.globals(&self.global_names);

        self.type_names.append(0, "main_signature");
        self.name_section.types(&self.type_names);

        Ok(())
    }

    pub fn codegen_fn_body(&mut self, fn_body: &'a hir::FnBody) -> Result<(), ()> {
        let func = self.hir.get_fn(&fn_body.fn_id).unwrap();
        let fn_analisis = self.hir.get_fn_analisis(&fn_body.fn_id);

        let type_idx = self.fn_types.get(&func.hir_id).ok_or(()).cloned()?;

        // Encode the function section.
        self.function_section.function(type_idx);

        let func_idx = self.function_section.len() - 1;
        let func_name = func.get_name();
        self.function_names.append(func_idx, func_name.as_str());

        let locals = vec![];
        let mut func = Function::new(locals);
        func.instructions().nop().end();
        self.code_section.function(&func);

        Ok(())
    }

    pub fn codegen_fn_declaration(&mut self, func: &hir::FnDeclaration) -> Result<(), ()> {
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
        stru: &hir::Struct,
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

/// The (code) Generator provides all the needed context for generating LLVM IR
/// while walking the Ast.
#[derive(Debug)]
pub struct Generator {}

fn path_to_module_name(path: &Path) -> String {
    path.with_extension("").to_string_lossy().into()
}

fn to_size_level(optimization_argument: char) -> u32 {
    match optimization_argument {
        's' => 1,
        'z' => 2,
        _ => 0,
    }
}

impl Generator {
    fn codegen_main(&mut self, hir: &hir::FnDeclaration) {}
}
