use inkwell::values::BasicValueEnum;
use parser::ast::*;
use arkc_frontend::SourceType;

#[derive(Debug, Clone)]
pub struct Error;

pub type CompileResult = Result<BasicValueEnum, Error>;

fn convert_type(&mut self, typ: &SourceType) -> BasicTypeEnum<'g> {
    match typ {
        TypeData::Primitive(p) => {
            match p {
                TypePrimitiveType(kind) => self
                    .context
                    .custom_width_int_type(self.integer_bit_count(*kind))
                    .into(),
                TypePrimitiveType::Float(FloatKind::F32) => self.context.f32_type().into(),
                TypePrimitiveType::Float(FloatKind::F64) => self.context.f64_type().into(),
                TypePrimitiveType::Char => self.context.i8_type().into(),
                TypePrimitiveType::Boolean => self.context.bool_type().into(),
                TypePrimitiveType::Unit => self.context.bool_type().into(),
                TypePrimitiveType::Pointer => self
                    .context
                    .i8_type()
                    .ptr_type(AddressSpace::default())
                    .into(),
            }
        }
        _ => todo!()
    }
}

/// Converts a ast::FnDecl directly to an inkwell FunctionType.
/// Note that the representation of functions in Ark is typically not
/// a FunctionType directly but a function pointeror closure tuple.
fn convert_function_type(&mut self, fn_decl: &FnDecl) -> FunctionType<'g> {
    let parameters = fmap(&fn_decl.inputs, |param| self.convert_type(param).into());
    let ret = self.convert_type(&fn_decl.output);
    ret.fn_type(&parameters, false)
}

pub fn compile_closure(ctx: &mut CodegenContext, func: &Function) -> CompileResult {
    let raw_fn = codegen_raw_fn(ctx, closure)?;
}

pub fn compile_element(ctx: &mut CodegenContext, ast: &ElemData) -> CompileResult {
    match ast {
        ElemData::Function(func) => compile_function(ctx, &func),
        ElemData::Flow(_) => panic!("cannot compile raw flow"),
        _ => panic!("unsupported"),
    }
}

pub fn compile_file(ctx: &mut CodegenContext, ast: &File) -> CompileResult {
    let mut val_opt = None;

    for ast in ast.elements {
        val_opt = Some(compile_element(ctx, &ast)?);
    }

    let val_or_nil = val_opt.unwrap_or_else(|| ctx.context.void_type());

    Ok(val_or_nil)
}
