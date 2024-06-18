mod expr;
#[cfg(test)]
pub mod tests;

use std::collections::HashMap;

use crate::specialize::{replace_type, specialize_type};
use bytecode::{
    AliasId, BytecodeBuilder, BytecodeFunction, BytecodeType, BytecodeTypeArray, ClassId,
    ConstPoolEntry, ConstPoolIdx, EnumId, FunctionId, GlobalId, Intrinsic, Label, Location,
    Register, StructId, TraitId,
};
use expr::{gen_expr, gen_expr_bin_cmp};
use parser::{
    ast::{self, CmpOp},
    Span,
};

use crate::{
    expr_block_always_returns,
    sema::{
        emit_as_bytecode_operation, AnalysisData, CallType, ContextFieldId, FctDefinition,
        FctDefinitionId, IdentType, LazyContextData, ScopeId, Sema, SourceFileId, VarId,
        VarLocation,
    },
    ty::SourceType,
};

pub fn bty_from_ty(ty: SourceType) -> BytecodeType {
    match ty {
        SourceType::Unit => BytecodeType::Unit,
        SourceType::Bool => BytecodeType::Bool,
        SourceType::UInt8 => BytecodeType::UInt8,
        SourceType::Char => BytecodeType::Char,
        SourceType::Int32 => BytecodeType::Int32,
        SourceType::Int64 => BytecodeType::Int64,
        SourceType::Float32 => BytecodeType::Float32,
        SourceType::Float64 => BytecodeType::Float64,
        //SourceType::Class(class_id, type_params) => BytecodeType::Class(
        //    ClassId(class_id.index().try_into().expect("overflow")),
        //    bty_array_from_ty(&type_params),
        //),
        //SourceType::Trait(trait_id, type_params) => BytecodeType::Trait(
        //     TraitId(trait_id.index().try_into().expect("overflow")),
        //    bty_array_from_ty(&type_params),
        //),
        //SourceType::Enum(enum_id, type_params) => BytecodeType::Enum(
        //    EnumId(enum_id.index().try_into().expect("overflow")),
        //    bty_array_from_ty(&type_params),
        //),
        //SourceType::Struct(struct_id, type_params) => BytecodeType::Struct(
        //    StructId(struct_id.index().try_into().expect("overflow")),
        //    bty_array_from_ty(&type_params),
        //),
        //SourceType::Tuple(subtypes) => BytecodeType::Tuple(bty_array_from_ty(&subtypes)),
        // SourceType::TypeParam(idx) => BytecodeType::TypeParam(idx.to_usize() as u32),
        //SourceType::Lambda(params, return_type) => BytecodeType::Lambda(
        //    bty_array_from_ty(&params),
        //    Box::new(bty_from_ty(*return_type)),
        //),
        SourceType::Ptr => BytecodeType::Ptr,
        SourceType::This => BytecodeType::This,
        //SourceType::TypeAlias(id) => {
        //    BytecodeType::TypeAlias(AliasId(id.index().try_into().expect("overflow")))
        //}
        _ => panic!("SourceType {:?} cannot be converted to BytecodeType", ty),
    }
}

pub fn generate_fct_id(sa: &Sema, id: FctDefinitionId) -> BytecodeFunction {
    let fct = sa.function(id);
    let analysis = fct.analysis();

    generate_fct(sa, &fct, analysis)
}

pub fn generate_fct(sa: &Sema, fct: &FctDefinition, src: &AnalysisData) -> BytecodeFunction {
    let ast_bytecode_generator = AstBytecodeGen {
        sa,
        // type_params_len: fct.type_params().len(),
        // is_lambda: fct.is_lambda(),
        return_type: Some(fct.return_type()),
        file_id: fct.file_id,
        span: fct.span,
        analysis: src,

        builder: BytecodeBuilder::new(),
        //loops: Vec::new(),
        var_registers: HashMap::new(),
        unit_register: None,
        entered_contexts: Vec::new(),
    };

    ast_bytecode_generator.generate_fct(&fct.ast)
}

pub fn register_bty_from_ty(ty: SourceType) -> BytecodeType {
    match ty {
        SourceType::Unit => BytecodeType::Unit,
        SourceType::Bool => BytecodeType::Bool,
        SourceType::UInt8 => BytecodeType::UInt8,
        SourceType::Char => BytecodeType::Char,
        SourceType::Int32 => BytecodeType::Int32,
        SourceType::Int64 => BytecodeType::Int64,
        SourceType::Float32 => BytecodeType::Float32,
        SourceType::Float64 => BytecodeType::Float64,
        //SourceType::Class(_, _) => BytecodeType::Ptr,
        //SourceType::Trait(trait_id, type_params) => BytecodeType::Trait(
        //    TraitId(trait_id.index().try_into().expect("overflow")),
        //    bty_array_from_ty(&type_params),
        //),
        //SourceType::Enum(enum_id, type_params) => BytecodeType::Enum(
        //     EnumId(enum_id.index().try_into().expect("overflow")),
        //    bty_array_from_ty(&type_params),
        //),
        //SourceType::Struct(struct_id, type_params) => BytecodeType::Struct(
        //    StructId(struct_id.index().try_into().expect("overflow")),
        //    bty_array_from_ty(&type_params),
        //),
        //SourceType::Tuple(subtypes) => BytecodeType::Tuple(bty_array_from_ty(&subtypes)),
        //SourceType::TypeParam(idx) => BytecodeType::TypeParam(idx.to_usize() as u32),
        //SourceType::Lambda(_, _) => BytecodeType::Ptr,
        SourceType::Ptr => BytecodeType::Ptr,
        _ => panic!("SourceType {:?} cannot be converted to BytecodeType", ty),
    }
}

struct IntrinsicInfo {
    intrinsic: Intrinsic,
    fct_id: Option<FctDefinitionId>,
}

impl IntrinsicInfo {
    fn with_fct(intrinsic: Intrinsic, fct_id: FctDefinitionId) -> IntrinsicInfo {
        IntrinsicInfo {
            intrinsic,
            fct_id: Some(fct_id),
        }
    }
}

impl From<Intrinsic> for IntrinsicInfo {
    fn from(intrinsic: Intrinsic) -> IntrinsicInfo {
        IntrinsicInfo {
            intrinsic,
            fct_id: None,
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum DataDest {
    // Do not store result. Only interested in side-effects of an expression.
    Effect,

    // Allocate a new register and store result in it.
    Alloc,

    // Store the result in the given register.
    Reg(Register),
}

impl DataDest {
    fn is_unit(&self) -> bool {
        match self {
            DataDest::Effect => true,
            DataDest::Reg(_) => false,
            DataDest::Alloc => true,
        }
    }

    fn is_effect(&self) -> bool {
        match self {
            DataDest::Effect => true,
            DataDest::Reg(_) => false,
            DataDest::Alloc => false,
        }
    }

    fn is_alloc(&self) -> bool {
        match self {
            DataDest::Effect => false,
            DataDest::Reg(_) => false,
            DataDest::Alloc => true,
        }
    }

    fn reg(&self) -> Register {
        match self {
            DataDest::Effect | DataDest::Alloc => panic!("not a register"),
            DataDest::Reg(reg) => *reg,
        }
    }
}

const SELF_VAR_ID: VarId = VarId(0);

struct EnteredContext {
    context_data: LazyContextData,
    register: Option<Register>,
}

struct AstBytecodeGen<'a> {
    sa: &'a Sema,
    //type_params_len: usize,
    //is_lambda: bool,
    return_type: Option<SourceType>,
    file_id: SourceFileId,
    span: Span,
    analysis: &'a AnalysisData,

    builder: BytecodeBuilder,
    //loops: Vec<LoopLabels>,
    var_registers: HashMap<VarId, Register>,
    entered_contexts: Vec<EnteredContext>,
    unit_register: Option<Register>,
}

impl<'a> AstBytecodeGen<'a> {
    fn loc(&self, span: Span) -> Location {
        self.sa.compute_loc(self.file_id, span)
    }

    fn load_from_context(
        &mut self,
        dest: Register,
        scope_id: ScopeId,
        field_id: ContextFieldId,
        location: Location,
    ) {
        unimplemented!();
        // Load context object.
        /*let entered_context = &self.entered_contexts[scope_id.0];
        let context_register = entered_context.register.expect("missing register");
        let context_data = entered_context.context_data.clone();
        let cls_id = context_data.class_id();
        let field_id = field_id_from_context_idx(field_id, context_data.has_parent_slot());
        let field_idx = self.builder.add_const_field_types(
            ClassId(cls_id.index().try_into().expect("overflow")),
            bty_array_from_ty(&self.identity_type_params()),
            field_id.0 as u32,
        );
        self.builder
            .emit_load_field(dest, context_register, field_idx, location);*/
    }

    fn var_reg(&self, var_id: VarId) -> Register {
        *self
            .var_registers
            .get(&var_id)
            .expect("no register for var found")
    }

    fn ensure_register(&mut self, dest: DataDest, ty: BytecodeType) -> Register {
        match dest {
            DataDest::Effect | DataDest::Alloc => {
                if ty.is_unit() {
                    self.ensure_unit_register()
                } else {
                    self.alloc_temp(ty)
                }
            }
            DataDest::Reg(reg) => reg,
        }
    }

    fn ensure_unit_register(&mut self) -> Register {
        if let Some(register) = self.unit_register {
            return register;
        }

        let register = self.builder.alloc_global(BytecodeType::Unit);
        self.unit_register = Some(register);
        register
    }

    fn visit_expr_block(&mut self, block: &ast::ExprBlockType, dest: DataDest) -> Register {
        self.push_scope();

        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }

        let result = if let Some(ref expr) = block.expr {
            gen_expr(self, expr, dest)
        } else {
            Register::invalid()
        };

        self.pop_scope();

        result
    }

    fn visit_expr_assert(&mut self, expr: &ast::ExprCallType, dest: DataDest) {
        assert!(dest.is_unit());
        let assert_reg = gen_expr(self, &*expr.args[0], DataDest::Alloc);
        self.builder.emit_push_register(assert_reg);
        let fid = self.sa.known.functions.assert();
        let idx = self
            .builder
            .add_const_fct(FunctionId(fid.index().try_into().expect("overflow")));
        let dest = self.ensure_unit_register();
        self.builder
            .emit_invoke_static(dest, idx, self.loc(expr.span));
        self.free_if_temp(assert_reg);
    }

    fn visit_expr_call_intrinsic(
        &mut self,
        expr: &ast::ExprCallType,
        info: IntrinsicInfo,
        dest: DataDest,
    ) -> Register {
        let intrinsic = info.intrinsic;
        let call_type = self.analysis.map_calls.get(expr.id).unwrap().clone();

        /*if call_type.is_method() {
            let object = expr.object().unwrap();

            match expr.args.len() {
                0 => self.emit_intrinsic_un(object, info, self.loc(expr.span), dest),
                1 => {
                    self.emit_intrinsic_bin(object, &expr.args[0], info, self.loc(expr.span), dest)
                }
                2 => {
                    assert_eq!(intrinsic, Intrinsic::ArraySet);
                    self.emit_intrinsic_array_set(
                        expr.object().unwrap(),
                        &expr.args[0],
                        &expr.args[1],
                        self.loc(expr.span),
                        dest,
                    )
                }
                _ => unreachable!(),
            }
        } else {*/
        match intrinsic {
            Intrinsic::Assert => {
                self.visit_expr_assert(expr, dest);
                Register::invalid()
            }

            //Intrinsic::ArrayGet => self.emit_intrinsic_bin(
            //    &expr.callee,
            //    &expr.args[0],
            //    info,
            //    self.loc(expr.span),
            //    dest,
            //),

            //Intrinsic::ArrayNewOfSize => self.emit_intrinsic_new_array(expr, dest),

            //Intrinsic::ArrayWithValues => {
            //    let ty = self.ty(expr.id);
            //    assert_eq!(
            //        ty.cls_id().expect("class expected"),
            //        self.sa.known.classes.array()
            //    );
            //    let type_params = ty.type_params();
            //    assert_eq!(1, type_params.len());
            //    let element_ty = type_params[0].clone();
            //    self.emit_array_with_variadic_arguments(expr, &[element_ty], 0, dest)
            //}
            _ => panic!("unimplemented intrinsic {:?}", intrinsic),
        }
        //}
    }

    fn visit_expr_call(&mut self, expr: &ast::ExprCallType, dest: DataDest) -> Register {
        if let Some(info) = self.get_intrinsic(expr.id) {
            if emit_as_bytecode_operation(info.intrinsic) {
                return self.visit_expr_call_intrinsic(expr, info, dest);
            }
        }

        let call_type = self.analysis.map_calls.get(expr.id).unwrap().clone();

        match *call_type {
            //CallType::NewEnum(ref enum_ty, variant_idx) => {
            //    return self.visit_expr_call_enum(expr, enum_ty.clone(), variant_idx, dest);
            //}

            //CallType::NewStruct(struct_id, ref type_params) => {
            //    return self.visit_expr_call_struct(expr, struct_id, type_params, dest);
            //}

            // CallType::NewClass(cls_id, ref type_params) => {
            //    return self.visit_expr_call_class(expr, cls_id, type_params, dest);
            //}

            //CallType::Lambda(ref params, ref return_type) => {
            //    return self.visit_expr_call_lambda(
            //        expr,
            //        params.clone(),
            //        return_type.clone(),
            //        dest,
            //    );
            //}

            // CallType::Expr(..)
            //| CallType::Method(..)
            //| CallType::GenericMethod(..)
            //| CallType::GenericStaticMethod(..)
            //| CallType::TraitObjectMethod(..)
            CallType::Function(..) => {}

            _ => panic!("unknown call type = {:?}", call_type),
        }

        // Find method that is called
        let callee_id = call_type.fct_id().expect("FctId missing");
        let callee = self.sa.function(callee_id);

        let callee_idx = self.add_const_pool_entry_for_call(&callee, &call_type);

        // Determine types for arguments and return values
        let (arg_types, return_type) = self.determine_callee_types(&call_type, &*callee);

        // Allocate register for result
        let return_reg = self.ensure_register(dest, register_bty_from_ty(return_type.clone()));

        // Evaluate object/self argument
        // TODO: class let object_argument = self.emit_call_object_argument(expr, &call_type);

        // Evaluate function arguments
        let arguments = self.emit_call_arguments(expr, &*callee, &call_type, &arg_types);

        //if let Some(obj_reg) = object_argument {
        //    self.builder.emit_push_register(obj_reg);
        //}

        for &arg_reg in &arguments {
            self.builder.emit_push_register(arg_reg);
        }

        // Emit the actual Invoke(Direct|Static|Virtual)XXX instruction
        self.emit_call_inst(return_reg, callee_idx, &call_type, self.loc(expr.span));

        //if let Some(obj_reg) = object_argument {
        //    self.free_if_temp(obj_reg);
        //}

        for arg_reg in arguments {
            self.free_if_temp(arg_reg);
        }

        return_reg
    }

    fn determine_callee_types(
        &mut self,
        call_type: &CallType,
        fct: &FctDefinition,
    ) -> (Vec<SourceType>, SourceType) {
        let return_type = self.specialize_type_for_call(&call_type, fct.return_type());

        let mut arg_types = Vec::with_capacity(fct.params_with_self().len());

        //if fct.has_hidden_self_argument() {
        //    let self_type = match call_type {
        //        CallType::TraitObjectMethod(trait_ty, _) => {
        //            // trait methods use Self as type for self argument but specialize_type_for_call can't handle Self.
        //            assert!(fct.params_with_self()[0].is_self() && !fct.is_static);
        //            trait_ty.clone()
        //        }
        //        _ => {
        //            let arg = fct.params_with_self()[0].clone();
        //            self.specialize_type_for_call(&call_type, arg.clone())
        //        }
        //    };

        //    arg_types.push(self_type);
        //}

        for arg in fct.params_without_self() {
            let arg = self.specialize_type_for_call(&call_type, arg.clone());
            arg_types.push(arg);
        }

        (arg_types, return_type)
    }

    fn emit_call_arguments(
        &mut self,
        expr: &ast::ExprCallType,
        callee: &FctDefinition,
        call_type: &CallType,
        arg_types: &[SourceType],
    ) -> Vec<Register> {
        let mut registers = Vec::new();

        // self was already emitted, needs to be ignored here.
        let arg_start_offset = 0;
        //let arg_start_offset = match *call_type {
        //    CallType::Expr(..) | CallType::Method(..) | CallType::GenericMethod(..) => 1,
        //    _ => 0,
        //};

        // Calculate number of non-variadic arguments
        let non_variadic_arguments = if callee.is_variadic.get() {
            arg_types.len() - arg_start_offset - 1
        } else {
            arg_types.len()
        };

        // Evaluate non-variadic arguments and track registers.
        for arg in expr.args.iter().take(non_variadic_arguments) {
            let reg = gen_expr(self, arg, DataDest::Alloc);
            registers.push(reg);
        }

        if callee.is_variadic.get() {
            todo!();
            //let array_reg = self.emit_array_with_variadic_arguments(
            //    expr,
            //     arg_types,
            //    non_variadic_arguments,
            //    DataDest::Alloc,
            //);
            //registers.push(array_reg);
        }

        registers
    }

    fn add_const_pool_entry_for_call(
        &mut self,
        fct: &FctDefinition,
        call_type: &CallType,
    ) -> ConstPoolIdx {
        //let type_params = self.determine_call_type_params(call_type);
        //assert_eq!(fct.type_params().len(), type_params.len());

        match *call_type {
            //CallType::GenericStaticMethod(id, ..) | CallType::GenericMethod(id, ..) => {
            //    self.builder.add_const_generic(
            //        id.0 as u32,
            //        FunctionId(fct.id().index().try_into().expect("overflow")),
            //        bty_array_from_ty(&type_params),
            //    )
            //}
            //CallType::TraitObjectMethod(ref trait_object_ty, _) => {
            //    self.builder.add_const(ConstPoolEntry::TraitObjectMethod(
            //        bty_from_ty(trait_object_ty.clone()),
            //        FunctionId(fct.id().index().try_into().expect("overflow")),
            //        bty_array_from_ty(&type_params),
            //    ))
            //}

            /*CallType::Method(..) | CallType::Expr(..) |*/
            CallType::Function(..) => {
                self.builder.add_const_fct_types(
                    FunctionId(fct.id().index().try_into().expect("overflow")),
                    BytecodeTypeArray::empty(), //bty_array_from_ty(&type_params),
                )
            }

            _ => panic!("unexpected call type {:?}", call_type),
        }
    }

    fn emit_call_inst(
        &mut self,
        return_reg: Register,
        callee_idx: ConstPoolIdx,
        call_type: &CallType,
        location: Location,
    ) {
        match *call_type {
            //CallType::Method(..) => {
            //    self.builder
            //        .emit_invoke_direct(return_reg, callee_idx, location);
            //}
            CallType::Function(..) => {
                self.builder
                    .emit_invoke_static(return_reg, callee_idx, location);
            }
            //CallType::Expr(..) => {
            //    self.builder
            //        .emit_invoke_direct(return_reg, callee_idx, location);
            //}
            //CallType::TraitObjectMethod(..) => {
            //    self.builder
            //        .emit_invoke_virtual(return_reg, callee_idx, location);
            //}
            //CallType::GenericMethod(..) => {
            //    self.builder
            //        .emit_invoke_generic_direct(return_reg, callee_idx, location);
            //}
            //CallType::GenericStaticMethod(..) => {
            //    self.builder
            //        .emit_invoke_generic_static(return_reg, callee_idx, location);
            //}
            //CallType::NewClass(..)
            //| CallType::NewStruct(..)
            //| CallType::NewEnum(..)
            //| CallType::Lambda(..)
            CallType::Intrinsic(..) => unreachable!(),
        }
    }

    fn visit_expr_ident(&mut self, ident: &ast::ExprIdentType, dest: DataDest) -> Register {
        let ident_type = self.analysis.map_idents.get(ident.id).unwrap();

        match ident_type {
            &IdentType::Var(var_id) => {
                self.visit_expr_ident_var(var_id, dest, self.loc(ident.span))
            }
            //&IdentType::Context(level, field_id) => {
            //    self.visit_expr_ident_context(level, field_id, dest, self.loc(ident.span))
            //}
            //&IdentType::Global(gid) => {
            //    self.visit_expr_ident_global(gid, dest, self.loc(ident.span))
            //}
            //&IdentType::Const(cid) => self.visit_expr_ident_const(cid, dest),
            //&IdentType::EnumVariant(enum_id, ref type_params, variant_idx) => self.emit_new_enum(
            //    enum_id,
            //    type_params.clone(),
            //     variant_idx,
            //    self.loc(ident.span),
            //    dest,
            //),

            //&IdentType::Field(_, _) => unreachable!(),
            //&IdentType::Struct(_) => unreachable!(),
            //&IdentType::StructField(_, _) => unreachable!(),
            &IdentType::Function(_, _) => unreachable!(),
            //&IdentType::Class(_, _) => unreachable!(),
        }
    }

    fn visit_expr_ident_var(
        &mut self,
        var_id: VarId,
        dest: DataDest,
        location: Location,
    ) -> Register {
        let var = self.analysis.vars.get_var(var_id);

        if dest.is_effect() {
            return Register::invalid();
        }

        if var.ty.is_unit() {
            assert!(dest.is_alloc());
            return self.ensure_unit_register();
        }

        match var.location {
            VarLocation::Context(scope_id, field_idx) => todo!(),
            //VarLocation::Context(scope_id, field_idx) => {
            //    let ty = register_bty_from_ty(var.ty.clone());
            //    let dest_reg = self.ensure_register(dest, ty);
            //    self.load_from_context(dest_reg, scope_id, field_idx, location);
            //    dest_reg
            //}
            VarLocation::Stack => {
                let var_reg = self.var_reg(var_id);

                if dest.is_alloc() {
                    return var_reg;
                }

                let dest = dest.reg();
                self.emit_mov(dest, var_reg);

                dest
            }
        }
    }

    fn visit_expr_un(&mut self, expr: &ast::ExprUnType, dest: DataDest) -> Register {
        if expr.op == ast::UnOp::Neg && expr.opnd.is_lit_int() {
            self.visit_expr_lit_int(expr.opnd.to_lit_int().unwrap(), dest, true)
        } else if let Some(intrinsic) = self.get_intrinsic(expr.id) {
            todo!()
            //self.emit_intrinsic_un(&expr.opnd, intrinsic, self.loc(expr.span), dest)
        } else {
            todo!()
            // self.visit_expr_un_method(expr, dest)
        }
    }

    fn visit_expr_lit_bool(&mut self, lit: &ast::ExprLitBoolType, dest: DataDest) -> Register {
        if dest.is_effect() {
            return Register::invalid();
        }

        let dest = self.ensure_register(dest, BytecodeType::Bool);

        if lit.value {
            self.builder.emit_const_true(dest);
        } else {
            self.builder.emit_const_false(dest);
        }

        dest
    }

    fn visit_expr_lit_int(
        &mut self,
        lit: &ast::ExprLitIntType,
        dest: DataDest,
        _neg: bool,
    ) -> Register {
        if dest.is_effect() {
            return Register::invalid();
        }

        let ty = self.analysis.ty(lit.id);
        let (value_i64, value_f64) = self.analysis.literal_value(lit.id);

        let ty = match ty {
            SourceType::UInt8 => BytecodeType::UInt8,
            SourceType::Int32 => BytecodeType::Int32,
            SourceType::Int64 => BytecodeType::Int64,
            SourceType::Float32 => {
                let dest = self.ensure_register(dest, BytecodeType::Float32);
                self.builder.emit_const_float32(dest, value_f64 as f32);
                return dest;
            }
            SourceType::Float64 => {
                let dest = self.ensure_register(dest, BytecodeType::Float64);
                self.builder.emit_const_float64(dest, value_f64);
                return dest;
            }
            _ => unreachable!(),
        };

        let dest = self.ensure_register(dest, ty.clone());

        match ty {
            BytecodeType::UInt8 => self.builder.emit_const_uint8(dest, value_i64 as u8),
            BytecodeType::Int32 => self.builder.emit_const_int32(dest, value_i64 as i32),
            BytecodeType::Int64 => self.builder.emit_const_int64(dest, value_i64),
            _ => unreachable!(),
        }

        dest
    }

    fn visit_expr_lit_float(&mut self, lit: &ast::ExprLitFloatType, dest: DataDest) -> Register {
        if dest.is_effect() {
            return Register::invalid();
        }

        let ty = self.analysis.ty(lit.id);
        let (_, value_f64) = self.analysis.literal_value(lit.id);

        let ty = match ty {
            SourceType::Float32 => BytecodeType::Float32,
            SourceType::Float64 => BytecodeType::Float64,
            _ => unreachable!(),
        };

        let dest = self.ensure_register(dest, ty.clone());

        match ty {
            BytecodeType::Float32 => self.builder.emit_const_float32(dest, value_f64 as f32),
            BytecodeType::Float64 => self.builder.emit_const_float64(dest, value_f64),
            _ => unreachable!(),
        }

        dest
    }

    fn visit_expr_bin(&mut self, expr: &ast::ExprBinType, dest: DataDest) -> Register {
        if expr.op.is_any_assign() {
            self.visit_expr_assign(expr, dest)
        } else if let ast::BinOp::Cmp(cmp_op) = expr.op {
            if cmp_op == CmpOp::Is || cmp_op == CmpOp::IsNot {
                self.emit_bin_is(expr, dest)
            } else {
                gen_expr_bin_cmp(self, expr, cmp_op, dest)
            }
        } else if expr.op == ast::BinOp::Or {
            self.emit_bin_or(expr, dest)
        } else if expr.op == ast::BinOp::And {
            self.emit_bin_and(expr, dest)
        } else if let Some(info) = self.get_intrinsic(expr.id) {
            todo!()
            //self.emit_intrinsic_bin(&expr.lhs, &expr.rhs, info, self.loc(expr.span), dest)
        } else {
            todo!()
            // self.visit_expr_bin_method(expr, dest)
        }
    }

    fn visit_expr_assign_var(&mut self, expr: &ast::ExprBinType, var_id: VarId) {
        let var = self.analysis.vars.get_var(var_id);

        match var.location {
            VarLocation::Context(scope_id, field_id) => {
                todo!()
                //let value_reg = gen_expr(self, &expr.rhs, DataDest::Alloc);
                //self.store_in_context(value_reg, scope_id, field_id, self.loc(expr.span));
                //self.free_if_temp(value_reg);
            }
            VarLocation::Stack => {
                let dest = if var.ty.is_unit() {
                    DataDest::Effect
                } else {
                    let var_reg = self.var_reg(var_id);
                    DataDest::Reg(var_reg)
                };

                gen_expr(self, &expr.rhs, dest);
            }
        }
    }

    fn visit_expr_assign_call(&mut self, expr: &ast::ExprBinType, call_expr: &ast::ExprCallType) {
        let object = &call_expr.callee;
        let index = &call_expr.args[0];
        let value = &expr.rhs;

        if let Some(info) = self.get_intrinsic(expr.id) {
            match info.intrinsic {
                Intrinsic::ArraySet => {
                    todo!()
                    //self.emit_intrinsic_array_set(
                    //    object,
                    //    index,
                    //    value,
                    //    self.loc(expr.span),
                    //    DataDest::Effect,
                    //);
                }
                _ => panic!("unexpected intrinsic {:?}", info.intrinsic),
            }
        } else {
            let call_type = self.analysis.map_calls.get(expr.id).unwrap();
            let fct_id = call_type.fct_id().unwrap();

            let obj_reg = gen_expr(self, object, DataDest::Alloc);
            let idx_reg = gen_expr(self, index, DataDest::Alloc);
            let val_reg = gen_expr(self, value, DataDest::Alloc);

            let obj_ty = self.ty(object.id());

            self.builder.emit_push_register(obj_reg);
            self.builder.emit_push_register(idx_reg);
            self.builder.emit_push_register(val_reg);

            //let type_params = obj_ty.type_params();

            let callee_idx = self.builder.add_const_fct_types(
                FunctionId(fct_id.index().try_into().expect("overflow")),
                BytecodeTypeArray::empty(), //bty_array_from_ty(&type_params),
            );
            let dest = self.ensure_unit_register();
            self.builder
                .emit_invoke_direct(dest, callee_idx, self.loc(expr.span));

            self.free_if_temp(obj_reg);
            self.free_if_temp(idx_reg);
            self.free_if_temp(val_reg);
        }
    }

    fn visit_expr_assign(&mut self, expr: &ast::ExprBinType, dest: DataDest) -> Register {
        assert!(dest.is_unit());

        if expr.lhs.is_ident() {
            let ident_type = self.analysis.map_idents.get(expr.lhs.id()).unwrap();
            match ident_type {
                &IdentType::Var(var_id) => self.visit_expr_assign_var(expr, var_id),
                //&IdentType::Context(level, field_id) => {
                //    self.visit_expr_assign_context(expr, level, field_id)
                //}
                //&IdentType::Global(gid) => self.visit_expr_assign_global(expr, gid),
                _ => unreachable!(),
            }
        } else {
            match *expr.lhs {
                // ast::ExprData::Dot(ref dot) => self.visit_expr_assign_dot(expr, dot),
                ast::ExprData::Call(ref call) => self.visit_expr_assign_call(expr, call),
                _ => unreachable!(),
            };
        }

        Register::invalid()
    }

    fn visit_expr_return(&mut self, ret: &ast::ExprReturnType, _dest: DataDest) -> Register {
        if let Some(ref expr) = ret.expr {
            let result_reg = gen_expr(self, expr, DataDest::Alloc);
            self.emit_ret_value(result_reg);
            self.free_if_temp(result_reg);
        } else {
            let dest = self.ensure_unit_register();
            self.builder.emit_ret(dest);
        }
        self.ensure_unit_register()
    }

    fn visit_stmt(&mut self, stmt: &ast::StmtData) {
        match *stmt {
            ast::StmtData::Expr(ref expr) => self.visit_stmt_expr(expr),
            // ast::StmtData::Let(ref stmt) => self.visit_stmt_let(stmt),
        }
    }

    fn visit_stmt_expr(&mut self, stmt: &ast::StmtExprType) {
        let reg = gen_expr(self, &stmt.expr, DataDest::Effect);
        self.free_if_temp(reg);
    }

    fn generate_fct(mut self, ast: &ast::Function) -> BytecodeFunction {
        self.push_scope();
        self.create_params(ast);
        self.enter_function_context();
        self.initialize_params(ast);
        self.emit_function_body(ast);
        self.leave_function_context();
        self.pop_scope();
        self.builder.generate()
    }

    fn create_params(&mut self, ast: &ast::Function) {
        let mut params = Vec::new();

        //if self.analysis.has_self() {
        //    let var_self = self.analysis.vars.get_self();
        //    let var_ty = var_self.ty.clone();

        //    let bty = bty_from_ty(var_ty.clone());
        //    params.push(bty);
        ////    let register_bty = register_bty_from_ty(var_ty);
        //    let reg = self.alloc_var(register_bty);
        //    self.var_registers.insert(SELF_VAR_ID, reg);
        //}

        for param in &ast.params {
            let var_id = *self.analysis.map_vars.get(param.id).unwrap();
            let ty = self.var_ty(var_id);

            let bty = bty_from_ty(ty.clone());
            let register_bty = register_bty_from_ty(ty);
            params.push(bty);
            let reg = self.alloc_var(register_bty);
            self.var_registers.insert(var_id, reg);
        }

        self.builder.set_params(params);
    }

    fn initialize_params(&mut self, ast: &ast::Function) {
        let next_register_idx = 0;
        /*let next_register_idx = if self.analysis.has_self() {
            let var_self = self.analysis.vars.get_self();
            let reg = Register(0);

            match var_self.location {
                VarLocation::Context(scope_id, field_id) => {
                    todo!()
                    //self.store_in_context(reg, scope_id, field_id, self.loc(self.span));
                }

                VarLocation::Stack => {
                    self.var_registers.insert(SELF_VAR_ID, reg);
                }
            }

            1
        } else {
            0
        };*/

        for (param_idx, param) in ast.params.iter().enumerate() {
            let var_id = *self.analysis.map_vars.get(param.id).unwrap();
            let var = self.analysis.vars.get_var(var_id);
            let reg = Register(next_register_idx + param_idx);

            match var.location {
                VarLocation::Context(scope_id, field_id) => {
                    todo!()
                    // self.store_in_context(reg, scope_id, field_id, self.loc(self.span));
                }
                VarLocation::Stack => {
                    self.var_registers.insert(var_id, reg);
                }
            }
        }
    }

    fn emit_function_body(&mut self, ast: &ast::Function) {
        let return_type = self
            .return_type
            .as_ref()
            .expect("missing return type")
            .clone();
        let bty_return_type = if return_type.is_unit() {
            None
        } else {
            Some(bty_from_ty(return_type.clone()))
        };
        self.builder.set_return_type(bty_return_type);

        let mut needs_return = true;

        let block = ast.block.as_ref().expect("missing block");
        let block = block.to_block().expect("block node expected");

        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }

        if let Some(ref value) = block.expr {
            let reg = gen_expr(self, value, DataDest::Alloc);

            if !expr_block_always_returns(block) {
                self.emit_ret_value(reg);
            }

            needs_return = false;
            self.free_if_temp(reg);
        }

        if needs_return && return_type.is_unit() {
            let dest = self.ensure_unit_register();
            self.builder.emit_ret(dest);
        }
    }

    fn emit_bin_or(&mut self, expr: &ast::ExprBinType, dest: DataDest) -> Register {
        if dest.is_effect() {
            let end_lbl = self.builder.create_label();
            let dest = gen_expr(self, &expr.lhs, DataDest::Alloc);
            self.builder.emit_jump_if_true(dest, end_lbl);
            self.free_if_temp(dest);

            self.emit_expr_for_effect(&expr.rhs);
            self.builder.bind_label(end_lbl);

            Register::invalid()
        } else {
            let end_lbl = self.builder.create_label();
            let dest = self.ensure_register(dest, BytecodeType::Bool);

            gen_expr(self, &expr.lhs, DataDest::Reg(dest));
            self.builder.emit_jump_if_true(dest, end_lbl);
            gen_expr(self, &expr.rhs, DataDest::Reg(dest));
            self.builder.bind_label(end_lbl);

            dest
        }
    }

    fn emit_bin_is(&mut self, expr: &ast::ExprBinType, dest: DataDest) -> Register {
        if dest.is_effect() {
            self.emit_expr_for_effect(&expr.lhs);
            self.emit_expr_for_effect(&expr.rhs);
            return Register::invalid();
        }

        let dest = self.ensure_register(dest, BytecodeType::Bool);

        let lhs_reg = gen_expr(self, &expr.lhs, DataDest::Alloc);
        let rhs_reg = gen_expr(self, &expr.rhs, DataDest::Alloc);

        self.builder.emit_test_identity(dest, lhs_reg, rhs_reg);

        if expr.op == ast::BinOp::Cmp(ast::CmpOp::IsNot) {
            self.builder.emit_not(dest, dest);
        }

        self.free_if_temp(lhs_reg);
        self.free_if_temp(rhs_reg);

        dest
    }

    fn emit_expr_for_effect(&mut self, expr: &ast::ExprData) {
        let reg = gen_expr(self, expr, DataDest::Effect);
        self.free_if_temp(reg);
    }

    fn emit_bin_and(&mut self, expr: &ast::ExprBinType, dest: DataDest) -> Register {
        if dest.is_effect() {
            let end_lbl = self.builder.create_label();
            let dest = gen_expr(self, &expr.lhs, DataDest::Alloc);
            self.builder.emit_jump_if_false(dest, end_lbl);
            self.free_if_temp(dest);

            self.emit_expr_for_effect(&expr.rhs);
            self.builder.bind_label(end_lbl);

            Register::invalid()
        } else {
            let end_lbl = self.builder.create_label();
            let dest = self.ensure_register(dest, BytecodeType::Bool);

            gen_expr(self, &expr.lhs, DataDest::Reg(dest));
            self.builder.emit_jump_if_false(dest, end_lbl);
            gen_expr(self, &expr.rhs, DataDest::Reg(dest));
            self.builder.bind_label(end_lbl);

            dest
        }
    }

    fn emit_mov(&mut self, dest: Register, src: Register) {
        if dest != src {
            self.builder.emit_mov(dest, src);
        }
    }

    fn emit_ret_value(&mut self, result_reg: Register) {
        let ret_ty = self
            .return_type
            .as_ref()
            .expect("missing return type")
            .clone();

        if ret_ty.is_unit() {
            let dest = self.ensure_unit_register();
            self.builder.emit_ret(dest);
            return;
        }

        self.builder.emit_ret(result_reg);
    }

    fn enter_function_context(&mut self) {
        let context_data = self.analysis.function_context_data();
        self.enter_context(context_data);
    }

    fn leave_function_context(&mut self) {
        let context_data = self.analysis.function_context_data();
        self.leave_context(context_data);
    }

    fn enter_context(&mut self, context_data: LazyContextData) {
        let register = None;
        //let register = if context_data.has_class_id() {
        //    Some(self.create_context(context_data.clone()))
        //} else {
        //    None
        //};

        self.entered_contexts.push(EnteredContext {
            context_data,
            register,
        });
    }

    fn leave_context(&mut self, context_data: LazyContextData) {
        let entered_context = self.entered_contexts.pop().expect("missing context");

        //if context_data.has_class_id() {
        //    assert!(entered_context.register.is_some());
        //} else {
        assert!(entered_context.register.is_none());
        //}
    }

    /*fn create_context(&mut self, context_data: LazyContextData) -> Register {
        let class_id = context_data.class_id();

        let context_register = self.builder.alloc_global(BytecodeType::Ptr);
        let idx = self.builder.add_const_cls_types(
            ClassId(class_id.index().try_into().expect("overflow")),
            bty_array_from_ty(&self.identity_type_params()),
        );
        self.builder
            .emit_new_object(context_register, idx, self.loc(self.span));

        if context_data.has_parent_slot() {
            // Load context field of lambda object in self.
            let temp_parent_context_reg = self.alloc_temp(BytecodeType::Ptr);

            let parent_context_reg = if let Some(parent_context_reg) = self.last_context_register()
            {
                parent_context_reg
            } else {
                let self_reg = self.var_reg(SELF_VAR_ID);

                let lambda_cls_id = self.sa.known.classes.lambda();
                let idx = self.builder.add_const_field_types(
                    ClassId(lambda_cls_id.index().try_into().expect("overflow")),
                    BytecodeTypeArray::empty(),
                    0,
                );
                self.builder.emit_load_field(
                    temp_parent_context_reg,
                    self_reg,
                    idx,
                    self.loc(self.span),
                );

                temp_parent_context_reg
            };

            // Store value in parent field of context object.
            assert!(context_data.has_parent_slot());
            let idx = self.builder.add_const_field_types(
                ClassId(class_id.index().try_into().expect("overflow")),
                bty_array_from_ty(&self.identity_type_params()),
                0,
            );
            self.builder.emit_store_field(
                parent_context_reg,
                context_register,
                idx,
                self.loc(self.span),
            );

            self.free_temp(temp_parent_context_reg);
        }

        context_register
    }*/

    fn specialize_type_for_call(&self, call_type: &CallType, ty: SourceType) -> SourceType {
        match call_type {
            CallType::Function(_, ref type_params) => specialize_type(self.sa, ty, type_params),

            //CallType::Function(_, ref type_params)
            //| CallType::Expr(_, _, ref type_params)
            //| CallType::Method(_, _, ref type_params) => specialize_type(self.sa, ty, type_params),

            //CallType::TraitObjectMethod(trait_ty, _) => {
            //    let container_type_params = trait_ty.type_params();
            //    specialize_type(self.sa, ty, &container_type_params)
            //}
            //CallType::GenericMethod(id, _trait_id, _method_id, type_params)
            //| CallType::GenericStaticMethod(id, _trait_id, _method_id, type_params) => {
            //    replace_type(
            //        self.sa,
            //        ty,
            //        Some(type_params),
            //        Some(SourceType::TypeParam(*id)),
            //        AliasReplacement::None,
            //    )
            //}

            //CallType::Lambda(..)
            //| CallType::NewClass(..)
            //| CallType::NewStruct(..)
            //| CallType::NewEnum(..)
            CallType::Intrinsic(..) => {
                unreachable!()
            }
        }
    }

    fn ty(&self, id: ast::NodeId) -> SourceType {
        self.analysis.ty(id)
    }

    fn var_ty(&self, id: VarId) -> SourceType {
        self.analysis.vars.get_var(id).ty.clone()
    }

    fn get_intrinsic(&self, id: ast::NodeId) -> Option<IntrinsicInfo> {
        let call_type = self.analysis.map_calls.get(id).expect("missing CallType");

        if let Some(intrinsic) = call_type.to_intrinsic() {
            return Some(intrinsic.into());
        }

        let fid = if let Some(fct_id) = call_type.fct_id() {
            fct_id
        } else {
            return None;
        };

        let fct = self.sa.function(fid);

        if let Some(intrinsic) = fct.intrinsic.get().cloned() {
            return Some(IntrinsicInfo::with_fct(intrinsic, fid));
        }

        None
    }

    fn push_scope(&mut self) {
        self.builder.push_scope();
    }

    fn pop_scope(&mut self) {
        self.builder.pop_scope();
    }

    fn alloc_var(&mut self, ty: BytecodeType) -> Register {
        assert!(!ty.is_class());
        self.builder.alloc_var(ty)
    }

    fn alloc_temp(&mut self, ty: BytecodeType) -> Register {
        assert!(!ty.is_class());
        self.builder.alloc_temp(ty)
    }

    fn free_if_temp(&mut self, reg: Register) {
        self.builder.free_if_temp(reg);
    }

    fn free_temp(&mut self, reg: Register) {
        self.builder.free_temp(reg);
    }
}
