use std::collections::HashMap;

use crate::generator::generate_fct_id;

use arkc_frontend::{sema::FctDefinitionId, sym::ModuleSymTable, test, Sema};
use bytecode::{
    BytecodeFunction, BytecodeOffset, BytecodeVisitor, ConstPoolIdx, GlobalId, Register,
};

use self::Bytecode::*;

#[cfg(test)]
fn code(code: &'static str) -> Vec<Bytecode> {
    test::check_valid(code, |sa| {
        let fct_id = fct_by_name(sa, "f");
        let fct = generate_fct_id(sa, fct_id);
        build(&fct)
    })
}

pub fn fct_by_name(sa: &Sema, name: &str) -> FctDefinitionId {
    let name = sa.interner.intern(name);
    ModuleSymTable::new(sa, sa.program_module_id())
        .get(name)
        .expect("symbol not found")
        .to_fct()
        .expect("function expected")
}

fn build(bc: &BytecodeFunction) -> Vec<Bytecode> {
    let mut visitor = BytecodeArrayBuilder::new(bc);
    bytecode::read(bc.code(), &mut visitor);
    visitor.generate()
}

struct BytecodeArrayBuilder<'a> {
    bc: &'a BytecodeFunction,
    code: Vec<Bytecode>,
    next_idx: usize,
    offset_to_index: HashMap<BytecodeOffset, usize>,
    pc: BytecodeOffset,
    jumps: Vec<(usize, BytecodeOffset)>,
}

impl<'a> BytecodeArrayBuilder<'a> {
    fn new(bc: &'a BytecodeFunction) -> BytecodeArrayBuilder<'a> {
        BytecodeArrayBuilder {
            bc,
            code: Vec::new(),
            offset_to_index: HashMap::new(),
            next_idx: 0,
            pc: BytecodeOffset(0),
            jumps: Vec::new(),
        }
    }

    fn generate(mut self) -> Vec<Bytecode> {
        self.resolve_jumps();
        self.code
    }

    fn resolve_jumps(&mut self) {
        self.offset_to_index
            .insert(BytecodeOffset(self.bc.code().len() as u32), self.next_idx);
        let jumps = std::mem::replace(&mut self.jumps, Vec::new());

        for (location, target) in jumps {
            let &idx = self.offset_to_index.get(&target).expect("offset not found");

            match &mut self.code[location] {
                Bytecode::Jump(ref mut target) => *target = idx,
                Bytecode::JumpIfFalse(_, ref mut target) => *target = idx,
                Bytecode::JumpIfTrue(_, ref mut target) => *target = idx,
                _ => unreachable!(),
            }
        }
    }

    fn emit(&mut self, inst: Bytecode) {
        self.code.push(inst);
    }
}

impl<'a> BytecodeVisitor for BytecodeArrayBuilder<'a> {
    fn visit_instruction(&mut self, offset: BytecodeOffset) {
        self.offset_to_index.insert(offset, self.next_idx);
        self.next_idx += 1;
        self.pc = offset;
    }

    fn visit_add(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::Add(dest, lhs, rhs));
    }

    fn visit_sub(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::Sub(dest, lhs, rhs));
    }

    fn visit_neg(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::Neg(dest, src));
    }

    fn visit_mul(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::Mul(dest, lhs, rhs));
    }

    fn visit_div(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::Div(dest, lhs, rhs));
    }

    fn visit_mod(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::Mod(dest, lhs, rhs));
    }

    fn visit_and(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::And(dest, lhs, rhs));
    }

    fn visit_or(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::Or(dest, lhs, rhs));
    }

    fn visit_xor(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::Xor(dest, lhs, rhs));
    }

    fn visit_not(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::Not(dest, src));
    }

    fn visit_shl(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::Shl(dest, lhs, rhs));
    }
    fn visit_shr(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::Shr(dest, lhs, rhs));
    }
    fn visit_sar(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::Sar(dest, lhs, rhs));
    }

    fn visit_mov(&mut self, dest: Register, src: Register) {
        self.emit(Bytecode::Mov(dest, src));
    }

    fn visit_load_enum_variant(&mut self, dest: Register, src: Register, idx: ConstPoolIdx) {
        self.emit(Bytecode::LoadEnumVariant(dest, src, idx));
    }

    fn visit_load_tuple_element(&mut self, src: Register, dest: Register, idx: ConstPoolIdx) {
        self.emit(Bytecode::LoadTupleElement(src, dest, idx));
    }

    fn visit_load_struct_field(&mut self, dest: Register, obj: Register, idx: ConstPoolIdx) {
        self.emit(Bytecode::LoadStructField(dest, obj, idx));
    }

    fn visit_load_field(&mut self, dest: Register, obj: Register, idx: ConstPoolIdx) {
        self.emit(Bytecode::LoadField(dest, obj, idx));
    }

    fn visit_store_field(&mut self, src: Register, obj: Register, field: ConstPoolIdx) {
        self.emit(Bytecode::StoreField(src, obj, field));
    }

    fn visit_load_global(&mut self, dest: Register, global_id: GlobalId) {
        self.emit(Bytecode::LoadGlobal(dest, global_id));
    }

    fn visit_store_global(&mut self, src: Register, global_id: GlobalId) {
        self.emit(Bytecode::StoreGlobal(src, global_id));
    }

    fn visit_push_register(&mut self, src: Register) {
        self.emit(Bytecode::PushRegister(src));
    }

    fn visit_const_true(&mut self, dest: Register) {
        self.emit(Bytecode::ConstTrue(dest));
    }
    fn visit_const_false(&mut self, dest: Register) {
        self.emit(Bytecode::ConstFalse(dest));
    }
    fn visit_const_char(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self.bc.const_pool(idx).to_char().expect("char expected");
        self.emit(Bytecode::ConstChar(dest, value));
    }
    fn visit_const_uint8(&mut self, dest: Register, value: u8) {
        self.emit(Bytecode::ConstUInt8(dest, value));
    }
    fn visit_const_int32(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self.bc.const_pool(idx).to_int32().expect("int expected");
        self.emit(Bytecode::ConstInt32(dest, value));
    }
    fn visit_const_int64(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self.bc.const_pool(idx).to_int64().expect("int64 expected");
        self.emit(Bytecode::ConstInt64(dest, value));
    }
    fn visit_const_float32(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self
            .bc
            .const_pool(idx)
            .to_float32()
            .expect("float32 expected");
        self.emit(Bytecode::ConstFloat32(dest, value));
    }
    fn visit_const_float64(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self
            .bc
            .const_pool(idx)
            .to_float64()
            .expect("float64 expected");
        self.emit(Bytecode::ConstFloat64(dest, value));
    }
    fn visit_const_string(&mut self, dest: Register, idx: ConstPoolIdx) {
        let value = self
            .bc
            .const_pool(idx)
            .to_string()
            .expect("string expected")
            .to_owned();
        self.emit(Bytecode::ConstString(dest, value));
    }

    fn visit_test_identity(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestIdentity(dest, lhs, rhs));
    }
    fn visit_test_eq(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestEq(dest, lhs, rhs));
    }
    fn visit_test_ne(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestNe(dest, lhs, rhs));
    }
    fn visit_test_gt(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestGt(dest, lhs, rhs));
    }
    fn visit_test_ge(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestGe(dest, lhs, rhs));
    }
    fn visit_test_lt(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestLt(dest, lhs, rhs));
    }
    fn visit_test_le(&mut self, dest: Register, lhs: Register, rhs: Register) {
        self.emit(Bytecode::TestLe(dest, lhs, rhs));
    }

    fn visit_jump_if_false(&mut self, opnd: Register, offset: u32) {
        let offset = BytecodeOffset(self.pc.to_u32() + offset);
        self.jumps.push((self.next_idx - 1, offset));
        self.emit(Bytecode::JumpIfFalse(opnd, 0));
    }
    fn visit_jump_if_true(&mut self, opnd: Register, offset: u32) {
        let offset = BytecodeOffset(self.pc.to_u32() + offset);
        self.jumps.push((self.next_idx - 1, offset));
        self.emit(Bytecode::JumpIfTrue(opnd, 0));
    }
    fn visit_jump_loop(&mut self, offset: u32) {
        let offset = BytecodeOffset(self.pc.to_u32() - offset);
        let &idx = self.offset_to_index.get(&offset).expect("offset not found");
        self.emit(Bytecode::JumpLoop(idx));
    }
    fn visit_jump(&mut self, offset: u32) {
        let offset = BytecodeOffset(self.pc.to_u32() + offset);
        self.jumps.push((self.next_idx - 1, offset));
        self.emit(Bytecode::Jump(0));
    }
    fn visit_loop_start(&mut self) {
        self.emit(Bytecode::LoopStart);
    }

    fn visit_invoke_direct(&mut self, dest: Register, fctdef: ConstPoolIdx) {
        self.emit(Bytecode::InvokeDirect(dest, fctdef));
    }

    fn visit_invoke_virtual(&mut self, dest: Register, fct_idx: ConstPoolIdx) {
        self.emit(Bytecode::InvokeVirtual(dest, fct_idx));
    }

    fn visit_invoke_static(&mut self, dest: Register, fctdef: ConstPoolIdx) {
        self.emit(Bytecode::InvokeStatic(dest, fctdef));
    }

    fn visit_invoke_lambda(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit(Bytecode::InvokeLambda(dest, idx));
    }

    fn visit_invoke_generic_static(&mut self, dest: Register, fct_idx: ConstPoolIdx) {
        self.emit(Bytecode::InvokeGenericStatic(dest, fct_idx));
    }

    fn visit_invoke_generic_direct(&mut self, dest: Register, fct_idx: ConstPoolIdx) {
        self.emit(Bytecode::InvokeGenericDirect(dest, fct_idx));
    }

    fn visit_new_object(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit(Bytecode::NewObject(dest, idx));
    }
    fn visit_new_object_initialized(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit(Bytecode::NewObjectInitialized(dest, idx));
    }
    fn visit_new_array(&mut self, dest: Register, idx: ConstPoolIdx, length: Register) {
        self.emit(Bytecode::NewArray(dest, idx, length));
    }
    fn visit_new_tuple(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit(Bytecode::NewTuple(dest, idx));
    }
    fn visit_new_enum(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit(Bytecode::NewEnum(dest, idx));
    }
    fn visit_new_struct(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit(Bytecode::NewStruct(dest, idx));
    }
    fn visit_new_trait_object(&mut self, dest: Register, idx: ConstPoolIdx, src: Register) {
        self.emit(Bytecode::NewTraitObject(dest, idx, src));
    }
    fn visit_new_lambda(&mut self, dest: Register, idx: ConstPoolIdx) {
        self.emit(Bytecode::NewLambda(dest, idx));
    }

    fn visit_array_length(&mut self, dest: Register, arr: Register) {
        self.emit(Bytecode::ArrayLength(dest, arr));
    }

    fn visit_load_array(&mut self, dest: Register, arr: Register, idx: Register) {
        self.emit(Bytecode::LoadArray(dest, arr, idx));
    }

    fn visit_store_array(&mut self, src: Register, arr: Register, idx: Register) {
        self.emit(Bytecode::StoreArray(src, arr, idx));
    }

    fn visit_ret(&mut self, opnd: Register) {
        self.emit(Bytecode::Ret(opnd));
    }
}

#[test]
fn gen_add_int() {
    let result = code("fn f() -> Int32 { return 1i32 + 2i32; }");
    let expected = vec![
        ConstInt32(r(1), 1),
        ConstInt32(r(2), 2),
        Add(r(0), r(1), r(2)),
        Ret(r(0)),
    ];
    assert_eq!(expected, result);
}

fn r(val: usize) -> Register {
    Register(val)
}

#[derive(PartialEq, Debug)]
pub enum Bytecode {
    Add(Register, Register, Register),
    Sub(Register, Register, Register),
    Neg(Register, Register),
    Mul(Register, Register, Register),
    Div(Register, Register, Register),
    Mod(Register, Register, Register),
    And(Register, Register, Register),
    Or(Register, Register, Register),
    Xor(Register, Register, Register),
    Not(Register, Register),
    Shl(Register, Register, Register),
    Shr(Register, Register, Register),
    Sar(Register, Register, Register),

    Mov(Register, Register),

    LoadTupleElement(Register, Register, ConstPoolIdx),
    LoadStructField(Register, Register, ConstPoolIdx),
    LoadEnumVariant(Register, Register, ConstPoolIdx),

    LoadField(Register, Register, ConstPoolIdx),
    StoreField(Register, Register, ConstPoolIdx),

    LoadGlobal(Register, GlobalId),
    StoreGlobal(Register, GlobalId),

    PushRegister(Register),

    ConstTrue(Register),
    ConstFalse(Register),
    ConstUInt8(Register, u8),
    ConstChar(Register, char),
    ConstInt32(Register, i32),
    ConstInt64(Register, i64),
    ConstFloat32(Register, f32),
    ConstFloat64(Register, f64),
    ConstString(Register, String),

    TestIdentity(Register, Register, Register),
    TestEq(Register, Register, Register),
    TestNe(Register, Register, Register),
    TestGt(Register, Register, Register),
    TestGe(Register, Register, Register),
    TestLt(Register, Register, Register),
    TestLe(Register, Register, Register),

    LoopStart,
    JumpLoop(usize),
    Jump(usize),
    JumpIfFalse(Register, usize),
    JumpIfTrue(Register, usize),

    InvokeDirect(Register, ConstPoolIdx),
    InvokeVirtual(Register, ConstPoolIdx),
    InvokeStatic(Register, ConstPoolIdx),
    InvokeLambda(Register, ConstPoolIdx),
    InvokeGenericStatic(Register, ConstPoolIdx),
    InvokeGenericDirect(Register, ConstPoolIdx),

    NewObject(Register, ConstPoolIdx),
    NewObjectInitialized(Register, ConstPoolIdx),
    NewArray(Register, ConstPoolIdx, Register),
    NewTuple(Register, ConstPoolIdx),
    NewEnum(Register, ConstPoolIdx),
    NewStruct(Register, ConstPoolIdx),
    NewTraitObject(Register, ConstPoolIdx, Register),
    NewLambda(Register, ConstPoolIdx),

    ArrayLength(Register, Register),

    LoadArray(Register, Register, Register),
    StoreArray(Register, Register, Register),

    Ret(Register),
}
