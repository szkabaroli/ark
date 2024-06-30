use ark_vm::builder::Builder;
use ark_vm::code::Code;
use ark_vm::Operand;
use arkc_hir::hir;

pub fn codegen(hir: &hir::File) -> Result<(), ()> {
    let mut builder = Builder::new();
    let mut codegen_ctx = CodegenContext::new(&hir);

    if codegen_ctx.codegen_hir(&hir, &builder).is_err() {
        panic!("LLVM codegen failed");
    }

    let condition = Operand::Int(0);
    let end_label = builder.create_named_label("if.end");
    let if_true = builder.create_named_label("if.then");

    // Push our condition operand into the stack
    builder.push(condition);
    builder.store(Operand::UInt16(0u16));
    builder.load(Operand::UInt16(0u16));

    // Add our `jump_if` instruction and tell it to jump to the `if_true` label.
    builder.jump_if(if_true);

    // These instructions will be skipped if the operand is true:
    // * Push "it was false" onto the stack.
    // * Unconditionally jump to the end of the program.
    builder.push(Operand::Int(1));
    builder.jump(end_label);

    // This instruction will be skipped of the operand is false:
    // * Push "it was true" onto the stack.
    builder.label(if_true);
    builder.push(Operand::Int(2));

    // Points at the end of the program, which will cause the machine to halt.
    builder.label(end_label);
    builder.ret();

    println!("{:#?}", builder);
    let code = Code::from(builder);
    println!("{:?}", code);

    let mut f = std::fs::File::create("./test.aot").unwrap();
    let _ = bincode::encode_into_std_write(code, &mut f, bincode::config::standard());

    Ok(())
}

/// The (code) CodegenContext provides all the needed context for generating VM Bytecode
/// while walking the HIR.
pub struct CodegenContext<'a> {
    pub hir: &'a hir::File,
    //pub scopes: Scopes<HirId, BasicValueEnum<'a>>,
    // pub cur_func: Option<FunctionValue<'a>>,
}

impl<'a> CodegenContext<'a> {
    pub fn new(hir: &'a hir::File) -> Self {
        Self { hir }
    }

    pub fn codegen_hir(&mut self, root: &'a hir::File, builder: &'a Builder) -> Result<(), ()> {
        for item in &root.elements {
            match &item.kind {
                hir::ElemKind::Extern(_) => todo!(),
                hir::ElemKind::Signature(_) => todo!(),
                hir::ElemKind::Function(_) => (),
                hir::ElemKind::Struct(_) => todo!(),
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
        Ok(())
    }
}
