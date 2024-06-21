use bytecode::{BytecodeType, Intrinsic, Register};
use parser::ast::{self, CmpOp};

use super::{CodeGen, DataDest};

pub(super) fn gen_expr(g: &mut CodeGen, expr: &ast::ExprKind, dest: DataDest) -> Register {
    match *expr {
        ast::ExprKind::Un(ref un) => g.visit_expr_un(un, dest),
        ast::ExprKind::Bin(ref bin) => g.visit_expr_bin(bin, dest),
        ast::ExprKind::Dot(ref field) => todo!(), //g.visit_expr_dot(field, dest),
        ast::ExprKind::Block(ref block) => g.visit_expr_block(block, dest),
        //ast::ExprData::If(ref expr) => g.visit_expr_if(expr, dest),
        //ast::ExprData::Template(ref template) => g.visit_expr_template(template, dest),
        //ast::ExprData::TypeParam(ref expr) => g.visit_expr_type_param(expr, dest),
        ast::ExprKind::Path(ref path) => todo!(), //g.visit_expr_path(path, dest),
        ast::ExprKind::LitChar(ref lit) => todo!(), //g.visit_expr_lit_char(lit, dest),
        ast::ExprKind::LitInt(ref lit) => g.visit_expr_lit_int(lit, dest, false),
        ast::ExprKind::LitFloat(ref lit) => g.visit_expr_lit_float(lit, dest),
        //ast::ExprData::LitStr(ref lit) => g.visit_expr_lit_string(lit, dest),
        ast::ExprKind::LitBool(ref lit) => g.visit_expr_lit_bool(lit, dest),
        ast::ExprKind::Ident(ref ident) => g.visit_expr_ident(ident, dest),
        ast::ExprKind::Call(ref call) => g.visit_expr_call(call, dest),
        //ast::ExprData::This(ref expr) => g.visit_expr_self(expr, dest),
        //ast::ExprData::Conv(ref conv) => g.visit_expr_conv(conv, dest),
        //ast::ExprData::Is(ref node) => g.visit_expr_is(node, dest),
        //ast::ExprData::Tuple(ref tuple) => g.visit_expr_tuple(tuple, dest),
        ast::ExprKind::Paren(ref paren) => gen_expr(g, &paren.expr, dest),
        //ast::ExprData::Match(ref expr) => gen_match(g, expr, dest),
        //ast::ExprData::Lambda(ref node) => g.visit_expr_lambda(node, dest),
        //ast::ExprData::For(ref node) => g.visit_expr_for(node, dest),
        //ast::ExprData::While(ref node) => g.visit_expr_while(node, dest),
        //ast::ExprData::Break(ref node) => g.visit_expr_break(node, dest),
        //ast::ExprData::Continue(ref node) => g.visit_expr_continue(node, dest),
        ast::ExprKind::Return(ref ret) => g.visit_expr_return(ret, dest),
        ast::ExprKind::Error { .. } => unreachable!(),
    }
}

pub(super) fn gen_expr_bin_cmp(
    g: &mut CodeGen,
    node: &ast::BinOp,
    cmp_op: CmpOp,
    dest: DataDest,
) -> Register {
    let lhs = gen_expr(g, &node.lhs, DataDest::Alloc);
    let rhs = gen_expr(g, &node.rhs, DataDest::Alloc);

    let result = if let Some(info) = g.get_intrinsic(node.id) {
        todo!()
        // gen_expr_bin_cmp_as_intrinsic(g, cmp_op, info.intrinsic, dest, lhs, rhs)
    } else {
        todo!()
        // gen_expr_bin_cmp_as_method(g, node, cmp_op, dest, lhs, rhs)
    };

    g.free_if_temp(lhs);
    g.free_if_temp(rhs);

    result
}
