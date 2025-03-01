use crate::ast::*;

pub trait Visitor: Sized {
    fn visit_file(&mut self, a: &File) {
        walk_file(self, a);
    }

    fn visit_fct(&mut self, f: &Arc<FnItem>) {
        walk_fct(self, &f);
    }

    fn visit_flow(&mut self, f: &Arc<FlowItem>) {
        walk_flow(self, &f);
    }

    fn visit_struct(&mut self, s: &Arc<StructItem>) {
        walk_struct(self, s);
    }

    fn visit_global(&mut self, s: &Arc<GlobalItem>) {
        walk_global(self, s);
    }

    fn visit_struct_field(&mut self, f: &StructField) {
        walk_struct_field(self, f);
    }

    fn visit_param(&mut self, p: &Param) {
        walk_param(self, p);
    }

    fn visit_type(&mut self, t: &TypeData) {
        walk_type(self, t);
    }

    fn visit_stmt(&mut self, s: &StmtData) {
        walk_stmt(self, s);
    }

    fn visit_expr(&mut self, e: &ExprKind) {
        walk_expr(self, e);
    }

    fn visit_import(&mut self, i: &Arc<Import>) {
        walk_import(self, i);
    }

    fn visit_interface(&mut self, i: &Arc<InterfaceItem>) {
        walk_interface(self, i);
    }
}

pub fn walk_file<V: Visitor>(v: &mut V, f: &File) {
    for e in &f.elements {
        walk_elem(v, e);
    }
}

pub fn walk_elem<V: Visitor>(v: &mut V, e: &ElemData) {
    match e {
        ElemData::Function(f) => v.visit_fct(f),
        ElemData::Flow(f) => v.visit_flow(f),
        ElemData::Interface(i) => v.visit_interface(i),
        //ElemData::Class(ref c) => v.visit_class(c),
        ElemData::Struct(ref s) => v.visit_struct(s),
        //ElemData::Trait(ref t) => v.visit_trait(t),
        //ElemData::Impl(ref i) => v.visit_impl(i),
        ElemData::Global(ref g) => v.visit_global(g),
        // ElemData::Const(ref c) => todo!(),
        //ElemData::Enum(ref e) => v.visit_enum(e),
        //ElemData::Alias(ref e) => v.visit_alias(e),
        //ElemData::Module(ref e) => v.visit_module(e),
        ElemData::Import(ref i) => v.visit_import(i),
        //ElemData::Extern(ref stmt) => v.visit_extern(stmt),
        //ElemData::TypeAlias(ref node) => v.visit_type_alias(node),
        ElemData::Error { .. } => {}
    }
}

pub fn walk_fct<V: Visitor>(v: &mut V, f: &FnItem) {
    for p in &f.signature.inputs {
        v.visit_param(p);
    }

    if let Some(ref ty) = f.signature.output {
        v.visit_type(ty);
    }

    if let Some(ref body) = f.body {
        v.visit_expr(body);
    }
}

pub fn walk_struct<V: Visitor>(v: &mut V, s: &StructItem) {
    for f in &s.fields {
        v.visit_struct_field(f);
    }
}

pub fn walk_interface<V: Visitor>(v: &mut V, s: &InterfaceItem) {
    // nothing to do (yet)
}

pub fn walk_global<V: Visitor>(v: &mut V, s: &GlobalItem) {
    // nothing to do (yet)
}

pub fn walk_struct_field<V: Visitor>(v: &mut V, f: &StructField) {
    v.visit_type(&f.ty);
}

pub fn walk_flow<V: Visitor>(v: &mut V, f: &FlowItem) {
    for p in &f.params {
        v.visit_param(p);
    }
}

pub fn walk_param<V: Visitor>(v: &mut V, p: &Param) {
    v.visit_type(&p.data_type);
}

pub fn walk_import<V: Visitor>(_v: &mut V, _use: &Arc<Import>) {
    // nothing to do
}

pub fn walk_type<V: Visitor>(v: &mut V, t: &TypeData) {
    match *t {
        TypeData::This(_) => {}
        TypeData::Basic(_) => {}

        /*TypeData::Tuple(ref tuple) => {
            for ty in &tuple.subtypes {
                v.visit_type(ty);
            }
        }*/

        /*TypeData::Lambda(ref fct) => {
            for ty in &fct.params {
                v.visit_type(ty);
            }

            if let Some(ref ret) = fct.ret {
                v.visit_type(&ret);
            }
        }*/

        /*TypeData::Path(..) => {}*/
        /*TypeData::Generic(ref ty) => {
            for ty in &ty.params {
                v.visit_type(ty);
            }
        }*/
        TypeData::Unknown { .. } => {}
    }
}

pub fn walk_stmt<V: Visitor>(v: &mut V, s: &StmtData) {
    match *s {
        StmtData::Let(ref value) => {
            if let Some(ref ty) = value.data_type {
                v.visit_type(ty);
            }

            if let Some(ref e) = value.expr {
                v.visit_expr(e);
            }
        }
        StmtData::Expr(ref value) => {
            v.visit_expr(&value.expr);
        }
    }
}

pub fn walk_expr<V: Visitor>(v: &mut V, e: &ExprKind) {
    match *e {
        ExprKind::Un(ref value) => {
            v.visit_expr(&value.opnd);
        }

        ExprKind::Bin(_, ref lhs, ref rhs) => {
            v.visit_expr(lhs);
            v.visit_expr(rhs);
        }

        ExprKind::Call(ref call) => {
            v.visit_expr(&call.callee);

            for arg in &call.args {
                v.visit_expr(arg);
            }
        }

        //ExprData::TypeParam(ref expr) => {
        //    v.visit_expr(&expr.callee);

        //    for arg in &expr.args {
        //        v.visit_type(arg);
        //    }
        //}
        ExprKind::Path(ref path) => {
            v.visit_expr(&path.lhs);
            v.visit_expr(&path.rhs);
        }

        ExprKind::Dot(ref value) => {
            v.visit_expr(&value.lhs);
            v.visit_expr(&value.rhs);
        }

        //ExprData::Conv(ref value) => {
        //    v.visit_expr(&value.object);
        //    v.visit_type(&value.data_type);
        //}

        //ExprData::Is(ref value) => {
        //    v.visit_expr(&value.value);
        //}

        //ExprData::Lambda(ref fct) => v.visit_fct(fct),
        ExprKind::Block(ref value) => {
            for stmt in &value.stmts {
                v.visit_stmt(stmt);
            }

            if let Some(ref expr) = value.expr {
                v.visit_expr(expr);
            }
        }

        //ExprData::Template(ref value) => {
        //    for part in &value.parts {
        //        v.visit_expr(part);
        //    }
        //}

        //ExprData::If(ref value) => {
        //    v.visit_expr(&value.cond);
        //    v.visit_expr(&value.then_block);

        //    if let Some(ref b) = value.else_block {
        //        v.visit_expr(b);
        //    }
        //}

        //ExprData::For(ref value) => {
        //    v.visit_expr(&value.expr);
        //    v.visit_expr(&value.block);
        //}

        //ExprData::While(ref value) => {
        //    v.visit_expr(&value.cond);
        //    v.visit_expr(&value.block);
        //}

        //ExprData::Tuple(ref value) => {
        //    for expr in &value.values {
        //        v.visit_expr(expr);
        //    }
        //}
        ExprKind::Paren(ref value) => {
            v.visit_expr(&value.expr);
        }

        //ExprData::Match(ref value) => {
        //    v.visit_expr(&value.expr);
        //}
        ExprKind::Return(ref value) => {
            if let Some(ref e) = value.expr {
                v.visit_expr(e);
            }
        }

        //ExprData::Break(_) => {}
        //ExprData::Continue(_) => {}

        //ExprData::This(_) => {}
        ExprKind::Struct(_) => {}
        ExprKind::Literal(_) => {}
        ExprKind::Ident(_) => {}
        ExprKind::Error { .. } => {}
    }
}
