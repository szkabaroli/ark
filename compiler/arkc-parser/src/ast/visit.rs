use crate::ast::*;

pub trait Visitor: Sized {
    fn visit_file(&mut self, a: &File) {
        walk_file(self, a);
    }

    fn visit_fct(&mut self, f: &Arc<Function>) {
        walk_fct(self, &f);
    }

    fn visit_flow(&mut self, f: &Arc<Flow>) {
        walk_flow(self, &f);
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

    fn visit_expr(&mut self, e: &ExprData) {
        walk_expr(self, e);
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
        //ElemData::Class(ref c) => v.visit_class(c),
        //ElemData::Struct(ref s) => v.visit_struct(s),
        //ElemData::Trait(ref t) => v.visit_trait(t),
        //ElemData::Impl(ref i) => v.visit_impl(i),
        //ElemData::Global(ref g) => v.visit_global(g),
        ElemData::Const(ref c) => todo!(),
        //ElemData::Enum(ref e) => v.visit_enum(e),
        //ElemData::Alias(ref e) => v.visit_alias(e),
        //ElemData::Module(ref e) => v.visit_module(e),
        //ElemData::Use(ref i) => v.visit_use(i),
        //ElemData::Extern(ref stmt) => v.visit_extern(stmt),
        //ElemData::TypeAlias(ref node) => v.visit_type_alias(node),
        ElemData::Error { .. } => {}
    }
}

pub fn walk_fct<V: Visitor>(v: &mut V, f: &Function) {
    for p in &f.params {
        v.visit_param(p);
    }

    if let Some(ref ty) = f.return_type {
        v.visit_type(ty);
    }

    if let Some(ref block) = f.block {
        v.visit_expr(block);
    }
}

pub fn walk_flow<V: Visitor>(v: &mut V, f: &Flow) {
    for p in &f.params {
        v.visit_param(p);
    }
}

pub fn walk_param<V: Visitor>(v: &mut V, p: &Param) {
    v.visit_type(&p.data_type);
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
        //StmtData::Let(ref value) => {
        //    if let Some(ref ty) = value.data_type {
        //        v.visit_type(ty);
        //    }

        //    if let Some(ref e) = value.expr {
        //        v.visit_expr(e);
        //    }
        //}
        StmtData::Expr(ref value) => {
            v.visit_expr(&value.expr);
        }
    }
}

pub fn walk_expr<V: Visitor>(v: &mut V, e: &ExprData) {
    match *e {
        ExprData::Un(ref value) => {
            v.visit_expr(&value.opnd);
        }

        ExprData::Bin(ref value) => {
            v.visit_expr(&value.lhs);
            v.visit_expr(&value.rhs);
        }

        ExprData::Call(ref call) => {
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
        ExprData::Path(ref path) => {
            v.visit_expr(&path.lhs);
            v.visit_expr(&path.rhs);
        }

        ExprData::Dot(ref value) => {
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
        ExprData::Block(ref value) => {
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
        ExprData::Paren(ref value) => {
            v.visit_expr(&value.expr);
        }

        //ExprData::Match(ref value) => {
        //    v.visit_expr(&value.expr);
        //}
        ExprData::Return(ref value) => {
            if let Some(ref e) = value.expr {
                v.visit_expr(e);
            }
        }

        //ExprData::Break(_) => {}
        //ExprData::Continue(_) => {}

        //ExprData::This(_) => {}
        ExprData::LitChar(_) => {}
        ExprData::LitInt(_) => {}
        ExprData::LitFloat(_) => {}
        //ExprData::LitStr(_) => {}
        ExprData::LitBool(_) => {}
        ExprData::Ident(_) => {}
        ExprData::Error { .. } => {}
    }
}
