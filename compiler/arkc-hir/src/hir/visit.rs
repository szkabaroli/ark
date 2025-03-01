use crate::hir::*;

pub trait Visitor: Sized {
    fn visit_file(&mut self, a: &File) {
        walk_file(self, a);
    }

    fn visit_fn_decl(&mut self, f: &FnDeclaration) {
        walk_fn_decl(self, f);
    }

    //fn visit_flow(&mut self, f: &Flow) {
    //    walk_flow(self, &f);
    //}

    fn visit_struct(&mut self, s: &Struct) {
        walk_struct(self, s);
    }

    fn visit_struct_field(&mut self, f: &StructField) {
        walk_struct_field(self, f);
    }

    fn visit_field_value(&mut self, f: &FieldValue) {
        walk_field_value(self, f);
    }

    // fn visit_param(&mut self, p: &Param) {
    //     walk_param(self, p);
    // }

    fn visit_type(&mut self, t: &TypeData) {
        walk_type(self, t);
    }

    fn visit_stmt(&mut self, s: &Statement) {
        walk_stmt(self, s);
    }

    fn visit_expr(&mut self, e: &Expr) {
        walk_expr(self, e);
    }

    //fn visit_import(&mut self, i: &Arc<Import>) {
    //    walk_import(self, i);
    //}
}

pub fn walk_file<V: Visitor>(v: &mut V, f: &File) {
    for e in &f.elements {
        walk_elem(v, e);
    }
}

pub fn walk_elem<V: Visitor>(v: &mut V, e: &Elem) {
    match e.kind {
        ElemKind::Function(ref f) => v.visit_fn_decl(f),
        //ElemKind::Flow(f) => v.visit_flow(f),
        //ElemData::Class(ref c) => v.visit_class(c),
        ElemKind::Struct(ref s) => v.visit_struct(s),
        ElemKind::Extern(_) => todo!(),
        ElemKind::Signature(_) => todo!(),
        //ElemData::Trait(ref t) => v.visit_trait(t),
        //ElemData::Impl(ref i) => v.visit_impl(i),
        //ElemData::Global(ref g) => v.visit_global(g),
        //ElemData::Const(ref c) => todo!(),
        //ElemData::Enum(ref e) => v.visit_enum(e),
        //ElemData::Alias(ref e) => v.visit_alias(e),
        //ElemData::Module(ref e) => v.visit_module(e),
        //ElemData::Import(ref i) => v.visit_import(i),
        //ElemData::Extern(ref stmt) => v.visit_extern(stmt),
        //ElemData::TypeAlias(ref node) => v.visit_type_alias(node),
    }
}

pub fn walk_fn_decl<V: Visitor>(v: &mut V, f: &FnDeclaration) {
    //for p in &f.signature.inputs {
    //    v.visit_param(p);
    //}

    //if let Some(ref ty) = f.signature.output {
    //    v.visit_type(ty);
    //}

    //if let Some(ref body) = f.body {
    //    v.visit_expr(body);
    //}
}

pub fn walk_struct<V: Visitor>(v: &mut V, s: &Struct) {
    for f in &s.fields {
        v.visit_struct_field(f);
    }
}

pub fn walk_struct_field<V: Visitor>(v: &mut V, f: &StructField) {
    v.visit_type(&f.ty);
}

pub fn walk_field_value<V: Visitor>(v: &mut V, f: &FieldValue) {
    v.visit_expr(&f.value);
}



//pub fn walk_flow<V: Visitor>(v: &mut V, f: &Flow) {
//    for p in &f.params {
//        v.visit_param(p);
//    }
//}

//pub fn walk_param<V: Visitor>(v: &mut V, p: &Param) {
//    v.visit_type(&p.data_type);
//}

//pub fn walk_import<V: Visitor>(_v: &mut V, _use: &Import) {
// nothing to do
//}

pub fn walk_type<V: Visitor>(v: &mut V, t: &TypeData) {
    match *t {
        TypeData::_Self => {}
        TypeData::Basic(_) => {}

        /*TypeData::Tuple(ref tuple) => {
            for ty in &tuple.subtypes {
                v.visit_type(ty);
            }
        }*/
        TypeData::Fn(ref func) => {
            for ty in func.params.iter() {
                v.visit_type(ty);
            }

            if let Some(ref ret) = func.ret {
                v.visit_type(&ret);
            }
        }

        /*TypeData::Path(..) => {}*/
        /*TypeData::Generic(ref ty) => {
            for ty in &ty.params {
                v.visit_type(ty);
            }
        }*/
        TypeData::Unknown { .. } => {}
    }
}

pub fn walk_stmt<V: Visitor>(v: &mut V, s: &Statement) {
    match *s.kind {
        StatementKind::Let(ref value) => {
            if let Some(ref ty) = value.ty {
                v.visit_type(ty);
            }

            if let Some(ref e) = value.expr {
                v.visit_expr(e);
            }
        }
        StatementKind::Expr(ref expr) => {
            v.visit_expr(expr);
        }
    }
}

pub fn walk_expr<V: Visitor>(v: &mut V, e: &Expr) {
    match *e.kind {
        //ExprKind::Un(ref value) => {
        //    v.visit_expr(&value.opnd);
        //}

        //ExprKind::Bin(_, ref lhs, ref rhs) => {
        //    v.visit_expr(lhs);
        //    v.visit_expr(rhs);
        //}
        ExprKind::Call(ref call) => {
            //v.visit_expr(&call.callee);

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
        //ExprKind::Path(ref path) => {
        //    v.visit_expr(&path.lhs);
        //    v.visit_expr(&path.rhs);
        //}
        ExprKind::Dot(ref value) => {
            //v.visit_expr(&value.lhs);
            //v.visit_expr(&value.rhs);
        }
        ExprKind::Block(ref value) => {
            for stmt in &value.stmts {
                v.visit_stmt(stmt);
            }

            if let Some(ref expr) = value.expr {
                v.visit_expr(expr);
            }
        }
        //ExprKind::Paren(ref value) => {
        //    v.visit_expr(&value.expr);
        //}
        ExprKind::Return(ref expr) => {
            if let Some(ref e) = expr {
                v.visit_expr(e);
            }
        }
        ExprKind::Struct(ref s) => {
            for f in &s.fields {
                v.visit_field_value(f);
            }
        }
        ExprKind::Lit(_) => {}
        ExprKind::Ident(_) => {} //ExprKind::Error { .. } => {}
    }
}
