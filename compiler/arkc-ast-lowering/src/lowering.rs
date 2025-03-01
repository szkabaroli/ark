use arkc_hir::hir::{self, NodeMap};
use arkc_hir::hir_map::HirMap;
use parser::ast::{self, ElemData};
use std::collections::BTreeMap;

pub struct LoweringContext {
    hir_map: HirMap,
    elements: Vec<hir::Elem>,
    bodies: BTreeMap<hir::FnBodyId, hir::FnBody>,
    // signatures: ResolutionMap<NodeId>, // FnHirId -> SignatureHirId
}

impl LoweringContext {
    pub fn new() -> Self {
        Self {
            elements: Vec::new(),
            hir_map: HirMap::new(),
            bodies: BTreeMap::new(),
        }
    }

    pub fn lower_file(&mut self, root: &ast::File) -> hir::File {
        root.elements.iter().for_each(|t| self.lower_element(t));

        let hir = hir::File {
            //arena: Arena::new(),
            //type_envs: Envs::default(),
            hir_map: Some(self.hir_map.clone()),
            bodies: self.bodies.clone(),
            func_analysis: NodeMap::new(),
            node_types: BTreeMap::new(),
            elements: self.elements.clone(),
        };

        self.elements.clear();
        self.bodies.clear();

        // hir.arena = hir::collect_arena(&hir);

        hir
    }

    pub fn lower_element(&mut self, element: &ast::ElemData) {
        match &element {
            ElemData::Function(f) => {
                let elem = hir::Elem {
                    kind: hir::ElemKind::Function(self.lower_fn_decl(f)),
                };

                self.elements.push(elem);
            }
            ElemData::Struct(s) => {
                let elem = hir::Elem {
                    kind: hir::ElemKind::Struct(self.lower_struct_item(s)),
                };

                self.elements.push(elem);
            }
            ElemData::Flow(_) => todo!(),
            ElemData::Global(_) => todo!(),
            ElemData::Interface(_) => todo!(),
            ElemData::Import(_) => todo!(),
            ElemData::Error { .. } => panic!("unexpected ast error"),
        }
    }

    pub fn lower_type(&mut self, ty: &ast::TypeData) -> hir::TypeData {
        match ty {
            ast::TypeData::This(_) => todo!(),
            ast::TypeData::Basic(basic) => hir::TypeData::Basic(hir::BasicType {
                path: hir::Path {
                    names: basic
                        .path
                        .names
                        .iter()
                        .map(|n| self.lower_identifier(n))
                        .collect(),
                },
            }),
            ast::TypeData::Unknown { id, span } => todo!(),
        }
    }

    pub fn lower_struct_item(&mut self, stru: &ast::StructItem) -> hir::Struct {
        let hir_id = self.hir_map.next_hir_id(stru.id);
        let ident = self.lower_identifier(&stru.name.as_ref().unwrap());
        let mut fields = vec![];

        for field in stru.fields.iter() {
            let hir_id = self.hir_map.next_hir_id(field.id);
            let name = self.lower_identifier(&stru.name.as_ref().unwrap());

            fields.push(hir::StructField {
                hir_id,
                name,
                ty: self.lower_type(&field.ty),
            })
        }

        let is_internal = match ident.name.as_str() {
            "int32" | "bool" | "char" | "int64" | "float64" => true,
            _ => false,
        };

        hir::Struct {
            name: ident,
            fields,
            hir_id,
            primitive_ty: None,
            is_internal,
        }
    }

    pub fn lower_fn_decl(&mut self, func: &ast::FnItem) -> hir::FnDeclaration {
        let body_id = self.hir_map.next_body_id();
        let hir_id = self.hir_map.next_hir_id(func.id);
        let ident = if let Some(ref name) = func.name {
            Some(self.lower_identifier(name))
        } else {
            None
        };

        let signature = self.lower_signature(&func.signature);

        let body = hir::FnBody {
            id: body_id.clone(),
            fn_id: hir_id.clone(),
            name: ident.clone(),
            mangled_name: None,
            body: {
                let expr = func.body.as_ref().unwrap();
                match **expr {
                    ast::ExprKind::Block(ref block) => self.lower_block_expr(block),
                    _ => panic!(),
                }
            },
        };
        self.bodies.insert(body_id.clone(), body);

        hir::FnDeclaration {
            name: ident,
            mangled_name: None,
            analysis: None,
            //arguments: f
            //    .arguments
            //    .iter()
            //    .map(|arg| self.lower_argument_decl(arg))
            //    .collect(),
            body_id,
            signature,
            hir_id,
        }
    }

    pub fn lower_expression(&mut self, expr: &ast::ExprKind) -> hir::Expr {
        match &expr {
            ast::ExprKind::Return(ret) => hir::Expr {
                hir_id: self.hir_map.next_hir_id(ret.id),
                kind: Box::from(hir::ExprKind::Return(
                    ret.expr.as_ref().map(|v| self.lower_expression(&*v)),
                )),
            },
            ast::ExprKind::Struct(stru) => hir::Expr {
                hir_id: self.hir_map.next_hir_id(stru.id),
                kind: Box::from(hir::ExprKind::Struct(self.lower_expr_struct(stru))),
            },
            ast::ExprKind::Block(block) => hir::Expr {
                hir_id: self.hir_map.next_hir_id(block.id),
                kind: Box::from(hir::ExprKind::Block(self.lower_block_expr(block))),
            },
            ast::ExprKind::Ident(ident) => hir::Expr {
                hir_id: self.hir_map.next_hir_id(ident.id),
                kind: Box::from(hir::ExprKind::Ident(self.lower_ident_expr(ident))),
            },
            ast::ExprKind::Literal(lit) => hir::Expr {
                hir_id: self.hir_map.next_hir_id(lit.id),
                kind: Box::from(hir::ExprKind::Lit(self.lower_literal(lit))),
            },
            ast::ExprKind::Call(call) => hir::Expr {
                hir_id: self.hir_map.next_hir_id(call.id),
                kind: Box::from(hir::ExprKind::Call(self.lower_call(call))),
            },
            _ => unimplemented!(),
        }
    }

    fn lower_expr_struct(&mut self, stru: &ast::ExprStruct) -> hir::ExprStruct {
        let mut fields = Vec::new();

        for field in stru.field_values.iter() {
            fields.push(hir::FieldValue {
                hir_id: self.hir_map.next_hir_id(field.id),
                name: self.lower_identifier(field.name.as_ref().unwrap()),
                value: self.lower_expression(&field.value),
            })
        }

        hir::ExprStruct {
            hir_id: self.hir_map.next_hir_id(stru.id),
            name: self.lower_expression(&stru.name),
            fields,
        }
    }

    pub fn lower_block_expr(&mut self, block: &ast::ExprBlockType) -> hir::Block {
        hir::Block {
            expr: block.expr.as_ref().map(|v| self.lower_expression(&*v)),
            stmts: block
                .stmts
                .iter()
                .map(|stmt| self.lower_statement(stmt))
                .collect(),
        }
    }

    pub fn lower_pattern(&mut self, pattern: &ast::PatternKind) -> hir::PatternKind {
        match pattern {
            ast::PatternKind::Ident(let_ident) => {
                hir::PatternKind::Ident(self.lower_identifier(let_ident.name.as_ref().unwrap()))
            }
        }
    }

    pub fn lower_let(&mut self, stmt: &ast::StmtLetType) -> hir::LetBinding {
        hir::LetBinding {
            hir_id: self.hir_map.next_hir_id(stmt.id),
            pattern: self.lower_pattern(&stmt.pattern),
            ty: stmt.data_type.as_ref().map(|ty| self.lower_type(ty)),
            expr: stmt.expr.as_ref().map(|expr| self.lower_expression(expr)),
        }
    }

    pub fn lower_statement(&mut self, stmt: &ast::StmtData) -> hir::Statement {
        hir::Statement {
            kind: match stmt {
                ast::StmtData::Expr(ref stmt) => {
                    Box::new(hir::StatementKind::Expr(self.lower_expression(&stmt.expr)))
                }
                ast::StmtData::Let(ref stmt) => {
                    Box::new(hir::StatementKind::Let(self.lower_let(&stmt)))
                }
            },
        }
    }

    pub fn lower_signature(&mut self, sig: &ast::FnSignature) -> hir::FnType {
        let return_type = if let Some(ref output) = sig.output {
            self.lower_type(output)
        } else {
            hir::TypeData::Unknown
        };

        hir::FnType {
            params: vec![],
            ret: Some(Box::from(return_type)),
        }
    }

    pub fn lower_identifier(&mut self, ident: &ast::Ident) -> hir::Identifier {
        let hir_id = self.hir_map.next_hir_id(ident.id);

        hir::Identifier {
            hir_id,
            name: ident.name_as_string.clone(),
        }
    }

    fn lower_ident_expr(&mut self, ident: &ast::ExprIdentType) -> hir::Identifier {
        hir::Identifier {
            hir_id: self.hir_map.next_hir_id(ident.id),
            name: ident.name.clone(),
        }
    }

    fn lower_literal(&mut self, lit: &ast::Literal) -> hir::Literal {
        let kind = match &lit.kind {
            ast::LitKind::Unit => hir::LiteralKind::Unit,
            ast::LitKind::Bool(val) => hir::LiteralKind::Bool(*val),
            ast::LitKind::Char(val) => hir::LiteralKind::Char(*val),
            ast::LitKind::Int(val) => hir::LiteralKind::Int(val.clone()),
            ast::LitKind::Float(val) => hir::LiteralKind::Float(val.clone()),
            ast::LitKind::Struct => todo!(),
        };

        hir::Literal {
            hir_id: self.hir_map.next_hir_id(lit.id),
            kind,
        }
    }
    
    fn lower_call(&mut self, call: &ast::Call) -> hir::FnCall {
        hir::FnCall {
            hir_id: self.hir_map.next_hir_id(call.id),
            callee: self.lower_expression(&call.callee),
            args: call.args.iter().map(|arg| self.lower_expression(&arg),).collect(),
        }
    }
}
