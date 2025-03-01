use std::{
    num::ParseIntError,
    sync::{atomic::AtomicUsize, Arc},
};

use crate::{
    ast::{self, *},
    green::{GreenTreeBuilder, Marker},
    lex,
    source_file::{SourceFile, SourceFileId},
    token::{
        TokenSet, ELEM_FIRST, EMPTY, EXPRESSION_FIRST, FIELD_FIRST, FIELD_VALUE_FIRST,
        FLOW_EXPRESSION_FIRST, IMPORT_PATH_ATOM_FIRST, MODIFIER_FIRST, PARAM_LIST_RS,
    },
    NodeId, ParseError, ParseErrorWithLocation, Span,
    TokenKind::{self, *},
};

enum StmtOrExpr {
    Stmt(ast::Stmt),
    Expr(ast::Expr),
}

enum FlowStmtOrExpr {
    Stmt(ast::FlowStmt),
    Expr(ast::FlowExpr),
}

enum FunctionOrStmt {
    Func(ast::FnItem),
    Expr(ast::Const),
}

pub struct NodeIdGenerator(AtomicUsize);

impl NodeIdGenerator {
    pub fn new() -> Self {
        NodeIdGenerator(AtomicUsize::new(0))
    }

    fn new_node_id(&self) -> NodeId {
        let value = self.0.load(std::sync::atomic::Ordering::Relaxed);
        self.0.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        NodeId(value)
    }
}

fn parse_string_to_i64(input: &str) -> Result<i64, ParseIntError> {
    input.parse::<i64>()
}

pub struct Parser {
    source_id: SourceFileId,
    tokens: Vec<TokenKind>,
    token_widths: Vec<u32>,
    token_idx: usize,
    id_gen: Arc<NodeIdGenerator>,
    content: Arc<String>,
    errors: Vec<ParseErrorWithLocation>,
    nodes: Vec<(usize, u32)>,
    offset: u32,
    builder: GreenTreeBuilder,
}

impl Parser {
    pub fn from_source(id_gen: Arc<NodeIdGenerator>, source: &SourceFile) -> Parser {
        Parser::common_init(
            id_gen,
            source.id.get().unwrap().clone(),
            source.content.clone(),
        )
    }

    fn common_init(
        id_gen: Arc<NodeIdGenerator>,
        source_id: SourceFileId,
        content: Arc<String>,
    ) -> Parser {
        let result = lex(source_id, &*content);

        Parser {
            source_id,
            tokens: result.tokens,
            token_widths: result.widths,
            token_idx: 0,
            id_gen,
            offset: 0,
            content,
            errors: result.errors,
            nodes: Vec::new(),
            builder: GreenTreeBuilder::new(),
        }
    }

    pub fn parse(mut self) -> (Arc<ast::File>, Vec<ParseErrorWithLocation>) {
        let ast_file = self.parse_file();
        assert!(self.nodes.is_empty());

        let tree = self.builder.create_tree();
        assert_eq!(tree.len(), self.content.len() as u32);

        (Arc::new(ast_file), self.errors)
    }

    fn parse_file(&mut self) -> ast::File {
        self.builder.start_node();
        self.skip_trivia();
        let mut elements = vec![];

        while !self.is_eof() {
            elements.push(self.parse_element());
        }

        let green = self.builder.finish_node(SOURCE_FILE);
        ast::File {
            source_id: self.source_id,
            green,
            elements,
        }
    }

    fn parse_modifiers(&mut self) -> Option<ModifierList> {
        if self.is_set(MODIFIER_FIRST) {
            self.start_node();
            let marker = self.builder.create_marker();
            let mut modifiers: Vec<Modifier> = Vec::new();

            while self.is_set(MODIFIER_FIRST) {
                modifiers.push(self.parse_modifier());
            }

            assert!(!modifiers.is_empty());
            let green = self.builder.finish_node_starting_at(MODIFIERS, marker);

            Some(ModifierList {
                id: self.new_node_id(),
                span: self.finish_node(),
                green,
                modifiers,
            })
        } else {
            None
        }
    }

    fn parse_modifier(&mut self) -> Modifier {
        self.start_node();
        let m = self.builder.create_marker();

        if self.eat(PUB_KW) {
            // done
        } else if self.eat(STATIC_KW) {
            // done
        } else {
            self.assert(AT);
            self.expect_identifier();
        }

        let green = self.builder.finish_node_starting_at(MODIFIER, m);

        Modifier {
            id: self.new_node_id(),
            span: self.finish_node(),
            green,
        }
    }

    fn parse_element(&mut self) -> Elem {
        self.builder.start_node();

        let modifiers = self.parse_modifiers();

        match self.current() {
            FN_KW => {
                let fct = self.parse_function(None);
                Arc::new(ElemData::Function(fct))
            }
            FLOW_KW => {
                let flow = self.parse_flow(None);
                Arc::new(ElemData::Flow(flow))
            }
            STRUCT_KW => {
                let struc = self.parse_struct();
                Arc::new(ElemData::Struct(struc))
            }
            IDENTIFIER => Arc::new(self.parse_function_or_global()),

            /*CLASS_KW => {
                let class = self.parse_class(modifiers);
                Arc::new(ElemData::Class(class))
            }*/

            /*TRAIT_KW => {
                let trait_ = self.parse_trait(modifiers);
                Arc::new(ElemData::Trait(trait_))
            }*/

            /*IMPL_KW => {
                let impl_ = self.parse_impl(modifiers);
                Arc::new(ElemData::Impl(impl_))
            }*/

            /*LET_KW => {
                let global = self.parse_global(modifiers);
                Arc::new(ElemData::Global(global))
            }*/

            /*CONST_KW => {
                let const_ = self.parse_const(modifiers);
                Arc::new(ElemData::Const(const_))
            }*/

            /*ENUM_KW => {
                let enum_ = self.parse_enum(modifiers);
                Arc::new(ElemData::Enum(enum_))
            }*/

            /*MOD_KW => {
                let module = self.parse_module(modifiers);
                Arc::new(ElemData::Module(module))
            }*/
            IMPORT_KW => {
                let import_stmt = self.parse_import(modifiers);
                Arc::new(ElemData::Import(import_stmt))
            }

            /*EXTERN_KW => {
                let extern_stmt = self.parse_extern(modifiers);
                Arc::new(ElemData::Extern(extern_stmt))
            }*/

            /*TYPE_KW => {
                let type_alias = self.parse_type_alias(modifiers);
                Arc::new(ElemData::TypeAlias(type_alias))
            }*/
            _ => {
                assert!(!ELEM_FIRST.contains(self.current()));
                let span = self.current_span();
                self.report_error_at(ParseError::ExpectedElement, span);
                self.advance();
                self.builder.finish_node(ERROR);

                Arc::new(ElemData::Error {
                    id: self.new_node_id(),
                    span,
                })
            }
        }
    }

    fn current(&self) -> TokenKind {
        self.nth(0)
    }

    fn nth(&self, idx: usize) -> TokenKind {
        if self.token_idx + idx < self.tokens.len() {
            self.tokens[self.token_idx + idx]
        } else {
            EOF
        }
    }

    fn current_span(&self) -> Span {
        if self.token_idx < self.tokens.len() {
            let length = self.token_widths[self.token_idx];
            Span::new(self.source_id, self.offset, length)
        } else {
            Span::at(self.source_id, self.offset)
        }
    }

    fn is_set(&self, set: TokenSet) -> bool {
        set.contains(self.current())
    }

    fn is(&self, kind: TokenKind) -> bool {
        self.current() == kind
    }

    fn report_error(&mut self, msg: ParseError) {
        self.report_error_at(msg, self.current_span());
    }

    fn report_error_at(&mut self, msg: ParseError, span: Span) {
        self.errors.push(ParseErrorWithLocation::new(span, msg));
    }

    fn assert(&mut self, kind: TokenKind) {
        assert!(self.eat(kind), "assert {:?}, {:?}", kind, self.current());
    }

    fn expect_identifier(&mut self) -> Option<Ident> {
        let span = self.current_span();

        if self.is(IDENTIFIER) {
            self.assert(IDENTIFIER);
            let value = self.source_span(span);

            Some(Arc::new(IdentData {
                id: self.new_node_id(),
                span,
                name_as_string: value,
            }))
        } else {
            self.report_error_at(ParseError::ExpectedIdentifier, span);
            None
        }
    }

    fn expect(&mut self, kind: TokenKind) -> bool {
        debug_assert!(token_name(kind).is_some());

        if self.eat(kind) {
            true
        } else {
            let kind = token_name(kind).expect("missing name");
            self.report_error(ParseError::ExpectedToken(kind.into()));
            false
        }
    }

    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.current() == kind {
            self.advance();
            true
        } else {
            false
        }
    }

    fn new_node_id(&mut self) -> NodeId {
        self.id_gen.new_node_id()
    }

    fn start_node(&mut self) {
        self.nodes.push((self.token_idx, self.offset));
    }

    fn finish_node(&mut self) -> Span {
        let (start_token, start_offset) = self.nodes.pop().expect("missing node start");

        let mut end_token = self.token_idx - 1;
        assert!(end_token < self.tokens.len());
        let mut end_offset = self.offset;

        while end_token > start_token {
            if !self.tokens[end_token].is_trivia() {
                break;
            }

            end_offset -= self.token_widths[end_token];
            end_token -= 1;
        }

        Span::new(self.source_id, start_offset, end_offset - start_offset)
    }

    fn is_eof(&self) -> bool {
        self.current() == EOF
    }

    fn advance(&mut self) {
        self.raw_advance();
        self.skip_trivia();
    }

    fn skip_trivia(&mut self) {
        while self.current().is_trivia() {
            self.raw_advance();
        }
    }

    fn raw_advance(&mut self) {
        if self.token_idx < self.tokens.len() {
            let kind = self.current();
            let value = self.source_span(self.current_span());
            let len = self.token_widths[self.token_idx];
            self.offset += len;
            debug_assert!(kind <= EOF);
            self.builder.token(kind, value);
            self.token_idx += 1;
        }
    }

    fn parse_import(&mut self, modifiers: Option<ModifierList>) -> Arc<Import> {
        self.start_node();
        self.assert(IMPORT_KW);
        let path = self.parse_import_path();
        self.expect(SEMICOLON);

        let green = self.builder.finish_node(IMPORT);

        Arc::new(Import {
            id: self.new_node_id(),
            span: self.finish_node(),
            green,
            modifiers,
            path,
        })
    }

    fn parse_import_path(&mut self) -> Arc<ImportPath> {
        self.start_node();
        self.builder.start_node();
        let mut path = Vec::new();

        let target = if self.is_set(IMPORT_PATH_ATOM_FIRST) {
            path.push(self.parse_import_atom());

            while self.is(COLON_COLON) && self.nth_is_set(1, IMPORT_PATH_ATOM_FIRST) {
                self.advance();
                path.push(self.parse_import_atom());
            }

            ImportPathDescriptor::Default
        } else {
            self.report_error(ParseError::ExpectedUsePath);
            ImportPathDescriptor::Error
        };

        let green = self.builder.finish_node(IMPORT_PATH);

        Arc::new(ImportPath {
            id: self.new_node_id(),
            span: self.finish_node(),
            green,
            path,
            target,
        })
    }

    fn parse_import_atom(&mut self) -> ImportAtom {
        assert!(self.is_set(IMPORT_PATH_ATOM_FIRST));
        self.start_node();
        self.builder.start_node();

        let value = if self.eat(PACKAGE_KW) {
            ImportPathComponentValue::Package
        } else {
            let name = self.expect_identifier();
            if let Some(name) = name {
                ImportPathComponentValue::Name(name)
            } else {
                ImportPathComponentValue::Error
            }
        };

        let green = self.builder.finish_node(IMPORT_COMPONENT);

        ImportAtom {
            green,
            span: self.finish_node(),
            value,
        }
    }

    fn parse_struct_field(&mut self) -> StructField {
        self.start_node();
        self.builder.start_node();

        let ident = self.expect_identifier();

        self.expect(COLON);
        let ty = self.parse_type();

        let green = self.builder.finish_node(STRUCT_FIELD);

        StructField {
            id: self.new_node_id(),
            span: self.finish_node(),
            green,
            name: ident,
            ty,
        }
    }

    fn parse_struct(&mut self) -> Arc<ast::StructItem> {
        self.start_node();
        self.assert(STRUCT_KW);
        let ident = self.expect_identifier();

        let fields = if self.is(L_PAREN) {
            self.parse_list(
                L_PAREN,
                COMMA,
                R_PAREN,
                ELEM_FIRST,
                ParseError::ExpectedField,
                LIST,
                |p| {
                    if p.is_set(FIELD_FIRST) {
                        Some(p.parse_struct_field())
                    } else {
                        None
                    }
                },
            )
        } else if self.is(L_BRACE) {
            self.parse_list(
                L_BRACE,
                COMMA,
                R_BRACE,
                ELEM_FIRST,
                ParseError::ExpectedField,
                LIST,
                |p| {
                    if p.is_set(FIELD_FIRST) {
                        Some(p.parse_struct_field())
                    } else {
                        None
                    }
                },
            )
        } else {
            Vec::new()
        };

        let green = self.builder.finish_node(STRUCT);

        Arc::new(StructItem {
            id: self.new_node_id(),
            name: ident,
            green,
            span: self.finish_node(),
            fields,
        })
    }

    fn parse_flow(&mut self, modifiers: Option<ModifierList>) -> Arc<ast::FlowItem> {
        let start = self.current_span().start();
        self.start_node();
        self.assert(FLOW_KW);
        let name = self.expect_identifier();
        let params = self.parse_function_params();
        let return_type = self.parse_function_type();
        let block = self.parse_flow_block();

        let declaration_span = self.span_from(start);

        let green = self.builder.finish_node(FLOW);

        Arc::new(ast::FlowItem {
            id: self.new_node_id(),
            name,
            declaration_span,
            span: self.finish_node(),
            params,
            return_type,
            block,
            green,
        })
    }

    fn parse_function_or_global(&mut self) -> ElemData {
        let start = self.current_span().start();
        self.start_node();

        let name = self.expect_identifier();
        self.skip_trivia();
        self.expect(COLON);
        self.skip_trivia();

        if self.is(FN_KW) {
            self.eat(FN_KW);
            let params = self.parse_function_params();
            let return_type = self.parse_function_type();
            let declaration_span = self.span_from(start);
            let body = self.parse_function_body();

            let green = self.builder.finish_node(FN);
            
            ElemData::Function(Arc::new(ast::FnItem {
                id: self.new_node_id(),
                green,
                name,
                declaration_span,
                span: self.finish_node(),
                signature: ast::FnSignature {
                    inputs: params,
                    output: return_type,
                    span: declaration_span,
                },
                body,
            }))
        } else {
            ElemData::Error {
                id: self.new_node_id(),
                span: self.finish_node(),
            }
        }
    }

    fn parse_function(&mut self, modifiers: Option<ModifierList>) -> Arc<ast::FnItem> {
        let start = self.current_span().start();
        self.start_node();
        self.assert(FN_KW);
        let name = self.expect_identifier();
        let params = self.parse_function_params();
        let return_type = self.parse_function_type();
        let declaration_span = self.span_from(start);
        let body = self.parse_function_body();

        let green = self.builder.finish_node(FN);

        Arc::new(ast::FnItem {
            id: self.new_node_id(),
            green,
            name,
            declaration_span,
            span: self.finish_node(),
            signature: ast::FnSignature {
                inputs: params,
                output: return_type,
                span: declaration_span,
            },
            body,
        })
    }

    fn parse_function_param_wrapper(&mut self) -> Option<Param> {
        if self.is(IDENTIFIER) {
            Some(self.parse_function_param())
        } else {
            None
        }
    }

    fn parse_function_params(&mut self) -> Vec<Param> {
        if self.is(L_PAREN) {
            self.parse_list(
                L_PAREN,
                COMMA,
                R_PAREN,
                PARAM_LIST_RS,
                ParseError::ExpectedParam,
                PARAM_LIST,
                |p| p.parse_function_param_wrapper(),
            )
        } else {
            self.report_error(ParseError::ExpectedParams);
            Vec::new()
        }
    }

    fn parse_function_param(&mut self) -> Param {
        self.start_node();
        //let mutable = self.eat(MUT_KW);
        let name = self.expect_identifier();

        self.expect(COLON);

        let data_type = self.parse_type();

        let variadic = self.eat(DOT_DOT_DOT);

        Param {
            id: self.new_node_id(),
            variadic,
            name,
            span: self.finish_node(),
            mutable: false,
            data_type,
        }
    }

    fn parse_function_type(&mut self) -> Option<Type> {
        if self.eat(ARROW) {
            let ty = self.parse_type();
            Some(ty)
        } else {
            None
        }
    }

    fn parse_path(&mut self) -> Path {
        self.start_node();
        let mut names = Vec::new();
        let name = self.expect_identifier();
        if let Some(name) = name {
            names.push(name);
        } else {
            // Advance by token to avoid infinite loop in `parse_match`.
            self.advance();
        }

        while self.eat(COLON_COLON) {
            let name = self.expect_identifier();
            if let Some(name) = name {
                names.push(name);
            } else {
                break;
            }
        }

        Arc::new(PathData {
            id: self.new_node_id(),
            span: self.finish_node(),
            names,
        })
    }

    fn parse_type_wrapper(&mut self) -> Option<Type> {
        if self.is(IDENTIFIER) || self.is(L_PAREN) {
            Some(self.parse_type())
        } else {
            None
        }
    }

    fn parse_type(&mut self) -> Type {
        self.builder.start_node();
        match self.current() {
            /*UPCASE_SELF_KW => {
                let span = self.current_span();
                self.assert(UPCASE_SELF_KW);
                let green = self.builder.finish_node(SELF_TYPE);
                Arc::new(TypeData::create_self(self.new_node_id(), span, green))
            }*/
            IDENTIFIER => {
                self.start_node();
                let path = self.parse_path();

                let params = if self.is(L_BRACKET) {
                    self.parse_list(
                        L_BRACKET,
                        COMMA,
                        R_BRACKET,
                        EMPTY,
                        ParseError::ExpectedType,
                        LIST,
                        |p| p.parse_type_wrapper(),
                    )
                } else {
                    Vec::new()
                };

                let green = self.builder.finish_node(REGULAR_TYPE);

                Arc::new(TypeData::create_basic(
                    self.new_node_id(),
                    self.finish_node(),
                    green,
                    path,
                    params,
                ))
            }

            /*L_PAREN => {
                self.start_node();
                let subtypes = self.parse_list(
                    L_PAREN,
                    COMMA,
                    R_PAREN,
                    TYPE_PARAM_RS,
                    ParseError::ExpectedType,
                    LIST,
                    |p| p.parse_type_wrapper(),
                );

                if self.eat(COLON) {
                    let ret = self.parse_type();

                    let green = self.builder.finish_node(LAMBDA_TYPE);

                    Arc::new(TypeData::create_fct(
                        self.new_node_id(),
                        self.finish_node(),
                        green,
                        subtypes,
                        Some(ret),
                    ))
                } else {
                    let green = self.builder.finish_node(TUPLE_TYPE);

                    Arc::new(TypeData::create_tuple(
                        self.new_node_id(),
                        self.finish_node(),
                        green,
                        subtypes,
                    ))
                }
            }*/
            _ => {
                let span = self.current_span();
                self.report_error(ParseError::ExpectedType);
                self.builder.abandon_node();
                Arc::new(TypeData::Unknown {
                    id: self.new_node_id(),
                    span,
                })
            }
        }
    }

    fn parse_function_body(&mut self) -> Option<Expr> {
        if self.eat(SEMICOLON) {
            None
        } else {
            let block = Arc::from(ExprKind::Block(self.parse_block()));
            Some(block)
        }
    }

    fn parse_flow_block(&mut self) -> Option<FlowExpr> {
        if self.eat(SEMICOLON) {
            None
        } else {
            let block = Arc::from(FlowExprData::FlowBlock(self.parse_block2()));
            Some(block)
        }
    }

    fn parse_block2(&mut self) -> ast::FlowExprBlockType {
        self.start_node();
        self.builder.start_node();
        let mut stmts = vec![];
        let mut expr = None;

        if self.expect(L_BRACE) {
            while !self.is(R_BRACE) && !self.is_eof() {
                let stmt_or_expr = self.parse_flow_stmt_or_expr();

                match stmt_or_expr {
                    FlowStmtOrExpr::Stmt(stmt) => stmts.push(stmt),
                    FlowStmtOrExpr::Expr(curr_expr) => {
                        if curr_expr.needs_semicolon() {
                            expr = Some(curr_expr);
                            break;
                        } else if !self.is(R_BRACE) {
                            stmts.push(Arc::new(FlowStmtData::create_expr(
                                self.new_node_id(),
                                curr_expr.span(),
                                curr_expr,
                            )));
                        } else {
                            expr = Some(curr_expr);
                        }
                    }
                }
            }

            self.expect(R_BRACE);
        }

        let green = self.builder.finish_node(BLOCK_EXPR);

        ast::FlowExprBlockType {
            id: self.new_node_id(),
            span: self.finish_node(),
            green,
            stmts,
            expr,
        }
    }

    fn parse_block(&mut self) -> ast::ExprBlockType {
        self.start_node();
        self.builder.start_node();
        let mut stmts = vec![];
        let mut expr = None;

        if self.expect(L_BRACE) {
            while !self.is(R_BRACE) && !self.is_eof() {
                let stmt_or_expr = self.parse_function_stmt_or_expr();

                match stmt_or_expr {
                    StmtOrExpr::Stmt(stmt) => stmts.push(stmt),
                    StmtOrExpr::Expr(curr_expr) => {
                        if curr_expr.needs_semicolon() {
                            expr = Some(curr_expr);
                            break;
                        } else if !self.is(R_BRACE) {
                            stmts.push(Arc::new(StmtData::create_expr(
                                self.new_node_id(),
                                curr_expr.span(),
                                curr_expr,
                            )));
                        } else {
                            expr = Some(curr_expr);
                        }
                    }
                }
            }

            self.expect(R_BRACE);
        }

        let green = self.builder.finish_node(BLOCK_EXPR);

        ast::ExprBlockType {
            id: self.new_node_id(),
            span: self.finish_node(),
            green,
            stmts,
            expr,
        }
    }

    fn parse_flow_expression(&mut self) -> FlowExpr {
        self.parse_flow_binary_expr(0)
    }

    fn parse_flow_binary_expr(&mut self, precedence: u32) -> FlowExpr {
        if !self.is_set(FLOW_EXPRESSION_FIRST) {
            self.report_error(ParseError::ExpectedExpression);
            return Arc::new(FlowExprData::Error {
                id: self.new_node_id(),
                span: self.current_span(),
            });
        }

        let start = self.current_span().start();
        let marker = self.builder.create_marker();
        let mut left = self.parse_postfix_flow_expr();

        loop {
            let right_precedence = match self.current() {
                ARROW => 1,
                _ => {
                    return left;
                }
            };

            if precedence >= right_precedence {
                return left;
            }

            let kind = self.current();
            self.advance();

            left = match kind {
                ARROW => {
                    let right = self.parse_postfix_flow_expr();
                    let span = self.span_from(start);

                    self.builder
                        .finish_node_starting_at(WIRE_EXPR, marker.clone());

                    Arc::new(FlowExprData::create_wire(
                        self.new_node_id(),
                        span,
                        left,
                        right,
                    ))
                }
                _ => {
                    let right = self.parse_flow_binary_expr(right_precedence);
                    //self.builder
                    //    .finish_node_starting_at(BINARY_EXPR, marker.clone());
                    todo!()
                    //self.create_binary(kind, start, left, right)
                }
            };
        }
    }

    fn parse_flow_stmt_or_expr(&mut self) -> FlowStmtOrExpr {
        match self.current() {
            NODE_KW => FlowStmtOrExpr::Stmt(self.parse_flow_node()),
            _ => {
                let expr = self.parse_flow_expression();

                if self.eat(SEMICOLON) {
                    let span = self.span_from(expr.span().start());
                    FlowStmtOrExpr::Stmt(Arc::new(FlowStmtData::create_expr(
                        self.new_node_id(),
                        span,
                        expr,
                    )))
                } else {
                    FlowStmtOrExpr::Expr(expr)
                }
            }
        }
    }

    fn parse_var_type(&mut self) -> Option<Type> {
        if self.eat(COLON) {
            Some(self.parse_type())
        } else {
            None
        }
    }

    fn parse_var_assignment(&mut self) -> Option<Expr> {
        if self.eat(EQ) {
            Some(self.parse_expression())
        } else {
            None
        }
    }

    fn parse_let_pattern(&mut self) -> Box<PatternKind> {
        self.start_node();

        //let mutable = self.eat(MUT_KW);
        let name = self.expect_identifier();

        Box::new(PatternKind::Ident(LetIdentType {
            id: self.new_node_id(),
            span: self.finish_node(),
            mutable: false,
            name,
        }))
    }

    fn parse_let(&mut self) -> Stmt {
        self.start_node();

        self.assert(LET_KW);
        let pattern = self.parse_let_pattern();
        let data_type = self.parse_var_type();
        let expr = self.parse_var_assignment();

        self.expect(SEMICOLON);

        Arc::new(StmtData::create_let(
            self.new_node_id(),
            self.finish_node(),
            pattern,
            data_type,
            expr,
        ))
    }

    fn parse_flow_node(&mut self) -> ast::FlowStmt {
        self.start_node();

        self.assert(NODE_KW);
        let name = self.expect_identifier();
        let data_type = self.parse_var_type();
        self.expect(SEMICOLON);

        Arc::new(FlowStmtData::create_flow_node(
            self.new_node_id(),
            self.finish_node(),
            name,
            data_type,
        ))
    }

    fn parse_function_stmt_or_expr(&mut self) -> StmtOrExpr {
        match self.current() {
            LET_KW => StmtOrExpr::Stmt(self.parse_let()),
            _ => {
                let expr = self.parse_expression();

                if self.eat(SEMICOLON) {
                    let span = self.span_from(expr.span().start());

                    StmtOrExpr::Stmt(Arc::new(StmtData::create_expr(
                        self.new_node_id(),
                        span,
                        expr,
                    )))
                } else {
                    StmtOrExpr::Expr(expr)
                }
            }
        }
    }

    fn parse_expression(&mut self) -> Expr {
        self.parse_binary_expr(0)
    }

    fn parse_binary_expr(&mut self, precedence: u32) -> Expr {
        if !self.is_set(EXPRESSION_FIRST) {
            self.report_error(ParseError::ExpectedExpression);
            return Arc::new(ExprKind::Error {
                id: self.new_node_id(),
                span: self.current_span(),
            });
        }

        let start = self.current_span().start();
        let marker = self.builder.create_marker();
        let mut left = self.parse_unary_expr();

        loop {
            let right_precedence = match self.current() {
                EQ => 1,
                OR_OR => 2,
                AND_AND => 3,
                EQ_EQ | NOT_EQ | LT | LE | GT | GE | EQ_EQ_EQ | NOT_EQ_EQ => 4,
                ADD | SUB | OR | CARET => 5,
                MUL | DIV | MODULO | AND | LT_LT | GT_GT | GT_GT_GT => 6,
                L_BRACE => 7,
                _ => {
                    return left;
                }
            };

            if precedence >= right_precedence {
                return left;
            }

            let kind = self.current();

            left = match kind {
                L_BRACE => self.parse_lit_struct(start, marker.clone(), left),
                /*AS_KW => {
                    let right = self.parse_type();
                    let span = self.span_from(start);

                    self.builder
                        .finish_node_starting_at(CONV_EXPR, marker.clone());

                    let expr = ExprData::create_conv(self.new_node_id(), span, left, right);

                    Arc::new(expr)
                }*/

                /*IS_KW => {
                    let right = self.parse_match_pattern();
                    let span = self.span_from(start);

                    self.builder
                        .finish_node_starting_at(IS_EXPR, marker.clone());

                    let expr = ExprData::create_is(self.new_node_id(), span, left, right);

                    Arc::new(expr)
                }*/
                _ => {
                    self.advance();
                    let right = self.parse_binary_expr(right_precedence);
                    self.builder
                        .finish_node_starting_at(BINARY_EXPR, marker.clone());
                    self.create_binary(kind, start, left, right)
                }
            };
        }
    }

    fn parse_unary_expr(&mut self) -> Expr {
        match self.current() {
            SUB | NOT => {
                self.start_node();
                self.builder.start_node();
                let kind = self.current();
                self.advance();
                let op = match kind {
                    SUB => UnOp::Neg,
                    NOT => UnOp::Not,
                    _ => unreachable!(),
                };

                let expr = self.parse_postfix_expr();
                let green = self.builder.finish_node(UNARY_EXPR);

                Arc::new(ExprKind::create_un(
                    self.new_node_id(),
                    self.finish_node(),
                    green,
                    op,
                    expr,
                ))
            }
            _ => self.parse_postfix_expr(),
        }
    }

    fn parse_postfix_flow_expr(&mut self) -> ast::FlowExpr {
        let start = self.current_span().start();
        let marker = self.builder.create_marker();
        let mut lhs = self.parse_flow_factor();

        loop {
            lhs = match self.current() {
                DOT => {
                    let op_span = self.current_span();
                    self.assert(DOT);
                    let rhs = self.parse_flow_factor();
                    let span = self.span_from(start);

                    self.builder
                        .finish_node_starting_at(POSTFIX_EXPR, marker.clone());

                    Arc::new(ast::FlowExprData::create_dot(
                        self.new_node_id(),
                        span,
                        op_span,
                        lhs,
                        rhs,
                    ))
                }
                _ => {
                    return lhs;
                }
            }
        }
    }

    fn parse_postfix_expr(&mut self) -> Expr {
        let start = self.current_span().start();
        let marker = self.builder.create_marker();
        let mut left = self.parse_factor();

        loop {
            left = match self.current() {
                DOT => {
                    let op_span = self.current_span();
                    self.assert(DOT);
                    let rhs = self.parse_factor();
                    let span = self.span_from(start);

                    self.builder
                        .finish_node_starting_at(POSTFIX_EXPR, marker.clone());

                    Arc::new(ExprKind::create_dot(
                        self.new_node_id(),
                        span,
                        op_span,
                        left,
                        rhs,
                    ))
                }
                L_PAREN => self.parse_call(start, marker.clone(), left),

                /*L_BRACKET => {
                    let op_span = self.current_span();
                    let types = self.parse_list(
                        L_BRACKET,
                        COMMA,
                        R_BRACKET,
                        TYPE_PARAM_RS,
                        ParseError::ExpectedType,
                        TYPE_LIST,
                        |p| p.parse_type_wrapper(),
                    );
                    let span = self.span_from(start);

                    self.builder
                        .finish_node_starting_at(POSTFIX_EXPR, marker.clone());

                    Arc::new(ExprData::create_type_param(
                        self.new_node_id(),
                        span,
                        op_span,
                        left,
                        types,
                    ))
                }*/
                COLON_COLON => {
                    let op_span = self.current_span();
                    self.assert(COLON_COLON);
                    let rhs = self.parse_factor();
                    let span = self.span_from(start);

                    self.builder
                        .finish_node_starting_at(POSTFIX_EXPR, marker.clone());

                    Arc::new(ExprKind::create_path(
                        self.new_node_id(),
                        span,
                        op_span,
                        left,
                        rhs,
                    ))
                }
                _ => {
                    return left;
                }
            }
        }
    }

    fn parse_lit_struct(&mut self, start: u32, marker: Marker, left: Expr) -> Expr {
        let span = self.span_from(start);

        let fields = if self.is(L_BRACE) {
            self.parse_list(
                L_BRACE,
                COMMA,
                R_BRACE,
                ELEM_FIRST,
                ParseError::ExpectedField,
                LIST,
                |p| {
                    if p.is_set(FIELD_VALUE_FIRST) {
                        Some(p.parse_field_value())
                    } else {
                        None
                    }
                },
            )
        } else {
            Vec::new()
        };

        self.builder.finish_node_starting_at(STRUCT, marker.clone());

        Arc::new(ExprKind::Struct(ExprStruct {
            id: self.new_node_id(),
            span,
            name: left,
            field_values: fields,
        }))
    }

    fn parse_call(&mut self, start: u32, marker: Marker, left: Expr) -> Expr {
        let args = self.parse_list(
            L_PAREN,
            COMMA,
            R_PAREN,
            EMPTY,
            ParseError::ExpectedExpression,
            ARG_LIST,
            |p| {
                if p.is_set(EXPRESSION_FIRST) {
                    Some(p.parse_expression())
                } else {
                    None
                }
            },
        );
        let span = self.span_from(start);

        self.builder
            .finish_node_starting_at(POSTFIX_EXPR, marker.clone());

        Arc::new(ExprKind::create_call(self.new_node_id(), span, left, args))
    }

    fn parse_list<F, R>(
        &mut self,
        start: TokenKind,
        sep: TokenKind,
        stop: TokenKind,
        recovery_set: TokenSet,
        msg: ParseError,
        node: TokenKind,
        mut parse: F,
    ) -> Vec<R>
    where
        F: FnMut(&mut Parser) -> Option<R>,
    {
        let mut data = vec![];
        self.builder.start_node();
        self.assert(start);

        while !self.is(stop.clone()) && !self.is_eof() {
            let pos_before_element = self.token_idx;
            let entry = parse(self);

            match entry {
                Some(entry) => {
                    // Callback needs to at least advance by one token, otherwise
                    // we might loop forever here.
                    assert!(self.token_idx > pos_before_element);
                    data.push(entry)
                }

                None => {
                    if self.is_set(recovery_set) {
                        break;
                    }

                    self.report_error(msg.clone());
                    self.advance();
                }
            }

            if !self.is(stop.clone()) {
                self.expect(sep);
            }
        }

        self.expect(stop);
        self.builder.finish_node(node);

        data
    }

    fn create_binary(&mut self, kind: TokenKind, start: u32, left: Expr, right: Expr) -> Expr {
        let op = match kind {
            EQ => BinOpKind::Assign,
            OR_OR => BinOpKind::Or,
            AND_AND => BinOpKind::And,
            EQ_EQ => BinOpKind::Cmp(CmpOp::Eq),
            NOT_EQ => BinOpKind::Cmp(CmpOp::Ne),
            LT => BinOpKind::Cmp(CmpOp::Lt),
            LE => BinOpKind::Cmp(CmpOp::Le),
            GT => BinOpKind::Cmp(CmpOp::Gt),
            GE => BinOpKind::Cmp(CmpOp::Ge),
            EQ_EQ_EQ => BinOpKind::Cmp(CmpOp::Is),
            NOT_EQ_EQ => BinOpKind::Cmp(CmpOp::IsNot),
            OR => BinOpKind::BitOr,
            AND => BinOpKind::BitAnd,
            CARET => BinOpKind::BitXor,
            ADD => BinOpKind::Add,
            SUB => BinOpKind::Sub,
            MUL => BinOpKind::Mul,
            DIV => BinOpKind::Div,
            MODULO => BinOpKind::Mod,
            LT_LT => BinOpKind::ShiftL,
            GT_GT => BinOpKind::ArithShiftR,
            GT_GT_GT => BinOpKind::LogicalShiftR,
            _ => panic!("unimplemented token {:?}", kind),
        };

        let span = self.span_from(start);

        Arc::new(ExprKind::create_bin(
            self.new_node_id(),
            span,
            op,
            left,
            right,
        ))
    }

    fn parse_identifier(&mut self) -> ast::ExprIdentType {
        self.builder.start_node();
        let ident = self.expect_identifier().expect("identifier expected");
        let green = self.builder.finish_node(IDENT_EXPR);

        ast::ExprIdentType {
            id: self.new_node_id(),
            span: ident.span,
            green,
            name: ident.name_as_string.clone(),
        }
    }

    fn parse_lit_bool(&mut self) -> ast::Literal {
        self.builder.start_node();
        let span = self.current_span();
        let kind = self.current();
        self.assert(kind);
        let green = self.builder.finish_node(BOOL_LIT_EXPR);
        let value = kind == TRUE;

        ast::Literal {
            id: self.new_node_id(),
            span,
            green,
            kind: ast::LitKind::Bool(value),
        }
    }

    fn parse_lit_int(&mut self) -> ast::Literal {
        let span = self.current_span();
        self.builder.start_node();
        self.assert(INT_LITERAL);
        let value = self.source_span(span);
        let green = self.builder.finish_node(INT_LIT_EXPR);

        ast::Literal {
            id: self.new_node_id(),
            span,
            green,
            kind: ast::LitKind::Int(value),
        }
    }

    fn parse_lit_float(&mut self) -> ast::Literal {
        let span = self.current_span();
        self.builder.start_node();
        self.assert(FLOAT_LITERAL);
        let value = self.source_span(span);

        let green = self.builder.finish_node(FLOAT_LIT_EXPR);

        ast::Literal {
            id: self.new_node_id(),
            span,
            green,
            kind: ast::LitKind::Float(value),
        }
    }

    fn parse_flow_parentheses(&mut self) -> ast::FlowExprParenType {
        self.start_node();
        self.builder.start_node();
        self.assert(L_PAREN);

        if self.eat(R_PAREN) {

            //    let green = self.builder.finish_node(TUPLE_EXPR);
            //    return Arc::new(ExprData::create_tuple(
            //        self.new_node_id(),
            //        self.finish_node(),
            //        green,
            //        Vec::new(),
            //    ));
        }

        let expr = self.parse_flow_expression();

        if self.current() == COMMA {
            let mut values = vec![expr];

            loop {
                self.expect(COMMA);

                if self.eat(R_PAREN) {
                    break;
                }

                if !self.is_set(FLOW_EXPRESSION_FIRST) {
                    break;
                }

                let expr = self.parse_flow_expression();
                values.push(expr);

                if self.eat(R_PAREN) {
                    break;
                }
            }

            let green = self.builder.finish_node(FLOW_PAREN_EXPR);

            ast::FlowExprParenType {
                id: self.new_node_id(),
                span: self.finish_node(),
                green,
                args: values,
            }
        } else {
            let green = self.builder.finish_node(FLOW_PAREN_EXPR);

            self.expect(R_PAREN);

            ast::FlowExprParenType {
                id: self.new_node_id(),
                span: self.finish_node(),
                green,
                args: vec![expr],
            }
        }
    }

    fn parse_parentheses(&mut self) -> ast::Paren {
        self.start_node();
        self.builder.start_node();
        self.assert(L_PAREN);

        if self.eat(R_PAREN) {
            //    let green = self.builder.finish_node(TUPLE_EXPR);
            //    return Arc::new(ExprData::create_tuple(
            //        self.new_node_id(),
            //        self.finish_node(),
            //        green,
            //        Vec::new(),
            //    ));
        }

        let expr = self.parse_expression();

        if self.current() == COMMA {
            unimplemented!();
            /*let mut values = vec![expr];

            loop {
                self.expect(COMMA);

                if self.eat(R_PAREN) {
                    break;
                }

                if !self.is_set(EXPRESSION_FIRST) {
                    break;
                }

                let expr = self.parse_expression();
                values.push(expr);

                if self.eat(R_PAREN) {
                    break;
                }
            }

            let green = self.builder.finish_node(TUPLE_EXPR);

            Arc::new(ExprData::create_tuple(
                self.new_node_id(),
                self.finish_node(),
                green,
                values,
            ))*/
        } else {
            let green = self.builder.finish_node(PAREN_EXPR);

            self.expect(R_PAREN);

            ast::Paren {
                id: self.new_node_id(),
                span: self.finish_node(),
                green,
                expr,
            }
        }
    }

    fn parse_flow_factor(&mut self) -> ast::FlowExpr {
        let span = self.current_span();

        Arc::from(match self.current() {
            L_PAREN => ast::FlowExprData::Paren(self.parse_flow_parentheses()),
            IDENTIFIER => ast::FlowExprData::Ident(self.parse_identifier()),
            FLOAT_LITERAL => ast::FlowExprData::Lit(self.parse_lit_float()),
            INT_LITERAL => ast::FlowExprData::Lit(self.parse_lit_int()),
            TRUE => ast::FlowExprData::Lit(self.parse_lit_bool()),
            FALSE => ast::FlowExprData::Lit(self.parse_lit_bool()),
            _ => {
                self.report_error(ParseError::ExpectedFactor);
                ast::FlowExprData::Error {
                    id: self.new_node_id(),
                    span,
                }
            }
        })
    }

    fn parse_factor(&mut self) -> Expr {
        let span = self.current_span();
        Arc::from(match self.current() {
            L_PAREN => ExprKind::Paren(self.parse_parentheses()),
            L_BRACE => ExprKind::Block(self.parse_block()),
            INT_LITERAL => ExprKind::Literal(self.parse_lit_int()),
            FLOAT_LITERAL => ExprKind::Literal(self.parse_lit_float()),
            IDENTIFIER => ExprKind::Ident(self.parse_identifier()),
            TRUE => ExprKind::Literal(self.parse_lit_bool()),
            FALSE => ExprKind::Literal(self.parse_lit_bool()),
            RETURN_KW => ExprKind::Return(self.parse_return()),
            _ => {
                self.report_error(ParseError::ExpectedFactor);
                ExprKind::Error {
                    id: self.new_node_id(),
                    span,
                }
            }
        })
    }

    fn parse_return(&mut self) -> ast::ExprReturnType {
        self.start_node();
        self.builder.start_node();
        self.assert(RETURN_KW);
        let expr = if self.is(SEMICOLON) {
            None
        } else {
            let expr = self.parse_expression();
            Some(expr)
        };

        let green = self.builder.finish_node(RETURN_EXPR);

        ast::ExprReturnType {
            id: self.new_node_id(),
            span: self.finish_node(),
            green,
            expr,
        }
    }

    fn source_span(&self, span: Span) -> String {
        let start = span.start() as usize;
        let end = span.end() as usize;
        String::from(&self.content[start..end])
    }

    fn span_from(&self, start: u32) -> Span {
        Span::new(self.source_id, start, self.offset - start)
    }

    fn nth_is_set(&self, idx: usize, set: TokenSet) -> bool {
        set.contains(self.nth(idx))
    }

    fn parse_field_value(&mut self) -> FieldValue {
        self.start_node();
        self.builder.start_node();

        let ident = self.expect_identifier();
        self.expect(COLON);
        let value = self.parse_expression();

        let green = self.builder.finish_node(FIELD_VALUE);

        FieldValue {
            id: self.new_node_id(),
            span: self.finish_node(),
            green,
            name: ident,
            value,
        }
    }
}

fn token_name(kind: TokenKind) -> Option<&'static str> {
    match kind {
        //PACKAGE_KW => Some("package"),
        //IN_KW => Some("in"),
        EQ => Some("="),
        COMMA => Some(","),
        SEMICOLON => Some(";"),
        DOT => Some("."),
        COLON => Some(":"),
        ARROW => Some("->"),
        DOUBLE_ARROW => Some("=>"),
        OR => Some("|"),
        L_PAREN => Some("("),
        R_PAREN => Some(")"),
        L_BRACKET => Some("["),
        R_BRACKET => Some("]"),
        L_BRACE => Some("{"),
        R_BRACE => Some("}"),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use id_arena::Arena;

    use crate::source_file::{SourceFile, SourceFileId};
    use crate::{ast::*, compute_line_column, compute_line_starts};
    use std::sync::Arc;

    use crate::error::ParseError;
    use crate::parser::{NodeIdGenerator, Parser};

    fn create_source<'a>(code: &'static str) -> (Arena<SourceFile>, SourceFileId) {
        let mut sources: Arena<SourceFile> = Arena::new();
        let id = sources.alloc(SourceFile::new("test.ark".into(), code));
        sources.get(id).unwrap().id.set(id).unwrap();

        (sources, id)
    }

    fn parse_expr(code: &'static str) -> Expr {
        let id_gen = Arc::from(NodeIdGenerator::new());
        let (sources, id) = create_source(code);
        let mut parser = Parser::from_source(id_gen, &sources[id]);

        let result = parser.parse_expression();
        assert!(parser.errors.is_empty());

        result
    }

    fn parse(code: &'static str) -> Arc<File> {
        let id_gen = Arc::from(NodeIdGenerator::new());
        let (sources, id) = create_source(code);
        let (file, errors) = Parser::from_source(id_gen, &sources[id]).parse();
        println!("{:#?}", errors);
        assert!(errors.is_empty());
        file
    }

    fn err_expr(code: &'static str, msg: ParseError, line: u32, col: u32) {
        let id_gen = Arc::from(NodeIdGenerator::new());
        let (sources, id) = create_source(code);
        let mut parser = Parser::from_source(id_gen, &sources[id]);

        let _expr = parser.parse_expression();

        let errors = parser.errors;
        assert_eq!(errors.len(), 1);
        let err = &errors[0];

        assert_eq!(msg, err.error);

        let line_starts = compute_line_starts(code);
        let (computed_line, computed_column) = compute_line_column(&line_starts, err.span.start());
        assert_eq!(line, computed_line);
        assert_eq!(col, computed_column);
    }

    #[test]
    fn parse_ident() {
        let expr = parse_expr("a");
        let ident = expr.to_ident().unwrap();
        assert_eq!("a", ident.name);
    }

    #[test]
    fn parse_number() {
        let expr = parse_expr("10");

        assert_eq!(
            String::from("10"),
            *expr.to_lit().unwrap().as_string().unwrap()
        );
    }

    #[test]
    fn parse_neg() {
        let expr = parse_expr("-1");

        let un = expr.to_un().unwrap();
        assert_eq!(UnOp::Neg, un.op);

        assert!(un.opnd.to_lit().unwrap().is_int());
    }

    #[test]
    fn parse_neg_twice() {
        let expr = parse_expr("-(-3)");

        let neg1 = expr.to_un().unwrap();
        assert_eq!(UnOp::Neg, neg1.op);

        let neg2 = neg1.opnd.to_paren().unwrap().expr.to_un().unwrap();
        assert_eq!(UnOp::Neg, neg2.op);

        assert!(neg2.opnd.to_lit().unwrap().is_int());
    }

    #[test]
    fn parse_neg_twice_without_parentheses() {
        err_expr("- -2", ParseError::ExpectedFactor, 1, 3);
    }

    #[test]
    fn parse_mul() {
        let expr = parse_expr("6*3");

        let (mul, lhs, rhs) = expr.to_bin().unwrap();
        assert_eq!(BinOpKind::Mul, mul.op);
        assert_eq!(
            String::from("6"),
            *lhs.to_lit().unwrap().as_string().unwrap()
        );
        assert_eq!(
            String::from("3"),
            *rhs.to_lit().unwrap().as_string().unwrap()
        );
    }

    #[test]
    fn parse_multiple_muls() {
        let expr = parse_expr("6*3*4");

        let (mul1, lhs1, rhs1) = expr.to_bin().unwrap();
        assert_eq!(BinOpKind::Mul, mul1.op);

        let (mul2, lhs2, rhs2) = lhs1.to_bin().unwrap();
        assert_eq!(BinOpKind::Mul, mul2.op);
        assert_eq!(
            String::from("6"),
            *lhs2.to_lit().unwrap().as_string().unwrap()
        );
        assert_eq!(
            String::from("3"),
            *rhs2.to_lit().unwrap().as_string().unwrap()
        );

        assert_eq!(
            String::from("4"),
            *rhs1.to_lit().unwrap().as_string().unwrap()
        );
    }

    #[test]
    fn parse_div() {
        let expr = parse_expr("4/5");

        let (div, lhs, rhs) = expr.to_bin().unwrap();
        assert_eq!(BinOpKind::Div, div.op);
        assert_eq!(
            String::from("4"),
            *lhs.to_lit().unwrap().as_string().unwrap()
        );
        assert_eq!(
            String::from("5"),
            *rhs.to_lit().unwrap().as_string().unwrap()
        );
    }

    #[test]
    fn parse_mod() {
        let expr = parse_expr("2%15");

        let (div, lhs, rhs) = expr.to_bin().unwrap();
        assert_eq!(BinOpKind::Mod, div.op);
        assert_eq!(
            String::from("2"),
            *lhs.to_lit().unwrap().as_string().unwrap()
        );
        assert_eq!(
            String::from("15"),
            *rhs.to_lit().unwrap().as_string().unwrap()
        );
    }

    #[test]
    fn parse_add() {
        let expr = parse_expr("2+3");

        let (add, lhs, rhs) = expr.to_bin().unwrap();
        assert_eq!(BinOpKind::Add, add.op);
        assert_eq!(
            String::from("2"),
            *lhs.to_lit().unwrap().as_string().unwrap()
        );
        assert_eq!(
            String::from("3"),
            *rhs.to_lit().unwrap().as_string().unwrap()
        );
    }

    #[test]
    fn parse_add_left_associativity() {
        let expr = parse_expr("1+2+3");

        let (_, lhs, rhs) = expr.to_bin().unwrap();
        assert_eq!(
            String::from("3"),
            *rhs.to_lit().unwrap().as_string().unwrap()
        );

        let (_, lhs, rhs) = lhs.to_bin().unwrap();
        assert_eq!(
            String::from("1"),
            *lhs.to_lit().unwrap().as_string().unwrap()
        );
        assert_eq!(
            String::from("2"),
            *rhs.to_lit().unwrap().as_string().unwrap()
        );
    }

    #[test]
    fn parse_add_right_associativity_via_parens() {
        let expr = parse_expr("1+(2+3)");

        let (_, lhs, rhs) = expr.to_bin().unwrap();
        assert_eq!(
            String::from("1"),
            *lhs.to_lit().unwrap().as_string().unwrap()
        );

        let (_, lhs, rhs) = rhs.to_paren().unwrap().expr.to_bin().unwrap();
        assert_eq!(
            String::from("2"),
            *lhs.to_lit().unwrap().as_string().unwrap()
        );
        assert_eq!(
            String::from("3"),
            *rhs.to_lit().unwrap().as_string().unwrap()
        );
    }

    #[test]
    fn parse_sub() {
        let expr = parse_expr("1-2");

        let (add, lhs, rhs) = expr.to_bin().unwrap();
        assert_eq!(BinOpKind::Sub, add.op);
        assert_eq!(
            String::from("1"),
            *lhs.to_lit().unwrap().as_string().unwrap()
        );
        assert_eq!(
            String::from("2"),
            *rhs.to_lit().unwrap().as_string().unwrap()
        );
    }

    #[test]
    fn parse_or() {
        let expr = parse_expr("1||2");

        let (add, lhs, rhs) = expr.to_bin().unwrap();
        assert_eq!(BinOpKind::Or, add.op);
        assert_eq!(
            String::from("1"),
            *lhs.to_lit().unwrap().as_string().unwrap()
        );
        assert_eq!(
            String::from("2"),
            *rhs.to_lit().unwrap().as_string().unwrap()
        );
    }

    #[test]
    fn parse_and() {
        let expr = parse_expr("1&&2");

        let (add, lhs, rhs) = expr.to_bin().unwrap();
        assert_eq!(BinOpKind::And, add.op);
        assert_eq!(
            String::from("1"),
            *lhs.to_lit().unwrap().as_string().unwrap()
        );
        assert_eq!(
            String::from("2"),
            *rhs.to_lit().unwrap().as_string().unwrap()
        );
    }

    #[test]
    fn parse_bit_or() {
        let expr = parse_expr("1|2");

        let (or, lhs, rhs) = expr.to_bin().unwrap();
        assert_eq!(BinOpKind::BitOr, or.op);
        assert_eq!(
            String::from("1"),
            *lhs.to_lit().unwrap().as_string().unwrap()
        );
        assert_eq!(
            String::from("2"),
            *rhs.to_lit().unwrap().as_string().unwrap()
        );
    }

    #[test]
    fn parse_bit_and() {
        let expr = parse_expr("1&2");

        let (and, lhs, rhs) = expr.to_bin().unwrap();
        assert_eq!(BinOpKind::BitAnd, and.op);
        assert_eq!(
            String::from("1"),
            *lhs.to_lit().unwrap().as_string().unwrap()
        );
        assert_eq!(
            String::from("2"),
            *rhs.to_lit().unwrap().as_string().unwrap()
        );
    }

    #[test]
    fn parse_bit_xor() {
        let expr = parse_expr("1^2");

        let (xor, lhs, rhs) = expr.to_bin().unwrap();
        assert_eq!(BinOpKind::BitXor, xor.op);
        assert_eq!(
            String::from("1"),
            *lhs.to_lit().unwrap().as_string().unwrap()
        );
        assert_eq!(
            String::from("2"),
            *rhs.to_lit().unwrap().as_string().unwrap()
        );
    }

    #[test]
    fn parse_lt() {
        let expr = parse_expr("1<2");

        let (cmp, lhs, rhs) = expr.to_bin().unwrap();
        assert_eq!(BinOpKind::Cmp(CmpOp::Lt), cmp.op);
        assert_eq!(
            String::from("1"),
            *lhs.to_lit().unwrap().as_string().unwrap()
        );
        assert_eq!(
            String::from("2"),
            *rhs.to_lit().unwrap().as_string().unwrap()
        );
    }

    #[test]
    fn parse_le() {
        let expr = parse_expr("1<=2");

        let (cmp, lhs, rhs) = expr.to_bin().unwrap();
        assert_eq!(BinOpKind::Cmp(CmpOp::Le), cmp.op);
        assert_eq!(
            String::from("1"),
            *lhs.to_lit().unwrap().as_string().unwrap()
        );
        assert_eq!(
            String::from("2"),
            *rhs.to_lit().unwrap().as_string().unwrap()
        );
    }

    #[test]
    fn parse_gt() {
        let expr = parse_expr("1>2");

        let (cmp, lhs, rhs) = expr.to_bin().unwrap();
        assert_eq!(BinOpKind::Cmp(CmpOp::Gt), cmp.op);
        assert_eq!(
            String::from("1"),
            *lhs.to_lit().unwrap().as_string().unwrap()
        );
        assert_eq!(
            String::from("2"),
            *rhs.to_lit().unwrap().as_string().unwrap()
        );
    }

    #[test]
    fn parse_ge() {
        let expr = parse_expr("1>=2");

        let (cmp, lhs, rhs) = expr.to_bin().unwrap();
        assert_eq!(BinOpKind::Cmp(CmpOp::Ge), cmp.op);
        assert_eq!(
            String::from("1"),
            *lhs.to_lit().unwrap().as_string().unwrap()
        );
        assert_eq!(
            String::from("2"),
            *rhs.to_lit().unwrap().as_string().unwrap()
        );
    }

    #[test]
    fn parse_eq() {
        let expr = parse_expr("1==2");

        let (cmp, lhs, rhs) = expr.to_bin().unwrap();
        assert_eq!(BinOpKind::Cmp(CmpOp::Eq), cmp.op);
        assert_eq!(
            String::from("1"),
            *lhs.to_lit().unwrap().as_string().unwrap()
        );
        assert_eq!(
            String::from("2"),
            *rhs.to_lit().unwrap().as_string().unwrap()
        );
    }

    #[test]
    fn parse_ne() {
        let expr = parse_expr("1!=2");

        let (cmp, lhs, rhs) = expr.to_bin().unwrap();
        assert_eq!(BinOpKind::Cmp(CmpOp::Ne), cmp.op);
        assert_eq!(
            String::from("1"),
            *lhs.to_lit().unwrap().as_string().unwrap()
        );
        assert_eq!(
            String::from("2"),
            *rhs.to_lit().unwrap().as_string().unwrap()
        );
    }

    #[test]
    fn parse_identity_not() {
        let expr = parse_expr("1!==2");

        let (cmp, lhs, rhs) = expr.to_bin().unwrap();
        assert_eq!(BinOpKind::Cmp(CmpOp::IsNot), cmp.op);
        assert_eq!(
            String::from("1"),
            *lhs.to_lit().unwrap().as_string().unwrap()
        );
        assert_eq!(
            String::from("2"),
            *rhs.to_lit().unwrap().as_string().unwrap()
        );
    }

    #[test]
    fn parse_identity() {
        let expr = parse_expr("1===2");

        let (cmp, lhs, rhs) = expr.to_bin().unwrap();
        assert_eq!(BinOpKind::Cmp(CmpOp::Is), cmp.op);
        assert_eq!(
            String::from("1"),
            *lhs.to_lit().unwrap().as_string().unwrap()
        );
        assert_eq!(
            String::from("2"),
            *rhs.to_lit().unwrap().as_string().unwrap()
        );
    }

    #[test]
    fn parse_true() {
        let expr = parse_expr("true");
        assert_eq!(true, expr.to_lit().unwrap().as_bool().unwrap());
    }

    #[test]
    fn parse_false() {
        let expr = parse_expr("true");
        assert_eq!(true, expr.to_lit().unwrap().as_bool().unwrap());
    }

    #[test]
    fn parse_assign() {
        let expr = parse_expr("a=4");

        let (assign, lhs, rhs) = expr.to_bin().unwrap();
        assert!(lhs.is_ident());
        assert_eq!(BinOpKind::Assign, assign.op);
        assert_eq!(
            String::from("4"),
            *rhs.to_lit().unwrap().as_string().unwrap()
        );
    }

    #[test]
    fn parse_shift_right() {
        let expr = parse_expr("a>>4");

        let (bin, _, _) = expr.to_bin().unwrap();
        assert_eq!(BinOpKind::ArithShiftR, bin.op);
    }

    #[test]
    fn parse_unsigned_shift_right() {
        let expr = parse_expr("a>>>4");

        let (bin, _, _) = expr.to_bin().unwrap();
        assert_eq!(BinOpKind::LogicalShiftR, bin.op);
    }

    #[test]
    fn parse_left() {
        let expr = parse_expr("a<<4");

        let (bin, _, _) = expr.to_bin().unwrap();
        assert_eq!(BinOpKind::ShiftL, bin.op);
    }

    #[test]
    fn parse_call_without_params() {
        let expr = parse_expr("fname()");

        let call = expr.to_call().unwrap();
        assert_eq!("fname", call.callee.to_ident().unwrap().name);
        assert_eq!(0, call.args.len());
    }

    #[test]
    fn parse_call_with_path() {
        let expr = parse_expr("Foo::get()");
        let call = expr.to_call().unwrap();

        assert!(call.callee.is_path());
        assert_eq!(0, call.args.len());
    }

    #[test]
    fn parse_new_call_path() {
        let expr = parse_expr("Foo::bar");
        let path = expr.to_path().unwrap();
        assert!(path.lhs.is_ident());
        assert!(path.rhs.is_ident());
    }

    #[test]
    fn parse_new_call_call() {
        let expr = parse_expr("foo(1,2)");
        let call = expr.to_call().unwrap();
        assert!(call.callee.is_ident());
        assert_eq!(call.args.len(), 2);
    }

    #[test]
    fn parse_call_with_params() {
        let expr = parse_expr("fname2(1,2,3)");

        let call = expr.to_call().unwrap();
        assert_eq!("fname2", call.callee.to_ident().unwrap().name);
        assert_eq!(3, call.args.len());
    }

    #[test]
    fn parse_function() {
        let prog = parse("fn b() {}");
        let fct = prog.fct0();

        assert_eq!("b", fct.name.as_ref().unwrap().name_as_string);
        assert_eq!(0, fct.signature.inputs.len());
        assert!(fct.signature.output.is_none());
    }

    #[test]
    fn parse_function_with_single_param() {
        let p1 = parse("fn f(a:int) { }");
        let f1 = p1.fct0();

        let p2 = parse("fn f(a:int,) { }");
        let f2 = p2.fct0();

        assert_eq!(f1.signature.inputs.len(), 1);
        assert_eq!(f2.signature.inputs.len(), 1);

        let p1 = &f1.signature.inputs[0];
        let p2 = &f2.signature.inputs[0];

        assert_eq!("a", p1.name.as_ref().unwrap().name_as_string);
        assert_eq!("a", p2.name.as_ref().unwrap().name_as_string);

        assert_eq!("int", p1.data_type.to_basic().unwrap().name());
        assert_eq!("int", p2.data_type.to_basic().unwrap().name());
    }

    #[test]
    fn parse_function_with_multiple_params() {
        let p1 = parse("fn f(a:int, b:str) { }");
        let f1 = p1.fct0();

        let p2 = parse("fn f(a:int, b:str,) { }");
        let f2 = p2.fct0();

        let p1a = &f1.signature.inputs[0];
        let p1b = &f1.signature.inputs[1];
        let p2a = &f2.signature.inputs[0];
        let p2b = &f2.signature.inputs[1];

        assert_eq!("a", p1a.name.as_ref().unwrap().name_as_string);
        assert_eq!("a", p2a.name.as_ref().unwrap().name_as_string);

        assert_eq!("b", p1b.name.as_ref().unwrap().name_as_string);
        assert_eq!("b", p2b.name.as_ref().unwrap().name_as_string);

        assert_eq!("int", p1a.data_type.to_basic().unwrap().name());
        assert_eq!("int", p2a.data_type.to_basic().unwrap().name());

        assert_eq!("str", p1b.data_type.to_basic().unwrap().name());
        assert_eq!("str", p2b.data_type.to_basic().unwrap().name());
    }

    #[test]
    fn parse_multiple_functions() {
        let prog = parse("fn f() { } fn g() { }");

        let f = prog.fct0();
        assert_eq!("f", f.name.as_ref().unwrap().name_as_string);

        let g = prog.fct(1);
        assert_eq!("g", g.name.as_ref().unwrap().name_as_string);
    }

    #[test]
    fn parse_empty_block() {
        let expr = parse_expr("{}");
        let block = expr.to_block().unwrap();

        assert_eq!(0, block.stmts.len());
    }

    #[test]
    fn parse_block() {
        let expr = parse_expr("{1}");
        assert!(expr
            .to_block()
            .unwrap()
            .expr
            .as_ref()
            .unwrap()
            .to_lit()
            .unwrap()
            .is_int());

        let expr = parse_expr("({}) + 1");
        assert!(expr.is_bin());

        let expr = parse_expr("1 + {}");
        assert!(expr.is_bin());
    }

    #[test]
    fn parse_block_with_one_stmt() {
        let expr = parse_expr("{ 1; 2 }");
        let block = expr.to_block().unwrap();

        assert_eq!(1, block.stmts.len());

        let expr = &block.stmts[0].to_expr().unwrap().expr;
        assert_eq!(
            &String::from("1"),
            expr.to_lit().unwrap().as_string().unwrap()
        );

        assert_eq!(
            &String::from("2"),
            block
                .expr
                .as_ref()
                .unwrap()
                .to_lit()
                .unwrap()
                .as_string()
                .unwrap()
        );
    }

    #[test]
    fn parse_block_with_multiple_stmts() {
        let expr = parse_expr("{ 1; 2; }");
        let block = expr.to_block().unwrap();

        assert_eq!(2, block.stmts.len());

        let expr = &block.stmts[0].to_expr().unwrap().expr;
        assert_eq!(
            String::from("1"),
            *expr.to_lit().unwrap().as_string().unwrap()
        );

        let expr = &block.stmts[1].to_expr().unwrap().expr;
        assert_eq!(
            String::from("2"),
            *expr.to_lit().unwrap().as_string().unwrap()
        );

        assert!(block.expr.is_none());
    }

    #[test]
    fn parse_return_value() {
        let expr = parse_expr("return 1;");
        let ret = expr.to_return().unwrap();

        assert_eq!(
            String::from("1"),
            *ret.expr
                .as_ref()
                .and_then(|v| v.to_lit())
                .and_then(|v| v.as_string())
                .unwrap()
        );
    }

    #[test]
    fn parse_return() {
        let expr = parse_expr("return;");
        let ret = expr.to_return().unwrap();

        assert!(ret.expr.is_none());
    }

    #[test]
    fn parse_flow_with_multiple_params() {
        let p1 = parse("flow f(a:int, b:str) { }");
        let f1 = p1.flow(0);
    }

    #[test]
    fn parse_flow_with_return_type() {
        let p1 = parse("flow f(a:int, b:str) -> Best { }");
    }

    #[test]
    fn parse_flow_with_nodes_block() {
        let p1 = parse("flow f(a:int, b:str) -> Out { node in: in; }");
    }

    #[test]
    fn parse_flow_with_expressions() {
        let p1 = parse(
            r#"flow f() { 
            node in: in; 
            node out: ret;

            in.then -> out;
        }"#,
        );
    }

    #[test]
    fn parse_flow_with_literals() {
        let p1 = parse(
            r#"flow f() { 
            node in: in; 
            node out: ret;

            1 -> out;
            0.0 -> out.test;
            false -> out.test;
        }"#,
        );
    }

    #[test]
    fn parse_flow_with_blocks() {
        let p1 = parse(
            r#"flow f() { 
            node in: in; 
            node out: ret;

            1 -> (data -> out.d, (10 -> out));
 
            (entry.a -> gt.a, entry.b -> gt.b) -> branch.cond;
        }"#,
        );
    }

    #[test]
    fn parse_struct_empty() {
        let prog = parse("struct Foo {}");
        let struc = prog.struct0();
        assert_eq!(0, struc.fields.len());
        assert_eq!("Foo", struc.name.as_ref().unwrap().name_as_string);
    }

    #[test]
    fn parse_struct_one_field() {
        let prog = parse(
            "struct Bar {
            f1: Foo1,
        }",
        );
        let struc = prog.struct0();
        assert_eq!(1, struc.fields.len());
        assert_eq!("Bar", struc.name.as_ref().unwrap().name_as_string);

        let f1 = &struc.fields[0];
        assert_eq!("f1", f1.name.as_ref().unwrap().name_as_string);
    }

    #[test]
    fn parse_struct_multiple_fields() {
        let prog = parse(
            "struct FooBar {
            fa: Foo1,
            fb: Foo2,
        }",
        );
        let struc = prog.struct0();
        assert_eq!(2, struc.fields.len());
        assert_eq!("FooBar", struc.name.as_ref().unwrap().name_as_string);

        let f1 = &struc.fields[0];
        assert_eq!("fa", f1.name.as_ref().unwrap().name_as_string);

        let f2 = &struc.fields[1];
        assert_eq!("fb", f2.name.as_ref().unwrap().name_as_string);
    }
}
