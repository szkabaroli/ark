use std::sync::Arc;

use crate::{
    ast::{
        self, BinOp, CmpOp, Elem, ElemData, Expr, ExprData, FlowExpr, FlowExprData, FlowStmtData,
        Ident, IdentData, ModifierList, Param, Path, PathData, Stmt, StmtData, Type, TypeData,
        UnOp,
    },
    green::{GreenTreeBuilder, Marker},
    lex,
    token::{TokenSet, ELEM_FIRST, EMPTY, EXPRESSION_FIRST, FLOW_EXPRESSION_FIRST, PARAM_LIST_RS},
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

pub struct Parser {
    tokens: Vec<TokenKind>,
    token_widths: Vec<u32>,
    token_idx: usize,
    next_node_id: usize,
    content: Arc<String>,
    errors: Vec<ParseErrorWithLocation>,
    nodes: Vec<(usize, u32)>,
    offset: u32,
    builder: GreenTreeBuilder,
}

impl Parser {
    pub fn from_string(code: &'static str) -> Parser {
        let content = Arc::new(String::from(code));
        Parser::common_init(content)
    }

    pub fn from_shared_string(content: Arc<String>) -> Parser {
        Parser::common_init(content)
    }

    fn common_init(content: Arc<String>) -> Parser {
        let result = lex(&*content);

        Parser {
            tokens: result.tokens,
            token_widths: result.widths,
            token_idx: 0,
            next_node_id: 0,
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
        ast::File { green, elements }
    }

    fn parse_element(&mut self) -> Elem {
        self.builder.start_node();

        //let modifiers = self.parse_modifiers();

        match self.current() {
            FN_KW => {
                let fct = self.parse_function(None);
                Arc::new(ElemData::Function(fct))
            }

            FLOW_KW => {
                let flow = self.parse_flow(None);
                Arc::new(ElemData::Flow(flow))
            }

            /*CLASS_KW => {
                let class = self.parse_class(modifiers);
                Arc::new(ElemData::Class(class))
            }*/

            /*STRUCT_KW => {
                let struc = self.parse_struct(modifiers);
                Arc::new(ElemData::Struct(struc))
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

            /*USE_KW => {
                let use_stmt = self.parse_use(modifiers);
                Arc::new(ElemData::Use(use_stmt))
            }*/

            /*EXTERN_KW => {
                let extern_stmt = self.parse_extern(modifiers);
                Arc::new(ElemData::Extern(extern_stmt))
            }*/

            /*TYPE_KW => {
                let type_alias = self.parse_type_alias(modifiers);
                Arc::new(ElemData::TypeAlias(type_alias))
            }*/
            _ => {
                //assert!(!ELEM_FIRST.contains(self.current()));
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
            Span::new(self.offset, length)
        } else {
            Span::at(self.offset)
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
        assert!(self.eat(kind));
    }

    fn expect_identifier(&mut self) -> Option<Ident> {
        let span = self.current_span();

        if self.is(IDENTIFIER) {
            self.assert(IDENTIFIER);
            let value = self.source_span(span);

            Some(Arc::new(IdentData {
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
        let value = self.next_node_id;
        self.next_node_id += 1;
        NodeId(value)
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

        Span::new(start_offset, end_offset - start_offset)
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

    fn parse_flow(&mut self, modifiers: Option<ModifierList>) -> Arc<ast::Flow> {
        let start = self.current_span().start();
        self.start_node();
        self.assert(FLOW_KW);
        let name = self.expect_identifier();
        let params = self.parse_function_params();
        let return_type = self.parse_function_type();
        let block = self.parse_flow_block();

        let declaration_span = self.span_from(start);

        let green = self.builder.finish_node(FLOW);

        Arc::new(ast::Flow {
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

    fn parse_function(&mut self, modifiers: Option<ModifierList>) -> Arc<ast::Function> {
        let start = self.current_span().start();
        self.start_node();
        self.assert(FN_KW);
        let name = self.expect_identifier();
        let params = self.parse_function_params();
        let return_type = self.parse_function_type();
        let declaration_span = self.span_from(start);
        let block = self.parse_function_body();

        let green = self.builder.finish_node(FN);

        Arc::new(ast::Function {
            id: self.new_node_id(),
            modifiers: modifiers.clone(),
            name,
            declaration_span,
            span: self.finish_node(),
            params,
            return_type,
            block,
            green,
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
            let block = Arc::from(ExprData::Block(self.parse_block()));
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
            // LET_KW => StmtOrExpr::Stmt(self.parse_let()),
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
            return Arc::new(ExprData::Error {
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
                //AS_KW | IS_KW => 7,
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

                Arc::new(ExprData::create_un(
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

                    Arc::new(ExprData::create_dot(
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

                    Arc::new(ExprData::create_path(
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

        Arc::new(ExprData::create_call(self.new_node_id(), span, left, args))
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
            EQ => BinOp::Assign,
            OR_OR => BinOp::Or,
            AND_AND => BinOp::And,
            EQ_EQ => BinOp::Cmp(CmpOp::Eq),
            NOT_EQ => BinOp::Cmp(CmpOp::Ne),
            LT => BinOp::Cmp(CmpOp::Lt),
            LE => BinOp::Cmp(CmpOp::Le),
            GT => BinOp::Cmp(CmpOp::Gt),
            GE => BinOp::Cmp(CmpOp::Ge),
            EQ_EQ_EQ => BinOp::Cmp(CmpOp::Is),
            NOT_EQ_EQ => BinOp::Cmp(CmpOp::IsNot),
            OR => BinOp::BitOr,
            AND => BinOp::BitAnd,
            CARET => BinOp::BitXor,
            ADD => BinOp::Add,
            SUB => BinOp::Sub,
            MUL => BinOp::Mul,
            DIV => BinOp::Div,
            MODULO => BinOp::Mod,
            LT_LT => BinOp::ShiftL,
            GT_GT => BinOp::ArithShiftR,
            GT_GT_GT => BinOp::LogicalShiftR,
            _ => panic!("unimplemented token {:?}", kind),
        };

        let span = self.span_from(start);

        Arc::new(ExprData::create_bin(
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

    fn parse_lit_bool(&mut self) -> ast::ExprLitBoolType {
        self.builder.start_node();
        let span = self.current_span();
        let kind = self.current();
        self.assert(kind);
        let value = kind == TRUE;
        self.builder.finish_node(BOOL_LIT_EXPR);

        ast::ExprLitBoolType {
            id: self.new_node_id(),
            span,
            value,
        }
    }

    fn parse_lit_int(&mut self) -> ast::ExprLitIntType {
        let span = self.current_span();
        self.builder.start_node();
        self.assert(INT_LITERAL);
        let value = self.source_span(span);

        let green = self.builder.finish_node(INT_LIT_EXPR);

        ast::ExprLitIntType {
            id: self.new_node_id(),
            span,
            green,
            value,
        }
    }

    fn parse_lit_float(&mut self) -> ast::ExprLitFloatType {
        let span = self.current_span();
        self.builder.start_node();
        self.assert(FLOAT_LITERAL);
        let value = self.source_span(span);

        let green = self.builder.finish_node(FLOAT_LIT_EXPR);

        ast::ExprLitFloatType {
            id: self.new_node_id(),
            span,
            green,
            value,
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

    fn parse_parentheses(&mut self) -> ast::ExprParenType {
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

            ast::ExprParenType {
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
            FLOAT_LITERAL => ast::FlowExprData::LitFloat(self.parse_lit_float()),
            INT_LITERAL => ast::FlowExprData::LitInt(self.parse_lit_int()),
            TRUE => ast::FlowExprData::LitBool(self.parse_lit_bool()),
            FALSE => ast::FlowExprData::LitBool(self.parse_lit_bool()),
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
            L_PAREN => ExprData::Paren(self.parse_parentheses()),
            L_BRACE => ExprData::Block(self.parse_block()),
            INT_LITERAL => ExprData::LitInt(self.parse_lit_int()),
            FLOAT_LITERAL => ExprData::LitFloat(self.parse_lit_float()),
            IDENTIFIER => ExprData::Ident(self.parse_identifier()),
            TRUE => ExprData::LitBool(self.parse_lit_bool()),
            FALSE => ExprData::LitBool(self.parse_lit_bool()),
            RETURN_KW => ExprData::Return(self.parse_return()),
            _ => {
                self.report_error(ParseError::ExpectedFactor);
                ExprData::Error {
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
        Span::new(start, self.offset - start)
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
    use crate::{ast::*, compute_line_column, compute_line_starts};
    use std::sync::Arc;

    use crate::error::ParseError;
    use crate::parser::Parser;

    fn parse_expr(code: &'static str) -> Expr {
        let mut parser = Parser::from_string(code);

        let result = parser.parse_expression();
        assert!(parser.errors.is_empty());

        result
    }

    fn parse(code: &'static str) -> Arc<File> {
        let (file, errors) = Parser::from_string(code).parse();
        println!("{:#?}", errors);
        assert!(errors.is_empty());
        file
    }

    fn err_expr(code: &'static str, msg: ParseError, line: u32, col: u32) {
        let mut parser = Parser::from_string(code);

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

        let lit = expr.to_lit_int().unwrap();
        assert_eq!(String::from("10"), lit.value);
    }

    #[test]
    fn parse_neg() {
        let expr = parse_expr("-1");

        let un = expr.to_un().unwrap();
        assert_eq!(UnOp::Neg, un.op);

        assert!(un.opnd.is_lit_int());
    }

    #[test]
    fn parse_neg_twice() {
        let expr = parse_expr("-(-3)");

        let neg1 = expr.to_un().unwrap();
        assert_eq!(UnOp::Neg, neg1.op);

        let neg2 = neg1.opnd.to_paren().unwrap().expr.to_un().unwrap();
        assert_eq!(UnOp::Neg, neg2.op);

        assert!(neg2.opnd.is_lit_int());
    }

    #[test]
    fn parse_neg_twice_without_parentheses() {
        err_expr("- -2", ParseError::ExpectedFactor, 1, 3);
    }

    #[test]
    fn parse_mul() {
        let expr = parse_expr("6*3");

        let mul = expr.to_bin().unwrap();
        assert_eq!(BinOp::Mul, mul.op);
        assert_eq!(String::from("6"), mul.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("3"), mul.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_multiple_muls() {
        let expr = parse_expr("6*3*4");

        let mul1 = expr.to_bin().unwrap();
        assert_eq!(BinOp::Mul, mul1.op);

        let mul2 = mul1.lhs.to_bin().unwrap();
        assert_eq!(BinOp::Mul, mul2.op);
        assert_eq!(String::from("6"), mul2.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("3"), mul2.rhs.to_lit_int().unwrap().value);

        assert_eq!(String::from("4"), mul1.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_div() {
        let expr = parse_expr("4/5");

        let div = expr.to_bin().unwrap();
        assert_eq!(BinOp::Div, div.op);
        assert_eq!(String::from("4"), div.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("5"), div.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_mod() {
        let expr = parse_expr("2%15");

        let div = expr.to_bin().unwrap();
        assert_eq!(BinOp::Mod, div.op);
        assert_eq!(String::from("2"), div.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("15"), div.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_add() {
        let expr = parse_expr("2+3");

        let add = expr.to_bin().unwrap();
        assert_eq!(BinOp::Add, add.op);
        assert_eq!(String::from("2"), add.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("3"), add.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_add_left_associativity() {
        let expr = parse_expr("1+2+3");

        let add = expr.to_bin().unwrap();
        assert_eq!(String::from("3"), add.rhs.to_lit_int().unwrap().value);

        let lhs = add.lhs.to_bin().unwrap();
        assert_eq!(String::from("1"), lhs.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), lhs.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_add_right_associativity_via_parens() {
        let expr = parse_expr("1+(2+3)");

        let add = expr.to_bin().unwrap();
        assert_eq!(String::from("1"), add.lhs.to_lit_int().unwrap().value);

        let rhs = add.rhs.to_paren().unwrap().expr.to_bin().unwrap();
        assert_eq!(String::from("2"), rhs.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("3"), rhs.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_sub() {
        let expr = parse_expr("1-2");

        let add = expr.to_bin().unwrap();
        assert_eq!(BinOp::Sub, add.op);
        assert_eq!(String::from("1"), add.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), add.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_or() {
        let expr = parse_expr("1||2");

        let add = expr.to_bin().unwrap();
        assert_eq!(BinOp::Or, add.op);
        assert_eq!(String::from("1"), add.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), add.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_and() {
        let expr = parse_expr("1&&2");

        let add = expr.to_bin().unwrap();
        assert_eq!(BinOp::And, add.op);
        assert_eq!(String::from("1"), add.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), add.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_bit_or() {
        let expr = parse_expr("1|2");

        let or = expr.to_bin().unwrap();
        assert_eq!(BinOp::BitOr, or.op);
        assert_eq!(String::from("1"), or.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), or.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_bit_and() {
        let expr = parse_expr("1&2");

        let and = expr.to_bin().unwrap();
        assert_eq!(BinOp::BitAnd, and.op);
        assert_eq!(String::from("1"), and.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), and.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_bit_xor() {
        let expr = parse_expr("1^2");

        let xor = expr.to_bin().unwrap();
        assert_eq!(BinOp::BitXor, xor.op);
        assert_eq!(String::from("1"), xor.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), xor.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_lt() {
        let expr = parse_expr("1<2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::Lt), cmp.op);
        assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_le() {
        let expr = parse_expr("1<=2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::Le), cmp.op);
        assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_gt() {
        let expr = parse_expr("1>2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::Gt), cmp.op);
        assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_ge() {
        let expr = parse_expr("1>=2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::Ge), cmp.op);
        assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_eq() {
        let expr = parse_expr("1==2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::Eq), cmp.op);
        assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_ne() {
        let expr = parse_expr("1!=2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::Ne), cmp.op);
        assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_identity_not() {
        let expr = parse_expr("1!==2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::IsNot), cmp.op);
        assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_identity() {
        let expr = parse_expr("1===2");

        let cmp = expr.to_bin().unwrap();
        assert_eq!(BinOp::Cmp(CmpOp::Is), cmp.op);
        assert_eq!(String::from("1"), cmp.lhs.to_lit_int().unwrap().value);
        assert_eq!(String::from("2"), cmp.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_true() {
        let expr = parse_expr("true");

        let lit = expr.to_lit_bool().unwrap();
        assert_eq!(true, lit.value);
    }

    #[test]
    fn parse_false() {
        let expr = parse_expr("true");

        let lit = expr.to_lit_bool().unwrap();
        assert_eq!(true, lit.value);
    }

    #[test]
    fn parse_assign() {
        let expr = parse_expr("a=4");

        let assign = expr.to_bin().unwrap();
        assert!(assign.lhs.is_ident());
        assert_eq!(BinOp::Assign, assign.op);
        assert_eq!(String::from("4"), assign.rhs.to_lit_int().unwrap().value);
    }

    #[test]
    fn parse_shift_right() {
        let expr = parse_expr("a>>4");

        let bin = expr.to_bin().unwrap();
        assert_eq!(BinOp::ArithShiftR, bin.op);
    }

    #[test]
    fn parse_unsigned_shift_right() {
        let expr = parse_expr("a>>>4");

        let bin = expr.to_bin().unwrap();
        assert_eq!(BinOp::LogicalShiftR, bin.op);
    }

    #[test]
    fn parse_left() {
        let expr = parse_expr("a<<4");

        let bin = expr.to_bin().unwrap();
        assert_eq!(BinOp::ShiftL, bin.op);
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
        assert_eq!(0, fct.params.len());
        assert!(fct.return_type.is_none());
    }

    #[test]
    fn parse_function_with_single_param() {
        let p1 = parse("fn f(a:int) { }");
        let f1 = p1.fct0();

        let p2 = parse("fn f(a:int,) { }");
        let f2 = p2.fct0();

        assert_eq!(f1.params.len(), 1);
        assert_eq!(f2.params.len(), 1);

        let p1 = &f1.params[0];
        let p2 = &f2.params[0];

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

        let p1a = &f1.params[0];
        let p1b = &f1.params[1];
        let p2a = &f2.params[0];
        let p2b = &f2.params[1];

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
        assert!(expr.to_block().unwrap().expr.as_ref().unwrap().is_lit_int());

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
        assert_eq!(String::from("1"), expr.to_lit_int().unwrap().value);

        assert_eq!(
            String::from("2"),
            block.expr.as_ref().unwrap().to_lit_int().unwrap().value
        );
    }

    #[test]
    fn parse_block_with_multiple_stmts() {
        let expr = parse_expr("{ 1; 2; }");
        let block = expr.to_block().unwrap();

        assert_eq!(2, block.stmts.len());

        let expr = &block.stmts[0].to_expr().unwrap().expr;
        assert_eq!(String::from("1"), expr.to_lit_int().unwrap().value);

        let expr = &block.stmts[1].to_expr().unwrap().expr;
        assert_eq!(String::from("2"), expr.to_lit_int().unwrap().value);

        assert!(block.expr.is_none());
    }

    #[test]
    fn parse_return_value() {
        let expr = parse_expr("return 1;");
        let ret = expr.to_return().unwrap();

        assert_eq!(
            String::from("1"),
            ret.expr.as_ref().unwrap().to_lit_int().unwrap().value
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
        let f1 = p1.flow(0);
    }

    #[test]
    fn parse_flow_with_nodes_block() {
        let p1 = parse("flow f(a:int, b:str) -> Out { node in: in; }");
        let f1 = p1.flow(0);
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
        let f1 = p1.flow(0);
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
        let f1 = p1.flow(0);
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
        let f1 = p1.flow(0);
        println!("{:#?}", f1);
    }
}
