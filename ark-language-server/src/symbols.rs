use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use crossbeam::channel::Sender;
use lsp_server::{Message, Request, Response};
use lsp_types::*;
use parser::ast::visit::Visitor;

use parser::ast::{self, visit};
use parser::{compute_line_column, compute_line_starts, ParseErrorWithLocation, Parser, Span};

use crate::{MainLoopTask, ServerState};

pub(super) fn document_hover_request(server_state: &mut ServerState, request: Request) {
    eprintln!("got hover request on main thread");
    let result = serde_json::from_value::<lsp_types::HoverParams>(request.params);
    
    match result {
        Ok(result) => {
            let sender = server_state.threadpool_sender.clone();
            let char = result.text_document_position_params.position.character;
            let line = result.text_document_position_params.position.line;

            server_state.threadpool.execute(move || {
                let mut value = String::new();
                value += &format!("ln {}, col: {}", line, char);

                let test_hover = Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value,
                    }),
                    range: None,
                };

                let response =
                    Response::new_ok(request.id, serde_json::to_value(test_hover).unwrap());
                
                sender
                    .send(MainLoopTask::SendResponse(Message::Response(response)))
                    .expect("send failed");
            });
        }
        Err(_) => {
            eprintln!("broken params");
        }
    }
}

pub(super) fn document_symbol_request(state: &mut ServerState, request: Request) {
    eprintln!("got documentSymbol request on main thread");
    let result = serde_json::from_value::<lsp_types::DocumentSymbolParams>(request.params);
    
    match result {
        Ok(result) => {
            let path = result
                .text_document
                .uri
                .to_file_path()
                .expect("file path expected");
            
            if let Some(content) = state.opened_files.get(&path) {
                let content = content.clone();
                eprintln!(
                    "got file for {} with {} lines",
                    path.display(),
                    content.lines().count()
                );

                let sender = state.threadpool_sender.clone();

                let file = state.parsed_files.get(&path).cloned().unwrap();

                state.threadpool.execute(move || {
                    eprintln!("parse file on background thread.");
                    let symbols = scan_for_symbols(content, file);
                    eprintln!("parse done on background thread.");
                    let response = DocumentSymbolResponse::Nested(symbols);
                    let response = Response::new_ok(request.id, response);

                    sender
                        .send(MainLoopTask::SendResponse(Message::Response(response)))
                        .expect("send failed");
                });
            } else {
                eprintln!("unknown file {}", path.display());
            }
        }
        Err(_) => {
            eprintln!("broken params");
        }
    }
}

pub(crate) fn scan_for_symbols(content: Arc<String>, file: Arc<ast::File>) -> Vec<DocumentSymbol> {
    let line_starts = compute_line_starts(&content);

    let mut scanner = SymbolScanner {
        symbols: Vec::new(),
        levels: Vec::new(),
        content,
    };

    scanner.visit_file(&file);
    transform(&line_starts, scanner.symbols)
}

fn transform(line_starts: &[u32], symbols: Vec<Symbol>) -> Vec<DocumentSymbol> {
    symbols
        .into_iter()
        .map(|s| {
            let range = range_from_span(line_starts, s.total_span);
            let selection_range = range_from_span(line_starts, s.name_span);

            let children = s.children.map(|c| transform(line_starts, c));

            #[allow(deprecated)]
            DocumentSymbol {
                name: s.name,
                kind: convert_kind(s.kind),
                tags: None,
                detail: None,
                range,
                deprecated: None,
                selection_range,
                children,
            }
        })
        .collect()
}

fn convert_kind(kind: ArkSymbolKind) -> SymbolKind {
    match kind {
        ArkSymbolKind::Function => SymbolKind::FUNCTION,
        ArkSymbolKind::Class => SymbolKind::CLASS,
        ArkSymbolKind::ClassField => SymbolKind::FIELD,
        ArkSymbolKind::Const => SymbolKind::CONSTANT,
        ArkSymbolKind::Struct => SymbolKind::STRUCT,
        ArkSymbolKind::StructField => SymbolKind::FIELD,
        ArkSymbolKind::Enum => SymbolKind::ENUM,
        ArkSymbolKind::EnumVariant => SymbolKind::ENUM_MEMBER,
        ArkSymbolKind::Impl => SymbolKind::NAMESPACE,
        ArkSymbolKind::Module => SymbolKind::NAMESPACE,
        ArkSymbolKind::Trait => SymbolKind::INTERFACE,
        ArkSymbolKind::Global => SymbolKind::VARIABLE,
    }
}

fn range_from_span(line_starts: &[u32], span: Span) -> Range {
    let start = position_from_offset(line_starts, span.start());
    let end = position_from_offset(line_starts, span.end());

    Range { start, end }
}

fn position_from_offset(line_starts: &[u32], offset: u32) -> Position {
    let (line, column) = compute_line_column(&line_starts, offset);
    Position::new(line - 1, column - 1)
}

#[derive(Debug)]
struct Symbol {
    name: String,
    name_span: Span,
    kind: ArkSymbolKind,
    total_span: Span,
    children: Option<Vec<Symbol>>,
}

#[derive(Debug)]
enum ArkSymbolKind {
    Function,
    Class,
    ClassField,
    Const,
    Struct,
    StructField,
    Enum,
    EnumVariant,
    Impl,
    Module,
    Trait,
    Global,
}

#[derive(Debug)]
struct SymbolScanner {
    symbols: Vec<Symbol>,
    levels: Vec<usize>,
    content: Arc<String>,
}

impl SymbolScanner {
    fn add_symbol(&mut self, name: String, name_span: Span, kind: ArkSymbolKind, total_span: Span) {
        self.symbols.push(Symbol {
            name,
            name_span,
            kind,
            total_span,
            children: None,
        });
    }

    fn start_children(&mut self) {
        self.levels.push(self.symbols.len() - 1);
    }

    fn stop_children(&mut self) {
        let parent = self.levels.pop().expect("missing start");
        let children = self.symbols.drain(parent + 1..).collect();
        self.symbols.last_mut().expect("missing parent").children = Some(children);
    }
}

impl visit::Visitor for SymbolScanner {
    //fn visit_module(&mut self, node: &Arc<ast::Module>) {
    //    let (name, name_span) = ensure_name(&node.name, "<mod>", node.span);
    //    self.add_symbol(name, name_span, ArkSymbolKind::Module, node.span);

    //    self.start_children();
    //    visit::walk_module(self, node);
    //    self.stop_children();
    //}

    //fn visit_trait(&mut self, node: &Arc<ast::Trait>) {
    //    let (name, name_span) = ensure_name(&node.name, "<trait>", node.span);
    //    self.add_symbol(name, name_span, ArkSymbolKind::Trait, node.span);
    //}

    //fn visit_global(&mut self, node: &Arc<ast::Global>) {
    //    let (name, name_span) = ensure_name(&node.name, "<global>", node.span);
    //    self.add_symbol(name, name_span, ArkSymbolKind::Global, node.span);
    //}

    //fn visit_impl(&mut self, node: &Arc<ast::Impl>) {
    //    let mut name: String = "impl".into();

    //    if let Some(ref type_params) = node.type_params {
    //        let span = type_params.span;
    //        let type_params_string =
    //            &self.content.as_str()[span.start() as usize..span.end() as usize];
    //        name.push_str(type_params_string);
    //    }

    //    if let Some(ref trait_ty) = node.trait_type {
    //        let span = trait_ty.span();
    //        let trait_ty_string =
    //            &self.content.as_str()[span.start() as usize..span.end() as usize];
    //        name.push_str(" ");
    //        name.push_str(trait_ty_string);
    //        name.push_str(" for");
    //    }

    //    let name_span = node.extended_type.span();
    //    let extended_type_string =
    //        &self.content.as_str()[name_span.start() as usize..name_span.end() as usize];
    //    name.push_str(" ");
    //    name.push_str(extended_type_string);
    //    self.add_symbol(name, name_span, ArkSymbolKind::Impl, node.span);

    //    self.start_children();
    //    visit::walk_impl(self, node);
    //    self.stop_children();
    //}

    //fn visit_const(&mut self, node: &Arc<ast::Const>) {
    //    let (name, name_span) = ensure_name(&node.name, "<const>", node.span);
    //    self.add_symbol(name, name_span, ArkSymbolKind::Const, node.span);
    //}

    //fn visit_class(&mut self, node: &Arc<ast::Class>) {
    //    let (name, name_span) = ensure_name(&node.name, "<class>", node.span);
    //    self.add_symbol(name, name_span, ArkSymbolKind::Class, node.span);

    //    self.start_children();
    //    for field in &node.fields {
    //        let (name, name_span) = ensure_name(&field.name, "<field>", field.span);
    //        self.add_symbol(name, name_span, ArkSymbolKind::ClassField, field.span);
    //    }
    //    self.stop_children();
    //}

    //fn visit_struct(&mut self, node: &Arc<ast::Struct>) {
    //    let (name, name_span) = ensure_name(&node.name, "<struct>", node.span);
    //    self.add_symbol(name, name_span, ArkSymbolKind::Struct, node.span);

    //    self.start_children();
    //    for field in &node.fields {
    //        let (name, name_span) = ensure_name(&field.name, "<field>", field.span);
    //        self.add_symbol(name, name_span, ArkSymbolKind::StructField, field.span);
    //    }
    //    self.stop_children();
    //}

    fn visit_fct(&mut self, node: &Arc<ast::Function>) {
        let (name, name_span) = ensure_name(&node.name, "<fn>", node.span);
        self.add_symbol(name, name_span, ArkSymbolKind::Function, node.span);
    }

    //fn visit_method(&mut self, node: &Arc<ast::Function>) {
    //    let (name, name_span) = ensure_name(&node.name, "<fn>", node.span);
    //    self.add_symbol(name, name_span, ArkSymbolKind::Function, node.span);
    //}

    //fn visit_enum(&mut self, node: &Arc<ast::Enum>) {
    //    let (name, name_span) = ensure_name(&node.name, "<fn>", node.span);
    //    self.add_symbol(name, name_span, ArkSymbolKind::Enum, node.span);

    //    self.start_children();
    //    for variant in &node.variants {
    //        let (name, name_span) = ensure_name(&variant.name, "<enum member>", variant.span);
    //        self.add_symbol(name, name_span, ArkSymbolKind::EnumVariant, node.span);
    //    }
    //    self.stop_children();
    //}
}

fn ensure_name(
    ident: &Option<ast::Ident>,
    default_name: &str,
    default_span: Span,
) -> (String, Span) {
    if let Some(ident) = ident {
        (ident.name_as_string.clone(), ident.span)
    } else {
        (default_name.into(), default_span)
    }
}
