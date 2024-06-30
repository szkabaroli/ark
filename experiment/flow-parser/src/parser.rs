use std::{collections::HashMap, sync::Arc};

use crate::{
    error::ParseError,
    parse_flow::{parse_flow, FlowEdge, FlowFctIO, FlowFile, FlowFunction, FlowNode},
    tree::TreeBuilder,
};
use multimap::MultiMap;
use parser::{ast, GreenTreeBuilder, NodeId, Span, TokenKind::*};

pub struct Parser {
    next_node_id: usize,
    content: Arc<String>,
    nodes: Vec<(usize, u32)>,
    offset: u32,
    file: FlowFile,
    errors: Vec<ParseError>,
    builder: GreenTreeBuilder,
}

#[derive(Debug)]
enum NodeType {
    Root,
    ForEach,
    IfBranch,
    ElseBranch,
}

#[derive(Debug)]
enum LeafType {
    Inputs,
    Return,
    Call(HashMap<String, String>),
    Const(HashMap<String, String>),
}

impl Parser {
    pub fn from_string(code: &str) -> Parser {
        let content = Arc::new(String::from(code));
        Parser::common_init(content)
    }

    fn common_init(content: Arc<String>) -> Parser {
        let result = parse_flow(&*content).unwrap();

        Parser {
            next_node_id: 0,
            offset: 0,
            content,
            file: result,
            nodes: Vec::new(),
            errors: Vec::new(),
            builder: GreenTreeBuilder::new(),
        }
    }

    pub fn parse(mut self) -> (Arc<ast::File>, Vec<ParseError>) {
        let ast_file = self.parse_file(self.file.clone());
        assert!(self.nodes.is_empty());

        let tree = self.builder.create_tree();

        (Arc::new(ast_file), self.errors)
    }

    fn parse_file(&mut self, file: FlowFile) -> ast::File {
        self.builder.start_node();
        let mut elements = vec![];

        for fct in file.functions.into_iter() {
            elements.push(Arc::new(ast::ElemData::Function(self.parse_function(fct))));
        }

        let green = self.builder.finish_node(SOURCE_FILE);
        ast::File { green, elements }
    }

    fn parse_function(&mut self, fct: FlowFunction) -> Arc<ast::Function> {
        let name = self.parse_identifier(fct.name.clone());
        let params = self.parse_function_params(fct.parameters.clone());
        let return_type = self.parse_function_type(fct.returns.clone());
        let block = self.parse_function_body(fct);

        self.builder.start_node();
        let green = self.builder.finish_node(FN);

        Arc::new(ast::Function {
            id: self.new_node_id(),
            modifiers: None,
            name,
            declaration_span: Span::at(0),
            span: Span::at(0),
            params,
            return_type,
            block,
            green,
        })
    }

    fn parse_identifier(&mut self, value: String) -> Option<ast::Ident> {
        Some(Arc::new(ast::IdentData {
            span: Span::at(0),
            name_as_string: value,
        }))
    }

    fn parse_function_params(&mut self, params: Vec<FlowFctIO>) -> Vec<ast::Param> {
        let mut data = vec![];

        for param in params {
            data.push(self.parse_param(param));
        }

        data
    }

    fn parse_param(&mut self, io: FlowFctIO) -> ast::Param {
        let data_type = self.parse_type(io.r#type);

        ast::Param {
            id: self.new_node_id(),
            variadic: false, // we not support variadic in flows
            name: self.parse_identifier(io.name),
            span: Span::at(0),
            mutable: false,
            data_type,
        }
    }

    fn parse_type(&mut self, ty: String) -> ast::Type {
        Arc::new(ast::TypeData::Unknown {
            id: self.new_node_id(),
            span: Span::at(0),
        })
    }

    fn parse_function_type(&mut self, params: Vec<FlowFctIO>) -> Option<ast::Type> {
        if params.len() == 1 {
            Some(self.parse_type(params[0].r#type.clone()))
        } else {
            unimplemented!()
        }
    }

    fn parse_function_body(&mut self, fct: FlowFunction) -> Option<ast::Expr> {
        let starter_node = fct
            .nodes
            .iter()
            .find(|s| s.r#type == "input")
            .unwrap()
            .clone();
        let starter_edge = fct
            .edges
            .iter()
            .find(|e| e.source == starter_node.id)
            .unwrap()
            .clone();

        let source_edge_map: MultiMap<String, FlowEdge> = MultiMap::from_iter(
            fct.edges
                .iter()
                .filter(|n| n.source_handle.starts_with('$'))
                .map(|e| (e.source.clone(), e.clone())),
        );

        let target_edge_map: MultiMap<String, FlowEdge> = MultiMap::from_iter(
            fct.edges
                .iter()
                .filter(|n| n.source_handle.starts_with('$'))
                .map(|e| (e.target.clone(), e.clone())),
        );

        let mut node_map: HashMap<String, FlowNode> =
            HashMap::from_iter(fct.nodes.iter().map(|n| (n.id.clone(), n.clone())));

        let mut builder = TreeBuilder::new();

        builder.start_node(NodeType::Root);
        builder.leaf(LeafType::Inputs);
        self.recurse_scope(
            &mut builder,
            &starter_node,
            &mut node_map,
            &source_edge_map,
            &target_edge_map,
        );
        let root = builder.finish_node();

        println!("{:#?}", root);

        None
    }

    fn recurse_scope(
        &mut self,
        builder: &mut TreeBuilder<NodeType, LeafType>,
        node: &FlowNode,
        node_map: &mut HashMap<String, FlowNode>,
        source_edge_map: &MultiMap<String, FlowEdge>,
        target_edge_map: &MultiMap<String, FlowEdge>,
    ) {
        if let Some(outbounds) = source_edge_map.clone().get_vec(&node.id) {
            println!(
                "all out {}: {:?}",
                node.id,
                outbounds
                    .iter()
                    .map(|o| format!(
                        "{}:{} -> {}:{}",
                        o.source, o.source_handle, o.target, o.target_handle
                    ))
                    .collect::<Vec<String>>()
            );

            for outbound in outbounds.iter() {
                println!(
                    "iter outbound: {}",
                    format!(
                        "{}:{} -> {}:{}",
                        outbound.source,
                        outbound.source_handle,
                        outbound.target,
                        outbound.target_handle
                    )
                );

                let target_node = node_map.get(&outbound.target).clone().unwrap().clone();

                match outbound.source_handle.as_str() {
                    "$flow" => {
                        if target_node.r#type == "call_fn" {
                            builder.leaf(LeafType::Call(target_node.data.clone()));
                        }
                        if target_node.r#type == "const" {
                            builder.leaf(LeafType::Const(target_node.data.clone()));
                        }
                        if target_node.r#type == "output" {
                            builder.leaf(LeafType::Return);
                        }

                        self.recurse_scope(
                            builder,
                            &target_node,
                            node_map,
                            source_edge_map,
                            target_edge_map,
                        );
                    }
                    "$each" => {
                        builder.start_node(NodeType::ForEach);
                        if target_node.r#type == "call_fn" {
                            builder.leaf(LeafType::Call(target_node.data.clone()));
                        }
                        if target_node.r#type == "const" {
                            builder.leaf(LeafType::Const(target_node.data.clone()));
                        }
                        if target_node.r#type == "output" {
                            builder.leaf(LeafType::Return);
                        }
                        self.recurse_scope(
                            builder,
                            &target_node,
                            node_map,
                            source_edge_map,
                            target_edge_map,
                        );
                        builder.finish_node();
                    }
                    "$if" => {
                        builder.start_node(NodeType::IfBranch);
                        if target_node.r#type == "call_fn" {
                            builder.leaf(LeafType::Call(target_node.data.clone()));
                        }
                        if target_node.r#type == "const" {
                            builder.leaf(LeafType::Const(target_node.data.clone()));
                        }
                        if target_node.r#type == "output" {
                            builder.leaf(LeafType::Return);
                        }
                        self.recurse_scope(
                            builder,
                            &target_node,
                            node_map,
                            source_edge_map,
                            target_edge_map,
                        );
                        builder.finish_node();
                    }
                    "$else" => {
                        builder.start_node(NodeType::ElseBranch);
                        if target_node.r#type == "call_fn" {
                            builder.leaf(LeafType::Call(target_node.data.clone()));
                        }
                        if target_node.r#type == "const" {
                            builder.leaf(LeafType::Const(target_node.data.clone()));
                        }
                        if target_node.r#type == "output" {
                            builder.leaf(LeafType::Return);
                        }
                        self.recurse_scope(
                            builder,
                            &target_node,
                            node_map,
                            source_edge_map,
                            target_edge_map,
                        );
                        builder.finish_node();
                    }
                    _ => unimplemented!(),
                }
            }
        }
    }

    fn new_node_id(&mut self) -> NodeId {
        let value = self.next_node_id;
        self.next_node_id += 1;
        NodeId(value)
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::ast;
    use super::Parser;
    use serde_json::Result;

    fn parse(code: &str) -> Arc<ast::File> {
        let (file, errors) = Parser::from_string(code).parse();
        assert!(errors.is_empty());
        file
    }

    #[test]
    fn for_each() -> Result<()> {
        let data: String = std::fs::read_to_string("../examples/for_each.json").unwrap();
        parse(data.as_str());
        Ok(())
    }

    #[test]
    fn branch() -> Result<()> {
        let data: String = std::fs::read_to_string("../examples/branch.json").unwrap();
        parse(data.as_str());
        Ok(())
    }
}
