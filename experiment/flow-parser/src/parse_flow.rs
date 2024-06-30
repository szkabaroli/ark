use std::collections::HashMap;

use serde::{Deserialize, Serialize};
use serde_json::Result;

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone, Hash)]
pub(crate) struct FlowFctIO {
    pub name: String,
    pub r#type: String,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone, Hash)]
pub(crate) struct Variable {
    pub name: String,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone, Hash)]
pub(crate) struct Position {
    pub x: i32,
    pub y: i32,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub(crate) struct FlowNode {
    pub id: String,
    pub r#type: String,
    pub data: HashMap<String, String>,
    pub position: Position,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone, Hash)]
#[serde(rename_all = "camelCase")]
pub(crate) struct FlowEdge {
    pub id: String,
    pub source: String,
    pub source_handle: String,
    pub target: String,
    pub target_handle: String
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub(crate) struct FlowFunction {
    pub name: String,
    pub parameters: Vec<FlowFctIO>,
    pub returns: Vec<FlowFctIO>,
    pub nodes: Vec<FlowNode>,
    pub edges: Vec<FlowEdge>,
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
pub(crate) struct FlowFile {
    pub variables: Vec<Variable>,
    pub functions: Vec<FlowFunction>,
}

pub(crate) fn parse_flow(content: &str) -> Result<FlowFile> {
    serde_json::from_str(content)
}
