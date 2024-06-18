use std::sync::Arc;

use crate::token::TokenKind;

pub type GreenNode = Arc<GreenNodeData>;
pub type GreenToken = Arc<GreenTokenData>;

#[derive(Clone, Debug)]
pub enum GreenElement {
    Node(GreenNode),
    Token(GreenToken),
}

impl GreenElement {
    pub fn kind(&self) -> TokenKind {
        match self {
            GreenElement::Node(ref node) => node.kind(),
            GreenElement::Token(ref token) => token.kind(),
        }
    }

    pub fn len(&self) -> u32 {
        match self {
            GreenElement::Node(ref node) => node.len(),
            GreenElement::Token(ref token) => token.len(),
        }
    }

    pub fn to_node(&self) -> Option<GreenNode> {
        match self {
            GreenElement::Node(ref node) => Some(node.clone()),
            GreenElement::Token(..) => None,
        }
    }

    pub fn to_token(&self) -> Option<GreenToken> {
        match self {
            GreenElement::Node(..) => None,
            GreenElement::Token(ref token) => Some(token.clone()),
        }
    }
}

impl From<GreenNode> for GreenElement {
    fn from(value: GreenNode) -> Self {
        GreenElement::Node(value)
    }
}

impl From<GreenToken> for GreenElement {
    fn from(value: GreenToken) -> Self {
        GreenElement::Token(value)
    }
}

#[derive(Clone, Debug)]
pub struct GreenNodeData {
    pub kind: TokenKind,
    pub len: u32,
    pub children: Vec<GreenElement>,
}

impl GreenNodeData {
    pub fn new(kind: TokenKind, len: u32, children: Vec<GreenElement>) -> GreenNodeData {
        GreenNodeData {
            kind,
            len,
            children,
        }
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    pub fn len(&self) -> u32 {
        self.len
    }

    pub fn children(&self) -> &[GreenElement] {
        &self.children
    }
}

#[derive(Clone, Debug)]
pub struct GreenTokenData {
    kind: TokenKind,
    len: u32,
    value: String,
}

impl GreenTokenData {
    pub fn new(kind: TokenKind, len: u32, value: String) -> GreenTokenData {
        GreenTokenData { kind, len, value }
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    pub fn len(&self) -> u32 {
        self.len
    }

    pub fn value(&self) -> &str {
        self.value.as_str()
    }
}

pub struct GreenTreeBuilder {
    nodes: Vec<(usize, u32)>,
    children: Vec<GreenElement>,
    offset: u32,
}

impl GreenTreeBuilder {
    pub fn new() -> GreenTreeBuilder {
        GreenTreeBuilder {
            nodes: Vec::new(),
            children: Vec::new(),
            offset: 0,
        }
    }

    pub fn start_node(&mut self) {
        self.nodes.push((self.children.len(), self.offset));
    }

    pub fn finish_node(&mut self, kind: TokenKind) -> GreenNode {
        assert!(kind > TokenKind::EOF);
        let (children_start, start) = self.nodes.pop().expect("missing node start");
        self.finish_node_common(kind, children_start, start)
    }

    pub fn finish_node_starting_at(&mut self, kind: TokenKind, marker: Marker) -> GreenNode {
        assert!(kind > TokenKind::EOF);
        let children_start = marker.children;
        let start = marker.offset;
        self.finish_node_common(kind, children_start, start)
    }

    fn finish_node_common(
        &mut self,
        kind: TokenKind,
        children_start: usize,
        start: u32,
    ) -> GreenNode {
        let children = self.children.drain(children_start..).collect::<Vec<_>>();
        let len = self.offset - start;
        let node = Arc::new(GreenNodeData::new(kind, len, children));
        self.children.push(GreenElement::Node(node.clone()));
        node
    }

    pub fn abandon_node(&mut self) {
        self.nodes.pop().expect("missing node start");
    }

    pub fn create_tree(self) -> GreenNode {
        assert_eq!(self.children.len(), 1);
        let child = self.children.into_iter().next().expect("missing element");
        child.to_node().expect("node expected")
    }

    pub fn token(&mut self, kind: TokenKind, value: String) {
        assert!(kind < TokenKind::EOF);
        let len = value.len().try_into().expect("overflow");
        self.offset += len;
        self.children
            .push(Arc::new(GreenTokenData::new(kind, len, value)).into());
    }

    pub fn create_marker(&mut self) -> Marker {
        Marker {
            children: self.children.len(),
            offset: self.offset,
        }
    }
}

#[derive(Clone)]
pub struct Marker {
    children: usize,
    offset: u32,
}
