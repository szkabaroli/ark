use std::sync::Arc;

pub type TreeNode<N, T> = Arc<TreeNodeData<N, T>>;
pub type TreeLeaf<T> = Arc<TreeLeafData<T>>;

#[derive(Clone, Debug)]
pub enum TreeElement<N, T> {
    Node(TreeNode<N, T>),
    Leaf(TreeLeaf<T>),
}

impl<N, T> TreeElement<N, T> {
    pub fn to_node(&self) -> Option<TreeNode<N, T>> {
        match self {
            TreeElement::Node(ref node) => Some(node.clone()),
            TreeElement::Leaf(..) => None,
        }
    }

    pub fn to_leaf(&self) -> Option<TreeLeaf<T>> {
        match self {
            TreeElement::Node(..) => None,
            TreeElement::Leaf(ref token) => Some(token.clone()),
        }
    }
}

impl<N, T> From<TreeNode<N, T>> for TreeElement<N, T> {
    fn from(value: TreeNode<N, T>) -> Self {
        TreeElement::Node(value)
    }
}

impl<N, T> From<TreeLeaf<T>> for TreeElement<N, T> {
    fn from(value: TreeLeaf<T>) -> Self {
        TreeElement::Leaf(value)
    }
}

#[derive(Clone, Debug)]
pub struct TreeNodeData<N, T> {
    // pub len: u32,
    pub value: N,
    pub children: Vec<TreeElement<N, T>>,
}

impl<N, T> TreeNodeData<N, T> {
    pub fn new(value: N, children: Vec<TreeElement<N, T>>) -> TreeNodeData<N, T> {
        TreeNodeData { value, children }
    }

    pub fn children(&self) -> &[TreeElement<N, T>] {
        &self.children
    }
}

#[derive(Clone, Debug)]
pub struct TreeLeafData<T> {
    // kind: TokenKind,
    value: T,
}

impl<T> TreeLeafData<T> {
    pub fn new(value: T) -> TreeLeafData<T> {
        TreeLeafData { value }
    }

    pub fn value(&self) -> &T {
        &self.value
    }
}

pub struct TreeBuilder<N, T> {
    nodes: Vec<(usize, N)>,
    children: Vec<TreeElement<N, T>>,
}

impl<N, T> TreeBuilder<N, T> {
    pub fn new() -> TreeBuilder<N, T> {
        TreeBuilder {
            nodes: Vec::new(),
            children: Vec::new(),
        }
    }

    pub fn start_node(&mut self, val: N) {
        self.nodes.push((self.children.len(), val));
    }

    pub fn finish_node(&mut self) -> TreeNode<N, T> {
        let (children_start, value) = self.nodes.pop().expect("missing node start");
        self.finish_node_common(children_start, value)
    }

    fn finish_node_common(&mut self, children_start: usize, value: N) -> TreeNode<N, T> {
        let children = self.children.drain(children_start..).collect::<Vec<_>>();

        let node = Arc::new(TreeNodeData::new(value, children));
        self.children.push(TreeElement::Node(node.clone()));
        node
    }

    pub fn abandon_node(&mut self) {
        self.nodes.pop().expect("missing node start");
    }

    pub fn create_tree(self) -> TreeNode<N, T> {
        assert_eq!(self.children.len(), 1);
        let child = self.children.into_iter().next().expect("missing element");
        child.to_node().expect("node expected")
    }

    pub fn leaf(&mut self, value: T) {
        self.children
            .push(Arc::new(TreeLeafData::new(value)).into());
    }
}
