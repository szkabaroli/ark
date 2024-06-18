impl RedTreeBuilder {
    pub fn new() -> GreenTreeBuilder {
        GreenTreeBuilder {
            nodes: Vec::new(),
            children: Vec::new(),
            offset: 0,
        }
    }
}