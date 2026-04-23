#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T: Clone> {
    pub node: T,
    pub span: Span,
}

impl<T: Clone> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn span_mut(&mut self) -> &mut Span {
        &mut self.span
    }

    pub fn node(&self) -> &T {
        &self.node
    }

    pub fn node_mut(&mut self) -> &mut T {
        &mut self.node
    }

    pub fn into_node(self) -> T {
        self.node
    }

    pub fn into_span(self) -> Span {
        self.span
    }

    pub fn map<U: Clone>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            node: f(self.node),
            span: self.span,
        }
    }
}
