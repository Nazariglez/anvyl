use std::ops::Range;

use crate::source::SourceId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct ByteSpan {
    pub start: usize,
    pub end: usize,
}

pub type Span = ByteSpan;

impl ByteSpan {
    #[must_use]
    pub fn new(start: usize, end: usize) -> Self {
        debug_assert!(start <= end);
        Self { start, end }
    }

    #[must_use]
    pub fn empty(offset: usize) -> Self {
        Self::new(offset, offset)
    }

    #[must_use]
    pub fn to_end(self) -> Self {
        Self::empty(self.end)
    }

    #[must_use]
    pub fn union(self, other: Self) -> Self {
        Self::new(self.start.min(other.start), self.end.max(other.end))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceSpan {
    pub source: SourceId,
    pub span: ByteSpan,
}

impl SourceSpan {
    #[must_use]
    pub fn new(source: SourceId, start: usize, end: usize) -> Self {
        Self::from_byte_span(source, ByteSpan::new(start, end))
    }

    #[must_use]
    pub fn from_byte_span(source: SourceId, span: ByteSpan) -> Self {
        Self { source, span }
    }

    #[must_use]
    pub fn empty(source: SourceId, offset: usize) -> Self {
        Self::from_byte_span(source, ByteSpan::empty(offset))
    }

    #[must_use]
    pub fn to_end(self) -> Self {
        Self {
            source: self.source,
            span: self.span.to_end(),
        }
    }

    #[must_use]
    pub fn union(self, other: Self) -> Self {
        debug_assert_eq!(self.source, other.source);
        Self {
            source: self.source,
            span: self.span.union(other.span),
        }
    }

    #[must_use]
    pub fn source(self) -> SourceId {
        self.source
    }

    #[must_use]
    pub fn byte(self) -> ByteSpan {
        self.span
    }

    #[must_use]
    pub fn start(self) -> usize {
        self.span.start
    }

    #[must_use]
    pub fn end(self) -> usize {
        self.span.end
    }
}

impl chumsky::span::Span for SourceSpan {
    type Context = SourceId;
    type Offset = usize;

    fn new(context: Self::Context, range: Range<Self::Offset>) -> Self {
        Self {
            source: context,
            span: ByteSpan::new(range.start.min(range.end), range.start.max(range.end)),
        }
    }

    fn context(&self) -> Self::Context {
        self.source
    }

    fn start(&self) -> Self::Offset {
        self.span.start
    }

    fn end(&self) -> Self::Offset {
        self.span.end
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn node(&self) -> &T {
        &self.node
    }
}
