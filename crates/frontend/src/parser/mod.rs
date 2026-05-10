mod common;
mod decl;
mod expr;
mod ops;
mod pattern;
mod stmt;
mod types;

#[cfg(test)]
mod tests;

use std::sync::atomic::{AtomicU64, Ordering};

use chumsky::{
    Boxed,
    error::Rich,
    extra::{self, SimpleState},
    input::{Input as _, MappedInput},
    prelude::*,
};
use decl::{
    annotations, const_decl, dataref_declaration, doc_comment_block, enum_declaration,
    extend_declaration, extern_declaration, function, import_declaration, struct_declaration,
};
use stmt::statement;

use crate::{
    ast::{self, ConstParamId, ExprId, TypeVarId},
    lexer::{LexedToken, Token, TokenStream},
    span::{SourceSpan, Spanned},
};

static NEXT_EXPR_ID: AtomicU64 = AtomicU64::new(0);

#[derive(Debug, Default)]
pub(super) struct ParserState {
    next_type_var_id: TypeVarId,
    next_const_param_id: ConstParamId,
}

pub(super) fn new_expr_id() -> ExprId {
    ExprId(NEXT_EXPR_ID.fetch_add(1, Ordering::Relaxed))
}

impl ParserState {
    pub(super) fn new_type_var_id(&mut self) -> TypeVarId {
        let id = TypeVarId(self.next_type_var_id.0);
        self.next_type_var_id = TypeVarId(id.0 + 1);
        id
    }

    pub(super) fn new_const_param_id(&mut self) -> ConstParamId {
        let id = ConstParamId(self.next_const_param_id.0);
        self.next_const_param_id = ConstParamId(id.0 + 1);
        id
    }
}

pub(super) type Input<'src> = MappedInput<
    Token,
    SourceSpan,
    &'src [LexedToken],
    fn(&'src LexedToken) -> (&'src Token, &'src SourceSpan),
>;
pub(super) type Extra<'src> =
    extra::Full<Rich<'src, Token, SourceSpan>, SimpleState<ParserState>, ()>;
pub(super) trait AnvParser<'src, T>:
    Parser<'src, Input<'src>, T, Extra<'src>> + Clone + 'src
{
}
impl<'src, T, P> AnvParser<'src, T> for P where
    P: Parser<'src, Input<'src>, T, Extra<'src>> + Clone + 'src
{
}

pub(super) type BoxedParser<'src, T> = Boxed<'src, 'src, Input<'src>, T, Extra<'src>>;

pub fn parse_ast(tokens: &TokenStream) -> Result<ast::Program, Vec<Rich<'_, Token, SourceSpan>>> {
    let mut state = SimpleState(ParserState::default());
    parser()
        .parse_with_state(token_input(tokens), &mut state)
        .into_result()
}

pub(super) fn token_input(tokens: &TokenStream) -> Input<'_> {
    tokens.tokens.as_slice().map(tokens.eoi, map_token)
}

fn map_token((token, span): &LexedToken) -> (&Token, &SourceSpan) {
    (token, span)
}

fn parser<'src>() -> BoxedParser<'src, ast::Program> {
    let stmt = statement();

    let func_decl = function(stmt.clone()).map(|func_node| {
        let span = func_node.span;
        Spanned::new(ast::Stmt::Func(func_node), span)
    });
    let struct_decl = struct_declaration(stmt.clone()).map(|struct_node| {
        let span = struct_node.span;
        Spanned::new(ast::Stmt::Aggregate(struct_node), span)
    });
    let dataref_decl = dataref_declaration(stmt.clone()).map(|dataref_node| {
        let span = dataref_node.span;
        Spanned::new(ast::Stmt::Aggregate(dataref_node), span)
    });
    let enum_decl = enum_declaration(stmt.clone()).map(|enum_node| {
        let span = enum_node.span;
        Spanned::new(ast::Stmt::Enum(enum_node), span)
    });
    let extend_decl = extend_declaration(stmt.clone()).map(|extend_node| {
        let span = extend_node.span;
        Spanned::new(ast::Stmt::Extend(extend_node), span)
    });
    let extern_decl = extern_declaration(stmt.clone());
    let const_decl = const_decl(stmt);

    let documented_decl = annotations()
        .then(doc_comment_block())
        .then(choice((
            func_decl,
            struct_decl,
            dataref_decl,
            enum_decl,
            const_decl,
            extern_decl,
        )))
        .map(|((annots, doc), mut stmt_node)| {
            match &mut stmt_node.node {
                ast::Stmt::Func(f) => {
                    f.node.doc = doc;
                    f.node.annotations = annots;
                }
                ast::Stmt::Aggregate(s) => {
                    s.node.doc = doc;
                    s.node.annotations = annots;
                }
                ast::Stmt::Enum(e) => {
                    e.node.doc = doc;
                    e.node.annotations = annots;
                }
                ast::Stmt::Const(c) => {
                    c.node.doc = doc;
                    c.node.annotations = annots;
                }
                ast::Stmt::ExternFunc(ef) => {
                    ef.node.doc = doc;
                    ef.node.annotations = annots;
                }
                ast::Stmt::ExternType(et) => {
                    et.node.doc = doc;
                    et.node.annotations = annots;
                }
                _ => unreachable!(),
            }
            stmt_node
        });

    let undocumented_decl = choice((import_declaration(), extend_decl));

    choice((documented_decl, undocumented_decl))
        .repeated()
        .collect::<Vec<_>>()
        .map(|stmts| ast::Program { stmts })
        .then_ignore(end())
        .boxed()
}
