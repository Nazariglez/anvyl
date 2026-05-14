use chumsky::prelude::*;

use super::{
    AnvParser, BoxedParser,
    common::{block_stmt, identifier},
    decl::{DeclPolicy, declaration_header, local_function, local_type_alias_statement},
    expr::{cond_expression, expression, for_header_expression},
    pattern::{binding_pattern, pattern},
    types::type_ident,
};
use crate::{
    ast,
    lexer::{Keyword, Op, Token},
    span::{SourceSpan, Spanned},
};

pub(super) fn statement<'src>() -> BoxedParser<'src, ast::StmtNode> {
    recursive(|stmt| {
        let expr = expression(stmt.clone());
        let func = local_function(stmt.clone());
        let bind = binding(stmt.clone());
        let const_s = const_stmt(stmt.clone());
        let type_alias = local_type_alias_statement();
        let ret = return_stmt(stmt.clone());
        let while_let_s = while_let_stmt(stmt.clone(), expr.clone());
        let while_s = while_stmt(stmt.clone(), expr.clone());
        let for_s = for_stmt(stmt.clone(), expr.clone());
        let break_s = break_stmt();
        let continue_s = continue_stmt();
        let defer_s = defer_stmt(stmt.clone(), expr.clone());

        let let_else = binding_pattern()
            .then_ignore(select! { Token::Op(Op::Assign) => () })
            .then(expression(stmt.clone()))
            .then_ignore(select! { Token::Keyword(Keyword::Else) => () })
            .then(block_stmt(stmt.clone(), expr.clone()))
            .map_with(|(((head, pat), value), else_block), e| {
                let s = e.span();
                let span = s.byte();
                Spanned::new(
                    ast::Stmt::LetElse(Spanned::new(
                        ast::LetElse {
                            head,
                            pattern: pat,
                            value,
                            else_block,
                        },
                        span,
                    )),
                    span,
                )
            });

        let at_stmt_start = select! {
            Token::Keyword(Keyword::Let) => (),
            Token::Keyword(Keyword::Var) => (),
            Token::Keyword(Keyword::Return) => (),
            Token::Keyword(Keyword::Fn) => (),
            Token::Keyword(Keyword::Pub) => (),
            Token::Keyword(Keyword::If) => (),
            Token::Keyword(Keyword::Match) => (),
            Token::Keyword(Keyword::Struct) => (),
            Token::Keyword(Keyword::While) => (),
            Token::Keyword(Keyword::For) => (),
            Token::Keyword(Keyword::Break) => (),
            Token::Keyword(Keyword::Continue) => (),
            Token::Keyword(Keyword::Defer) => (),
            Token::Keyword(Keyword::Const) => (),
            Token::Keyword(Keyword::Lazy) => (),
            Token::Keyword(Keyword::Type) => (),
            Token::At => (),
            Token::DocComment(_) => (),
        }
        .rewind();

        let at_assign_start = select! { Token::Ident(_) => () }
            .then(select! {
                Token::Op(Op::Assign) => (),
                Token::Op(Op::AddAssign) => (),
                Token::Op(Op::SubAssign) => (),
                Token::Op(Op::MulAssign) => (),
                Token::Op(Op::DivAssign) => (),
                Token::Op(Op::CaretAssign) => (),
                Token::Op(Op::BitAndAssign) => (),
                Token::Op(Op::BitOrAssign) => (),
            })
            .to(())
            .rewind();

        let expr_stmt = expr
            .clone()
            .then_ignore(
                select! { Token::Semicolon => () }
                    .or(at_stmt_start)
                    .or(at_assign_start),
            )
            .map(|expr_node| {
                let span = expr_node.span;
                Spanned::new(ast::Stmt::Expr(expr_node), span)
            });

        let if_as_stmt = select! { Token::Keyword(Keyword::If) => () }
            .rewind()
            .ignore_then(expr)
            .then_ignore(
                select! {
                    Token::Keyword(_) => (),
                    Token::Ident(_) => (),
                    Token::Literal(_) => (),
                }
                .rewind(),
            )
            .map(|expr_node| {
                let span = expr_node.span;
                Spanned::new(ast::Stmt::Expr(expr_node), span)
            });

        choice((
            func.map(|func_node| {
                let span = func_node.span;
                Spanned::new(ast::Stmt::Func(func_node), span)
            }),
            let_else,
            bind.map(|bind_node| {
                let span = bind_node.span;
                Spanned::new(ast::Stmt::Binding(bind_node), span)
            }),
            const_s,
            type_alias,
            ret,
            while_let_s,
            while_s,
            for_s,
            break_s,
            continue_s,
            defer_s,
            if_as_stmt,
            expr_stmt,
        ))
    })
    .labelled("statement")
    .as_context()
    .boxed()
}

fn binding<'src>(stmt: impl AnvParser<'src, ast::StmtNode>) -> BoxedParser<'src, ast::BindingNode> {
    let mutability = select! {
        Token::Keyword(Keyword::Let) => ast::Mutability::Immutable,
        Token::Keyword(Keyword::Var) => ast::Mutability::Mutable,
    };

    mutability
        .then(pattern())
        .then(
            select! {
                Token::Colon => (),
            }
            .ignore_then(type_ident())
            .or_not(),
        )
        .then_ignore(select! {
            Token::Op(Op::Assign) => (),
        })
        .then(expression(stmt))
        .then_ignore(select! {
            Token::Semicolon => (),
        })
        .map_with(|(((mutability, pat), ty), value), e| {
            let s = e.span();
            Spanned::new(
                ast::Binding {
                    pattern: pat,
                    ty,
                    mutability,
                    value,
                },
                s.byte(),
            )
        })
        .boxed()
}

fn const_stmt<'src>(stmt: impl AnvParser<'src, ast::StmtNode>) -> BoxedParser<'src, ast::StmtNode> {
    declaration_header(DeclPolicy::LOCAL_CONST)
        .then(
            select! { Token::Keyword(Keyword::Const) => () }
                .ignore_then(identifier())
                .then(
                    select! { Token::Colon => () }
                        .ignore_then(type_ident())
                        .or_not(),
                )
                .then_ignore(select! { Token::Op(Op::Assign) => () })
                .then(expression(stmt))
                .then_ignore(select! { Token::Semicolon => () }),
        )
        .map_with(|(header, ((name, ty), value)), e| {
            let span = e.span().byte();
            let node = Spanned::new(
                ast::ConstDecl {
                    annotations: header.annotations,
                    doc: header.doc,
                    name,
                    ty,
                    value,
                    visibility: header.visibility,
                },
                span,
            );
            Spanned::new(ast::Stmt::Const(node), span)
        })
        .labelled("const statement")
        .as_context()
        .boxed()
}

fn while_stmt<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::StmtNode> {
    let cond_expr = cond_expression();

    select! {
        Token::Keyword(Keyword::While) => (),
    }
    .ignore_then(cond_expr)
    .then(block_stmt(stmt, expr))
    .map_with(|(cond, body), e| {
        let s = e.span();
        let span = s.byte();
        let while_node = Spanned::new(ast::While { cond, body }, span);
        Spanned::new(ast::Stmt::While(while_node), span)
    })
    .labelled("while statement")
    .as_context()
    .boxed()
}

fn while_let_stmt<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::StmtNode> {
    select! { Token::Keyword(Keyword::While) => () }
        .ignore_then(binding_pattern())
        .then_ignore(select! { Token::Op(Op::Assign) => () })
        .then(cond_expression())
        .then(block_stmt(stmt, expr))
        .map_with(|(((head, pat), value), body), e| {
            let s = e.span();
            let span = s.byte();
            let node = Spanned::new(
                ast::WhileLet {
                    head,
                    pattern: pat,
                    value,
                    body,
                },
                span,
            );
            Spanned::new(ast::Stmt::WhileLet(node), span)
        })
        .labelled("while-let statement")
        .as_context()
        .boxed()
}

// rev key is only valid on for loops
fn contextual_rev<'src>() -> BoxedParser<'src, bool> {
    select! {
        Token::Ident(ident) if ident.0.as_ref() == "rev" => true,
    }
    .or_not()
    .map(|o| o.unwrap_or(false))
    .boxed()
}

// step key is only valid on for loops
fn contextual_step<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, Option<ast::ExprNode>> {
    select! {
        Token::Ident(ident) if ident.0.as_ref() == "step" => (),
    }
    .ignore_then(for_header_expression(stmt))
    .or_not()
    .boxed()
}

fn for_binding_segment<'src>() -> BoxedParser<'src, ast::ForBinding> {
    select! { Token::Keyword(Keyword::Var) => () }
        .or_not()
        .then(pattern())
        .map(|(mutable, pattern)| ast::ForBinding {
            mutable: mutable.is_some(),
            pattern,
        })
        .boxed()
}

fn for_binding_segments<'src>() -> BoxedParser<'src, Vec<ast::ForBinding>> {
    for_binding_segment()
        .separated_by(select! { Token::Comma => () })
        .at_least(1)
        .at_most(2)
        .collect::<Vec<_>>()
        .boxed()
}

fn for_stmt<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::StmtNode> {
    select! {
        Token::Keyword(Keyword::For) => (),
    }
    .ignore_then(for_binding_segments())
    .then_ignore(select! {
        Token::Keyword(Keyword::In) => (),
    })
    .then(contextual_rev())
    .then(for_header_expression(stmt.clone()))
    .then(contextual_step(stmt.clone()))
    .then(block_stmt(stmt, expr))
    .map_with(|((((bindings, reversed), iterable), step), body), e| {
        let s = e.span();
        let span = s.byte();
        let for_node = Spanned::new(
            ast::For {
                bindings,
                iterable,
                step,
                reversed,
                body,
            },
            span,
        );
        Spanned::new(ast::Stmt::For(Box::new(for_node)), span)
    })
    .labelled("for statement")
    .as_context()
    .boxed()
}

fn break_stmt<'src>() -> BoxedParser<'src, ast::StmtNode> {
    select! {
        Token::Keyword(Keyword::Break) => (),
    }
    .then_ignore(select! {
        Token::Semicolon => (),
    })
    .map_with(|(), e| {
        let span: SourceSpan = e.span();
        let span = span.byte();
        Spanned::new(ast::Stmt::Break, span)
    })
    .labelled("break statement")
    .as_context()
    .boxed()
}

fn continue_stmt<'src>() -> BoxedParser<'src, ast::StmtNode> {
    select! {
        Token::Keyword(Keyword::Continue) => (),
    }
    .then_ignore(select! {
        Token::Semicolon => (),
    })
    .map_with(|(), e| {
        let span: SourceSpan = e.span();
        let span = span.byte();
        Spanned::new(ast::Stmt::Continue, span)
    })
    .labelled("continue statement")
    .as_context()
    .boxed()
}

fn return_stmt<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
) -> BoxedParser<'src, ast::StmtNode> {
    select! {
        Token::Keyword(Keyword::Return) => (),
    }
    .ignore_then(expression(stmt).or_not())
    .then_ignore(select! {
        Token::Semicolon => (),
    })
    .map_with(|value_opt, e| {
        let s = e.span();
        let span = s.byte();
        let ret = ast::Return { value: value_opt };
        Spanned::new(ast::Stmt::Return(Spanned::new(ret, span)), span)
    })
    .labelled("return")
    .as_context()
    .boxed()
}

fn defer_stmt<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::StmtNode> {
    let block_body = block_stmt(stmt.clone(), expr.clone()).map(ast::DeferBody::Block);
    let expr_body = expression(stmt)
        .then_ignore(select! { Token::Semicolon => () })
        .map(ast::DeferBody::Expr);

    select! { Token::Keyword(Keyword::Defer) => () }
        .ignore_then(block_body.or(expr_body))
        .map_with(|body, e| {
            let s = e.span();
            let span = s.byte();
            let defer = ast::Defer { body };
            Spanned::new(ast::Stmt::Defer(Spanned::new(defer, span)), span)
        })
        .labelled("defer")
        .as_context()
        .boxed()
}
