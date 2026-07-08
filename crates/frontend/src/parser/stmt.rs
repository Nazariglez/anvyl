use chumsky::prelude::*;

use super::{
    AnvParser, BoxedParser,
    common::{block_stmt, identifier},
    decl::{DeclPolicy, declaration_header, local_function, local_type_alias_statement},
    expr::{cond_expression, expression, for_header_expression},
    pattern::{conditional_pattern, local_refutable_pattern, pattern},
    types::type_ident,
};
use crate::{
    ast,
    lexer::{Keyword, Op, Token},
    span::{SourceSpan, Span, Spanned},
};

pub(super) fn statement<'src>() -> BoxedParser<'src, ast::StmtNode> {
    recursive(|stmt| {
        let expr = expression(stmt.clone());
        let func = local_function(stmt.clone());
        let bind = binding(stmt.clone());
        let const_s = const_stmt(stmt.clone());
        let type_alias = local_type_alias_statement();
        let ret = return_stmt(expr.clone());
        let while_let_s = while_let_stmt(stmt.clone(), expr.clone());
        let while_s = while_stmt(stmt.clone(), expr.clone());
        let for_s = for_stmt(stmt.clone(), expr.clone());
        let break_s = break_stmt();
        let continue_s = continue_stmt();
        let defer_s = defer_stmt(stmt.clone(), expr.clone());

        let let_else = local_refutable_pattern()
            .then_ignore(select! { Token::Op(Op::Assign) => () })
            .then(expression(stmt.clone()))
            .then_ignore(select! { Token::Keyword(Keyword::Else) => () })
            .then(let_else_fallback(stmt.clone(), expr.clone()))
            .map_with(|(((head, pat), value), fallback), e| {
                let span = e.span().byte();
                Spanned::new(
                    ast::Stmt::LetElse(Spanned::new(
                        ast::LetElse {
                            mutability: head,
                            pattern: pat,
                            value,
                            fallback,
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
        .ignore_then(conditional_pattern())
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

fn for_binding_segment<'src>() -> BoxedParser<'src, ast::ForBinding> {
    select! { Token::Keyword(Keyword::Ref) => ast::RefAccess::Ref }
        .or_not()
        .then(pattern())
        .map(|(access, pattern)| ast::ForBinding {
            access: access.unwrap_or(ast::RefAccess::Value),
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
    .then(for_header_expression(stmt.clone()))
    .then(block_stmt(stmt, expr))
    .map_with(|((bindings, iterable), body), e| {
        let s = e.span();
        let span = s.byte();
        let for_node = Spanned::new(
            ast::For {
                bindings,
                iterable,
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

fn let_else_fallback<'src>(
    stmt: impl AnvParser<'src, ast::StmtNode>,
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::LetElseFallbackNode> {
    let block = block_stmt(stmt, expr.clone())
        .map_with(|block, e| Spanned::new(ast::LetElseFallback::Block(block), e.span().byte()));
    let ret = return_node(expr)
        .map_with(|ret, e| Spanned::new(ast::LetElseFallback::Return(ret), e.span().byte()));
    let break_ = control_transfer_span(Keyword::Break)
        .map(|span| Spanned::new(ast::LetElseFallback::Break, span));
    let continue_ = control_transfer_span(Keyword::Continue)
        .map(|span| Spanned::new(ast::LetElseFallback::Continue, span));

    choice((block, ret, break_, continue_))
        .labelled("let-else fallback")
        .as_context()
        .boxed()
}

fn control_transfer_span<'src>(keyword: Keyword) -> BoxedParser<'src, Span> {
    select! { Token::Keyword(found) if found == keyword => () }
        .then_ignore(select! { Token::Semicolon => () })
        .map_with(|(), e| {
            let span: SourceSpan = e.span();
            span.byte()
        })
        .boxed()
}

fn break_stmt<'src>() -> BoxedParser<'src, ast::StmtNode> {
    control_transfer_span(Keyword::Break)
        .map(|span| Spanned::new(ast::Stmt::Break, span))
        .labelled("break statement")
        .as_context()
        .boxed()
}

fn continue_stmt<'src>() -> BoxedParser<'src, ast::StmtNode> {
    control_transfer_span(Keyword::Continue)
        .map(|span| Spanned::new(ast::Stmt::Continue, span))
        .labelled("continue statement")
        .as_context()
        .boxed()
}

fn return_node<'src>(
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::ReturnNode> {
    select! { Token::Keyword(Keyword::Return) => () }
        .ignore_then(expr.or_not())
        .then_ignore(select! { Token::Semicolon => () })
        .map_with(|value, e| Spanned::new(ast::Return { value }, e.span().byte()))
        .boxed()
}

fn return_stmt<'src>(
    expr: impl AnvParser<'src, ast::ExprNode>,
) -> BoxedParser<'src, ast::StmtNode> {
    return_node(expr)
        .map(|ret| {
            let span = ret.span;
            Spanned::new(ast::Stmt::Return(ret), span)
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
