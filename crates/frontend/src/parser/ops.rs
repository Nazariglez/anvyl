use chumsky::prelude::*;

use super::{AnvParser, BoxedParser, new_expr_id};
use crate::{
    ast,
    lexer::{Op, Token},
    span::Spanned,
};

pub(super) fn infix_left<'src>(
    lower: impl AnvParser<'src, ast::ExprNode>,
    op: impl AnvParser<'src, ast::BinaryOp>,
) -> BoxedParser<'src, ast::ExprNode> {
    let op_rhs = op.then(lower.clone());
    lower
        .foldl(op_rhs.repeated(), |left, (op, right)| {
            let span = left.span.union(right.span);
            let bin_node = Spanned::new(
                ast::Binary {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                },
                span,
            );

            let expr_id = new_expr_id();
            let expr = ast::Expr::new(ast::ExprKind::Binary(bin_node), expr_id);
            Spanned::new(expr, span)
        })
        .boxed()
}

pub(super) fn mul_div_op<'src>() -> BoxedParser<'src, ast::BinaryOp> {
    select! {
        Token::Op(Op::Mul) => ast::BinaryOp::Mul,
        Token::Op(Op::Div) => ast::BinaryOp::Div,
        Token::Op(Op::Rem) => ast::BinaryOp::Rem,
    }
    .labelled("multiplicative op")
    .as_context()
    .boxed()
}

pub(super) fn add_sub_op<'src>() -> BoxedParser<'src, ast::BinaryOp> {
    select! {
        Token::Op(Op::Add) => ast::BinaryOp::Add,
        Token::Op(Op::Sub) => ast::BinaryOp::Sub,
    }
    .labelled("additive op")
    .as_context()
    .boxed()
}

pub(super) fn cmp_op<'src>() -> BoxedParser<'src, ast::BinaryOp> {
    select! {
        Token::Op(Op::LessThan) => ast::BinaryOp::LessThan,
        Token::Op(Op::GreaterThan) => ast::BinaryOp::GreaterThan,
        Token::Op(Op::LessThanEq) => ast::BinaryOp::LessThanEq,
        Token::Op(Op::GreaterThanEq) => ast::BinaryOp::GreaterThanEq,
    }
    .labelled("comparison op")
    .as_context()
    .boxed()
}

pub(super) fn eq_op<'src>() -> BoxedParser<'src, ast::BinaryOp> {
    select! {
        Token::Op(Op::Eq) => ast::BinaryOp::Eq,
        Token::Op(Op::NotEq) => ast::BinaryOp::NotEq,
    }
    .labelled("equality op")
    .as_context()
    .boxed()
}

pub(super) fn xor_op<'src>() -> BoxedParser<'src, ast::BinaryOp> {
    select! {
        Token::Op(Op::Caret) => ast::BinaryOp::Xor,
    }
    .labelled("xor op")
    .as_context()
    .boxed()
}

pub(super) fn bit_and_op<'src>() -> BoxedParser<'src, ast::BinaryOp> {
    select! {
        Token::Op(Op::BitAnd) => ast::BinaryOp::BitAnd,
    }
    .labelled("bitwise and op")
    .as_context()
    .boxed()
}

pub(super) fn bit_or_op<'src>() -> BoxedParser<'src, ast::BinaryOp> {
    select! {
        Token::Op(Op::BitOr) => ast::BinaryOp::BitOr,
    }
    .labelled("bitwise or op")
    .as_context()
    .boxed()
}

pub(super) fn shift_op<'src>() -> BoxedParser<'src, ast::BinaryOp> {
    choice((
        select! { Token::Op(Op::LessThan) => () }
            .then(select! { Token::Op(Op::LessThan) => () })
            .to(ast::BinaryOp::Shl),
        select! { Token::Op(Op::GreaterThan) => () }
            .then(select! { Token::Op(Op::GreaterThan) => () })
            .to(ast::BinaryOp::Shr),
    ))
    .labelled("shift op")
    .as_context()
    .boxed()
}

pub(super) fn and_op<'src>() -> BoxedParser<'src, ast::BinaryOp> {
    select! {
        Token::Op(Op::And) => ast::BinaryOp::And,
    }
    .labelled("logical and op")
    .as_context()
    .boxed()
}

pub(super) fn coalesce_op<'src>() -> BoxedParser<'src, ast::BinaryOp> {
    select! {
        Token::Op(Op::Coalesce) => ast::BinaryOp::Coalesce,
    }
    .labelled("coalesce op")
    .as_context()
    .boxed()
}

pub(super) fn or_op<'src>() -> BoxedParser<'src, ast::BinaryOp> {
    select! {
        Token::Op(Op::Or) => ast::BinaryOp::Or,
    }
    .labelled("logical or op")
    .as_context()
    .boxed()
}

pub(super) fn assign_op<'src>() -> BoxedParser<'src, ast::AssignOp> {
    choice((
        // two tokens compound assignments for <<= and >>=
        // <<= lexes as LessThan + LessThanEq
        select! { Token::Op(Op::LessThan) => () }
            .then(select! { Token::Op(Op::LessThanEq) => () })
            .to(ast::AssignOp::ShlAssign),
        // >>= lexes as GreaterThan + GreaterThanEq
        select! { Token::Op(Op::GreaterThan) => () }
            .then(select! { Token::Op(Op::GreaterThanEq) => () })
            .to(ast::AssignOp::ShrAssign),
        select! {
            Token::Op(Op::Assign) => ast::AssignOp::Assign,
            Token::Op(Op::AddAssign) => ast::AssignOp::AddAssign,
            Token::Op(Op::SubAssign) => ast::AssignOp::SubAssign,
            Token::Op(Op::MulAssign) => ast::AssignOp::MulAssign,
            Token::Op(Op::DivAssign) => ast::AssignOp::DivAssign,
            Token::Op(Op::CaretAssign) => ast::AssignOp::XorAssign,
            Token::Op(Op::BitAndAssign) => ast::AssignOp::BitAndAssign,
            Token::Op(Op::BitOrAssign) => ast::AssignOp::BitOrAssign,
        },
    ))
    .labelled("assign op")
    .as_context()
    .boxed()
}
