use crate::ast::{BlockNode, ExprKind, ExprNode, Stmt, StmtNode};

pub(super) fn block_diverges(block: &BlockNode) -> bool {
    block.node.stmts.iter().any(stmt_diverges)
        || block
            .node
            .tail
            .as_ref()
            .is_some_and(|expr| expr_diverges(expr))
}

fn stmt_diverges(stmt: &StmtNode) -> bool {
    match &stmt.node {
        Stmt::Return(_) | Stmt::Break | Stmt::Continue => true,
        Stmt::Expr(expr) => expr_diverges(expr),
        _ => false,
    }
}

pub(super) fn expr_diverges(expr: &ExprNode) -> bool {
    match &expr.node.kind {
        ExprKind::If(if_node) => {
            let Some(else_block) = &if_node.node.else_block else {
                return false;
            };
            block_diverges(&if_node.node.then_block) && block_diverges(else_block)
        }
        ExprKind::Block(block) => block_diverges(block),
        _ => false,
    }
}
