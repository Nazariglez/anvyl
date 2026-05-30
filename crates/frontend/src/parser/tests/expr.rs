use super::helpers::parse_expr;
use crate::ast;

fn expect_match(source: &str) -> ast::MatchNode {
    let expr = parse_expr(source);
    match expr.node.kind {
        ast::ExprKind::Match(node) => node,
        other => panic!("expected match, found {other:?}"),
    }
}

fn expect_dyn_downcast(head: &ast::MatchArmHead) -> &ast::DynDowncastArmNode {
    match head {
        ast::MatchArmHead::DynDowncast(arm) => arm,
        other => panic!("expected dynamic downcast arm, found {other:?}"),
    }
}

#[test]
fn dynamic_match_downcast_arm_ids_are_distinct() {
    let match_node = expect_match(
        "match actor as? { Enemy(enemy) => enemy, Bullet(bullet) => bullet, other => other }",
    );
    let first = expect_dyn_downcast(&match_node.node.arms[0].node.head);
    let second = expect_dyn_downcast(&match_node.node.arms[1].node.head);
    assert_ne!(first.node.id, second.node.id);
}
