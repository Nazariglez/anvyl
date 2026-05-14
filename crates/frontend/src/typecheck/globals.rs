use super::{
    GlobalKey, TypeChecker, check_value_expr_checked_with_hint, push_source_scope,
    register_declarations,
};
use crate::{
    ast::{GlobalDeclNode, Program, Stmt},
    typecheck::ModuleScope,
};

pub(super) fn check_global_initializers(
    module: &ModuleScope,
    program: &Program,
    tc: &mut TypeChecker,
) {
    if !program
        .stmts
        .iter()
        .any(|stmt| matches!(stmt.node, Stmt::Global(_)))
    {
        return;
    }

    tc.with_current_module(module, |tc| {
        push_source_scope(tc);
        register_declarations(program, tc);
        for stmt in &program.stmts {
            let Stmt::Global(global) = &stmt.node else {
                continue;
            };
            check_global_initializer(module, global, tc);
        }
        tc.pop_scope();
    });
}

fn check_global_initializer(module: &ModuleScope, global: &GlobalDeclNode, tc: &mut TypeChecker) {
    let key = GlobalKey {
        module: module.clone(),
        name: global.node.name,
    };
    let expected = tc.global_handle(&key);

    tc.enter_global_initializer();
    let value = check_value_expr_checked_with_hint(&global.node.value, Some(expected.clone()), tc);
    tc.reject_extern_any_escape(&value, global.node.value.span);
    tc.expect_assignable_expr(
        global.node.value.span,
        global.node.value.node.id,
        value.handle,
        expected.clone(),
    );
    tc.solve_constraints();
    tc.exit_global_initializer();

    let ty = tc.handle_type(&expected);
    tc.reject_user_any_type(&ty, global.span);
}
