use super::{
    CheckedType, GlobalAccessFact, GlobalAccessMode, GlobalKey, GlobalSig, Type, TypeChecker,
    TypeError, ValueDecl, check_value_expr_checked_with_hint, push_type_closure_error,
    register_declarations, type_closure_facts,
};
use crate::{
    ast::{ExprId, GlobalDeclNode, Program, Stmt},
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
        tc.push_scope();
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

impl TypeChecker {
    pub(super) fn record_global_access(
        &mut self,
        expr_id: ExprId,
        root_expr_id: ExprId,
        key: &GlobalKey,
        mode: GlobalAccessMode,
    ) {
        self.semantic_facts.record_global_access(
            self.current_body(),
            GlobalAccessFact {
                expr_id,
                root_expr_id,
                key: key.clone(),
                mode,
                init_effect: mode.init_effect(),
            },
        );
    }

    pub(super) fn seed_global_types(&mut self) {
        let globals = self
            .decls
            .values()
            .filter_map(|value| match &value.decl {
                ValueDecl::Global(sig) => {
                    Some((sig.key.clone(), sig.ty.clone(), sig.initializer_span))
                }
                ValueDecl::Func(_) | ValueDecl::Const(_) => None,
            })
            .collect::<Vec<_>>();

        for (key, ty, span) in globals {
            let id = if matches!(ty, Type::Infer) {
                self.solver.alloc_fresh_local_type(Some(span))
            } else {
                self.solver.alloc_local_type(&ty)
            };
            self.global_types.insert(key, id);
        }
    }

    pub(super) fn global_handle(&self, key: &GlobalKey) -> super::TypeHandle {
        let id = *self
            .global_types
            .get(key)
            .expect("global type was not seeded");
        self.local_handle(id)
    }

    pub(super) fn global_checked(&self, sig: &GlobalSig) -> CheckedType {
        let handle = self.global_handle(&sig.key);
        CheckedType::new(self.handle_type(&handle), handle)
    }

    pub(super) fn sync_global_types(&mut self) {
        let globals = self
            .global_types
            .iter()
            .map(|(key, id)| (key.clone(), *id))
            .collect::<Vec<_>>();

        for (key, id) in globals {
            let ty = self.solver.local_type_to_type(id);
            if let Some(sig) = self.decls.global(&key) {
                if type_closure_facts(&ty).contains_any {
                    self.push_error_once(TypeError::AnyOutsideExternBoundary {
                        span: Some(sig.span),
                    });
                }
                let mut errors = vec![];
                push_type_closure_error(&mut errors, &ty, Some(sig.initializer_span));
                for error in errors {
                    self.push_error_once(error);
                }
            }
            self.decls.set_global_type(&key, &ty);
            self.solver.set_local_type_from_type(id, &ty);
        }
    }
}

fn check_global_initializer(module: &ModuleScope, global: &GlobalDeclNode, tc: &mut TypeChecker) {
    let key = GlobalKey {
        module: module.clone(),
        name: global.node.name,
    };
    let expected = tc.global_handle(&key);

    tc.with_global_initializer_body(key, |tc| {
        let value =
            check_value_expr_checked_with_hint(&global.node.value, Some(expected.clone()), tc);
        tc.record_escaping_use(&global.node.value);
        tc.reject_extern_any_escape(&value, global.node.value.span);
        tc.expect_assignable_expr(
            global.node.value.span,
            global.node.value.node.id,
            value.handle,
            expected.clone(),
        );
        tc.solve_constraints();
    });

    let ty = tc.handle_type(&expected);
    tc.reject_user_any_type(&ty, global.span);
}
