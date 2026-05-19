use std::collections::{HashMap, HashSet};

use super::{
    TypeChecker, TypeError, check_expected_value_expr,
    decls::{ExtendId, FieldSchema, MethodKey, MethodMode, NominalKey, ValueDecl},
    generic::{GenericArgs, GenericOwnerFrame, GenericParams},
    type_refs::GenericTypeContext,
};
use crate::{
    ast::{
        ExprKind, ExprNode, Func, FuncParam, Ident, InferredEnumArgs, MethodSig, Mutability, Param,
        Program, Range, Stmt, StringPart, StructField, Type,
    },
    span::Span,
};

pub(super) fn check_decl_param_order(program: &Program, tc: &mut TypeChecker) {
    for stmt in &program.stmts {
        match &stmt.node {
            Stmt::Func(func) => check_param_order(&func.node.params, func.span, tc),
            Stmt::Aggregate(agg) => {
                for method in &agg.node.methods {
                    check_param_order(&method.sig.params, agg.span, tc);
                }
            }
            Stmt::Extend(extend) => {
                for method in &extend.node.methods {
                    check_param_order(&method.node.sig.params, method.span, tc);
                }
            }
            _ => {}
        }
    }
}

pub(super) fn check_param_order(params: &[Param], span: Span, tc: &mut TypeChecker) {
    let mut saw_default = false;
    for param in params {
        if param.default.is_some() {
            saw_default = true;
        } else if saw_default {
            tc.push_error(TypeError::RequiredParamAfterDefault {
                name: param.name,
                span: tc.error_span(span),
            });
        }
    }
}

pub(super) fn check_decl_param_defaults(program: &Program, tc: &mut TypeChecker) {
    let mut extend_index = 0;
    for stmt in &program.stmts {
        match &stmt.node {
            Stmt::Func(func_node) => check_func_defaults(&func_node.node, func_node.span, tc),
            Stmt::Aggregate(agg_node) => {
                let agg = &agg_node.node;
                let key = NominalKey {
                    module: tc.current_module.clone(),
                    kind: agg.kind.into(),
                    name: agg.name,
                };
                let Some(schema) = tc.decls.aggregate(&key).cloned() else {
                    continue;
                };
                let owner_generics =
                    tc.generic_context(&agg.type_params, &agg.const_params, agg_node.span);
                for method in &agg.methods {
                    let mode = MethodMode::from_receiver(method.sig.receiver);
                    if let Some(method_schema) = schema
                        .methods
                        .get(&MethodKey::new(method.sig.name, mode.surface()))
                    {
                        check_method_defaults(
                            &method.sig,
                            &method_schema.params,
                            method_schema.mode.receiver().is_some(),
                            method_schema.generics.clone(),
                            &owner_generics,
                            schema.generics.clone(),
                            agg_node.span,
                            tc,
                        );
                    }
                }
            }
            Stmt::Extend(extend_node) => {
                let extend = &extend_node.node;
                let id = ExtendId {
                    module: tc.current_module.clone(),
                    index: extend_index,
                };
                extend_index += 1;
                let Some(schema) = tc.decls.extend(&id).cloned() else {
                    continue;
                };
                let owner_generics =
                    tc.generic_context(&extend.type_params, &extend.const_params, extend_node.span);
                for method_node in &extend.methods {
                    let sig = &method_node.node.sig;
                    let mode = MethodMode::from_receiver(sig.receiver);
                    if let Some(method_schema) = schema
                        .methods
                        .get(&MethodKey::new(sig.name, mode.surface()))
                    {
                        check_method_defaults(
                            sig,
                            &method_schema.params,
                            method_schema.mode.receiver().is_some(),
                            method_schema.generics.clone(),
                            &owner_generics,
                            schema.generics.clone(),
                            method_node.span,
                            tc,
                        );
                    }
                }
            }
            _ => {}
        }
    }
}

fn check_func_defaults(func: &Func, span: Span, tc: &mut TypeChecker) {
    if !has_param_defaults(&func.params) {
        return;
    }
    let Some((param_types, params)) = func_decl_params(func.name, tc) else {
        return;
    };
    let generics = tc.generic_context(&func.type_params, &func.const_params, span);
    check_param_defaults_in_decl_scope(
        &func.params,
        &param_types,
        false,
        GenericOwnerFrame {
            params,
            args: GenericArgs::default(),
            generics,
        },
        tc,
    );
}

fn check_method_defaults(
    sig: &MethodSig,
    params: &[FuncParam],
    has_receiver: bool,
    generics: GenericParams,
    owner_generics: &GenericTypeContext,
    owner_params: GenericParams,
    span: Span,
    tc: &mut TypeChecker,
) {
    if !has_param_defaults(&sig.params) {
        return;
    }
    let context =
        tc.extended_generic_context(owner_generics, &sig.type_params, &sig.const_params, span);
    check_param_defaults_in_decl_scope(
        &sig.params,
        params,
        has_receiver,
        GenericOwnerFrame {
            params: combine_params(owner_params, generics),
            args: GenericArgs::default(),
            generics: context,
        },
        tc,
    );
}

pub(super) fn has_param_defaults(params: &[Param]) -> bool {
    params.iter().any(|param| param.default.is_some())
}

fn func_decl_params(name: Ident, tc: &TypeChecker) -> Option<(Vec<FuncParam>, GenericParams)> {
    let value = tc.decls.local_value(&tc.current_module, name)?;
    let ValueDecl::Func(sig) = value.decl else {
        return None;
    };
    let Type::Func { params, .. } = sig.ty else {
        return None;
    };
    Some((params, sig.generics))
}

pub(super) fn combine_params(mut owner: GenericParams, callable: GenericParams) -> GenericParams {
    owner.type_params.extend(callable.type_params);
    owner.const_params.extend(callable.const_params);
    owner
}

pub(super) fn check_aggregate_field_defaults(
    fields: &[StructField],
    schema: &HashMap<Ident, FieldSchema>,
    generics: GenericOwnerFrame,
    tc: &mut TypeChecker,
) {
    tc.push_generic_context(generics.generics.clone());
    tc.push_generic_owner_frame(generics);
    let forbidden = fields
        .iter()
        .map(|field| field.name)
        .collect::<HashSet<_>>();
    for field in fields {
        let Some(default) = &field.default else {
            continue;
        };
        let Some(schema) = schema.get(&field.name) else {
            continue;
        };
        if validate_default(default, DefaultKind::Field, &forbidden, tc) {
            check_default_type(default, tc.type_handle(&schema.ty), tc);
        }
    }
    tc.pop_generic_owner_frame();
    tc.pop_generic_context();
}

pub(super) fn check_param_defaults_in_decl_scope(
    params: &[Param],
    param_types: &[FuncParam],
    has_receiver: bool,
    generics: GenericOwnerFrame,
    tc: &mut TypeChecker,
) {
    if !has_param_defaults(params) {
        return;
    }
    tc.push_generic_context(generics.generics.clone());
    tc.push_generic_owner_frame(generics);
    tc.enter_named_function();
    tc.push_scope();
    check_param_defaults(params, param_types, has_receiver, tc);
    tc.pop_scope();
    tc.exit_named_function();
    tc.pop_generic_owner_frame();
    tc.pop_generic_context();
}

fn check_param_defaults(
    params: &[Param],
    param_types: &[FuncParam],
    has_receiver: bool,
    tc: &mut TypeChecker,
) {
    let forbidden = params
        .iter()
        .map(|param| param.name)
        .collect::<HashSet<_>>();
    for (param, param_ty) in params.iter().zip(param_types) {
        let Some(default) = &param.default else {
            continue;
        };
        if matches!(param.mutability, Mutability::Mutable) {
            tc.push_error(TypeError::VarParamDefault {
                name: param.name,
                span: tc.error_span(default.span),
            });
            continue;
        }
        if validate_default(default, DefaultKind::Param { has_receiver }, &forbidden, tc) {
            check_default_type(default, tc.type_handle(&param_ty.ty), tc);
        }
    }
}

fn check_default_type(default: &ExprNode, expected: super::TypeHandle, tc: &mut TypeChecker) {
    let checked = check_expected_value_expr(default, expected, tc);
    tc.reject_extern_any_escape(&checked, default.span);
    tc.solve_constraints();
}

#[derive(Clone, Copy)]
enum DefaultKind {
    Field,
    Param { has_receiver: bool },
}

fn validate_default(
    expr: &ExprNode,
    kind: DefaultKind,
    forbidden: &HashSet<Ident>,
    tc: &mut TypeChecker,
) -> bool {
    let before = tc.errors.len();
    validate_expr(expr, kind, forbidden, tc);
    tc.errors.len() == before
}

fn validate_expr(
    expr: &ExprNode,
    kind: DefaultKind,
    forbidden: &HashSet<Ident>,
    tc: &mut TypeChecker,
) {
    check_forbidden_name(expr, kind, forbidden, tc);
    match &expr.node.kind {
        ExprKind::Ident(_) | ExprKind::TypeSubject(_) | ExprKind::Lit(_) => {}
        ExprKind::Call(call) => {
            validate_expr(&call.node.func, kind, forbidden, tc);
            validate_exprs(&call.node.args, kind, forbidden, tc);
        }
        ExprKind::Binary(binary) => {
            validate_expr(&binary.node.left, kind, forbidden, tc);
            validate_expr(&binary.node.right, kind, forbidden, tc);
        }
        ExprKind::Unary(unary) => validate_expr(&unary.node.expr, kind, forbidden, tc),
        ExprKind::Tuple(items) => validate_exprs(items, kind, forbidden, tc),
        ExprKind::TupleIndex(index) => validate_expr(&index.node.target, kind, forbidden, tc),
        ExprKind::Field(field) => validate_expr(&field.node.target, kind, forbidden, tc),
        ExprKind::StructLiteral(lit) => {
            for (_, value) in &lit.node.fields {
                validate_expr(value, kind, forbidden, tc);
            }
        }
        ExprKind::Range(range) => match &range.node {
            Range::Bounded { start, end, .. } => {
                validate_expr(start, kind, forbidden, tc);
                validate_expr(end, kind, forbidden, tc);
            }
            Range::From { start } => validate_expr(start, kind, forbidden, tc),
            Range::To { end, .. } => validate_expr(end, kind, forbidden, tc),
        },
        ExprKind::ArrayLiteral(lit) => validate_exprs(&lit.node.elements, kind, forbidden, tc),
        ExprKind::ArrayFill(fill) => {
            validate_expr(&fill.node.value, kind, forbidden, tc);
            validate_expr(&fill.node.len, kind, forbidden, tc);
        }
        ExprKind::MapLiteral(map) => {
            for (key, value) in &map.node.entries {
                validate_expr(key, kind, forbidden, tc);
                validate_expr(value, kind, forbidden, tc);
            }
        }
        ExprKind::Index(index) => {
            validate_expr(&index.node.target, kind, forbidden, tc);
            validate_expr(&index.node.index, kind, forbidden, tc);
        }
        ExprKind::StringInterp(parts) => {
            for part in parts {
                if let StringPart::Expr(expr, _) = part {
                    validate_expr(expr, kind, forbidden, tc);
                }
            }
        }
        ExprKind::Cast(cast) => validate_expr(&cast.node.expr, kind, forbidden, tc),
        ExprKind::InferredEnum(inferred) => match &inferred.node.args {
            InferredEnumArgs::Unit => {}
            InferredEnumArgs::Tuple(args) => validate_exprs(args, kind, forbidden, tc),
            InferredEnumArgs::Struct(fields) => {
                for (_, value) in fields {
                    validate_expr(value, kind, forbidden, tc);
                }
            }
        },
        ExprKind::Block(_)
        | ExprKind::Assign(_)
        | ExprKind::If(_)
        | ExprKind::Ternary(_)
        | ExprKind::IfLet(_)
        | ExprKind::Match(_)
        | ExprKind::ExactDowncast(_)
        | ExprKind::Try(_)
        | ExprKind::Lambda(_)
        | ExprKind::IntrinsicCall(_) => tc.push_error(TypeError::InvalidDefaultExpression {
            kind: expr.node.kind.variant_name(),
            span: tc.error_span(expr.span),
        }),
    }
}

fn validate_exprs(
    exprs: &[ExprNode],
    kind: DefaultKind,
    forbidden: &HashSet<Ident>,
    tc: &mut TypeChecker,
) {
    for expr in exprs {
        validate_expr(expr, kind, forbidden, tc);
    }
}

fn check_forbidden_name(
    expr: &ExprNode,
    kind: DefaultKind,
    forbidden: &HashSet<Ident>,
    tc: &mut TypeChecker,
) {
    let ExprKind::Ident(name) = &expr.node.kind else {
        return;
    };
    if forbidden.contains(name) {
        match kind {
            DefaultKind::Field => tc.push_error(TypeError::DefaultReferencesField {
                name: *name,
                span: tc.error_span(expr.span),
            }),
            DefaultKind::Param { .. } => tc.push_error(TypeError::DefaultReferencesParameter {
                name: *name,
                span: tc.error_span(expr.span),
            }),
        }
        return;
    }
    if matches!(kind, DefaultKind::Param { has_receiver: true }) && name.as_str() == "self" {
        tc.push_error(TypeError::DefaultReferencesSelf {
            span: tc.error_span(expr.span),
        });
    }
}
