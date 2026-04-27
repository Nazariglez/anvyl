use std::collections::HashMap;

use internment::Intern;

use super::{
    const_eval::ConstValue,
    constraint::{TypeRef, resolve_constraints},
    decl::check_body_common,
    error::{Diagnostic, DiagnosticKind},
    expr::{check_expr, field_path, root_ident},
    infer::{
        ConstInferenceSlots, build_const_subst, build_param_ref, build_subst,
        constrain_slots_from_type, create_inference_slots, infer_const_args_from_checked_args,
        infer_type_args_from_call, subst_type,
    },
    postfix::resolve_builtin_or_extend,
    types::{
        DeepLookup, EnumDef, ExternMethodDef, ExternTypeDef, InstantiationContext, MethodContext,
        MethodDef, MethodSpecKey, ModuleDef, SpecializationKey, SpecializationResult,
        SpecializationState, StructDef, TypeChecker, TypedBodyResult,
    },
    unify::{contains_infer, is_assignable},
};
use crate::{
    ast::{
        BlockNode, CallNode, ExprKind, ExprNode, FieldAccessNode, FuncNode, FuncParam, Ident, Lit,
        MethodReceiver, Mutability, Type, TypeParam, VariantKind,
    },
    span::Span,
};

fn paths_alias(a: &[Ident], b: &[Ident]) -> bool {
    a.iter().zip(b).all(|(x, y)| x == y)
}

pub(super) fn check_var_param_args(
    params: impl IntoIterator<Item = (Ident, Mutability)>,
    args: &[ExprNode],
    type_checker: &TypeChecker,
    errors: &mut Vec<Diagnostic>,
) {
    let mut var_paths: Vec<(Ident, Ident, Vec<Ident>)> = vec![]; // (param_name, root, field_path)

    for ((param_name, mutability), arg) in params.into_iter().zip(args.iter()) {
        if mutability != Mutability::Mutable {
            continue;
        }

        if let ExprKind::Index(_) = &arg.node.kind {
            let mut target = arg;
            while let ExprKind::Index(idx) = &target.node.kind {
                target = &idx.node.target;
            }
            let ExprKind::Ident(root) = &target.node.kind else {
                errors.push(
                    Diagnostic::new(
                        arg.span,
                        DiagnosticKind::VarParamNotLvalue { param: param_name },
                    )
                    .with_help(
                        "only indexed variables (e.g., arr[i], matrix[i][j]) can be \
                         passed to 'var' parameters; extract to a 'var' variable first",
                    ),
                );
                continue;
            };
            let root = *root;
            let Some(info) = type_checker.get_var(root) else {
                continue;
            };
            if !info.mutable {
                errors.push(
                    Diagnostic::new(
                        arg.span,
                        DiagnosticKind::VarParamImmutableBinding {
                            param: param_name,
                            binding: root,
                        },
                    )
                    .with_help("declare with 'var' to allow mutation"),
                );
                continue;
            }
            for (prev_param, prev_root, _) in &var_paths {
                if root == *prev_root {
                    errors.push(
                        Diagnostic::new(
                            arg.span,
                            DiagnosticKind::VarParamAliasing {
                                param_a: *prev_param,
                                param_b: param_name,
                                binding: root,
                            },
                        )
                        .with_help(
                            "each 'var' parameter must refer to a distinct variable or field",
                        ),
                    );
                }
            }
            var_paths.push((param_name, root, vec![]));
            continue;
        }

        let Some((root, path)) = field_path(arg) else {
            errors.push(
                Diagnostic::new(
                    arg.span,
                    DiagnosticKind::VarParamNotLvalue { param: param_name },
                )
                .with_help("pass a variable declared with 'var', not a literal or expression"),
            );
            continue;
        };

        let Some(info) = type_checker.get_var(root) else {
            continue;
        };

        if !info.mutable {
            errors.push(
                Diagnostic::new(
                    arg.span,
                    DiagnosticKind::VarParamImmutableBinding {
                        param: param_name,
                        binding: root,
                    },
                )
                .with_help("declare with 'var' to allow mutation"),
            );
            continue;
        }

        for (prev_param, prev_root, prev_path) in &var_paths {
            if root == *prev_root && paths_alias(prev_path, &path) {
                errors.push(
                    Diagnostic::new(
                        arg.span,
                        DiagnosticKind::VarParamAliasing {
                            param_a: *prev_param,
                            param_b: param_name,
                            binding: root,
                        },
                    )
                    .with_help("each 'var' parameter must refer to a distinct variable or field"),
                );
            }
        }

        var_paths.push((param_name, root, path));
    }
}

pub(super) fn check_receiver_mutability(
    target: &ExprNode,
    type_label: Ident,
    method_name: Ident,
    type_checker: &TypeChecker,
    errors: &mut Vec<Diagnostic>,
) {
    let Some(root) = root_ident(target) else {
        // Receiver is a temporary (function return, constructor, etc.).
        // Temporaries are implicitly mutable — they are unaliased owned
        // values with no binding to protect. var self calls are always valid.
        return;
    };
    let Some(info) = type_checker.get_var(root) else {
        return;
    };
    if !info.mutable {
        errors.push(
            Diagnostic::new(
                target.span,
                DiagnosticKind::MutatingMethodOnImmutable {
                    struct_name: type_label,
                    method: method_name,
                },
            )
            .with_help("declare with 'var' to allow calling mutating methods"),
        );
    }
}

pub(super) fn check_var_self_aliasing(
    receiver: &ExprNode,
    params: impl IntoIterator<Item = (Ident, Mutability)>,
    args: &[ExprNode],
    errors: &mut Vec<Diagnostic>,
) {
    let Some((receiver_root, receiver_path)) = field_path(receiver) else {
        // receiver is a temporary, no named root, so it cannot alias any var parameter
        // aliasing check is not needed
        return;
    };

    let self_name = Ident(Intern::new("self".to_string()));

    for ((param_name, mutability), arg) in params.into_iter().zip(args.iter()) {
        if mutability != Mutability::Mutable {
            continue;
        }
        let Some((arg_root, arg_path)) = field_path(arg) else {
            continue;
        };
        if arg_root == receiver_root && paths_alias(&receiver_path, &arg_path) {
            errors.push(
                Diagnostic::new(
                    arg.span,
                    DiagnosticKind::VarParamAliasing {
                        param_a: self_name,
                        param_b: param_name,
                        binding: receiver_root,
                    },
                )
                .with_help(
                    "receiver and argument refer to the same variable or overlapping fields",
                ),
            );
        }
    }
}

fn list_type_label() -> Ident {
    use std::sync::LazyLock;
    static LABEL: LazyLock<Ident> = LazyLock::new(|| Ident(Intern::new("list".to_string())));
    *LABEL
}

fn map_type_label() -> Ident {
    use std::sync::LazyLock;
    static LABEL: LazyLock<Ident> = LazyLock::new(|| Ident(Intern::new("map".to_string())));
    *LABEL
}

fn check_predicate_combinator(
    call: &CallNode,
    elem: &Type,
    ret_type: &Type,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) -> Type {
    let func_param = Type::Func {
        params: vec![FuncParam::immut(elem.clone())],
        ret: Box::new(Type::Bool),
    };
    let param_types = [func_param];
    check_call_signature(
        call.span,
        &param_types,
        param_types.len(),
        ret_type,
        &call.node.args,
        type_checker,
        errors,
    )
}

fn callback_param_is_mutable(type_checker: &TypeChecker, arg: Option<&ExprNode>) -> bool {
    let Some(arg) = arg else {
        return false;
    };
    match &arg.node.kind {
        ExprKind::Lambda(lambda) => lambda
            .node
            .params
            .first()
            .is_some_and(|param| param.mutable),
        ExprKind::Ident(name) => type_checker
            .get_var(*name)
            .and_then(|info| match &info.ty {
                Type::Func { params, .. } => params.first(),
                _ => None,
            })
            .is_some_and(|param| param.mutable),
        _ => false,
    }
}

fn check_for_each_var_mutation(
    target: &ExprNode,
    args: &[ExprNode],
    type_checker: &TypeChecker,
    errors: &mut Vec<Diagnostic>,
) {
    let Some(first_arg) = args.first() else {
        return;
    };

    if let ExprKind::Lambda(lambda) = &first_arg.node.kind {
        if let Some(first_param) = lambda.node.params.first()
            && first_param.mutable
            && let Some(root) = root_ident(target)
            && let Some(info) = type_checker.get_var(root)
            && !info.mutable
        {
            errors.push(
                Diagnostic::new(
                    first_arg.span,
                    DiagnosticKind::MutableParamRequiresVarTarget {
                        name: first_param.name,
                    },
                )
                .with_help("declare the collection with 'var' to allow in-place mutation"),
            );
        }
        return;
    }

    if let Some((_, arg_ty)) = type_checker.get_type(first_arg.node.id) {
        let arg_ty = arg_ty.clone();
        if let Type::Func { params, .. } = &arg_ty
            && let Some(first_fp) = params.first()
            && first_fp.mutable
            && let Some(root) = root_ident(target)
            && let Some(info) = type_checker.get_var(root)
            && !info.mutable
        {
            errors.push(
                Diagnostic::new(
                    first_arg.span,
                    DiagnosticKind::MutableFnParamRequiresVarTarget,
                )
                .with_help("declare the collection with 'var' to allow in-place mutation"),
            );
        }
    }
}

fn infer_lambda_return_type(args: &[ExprNode], type_checker: &TypeChecker) -> Type {
    match args.first().and_then(|a| type_checker.get_type(a.node.id)) {
        Some((_, Type::Func { ret, .. })) => (**ret).clone(),
        _ => Type::Infer,
    }
}

pub(super) fn check_list_method(
    call: &CallNode,
    target: &ExprNode,
    method_name: Ident,
    elem: &Type,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) -> Option<Type> {
    let label = list_type_label();
    let node = &call.node;
    Some(match method_name.0.as_ref().as_str() {
        "push" => {
            check_receiver_mutability(target, label, method_name, type_checker, errors);
            let param_types = std::slice::from_ref(elem);
            check_call_signature(
                call.span,
                param_types,
                param_types.len(),
                &Type::Void,
                &node.args,
                type_checker,
                errors,
            )
        }
        "pop" => {
            check_receiver_mutability(target, label, method_name, type_checker, errors);
            check_call_signature(
                call.span,
                &[],
                0,
                &Type::option_of(elem.clone()),
                &node.args,
                type_checker,
                errors,
            )
        }
        "map" => {
            let func_param = Type::Func {
                params: vec![FuncParam::immut(elem.clone())],
                ret: Box::new(Type::Infer),
            };
            let param_types = [func_param];
            check_call_signature(
                call.span,
                &param_types,
                param_types.len(),
                &Type::Infer,
                &node.args,
                type_checker,
                errors,
            );
            let result_elem = infer_lambda_return_type(&node.args, type_checker);
            Type::List {
                elem: Box::new(result_elem),
            }
        }
        "filter" => {
            let ret = Type::List {
                elem: Box::new(elem.clone()),
            };
            check_predicate_combinator(call, elem, &ret, type_checker, errors)
        }
        "fold" => {
            if node.args.len() != 2 {
                let expected = 2;
                let found = node.args.len();
                let kind = if found < expected {
                    DiagnosticKind::TooFewArguments { expected, found }
                } else {
                    DiagnosticKind::TooManyArguments { expected, found }
                };
                errors.push(Diagnostic::new(call.span, kind));
                return Some(Type::Infer);
            }
            // typecheck the init value first so we can use its concrete type
            // for the accumulator parameter in the lambda's expected type
            check_expr(&node.args[0], type_checker, errors, None);
            let init_ty = match type_checker.get_type(node.args[0].node.id) {
                Some((_, ty)) => ty.clone(),
                None => Type::Infer,
            };
            let func_param = Type::Func {
                params: vec![
                    FuncParam::immut(init_ty.clone()),
                    FuncParam::immut(elem.clone()),
                ],
                ret: Box::new(init_ty.clone()),
            };
            check_expr(&node.args[1], type_checker, errors, Some(&func_param));
            let arg_ref = TypeRef::Expr(node.args[1].node.id);
            let param_ref = TypeRef::concrete(&func_param);
            type_checker.constrain_assignable(node.args[1].span, arg_ref, param_ref, errors);
            init_ty
        }
        "for_each" => {
            let is_mutating = callback_param_is_mutable(type_checker, node.args.first());
            let func_param = Type::Func {
                params: vec![FuncParam::new(elem.clone(), is_mutating)],
                ret: Box::new(Type::Void),
            };
            let param_types = [func_param];
            let result = check_call_signature(
                call.span,
                &param_types,
                param_types.len(),
                &Type::Void,
                &node.args,
                type_checker,
                errors,
            );
            check_for_each_var_mutation(target, &node.args, type_checker, errors);
            result
        }
        "any" | "all" => check_predicate_combinator(call, elem, &Type::Bool, type_checker, errors),
        "find" => {
            let ret = Type::option_of(elem.clone());
            check_predicate_combinator(call, elem, &ret, type_checker, errors)
        }
        "find_index" => {
            let ret = Type::option_of(Type::Int);
            check_predicate_combinator(call, elem, &ret, type_checker, errors)
        }
        "count" => check_predicate_combinator(call, elem, &Type::Int, type_checker, errors),
        "len" => check_call_signature(
            call.span,
            &[],
            0,
            &Type::Int,
            &node.args,
            type_checker,
            errors,
        ),
        "sort_by" => {
            check_receiver_mutability(target, label, method_name, type_checker, errors);
            let func_param = Type::Func {
                params: vec![
                    FuncParam::immut(elem.clone()),
                    FuncParam::immut(elem.clone()),
                ],
                ret: Box::new(Type::Bool),
            };
            let param_types = [func_param];
            check_call_signature(
                call.span,
                &param_types,
                param_types.len(),
                &Type::Void,
                &node.args,
                type_checker,
                errors,
            )
        }
        _ => return None,
    })
}

pub(super) fn check_map_method(
    call: &CallNode,
    target: &ExprNode,
    method_name: Ident,
    key: &Type,
    value: &Type,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) -> Option<Type> {
    let label = map_type_label();
    let node = &call.node;
    Some(match method_name.0.as_ref().as_str() {
        "insert" => {
            check_receiver_mutability(target, label, method_name, type_checker, errors);
            let param_types = [key.clone(), value.clone()];
            check_call_signature(
                call.span,
                &param_types,
                param_types.len(),
                &Type::Void,
                &node.args,
                type_checker,
                errors,
            )
        }
        "remove" => {
            check_receiver_mutability(target, label, method_name, type_checker, errors);
            let param_types = std::slice::from_ref(key);
            check_call_signature(
                call.span,
                param_types,
                param_types.len(),
                &Type::option_of(value.clone()),
                &node.args,
                type_checker,
                errors,
            )
        }
        "map_values" => {
            let func_param = Type::Func {
                params: vec![FuncParam::immut(value.clone())],
                ret: Box::new(Type::Infer),
            };
            let param_types = [func_param];
            check_call_signature(
                call.span,
                &param_types,
                param_types.len(),
                &Type::Infer,
                &node.args,
                type_checker,
                errors,
            );
            let result_value = infer_lambda_return_type(&node.args, type_checker);
            Type::Map {
                key: Box::new(key.clone()),
                value: Box::new(result_value),
            }
        }
        "filter" => {
            let func_param = Type::Func {
                params: vec![
                    FuncParam::immut(key.clone()),
                    FuncParam::immut(value.clone()),
                ],
                ret: Box::new(Type::Bool),
            };
            let param_types = [func_param];
            let ret_type = Type::Map {
                key: Box::new(key.clone()),
                value: Box::new(value.clone()),
            };
            check_call_signature(
                call.span,
                &param_types,
                param_types.len(),
                &ret_type,
                &node.args,
                type_checker,
                errors,
            )
        }
        _ => return None,
    })
}

fn check_generic_call(
    call: &CallNode,
    func_name: Ident,
    func_ty: &Type,
    type_params: &[TypeParam],
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
    expected: Option<&Type>,
) -> Type {
    let node = &call.node;
    let has_type_args = !node.type_args.is_empty();
    let const_params = type_checker
        .func_const_params
        .get(&func_name)
        .cloned()
        .unwrap_or_default();

    let (type_args, const_args) = if has_type_args {
        let same_param_count = type_params.len() == node.type_args.len();
        if !same_param_count {
            errors.push(Diagnostic::new(
                call.span,
                DiagnosticKind::GenericArgNumMismatch {
                    expected: type_params.len(),
                    found: node.type_args.len(),
                },
            ));
            return Type::Infer;
        }

        let Type::Func { params, ret: _ } = func_ty else {
            errors.push(Diagnostic::new(
                call.span,
                DiagnosticKind::NotAFunction {
                    expr_type: func_ty.clone(),
                },
            ));
            return Type::Infer;
        };

        let defaults = type_checker.func_param_defaults(func_name);
        let required_count = required_param_count(defaults, params.len());
        if node.args.len() < required_count {
            errors.push(Diagnostic::new(
                call.span,
                DiagnosticKind::TooFewArguments {
                    expected: required_count,
                    found: node.args.len(),
                },
            ));
            return Type::Infer;
        }
        if node.args.len() > params.len() {
            errors.push(Diagnostic::new(
                call.span,
                DiagnosticKind::TooManyArguments {
                    expected: params.len(),
                    found: node.args.len(),
                },
            ));
            return Type::Infer;
        }

        let subst = build_subst(type_params, &node.type_args);

        type_checker.push_const_params(&const_params);
        let resolved_param_tys: Vec<Type> = params
            .iter()
            .map(|p| type_checker.resolve_type(&p.ty))
            .collect();
        type_checker.pop_const_params(const_params.len());

        for (arg_expr, resolved_ty) in node.args.iter().zip(resolved_param_tys.iter()) {
            let instantiated = subst_type(resolved_ty, &subst, &HashMap::new());
            check_and_constrain_arg(arg_expr, &instantiated, type_checker, errors);
        }

        let inferred_const_args = infer_const_args_from_checked_args(
            &resolved_param_tys,
            &node.args,
            &const_params,
            type_checker,
        );
        (node.type_args.clone(), inferred_const_args)
    } else {
        let Type::Func { params, ret } = func_ty else {
            errors.push(Diagnostic::new(
                call.span,
                DiagnosticKind::NotAFunction {
                    expr_type: func_ty.clone(),
                },
            ));
            return Type::Infer;
        };

        let param_tys: Vec<Type> = params.iter().map(|p| p.ty.clone()).collect();
        // put const params in scope so inference can resolve named lengths and extract their values
        type_checker.push_const_params(&const_params);
        let result = infer_type_args_from_call(
            call.span,
            type_params,
            &const_params,
            &param_tys,
            &node.args,
            func_ty.clone(),
            ret,
            expected,
            type_checker,
            errors,
        );
        type_checker.pop_const_params(const_params.len());
        let Some((inferred, inferred_const_args)) = result else {
            return Type::Infer;
        };

        (inferred, inferred_const_args)
    };

    if let Some(param_info) = type_checker.func_param_info.get(&func_name).cloned() {
        check_var_param_args(param_info, &node.args, type_checker, errors);
    }

    let ret = instantiate_and_check_fn(
        func_name,
        &type_args,
        &const_args,
        call.span,
        type_checker,
        errors,
    );
    type_checker
        .resolved_call_type_args
        .insert(node.func.node.id, (func_name, type_args, const_args));
    ret
}

pub(super) fn check_call(
    call: &CallNode,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
    expected: Option<&Type>,
) -> Type {
    let node = &call.node;

    if let ExprKind::Field(field_access) = &node.func.node.kind
        && let Some(result) = try_check_method_call(call, field_access, type_checker, errors)
    {
        return result;
    }

    let func_ty = check_expr(&node.func, type_checker, errors, None);

    // try to get the function name to look up type parameters
    let func_name = match &node.func.node.kind {
        ExprKind::Ident(ident) => Some(*ident),
        _ => None,
    };

    // lookup type params for generic functions
    let type_params = func_name
        .and_then(|name| type_checker.func_type_params.get(&name))
        .cloned()
        .unwrap_or_default();

    let has_const_params = func_name
        .and_then(|name| type_checker.func_const_params.get(&name))
        .is_some_and(|cp| !cp.is_empty());

    let is_generic = !type_params.is_empty() || has_const_params;
    let has_type_args = !node.type_args.is_empty();

    // handle generic function calls with template instantiation
    match (is_generic, has_type_args) {
        (false, true) => {
            errors.push(Diagnostic::new(
                call.span,
                DiagnosticKind::NotGenericFunction,
            ));
            return Type::Infer;
        }
        (true, _) => {
            let Some(name) = func_name else {
                errors.push(Diagnostic::new(
                    call.span,
                    DiagnosticKind::NotAFunction {
                        expr_type: func_ty.clone(),
                    },
                ));
                return Type::Infer;
            };
            return check_generic_call(
                call,
                name,
                &func_ty,
                &type_params,
                type_checker,
                errors,
                expected,
            );
        }
        (false, false) => {}
    }

    // fallback to normal call
    let required_count = if let Some(name) = func_name {
        let defaults = type_checker.func_param_defaults(name);
        match &func_ty {
            Type::Func { params, .. } => required_param_count(defaults, params.len()),
            _ => 0,
        }
    } else {
        // fn pointer — all params are required
        match &func_ty {
            Type::Func { params, .. } => params.len(),
            _ => 0,
        }
    };
    let cast_accepts: Vec<bool> = func_name
        .and_then(|n| type_checker.func_cast_accept.get(&n))
        .cloned()
        .unwrap_or_default();
    let result = check_call_with_type(
        call,
        func_ty,
        required_count,
        &cast_accepts,
        type_checker,
        errors,
    );

    if let Some(name) = func_name
        && let Some(param_info) = type_checker.func_param_info.get(&name).cloned()
    {
        check_var_param_args(param_info, &node.args, type_checker, errors);
    }

    result
}

fn check_and_constrain_arg(
    arg_expr: &ExprNode,
    param_ty: &Type,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) {
    check_expr(arg_expr, type_checker, errors, Some(param_ty));
    let arg_ref = TypeRef::Expr(arg_expr.node.id);
    let param_ref = TypeRef::concrete(param_ty);
    type_checker.constrain_assignable(arg_expr.span, arg_ref, param_ref, errors);
}

fn check_cast_accept_arg(
    call_func_expr_id: crate::ast::ExprId,
    param_idx: usize,
    arg_expr: &ExprNode,
    param_ty: &Type,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) {
    check_expr(arg_expr, type_checker, errors, Some(param_ty));

    let arg_ty = type_checker
        .get_type(arg_expr.node.id)
        .map_or(Type::Infer, |(_, ty)| ty.clone());

    // if types already match or arg is unresolved constrain normally
    if is_assignable(&arg_ty, param_ty) || contains_infer(&arg_ty) {
        let arg_ref = TypeRef::Expr(arg_expr.node.id);
        let param_ref = TypeRef::concrete(param_ty);
        type_checker.constrain_assignable(arg_expr.span, arg_ref, param_ref, errors);
        return;
    }

    // if types differ look for a cast_from entry
    let resolved_param_ty = type_checker.resolve_type(param_ty);
    if let Some(entry) = type_checker
        .ctx
        .cast_defs
        .get(&(arg_ty.clone(), resolved_param_ty.clone()))
    {
        type_checker.cast_call_insertions.insert(
            (call_func_expr_id, param_idx),
            (entry.internal_name, resolved_param_ty),
        );
    } else {
        let available: Vec<String> = type_checker
            .ctx
            .cast_defs
            .iter()
            .filter(|((_, target), _)| *target == resolved_param_ty)
            .map(|((source, _), _)| format!("{source}"))
            .collect();
        let mut diag = Diagnostic::new(
            arg_expr.span,
            DiagnosticKind::MismatchedTypes {
                expected: param_ty.clone(),
                found: arg_ty,
            },
        );
        if !available.is_empty() {
            diag = diag.with_help(format!(
                "available conversions to '{}': {}",
                resolved_param_ty,
                available.join(", ")
            ));
        }
        errors.push(diag);
    }
}

pub(super) fn check_call_signature_with_cast(
    call_span: Span,
    call_func_expr_id: crate::ast::ExprId,
    param_types: &[Type],
    cast_accepts: &[bool],
    required_count: usize,
    ret_type: &Type,
    args: &[ExprNode],
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) -> Type {
    let total_count = param_types.len();

    if args.len() < required_count {
        errors.push(Diagnostic::new(
            call_span,
            DiagnosticKind::TooFewArguments {
                expected: required_count,
                found: args.len(),
            },
        ));
        return Type::Infer;
    }
    if args.len() > total_count {
        errors.push(Diagnostic::new(
            call_span,
            DiagnosticKind::TooManyArguments {
                expected: total_count,
                found: args.len(),
            },
        ));
        return Type::Infer;
    }

    for (idx, (arg_expr, param_ty)) in args.iter().zip(param_types.iter()).enumerate() {
        if cast_accepts.get(idx).copied().unwrap_or(false) {
            check_cast_accept_arg(
                call_func_expr_id,
                idx,
                arg_expr,
                param_ty,
                type_checker,
                errors,
            );
        } else {
            check_expr(arg_expr, type_checker, errors, Some(param_ty));
            let arg_ref = TypeRef::Expr(arg_expr.node.id);
            let param_ref = TypeRef::concrete(param_ty);
            type_checker.constrain_assignable(arg_expr.span, arg_ref, param_ref, errors);
        }
    }

    ret_type.clone()
}

pub(super) fn resolve_cast_accept_errors(
    call_func_expr_id: crate::ast::ExprId,
    cast_accepts: &[bool],
    param_types: &[Type],
    args: &[ExprNode],
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) {
    for (idx, ((arg_expr, param_ty), &is_cast)) in args
        .iter()
        .zip(param_types.iter())
        .zip(cast_accepts.iter())
        .enumerate()
    {
        if !is_cast {
            continue;
        }
        let arg_ty = match type_checker.get_type(arg_expr.node.id) {
            Some((_, ty)) => ty.clone(),
            None => continue,
        };
        let resolved_param_ty = type_checker.resolve_type(param_ty);
        if is_assignable(&arg_ty, &resolved_param_ty) {
            continue;
        }

        if let Some(entry) = type_checker
            .ctx
            .cast_defs
            .get(&(arg_ty.clone(), resolved_param_ty.clone()))
        {
            type_checker.cast_call_insertions.insert(
                (call_func_expr_id, idx),
                (entry.internal_name, resolved_param_ty.clone()),
            );
            // remove the MismatchedTypes error that constrain_assignable added for this arg
            errors.retain(|e| {
                !(e.span == arg_expr.span
                    && matches!(&e.kind, DiagnosticKind::MismatchedTypes { expected, found }
                        if expected == &resolved_param_ty && found == &arg_ty))
            });
        }
    }
}

pub(super) fn required_param_count(defaults: &[Option<ConstValue>], total: usize) -> usize {
    if defaults.is_empty() {
        total
    } else {
        defaults.iter().take_while(|d| d.is_none()).count()
    }
}

pub(super) fn check_call_signature(
    call_span: Span,
    param_types: &[Type],
    required_count: usize,
    ret_type: &Type,
    args: &[ExprNode],
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) -> Type {
    let total_count = param_types.len();

    if args.len() < required_count {
        errors.push(Diagnostic::new(
            call_span,
            DiagnosticKind::TooFewArguments {
                expected: required_count,
                found: args.len(),
            },
        ));
        return Type::Infer;
    }
    if args.len() > total_count {
        errors.push(Diagnostic::new(
            call_span,
            DiagnosticKind::TooManyArguments {
                expected: total_count,
                found: args.len(),
            },
        ));
        return Type::Infer;
    }

    for (arg_expr, param_ty) in args.iter().zip(param_types.iter()) {
        check_expr(arg_expr, type_checker, errors, Some(param_ty));
        let arg_ref = TypeRef::Expr(arg_expr.node.id);
        let param_ref = TypeRef::concrete(param_ty);
        type_checker.constrain_assignable(arg_expr.span, arg_ref, param_ref, errors);
    }

    ret_type.clone()
}

fn check_call_with_type(
    call: &CallNode,
    func_ty: Type,
    required_count: usize,
    cast_accepts: &[bool],
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) -> Type {
    let node = &call.node;

    if let Type::Func { params, ret } = &func_ty {
        let param_tys: Vec<Type> = params.iter().map(|p| p.ty.clone()).collect();
        if cast_accepts.is_empty() {
            check_call_signature(
                call.span,
                &param_tys,
                required_count,
                ret,
                &node.args,
                type_checker,
                errors,
            )
        } else {
            check_call_signature_with_cast(
                call.span,
                node.func.node.id,
                &param_tys,
                cast_accepts,
                required_count,
                ret,
                &node.args,
                type_checker,
                errors,
            )
        }
    } else {
        errors.push(Diagnostic::new(
            call.span,
            DiagnosticKind::NotAFunction { expr_type: func_ty },
        ));
        Type::Infer
    }
}

pub(super) fn check_module_func_call(
    call: &CallNode,
    module_name: Ident,
    func_name: Ident,
    module_def: &ModuleDef,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
    expected: Option<&Type>,
) -> Type {
    let Some(func_ty) = module_def.funcs.get(&func_name).cloned() else {
        let err_kind = if module_def.all_names.contains(&func_name) {
            DiagnosticKind::PrivateModuleMember {
                module: module_name,
                member: func_name,
            }
        } else {
            DiagnosticKind::UnknownModuleMember {
                module: module_name,
                member: func_name,
            }
        };
        errors.push(Diagnostic::new(call.span, err_kind));
        return Type::Infer;
    };

    let type_params = module_def.func_type_params.get(&func_name).cloned();

    if let Some(type_params) = type_params.filter(|tp| !tp.is_empty()) {
        let template = module_def
            .generic_func_templates
            .get(&func_name)
            .cloned()
            .expect("generic template must exist when type_params are present");

        // temporarily register the template and its source module so instantiate_and_check_fn can look up the full ModuleCheckContext
        let prev_tp = type_checker.func_type_params.remove(&func_name);
        let prev_tmpl = type_checker.generic_func_templates.remove(&func_name);
        let prev_source = type_checker.generic_func_source_module.remove(&func_name);
        type_checker
            .func_type_params
            .insert(func_name, type_params.clone());
        type_checker
            .generic_func_templates
            .insert(func_name, template);
        type_checker
            .generic_func_source_module
            .insert(func_name, module_def.source_path.clone());

        let node = &call.node;
        let has_explicit_type_args = !node.type_args.is_empty();
        let module_const_params = module_def
            .func_const_params
            .get(&func_name)
            .cloned()
            .unwrap_or_default();

        let result = if has_explicit_type_args {
            let const_args = if let Type::Func { params, .. } = &func_ty {
                let param_tys: Vec<Type> = params.iter().map(|p| p.ty.clone()).collect();
                infer_const_args_from_checked_args(
                    &param_tys,
                    &node.args,
                    &module_const_params,
                    type_checker,
                )
            } else {
                vec![]
            };
            let ret = instantiate_and_check_fn(
                func_name,
                &node.type_args,
                &const_args,
                call.span,
                type_checker,
                errors,
            );
            type_checker.resolved_call_type_args.insert(
                node.func.node.id,
                (func_name, node.type_args.clone(), const_args),
            );
            ret
        } else {
            let Type::Func { params, ret } = &func_ty else {
                errors.push(Diagnostic::new(
                    call.span,
                    DiagnosticKind::NotAFunction { expr_type: func_ty },
                ));
                restore_generic_maps(type_checker, func_name, prev_tp, prev_tmpl, prev_source);
                return Type::Infer;
            };

            let param_tys: Vec<Type> = params.iter().map(|p| p.ty.clone()).collect();
            let Some((inferred, inferred_const_args)) = infer_type_args_from_call(
                call.span,
                &type_params,
                &module_const_params,
                &param_tys,
                &node.args,
                func_ty.clone(),
                ret,
                expected,
                type_checker,
                errors,
            ) else {
                restore_generic_maps(type_checker, func_name, prev_tp, prev_tmpl, prev_source);
                return Type::Infer;
            };

            let ret = instantiate_and_check_fn(
                func_name,
                &inferred,
                &inferred_const_args,
                call.span,
                type_checker,
                errors,
            );
            type_checker.resolved_call_type_args.insert(
                node.func.node.id,
                (func_name, inferred, inferred_const_args),
            );
            ret
        };

        restore_generic_maps(type_checker, func_name, prev_tp, prev_tmpl, prev_source);

        if let Some(param_info) = module_def.func_param_info.get(&func_name).cloned() {
            check_var_param_args(param_info, &call.node.args, type_checker, errors);
        }

        return result;
    }

    let defaults: &[Option<ConstValue>] = module_def
        .func_param_defaults
        .get(&func_name)
        .map_or(&[], Vec::as_slice);
    let required_count = match &func_ty {
        Type::Func { params, .. } => required_param_count(defaults, params.len()),
        _ => 0,
    };
    let cast_accepts = module_def
        .func_cast_accept
        .get(&func_name)
        .cloned()
        .unwrap_or_default();
    let result = check_call_with_type(
        call,
        func_ty,
        required_count,
        &cast_accepts,
        type_checker,
        errors,
    );

    if let Some(param_info) = module_def.func_param_info.get(&func_name).cloned() {
        check_var_param_args(param_info, &call.node.args, type_checker, errors);
    }

    result
}

fn restore_or_remove<V>(map: &mut HashMap<Ident, V>, key: Ident, prev: Option<V>) {
    match prev {
        Some(val) => {
            map.insert(key, val);
        }
        None => {
            map.remove(&key);
        }
    }
}

fn restore_generic_maps(
    tc: &mut TypeChecker,
    name: Ident,
    prev_tp: Option<Vec<TypeParam>>,
    prev_tmpl: Option<FuncNode>,
    prev_source: Option<Vec<String>>,
) {
    restore_or_remove(&mut tc.func_type_params, name, prev_tp);
    restore_or_remove(&mut tc.generic_func_templates, name, prev_tmpl);
    restore_or_remove(&mut tc.generic_func_source_module, name, prev_source);
}

fn try_check_method_call(
    call: &CallNode,
    field_access: &FieldAccessNode,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) -> Option<Type> {
    let method_name = field_access.node.field;
    let target = &field_access.node.target;

    if let ExprKind::Ident(type_name) = &target.node.kind
        && let Some(enum_def) = type_checker.get_enum(*type_name).cloned()
    {
        return Some(check_enum_tuple_variant(
            call,
            *type_name,
            method_name,
            &enum_def,
            type_checker,
            errors,
        ));
    }

    if let ExprKind::Ident(type_name) = &target.node.kind
        && let Some(struct_def) = type_checker.get_struct(*type_name).cloned()
    {
        return Some(check_static_method_call(
            call,
            *type_name,
            method_name,
            &struct_def,
            type_checker,
            errors,
        ));
    }

    let target_ty = check_expr(target, type_checker, errors, None);
    if let Some(agg) = target_ty.as_aggregate() {
        match type_checker.get_struct_deep(agg.name, agg.origin) {
            DeepLookup::Found(def) => {
                let struct_def = def.clone();
                return Some(check_instance_method_call(
                    call,
                    agg.name,
                    method_name,
                    agg.type_args,
                    &struct_def,
                    Some(target),
                    type_checker,
                    errors,
                ));
            }
            DeepLookup::Ambiguous(first, second) => {
                errors.push(Diagnostic::new(
                    field_access.span,
                    DiagnosticKind::AmbiguousType {
                        name: agg.name,
                        first_module: first,
                        second_module: second,
                    },
                ));
                return Some(Type::Infer);
            }
            DeepLookup::NotFound => {}
        }
    }

    resolve_builtin_or_extend(
        &target_ty,
        method_name,
        call,
        Some(target),
        field_access.span,
        type_checker,
        errors,
    )
}

fn check_enum_tuple_variant(
    call: &CallNode,
    enum_name: Ident,
    variant_name: Ident,
    enum_def: &EnumDef,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) -> Type {
    let variant = enum_def.variants.iter().find(|v| v.name == variant_name);

    let Some(variant) = variant else {
        errors.push(Diagnostic::new(
            call.span,
            DiagnosticKind::UnknownEnumVariant {
                enum_name,
                variant_name,
            },
        ));
        return Type::Infer;
    };

    enum_def.check_deprecation(enum_name, variant, call.span, errors);

    let VariantKind::Tuple(expected_types) = &variant.kind else {
        // not a tuple variant
        match &variant.kind {
            VariantKind::Unit | VariantKind::Struct(_) => {
                errors.push(Diagnostic::new(
                    call.span,
                    DiagnosticKind::EnumVariantNotTuple {
                        enum_name,
                        variant_name,
                    },
                ));
            }
            VariantKind::Tuple(_) => unreachable!(),
        }
        return Type::Infer;
    };

    let node = &call.node;
    let is_generic = !enum_def.type_params.is_empty();

    // create inference slots for generic enums
    let slots = if is_generic {
        let call_id = type_checker.next_call_id();
        create_inference_slots(&enum_def.type_params, type_checker, call_id)
    } else {
        HashMap::new()
    };
    let mut enum_const_slots: ConstInferenceSlots = HashMap::new();

    // check argument count
    if node.args.len() != expected_types.len() {
        errors.push(Diagnostic::new(
            call.span,
            DiagnosticKind::EnumVariantArityMismatch {
                enum_name,
                variant_name,
                expected: expected_types.len(),
                found: node.args.len(),
            },
        ));
        return Type::Infer;
    }

    // check each argument
    for (arg_expr, expected_ty) in node.args.iter().zip(expected_types.iter()) {
        check_expr(arg_expr, type_checker, errors, None);
        let arg_ref = TypeRef::Expr(arg_expr.node.id);
        let expected_ref = if is_generic {
            if let Some((_, arg_ty)) = type_checker.get_type(arg_expr.node.id) {
                let arg_ty = arg_ty.clone();
                constrain_slots_from_type(
                    expected_ty,
                    &arg_ty,
                    &slots,
                    &mut enum_const_slots,
                    arg_expr.span,
                    type_checker,
                    errors,
                );
            }
            build_param_ref(expected_ty, &slots, &enum_const_slots, type_checker)
        } else {
            let resolved = type_checker.resolve_type(expected_ty);
            TypeRef::concrete(&resolved)
        };
        type_checker.constrain_assignable(arg_expr.span, arg_ref, expected_ref, errors);
    }

    if is_generic {
        resolve_constraints(type_checker, errors);
    }

    // build the result type
    let type_args = if is_generic {
        enum_def
            .type_params
            .iter()
            .map(|param| {
                let slot_name = slots.get(&param.id).expect("slot exists");
                type_checker
                    .get_var(*slot_name)
                    .map_or(Type::Infer, |info| info.ty.clone())
            })
            .collect()
    } else {
        vec![]
    };

    enum_def.make_type(enum_name, type_args)
}

fn check_generic_static_method(
    call: &CallNode,
    struct_name: Ident,
    method_name: Ident,
    method: &MethodDef,
    struct_def: &StructDef,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) -> Type {
    let node = &call.node;
    let is_struct_generic = !struct_def.type_params.is_empty();
    let has_explicit_type_args = !node.type_args.is_empty();

    let (struct_type_args, method_type_args) = if has_explicit_type_args {
        let same_count = node.type_args.len() == method.type_params.len();
        if !same_count {
            errors.push(Diagnostic::new(
                call.span,
                DiagnosticKind::GenericArgNumMismatch {
                    expected: method.type_params.len(),
                    found: node.type_args.len(),
                },
            ));
            return Type::Infer;
        }

        let struct_type_args = if is_struct_generic {
            let param_templates: Vec<Type> = method.params.iter().map(|p| p.ty.clone()).collect();
            let expected_ty = Type::Func {
                params: param_templates
                    .iter()
                    .map(|t| FuncParam::immut(t.clone()))
                    .collect(),
                ret: Box::new(method.ret.clone()),
            };
            let Some((inferred, _)) = infer_type_args_from_call(
                call.span,
                &struct_def.type_params,
                &[],
                &param_templates,
                &node.args,
                expected_ty,
                &method.ret,
                None,
                type_checker,
                errors,
            ) else {
                return Type::Infer;
            };
            inferred
        } else {
            vec![]
        };

        (struct_type_args, node.type_args.clone())
    } else {
        let all_type_params: Vec<_> = struct_def
            .type_params
            .iter()
            .chain(method.type_params.iter())
            .cloned()
            .collect();

        let param_templates: Vec<Type> = method.params.iter().map(|p| p.ty.clone()).collect();
        let expected_ty = Type::Func {
            params: param_templates
                .iter()
                .map(|t| FuncParam::immut(t.clone()))
                .collect(),
            ret: Box::new(method.ret.clone()),
        };

        let Some((all_inferred, _)) = infer_type_args_from_call(
            call.span,
            &all_type_params,
            &[],
            &param_templates,
            &node.args,
            expected_ty,
            &method.ret,
            None,
            type_checker,
            errors,
        ) else {
            return Type::Infer;
        };

        let n_struct = struct_def.type_params.len();
        let struct_type_args = all_inferred[..n_struct].to_vec();
        let method_type_args = all_inferred[n_struct..].to_vec();
        (struct_type_args, method_type_args)
    };

    check_var_param_args(
        method.params.iter().map(|p| (p.name, p.mutability)),
        &node.args,
        type_checker,
        errors,
    );

    instantiate_method_body(
        &MethodSpec {
            struct_name,
            method_name,
            struct_type_args: &struct_type_args,
            method_type_args: &method_type_args,
            struct_def,
            method,
        },
        call.span,
        type_checker,
        errors,
    )
}

pub(super) fn check_static_method_call(
    call: &CallNode,
    struct_name: Ident,
    method_name: Ident,
    struct_def: &StructDef,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) -> Type {
    let Some(method) = struct_def.methods.get(&method_name) else {
        errors.push(Diagnostic::new(
            call.span,
            DiagnosticKind::UnknownMethod {
                kind: struct_def.kind.keyword(),
                struct_name,
                method: method_name,
            },
        ));
        return Type::Infer;
    };

    if method.receiver.is_some() {
        errors.push(Diagnostic::new(
            call.span,
            DiagnosticKind::InstanceMethodOnType {
                struct_name,
                method: method_name,
            },
        ));
        return Type::Infer;
    }

    type_checker.check_member_access(
        &method.annotations,
        call.span,
        "method",
        method_name,
        struct_name,
        struct_def.defining_module.as_deref(),
        errors,
    );

    let node = &call.node;
    let has_method_type_params = !method.type_params.is_empty();
    let is_struct_generic = !struct_def.type_params.is_empty();

    if has_method_type_params {
        let method = method.clone();
        return check_generic_static_method(
            call,
            struct_name,
            method_name,
            &method,
            struct_def,
            type_checker,
            errors,
        );
    }

    if is_struct_generic {
        let param_templates: Vec<Type> = method.params.iter().map(|p| p.ty.clone()).collect();
        let expected_ty = Type::Func {
            params: param_templates
                .iter()
                .map(|t| FuncParam::immut(t.clone()))
                .collect(),
            ret: Box::new(method.ret.clone()),
        };
        let Some((inferred_type_args, _)) = infer_type_args_from_call(
            call.span,
            &struct_def.type_params,
            &[],
            &param_templates,
            &node.args,
            expected_ty,
            &method.ret,
            None,
            type_checker,
            errors,
        ) else {
            return Type::Infer;
        };

        let subst = build_subst(&struct_def.type_params, &inferred_type_args);

        let ret_ty = subst_type(&method.ret, &subst, &HashMap::new());

        check_var_param_args(
            method.params.iter().map(|p| (p.name, p.mutability)),
            &node.args,
            type_checker,
            errors,
        );

        let method = method.clone();
        instantiate_method_body(
            &MethodSpec {
                struct_name,
                method_name,
                struct_type_args: &inferred_type_args,
                method_type_args: &[],
                struct_def,
                method: &method,
            },
            call.span,
            type_checker,
            errors,
        );

        return ret_ty;
    }

    let param_types = method
        .params
        .iter()
        .map(|p| p.ty.clone())
        .collect::<Vec<_>>();
    let ret_type = method.ret.clone();
    let required = required_param_count(&method.param_defaults, param_types.len());
    let result = check_call_signature(
        call.span,
        &param_types,
        required,
        &ret_type,
        &node.args,
        type_checker,
        errors,
    );

    check_var_param_args(
        method.params.iter().map(|p| (p.name, p.mutability)),
        &node.args,
        type_checker,
        errors,
    );

    result
}

#[allow(clippy::too_many_arguments)]
fn check_generic_instance_method(
    call: &CallNode,
    struct_name: Ident,
    method_name: Ident,
    type_args: &[Type],
    struct_def: &StructDef,
    method: &MethodDef,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) -> Type {
    let node = &call.node;

    // apply struct substitution first, leaving method type vars in place
    let struct_subst = build_subst(&struct_def.type_params, type_args);

    let partially_subst_params: Vec<Type> = method
        .params
        .iter()
        .map(|p| subst_type(&p.ty, &struct_subst, &HashMap::new()))
        .collect();

    let has_explicit_type_args = !node.type_args.is_empty();

    let method_type_args = if has_explicit_type_args {
        let same_count = node.type_args.len() == method.type_params.len();
        if !same_count {
            errors.push(Diagnostic::new(
                call.span,
                DiagnosticKind::GenericArgNumMismatch {
                    expected: method.type_params.len(),
                    found: node.type_args.len(),
                },
            ));
            return Type::Infer;
        }

        let mut combined_subst = build_subst(&struct_def.type_params, type_args);
        combined_subst.extend(build_subst(&method.type_params, &node.type_args));

        for (arg_expr, param_ty) in node.args.iter().zip(partially_subst_params.iter()) {
            let instantiated = subst_type(param_ty, &combined_subst, &HashMap::new());
            check_and_constrain_arg(arg_expr, &instantiated, type_checker, errors);
        }

        node.type_args.clone()
    } else {
        let partially_subst_ret = subst_type(&method.ret, &struct_subst, &HashMap::new());
        let expected_ty = Type::Func {
            params: partially_subst_params
                .iter()
                .map(|t| FuncParam::immut(t.clone()))
                .collect(),
            ret: Box::new(partially_subst_ret.clone()),
        };

        let Some((inferred, _)) = infer_type_args_from_call(
            call.span,
            &method.type_params,
            &[],
            &partially_subst_params,
            &node.args,
            expected_ty,
            &partially_subst_ret,
            None,
            type_checker,
            errors,
        ) else {
            return Type::Infer;
        };

        inferred
    };

    check_var_param_args(
        method.params.iter().map(|p| (p.name, p.mutability)),
        &node.args,
        type_checker,
        errors,
    );

    instantiate_method_body(
        &MethodSpec {
            struct_name,
            method_name,
            struct_type_args: type_args,
            method_type_args: &method_type_args,
            struct_def,
            method,
        },
        call.span,
        type_checker,
        errors,
    )
}

#[allow(clippy::too_many_arguments)]
pub(super) fn check_instance_method_call(
    call: &CallNode,
    struct_name: Ident,
    method_name: Ident,
    type_args: &[Type],
    struct_def: &StructDef,
    target: Option<&ExprNode>,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) -> Type {
    let Some(method) = struct_def.methods.get(&method_name) else {
        errors.push(Diagnostic::new(
            call.span,
            DiagnosticKind::UnknownMethod {
                kind: struct_def.kind.keyword(),
                struct_name,
                method: method_name,
            },
        ));
        return Type::Infer;
    };

    if method.receiver.is_none() {
        errors.push(Diagnostic::new(
            call.span,
            DiagnosticKind::StaticMethodOnValue {
                struct_name,
                method: method_name,
            },
        ));
        return Type::Infer;
    }

    type_checker.check_member_access(
        &method.annotations,
        call.span,
        "method",
        method_name,
        struct_name,
        struct_def.defining_module.as_deref(),
        errors,
    );

    if let Some(target) = target
        && matches!(method.receiver, Some(MethodReceiver::Var))
    {
        check_receiver_mutability(target, struct_name, method_name, type_checker, errors);
        check_var_self_aliasing(
            target,
            method.params.iter().map(|p| (p.name, p.mutability)),
            &call.node.args,
            errors,
        );
    }

    let node = &call.node;
    let has_method_type_params = !method.type_params.is_empty();

    if has_method_type_params {
        let method = method.clone();
        return check_generic_instance_method(
            call,
            struct_name,
            method_name,
            type_args,
            struct_def,
            &method,
            type_checker,
            errors,
        );
    }

    let subst = build_subst(&struct_def.type_params, type_args);

    let param_types: Vec<Type> = method
        .params
        .iter()
        .map(|p| subst_type(&p.ty, &subst, &HashMap::new()))
        .collect();
    let ret_type = subst_type(&method.ret, &subst, &HashMap::new());
    let required = required_param_count(&method.param_defaults, param_types.len());

    let cast_accepts: Vec<bool> = method.params.iter().map(|p| p.cast_accept).collect();
    let result = if cast_accepts.iter().any(|f| *f) {
        check_call_signature_with_cast(
            call.span,
            call.node.func.node.id,
            &param_types,
            &cast_accepts,
            required,
            &ret_type,
            &node.args,
            type_checker,
            errors,
        )
    } else {
        check_call_signature(
            call.span,
            &param_types,
            required,
            &ret_type,
            &node.args,
            type_checker,
            errors,
        )
    };

    check_var_param_args(
        method.params.iter().map(|p| (p.name, p.mutability)),
        &node.args,
        type_checker,
        errors,
    );

    if !struct_def.type_params.is_empty() {
        let method = method.clone();
        instantiate_method_body(
            &MethodSpec {
                struct_name,
                method_name,
                struct_type_args: type_args,
                method_type_args: &[],
                struct_def,
                method: &method,
            },
            call.span,
            type_checker,
            errors,
        );
    }

    result
}

pub(super) fn check_extern_instance_method_call(
    call: &CallNode,
    type_name: Ident,
    method_name: Ident,
    method: &ExternMethodDef,
    target: Option<&ExprNode>,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) -> Type {
    if let Some(target) = target
        && matches!(method.receiver, Some(MethodReceiver::Var))
    {
        check_receiver_mutability(target, type_name, method_name, type_checker, errors);
        check_var_self_aliasing(
            target,
            method.params.iter().map(|p| (p.name, p.mutability)),
            &call.node.args,
            errors,
        );
    }

    let param_types: Vec<Type> = method.params.iter().map(|p| p.ty.clone()).collect();

    let result = check_call_signature(
        call.span,
        &param_types,
        param_types.len(),
        &method.ret,
        &call.node.args,
        type_checker,
        errors,
    );

    check_var_param_args(
        method.params.iter().map(|p| (p.name, p.mutability)),
        &call.node.args,
        type_checker,
        errors,
    );

    result
}

pub(super) fn check_extern_static_method_call(
    call: &CallNode,
    type_name: Ident,
    method_name: Ident,
    extern_def: &ExternTypeDef,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) -> Type {
    let Some(method) = extern_def.statics.get(&method_name) else {
        if extern_def.methods.contains_key(&method_name) {
            errors.push(Diagnostic::new(
                call.span,
                DiagnosticKind::InstanceMethodOnType {
                    struct_name: type_name,
                    method: method_name,
                },
            ));
        } else {
            errors.push(Diagnostic::new(
                call.span,
                DiagnosticKind::ExternUnknownMethod {
                    type_name,
                    method: method_name,
                },
            ));
        }
        return Type::Infer;
    };

    let param_types: Vec<Type> = method.params.iter().map(|p| p.ty.clone()).collect();

    let result = check_call_signature(
        call.span,
        &param_types,
        param_types.len(),
        &method.ret,
        &call.node.args,
        type_checker,
        errors,
    );

    check_var_param_args(
        method.params.iter().map(|p| (p.name, p.mutability)),
        &call.node.args,
        type_checker,
        errors,
    );

    result
}

pub(super) fn generic_context_strings(
    name: &str,
    type_params: &[TypeParam],
    type_args: &[Type],
) -> (String, String) {
    let args_str = type_args
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(", ");
    let label = format!("in this instantiation of '{name}<{args_str}>'");
    let mapping = type_params
        .iter()
        .zip(type_args.iter())
        .map(|(p, a)| format!("{} = {a}", p.name))
        .collect::<Vec<_>>()
        .join(", ");
    let note = format!("where {mapping}");
    (label, note)
}

fn find_type_param_name(ty: &Type, type_params: &[TypeParam], type_args: &[Type]) -> Option<Ident> {
    type_args
        .iter()
        .position(|arg| arg == ty)
        .and_then(|i| type_params.get(i))
        .map(|tp| tp.name)
}

fn generic_instantiation_help(
    err_kind: &DiagnosticKind,
    context_name: &str,
    type_params: &[TypeParam],
    type_args: &[Type],
) -> Option<String> {
    match err_kind {
        DiagnosticKind::InvalidOperand { op, operand_type } => {
            let param = find_type_param_name(operand_type, type_params, type_args);
            let op_str = op.as_str();
            match op_str {
                "+" => {
                    let valid = "int, float, double, or string";
                    Some(match param {
                        Some(p) => {
                            format!("'{context_name}<{p}>' uses '+' on {p} — {p} must be {valid}")
                        }
                        None => format!("'{context_name}' uses '+' which requires {valid}"),
                    })
                }
                "-" | "*" | "/" | "%" => {
                    let valid = "int, float, or double";
                    Some(match param {
                        Some(p) => format!(
                            "'{context_name}<{p}>' uses '{op_str}' on {p} — {p} must be {valid}"
                        ),
                        None => format!("'{context_name}' uses '{op_str}' which requires {valid}"),
                    })
                }
                "<" | ">" | "<=" | ">=" => {
                    let valid = "int, float, double, or string";
                    Some(match param {
                        Some(p) => format!(
                            "'{context_name}<{p}>' uses '{op_str}' on {p} — {p} must be {valid}"
                        ),
                        None => format!("'{context_name}' uses '{op_str}' which requires {valid}"),
                    })
                }
                "&&" | "||" | "^" => {
                    let valid = "bool";
                    Some(match param {
                        Some(p) => format!(
                            "'{context_name}<{p}>' uses '{op_str}' on {p} — {p} must be {valid}"
                        ),
                        None => format!("'{context_name}' uses '{op_str}' which requires {valid}"),
                    })
                }
                _ => None,
            }
        }
        DiagnosticKind::IndexOnNonArray { found } => {
            let param = find_type_param_name(found, type_params, type_args);
            Some(match param {
                Some(p) => format!(
                    "'{context_name}<{p}>' indexes {p} — {p} must be an array, list, or slice"
                ),
                None => {
                    format!(
                        "'{context_name}' uses indexing which requires an array, list, or slice"
                    )
                }
            })
        }
        DiagnosticKind::UnknownMethod {
            kind: _,
            struct_name,
            method,
        } => {
            let param = type_args
                .iter()
                .position(|arg| arg.to_string() == struct_name.to_string())
                .and_then(|i| type_params.get(i))
                .map(|tp| tp.name);
            Some(match param {
                Some(p) => format!(
                    "'{context_name}<{p}>' calls '.{method}()' on {p} — {p} must be a type with a '{method}' method"
                ),
                None => format!(
                    "'{context_name}' calls '.{method}()' which requires a type with a '{method}' method"
                ),
            })
        }
        DiagnosticKind::NotEquatable { ty } => {
            let param = find_type_param_name(ty, type_params, type_args);
            Some(match param {
                Some(p) => format!(
                    "'{context_name}<{p}>' compares {p} with '==' — {p} must be an equatable type"
                ),
                None => format!("'{context_name}' uses '==' which requires an equatable type"),
            })
        }
        _ => None,
    }
}

pub(super) fn report_cached_spec_error(
    cached: &SpecializationResult,
    context_name: &str,
    type_params: &[TypeParam],
    type_args: &[Type],
    call_span: Span,
    errors: &mut Vec<Diagnostic>,
) {
    if let Some((body_span, err_kind)) = &cached.err {
        let (_, note) = generic_context_strings(context_name, type_params, type_args);
        let mut err = Diagnostic::new(call_span, err_kind.clone())
            .with_secondary(*body_span, "required by this expression".to_string())
            .with_note(note);
        if let Some(help) =
            generic_instantiation_help(err_kind, context_name, type_params, type_args)
        {
            err.help = Some(help);
        }
        errors.push(err);
    }
}

pub(super) fn report_instantiation_errors(
    body_errors: Vec<Diagnostic>,
    context_name: &str,
    type_params: &[TypeParam],
    type_args: &[Type],
    call_span: Span,
    errors: &mut Vec<Diagnostic>,
) {
    let (_, note) = generic_context_strings(context_name, type_params, type_args);
    for err in body_errors {
        let body_span = err.span;
        let mut new_err = Diagnostic::new(call_span, err.kind.clone())
            .with_secondary(body_span, "required by this expression".to_string())
            .with_note(note.clone());
        new_err.secondary.extend(err.secondary.iter().cloned());
        new_err.notes.extend(err.notes.iter().cloned());
        let help = generic_instantiation_help(&err.kind, context_name, type_params, type_args);
        new_err.help = help.or_else(|| err.help.clone());
        errors.push(new_err);
    }
}

pub(super) fn check_instantiated_body(
    ictx: &InstantiationContext<'_>,
    body: &BlockNode,
    call_span: Span,
    type_checker: &mut TypeChecker,
    body_errors: &mut Vec<Diagnostic>,
) -> TypedBodyResult {
    // swap the types map so this body records expression types in its own slot,
    // nested specializations make the same swap safely
    let prev_types = std::mem::take(&mut type_checker.types);
    let prev_binding_types = std::mem::take(&mut type_checker.binding_types);

    let prev_ctx = ictx.module_env.map(|env| {
        let saved = type_checker.ctx.clone();
        type_checker.ctx = env.clone();
        saved
    });

    // bring the module's function bindings into scope so the specialized body can call
    // both public and private siblings
    if let Some(env) = ictx.module_env {
        type_checker.push_scope();
        for (name, info) in &env.func_bindings {
            if info.is_function {
                type_checker.set_func_var(*name, info.ty.clone());
            } else {
                type_checker.set_var(*name, info.ty.clone(), info.mutable);
            }
        }
    }

    if let Some(method_ctx) = &ictx.method_ctx {
        type_checker.push_method_context(method_ctx.clone());
    }

    let type_subst_count = ictx.type_subst.len();
    type_checker
        .current_type_subst
        .extend_from_slice(&ictx.type_subst);

    check_body_common(
        &ictx.params,
        body,
        &ictx.ret_ty,
        call_span,
        type_checker,
        body_errors,
    );

    type_checker.pop_type_subst(type_subst_count);

    if ictx.method_ctx.is_some() {
        type_checker.pop_method_context();
    }

    if ictx.module_env.is_some() {
        type_checker.pop_scope();
    }

    if let Some(saved) = prev_ctx {
        type_checker.ctx = saved;
    }

    let body_types = std::mem::replace(&mut type_checker.types, prev_types);
    let binding_types = std::mem::replace(&mut type_checker.binding_types, prev_binding_types);

    TypedBodyResult {
        ret_ty: ictx.ret_ty.clone(),
        body_types,
        binding_types,
        first_error: body_errors.first().map(|err| (err.span, err.kind.clone())),
    }
}

fn instantiate_and_check_fn(
    func_name: Ident,
    type_args: &[Type],
    const_args: &[usize],
    call_span: Span,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) -> Type {
    // look up type parameters early so they are available for cache-hit context
    let Some(type_params) = type_checker.func_type_params.get(&func_name).cloned() else {
        errors.push(Diagnostic::new(
            call_span,
            DiagnosticKind::UnknownFunction { name: func_name },
        ));
        return Type::Infer;
    };

    let cache_key = SpecializationKey {
        func_name,
        type_args: type_args.to_vec(),
        const_args: const_args.to_vec(),
    };

    // look up the generic function template
    let Some(fn_template) = type_checker.generic_func_templates.get(&func_name).cloned() else {
        errors.push(Diagnostic::new(
            call_span,
            DiagnosticKind::UnknownFunction { name: func_name },
        ));
        return Type::Infer;
    };

    // verify arity
    if type_params.len() != type_args.len() {
        errors.push(Diagnostic::new(
            call_span,
            DiagnosticKind::GenericArgNumMismatch {
                expected: type_params.len(),
                found: type_args.len(),
            },
        ));
        return Type::Infer;
    }

    // build substitution maps
    let subst = build_subst(&type_params, type_args);
    let fn_const_params = type_checker
        .func_const_params
        .get(&func_name)
        .cloned()
        .unwrap_or_default();
    let const_subst = build_const_subst(&fn_const_params, const_args);

    let func = &fn_template.node;

    type_checker.push_const_params(&fn_const_params);
    let mut params: Vec<(Ident, Type, bool)> = func
        .params
        .iter()
        .map(|p| {
            (
                p.name,
                subst_type(&type_checker.resolve_type(&p.ty), &subst, &const_subst),
                matches!(p.mutability, Mutability::Mutable),
            )
        })
        .collect();
    let ret_ty = subst_type(&type_checker.resolve_type(&func.ret), &subst, &const_subst);
    type_checker.pop_const_params(fn_const_params.len());

    match type_checker.specialization_cache.get(&cache_key) {
        Some(SpecializationState::Done(cached)) => {
            report_cached_spec_error(
                cached,
                &func_name.to_string(),
                &type_params,
                type_args,
                call_span,
                errors,
            );
            return cached.ret_ty.clone();
        }
        Some(SpecializationState::InProgress) => {
            return ret_ty;
        }
        None => {}
    }

    // inject const params as immutable variables so the body can reference them by name
    for param in &fn_const_params {
        params.push((param.name, Type::Int, false));
    }

    let owned_module_env = type_checker
        .generic_func_source_module
        .get(&func_name)
        .and_then(|path| type_checker.module_check_contexts.get(path).cloned());

    let type_subst: Vec<(Ident, Type)> = type_params
        .iter()
        .zip(type_args.iter())
        .map(|(p, ty)| (p.name, ty.clone()))
        .collect();

    let ictx = InstantiationContext {
        module_env: owned_module_env.as_ref(),
        params,
        ret_ty,
        method_ctx: None,
        type_subst,
    };
    type_checker
        .specialization_cache
        .insert(cache_key.clone(), SpecializationState::InProgress);
    let mut body_errors = vec![];
    let result =
        check_instantiated_body(&ictx, &func.body, call_span, type_checker, &mut body_errors)
            .into_spec_result();

    report_instantiation_errors(
        body_errors,
        &func_name.to_string(),
        &type_params,
        type_args,
        call_span,
        errors,
    );

    let ret = result.ret_ty.clone();
    type_checker
        .specialization_cache
        .insert(cache_key, SpecializationState::Done(result));
    ret
}

struct MethodSpec<'a> {
    struct_name: Ident,
    method_name: Ident,
    struct_type_args: &'a [Type],
    method_type_args: &'a [Type],
    struct_def: &'a StructDef,
    method: &'a MethodDef,
}

fn instantiate_method_body(
    target: &MethodSpec<'_>,
    call_span: Span,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) -> Type {
    let MethodSpec {
        struct_name,
        method_name,
        struct_type_args,
        method_type_args,
        struct_def,
        method,
    } = target;

    let all_type_args: Vec<Type> = struct_type_args
        .iter()
        .chain(method_type_args.iter())
        .cloned()
        .collect();

    // collect all type params (struct params first, then method params) mirroring all_type_args
    let all_type_params: Vec<TypeParam> = struct_def
        .type_params
        .iter()
        .chain(method.type_params.iter())
        .cloned()
        .collect();

    let cache_key = MethodSpecKey {
        struct_name: *struct_name,
        method_name: *method_name,
        type_args: all_type_args.clone(),
        const_args: vec![],
    };

    if let Some(cached) = type_checker.method_spec_cache.get(&cache_key) {
        let context_name = format!("{struct_name}.{method_name}");
        report_cached_spec_error(
            cached,
            &context_name,
            &all_type_params,
            &all_type_args,
            call_span,
            errors,
        );
        return cached.ret_ty.clone();
    }

    let mut subst = build_subst(&struct_def.type_params, struct_type_args);
    subst.extend(build_subst(&method.type_params, method_type_args));

    let specialized_param_types: Vec<Type> = method
        .params
        .iter()
        .map(|p| subst_type(&p.ty, &subst, &HashMap::new()))
        .collect();
    let specialized_ret = subst_type(&method.ret, &subst, &HashMap::new());

    let self_type = struct_def.make_type(*struct_name, struct_type_args.to_vec());
    let self_ident = Ident(Intern::new("self".to_string()));

    let mut params: Vec<(Ident, Type, bool)> = vec![];
    if let Some(receiver) = method.receiver {
        let self_mutable = matches!(receiver, MethodReceiver::Var);
        params.push((self_ident, self_type, self_mutable));
    }
    for (param, specialized_ty) in method.params.iter().zip(specialized_param_types.iter()) {
        params.push((
            param.name,
            specialized_ty.clone(),
            matches!(param.mutability, Mutability::Mutable),
        ));
    }

    let type_subst: Vec<(Ident, Type)> = all_type_params
        .iter()
        .zip(all_type_args.iter())
        .map(|(p, ty)| (p.name, ty.clone()))
        .collect();

    let ictx = InstantiationContext {
        module_env: None,
        params,
        ret_ty: specialized_ret,
        method_ctx: Some(MethodContext {
            struct_name: *struct_name,
            receiver: method.receiver,
        }),
        type_subst,
    };
    let mut body_errors = vec![];
    let result = check_instantiated_body(
        &ictx,
        &method.body,
        call_span,
        type_checker,
        &mut body_errors,
    )
    .into_spec_result();

    report_instantiation_errors(
        body_errors,
        &format!("{struct_name}.{method_name}"),
        &all_type_params,
        &all_type_args,
        call_span,
        errors,
    );

    let ret = result.ret_ty.clone();
    type_checker.method_spec_cache.insert(cache_key, result);
    ret
}

pub(super) fn type_call_on_base(
    base_ty: &Type,
    call_node: &CallNode,
    required_count: usize,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) -> Type {
    // check each argument and pass expected type for lambdas so they can infer param types
    if let Type::Func { params, .. } = base_ty {
        for (arg, param_fp) in call_node.node.args.iter().zip(params.iter()) {
            let needs_expected = matches!(
                arg.node.kind,
                ExprKind::Lambda(_) | ExprKind::Lit(Lit::Float { .. }) | ExprKind::InferredEnum(_)
            );
            let expected = if needs_expected {
                Some(&param_fp.ty)
            } else {
                None
            };
            check_expr(arg, type_checker, errors, expected);
        }
        for arg in call_node.node.args.iter().skip(params.len()) {
            check_expr(arg, type_checker, errors, None);
        }
    } else {
        for arg in &call_node.node.args {
            check_expr(arg, type_checker, errors, None);
        }
    }

    let Type::Func { params, ret } = base_ty else {
        if matches!(base_ty, Type::Infer) {
            return Type::Infer;
        }
        errors.push(Diagnostic::new(
            call_node.span,
            DiagnosticKind::NotAFunction {
                expr_type: base_ty.clone(),
            },
        ));
        return Type::Infer;
    };

    let args_len = call_node.node.args.len();
    if args_len < required_count || args_len > params.len() {
        errors.push(Diagnostic::new(
            call_node.span,
            DiagnosticKind::MismatchedTypes {
                expected: base_ty.clone(),
                found: Type::Func {
                    params: vec![FuncParam::immut(Type::Infer); args_len],
                    ret: Box::new(Type::Infer),
                },
            },
        ));
        return Type::Infer;
    }

    for (arg_expr, param_fp) in call_node.node.args.iter().zip(params.iter()) {
        let arg_ref = TypeRef::Expr(arg_expr.node.id);
        let param_ref = TypeRef::concrete(&param_fp.ty);
        type_checker.constrain_assignable(arg_expr.span, arg_ref, param_ref, errors);
    }

    (**ret).clone()
}
