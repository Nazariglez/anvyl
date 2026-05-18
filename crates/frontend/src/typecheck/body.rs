use std::{collections::HashMap, rc::Rc};

use super::{
    CheckedType, LocalBindingKind, LocalCallableInfo, LocalConstInfo, LocalSymbol, ReturnFrame,
    ReturnMode, ScopeState, TypeChecker, TypeError, VarInfo, annotation, check_expr_checked,
    check_expr_checked_with_hint, checked_void, const_eval, control_flow,
    decl_validate::{
        check_method_generic_shadows, check_param_order, has_generics, has_mutable_param,
        is_generic, method_sig_is_generic, validate_return_spec, validate_type_alias_def,
    },
    decls::{
        AggregateSchema, CallableDef, CallableId, CallableKind, CallableRef, CallableSig,
        DeclError, ExtendId, FieldSchema, MethodKey, MethodMode, ModuleScope, NominalKey,
        TypeAliasDef, generic_params, nominal_type, required_param_count, stmt_visibility,
    },
    dyn_infer::DynInference,
    generic::{
        ConstSubst, GenericArgs, GenericOwnerFrame, GenericParams, SpecializationState, TypeSubst,
        check_with_specialization, combined_callable_params, specialization_key,
    },
    infer::TypeHandle,
    pattern::{self, check_binding},
    place::{PlaceIdentity, PlaceRoot},
    type_ops::type_depends_on_generics,
    type_refs::{GenericTypeContext, LocalTypeAlias},
    validate_const_expr_type,
};
use crate::{
    ast::{
        AggregateDeclNode, BlockNode, ConstValue, ExprId, ExprKind, ExprNode, ExtendDeclNode, Func,
        FuncNode, FuncParam, Ident, Lit, MethodReceiver, Mutability, Param, Pattern, PatternHead,
        PatternNode, Program, ReturnAccess, ReturnSpec, Stmt, StmtNode, StructDecl, StructField,
        Type, TypeAliasDeclNode, Visibility,
    },
    span::Span,
};

#[derive(Clone)]
pub(super) enum CallableTemplateEnv {
    SourceModule,
    Local(ScopeState),
}

struct SourceFuncSig {
    owner_generics: GenericParams,
    owner_args: GenericArgs,
    generics: GenericParams,
    generic_context: GenericTypeContext,
    params: Vec<FuncParam>,
    required_params: usize,
    ret: ReturnSpec,
    surface_ty: Type,
}

struct LocalFuncDecl<'a> {
    id: CallableId,
    sig: SourceFuncSig,
    func: &'a FuncNode,
}

#[derive(Clone)]
pub(super) struct CallableTemplate {
    span: Span,
    mode: MethodMode,
    generics: GenericTypeContext,
    env: CallableTemplateEnv,
    params: Vec<Param>,
    ret: ReturnSpec,
    ret_span: Span,
    body: BlockNode,
}

#[derive(Clone, Copy)]
pub(super) enum CallableBody<'a> {
    Block(&'a BlockNode),
    Expr(&'a ExprNode),
}

impl CallableBody<'_> {
    fn span(&self) -> Span {
        match self {
            Self::Block(block) => block.span,
            Self::Expr(expr) => expr.span,
        }
    }

    fn diverges(&self) -> bool {
        match self {
            Self::Block(block) => control_flow::block_diverges(block),
            Self::Expr(expr) => control_flow::expr_diverges(expr),
        }
    }

    fn value_expr_id(&self) -> Option<ExprId> {
        self.value_expr().map(|expr| expr.node.id)
    }

    fn value_expr(&self) -> Option<&ExprNode> {
        match self {
            Self::Block(block) => block.node.tail.as_deref(),
            Self::Expr(expr) => Some(expr),
        }
    }

    fn check_with_hint(&self, expected: Option<TypeHandle>, tc: &mut TypeChecker) -> CheckedType {
        match self {
            Self::Block(block) => check_block_checked_with_hint(block, expected, tc),
            Self::Expr(expr) => check_expr_checked_with_hint(expr, expected, tc),
        }
    }
}

pub(super) fn collect_callable_templates(
    module: &ModuleScope,
    program: &Program,
    tc: &mut TypeChecker,
) {
    let mut extend_index = 0;

    for stmt in &program.stmts {
        match &stmt.node {
            Stmt::Func(func_node) => {
                let func = &func_node.node;
                if !is_generic(func) {
                    continue;
                }
                let generics =
                    tc.generic_context(&func.type_params, &func.const_params, func_node.span);
                tc.store_callable_template(
                    CallableId::function(module.clone(), func.name),
                    CallableTemplate {
                        span: func_node.span,
                        mode: MethodMode::Static,
                        generics,
                        env: CallableTemplateEnv::SourceModule,
                        params: func.params.clone(),
                        ret: func.ret.clone(),
                        ret_span: func_node.span,
                        body: func.body.clone(),
                    },
                );
            }
            Stmt::Aggregate(agg_node) => {
                let agg = &agg_node.node;
                let owner = NominalKey {
                    module: module.clone(),
                    kind: agg.kind.into(),
                    name: agg.name,
                };
                let owner_is_generic = has_generics(&agg.type_params, &agg.const_params);
                let has_generic_method = agg
                    .methods
                    .iter()
                    .any(|method| method_sig_is_generic(&method.sig));
                if !owner_is_generic && !has_generic_method {
                    continue;
                }
                let owner_generics =
                    tc.generic_context(&agg.type_params, &agg.const_params, agg_node.span);
                for method in &agg.methods {
                    let method_is_generic = method_sig_is_generic(&method.sig);
                    if !owner_is_generic && !method_is_generic {
                        continue;
                    }
                    let generics = tc.extended_generic_context(
                        &owner_generics,
                        &method.sig.type_params,
                        &method.sig.const_params,
                        agg_node.span,
                    );
                    let mode = MethodMode::from_receiver(method.sig.receiver);
                    tc.store_callable_template(
                        CallableId::aggregate_method(
                            owner.clone(),
                            method.sig.name,
                            mode.surface(),
                        ),
                        CallableTemplate {
                            span: agg_node.span,
                            mode,
                            generics,
                            env: CallableTemplateEnv::SourceModule,
                            params: method.sig.params.clone(),
                            ret: method.sig.ret.clone(),
                            ret_span: agg_node.span,
                            body: method.body.clone(),
                        },
                    );
                }
            }
            Stmt::Extend(extend_node) => {
                let extend = &extend_node.node;
                let extend_id = ExtendId {
                    module: module.clone(),
                    index: extend_index,
                };
                extend_index += 1;
                let owner_is_generic = has_generics(&extend.type_params, &extend.const_params);
                let has_generic_method = extend
                    .methods
                    .iter()
                    .any(|method| method_sig_is_generic(&method.node.sig));
                if !owner_is_generic && !has_generic_method {
                    continue;
                }
                let owner_generics =
                    tc.generic_context(&extend.type_params, &extend.const_params, extend_node.span);
                for method_node in &extend.methods {
                    let method = &method_node.node;
                    let method_is_generic = method_sig_is_generic(&method.sig);
                    if !owner_is_generic && !method_is_generic {
                        continue;
                    }
                    let mode = MethodMode::from_receiver(method.sig.receiver);
                    let generics = tc.extended_generic_context(
                        &owner_generics,
                        &method.sig.type_params,
                        &method.sig.const_params,
                        method_node.span,
                    );
                    tc.store_callable_template(
                        CallableId::extend_method(
                            extend_id.clone(),
                            method.sig.name,
                            mode.surface(),
                        ),
                        CallableTemplate {
                            span: method_node.span,
                            mode,
                            generics,
                            env: CallableTemplateEnv::SourceModule,
                            params: method.sig.params.clone(),
                            ret: method.sig.ret.clone(),
                            ret_span: method_node.span,
                            body: method.body.clone(),
                        },
                    );
                }
            }
            _ => {}
        }
    }
}

pub(super) fn push_source_scope(tc: &mut TypeChecker) {
    tc.push_scope();
    register_builtins(tc);
}

fn register_builtins(tc: &mut TypeChecker) {
    let builtins = [
        ("println", vec![FuncParam::immut(Type::Any)], Type::Void),
        ("assert", vec![FuncParam::immut(Type::Bool)], Type::Void),
        (
            "assert_msg",
            vec![FuncParam::immut(Type::Bool), FuncParam::immut(Type::String)],
            Type::Void,
        ),
    ];

    for (name, params, ret) in builtins {
        tc.define(
            Ident::new(name),
            Type::Func {
                params,
                ret: Box::new(ReturnSpec::value(ret)),
            },
            false,
        );
    }
}

pub(super) fn register_declarations(program: &Program, tc: &mut TypeChecker) {
    let extern_functions = tc
        .externs
        .functions_in_scope(&tc.current_module)
        .map(|function| (function.key.name, function.signature.to_func_type()))
        .collect::<Vec<_>>();
    for (name, ty) in extern_functions {
        tc.define(name, ty, false);
    }

    let register_dyn_infer = tc.should_register_dyn_infer_params();

    for stmt in &program.stmts {
        match &stmt.node {
            Stmt::Func(func_node) => {
                let func = &func_node.node;
                if is_generic(func) {
                    continue;
                }
                let func_ty = if register_dyn_infer {
                    let func_ty = tc.callable_type_from_sig(
                        &func.params,
                        &func.ret,
                        func_node.span,
                        matches!(func.visibility, Visibility::Public),
                    );
                    tc.decls
                        .set_func_type(&tc.current_module, func.name, &func_ty);
                    func_ty
                } else {
                    tc.decls
                        .local_value(&tc.current_module, func.name)
                        .map_or_else(
                            || {
                                debug_assert!(
                                    false,
                                    "registered function missing declaration type"
                                );
                                Type::Infer
                            },
                            |value| value.decl.ty().clone(),
                        )
                };
                tc.define(func.name, func_ty, false);
            }
            Stmt::Aggregate(_) | Stmt::Enum(_) | Stmt::ExternFunc(_) | Stmt::ExternType(_) => {}
            Stmt::Const(const_node) => {
                let c = &const_node.node;
                let ty = match &c.ty {
                    Some(t) => tc.resolve_type_for_tc_at(t, const_node.span),
                    None => Type::Infer,
                };
                tc.define_value(c.name, ty, LocalBindingKind::constant(), None);
            }
            _ => {}
        }
    }

    if register_dyn_infer {
        register_callable_dyn_infer_params(program, tc);
    }
}

fn register_callable_dyn_infer_params(program: &Program, tc: &mut TypeChecker) {
    let module = tc.current_module.clone();
    let mut extend_index = 0;

    for stmt in &program.stmts {
        let exported = matches!(stmt_visibility(stmt), Visibility::Public);
        match &stmt.node {
            Stmt::Func(func_node)
                if is_generic(&func_node.node)
                    && callable_sig_has_raw_dyn_infer(
                        &func_node.node.params,
                        &func_node.node.ret,
                    ) =>
            {
                let sig = source_func_sig(&func_node.node, func_node.span, tc);
                tc.decls
                    .set_func_type(&module, func_node.node.name, &sig.surface_ty);
            }
            Stmt::Aggregate(agg_node) => {
                register_aggregate_method_dyn_infer_params(agg_node, &module, exported, tc);
            }
            Stmt::Extend(extend_node) => {
                let id = ExtendId {
                    module: module.clone(),
                    index: extend_index,
                };
                extend_index += 1;
                register_extend_method_dyn_infer_params(extend_node, &id, exported, tc);
            }
            _ => {}
        }
    }
}

fn register_aggregate_method_dyn_infer_params(
    agg_node: &AggregateDeclNode,
    module: &ModuleScope,
    exported: bool,
    tc: &mut TypeChecker,
) {
    let agg = &agg_node.node;
    let key = NominalKey {
        module: module.clone(),
        kind: agg.kind.into(),
        name: agg.name,
    };
    let owner_generics = tc.generic_context(&agg.type_params, &agg.const_params, agg_node.span);
    for method in &agg.methods {
        if !callable_sig_has_raw_dyn_infer(&method.sig.params, &method.sig.ret) {
            continue;
        }
        let mode = MethodMode::from_receiver(method.sig.receiver);
        let generics = tc.extended_generic_context(
            &owner_generics,
            &method.sig.type_params,
            &method.sig.const_params,
            agg_node.span,
        );
        let (params, ret) = resolve_callable_sig_types(
            &method.sig.params,
            &method.sig.ret,
            generics,
            agg_node.span,
            exported,
            tc,
        );
        let Some(schema) = tc.decls.aggregate_mut(&key) else {
            continue;
        };
        let Some(method_schema) = schema
            .methods
            .get_mut(&MethodKey::new(method.sig.name, mode.surface()))
        else {
            continue;
        };
        method_schema.params = params;
        method_schema.ret = ret;
    }
}

fn register_extend_method_dyn_infer_params(
    extend_node: &ExtendDeclNode,
    id: &ExtendId,
    exported: bool,
    tc: &mut TypeChecker,
) {
    let extend = &extend_node.node;
    let owner_generics =
        tc.generic_context(&extend.type_params, &extend.const_params, extend_node.span);
    for method_node in &extend.methods {
        let method = &method_node.node;
        if !callable_sig_has_raw_dyn_infer(&method.sig.params, &method.sig.ret) {
            continue;
        }
        let mode = MethodMode::from_receiver(method.sig.receiver);
        let generics = tc.extended_generic_context(
            &owner_generics,
            &method.sig.type_params,
            &method.sig.const_params,
            method_node.span,
        );
        let (params, ret) = resolve_callable_sig_types(
            &method.sig.params,
            &method.sig.ret,
            generics,
            method_node.span,
            exported,
            tc,
        );
        let Some(extend) = tc.decls.extend_mut(id) else {
            continue;
        };
        let Some(method_schema) = extend
            .methods
            .get_mut(&MethodKey::new(method.sig.name, mode.surface()))
        else {
            continue;
        };
        method_schema.params = params;
        method_schema.ret = ret;
    }
}

fn callable_sig_has_raw_dyn_infer(params: &[Param], ret: &ReturnSpec) -> bool {
    params
        .iter()
        .any(|param| DynInference::has_raw_hole(&param.ty))
        || DynInference::has_raw_hole(&ret.ty)
}

fn resolve_callable_sig_types(
    params: &[Param],
    ret: &ReturnSpec,
    generics: GenericTypeContext,
    span: Span,
    exported: bool,
    tc: &mut TypeChecker,
) -> (Vec<FuncParam>, ReturnSpec) {
    tc.push_generic_context(generics);
    let params = tc.resolve_callable_params(params, exported);
    let ret = ret.with_ty(tc.resolve_type_for_tc_at(&ret.ty, span));
    tc.pop_generic_context();
    (params, ret)
}

pub(super) fn check_stmts(stmts: &[StmtNode], tc: &mut TypeChecker) {
    for stmt in stmts {
        check_stmt(stmt, None, tc);
    }
}

fn source_func_sig(func: &Func, span: Span, tc: &mut TypeChecker) -> SourceFuncSig {
    let owner = tc.visible_generic_owner();
    let mut generics = generic_params(&func.type_params, &func.const_params);
    let generic_context =
        tc.extended_generic_context(&owner.generics, &func.type_params, &func.const_params, span);
    check_param_order(&func.params, span, tc);
    validate_return_spec(
        &func.ret,
        !generics.is_empty() || !owner.params.is_empty(),
        has_mutable_param(&func.params),
        span,
        tc,
    );

    tc.push_generic_context(generic_context.clone());
    tc.resolve_generic_bounds_for_tc(&mut generics, span);
    let exported = matches!(func.visibility, Visibility::Public);
    let params = tc.resolve_callable_params(&func.params, exported);
    let ret = func
        .ret
        .with_ty(tc.resolve_type_for_tc_at(&func.ret.ty, span));
    tc.pop_generic_context();

    SourceFuncSig {
        owner_generics: owner.params,
        owner_args: owner.args,
        generics,
        generic_context,
        required_params: required_param_count(&func.params),
        surface_ty: Type::Func {
            params: params.clone(),
            ret: Box::new(ret.clone()),
        },
        params,
        ret,
    }
}

fn register_local_type_aliases(stmts: &[StmtNode], tc: &mut TypeChecker) {
    for stmt in stmts {
        let Stmt::TypeAlias(alias_node) = &stmt.node else {
            continue;
        };
        let alias = &alias_node.node;
        let owner = tc.generic_contexts.last().cloned().unwrap_or_default();
        let generic_context = tc.extended_generic_context(
            &owner,
            &alias.type_params,
            &alias.const_params,
            alias_node.span,
        );
        let mut errors = vec![];
        let policy = annotation::normalize_annotations(
            tc.source_id(),
            &alias.annotations,
            annotation::AnnotationTarget::TypeAlias,
            &mut errors,
        );
        tc.errors.extend(errors.into_iter().map(TypeError::Decl));
        let local = LocalTypeAlias {
            key: alias_node.span,
            def: TypeAliasDef {
                module: tc.current_module.clone(),
                name: alias.name,
                generics: generic_params(&alias.type_params, &alias.const_params),
                generic_context,
                aliased: alias.aliased.clone(),
                policy,
                span: tc.source_span(alias_node.span),
            },
            visible_depth: tc.local_type_scopes.depth(),
        };
        if !tc.local_type_scopes.insert(local) {
            tc.push_error(TypeError::Decl(DeclError::DuplicateType {
                module: tc.current_module.clone(),
                name: alias.name,
                span: tc.error_span(alias_node.span),
            }));
        }
    }
}

pub(super) fn register_block_declarations(
    stmts: &[StmtNode],
    tc: &mut TypeChecker,
) -> Vec<Option<LocalConstInfo>> {
    let mut declarations = vec![None; stmts.len()];
    let mut funcs = vec![];
    register_local_type_aliases(stmts, tc);
    let mut sig_env = tc.scopes.clone();
    add_callable_decl_placeholders(stmts, &mut sig_env, tc);
    for (index, stmt) in stmts.iter().enumerate() {
        match &stmt.node {
            Stmt::Func(func_node) => {
                let func = &func_node.node;
                let module = tc.current_module.clone();
                let env = CallableTemplateEnv::Local(tc.scope_state_from(sig_env.clone()));
                let sig = with_callable_body_env(&module, &env, tc, |tc| {
                    source_func_sig(func, func_node.span, tc)
                });
                let id = CallableId::local_function(
                    tc.current_module.clone(),
                    func.name,
                    func_node.span,
                );
                funcs.push(LocalFuncDecl {
                    id,
                    sig,
                    func: func_node,
                });
            }
            Stmt::Const(const_node) => {
                let info = tc.declare_local_const(
                    const_node,
                    CallableTemplateEnv::Local(tc.scope_state_from(sig_env.clone())),
                );
                add_env_symbol(const_node.node.name, info.symbol(), &mut sig_env);
                declarations[index] = Some(info);
            }
            _ => add_stmt_capture_blockers(&stmt.node, &mut sig_env, tc),
        }
    }

    for decl in &funcs {
        let func = &decl.func.node;
        let callee = CallableRef {
            def: CallableDef {
                id: decl.id.clone(),
                sig: CallableSig {
                    owner_generics: decl.sig.owner_generics.clone(),
                    generics: decl.sig.generics.clone(),
                    params: decl.sig.params.clone(),
                    required_params: decl.sig.required_params,
                    ret: decl.sig.ret.clone(),
                },
            },
            receiver_ty: None,
            owner_args: decl.sig.owner_args.clone(),
        };
        tc.define_local_callable(func.name, callee, decl.sig.surface_ty.clone());
    }

    let mut funcs = funcs.into_iter();
    let mut env = tc.scopes.clone();
    for (stmt, local_const) in stmts.iter().zip(declarations.iter().copied()) {
        match &stmt.node {
            Stmt::Func(_) => {
                let decl = funcs.next().expect("function declaration was collected");
                store_local_callable_template(decl, env.clone(), tc);
            }
            Stmt::Const(const_node) => {
                let Some(info) = local_const else {
                    continue;
                };
                tc.set_local_const_env(
                    info.id,
                    CallableTemplateEnv::Local(tc.scope_state_from(env.clone())),
                );
                add_env_symbol(const_node.node.name, info.symbol(), &mut env);
            }
            _ => add_stmt_capture_blockers(&stmt.node, &mut env, tc),
        }
    }

    declarations
}

fn add_stmt_capture_blockers(
    stmt: &Stmt,
    env: &mut [HashMap<Ident, LocalSymbol>],
    tc: &mut TypeChecker,
) {
    match stmt {
        Stmt::Binding(binding) => add_pattern_capture_blockers(
            &binding.node.pattern,
            LocalBindingKind::from_mutable(matches!(binding.node.mutability, Mutability::Mutable)),
            env,
            tc,
        ),
        Stmt::LetElse(let_else) => add_pattern_capture_blockers(
            &let_else.node.pattern,
            LocalBindingKind::from_mutable(matches!(let_else.node.head, PatternHead::Var)),
            env,
            tc,
        ),
        _ => {}
    }
}

fn add_callable_decl_placeholders(
    stmts: &[StmtNode],
    env: &mut [HashMap<Ident, LocalSymbol>],
    tc: &mut TypeChecker,
) {
    for stmt in stmts {
        let Stmt::Func(func_node) = &stmt.node else {
            continue;
        };
        let func = &func_node.node;
        let id = CallableId::local_function(tc.current_module.clone(), func.name, func_node.span);
        let callee = CallableRef {
            def: CallableDef {
                id,
                sig: CallableSig {
                    owner_generics: GenericParams::default(),
                    generics: GenericParams::default(),
                    params: vec![],
                    required_params: 0,
                    ret: ReturnSpec::value(Type::Infer),
                },
            },
            receiver_ty: None,
            owner_args: GenericArgs::default(),
        };
        let binding_id = tc.fresh_binding_id();
        let type_id = tc.solver.alloc_local_type(&Type::Infer);
        add_env_symbol(
            func.name,
            LocalSymbol::Callable(Box::new(LocalCallableInfo {
                binding_id,
                type_id,
                callee,
            })),
            env,
        );
    }
}

fn store_local_callable_template(
    decl: LocalFuncDecl<'_>,
    env: Vec<HashMap<Ident, LocalSymbol>>,
    tc: &mut TypeChecker,
) {
    let has_template = is_generic(&decl.func.node)
        || decl.func.node.ret.is_infer()
        || !decl.sig.owner_generics.is_empty();
    if !has_template {
        return;
    }
    tc.store_callable_template(
        decl.id,
        CallableTemplate {
            span: decl.func.span,
            mode: MethodMode::Static,
            generics: decl.sig.generic_context,
            env: CallableTemplateEnv::Local(tc.scope_state_from(env)),
            params: decl.func.node.params.clone(),
            ret: decl.func.node.ret.clone(),
            ret_span: decl.func.span,
            body: decl.func.node.body.clone(),
        },
    );
}

fn add_capture_blocker(
    name: Ident,
    kind: LocalBindingKind,
    env: &mut [HashMap<Ident, LocalSymbol>],
    tc: &mut TypeChecker,
) {
    debug_assert!(kind.requires_runtime_capture());
    let binding_id = tc.fresh_binding_id();
    let type_id = tc.solver.alloc_local_type(&Type::Infer);
    add_env_symbol(
        name,
        LocalSymbol::Value(VarInfo {
            binding_id,
            type_id,
            kind,
            const_value: None,
            local_const: None,
            alias: None,
        }),
        env,
    );
}

fn add_env_symbol(name: Ident, symbol: LocalSymbol, env: &mut [HashMap<Ident, LocalSymbol>]) {
    if let Some(scope) = env.last_mut() {
        scope.insert(name, symbol);
    }
}

fn add_pattern_capture_blockers(
    pattern: &PatternNode,
    kind: LocalBindingKind,
    env: &mut [HashMap<Ident, LocalSymbol>],
    tc: &mut TypeChecker,
) {
    match &pattern.node {
        Pattern::Ident(name) => add_capture_blocker(*name, kind, env, tc),
        Pattern::Tuple(fields)
        | Pattern::EnumTuple { fields, .. }
        | Pattern::InferredEnumTuple { fields, .. }
        | Pattern::Or(fields) => {
            for field in fields {
                add_pattern_capture_blockers(field, kind, env, tc);
            }
        }
        Pattern::Struct { fields, .. }
        | Pattern::EnumStruct { fields, .. }
        | Pattern::InferredEnumStruct { fields, .. } => {
            for (_, field) in fields {
                add_pattern_capture_blockers(field, kind, env, tc);
            }
        }
        Pattern::Optional(inner) => add_pattern_capture_blockers(inner, kind, env, tc),
        Pattern::Wildcard
        | Pattern::EnumUnit { .. }
        | Pattern::InferredEnumUnit { .. }
        | Pattern::Range { .. }
        | Pattern::Lit(_)
        | Pattern::Rest
        | Pattern::Nil => {}
    }
}

fn check_aggregate_decl(agg_node: &AggregateDeclNode, tc: &mut TypeChecker) {
    let agg = &agg_node.node;
    let key = NominalKey {
        module: tc.current_module.clone(),
        kind: agg.kind.into(),
        name: agg.name,
    };
    let Some(schema) = tc.decls.aggregate(&key).cloned() else {
        return;
    };

    check_aggregate_field_defaults(&agg.fields, &schema.fields, tc);
    check_method_generic_shadows(agg, agg_node.span, tc);
    check_aggregate_method_bodies(agg, agg_node.span, &key, &schema, tc);
}

fn check_aggregate_field_defaults(
    fields: &[StructField],
    schema: &HashMap<Ident, FieldSchema>,
    tc: &mut TypeChecker,
) {
    for field in fields {
        let Some(default) = &field.default else {
            continue;
        };
        let Some(schema) = schema.get(&field.name) else {
            continue;
        };
        if type_depends_on_generics(&schema.ty) {
            tc.push_error(TypeError::GenericFieldDefault {
                span: tc.error_span(default.span),
            });
            continue;
        }
        let expected = tc.type_handle(&schema.ty);
        if let Err(error) = validate_const_expr_type(default, Some(expected), tc) {
            tc.push_error(error);
            continue;
        }
        if matches!(default.node.kind, ExprKind::Lit(Lit::Nil))
            && tc.decls.semantic_option_inner(&schema.ty).is_some()
        {
            continue;
        }
        if empty_heap_collection_default(default, &schema.ty) {
            continue;
        }
        if let Err(error) = tc.eval_const_expr(default, false) {
            tc.push_error(error);
        }
    }
}

fn empty_heap_collection_default(expr: &ExprNode, ty: &Type) -> bool {
    match (&expr.node.kind, ty) {
        (ExprKind::ArrayLiteral(lit), Type::List { .. }) => lit.node.elements.is_empty(),
        (ExprKind::MapLiteral(lit), Type::Map { .. }) => lit.node.entries.is_empty(),
        _ => false,
    }
}

pub(super) fn check_module_bodies(module: &ModuleScope, program: &Program, tc: &mut TypeChecker) {
    with_source_module_scope(module, tc, |tc| check_stmts(&program.stmts, tc));
}

fn check_stmt(stmt: &StmtNode, local_const: Option<LocalConstInfo>, tc: &mut TypeChecker) {
    match &stmt.node {
        Stmt::Func(func_node) => {
            let func = &func_node.node;
            let id =
                CallableId::local_function(tc.current_module.clone(), func.name, func_node.span);
            let local = tc.local_callable(&id);
            if is_generic(func) && local.is_none() {
                return;
            }
            if let Some(info) = local
                && tc.callable_template(&info.callee.def.id).is_some()
            {
                return;
            }
            check_func(func_node, tc);
        }
        Stmt::Binding(binding_node) => {
            check_binding(binding_node, tc);
        }
        Stmt::Return(ret_node) => {
            control_flow::check_return(ret_node, tc);
        }
        Stmt::Expr(expr_node) => {
            tc.discard_depth += 1;
            check_expr_checked(expr_node, tc);
            tc.discard_depth -= 1;
        }
        Stmt::While(while_node) => {
            control_flow::check_while(while_node, tc);
        }
        Stmt::WhileLet(while_let_node) => {
            pattern::check_while_let(while_let_node, tc);
        }
        Stmt::For(for_node) => {
            control_flow::check_for(for_node, tc);
        }
        Stmt::Break => {
            control_flow::check_break(stmt.span, tc);
        }
        Stmt::Continue => {
            control_flow::check_continue(stmt.span, tc);
        }
        Stmt::Extend(extend_node) => {
            check_extend(extend_node, tc);
        }
        Stmt::Aggregate(agg_node) => {
            check_aggregate_decl(agg_node, tc);
        }
        Stmt::Enum(_) | Stmt::Contract(_) => {}
        Stmt::Const(const_node) => {
            if tc.scopes.len() > 1 {
                match local_const {
                    Some(info) => {
                        if tc.define_local_symbol(const_node.node.name, info.symbol()) {
                            tc.define_closure_binding(
                                info.binding_id,
                                const_node.node.name,
                                info.type_id,
                                LocalBindingKind::constant(),
                            );
                        }
                        if let Err(err) = tc.eval_local_const(info.id, const_node.span) {
                            tc.push_error(err);
                        }
                    }
                    None => const_eval::check_const(const_node, tc),
                }
            }
        }
        Stmt::Global(_) => {}
        Stmt::TypeAlias(alias_node) => {
            check_type_alias(alias_node, tc);
        }
        Stmt::LetElse(let_else_node) => {
            pattern::check_let_else(let_else_node, tc);
        }
        Stmt::Defer(defer_node) => {
            control_flow::check_defer(defer_node, tc);
        }
        Stmt::Import(_) | Stmt::ExternFunc(_) | Stmt::ExternType(_) => {}
    }
}

fn check_func(func_node: &FuncNode, tc: &mut TypeChecker) {
    let func = &func_node.node;
    let id = CallableId::local_function(tc.current_module.clone(), func.name, func_node.span);
    let local = tc.local_callable(&id);
    let (param_types, ret) = match local.as_ref() {
        Some(info) => (&info.callee.def.sig.params, &info.callee.def.sig.ret),
        None => {
            let func_ty = match tc.lookup(func.name) {
                Some(info) => tc.solver.local_type_to_type(info.type_id),
                None => tc.func_type_from_sig(&func.params, &func.ret, func_node.span),
            };
            let Type::Func { params, ret } = func_ty else {
                return;
            };
            check_func_body(
                None,
                &func.params,
                &params,
                ret.as_ref(),
                &func.body,
                func_node.span,
                &[],
                tc,
            );
            return;
        }
    };
    check_func_body(
        None,
        &func.params,
        param_types,
        ret,
        &func.body,
        func_node.span,
        &[],
        tc,
    );
}

fn check_extend(extend_node: &ExtendDeclNode, tc: &mut TypeChecker) {
    let extend = &extend_node.node;
    if has_generics(&extend.type_params, &extend.const_params) {
        return;
    }

    let self_ty = tc.resolve_type_for_tc_at(&extend.ty, extend_node.span);
    let Some(schema) = tc
        .decls
        .extends()
        .find(|schema| schema.origin == tc.current_module && schema.span.byte() == extend_node.span)
        .cloned()
    else {
        return;
    };

    for method_node in &extend.methods {
        let method = &method_node.node;
        if method_sig_is_generic(&method.sig) {
            continue;
        }
        let mode = MethodMode::from_receiver(method.sig.receiver);
        let Some(method_schema) = schema
            .methods
            .get(&MethodKey::new(method.sig.name, mode.surface()))
        else {
            continue;
        };
        check_func_body(
            mode.receiver().map(|receiver| (receiver, self_ty.clone())),
            &method.sig.params,
            &method_schema.params,
            &method_schema.ret,
            &method.body,
            extend_node.span,
            &[],
            tc,
        );
    }
    for (cast, schema) in extend.cast_froms.iter().zip(schema.cast_froms) {
        check_func_body(
            None,
            std::slice::from_ref(&cast.node.param),
            std::slice::from_ref(&schema.param),
            &ReturnSpec::value(self_ty.clone()),
            &cast.node.body,
            cast.span,
            &[],
            tc,
        );
    }
}

fn check_aggregate_method_bodies(
    agg: &StructDecl,
    span: Span,
    key: &NominalKey,
    schema: &AggregateSchema,
    tc: &mut TypeChecker,
) {
    if !schema.generics.is_empty() {
        return;
    }

    let self_ty = nominal_type(key);
    for method in &agg.methods {
        if method_sig_is_generic(&method.sig) {
            continue;
        }
        let mode = MethodMode::from_receiver(method.sig.receiver);
        let Some(method_schema) = schema
            .methods
            .get(&MethodKey::new(method.sig.name, mode.surface()))
        else {
            continue;
        };
        check_func_body(
            method_schema
                .mode
                .receiver()
                .map(|receiver| (receiver, self_ty.clone())),
            &method.sig.params,
            &method_schema.params,
            &method_schema.ret,
            &method.body,
            span,
            &[],
            tc,
        );
    }
}

pub(super) fn check_block_checked(block: &BlockNode, tc: &mut TypeChecker) -> CheckedType {
    check_block_checked_with_hint(block, None, tc)
}

pub(super) fn check_block_checked_with_hint(
    block: &BlockNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    tc.push_scope();
    let declarations = register_block_declarations(&block.node.stmts, tc);
    for (stmt, local_const) in block.node.stmts.iter().zip(declarations) {
        check_stmt(stmt, local_const, tc);
    }
    let checked = match &block.node.tail {
        Some(expr) => check_expr_checked_with_hint(expr, expected, tc),
        None => checked_void(tc),
    };
    tc.pop_scope();
    checked
}

fn check_type_alias(alias_node: &TypeAliasDeclNode, tc: &mut TypeChecker) {
    let Some(local) = tc.local_type_scopes.by_key(alias_node.span).cloned() else {
        return;
    };
    tc.push_generic_context(local.def.generic_context.clone());
    let aliased = tc.resolve_type_for_tc_at(&local.def.aliased, alias_node.span);
    tc.pop_generic_context();
    validate_type_alias_def(&local.def, &aliased, &mut tc.errors);
}

fn check_callable_body_with_return(
    body: CallableBody<'_>,
    expected_ret: Option<&ReturnSpec>,
    source: Option<&PlaceIdentity>,
    callable_span: Span,
    tc: &mut TypeChecker,
) -> CheckedType {
    if expected_ret.is_some_and(ReturnSpec::is_place) {
        return check_callable_body_place_return(body, expected_ret, source, callable_span, tc);
    }

    let expected = expected_ret.map(|ret| tc.type_handle(&ret.ty));
    let checked = body.check_with_hint(expected, tc);
    finish_callable_body_value_return(body, &checked, expected_ret, callable_span, tc);
    checked
}

pub(super) fn check_callable_body_place_return(
    body: CallableBody<'_>,
    expected_ret: Option<&ReturnSpec>,
    source: Option<&PlaceIdentity>,
    callable_span: Span,
    tc: &mut TypeChecker,
) -> CheckedType {
    match body {
        CallableBody::Block(block) => {
            tc.push_scope();
            let declarations = register_block_declarations(&block.node.stmts, tc);
            for (stmt, local_const) in block.node.stmts.iter().zip(declarations) {
                check_stmt(stmt, local_const, tc);
            }
            let checked = match &block.node.tail {
                Some(expr) => check_tail_place_return(expr, expected_ret, source, tc),
                None => checked_void(tc),
            };
            finish_missing_place_return(
                &checked,
                control_flow::block_diverges(block),
                expected_ret,
                block.span.to_end(),
                callable_span,
                tc,
            );
            tc.pop_scope();
            checked
        }
        CallableBody::Expr(expr) => check_tail_place_return(expr, expected_ret, source, tc),
    }
}

fn check_tail_place_return(
    expr: &ExprNode,
    expected_ret: Option<&ReturnSpec>,
    source: Option<&PlaceIdentity>,
    tc: &mut TypeChecker,
) -> CheckedType {
    if let ExprKind::Block(block) = &expr.node.kind {
        return check_callable_body_place_return(
            CallableBody::Block(block),
            expected_ret,
            source,
            expr.span,
            tc,
        );
    }

    let ret = expected_ret
        .cloned()
        .unwrap_or_else(|| ReturnSpec::place(Type::InferReturn));
    let checked = control_flow::check_return_expr(expr, &ret, source, tc);
    if expected_ret.is_none() {
        tc.push_inferred_return(expr.span, checked.handle.clone());
    }
    checked
}

fn finish_missing_place_return(
    checked: &CheckedType,
    diverges: bool,
    expected_ret: Option<&ReturnSpec>,
    missing_span: Span,
    callable_span: Span,
    tc: &mut TypeChecker,
) {
    if checked.ty.is_void() && !diverges {
        match expected_ret {
            Some(ret) if !ret.ty.is_void() => tc.push_error(TypeError::MissingReturn {
                expected: ret.ty.clone(),
                span: tc.error_span(missing_span),
            }),
            None => tc.push_inferred_return(callable_span, tc.type_handle(&Type::Void)),
            _ => {}
        }
    }
}

fn finish_callable_body_value_return(
    body: CallableBody<'_>,
    checked: &CheckedType,
    expected_ret: Option<&ReturnSpec>,
    callable_span: Span,
    tc: &mut TypeChecker,
) {
    if checked.ty.is_void() {
        match expected_ret {
            Some(ret) if !ret.ty.is_void() && !body.diverges() => {
                tc.push_error(TypeError::MissingReturn {
                    expected: ret.ty.clone(),
                    span: tc.error_span(body.span().to_end()),
                });
            }
            None if !body.diverges() => {
                tc.push_inferred_return(callable_span, tc.type_handle(&Type::Void));
            }
            _ => {}
        }
        return;
    }

    if let Some(expr) = body.value_expr() {
        tc.record_escaping_use(expr);
    }
    tc.reject_extern_any_escape(checked, body.span());
    match expected_ret {
        Some(ret) => {
            let ret_handle = tc.type_handle(&ret.ty);
            match body.value_expr_id() {
                Some(expr_id) => tc.expect_assignable_expr(
                    body.span(),
                    expr_id,
                    checked.handle.clone(),
                    ret_handle,
                ),
                None => tc.expect_assignable(body.span(), checked.handle.clone(), ret_handle),
            }
        }
        None => tc.push_inferred_return(body.span(), checked.handle.clone()),
    }
}

pub(super) fn with_callable_body_scope<R>(
    tc: &mut TypeChecker,
    before_scope: impl FnOnce(&mut TypeChecker),
    check: impl FnOnce(&mut TypeChecker) -> R,
    after_scope: impl FnOnce(&mut TypeChecker),
) -> R {
    let flow = tc.enter_function_control_flow();
    before_scope(tc);
    tc.push_scope();
    let ret = check(tc);
    tc.pop_scope();
    after_scope(tc);
    tc.exit_function_control_flow(flow);
    ret
}

pub(super) struct CallableParamBinding<'a> {
    pub(super) name: Ident,
    pub(super) source_ty: Option<&'a Type>,
    pub(super) ty: &'a FuncParam,
}

pub(super) fn check_callable_body_frame(
    params: &[CallableParamBinding<'_>],
    expected_ret: Option<&ReturnSpec>,
    infer_access: ReturnAccess,
    mut source: Option<PlaceIdentity>,
    body: CallableBody<'_>,
    span: Span,
    tc: &mut TypeChecker,
) -> Option<Type> {
    for param in params {
        let kind = LocalBindingKind::from_param(param.ty.mutable, &param.ty.ty);
        let type_id = tc.define_value(param.name, param.ty.ty.clone(), kind, None);
        tc.mark_non_escaping_callback_param(param.name, type_id, param.ty, param.source_ty);
        if source.is_none() && param.ty.mutable {
            source = Some(PlaceIdentity::root(PlaceRoot::Local(type_id)));
        }
    }
    let return_mode = match expected_ret {
        Some(ret) => ReturnMode::Explicit {
            ret: ret.clone(),
            source: source.clone(),
        },
        None => ReturnMode::Infer {
            access: infer_access,
            source: source.clone(),
            candidates: vec![],
        },
    };
    tc.push_return_frame(return_mode);
    check_callable_body_with_return(body, expected_ret, source.as_ref(), span, tc);
    let frame = tc.pop_return_frame();
    frame.and_then(|frame| infer_return_type(frame, tc))
}

fn check_func_body(
    self_binding: Option<(MethodReceiver, Type)>,
    params: &[Param],
    param_types: &[FuncParam],
    ret: &ReturnSpec,
    body: &BlockNode,
    span: Span,
    const_bindings: &[(Ident, ConstValue)],
    tc: &mut TypeChecker,
) -> Option<Type> {
    check_param_default_values(params, param_types, tc);
    with_callable_body_scope(
        tc,
        TypeChecker::enter_named_function,
        |tc| {
            for (name, value) in const_bindings {
                tc.define_const(*name, const_eval::const_type(value), value.clone());
            }
            let mut source = None;
            if let Some((receiver, self_ty)) = self_binding {
                let kind = match receiver {
                    MethodReceiver::Var => LocalBindingKind::borrowed_self(),
                    MethodReceiver::Value => LocalBindingKind::readonly_self(),
                };
                let type_id = tc.define_value(Ident::new("self"), self_ty, kind, None);
                if matches!(receiver, MethodReceiver::Var) {
                    source = Some(PlaceIdentity::root(PlaceRoot::Local(type_id)));
                }
            }
            let bindings = params
                .iter()
                .zip(param_types)
                .map(|(param, ty)| CallableParamBinding {
                    name: param.name,
                    source_ty: Some(&param.ty),
                    ty,
                })
                .collect::<Vec<_>>();
            check_callable_body_frame(
                &bindings,
                (!ret.is_infer()).then_some(ret),
                ret.access,
                source,
                CallableBody::Block(body),
                span,
                tc,
            )
        },
        TypeChecker::exit_named_function,
    )
}

fn infer_return_type(frame: ReturnFrame, tc: &mut TypeChecker) -> Option<Type> {
    let ReturnMode::Infer { candidates, .. } = frame.mode else {
        return None;
    };
    let mut candidates = candidates.into_iter();
    let (_, first) = candidates.next()?;
    tc.solve_constraints();
    let inferred = tc.handle_type(&first);
    for (span, candidate) in candidates {
        let found = tc.handle_type(&candidate);
        if inferred != found && !matches!(inferred, Type::Infer) && !matches!(found, Type::Infer) {
            tc.push_error(TypeError::InferReturnMismatch {
                expected: inferred.clone(),
                found,
                span: tc.error_span(span),
            });
        }
    }
    Some(inferred)
}

fn check_param_default_values(params: &[Param], param_types: &[FuncParam], tc: &mut TypeChecker) {
    for (param, param_ty) in params.iter().zip(param_types) {
        let Some(default) = &param.default else {
            continue;
        };
        let expected = tc.type_handle(&param_ty.ty);
        match validate_const_expr_type(default, Some(expected), tc) {
            Ok(_) => {
                if let Err(error) = tc.eval_const_expr(default, false) {
                    tc.push_error(error);
                }
            }
            Err(error) => tc.push_error(error),
        }
    }
}

pub(super) fn with_callable_body_env<R>(
    module: &ModuleScope,
    env: &CallableTemplateEnv,
    tc: &mut TypeChecker,
    f: impl FnOnce(&mut TypeChecker) -> R,
) -> R {
    match env {
        CallableTemplateEnv::SourceModule => with_source_module_scope(module, tc, f),
        CallableTemplateEnv::Local(state) => {
            let previous_module = std::mem::replace(&mut tc.current_module, module.clone());
            let previous_state = tc.replace_scope_state(state.clone());
            let ret = f(tc);
            tc.restore_scope_state(previous_state);
            tc.current_module = previous_module;
            ret
        }
    }
}

pub(super) fn check_specialized_callable_body(
    callee: &CallableRef,
    param_types: &[FuncParam],
    ret: &ReturnSpec,
    args: &GenericArgs,
    type_subst: TypeSubst,
    const_subst: ConstSubst,
    const_bindings: &[(Ident, ConstValue)],
    tc: &mut TypeChecker,
) -> Option<Type> {
    if args.is_empty()
        || matches!(
            callee.def.id.kind,
            CallableKind::ExternFunction | CallableKind::EnumVariant
        )
    {
        return None;
    }

    let template = tc.callable_template(&callee.def.id).cloned()?;
    let inferred = template.ret.is_infer();
    let key = specialization_key(callee.def.id.clone(), args);
    match tc.specialization(&key).cloned() {
        Some(SpecializationState::InProgress) if inferred => {
            tc.push_error(TypeError::InferReturnRecursive {
                span: tc.error_span(template.ret_span),
            });
            return Some(Type::Infer);
        }
        Some(SpecializationState::InProgress) => return None,
        Some(SpecializationState::Done(body)) => {
            tc.restore_specialization(body.facts);
            if let Err(message) = tc.dyn_infer.restore_specialization(body.dyn_infer) {
                tc.push_error(TypeError::CompileError {
                    message,
                    span: None,
                });
            }
            return body.inferred_ret;
        }
        None => {}
    }

    let receiver = template.mode.receiver().zip(callee.receiver_ty.clone());

    let owner_frame = GenericOwnerFrame {
        params: combined_callable_params(callee),
        args: args.clone(),
        generics: template.generics.clone(),
    };
    check_with_specialization(key, type_subst, const_subst, owner_frame, tc, |tc| {
        with_callable_body_env(&callee.def.id.module, &template.env, tc, |tc| {
            check_func_body(
                receiver,
                &template.params,
                param_types,
                ret,
                &template.body,
                template.span,
                const_bindings,
                tc,
            )
        })
    })
}

fn with_global_scope<R>(tc: &mut TypeChecker, f: impl FnOnce(&mut TypeChecker) -> R) -> R {
    let state = tc.take_scope_state();
    tc.replace_scopes(state.scopes.first().cloned().into_iter().collect());
    let ret = f(tc);
    tc.restore_scope_state(state);
    ret
}

fn with_source_module_scope<R>(
    module: &ModuleScope,
    tc: &mut TypeChecker,
    f: impl FnOnce(&mut TypeChecker) -> R,
) -> R {
    let previous_module = std::mem::replace(&mut tc.current_module, module.clone());
    let ret = match module {
        ModuleScope::Root => with_global_scope(tc, f),
        ModuleScope::Named(_) | ModuleScope::Package(_) => {
            let state = tc.take_scope_state();
            tc.replace_scopes(vec![]);
            push_source_scope(tc);
            if let Some(program) = tc.module_programs.get(module).map(Rc::clone) {
                register_declarations(program.as_ref(), tc);
                tc.eval_module_consts(module);
            }
            let ret = f(tc);
            tc.restore_scope_state(state);
            ret
        }
    };
    tc.current_module = previous_module;
    ret
}
