use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
    sync::LazyLock,
};

use internment::Intern;

use super::{
    annotations::{AnnotationTarget, AppliedAnnotations, normalize_annotations},
    composite::{is_all_nil_array_literal, is_empty_map_literal},
    const_eval::{
        ConstDef, build_const_dependency_graph, collect_const_decls, eval_const_expr,
        validate_const_expr,
    },
    constraint::TypeRef,
    control::{block_always_diverges, check_for, check_while, check_while_let, is_if_without_else},
    decl::{check_body_common, check_func, check_struct},
    error::{Diagnostic, DiagnosticKind},
    expr::check_expr,
    infer::type_from_fn,
    pattern::{check_pattern, is_refutable},
    types::{
        CastEntry, EnumDef, ExtendEntry, ExtendMethodDef, ExternFieldDef, ExternMethodDef,
        ExternOpDef, ExternTypeDef, ExternUnaryOpDef, GenericExtendTemplate, ModuleDef,
        ModuleExtendEntry, ModuleGenericExtendEntry, StructDef, TypeChecker, VarInfo,
        build_cast_accept, build_param_info, unwrap_opt_typ, validate_map_key_type,
    },
    unify::contains_infer,
    visit::type_any,
};
use crate::{
    ast::{
        AggregateKind, ArrayLen, BlockNode, ConstDeclNode, ConstParam, DeferBody, ExprId, ExprKind,
        ExprNode, ExternFunc, ExternTypeMember, Func, FuncNode, FuncParam, Ident, ImportKind,
        ModulePath, Mutability, Param, ReturnNode, Stmt, StmtNode, Type, TypeParam, VariantKind,
        Visibility,
    },
    backend_names,
    span::Span,
};

static IDENT_LIST: LazyLock<Ident> = LazyLock::new(|| Ident(Intern::new("__List".to_string())));
static IDENT_MAP: LazyLock<Ident> = LazyLock::new(|| Ident(Intern::new("__Map".to_string())));
static IDENT_TUPLE: LazyLock<Ident> = LazyLock::new(|| Ident(Intern::new("__Tuple".to_string())));
static IDENT_ARRAY: LazyLock<Ident> = LazyLock::new(|| Ident(Intern::new("__Array".to_string())));

pub(super) fn check_block_stmts(
    stmts: &[StmtNode],
    tail: Option<&ExprNode>,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
    expected_tail: Option<&Type>,
) -> Option<ExprId> {
    let in_function = !type_checker.return_types.is_empty();

    type_checker.push_scope();
    collect_scope_types(stmts, type_checker, errors);

    let const_snapshot = if in_function {
        type_checker.const_scope_stack.push(HashSet::new());
        Some(type_checker.ctx.const_defs.clone())
    } else {
        evaluate_module_consts(stmts, type_checker, errors);
        None
    };

    for stmt in stmts {
        check_stmt(stmt, type_checker, errors);
    }

    let tail_id = tail.map(|expr| {
        let _ = check_expr(expr, type_checker, errors, expected_tail);
        expr.node.id
    });

    if let Some(snapshot) = const_snapshot {
        type_checker.const_scope_stack.pop();
        type_checker.ctx.const_defs = snapshot;
    }

    type_checker.pop_scope();
    tail_id
}

fn evaluate_module_consts(
    stmts: &[StmtNode],
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) {
    let decls = collect_const_decls(stmts);
    if decls.is_empty() {
        return;
    }

    let order = match build_const_dependency_graph(&decls) {
        Ok(order) => order,
        Err(err) => {
            errors.push(err);
            return;
        }
    };

    for idx in order {
        let decl = decls[idx];
        eval_and_insert_const(decl, type_checker, errors);
    }
}

fn eval_and_insert_const(
    decl: &ConstDeclNode,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) -> bool {
    let name = decl.node.name;

    let known_consts: HashSet<Ident> = type_checker.ctx.const_defs.keys().copied().collect();
    if let Err(err) = validate_const_expr(&decl.node.value, &known_consts) {
        errors.push(err);
        return false;
    }

    let annotated_ty = decl
        .node
        .ty
        .as_ref()
        .map(|ty| type_checker.resolve_type(ty));

    let _ = check_expr(
        &decl.node.value,
        type_checker,
        errors,
        annotated_ty.as_ref(),
    );

    let const_value = match eval_const_expr(&decl.node.value, &type_checker.ctx.const_defs) {
        Ok(val) => val,
        Err(err) => {
            errors.push(err);
            return false;
        }
    };

    let value_ty = const_value.ty();
    if let Some(ref ann_ty) = annotated_ty
        && *ann_ty != value_ty
    {
        errors.push(Diagnostic::new(
            decl.span,
            DiagnosticKind::ConstTypeMismatch {
                expected: ann_ty.clone(),
                got: value_ty,
            },
        ));
        return false;
    }

    let final_ty = annotated_ty.unwrap_or(value_ty);
    type_checker.ctx.const_defs.insert(
        name,
        ConstDef {
            ty: final_ty,
            value: const_value,
            visibility: decl.node.visibility,
        },
    );
    true
}

pub(super) fn check_block_expr(
    block: &BlockNode,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
    expected_tail: Option<&Type>,
) -> (Type, Option<ExprId>) {
    let last_expr_id = check_block_stmts(
        &block.node.stmts,
        block.node.tail.as_deref(),
        type_checker,
        errors,
        expected_tail,
    );
    let Some(id) = last_expr_id else {
        return (Type::Void, None);
    };

    let ty = type_checker
        .get_type(id)
        .cloned()
        .map_or(Type::Void, |(_, ty)| ty);

    (ty, Some(id))
}

struct FuncRegistration {
    func_ty: Type,
    param_info: Vec<(Ident, Mutability)>,
    cast_accept: Vec<bool>,
    type_params: Option<Vec<TypeParam>>,
    const_params: Option<Vec<ConstParam>>,
    template: Option<FuncNode>,
}

enum DeclInfo {
    Func {
        name: Ident,
        visibility: Visibility,
        reg: FuncRegistration,
    },
    ExternFunc {
        name: Ident,
        func_ty: Type,
        param_info: Vec<(Ident, Mutability)>,
    },
    ExternType {
        name: Ident,
        def: ExternTypeDef,
    },
    Aggregate {
        name: Ident,
        visibility: Visibility,
        def: StructDef,
    },
    Enum {
        name: Ident,
        visibility: Visibility,
        def: EnumDef,
    },
    Const {
        name: Ident,
    },
}

fn extract_decl(stmt: &StmtNode, resolve: &impl Fn(&Type) -> Type) -> Option<DeclInfo> {
    match &stmt.node {
        Stmt::Func(node) => {
            let func = &node.node;
            Some(DeclInfo::Func {
                name: func.name,
                visibility: func.visibility,
                reg: build_func_registration(func, node, resolve),
            })
        }
        Stmt::ExternFunc(node) => {
            let extern_func = &node.node;
            let (func_ty, param_info) = build_extern_func_registration(extern_func, resolve);
            Some(DeclInfo::ExternFunc {
                name: extern_func.name,
                func_ty,
                param_info,
            })
        }
        Stmt::ExternType(node) => Some(DeclInfo::ExternType {
            name: node.node.name,
            def: build_extern_type_def(node.node.has_init, &node.node.members, resolve),
        }),
        Stmt::Aggregate(node) => Some(DeclInfo::Aggregate {
            name: node.node.name,
            visibility: node.node.visibility,
            def: StructDef::from_ast(&node.node, node.span),
        }),
        Stmt::Enum(node) => Some(DeclInfo::Enum {
            name: node.node.name,
            visibility: node.node.visibility,
            def: EnumDef::from_ast(&node.node),
        }),
        Stmt::Const(node) => Some(DeclInfo::Const {
            name: node.node.name,
        }),
        _ => None,
    }
}

fn apply_decl_to_scope(decl: DeclInfo, type_checker: &mut TypeChecker) {
    let origin: Option<ModulePath> = type_checker.ctx.module_path.as_deref().map(Rc::from);
    match decl {
        DeclInfo::Func {
            name,
            reg:
                FuncRegistration {
                    func_ty,
                    param_info,
                    cast_accept,
                    type_params,
                    const_params,
                    template,
                },
            ..
        } => {
            type_checker.ctx.func_bindings.insert(
                name,
                VarInfo {
                    ty: func_ty.clone(),
                    mutable: false,
                    is_function: true,
                },
            );
            type_checker.set_func_var(name, func_ty);
            type_checker.func_param_info.insert(name, param_info);
            if cast_accept.iter().any(|f| *f) {
                type_checker.func_cast_accept.insert(name, cast_accept);
            }
            if let Some(tp) = type_params {
                type_checker.func_type_params.insert(name, tp);
            }
            if let Some(cp) = const_params {
                type_checker.func_const_params.insert(name, cp);
            }
            if let Some(tmpl) = template {
                type_checker.generic_func_templates.insert(name, tmpl);
            }
        }
        DeclInfo::ExternFunc {
            name,
            func_ty,
            param_info,
        } => {
            type_checker.ctx.func_bindings.insert(
                name,
                VarInfo {
                    ty: func_ty.clone(),
                    mutable: false,
                    is_function: true,
                },
            );
            type_checker.set_func_var(name, func_ty);
            type_checker.func_param_info.insert(name, param_info);
        }
        DeclInfo::ExternType { name, mut def } => {
            def.defining_module.clone_from(&origin);
            type_checker.ctx.extern_type_defs.insert(name, def);
        }
        DeclInfo::Aggregate { name, mut def, .. } => {
            def.defining_module.clone_from(&origin);
            type_checker.ctx.struct_defs.insert(name, def);
        }
        DeclInfo::Enum { name, mut def, .. } => {
            def.defining_module.clone_from(&origin);
            type_checker.ctx.enum_defs.insert(name, def);
        }
        DeclInfo::Const { .. } => {}
    }
}

fn apply_decl_to_summary(
    decl: DeclInfo,
    module_def: &mut ModuleDef,
    defining_module: &[String],
) -> Option<Ident> {
    let origin: Option<ModulePath> = Some(Rc::from(defining_module));
    match decl {
        DeclInfo::Func {
            name,
            visibility,
            reg:
                FuncRegistration {
                    func_ty,
                    param_info,
                    cast_accept,
                    type_params,
                    const_params,
                    template,
                },
            ..
        } => {
            module_def.all_names.insert(name);
            if visibility != Visibility::Public {
                return None;
            }
            module_def.funcs.insert(name, func_ty);
            module_def.func_param_info.insert(name, param_info);
            if cast_accept.iter().any(|f| *f) {
                module_def.func_cast_accept.insert(name, cast_accept);
            }
            if let Some(tp) = type_params {
                module_def.func_type_params.insert(name, tp);
            }
            if let Some(cp) = const_params {
                module_def.func_const_params.insert(name, cp);
            }
            if let Some(tmpl) = template {
                module_def.generic_func_templates.insert(name, tmpl);
            }
            Some(name)
        }
        DeclInfo::ExternFunc {
            name,
            func_ty,
            param_info,
        } => {
            module_def.all_names.insert(name);
            module_def.funcs.insert(name, func_ty);
            module_def.func_param_info.insert(name, param_info);
            None
        }
        DeclInfo::ExternType { name, mut def } => {
            module_def.all_names.insert(name);
            def.defining_module.clone_from(&origin);
            module_def.extern_types.insert(name, def);
            None
        }
        DeclInfo::Aggregate {
            name,
            visibility,
            mut def,
        } => {
            module_def.all_names.insert(name);
            if visibility != Visibility::Public {
                return None;
            }
            def.defining_module.clone_from(&origin);
            module_def.struct_defs.insert(name, def);
            Some(name)
        }
        DeclInfo::Enum {
            name,
            visibility,
            mut def,
        } => {
            module_def.all_names.insert(name);
            if visibility != Visibility::Public {
                return None;
            }
            def.defining_module.clone_from(&origin);
            module_def.enum_defs.insert(name, def);
            Some(name)
        }
        DeclInfo::Const { name } => {
            module_def.all_names.insert(name);
            None
        }
    }
}

fn build_func_registration(
    func: &Func,
    node: &FuncNode,
    resolve: impl Fn(&Type) -> Type,
) -> FuncRegistration {
    let func_ty = resolve(&type_from_fn(func));
    let param_info = build_param_info(&func.params);
    let cast_accept = build_cast_accept(&func.params);
    let is_generic = !func.type_params.is_empty() || !func.const_params.is_empty();
    let type_params = is_generic.then(|| func.type_params.clone());
    let const_params = is_generic.then(|| func.const_params.clone());
    let template = is_generic.then(|| node.clone());
    FuncRegistration {
        func_ty,
        param_info,
        cast_accept,
        type_params,
        const_params,
        template,
    }
}

fn build_extern_func_registration(
    extern_func: &ExternFunc,
    resolve: impl Fn(&Type) -> Type,
) -> (Type, Vec<(Ident, Mutability)>) {
    let func_ty = Type::Func {
        params: extern_func
            .params
            .iter()
            .map(|p| FuncParam::new(resolve(&p.ty), matches!(p.mutability, Mutability::Mutable)))
            .collect(),
        ret: Box::new(resolve(&extern_func.ret)),
    };
    let param_info = build_param_info(&extern_func.params);
    (func_ty, param_info)
}

fn push_reexport_collision(
    errors: &mut Vec<Diagnostic>,
    span: Span,
    name: Ident,
    existing: &str,
    source: &str,
    help: &str,
) {
    errors.push(
        Diagnostic::new(
            span,
            DiagnosticKind::ReExportCollision {
                name,
                first_source: existing.to_string(),
                second_source: source.to_string(),
            },
        )
        .with_help(help),
    );
}

fn copy_func_ancillary_to_module(
    name: Ident,
    bind_as: Ident,
    source: &ModuleDef,
    target: &mut ModuleDef,
) {
    if let Some(param_info) = source.func_param_info.get(&name) {
        target.func_param_info.insert(bind_as, param_info.clone());
    }
    if let Some(tp) = source.func_type_params.get(&name) {
        target.func_type_params.insert(bind_as, tp.clone());
    }
    if let Some(cp) = source.func_const_params.get(&name) {
        target.func_const_params.insert(bind_as, cp.clone());
    }
    if let Some(tmpl) = source.generic_func_templates.get(&name) {
        target.generic_func_templates.insert(bind_as, tmpl.clone());
    }
    if let Some(defaults) = source.func_param_defaults.get(&name) {
        target.func_param_defaults.insert(bind_as, defaults.clone());
    }
    if let Some(ca) = source.func_cast_accept.get(&name) {
        target.func_cast_accept.insert(bind_as, ca.clone());
    }
}

fn copy_func_ancillary_to_checker(
    name: Ident,
    bind_as: Ident,
    source: &ModuleDef,
    type_checker: &mut TypeChecker,
) {
    if let Some(param_info) = source.func_param_info.get(&name) {
        type_checker
            .func_param_info
            .insert(bind_as, param_info.clone());
    }
    if let Some(tp) = source.func_type_params.get(&name) {
        type_checker.func_type_params.insert(bind_as, tp.clone());
    }
    if let Some(cp) = source.func_const_params.get(&name) {
        type_checker.func_const_params.insert(bind_as, cp.clone());
    }
    if let Some(tmpl) = source.generic_func_templates.get(&name) {
        type_checker
            .generic_func_templates
            .insert(bind_as, tmpl.clone());
        type_checker
            .generic_func_source_module
            .insert(bind_as, source.source_path.clone());
    }
    if let Some(defaults) = source.func_param_defaults.get(&name) {
        type_checker
            .func_param_defaults
            .insert(bind_as, defaults.clone());
    }
    if let Some(ca) = source.func_cast_accept.get(&name) {
        type_checker.func_cast_accept.insert(bind_as, ca.clone());
    }
}

fn merge_symbol(name: Ident, bind_as: Ident, source: &ModuleDef, target: &mut ModuleDef) {
    if let Some(ty) = source.funcs.get(&name) {
        target.funcs.insert(bind_as, ty.clone());
        target.all_names.insert(bind_as);
        copy_func_ancillary_to_module(name, bind_as, source, target);
    } else if let Some(struct_def) = source.struct_defs.get(&name) {
        target.struct_defs.insert(bind_as, struct_def.clone());
        target.all_names.insert(bind_as);
    } else if let Some(enum_def) = source.enum_defs.get(&name) {
        target.enum_defs.insert(bind_as, enum_def.clone());
        target.all_names.insert(bind_as);
    } else if let Some(extern_def) = source.extern_types.get(&name) {
        target.extern_types.insert(bind_as, extern_def.clone());
        target.all_names.insert(bind_as);
    } else if let Some(const_def) = source.const_defs.get(&name) {
        target.const_defs.insert(bind_as, const_def.clone());
        target.all_names.insert(bind_as);
    }
}

fn inject_module_item(
    name: Ident,
    bind_as: Ident,
    module_def: &ModuleDef,
    type_checker: &mut TypeChecker,
) {
    if let Some(ty) = module_def.funcs.get(&name) {
        type_checker.set_func_var(bind_as, ty.clone());
        copy_func_ancillary_to_checker(name, bind_as, module_def, type_checker);
    } else if let Some(struct_def) = module_def.struct_defs.get(&name) {
        type_checker
            .ctx
            .struct_defs
            .insert(bind_as, struct_def.clone());
    } else if let Some(enum_def) = module_def.enum_defs.get(&name) {
        type_checker.ctx.enum_defs.insert(bind_as, enum_def.clone());
    } else if let Some(extern_def) = module_def.extern_types.get(&name) {
        type_checker
            .ctx
            .extern_type_defs
            .insert(bind_as, extern_def.clone());
    } else if let Some(sub_module) = module_def.re_exported_modules.get(&name) {
        type_checker
            .ctx
            .module_defs
            .insert(bind_as, sub_module.clone());
    } else if let Some(const_def) = module_def.const_defs.get(&name) {
        type_checker
            .ctx
            .const_defs
            .insert(bind_as, const_def.clone());
    }

    // symbol not found is silently skipped here, check_stmt will report the error
}

fn build_extern_type_def(
    has_init: bool,
    members: &[ExternTypeMember],
    resolve: impl Fn(&Type) -> Type,
) -> ExternTypeDef {
    let mut field_order = vec![];
    let mut fields = HashMap::new();
    let mut methods = HashMap::new();
    let mut statics = HashMap::new();
    let mut operators = vec![];
    let mut unary_operators = vec![];

    for member in members {
        match member {
            ExternTypeMember::Field {
                name,
                ty,
                computed,
                doc: _,
            } => {
                if !computed {
                    field_order.push(*name);
                }
                fields.insert(*name, ExternFieldDef { ty: resolve(ty) });
            }
            ExternTypeMember::Method {
                name,
                receiver,
                params,
                ret,
                ..
            } => {
                methods.insert(
                    *name,
                    ExternMethodDef {
                        receiver: Some(*receiver),
                        params: params
                            .iter()
                            .map(|p| Param {
                                mutability: p.mutability,
                                name: p.name,
                                ty: resolve(&p.ty),
                                default: p.default.clone(),
                                cast_accept: p.cast_accept,
                            })
                            .collect(),
                        ret: resolve(ret),
                    },
                );
            }
            ExternTypeMember::StaticMethod {
                name, params, ret, ..
            } => {
                statics.insert(
                    *name,
                    ExternMethodDef {
                        receiver: None,
                        params: params
                            .iter()
                            .map(|p| Param {
                                mutability: p.mutability,
                                name: p.name,
                                ty: resolve(&p.ty),
                                default: p.default.clone(),
                                cast_accept: p.cast_accept,
                            })
                            .collect(),
                        ret: resolve(ret),
                    },
                );
            }
            ExternTypeMember::Operator {
                op,
                other_ty,
                ret,
                self_on_right,
            } => {
                operators.push(ExternOpDef {
                    op: *op,
                    other_ty: resolve(other_ty),
                    ret: resolve(ret),
                    self_on_right: *self_on_right,
                });
            }
            ExternTypeMember::UnaryOperator { op, ret } => {
                unary_operators.push(ExternUnaryOpDef {
                    op: *op,
                    ret: resolve(ret),
                });
            }
        }
    }

    ExternTypeDef {
        defining_module: None,
        has_init,
        field_order,
        fields,
        methods,
        statics,
        operators,
        unary_operators,
    }
}

fn try_inject_import(
    source: Ident,
    bind_as: Ident,
    span: Span,
    module_def: &ModuleDef,
    type_checker: &mut TypeChecker,
    imported: &mut HashSet<Ident>,
    seen: &mut HashSet<Ident>,
    errors: &mut Vec<Diagnostic>,
) {
    let existing = if imported.contains(&bind_as) {
        Some("a previously imported name")
    } else if seen.contains(&bind_as) {
        Some("a locally defined type")
    } else {
        None
    };
    if let Some(existing) = existing {
        errors.push(Diagnostic::new(
            span,
            DiagnosticKind::ImportNameConflict {
                name: bind_as,
                existing,
            },
        ));
    } else {
        inject_module_item(source, bind_as, module_def, type_checker);
        imported.insert(bind_as);
        seen.insert(bind_as);
    }
}

pub(super) fn collect_scope_types(
    stmts: &[StmtNode],
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) {
    // register extern type names before anything else, so they can be
    // resolved when building function signatures that reference them
    for stmt in stmts {
        if let Stmt::ExternType(node) = &stmt.node {
            type_checker.ctx.extern_type_defs.insert(
                node.node.name,
                ExternTypeDef {
                    defining_module: None,
                    has_init: false,
                    field_order: vec![],
                    fields: HashMap::new(),
                    methods: HashMap::new(),
                    statics: HashMap::new(),
                    operators: vec![],
                    unary_operators: vec![],
                },
            );
        }
    }

    let mut seen: HashSet<Ident> = HashSet::new();
    let mut imported: HashSet<Ident> = HashSet::new();
    let mut imported_modules: HashSet<Ident> = HashSet::new();

    for stmt in stmts {
        if let Some(decl) = extract_decl(stmt, &|ty| type_checker.resolve_type(ty)) {
            let duplicate_name = match &decl {
                DeclInfo::ExternType { name, .. }
                | DeclInfo::Aggregate { name, .. }
                | DeclInfo::Enum { name, .. } => Some(*name),
                _ => None,
            };

            if let Some(name) = duplicate_name
                && !seen.insert(name)
            {
                errors.push(Diagnostic::new(
                    stmt.span,
                    DiagnosticKind::DuplicateTypeDefinition { name },
                ));
                continue;
            }

            apply_decl_to_scope(decl, type_checker);
            continue;
        }

        match &stmt.node {
            Stmt::Import(node) => {
                let import = &node.node;
                let path_key: Vec<String> = import.path.iter().map(ToString::to_string).collect();

                let Some(module_def) = type_checker.resolved_module_defs.get(&path_key).cloned()
                else {
                    // module wasn't resolved (std.*, unresolved error, etc.)
                    continue;
                };

                // Activate extend methods from the imported module for all import forms
                let binding_name = match &import.kind {
                    ImportKind::Module => *import.path.last().expect("import path cannot be empty"),
                    ImportKind::ModuleAs(alias) => *alias,
                    ImportKind::Selective(_) | ImportKind::Wildcard => {
                        *import.path.last().expect("import path cannot be empty")
                    }
                };
                for entry in &module_def.extend_methods {
                    let key = (entry.ty.clone(), entry.name);
                    let entries = type_checker.ctx.extend_defs.entry(key).or_default();
                    let already_registered = entries.iter().any(|e| e.source_module == path_key);
                    if !already_registered {
                        entries.push(ExtendEntry {
                            source_module: path_key.clone(),
                            binding: binding_name,
                            def: entry.def.clone(),
                        });
                    }
                }
                for entry in &module_def.generic_extend_methods {
                    let key = (entry.base_name, entry.method_name);
                    let entries = type_checker
                        .ctx
                        .generic_extend_templates
                        .entry(key)
                        .or_default();
                    let already_registered = entries
                        .iter()
                        .any(|e| e.source_module == path_key && e.target_type == entry.target_type);
                    if !already_registered {
                        entries.push(GenericExtendTemplate {
                            type_params: entry.type_params.clone(),
                            const_params: entry.const_params.clone(),
                            target_type: entry.target_type.clone(),
                            method: entry.method.clone(),
                            source_module: path_key.clone(),
                            binding: binding_name,
                        });
                    }
                }
                for entry in &module_def.cast_entries {
                    let key = (entry.source_type.clone(), entry.target_type.clone());
                    type_checker
                        .ctx
                        .cast_defs
                        .entry(key)
                        .or_insert_with(|| entry.clone());
                }

                match &import.kind {
                    ImportKind::Module => {
                        let binding = *import.path.last().expect("import path cannot be empty");
                        if imported_modules.contains(&binding) {
                            errors.push(Diagnostic::new(
                                stmt.span,
                                DiagnosticKind::ModuleBindingConflict { name: binding },
                            ));
                        } else {
                            imported_modules.insert(binding);
                            type_checker.ctx.module_defs.insert(binding, module_def);
                        }
                    }
                    ImportKind::ModuleAs(alias) => {
                        if imported_modules.contains(alias) {
                            errors.push(Diagnostic::new(
                                stmt.span,
                                DiagnosticKind::ModuleBindingConflict { name: *alias },
                            ));
                        } else {
                            imported_modules.insert(*alias);
                            type_checker.ctx.module_defs.insert(*alias, module_def);
                        }
                    }
                    ImportKind::Selective(items) => {
                        for item in items {
                            let bind_as = item.alias.unwrap_or(item.name);
                            try_inject_import(
                                item.name,
                                bind_as,
                                stmt.span,
                                &module_def,
                                type_checker,
                                &mut imported,
                                &mut seen,
                                errors,
                            );
                        }
                    }
                    ImportKind::Wildcard => {
                        let names: Vec<Ident> = module_def.all_public_names().collect();
                        for name in names {
                            try_inject_import(
                                name,
                                name,
                                stmt.span,
                                &module_def,
                                type_checker,
                                &mut imported,
                                &mut seen,
                                errors,
                            );
                        }
                        let sub_module_names: Vec<Ident> =
                            module_def.re_exported_modules.keys().copied().collect();
                        for name in sub_module_names {
                            try_inject_import(
                                name,
                                name,
                                stmt.span,
                                &module_def,
                                type_checker,
                                &mut imported,
                                &mut seen,
                                errors,
                            );
                        }
                    }
                }
            }

            Stmt::Extend(node) => {
                let decl = &node.node;
                if !decl.type_params.is_empty() || !decl.const_params.is_empty() {
                    // generic templates are registered by check_extend_decl, not here
                    continue;
                }
                let resolved_ty = type_checker.resolve_type(&decl.ty);
                if !is_valid_concrete_extend_type(&resolved_ty) {
                    continue;
                }

                for method in &decl.methods {
                    if method.node.params.is_empty() {
                        continue;
                    }
                    let self_param = &method.node.params[0];
                    if self_param.name.0.as_ref() != "self" {
                        continue;
                    }
                    if self_param.ty != Type::Infer {
                        continue;
                    }

                    let module_str = type_checker
                        .ctx
                        .module_path
                        .as_ref()
                        .map(|p| p.join("::"))
                        .unwrap_or_default();
                    let internal_name = backend_names::encode_extend_name(
                        &module_str,
                        &resolved_ty,
                        method.node.name,
                    );

                    let mut params = method.node.params.clone();
                    params[0].ty = resolved_ty.clone();
                    let ret = type_checker.resolve_type(&method.node.ret);

                    let source_module = type_checker.ctx.module_path.clone().unwrap_or_default();
                    let def = ExtendMethodDef {
                        params,
                        ret,
                        internal_name,
                        annotations: AppliedAnnotations::default(),
                    };
                    type_checker
                        .ctx
                        .extend_defs
                        .entry((resolved_ty.clone(), method.node.name))
                        .or_default()
                        .push(ExtendEntry {
                            source_module,
                            binding: Ident(Intern::new(String::new())),
                            def,
                        });
                }

                for cast_from in &decl.cast_froms {
                    let source_ty = type_checker.resolve_type(&cast_from.node.param.ty);
                    let module_str = type_checker
                        .ctx
                        .module_path
                        .as_ref()
                        .map(|p| p.join("::"))
                        .unwrap_or_default();
                    let internal_name =
                        backend_names::encode_cast_name(&module_str, &resolved_ty, &source_ty);
                    let entry = CastEntry {
                        internal_name,
                        source_type: source_ty.clone(),
                        target_type: resolved_ty.clone(),
                    };
                    type_checker
                        .ctx
                        .cast_defs
                        .insert((source_ty, resolved_ty.clone()), entry);
                }
            }
            _ => {}
        }
    }
}

fn collect_pre_imported_types(
    stmts: &[StmtNode],
    resolved_module_defs: &HashMap<Vec<String>, ModuleDef>,
) -> (
    HashMap<Ident, StructDef>,
    HashMap<Ident, EnumDef>,
    HashMap<Ident, ExternTypeDef>,
) {
    let mut structs: HashMap<Ident, StructDef> = HashMap::new();
    let mut enums: HashMap<Ident, EnumDef> = HashMap::new();
    let mut extern_types: HashMap<Ident, ExternTypeDef> = HashMap::new();

    for stmt in stmts {
        let Stmt::Import(node) = &stmt.node else {
            continue;
        };
        let import = &node.node;
        let path_key: Vec<String> = import.path.iter().map(ToString::to_string).collect();
        let Some(source_def) = resolved_module_defs.get(&path_key) else {
            continue;
        };
        match &import.kind {
            ImportKind::Selective(items) => {
                for item in items {
                    let bind_as = item.alias.unwrap_or(item.name);
                    if let Some(def) = source_def.struct_defs.get(&item.name) {
                        structs.entry(bind_as).or_insert_with(|| def.clone());
                    }
                    if let Some(def) = source_def.enum_defs.get(&item.name) {
                        enums.entry(bind_as).or_insert_with(|| def.clone());
                    }
                    if let Some(def) = source_def.extern_types.get(&item.name) {
                        extern_types.entry(bind_as).or_insert_with(|| def.clone());
                    }
                }
            }
            ImportKind::Wildcard => {
                for (name, def) in &source_def.struct_defs {
                    structs.entry(*name).or_insert_with(|| def.clone());
                }
                for (name, def) in &source_def.enum_defs {
                    enums.entry(*name).or_insert_with(|| def.clone());
                }
                for (name, def) in &source_def.extern_types {
                    extern_types.entry(*name).or_insert_with(|| def.clone());
                }
            }
            ImportKind::Module | ImportKind::ModuleAs(_) => {}
        }
    }

    (structs, enums, extern_types)
}

pub(super) fn build_module_def_with_reexports(
    stmts: &[StmtNode],
    type_checker: &TypeChecker,
    module_path: &[String],
) -> (ModuleDef, Vec<Diagnostic>) {
    let mut module_def = ModuleDef {
        source_path: module_path.to_vec(),
        ..ModuleDef::default()
    };
    let mut errors: Vec<Diagnostic> = vec![];

    let mut local_extern_types = HashSet::new();
    for stmt in stmts {
        if let Stmt::ExternType(node) = &stmt.node {
            local_extern_types.insert(node.node.name);
        }
    }

    let (pre_imported_structs, pre_imported_enums, pre_imported_extern_types) =
        collect_pre_imported_types(stmts, &type_checker.resolved_module_defs);

    let resolve = |ty: &Type| -> Type {
        fn pre_resolve(ty: &Type, local: &HashSet<Ident>) -> Type {
            match ty {
                Type::UnresolvedName(name) if local.contains(name) => Type::Extern {
                    name: *name,
                    origin: None,
                },
                Type::Func { params, ret } => Type::Func {
                    params: params
                        .iter()
                        .map(|p| FuncParam::new(pre_resolve(&p.ty, local), p.mutable))
                        .collect(),
                    ret: Box::new(pre_resolve(ret, local)),
                },
                _ => ty.clone(),
            }
        }
        let pre = pre_resolve(ty, &local_extern_types);
        if let Type::UnresolvedName(name) = &pre {
            if let Some(def) = pre_imported_structs.get(name) {
                return def.make_type(*name, vec![]);
            }
            if let Some(def) = pre_imported_enums.get(name) {
                return def.make_type(*name, vec![]);
            }
            if let Some(def) = pre_imported_extern_types.get(name) {
                return def.make_type(*name);
            }
        }
        type_checker.resolve_type_with_module_fallback(&pre)
    };

    let mut reexported_from: HashMap<Ident, String> = HashMap::new();

    for stmt in stmts {
        if let Some(decl) = extract_decl(stmt, &resolve) {
            if let Some(name) = apply_decl_to_summary(decl, &mut module_def, module_path) {
                reexported_from.insert(name, "<local>".to_string());
            }
            continue;
        }

        match &stmt.node {
            Stmt::Import(node) => {
                let import = &node.node;
                if import.visibility != Visibility::Public {
                    continue;
                }

                let path_key: Vec<String> = import.path.iter().map(ToString::to_string).collect();
                let source_label = path_key.last().cloned().unwrap_or_default();

                let Some(source_def) = type_checker.resolved_module_defs.get(&path_key).cloned()
                else {
                    continue;
                };

                match &import.kind {
                    ImportKind::Selective(items) => {
                        for item in items {
                            let bind_as = item.alias.unwrap_or(item.name);
                            if let Some(existing) = reexported_from.get(&bind_as) {
                                push_reexport_collision(
                                    &mut errors,
                                    node.span,
                                    bind_as,
                                    existing,
                                    &source_label,
                                    "use selective re-exports or aliasing to resolve the conflict",
                                );
                            } else {
                                merge_symbol(item.name, bind_as, &source_def, &mut module_def);
                                reexported_from.insert(bind_as, source_label.clone());
                            }
                        }
                    }
                    ImportKind::Wildcard => {
                        for name in source_def.all_public_names().collect::<Vec<_>>() {
                            if let Some(existing) = reexported_from.get(&name) {
                                push_reexport_collision(
                                    &mut errors,
                                    node.span,
                                    name,
                                    existing,
                                    &source_label,
                                    "use selective re-exports or aliasing to resolve the conflict",
                                );
                            } else {
                                merge_symbol(name, name, &source_def, &mut module_def);
                                reexported_from.insert(name, source_label.clone());
                            }
                        }
                    }
                    ImportKind::Module => {
                        let binding = *import.path.last().expect("import path cannot be empty");
                        if let Some(existing) = reexported_from.get(&binding) {
                            push_reexport_collision(
                                &mut errors,
                                node.span,
                                binding,
                                existing,
                                &source_label,
                                "use `pub import X as alias;` to rename the re-exported module",
                            );
                        } else {
                            module_def.re_exported_modules.insert(binding, source_def);
                            reexported_from.insert(binding, source_label.clone());
                        }
                    }
                    ImportKind::ModuleAs(alias) => {
                        if let Some(existing) = reexported_from.get(alias) {
                            push_reexport_collision(
                                &mut errors,
                                node.span,
                                *alias,
                                existing,
                                &source_label,
                                "use a different alias to resolve the conflict",
                            );
                        } else {
                            module_def.re_exported_modules.insert(*alias, source_def);
                            reexported_from.insert(*alias, source_label.clone());
                        }
                    }
                }
            }
            Stmt::Extend(node) => {
                let decl = &node.node;
                if decl.visibility != Visibility::Public {
                    continue;
                }

                if !decl.type_params.is_empty() || !decl.const_params.is_empty() {
                    // build the exported extend target, unresolved names
                    // use the correct concrete type variant while typed targets resolve directly
                    let target_type = if let Type::UnresolvedName(name) = &decl.ty {
                        let type_args: Vec<Type> = decl
                            .type_params
                            .iter()
                            .map(|p| Type::UnresolvedName(p.name))
                            .collect();
                        if type_checker.get_enum(*name).is_some() {
                            Type::Enum {
                                name: *name,
                                type_args,
                                origin: None,
                            }
                        } else if let Some(def) = type_checker.get_struct(*name) {
                            def.kind.make_type(*name, type_args, None)
                        } else {
                            Type::Struct {
                                name: *name,
                                type_args,
                                origin: None,
                            }
                        }
                    } else {
                        resolve(&decl.ty)
                    };
                    let Some(base_key) = extend_base_key(&target_type) else {
                        continue;
                    };
                    for method in &decl.methods {
                        if method.node.params.is_empty() {
                            continue;
                        }
                        if method.node.params[0].name.0.as_ref() != "self" {
                            continue;
                        }
                        module_def
                            .generic_extend_methods
                            .push(ModuleGenericExtendEntry {
                                base_name: base_key,
                                type_params: decl.type_params.clone(),
                                const_params: decl.const_params.clone(),
                                target_type: target_type.clone(),
                                method_name: method.node.name,
                                method: method.clone(),
                            });
                    }
                    continue;
                }

                let resolved_ty = resolve(&decl.ty);
                if !is_valid_concrete_extend_type(&resolved_ty) {
                    continue;
                }
                let module_str = module_path.join("::");
                for method in &decl.methods {
                    if method.node.params.is_empty() {
                        continue;
                    }
                    let self_param = &method.node.params[0];
                    if self_param.name.0.as_ref() != "self" {
                        continue;
                    }
                    let internal_name = backend_names::encode_extend_name(
                        &module_str,
                        &resolved_ty,
                        method.node.name,
                    );
                    let mut params = method.node.params.clone();
                    params[0].ty = resolved_ty.clone();
                    let ret = resolve(&method.node.ret);
                    let annotations = normalize_annotations(
                        &method.node.annotations,
                        AnnotationTarget::ExtendMethod,
                        &mut errors,
                    );
                    let def = ExtendMethodDef {
                        params,
                        ret,
                        internal_name,
                        annotations,
                    };
                    module_def.extend_methods.push(ModuleExtendEntry {
                        ty: resolved_ty.clone(),
                        name: method.node.name,
                        def,
                    });
                }
                for cast_from in &decl.cast_froms {
                    let source_ty = resolve(&cast_from.node.param.ty);
                    let internal_name =
                        backend_names::encode_cast_name(&module_str, &resolved_ty, &source_ty);
                    module_def.cast_entries.push(CastEntry {
                        internal_name,
                        source_type: source_ty,
                        target_type: resolved_ty.clone(),
                    });
                }
            }
            _ => {}
        }
    }

    (module_def, errors)
}

pub(super) fn check_stmt(
    stmt: &StmtNode,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) {
    match &stmt.node {
        Stmt::Import(node) => {
            let import = &node.node;
            let path_key: Vec<String> = import.path.iter().map(ToString::to_string).collect();

            let Some(module_def) = type_checker.resolved_module_defs.get(&path_key) else {
                return;
            };
            let module_def = module_def.clone();
            let module_name = *import.path.last().expect("import path cannot be empty");

            if let ImportKind::Selective(items) = &import.kind {
                for item in items {
                    let is_public = module_def.funcs.contains_key(&item.name)
                        || module_def.struct_defs.contains_key(&item.name)
                        || module_def.enum_defs.contains_key(&item.name)
                        || module_def.extern_types.contains_key(&item.name)
                        || module_def.re_exported_modules.contains_key(&item.name)
                        || module_def.const_defs.contains_key(&item.name);
                    if !is_public {
                        let err_kind = if module_def.all_names.contains(&item.name) {
                            DiagnosticKind::PrivateModuleMember {
                                module: module_name,
                                member: item.name,
                            }
                        } else {
                            DiagnosticKind::UnknownModuleMember {
                                module: module_name,
                                member: item.name,
                            }
                        };
                        errors.push(Diagnostic::new(node.span, err_kind));
                    }
                }
            }
        }
        Stmt::ExternFunc(node) => {
            let annotations =
                normalize_annotations(&node.node.annotations, AnnotationTarget::ExternFunc, errors);
            type_checker
                .func_annotations
                .insert(node.node.name, annotations);
        }
        Stmt::ExternType(node) => {
            let _ =
                normalize_annotations(&node.node.annotations, AnnotationTarget::ExternType, errors);
        }
        Stmt::Func(node) => {
            let annotations =
                normalize_annotations(&node.node.annotations, AnnotationTarget::Func, errors);
            type_checker
                .func_annotations
                .insert(node.node.name, annotations);
            check_func(node, type_checker, errors);
        }
        Stmt::Aggregate(node) => {
            let ann_target = match node.node.kind {
                AggregateKind::DataRef => AnnotationTarget::DataRef,
                AggregateKind::Struct => AnnotationTarget::Struct,
            };
            let annotations = normalize_annotations(&node.node.annotations, ann_target, errors);
            if let Some(sd) = type_checker.ctx.struct_defs.get_mut(&node.node.name) {
                sd.annotations = annotations;
            }
            check_struct(node, type_checker, errors);
        }
        Stmt::Enum(node) => {
            let enum_anns =
                normalize_annotations(&node.node.annotations, AnnotationTarget::Enum, errors);
            let decl = &node.node;

            let mut variant_anns: HashMap<Ident, AppliedAnnotations> = HashMap::new();
            let mut variant_field_anns: HashMap<Ident, HashMap<Ident, AppliedAnnotations>> =
                HashMap::new();

            for variant in &decl.variants {
                let v_ann =
                    normalize_annotations(&variant.annotations, AnnotationTarget::Variant, errors);
                variant_anns.insert(variant.name, v_ann);

                let has_any = match &variant.kind {
                    VariantKind::Unit => false,
                    VariantKind::Tuple(types) => types.iter().any(Type::contains_any),
                    VariantKind::Struct(fields) => fields.iter().any(|f| f.ty.contains_any()),
                };
                if has_any {
                    errors.push(Diagnostic::new(
                        node.span,
                        DiagnosticKind::AnyTypeNotAllowed,
                    ));
                }

                if let VariantKind::Struct(fields) = &variant.kind {
                    let mut field_map = HashMap::new();
                    for field in fields {
                        let f_ann = normalize_annotations(
                            &field.annotations,
                            AnnotationTarget::Field,
                            errors,
                        );
                        field_map.insert(field.name, f_ann);
                    }
                    variant_field_anns.insert(variant.name, field_map);
                }
            }

            if let Some(enum_def) = type_checker.ctx.enum_defs.get_mut(&decl.name) {
                enum_def.annotations = enum_anns;
                for variant_def in &mut enum_def.variants {
                    if let Some(v_ann) = variant_anns.remove(&variant_def.name) {
                        variant_def.annotations = v_ann;
                    }
                    if let Some(f_anns) = variant_field_anns.remove(&variant_def.name) {
                        variant_def.field_annotations = Some(f_anns);
                    }
                }
            }
        }
        Stmt::Expr(node) => {
            let _ = check_expr(node, type_checker, errors, None);
        }
        Stmt::Binding(node) => check_binding(node, type_checker, errors),
        Stmt::LetElse(node) => check_let_else(node, type_checker, errors),
        Stmt::Return(node) => check_ret(node, type_checker, errors),
        Stmt::While(node) => check_while(node, type_checker, errors),
        Stmt::WhileLet(node) => check_while_let(node, type_checker, errors),
        Stmt::For(node) => check_for(node, type_checker, errors),
        Stmt::Break => check_break(stmt.span, type_checker, errors),
        Stmt::Continue => check_continue(stmt.span, type_checker, errors),
        Stmt::Const(decl) => {
            let _ = normalize_annotations(&decl.node.annotations, AnnotationTarget::Const, errors);
            // module-level consts are already processed by evaluate_module_consts,
            // only handle the function-body case here
            if type_checker.return_types.is_empty() {
                return;
            }

            let name = decl.node.name;

            let already_in_scope = type_checker
                .const_scope_stack
                .last()
                .is_some_and(|scope| scope.contains(&name));
            if already_in_scope {
                errors.push(Diagnostic::new(
                    stmt.span,
                    DiagnosticKind::DuplicateConst { name },
                ));
                return;
            }

            if eval_and_insert_const(decl, type_checker, errors)
                && let Some(scope) = type_checker.const_scope_stack.last_mut()
            {
                scope.insert(name);
            }
        }
        Stmt::Extend(node) => check_extend_decl(node, type_checker, errors),
        Stmt::Defer(node) => check_defer(node, type_checker, errors),
    }
}

pub(super) fn extend_base_key(ty: &Type) -> Option<Ident> {
    if let Some(agg) = ty.as_aggregate() {
        return Some(agg.name);
    }
    match ty {
        Type::Enum { name, .. } | Type::Extern { name, .. } => Some(*name),
        Type::List { .. } => Some(*IDENT_LIST),
        Type::Map { .. } => Some(*IDENT_MAP),
        Type::Tuple(_) => Some(*IDENT_TUPLE),
        Type::Array { .. } => Some(*IDENT_ARRAY),
        _ => None,
    }
}

fn collect_undeclared_type_param_names(ty: &Type, declared: &[TypeParam], out: &mut Vec<Ident>) {
    match ty {
        Type::UnresolvedName(name) if !declared.iter().any(|tp| tp.name == *name) => {
            out.push(*name);
        }
        Type::List { elem } | Type::Array { elem, .. } | Type::Slice { elem } => {
            collect_undeclared_type_param_names(elem, declared, out);
        }
        Type::Map { key, value } => {
            collect_undeclared_type_param_names(key, declared, out);
            collect_undeclared_type_param_names(value, declared, out);
        }
        Type::Tuple(elems) => {
            for elem in elems {
                collect_undeclared_type_param_names(elem, declared, out);
            }
        }
        Type::Struct { type_args, .. }
        | Type::DataRef { type_args, .. }
        | Type::Enum { type_args, .. } => {
            for arg in type_args {
                collect_undeclared_type_param_names(arg, declared, out);
            }
        }
        Type::NamedTuple(fields) => {
            for (_, field_ty) in fields {
                collect_undeclared_type_param_names(field_ty, declared, out);
            }
        }
        Type::Func { params, ret } => {
            for p in params {
                collect_undeclared_type_param_names(&p.ty, declared, out);
            }
            collect_undeclared_type_param_names(ret, declared, out);
        }
        _ => {}
    }
}

fn type_contains_const_param(ty: &Type, id: crate::ast::ConstParamId) -> bool {
    match ty {
        Type::Array { elem, len } => {
            matches!(len, ArrayLen::Param(pid) if *pid == id) || type_contains_const_param(elem, id)
        }
        Type::List { elem } | Type::Slice { elem } => type_contains_const_param(elem, id),
        Type::Map { key, value } => {
            type_contains_const_param(key, id) || type_contains_const_param(value, id)
        }
        Type::Tuple(elems) => elems.iter().any(|e| type_contains_const_param(e, id)),
        Type::NamedTuple(fields) => fields.iter().any(|(_, t)| type_contains_const_param(t, id)),
        Type::Struct { type_args, .. }
        | Type::DataRef { type_args, .. }
        | Type::Enum { type_args, .. } => {
            type_args.iter().any(|a| type_contains_const_param(a, id))
        }
        Type::Func { params, ret } => {
            params.iter().any(|p| type_contains_const_param(&p.ty, id))
                || type_contains_const_param(ret, id)
        }
        _ => false,
    }
}

fn is_valid_concrete_extend_type(ty: &Type) -> bool {
    match ty {
        Type::Int
        | Type::Float
        | Type::Double
        | Type::Bool
        | Type::String
        | Type::Extern { .. } => true,
        Type::Struct { type_args, .. }
        | Type::DataRef { type_args, .. }
        | Type::Enum { type_args, .. } => type_args.iter().all(is_valid_concrete_extend_type),
        Type::List { elem } | Type::Array { elem, .. } | Type::Slice { elem } => {
            is_valid_concrete_extend_type(elem)
        }
        Type::Map { key, value } => {
            is_valid_concrete_extend_type(key) && is_valid_concrete_extend_type(value)
        }
        Type::Tuple(fields) => fields.iter().all(is_valid_concrete_extend_type),
        Type::NamedTuple(fields) => fields.iter().all(|(_, t)| is_valid_concrete_extend_type(t)),
        _ => false,
    }
}

fn check_extend_decl(
    node: &crate::ast::ExtendDeclNode,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) {
    let decl = &node.node;

    if !decl.type_params.is_empty() || !decl.const_params.is_empty() {
        check_generic_extend_decl(node, type_checker, errors);
        return;
    }

    let resolved_ty = type_checker.resolve_type(&decl.ty);

    if !is_valid_concrete_extend_type(&resolved_ty) {
        errors.push(Diagnostic::new(
            node.span,
            DiagnosticKind::ExtendUnsupportedType { ty: resolved_ty },
        ));
        return;
    }

    for method in &decl.methods {
        // Validate self param exists
        if method.node.params.is_empty() || method.node.params[0].name.0.as_ref() != "self" {
            errors.push(Diagnostic::new(
                method.span,
                DiagnosticKind::ExtendMethodMissingSelf {
                    method: method.node.name,
                },
            ));
            continue;
        }

        // Validate no type annotation on self (ty must be Infer)
        if method.node.params[0].ty != Type::Infer {
            errors.push(Diagnostic::new(
                method.span,
                DiagnosticKind::ExtendSelfTypeAnnotation {
                    method: method.node.name,
                },
            ));
            continue;
        }

        // Native conflict check
        let has_native_conflict = if let Some(agg) = resolved_ty.as_aggregate() {
            type_checker
                .get_struct(agg.name)
                .is_some_and(|def| def.methods.contains_key(&method.node.name))
        } else if let Type::Extern { name, .. } = &resolved_ty {
            type_checker
                .get_extern_type(*name)
                .is_some_and(|def| def.methods.contains_key(&method.node.name))
        } else {
            false
        };
        if has_native_conflict {
            errors.push(Diagnostic::new(
                method.span,
                DiagnosticKind::ExtendMethodConflict {
                    ty: resolved_ty.clone(),
                    method: method.node.name,
                },
            ));
            continue;
        }

        // Duplicate check: count entries from current module (source_module == []) for this key
        let key = (resolved_ty.clone(), method.node.name);
        let local_count = type_checker.ctx.extend_defs.get(&key).map_or(0, |entries| {
            entries
                .iter()
                .filter(|e| e.source_module.is_empty())
                .count()
        });
        if local_count > 1 {
            errors.push(Diagnostic::new(
                method.span,
                DiagnosticKind::DuplicateExtendMethod {
                    ty: resolved_ty.clone(),
                    method: method.node.name,
                },
            ));
            // Don't skip, still typecheck the body
        }

        // Normalize annotations and update the pre registered entry
        let annotations = normalize_annotations(
            &method.node.annotations,
            AnnotationTarget::ExtendMethod,
            errors,
        );
        let key = (resolved_ty.clone(), method.node.name);
        if let Some(entry) = type_checker
            .ctx
            .extend_defs
            .get_mut(&key)
            .and_then(|entries| entries.iter_mut().find(|e| e.source_module.is_empty()))
        {
            entry.def.annotations = annotations;
        }

        // Build param list for body checking
        let is_var_self = matches!(method.node.params[0].mutability, Mutability::Mutable);
        let mut params: Vec<(Ident, Type, bool)> =
            vec![(method.node.params[0].name, resolved_ty.clone(), is_var_self)];
        for p in &method.node.params[1..] {
            params.push((
                p.name,
                type_checker.resolve_type(&p.ty),
                matches!(p.mutability, Mutability::Mutable),
            ));
        }

        let ret_ty = type_checker.resolve_type(&method.node.ret);
        check_body_common(
            &params,
            &method.node.body,
            &ret_ty,
            method.span,
            type_checker,
            errors,
        );
    }

    for cast_from in &decl.cast_froms {
        let cf = &cast_from.node;
        let source_ty = type_checker.resolve_type(&cf.param.ty);

        // check self conversion because is pointless
        if source_ty == resolved_ty {
            errors.push(Diagnostic::new(
                cast_from.span,
                DiagnosticKind::CastFromSelfConversion {
                    ty: resolved_ty.clone(),
                },
            ));
            continue;
        }

        // if return type does not match the extend target type raise an error
        if let Some(ret_ty) = &cf.ret {
            let resolved_ret = type_checker.resolve_type(ret_ty);
            if resolved_ret != resolved_ty {
                errors.push(Diagnostic::new(
                    cast_from.span,
                    DiagnosticKind::CastFromReturnTypeMismatch {
                        expected: resolved_ty.clone(),
                        found: resolved_ret,
                    },
                ));
                continue;
            }
        }

        // if there is a duplicate cast_from with the same source type raise an error
        let earlier = decl
            .cast_froms
            .iter()
            .take_while(|c| !std::ptr::eq(*c, cast_from))
            .find(|c| type_checker.resolve_type(&c.node.param.ty) == source_ty);
        if let Some(first) = earlier {
            errors.push(
                Diagnostic::new(
                    cast_from.span,
                    DiagnosticKind::DuplicateCastFrom {
                        source_ty: source_ty.clone(),
                        target_ty: resolved_ty.clone(),
                    },
                )
                .with_secondary(first.span, "first defined here"),
            );
        }

        let is_var = matches!(cf.param.mutability, Mutability::Mutable);
        let params = vec![(cf.param.name, source_ty, is_var)];
        check_body_common(
            &params,
            &cf.body,
            &resolved_ty,
            cast_from.span,
            type_checker,
            errors,
        );
    }
}

fn resolve_generic_extend_named_type(
    type_checker: &TypeChecker,
    ty_name: Ident,
    name: Ident,
    type_args: Vec<Type>,
    declared_param_count: usize,
    span: Span,
    errors: &mut Vec<Diagnostic>,
) -> Option<Type> {
    // none means Enum while Some(kind) means Struct or DataRef
    let (def_tp, agg_kind): (usize, Option<AggregateKind>) =
        if let Some(def) = type_checker.get_enum(name) {
            (def.type_params.len(), None)
        } else if let Some(def) = type_checker.get_struct(name) {
            (def.type_params.len(), Some(def.kind))
        } else {
            return None;
        };

    if def_tp == 0 {
        errors.push(Diagnostic::new(
            span,
            DiagnosticKind::ExtendTypeParamsOnNonGeneric { ty_name },
        ));
        return None;
    }
    if declared_param_count != def_tp {
        errors.push(Diagnostic::new(
            span,
            DiagnosticKind::ExtendTypeParamCountMismatch {
                ty_name,
                expected: def_tp,
                found: declared_param_count,
            },
        ));
        return None;
    }
    Some(match agg_kind {
        None => Type::Enum {
            name,
            type_args,
            origin: None,
        },
        Some(kind) => kind.make_type(name, type_args, None),
    })
}

fn check_generic_extend_decl(
    node: &crate::ast::ExtendDeclNode,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) {
    let decl = &node.node;

    // put const params in scope
    let const_param_count = decl.const_params.len();
    type_checker.push_const_params(&decl.const_params);

    // build the full extend target type, constructing legacy name-based forms manually and resolving typed forms directly
    let target_type = if let Type::UnresolvedName(base_name) = &decl.ty {
        let resolved = type_checker.resolve_type(&decl.ty);
        let type_args: Vec<Type> = decl
            .type_params
            .iter()
            .map(|p| Type::UnresolvedName(p.name))
            .collect();
        let target_name = if let Some(agg) = resolved.as_aggregate() {
            agg.name
        } else if let Type::Enum { name, .. } = &resolved {
            *name
        } else {
            errors.push(Diagnostic::new(
                node.span,
                DiagnosticKind::ExtendUnsupportedType { ty: resolved },
            ));
            type_checker.pop_const_params(const_param_count);
            return;
        };
        let Some(ty) = resolve_generic_extend_named_type(
            type_checker,
            *base_name,
            target_name,
            type_args,
            decl.type_params.len(),
            node.span,
            errors,
        ) else {
            type_checker.pop_const_params(const_param_count);
            return;
        };
        ty
    } else {
        // new path: "extend<T, N: int> type_expr" — resolve concrete parts;
        // type params stay as UnresolvedName, const params are converted to ArrayLen::Param
        type_checker.resolve_type(&decl.ty)
    };

    // extract the base key for hashmap lookup
    let Some(base_key) = extend_base_key(&target_type) else {
        errors.push(Diagnostic::new(
            node.span,
            DiagnosticKind::ExtendUnsupportedType { ty: target_type },
        ));
        type_checker.pop_const_params(const_param_count);
        return;
    };

    // reject unresolved names in target type unless they are declared type params
    let mut undeclared = vec![];
    collect_undeclared_type_param_names(&target_type, &decl.type_params, &mut undeclared);
    for name in undeclared {
        errors.push(Diagnostic::new(
            node.span,
            DiagnosticKind::ExtendUndeclaredTypeParam { name },
        ));
    }

    // validate all declared type params appear somewhere in target_type
    for param in &decl.type_params {
        let used = type_any(
            &target_type,
            &mut |t| matches!(t, Type::UnresolvedName(n) if *n == param.name),
        );
        if !used {
            errors.push(Diagnostic::new(
                node.span,
                DiagnosticKind::ExtendUnusedTypeParam {
                    param_name: param.name,
                },
            ));
            type_checker.pop_const_params(const_param_count);
            return;
        }
    }

    // validate all declared const params appear somewhere in target_type
    for param in &decl.const_params {
        let used = type_contains_const_param(&target_type, param.id);
        if !used {
            errors.push(Diagnostic::new(
                node.span,
                DiagnosticKind::ExtendUnusedTypeParam {
                    param_name: param.name,
                },
            ));
            type_checker.pop_const_params(const_param_count);
            return;
        }
    }

    for method in &decl.methods {
        // validate self param exists
        if method.node.params.is_empty() || method.node.params[0].name.0.as_ref() != "self" {
            errors.push(Diagnostic::new(
                method.span,
                DiagnosticKind::ExtendMethodMissingSelf {
                    method: method.node.name,
                },
            ));
            continue;
        }

        // validate no type annotation on self
        if method.node.params[0].ty != Type::Infer {
            errors.push(Diagnostic::new(
                method.span,
                DiagnosticKind::ExtendSelfTypeAnnotation {
                    method: method.node.name,
                },
            ));
            continue;
        }

        // native conflict check (only for named struct/dataref types)
        let has_native_conflict = target_type
            .as_aggregate()
            .and_then(|agg| type_checker.get_struct(agg.name))
            .is_some_and(|def| def.methods.contains_key(&method.node.name));
        if has_native_conflict {
            errors.push(Diagnostic::new(
                method.span,
                DiagnosticKind::ExtendMethodConflict {
                    ty: target_type.clone(),
                    method: method.node.name,
                },
            ));
            continue;
        }

        // duplicate check, reject only if there's already a local template with the same pattern
        let key = (base_key, method.node.name);
        let has_same_pattern = type_checker
            .ctx
            .generic_extend_templates
            .get(&key)
            .is_some_and(|entries| {
                entries
                    .iter()
                    .any(|e| e.source_module.is_empty() && e.target_type == target_type)
            });
        if has_same_pattern {
            errors.push(Diagnostic::new(
                method.span,
                DiagnosticKind::DuplicateExtendMethod {
                    ty: target_type.clone(),
                    method: method.node.name,
                },
            ));
            continue;
        }

        // validate annotations eagerly for error reporting
        normalize_annotations(
            &method.node.annotations,
            AnnotationTarget::ExtendMethod,
            errors,
        );

        // store template, body is NOT typechecked here (lazy specialization)
        type_checker
            .ctx
            .generic_extend_templates
            .entry(key)
            .or_default()
            .push(GenericExtendTemplate {
                type_params: decl.type_params.clone(),
                const_params: decl.const_params.clone(),
                target_type: target_type.clone(),
                method: method.clone(),
                source_module: vec![],
                binding: Ident(Intern::new(String::new())),
            });
    }

    type_checker.pop_const_params(const_param_count);
}

pub(super) fn check_binding(
    binding: &crate::ast::BindingNode,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) {
    let node = &binding.node;
    if let Some(annot_ty) = &node.ty
        && annot_ty.contains_any()
    {
        errors.push(Diagnostic::new(
            binding.span,
            DiagnosticKind::AnyTypeNotAllowed,
        ));
    }
    let expected = node
        .ty
        .as_ref()
        .map(|annot_ty| type_checker.resolve_type(annot_ty));
    check_expr(&node.value, type_checker, errors, expected.as_ref());

    // preserve lambda "as" param auto-casts when the lambda is bound to a named variable
    if matches!(node.value.node.kind, ExprKind::Lambda(_)) {
        let lambda_id = node.value.node.id;
        if let Some(cast_accepts) = type_checker.lambda_cast_accepts.get(&lambda_id).cloned()
            && let crate::ast::Pattern::Ident(name) = node.pattern.node
        {
            type_checker.func_cast_accept.insert(name, cast_accepts);
        }
    }

    if is_if_without_else(&node.value) {
        errors.push(
            Diagnostic::new(node.value.span, DiagnosticKind::IfMissingElse).with_help(
                "add an `else` branch, or end the `if` with `;` if you meant a statement",
            ),
        );
    }

    let val_ref = TypeRef::Expr(node.value.node.id);

    let value_ty = type_checker
        .get_type(node.value.node.id)
        .map_or(Type::Infer, |(_, ty)| ty.clone());

    let binding_ty = if let Some(annot_ty) = &node.ty {
        let resolved_annot = resolve_array_infer_annotation(annot_ty, &value_ty);
        let resolved_annot = type_checker.resolve_type(&resolved_annot);

        // validate map type annotation are keyable
        if let Type::Map { key, .. } = &resolved_annot {
            validate_map_key_type(binding.span, key, type_checker, errors);
        }

        let should_retag_literal = is_array_literal_with_infer_elem(&node.value, &value_ty)
            || is_array_lit_with_list_annotation(&node.value, &resolved_annot);

        if should_retag_literal {
            update_array_literal_typ(&node.value, &resolved_annot, type_checker);
        }

        let annot_ref = TypeRef::concrete(&resolved_annot);
        type_checker.constrain_assignable(binding.span, val_ref, annot_ref, errors);

        resolved_annot
    } else {
        if is_all_nil_array_literal(&node.value) {
            errors.push(
                Diagnostic::new(node.value.span, DiagnosticKind::ArrayAllNilAmbiguous)
                    .with_help("add a type annotation, e.g. `let a: [int?; _] = [nil, nil];`"),
            );
        }
        if is_empty_map_literal(&node.value) {
            errors.push(Diagnostic::new(
                node.value.span,
                DiagnosticKind::MapEmptyLiteralNoContext,
            ));
        }
        value_ty
    };

    let mutable = matches!(node.mutability, Mutability::Mutable);
    type_checker.set_binding_type(node.value.node.id, binding_ty.clone());
    check_pattern(&node.pattern, &binding_ty, mutable, type_checker, errors);
}

pub(super) fn check_let_else(
    let_else_node: &crate::ast::LetElseNode,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) {
    let node = &let_else_node.node;

    let value_ty = check_expr(&node.value, type_checker, errors, None);

    if !is_refutable(&node.pattern.node, &value_ty, type_checker) {
        errors.push(Diagnostic::new(
            node.pattern.span,
            DiagnosticKind::LetElseIrrefutable,
        ));
        check_pattern(&node.pattern, &value_ty, false, type_checker, errors);
        return;
    }

    type_checker.push_scope();
    check_block_stmts(
        &node.else_block.node.stmts,
        node.else_block.node.tail.as_deref(),
        type_checker,
        errors,
        None,
    );
    type_checker.pop_scope();

    if !block_always_diverges(&node.else_block) {
        errors.push(Diagnostic::new(
            node.else_block.span,
            DiagnosticKind::LetElseMustDiverge,
        ));
    }

    check_pattern(&node.pattern, &value_ty, false, type_checker, errors);
}

fn is_array_literal_with_infer_elem(expr: &ExprNode, value_ty: &Type) -> bool {
    let is_array_literal = matches!(
        &expr.node.kind,
        ExprKind::ArrayLiteral(_) | ExprKind::ArrayFill(_)
    );
    let has_infer_elem = match unwrap_opt_typ(value_ty) {
        Type::Array { elem, .. } => contains_infer(elem),
        _ => false,
    };
    is_array_literal && has_infer_elem
}

fn is_array_lit_with_list_annotation(expr: &ExprNode, annot_ty: &Type) -> bool {
    let is_array_literal = matches!(
        &expr.node.kind,
        ExprKind::ArrayLiteral(_) | ExprKind::ArrayFill(_)
    );
    let is_list = unwrap_opt_typ(annot_ty).is_list();
    is_array_literal && is_list
}

fn update_array_literal_typ(expr: &ExprNode, annot_ty: &Type, type_checker: &mut TypeChecker) {
    let base_annot = unwrap_opt_typ(annot_ty);

    match &expr.node.kind {
        ExprKind::ArrayLiteral(lit) => match base_annot {
            Type::Array { elem, len } => {
                let new_ty = Type::Array {
                    elem: elem.clone(),
                    len: *len,
                };
                type_checker.set_type(expr.node.id, new_ty, expr.span);
                for el in &lit.node.elements {
                    type_checker.set_type(el.node.id, *elem.clone(), el.span);
                }
            }
            Type::List { elem } => {
                let new_ty = Type::List { elem: elem.clone() };
                type_checker.set_type(expr.node.id, new_ty, expr.span);
                for el in &lit.node.elements {
                    type_checker.set_type(el.node.id, *elem.clone(), el.span);
                }
            }
            _ => {}
        },
        ExprKind::ArrayFill(fill) => match base_annot {
            Type::Array { elem, len } => {
                let new_ty = Type::Array {
                    elem: elem.clone(),
                    len: *len,
                };
                type_checker.set_type(expr.node.id, new_ty, expr.span);
                type_checker.set_type(fill.node.value.node.id, *elem.clone(), fill.node.value.span);
            }
            Type::List { elem } => {
                let new_ty = Type::List { elem: elem.clone() };
                type_checker.set_type(expr.node.id, new_ty, expr.span);
                type_checker.set_type(fill.node.value.node.id, *elem.clone(), fill.node.value.span);
            }
            _ => {}
        },
        _ => {}
    }
}

fn resolve_array_infer_annotation(annot_ty: &Type, value_ty: &Type) -> Type {
    match annot_ty {
        Type::Array {
            elem,
            len: ArrayLen::Infer,
        } => {
            let inferred_len = match value_ty {
                Type::Array {
                    len: ArrayLen::Fixed(n),
                    ..
                } => ArrayLen::Fixed(*n),
                _ => ArrayLen::Infer,
            };
            Type::Array {
                elem: elem.clone(),
                len: inferred_len,
            }
        }
        Type::Array { elem, len } => {
            let resolved_elem = match value_ty {
                Type::Array {
                    elem: value_elem, ..
                } if value_elem.is_infer() => elem.clone(),
                _ => elem.clone(),
            };
            Type::Array {
                elem: resolved_elem,
                len: *len,
            }
        }
        Type::List { elem } => {
            let resolved_elem = match value_ty {
                Type::Array {
                    elem: value_elem, ..
                } if value_elem.is_infer() => elem.clone(),
                _ => elem.clone(),
            };
            Type::List {
                elem: resolved_elem,
            }
        }
        annot_ty if annot_ty.is_option() => {
            let inner = annot_ty.option_inner().expect("is_option guarantees inner");
            Type::option_of(resolve_array_infer_annotation(inner, value_ty))
        }
        _ => annot_ty.clone(),
    }
}

pub(super) fn check_ret(
    ret: &ReturnNode,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) {
    if type_checker.in_defer {
        errors.push(Diagnostic::new(ret.span, DiagnosticKind::ReturnInDefer));
        return;
    }

    let node = &ret.node;

    // if return is outside a function then we just return (although this shouldn't happen)
    let Some(expected_ret) = type_checker.current_return_type().cloned() else {
        return;
    };

    type_checker.mark_explicit_return();

    match (&node.value, &expected_ret) {
        // returning a value in a non-void fn needs constraining
        (Some(value_expr), expected_ty) => {
            check_expr(value_expr, type_checker, errors, Some(expected_ty));
            let expr_ref = TypeRef::Expr(value_expr.node.id);
            let ret_ref = TypeRef::concrete(expected_ty);
            type_checker.constrain_assignable(ret.span, expr_ref, ret_ref, errors);
        }

        // returning nothing in a void fn is fine
        (None, Type::Void) => {}

        // returning nothing in a non-void fn is invalid
        (None, expected_ty) => {
            errors.push(Diagnostic::new(
                ret.span,
                DiagnosticKind::MismatchedTypes {
                    expected: expected_ty.clone(),
                    found: Type::Void,
                },
            ));
        }
    }
}

fn check_break(span: Span, type_checker: &mut TypeChecker, errors: &mut Vec<Diagnostic>) {
    if type_checker.in_defer {
        errors.push(Diagnostic::new(span, DiagnosticKind::BreakInDefer));
        return;
    }
    if !type_checker.in_loop() {
        errors.push(
            Diagnostic::new(span, DiagnosticKind::BreakOutsideLoop)
                .with_help("break can only appear inside `while` or `for` loops"),
        );
    }
}

fn check_continue(span: Span, type_checker: &mut TypeChecker, errors: &mut Vec<Diagnostic>) {
    if type_checker.in_defer {
        errors.push(Diagnostic::new(span, DiagnosticKind::ContinueInDefer));
        return;
    }
    if !type_checker.in_loop() {
        errors.push(
            Diagnostic::new(span, DiagnosticKind::ContinueOutsideLoop)
                .with_help("continue can only appear inside `while` or `for` loops"),
        );
    }
}

fn check_defer(
    defer_node: &crate::ast::DeferNode,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<Diagnostic>,
) {
    let prev_in_defer = type_checker.in_defer;
    type_checker.in_defer = true;

    match &defer_node.node.body {
        DeferBody::Expr(expr) => {
            check_expr(expr, type_checker, errors, None);
        }
        DeferBody::Block(block) => {
            check_block_stmts(
                &block.node.stmts,
                block.node.tail.as_deref(),
                type_checker,
                errors,
                None,
            );
        }
    }

    type_checker.in_defer = prev_in_defer;
}
