use std::collections::HashSet;

use super::{
    CheckedType, TypeChecker, TypeError, VariantShape,
    annotation::DeprecatedUseKind,
    check_default_stringify_conversion, check_expected_value_expr, check_expr_checked,
    check_unprojected_expected, check_value_expr_checked_with_hint, checked_from_type, const_eval,
    const_term::ConstTerm,
    decls::{
        CoreRangeKind, FieldSchema, ModuleMemberLookup, NamedSchemas, NominalKey, TypeBinding,
        nominal_generic_args, nominal_type, nominal_type_with_args,
    },
    enum_variant, expected_assignable_type, extern_boundary, field_check,
    generic::{ArityError, GenericArgs, GenericParams},
    generic_bind::bind_exact_generic_args,
    infer::{GenericSolverSeeds, GenericSolverVars, TypeHandle},
    solve_and_checked_from_handle,
    type_ops::type_depends_on_generics,
    type_refs::map_key_type_error,
};
use crate::{
    ast::{
        ArrayFillNode, ArrayLen, ArrayLiteralNode, ConstArg, ExprId, ExprKind, ExprNode,
        FormatKind, FormatSign, FormatSpec, GenericArg, Ident, InferredEnumArgs, InferredEnumNode,
        Lit, MapLiteralNode, NominalKind, Range, RangeNode, StringPart, StructLiteralNode, Type,
    },
    span::Span,
};

#[derive(Clone, Copy, PartialEq, Eq)]
enum CollectionLiteralKind {
    Array,
    List,
}

fn expected_collection(
    expected: Option<&TypeHandle>,
    tc: &TypeChecker,
) -> Option<(TypeHandle, CollectionLiteralKind)> {
    match expected_assignable_type(expected, tc)? {
        Type::Array { elem, .. } | Type::Slice { elem } => {
            Some((tc.type_handle(&elem), CollectionLiteralKind::Array))
        }
        Type::List { elem } => Some((tc.type_handle(&elem), CollectionLiteralKind::List)),
        _ => None,
    }
}

fn expected_map(
    expected: Option<&TypeHandle>,
    tc: &TypeChecker,
) -> Option<(TypeHandle, TypeHandle)> {
    let Type::Map { key, value } = expected_assignable_type(expected, tc)? else {
        return None;
    };
    Some((tc.type_handle(&key), tc.type_handle(&value)))
}

fn collection_literal_handle(
    kind: CollectionLiteralKind,
    elem: TypeHandle,
    len: ArrayLen,
    tc: &mut TypeChecker,
) -> TypeHandle {
    match kind {
        CollectionLiteralKind::Array => tc.array_handle(&elem, &len),
        CollectionLiteralKind::List => tc.list_handle(&elem),
    }
}

fn contains_nil(elements: &[ExprNode]) -> bool {
    elements
        .iter()
        .any(|element| matches!(element.node.kind, ExprKind::Lit(Lit::Nil)))
}

fn option_elem_handle(elem: TypeHandle, span: Span, tc: &mut TypeChecker) -> TypeHandle {
    let ty = tc.handle_type(&elem);
    if tc.decls.semantic_option_inner(&ty).is_some() {
        return elem;
    }
    let option_ty = tc.core_option_or_infer(ty, span);
    tc.type_handle(&option_ty)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum LiteralMapKey {
    Int(i64),
    Bool(bool),
    String(String),
    Tuple(Vec<LiteralMapKey>),
}

fn literal_map_key(expr: &ExprNode) -> Option<LiteralMapKey> {
    match &expr.node.kind {
        ExprKind::Lit(Lit::Int(value)) => Some(LiteralMapKey::Int(*value)),
        ExprKind::Lit(Lit::Bool(value)) => Some(LiteralMapKey::Bool(*value)),
        ExprKind::Lit(Lit::String(value)) => Some(LiteralMapKey::String(value.clone())),
        ExprKind::Tuple(elems) => elems
            .iter()
            .map(literal_map_key)
            .collect::<Option<Vec<_>>>()
            .map(LiteralMapKey::Tuple),
        _ => None,
    }
}

fn check_duplicate_map_keys(lit: &MapLiteralNode, tc: &mut TypeChecker) {
    let mut keys = HashSet::new();
    for (key, _) in &lit.node.entries {
        let Some(key_value) = literal_map_key(key) else {
            continue;
        };
        if !keys.insert(key_value) {
            tc.push_error(TypeError::DuplicateMapKey {
                span: tc.error_span(key.span),
            });
        }
    }
}

pub(super) fn check_map_lit_hint(
    expr: &ExprNode,
    lit: &MapLiteralNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    check_duplicate_map_keys(lit, tc);
    let (key, value, has_hint) = match expected_map(expected.as_ref(), tc) {
        Some((key, value)) => (key, value, true),
        None => (
            tc.fresh_temp_handle(lit.span),
            tc.fresh_temp_handle(lit.span),
            false,
        ),
    };

    if lit.node.entries.is_empty() && !has_hint {
        tc.push_error(TypeError::CannotInferType {
            span: tc.error_span(lit.span),
        });
        return checked_from_type(expr, Type::Infer, tc);
    }

    let mut contains_extern_any = false;
    let mut contains_poison = false;
    for (key_expr, value_expr) in &lit.node.entries {
        let key_checked = check_expected_value_expr(key_expr, key.clone(), tc);
        tc.record_aggregate_elem_flow(expr.node.id, key_expr);
        let value_checked = check_expected_value_expr(value_expr, value.clone(), tc);
        tc.record_aggregate_elem_flow(expr.node.id, value_expr);
        contains_extern_any |= key_checked.contains_extern_any || value_checked.contains_extern_any;
        contains_poison |=
            tc.checked_is_poison(&key_checked) || tc.checked_is_poison(&value_checked);
    }
    if contains_poison {
        return checked_from_type(expr, Type::Infer, tc);
    }

    let map = tc.map_handle(&key, &value);
    let mut checked = solve_and_checked_from_handle(expr, map, tc);
    if !has_hint
        && let Type::Map { key, .. } = &checked.ty
        && let Some(err) = map_key_type_error(&tc.decls, key, tc.error_span(lit.span))
    {
        tc.push_error(err);
    }
    checked.contains_extern_any = contains_extern_any;
    checked
}

pub(super) fn check_array_lit_hint(
    expr: &ExprNode,
    lit: &ArrayLiteralNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let expected = expected.as_ref();
    let expected_collection = expected_collection(expected, tc);
    let has_nil = contains_nil(&lit.node.elements);
    if expected_collection.is_none()
        && !lit.node.elements.is_empty()
        && lit
            .node
            .elements
            .iter()
            .all(|element| matches!(element.node.kind, ExprKind::Lit(Lit::Nil)))
    {
        tc.push_error(TypeError::AllNilArrayLiteral {
            span: tc.error_span(lit.span),
        });
    }
    let (elem, kind) = expected_collection
        .unwrap_or_else(|| (tc.fresh_temp_handle(lit.span), CollectionLiteralKind::Array));
    let elem = if has_nil {
        option_elem_handle(elem, lit.span, tc)
    } else {
        elem
    };
    let array = collection_literal_handle(
        kind,
        elem.clone(),
        ArrayLen::Fixed(lit.node.elements.len()),
        tc,
    );
    let mut contains_extern_any = false;
    let mut contains_poison = false;
    for value in &lit.node.elements {
        let checked = check_expected_value_expr(value, elem.clone(), tc);
        tc.record_aggregate_elem_flow(expr.node.id, value);
        contains_extern_any |= checked.contains_extern_any;
        contains_poison |= tc.checked_is_poison(&checked);
    }
    if contains_poison {
        return checked_from_type(expr, Type::Infer, tc);
    }
    let mut checked = solve_and_checked_from_handle(expr, array, tc);
    checked.contains_extern_any = contains_extern_any;
    checked
}

pub(super) fn check_array_fill_hint(
    expr: &ExprNode,
    fill: &ArrayFillNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let len = match tc.eval_const_expr(&fill.node.len, true) {
        Ok(const_value) => {
            match const_eval::const_usize(&const_value, tc.error_span(fill.node.len.span)) {
                Ok(len) => ArrayLen::Fixed(len),
                Err(err) => {
                    tc.push_error(err);
                    ArrayLen::Infer
                }
            }
        }
        Err(TypeError::NonConstExpression { .. }) => {
            tc.push_error(TypeError::ArrayFillLengthNotConst {
                span: tc.error_span(fill.node.len.span),
            });
            ArrayLen::Infer
        }
        Err(err) => {
            tc.push_error(err);
            ArrayLen::Infer
        }
    };
    let (elem, kind) = expected_collection(expected.as_ref(), tc).unwrap_or_else(|| {
        (
            tc.fresh_temp_handle(fill.node.value.span),
            CollectionLiteralKind::Array,
        )
    });
    let value = check_expected_value_expr(&fill.node.value, elem.clone(), tc);
    tc.record_aggregate_elem_flow(expr.node.id, &fill.node.value);
    let array = collection_literal_handle(kind, elem, len, tc);
    let mut checked = solve_and_checked_from_handle(expr, array, tc);
    checked.contains_extern_any = value.contains_extern_any;
    checked
}

fn tuple_hints(
    elems: &[ExprNode],
    expected: Option<&TypeHandle>,
    tc: &mut TypeChecker,
) -> Vec<TypeHandle> {
    match expected_assignable_type(expected, tc) {
        Some(Type::Tuple(types)) if types.len() == elems.len() => {
            types.iter().map(|ty| tc.type_handle(ty)).collect()
        }
        _ => elems
            .iter()
            .map(|elem| tc.fresh_temp_handle(elem.span))
            .collect(),
    }
}

pub(super) fn check_tuple_checked_with_hint(
    expr: &ExprNode,
    elems: &[ExprNode],
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let hints = tuple_hints(elems, expected.as_ref(), tc);
    let mut contains_extern_any = false;
    let mut contains_poison = false;
    for (elem, hint) in elems.iter().zip(&hints) {
        let checked = check_expected_value_expr(elem, hint.clone(), tc);
        tc.record_aggregate_elem_flow(expr.node.id, elem);
        contains_extern_any |= checked.contains_extern_any;
        contains_poison |= tc.checked_is_poison(&checked);
    }
    if contains_poison {
        return checked_from_type(expr, Type::Infer, tc);
    }
    let tuple = tc.tuple_handle(hints);
    let mut checked = solve_and_checked_from_handle(expr, tuple, tc);
    checked.contains_extern_any = contains_extern_any;
    checked
}

struct NominalLiteralSolver {
    vars: GenericSolverVars,
}

impl NominalLiteralSolver {
    fn new(
        generics: &GenericParams,
        args: &[GenericArg],
        span: Span,
        tc: &mut TypeChecker,
    ) -> Option<Self> {
        let seeds = if args.is_empty() {
            GenericSolverSeeds::default()
        } else {
            let args = bind_exact_generic_args(tc, generics, args, span)?;
            GenericSolverSeeds::from_args(generics, &args)
        };
        Some(Self::from_seeds(generics, &seeds, span, tc))
    }

    fn without_args(generics: &GenericParams, span: Span, tc: &mut TypeChecker) -> Self {
        Self::from_seeds(generics, &GenericSolverSeeds::default(), span, tc)
    }

    fn from_seeds(
        generics: &GenericParams,
        seeds: &GenericSolverSeeds,
        span: Span,
        tc: &mut TypeChecker,
    ) -> Self {
        Self {
            vars: tc
                .solver
                .generic_solver_vars(generics, seeds, tc.error_span(span)),
        }
    }

    fn bind_expected(
        &self,
        key: &NominalKey,
        generics: &GenericParams,
        expected: Option<&Type>,
        span: Span,
        tc: &mut TypeChecker,
    ) -> bool {
        let Some(expected) = expected else {
            return true;
        };
        if tc.decls.key_for_type(expected).as_ref() != Some(key) {
            return true;
        }
        let template = nominal_literal_type(key, generics, None);
        let template = tc.solver.instantiate_generic_type(&template, &self.vars);
        let expected = tc.type_handle(expected);
        tc.expect_equal(span, template, expected);
        !tc.solve_constraints()
    }

    fn instantiate(&self, ty: &Type, tc: &mut TypeChecker) -> TypeHandle {
        tc.solver.instantiate_generic_type(ty, &self.vars)
    }

    fn finalize(
        &self,
        key: &NominalKey,
        generics: &GenericParams,
        span: Span,
        tc: &mut TypeChecker,
    ) -> Option<Type> {
        let args = match tc.solver.finalize_generic_args(generics, &self.vars) {
            Ok(args) => args,
            Err(unbound) => {
                tc.push_unbound_generic_errors(unbound, span);
                return None;
            }
        };
        if !tc.check_generic_bounds(generics, &args, span) {
            return None;
        }
        Some(nominal_literal_type(key, generics, Some(&args)))
    }
}

pub(super) fn check_struct_lit_hint(
    expr: &ExprNode,
    lit: &StructLiteralNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    if let Some(checked) = check_enum_struct_variant_lit(expr, lit, expected.clone(), tc) {
        return checked;
    }

    let Some(target) = resolve_struct_target(lit, tc) else {
        check_unknown_nominal_fields(&lit.node.fields, tc);
        return checked_from_type(expr, Type::Infer, tc);
    };
    let key = target.key.clone();

    if key.kind == NominalKind::Extern {
        return extern_boundary::check_extern_lit(expr, lit, &key, expected.as_ref(), tc);
    }

    let valid_literal_target = matches!(key.kind, NominalKind::Struct | NominalKind::DataRef);
    if !valid_literal_target {
        let kind = match key.kind {
            NominalKind::Enum => "enum",
            NominalKind::Extern => "extern",
            NominalKind::Struct | NominalKind::DataRef => unreachable!(),
        };
        tc.push_error(TypeError::InvalidStructLiteral {
            name: key.name,
            kind: kind.to_string(),
            span: tc.error_span(lit.span),
        });
        check_unknown_nominal_fields(&lit.node.fields, tc);
        return checked_from_type(expr, Type::Infer, tc);
    }

    let agg = tc
        .decls
        .aggregate(&key)
        .expect("aggregate exists for resolved key")
        .clone();
    let kind = match key.kind {
        NominalKind::Struct => DeprecatedUseKind::Struct,
        NominalKind::DataRef => DeprecatedUseKind::DataRef,
        NominalKind::Enum | NominalKind::Extern => unreachable!("aggregate key checked above"),
    };
    tc.warn_deprecated(&agg.policy, kind, key.name, lit.span);
    let expected_ty = expected.as_ref().map(|handle| tc.handle_type(handle));
    let inf = match &target.seeds {
        Some(seeds) => NominalLiteralSolver::from_seeds(&agg.generics, seeds, lit.span, tc),
        None => {
            let Some(inf) =
                NominalLiteralSolver::new(&agg.generics, &lit.node.generic_args, lit.span, tc)
            else {
                check_unknown_nominal_fields(&lit.node.fields, tc);
                return checked_from_type(expr, Type::Infer, tc);
            };
            inf
        }
    };
    let expected_ok = inf.bind_expected(&key, &agg.generics, expected_ty.as_ref(), lit.span, tc);
    let field_check = check_nominal_fields(
        expr.node.id,
        &lit.node.fields,
        &agg.fields,
        nominal_type(&key),
        lit.span,
        &inf,
        tc,
    );
    if !expected_ok || field_check.failed {
        return checked_from_type(expr, Type::Infer, tc);
    }
    let Some(ty) = inf.finalize(&key, &agg.generics, lit.span, tc) else {
        return checked_from_type(expr, Type::Infer, tc);
    };
    tc.reject_user_any_type(&ty, lit.span);
    let handle = tc.type_handle(&ty);
    let mut checked = solve_and_checked_from_handle(expr, handle, tc);
    checked.contains_extern_any = field_check.contains_extern_any;
    checked
}

fn check_enum_struct_variant_lit(
    expr: &ExprNode,
    lit: &StructLiteralNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> Option<CheckedType> {
    let qualifier = lit.node.qualifier?;
    let subject = tc.visible_type_subject(qualifier, lit.span)?;
    let key = tc
        .decls
        .key_for_type(&subject)
        .filter(|key| key.kind == NominalKind::Enum)?;
    let Some(resolved) = enum_variant::resolve_use(tc, &key, lit.node.name, lit.span) else {
        check_unknown_nominal_fields(&lit.node.fields, tc);
        return Some(checked_from_type(expr, Type::Infer, tc));
    };
    let Some(fields) = enum_variant::expect_struct(tc, &resolved, lit.span) else {
        check_unknown_nominal_fields(&lit.node.fields, tc);
        return Some(checked_from_type(expr, Type::Infer, tc));
    };

    if !lit.node.generic_args.is_empty() {
        tc.push_error(TypeError::GenericArity(ArityError::TypeArgs {
            expected: 0,
            found: lit.node.generic_args.len(),
        }));
        check_unknown_nominal_fields(&lit.node.fields, tc);
        return Some(checked_from_type(expr, Type::Infer, tc));
    }

    let expected_ty = expected.as_ref().map(|handle| tc.handle_type(handle));
    let inf = NominalLiteralSolver::without_args(&resolved.generics, lit.span, tc);
    let expected_ok =
        inf.bind_expected(&key, &resolved.generics, expected_ty.as_ref(), lit.span, tc);
    let field_check = check_variant_literal_fields(
        expr.node.id,
        &lit.node.fields,
        fields,
        &key,
        lit.node.name,
        lit.span,
        &inf,
        tc,
    );
    if !expected_ok || field_check.failed {
        return Some(checked_from_type(expr, Type::Infer, tc));
    }
    let Some(ty) = inf.finalize(&key, &resolved.generics, lit.span, tc) else {
        return Some(checked_from_type(expr, Type::Infer, tc));
    };
    tc.reject_user_any_type(&ty, lit.span);
    let handle = tc.type_handle(&ty);
    let mut checked = solve_and_checked_from_handle(expr, handle, tc);
    checked.contains_extern_any = field_check.contains_extern_any;
    Some(checked)
}

pub(super) fn check_inferred_enum_hint(
    expr: &ExprNode,
    node: &InferredEnumNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let Some(expected) = expected else {
        return cannot_infer_inferred_enum(expr, node, tc);
    };
    let expected_ty = tc.handle_type(&expected);
    let expected_key = tc
        .decls
        .key_for_type(&expected_ty)
        .filter(|key| key.kind == NominalKind::Enum);
    let Some(key) = expected_key else {
        return cannot_infer_inferred_enum(expr, node, tc);
    };

    if tc.decls.enum_schema(&key).is_none() {
        return checked_from_type(expr, Type::Infer, tc);
    }
    let Some(resolved) = enum_variant::resolve_use(tc, &key, node.node.variant, node.span) else {
        check_inferred_enum_args(&node.node.args, tc);
        return checked_from_type(expr, Type::Infer, tc);
    };
    let generics = resolved.generics.clone();

    let inf = NominalLiteralSolver::without_args(&generics, node.span, tc);
    if !inf.bind_expected(&key, &generics, Some(&expected_ty), node.span, tc) {
        check_inferred_enum_args(&node.node.args, tc);
        return checked_from_type(expr, Type::Infer, tc);
    }

    let Some(payload) = enum_variant::expect_shape(
        tc,
        &resolved,
        inferred_enum_arg_shape(&node.node.args),
        node.span,
    ) else {
        check_inferred_enum_args(&node.node.args, tc);
        return checked_from_type(expr, Type::Infer, tc);
    };

    let mut contains_extern_any = false;
    match (payload, &node.node.args) {
        (enum_variant::VariantPayloadRef::Unit, InferredEnumArgs::Unit) => {}
        (enum_variant::VariantPayloadRef::Tuple(params), InferredEnumArgs::Tuple(args)) => {
            if params.len() != args.len() {
                enum_variant::push_arg_count_mismatch(
                    tc,
                    key.name,
                    node.node.variant,
                    params.len(),
                    args.len(),
                    node.span,
                );
                check_exprs_without_hint(args, tc);
                return checked_from_type(expr, Type::Infer, tc);
            }
            let mut failed = false;
            for (arg, param) in args.iter().zip(params) {
                let hint = inf.instantiate(param, tc);
                let checked = check_expected_value_expr(arg, hint, tc);
                tc.record_aggregate_elem_flow(expr.node.id, arg);
                contains_extern_any |= checked.contains_extern_any;
                failed |= tc.solve_constraints();
            }
            if failed || inf.finalize(&key, &generics, node.span, tc).is_none() {
                return checked_from_type(expr, Type::Infer, tc);
            }
        }
        (enum_variant::VariantPayloadRef::Struct(fields), InferredEnumArgs::Struct(args)) => {
            let field_check = check_variant_literal_fields(
                expr.node.id,
                args,
                fields,
                &key,
                node.node.variant,
                node.span,
                &inf,
                tc,
            );
            contains_extern_any |= field_check.contains_extern_any;
            if field_check.failed || inf.finalize(&key, &generics, node.span, tc).is_none() {
                return checked_from_type(expr, Type::Infer, tc);
            }
        }
        _ => unreachable!("inferred enum shape was validated before payload checking"),
    }

    let mut checked = solve_and_checked_from_handle(expr, expected, tc);
    checked.contains_extern_any = contains_extern_any;
    checked
}

fn cannot_infer_inferred_enum(
    expr: &ExprNode,
    node: &InferredEnumNode,
    tc: &mut TypeChecker,
) -> CheckedType {
    tc.push_error(TypeError::CannotInferEnum {
        span: tc.error_span(node.span),
    });
    check_inferred_enum_args(&node.node.args, tc);
    checked_from_type(expr, Type::Infer, tc)
}

#[derive(Default)]
struct NominalFieldCheck {
    failed: bool,
    contains_extern_any: bool,
}

fn check_nominal_fields(
    aggregate: ExprId,
    fields: &[(Ident, ExprNode)],
    schema: &NamedSchemas<FieldSchema>,
    owner_ty: Type,
    span: Span,
    inf: &NominalLiteralSolver,
    tc: &mut TypeChecker,
) -> NominalFieldCheck {
    check_expr_fields(
        aggregate,
        fields,
        schema,
        field_check::FieldOwner::Nominal(owner_ty),
        field_check::MissingFields::AllowDefaults,
        span,
        inf,
        tc,
    )
}

fn check_variant_literal_fields(
    aggregate: ExprId,
    fields: &[(Ident, ExprNode)],
    schema: &NamedSchemas<FieldSchema>,
    key: &NominalKey,
    variant: Ident,
    span: Span,
    inf: &NominalLiteralSolver,
    tc: &mut TypeChecker,
) -> NominalFieldCheck {
    check_expr_fields(
        aggregate,
        fields,
        schema,
        field_check::FieldOwner::Variant {
            key: key.clone(),
            variant,
        },
        field_check::MissingFields::RequireAll,
        span,
        inf,
        tc,
    )
}

fn check_expr_fields(
    aggregate: ExprId,
    fields: &[(Ident, ExprNode)],
    schema: &NamedSchemas<FieldSchema>,
    owner: field_check::FieldOwner,
    missing: field_check::MissingFields,
    span: Span,
    inf: &NominalLiteralSolver,
    tc: &mut TypeChecker,
) -> NominalFieldCheck {
    let shape = field_check::check_named(
        fields,
        schema,
        &owner,
        missing,
        Some(span),
        |value| value.span,
        tc,
    );
    for index in &shape.invalid_indices {
        check_expr_checked(&fields[*index].1, tc);
    }
    let mut check = NominalFieldCheck {
        failed: shape.failed,
        contains_extern_any: false,
    };
    for field in shape.fields {
        let value = &fields[field.index].1;
        tc.check_matched_field_access_policy(&owner, field.name, &field.policy, value.span);
        let hint = inf.instantiate(&field.ty, tc);
        let checked = check_expected_value_expr(value, hint.clone(), tc);
        tc.record_aggregate_elem_flow(aggregate, value);
        check.contains_extern_any |= checked.contains_extern_any;
        check.failed |= tc.solve_constraints();
    }
    check
}

pub(super) fn check_unknown_nominal_fields(fields: &[(Ident, ExprNode)], tc: &mut TypeChecker) {
    for (_, value) in fields {
        check_expr_checked(value, tc);
    }
}

fn check_inferred_enum_args(args: &InferredEnumArgs, tc: &mut TypeChecker) {
    match args {
        InferredEnumArgs::Unit => {}
        InferredEnumArgs::Tuple(args) => check_exprs_without_hint(args, tc),
        InferredEnumArgs::Struct(fields) => check_unknown_nominal_fields(fields, tc),
    }
}

fn check_exprs_without_hint(args: &[ExprNode], tc: &mut TypeChecker) {
    for arg in args {
        check_expr_checked(arg, tc);
    }
}

fn inferred_enum_arg_shape(args: &InferredEnumArgs) -> VariantShape {
    match args {
        InferredEnumArgs::Unit => VariantShape::Unit,
        InferredEnumArgs::Tuple(_) => VariantShape::Tuple,
        InferredEnumArgs::Struct(_) => VariantShape::Struct,
    }
}

fn nominal_literal_type(
    key: &NominalKey,
    generics: &GenericParams,
    args: Option<&GenericArgs>,
) -> Type {
    if let Some(args) = args {
        let const_args = ConstTerm::to_args_no_infer(&args.const_args)
            .expect("nominal literal finalization must not produce inference const terms");
        return nominal_type_with_args(key, &args.type_args, &const_args);
    }

    let type_args = generics
        .type_params
        .iter()
        .map(|param| Type::Var(param.id))
        .collect::<Vec<_>>();
    let const_args = generics
        .const_params
        .iter()
        .map(|param| ConstArg::Param(param.id))
        .collect::<Vec<_>>();
    nominal_type_with_args(key, &type_args, &const_args)
}

struct StructLiteralTarget {
    key: NominalKey,
    seeds: Option<GenericSolverSeeds>,
}

fn resolve_struct_target(
    lit: &StructLiteralNode,
    tc: &mut TypeChecker,
) -> Option<StructLiteralTarget> {
    if lit.node.qualifier.is_none()
        && let Some(alias) = tc.local_type_scopes.visible(lit.node.name, None).cloned()
    {
        let expanded = if lit.node.generic_args.is_empty() {
            tc.resolve_local_alias_target_for_tc_at(&alias, lit.span, lit.node.name)
        } else {
            let ty = Type::UnresolvedNominal {
                qualifier: None,
                name: lit.node.name,
                generic_args: lit.node.generic_args.clone(),
            };
            tc.resolve_type_for_tc_at(&ty, lit.span)
        };
        return struct_literal_target_from_expanded(lit, expanded, tc);
    }

    let lookup =
        tc.decls
            .resolve_type_member(&tc.current_module, lit.node.qualifier, lit.node.name);
    tc.mark_import_used(lookup.import);
    let binding = match lookup.result {
        ModuleMemberLookup::Found(binding) => binding,
        ModuleMemberLookup::Private => {
            tc.push_error(TypeError::PrivateModuleMember {
                module: lookup.target.unwrap_or_else(|| tc.current_module.clone()),
                name: lit.node.name,
                span: tc.error_span(lit.span),
            });
            return None;
        }
        ModuleMemberLookup::Missing => {
            tc.push_error(TypeError::UnknownStructLiteral {
                qualifier: lit.node.qualifier,
                name: lit.node.name,
                span: tc.error_span(lit.span),
            });
            return None;
        }
    };
    match binding {
        TypeBinding::Nominal(key) => Some(StructLiteralTarget { key, seeds: None }),
        TypeBinding::Alias(key) => {
            let expanded = if lit.node.generic_args.is_empty() {
                tc.resolve_module_alias_target_for_tc_at(&key, lit.span, lit.node.name)
            } else {
                let ty = Type::UnresolvedNominal {
                    qualifier: lit.node.qualifier,
                    name: lit.node.name,
                    generic_args: lit.node.generic_args.clone(),
                };
                tc.resolve_type_for_tc_at(&ty, lit.span)
            };
            struct_literal_target_from_expanded(lit, expanded, tc)
        }
        TypeBinding::Contract(_) => {
            tc.resolve_type_for_tc_at(
                &Type::UnresolvedNominal {
                    qualifier: lit.node.qualifier,
                    name: lit.node.name,
                    generic_args: lit.node.generic_args.clone(),
                },
                lit.span,
            );
            None
        }
    }
}

fn struct_literal_target_from_expanded(
    lit: &StructLiteralNode,
    expanded: Type,
    tc: &mut TypeChecker,
) -> Option<StructLiteralTarget> {
    if matches!(expanded, Type::Infer) {
        return None;
    }
    let Some(key) = tc.decls.key_for_type(&expanded) else {
        tc.push_error(TypeError::InvalidStructLiteral {
            name: lit.node.name,
            kind: expanded.to_string(),
            span: tc.error_span(lit.span),
        });
        return None;
    };
    let seeds = tc
        .decls
        .nominal_generics(&key)
        .map(|generics| literal_target_seeds(&generics, &expanded));
    Some(StructLiteralTarget { key, seeds })
}

fn literal_target_seeds(generics: &GenericParams, expanded: &Type) -> GenericSolverSeeds {
    let Some(args) = nominal_generic_args(expanded) else {
        return GenericSolverSeeds::default();
    };
    let mut seeds = GenericSolverSeeds::default();
    for (param, ty) in generics.type_params.iter().zip(args.type_args) {
        if !type_depends_on_generics(&ty) {
            seeds.type_args.insert(param.id, ty);
        }
    }
    for (param, term) in generics.const_params.iter().zip(args.const_args) {
        if !matches!(term, ConstTerm::Param(_)) {
            seeds.const_args.insert(param.id, term);
        }
    }
    seeds
}

pub(super) fn type_from_lit(lit: &Lit) -> Type {
    match lit {
        Lit::Int(_) => Type::Int,
        Lit::Float(_) => Type::Float,
        Lit::Bool(_) => Type::Bool,
        Lit::String(_) => Type::String,
        Lit::Nil => Type::Infer,
    }
}

fn expected_range_bound(expected: Option<&TypeHandle>, tc: &TypeChecker) -> Option<TypeHandle> {
    let expected = expected_assignable_type(expected, tc)?;
    let inner = tc.decls.core_range_inner(&expected)?;
    Some(tc.type_handle(inner))
}

pub(super) fn check_range_expr(
    expr: &ExprNode,
    range: &RangeNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let bound = expected_range_bound(expected.as_ref(), tc)
        .unwrap_or_else(|| tc.fresh_temp_handle(range.span));
    let (kind, contains_extern_any) = match &range.node {
        Range::Bounded {
            start,
            end,
            inclusive,
        } => {
            let start = check_unprojected_expected(start, bound.clone(), tc);
            let end = check_unprojected_expected(end, bound.clone(), tc);
            let kind = if *inclusive {
                CoreRangeKind::Inclusive
            } else {
                CoreRangeKind::Exclusive
            };
            (kind, start.contains_extern_any || end.contains_extern_any)
        }
        Range::From { start } => {
            let start = check_unprojected_expected(start, bound.clone(), tc);
            (CoreRangeKind::From, start.contains_extern_any)
        }
        Range::To { end, inclusive } => {
            let end = check_unprojected_expected(end, bound.clone(), tc);
            let kind = if *inclusive {
                CoreRangeKind::ToInclusive
            } else {
                CoreRangeKind::To
            };
            (kind, end.contains_extern_any)
        }
    };
    tc.solve_constraints();
    let ty = tc
        .decls
        .core_range_of(kind, tc.handle_type(&bound))
        .expect("core range declaration is available");
    let mut checked = checked_from_type(expr, ty, tc);
    checked.contains_extern_any = contains_extern_any;
    checked
}

pub(super) fn check_string_interp(
    expr: &ExprNode,
    parts: &[StringPart],
    tc: &mut TypeChecker,
) -> CheckedType {
    let mut contains_extern_any = false;
    for part in parts {
        let StringPart::Expr(inner, spec) = part else {
            continue;
        };
        let checked = check_value_expr_checked_with_hint(inner, None, tc);
        if let Some(spec) = spec {
            tc.reject_dyn_format(&checked.ty, inner.span);
            validate_format_spec(&checked.ty, &spec.node, spec.span, tc);
        } else if !checked.ty.is_str() {
            check_default_stringify_conversion(&checked, inner.span, tc);
            tc.record_stringify(inner.node.id, inner.node.id);
        }
        contains_extern_any |= checked.contains_extern_any;
    }
    let mut checked = checked_from_type(expr, Type::String, tc);
    checked.contains_extern_any = contains_extern_any;
    checked
}

fn validate_format_spec(ty: &Type, spec: &FormatSpec, span: Span, tc: &mut TypeChecker) {
    if matches!(ty, Type::Infer) {
        return;
    }
    let reason = match spec.kind {
        FormatKind::Hex | FormatKind::HexUpper | FormatKind::Binary if !ty.is_int() => {
            Some("integer format requires int")
        }
        FormatKind::Exp | FormatKind::ExpUpper if !ty.is_float() => {
            Some("scientific format requires float")
        }
        _ if spec.precision.is_some() && !(ty.is_float() || ty.is_str()) => {
            Some("precision requires float or string")
        }
        _ if spec.sign == FormatSign::Always && !(ty.is_int() || ty.is_float()) => {
            Some("sign requires numeric type")
        }
        _ => None,
    };
    if let Some(reason) = reason {
        tc.push_error(TypeError::InvalidFormatSpec {
            reason,
            span: tc.error_span(span),
        });
    }
}
