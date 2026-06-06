use std::collections::hash_map::Values;

use crate::{
    ast::{ContractRef, ExprId, Ident, NominalKind, Type, TypeVisitor},
    diagnostic::DiagnosticTag,
    externs::{self, RawExterns, catalog::ExternCatalog},
    lint::{LintEvent, LintId},
    span::Span,
    test_support::{
        core_option_type, parse_program, resolved_modules_with_core_option,
        resolved_with_core_option,
    },
    typecheck::{
        self, BindingPromotionMap, BodyInstanceKey, CallMap, CallableId, CallableInstanceKey,
        CompileWarning, ContractWitnessMap, DeprecatedUseKind, DynCallMap, DynConversionMap,
        DynDowncastMap, DynWeakeningMap, ExpectedProjectionFact, ExpectedProjectionMap,
        ExternUseMap, GlobalAccessMap, LambdaCaptureMap, LambdaEscapeMap, MemberPathMap,
        SemanticBodyFacts, SemanticCheckOutput, SemanticProgram, TypeError,
        decls::DeclarationIndex,
    },
};

pub(crate) struct TypecheckTestResult {
    program: SemanticProgram,
    source_types: typecheck::infer::SourceExprTypes,
    public_facts: typecheck::TypecheckFacts,
    flat_facts: SemanticBodyFacts,
    warnings: Vec<CompileWarning>,
    lint_events: Vec<LintEvent>,
}

impl TypecheckTestResult {
    fn from_semantic(semantic: SemanticCheckOutput) -> Self {
        let flat_facts = semantic.program.facts.flattened_body_facts();
        Self {
            program: semantic.program,
            source_types: semantic.source_types,
            public_facts: semantic.public_facts,
            flat_facts,
            warnings: semantic.warnings,
            lint_events: semantic.lint_events,
        }
    }

    pub(crate) fn types(&self) -> impl Iterator<Item = (&ExprId, (Span, &Type))> {
        self.source_types.iter().map(|(id, (span, ty))| {
            let span = span
                .expect("test expression type missing source span")
                .byte();
            (id, (span, ty))
        })
    }

    pub(crate) fn body(&self, key: &BodyInstanceKey) -> Option<&SemanticBodyFacts> {
        self.program.facts.body(key)
    }

    pub(crate) fn expect_body(&self, key: &BodyInstanceKey) -> &SemanticBodyFacts {
        self.body(key).expect("missing semantic body facts")
    }

    pub(crate) fn bodies(&self) -> Values<'_, BodyInstanceKey, SemanticBodyFacts> {
        self.program.facts.bodies.values()
    }

    pub(crate) fn calls(&self) -> &CallMap {
        &self.flat_facts.calls
    }

    pub(crate) fn default_args(&self) -> &typecheck::DefaultArgMap {
        &self.flat_facts.default_args
    }

    pub(crate) fn extern_uses(&self) -> &ExternUseMap {
        &self.flat_facts.extern_uses
    }

    pub(crate) fn member_paths(&self) -> &MemberPathMap {
        &self.flat_facts.member_paths
    }

    pub(crate) fn expected_projections(&self) -> &ExpectedProjectionMap {
        &self.flat_facts.expected_projections
    }

    pub(crate) fn contract_witnesses(&self) -> &ContractWitnessMap {
        &self.program.facts.contract_witnesses
    }

    pub(crate) fn dyn_conversions(&self) -> &DynConversionMap {
        &self.flat_facts.dyn_conversions
    }

    pub(crate) fn dyn_weakenings(&self) -> &DynWeakeningMap {
        &self.flat_facts.dyn_weakenings
    }

    pub(crate) fn dyn_calls(&self) -> &DynCallMap {
        &self.flat_facts.dyn_calls
    }

    pub(crate) fn dyn_downcasts(&self) -> &DynDowncastMap {
        &self.flat_facts.dyn_downcasts
    }

    pub(crate) fn global_accesses(&self) -> &GlobalAccessMap {
        &self.flat_facts.global_accesses
    }

    pub(crate) fn for_step_runtime_checks(&self) -> &typecheck::ForStepRuntimeCheckMap {
        self.public_facts.for_step_runtime_checks()
    }

    pub(crate) fn lambda_escapes(&self) -> &LambdaEscapeMap {
        self.public_facts.lambda_escapes()
    }

    pub(crate) fn lambda_captures(&self) -> &LambdaCaptureMap {
        self.public_facts.lambda_captures()
    }

    pub(crate) fn binding_promotions(&self) -> &BindingPromotionMap {
        self.public_facts.binding_promotions()
    }

    pub(crate) fn warnings(&self) -> &[CompileWarning] {
        &self.warnings
    }

    pub(crate) fn lint_events(&self) -> &[LintEvent] {
        &self.lint_events
    }

    pub(crate) fn decls(&self) -> &DeclarationIndex {
        &self.program.declarations
    }

    pub(crate) fn function_facts(&self) -> &[typecheck::SemanticFunctionInstanceFact] {
        &self.program.declaration_facts.functions
    }

    pub(crate) fn externs(&self) -> &ExternCatalog {
        &self.program.externs
    }

    pub(crate) fn function_body(&self, name: &str) -> BodyInstanceKey {
        self.program
            .declaration_facts
            .functions
            .iter()
            .find(|function| function.id.name == Ident::new(name))
            .expect("missing semantic function fact")
            .body
            .clone()
    }
}

pub(crate) fn nominal_struct(name: &str) -> Type {
    Type::nominal(NominalKind::Struct, Ident::new(name), vec![], vec![], None)
}

pub(crate) fn core_option(inner: Type) -> Type {
    core_option_type(inner)
}

pub(crate) fn generic_body(name: &str, type_args: Vec<Type>) -> BodyInstanceKey {
    BodyInstanceKey::Callable(CallableInstanceKey {
        target: CallableId::function(typecheck::ModuleScope::Root, Ident::new(name)),
        args: typecheck::GenericArgs {
            type_args,
            const_args: vec![],
        },
    })
}

pub(crate) fn single_expected_projection(
    result: &TypecheckTestResult,
) -> (ExprId, &ExpectedProjectionFact) {
    assert_eq!(result.expected_projections().len(), 1);
    let (&expr_id, fact) = result
        .expected_projections()
        .iter()
        .next()
        .expect("missing projection fact");
    assert_eq!(fact.expr_id, expr_id);
    (expr_id, fact)
}

pub(crate) fn assert_expected_projection(
    result: &TypecheckTestResult,
    path: &[&str],
    target_ty: Type,
) -> ExprId {
    let (expr_id, fact) = single_expected_projection(result);
    let path = path
        .iter()
        .map(|name| Ident::new(*name))
        .collect::<Vec<_>>();
    assert_eq!(fact.path, path);
    assert_eq!(fact.target_ty, target_ty);
    expr_id
}

pub(crate) fn assert_expr_type(result: &TypecheckTestResult, expr_id: ExprId, expected: &Type) {
    let (_, ty) = result
        .types()
        .find(|(id, _)| **id == expr_id)
        .expect("missing expression type")
        .1;
    assert_eq!(ty, expected);
}

pub(crate) fn assert_deprecated_warning(
    result: &TypecheckTestResult,
    kind: DeprecatedUseKind,
    name: &str,
    reason: Option<&str>,
) {
    let events = result.lint_events();
    assert_eq!(events.len(), 1);
    let event = &events[0];
    assert_eq!(event.id, LintId::Deprecated);
    assert_eq!(event.tags, [DiagnosticTag::Deprecated]);
    assert_eq!(
        event.message,
        super::super::annotation::render_deprecated_access(kind, Ident::new(name), reason)
    );
}

pub(crate) fn assert_typecheck_closed(result: &TypecheckTestResult) {
    for ty in result.types().map(|(_, (_, ty))| ty) {
        assert_closed_type(ty, "result");
    }
    for body in result.bodies() {
        for fact in body.expr_types.values() {
            let ty = fact
                .ty
                .as_ref()
                .expect("semantic expression type not finalized");
            assert_closed_type(ty, "semantic expression");
        }
    }
    for target in result.calls().values() {
        let facts = typecheck::call_target_closure_facts(target);
        assert!(
            !facts.types.infer.contains_type,
            "call target contains inferred type: {target:?}"
        );
        assert!(
            facts.types.first_unresolved.is_none(),
            "call target contains unresolved type ref: {target:?}"
        );
        assert!(
            !facts.contains_unresolved_const(),
            "call target contains unresolved const: {target:?}"
        );
        assert!(
            !facts.consts.contains_infer,
            "call target contains inferred const: {target:?}"
        );
    }
    result.externs().for_each_resolved_ty(|ty, _| {
        assert_closed_type(&ty.ty, "extern");
    });
}

fn assert_closed_type(ty: &Type, label: &str) {
    let facts = typecheck::type_closure_facts(ty);
    assert!(
        !facts.infer.contains_type,
        "{label} type contains inferred type: {ty:?}"
    );
    assert!(
        facts.first_unresolved.is_none(),
        "{label} type contains unresolved type ref: {ty:?}"
    );
    assert!(
        !facts.contains_unresolved_const,
        "{label} type contains unresolved const: {ty:?}"
    );
    assert!(
        !type_contains_dyn_hole(ty),
        "{label} type contains inferred dynamic contract: {ty:?}"
    );
}

fn type_contains_dyn_hole(ty: &Type) -> bool {
    struct DynHoleVisitor;

    impl TypeVisitor for DynHoleVisitor {
        fn visit_contract_ref_leaf(&mut self, contract: &ContractRef) -> bool {
            matches!(contract, ContractRef::Infer | ContractRef::Hole(_))
        }
    }

    let mut visitor = DynHoleVisitor;
    visitor.visit_type(ty)
}

pub(crate) fn output(source: &str) -> typecheck::TypecheckOutput {
    let program = parse_program(source);
    let resolved = resolved_with_core_option(&program);
    let raw_externs =
        externs::prepare_raw_externs(RawExterns::default(), &program, &resolved).unwrap();
    typecheck::check_with_modules(
        &program,
        &resolved,
        raw_externs,
        typecheck::TypecheckConfig::default(),
    )
}

pub(crate) fn check(source: &str) -> Result<TypecheckTestResult, Vec<TypeError>> {
    let program = parse_program(source);
    let resolved = resolved_with_core_option(&program);
    let raw_externs =
        externs::prepare_raw_externs(RawExterns::default(), &program, &resolved).unwrap();
    check_with_raw_externs(&program, &resolved, raw_externs)
}

pub(crate) fn check_with_raw_externs(
    program: &crate::ast::Program,
    resolved: &crate::resolve::ResolveResult,
    raw_externs: RawExterns,
) -> Result<TypecheckTestResult, Vec<TypeError>> {
    let semantic = typecheck::check_semantic_with_modules(
        program,
        resolved,
        raw_externs,
        typecheck::TypecheckConfig::default(),
    )
    .map_err(|failure| failure.errors)?;
    Ok(TypecheckTestResult::from_semantic(semantic))
}

pub(crate) fn errors(source: &str) -> Vec<TypeError> {
    match check(source) {
        Ok(_) => panic!("expected typecheck errors in: {source}"),
        Err(errors) => errors,
    }
}

pub(crate) fn assert_single_error(source: &str, matches: impl FnOnce(&TypeError) -> bool) {
    let errors = errors(source);
    assert_eq!(errors.len(), 1);
    assert!(matches(&errors[0]), "unexpected error: {:?}", errors[0]);
}

pub(crate) fn check_mods(
    root_source: &str,
    dep_source: &str,
) -> Result<TypecheckTestResult, Vec<TypeError>> {
    check_named(root_source, &[("gamekit", dep_source)])
}

pub(crate) fn check_named(
    root_source: &str,
    modules: &[(&str, &str)],
) -> Result<TypecheckTestResult, Vec<TypeError>> {
    let root = parse_program(root_source);
    let resolved = resolved_modules_with_core_option(&root, modules);
    let raw_externs =
        externs::prepare_raw_externs(RawExterns::default(), &root, &resolved).unwrap();
    check_with_raw_externs(&root, &resolved, raw_externs)
}

fn last_expr_type(result: &TypecheckTestResult) -> Option<Type> {
    result
        .types()
        .max_by(|(_, (left, _)), (_, (right, _))| {
            left.end
                .cmp(&right.end)
                .then_with(|| right.start.cmp(&left.start))
        })
        .map(|(_, (_, ty))| ty.clone())
}

pub(crate) fn ty_of(source: &str) -> Type {
    let result = check(source).expect("typecheck failed");
    last_expr_type(&result).unwrap_or(Type::Void)
}

pub(crate) fn assert_ty(source: &str, expected: Type) {
    let ty = ty_of(source);
    assert_eq!(ty, expected, "source: {source}");
}

pub(crate) fn assert_err(source: &str) {
    let result = check(source);
    assert!(result.is_err(), "expected error but got Ok in: {source}");
}

pub(crate) fn assert_err_count(source: &str, count: usize) {
    match check(source) {
        Ok(_) if count == 0 => {}
        Ok(_) => panic!("expected {count} errors in: {source}, got Ok"),
        Err(errors) => assert_eq!(
            errors.len(),
            count,
            "expected {count} errors in: {source}, got {errors:?}"
        ),
    }
}

pub(crate) fn assert_ty_mods(root: &str, dep: &str, expected: Type) {
    assert_ty_named(root, &[("gamekit", dep)], expected);
}

pub(crate) fn assert_ty_named(root: &str, modules: &[(&str, &str)], expected: Type) {
    let result = check_named(root, modules).expect("typecheck failed");
    let ty = last_expr_type(&result).unwrap_or(Type::Void);
    assert_eq!(ty, expected, "root: {root}");
}

pub(crate) fn assert_calls(source: &str, count: usize) {
    let result = check(source).expect("typecheck failed");
    assert_eq!(
        result.calls().len(),
        count,
        "expected {count} call targets in: {source}"
    );
}

pub(crate) fn assert_calls_with_modules(root: &str, dep: &str, count: usize) {
    let result = check_mods(root, dep).expect("typecheck failed");
    assert_eq!(
        result.calls().len(),
        count,
        "expected {count} call targets in: {root}"
    );
}
