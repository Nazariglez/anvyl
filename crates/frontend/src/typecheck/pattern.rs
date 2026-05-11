use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum PatternCover {
    CatchAll,
    Bool(bool),
    Int(i64),
    Float(u64),
    String(String),
    EnumVariant { key: NominalKey, variant: Ident },
    Tuple(Vec<PatternCover>),
    Or(Vec<PatternCover>),
    Unsupported,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Refutability {
    Irrefutable,
    Refutable,
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum PatternBindMode {
    Owned { mutable: bool },
    Alias,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PatternContext {
    Binding,
    For,
    IfLet,
    WhileLet,
    LetElse,
    Match,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct PatternOutcome {
    pub(crate) cover: PatternCover,
    pub(crate) had_error: bool,
    pub(crate) refutability: Refutability,
}

pub(super) struct PatternPlace {
    pub(super) expected_handle: TypeHandle,
    pub(super) expected_ty: Type,
    pub(super) access: PlaceAccess,
    pub(super) facts: PlaceUseFacts,
    pub(super) identity: PlaceIdentity,
    pub(super) accepts_extern_any: bool,
}

enum StructPatternTarget {
    Found(NominalKey, Type),
    Missing,
    ReportedError,
}

#[derive(Clone)]
struct PatternInput {
    expected: TypeHandle,
    expected_ty: Type,
    access: PlaceAccess,
    identity: PlaceIdentity,
    facts: PlaceUseFacts,
    accepts_extern_any: bool,
}

impl PatternInput {
    fn owned(expected_ty: Type, tc: &mut TypeChecker) -> Self {
        Self {
            expected: tc.type_handle(&expected_ty),
            expected_ty,
            access: PlaceAccess::Mutable,
            identity: PlaceIdentity::unknown(),
            facts: PlaceUseFacts::default(),
            accepts_extern_any: false,
        }
    }

    fn recovery(access: PlaceAccess, tc: &mut TypeChecker) -> Self {
        Self {
            expected: tc.type_handle(&Type::Infer),
            expected_ty: Type::Infer,
            access,
            identity: PlaceIdentity::unknown(),
            facts: PlaceUseFacts::default(),
            accepts_extern_any: false,
        }
    }

    fn from_place(place: PatternPlace) -> Self {
        Self {
            expected: place.expected_handle,
            expected_ty: place.expected_ty,
            access: place.access,
            identity: place.identity,
            facts: place.facts,
            accepts_extern_any: place.accepts_extern_any,
        }
    }

    fn tuple_field(&self, index: usize, expected_ty: Type, tc: &mut TypeChecker) -> Self {
        self.project(
            expected_ty,
            place::projected_field_access(self.access),
            self.facts.clone(),
            self.accepts_extern_any,
            |identity| identity.tuple(index),
            tc,
        )
    }

    fn optional_some(&self, expected_ty: Type, tc: &mut TypeChecker) -> Self {
        self.project(
            expected_ty,
            place::projected_field_access(self.access),
            self.facts.clone(),
            self.accepts_extern_any,
            |identity| identity.variant(Ident::new("Some")).tuple(0),
            tc,
        )
    }

    fn enum_tuple_field(
        &self,
        variant: Ident,
        index: usize,
        expected_ty: Type,
        tc: &mut TypeChecker,
    ) -> Self {
        self.project(
            expected_ty,
            place::projected_field_access(self.access),
            self.facts.clone(),
            self.accepts_extern_any,
            |identity| identity.variant(variant).tuple(index),
            tc,
        )
    }

    fn enum_struct_field(
        &self,
        variant: Ident,
        field: Ident,
        expected_ty: Type,
        tc: &mut TypeChecker,
    ) -> Self {
        self.project(
            expected_ty,
            place::projected_field_access(self.access),
            self.facts.clone(),
            self.accepts_extern_any,
            |identity| identity.variant(variant).field(field),
            tc,
        )
    }

    fn aggregate_field(
        &self,
        field: Ident,
        expected_ty: Type,
        access: PlaceAccess,
        extern_facts: Option<(PlaceUseFacts, bool)>,
        tc: &mut TypeChecker,
    ) -> Self {
        let (facts, accepts_extern_any) =
            extern_facts.unwrap_or_else(|| (self.facts.clone(), self.accepts_extern_any));
        self.project(
            expected_ty,
            access,
            facts,
            accepts_extern_any,
            |identity| identity.field(field),
            tc,
        )
    }

    fn project(
        &self,
        expected_ty: Type,
        access: PlaceAccess,
        facts: PlaceUseFacts,
        accepts_extern_any: bool,
        project_identity: impl FnOnce(PlaceIdentity) -> PlaceIdentity,
        tc: &mut TypeChecker,
    ) -> Self {
        Self {
            expected: tc.type_handle(&expected_ty),
            expected_ty,
            access,
            identity: project_identity(self.identity.clone()),
            facts,
            accepts_extern_any,
        }
    }
}

impl PatternOutcome {
    fn refutable(cover: PatternCover) -> Self {
        Self {
            cover,
            had_error: false,
            refutability: Refutability::Refutable,
        }
    }

    fn irrefutable(cover: PatternCover) -> Self {
        Self {
            cover,
            had_error: false,
            refutability: Refutability::Irrefutable,
        }
    }

    fn error() -> Self {
        Self {
            cover: PatternCover::Unsupported,
            had_error: true,
            refutability: Refutability::Unknown,
        }
    }
}

fn combine_refutability(left: Refutability, right: Refutability) -> Refutability {
    match (left, right) {
        (Refutability::Unknown, _) | (_, Refutability::Unknown) => Refutability::Unknown,
        (Refutability::Refutable, _) | (_, Refutability::Refutable) => Refutability::Refutable,
        (Refutability::Irrefutable, Refutability::Irrefutable) => Refutability::Irrefutable,
    }
}

fn or_refutability(alternatives: impl Iterator<Item = Refutability>) -> Refutability {
    let mut saw_unknown = false;
    for refutability in alternatives {
        match refutability {
            Refutability::Irrefutable => return Refutability::Irrefutable,
            Refutability::Unknown => saw_unknown = true,
            Refutability::Refutable => {}
        }
    }
    if saw_unknown {
        Refutability::Unknown
    } else {
        Refutability::Refutable
    }
}

fn same_binding_names(left: &BindingEnv, right: &BindingEnv) -> bool {
    left.bindings.len() == right.bindings.len()
        && left.iter().all(|(name, _)| right.binding(name).is_some())
}

struct PatternCheckResult {
    outcome: PatternOutcome,
    bindings: BindingAlternatives,
}

#[derive(Clone)]
struct BindingAlternatives {
    envs: Vec<BindingEnv>,
}

#[derive(Clone, Default)]
struct BindingEnv {
    bindings: Vec<(Ident, PatternBinding)>,
}

#[derive(Clone)]
struct PatternBinding {
    ty: TypeHandle,
    span: Span,
    kind: PatternBindingKind,
}

#[derive(Clone)]
enum PatternBindingKind {
    Owned { mutable: bool },
    Alias(place::AliasTarget),
}

impl PatternCheckResult {
    fn empty(outcome: PatternOutcome) -> Self {
        Self {
            outcome,
            bindings: BindingAlternatives::single_empty(),
        }
    }
}

impl BindingAlternatives {
    fn single_empty() -> Self {
        Self {
            envs: vec![BindingEnv::default()],
        }
    }

    fn single(env: BindingEnv) -> Self {
        Self { envs: vec![env] }
    }

    fn product(self, other: Self, tc: &mut TypeChecker) -> Self {
        let mut envs = vec![];
        for left in &self.envs {
            for right in &other.envs {
                envs.push(left.clone().merge(right.clone(), tc));
            }
        }
        Self { envs }
    }

    fn or(alternatives: &[PatternCheckResult], span: Span, tc: &mut TypeChecker) -> (Self, bool) {
        let Some((first, _)) = alternatives.split_first() else {
            return (Self::single_empty(), true);
        };
        let expected = first
            .bindings
            .envs
            .first()
            .expect("pattern alternatives are never empty");
        let mut valid = true;
        let mut envs = vec![];
        for alternative in alternatives {
            for env in &alternative.bindings.envs {
                if !same_binding_names(expected, env) {
                    tc.push_error(TypeError::OrPatternBindingMismatch {
                        span: tc.error_span(span),
                    });
                    valid = false;
                    continue;
                }
                for (name, expected_binding) in expected.iter() {
                    let Some(binding) = env.binding(name) else {
                        continue;
                    };
                    let expected_ty = tc.handle_type(&expected_binding.ty);
                    let found = tc.handle_type(&binding.ty);
                    if expected_ty != found {
                        tc.push_error(TypeError::OrPatternBindingTypeMismatch {
                            name,
                            expected: expected_ty,
                            found,
                            span: tc.error_span(span),
                        });
                        valid = false;
                    }
                }
                envs.push(env.clone());
            }
        }
        (Self { envs }, valid)
    }
}

impl PatternBinding {
    fn is_alias(&self) -> bool {
        matches!(self.kind, PatternBindingKind::Alias(_))
    }
}

impl BindingEnv {
    fn iter(&self) -> impl Iterator<Item = (Ident, &PatternBinding)> {
        self.bindings.iter().map(|(name, binding)| (*name, binding))
    }

    fn binding(&self, name: Ident) -> Option<&PatternBinding> {
        self.bindings
            .iter()
            .find_map(|(candidate, binding)| (*candidate == name).then_some(binding))
    }

    fn alias_target(&self, name: Ident) -> Option<&place::AliasTarget> {
        match &self.binding(name)?.kind {
            PatternBindingKind::Alias(target) => Some(target),
            PatternBindingKind::Owned { .. } => None,
        }
    }

    fn insert(&mut self, name: Ident, binding: PatternBinding, tc: &mut TypeChecker) -> bool {
        if self.binding(name).is_some() {
            tc.push_error(TypeError::DuplicateName {
                name,
                span: tc.error_span(binding.span),
            });
            return false;
        }
        self.bindings.push((name, binding));
        true
    }

    fn merge(mut self, other: Self, tc: &mut TypeChecker) -> Self {
        for (name, binding) in other.bindings {
            self.insert(name, binding, tc);
        }
        self
    }
}

struct PatternChecker<'tc> {
    tc: &'tc mut TypeChecker,
    extern_site: Option<ExprId>,
    context: PatternContext,
    mode: PatternBindMode,
}

impl<'tc> PatternChecker<'tc> {
    fn new(
        tc: &'tc mut TypeChecker,
        extern_site: Option<ExprId>,
        context: PatternContext,
        mode: PatternBindMode,
    ) -> Self {
        Self {
            tc,
            extern_site,
            context,
            mode,
        }
    }

    fn check_root(&mut self, pattern: &PatternNode, expected: &Type) -> PatternOutcome {
        let input = PatternInput::owned(expected.clone(), self.tc);
        let mut result = self.check(pattern, input.clone());
        self.apply_context_policy(pattern, &input.expected_ty, &mut result.outcome);
        self.install_root_bindings(&result.bindings, pattern.span);
        result.outcome
    }

    fn check_from_handle_root(
        &mut self,
        pattern: &PatternNode,
        input: PatternInput,
    ) -> PatternOutcome {
        let mut result = self.check(pattern, input.clone());
        self.apply_context_policy(pattern, &input.expected_ty, &mut result.outcome);
        self.install_root_bindings(&result.bindings, pattern.span);
        result.outcome
    }

    fn apply_context_policy(
        &mut self,
        pattern: &PatternNode,
        expected: &Type,
        outcome: &mut PatternOutcome,
    ) {
        match self.context {
            PatternContext::IfLet | PatternContext::WhileLet | PatternContext::LetElse
                if expected.is_option() && outcome.refutability == Refutability::Irrefutable =>
            {
                self.tc.push_error(TypeError::RequiresUnwrappingPattern {
                    span: self.tc.error_span(pattern.span),
                });
                outcome.had_error = true;
            }
            PatternContext::LetElse if outcome.refutability == Refutability::Irrefutable => {
                self.tc.push_error(TypeError::IrrefutableLetElse {
                    span: self.tc.error_span(pattern.span),
                });
                outcome.had_error = true;
            }
            _ => {}
        }
    }

    fn check_ident(&mut self, name: Ident, input: PatternInput, span: Span) -> PatternCheckResult {
        let kind = match self.mode {
            PatternBindMode::Alias => {
                if !input.access.can_assign() {
                    self.tc
                        .push_error(TypeError::VarPatternRequiresMutablePlace {
                            span: self.tc.error_span(span),
                        });
                }
                PatternBindingKind::Alias(place::AliasTarget {
                    access: input.access,
                    identity: input.identity,
                    facts: input.facts,
                    accepts_extern_any: input.accepts_extern_any,
                })
            }
            PatternBindMode::Owned { mutable } => PatternBindingKind::Owned { mutable },
        };
        let mut env = BindingEnv::default();
        env.insert(
            name,
            PatternBinding {
                ty: input.expected,
                span,
                kind,
            },
            self.tc,
        );
        PatternCheckResult {
            outcome: PatternOutcome::irrefutable(PatternCover::CatchAll),
            bindings: BindingAlternatives::single(env),
        }
    }

    fn check(&mut self, pattern: &PatternNode, input: PatternInput) -> PatternCheckResult {
        match &pattern.node {
            Pattern::Ident(name) => self.check_ident(*name, input, pattern.span),
            Pattern::Wildcard => {
                PatternCheckResult::empty(PatternOutcome::irrefutable(PatternCover::CatchAll))
            }
            Pattern::Tuple(elems) => self.check_tuple(elems, pattern.span, input),
            Pattern::Lit(lit) => {
                PatternCheckResult::empty(self.check_lit(pattern.span, lit, &input.expected_ty))
            }
            Pattern::Nil => {
                PatternCheckResult::empty(self.check_nil(pattern.span, &input.expected_ty))
            }
            Pattern::Optional(inner) => self.check_optional(inner, input),
            Pattern::Range { start, end, .. } => PatternCheckResult::empty(self.check_range(
                pattern.span,
                start.clone(),
                end.clone(),
                &input.expected_ty,
            )),
            Pattern::Or(alternatives) => self.check_or(alternatives, pattern.span, input),
            Pattern::Rest => PatternCheckResult::empty(self.unsupported_named("..", pattern.span)),
            Pattern::Struct { name, fields } => {
                self.check_struct(*name, fields, pattern.span, input)
            }
            Pattern::EnumUnit { qualifier, variant } => PatternCheckResult::empty(
                self.check_enum_unit(Some(*qualifier), *variant, pattern.span, &input.expected_ty),
            ),
            Pattern::InferredEnumUnit { variant } => PatternCheckResult::empty(
                self.check_enum_unit(None, *variant, pattern.span, &input.expected_ty),
            ),
            Pattern::EnumTuple {
                qualifier,
                variant,
                fields,
            } => self.check_enum_tuple(Some(*qualifier), *variant, fields, pattern.span, input),
            Pattern::InferredEnumTuple { variant, fields } => {
                self.check_enum_tuple(None, *variant, fields, pattern.span, input)
            }
            Pattern::EnumStruct {
                qualifier,
                variant,
                fields,
                has_rest,
            } => self.check_enum_struct(
                Some(*qualifier),
                *variant,
                fields,
                *has_rest,
                pattern.span,
                input,
            ),
            Pattern::InferredEnumStruct {
                variant,
                fields,
                has_rest,
            } => self.check_enum_struct(None, *variant, fields, *has_rest, pattern.span, input),
        }
    }

    fn check_or(
        &mut self,
        alternatives: &[PatternNode],
        span: Span,
        input: PatternInput,
    ) -> PatternCheckResult {
        let mut checked = Vec::with_capacity(alternatives.len());
        for alternative in alternatives {
            checked.push(self.check(alternative, input.clone()));
        }
        let mut had_error = checked
            .iter()
            .any(|alternative| alternative.outcome.had_error);
        let (bindings, valid) = BindingAlternatives::or(&checked, span, self.tc);
        had_error |= !valid;
        let refutability = or_refutability(checked.iter().map(|alt| alt.outcome.refutability));
        let covers = checked
            .into_iter()
            .map(|alternative| alternative.outcome.cover)
            .collect();
        PatternCheckResult {
            outcome: PatternOutcome {
                cover: PatternCover::Or(covers),
                had_error,
                refutability,
            },
            bindings,
        }
    }

    fn install_root_bindings(&mut self, bindings: &BindingAlternatives, span: Span) {
        let Some(first) = bindings.envs.first() else {
            return;
        };
        for env in &bindings.envs[1..] {
            if !same_binding_names(first, env) {
                self.tc.push_error(TypeError::OrPatternBindingMismatch {
                    span: self.tc.error_span(span),
                });
                return;
            }
        }
        let group = (bindings.envs.len() > 1
            && first.iter().any(|(_, binding)| binding.is_alias()))
        .then(|| self.tc.fresh_alias_alt_group());
        for (name, binding) in first.iter() {
            match &binding.kind {
                PatternBindingKind::Owned { mutable } => {
                    self.tc
                        .define_pattern_binding_from_handle(name, &binding.ty, *mutable);
                }
                PatternBindingKind::Alias(_) => {
                    let targets = bindings
                        .envs
                        .iter()
                        .filter_map(|env| env.alias_target(name))
                        .cloned()
                        .collect();
                    let target = place::AliasTarget::merged(group, targets);
                    self.tc
                        .define_alias_binding_from_handle(name, &binding.ty, target);
                }
            }
        }
    }

    fn check_tuple(
        &mut self,
        elems: &[PatternNode],
        span: Span,
        input: PatternInput,
    ) -> PatternCheckResult {
        let elem_tys = match &input.expected_ty {
            Type::Tuple(tys) => tys.clone(),
            Type::Infer => vec![Type::Infer; elems.len()],
            _ => {
                self.tc.push_error(TypeError::TuplePatternOnNonTuple {
                    ty: input.expected_ty,
                    span: self.tc.error_span(span),
                });
                return PatternCheckResult::empty(PatternOutcome::error());
            }
        };
        if elems.len() != elem_tys.len() {
            self.tc.push_error(TypeError::TuplePatternArityMismatch {
                expected: elem_tys.len(),
                found: elems.len(),
                span: self.tc.error_span(span),
            });
            return PatternCheckResult::empty(PatternOutcome::error());
        }
        let mut had_error = false;
        let mut refutability = Refutability::Irrefutable;
        let mut bindings = BindingAlternatives::single_empty();
        let mut covers = vec![];
        for (index, (elem, elem_ty)) in elems.iter().zip(elem_tys).enumerate() {
            let elem_input = input.tuple_field(index, elem_ty, self.tc);
            let result = self.check(elem, elem_input);
            had_error |= result.outcome.had_error;
            refutability = combine_refutability(refutability, result.outcome.refutability);
            bindings = bindings.product(result.bindings, self.tc);
            covers.push(result.outcome.cover);
        }
        PatternCheckResult {
            outcome: PatternOutcome {
                cover: PatternCover::Tuple(covers),
                had_error,
                refutability,
            },
            bindings,
        }
    }

    fn check_lit(&mut self, span: Span, lit: &Lit, expected: &Type) -> PatternOutcome {
        let lit_ty = type_from_lit(lit);
        if lit_ty != *expected && !matches!(expected, Type::Infer) {
            self.tc.push_error(TypeError::InvalidLiteralPattern {
                expected: expected.clone(),
                found: lit_ty,
                span: self.tc.error_span(span),
            });
            return PatternOutcome::error();
        }
        let cover = match lit {
            Lit::Bool(value) => PatternCover::Bool(*value),
            Lit::Int(value) => PatternCover::Int(*value),
            Lit::Float(value) => PatternCover::Float(value.to_bits()),
            Lit::String(value) => PatternCover::String(value.clone()),
            Lit::Nil => PatternCover::Unsupported,
        };
        PatternOutcome::refutable(cover)
    }

    fn check_nil(&mut self, span: Span, expected: &Type) -> PatternOutcome {
        if !expected.is_option() && !matches!(expected, Type::Infer) {
            self.tc.push_error(TypeError::OptionalPatternOnNonOptional {
                span: self.tc.error_span(span),
            });
            return PatternOutcome::error();
        }
        PatternOutcome::refutable(
            self.option_cover(expected, "None")
                .unwrap_or(PatternCover::CatchAll),
        )
    }

    fn check_optional(&mut self, inner: &PatternNode, input: PatternInput) -> PatternCheckResult {
        if matches!(inner.node, Pattern::Optional(_)) {
            self.tc.push_error(TypeError::NestedOptionalPattern {
                span: self.tc.error_span(inner.span),
            });
            let recovery = input.project(
                Type::Infer,
                input.access,
                input.facts.clone(),
                input.accepts_extern_any,
                |identity| identity,
                self.tc,
            );
            self.check(inner, recovery);
            return PatternCheckResult::empty(PatternOutcome::error());
        }
        let Some(inner_ty) = input.expected_ty.option_inner() else {
            if !matches!(input.expected_ty, Type::Infer) {
                self.tc.push_error(TypeError::OptionalPatternOnNonOptional {
                    span: self.tc.error_span(inner.span),
                });
            }
            let recovery = input.project(
                Type::Infer,
                input.access,
                input.facts.clone(),
                input.accepts_extern_any,
                |identity| identity,
                self.tc,
            );
            self.check(inner, recovery);
            return PatternCheckResult::empty(PatternOutcome::error());
        };
        let inner_input = input.optional_some(inner_ty.clone(), self.tc);
        let result = self.check(inner, inner_input);
        PatternCheckResult {
            outcome: PatternOutcome {
                cover: self
                    .option_cover(&input.expected_ty, "Some")
                    .unwrap_or(result.outcome.cover),
                had_error: result.outcome.had_error,
                refutability: Refutability::Refutable,
            },
            bindings: result.bindings,
        }
    }

    fn option_cover(&self, expected: &Type, variant: &str) -> Option<PatternCover> {
        let key = self.tc.decls.key_for_type(expected).filter(|key| {
            key.kind == NominalKind::Enum && key.name.0.as_ref() == Type::OPTION_ENUM_NAME
        })?;
        Some(PatternCover::EnumVariant {
            key,
            variant: Ident::new(variant),
        })
    }

    fn check_range(
        &mut self,
        span: Span,
        start: Option<Lit>,
        end: Option<Lit>,
        expected: &Type,
    ) -> PatternOutcome {
        for lit in start.iter().chain(end.iter()) {
            let found = type_from_lit(lit);
            if found != *expected && !matches!(expected, Type::Infer) {
                self.tc.push_error(TypeError::InvalidLiteralPattern {
                    expected: expected.clone(),
                    found,
                    span: self.tc.error_span(span),
                });
                return PatternOutcome::error();
            }
        }
        self.unsupported_named("range", span)
    }

    fn resolve_struct_pattern_target(&mut self, name: Ident, span: Span) -> StructPatternTarget {
        if self.tc.local_type_scopes.visible(name, None).is_some() {
            let expanded = self
                .tc
                .resolve_type_for_tc_at(&Type::UnresolvedName(name), span);
            return self.struct_pattern_target_from_expanded(expanded);
        }
        let Some(binding) =
            self.tc
                .decls
                .resolve_visible_type_binding(&self.tc.current_module, None, name)
        else {
            return StructPatternTarget::Missing;
        };
        match binding {
            TypeBinding::Nominal(key) => {
                let ty = nominal_type(&key);
                StructPatternTarget::Found(key, ty)
            }
            TypeBinding::Alias(_) => {
                let expanded = self
                    .tc
                    .resolve_type_for_tc_at(&Type::UnresolvedName(name), span);
                self.struct_pattern_target_from_expanded(expanded)
            }
        }
    }

    fn struct_pattern_target_from_expanded(&self, expanded: Type) -> StructPatternTarget {
        if matches!(expanded, Type::Infer) {
            return StructPatternTarget::ReportedError;
        }
        let Some(key) = self.tc.decls.key_for_type(&expanded) else {
            return StructPatternTarget::Missing;
        };
        StructPatternTarget::Found(key, expanded)
    }

    fn check_struct(
        &mut self,
        name: Ident,
        fields: &[(Ident, PatternNode)],
        span: Span,
        input: PatternInput,
    ) -> PatternCheckResult {
        let (key, head_ty) = match self.resolve_struct_pattern_target(name, span) {
            StructPatternTarget::Found(key, ty) => (key, ty),
            StructPatternTarget::Missing => {
                self.tc.push_error(TypeError::UnknownType {
                    qualifier: None,
                    name,
                    span: self.tc.error_span(span),
                });
                self.check_field_patterns(fields, input.access);
                return PatternCheckResult::empty(PatternOutcome::error());
            }
            StructPatternTarget::ReportedError => {
                self.check_field_patterns(fields, input.access);
                return PatternCheckResult::empty(PatternOutcome::error());
            }
        };

        let expected_key = self.tc.decls.key_for_type(&input.expected_ty);
        if expected_key.as_ref() != Some(&key) && !matches!(input.expected_ty, Type::Infer) {
            self.tc.push_error(TypeError::TypeMismatch {
                expected: head_ty,
                found: input.expected_ty,
                span: self.tc.error_span(span),
            });
            return PatternCheckResult::empty(PatternOutcome::error());
        }

        match key.kind {
            NominalKind::Struct | NominalKind::DataRef => {
                let Some(agg) = self.tc.decls.aggregate(&key).cloned() else {
                    return PatternCheckResult::empty(PatternOutcome::error());
                };
                let owner_ty = if matches!(expected_key.as_ref(), Some(found) if found == &key) {
                    input.expected_ty.clone()
                } else {
                    head_ty
                };
                self.check_struct_fields(fields, owner_ty, &agg.fields, input)
            }
            NominalKind::Extern => {
                let Some(owner) = self.tc.externs.type_by_nominal(&key) else {
                    return PatternCheckResult::empty(PatternOutcome::error());
                };
                let field_schema = self
                    .tc
                    .extern_type(owner)
                    .fields
                    .iter()
                    .map(|field| {
                        (
                            field.name,
                            FieldSchema {
                                ty: field.ty.ty.clone(),
                                has_default: false,
                                policy: annotation::AccessPolicy::default(),
                                span: None,
                                embed: None,
                            },
                        )
                    })
                    .collect();
                self.check_struct_fields(fields, nominal_type(&key), &field_schema, input)
            }
            NominalKind::Enum => PatternCheckResult::empty(self.unsupported_named("Struct", span)),
        }
    }

    fn check_struct_fields(
        &mut self,
        fields: &[(Ident, PatternNode)],
        owner_ty: Type,
        schema: &HashMap<Ident, FieldSchema>,
        input: PatternInput,
    ) -> PatternCheckResult {
        let owner = field_check::FieldOwner::Nominal(owner_ty.clone());
        let shape = self.check_field_shape(
            fields,
            schema,
            &owner,
            field_check::MissingFields::None,
            None,
        );
        let mut had_error = shape.failed;
        let mut refutability = if shape.failed {
            Refutability::Unknown
        } else {
            Refutability::Irrefutable
        };
        let mut bindings = BindingAlternatives::single_empty();
        self.check_bad_field_patterns(fields, &shape, input.access);
        for field in shape.fields {
            let pattern = &fields[field.index].1;
            self.record_extern_field_read(field.name, pattern, &owner_ty);
            self.tc.check_matched_field_access_policy(
                &owner,
                field.name,
                &field.policy,
                pattern.span,
            );
            let field_access = self.struct_field_access(&owner_ty, field.name, input.access);
            let field_ty = self
                .tc
                .decls
                .aggregate_field_type(&owner_ty, field.name)
                .unwrap_or(field.ty);
            let extern_facts = self.extern_field_alias_facts(&owner_ty, field.name, &input.facts);
            let field_input =
                input.aggregate_field(field.name, field_ty, field_access, extern_facts, self.tc);
            let result = self.check(pattern, field_input);
            had_error |= result.outcome.had_error;
            refutability = combine_refutability(refutability, result.outcome.refutability);
            bindings = bindings.product(result.bindings, self.tc);
        }
        PatternCheckResult {
            outcome: PatternOutcome {
                cover: PatternCover::CatchAll,
                had_error,
                refutability,
            },
            bindings,
        }
    }

    fn check_field_patterns(&mut self, fields: &[(Ident, PatternNode)], access: PlaceAccess) {
        for (_, pattern) in fields {
            let input = PatternInput::recovery(access, self.tc);
            self.check(pattern, input);
        }
    }

    fn check_bad_field_patterns(
        &mut self,
        fields: &[(Ident, PatternNode)],
        shape: &field_check::FieldShape,
        access: PlaceAccess,
    ) {
        for (index, (_, pattern)) in fields.iter().enumerate() {
            if shape.fields.iter().all(|field| field.index != index) {
                let input = PatternInput::recovery(access, self.tc);
                self.check(pattern, input);
            }
        }
    }

    fn struct_field_access(
        &self,
        owner_ty: &Type,
        field_name: Ident,
        receiver_access: PlaceAccess,
    ) -> PlaceAccess {
        if let Some(owner) = self.tc.extern_type_id(owner_ty) {
            let Some((_, decl)) = self.tc.extern_field(owner, field_name) else {
                return PlaceAccess::NotPlace;
            };
            return place::extern_field_access(receiver_access, decl.computed);
        }

        if self.tc.decls.key_for_type(owner_ty).is_some() {
            return place::projected_field_access(receiver_access);
        }

        receiver_access
    }

    fn check_field_shape(
        &mut self,
        fields: &[(Ident, PatternNode)],
        schema: &HashMap<Ident, FieldSchema>,
        owner: &field_check::FieldOwner,
        missing: field_check::MissingFields,
        span: Option<Span>,
    ) -> field_check::FieldShape {
        let uses = fields
            .iter()
            .enumerate()
            .map(|(index, (name, pattern))| field_check::FieldUse {
                name: *name,
                span: pattern.span,
                index,
            })
            .collect::<Vec<_>>();
        field_check::check(&uses, schema, owner, missing, span, self.tc)
    }

    fn check_enum_unit(
        &mut self,
        qualifier: Option<Ident>,
        variant: Ident,
        span: Span,
        expected: &Type,
    ) -> PatternOutcome {
        let Some(resolved) =
            enum_variant::resolve_pattern(self.tc, qualifier, variant, span, expected)
        else {
            return PatternOutcome::error();
        };
        match resolved.schema.payload {
            VariantPayload::Unit => PatternOutcome::refutable(PatternCover::EnumVariant {
                key: resolved.key,
                variant,
            }),
            VariantPayload::Tuple(_) | VariantPayload::Struct(_) => {
                enum_variant::push_shape_mismatch(self.tc, &resolved, VariantShape::Unit, span);
                PatternOutcome::error()
            }
        }
    }

    fn check_enum_tuple(
        &mut self,
        qualifier: Option<Ident>,
        variant: Ident,
        fields: &[PatternNode],
        span: Span,
        input: PatternInput,
    ) -> PatternCheckResult {
        let Some(resolved) =
            enum_variant::resolve_pattern(self.tc, qualifier, variant, span, &input.expected_ty)
        else {
            let bindings = self.check_tuple_fields_recovery(fields, input.access);
            return PatternCheckResult {
                outcome: PatternOutcome::error(),
                bindings,
            };
        };
        let VariantPayload::Tuple(payloads) = &resolved.schema.payload else {
            enum_variant::push_shape_mismatch(self.tc, &resolved, VariantShape::Tuple, span);
            let bindings = self.check_tuple_fields_recovery(fields, input.access);
            return PatternCheckResult {
                outcome: PatternOutcome::error(),
                bindings,
            };
        };
        if payloads.len() != fields.len() {
            self.tc.push_error(TypeError::WrongArgCount {
                expected: payloads.len(),
                found: fields.len(),
                span: self.tc.error_span(span),
            });
            let bindings = self.check_tuple_fields_recovery(fields, input.access);
            return PatternCheckResult {
                outcome: PatternOutcome::error(),
                bindings,
            };
        }
        let mut had_error = false;
        let mut bindings = BindingAlternatives::single_empty();
        for (index, (field, payload)) in fields.iter().zip(payloads).enumerate() {
            let ty = self.payload_ty(payload, &resolved, &input.expected_ty, span);
            let field_input = input.enum_tuple_field(variant, index, ty, self.tc);
            let result = self.check(field, field_input);
            had_error |= result.outcome.had_error;
            bindings = bindings.product(result.bindings, self.tc);
        }
        PatternCheckResult {
            outcome: PatternOutcome {
                cover: PatternCover::EnumVariant {
                    key: resolved.key,
                    variant,
                },
                had_error,
                refutability: Refutability::Refutable,
            },
            bindings,
        }
    }

    fn check_enum_struct(
        &mut self,
        qualifier: Option<Ident>,
        variant: Ident,
        fields: &[(Ident, PatternNode)],
        has_rest: bool,
        span: Span,
        input: PatternInput,
    ) -> PatternCheckResult {
        let Some(resolved) =
            enum_variant::resolve_pattern(self.tc, qualifier, variant, span, &input.expected_ty)
        else {
            self.check_field_patterns(fields, input.access);
            return PatternCheckResult::empty(PatternOutcome::error());
        };
        let VariantPayload::Struct(schema) = &resolved.schema.payload else {
            enum_variant::push_shape_mismatch(self.tc, &resolved, VariantShape::Struct, span);
            self.check_field_patterns(fields, input.access);
            return PatternCheckResult::empty(PatternOutcome::error());
        };

        let owner = field_check::FieldOwner::Variant {
            key: resolved.key.clone(),
            variant,
        };
        let shape = self.check_field_shape(
            fields,
            schema,
            &owner,
            field_check::MissingFields::AllowRest { has_rest },
            Some(span),
        );
        let mut had_error = shape.failed;
        let mut bindings = BindingAlternatives::single_empty();
        self.check_bad_field_patterns(fields, &shape, input.access);
        for field in shape.fields {
            self.tc.check_matched_field_access_policy(
                &owner,
                field.name,
                &field.policy,
                fields[field.index].1.span,
            );
            let ty = self.payload_ty(&field.ty, &resolved, &input.expected_ty, span);
            let field_input = input.enum_struct_field(variant, field.name, ty, self.tc);
            let result = self.check(&fields[field.index].1, field_input);
            had_error |= result.outcome.had_error;
            bindings = bindings.product(result.bindings, self.tc);
        }
        PatternCheckResult {
            outcome: PatternOutcome {
                cover: PatternCover::EnumVariant {
                    key: resolved.key,
                    variant,
                },
                had_error,
                refutability: Refutability::Refutable,
            },
            bindings,
        }
    }

    fn check_tuple_fields_recovery(
        &mut self,
        fields: &[PatternNode],
        access: PlaceAccess,
    ) -> BindingAlternatives {
        let mut bindings = BindingAlternatives::single_empty();
        for field in fields {
            let input = PatternInput::recovery(access, self.tc);
            let result = self.check(field, input);
            bindings = bindings.product(result.bindings, self.tc);
        }
        bindings
    }

    fn payload_ty(
        &mut self,
        ty: &Type,
        resolved: &enum_variant::ResolvedEnumVariant,
        expected: &Type,
        span: Span,
    ) -> Type {
        let Some(args) = self.expected_enum_args(resolved, expected) else {
            return if resolved.generics.is_empty() {
                ty.clone()
            } else {
                Type::Infer
            };
        };
        let (types, consts) = resolved.generics.substitutions(&args);
        self.tc.substitute_checked(ty, &types, &consts, span)
    }

    fn expected_enum_args(
        &self,
        resolved: &enum_variant::ResolvedEnumVariant,
        expected: &Type,
    ) -> Option<GenericArgs> {
        resolved.owner_args_from_expected(expected, self.tc)
    }

    fn extern_field_alias_facts(
        &self,
        owner_ty: &Type,
        field_name: Ident,
        receiver_facts: &PlaceUseFacts,
    ) -> Option<(PlaceUseFacts, bool)> {
        let owner = self.tc.extern_type_id(owner_ty)?;
        let (field, decl) = self.tc.extern_field(owner, field_name)?;
        Some((
            PlaceUseFacts::for_extern_field(receiver_facts, field),
            decl.ty.contains_any(),
        ))
    }

    fn record_extern_field_read(
        &mut self,
        field_name: Ident,
        pattern: &PatternNode,
        owner_ty: &Type,
    ) {
        let Some(site) = self.extern_site else {
            return;
        };
        let Some(owner) = self.tc.extern_type_id(owner_ty) else {
            return;
        };
        let Some((field, decl)) = self
            .tc
            .extern_field(owner, field_name)
            .map(|(id, decl)| (id, decl.clone()))
        else {
            return;
        };
        self.tc
            .record_extern_use(site, ExternUseTarget::FieldRead(field));
        self.tc
            .reject_extern_any_escape_fact(decl.ty.contains_any(), pattern.span);
    }

    fn unsupported_named(&mut self, pattern: &'static str, span: Span) -> PatternOutcome {
        self.tc.push_error(TypeError::UnsupportedPattern {
            pattern,
            span: self.tc.error_span(span),
        });
        PatternOutcome::error()
    }
}

pub(super) fn check(
    pattern: &PatternNode,
    expected: &Type,
    mode: PatternBindMode,
    context: PatternContext,
    tc: &mut TypeChecker,
) -> PatternOutcome {
    PatternChecker::new(tc, None, context, mode).check_root(pattern, expected)
}

pub(super) fn check_place_at(
    pattern: &PatternNode,
    place: PatternPlace,
    mode: PatternBindMode,
    site: ExprId,
    context: PatternContext,
    tc: &mut TypeChecker,
) -> PatternOutcome {
    let input = PatternInput::from_place(place);
    PatternChecker::new(tc, Some(site), context, mode).check_from_handle_root(pattern, input)
}
