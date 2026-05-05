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
    Unsupported,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Refutability {
    Irrefutable,
    Refutable,
    Unknown,
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

struct PatternChecker<'tc> {
    tc: &'tc mut TypeChecker,
    extern_site: Option<ExprId>,
    context: PatternContext,
}

impl<'tc> PatternChecker<'tc> {
    fn new(tc: &'tc mut TypeChecker, extern_site: Option<ExprId>, context: PatternContext) -> Self {
        Self {
            tc,
            extern_site,
            context,
        }
    }

    fn check_root(
        &mut self,
        pattern: &PatternNode,
        expected: &Type,
        mutable: bool,
    ) -> PatternOutcome {
        let mut outcome = self.check(pattern, expected, mutable);
        self.apply_context_policy(pattern, expected, &mut outcome);
        outcome
    }

    fn check_from_handle_root(
        &mut self,
        pattern: &PatternNode,
        expected_handle: TypeHandle,
        expected_ty: Type,
        mutable: bool,
    ) -> PatternOutcome {
        let mut outcome = match &pattern.node {
            Pattern::Ident(name) => {
                self.tc.define(*name, expected_ty.clone(), mutable);
                if let Some(type_id) = self.tc.lookup(*name).map(|info| info.type_id) {
                    self.tc
                        .set_local_type_from_handle(type_id, expected_handle.clone());
                    self.tc.expect_equal(
                        pattern.span,
                        self.tc.local_handle(type_id),
                        expected_handle,
                    );
                }
                PatternOutcome::irrefutable(PatternCover::CatchAll)
            }
            _ => self.check(pattern, &expected_ty, mutable),
        };
        self.apply_context_policy(pattern, &expected_ty, &mut outcome);
        outcome
    }

    fn apply_context_policy(
        &mut self,
        pattern: &PatternNode,
        expected: &Type,
        outcome: &mut PatternOutcome,
    ) {
        match self.context {
            PatternContext::IfLet | PatternContext::WhileLet
                if expected.is_option() && outcome.refutability == Refutability::Irrefutable =>
            {
                self.tc
                    .push_error(TypeError::RequiresUnwrappingPattern { span: pattern.span });
                outcome.had_error = true;
            }
            PatternContext::LetElse if outcome.refutability == Refutability::Irrefutable => {
                self.tc
                    .push_error(TypeError::IrrefutableLetElse { span: pattern.span });
                outcome.had_error = true;
            }
            _ => {}
        }
    }

    fn check(&mut self, pattern: &PatternNode, expected: &Type, mutable: bool) -> PatternOutcome {
        match &pattern.node {
            Pattern::Ident(name) => {
                self.tc.define(*name, expected.clone(), mutable);
                PatternOutcome::irrefutable(PatternCover::CatchAll)
            }
            Pattern::Wildcard => PatternOutcome::irrefutable(PatternCover::CatchAll),
            Pattern::VarIdent(_) => {
                let mut outcome = self.unsupported(pattern);
                outcome.refutability = Refutability::Irrefutable;
                outcome
            }
            Pattern::Tuple(elems) => self.check_tuple(elems, pattern.span, expected, mutable),
            Pattern::Lit(lit) => self.check_lit(pattern.span, lit, expected),
            Pattern::Nil => self.check_nil(pattern.span, expected),
            Pattern::Optional(inner) => self.check_optional(inner, expected, mutable),
            Pattern::Range { start, end, .. } => {
                self.check_range(pattern.span, start.clone(), end.clone(), expected)
            }
            Pattern::Or(_) => {
                self.tc
                    .push_error(TypeError::OrPatternUnsupported { span: pattern.span });
                PatternOutcome::error()
            }
            Pattern::Rest => self.unsupported_named("..", pattern.span),
            Pattern::Struct { name, fields } => {
                self.check_struct(*name, fields, pattern.span, expected, mutable)
            }
            Pattern::EnumUnit { qualifier, variant } => {
                self.check_enum_unit(Some(*qualifier), *variant, pattern.span, expected)
            }
            Pattern::InferredEnumUnit { variant } => {
                self.check_enum_unit(None, *variant, pattern.span, expected)
            }
            Pattern::EnumTuple {
                qualifier,
                variant,
                fields,
            } => self.check_enum_tuple(
                Some(*qualifier),
                *variant,
                fields,
                pattern.span,
                expected,
                mutable,
            ),
            Pattern::InferredEnumTuple { variant, fields } => {
                self.check_enum_tuple(None, *variant, fields, pattern.span, expected, mutable)
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
                expected,
                mutable,
            ),
            Pattern::InferredEnumStruct {
                variant,
                fields,
                has_rest,
            } => self.check_enum_struct(
                None,
                *variant,
                fields,
                *has_rest,
                pattern.span,
                expected,
                mutable,
            ),
        }
    }

    fn check_tuple(
        &mut self,
        elems: &[PatternNode],
        span: Span,
        expected: &Type,
        mutable: bool,
    ) -> PatternOutcome {
        let elem_tys = match expected {
            Type::Tuple(tys) => tys.clone(),
            Type::Infer => vec![Type::Infer; elems.len()],
            _ => {
                self.tc.push_error(TypeError::TuplePatternOnNonTuple {
                    ty: expected.clone(),
                    span,
                });
                return PatternOutcome::error();
            }
        };
        if elems.len() != elem_tys.len() {
            self.tc.push_error(TypeError::TuplePatternArityMismatch {
                expected: elem_tys.len(),
                found: elems.len(),
                span,
            });
            return PatternOutcome::error();
        }
        let mut had_error = false;
        let mut refutability = Refutability::Irrefutable;
        let covers = elems
            .iter()
            .zip(elem_tys.iter())
            .map(|(elem, elem_ty)| {
                let outcome = self.check(elem, elem_ty, mutable);
                had_error |= outcome.had_error;
                refutability = combine_refutability(refutability, outcome.refutability);
                outcome.cover
            })
            .collect();
        PatternOutcome {
            cover: PatternCover::Tuple(covers),
            had_error,
            refutability,
        }
    }

    fn check_lit(&mut self, span: Span, lit: &Lit, expected: &Type) -> PatternOutcome {
        let lit_ty = type_from_lit(lit);
        if lit_ty != *expected && !matches!(expected, Type::Infer) {
            self.tc.push_error(TypeError::InvalidLiteralPattern {
                expected: expected.clone(),
                found: lit_ty,
                span,
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
            self.tc
                .push_error(TypeError::OptionalPatternOnNonOptional { span });
            return PatternOutcome::error();
        }
        PatternOutcome::refutable(
            self.option_cover(expected, "None")
                .unwrap_or(PatternCover::CatchAll),
        )
    }

    fn check_optional(
        &mut self,
        inner: &PatternNode,
        expected: &Type,
        mutable: bool,
    ) -> PatternOutcome {
        if matches!(inner.node, Pattern::Optional(_)) {
            self.tc
                .push_error(TypeError::NestedOptionalPattern { span: inner.span });
            self.check(inner, &Type::Infer, mutable);
            return PatternOutcome::error();
        }
        let Some(inner_ty) = expected.option_inner() else {
            if !matches!(expected, Type::Infer) {
                self.tc
                    .push_error(TypeError::OptionalPatternOnNonOptional { span: inner.span });
            }
            self.check(inner, &Type::Infer, mutable);
            return PatternOutcome::error();
        };
        let outcome = self.check(inner, inner_ty, mutable);
        PatternOutcome {
            cover: self.option_cover(expected, "Some").unwrap_or(outcome.cover),
            had_error: outcome.had_error,
            refutability: Refutability::Refutable,
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
                    span,
                });
                return PatternOutcome::error();
            }
        }
        self.unsupported_named("range", span)
    }

    fn check_struct(
        &mut self,
        name: Ident,
        fields: &[(Ident, PatternNode)],
        span: Span,
        expected: &Type,
        mutable: bool,
    ) -> PatternOutcome {
        let Some(key) = self.tc.resolve_visible_type_key(None, name) else {
            self.tc.push_error(TypeError::UnknownType {
                qualifier: None,
                name,
                span,
            });
            self.check_field_patterns(fields, mutable);
            return PatternOutcome::error();
        };

        let expected_key = self.tc.decls.key_for_type(expected);
        if expected_key.as_ref() != Some(&key) && !matches!(expected, Type::Infer) {
            self.tc.push_error(TypeError::TypeMismatch {
                expected: nominal_type(&key),
                found: expected.clone(),
                span,
            });
            return PatternOutcome::error();
        }

        match key.kind {
            NominalKind::Struct | NominalKind::DataRef => {
                let Some(agg) = self.tc.decls.aggregate(&key).cloned() else {
                    return PatternOutcome::error();
                };
                self.check_struct_fields(fields, nominal_type(&key), &agg.fields, mutable)
            }
            NominalKind::Extern => {
                let Some(owner) = self.tc.externs.type_by_nominal(&key) else {
                    return PatternOutcome::error();
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
                            },
                        )
                    })
                    .collect();
                self.check_struct_fields(fields, nominal_type(&key), &field_schema, mutable)
            }
            NominalKind::Enum => self.unsupported_named("Struct", span),
        }
    }

    fn check_struct_fields(
        &mut self,
        fields: &[(Ident, PatternNode)],
        owner_ty: Type,
        schema: &HashMap<Ident, FieldSchema>,
        mutable: bool,
    ) -> PatternOutcome {
        let shape = self.check_field_shape(
            fields,
            schema,
            field_check::FieldOwner::Nominal(owner_ty.clone()),
            field_check::MissingFields::None,
            Span::new(0, 0),
        );
        let mut had_error = shape.failed;
        self.check_bad_field_patterns(fields, &shape, mutable);
        for field in shape.fields {
            let pattern = &fields[field.index].1;
            self.record_extern_field_read(field.name, pattern, &owner_ty);
            had_error |= self.check(pattern, &field.ty, mutable).had_error;
        }
        PatternOutcome {
            cover: PatternCover::CatchAll,
            had_error,
            refutability: Refutability::Refutable,
        }
    }

    fn check_field_patterns(&mut self, fields: &[(Ident, PatternNode)], mutable: bool) {
        for (_, pattern) in fields {
            self.check(pattern, &Type::Infer, mutable);
        }
    }

    fn check_bad_field_patterns(
        &mut self,
        fields: &[(Ident, PatternNode)],
        shape: &field_check::FieldShape,
        mutable: bool,
    ) {
        for (index, (_, pattern)) in fields.iter().enumerate() {
            if shape.fields.iter().all(|field| field.index != index) {
                self.check(pattern, &Type::Infer, mutable);
            }
        }
    }

    fn check_field_shape(
        &mut self,
        fields: &[(Ident, PatternNode)],
        schema: &HashMap<Ident, FieldSchema>,
        owner: field_check::FieldOwner,
        missing: field_check::MissingFields,
        span: Span,
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
        match resolved.schema {
            VariantSchema::Unit => PatternOutcome::refutable(PatternCover::EnumVariant {
                key: resolved.key,
                variant,
            }),
            VariantSchema::Tuple(_) | VariantSchema::Struct(_) => {
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
        expected: &Type,
        mutable: bool,
    ) -> PatternOutcome {
        let Some(resolved) =
            enum_variant::resolve_pattern(self.tc, qualifier, variant, span, expected)
        else {
            self.check_tuple_fields_recovery(fields, mutable);
            return PatternOutcome::error();
        };
        let VariantSchema::Tuple(payloads) = &resolved.schema else {
            enum_variant::push_shape_mismatch(self.tc, &resolved, VariantShape::Tuple, span);
            self.check_tuple_fields_recovery(fields, mutable);
            return PatternOutcome::error();
        };
        if payloads.len() != fields.len() {
            self.tc.push_error(TypeError::WrongArgCount {
                expected: payloads.len(),
                found: fields.len(),
                span,
            });
            self.check_tuple_fields_recovery(fields, mutable);
            return PatternOutcome::error();
        }
        let mut had_error = false;
        for (field, payload) in fields.iter().zip(payloads) {
            let ty = self.payload_ty(payload, &resolved, expected, span);
            had_error |= self.check(field, &ty, mutable).had_error;
        }
        PatternOutcome {
            cover: PatternCover::EnumVariant {
                key: resolved.key,
                variant,
            },
            had_error,
            refutability: Refutability::Refutable,
        }
    }

    fn check_enum_struct(
        &mut self,
        qualifier: Option<Ident>,
        variant: Ident,
        fields: &[(Ident, PatternNode)],
        has_rest: bool,
        span: Span,
        expected: &Type,
        mutable: bool,
    ) -> PatternOutcome {
        let Some(resolved) =
            enum_variant::resolve_pattern(self.tc, qualifier, variant, span, expected)
        else {
            self.check_field_patterns(fields, mutable);
            return PatternOutcome::error();
        };
        let VariantSchema::Struct(schema) = &resolved.schema else {
            enum_variant::push_shape_mismatch(self.tc, &resolved, VariantShape::Struct, span);
            self.check_field_patterns(fields, mutable);
            return PatternOutcome::error();
        };

        let shape = self.check_field_shape(
            fields,
            schema,
            field_check::FieldOwner::Variant {
                enum_name: resolved.key.name,
                variant,
            },
            field_check::MissingFields::AllowRest { has_rest },
            span,
        );
        let mut had_error = shape.failed;
        self.check_bad_field_patterns(fields, &shape, mutable);
        for field in shape.fields {
            let ty = self.payload_ty(&field.ty, &resolved, expected, span);
            had_error |= self.check(&fields[field.index].1, &ty, mutable).had_error;
        }
        PatternOutcome {
            cover: PatternCover::EnumVariant {
                key: resolved.key,
                variant,
            },
            had_error,
            refutability: Refutability::Refutable,
        }
    }

    fn check_tuple_fields_recovery(&mut self, fields: &[PatternNode], mutable: bool) {
        for field in fields {
            self.check(field, &Type::Infer, mutable);
        }
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

    fn unsupported(&mut self, pattern: &PatternNode) -> PatternOutcome {
        self.unsupported_named(pattern.node.variant_name(), pattern.span)
    }

    fn unsupported_named(&mut self, pattern: &'static str, span: Span) -> PatternOutcome {
        self.tc
            .push_error(TypeError::UnsupportedPattern { pattern, span });
        PatternOutcome::error()
    }
}

pub(super) fn check(
    pattern: &PatternNode,
    expected: &Type,
    mutable: bool,
    context: PatternContext,
    tc: &mut TypeChecker,
) -> PatternOutcome {
    PatternChecker::new(tc, None, context).check_root(pattern, expected, mutable)
}

pub(super) fn check_at(
    pattern: &PatternNode,
    expected: &Type,
    mutable: bool,
    site: ExprId,
    context: PatternContext,
    tc: &mut TypeChecker,
) -> PatternOutcome {
    PatternChecker::new(tc, Some(site), context).check_root(pattern, expected, mutable)
}

pub(super) fn check_handle_at(
    pattern: &PatternNode,
    expected_handle: TypeHandle,
    expected_ty: Type,
    mutable: bool,
    site: ExprId,
    context: PatternContext,
    tc: &mut TypeChecker,
) -> PatternOutcome {
    PatternChecker::new(tc, Some(site), context).check_from_handle_root(
        pattern,
        expected_handle,
        expected_ty,
        mutable,
    )
}
