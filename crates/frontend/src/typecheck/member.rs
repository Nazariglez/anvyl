use super::{
    CallableRef, DeclarationIndex, Exposure, ExtendMethodMatch, ExtendMethodSchema, ExtendSchema,
    GenericArgs, GenericParams, MethodKey, MethodMode, MethodSurface, ModuleScope, PromotedAlias,
    ResolvedNominal, SurfaceSlot, TypeChecker,
    annotation::AccessPolicy,
    extend_target::{
        ExtendTargetPattern, MatchedTargetPattern, match_exact_target, most_specific_target_match,
        permits_receiver_conversion,
    },
    generic_template_type, match_generic_template_args,
    place::{self, PlaceAccess},
};
use crate::{
    ast::{Ident, Type},
    externs::catalog::{ExternFieldRef, ExternMethodRef, ExternTypeId, ResolvedExternSignature},
};

#[derive(Clone)]
pub(super) struct FieldAccess {
    pub(super) ty: Type,
    pub(super) policy: AccessPolicy,
    pub(super) origin: ModuleScope,
}

#[derive(Clone)]
pub(super) struct ExternFieldAccess {
    pub(super) field_ref: ExternFieldRef,
    pub(super) ty: Type,
    pub(super) contains_any: bool,
    pub(super) access: PlaceAccess,
}

pub(super) struct PromotedFieldAccess {
    pub(super) path: Vec<Ident>,
    pub(super) origin_owner: Type,
    pub(super) origin_field: Ident,
    pub(super) exposure: Exposure,
    pub(super) target: PromotedFieldTarget,
}

pub(super) enum PromotedFieldTarget {
    Aggregate(FieldAccess),
    Extern(ExternFieldAccess),
}

#[derive(Clone, Copy)]
enum PromotedMemberKind {
    Field,
    Method,
}

struct PromotedMember {
    alias: PromotedAlias,
    origin_owner: Type,
    origin_member: Ident,
}

pub(super) enum FieldResolution {
    Direct(FieldAccess),
    Promoted(PromotedFieldAccess),
    AmbiguousPromoted {
        ty: Type,
        name: Ident,
        candidates: Vec<Vec<Ident>>,
    },
    Extern(ExternFieldAccess),
    StaticOnValue {
        ty: Type,
    },
    Missing {
        ty: Type,
    },
    NonAggregate {
        ty: Type,
    },
}

#[derive(Clone)]
pub(super) struct MethodAccess {
    pub(super) callee: CallableRef,
    pub(super) mode: MethodMode,
    pub(super) policy: AccessPolicy,
    pub(super) origin: ModuleScope,
}

#[derive(Clone)]
pub(super) struct ExtendMethodAccess {
    pub(super) callee: CallableRef,
    pub(super) mode: MethodMode,
    pub(super) extend: ExtendSchema,
    pub(super) method: ExtendMethodSchema,
}

#[derive(Clone)]
pub(super) struct ExternMethodAccess {
    pub(super) method_ref: ExternMethodRef,
    pub(super) receiver: anvyx_externs::ReceiverMode,
    pub(super) name: Ident,
    pub(super) signature: ResolvedExternSignature,
}

#[derive(Clone)]
pub(super) struct PromotedMethodAccess {
    pub(super) path: Vec<Ident>,
    pub(super) origin_owner: Type,
    pub(super) origin_method: Ident,
    pub(super) exposure: Exposure,
    pub(super) target: PromotedMethodTarget,
}

#[derive(Clone)]
pub(super) enum PromotedMethodTarget {
    Aggregate(Box<MethodAccess>),
    Extern(ExternMethodAccess),
}

pub(super) enum MethodResolution {
    Direct(Box<MethodAccess>),
    Extend(Box<ExtendMethodAccess>),
    Promoted(Box<PromotedMethodAccess>),
    AmbiguousPromoted {
        ty: Type,
        name: Ident,
        candidates: Vec<Vec<Ident>>,
    },
    Extern(Box<ExternMethodAccess>),
    StaticOnValue {
        ty: Type,
    },
    ExtendError(ExtendMethodError),
    Missing {
        ty: Type,
    },
    NonAggregate {
        ty: Type,
    },
}

pub(super) enum ExtendMethodError {
    Unbound(Vec<Ident>),
    Ambiguous { receiver: Type, name: Ident },
}

impl TypeChecker {
    pub(super) fn extend_visible(&self, extend: &ExtendSchema) -> bool {
        Self::extend_visible_in(&self.decls, &self.current_module, extend)
    }

    pub(super) fn extend_visible_in(
        decls: &DeclarationIndex,
        current_module: &ModuleScope,
        extend: &ExtendSchema,
    ) -> bool {
        extend.origin == *current_module
            || (extend.exported && decls.imports_module(current_module, &extend.origin))
    }

    pub(super) fn find_extend_method(
        &self,
        receiver: &Type,
        name: Ident,
    ) -> Option<ExtendMethodMatch<'_>> {
        self.decls
            .find_extend_method(MethodSurface::Instance, receiver, name, |ext| {
                self.extend_visible(ext)
            })
    }

    pub(super) fn find_static_extend_method(
        &self,
        target: &Type,
        name: Ident,
    ) -> Option<ExtendMethodMatch<'_>> {
        self.decls
            .find_extend_method(MethodSurface::Static, target, name, |ext| {
                self.extend_visible(ext)
            })
    }
}

impl DeclarationIndex {
    pub(crate) fn find_extend_method<F>(
        &self,
        surface: MethodSurface,
        subject: &Type,
        name: Ident,
        mut visible: F,
    ) -> Option<ExtendMethodMatch<'_>>
    where
        F: FnMut(&ExtendSchema) -> bool,
    {
        let method_key = MethodKey::new(name, surface);
        let candidates = self
            .extends()
            .filter(|ext| visible(ext))
            .filter_map(|ext| extend_candidate(self, ext, method_key, subject))
            .collect::<Vec<_>>();

        select_extend_candidate(candidates)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ExtendReceiverMatch {
    Exact,
    SliceView,
}

struct ExtendCandidate<'a> {
    extend: &'a ExtendSchema,
    method: &'a ExtendMethodSchema,
    target: Type,
    receiver_ty: Type,
    owner_args: Result<GenericArgs, Vec<Ident>>,
    receiver_match: ExtendReceiverMatch,
}

fn extend_candidate<'a>(
    decls: &DeclarationIndex,
    ext: &'a ExtendSchema,
    method_key: MethodKey,
    subject: &Type,
) -> Option<ExtendCandidate<'a>> {
    let method = ext.methods.get(&method_key)?;
    let pattern = ExtendTargetPattern::from(ext);
    let static_nominal_family = method_key.surface == MethodSurface::Static;
    if let Some(target_match) = match_exact_target(decls, &pattern, subject, static_nominal_family)
    {
        return Some(ExtendCandidate {
            extend: ext,
            method,
            target: target_match.templated_target,
            receiver_ty: target_match.receiver_ty,
            owner_args: target_match.owner_args,
            receiver_match: ExtendReceiverMatch::Exact,
        });
    }

    if !permits_receiver_conversion(&pattern) || method_key.surface != MethodSurface::Instance {
        return None;
    }
    let target = generic_template_type(&ext.target, &ext.generics);
    let (receiver_ty, owner_args) = slice_view_receiver_match(&ext.generics, &target, subject)?;
    Some(ExtendCandidate {
        extend: ext,
        method,
        target,
        receiver_ty,
        owner_args,
        receiver_match: ExtendReceiverMatch::SliceView,
    })
}

fn slice_view_receiver_match(
    generics: &GenericParams,
    target: &Type,
    subject: &Type,
) -> Option<(Type, Result<GenericArgs, Vec<Ident>>)> {
    let Type::Slice { elem: target_elem } = target else {
        return None;
    };
    let (Type::List { elem: subject_elem }
    | Type::Array {
        elem: subject_elem, ..
    }) = subject
    else {
        return None;
    };
    let receiver_ty = Type::Slice {
        elem: subject_elem.clone(),
    };
    let owner_args = match_generic_template_args(generics, target_elem, subject_elem)?;
    Some((receiver_ty, owner_args))
}

fn select_extend_candidate(
    mut candidates: Vec<ExtendCandidate<'_>>,
) -> Option<ExtendMethodMatch<'_>> {
    if candidates
        .iter()
        .any(|candidate| candidate.receiver_match == ExtendReceiverMatch::Exact)
    {
        candidates.retain(|candidate| candidate.receiver_match == ExtendReceiverMatch::Exact);
    }

    match candidates.len() {
        0 => None,
        1 => {
            let candidate = candidates.pop().expect("one extend candidate");
            Some(ExtendMethodMatch::Match {
                extend: candidate.extend,
                method: candidate.method,
                receiver_ty: candidate.receiver_ty,
                owner_args: candidate.owner_args,
            })
        }
        _ => Some(most_specific_extend(candidates)),
    }
}

fn most_specific_extend(mut candidates: Vec<ExtendCandidate<'_>>) -> ExtendMethodMatch<'_> {
    let target_matches = candidates
        .iter()
        .map(|candidate| MatchedTargetPattern {
            pattern: ExtendTargetPattern::from(candidate.extend),
            target: &candidate.target,
        })
        .collect::<Vec<_>>();
    let Some(winner) = most_specific_target_match(&target_matches) else {
        return ExtendMethodMatch::Ambiguous;
    };

    let candidate = candidates.swap_remove(winner);
    ExtendMethodMatch::Match {
        extend: candidate.extend,
        method: candidate.method,
        receiver_ty: candidate.receiver_ty,
        owner_args: candidate.owner_args,
    }
}

pub(super) fn resolve_field(
    receiver: &Type,
    name: Ident,
    receiver_access: PlaceAccess,
    tc: &mut TypeChecker,
) -> FieldResolution {
    let Some(nominal) = tc.resolve_nominal(receiver) else {
        return FieldResolution::NonAggregate {
            ty: receiver.clone(),
        };
    };
    let owner_ty = nominal.surface_ty();
    let ResolvedNominal::Aggregate(agg) = nominal else {
        if let ResolvedNominal::Extern { id, .. } = nominal {
            return match resolve_extern_field(id, name, receiver_access, tc) {
                Some(field) => FieldResolution::Extern(field),
                None => FieldResolution::Missing { ty: owner_ty },
            };
        }
        return FieldResolution::Missing { ty: owner_ty };
    };
    let key = &agg.key;
    let Some(field) = agg.fields.get(name) else {
        let has_static = agg.methods.contains_key(&MethodKey::static_(name));
        if let Some(promoted) = resolve_promoted_field(receiver, name, receiver_access, tc) {
            return promoted;
        }
        if has_static || tc.find_static_extend_method(receiver, name).is_some() {
            return FieldResolution::StaticOnValue {
                ty: receiver.clone(),
            };
        }
        return FieldResolution::Missing {
            ty: receiver.clone(),
        };
    };

    FieldResolution::Direct(FieldAccess {
        ty: super::substitute_aggregate_member(receiver, &agg.generics, &field.ty),
        policy: field.policy.clone(),
        origin: key.module.clone(),
    })
}

fn resolve_promoted_field(
    receiver: &Type,
    name: Ident,
    receiver_access: PlaceAccess,
    tc: &mut TypeChecker,
) -> Option<FieldResolution> {
    let member = match resolve_promoted_member(receiver, name, PromotedMemberKind::Field, tc)? {
        Ok(member) => member,
        Err(candidates) => {
            return Some(FieldResolution::AmbiguousPromoted {
                ty: receiver.clone(),
                name,
                candidates,
            });
        }
    };
    let alias = member.alias;
    let origin_owner = member.origin_owner;
    let origin_field = member.origin_member;
    if let Some(owner) = tc.extern_type_id(&origin_owner) {
        let field = resolve_extern_field(owner, origin_field, receiver_access, tc)?;
        return Some(FieldResolution::Promoted(PromotedFieldAccess {
            path: alias.path.clone(),
            origin_owner,
            origin_field,
            exposure: alias.exposure,
            target: PromotedFieldTarget::Extern(field),
        }));
    }

    let key = tc.decls.key_for_type(&origin_owner)?;
    let origin = tc.decls.aggregate(&key)?;
    let field = origin.fields.get(origin_field)?;
    Some(FieldResolution::Promoted(PromotedFieldAccess {
        path: alias.path.clone(),
        origin_owner: origin_owner.clone(),
        origin_field,
        exposure: alias.exposure,
        target: PromotedFieldTarget::Aggregate(FieldAccess {
            ty: super::substitute_aggregate_member(&origin_owner, &origin.generics, &field.ty),
            policy: field.policy.clone(),
            origin: key.module.clone(),
        }),
    }))
}

pub(super) fn promoted_field_paths(
    receiver: &Type,
    name: Ident,
    tc: &mut TypeChecker,
) -> Option<Vec<Vec<Ident>>> {
    let surface = tc.promoted_surface_for(receiver)?;
    Some(
        surface
            .fields
            .get(&name)?
            .aliases
            .iter()
            .map(|alias| alias.path.clone())
            .collect(),
    )
}

pub(super) fn resolve_method(
    receiver: &Type,
    name: Ident,
    tc: &mut TypeChecker,
) -> MethodResolution {
    let mut static_method_on_value = false;
    let mut aggregate_ty = None;

    if let Some(nominal) = tc.resolve_nominal(receiver) {
        aggregate_ty = Some(nominal.surface_ty());
        match nominal {
            ResolvedNominal::Aggregate(agg) => {
                if let Some(method) = agg.methods.get(&MethodKey::instance(name)) {
                    return MethodResolution::Direct(Box::new(MethodAccess {
                        callee: tc.decls.callable_for_aggregate_method(
                            agg,
                            name,
                            method,
                            receiver.clone(),
                        ),
                        mode: method.mode,
                        policy: method.policy.clone(),
                        origin: agg.key.module.clone(),
                    }));
                }
                static_method_on_value |= agg.methods.contains_key(&MethodKey::static_(name));
            }
            ResolvedNominal::Extern { id, .. } => {
                if let Some(method) = resolve_extern_method(id, name, tc) {
                    return MethodResolution::Extern(Box::new(method));
                }
                static_method_on_value = tc.externs.static_method(id, name).is_some();
            }
            ResolvedNominal::Enum { .. } => {}
        }
    }

    if let Some(matched) = tc.find_extend_method(receiver, name) {
        return match extend_method_parts(receiver.clone(), name, &matched) {
            Ok((extend, method, receiver_ty, owner_args)) => {
                MethodResolution::Extend(Box::new(ExtendMethodAccess {
                    callee: tc.decls.callable_for_extend_method(
                        receiver_ty,
                        extend,
                        name,
                        method,
                        owner_args,
                    ),
                    mode: method.mode,
                    extend: extend.clone(),
                    method: method.clone(),
                }))
            }
            Err(error) => MethodResolution::ExtendError(error),
        };
    }

    if let Some(promoted) = resolve_promoted_method(receiver, name, tc) {
        return promoted;
    }

    static_method_on_value |= tc.find_static_extend_method(receiver, name).is_some();
    if static_method_on_value {
        return MethodResolution::StaticOnValue {
            ty: receiver.clone(),
        };
    }

    match aggregate_ty {
        Some(ty) => MethodResolution::Missing { ty },
        None => MethodResolution::NonAggregate {
            ty: receiver.clone(),
        },
    }
}

fn resolve_promoted_method(
    receiver: &Type,
    name: Ident,
    tc: &mut TypeChecker,
) -> Option<MethodResolution> {
    let member = match resolve_promoted_member(receiver, name, PromotedMemberKind::Method, tc)? {
        Ok(member) => member,
        Err(candidates) => {
            return Some(MethodResolution::AmbiguousPromoted {
                ty: receiver.clone(),
                name,
                candidates,
            });
        }
    };
    let alias = member.alias;
    let origin_owner = member.origin_owner;
    let origin_method = member.origin_member;
    if let Some(owner) = tc.extern_type_id(&origin_owner) {
        let method = resolve_extern_method(owner, origin_method, tc)?;
        return Some(MethodResolution::Promoted(Box::new(PromotedMethodAccess {
            path: alias.path.clone(),
            origin_owner,
            origin_method,
            exposure: alias.exposure,
            target: PromotedMethodTarget::Extern(method),
        })));
    }

    let key = tc.decls.key_for_type(&origin_owner)?;
    let origin = tc.decls.aggregate(&key)?;
    let method = origin.methods.get(&MethodKey::instance(origin_method))?;
    Some(MethodResolution::Promoted(Box::new(PromotedMethodAccess {
        path: alias.path.clone(),
        origin_owner: origin_owner.clone(),
        origin_method,
        exposure: alias.exposure,
        target: PromotedMethodTarget::Aggregate(Box::new(MethodAccess {
            callee: tc.decls.callable_for_aggregate_method(
                origin,
                origin_method,
                method,
                origin_owner.clone(),
            ),
            mode: method.mode,
            policy: method.policy.clone(),
            origin: key.module.clone(),
        })),
    })))
}

fn resolve_promoted_member(
    receiver: &Type,
    name: Ident,
    kind: PromotedMemberKind,
    tc: &mut TypeChecker,
) -> Option<Result<PromotedMember, Vec<Vec<Ident>>>> {
    let surface = tc.promoted_surface_for(receiver)?;
    let slot = match kind {
        PromotedMemberKind::Field => surface.fields.get(&name)?,
        PromotedMemberKind::Method => surface.methods.get(&name)?,
    };
    let alias = match single_promoted_alias(slot) {
        Ok(alias) => alias.clone(),
        Err(candidates) => return Some(Err(candidates)),
    };
    Some(Ok(PromotedMember {
        origin_owner: alias.origin.0.clone(),
        origin_member: alias.origin_member,
        alias,
    }))
}

fn single_promoted_alias(
    slot: &SurfaceSlot<PromotedAlias>,
) -> Result<&PromotedAlias, Vec<Vec<Ident>>> {
    match slot.aliases.as_slice() {
        [alias] if !slot.ambiguous => Ok(alias),
        aliases => Err(aliases.iter().map(|alias| alias.path.clone()).collect()),
    }
}

fn resolve_extern_field(
    owner: ExternTypeId,
    name: Ident,
    receiver_access: PlaceAccess,
    tc: &TypeChecker,
) -> Option<ExternFieldAccess> {
    let (field_ref, decl) = tc.extern_field(owner, name)?;
    Some(ExternFieldAccess {
        field_ref,
        ty: decl.ty.ty.clone(),
        contains_any: decl.ty.contains_any(),
        access: place::extern_field_access(receiver_access, decl.computed, decl.writable),
    })
}

fn resolve_extern_method(
    owner: ExternTypeId,
    name: Ident,
    tc: &TypeChecker,
) -> Option<ExternMethodAccess> {
    let (method_ref, decl) = tc.externs.method(owner, name)?;
    Some(ExternMethodAccess {
        method_ref,
        receiver: decl.receiver,
        name: decl.name,
        signature: decl.signature.clone(),
    })
}

pub(super) fn extend_method_parts<'a>(
    receiver: Type,
    name: Ident,
    matched: &'a ExtendMethodMatch<'a>,
) -> Result<(&'a ExtendSchema, &'a ExtendMethodSchema, Type, GenericArgs), ExtendMethodError> {
    match matched {
        ExtendMethodMatch::Match {
            extend,
            method,
            receiver_ty,
            owner_args: Ok(owner_args),
        } => Ok((extend, method, receiver_ty.clone(), owner_args.clone())),
        ExtendMethodMatch::Match {
            owner_args: Err(unbound),
            ..
        } => Err(ExtendMethodError::Unbound(unbound.clone())),
        ExtendMethodMatch::Ambiguous => Err(ExtendMethodError::Ambiguous { receiver, name }),
    }
}
