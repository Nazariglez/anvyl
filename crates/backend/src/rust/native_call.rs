use anvyx_externs::{RustParamAdapter, RustReturnAdapter};

use super::rir::{
    RirCoreEnumKind, RirNativeParam, RirNativeReturn, RirParamEscape, RirPassMode, RirProgram,
    RirType, RirTypeId,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum NativeArgAction {
    Direct,
    SnapshotString,
    NativeRefBorrow { mutable: bool },
    RejectLiveBoundary,
}

impl NativeArgAction {
    pub(super) fn native_ref_borrow_mutability(self) -> Option<bool> {
        match self {
            Self::NativeRefBorrow { mutable } => Some(mutable),
            Self::Direct | Self::SnapshotString | Self::RejectLiveBoundary => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NativeReentryPolicy {
    Safe,
    SnapshotStringBorrow,
    UnsupportedLiveBoundary,
}

pub(super) fn plan_param(
    program: &RirProgram,
    ty: RirTypeId,
    adapter: &RustParamAdapter,
) -> RirNativeParam {
    match adapter {
        RustParamAdapter::Value => plan_value_param(program, ty),
        RustParamAdapter::OwnedNamed => RirNativeParam::OwnedNamed,
        RustParamAdapter::Borrow => RirNativeParam::Borrow,
        RustParamAdapter::MutBorrow => RirNativeParam::MutBorrow,
        RustParamAdapter::MutPlace => RirNativeParam::MutPlace,
        RustParamAdapter::ScopedLambda => RirNativeParam::ScopedLambda,
        RustParamAdapter::EscapingLambda => RirNativeParam::EscapingLambda,
        RustParamAdapter::AnvCallback => RirNativeParam::AnvCallback,
        RustParamAdapter::InitField(inner) => {
            RirNativeParam::InitField(Box::new(plan_param(program, ty, inner)))
        }
        RustParamAdapter::Option(inner) => {
            let payload_ty =
                option_payload_ty(program, ty).expect("attested native option parameter");
            RirNativeParam::Option(Box::new(plan_param(program, payload_ty, inner)))
        }
        RustParamAdapter::Result(ok, err) => {
            let (ok_ty, err_ty) =
                result_payload_tys(program, ty).expect("attested native result parameter");
            RirNativeParam::Result(
                Box::new(plan_param(program, ok_ty, ok)),
                Box::new(plan_param(program, err_ty, err)),
            )
        }
        RustParamAdapter::Slice(inner) => {
            let elem_ty = slice_elem_ty(program, ty).expect("attested native slice parameter");
            RirNativeParam::Slice(Box::new(plan_param(program, elem_ty, inner)))
        }
    }
}

fn plan_value_param(program: &RirProgram, ty: RirTypeId) -> RirNativeParam {
    if native_ref_ty(program, ty) {
        return RirNativeParam::SharedNamed;
    }
    match program.types.get(ty.index()) {
        Some(RirType::Option(inner)) => {
            RirNativeParam::Option(Box::new(plan_value_param(program, *inner)))
        }
        Some(RirType::Array { elem, .. }) => {
            RirNativeParam::Array(Box::new(plan_value_param(program, *elem)))
        }
        _ => match result_payload_tys(program, ty) {
            Some((ok, err)) => RirNativeParam::Result(
                Box::new(plan_value_param(program, ok)),
                Box::new(plan_value_param(program, err)),
            ),
            None => RirNativeParam::Value,
        },
    }
}

fn option_payload_ty(program: &RirProgram, ty: RirTypeId) -> Option<RirTypeId> {
    let RirType::Option(payload) = program.types.get(ty.index())? else {
        return None;
    };
    Some(*payload)
}

fn slice_elem_ty(program: &RirProgram, ty: RirTypeId) -> Option<RirTypeId> {
    let RirType::Slice(elem) = program.types.get(ty.index())? else {
        return None;
    };
    Some(*elem)
}

fn result_payload_tys(program: &RirProgram, ty: RirTypeId) -> Option<(RirTypeId, RirTypeId)> {
    let RirType::Enum(id) = program.types.get(ty.index())? else {
        return None;
    };
    let enm = program.enums.get(id.index())?;
    let [ok, err] = enm.variants.as_slice() else {
        return None;
    };
    let [ok_field] = ok.fields.as_slice() else {
        return None;
    };
    let [err_field] = err.fields.as_slice() else {
        return None;
    };
    (enm.core == Some(RirCoreEnumKind::Result)).then_some((ok_field.ty, err_field.ty))
}

fn native_ref_ty(program: &RirProgram, ty: RirTypeId) -> bool {
    matches!(program.types.get(ty.index()), Some(RirType::Struct(id))
        if program.structs.get(id.index()).is_some_and(|strukt| strukt.native_ref))
}

pub(super) fn classify_param(plan: &RirNativeParam) -> (RirPassMode, RirParamEscape) {
    match plan {
        RirNativeParam::Value
        | RirNativeParam::SharedNamed
        | RirNativeParam::OwnedNamed
        | RirNativeParam::InitField(_)
        | RirNativeParam::Option(_)
        | RirNativeParam::Result(_, _)
        | RirNativeParam::Array(_)
        | RirNativeParam::Slice(_) => (RirPassMode::Value, RirParamEscape::NonEscaping),
        RirNativeParam::Borrow => (RirPassMode::SharedBorrow, RirParamEscape::NonEscaping),
        RirNativeParam::MutBorrow => (RirPassMode::MutBorrow, RirParamEscape::NonEscaping),
        RirNativeParam::MutPlace => (RirPassMode::MutPlace, RirParamEscape::NonEscaping),
        RirNativeParam::ScopedLambda => (RirPassMode::ScopedLambda, RirParamEscape::NonEscaping),
        RirNativeParam::EscapingLambda => (RirPassMode::EscapingLambda, RirParamEscape::Escaping),
        RirNativeParam::AnvCallback => (RirPassMode::AnvCallback, RirParamEscape::Escaping),
    }
}

pub(super) fn classify_arg_action(
    plan: &RirNativeParam,
    mode: RirPassMode,
    native_ref: bool,
    suspends_runtime_entry: bool,
    ty: &RirType,
) -> NativeArgAction {
    match mode {
        RirPassMode::SharedBorrow if native_ref => {
            return NativeArgAction::NativeRefBorrow { mutable: false };
        }
        RirPassMode::MutBorrow if native_ref => {
            return NativeArgAction::NativeRefBorrow { mutable: true };
        }
        _ => {}
    }
    if !suspends_runtime_entry {
        return NativeArgAction::Direct;
    }
    match reentry_policy(plan, ty) {
        NativeReentryPolicy::Safe => NativeArgAction::Direct,
        NativeReentryPolicy::SnapshotStringBorrow => NativeArgAction::SnapshotString,
        NativeReentryPolicy::UnsupportedLiveBoundary => NativeArgAction::RejectLiveBoundary,
    }
}

pub(super) fn classify_return(
    program: &RirProgram,
    ty: RirTypeId,
    adapter: &RustReturnAdapter,
) -> Option<RirNativeReturn> {
    match adapter {
        RustReturnAdapter::Void => matches!(program.types.get(ty.index()), Some(RirType::Void))
            .then_some(RirNativeReturn::Void),
        RustReturnAdapter::Value => Some(plan_value_return(program, ty)),
        RustReturnAdapter::OwnedNamed => Some(RirNativeReturn::OwnedNamed {
            adopt: matches!(program.types.get(ty.index()), Some(RirType::Struct(id))
                if program.structs[id.index()].native_ref),
        }),
        RustReturnAdapter::Option(inner) => {
            let Some(RirType::Option(payload_ty)) = program.types.get(ty.index()) else {
                return None;
            };
            Some(RirNativeReturn::Option {
                payload_ty: *payload_ty,
                payload: Box::new(classify_return(program, *payload_ty, inner)?),
            })
        }
        RustReturnAdapter::Result(ok, err) => {
            let (ok_ty, err_ty) = result_payload_tys(program, ty)?;
            Some(RirNativeReturn::Result {
                ok_ty,
                ok: Box::new(classify_return(program, ok_ty, ok)?),
                err_ty,
                err: Box::new(classify_return(program, err_ty, err)?),
            })
        }
    }
}

fn plan_value_return(program: &RirProgram, ty: RirTypeId) -> RirNativeReturn {
    if native_ref_ty(program, ty) {
        return RirNativeReturn::SharedNamed;
    }
    match program.types.get(ty.index()) {
        Some(RirType::Option(inner)) => RirNativeReturn::Option {
            payload_ty: *inner,
            payload: Box::new(plan_value_return(program, *inner)),
        },
        Some(RirType::Array { elem, .. }) => RirNativeReturn::Array {
            elem_ty: *elem,
            elem: Box::new(plan_value_return(program, *elem)),
        },
        _ => match result_payload_tys(program, ty) {
            Some((ok_ty, err_ty)) => RirNativeReturn::Result {
                ok_ty,
                ok: Box::new(plan_value_return(program, ok_ty)),
                err_ty,
                err: Box::new(plan_value_return(program, err_ty)),
            },
            None => RirNativeReturn::Value,
        },
    }
}

fn reentry_policy(plan: &RirNativeParam, ty: &RirType) -> NativeReentryPolicy {
    match plan {
        RirNativeParam::Borrow if matches!(ty, RirType::String) => {
            NativeReentryPolicy::SnapshotStringBorrow
        }
        RirNativeParam::Borrow | RirNativeParam::MutBorrow | RirNativeParam::Slice(_) => {
            NativeReentryPolicy::UnsupportedLiveBoundary
        }
        RirNativeParam::InitField(inner) | RirNativeParam::Option(inner) => {
            nested_value_reentry_policy(inner, ty)
        }
        RirNativeParam::Result(_, _) => NativeReentryPolicy::UnsupportedLiveBoundary,
        RirNativeParam::Value
        | RirNativeParam::SharedNamed
        | RirNativeParam::OwnedNamed
        | RirNativeParam::Array(_)
        | RirNativeParam::MutPlace
        | RirNativeParam::ScopedLambda
        | RirNativeParam::EscapingLambda
        | RirNativeParam::AnvCallback => NativeReentryPolicy::Safe,
    }
}

fn nested_value_reentry_policy(plan: &RirNativeParam, ty: &RirType) -> NativeReentryPolicy {
    match reentry_policy(plan, ty) {
        NativeReentryPolicy::Safe => NativeReentryPolicy::Safe,
        NativeReentryPolicy::SnapshotStringBorrow
        | NativeReentryPolicy::UnsupportedLiveBoundary => {
            NativeReentryPolicy::UnsupportedLiveBoundary
        }
    }
}
