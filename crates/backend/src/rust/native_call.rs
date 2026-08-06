use anvyx_runtime::{RustParamAbi, RustReturnAbi};

use super::rir::{
    RirCoreEnumKind, RirNativeReturn, RirParamEscape, RirPassMode, RirProgram, RirType, RirTypeId,
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

pub(super) fn classify_param(abi: &RustParamAbi) -> (RirPassMode, RirParamEscape) {
    match abi {
        RustParamAbi::Value(_)
        | RustParamAbi::OwnedNamed(_)
        | RustParamAbi::InitField(_)
        | RustParamAbi::Option(_)
        | RustParamAbi::Result(_, _)
        | RustParamAbi::Slice(_) => (RirPassMode::Value, RirParamEscape::NonEscaping),
        RustParamAbi::Borrow(_) => (RirPassMode::SharedBorrow, RirParamEscape::NonEscaping),
        RustParamAbi::MutBorrow(_) => (RirPassMode::MutBorrow, RirParamEscape::NonEscaping),
        RustParamAbi::MutPlace(_) => (RirPassMode::MutPlace, RirParamEscape::NonEscaping),
        RustParamAbi::ScopedLambda(_) => (RirPassMode::ScopedLambda, RirParamEscape::NonEscaping),
        RustParamAbi::EscapingLambda(_) => (RirPassMode::EscapingLambda, RirParamEscape::Escaping),
        RustParamAbi::AnvCallback(_) => (RirPassMode::AnvCallback, RirParamEscape::Escaping),
    }
}

pub(super) fn classify_arg_action(
    abi: &RustParamAbi,
    mode: RirPassMode,
    native_ref: bool,
    suspends_runtime_entry: bool,
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
    match reentry_policy(abi) {
        NativeReentryPolicy::Safe => NativeArgAction::Direct,
        NativeReentryPolicy::SnapshotStringBorrow => NativeArgAction::SnapshotString,
        NativeReentryPolicy::UnsupportedLiveBoundary => NativeArgAction::RejectLiveBoundary,
    }
}

pub(super) fn classify_return(
    program: &RirProgram,
    ty: RirTypeId,
    abi: &RustReturnAbi,
) -> Option<RirNativeReturn> {
    match abi {
        RustReturnAbi::Void => matches!(program.types.get(ty.index()), Some(RirType::Void))
            .then_some(RirNativeReturn::Void),
        RustReturnAbi::Value(value) => Some(RirNativeReturn::Value(value.clone())),
        RustReturnAbi::OwnedNamed(value) => Some(RirNativeReturn::OwnedNamed {
            ty: value.clone(),
            adopt: matches!(program.types.get(ty.index()), Some(RirType::Struct(id))
                if program.structs[id.index()].native_ref),
        }),
        RustReturnAbi::Option(inner) => {
            let Some(RirType::Option(payload_ty)) = program.types.get(ty.index()) else {
                return None;
            };
            Some(RirNativeReturn::Option {
                payload_ty: *payload_ty,
                payload: Box::new(classify_return(program, *payload_ty, inner)?),
            })
        }
        RustReturnAbi::Result(ok, err) => {
            let Some(RirType::Enum(id)) = program.types.get(ty.index()) else {
                return None;
            };
            let enm = &program.enums[id.index()];
            let [ok_variant, err_variant] = enm.variants.as_slice() else {
                return None;
            };
            let ([ok_field], [err_field]) =
                (ok_variant.fields.as_slice(), err_variant.fields.as_slice())
            else {
                return None;
            };
            if enm.core != Some(RirCoreEnumKind::Result) {
                return None;
            }
            Some(RirNativeReturn::Result {
                ok_ty: ok_field.ty,
                ok: Box::new(classify_return(program, ok_field.ty, ok)?),
                err_ty: err_field.ty,
                err: Box::new(classify_return(program, err_field.ty, err)?),
            })
        }
    }
}

fn reentry_policy(abi: &RustParamAbi) -> NativeReentryPolicy {
    match abi {
        RustParamAbi::Borrow(anvyx_runtime::ExternTypeExpr::String) => {
            NativeReentryPolicy::SnapshotStringBorrow
        }
        RustParamAbi::Borrow(_) | RustParamAbi::MutBorrow(_) | RustParamAbi::Slice(_) => {
            NativeReentryPolicy::UnsupportedLiveBoundary
        }
        RustParamAbi::InitField(inner) | RustParamAbi::Option(inner) => {
            nested_value_reentry_policy(inner)
        }
        RustParamAbi::Result(ok, err) => match (reentry_policy(ok), reentry_policy(err)) {
            (
                NativeReentryPolicy::UnsupportedLiveBoundary
                | NativeReentryPolicy::SnapshotStringBorrow,
                _,
            )
            | (
                _,
                NativeReentryPolicy::UnsupportedLiveBoundary
                | NativeReentryPolicy::SnapshotStringBorrow,
            ) => NativeReentryPolicy::UnsupportedLiveBoundary,
            (NativeReentryPolicy::Safe, NativeReentryPolicy::Safe) => NativeReentryPolicy::Safe,
        },
        RustParamAbi::Value(_)
        | RustParamAbi::OwnedNamed(_)
        | RustParamAbi::MutPlace(_)
        | RustParamAbi::ScopedLambda(_)
        | RustParamAbi::EscapingLambda(_)
        | RustParamAbi::AnvCallback(_) => NativeReentryPolicy::Safe,
    }
}

fn nested_value_reentry_policy(abi: &RustParamAbi) -> NativeReentryPolicy {
    match reentry_policy(abi) {
        NativeReentryPolicy::Safe => NativeReentryPolicy::Safe,
        NativeReentryPolicy::SnapshotStringBorrow
        | NativeReentryPolicy::UnsupportedLiveBoundary => {
            NativeReentryPolicy::UnsupportedLiveBoundary
        }
    }
}
