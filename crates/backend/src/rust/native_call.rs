use anvyx_frontend::air::{CallArg, ParamEscape, Program, TypeData};
use anvyx_runtime::{RustExternAbi, RustParamAbi, RustWrapperCtx};

use super::rir::{RirCallArg, RirExternParam, RirParamAbi, RirParamEscape, RirParamSemantic};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ProviderEntryPlan {
    None,
    SuspendRuntimeEntry,
}

impl ProviderEntryPlan {
    pub(super) fn for_retained_callbacks(retained_callbacks: bool) -> Self {
        if retained_callbacks {
            Self::SuspendRuntimeEntry
        } else {
            Self::None
        }
    }

    pub(super) fn suspends_runtime_entry(self) -> bool {
        matches!(self, Self::SuspendRuntimeEntry)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum NativeHiddenCtxPlan {
    None,
    Runtime,
}

impl NativeHiddenCtxPlan {
    pub(super) fn from_abi(ctx: RustWrapperCtx) -> Self {
        match ctx {
            RustWrapperCtx::None => Self::None,
            RustWrapperCtx::HiddenRuntime => Self::Runtime,
        }
    }

    pub(super) fn borrows_runtime(self) -> bool {
        matches!(self, Self::Runtime)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum NativeArgBoundary {
    Direct,
    SnapshotString,
    NativeRefBorrow { mutable: bool },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NativeReentryPolicy {
    Safe,
    SnapshotStringBorrow,
    UnsupportedLiveBoundary,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct NativeCallPlan {
    params: Vec<NativeParamAbi>,
    provider_entry: ProviderEntryPlan,
}

impl NativeCallPlan {
    pub(super) fn for_abi(abi: &RustExternAbi, retained_callbacks: bool) -> Self {
        Self::new(
            abi.params.iter().map(classify_param).collect(),
            retained_callbacks,
        )
    }

    pub(super) fn new(params: Vec<NativeParamAbi>, retained_callbacks: bool) -> Self {
        Self {
            params,
            provider_entry: ProviderEntryPlan::for_retained_callbacks(retained_callbacks),
        }
    }

    pub(super) fn param_semantics(&self) -> Vec<RirParamSemantic> {
        self.params.iter().map(|param| param.semantic).collect()
    }

    pub(super) fn rejects_reentry_arg(
        &self,
        index: usize,
        arg: &CallArg,
        arg_is_string: bool,
    ) -> bool {
        self.provider_entry.suspends_runtime_entry()
            && self
                .params
                .get(index)
                .is_some_and(|param| param.rejects_reentry_arg(arg, arg_is_string))
    }

    pub(super) fn rejects_reentry_rir_arg(
        &self,
        index: usize,
        arg: &RirCallArg,
        arg_is_string: bool,
    ) -> bool {
        self.provider_entry.suspends_runtime_entry()
            && self
                .params
                .get(index)
                .is_some_and(|param| param.rejects_reentry_rir_arg(arg, arg_is_string))
    }

    pub(super) fn rir_arg_boundary(
        &self,
        index: usize,
        arg: &RirCallArg,
        arg_is_string: bool,
        arg_is_native_ref: bool,
    ) -> NativeArgBoundary {
        self.params
            .get(index)
            .map_or(NativeArgBoundary::Direct, |param| {
                param.rir_arg_boundary(
                    self.provider_entry.suspends_runtime_entry(),
                    arg,
                    arg_is_string,
                    arg_is_native_ref,
                )
            })
    }

    pub(super) fn provider_entry(&self) -> ProviderEntryPlan {
        self.provider_entry
    }

    pub(super) fn matches_signature(&self, params: &[RirExternParam]) -> bool {
        self.params.len() == params.len()
            && params
                .iter()
                .zip(&self.params)
                .all(|(param, planned)| planned.matches_rir_param(*param))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct NativeParamAbi {
    pub semantic: RirParamSemantic,
    pub abi: RirParamAbi,
    pub escape: RirParamEscape,
    reentry: NativeReentryPolicy,
}

impl NativeParamAbi {
    fn new(
        semantic: RirParamSemantic,
        abi: RirParamAbi,
        escape: RirParamEscape,
        reentry: NativeReentryPolicy,
    ) -> Self {
        Self {
            semantic,
            abi,
            escape,
            reentry,
        }
    }

    pub(super) fn matches_rir_param(self, param: RirExternParam) -> bool {
        param.semantic == self.semantic && param.abi == self.abi && param.escape == self.escape
    }

    fn rejects_reentry_arg(self, arg: &CallArg, arg_is_string: bool) -> bool {
        self.rejects_reentry_arg_shape(
            matches!(arg, CallArg::SharedBorrow(_)),
            matches!(arg, CallArg::InitFieldOmitted),
            arg_is_string,
        )
    }

    fn rejects_reentry_rir_arg(self, arg: &RirCallArg, arg_is_string: bool) -> bool {
        self.rejects_reentry_arg_shape(
            matches!(arg, RirCallArg::SharedBorrow(_)),
            matches!(arg, RirCallArg::InitFieldOmitted),
            arg_is_string,
        )
    }

    fn rejects_reentry_arg_shape(
        self,
        shared_borrow: bool,
        omitted: bool,
        arg_is_string: bool,
    ) -> bool {
        match self.reentry {
            NativeReentryPolicy::Safe => false,
            NativeReentryPolicy::SnapshotStringBorrow => shared_borrow && !arg_is_string,
            NativeReentryPolicy::UnsupportedLiveBoundary => !omitted,
        }
    }

    fn rir_arg_boundary(
        self,
        suspends_runtime_entry: bool,
        arg: &RirCallArg,
        arg_is_string: bool,
        arg_is_native_ref: bool,
    ) -> NativeArgBoundary {
        if suspends_runtime_entry
            && self.reentry == NativeReentryPolicy::SnapshotStringBorrow
            && arg_is_string
            && matches!(arg, RirCallArg::SharedBorrow(_))
        {
            return NativeArgBoundary::SnapshotString;
        }
        match (self.abi, arg_is_native_ref) {
            (RirParamAbi::SharedBorrow, true) => {
                NativeArgBoundary::NativeRefBorrow { mutable: false }
            }
            (RirParamAbi::MutBorrow, true) => NativeArgBoundary::NativeRefBorrow { mutable: true },
            _ => NativeArgBoundary::Direct,
        }
    }
}

pub(super) fn air_has_retained_callbacks(program: &Program) -> bool {
    program.externs.iter().any(|ext| {
        ext.params.iter().any(|param| {
            param.escape == ParamEscape::Escaping
                && matches!(program.type_arena.data(param.ty), TypeData::Function(_))
        })
    })
}

pub(super) fn classify_param(abi: &RustParamAbi) -> NativeParamAbi {
    let reentry = reentry_policy(abi);
    match abi {
        RustParamAbi::Value(_)
        | RustParamAbi::OwnedNamed(_)
        | RustParamAbi::InitField(_)
        | RustParamAbi::Option(_)
        | RustParamAbi::Result(_, _)
        | RustParamAbi::Slice(_) => NativeParamAbi::new(
            RirParamSemantic::Value,
            RirParamAbi::Value,
            RirParamEscape::NonEscaping,
            reentry,
        ),
        RustParamAbi::Borrow(_) => NativeParamAbi::new(
            RirParamSemantic::SharedBorrow,
            RirParamAbi::SharedBorrow,
            RirParamEscape::NonEscaping,
            reentry,
        ),
        RustParamAbi::MutBorrow(_) => NativeParamAbi::new(
            RirParamSemantic::MutBorrow,
            RirParamAbi::MutBorrow,
            RirParamEscape::NonEscaping,
            reentry,
        ),
        RustParamAbi::MutPlace(_) => NativeParamAbi::new(
            RirParamSemantic::MutPlace,
            RirParamAbi::MutPlace,
            RirParamEscape::NonEscaping,
            reentry,
        ),
        RustParamAbi::ScopedLambda(_) => NativeParamAbi::new(
            RirParamSemantic::ScopedLambda,
            RirParamAbi::ScopedLambda,
            RirParamEscape::NonEscaping,
            reentry,
        ),
        RustParamAbi::EscapingLambda(_) => NativeParamAbi::new(
            RirParamSemantic::EscapingLambda,
            RirParamAbi::EscapingLambda,
            RirParamEscape::Escaping,
            reentry,
        ),
    }
}

fn reentry_policy(abi: &RustParamAbi) -> NativeReentryPolicy {
    match abi {
        RustParamAbi::Borrow(anvyx_runtime::ExternTypeExpr::String) => {
            NativeReentryPolicy::SnapshotStringBorrow
        }
        RustParamAbi::Borrow(_) | RustParamAbi::MutBorrow(_) | RustParamAbi::MutPlace(_) => {
            NativeReentryPolicy::UnsupportedLiveBoundary
        }
        RustParamAbi::Slice(_) => NativeReentryPolicy::UnsupportedLiveBoundary,
        RustParamAbi::InitField(inner) | RustParamAbi::Option(inner) => {
            nested_value_reentry_policy(inner)
        }
        RustParamAbi::Result(ok, err) => match (reentry_policy(ok), reentry_policy(err)) {
            (NativeReentryPolicy::UnsupportedLiveBoundary, _)
            | (_, NativeReentryPolicy::UnsupportedLiveBoundary) => {
                NativeReentryPolicy::UnsupportedLiveBoundary
            }
            (NativeReentryPolicy::SnapshotStringBorrow, _)
            | (_, NativeReentryPolicy::SnapshotStringBorrow) => {
                NativeReentryPolicy::UnsupportedLiveBoundary
            }
            (NativeReentryPolicy::Safe, NativeReentryPolicy::Safe) => NativeReentryPolicy::Safe,
        },
        RustParamAbi::Value(_)
        | RustParamAbi::OwnedNamed(_)
        | RustParamAbi::ScopedLambda(_)
        | RustParamAbi::EscapingLambda(_) => NativeReentryPolicy::Safe,
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
