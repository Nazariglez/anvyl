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
enum NativeArgMode {
    Direct,
    SharedBorrow,
    MutBorrow,
    Omitted,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct NativeArgFacts {
    mode: NativeArgMode,
    string: bool,
    native_ref: bool,
}

impl NativeArgFacts {
    pub(super) fn air(arg: &CallArg, string: bool, native_ref: bool) -> Self {
        let mode = match arg {
            CallArg::SharedBorrow(_) => NativeArgMode::SharedBorrow,
            CallArg::MutBorrow(_) => NativeArgMode::MutBorrow,
            CallArg::InitFieldOmitted => NativeArgMode::Omitted,
            _ => NativeArgMode::Direct,
        };
        Self {
            mode,
            string,
            native_ref,
        }
    }

    pub(super) fn rir(arg: &RirCallArg, string: bool, native_ref: bool) -> Self {
        let mode = match arg {
            RirCallArg::SharedBorrow(_) => NativeArgMode::SharedBorrow,
            RirCallArg::MutBorrow(_) => NativeArgMode::MutBorrow,
            RirCallArg::InitFieldOmitted => NativeArgMode::Omitted,
            _ => NativeArgMode::Direct,
        };
        Self {
            mode,
            string,
            native_ref,
        }
    }
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

    pub(super) fn rejects_reentry_arg(&self, index: usize, facts: NativeArgFacts) -> bool {
        self.provider_entry.suspends_runtime_entry()
            && self
                .params
                .get(index)
                .is_some_and(|param| param.rejects_reentry_arg(facts))
    }

    pub(super) fn arg_boundary(&self, index: usize, facts: NativeArgFacts) -> NativeArgBoundary {
        self.params
            .get(index)
            .map_or(NativeArgBoundary::Direct, |param| {
                param.arg_boundary(self.provider_entry.suspends_runtime_entry(), facts)
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

    fn rejects_reentry_arg(self, facts: NativeArgFacts) -> bool {
        match self.arg_boundary(true, facts) {
            NativeArgBoundary::SnapshotString | NativeArgBoundary::NativeRefBorrow { .. } => false,
            NativeArgBoundary::Direct => match self.reentry {
                NativeReentryPolicy::Safe => false,
                NativeReentryPolicy::SnapshotStringBorrow => {
                    facts.mode == NativeArgMode::SharedBorrow && !facts.string
                }
                NativeReentryPolicy::UnsupportedLiveBoundary => {
                    facts.mode != NativeArgMode::Omitted
                }
            },
        }
    }

    fn arg_boundary(
        self,
        suspends_runtime_entry: bool,
        facts: NativeArgFacts,
    ) -> NativeArgBoundary {
        if suspends_runtime_entry
            && self.reentry == NativeReentryPolicy::SnapshotStringBorrow
            && facts.string
            && facts.mode == NativeArgMode::SharedBorrow
        {
            return NativeArgBoundary::SnapshotString;
        }
        match (self.abi, facts.native_ref) {
            (RirParamAbi::SharedBorrow, true) if facts.mode == NativeArgMode::SharedBorrow => {
                NativeArgBoundary::NativeRefBorrow { mutable: false }
            }
            (RirParamAbi::MutBorrow, true) if facts.mode == NativeArgMode::MutBorrow => {
                NativeArgBoundary::NativeRefBorrow { mutable: true }
            }
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
