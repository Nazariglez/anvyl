use anvyx_frontend::air::{CallArg, ParamEscape, Program, TypeData};
use anvyx_runtime::{RustExternAbi, RustParamAbi, RustWrapperCtx};

use super::rir::{RirCallArg, RirExternParam, RirParamAbi, RirParamEscape, RirParamSemantic};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ProviderEntryPlan {
    None,
    SuspendRuntimeEntry(ProviderEntryReason),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ProviderEntryReason {
    RetainedCallbacks,
}

impl ProviderEntryPlan {
    pub(super) fn for_retained_callbacks(retained_callbacks: bool) -> Self {
        if retained_callbacks {
            Self::SuspendRuntimeEntry(ProviderEntryReason::RetainedCallbacks)
        } else {
            Self::None
        }
    }

    pub(super) fn suspends_runtime_entry(self) -> bool {
        matches!(self, Self::SuspendRuntimeEntry(_))
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

    pub(super) fn dynamic(semantic: RirParamSemantic, string: bool, native_ref: bool) -> Self {
        let mode = match semantic {
            RirParamSemantic::SharedBorrow => NativeArgMode::SharedBorrow,
            RirParamSemantic::MutBorrow => NativeArgMode::MutBorrow,
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
        Self {
            params: abi.params.iter().map(classify_param).collect(),
            provider_entry: ProviderEntryPlan::for_retained_callbacks(retained_callbacks),
        }
    }

    pub(super) fn param_semantics(&self) -> Vec<RirParamSemantic> {
        self.params.iter().map(|param| param.semantic).collect()
    }

    pub(super) fn arg_action(&self, index: usize, facts: NativeArgFacts) -> NativeArgAction {
        self.params
            .get(index)
            .map_or(NativeArgAction::Direct, |param| {
                param.arg_action(self.provider_entry.suspends_runtime_entry(), facts)
            })
    }

    pub(super) fn rejects_reentry_arg(&self, index: usize, facts: NativeArgFacts) -> bool {
        self.arg_action(index, facts) == NativeArgAction::RejectLiveBoundary
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

    fn arg_action(self, suspends_runtime_entry: bool, facts: NativeArgFacts) -> NativeArgAction {
        if suspends_runtime_entry
            && self.reentry == NativeReentryPolicy::SnapshotStringBorrow
            && facts.string
            && facts.mode == NativeArgMode::SharedBorrow
        {
            return NativeArgAction::SnapshotString;
        }
        match (self.abi, facts.native_ref) {
            (RirParamAbi::SharedBorrow, true) if facts.mode == NativeArgMode::SharedBorrow => {
                NativeArgAction::NativeRefBorrow { mutable: false }
            }
            (RirParamAbi::MutBorrow, true) if facts.mode == NativeArgMode::MutBorrow => {
                NativeArgAction::NativeRefBorrow { mutable: true }
            }
            _ if suspends_runtime_entry && self.rejects_live_boundary(facts) => {
                NativeArgAction::RejectLiveBoundary
            }
            _ => NativeArgAction::Direct,
        }
    }

    fn rejects_live_boundary(self, facts: NativeArgFacts) -> bool {
        match self.reentry {
            NativeReentryPolicy::Safe => false,
            NativeReentryPolicy::SnapshotStringBorrow => {
                facts.mode == NativeArgMode::SharedBorrow && !facts.string
            }
            NativeReentryPolicy::UnsupportedLiveBoundary => facts.mode != NativeArgMode::Omitted,
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
        RustParamAbi::AnvCallback(_) => NativeParamAbi::new(
            RirParamSemantic::AnvCallback,
            RirParamAbi::AnvCallback,
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

#[cfg(test)]
mod tests {
    use anvyx_runtime::{
        CallbackPolicy, ExternCallbackSignature, ExternTypeExpr, RustAbiSupport, RustParamAbi,
        RustReturnAbi,
    };

    use super::*;

    fn named(name: &str) -> ExternTypeExpr {
        ExternTypeExpr::Named {
            module: None,
            name: name.into(),
            args: vec![],
        }
    }

    fn facts(mode: NativeArgMode, string: bool, native_ref: bool) -> NativeArgFacts {
        NativeArgFacts {
            mode,
            string,
            native_ref,
        }
    }

    #[test]
    fn native_call_plan_arg_actions() {
        let cases = [
            (
                "plain value stays direct without retained callbacks",
                RustParamAbi::Value(ExternTypeExpr::Int),
                false,
                facts(NativeArgMode::Direct, false, false),
                NativeArgAction::Direct,
            ),
            (
                "slice value is rejected across retained callback reentry",
                RustParamAbi::Slice(Box::new(RustParamAbi::Value(ExternTypeExpr::Int))),
                true,
                facts(NativeArgMode::Direct, false, false),
                NativeArgAction::RejectLiveBoundary,
            ),
            (
                "string shared borrow is snapshotted across retained callback reentry",
                RustParamAbi::Borrow(ExternTypeExpr::String),
                true,
                facts(NativeArgMode::SharedBorrow, true, false),
                NativeArgAction::SnapshotString,
            ),
            (
                "non-string shared borrow for string ABI is rejected across retained callback reentry",
                RustParamAbi::Borrow(ExternTypeExpr::String),
                true,
                facts(NativeArgMode::SharedBorrow, false, false),
                NativeArgAction::RejectLiveBoundary,
            ),
            (
                "native shared ref gets a resource borrow wrapper",
                RustParamAbi::Borrow(named("Counter")),
                true,
                facts(NativeArgMode::SharedBorrow, false, true),
                NativeArgAction::NativeRefBorrow { mutable: false },
            ),
            (
                "native mutable ref gets a mutable resource borrow wrapper",
                RustParamAbi::MutBorrow(named("Counter")),
                true,
                facts(NativeArgMode::MutBorrow, false, true),
                NativeArgAction::NativeRefBorrow { mutable: true },
            ),
            (
                "mutable place remains descriptor-safe across retained callback reentry",
                RustParamAbi::MutPlace(named("Counter")),
                true,
                facts(NativeArgMode::Direct, false, false),
                NativeArgAction::Direct,
            ),
            (
                "scoped callback is direct across retained callback reentry",
                RustParamAbi::ScopedLambda(ExternCallbackSignature {
                    params: vec![],
                    ret: Box::new(ExternTypeExpr::Void),
                    policy: CallbackPolicy::default(),
                }),
                true,
                facts(NativeArgMode::Direct, false, false),
                NativeArgAction::Direct,
            ),
        ];

        for (name, abi, retained, arg_facts, expected) in cases {
            let plan =
                NativeCallPlan::for_abi(&extern_abi(vec![abi], RustWrapperCtx::None), retained);
            assert_eq!(plan.arg_action(0, arg_facts), expected, "{name}");
        }
    }

    fn extern_abi(params: Vec<RustParamAbi>, ctx: RustWrapperCtx) -> RustExternAbi {
        RustExternAbi {
            params,
            ret: RustReturnAbi::Void,
            fallible: false,
            support: RustAbiSupport::Direct,
            ctx,
        }
    }
}
