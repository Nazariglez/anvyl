mod analysis;
pub mod cargo_job;
mod emit;
mod exact_witness;
mod mut_place;
mod native;
mod native_call;
mod place;
mod place_access;
mod rep_policy;
mod retained_callbacks;
mod rir;
mod runtime_owner;
#[cfg(test)]
mod source_job;
mod syntax;
mod target;
mod value;
mod write;

use std::{
    collections::{BTreeMap, HashMap, HashSet},
    error::Error,
    fmt,
};

use anvyx_frontend::{
    air::{
        self, AggregateCtor, CallArg, Callee, ConstId, ConstValue, ExternId, FieldId, FunctionId,
        FunctionValueCapability, GlobalId, LocalId, MapWriteKind, Mutability, Operand, ParamEscape,
        ParamMode, Place, RValue, ReturnMode, TypeData, TypeId, TypePassClasses, VerifiedProgram,
        place_model,
    },
    ast::{FormatKind, FormatSign, FormatSpec, Ident},
    diagnostic::Diagnostic,
    span::SourceSpan,
};
use anvyx_runtime::RustProviderSupport;

use self::{
    place_access::{
        DataRefProjectionPlan, DataRefSegmentPlan, PlaceAccessCx, PlaceAccessIntent,
        PlaceAccessPlan, PlaceAccessRoot,
    },
    rep_policy::{
        LambdaStorageFamily, RirRustRepPolicy, RustDynamicBoxReason, RustDynamicCarrierPlan,
        RustDynamicLayoutPlan, RustLayoutGap, RustLifecycleGap, RustMaterializerGraph,
        RustPayloadStorage, RustRecipePosition, RustRepresentationPlan,
    },
    rir::{
        RirCallArg, RirCallTarget, RirCellDecl, RirCellId, RirCellLifetime, RirCellRef,
        RirCellStorage, RirCollectionAccess, RirCollectionFor, RirCollectionLoanMode,
        RirCollectionLoanScope, RirCollectionRootKind, RirCollectionStorage,
        RirCollectionStorageId, RirCollectionStorageKind, RirConst, RirConstId, RirConstValue,
        RirCoreEnumKind, RirDataRef, RirDataRefId, RirDataRefPlace, RirDataRefPlaceId,
        RirDynBoxReason, RirDynCarrier, RirDynCarrierId, RirDynDispatch, RirDynDispatchArm,
        RirDynDispatchId, RirDynDispatchParam, RirDynMatchMapping, RirDynReceiver, RirDynStorage,
        RirDynVariant, RirDynVariantId, RirDynVariantSet, RirDynVariantSetId, RirDynWeakeningId,
        RirEnum, RirEnumId, RirEnumRepr, RirEnumRole, RirEnumStringifyVariant, RirExtern,
        RirExternId, RirExternParam, RirField, RirFieldId, RirFlag, RirFlagId, RirFlagMember,
        RirFlagMemberId, RirFlagStaticOp, RirFunction, RirFunctionId, RirGlobal, RirGlobalId,
        RirIf, RirIterCountCheck, RirLambda, RirLambdaCapture, RirLambdaCaptureArg,
        RirLambdaCaptureKind, RirLambdaEnvField, RirLambdaEnvFieldKind, RirLambdaEnvId,
        RirLambdaEnvLayout, RirLambdaEscape, RirLambdaId, RirLambdaParam, RirLambdaSig,
        RirLambdaSigId, RirLambdaStorage, RirLocal, RirLocalBinding, RirLocalId, RirLoop,
        RirLoopId, RirMapEntryMatch, RirMapWriteKind, RirMutPlaceAccess, RirMutPlaceArg,
        RirMutPlaceHandle, RirOperand, RirOptionMatch, RirOptionPayloadBinding, RirOptionSubject,
        RirOrdinalAdapter, RirOrdinalPlan, RirOwnedOperand, RirOwnedSource, RirOwnedValue,
        RirParamEscape, RirPassMode, RirPatternAlternative, RirPatternArm, RirPatternBinding,
        RirPatternBindingMode, RirPatternMatch, RirPatternPath, RirPatternPathStep, RirPatternTest,
        RirPlace, RirPlaceRoot, RirPlaceStep, RirPlaceStepKind, RirProgram, RirRValue, RirRangeFor,
        RirRawEnumValue, RirResolvedCallTarget, RirReturn, RirScopedPlaceCellDecl,
        RirScopedPlaceCellId, RirScopedPlaceCellRef, RirScopedPlaceSource, RirStmt,
        RirStringLiteral, RirStringLiteralId, RirStringifyHelper, RirStringifyHelperId,
        RirStringifyHelperKind, RirStringifyReq, RirStringifyReqId, RirStringifyReqKind, RirStruct,
        RirStructId, RirStructRole, RirStructuredBlock, RirSymbol, RirTerm, RirTuple, RirTupleId,
        RirType, RirTypeId, RirVariant, RirVariantId, RirVariantKind,
    },
};

#[cfg(test)]
mod tests;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustSource {
    text: String,
}

impl RustSource {
    pub fn new(text: String) -> Self {
        Self { text }
    }

    pub fn as_str(&self) -> &str {
        &self.text
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustPlanConfig {
    pub symbol_prefix: String,
    pub native_providers: Vec<RustProviderSupport>,
}

impl Default for RustPlanConfig {
    fn default() -> Self {
        Self {
            symbol_prefix: "anv".into(),
            native_providers: vec![],
        }
    }
}

struct RirPlan {
    program: RirProgram,
}

pub fn generate(
    program: &VerifiedProgram<'_>,
    config: RustPlanConfig,
) -> Result<RustSource, RustPlanError> {
    let plan = plan(program, config)?;
    Ok(emit::emit(&plan))
}

fn plan(program: &VerifiedProgram<'_>, config: RustPlanConfig) -> Result<RirPlan, RustPlanError> {
    let mut cx = PlanCx::new(program.program(), config);
    let mut rir = cx
        .plan()
        .map_err(|gaps| RustPlanError::TargetGaps(RustTargetGaps(gaps)))?;
    exact_witness::propagate(&mut rir);
    rir::verify(&rir).map_err(|errors| {
        RustPlanError::InvalidPlan(errors.into_iter().map(|error| error.to_string()).collect())
    })?;
    Ok(RirPlan { program: rir })
}

pub fn rust_generation_failure_summary(count: usize) -> String {
    let noun = if count == 1 { "error" } else { "errors" };
    format!("Rust generation failed with {count} {noun}")
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustTargetGaps(Vec<RustTargetGap>);

impl RustTargetGaps {
    pub fn iter(&self) -> impl Iterator<Item = &RustTargetGap> {
        self.0.iter()
    }

    pub fn summary(&self) -> String {
        rust_generation_failure_summary(self.0.len())
    }
}

impl fmt::Display for RustTargetGaps {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.summary())?;
        for gap in self.0.iter().take(8) {
            writeln!(f, "  - {gap}")?;
        }
        if self.0.len() > 8 {
            writeln!(f, "  - ... and {} more", self.0.len() - 8)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RustPlanError {
    TargetGaps(RustTargetGaps),
    InvalidPlan(Vec<String>),
}

impl fmt::Display for RustPlanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TargetGaps(gaps) => write!(f, "{gaps}"),
            Self::InvalidPlan(errors) => {
                writeln!(f, "invalid RIR produced by planner: {}", errors.len())?;
                for error in errors.iter().take(8) {
                    writeln!(f, "  - {error}")?;
                }
                if errors.len() > 8 {
                    writeln!(f, "  - ... and {} more", errors.len() - 8)?;
                }
                Ok(())
            }
        }
    }
}

impl Error for RustPlanError {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustTargetGap {
    pub site: RustTargetGapSite,
    pub kind: RustTargetGapKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustExternGapSite {
    pub id: ExternId,
    pub name: String,
    pub subject: RustExternSubject,
    pub span: Option<SourceSpan>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustExternSubject {
    Function,
    FieldGetter,
    FieldSetter,
    Method,
    StaticMethod,
    Initializer,
    UnaryOperator,
    BinaryOperator { self_on_right: bool },
}

impl RustExternSubject {
    fn as_str(self) -> &'static str {
        match self {
            Self::Function => "extern function",
            Self::FieldGetter => "extern field getter",
            Self::FieldSetter => "extern field setter",
            Self::Method => "extern method",
            Self::StaticMethod => "extern static method",
            Self::Initializer => "extern initializer",
            Self::UnaryOperator => "extern unary operator",
            Self::BinaryOperator { .. } => "extern binary operator",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RustTargetGapSite {
    Entry,
    Type(TypeId),
    Global(GlobalId),
    Function(FunctionId),
    Extern(RustExternGapSite),
    Statement(FunctionId, usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustTargetGapKind {
    UnsupportedType,
    UnsupportedParamMode,
    UnsupportedCallArgMode,
    UnsupportedReturnMode,
    UnsupportedPlaceProjection,
    UnsupportedRValue,
    UnsupportedDynamicPlaceReturn,
    UnsupportedDynamicLayout,
    UnsupportedDynamicLifecycle,
    UnsupportedDynamicPayload,
    UnsupportedExternType,
    MissingExternBinding,
    MissingExternSupport,
    MissingExternExport,
    UnsupportedEntry,
    UnsupportedRustAbi,
    UnsupportedLambdaCall,
    UnsupportedLambdaCapture,
    UnsupportedLambdaCell,
    UnsupportedLambdaExternBoundary,
    UnsupportedGlobalType,
    UnsupportedGlobalAccess,
    UnsupportedGlobalProjection,
    UnsupportedGlobalInitializer,
    UnsupportedGlobalValueRead,
    UnsupportedGlobalRooting,
    NonCopyValueRequired,
    UnsupportedStructuralStringify,
    UnsupportedSliceView,
    UnsupportedMutablePlace,
    UnsupportedMutablePlaceProjection,
    UnsupportedMutablePlaceDataRef,
    UnsupportedMutablePlaceNativeBoundary,
    UnsupportedMapKey,
}

impl RustTargetGap {
    fn presentation(
        &self,
    ) -> (
        String,
        Option<(SourceSpan, &'static str)>,
        Option<&'static str>,
    ) {
        let RustTargetGapSite::Extern(site) = &self.site else {
            return (format!("{:?} at {:?}", self.kind, self.site), None, None);
        };
        let subject = site.subject.as_str();
        let name = &site.name;
        let (message, label, help) = match self.kind {
            RustTargetGapKind::MissingExternBinding => (
                format!("{subject} `{name}` has no Rust provider binding"),
                Some("declared here without a Rust provider binding"),
                Some(
                    "import this declaration from a Rust provider package before building for the Rust backend",
                ),
            ),
            RustTargetGapKind::MissingExternSupport => (
                format!("configured Rust provider support for {subject} `{name}` is unavailable"),
                Some("configured Rust provider support is unavailable"),
                Some("configure the Rust provider support required by this imported declaration"),
            ),
            RustTargetGapKind::MissingExternExport => (
                format!("Rust provider support does not export a binding for {subject} `{name}`"),
                Some("the Rust provider does not export this binding"),
                Some("use a provider package that exports a Rust binding for this declaration"),
            ),
            RustTargetGapKind::UnsupportedRustAbi => (
                format!("Rust provider ABI for {subject} `{name}` is unsupported"),
                Some("this provider ABI is unsupported"),
                None,
            ),
            RustTargetGapKind::UnsupportedLambdaExternBoundary => (
                format!(
                    "callback boundary for {subject} `{name}` is unsupported by the Rust backend"
                ),
                Some("this callback boundary is unsupported"),
                None,
            ),
            _ => (
                format!("Rust backend cannot generate {subject} `{name}`"),
                None,
                None,
            ),
        };
        (message, site.span.zip(label), help)
    }

    fn message(&self) -> String {
        self.presentation().0
    }

    pub fn diagnostic(&self) -> Diagnostic {
        let (message, primary, help) = self.presentation();
        let mut diagnostic = Diagnostic::error(message);
        if let Some((span, label)) = primary {
            diagnostic = diagnostic.with_primary_message(span, label);
        }
        if let Some(help) = help {
            diagnostic = diagnostic.with_help(help);
        }
        diagnostic
    }
}

impl fmt::Display for RustTargetGap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.message())
    }
}

impl Error for RustTargetGap {}

struct PlanCx<'a> {
    air: &'a air::Program,
    classes: TypePassClasses,
    config: RustPlanConfig,
    type_map: HashMap<TypeId, RirTypeId>,
    lambda_sig_map: HashMap<TypeId, RirLambdaSigId>,
    const_map: HashMap<ConstId, RirConstId>,
    string_literal_map: HashMap<String, RirStringLiteralId>,
    stringify_in_progress: HashSet<RirTypeId>,
    function_map: HashMap<FunctionId, RirFunctionId>,
    global_map: HashMap<GlobalId, RirGlobalId>,
    function_lambda_map: HashMap<FunctionId, RirLambdaId>,
    lambda_map: HashMap<air::LambdaId, RirLambdaId>,
    lambda_sources: Vec<PlannerLambdaSource>,
    function_type_copyable: HashMap<TypeId, bool>,
    function_type_shareable: HashMap<TypeId, bool>,
    lambda_runtime_capture_slots: HashMap<(air::LambdaId, air::LambdaCaptureSlotId), usize>,
    lambda_capture_modes: HashMap<(air::LambdaId, air::LambdaCaptureSlotId), RirPassMode>,
    capture_cell_map: HashMap<air::CaptureCellId, RirCellId>,
    scoped_place_cell_map: HashMap<air::ScopedBorrowId, RirScopedPlaceCellId>,
    extern_map: HashMap<ExternId, RirExternId>,
    externs: Vec<RirExtern>,
    dataref_map: HashMap<air::AggregateId, RirDataRefId>,
    dataref_places: Vec<RirDataRefPlace>,
    dataref_place_map:
        BTreeMap<(RirDataRefId, Vec<RirPlaceStep>, rir::RirMaterializerId), RirDataRefPlaceId>,
    enum_map: HashMap<air::EnumId, RirEnumId>,
    flag_map: HashMap<air::FlagId, RirFlagId>,
    tuple_map: HashMap<Vec<RirTypeId>, RirTupleId>,
    materializers: RustMaterializerGraph,
    dynamic_layout: Option<RustDynamicLayoutPlan>,
    dynamic_types: Vec<(TypeId, air::ContractSurfaceId, RirEnumId)>,
    dyn_surface_map: HashMap<air::ContractSurfaceId, RirDynCarrierId>,
    dyn_variant_map: HashMap<air::ContractWitnessId, RirDynVariantId>,
    dyn_carrier_witnesses: HashMap<RirDynCarrierId, Vec<air::ContractWitnessId>>,
    dyn_dispatch_map: HashMap<(air::ContractSurfaceId, air::ContractSlotId), RirDynDispatchId>,
    dyn_dispatch_params: Vec<Vec<RirDynDispatchParam>>,
    dyn_weakening_map: HashMap<air::ContractWeakeningId, RirDynWeakeningId>,
    dyn_variant_set_map: HashMap<(RirDynCarrierId, RirTypeId), RirDynVariantSetId>,
    dyn_variant_set_requests: Vec<(RirDynCarrierId, RirTypeId)>,
    dyn_reborrows: Vec<(LocalId, RirDynCarrierId)>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct ZeroEnvFunctionValue {
    lambda: RirLambdaId,
    ty: RirTypeId,
}

impl ZeroEnvFunctionValue {
    fn rvalue(self) -> RirRValue {
        RirRValue::Lambda {
            lambda: self.lambda,
            captures: vec![],
            ty: self.ty,
        }
    }
}

#[derive(Clone, Copy)]
enum PlannerLambdaSource {
    Function(FunctionId),
    Lambda(air::LambdaId),
}

struct PlannedLambdaCaptures {
    captures: Vec<RirLambdaCapture>,
    env_fields: Vec<RirLambdaEnvField>,
}

struct PlannedRValue {
    stmts: Vec<RirStmt>,
    value: RirRValue,
}

impl PlannedRValue {
    fn from_value(value: RirRValue) -> Self {
        Self {
            stmts: vec![],
            value,
        }
    }
}

struct PlannedOperand {
    stmts: Vec<RirStmt>,
    operand: RirOperand,
}

struct PlannedOwnedOperand {
    stmts: Vec<RirStmt>,
    operand: RirOwnedOperand,
}

struct PlannedOwnedValue {
    stmts: Vec<RirStmt>,
    value: RirOwnedValue,
}

impl PlannedOperand {
    fn from_operand(operand: RirOperand) -> Self {
        Self {
            stmts: vec![],
            operand,
        }
    }
}

struct PlannedOperands {
    stmts: Vec<RirStmt>,
    operands: Vec<RirOperand>,
}

struct PlannedLambdaCaptureArgs {
    stmts: Vec<RirStmt>,
    captures: Vec<RirLambdaCaptureArg>,
}

struct PlannedCollectionLoanRoot {
    root: RirCollectionAccess,
    root_kind: RirCollectionRootKind,
    mode: RirCollectionLoanMode,
}

struct PlannedCollectionAccess {
    stmts: Vec<RirStmt>,
    access: RirCollectionAccess,
}

#[derive(Clone, Copy)]
pub(super) enum CollectionAccessOp {
    ShapeLoan,
    Len,
    SequenceSlotRead,
    SequenceSlotWrite,
    SliceView,
    MapGet,
    MapEntryRead,
    MapValueRead,
    MapValueWrite,
    IndexedMapAssign,
    StructuralMutation,
}

impl CollectionAccessOp {
    pub(super) fn intent(self) -> PlaceAccessIntent {
        match self {
            Self::ShapeLoan => PlaceAccessIntent::CollectionLoan,
            Self::Len
            | Self::SequenceSlotRead
            | Self::MapGet
            | Self::MapEntryRead
            | Self::MapValueRead => PlaceAccessIntent::ReadValue,
            Self::SliceView => PlaceAccessIntent::SliceView,
            Self::SequenceSlotWrite | Self::MapValueWrite | Self::IndexedMapAssign => {
                PlaceAccessIntent::Assign
            }
            Self::StructuralMutation => PlaceAccessIntent::StructuralMutation,
        }
    }

    pub(super) fn slot(slots: &[air::AirCollectionSlot]) -> Self {
        let map = slots
            .iter()
            .any(|slot| matches!(slot.kind, air::AirCollectionSlotKind::MapValue));
        let write = slots.iter().any(|slot| slot.mutable);
        match (map, write) {
            (true, true) => Self::MapValueWrite,
            (true, false) => Self::MapValueRead,
            (false, true) => Self::SequenceSlotWrite,
            (false, false) => Self::SequenceSlotRead,
        }
    }

    pub(super) fn map_write(kind: MapWriteKind) -> Self {
        match kind {
            MapWriteKind::StructuralInsert => Self::StructuralMutation,
            MapWriteKind::IndexedAssignment => Self::IndexedMapAssign,
        }
    }
}

struct PlannedCallArg {
    stmts: Vec<RirStmt>,
    arg: RirCallArg,
}

struct PlannedMutPlaceArg {
    stmts: Vec<RirStmt>,
    arg: RirMutPlaceArg,
}

struct DataRefSegment {
    object: RirOperand,
    object_ty: TypeId,
    object_must_materialize: bool,
    place: RirDataRefPlaceId,
}

enum AssignTarget {
    CaptureCell(air::CaptureCellId),
    ScopedPlaceCell(air::ScopedBorrowId),
    ProjectedMutPlace,
    ProjectedGlobal(GlobalId),
    DataRef,
    Assign { source_mut_param: bool },
}

impl PlannedCallArg {
    fn from_arg(arg: RirCallArg) -> Self {
        Self { stmts: vec![], arg }
    }
}

impl<'a> PlanCx<'a> {
    fn new(air: &'a air::Program, config: RustPlanConfig) -> Self {
        let materializers = RustMaterializerGraph::with_native_support(&config.native_providers);
        Self {
            air,
            classes: TypePassClasses::analyze(air),
            config,
            type_map: HashMap::new(),
            lambda_sig_map: HashMap::new(),
            const_map: HashMap::new(),
            string_literal_map: HashMap::new(),
            stringify_in_progress: HashSet::new(),
            function_map: HashMap::new(),
            global_map: HashMap::new(),
            function_lambda_map: HashMap::new(),
            lambda_map: HashMap::new(),
            lambda_sources: vec![],
            function_type_copyable: HashMap::new(),
            function_type_shareable: HashMap::new(),
            lambda_runtime_capture_slots: HashMap::new(),
            lambda_capture_modes: HashMap::new(),
            capture_cell_map: HashMap::new(),
            scoped_place_cell_map: HashMap::new(),
            extern_map: HashMap::new(),
            externs: vec![],
            dataref_map: HashMap::new(),
            dataref_places: vec![],
            dataref_place_map: BTreeMap::new(),
            enum_map: HashMap::new(),
            flag_map: HashMap::new(),
            tuple_map: HashMap::new(),
            materializers,
            dynamic_layout: None,
            dynamic_types: vec![],
            dyn_surface_map: HashMap::new(),
            dyn_variant_map: HashMap::new(),
            dyn_carrier_witnesses: HashMap::new(),
            dyn_dispatch_map: HashMap::new(),
            dyn_dispatch_params: vec![],
            dyn_weakening_map: HashMap::new(),
            dyn_variant_set_map: HashMap::new(),
            dyn_variant_set_requests: vec![],
            dyn_reborrows: vec![],
        }
    }

    fn gap(site: RustTargetGapSite, kind: RustTargetGapKind) -> RustTargetGap {
        RustTargetGap { site, kind }
    }

    fn entry_gap(&self) -> Option<RustTargetGap> {
        let entry = self.air.entry()?;
        (!self.air.function(entry).signature.params.is_empty()).then(|| {
            Self::gap(
                RustTargetGapSite::Entry,
                RustTargetGapKind::UnsupportedEntry,
            )
        })
    }

    fn plan(&mut self) -> Result<RirProgram, Vec<RustTargetGap>> {
        macro_rules! stage {
            ($operation:expr) => {
                $operation.map_err(|gap| vec![gap])?
            };
        }

        let mut program = RirProgram::default();
        stage!(self.plan_types(&mut program));
        self.plan_consts(&mut program);
        self.plan_externs(&program)?;
        self.plan_function_ids();
        stage!(self.plan_globals(&mut program));
        self.plan_function_type_capture_policy(&mut program);
        self.plan_cells(&mut program);
        stage!(self.plan_scoped_place_cells(&mut program));
        stage!(self.plan_lambdas(&mut program));
        stage!(self.check_lambda_value_capture_cycles(&program));
        self.plan_function_type_copyability(&program);
        self.finalize_callable_materializers();
        self.reserve_dynamic_ids();
        stage!(self.plan_materializers(&mut program));
        program.materializers = self.materializers.freeze(&self.type_map);
        self.check_global_materializers()?;
        stage!(self.plan_collection_storages(&mut program));
        stage!(self.check_lambda_env_storage_support(&program));
        stage!(self.fill_dynamic_carriers(&mut program));
        stage!(self.plan_stringify_helpers(&mut program));

        let mut functions = Vec::with_capacity(self.air.functions.len());
        let mut gaps = self.entry_gap().into_iter().collect::<Vec<_>>();
        for index in 0..self.air.functions.len() {
            let id = FunctionId::from_index(index);
            match self.plan_function(id, &program) {
                Ok(function) => functions.push(function),
                Err(gap) => gaps.push(gap),
            }
        }
        if !gaps.is_empty() {
            return Err(gaps);
        }
        program.functions = functions;
        self.fill_dynamic_variant_sets(&mut program);
        program.dataref_places = std::mem::take(&mut self.dataref_places);
        program.externs = std::mem::take(&mut self.externs);

        let owned = analysis::owned_string_literals(&program);
        for literal in &mut program.string_literals {
            literal.needs_owned = owned.contains(&literal.id);
        }
        program.entry = self.air.entry().map(|entry| self.function_map[&entry]);
        Ok(program)
    }

    fn check_global_materializers(&self) -> Result<(), Vec<RustTargetGap>> {
        let mut gaps = vec![];
        for (index, global) in self.air.globals.iter().enumerate() {
            if self
                .materializers
                .get(global.ty, RustRecipePosition::Global)
                .is_none()
            {
                gaps.push(Self::gap(
                    RustTargetGapSite::Global(GlobalId::from_index(index)),
                    RustTargetGapKind::UnsupportedGlobalType,
                ));
            }
        }
        if gaps.is_empty() { Ok(()) } else { Err(gaps) }
    }

    fn plan_collection_storages(&mut self, program: &mut RirProgram) -> Result<(), RustTargetGap> {
        for index in 0..program.types.len() {
            let ty = TypeId::from_index(index);
            let rir_ty = program.types[index];
            let kind = match (self.air.type_arena.data(ty), rir_ty) {
                (TypeData::Array { elem, .. }, RirType::Array { elem: elem_ty, .. }) => {
                    let elem_materializer = self
                        .materializers
                        .get(
                            *elem,
                            RustRecipePosition::StoredPayload(
                                LambdaStorageFamily::FixedArrayElement,
                            ),
                        )
                        .ok_or_else(|| {
                            Self::gap(
                                RustTargetGapSite::Type(ty),
                                RustTargetGapKind::NonCopyValueRequired,
                            )
                        })?;
                    RirCollectionStorageKind::Array {
                        elem_ty,
                        elem_materializer,
                    }
                }
                (TypeData::Slice(elem), RirType::Slice(elem_ty)) => {
                    let elem_materializer = self
                        .materializers
                        .get(*elem, RustRecipePosition::Value)
                        .ok_or_else(|| {
                            Self::gap(
                                RustTargetGapSite::Type(ty),
                                RustTargetGapKind::NonCopyValueRequired,
                            )
                        })?;
                    RirCollectionStorageKind::Slice {
                        elem_ty,
                        elem_materializer,
                    }
                }
                (TypeData::List(elem), RirType::List(elem_ty)) => {
                    let elem_materializer = self
                        .materializers
                        .get(
                            *elem,
                            RustRecipePosition::StoredPayload(LambdaStorageFamily::ListElement),
                        )
                        .ok_or_else(|| {
                            Self::gap(
                                RustTargetGapSite::Type(ty),
                                RustTargetGapKind::NonCopyValueRequired,
                            )
                        })?;
                    RirCollectionStorageKind::List {
                        elem_ty,
                        elem_materializer,
                        symbol: RirSymbol::new(format!("list_storage{index}")),
                    }
                }
                (
                    TypeData::Map { key, value, .. },
                    RirType::Map {
                        key: key_ty,
                        value: value_ty,
                    },
                ) => {
                    let key_materializer = self
                        .materializers
                        .get(*key, RustRecipePosition::MapKey)
                        .ok_or_else(|| {
                            Self::gap(
                                RustTargetGapSite::Type(ty),
                                RustTargetGapKind::UnsupportedMapKey,
                            )
                        })?;
                    let value_materializer = self
                        .materializers
                        .get(
                            *value,
                            RustRecipePosition::StoredPayload(LambdaStorageFamily::MapValue),
                        )
                        .ok_or_else(|| {
                            Self::gap(
                                RustTargetGapSite::Type(ty),
                                RustTargetGapKind::NonCopyValueRequired,
                            )
                        })?;
                    RirCollectionStorageKind::Map {
                        key_ty,
                        value_ty,
                        key_materializer,
                        value_materializer,
                        symbol: RirSymbol::new(format!("map_storage{index}")),
                    }
                }
                _ => continue,
            };
            let id = RirCollectionStorageId::from_index(program.collection_storages.len());
            program.collection_storages.push(RirCollectionStorage {
                id,
                value_ty: RirTypeId::from_index(index),
                kind,
            });
        }
        Ok(())
    }

    fn plan_types(&mut self, program: &mut RirProgram) -> Result<(), RustTargetGap> {
        for index in 0..self.air.type_arena.len() {
            self.type_map
                .insert(TypeId::from_index(index), RirTypeId::from_index(index));
        }

        let dynamic_layout = self
            .air_policy()
            .dynamic_layout_plan()
            .map_err(|_| Self::gap(RustTargetGapSite::Entry, RustTargetGapKind::UnsupportedType))?;
        let mut dynamic_enums = BTreeMap::new();
        for surface in &dynamic_layout.declaration_order {
            let id = RirEnumId::from_index(program.enums.len());
            let display = &self.air.contract_surfaces[surface.index()].display_name;
            program.enums.push(RirEnum {
                id,
                role: RirEnumRole::DynamicCarrier,
                native: None,
                native_layout: None,
                core: None,
                repr: RirEnumRepr::Adt,
                raw_type: None,
                symbol: RirSymbol::new(format!(
                    "{}Dyn{}_{}",
                    sanitize(&self.config.symbol_prefix),
                    sanitize(display),
                    surface.index()
                )),
                display: RirSymbol::new(display.clone()),
                variants: vec![],
            });
            dynamic_enums.insert(*surface, id);
        }

        let mut aggregate_types = vec![];
        let mut dynamic_types = vec![];
        let mut extern_types = vec![];
        let mut extern_enum_types = vec![];
        let mut enum_types = vec![];
        let mut dataref_types = vec![];
        for (index, ty) in self.air.type_arena.iter().enumerate() {
            debug_assert_eq!(program.types.len(), index);
            let type_id = TypeId::from_index(index);
            let rir = match ty {
                TypeData::Int => RirType::Int,
                TypeData::Float => RirType::Float,
                TypeData::Bool => RirType::Bool,
                TypeData::String => RirType::String,
                TypeData::Char => RirType::Char,
                TypeData::Void => RirType::Void,
                TypeData::Aggregate(aggregate) => {
                    let struct_id = self.reserve_struct(program, type_id, *aggregate)?;
                    aggregate_types.push((type_id, *aggregate, struct_id));
                    RirType::Struct(struct_id)
                }
                TypeData::Enum(enm) => {
                    let enum_id = self.reserve_enum(program, type_id, *enm);
                    enum_types.push((type_id, *enm, enum_id));
                    RirType::Enum(enum_id)
                }
                TypeData::Flag(flag) => {
                    let flag_id = self.reserve_flag(program, type_id, *flag);
                    RirType::Flag(flag_id)
                }
                TypeData::DataRef(aggregate) => {
                    let dataref_id = self.reserve_dataref(program, type_id, *aggregate)?;
                    dataref_types.push((type_id, *aggregate, dataref_id));
                    RirType::DataRef(dataref_id)
                }
                TypeData::Extern(ext) => {
                    let decl = self.air.extern_type(*ext);
                    if decl.variants.is_empty() {
                        let struct_id = self.reserve_extern_struct(program, type_id, *ext)?;
                        extern_types.push((type_id, *ext, struct_id));
                        RirType::Struct(struct_id)
                    } else {
                        let enum_id = self.reserve_extern_enum(program, type_id, *ext)?;
                        extern_enum_types.push((type_id, *ext, enum_id));
                        RirType::Enum(enum_id)
                    }
                }
                TypeData::Array { elem, len } => RirType::Array {
                    elem: self.type_map[elem],
                    len: *len as u64,
                },
                TypeData::List(elem) => RirType::List(self.type_map[elem]),
                TypeData::Map { key, value, .. } => RirType::Map {
                    key: self.type_map[key],
                    value: self.type_map[value],
                },
                TypeData::Optional(inner) => RirType::Option(self.type_map[inner]),
                TypeData::Tuple(elems) => {
                    let fields = elems
                        .iter()
                        .map(|elem| self.type_map[elem])
                        .collect::<Vec<_>>();
                    let tuple_id = self.intern_tuple(program, type_id, fields);
                    RirType::Tuple(tuple_id)
                }
                TypeData::Slice(elem) => RirType::Slice(self.type_map[elem]),
                TypeData::Function(sig) => {
                    if matches!(sig.ret, ReturnMode::Place(_)) {
                        return Err(Self::gap(
                            RustTargetGapSite::Type(type_id),
                            RustTargetGapKind::UnsupportedReturnMode,
                        ));
                    }
                    let policy = RirRustRepPolicy::new(program);
                    let supported = sig.params.iter().all(|param| {
                        let mode = rir::source_pass_mode(self.air, param.ty, param.mode);
                        policy.supports_param(self.type_map[&param.ty], mode)
                    });
                    if !supported {
                        return Err(Self::gap(
                            RustTargetGapSite::Type(type_id),
                            RustTargetGapKind::UnsupportedParamMode,
                        ));
                    }
                    let sig = self.intern_lambda_sig(program, sig);
                    self.lambda_sig_map.insert(type_id, sig);
                    RirType::Lambda(sig)
                }
                TypeData::Dyn(surface) => {
                    let enum_id = dynamic_enums[surface];
                    dynamic_types.push((type_id, *surface, enum_id));
                    RirType::Enum(enum_id)
                }
                TypeData::Any => {
                    return Err(Self::gap(
                        RustTargetGapSite::Type(type_id),
                        RustTargetGapKind::UnsupportedType,
                    ));
                }
            };
            program.types.push(rir);
        }
        for &(type_id, aggregate, struct_id) in &aggregate_types {
            self.fill_struct(program, type_id, aggregate, struct_id)?;
        }
        for &(type_id, ext, struct_id) in &extern_types {
            self.fill_extern_struct(program, type_id, ext, struct_id)?;
        }
        for &(type_id, ext, enum_id) in &extern_enum_types {
            self.fill_extern_enum(program, type_id, ext, enum_id)?;
        }
        for &(_, aggregate, dataref_id) in &dataref_types {
            self.fill_dataref(program, aggregate, dataref_id)?;
        }
        for &(type_id, enm, enum_id) in &enum_types {
            self.fill_enum(program, type_id, enm, enum_id)?;
        }
        self.dynamic_layout = Some(dynamic_layout);
        self.dynamic_types.clone_from(&dynamic_types);
        Ok(())
    }

    fn intern_lambda_sig(
        &self,
        program: &mut RirProgram,
        sig: &air::SignatureType,
    ) -> RirLambdaSigId {
        let params = sig
            .params
            .iter()
            .map(|param| {
                let mode = rir::source_pass_mode(self.air, param.ty, param.mode);
                RirLambdaParam {
                    ty: self.type_map[&param.ty],
                    mode,
                    escape: rir_param_escape(param.escape),
                }
            })
            .collect::<Vec<_>>();
        let ret = self.type_map[&sig.ret.ty()];
        if let Some(existing) = program
            .lambda_sigs
            .iter()
            .find(|existing| existing.params == params && existing.ret == ret)
        {
            return existing.id;
        }
        let id = RirLambdaSigId::from_index(program.lambda_sigs.len());
        program.lambda_sigs.push(RirLambdaSig { id, params, ret });
        id
    }

    fn intern_tuple(
        &mut self,
        program: &mut RirProgram,
        type_id: TypeId,
        fields: Vec<RirTypeId>,
    ) -> RirTupleId {
        if let Some(id) = self.tuple_map.get(&fields) {
            return *id;
        }
        let id = RirTupleId::from_index(program.tuples.len());
        let field_decls = fields
            .iter()
            .enumerate()
            .map(|(index, ty)| RirField {
                id: RirFieldId::from_index(index),
                symbol: RirSymbol::new(format!("_{index}")),
                ty: *ty,
            })
            .collect();
        program.tuples.push(RirTuple {
            id,
            symbol: RirSymbol::new(format!(
                "{}T{}_Tuple",
                self.config.symbol_prefix,
                type_id.index()
            )),
            display: RirSymbol::new(format!("tuple{}", type_id.index())),
            fields: field_decls,
        });
        self.tuple_map.insert(fields, id);
        id
    }

    fn reserve_struct(
        &mut self,
        program: &mut RirProgram,
        type_id: TypeId,
        aggregate: air::AggregateId,
    ) -> Result<RirStructId, RustTargetGap> {
        let decl = self.air.aggregate(aggregate);
        if decl.kind != air::AggregateKind::Struct {
            return Err(Self::gap(
                RustTargetGapSite::Type(type_id),
                RustTargetGapKind::UnsupportedType,
            ));
        }
        let id = RirStructId::from_index(program.structs.len());
        program.structs.push(RirStruct {
            id,
            role: RirStructRole::Source,
            symbol: RirSymbol::new(format!(
                "{}T{}_{}",
                self.config.symbol_prefix,
                type_id.index(),
                sanitize(decl.name.as_str())
            )),
            display: RirSymbol::new(decl.name.as_str()),
            native: None,
            native_layout: None,
            native_ref: false,
            fields: vec![],
        });
        Ok(id)
    }

    fn fill_struct(
        &self,
        program: &mut RirProgram,
        type_id: TypeId,
        aggregate: air::AggregateId,
        struct_id: RirStructId,
    ) -> Result<(), RustTargetGap> {
        let decl = self.air.aggregate(aggregate);
        let mut seen = vec![];
        let mut fields = vec![];
        for (index, field) in decl.fields.iter().enumerate() {
            let Some(&ty) = self.type_map.get(&field.ty) else {
                return Err(Self::gap(
                    RustTargetGapSite::Type(field.ty),
                    RustTargetGapKind::UnsupportedType,
                ));
            };
            if field.ty == type_id {
                return Err(Self::gap(
                    RustTargetGapSite::Type(field.ty),
                    RustTargetGapKind::UnsupportedType,
                ));
            }
            fields.push(RirField {
                id: RirFieldId::from_index(index),
                symbol: scoped_symbol(field.name.as_str(), &mut seen),
                ty,
            });
        }
        program.structs[struct_id.index()].fields = fields;
        Ok(())
    }

    fn reserve_dataref(
        &mut self,
        program: &mut RirProgram,
        type_id: TypeId,
        aggregate: air::AggregateId,
    ) -> Result<RirDataRefId, RustTargetGap> {
        let decl = self.air.aggregate(aggregate);
        if decl.kind != air::AggregateKind::DataRef {
            return Err(Self::gap(
                RustTargetGapSite::Type(type_id),
                RustTargetGapKind::UnsupportedType,
            ));
        }
        let id = RirDataRefId::from_index(program.datarefs.len());
        self.dataref_map.insert(aggregate, id);
        let base = format!(
            "{}T{}_{}",
            self.config.symbol_prefix,
            type_id.index(),
            sanitize(decl.name.as_str())
        );
        program.datarefs.push(RirDataRef {
            id,
            native_key: None,
            symbol: RirSymbol::new(&base),
            display: RirSymbol::new(decl.name.as_str()),
            cycle_capable: decl.cycle_capable,
            fields: vec![],
        });
        Ok(id)
    }

    fn fill_dataref(
        &self,
        program: &mut RirProgram,
        aggregate: air::AggregateId,
        dataref_id: RirDataRefId,
    ) -> Result<(), RustTargetGap> {
        let decl = self.air.aggregate(aggregate);
        let mut seen = vec![];
        let mut fields = vec![];
        for (index, field) in decl.fields.iter().enumerate() {
            let Some(&ty) = self.type_map.get(&field.ty) else {
                return Err(Self::gap(
                    RustTargetGapSite::Type(field.ty),
                    RustTargetGapKind::UnsupportedType,
                ));
            };
            fields.push(RirField {
                id: RirFieldId::from_index(index),
                symbol: scoped_symbol(field.name.as_str(), &mut seen),
                ty,
            });
        }
        program.datarefs[dataref_id.index()].fields = fields;
        Ok(())
    }

    fn reserve_extern_struct(
        &mut self,
        program: &mut RirProgram,
        type_id: TypeId,
        ext: air::ExternTypeId,
    ) -> Result<RirStructId, RustTargetGap> {
        let decl = self.air.extern_type(ext);
        if !matches!(decl.rep, air::ExternRep::Inline | air::ExternRep::Shared) {
            return Err(Self::gap(
                RustTargetGapSite::Type(type_id),
                RustTargetGapKind::UnsupportedType,
            ));
        }
        let id = RirStructId::from_index(program.structs.len());
        let native = self.native_type_binding(type_id, decl)?;
        let native_ref = decl.rep == air::ExternRep::Shared;
        program.structs.push(RirStruct {
            id,
            role: RirStructRole::Extern,
            symbol: RirSymbol::new(format!(
                "{}T{}_{}",
                self.config.symbol_prefix,
                type_id.index(),
                sanitize(decl.name.as_str())
            )),
            display: RirSymbol::new(decl.name.as_str()),
            native: Some(native.clone()),
            native_layout: decl.layout.map(|layout| rir::RirDynLayout {
                size: layout.size,
                align: layout.align,
            }),
            native_ref,
            fields: vec![],
        });
        Ok(id)
    }

    fn fill_extern_struct(
        &self,
        program: &mut RirProgram,
        _type_id: TypeId,
        ext: air::ExternTypeId,
        struct_id: RirStructId,
    ) -> Result<(), RustTargetGap> {
        let decl = self.air.extern_type(ext);
        if decl.rep == air::ExternRep::Shared {
            program.structs[struct_id.index()].fields = vec![];
            return Ok(());
        }
        let mut seen = vec![];
        let fields = self
            .inline_extern_storage_fields(ext)
            .map(|(_, field, id)| {
                let Some(&ty) = self.type_map.get(&field.ty) else {
                    return Err(Self::gap(
                        RustTargetGapSite::Type(field.ty),
                        RustTargetGapKind::UnsupportedType,
                    ));
                };
                Ok(RirField {
                    id,
                    symbol: scoped_symbol(field.name.as_str(), &mut seen),
                    ty,
                })
            })
            .collect::<Result<Vec<_>, RustTargetGap>>()?;
        program.structs[struct_id.index()].fields = fields;
        Ok(())
    }

    fn reserve_extern_enum(
        &mut self,
        program: &mut RirProgram,
        type_id: TypeId,
        ext: air::ExternTypeId,
    ) -> Result<RirEnumId, RustTargetGap> {
        let decl = self.air.extern_type(ext);
        if decl.rep != air::ExternRep::Inline {
            return Err(Self::gap(
                RustTargetGapSite::Type(type_id),
                RustTargetGapKind::UnsupportedType,
            ));
        }
        let id = RirEnumId::from_index(program.enums.len());
        let native = self.native_type_binding(type_id, decl)?;
        program.enums.push(RirEnum {
            id,
            role: RirEnumRole::Extern,
            native: Some(native.clone()),
            native_layout: decl.layout.map(|layout| rir::RirDynLayout {
                size: layout.size,
                align: layout.align,
            }),
            core: None,
            repr: RirEnumRepr::Adt,
            raw_type: None,
            symbol: RirSymbol::new(format!(
                "{}T{}_{}",
                self.config.symbol_prefix,
                type_id.index(),
                sanitize(decl.name.as_str())
            )),
            display: RirSymbol::new(decl.name.as_str()),
            variants: vec![],
        });
        Ok(id)
    }

    fn fill_extern_enum(
        &self,
        program: &mut RirProgram,
        type_id: TypeId,
        ext: air::ExternTypeId,
        enum_id: RirEnumId,
    ) -> Result<(), RustTargetGap> {
        let decl = self.air.extern_type(ext);
        let mut seen_variants = vec![];
        let mut variants = vec![];
        for (variant_index, variant) in decl.variants.iter().enumerate() {
            let (kind, fields) = match &variant.shape {
                air::VariantShape::Unit => (RirVariantKind::Unit, vec![]),
                air::VariantShape::Tuple(types) => {
                    let fields = types
                        .iter()
                        .enumerate()
                        .map(|(index, ty)| self.enum_field(type_id, *ty, index))
                        .collect::<Result<Vec<_>, _>>()?;
                    (RirVariantKind::Tuple, fields)
                }
                air::VariantShape::Struct(fields) => {
                    let mut seen_fields = vec![];
                    let fields = fields
                        .iter()
                        .enumerate()
                        .map(|(index, field)| {
                            self.enum_field(type_id, field.ty, index).map(|mut rir| {
                                rir.symbol = scoped_symbol(field.name.as_str(), &mut seen_fields);
                                rir
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    (RirVariantKind::Struct, fields)
                }
            };
            variants.push(RirVariant {
                id: RirVariantId::from_index(variant_index),
                symbol: scoped_symbol(variant.name.as_str(), &mut seen_variants),
                display: RirSymbol::new(variant.name.as_str()),
                kind,
                raw_value: None,
                fields,
            });
        }
        program.enums[enum_id.index()].variants = variants;
        Ok(())
    }

    fn reserve_flag(
        &mut self,
        program: &mut RirProgram,
        type_id: TypeId,
        flag: air::FlagId,
    ) -> RirFlagId {
        let decl = self.air.flag_decl(flag);
        let id = RirFlagId::from_index(program.flags.len());
        self.flag_map.insert(flag, id);
        let mut seen = target::flag_reserved_associated_symbols()
            .iter()
            .map(|name| (*name).to_string())
            .collect();
        let members = decl
            .members
            .iter()
            .map(|member| RirFlagMember {
                id: RirFlagMemberId::from_index(member.id.index()),
                symbol: scoped_symbol(&member.name.as_str().to_ascii_uppercase(), &mut seen),
                display: RirSymbol::new(member.name.as_str()),
                value: member.value,
                atomic: member.atomic,
            })
            .collect();
        program.flags.push(RirFlag {
            id,
            symbol: RirSymbol::new(format!(
                "{}T{}_{}",
                self.config.symbol_prefix,
                type_id.index(),
                sanitize(decl.name.as_str())
            )),
            display: RirSymbol::new(decl.name.as_str()),
            known_bits: decl.known_bits,
            members,
        });
        id
    }

    fn reserve_enum(
        &mut self,
        program: &mut RirProgram,
        type_id: TypeId,
        enm: air::EnumId,
    ) -> RirEnumId {
        let decl = self.air.enum_decl(enm);
        let id = RirEnumId::from_index(program.enums.len());
        self.enum_map.insert(enm, id);
        program.enums.push(RirEnum {
            id,
            role: RirEnumRole::Source,
            native: None,
            native_layout: None,
            core: decl.core.map(rir_core_enum_kind),
            repr: rir_enum_repr(decl.repr),
            raw_type: decl.raw_type.map(|ty| self.type_map[&ty]),
            symbol: RirSymbol::new(format!(
                "{}T{}_{}",
                self.config.symbol_prefix,
                type_id.index(),
                sanitize(decl.name.as_str())
            )),
            display: RirSymbol::new(decl.name.as_str()),
            variants: vec![],
        });
        id
    }

    fn fill_enum(
        &mut self,
        program: &mut RirProgram,
        type_id: TypeId,
        enm: air::EnumId,
        enum_id: RirEnumId,
    ) -> Result<(), RustTargetGap> {
        let decl = self.air.enum_decl(enm);
        let mut seen_variants = vec![];
        let mut variants = vec![];
        for (variant_index, variant) in decl.variants.iter().enumerate() {
            let (kind, fields) = match &variant.shape {
                air::VariantShape::Unit => (RirVariantKind::Unit, vec![]),
                air::VariantShape::Tuple(types) => {
                    let fields = types
                        .iter()
                        .enumerate()
                        .map(|(index, ty)| self.enum_field(type_id, *ty, index))
                        .collect::<Result<Vec<_>, _>>()?;
                    (RirVariantKind::Tuple, fields)
                }
                air::VariantShape::Struct(fields) => {
                    let mut seen_fields = vec![];
                    let fields = fields
                        .iter()
                        .enumerate()
                        .map(|(index, field)| {
                            self.enum_field(type_id, field.ty, index).map(|mut rir| {
                                rir.symbol = scoped_symbol(field.name.as_str(), &mut seen_fields);
                                rir
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    (RirVariantKind::Struct, fields)
                }
            };
            let raw_value = variant
                .raw_value
                .as_ref()
                .map(|value| self.rir_raw_enum_value(program, value));
            variants.push(RirVariant {
                id: RirVariantId::from_index(variant_index),
                symbol: scoped_symbol(variant.name.as_str(), &mut seen_variants),
                display: RirSymbol::new(variant.name.as_str()),
                kind,
                raw_value,
                fields,
            });
        }
        program.enums[enum_id.index()].variants = variants;
        Ok(())
    }

    fn plan_dynamic_weakenings(&mut self, program: &mut RirProgram) {
        for (index, weakening) in self.air.contract_weakenings.iter().enumerate() {
            let air_id = air::ContractWeakeningId::from_index(index);
            let (Some(&source), Some(&target)) = (
                self.dyn_surface_map.get(&weakening.source),
                self.dyn_surface_map.get(&weakening.target),
            ) else {
                continue;
            };
            let source_carrier = &program.dyn_carriers[source.index()];
            let target_carrier = &program.dyn_carriers[target.index()];
            let mut arms = Vec::with_capacity(source_carrier.variants.len());
            for (source_index, source_variant) in source_carrier.variants.iter().enumerate() {
                let source_id = RirDynVariantId::new(source, source_index);
                let source_witness = &self.air.contract_witnesses
                    [self.dyn_carrier_witnesses[&source][source_index].index()];
                let target_index = target_carrier
                    .variants
                    .iter()
                    .enumerate()
                    .find_map(|(target_index, target_variant)| {
                        let target_witness = &self.air.contract_witnesses
                            [self.dyn_carrier_witnesses[&target][target_index].index()];
                        (target_witness.key.concrete_ty == source_witness.key.concrete_ty
                            && target_witness.key.slots.len() == weakening.target_to_source.len()
                            && target_witness
                                .key
                                .slots
                                .iter()
                                .zip(&weakening.target_to_source)
                                .all(|(target_slot, source_slot)| {
                                    source_witness
                                        .key
                                        .slots
                                        .iter()
                                        .find(|slot| slot.slot == *source_slot)
                                        .is_some_and(|source_slot| {
                                            source_slot.target == target_slot.target
                                        })
                                })
                            && source_variant.storage == target_variant.storage)
                            .then_some(target_index)
                    })
                    .expect("representation plan verifies weakening witnesses");
                debug_assert_eq!(source_id, RirDynVariantId::new(source, source_index));
                arms.push(rir::RirDynWeakenArm {
                    target: RirDynVariantId::new(target, target_index),
                });
            }
            let id = RirDynWeakeningId::from_index(program.dyn_weakenings.len());
            program.dyn_weakenings.push(rir::RirDynWeakening {
                id,
                source,
                target,
                arms,
            });
            self.dyn_weakening_map.insert(air_id, id);
        }
    }

    fn reserve_dynamic_ids(&mut self) {
        let layout = self
            .dynamic_layout
            .as_ref()
            .expect("dynamic layout planned with types");
        for (carrier_index, carrier) in layout.carriers.iter().enumerate() {
            let carrier_id = RirDynCarrierId::from_index(carrier_index);
            self.dyn_surface_map.insert(carrier.surface, carrier_id);
            let witnesses = carrier
                .variants
                .iter()
                .enumerate()
                .map(|(variant_index, variant)| {
                    self.dyn_variant_map.insert(
                        variant.witness,
                        RirDynVariantId::new(carrier_id, variant_index),
                    );
                    variant.witness
                })
                .collect();
            self.dyn_carrier_witnesses.insert(carrier_id, witnesses);
        }
    }

    fn fill_dynamic_carriers(&mut self, program: &mut RirProgram) -> Result<(), RustTargetGap> {
        let layout = self
            .dynamic_layout
            .take()
            .expect("dynamic layout planned with types");
        for carrier in &layout.carriers {
            let (type_id, _, enum_id) = self
                .dynamic_types
                .iter()
                .find(|(_, surface, _)| *surface == carrier.surface)
                .copied()
                .ok_or_else(|| {
                    Self::gap(RustTargetGapSite::Entry, RustTargetGapKind::UnsupportedType)
                })?;
            self.fill_dynamic_carrier(program, type_id, enum_id, carrier)?;
        }
        self.plan_dynamic_weakenings(program);
        Ok(())
    }

    fn plan_resolved_call_target(
        &self,
        target: &air::ContractWitnessTarget,
        receiver_ty: TypeId,
    ) -> RirResolvedCallTarget {
        match target {
            air::ContractWitnessTarget::Function { function }
            | air::ContractWitnessTarget::IteratorFunction { function } => {
                RirResolvedCallTarget::Function(self.function_map[function])
            }
            air::ContractWitnessTarget::Extern { function } => {
                RirResolvedCallTarget::Extern(self.extern_map[function])
            }
            air::ContractWitnessTarget::Promoted { fields, target } => {
                let mut source_ty = receiver_ty;
                let projections = fields
                    .iter()
                    .map(|field_id| {
                        let TypeData::Aggregate(aggregate) = self.air.type_arena.data(source_ty)
                        else {
                            unreachable!("AIR verifies promoted dynamic receiver fields")
                        };
                        let field = &self.air.aggregate(*aggregate).fields[field_id.index()];
                        let step = RirPlaceStep {
                            source_ty: self.type_map[&source_ty],
                            target_ty: self.type_map[&field.ty],
                            kind: RirPlaceStepKind::StructField(RirFieldId::from_index(
                                field_id.index(),
                            )),
                        };
                        source_ty = field.ty;
                        step
                    })
                    .collect();
                RirResolvedCallTarget::Promoted {
                    projections,
                    target: Box::new(self.plan_resolved_call_target(target, source_ty)),
                }
            }
        }
    }

    fn resolved_receiver_mode(
        &self,
        target: &RirResolvedCallTarget,
        fallback: RirPassMode,
    ) -> RirPassMode {
        match target.base() {
            RirResolvedCallTarget::Function(_) => fallback,
            RirResolvedCallTarget::Extern(id) => self.externs[id.index()].params[0].mode,
            RirResolvedCallTarget::Promoted { .. } => unreachable!(),
        }
    }

    fn dyn_target_receiver_mode(&self, target: &air::ContractWitnessTarget) -> ParamMode {
        match target {
            air::ContractWitnessTarget::Function { function }
            | air::ContractWitnessTarget::IteratorFunction { function } => {
                self.air.functions[function.index()].signature.params[0].mode
            }
            air::ContractWitnessTarget::Extern { function } => {
                let air::ExternMember::Method { receiver, .. } =
                    &self.air.externs[function.index()].member
                else {
                    unreachable!("verified dynamic extern target is a method")
                };
                receiver.mode
            }
            air::ContractWitnessTarget::Promoted { target, .. } => {
                self.dyn_target_receiver_mode(target)
            }
        }
    }

    fn dyn_target_params(&self, target: &air::ContractWitnessTarget) -> Vec<RirDynDispatchParam> {
        match target {
            air::ContractWitnessTarget::Function { function }
            | air::ContractWitnessTarget::IteratorFunction { function } => self.air.functions
                [function.index()]
            .signature
            .params
            .iter()
            .skip(1)
            .map(|param| RirDynDispatchParam {
                ty: self.type_map[&param.ty],
                mode: rir::source_pass_mode(self.air, param.ty, param.mode),
                escape: rir_param_escape(param.escape),
            })
            .collect(),
            air::ContractWitnessTarget::Extern { function } => self.externs
                [self.extern_map[function].index()]
            .params
            .iter()
            .skip(1)
            .map(|param| RirDynDispatchParam {
                ty: param.ty,
                mode: param.mode,
                escape: param.escape,
            })
            .collect(),
            air::ContractWitnessTarget::Promoted { target, .. } => self.dyn_target_params(target),
        }
    }

    fn fill_dynamic_carrier(
        &mut self,
        program: &mut RirProgram,
        type_id: TypeId,
        enum_id: RirEnumId,
        plan: &RustDynamicCarrierPlan,
    ) -> Result<(), RustTargetGap> {
        self.materializers
            .get(type_id, RustRecipePosition::Value)
            .ok_or_else(|| {
                Self::gap(
                    RustTargetGapSite::Type(type_id),
                    RustTargetGapKind::UnsupportedDynamicPayload,
                )
            })?;
        if let Err(gap) = plan.layout
            && !matches!(gap, RustLayoutGap::FunctionLayoutUnknown(_))
        {
            return Err(Self::gap(
                RustTargetGapSite::Type(type_id),
                RustTargetGapKind::UnsupportedDynamicLayout,
            ));
        }
        if let Err(gap) = plan.lifecycle
            && !matches!(gap, RustLifecycleGap::FunctionMetadataUnknown(_))
        {
            return Err(Self::gap(
                RustTargetGapSite::Type(type_id),
                RustTargetGapKind::UnsupportedDynamicLifecycle,
            ));
        }
        let carrier_id = self.dyn_surface_map[&plan.surface];
        debug_assert_eq!(carrier_id.index(), program.dyn_carriers.len());
        let mut variants = Vec::with_capacity(plan.variants.len());
        let mut enum_variants = Vec::with_capacity(plan.variants.len());
        for (index, variant) in plan.variants.iter().enumerate() {
            if let Err(gap) = variant.inline_payload_layout
                && !matches!(gap, RustLayoutGap::RecursiveInline(_))
                && !(matches!(gap, RustLayoutGap::FunctionLayoutUnknown(_))
                    && matches!(
                        variant.box_reason,
                        Some(RustDynamicBoxReason::Function | RustDynamicBoxReason::FunctionField)
                    ))
            {
                return Err(Self::gap(
                    RustTargetGapSite::Type(variant.concrete_ty),
                    RustTargetGapKind::UnsupportedDynamicLayout,
                ));
            }
            if let Err(gap) = variant.lifecycle
                && !matches!(gap, RustLifecycleGap::FunctionMetadataUnknown(_))
            {
                return Err(Self::gap(
                    RustTargetGapSite::Type(variant.concrete_ty),
                    RustTargetGapKind::UnsupportedDynamicLifecycle,
                ));
            }
            let payload = self
                .materializers
                .get(
                    variant.concrete_ty,
                    RustRecipePosition::StoredPayload(LambdaStorageFamily::DynamicPayload),
                )
                .ok_or_else(|| {
                    Self::gap(
                        RustTargetGapSite::Type(variant.concrete_ty),
                        RustTargetGapKind::UnsupportedDynamicPayload,
                    )
                })?;
            let storage = match variant.storage {
                RustPayloadStorage::Inline => RirDynStorage::Inline,
                RustPayloadStorage::Boxed => RirDynStorage::Boxed,
            };
            let concrete_ty = self.type_map[&variant.concrete_ty];
            let box_reason = variant.box_reason.map(|reason| match reason {
                RustDynamicBoxReason::Function => RirDynBoxReason::Function,
                RustDynamicBoxReason::FunctionField => RirDynBoxReason::FunctionField,
                RustDynamicBoxReason::Recursive => RirDynBoxReason::Recursive,
                RustDynamicBoxReason::Threshold => RirDynBoxReason::Threshold,
                RustDynamicBoxReason::WeakeningClass(surface) => {
                    RirDynBoxReason::WeakeningClass(self.dyn_surface_map[&surface])
                }
            });
            let variant_id = self.dyn_variant_map[&variant.witness];
            debug_assert_eq!(variant_id, RirDynVariantId::new(carrier_id, index));
            variants.push(RirDynVariant {
                id: variant_id,
                concrete_ty,
                storage,
                inline_layout: match variant.inline_payload_layout {
                    Ok(layout) => rir::RirDynInlineLayout::Known(rir::RirDynLayout {
                        size: layout.size,
                        align: layout.align,
                    }),
                    Err(RustLayoutGap::FunctionLayoutUnknown(_)) => {
                        rir::RirDynInlineLayout::Function
                    }
                    Err(RustLayoutGap::RecursiveInline(_) | RustLayoutGap::RecursiveCarrier(_)) => {
                        rir::RirDynInlineLayout::Recursive
                    }
                    Err(RustLayoutGap::ProviderInlineLayoutUnknown(_)) => {
                        rir::RirDynInlineLayout::Provider
                    }
                    Err(
                        RustLayoutGap::UnsupportedType(_)
                        | RustLayoutGap::MissingWeakeningWitness { .. },
                    ) => rir::RirDynInlineLayout::Unsupported,
                    Err(RustLayoutGap::ArithmeticOverflow) => rir::RirDynInlineLayout::Overflow,
                },
                box_reason,
                payload,
            });
            enum_variants.push(RirVariant {
                id: RirVariantId::from_index(index),
                symbol: RirSymbol::new(format!("Witness{}", variant.witness.index())),
                display: RirSymbol::new(format!("Witness{}", variant.witness.index())),
                kind: RirVariantKind::Tuple,
                raw_value: None,
                fields: vec![RirField {
                    id: RirFieldId::from_index(0),
                    symbol: RirSymbol::new("value"),
                    ty: concrete_ty,
                }],
            });
        }
        program.enums[enum_id.index()].variants = enum_variants;
        for slot in &self.air.contract_surfaces[plan.surface.index()].slots {
            let mut arms = Vec::with_capacity(variants.len());
            let mut params = None;
            for plan_variant in &plan.variants {
                let witness = &self.air.contract_witnesses[plan_variant.witness.index()];
                let target = &witness
                    .key
                    .slots
                    .iter()
                    .find(|witness| witness.slot == slot.id)
                    .expect("verified witness slot")
                    .target;
                let fallback = rir::source_pass_mode(
                    self.air,
                    witness.key.concrete_ty,
                    self.dyn_target_receiver_mode(target),
                );
                let target_params = self.dyn_target_params(target);
                let target = self.plan_resolved_call_target(target, witness.key.concrete_ty);
                if let RirResolvedCallTarget::Extern(id) = target.base()
                    && self.externs[id.index()]
                        .params
                        .first()
                        .is_some_and(|param| {
                            param.action == native_call::NativeArgAction::RejectLiveBoundary
                        })
                {
                    return Err(Self::gap(
                        RustTargetGapSite::Type(witness.key.concrete_ty),
                        RustTargetGapKind::UnsupportedCallArgMode,
                    ));
                }
                if params.is_none() {
                    params = Some(target_params);
                }
                let receiver = self.resolved_receiver_mode(&target, fallback);
                arms.push(RirDynDispatchArm { receiver, target });
            }
            let id = RirDynDispatchId::from_index(program.dyn_dispatches.len());
            let params = params.unwrap_or_else(|| {
                slot.params
                    .iter()
                    .map(|param| RirDynDispatchParam {
                        ty: self.type_map[&param.ty],
                        mode: rir::source_pass_mode(self.air, param.ty, param.mode),
                        escape: rir_param_escape(param.escape),
                    })
                    .collect()
            });
            self.dyn_dispatch_params.push(params.clone());
            program.dyn_dispatches.push(RirDynDispatch {
                id,
                carrier: carrier_id,
                params,
                result_ty: self.type_map[&match slot.ret {
                    air::ContractReturnDecl::Value(ty) => ty,
                    air::ContractReturnDecl::Place(_) | air::ContractReturnDecl::Iter => {
                        unreachable!("dynamic value return")
                    }
                }],
                arms,
            });
            self.dyn_dispatch_map.insert((plan.surface, slot.id), id);
        }
        program.dyn_carriers.push(RirDynCarrier {
            id: carrier_id,
            storage_ty: self.type_map[&type_id],
            variants,
        });
        Ok(())
    }

    fn enum_field(
        &self,
        enum_ty: TypeId,
        ty: TypeId,
        index: usize,
    ) -> Result<RirField, RustTargetGap> {
        let Some(&rir_ty) = self.type_map.get(&ty) else {
            return Err(Self::gap(
                RustTargetGapSite::Type(ty),
                RustTargetGapKind::UnsupportedType,
            ));
        };
        let recursive = ty == enum_ty;
        if recursive {
            return Err(Self::gap(
                RustTargetGapSite::Type(ty),
                RustTargetGapKind::UnsupportedType,
            ));
        }
        Ok(RirField {
            id: RirFieldId::from_index(index),
            symbol: RirSymbol::new(format!("f{index}")),
            ty: rir_ty,
        })
    }

    fn plan_stringify_helpers(&mut self, program: &mut RirProgram) -> Result<(), RustTargetGap> {
        let mut tys = vec![];
        for function in &self.air.functions {
            function.body.for_each_rvalue(&mut |value| {
                if let RValue::Stringify { source_ty, .. } = value {
                    tys.push(*source_ty);
                }
            });
        }
        for ty in tys {
            self.require_stringify(program, ty)?;
        }
        Ok(())
    }

    fn require_stringify(
        &mut self,
        program: &mut RirProgram,
        ty: TypeId,
    ) -> Result<(), RustTargetGap> {
        let rir_ty = self.type_map[&ty];
        if matches!(
            self.air.type_arena.data(ty),
            TypeData::Int | TypeData::Float | TypeData::Bool | TypeData::String | TypeData::Char
        ) {
            return Ok(());
        }
        if program.stringify_req(rir_ty).is_some() || !self.stringify_in_progress.insert(rir_ty) {
            return Ok(());
        }
        let kind = match self.air.type_arena.data(ty) {
            TypeData::Aggregate(aggregate) => {
                self.classify_aggregate_stringify(program, ty, *aggregate)?
            }
            TypeData::Enum(enm) => {
                let helper = self.require_enum_stringify_helper(program, ty, *enm)?;
                RirStringifyReqKind::Helper(helper)
            }
            TypeData::Flag(flag) => {
                let helper = self.require_flag_stringify_helper(program, ty, *flag);
                RirStringifyReqKind::Helper(helper)
            }
            _ => {
                return Err(Self::gap(
                    RustTargetGapSite::Type(ty),
                    RustTargetGapKind::UnsupportedStructuralStringify,
                ));
            }
        };
        self.stringify_in_progress.remove(&rir_ty);
        let id = RirStringifyReqId::from_index(program.stringify_reqs.len());
        program.stringify_reqs.push(RirStringifyReq {
            id,
            ty: rir_ty,
            kind,
        });
        Ok(())
    }

    fn classify_aggregate_stringify(
        &mut self,
        program: &mut RirProgram,
        ty: TypeId,
        aggregate: air::AggregateId,
    ) -> Result<RirStringifyReqKind, RustTargetGap> {
        let decl = self.air.aggregate(aggregate);
        if let Some(function) = decl.stringify_override {
            let override_fn = self.air.function(function);
            let Some(receiver) = override_fn.signature.params.first() else {
                return Err(Self::gap(
                    RustTargetGapSite::Function(function),
                    RustTargetGapKind::UnsupportedStructuralStringify,
                ));
            };
            return match receiver.mode {
                ParamMode::SharedBorrow => Ok(RirStringifyReqKind::Override {
                    function: self.function_map[&function],
                    mode: RirPassMode::SharedBorrow,
                }),
                ParamMode::Value
                    if program.value_materializers[self.type_map[&ty].index()]
                        .and_then(|id| program.materializers.get(id.index()))
                        .is_some_and(rir::RirMaterializer::is_copy) =>
                {
                    Ok(RirStringifyReqKind::Override {
                        function: self.function_map[&function],
                        mode: RirPassMode::Value,
                    })
                }
                ParamMode::Value | ParamMode::MutBorrow => Err(Self::gap(
                    RustTargetGapSite::Function(function),
                    RustTargetGapKind::NonCopyValueRequired,
                )),
            };
        }
        let helper = self.require_structural_helper(program, ty, aggregate)?;
        Ok(RirStringifyReqKind::Helper(helper))
    }

    fn require_structural_helper(
        &mut self,
        program: &mut RirProgram,
        ty: TypeId,
        aggregate: air::AggregateId,
    ) -> Result<RirStringifyHelperId, RustTargetGap> {
        let rir_ty = self.type_map[&ty];
        let decl = self.air.aggregate(aggregate);
        if decl.kind != air::AggregateKind::Struct {
            return Err(Self::gap(
                RustTargetGapSite::Type(ty),
                RustTargetGapKind::UnsupportedStructuralStringify,
            ));
        }
        let fields = decl.fields.iter().map(|field| field.ty).collect::<Vec<_>>();
        for field in fields {
            self.require_stringify(program, field)?;
        }
        let RirType::Struct(strukt) = program.types[rir_ty.index()] else {
            return Err(Self::gap(
                RustTargetGapSite::Type(ty),
                RustTargetGapKind::UnsupportedStructuralStringify,
            ));
        };
        Ok(self.push_stringify_helper(program, rir_ty, RirStringifyHelperKind::Struct(strukt)))
    }

    fn require_enum_stringify_helper(
        &mut self,
        program: &mut RirProgram,
        ty: TypeId,
        enm: air::EnumId,
    ) -> Result<RirStringifyHelperId, RustTargetGap> {
        let rir_ty = self.type_map[&ty];
        let rir_enum = self.enum_map[&enm];
        let fields = self
            .air
            .enum_decl(enm)
            .variants
            .iter()
            .flat_map(|variant| match &variant.shape {
                air::VariantShape::Unit => vec![],
                air::VariantShape::Tuple(fields) => fields.clone(),
                air::VariantShape::Struct(fields) => fields.iter().map(|field| field.ty).collect(),
            })
            .collect::<Vec<_>>();
        for field in fields {
            self.require_stringify(program, field)?;
        }
        let display = program.enums[rir_enum.index()].display.as_str().to_string();
        let specs = program.enums[rir_enum.index()]
            .variants
            .iter()
            .zip(&self.air.enum_decl(enm).variants)
            .map(|(rir, air)| {
                let fields = match &air.shape {
                    air::VariantShape::Struct(fields) => fields
                        .iter()
                        .map(|field| format!("{}: ", field.name.as_str()))
                        .collect(),
                    air::VariantShape::Unit | air::VariantShape::Tuple(_) => vec![],
                };
                (format!("{display}.{}", rir.display.as_str()), fields)
            })
            .collect::<Vec<(String, Vec<String>)>>();
        let mut variants = vec![];
        for (label, fields) in specs {
            variants.push(RirEnumStringifyVariant {
                label: self.intern_string_literal(program, &label),
                field_labels: fields
                    .iter()
                    .map(|field| self.intern_string_literal(program, field))
                    .collect(),
            });
        }
        Ok(self.push_stringify_helper(
            program,
            rir_ty,
            RirStringifyHelperKind::Enum {
                enm: rir_enum,
                variants,
            },
        ))
    }

    fn require_flag_stringify_helper(
        &mut self,
        program: &mut RirProgram,
        ty: TypeId,
        flag: air::FlagId,
    ) -> RirStringifyHelperId {
        let rir_ty = self.type_map[&ty];
        let rir_flag = self.flag_map[&flag];
        let display = program.flags[rir_flag.index()].display.as_str().to_string();
        let empty = self.intern_string_literal(program, &format!("{display}.empty()"));
        let names = program.flags[rir_flag.index()]
            .members
            .iter()
            .map(|member| format!("{display}.{}", member.display.as_str()))
            .collect::<Vec<_>>();
        let members = names
            .iter()
            .map(|name| self.intern_string_literal(program, name))
            .collect();
        self.push_stringify_helper(
            program,
            rir_ty,
            RirStringifyHelperKind::Flag {
                flag: rir_flag,
                empty,
                members,
            },
        )
    }

    fn push_stringify_helper(
        &self,
        program: &mut RirProgram,
        ty: RirTypeId,
        kind: RirStringifyHelperKind,
    ) -> RirStringifyHelperId {
        let id = RirStringifyHelperId::from_index(program.stringify_helpers.len());
        program.stringify_helpers.push(RirStringifyHelper {
            id,
            ty,
            symbol: RirSymbol::new(format!(
                "{}stringify_{}",
                self.config.symbol_prefix,
                type_suffix(program, ty)
            )),
            kind,
        });
        id
    }

    fn intern_string_literal(
        &mut self,
        program: &mut RirProgram,
        text: &str,
    ) -> RirStringLiteralId {
        if let Some(id) = self.string_literal_map.get(text) {
            return *id;
        }
        let id = RirStringLiteralId::from_index(program.string_literals.len());
        program.string_literals.push(RirStringLiteral {
            id,
            text: text.to_string(),
            needs_owned: false,
        });
        self.string_literal_map.insert(text.to_string(), id);
        id
    }

    fn rir_raw_enum_value(
        &mut self,
        program: &mut RirProgram,
        value: &air::RawEnumValue,
    ) -> RirRawEnumValue {
        match value {
            air::RawEnumValue::Int(value) => RirRawEnumValue::Int(*value),
            air::RawEnumValue::String(value) => {
                RirRawEnumValue::String(self.intern_string_literal(program, value))
            }
        }
    }

    fn plan_consts(&mut self, program: &mut RirProgram) {
        for index in 0..self.air.const_arena.len() {
            let air_id = ConstId::from_index(index);
            let konst = self.air.const_arena.get(air_id);
            let id = RirConstId::from_index(program.consts.len());
            let value = match &konst.value {
                ConstValue::Int(value) => RirConstValue::Int(*value),
                ConstValue::Flag { flag, bits } => RirConstValue::Flag {
                    flag: self.flag_map[flag],
                    bits: *bits,
                },
                ConstValue::Float(value) => RirConstValue::Float(*value),
                ConstValue::Bool(value) => RirConstValue::Bool(*value),
                ConstValue::String(value) => {
                    RirConstValue::String(self.intern_string_literal(program, value))
                }
                ConstValue::Char(value) => RirConstValue::Char(*value),
                ConstValue::Nil => RirConstValue::Nil,
            };
            program.consts.push(RirConst {
                id,
                ty: self.type_map[&konst.ty],
                value,
            });
            self.const_map.insert(air_id, id);
        }
    }

    fn plan_externs(&mut self, program: &RirProgram) -> Result<(), Vec<RustTargetGap>> {
        let mut pending = Vec::with_capacity(self.air.externs.len());
        let mut retained_callbacks = false;
        let mut gaps = vec![];
        for index in 0..self.air.externs.len() {
            let air_id = ExternId::from_index(index);
            let decl = self.air.extern_decl(air_id);
            retained_callbacks |= decl.call_params().any(|param| {
                param.escape == ParamEscape::Escaping
                    && matches!(self.air.type_arena.data(param.ty), TypeData::Function(_))
            });
            pending.push((air_id, self.native_extern(air_id, decl)));
        }

        let mut externs = Vec::with_capacity(pending.len());
        let mut extern_map = HashMap::with_capacity(pending.len());
        for (air_id, native) in pending {
            let (binding, callback_receiver) = match native {
                Ok(native) => native,
                Err(gap) => {
                    gaps.push(gap);
                    continue;
                }
            };
            let decl = self.air.extern_decl(air_id);
            let params = self.extern_params(decl, &binding.abi.params, retained_callbacks);
            if self.unsupported_lambda_extern_boundary(decl, &params) {
                gaps.push(Self::gap(
                    RustTargetGapSite::Extern(self.extern_gap_site(air_id, decl)),
                    RustTargetGapKind::UnsupportedLambdaExternBoundary,
                ));
                continue;
            }
            let ret = self.type_map[&decl.return_type];
            let Some(ret_plan) = native_call::classify_return(program, ret, &binding.abi.ret)
            else {
                gaps.push(Self::gap(
                    RustTargetGapSite::Extern(self.extern_gap_site(air_id, decl)),
                    RustTargetGapKind::UnsupportedRustAbi,
                ));
                continue;
            };
            let id = RirExternId::from_index(externs.len());
            externs.push(RirExtern {
                id,
                path: binding.path.clone(),
                params,
                ret,
                ret_plan,
                callback_receiver,
                ctx: binding.abi.ctx,
                fallible: binding.abi.fallible,
                suspends_runtime_entry: retained_callbacks,
            });
            extern_map.insert(air_id, id);
        }
        if !gaps.is_empty() {
            return Err(gaps);
        }
        self.externs = externs;
        self.extern_map = extern_map;
        Ok(())
    }

    fn extern_params(
        &self,
        decl: &air::ExternDecl,
        abi: &[anvyx_runtime::RustParamAbi],
        suspends_runtime_entry: bool,
    ) -> Vec<RirExternParam> {
        decl.call_params()
            .zip(abi)
            .map(|(param, abi)| {
                let (mode, escape) = native_call::classify_param(abi);
                let air_ty = self.air.type_arena.data(param.ty);
                let native_ref = matches!(air_ty, TypeData::Extern(ext)
                    if self.air.extern_type(*ext).rep == air::ExternRep::Shared);
                RirExternParam {
                    ty: self.type_map[&param.ty],
                    mode,
                    escape,
                    abi: abi.clone(),
                    action: native_call::classify_arg_action(
                        abi,
                        mode,
                        native_ref,
                        suspends_runtime_entry,
                    ),
                }
            })
            .collect()
    }

    fn extern_gap_site(&self, id: ExternId, decl: &air::ExternDecl) -> RustExternGapSite {
        let member_name = |owner| format!("{}.{}", self.air.extern_type(owner).name, decl.name);
        let (subject, name) = match &decl.member {
            air::ExternMember::FreeFunction => (RustExternSubject::Function, decl.name.to_string()),
            air::ExternMember::FieldGetter { owner, .. } => {
                (RustExternSubject::FieldGetter, member_name(*owner))
            }
            air::ExternMember::FieldSetter { owner, .. } => {
                (RustExternSubject::FieldSetter, member_name(*owner))
            }
            air::ExternMember::Method { owner, .. } => {
                (RustExternSubject::Method, member_name(*owner))
            }
            air::ExternMember::StaticMethod { owner } => {
                (RustExternSubject::StaticMethod, member_name(*owner))
            }
            air::ExternMember::Init { owner } => (
                RustExternSubject::Initializer,
                format!("{}.init", self.air.extern_type(*owner).name),
            ),
            air::ExternMember::UnaryOperator { owner, op, .. } => (
                RustExternSubject::UnaryOperator,
                format!("{}.{op}", self.air.extern_type(*owner).name),
            ),
            air::ExternMember::BinaryOperator {
                owner,
                op,
                self_on_right,
                ..
            } => {
                let side = if *self_on_right { "right" } else { "left" };
                (
                    RustExternSubject::BinaryOperator {
                        self_on_right: *self_on_right,
                    },
                    format!(
                        "{}.{op} (receiver on {side})",
                        self.air.extern_type(*owner).name
                    ),
                )
            }
        };
        RustExternGapSite {
            id,
            name,
            subject,
            span: decl.span,
        }
    }

    fn native_type_binding(
        &self,
        type_id: TypeId,
        decl: &air::ExternTypeDecl,
    ) -> Result<&anvyx_runtime::RustTypeBinding, RustTargetGap> {
        let Some(binding) = &decl.binding else {
            return Err(Self::gap(
                RustTargetGapSite::Type(type_id),
                RustTargetGapKind::UnsupportedExternType,
            ));
        };
        self.materializers.native_type(binding).ok_or_else(|| {
            Self::gap(
                RustTargetGapSite::Type(type_id),
                RustTargetGapKind::UnsupportedExternType,
            )
        })
    }

    fn native_extern(
        &self,
        id: ExternId,
        decl: &air::ExternDecl,
    ) -> Result<(&anvyx_runtime::RustExternBinding, Option<usize>), RustTargetGap> {
        native::resolve_extern(&self.config.native_providers, self.air, decl).map_err(|error| {
            let kind = match error {
                native::ResolveExternError::MissingBinding => {
                    RustTargetGapKind::MissingExternBinding
                }
                native::ResolveExternError::MissingConfiguredSupport => {
                    RustTargetGapKind::MissingExternSupport
                }
                native::ResolveExternError::MissingExport => RustTargetGapKind::MissingExternExport,
                native::ResolveExternError::UnsupportedAbi => RustTargetGapKind::UnsupportedRustAbi,
            };
            Self::gap(
                RustTargetGapSite::Extern(self.extern_gap_site(id, decl)),
                kind,
            )
        })
    }

    fn unsupported_lambda_extern_boundary(
        &self,
        decl: &air::ExternDecl,
        params: &[RirExternParam],
    ) -> bool {
        let policy = self.air_policy();
        policy.contains_function_payload(decl.return_type)
            || decl.call_params().enumerate().any(|(index, param)| {
                policy.contains_function_payload(param.ty)
                    && !(matches!(self.air.type_arena.data(param.ty), TypeData::Function(_))
                        && params.get(index).is_some_and(|native_param| {
                            matches!(
                                (param.escape, native_param.mode),
                                (ParamEscape::NonEscaping, RirPassMode::ScopedLambda)
                                    | (
                                        ParamEscape::Escaping,
                                        RirPassMode::EscapingLambda | RirPassMode::AnvCallback
                                    )
                            )
                        }))
            })
    }

    fn plan_function_ids(&mut self) {
        for index in 0..self.air.functions.len() {
            let air_id = FunctionId::from_index(index);
            self.function_map
                .insert(air_id, RirFunctionId::from_index(index));
        }
    }

    fn plan_globals(&mut self, program: &mut RirProgram) -> Result<(), RustTargetGap> {
        for index in 0..self.air.globals.len() {
            let air_id = GlobalId::from_index(index);
            let decl = &self.air.globals[index];
            let id = RirGlobalId::from_index(program.globals.len());
            let Some(&ty) = self.type_map.get(&decl.ty) else {
                return Err(Self::gap(
                    RustTargetGapSite::Global(air_id),
                    RustTargetGapKind::UnsupportedGlobalType,
                ));
            };
            let Some(&init) = self.function_map.get(&decl.init) else {
                return Err(Self::gap(
                    RustTargetGapSite::Global(air_id),
                    RustTargetGapKind::UnsupportedGlobalInitializer,
                ));
            };
            self.global_map.insert(air_id, id);
            program.globals.push(RirGlobal {
                id,
                name: global_display(self.air, decl),
                slot_symbol: global_slot_symbol(id, decl),
                ty,
                mutable: decl.mutability == Mutability::Mutable,
                init,
            });
        }
        Ok(())
    }

    fn plan_cells(&mut self, program: &mut RirProgram) {
        for (index, cell) in self.air.capture_cells.iter().enumerate() {
            let air_id = air::CaptureCellId::from_index(index);
            let id = RirCellId::from_index(program.cells.len());
            self.capture_cell_map.insert(air_id, id);
            program.cells.push(RirCellDecl {
                id,
                owner: self.function_map[&cell.owner],
                source_local: RirLocalId::from_index(cell.source_local.index()),
                payload_ty: self.type_map[&cell.ty],
                storage: self.classify_capture_cell_storage(air_id),
                lifetime: rir_cell_lifetime(cell.lifetime),
                symbol: RirSymbol::new(format!("__cell{}", id.index())),
            });
        }
    }

    fn classify_capture_cell_storage(&self, cell: air::CaptureCellId) -> RirCellStorage {
        if self.air.lambdas.iter().any(|lambda| {
            lambda.escape == air::LambdaEscape::Escaping
                && lambda.captures.iter().any(|capture| {
                    matches!(capture, air::LambdaCaptureDecl::CaptureCell { cell: found, .. } if *found == cell)
                })
        }) {
            RirCellStorage::Heap
        } else {
            RirCellStorage::StackScoped
        }
    }

    fn plan_scoped_place_cells(&mut self, program: &mut RirProgram) -> Result<(), RustTargetGap> {
        for (index, borrow) in self.air.scoped_borrows.iter().enumerate() {
            let air_id = air::ScopedBorrowId::from_index(index);
            let id = RirScopedPlaceCellId::from_index(program.scoped_place_cells.len());
            let source = self.plan_scoped_place_source(borrow)?;
            self.scoped_place_cell_map.insert(air_id, id);
            program.scoped_place_cells.push(RirScopedPlaceCellDecl {
                id,
                owner: self.function_map[&borrow.owner],
                source,
                payload_ty: self.type_map[&borrow.ty],
                symbol: RirSymbol::new(format!("__scoped{}", id.index())),
            });
        }
        Ok(())
    }

    fn plan_scoped_place_source(
        &mut self,
        borrow: &air::ScopedBorrowDecl,
    ) -> Result<RirScopedPlaceSource, RustTargetGap> {
        match &borrow.source {
            air::ScopedBorrowSource::SourceMutParam { local } => {
                Ok(RirScopedPlaceSource::SourceMutParam {
                    place: RirMutPlaceArg::from_handle(
                        RirMutPlaceHandle::Param {
                            local: RirLocalId::from_index(local.index()),
                        },
                        vec![],
                    ),
                })
            }
            air::ScopedBorrowSource::RefSelf { local } => Ok(RirScopedPlaceSource::RefSelf {
                place: RirMutPlaceArg::from_handle(
                    RirMutPlaceHandle::Param {
                        local: RirLocalId::from_index(local.index()),
                    },
                    vec![],
                ),
            }),
            air::ScopedBorrowSource::PatternAlias { source } => self
                .plan_alias_scoped_place_source(borrow.owner, source)
                .map(|place| RirScopedPlaceSource::PatternAlias { place }),
            air::ScopedBorrowSource::ForRefAlias { source } => self
                .plan_alias_scoped_place_source(borrow.owner, source)
                .map(|place| RirScopedPlaceSource::ForRefAlias { place }),
        }
    }

    fn plan_alias_scoped_place_source(
        &mut self,
        owner: FunctionId,
        source: &Place,
    ) -> Result<RirMutPlaceArg, RustTargetGap> {
        let plan = self
            .access()
            .plan(owner, PlaceAccessIntent::MutPlaceArg, source)
            .map_err(|gap| Self::access_gap(owner, gap))?;
        let mut locals = vec![];
        let planned = self.plan_mut_place_arg(owner, &plan, &mut locals)?;
        if !planned.stmts.is_empty() || !locals.is_empty() {
            return Err(Self::gap(
                RustTargetGapSite::Function(owner),
                RustTargetGapKind::UnsupportedMutablePlaceProjection,
            ));
        }
        Ok(planned.arg)
    }

    fn plan_lambdas(&mut self, program: &mut RirProgram) -> Result<(), RustTargetGap> {
        let mut function_refs = Vec::new();
        for function in &self.air.functions {
            function.body.for_each_rvalue(&mut |value| {
                if let RValue::FunctionRef { function, .. } = value
                    && !function_refs.contains(function)
                {
                    function_refs.push(*function);
                }
            });
        }
        for air_id in function_refs {
            let function = self.air.function(air_id);
            if matches!(function.kind, air::FunctionKind::Lambda(_)) {
                continue;
            }
            let sig = self.function_lambda_sig(program, function);
            let id = self.push_zero_env_lambda(
                program,
                PlannerLambdaSource::Function(air_id),
                self.function_map[&air_id],
                sig,
                RirLambdaEscape::Escaping,
            );
            self.function_lambda_map.insert(air_id, id);
        }
        for (index, decl) in self.air.lambdas.iter().enumerate() {
            let lambda = air::LambdaId::from_index(index);
            let planned = self.plan_lambda_captures(program, lambda, decl)?;
            let sig = self.intern_lambda_sig(program, &decl.signature);
            let id = self.push_lambda(
                program,
                PlannerLambdaSource::Lambda(lambda),
                self.function_map[&decl.body],
                sig,
                match decl.escape {
                    air::LambdaEscape::NonEscaping => RirLambdaEscape::NonEscaping,
                    air::LambdaEscape::Escaping => RirLambdaEscape::Escaping,
                },
                planned.captures,
                planned.env_fields,
            );
            self.lambda_map.insert(lambda, id);
        }
        Ok(())
    }

    fn check_lambda_env_storage_support(&self, program: &RirProgram) -> Result<(), RustTargetGap> {
        let policy = RirRustRepPolicy::new(program);
        for env in &program.lambda_envs {
            for field in &env.fields {
                if !policy.lambda_env_field_storage_supported(field) {
                    let lambda = &program.lambdas[env.lambda.index()];
                    return Err(Self::gap(
                        self.lambda_gap_site(lambda),
                        RustTargetGapKind::UnsupportedLambdaCapture,
                    ));
                }
            }
        }
        Ok(())
    }

    fn lambda_gap_site(&self, lambda: &RirLambda) -> RustTargetGapSite {
        match self.lambda_sources[lambda.id.index()] {
            PlannerLambdaSource::Lambda(id) => {
                RustTargetGapSite::Function(self.air.lambdas[id.index()].owner)
            }
            PlannerLambdaSource::Function(id) => RustTargetGapSite::Function(id),
        }
    }

    fn check_lambda_value_capture_cycles(&self, program: &RirProgram) -> Result<(), RustTargetGap> {
        let policy = RirRustRepPolicy::new(program);
        for lambda in &program.lambdas {
            if policy.lambda_has_recursive_inline_value_capture(lambda) {
                return Err(Self::gap(
                    self.lambda_gap_site(lambda),
                    RustTargetGapKind::UnsupportedLambdaCapture,
                ));
            }
        }
        Ok(())
    }

    fn plan_function_type_capture_policy(&mut self, program: &mut RirProgram) {
        let mut heap_env_sigs = HashSet::new();
        for decl in &self.air.lambdas {
            if decl.escape != air::LambdaEscape::Escaping
                || decl
                    .captures
                    .iter()
                    .all(|capture| matches!(capture, air::LambdaCaptureDecl::NoRuntime { .. }))
            {
                continue;
            }
            heap_env_sigs.insert(self.intern_lambda_sig(program, &decl.signature));
        }
        self.function_type_copyable.clear();
        self.function_type_shareable.clear();
        for (ty, sig) in &self.lambda_sig_map {
            let has_heap_env = heap_env_sigs.contains(sig);
            self.function_type_copyable.insert(*ty, !has_heap_env);
            self.function_type_shareable.insert(*ty, true);
        }
    }

    fn finalize_callable_materializers(&mut self) {
        let callables = self
            .function_type_shareable
            .iter()
            .filter_map(|(&ty, &shareable)| {
                if self.function_type_copyable[&ty] {
                    Some((ty, rir::RirMaterializerAction::Copy))
                } else if shareable {
                    Some((ty, rir::RirMaterializerAction::CallableShare))
                } else {
                    None
                }
            });
        self.materializers.set_callable_materializers(callables);
    }

    fn plan_materializers(&mut self, program: &mut RirProgram) -> Result<(), RustTargetGap> {
        use LambdaStorageFamily::{
            DataRefProjection, EnumPayload, FixedArrayElement, ListElement, MapValue,
            OptionalPayload, StructField, TupleField, UnknownOrigin,
        };
        use RustRecipePosition::{Global, MapKey, StoredPayload, Value};

        const STORED: [LambdaStorageFamily; 9] = [
            UnknownOrigin,
            StructField,
            DataRefProjection,
            EnumPayload,
            FixedArrayElement,
            ListElement,
            TupleField,
            OptionalPayload,
            MapValue,
        ];

        program
            .value_materializers
            .resize(program.types.len(), None);
        let plan = RustRepresentationPlan::new(self.air, &self.classes);
        for index in 0..self.air.type_arena.len() {
            let ty = TypeId::from_index(index);
            let value_required = matches!(self.air.type_arena.data(ty), TypeData::Extern(_));
            let positions = [Value, Global, MapKey]
                .into_iter()
                .chain(STORED.map(StoredPayload));
            for position in positions {
                let id = match self.materializers.reserve(
                    plan,
                    ty,
                    position,
                    &self.dyn_surface_map,
                    &self.dyn_variant_map,
                ) {
                    Ok(id) => id,
                    Err(_) if position == Value && value_required => {
                        return Err(Self::gap(
                            RustTargetGapSite::Type(ty),
                            RustTargetGapKind::UnsupportedType,
                        ));
                    }
                    Err(_) => continue,
                };
                if position == Value {
                    program.value_materializers[index] = Some(id);
                }
            }
        }
        self.materializers
            .fill(plan, &self.dyn_surface_map, &self.dyn_variant_map)
            .map_err(|_| Self::gap(RustTargetGapSite::Entry, RustTargetGapKind::UnsupportedType))
    }

    fn plan_function_type_copyability(&mut self, program: &RirProgram) {
        let policy = RirRustRepPolicy::new(program);
        let source_copyable = |ty| self.rust_copyable_rir_type(ty);
        let mut copyable = HashMap::new();
        let mut shareable = HashMap::new();
        for (ty, sig) in &self.lambda_sig_map {
            let is_copyable = policy.lambda_sig_copyable_with(*sig, source_copyable);
            copyable.insert(*ty, is_copyable);
            shareable.insert(
                *ty,
                is_copyable || policy.lambda_sig_cloneable_with(*sig, source_copyable),
            );
        }
        self.function_type_copyable = copyable;
        self.function_type_shareable = shareable;
    }

    fn plan_lambda_captures(
        &mut self,
        program: &RirProgram,
        lambda: air::LambdaId,
        decl: &air::LambdaDecl,
    ) -> Result<PlannedLambdaCaptures, RustTargetGap> {
        let policy = RirRustRepPolicy::new(program);
        let mut captures = vec![];
        let mut env_fields = vec![];
        for (slot, capture) in decl.captures.iter().enumerate() {
            let source_slot = air::LambdaCaptureSlotId::from_index(slot);
            let Some(capture) =
                self.plan_lambda_capture(program, decl.owner, decl.escape, capture)?
            else {
                continue;
            };
            let capture_index = captures.len();
            self.lambda_runtime_capture_slots
                .insert((lambda, source_slot), capture_index);
            self.lambda_capture_modes
                .insert((lambda, source_slot), capture.mode);
            if decl.escape == air::LambdaEscape::Escaping {
                env_fields.push(RirLambdaEnvField {
                    ty: capture.ty,
                    symbol: RirSymbol::new(format!("c{capture_index}")),
                    kind: match capture.kind {
                        RirLambdaCaptureKind::HeapCell { cell } => {
                            RirLambdaEnvFieldKind::HeapCell { cell }
                        }
                        _ => RirLambdaEnvFieldKind::Value,
                    },
                });
            }
            captures.push(capture);
        }
        for capture in &captures {
            if !policy.supports_param(capture.ty, capture.mode) {
                return Err(Self::gap(
                    RustTargetGapSite::Function(decl.owner),
                    RustTargetGapKind::UnsupportedLambdaCapture,
                ));
            }
        }
        Ok(PlannedLambdaCaptures {
            captures,
            env_fields,
        })
    }

    fn plan_lambda_capture(
        &self,
        program: &RirProgram,
        owner: FunctionId,
        escape: air::LambdaEscape,
        capture: &air::LambdaCaptureDecl,
    ) -> Result<Option<RirLambdaCapture>, RustTargetGap> {
        let policy = RirRustRepPolicy::new(program);
        match capture {
            air::LambdaCaptureDecl::NoRuntime { .. } => Ok(None),
            air::LambdaCaptureDecl::ReadonlyLocal { ty, .. } => {
                let source_ty = *ty;
                let function_ty = matches!(self.air.type_data(source_ty), TypeData::Function(_));
                let copyable = self.rust_copyable_air_type(source_ty);
                let ty = self.type_map[ty];
                if !function_ty
                    && !copyable
                    && !policy.capture_shareable_with(ty, |ty| self.rust_copyable_rir_type(ty))
                {
                    return Err(Self::gap(
                        RustTargetGapSite::Function(owner),
                        RustTargetGapKind::UnsupportedLambdaCapture,
                    ));
                }
                let by_value = escape == air::LambdaEscape::Escaping || (!function_ty && copyable);
                let mode = if by_value {
                    RirPassMode::Value
                } else {
                    RirPassMode::SharedBorrow
                };
                Ok(Some(RirLambdaCapture {
                    ty,
                    mode,
                    kind: RirLambdaCaptureKind::Param,
                }))
            }
            air::LambdaCaptureDecl::ScopedLocal { .. } => Err(Self::gap(
                RustTargetGapSite::Function(owner),
                RustTargetGapKind::UnsupportedLambdaCapture,
            )),
            air::LambdaCaptureDecl::ScopedBorrow { .. }
                if escape == air::LambdaEscape::Escaping =>
            {
                Err(Self::gap(
                    RustTargetGapSite::Function(owner),
                    RustTargetGapKind::UnsupportedLambdaCapture,
                ))
            }
            air::LambdaCaptureDecl::ScopedBorrow { borrow, ty, .. } => {
                let ty = self.type_map[ty];
                Ok(Some(RirLambdaCapture {
                    ty,
                    mode: RirPassMode::ScopedPlaceCell,
                    kind: RirLambdaCaptureKind::ScopedPlaceCell {
                        cell: self.scoped_place_cell_map[borrow],
                    },
                }))
            }
            air::LambdaCaptureDecl::CaptureCell { cell, ty, .. } => {
                let ty = self.type_map[ty];
                let cell = self.capture_cell_map[cell];
                let storage = program.cells[cell.index()].storage;
                let (mode, kind) = match storage {
                    RirCellStorage::StackScoped if escape == air::LambdaEscape::NonEscaping => (
                        RirPassMode::StackCell,
                        RirLambdaCaptureKind::StackCell { cell },
                    ),
                    RirCellStorage::Heap => (
                        RirPassMode::HeapCell,
                        RirLambdaCaptureKind::HeapCell { cell },
                    ),
                    RirCellStorage::StackScoped => {
                        return Err(Self::gap(
                            RustTargetGapSite::Function(owner),
                            RustTargetGapKind::UnsupportedLambdaCapture,
                        ));
                    }
                };
                Ok(Some(RirLambdaCapture { ty, mode, kind }))
            }
        }
    }

    fn push_zero_env_lambda(
        &mut self,
        program: &mut RirProgram,
        source: PlannerLambdaSource,
        function: RirFunctionId,
        sig: RirLambdaSigId,
        escape: RirLambdaEscape,
    ) -> RirLambdaId {
        self.push_lambda(program, source, function, sig, escape, vec![], vec![])
    }

    fn push_lambda(
        &mut self,
        program: &mut RirProgram,
        source: PlannerLambdaSource,
        function: RirFunctionId,
        sig: RirLambdaSigId,
        escape: RirLambdaEscape,
        captures: Vec<RirLambdaCapture>,
        env_fields: Vec<RirLambdaEnvField>,
    ) -> RirLambdaId {
        let id = RirLambdaId::from_index(program.lambdas.len());
        let storage = if captures.is_empty() {
            RirLambdaStorage::ZeroEnv
        } else if escape == RirLambdaEscape::Escaping {
            let env = RirLambdaEnvId::from_index(program.lambda_envs.len());
            program.lambda_envs.push(RirLambdaEnvLayout {
                id: env,
                lambda: id,
                symbol: RirSymbol::new(format!("LambdaEnv{}", env.index())),
                fields: env_fields,
            });
            RirLambdaStorage::HeapEnv { env }
        } else {
            RirLambdaStorage::ScopedCaptures
        };
        self.lambda_sources.push(source);
        program.lambdas.push(RirLambda {
            id,
            function,
            sig,
            escape,
            storage,
            captures,
        });
        id
    }

    fn function_lambda_sig(
        &self,
        program: &mut RirProgram,
        function: &air::Function,
    ) -> RirLambdaSigId {
        let sig = air::SignatureType::new(
            function
                .signature
                .params
                .iter()
                .map(air::Param::param_type)
                .collect(),
            function.signature.return_mode,
        );
        self.intern_lambda_sig(program, &sig)
    }

    fn plan_function(
        &mut self,
        air_id: FunctionId,
        program: &RirProgram,
    ) -> Result<RirFunction, RustTargetGap> {
        let function = self.air.function(air_id);
        let policy = RirRustRepPolicy::new(program);
        let place_return = matches!(function.signature.return_mode, ReturnMode::Place(_));
        let slice_return = matches!(
            self.air.type_arena.data(function.signature.return_type()),
            TypeData::Slice(_)
        );
        if place_return || slice_return {
            return Err(Self::gap(
                RustTargetGapSite::Function(air_id),
                RustTargetGapKind::UnsupportedReturnMode,
            ));
        }
        for param in &function.signature.params {
            let mode = rir::source_pass_mode(self.air, param.ty, param.mode);
            if !policy.supports_param(self.type_map[&param.ty], mode) {
                return Err(Self::gap(
                    RustTargetGapSite::Function(air_id),
                    RustTargetGapKind::UnsupportedParamMode,
                ));
            }
        }
        let mut locals = function
            .locals
            .iter()
            .enumerate()
            .map(|(index, local)| RirLocal {
                id: RirLocalId::from_index(index),
                ty: self.type_map[&local.ty],
                mutable: local.mutability == Mutability::Mutable,
                symbol: local_symbol(index, local.name.as_ref()),
                binding: RirLocalBinding::Value,
            })
            .collect::<Vec<_>>();
        for param in &function.signature.params {
            let local = &mut locals[param.local_id.index()];
            local.binding = RirLocalBinding::Parameter {
                mode: rir::source_pass_mode(self.air, param.ty, param.mode),
                escape: rir_param_escape(param.escape),
            };
        }
        let mut params = vec![];
        if let air::FunctionKind::Lambda(lambda) = function.kind {
            for (index, capture) in program.lambdas[self.lambda_map[&lambda].index()]
                .captures
                .iter()
                .enumerate()
            {
                let local = RirLocalId::from_index(locals.len());
                locals.push(RirLocal {
                    id: local,
                    ty: capture.ty,
                    mutable: capture.mode == RirPassMode::MutBorrow,
                    symbol: local_symbol(local.index(), None),
                    binding: RirLocalBinding::Parameter {
                        mode: capture.mode,
                        escape: RirParamEscape::NonEscaping,
                    },
                });
                params.push(local);
                debug_assert_eq!(local.index(), function.locals.len() + index);
            }
        }
        params.extend(
            function
                .signature
                .params
                .iter()
                .map(|param| RirLocalId::from_index(param.local_id.index())),
        );
        let mut zero_env_function_values = vec![None; locals.len()];
        let mut initialized_cells = vec![false; self.air.capture_cells.len()];
        let mut possible_cells = vec![false; self.air.capture_cells.len()];
        if let air::FunctionKind::Lambda(lambda) = function.kind {
            for capture in &self.air.lambdas[lambda.index()].captures {
                if let air::LambdaCaptureDecl::CaptureCell { cell, .. } = capture {
                    initialized_cells[cell.index()] = true;
                    possible_cells[cell.index()] = true;
                }
            }
        }
        let body = self.plan_air_block(
            air_id,
            &function.body.block,
            &mut locals,
            &mut zero_env_function_values,
            &mut initialized_cells,
            &mut possible_cells,
            false,
        )?;
        Ok(RirFunction {
            id: self.function_map[&air_id],
            symbol: function_symbol(
                &self.config.symbol_prefix,
                air_id,
                function,
                self.air,
                program,
                &self.type_map,
            )?,
            params,
            ret: RirReturn {
                ty: self.type_map[&function.signature.return_type()],
            },
            locals,
            body,
        })
    }

    fn bind_local(locals: &mut [RirLocal], local: RirLocalId, binding: RirLocalBinding) {
        debug_assert!(matches!(
            binding,
            RirLocalBinding::DirectPayload | RirLocalBinding::ScopedPlacePayload
        ));
        let local = &mut locals[local.index()];
        debug_assert!(matches!(local.binding, RirLocalBinding::Value));
        local.binding = binding;
    }

    fn plan_air_block(
        &mut self,
        function: FunctionId,
        block: &air::AirBlock,
        locals: &mut Vec<RirLocal>,
        zero_env_function_values: &mut Vec<Option<ZeroEnvFunctionValue>>,
        initialized_cells: &mut [bool],
        possible_cells: &mut [bool],
        in_loop: bool,
    ) -> Result<RirStructuredBlock, RustTargetGap> {
        let mut stmts = vec![];
        for (index, stmt) in block.stmts.iter().enumerate() {
            stmts.extend(self.plan_air_stmt(
                function,
                index,
                stmt,
                locals,
                zero_env_function_values,
                initialized_cells,
                possible_cells,
                in_loop,
            )?);
        }
        let (tail_stmts, term) = self.plan_air_tail(function, &block.tail, locals)?;
        stmts.extend(tail_stmts);
        Ok(RirStructuredBlock { stmts, term })
    }

    fn plan_global_root_value(
        &mut self,
        function: FunctionId,
        global: GlobalId,
        value: &RValue,
        locals: &mut Vec<RirLocal>,
        zero_env_function_values: &[Option<ZeroEnvFunctionValue>],
    ) -> Result<(Vec<RirStmt>, RirRValue), RustTargetGap> {
        let planned = self.plan_rvalue(function, value, locals, zero_env_function_values)?;
        let mut stmts = planned.stmts;
        let value = self.rvalue_short_region_value(
            function,
            planned.value,
            self.air.globals[global.index()].ty,
            locals,
            &mut stmts,
        );
        Ok((stmts, value))
    }

    fn plan_ordinal_plan(
        &mut self,
        function: FunctionId,
        plan: &air::AirOrdinalPlan,
        locals: &mut Vec<RirLocal>,
    ) -> Result<(Vec<RirStmt>, RirOrdinalPlan), RustTargetGap> {
        let mut stmts = vec![];
        let mut adapters = vec![];
        for adapter in &plan.adapters {
            match adapter {
                air::AirOrdinalAdapter::Rev => adapters.push(RirOrdinalAdapter::Rev),
                air::AirOrdinalAdapter::Skip { count: value } => {
                    let planned = self.plan_operand_read(function, value, locals)?;
                    stmts.extend(planned.stmts);
                    adapters.push(RirOrdinalAdapter::Skip {
                        count: planned.operand,
                    });
                }
                air::AirOrdinalAdapter::Take { count: value } => {
                    let planned = self.plan_operand_read(function, value, locals)?;
                    stmts.extend(planned.stmts);
                    adapters.push(RirOrdinalAdapter::Take {
                        count: planned.operand,
                    });
                }
                air::AirOrdinalAdapter::StepBy { step: value } => {
                    let planned = self.plan_operand_read(function, value, locals)?;
                    stmts.extend(planned.stmts);
                    adapters.push(RirOrdinalAdapter::StepBy {
                        step: planned.operand,
                    });
                }
            }
        }
        Ok((stmts, RirOrdinalPlan { adapters }))
    }

    fn plan_pattern_match(
        &mut self,
        function: FunctionId,
        match_: &air::AirPatternMatch,
        locals: &mut Vec<RirLocal>,
        zero_env_function_values: &mut Vec<Option<ZeroEnvFunctionValue>>,
        initialized_cells: &mut [bool],
        possible_cells: &mut [bool],
        in_loop: bool,
    ) -> Result<Vec<RirStmt>, RustTargetGap> {
        let aliases = Self::pattern_alias_locals(match_);
        let (mut stmts, subject_place) = if aliases.is_empty() {
            let subject = self.lower_place_read(function, &match_.subject, locals)?;
            let RirOperand::Place(subject_place) = subject.operand else {
                unreachable!("place read returns a place operand")
            };
            (subject.stmts, subject_place)
        } else {
            (
                vec![],
                self.plan_place(
                    function,
                    &self
                        .access()
                        .plan(function, PlaceAccessIntent::PayloadAlias, &match_.subject)
                        .map_err(|gap| Self::access_gap(function, gap))?,
                ),
            )
        };
        for local in aliases {
            Self::bind_local(locals, local, RirLocalBinding::DirectPayload);
        }
        let entry_functions = zero_env_function_values.clone();
        let entry_cells = initialized_cells.to_vec();
        let entry_possible = possible_cells.to_vec();
        let mut states = vec![];
        let mut cell_states = vec![];
        let mut possible_states = vec![];
        let arms = match_
            .arms
            .iter()
            .map(|arm| {
                let mut arm_functions = entry_functions.clone();
                let mut arm_cells = entry_cells.clone();
                let mut arm_possible = entry_possible.clone();
                let block = self.plan_air_block(
                    function,
                    &arm.block,
                    locals,
                    &mut arm_functions,
                    &mut arm_cells,
                    &mut arm_possible,
                    in_loop,
                )?;
                states.push(arm_functions);
                cell_states.push(arm_cells);
                possible_states.push(arm_possible);
                Ok(RirPatternArm {
                    alternatives: arm
                        .alternatives
                        .iter()
                        .map(|alternative| {
                            self.plan_pattern_alternative(function, alternative, match_.subject.ty)
                        })
                        .collect::<Result<Vec<_>, RustTargetGap>>()?,
                    block,
                })
            })
            .collect::<Result<Vec<_>, RustTargetGap>>()?;
        if !states.is_empty() {
            Self::merge_zero_env_function_values(
                zero_env_function_values,
                locals.len(),
                states.iter(),
            );
            for index in 0..initialized_cells.len() {
                initialized_cells[index] = cell_states.iter().all(|state| state[index]);
                possible_cells[index] = possible_states.iter().any(|state| state[index]);
            }
        }
        stmts.push(RirStmt::PatternMatch(RirPatternMatch {
            subject: subject_place,
            arms,
        }));
        Ok(stmts)
    }

    fn pattern_alias_locals(match_: &air::AirPatternMatch) -> Vec<RirLocalId> {
        let mut locals = vec![];
        for arm in &match_.arms {
            for alternative in &arm.alternatives {
                for binding in &alternative.bindings {
                    if binding.mode == air::AirPatternBindingMode::Alias {
                        let local = RirLocalId::from_index(binding.local.index());
                        if !locals.contains(&local) {
                            locals.push(local);
                        }
                    }
                }
            }
        }
        locals
    }

    fn plan_pattern_alternative(
        &mut self,
        function: FunctionId,
        alternative: &air::AirPatternAlternative,
        subject_ty: TypeId,
    ) -> Result<RirPatternAlternative, RustTargetGap> {
        Ok(RirPatternAlternative {
            tests: alternative
                .tests
                .iter()
                .map(|test| self.plan_pattern_test(test, subject_ty))
                .collect(),
            bindings: alternative
                .bindings
                .iter()
                .map(|binding| self.plan_pattern_binding(function, binding, subject_ty))
                .collect::<Result<Vec<_>, RustTargetGap>>()?,
        })
    }

    fn plan_pattern_test(&self, test: &air::AirPatternTest, subject_ty: TypeId) -> RirPatternTest {
        match test {
            air::AirPatternTest::Any { branches } => RirPatternTest::Any {
                branches: branches
                    .iter()
                    .map(|tests| {
                        tests
                            .iter()
                            .map(|test| self.plan_pattern_test(test, subject_ty))
                            .collect()
                    })
                    .collect(),
            },
            air::AirPatternTest::Literal { path, value } => RirPatternTest::Literal {
                path: self.plan_pattern_path(path, subject_ty),
                value: self.const_map[value],
            },
            air::AirPatternTest::Nil { path } => RirPatternTest::Nil {
                path: self.plan_pattern_path(path, subject_ty),
            },
            air::AirPatternTest::OptionalSome { path } => RirPatternTest::OptionalSome {
                path: self.plan_pattern_path(path, subject_ty),
            },
            air::AirPatternTest::FlagValue { path, flag, bits } => RirPatternTest::FlagValue {
                path: self.plan_pattern_path(path, subject_ty),
                flag: self.flag_map[flag],
                bits: *bits,
            },
            air::AirPatternTest::EnumVariant {
                path,
                enum_id,
                variant,
            } => RirPatternTest::EnumVariant {
                path: self.plan_pattern_path(path, subject_ty),
                enum_id: self.enum_map[enum_id],
                variant: RirVariantId::from_index(variant.index()),
            },
        }
    }

    fn plan_pattern_binding(
        &mut self,
        function: FunctionId,
        binding: &air::AirPatternBinding,
        subject_ty: TypeId,
    ) -> Result<RirPatternBinding, RustTargetGap> {
        let mode = match binding.mode {
            air::AirPatternBindingMode::Owned => RirPatternBindingMode::Owned {
                materializer: self.reusable_materializer(
                    function,
                    binding.ty,
                    RustRecipePosition::Value,
                )?,
            },
            air::AirPatternBindingMode::Alias => RirPatternBindingMode::Alias,
        };
        Ok(RirPatternBinding {
            local: RirLocalId::from_index(binding.local.index()),
            path: self.plan_pattern_path(&binding.path, subject_ty),
            ty: self.type_map[&binding.ty],
            mode,
        })
    }

    fn plan_pattern_path(&self, path: &air::AirPatternPath, subject_ty: TypeId) -> RirPatternPath {
        let mut ty = subject_ty;
        let steps = path
            .steps
            .iter()
            .map(|step| match *step {
                air::AirPatternPathStep::Field(field) => {
                    let source_ty = self.type_map[&ty];
                    let (target_ty, kind) = match self.air.type_arena.data(ty) {
                        TypeData::Aggregate(aggregate) => {
                            let target_ty = self.air.aggregate(*aggregate).fields[field.index()].ty;
                            (
                                target_ty,
                                RirPlaceStepKind::StructField(RirFieldId::from_index(
                                    field.index(),
                                )),
                            )
                        }
                        TypeData::Extern(extern_id) => {
                            let target_ty =
                                self.air.extern_type(*extern_id).fields[field.index()].ty;
                            let field = self
                                .extern_storage_field(ty, field)
                                .expect("AIR verifies pattern extern field path");
                            (target_ty, RirPlaceStepKind::ExternField(field))
                        }
                        TypeData::DataRef(dataref) => {
                            let target_ty = self.air.aggregate(*dataref).fields[field.index()].ty;
                            (
                                target_ty,
                                RirPlaceStepKind::DataRefField(RirFieldId::from_index(
                                    field.index(),
                                )),
                            )
                        }
                        _ => unreachable!("AIR verifies pattern field path"),
                    };
                    let projection = RirPlaceStep {
                        source_ty,
                        target_ty: self.type_map[&target_ty],
                        kind,
                    };
                    ty = target_ty;
                    RirPatternPathStep::Place(projection)
                }
                air::AirPatternPathStep::TupleField(field) => {
                    let TypeData::Tuple(fields) = self.air.type_arena.data(ty) else {
                        unreachable!("AIR verifies pattern tuple path")
                    };
                    let target_ty = fields[field as usize];
                    let projection = RirPlaceStep {
                        source_ty: self.type_map[&ty],
                        target_ty: self.type_map[&target_ty],
                        kind: RirPlaceStepKind::TupleField(RirFieldId::from_index(field as usize)),
                    };
                    ty = target_ty;
                    RirPatternPathStep::Place(projection)
                }
                air::AirPatternPathStep::OptionalSome => {
                    let TypeData::Optional(target_ty) = self.air.type_arena.data(ty) else {
                        unreachable!("AIR verifies optional pattern path")
                    };
                    let step = RirPatternPathStep::OptionalSome {
                        source_ty: self.type_map[&ty],
                        target_ty: self.type_map[target_ty],
                    };
                    ty = *target_ty;
                    step
                }
                air::AirPatternPathStep::EnumTupleField {
                    enum_id,
                    variant,
                    field,
                } => {
                    let air::VariantShape::Tuple(fields) =
                        &self.air.enums[enum_id.index()].variants[variant.index()].shape
                    else {
                        unreachable!("AIR verifies tuple enum pattern path")
                    };
                    let target_ty = fields[field as usize];
                    let step = RirPatternPathStep::EnumTupleField {
                        source_ty: self.type_map[&ty],
                        target_ty: self.type_map[&target_ty],
                        enum_id: self.enum_map[&enum_id],
                        variant: RirVariantId::from_index(variant.index()),
                        field,
                    };
                    ty = target_ty;
                    step
                }
                air::AirPatternPathStep::EnumStructField {
                    enum_id,
                    variant,
                    field,
                } => {
                    let air::VariantShape::Struct(fields) =
                        &self.air.enums[enum_id.index()].variants[variant.index()].shape
                    else {
                        unreachable!("AIR verifies struct enum pattern path")
                    };
                    let target_ty = fields[field as usize].ty;
                    let step = RirPatternPathStep::EnumStructField {
                        source_ty: self.type_map[&ty],
                        target_ty: self.type_map[&target_ty],
                        enum_id: self.enum_map[&enum_id],
                        variant: RirVariantId::from_index(variant.index()),
                        field,
                    };
                    ty = target_ty;
                    step
                }
            })
            .collect();
        RirPatternPath { steps }
    }

    fn plan_air_stmt(
        &mut self,
        function: FunctionId,
        index: usize,
        stmt: &air::AirStmt,
        locals: &mut Vec<RirLocal>,
        zero_env_function_values: &mut Vec<Option<ZeroEnvFunctionValue>>,
        initialized_cells: &mut [bool],
        possible_cells: &mut [bool],
        in_loop: bool,
    ) -> Result<Vec<RirStmt>, RustTargetGap> {
        match stmt {
            air::AirStmt::Init { local, value } => {
                let mut planned =
                    self.plan_rvalue(function, value, locals, zero_env_function_values)?;
                if locals[local.index()].mutable
                    && let RirRValue::SliceView { mutable, .. } = &mut planned.value
                {
                    *mutable = true;
                }
                let state = Self::zero_env_function_rvalue(&planned.value);
                let mut stmts = planned.stmts;
                stmts.push(RirStmt::Init {
                    local: RirLocalId::from_index(local.index()),
                    value: planned.value,
                });
                Self::set_zero_env_function(
                    zero_env_function_values,
                    RirLocalId::from_index(local.index()),
                    state,
                );
                Ok(stmts)
            }
            air::AirStmt::Assign { dst, value } => {
                let planned =
                    self.plan_rvalue(function, value, locals, zero_env_function_values)?;
                let state = Self::zero_env_function_rvalue(&planned.value);
                let mut stmts = planned.stmts;
                self.lower_place_write(
                    function,
                    dst,
                    planned.value,
                    locals,
                    &mut stmts,
                    initialized_cells,
                    possible_cells,
                    in_loop,
                )?;
                Self::set_place_zero_env_function(zero_env_function_values, dst, state);
                Ok(stmts)
            }
            air::AirStmt::Eval(value) => {
                let planned =
                    self.plan_rvalue(function, value, locals, zero_env_function_values)?;
                let mut stmts = planned.stmts;
                stmts.push(RirStmt::Eval(planned.value));
                Ok(stmts)
            }
            air::AirStmt::GlobalEnsure { global } => Ok(vec![RirStmt::GlobalEnsure {
                global: self.global_map[global],
            }]),
            air::AirStmt::GlobalSetRoot {
                global,
                value,
                init,
            } => {
                if *init != air::GlobalInitEffect::StoreWithoutInit {
                    return Err(Self::gap(
                        RustTargetGapSite::Statement(function, index),
                        RustTargetGapKind::UnsupportedGlobalRooting,
                    ));
                }
                let (mut stmts, value) = self.plan_global_root_value(
                    function,
                    *global,
                    value,
                    locals,
                    zero_env_function_values,
                )?;
                stmts.push(RirStmt::GlobalSetRoot {
                    global: self.global_map[global],
                    value,
                });
                Ok(stmts)
            }
            air::AirStmt::GlobalUpdateRoot { global, value } => {
                let (mut stmts, value) = self.plan_global_root_value(
                    function,
                    *global,
                    value,
                    locals,
                    zero_env_function_values,
                )?;
                let global_decl = &self.air.globals[global.index()];
                if matches!(
                    self.air.type_arena.data(global_decl.ty),
                    TypeData::List(_) | TypeData::Map { .. }
                ) {
                    stmts.push(RirStmt::MutPlaceSet {
                        place: RirMutPlaceArg::from_handle(
                            RirMutPlaceHandle::Global {
                                global: self.global_map[global],
                            },
                            vec![],
                        ),
                        value,
                    });
                } else {
                    stmts.push(RirStmt::GlobalUpdateRoot {
                        global: self.global_map[global],
                        value,
                    });
                }
                Ok(stmts)
            }
            air::AirStmt::If(branch) => {
                let cond = self.plan_operand_read(function, &branch.cond, locals)?;
                let entry_functions = zero_env_function_values.clone();
                let entry_cells = initialized_cells.to_vec();
                let entry_possible = possible_cells.to_vec();
                let mut then_functions = entry_functions.clone();
                let mut then_cells = entry_cells.clone();
                let mut then_possible = entry_possible.clone();
                let then_block = self.plan_air_block(
                    function,
                    &branch.then_block,
                    locals,
                    &mut then_functions,
                    &mut then_cells,
                    &mut then_possible,
                    in_loop,
                )?;
                let (else_block, else_functions, else_cells, else_possible) =
                    match &branch.else_block {
                        Some(block) => {
                            let mut else_functions = entry_functions.clone();
                            let mut else_cells = entry_cells.clone();
                            let mut else_possible = entry_possible.clone();
                            let block = self.plan_air_block(
                                function,
                                block,
                                locals,
                                &mut else_functions,
                                &mut else_cells,
                                &mut else_possible,
                                in_loop,
                            )?;
                            (Some(block), else_functions, else_cells, else_possible)
                        }
                        None => (None, entry_functions, entry_cells, entry_possible),
                    };
                Self::merge_zero_env_function_values(
                    zero_env_function_values,
                    locals.len(),
                    [&then_functions, &else_functions],
                );
                for (dst, (then, else_)) in initialized_cells
                    .iter_mut()
                    .zip(then_cells.iter().zip(&else_cells))
                {
                    *dst = *then && *else_;
                }
                for (dst, (then, else_)) in possible_cells
                    .iter_mut()
                    .zip(then_possible.iter().zip(&else_possible))
                {
                    *dst = *then || *else_;
                }
                let mut stmts = cond.stmts;
                stmts.push(RirStmt::If(RirIf {
                    cond: cond.operand,
                    then_block,
                    else_block,
                }));
                Ok(stmts)
            }
            air::AirStmt::Loop(loop_) => {
                let body = self.plan_loop_body(
                    function,
                    loop_.id,
                    &loop_.body,
                    locals,
                    zero_env_function_values,
                    initialized_cells,
                    possible_cells,
                )?;
                Ok(vec![RirStmt::Loop(RirLoop {
                    id: RirLoopId::from_index(loop_.id.index()),
                    body,
                })])
            }
            air::AirStmt::RangeFor(range) => {
                let start = self.plan_operand_read(function, &range.start, locals)?;
                let end = self.plan_operand_read(function, &range.end, locals)?;
                let (ordinal_stmts, ordinal_plan) =
                    self.plan_ordinal_plan(function, &range.ordinal_plan, locals)?;
                let body = self.plan_loop_body(
                    function,
                    range.id,
                    &range.body,
                    locals,
                    zero_env_function_values,
                    initialized_cells,
                    possible_cells,
                )?;
                let mut stmts = start.stmts;
                stmts.extend(end.stmts);
                stmts.extend(ordinal_stmts);
                stmts.push(RirStmt::RangeFor(RirRangeFor {
                    id: RirLoopId::from_index(range.id.index()),
                    start: start.operand,
                    end: end.operand,
                    ordinal_plan,
                    inclusive: range.inclusive,
                    ordinal: range
                        .ordinal
                        .map(|local| RirLocalId::from_index(local.index())),
                    item: RirLocalId::from_index(range.item.index()),
                    body,
                }));
                Ok(stmts)
            }
            air::AirStmt::CollectionFor(for_) => {
                let (mut stmts, ordinal_plan) =
                    self.plan_ordinal_plan(function, &for_.ordinal_plan, locals)?;
                let body = self.plan_loop_body(
                    function,
                    for_.id,
                    &for_.body,
                    locals,
                    zero_env_function_values,
                    initialized_cells,
                    possible_cells,
                )?;
                stmts.push(RirStmt::CollectionFor(RirCollectionFor {
                    id: RirLoopId::from_index(for_.id.index()),
                    len: RirLocalId::from_index(for_.len.index()),
                    ordinal_plan,
                    index: RirLocalId::from_index(for_.index.index()),
                    ordinal: for_
                        .ordinal
                        .map(|local| RirLocalId::from_index(local.index())),
                    body,
                }));
                Ok(stmts)
            }
            air::AirStmt::CollectionLoan(loan) => {
                let root = self.lower_collection_loan_root(function, index, loan, locals)?;
                let body = self.plan_air_block(
                    function,
                    &loan.body,
                    locals,
                    zero_env_function_values,
                    initialized_cells,
                    possible_cells,
                    in_loop,
                )?;
                Ok(vec![RirStmt::CollectionLoanScope(RirCollectionLoanScope {
                    root: root.root,
                    root_kind: root.root_kind,
                    mode: root.mode,
                    body,
                })])
            }
            air::AirStmt::CollectionSlotScope(scope) => self.plan_collection_slot_scope(
                function,
                scope,
                locals,
                zero_env_function_values,
                initialized_cells,
                possible_cells,
                in_loop,
            ),
            air::AirStmt::PatternMatch(match_) => self.plan_pattern_match(
                function,
                match_,
                locals,
                zero_env_function_values,
                initialized_cells,
                possible_cells,
                in_loop,
            ),
            air::AirStmt::DynMatch(match_) => self.plan_dyn_match(
                function,
                match_,
                locals,
                zero_env_function_values,
                initialized_cells,
                possible_cells,
                in_loop,
            ),
            air::AirStmt::OptionalMatch(match_) => {
                let (mut stmts, subject) = self.plan_option_subject(function, match_, locals)?;
                let payload = match match_.payload {
                    Some(payload) if match_.payload_ref => {
                        let local = RirLocalId::from_index(payload.index());
                        let binding = match subject {
                            RirOptionSubject::Place(_) => RirLocalBinding::DirectPayload,
                            RirOptionSubject::MutPlace(_) => RirLocalBinding::ScopedPlacePayload,
                        };
                        Self::bind_local(locals, local, binding);
                        Some(RirOptionPayloadBinding::Ref {
                            local,
                            escapes: match_.payload_escapes,
                        })
                    }
                    Some(payload) => {
                        let TypeData::Optional(inner) = self.air.type_arena.data(match_.discr.ty)
                        else {
                            unreachable!("verified option match subject")
                        };
                        Some(RirOptionPayloadBinding::Owned {
                            local: RirLocalId::from_index(payload.index()),
                            materializer: self.reusable_materializer(
                                function,
                                *inner,
                                RustRecipePosition::Value,
                            )?,
                        })
                    }
                    None => None,
                };
                let entry_functions = zero_env_function_values.clone();
                let entry_cells = initialized_cells.to_vec();
                let entry_possible = possible_cells.to_vec();
                let mut some_functions = entry_functions.clone();
                let mut some_cells = entry_cells.clone();
                let mut some_possible = entry_possible.clone();
                let some_block = self.plan_air_block(
                    function,
                    &match_.some_block,
                    locals,
                    &mut some_functions,
                    &mut some_cells,
                    &mut some_possible,
                    in_loop,
                )?;
                let mut none_functions = entry_functions;
                let mut none_cells = entry_cells;
                let mut none_possible = entry_possible;
                let none_block = self.plan_air_block(
                    function,
                    &match_.none_block,
                    locals,
                    &mut none_functions,
                    &mut none_cells,
                    &mut none_possible,
                    in_loop,
                )?;
                Self::merge_zero_env_function_values(
                    zero_env_function_values,
                    locals.len(),
                    [&some_functions, &none_functions],
                );
                for (dst, (some, none)) in initialized_cells
                    .iter_mut()
                    .zip(some_cells.iter().zip(&none_cells))
                {
                    *dst = *some && *none;
                }
                for (dst, (some, none)) in possible_cells
                    .iter_mut()
                    .zip(some_possible.iter().zip(&none_possible))
                {
                    *dst = *some || *none;
                }
                stmts.push(RirStmt::OptionMatch(RirOptionMatch {
                    subject,
                    payload,
                    some_block,
                    none_block,
                }));
                Ok(stmts)
            }
            air::AirStmt::MapEntryMatch(match_) => {
                let mut key = self.plan_operand_read(function, &match_.key, locals)?;
                let payload = match_
                    .payload
                    .map(|payload| RirLocalId::from_index(payload.index()));
                if let Some(payload) = payload {
                    Self::bind_local(locals, payload, RirLocalBinding::ScopedPlacePayload);
                }
                let entry_functions = zero_env_function_values.clone();
                let entry_cells = initialized_cells.to_vec();
                let entry_possible = possible_cells.to_vec();
                let mut some_functions = entry_functions.clone();
                let mut some_cells = entry_cells.clone();
                let mut some_possible = entry_possible.clone();
                let some_block = self.plan_air_block(
                    function,
                    &match_.some_block,
                    locals,
                    &mut some_functions,
                    &mut some_cells,
                    &mut some_possible,
                    in_loop,
                )?;
                let mut none_functions = entry_functions;
                let mut none_cells = entry_cells;
                let mut none_possible = entry_possible;
                let none_block = self.plan_air_block(
                    function,
                    &match_.none_block,
                    locals,
                    &mut none_functions,
                    &mut none_cells,
                    &mut none_possible,
                    in_loop,
                )?;
                Self::merge_zero_env_function_values(
                    zero_env_function_values,
                    locals.len(),
                    [&some_functions, &none_functions],
                );
                for (dst, (some, none)) in initialized_cells
                    .iter_mut()
                    .zip(some_cells.iter().zip(&none_cells))
                {
                    *dst = *some && *none;
                }
                for (dst, (some, none)) in possible_cells
                    .iter_mut()
                    .zip(some_possible.iter().zip(&none_possible))
                {
                    *dst = *some || *none;
                }
                let map_plan = self
                    .access()
                    .plan(function, PlaceAccessIntent::MutPlaceArg, &match_.map)
                    .map_err(|gap| Self::access_gap(function, gap))?;
                let planned_map = self.plan_mut_place_arg(function, &map_plan, locals)?;
                key.stmts.extend(planned_map.stmts);
                key.stmts.push(RirStmt::MapEntryMatch(RirMapEntryMatch {
                    map: planned_map.arg,
                    key: key.operand,
                    payload,
                    payload_escapes: match_.payload_escapes,
                    some_block,
                    none_block,
                }));
                Ok(key.stmts)
            }
        }
    }

    fn fill_dynamic_variant_sets(&mut self, program: &mut RirProgram) {
        for (carrier_id, target) in std::mem::take(&mut self.dyn_variant_set_requests) {
            let carrier = &program.dyn_carriers[carrier_id.index()];
            let variants = carrier
                .variants
                .iter()
                .enumerate()
                .filter_map(|(index, variant)| {
                    (variant.concrete_ty == target)
                        .then_some(RirDynVariantId::new(carrier.id, index))
                })
                .collect();
            let id = self.dyn_variant_set_map[&(carrier_id, target)];
            debug_assert_eq!(
                id,
                RirDynVariantSetId::from_index(program.dyn_variant_sets.len())
            );
            program.dyn_variant_sets.push(RirDynVariantSet {
                id,
                carrier: carrier_id,
                target,
                variants,
            });
        }
    }

    fn dyn_variant_set(
        &mut self,
        surface: air::ContractSurfaceId,
        target: TypeId,
    ) -> RirDynVariantSetId {
        let key = (self.dyn_surface_map[&surface], self.type_map[&target]);
        if let Some(&id) = self.dyn_variant_set_map.get(&key) {
            return id;
        }
        let id = RirDynVariantSetId::from_index(self.dyn_variant_set_requests.len());
        self.dyn_variant_set_map.insert(key, id);
        self.dyn_variant_set_requests.push(key);
        id
    }

    fn dyn_match_mappings(&mut self, match_: &air::AirDynMatch) -> Vec<RirDynMatchMapping> {
        let mut surfaces = vec![match_.surface];
        surfaces.extend(
            self.air
                .contract_weakenings
                .iter()
                .filter(|weakening| weakening.target == match_.surface)
                .map(|weakening| weakening.source),
        );
        surfaces.sort();
        surfaces.dedup();
        let direct = surfaces
            .iter()
            .position(|&surface| surface == match_.surface)
            .expect("dynamic match surface");
        surfaces.swap(0, direct);
        surfaces
            .into_iter()
            .map(|surface| {
                let carrier = self.dyn_surface_map[&surface];
                let targets = self.dyn_carrier_witnesses[&carrier]
                    .iter()
                    .map(|witness| {
                        let concrete = self.air.contract_witnesses[witness.index()].key.concrete_ty;
                        match_.arms.iter().position(|arm| arm.target == concrete)
                    })
                    .collect();
                RirDynMatchMapping {
                    carrier,
                    variants: match_
                        .arms
                        .iter()
                        .map(|arm| self.dyn_variant_set(surface, arm.target))
                        .collect(),
                    targets,
                }
            })
            .collect()
    }

    fn plan_dyn_match(
        &mut self,
        function: FunctionId,
        match_: &air::AirDynMatch,
        locals: &mut Vec<RirLocal>,
        zero_env_function_values: &mut Vec<Option<ZeroEnvFunctionValue>>,
        initialized_cells: &mut [bool],
        possible_cells: &mut [bool],
        in_loop: bool,
    ) -> Result<Vec<RirStmt>, RustTargetGap> {
        let aliases = match_
            .arms
            .iter()
            .any(|arm| matches!(arm.binding, air::AirDynMatchTargetBinding::Alias(_)));
        let (mut stmts, source, mutable, borrowed) = match &match_.source {
            air::AirDynMatchSource::Mutable(place) => {
                if let air::PlaceRoot::DynBorrowParam(id) = place.root {
                    let decl = &self.air.dyn_borrow_params[id.index()];
                    let carrier = self.dyn_surface_map[&decl.surface];
                    (
                        vec![],
                        rir::RirDynMatchSource::Borrowed(rir::RirDynBorrow {
                            source: rir::RirDynBorrowSource::Borrowed {
                                local: RirLocalId::from_index(decl.source.index()),
                                carrier,
                            },
                            target: carrier,
                            weakening: None,
                        }),
                        true,
                        true,
                    )
                } else {
                    let access = self
                        .access()
                        .plan(function, PlaceAccessIntent::MutPlaceArg, place)
                        .map_err(|gap| Self::access_gap(function, gap))?;
                    let source = self.plan_mut_place_arg(function, &access, locals)?;
                    (
                        source.stmts,
                        rir::RirDynMatchSource::MutPlace(source.arg),
                        true,
                        false,
                    )
                }
            }
            air::AirDynMatchSource::Owned(value) => {
                let value =
                    self.plan_air_owned_value(function, value, RustRecipePosition::Value, locals)?;
                (
                    value.stmts,
                    rir::RirDynMatchSource::Owned(value.value),
                    false,
                    false,
                )
            }
            air::AirDynMatchSource::Borrowed(borrow) => {
                let planned = self.plan_dyn_borrow_arg(function, borrow, locals)?;
                let RirCallArg::DynBorrow(borrow) = planned.arg else {
                    unreachable!("dynamic match borrow planning")
                };
                (
                    planned.stmts,
                    rir::RirDynMatchSource::Borrowed(borrow),
                    aliases,
                    true,
                )
            }
        };
        let mappings = self.dyn_match_mappings(match_);
        let entry_functions = zero_env_function_values.clone();
        let entry_cells = initialized_cells.to_vec();
        let entry_possible = possible_cells.to_vec();
        let mut arm_states = Vec::with_capacity(match_.arms.len() + 1);
        let mut arms = Vec::with_capacity(match_.arms.len());
        for arm in &match_.arms {
            let binding = match arm.binding {
                air::AirDynMatchTargetBinding::Discard => rir::RirDynMatchBinding::Discard,
                air::AirDynMatchTargetBinding::Alias(local) if mutable => {
                    let local = RirLocalId::from_index(local.index());
                    Self::bind_local(locals, local, RirLocalBinding::ScopedPlacePayload);
                    rir::RirDynMatchBinding::Alias(local)
                }
                air::AirDynMatchTargetBinding::Take(local)
                | air::AirDynMatchTargetBinding::Materialize(local)
                    if !mutable && !borrowed =>
                {
                    rir::RirDynMatchBinding::Take(RirLocalId::from_index(local.index()))
                }
                air::AirDynMatchTargetBinding::Materialize(local) if !mutable && borrowed => {
                    rir::RirDynMatchBinding::Materialize {
                        local: RirLocalId::from_index(local.index()),
                        materializer: self.reusable_materializer(
                            function,
                            arm.target,
                            RustRecipePosition::Value,
                        )?,
                    }
                }
                _ => unreachable!("verified dynamic match target binding"),
            };
            let mut functions = entry_functions.clone();
            let mut cells = entry_cells.clone();
            let mut possible = entry_possible.clone();
            let block = self.plan_air_block(
                function,
                &arm.block,
                locals,
                &mut functions,
                &mut cells,
                &mut possible,
                in_loop,
            )?;
            arm_states.push((functions, cells, possible));
            arms.push(rir::RirDynMatchArm { binding, block });
        }
        let fallback_binding = match match_.fallback.binding {
            air::AirDynMatchFallbackBinding::Discard => rir::RirDynMatchFallbackBinding::Discard,
            air::AirDynMatchFallbackBinding::Alias(local) if mutable && !borrowed => {
                let local = RirLocalId::from_index(local.index());
                Self::bind_local(locals, local, RirLocalBinding::ScopedPlacePayload);
                rir::RirDynMatchFallbackBinding::Alias(local)
            }
            air::AirDynMatchFallbackBinding::Alias(local) if borrowed => {
                rir::RirDynMatchFallbackBinding::Preserve(RirLocalId::from_index(local.index()))
            }
            air::AirDynMatchFallbackBinding::Preserve(local) if !mutable && !borrowed => {
                rir::RirDynMatchFallbackBinding::Take(RirLocalId::from_index(local.index()))
            }
            air::AirDynMatchFallbackBinding::Preserve(local) if borrowed => {
                rir::RirDynMatchFallbackBinding::Preserve(RirLocalId::from_index(local.index()))
            }
            _ => unreachable!("verified dynamic match fallback binding"),
        };
        let mut fallback_functions = entry_functions;
        let mut fallback_cells = entry_cells;
        let mut fallback_possible = entry_possible;
        let reborrow = match fallback_binding {
            rir::RirDynMatchFallbackBinding::Preserve(local) => Some((
                LocalId::from_index(local.index()),
                self.dyn_surface_map[&match_.surface],
            )),
            _ => None,
        };
        if let Some(reborrow) = reborrow {
            self.dyn_reborrows.push(reborrow);
        }
        let fallback = self.plan_air_block(
            function,
            &match_.fallback.block,
            locals,
            &mut fallback_functions,
            &mut fallback_cells,
            &mut fallback_possible,
            in_loop,
        );
        if reborrow.is_some() {
            self.dyn_reborrows.pop();
        }
        let fallback = fallback?;
        arm_states.push((fallback_functions, fallback_cells, fallback_possible));
        Self::merge_zero_env_function_values(
            zero_env_function_values,
            locals.len(),
            arm_states.iter().map(|(functions, _, _)| functions),
        );
        for (index, dst) in initialized_cells.iter_mut().enumerate() {
            *dst = arm_states.iter().all(|(_, cells, _)| cells[index]);
        }
        for (index, dst) in possible_cells.iter_mut().enumerate() {
            *dst = arm_states.iter().any(|(_, _, possible)| possible[index]);
        }
        stmts.push(RirStmt::DynMatch(rir::RirDynMatch {
            carrier: self.dyn_surface_map[&match_.surface],
            source,
            arms,
            mappings,
            fallback_binding,
            fallback,
        }));
        Ok(stmts)
    }

    fn plan_option_subject(
        &mut self,
        function: FunctionId,
        match_: &air::AirOptionalMatch,
        locals: &mut Vec<RirLocal>,
    ) -> Result<(Vec<RirStmt>, RirOptionSubject), RustTargetGap> {
        if !match_.payload_ref {
            let discr = self.lower_place_read(function, &match_.discr, locals)?;
            let RirOperand::Place(place) = discr.operand else {
                unreachable!("place read returns a place operand")
            };
            return Ok((discr.stmts, RirOptionSubject::Place(place)));
        }

        let plan = self
            .access()
            .plan(function, PlaceAccessIntent::PayloadAlias, &match_.discr)
            .map_err(|gap| Self::access_gap(function, gap))?;
        let payload_root = matches!(
            match_.discr.root,
            air::PlaceRoot::Local(local)
                if locals.get(local.index()).is_some_and(|local| {
                    matches!(
                        local.binding,
                        RirLocalBinding::DirectPayload | RirLocalBinding::ScopedPlacePayload
                    )
                })
        );
        if plan.payload_alias_direct_place() && !payload_root {
            let discr = self.lower_place_read(function, &match_.discr, locals)?;
            let RirOperand::Place(place) = discr.operand else {
                unreachable!("place read returns a place operand")
            };
            return Ok((discr.stmts, RirOptionSubject::Place(place)));
        }

        let planned = self.plan_mut_place_arg(function, &plan, locals)?;
        Ok((planned.stmts, RirOptionSubject::MutPlace(planned.arg)))
    }

    fn set_zero_env_function(
        zero_env_function_values: &mut Vec<Option<ZeroEnvFunctionValue>>,
        local: RirLocalId,
        known: Option<ZeroEnvFunctionValue>,
    ) {
        if zero_env_function_values.len() <= local.index() {
            zero_env_function_values.resize(local.index() + 1, None);
        }
        zero_env_function_values[local.index()] = known;
    }

    fn set_place_zero_env_function(
        zero_env_function_values: &mut Vec<Option<ZeroEnvFunctionValue>>,
        place: &Place,
        known: Option<ZeroEnvFunctionValue>,
    ) {
        let Some(local) = place.root.local() else {
            return;
        };
        Self::set_zero_env_function(
            zero_env_function_values,
            RirLocalId::from_index(local.index()),
            place.projection.is_empty().then_some(known).flatten(),
        );
    }

    fn merge_zero_env_function_values<'b>(
        zero_env_function_values: &mut Vec<Option<ZeroEnvFunctionValue>>,
        len: usize,
        states: impl IntoIterator<Item = &'b Vec<Option<ZeroEnvFunctionValue>>>,
    ) {
        let states = states.into_iter().collect::<Vec<_>>();
        zero_env_function_values.clear();
        zero_env_function_values.resize(len, None);
        let Some(first_state) = states.first() else {
            return;
        };
        for (index, slot) in zero_env_function_values.iter_mut().enumerate() {
            let first = first_state.get(index).copied().flatten();
            if states
                .iter()
                .all(|state| state.get(index).copied().flatten() == first)
            {
                *slot = first;
            }
        }
    }

    fn plan_air_tail(
        &mut self,
        function: FunctionId,
        tail: &air::AirTail,
        locals: &mut Vec<RirLocal>,
    ) -> Result<(Vec<RirStmt>, RirTerm), RustTargetGap> {
        Ok(match tail {
            air::AirTail::None => (vec![], RirTerm::None),
            air::AirTail::Return(Some(value)) => {
                let planned = self.plan_operand_read(function, value, locals)?;
                (planned.stmts, RirTerm::Return(Some(planned.operand)))
            }
            air::AirTail::ReturnOwned(owned) => {
                let planned =
                    self.plan_air_owned_value(function, owned, RustRecipePosition::Value, locals)?;
                (planned.stmts, RirTerm::ReturnOwned(planned.value))
            }
            air::AirTail::Return(None) => (vec![], RirTerm::Return(None)),
            air::AirTail::Unreachable => (vec![], RirTerm::Unreachable),
            air::AirTail::Break(id) => (vec![], RirTerm::Break(RirLoopId::from_index(id.index()))),
            air::AirTail::Continue(id) => {
                (vec![], RirTerm::Continue(RirLoopId::from_index(id.index())))
            }
        })
    }

    fn plan_rvalue(
        &mut self,
        function: FunctionId,
        value: &RValue,
        locals: &mut Vec<RirLocal>,
        zero_env_function_values: &[Option<ZeroEnvFunctionValue>],
    ) -> Result<PlannedRValue, RustTargetGap> {
        let planned = match value {
            RValue::DynPack { value, witness, .. } => {
                let variant = self.dyn_variant_map[witness];
                let surface = self.air.contract_witnesses[witness.index()].key.surface;
                let ty = self
                    .dynamic_types
                    .iter()
                    .find_map(|(ty, candidate, _)| (*candidate == surface).then_some(*ty))
                    .map(|ty| self.type_map[&ty])
                    .expect("reachable verified dynamic carrier");
                let value = self.plan_air_owned_value(
                    function,
                    value,
                    RustRecipePosition::StoredPayload(LambdaStorageFamily::DynamicPayload),
                    locals,
                )?;
                PlannedRValue {
                    stmts: value.stmts,
                    value: RirRValue::DynPack {
                        variant,
                        value: value.value,
                        ty,
                    },
                }
            }
            RValue::DynCall {
                receiver,
                surface,
                slot,
                args,
            } => {
                let (mut stmts, receiver) = match receiver {
                    air::DynReceiver::Owned(value) => {
                        let value = self.plan_air_owned_value(
                            function,
                            value,
                            RustRecipePosition::Value,
                            locals,
                        )?;
                        let receiver = match &value.value.value {
                            RirOwnedOperand::DynBorrow(borrow) => {
                                RirDynReceiver::Borrowed(borrow.clone())
                            }
                            _ => RirDynReceiver::Owned(value.value),
                        };
                        (value.stmts, receiver)
                    }
                    air::DynReceiver::Borrowed(borrow) => {
                        let planned = self.plan_dyn_borrow_arg(function, borrow, locals)?;
                        let RirCallArg::DynBorrow(borrow) = planned.arg else {
                            unreachable!("dynamic borrow planner returns dynamic borrow argument")
                        };
                        (planned.stmts, RirDynReceiver::Borrowed(borrow))
                    }
                    air::DynReceiver::MutableOwned(place) => {
                        if let Some(borrow) = self.dyn_borrow_for_place(place) {
                            (vec![], RirDynReceiver::Borrowed(borrow))
                        } else {
                            let plan = self
                                .access()
                                .plan(function, PlaceAccessIntent::MutPlaceArg, place)
                                .map_err(|gap| Self::access_gap(function, gap))?;
                            let planned = self.plan_mut_place_arg(function, &plan, locals)?;
                            (planned.stmts, RirDynReceiver::MutPlace(planned.arg))
                        }
                    }
                };
                let required = &self.air.contract_surfaces[surface.index()].slots[slot.index()];
                let dispatch = self.dyn_dispatch_map[&(*surface, *slot)];
                let mut rir_args = Vec::with_capacity(args.len());
                for (index, arg) in args.iter().enumerate() {
                    let source = &required.params[index];
                    let expected = rir::source_pass_mode(self.air, source.ty, source.mode);
                    let param = self.dyn_dispatch_params[dispatch.index()][index];
                    let planned = self.plan_arg(function, arg, expected, locals)?;
                    stmts.extend(planned.stmts);
                    rir_args.push(self.adapt_dyn_arg(param, planned.arg));
                }
                let air::ContractReturnDecl::Value(ret) = required.ret else {
                    return Err(Self::gap(
                        RustTargetGapSite::Function(function),
                        RustTargetGapKind::UnsupportedDynamicPlaceReturn,
                    ));
                };
                PlannedRValue {
                    stmts,
                    value: RirRValue::DynCall {
                        dispatch,
                        exact_variant: None,
                        receiver,
                        args: rir_args,
                        ty: self.type_map[&ret],
                    },
                }
            }
            RValue::DynWeaken {
                value,
                weakening,
                ty,
            } => {
                let weakening = self.dyn_weakening_map[weakening];
                let value =
                    self.plan_air_owned_value(function, value, RustRecipePosition::Value, locals)?;
                PlannedRValue {
                    stmts: value.stmts,
                    value: RirRValue::DynWeaken {
                        weakening,
                        value: value.value,
                        ty: self.type_map[ty],
                    },
                }
            }
            RValue::DynDowncast {
                value,
                surface,
                target,
                ty,
            } => {
                let variants = self.dyn_variant_set(*surface, *target);
                let value =
                    self.plan_air_owned_value(function, value, RustRecipePosition::Value, locals)?;
                PlannedRValue {
                    stmts: value.stmts,
                    value: RirRValue::DynDowncast {
                        variants,
                        value: value.value,
                        ty: self.type_map[ty],
                    },
                }
            }
            RValue::Use(operand) => {
                return self.plan_use(function, operand, locals, zero_env_function_values);
            }
            RValue::Materialize(owned) => {
                let planned =
                    self.plan_air_owned_value(function, owned, RustRecipePosition::Value, locals)?;
                return Ok(PlannedRValue {
                    stmts: planned.stmts,
                    value: RirRValue::Materialize(planned.value),
                });
            }
            RValue::FunctionValue { value, capability } => {
                let ty = self
                    .air
                    .operand_ty(&value.value)
                    .expect("verified function value operand has type");
                let value =
                    self.plan_air_owned_value(function, value, RustRecipePosition::Value, locals)?;
                PlannedRValue {
                    stmts: value.stmts,
                    value: RirRValue::FunctionValue {
                        value: value.value,
                        escape: rir_function_value_escape(*capability),
                        ty: self.type_map[&ty],
                    },
                }
            }
            RValue::Unary { op, value, ty } => {
                let value = self.plan_operand_read(function, value, locals)?;
                PlannedRValue {
                    stmts: value.stmts,
                    value: RirRValue::Unary {
                        op: *op,
                        value: value.operand,
                        ty: self.type_map[ty],
                    },
                }
            }
            RValue::Binary { op, lhs, rhs, ty } => {
                let lhs = self.plan_operand_read(function, lhs, locals)?;
                let rhs = self.plan_operand_read(function, rhs, locals)?;
                let mut stmts = lhs.stmts;
                stmts.extend(rhs.stmts);
                PlannedRValue {
                    stmts,
                    value: RirRValue::Binary {
                        op: *op,
                        lhs: lhs.operand,
                        rhs: rhs.operand,
                        ty: self.type_map[ty],
                    },
                }
            }
            RValue::SharedRefEq { lhs, rhs, negated } => {
                let lhs = self.plan_operand_read(function, lhs, locals)?;
                let rhs = self.plan_operand_read(function, rhs, locals)?;
                let mut stmts = lhs.stmts;
                stmts.extend(rhs.stmts);
                PlannedRValue {
                    stmts,
                    value: RirRValue::SharedRefEq {
                        lhs: lhs.operand,
                        rhs: rhs.operand,
                        negated: *negated,
                    },
                }
            }
            RValue::Cast { value, target } => {
                let value = self.plan_operand_read(function, value, locals)?;
                PlannedRValue {
                    stmts: value.stmts,
                    value: RirRValue::Cast {
                        value: value.operand,
                        target: self.type_map[target],
                    },
                }
            }
            RValue::RawProject { value, target } => {
                let value = self.plan_operand_read(function, value, locals)?;
                PlannedRValue {
                    stmts: value.stmts,
                    value: RirRValue::RawProject {
                        value: value.operand,
                        target: self.type_map[target],
                    },
                }
            }
            RValue::RawTryConstruct { value, target, ty } => {
                let value = self.plan_operand_read(function, value, locals)?;
                PlannedRValue {
                    stmts: value.stmts,
                    value: RirRValue::RawTryConstruct {
                        value: value.operand,
                        target: self.type_map[target],
                        ty: self.type_map[ty],
                    },
                }
            }
            RValue::FlagStatic { op, ty } => PlannedRValue {
                stmts: vec![],
                value: RirRValue::FlagStatic {
                    op: match op {
                        air::FlagStaticOp::Empty => RirFlagStaticOp::Empty,
                        air::FlagStaticOp::All => RirFlagStaticOp::All,
                    },
                    ty: self.type_map[ty],
                },
            },
            RValue::OptionalSome { value, ty } => {
                let value = self.plan_air_owned_value(
                    function,
                    value,
                    RustRecipePosition::StoredPayload(LambdaStorageFamily::OptionalPayload),
                    locals,
                )?;
                PlannedRValue {
                    stmts: value.stmts,
                    value: RirRValue::OptionalSome {
                        value: value.value,
                        ty: self.type_map[ty],
                    },
                }
            }
            RValue::Call { callee, args } => self.plan_call(function, callee, args, locals)?,
            RValue::Stringify { value, source_ty } => {
                let value = self.plan_operand_read(function, value, locals)?;
                PlannedRValue {
                    stmts: value.stmts,
                    value: RirRValue::Stringify {
                        value: value.operand,
                        source_ty: self.type_map[source_ty],
                    },
                }
            }
            RValue::StringConcat { parts } => {
                let parts = self.plan_operands_read(function, parts, locals)?;
                PlannedRValue {
                    stmts: parts.stmts,
                    value: RirRValue::StringConcat {
                        parts: parts.operands,
                    },
                }
            }
            RValue::Format { value, spec } => {
                let air_ty = self.operand_ty(value);
                if !format_supported(self.air.type_arena.data(air_ty), *spec) {
                    return Err(Self::gap(
                        RustTargetGapSite::Function(function),
                        RustTargetGapKind::UnsupportedRValue,
                    ));
                }
                let source_ty = self.type_map[&air_ty];
                let value = self.plan_operand_read(function, value, locals)?;
                PlannedRValue {
                    stmts: value.stmts,
                    value: RirRValue::Format {
                        value: value.operand,
                        source_ty,
                        spec: *spec,
                    },
                }
            }
            RValue::Aggregate { kind, fields, ty } => {
                return self.plan_aggregate(function, kind, fields, *ty, locals);
            }
            RValue::Len { source }
                if matches!(
                    self.air.type_arena.data(source.ty),
                    TypeData::Array { .. }
                        | TypeData::List(_)
                        | TypeData::Map { .. }
                        | TypeData::Slice(_)
                ) =>
            {
                let source =
                    self.plan_collection_access(function, source, CollectionAccessOp::Len, locals)?;
                PlannedRValue {
                    stmts: source.stmts,
                    value: RirRValue::CollectionLen {
                        source: source.access,
                    },
                }
            }
            RValue::Len { source } => {
                let source = self.lower_place_read(function, source, locals)?;
                let RirOperand::Place(source_place) = source.operand else {
                    unreachable!("place read returns a place operand")
                };
                PlannedRValue {
                    stmts: source.stmts,
                    value: RirRValue::Len {
                        source: source_place,
                    },
                }
            }
            RValue::ListPush { list, value } => {
                let list = self.structural_collection_access(function, list, locals)?;
                let value = self.plan_air_owned_value(
                    function,
                    value,
                    RustRecipePosition::StoredPayload(LambdaStorageFamily::ListElement),
                    locals,
                )?;
                let mut stmts = list.stmts;
                stmts.extend(value.stmts);
                PlannedRValue {
                    stmts,
                    value: RirRValue::ListPush {
                        list: list.access,
                        value: value.value,
                    },
                }
            }
            RValue::SliceView {
                source,
                start,
                end,
                inclusive,
                ty,
            } => {
                let source = self.plan_collection_access(
                    function,
                    source,
                    CollectionAccessOp::SliceView,
                    locals,
                )?;
                PlannedRValue {
                    stmts: source.stmts,
                    value: RirRValue::SliceView {
                        source: source.access,
                        start: RirLocalId::from_index(start.index()),
                        end: RirLocalId::from_index(end.index()),
                        inclusive: *inclusive,
                        mutable: false,
                        ty: self.type_map[ty],
                    },
                }
            }
            RValue::RangeListCopy {
                source,
                start,
                end,
                inclusive,
                ty,
            } => {
                let source = self.lower_place_read(function, source, locals)?;
                let RirOperand::Place(source_place) = source.operand else {
                    unreachable!("place read returns a place operand")
                };
                PlannedRValue {
                    stmts: source.stmts,
                    value: RirRValue::RangeListCopy {
                        source: source_place,
                        start: RirLocalId::from_index(start.index()),
                        end: RirLocalId::from_index(end.index()),
                        inclusive: *inclusive,
                        ty: self.type_map[ty],
                    },
                }
            }
            RValue::MapGet { map, key, ty } => {
                let map =
                    self.plan_collection_access(function, map, CollectionAccessOp::MapGet, locals)?;
                let key = self.plan_operand_read(function, key, locals)?;
                let mut stmts = map.stmts;
                stmts.extend(key.stmts);
                PlannedRValue {
                    stmts,
                    value: RirRValue::MapGet {
                        map: map.access,
                        key: key.operand,
                        ty: self.type_map[ty],
                    },
                }
            }
            RValue::MapInsert {
                map,
                key,
                value,
                kind,
            } => {
                let map = self.plan_collection_access(
                    function,
                    map,
                    CollectionAccessOp::map_write(*kind),
                    locals,
                )?;
                let key =
                    self.plan_air_owned_value(function, key, RustRecipePosition::MapKey, locals)?;
                let value = self.plan_air_owned_value(
                    function,
                    value,
                    RustRecipePosition::StoredPayload(LambdaStorageFamily::MapValue),
                    locals,
                )?;
                let mut stmts = map.stmts;
                stmts.extend(key.stmts);
                stmts.extend(value.stmts);
                PlannedRValue {
                    stmts,
                    value: RirRValue::MapInsert {
                        map: map.access,
                        key: key.value,
                        value: value.value,
                        kind: rir_map_write_kind(*kind),
                    },
                }
            }
            RValue::MapRemove { map, key, ty } => {
                let map = self.structural_collection_access(function, map, locals)?;
                let key = self.plan_operand_read(function, key, locals)?;
                let mut stmts = map.stmts;
                stmts.extend(key.stmts);
                PlannedRValue {
                    stmts,
                    value: RirRValue::MapRemove {
                        map: map.access,
                        key: key.operand,
                        ty: self.type_map[ty],
                    },
                }
            }
            RValue::CheckedIterCount { count, check } => {
                let count = self.plan_operand_read(function, count, locals)?;
                PlannedRValue {
                    stmts: count.stmts,
                    value: RirRValue::CheckedIterCount {
                        count: count.operand,
                        check: match check {
                            air::IterCountCheck::SkipNonNegative => {
                                RirIterCountCheck::SkipNonNegative
                            }
                            air::IterCountCheck::TakeNonNegative => {
                                RirIterCountCheck::TakeNonNegative
                            }
                            air::IterCountCheck::StepByPositive => {
                                RirIterCountCheck::StepByPositive
                            }
                        },
                    },
                }
            }
            RValue::FunctionRef {
                function: target,
                ty,
            } => PlannedRValue::from_value(RirRValue::Lambda {
                lambda: self.function_lambda_map[target],
                captures: vec![],
                ty: self.type_map[ty],
            }),
            RValue::MakeLambda {
                lambda,
                captures,
                ty,
            } => {
                let captures =
                    self.plan_lambda_capture_args(function, *lambda, captures, locals)?;
                PlannedRValue {
                    stmts: captures.stmts,
                    value: RirRValue::Lambda {
                        lambda: self.lambda_map[lambda],
                        captures: captures.captures,
                        ty: self.type_map[ty],
                    },
                }
            }
            RValue::MapEntryAt { map, index, ty } => {
                let map = self.plan_collection_access(
                    function,
                    map,
                    CollectionAccessOp::MapEntryRead,
                    locals,
                )?;
                PlannedRValue {
                    stmts: map.stmts,
                    value: RirRValue::MapEntryAt {
                        map: map.access,
                        index: RirLocalId::from_index(index.index()),
                        ty: self.type_map[ty],
                    },
                }
            }
            RValue::MapKeyAt { map, index, ty } => {
                let map = self.plan_collection_access(
                    function,
                    map,
                    CollectionAccessOp::MapEntryRead,
                    locals,
                )?;
                PlannedRValue {
                    stmts: map.stmts,
                    value: RirRValue::MapKeyAt {
                        map: map.access,
                        index: RirLocalId::from_index(index.index()),
                        ty: self.type_map[ty],
                    },
                }
            }
            RValue::MapValueAt { map, index, ty } => {
                let map = self.plan_collection_access(
                    function,
                    map,
                    CollectionAccessOp::MapEntryRead,
                    locals,
                )?;
                PlannedRValue {
                    stmts: map.stmts,
                    value: RirRValue::MapValueAt {
                        map: map.access,
                        index: RirLocalId::from_index(index.index()),
                        ty: self.type_map[ty],
                    },
                }
            }
            RValue::ListPop { .. } => {
                return Err(Self::gap(
                    RustTargetGapSite::Function(function),
                    RustTargetGapKind::UnsupportedRValue,
                ));
            }
        };
        Ok(planned)
    }

    fn plan_loop_body(
        &mut self,
        function: FunctionId,
        loop_id: air::AirLoopId,
        body: &air::AirBlock,
        locals: &mut Vec<RirLocal>,
        zero_env_function_values: &mut Vec<Option<ZeroEnvFunctionValue>>,
        initialized_cells: &mut [bool],
        possible_cells: &mut [bool],
    ) -> Result<RirStructuredBlock, RustTargetGap> {
        let entry_functions = zero_env_function_values.clone();
        let entry_cells = initialized_cells.to_vec();
        let entry_possible = possible_cells.to_vec();
        let mut body_functions = entry_functions.clone();
        let mut body_cells = entry_cells.clone();
        let mut body_possible = entry_possible.clone();
        let planned = self.plan_air_block(
            function,
            body,
            locals,
            &mut body_functions,
            &mut body_cells,
            &mut body_possible,
            true,
        )?;
        Self::merge_zero_env_function_values(
            zero_env_function_values,
            locals.len(),
            [&entry_functions, &body_functions],
        );
        for (dst, (entry, body)) in initialized_cells
            .iter_mut()
            .zip(entry_cells.iter().zip(&body_cells))
        {
            *dst = *entry && *body;
        }
        for (dst, (entry, body)) in possible_cells
            .iter_mut()
            .zip(entry_possible.iter().zip(&body_possible))
        {
            *dst = *entry || *body;
        }
        for (index, cell) in self.air.capture_cells.iter().enumerate() {
            if matches!(
                cell.lifetime,
                air::CaptureCellLifetime::Loop { loop_id: id } if id == loop_id
            ) {
                initialized_cells[index] = entry_cells[index];
                possible_cells[index] = entry_possible[index];
            }
        }
        Ok(planned)
    }

    fn plan_lambda_capture_args(
        &mut self,
        function: FunctionId,
        lambda: air::LambdaId,
        captures: &[air::LambdaCaptureArg],
        locals: &mut Vec<RirLocal>,
    ) -> Result<PlannedLambdaCaptureArgs, RustTargetGap> {
        let mut stmts = vec![];
        let mut planned = vec![];
        for (index, capture) in captures.iter().enumerate() {
            match capture {
                air::LambdaCaptureArg::NoRuntime => {}
                air::LambdaCaptureArg::ReadonlyLocal { value } => {
                    let slot = air::LambdaCaptureSlotId::from_index(index);
                    match self.lambda_capture_modes[&(lambda, slot)] {
                        RirPassMode::Value => {
                            let value = self.plan_air_owned_value(
                                function,
                                value,
                                RustRecipePosition::Value,
                                locals,
                            )?;
                            stmts.extend(value.stmts);
                            planned.push(RirLambdaCaptureArg::Owned { value: value.value });
                        }
                        RirPassMode::SharedBorrow => {
                            if value.source != air::ValueSource::Reusable {
                                return Err(Self::gap(
                                    RustTargetGapSite::Function(function),
                                    RustTargetGapKind::UnsupportedLambdaCapture,
                                ));
                            }
                            let Operand::Place(place) = &value.value else {
                                return Err(Self::gap(
                                    RustTargetGapSite::Function(function),
                                    RustTargetGapKind::UnsupportedLambdaCapture,
                                ));
                            };
                            let value = self.lower_place_read(function, place, locals)?;
                            stmts.extend(value.stmts);
                            let RirOperand::Place(place) = value.operand else {
                                unreachable!("place read returns a place operand")
                            };
                            planned.push(RirLambdaCaptureArg::Shared { place });
                        }
                        _ => unreachable!("readonly capture has value or shared mode"),
                    }
                }
                air::LambdaCaptureArg::ScopedLocal { .. } => {
                    return Err(Self::gap(
                        RustTargetGapSite::Function(function),
                        RustTargetGapKind::UnsupportedLambdaCapture,
                    ));
                }
                air::LambdaCaptureArg::ScopedBorrow { place } => {
                    let Some(borrow) = self.place_scoped_borrow(function, place) else {
                        return Err(Self::gap(
                            RustTargetGapSite::Function(function),
                            RustTargetGapKind::UnsupportedLambdaCapture,
                        ));
                    };
                    planned.push(RirLambdaCaptureArg::ScopedPlaceCell {
                        cell: self.scoped_place_cell_ref(function, borrow),
                    });
                }
                air::LambdaCaptureArg::CaptureCell { cell } => {
                    let cell_ref = self.capture_cell_ref(function, *cell);
                    match self.classify_capture_cell_storage(*cell) {
                        RirCellStorage::StackScoped => {
                            planned.push(RirLambdaCaptureArg::StackCell { cell: cell_ref });
                        }
                        RirCellStorage::Heap => {
                            planned.push(RirLambdaCaptureArg::HeapCell { cell: cell_ref });
                        }
                    }
                }
            }
        }
        Ok(PlannedLambdaCaptureArgs {
            stmts,
            captures: planned,
        })
    }

    fn air_place_value_readable(&mut self, place: &Place) -> bool {
        let position =
            if matches!(place.root, air::PlaceRoot::Global(_)) && place.projection.is_empty() {
                RustRecipePosition::Global
            } else {
                RustRecipePosition::Value
            };
        self.materializers.get(place.ty, position).is_some()
    }

    fn zero_env_function_rvalue(value: &RirRValue) -> Option<ZeroEnvFunctionValue> {
        let RirRValue::Lambda {
            lambda,
            captures,
            ty,
        } = value
        else {
            return None;
        };
        captures.is_empty().then_some(ZeroEnvFunctionValue {
            lambda: *lambda,
            ty: *ty,
        })
    }

    fn zero_env_function_place(
        &self,
        place: &Place,
        zero_env_function_values: &[Option<ZeroEnvFunctionValue>],
    ) -> Option<ZeroEnvFunctionValue> {
        if place.projection.is_empty()
            && matches!(self.air.type_arena.data(place.ty), TypeData::Function(_))
            && let Some(local) = place.root.local()
        {
            return zero_env_function_values
                .get(local.index())
                .copied()
                .flatten();
        }
        None
    }

    fn plan_use(
        &mut self,
        function: FunctionId,
        operand: &Operand,
        locals: &mut Vec<RirLocal>,
        zero_env_function_values: &[Option<ZeroEnvFunctionValue>],
    ) -> Result<PlannedRValue, RustTargetGap> {
        let Operand::Place(place) = operand else {
            return Ok(PlannedRValue::from_value(RirRValue::Use(
                self.plan_operand(function, operand),
            )));
        };
        if self.unsupported_function_capture_value(place) {
            return Err(Self::gap(
                RustTargetGapSite::Function(function),
                RustTargetGapKind::UnsupportedLambdaCapture,
            ));
        }
        let read_plan = self
            .access()
            .plan(function, PlaceAccessIntent::ReadValue, place)
            .map_err(|gap| Self::access_gap(function, gap))?;
        if let Some(value) = self.projected_sequence_read(function, place, &read_plan, locals)? {
            return Ok(value);
        }
        let physical = self.plan_place(function, &read_plan);
        let payload_binding = matches!(
            physical.root,
            RirPlaceRoot::Local(local)
                if locals.get(local.index()).is_some_and(|local| matches!(local.binding, RirLocalBinding::DirectPayload | RirLocalBinding::ScopedPlacePayload))
        );
        let guarded = read_plan.dataref_plan().is_some()
            || payload_binding
            || !matches!(
                read_plan.root,
                PlaceAccessRoot::Local {
                    source_mut_param: false,
                    ..
                } | PlaceAccessRoot::LambdaCapture(_)
            );
        if guarded || matches!(self.air.type_arena.data(place.ty), TypeData::Dyn(_)) {
            return self.plan_implicit_owned_value(function, place, locals);
        }
        if !self.air_place_value_readable(place) {
            if let Some(state) = self.zero_env_function_place(place, zero_env_function_values) {
                return Ok(PlannedRValue::from_value(state.rvalue()));
            }
            let reconstructable = matches!(
                self.air.type_arena.data(place.ty),
                TypeData::Aggregate(_)
                    | TypeData::Tuple(_)
                    | TypeData::Enum(_)
                    | TypeData::Array { .. }
            );
            if !reconstructable {
                return Err(Self::gap(
                    RustTargetGapSite::Function(function),
                    RustTargetGapKind::NonCopyValueRequired,
                ));
            }
        }
        if matches!(place.root, air::PlaceRoot::Global(_)) && place.projection.is_empty() {
            return Ok(PlannedRValue::from_value(RirRValue::Use(
                self.plan_operand(function, operand),
            )));
        }
        let TypeData::Aggregate(aggregate) = self.air.type_arena.data(place.ty) else {
            return Ok(PlannedRValue::from_value(RirRValue::Use(
                self.plan_operand(function, operand),
            )));
        };
        let fields = self.air.aggregate(*aggregate).fields.clone();
        let mut stmts = vec![];
        let mut planned_fields = vec![];
        for (index, field) in fields.iter().enumerate() {
            let mut field_place = place.clone();
            field_place
                .projection
                .push(air::Projection::Field(FieldId::from_index(index)));
            field_place.ty = field.ty;
            let planned = self.plan_air_owned_value(
                function,
                &air::OwnedValue::reusable(Operand::Place(field_place)),
                RustRecipePosition::StoredPayload(LambdaStorageFamily::StructField),
                locals,
            )?;
            stmts.extend(planned.stmts);
            planned_fields.push(planned.value);
        }
        Ok(PlannedRValue {
            stmts,
            value: RirRValue::Struct {
                ty: self.type_map[&place.ty],
                fields: planned_fields,
            },
        })
    }

    fn projected_sequence_read(
        &mut self,
        function: FunctionId,
        place: &Place,
        read_plan: &PlaceAccessPlan,
        locals: &mut Vec<RirLocal>,
    ) -> Result<Option<PlannedRValue>, RustTargetGap> {
        let Some(last) = read_plan.steps().last() else {
            return Ok(None);
        };
        let (place_model::ProjectionKind::ArrayIndex(_)
        | place_model::ProjectionKind::ListIndex(_)
        | place_model::ProjectionKind::SliceIndex(_)) = last.kind()
        else {
            return Ok(None);
        };
        let dynamic_element = matches!(self.air.type_arena.data(place.ty), TypeData::Dyn(_));
        if !dynamic_element
            && matches!(
                read_plan.root,
                PlaceAccessRoot::Local {
                    source_mut_param: false,
                    ..
                } | PlaceAccessRoot::Global(_)
            )
        {
            return Ok(None);
        }
        let mut root = place.clone();
        root.projection.pop();
        root.ty = last.source_ty();
        let collection = self.plan_collection_access(
            function,
            &root,
            CollectionAccessOp::SequenceSlotRead,
            locals,
        )?;
        Ok(Some(PlannedRValue {
            stmts: collection.stmts,
            value: RirRValue::SequenceSlotAt {
                collection: collection.access,
                step: self.rir_place_step(last),
            },
        }))
    }

    fn plan_aggregate(
        &mut self,
        function: FunctionId,
        kind: &AggregateCtor,
        fields: &[air::OwnedValue<Operand>],
        ty: TypeId,
        locals: &mut Vec<RirLocal>,
    ) -> Result<PlannedRValue, RustTargetGap> {
        if matches!(kind, AggregateCtor::Map) && !fields.len().is_multiple_of(2) {
            return Err(Self::gap(
                RustTargetGapSite::Function(function),
                RustTargetGapKind::UnsupportedRValue,
            ));
        }
        let mut stmts = vec![];
        let mut planned_fields = vec![];
        for (index, field) in fields.iter().enumerate() {
            let position = match kind {
                AggregateCtor::Struct(_) => {
                    RustRecipePosition::StoredPayload(LambdaStorageFamily::StructField)
                }
                AggregateCtor::DataRef(_) => {
                    RustRecipePosition::StoredPayload(LambdaStorageFamily::DataRefProjection)
                }
                AggregateCtor::EnumVariant { .. } => {
                    RustRecipePosition::StoredPayload(LambdaStorageFamily::EnumPayload)
                }
                AggregateCtor::Array | AggregateCtor::ArrayFill => {
                    RustRecipePosition::StoredPayload(LambdaStorageFamily::FixedArrayElement)
                }
                AggregateCtor::List => {
                    RustRecipePosition::StoredPayload(LambdaStorageFamily::ListElement)
                }
                AggregateCtor::Tuple => {
                    RustRecipePosition::StoredPayload(LambdaStorageFamily::TupleField)
                }
                AggregateCtor::Map if index.is_multiple_of(2) => RustRecipePosition::MapKey,
                AggregateCtor::Map => {
                    RustRecipePosition::StoredPayload(LambdaStorageFamily::MapValue)
                }
            };
            let planned = self.plan_air_owned_value(function, field, position, locals)?;
            stmts.extend(planned.stmts);
            planned_fields.push(planned.value);
        }
        let value = match kind {
            AggregateCtor::Struct(_) => RirRValue::Struct {
                ty: self.type_map[&ty],
                fields: planned_fields,
            },
            AggregateCtor::EnumVariant { variant, .. } => RirRValue::EnumVariant {
                ty: self.type_map[&ty],
                variant: RirVariantId::from_index(variant.index()),
                fields: planned_fields,
            },
            AggregateCtor::Array | AggregateCtor::ArrayFill => RirRValue::Array {
                ty: self.type_map[&ty],
                elems: planned_fields,
            },
            AggregateCtor::List => RirRValue::List {
                ty: self.type_map[&ty],
                elems: planned_fields,
            },
            AggregateCtor::Tuple => RirRValue::Tuple {
                ty: self.type_map[&ty],
                fields: planned_fields,
            },
            AggregateCtor::Map => RirRValue::Map {
                ty: self.type_map[&ty],
                entries: planned_fields
                    .chunks_exact(2)
                    .map(|entry| (entry[0].clone(), entry[1].clone()))
                    .collect(),
            },
            AggregateCtor::DataRef(_) => RirRValue::DataRefAlloc {
                ty: self.type_map[&ty],
                fields: planned_fields,
            },
        };
        Ok(PlannedRValue { stmts, value })
    }

    fn plan_call(
        &mut self,
        function_id: FunctionId,
        callee: &Callee,
        args: &[CallArg],
        locals: &mut Vec<RirLocal>,
    ) -> Result<PlannedRValue, RustTargetGap> {
        if let Callee::Extern(id) = callee {
            return self.plan_native_call(function_id, *id, args, locals);
        }

        let (target, ty, callee_stmts, expected_args) = match callee {
            Callee::Function(id) => {
                let function = self.air.function(*id);
                (
                    RirCallTarget::Function(self.function_map[id]),
                    self.type_map[&function.signature.return_type()],
                    vec![],
                    function
                        .signature
                        .params
                        .iter()
                        .map(|param| rir::source_pass_mode(self.air, param.ty, param.mode))
                        .collect::<Vec<_>>(),
                )
            }
            Callee::Extern(_) => unreachable!("extern calls are planned by plan_native_call"),
            Callee::Lambda(operand) => {
                let air_ty = self.operand_ty(operand);
                let TypeData::Function(sig_ty) = self.air.type_arena.data(air_ty) else {
                    return Err(Self::gap(
                        RustTargetGapSite::Function(function_id),
                        RustTargetGapKind::UnsupportedLambdaCall,
                    ));
                };
                let callee = self.plan_operand_read(function_id, operand, locals)?;
                let sig = self.lambda_sig_map[&air_ty];
                (
                    RirCallTarget::LambdaValue {
                        callee: callee.operand,
                        sig,
                    },
                    self.type_map[&sig_ty.ret.ty()],
                    callee.stmts,
                    sig_ty
                        .params
                        .iter()
                        .map(|param| rir::source_pass_mode(self.air, param.ty, param.mode))
                        .collect::<Vec<_>>(),
                )
            }
        };
        let mut stmts = callee_stmts;
        let mut planned_args = vec![];
        if args.len() != expected_args.len() {
            return Err(Self::gap(
                RustTargetGapSite::Function(function_id),
                RustTargetGapKind::UnsupportedCallArgMode,
            ));
        }
        for (arg, expected) in args.iter().zip(expected_args) {
            let planned = self.plan_arg(function_id, arg, expected, locals)?;
            stmts.extend(planned.stmts);
            planned_args.push(planned.arg);
        }
        Ok(PlannedRValue {
            stmts,
            value: RirRValue::Call {
                callee: target,
                args: planned_args,
                ty,
            },
        })
    }

    fn plan_native_call(
        &mut self,
        function_id: FunctionId,
        id: ExternId,
        args: &[CallArg],
        locals: &mut Vec<RirLocal>,
    ) -> Result<PlannedRValue, RustTargetGap> {
        let ext_index = self.extern_map[&id].index();
        if args.len() != self.externs[ext_index].params.len() {
            return Err(Self::gap(
                RustTargetGapSite::Function(function_id),
                RustTargetGapKind::UnsupportedCallArgMode,
            ));
        }

        let mut stmts = vec![];
        let mut planned_args = vec![];
        for (index, arg) in args.iter().enumerate() {
            let mode = self.externs[ext_index].params[index].mode;
            let planned = self.plan_arg(function_id, arg, mode, locals)?;
            if self.externs[ext_index].rejects_reentry_arg(index, &planned.arg) {
                return Err(Self::gap(
                    RustTargetGapSite::Function(function_id),
                    RustTargetGapKind::UnsupportedCallArgMode,
                ));
            }
            stmts.extend(planned.stmts);
            planned_args.push(planned.arg);
        }

        Ok(PlannedRValue {
            stmts,
            value: RirRValue::Call {
                callee: RirCallTarget::Extern(self.extern_map[&id]),
                args: planned_args,
                ty: self.externs[ext_index].ret,
            },
        })
    }

    fn plan_collection_slot_scope(
        &mut self,
        function: FunctionId,
        scope: &air::AirCollectionSlotScope,
        locals: &mut Vec<RirLocal>,
        zero_env_function_values: &mut Vec<Option<ZeroEnvFunctionValue>>,
        initialized_cells: &mut [bool],
        possible_cells: &mut [bool],
        in_loop: bool,
    ) -> Result<Vec<RirStmt>, RustTargetGap> {
        let access = self.collection_slot_access(function, scope, locals)?;
        let body = self.plan_air_block(
            function,
            &scope.body,
            locals,
            zero_env_function_values,
            initialized_cells,
            possible_cells,
            in_loop,
        )?;
        let (body, _) = self.collection_slot_block(function, scope, &access, body, true)?;
        Ok(vec![RirStmt::CollectionSlotScope(body)])
    }

    fn collection_slot_block(
        &self,
        function: FunctionId,
        scope: &air::AirCollectionSlotScope,
        access: &RirCollectionAccess,
        mut body: RirStructuredBlock,
        init: bool,
    ) -> Result<(RirStructuredBlock, bool), RustTargetGap> {
        let mut stmts = if init {
            let mut stmts = self.collection_slot_reads(function, scope, access, true);
            stmts.extend(self.collection_slot_scoped_place_cell_inits(function, scope));
            stmts
        } else {
            vec![]
        };
        let mut first = init;
        let mut scoped_cells_initialized = init;
        let mut block_updates_slot = false;
        for stmt in std::mem::take(&mut body.stmts) {
            if first {
                first = false;
            } else {
                stmts.extend(self.collection_slot_reads(function, scope, access, false));
                if !scoped_cells_initialized {
                    stmts.extend(self.collection_slot_scoped_place_cell_inits(function, scope));
                    scoped_cells_initialized = true;
                }
            }
            let (stmt, stmt_updates_slot) =
                self.collection_slot_stmt(function, scope, access, stmt)?;
            block_updates_slot = stmt_updates_slot;
            stmts.push(stmt);
            if stmt_updates_slot {
                stmts.extend(self.collection_slot_writes(function, scope, access));
            }
        }
        body.stmts = stmts;
        Ok((body, block_updates_slot))
    }

    fn collection_slot_stmt(
        &self,
        function: FunctionId,
        scope: &air::AirCollectionSlotScope,
        access: &RirCollectionAccess,
        stmt: RirStmt,
    ) -> Result<(RirStmt, bool), RustTargetGap> {
        Ok(match stmt {
            RirStmt::If(mut branch) => {
                let (then_block, mut updates_slot) =
                    self.collection_slot_block(function, scope, access, branch.then_block, false)?;
                branch.then_block = then_block;
                if let Some(block) = branch.else_block {
                    let (block, else_updates_slot) =
                        self.collection_slot_block(function, scope, access, block, false)?;
                    updates_slot |= else_updates_slot;
                    branch.else_block = Some(block);
                }
                (RirStmt::If(branch), updates_slot)
            }
            RirStmt::Loop(mut loop_) => {
                let (body, updates_slot) =
                    self.collection_slot_block(function, scope, access, loop_.body, false)?;
                loop_.body = body;
                (RirStmt::Loop(loop_), updates_slot)
            }
            RirStmt::RangeFor(mut range) => {
                let (body, updates_slot) =
                    self.collection_slot_block(function, scope, access, range.body, false)?;
                range.body = body;
                (RirStmt::RangeFor(range), updates_slot)
            }
            RirStmt::CollectionFor(mut for_) => {
                let (body, updates_slot) =
                    self.collection_slot_block(function, scope, access, for_.body, false)?;
                for_.body = body;
                (RirStmt::CollectionFor(for_), updates_slot)
            }
            RirStmt::CollectionLoanScope(mut loan) => {
                let (body, updates_slot) =
                    self.collection_slot_block(function, scope, access, loan.body, false)?;
                loan.body = body;
                (RirStmt::CollectionLoanScope(loan), updates_slot)
            }
            RirStmt::CollectionSlotScope(block) => {
                let (block, updates_slot) =
                    self.collection_slot_block(function, scope, access, block, false)?;
                (RirStmt::CollectionSlotScope(block), updates_slot)
            }
            RirStmt::OptionMatch(mut match_) => {
                let (some_block, some_updates_slot) =
                    self.collection_slot_block(function, scope, access, match_.some_block, false)?;
                let (none_block, none_updates_slot) =
                    self.collection_slot_block(function, scope, access, match_.none_block, false)?;
                match_.some_block = some_block;
                match_.none_block = none_block;
                (
                    RirStmt::OptionMatch(match_),
                    some_updates_slot || none_updates_slot,
                )
            }
            RirStmt::MapEntryMatch(mut match_) => {
                let (some_block, some_updates_slot) =
                    self.collection_slot_block(function, scope, access, match_.some_block, false)?;
                let (none_block, none_updates_slot) =
                    self.collection_slot_block(function, scope, access, match_.none_block, false)?;
                match_.some_block = some_block;
                match_.none_block = none_block;
                (
                    RirStmt::MapEntryMatch(match_),
                    some_updates_slot || none_updates_slot,
                )
            }
            stmt => {
                let updates_slot = self.direct_stmt_updates_collection_slot(function, scope, &stmt);
                (stmt, updates_slot)
            }
        })
    }

    fn direct_stmt_updates_collection_slot(
        &self,
        function: FunctionId,
        scope: &air::AirCollectionSlotScope,
        stmt: &RirStmt,
    ) -> bool {
        match stmt {
            RirStmt::Assign { dst, .. } => Self::place_is_collection_slot(scope, dst),
            RirStmt::MutPlaceSet { place, value } => {
                Self::mut_place_arg_is_collection_slot_update(scope, place)
                    || self.rvalue_updates_collection_slot(function, scope, value)
            }
            RirStmt::ScopedPlaceCellSet { cell: _, value } => {
                !scope.slots.is_empty()
                    || self.rvalue_updates_collection_slot(function, scope, value)
            }
            RirStmt::Eval(value)
            | RirStmt::Init { value, .. }
            | RirStmt::CellInit { value, .. }
            | RirStmt::CellSet { value, .. } => {
                self.rvalue_updates_collection_slot(function, scope, value)
            }
            _ => false,
        }
    }

    fn rvalue_updates_collection_slot(
        &self,
        function: FunctionId,
        scope: &air::AirCollectionSlotScope,
        value: &RirRValue,
    ) -> bool {
        match value {
            RirRValue::Call { callee, args, .. } => {
                let active_for_ref_cell = !self
                    .collection_slot_scoped_place_cell_inits(function, scope)
                    .is_empty();
                self.call_target_updates_collection_slot(function, scope, callee)
                    || args
                        .iter()
                        .any(|arg| Self::call_arg_updates_collection_slot(scope, arg))
                    || active_for_ref_cell && args.iter().any(Self::call_arg_may_invoke_lambda)
            }
            _ => false,
        }
    }

    fn call_target_updates_collection_slot(
        &self,
        function: FunctionId,
        scope: &air::AirCollectionSlotScope,
        callee: &RirCallTarget,
    ) -> bool {
        matches!(callee, RirCallTarget::LambdaValue { .. })
            && !self
                .collection_slot_scoped_place_cell_inits(function, scope)
                .is_empty()
    }

    fn call_arg_may_invoke_lambda(arg: &RirCallArg) -> bool {
        matches!(
            arg,
            RirCallArg::ScopedLambda { .. }
                | RirCallArg::EscapingLambda { .. }
                | RirCallArg::AnvCallback { .. }
                | RirCallArg::Value(_)
        )
    }

    fn call_arg_updates_collection_slot(
        scope: &air::AirCollectionSlotScope,
        arg: &RirCallArg,
    ) -> bool {
        match arg {
            RirCallArg::MutBorrow(place) => Self::place_is_collection_slot(scope, place),
            RirCallArg::MutPlace(arg) => Self::mut_place_arg_is_collection_slot_update(scope, arg),
            _ => false,
        }
    }

    fn mut_place_arg_is_collection_slot_update(
        scope: &air::AirCollectionSlotScope,
        arg: &RirMutPlaceArg,
    ) -> bool {
        match &arg.access {
            RirMutPlaceAccess::Handle(RirMutPlaceHandle::Local { local }) => {
                Self::local_is_collection_slot(scope, *local)
            }
            RirMutPlaceAccess::Handle(RirMutPlaceHandle::ScopedPlaceCell { .. }) => true,
            _ => false,
        }
    }

    fn place_is_collection_slot(scope: &air::AirCollectionSlotScope, place: &RirPlace) -> bool {
        let RirPlaceRoot::Local(local) = place.root else {
            unreachable!("expected a local RIR place")
        };
        Self::local_is_collection_slot(scope, local)
    }

    fn local_is_collection_slot(scope: &air::AirCollectionSlotScope, local: RirLocalId) -> bool {
        scope
            .slots
            .iter()
            .any(|slot| RirLocalId::from_index(slot.local.index()) == local)
    }

    fn structural_collection_access(
        &mut self,
        function: FunctionId,
        root: &Place,
        locals: &mut Vec<RirLocal>,
    ) -> Result<PlannedCollectionAccess, RustTargetGap> {
        self.plan_collection_access(
            function,
            root,
            CollectionAccessOp::StructuralMutation,
            locals,
        )
    }

    fn collection_slot_access(
        &mut self,
        function: FunctionId,
        scope: &air::AirCollectionSlotScope,
        locals: &mut Vec<RirLocal>,
    ) -> Result<RirCollectionAccess, RustTargetGap> {
        let op = CollectionAccessOp::slot(&scope.slots);
        Ok(self
            .plan_collection_access(function, &scope.root, op, locals)?
            .access)
    }

    fn plan_collection_access(
        &mut self,
        function: FunctionId,
        root: &Place,
        op: CollectionAccessOp,
        locals: &mut Vec<RirLocal>,
    ) -> Result<PlannedCollectionAccess, RustTargetGap> {
        let plan = self
            .access()
            .plan(function, op.intent(), root)
            .map_err(|gap| Self::access_gap(function, gap))?;
        self.plan_collection_access_from_plan(
            function,
            RustTargetGapSite::Function(function),
            &plan,
            op,
            locals,
        )
    }

    fn plan_collection_access_from_plan(
        &mut self,
        function: FunctionId,
        site: RustTargetGapSite,
        plan: &PlaceAccessPlan,
        op: CollectionAccessOp,
        locals: &mut Vec<RirLocal>,
    ) -> Result<PlannedCollectionAccess, RustTargetGap> {
        if plan.dataref_plan().is_some() {
            let planned = self.plan_mut_place_arg(function, plan, locals)?;
            return Ok(PlannedCollectionAccess {
                stmts: planned.stmts,
                access: RirCollectionAccess::MutPlace(planned.arg),
            });
        }
        if plan
            .steps()
            .iter()
            .any(|step| matches!(step.kind(), place_model::ProjectionKind::DataRefField(_)))
        {
            return Err(Self::gap(
                site,
                RustTargetGapKind::UnsupportedMutablePlaceDataRef,
            ));
        }
        let payload_binding = match plan.root {
            PlaceAccessRoot::Local { local, .. } => {
                locals.get(local.index()).is_some_and(|local| {
                    matches!(
                        local.binding,
                        RirLocalBinding::DirectPayload | RirLocalBinding::ScopedPlacePayload
                    )
                })
            }
            _ => false,
        };
        let dynamic_collection_projection = plan.steps().iter().any(|step| {
            matches!(
                step.kind(),
                place_model::ProjectionKind::ListIndex(_)
                    | place_model::ProjectionKind::SliceIndex(_)
            )
        });
        let global_slice_view = matches!(op, CollectionAccessOp::SliceView)
            && matches!(plan.root, PlaceAccessRoot::Global(_));
        let use_mut_place = payload_binding
            || dynamic_collection_projection
            || global_slice_view
            || matches!(
                plan.root,
                PlaceAccessRoot::Local {
                    source_mut_param: true,
                    ..
                } | PlaceAccessRoot::CaptureCell(_)
                    | PlaceAccessRoot::ScopedPlaceCell(_)
            );
        if use_mut_place {
            let planned = self.plan_mut_place_arg(function, plan, locals)?;
            return Ok(PlannedCollectionAccess {
                stmts: planned.stmts,
                access: RirCollectionAccess::MutPlace(planned.arg),
            });
        }
        let access = match plan.root {
            PlaceAccessRoot::Local {
                local,
                source_mut_param: false,
            } => RirCollectionAccess::Direct(RirPlace {
                root: RirPlaceRoot::Local(RirLocalId::from_index(local.index())),
                projections: self.rir_place_steps(plan, 0..plan.steps().len()),
            }),
            PlaceAccessRoot::Global(global) => RirCollectionAccess::Direct(RirPlace {
                root: RirPlaceRoot::Global(self.global_map[&global]),
                projections: self.rir_place_steps(plan, 0..plan.steps().len()),
            }),
            PlaceAccessRoot::LambdaCapture(slot) => {
                let air::FunctionKind::Lambda(lambda) = self.air.function(function).kind else {
                    unreachable!("AIR verifier rejects capture roots outside lambdas")
                };
                let runtime = self.lambda_runtime_capture_slots[&(lambda, slot)];
                RirCollectionAccess::Direct(RirPlace {
                    root: RirPlaceRoot::Local(RirLocalId::from_index(
                        self.air.function(function).locals.len() + runtime,
                    )),
                    projections: self.rir_place_steps(plan, 0..plan.steps().len()),
                })
            }
            PlaceAccessRoot::Local {
                source_mut_param: true,
                ..
            }
            | PlaceAccessRoot::CaptureCell(_)
            | PlaceAccessRoot::ScopedPlaceCell(_) => unreachable!("mut-place roots handled above"),
        };
        Ok(PlannedCollectionAccess {
            stmts: vec![],
            access,
        })
    }

    fn collection_slot_scoped_place_cell_inits(
        &self,
        function: FunctionId,
        scope: &air::AirCollectionSlotScope,
    ) -> Vec<RirStmt> {
        self.air
            .scoped_borrows
            .iter()
            .enumerate()
            .filter_map(|(index, borrow)| {
                let air::ScopedBorrowSource::ForRefAlias { source } = &borrow.source else {
                    return None;
                };
                let air::PlaceRoot::Local(local) = source.root else {
                    return None;
                };
                if borrow.owner != function || !scope.slots.iter().any(|slot| slot.local == local) {
                    return None;
                }
                Some(RirStmt::ScopedPlaceCellInit {
                    cell: self.scoped_place_cell_map[&air::ScopedBorrowId::from_index(index)],
                })
            })
            .collect()
    }

    fn collection_slot_reads(
        &self,
        function: FunctionId,
        scope: &air::AirCollectionSlotScope,
        access: &RirCollectionAccess,
        init: bool,
    ) -> Vec<RirStmt> {
        scope
            .slots
            .iter()
            .map(|slot| {
                let local = RirLocalId::from_index(slot.local.index());
                let value = self.collection_slot_read(function, scope, access, slot);
                if init {
                    RirStmt::Init { local, value }
                } else {
                    RirStmt::Assign {
                        dst: RirPlace::local(local, vec![]),
                        value,
                    }
                }
            })
            .collect()
    }

    fn collection_slot_writes(
        &self,
        function: FunctionId,
        scope: &air::AirCollectionSlotScope,
        access: &RirCollectionAccess,
    ) -> Vec<RirStmt> {
        scope
            .slots
            .iter()
            .filter(|slot| slot.mutable)
            .map(|slot| self.collection_slot_write(function, scope, access, slot))
            .collect()
    }

    fn collection_slot_read(
        &self,
        function: FunctionId,
        scope: &air::AirCollectionSlotScope,
        access: &RirCollectionAccess,
        slot: &air::AirCollectionSlot,
    ) -> RirRValue {
        match slot.kind {
            air::AirCollectionSlotKind::SequenceElement => RirRValue::SequenceSlotAt {
                collection: access.clone(),
                step: self.collection_slot_step(function, scope, slot),
            },
            air::AirCollectionSlotKind::MapValue => RirRValue::MapValueAt {
                map: access.clone(),
                index: RirLocalId::from_index(scope.index.index()),
                ty: self.type_map[&slot.ty],
            },
        }
    }

    fn collection_slot_write(
        &self,
        function: FunctionId,
        scope: &air::AirCollectionSlotScope,
        access: &RirCollectionAccess,
        slot: &air::AirCollectionSlot,
    ) -> RirStmt {
        let local = RirLocalId::from_index(slot.local.index());
        let value = RirOperand::Place(RirPlace::local(local, vec![]));
        match slot.kind {
            air::AirCollectionSlotKind::SequenceElement => RirStmt::SequenceSlotSet {
                collection: access.clone(),
                step: self.collection_slot_step(function, scope, slot),
                value,
            },
            air::AirCollectionSlotKind::MapValue => RirStmt::MapValueSet {
                map: access.clone(),
                index: RirLocalId::from_index(scope.index.index()),
                value,
            },
        }
    }

    fn unsupported_function_capture_value(&self, place: &Place) -> bool {
        self.air_policy()
            .scoped_function_value_gap(place.ty, place.root)
            .is_some()
    }

    fn plan_dyn_borrow_arg(
        &mut self,
        function: FunctionId,
        borrow: &air::DynBorrow,
        locals: &mut Vec<RirLocal>,
    ) -> Result<PlannedCallArg, RustTargetGap> {
        let (stmts, source) = match &borrow.source {
            air::DynBorrowSource::Concrete { place, witness } => {
                let plan = self
                    .access()
                    .plan(function, PlaceAccessIntent::MutPlaceArg, place)
                    .map_err(|gap| Self::access_gap(function, gap))?;
                let planned = self.plan_mut_place_arg(function, &plan, locals)?;
                (
                    planned.stmts,
                    rir::RirDynBorrowSource::Concrete {
                        place: planned.arg,
                        variant: self.dyn_variant_map[witness],
                    },
                )
            }
            air::DynBorrowSource::Owned(place) => {
                if let Some(reborrow) = self.dyn_borrow_for_place(place) {
                    (vec![], reborrow.source)
                } else {
                    let plan = self
                        .access()
                        .plan(function, PlaceAccessIntent::MutPlaceArg, place)
                        .map_err(|gap| Self::access_gap(function, gap))?;
                    let planned = self.plan_mut_place_arg(function, &plan, locals)?;
                    let TypeData::Dyn(surface) = self.air.type_arena.data(place.ty) else {
                        unreachable!("AIR verifies owned dynamic borrow source")
                    };
                    (
                        planned.stmts,
                        rir::RirDynBorrowSource::Owned {
                            place: planned.arg,
                            carrier: self.dyn_surface_map[surface],
                        },
                    )
                }
            }
            air::DynBorrowSource::Borrowed(place) => {
                let air::PlaceRoot::DynBorrowParam(id) = place.root else {
                    unreachable!("AIR verifies borrowed dynamic source root")
                };
                let decl = &self.air.dyn_borrow_params[id.index()];
                (
                    vec![],
                    rir::RirDynBorrowSource::Borrowed {
                        local: RirLocalId::from_index(decl.source.index()),
                        carrier: self.dyn_surface_map[&decl.surface],
                    },
                )
            }
        };
        Ok(PlannedCallArg {
            stmts,
            arg: RirCallArg::DynBorrow(rir::RirDynBorrow {
                source,
                target: self.dyn_surface_map[&borrow.surface],
                weakening: borrow.weakening.map(|id| self.dyn_weakening_map[&id]),
            }),
        })
    }

    fn adapt_dyn_arg(&mut self, param: RirDynDispatchParam, arg: RirCallArg) -> RirCallArg {
        if arg.mode() == param.mode {
            return arg;
        }
        let owned = match arg {
            RirCallArg::Value(owned) => owned,
            RirCallArg::SharedBorrow(place) => RirOwnedValue {
                value: RirOwnedOperand::Value(RirOperand::Place(place)),
                source: RirOwnedSource::Reuse(
                    self.materializers
                        .get(
                            TypeId::from_index(param.ty.index()),
                            RustRecipePosition::Value,
                        )
                        .expect("verified dynamic argument materializer"),
                ),
            },
            arg => unreachable!("verified dynamic argument adaptation: {arg:?}"),
        };
        match param.mode {
            RirPassMode::Value => RirCallArg::Value(owned),
            RirPassMode::SharedBorrow => match owned {
                RirOwnedValue {
                    value: RirOwnedOperand::Value(RirOperand::Place(place)),
                    source: RirOwnedSource::Reuse(_),
                } => RirCallArg::SharedBorrow(place),
                _ => unreachable!("verified dynamic borrow adaptation"),
            },
            RirPassMode::ScopedLambda | RirPassMode::EscapingLambda | RirPassMode::AnvCallback => {
                let sig = self.lambda_sig_map[&TypeId::from_index(param.ty.index())];
                match param.mode {
                    RirPassMode::ScopedLambda => RirCallArg::ScopedLambda { callee: owned, sig },
                    RirPassMode::EscapingLambda => {
                        RirCallArg::EscapingLambda { callee: owned, sig }
                    }
                    RirPassMode::AnvCallback => RirCallArg::AnvCallback { callee: owned, sig },
                    _ => unreachable!(),
                }
            }
            _ => unreachable!("verified dynamic argument mode"),
        }
    }

    fn plan_arg(
        &mut self,
        function: FunctionId,
        arg: &CallArg,
        expected: RirPassMode,
        locals: &mut Vec<RirLocal>,
    ) -> Result<PlannedCallArg, RustTargetGap> {
        match arg {
            CallArg::DynBorrow(borrow) => self.plan_dyn_borrow_arg(function, borrow, locals),
            CallArg::Value(owned)
                if matches!(
                    expected,
                    RirPassMode::ScopedLambda
                        | RirPassMode::EscapingLambda
                        | RirPassMode::AnvCallback
                ) =>
            {
                let air_ty = self.operand_ty(&owned.value);
                let TypeData::Function(_) = self.air.type_arena.data(air_ty) else {
                    return Err(Self::gap(
                        RustTargetGapSite::Function(function),
                        RustTargetGapKind::UnsupportedLambdaExternBoundary,
                    ));
                };
                if let Operand::Place(place) = &owned.value
                    && self.unsupported_function_capture_value(place)
                {
                    return Err(Self::gap(
                        RustTargetGapSite::Function(function),
                        RustTargetGapKind::UnsupportedLambdaCapture,
                    ));
                }
                let planned =
                    self.plan_air_owned_value(function, owned, RustRecipePosition::Value, locals)?;
                let sig = self.lambda_sig_map[&air_ty];
                let arg = match expected {
                    RirPassMode::ScopedLambda => RirCallArg::ScopedLambda {
                        callee: planned.value,
                        sig,
                    },
                    RirPassMode::EscapingLambda => RirCallArg::EscapingLambda {
                        callee: planned.value,
                        sig,
                    },
                    RirPassMode::AnvCallback => RirCallArg::AnvCallback {
                        callee: planned.value,
                        sig,
                    },
                    _ => unreachable!("checked expected callback mode"),
                };
                Ok(PlannedCallArg {
                    stmts: planned.stmts,
                    arg,
                })
            }
            CallArg::Value(owned) | CallArg::InitFieldProvided(owned) => {
                let init_field = matches!(arg, CallArg::InitFieldProvided(_));
                if let Operand::Place(place) = &owned.value
                    && self.unsupported_function_capture_value(place)
                {
                    return Err(Self::gap(
                        RustTargetGapSite::Function(function),
                        RustTargetGapKind::UnsupportedLambdaCapture,
                    ));
                }
                let planned =
                    self.plan_air_owned_value(function, owned, RustRecipePosition::Value, locals)?;
                Ok(PlannedCallArg {
                    stmts: planned.stmts,
                    arg: if init_field {
                        RirCallArg::InitFieldProvided(planned.value)
                    } else {
                        RirCallArg::Value(planned.value)
                    },
                })
            }
            CallArg::InitFieldOmitted => Ok(PlannedCallArg::from_arg(RirCallArg::InitFieldOmitted)),
            CallArg::SharedBorrow(place) if expected == RirPassMode::Value => {
                let owned = air::OwnedValue::reusable(Operand::Place(place.clone()));
                let planned =
                    self.plan_air_owned_value(function, &owned, RustRecipePosition::Value, locals)?;
                Ok(PlannedCallArg {
                    stmts: planned.stmts,
                    arg: RirCallArg::Value(planned.value),
                })
            }
            CallArg::SharedBorrow(place) => {
                let planned = self.lower_place_read(function, place, locals)?;
                let RirOperand::Place(place) = planned.operand else {
                    unreachable!("place read returns a place operand")
                };
                Ok(PlannedCallArg {
                    stmts: planned.stmts,
                    arg: RirCallArg::SharedBorrow(place),
                })
            }
            CallArg::SharedStringConst(id) => {
                let ConstValue::String(text) = &self.air.const_arena.get(*id).value else {
                    unreachable!("shared string argument must reference a string constant")
                };
                Ok(PlannedCallArg::from_arg(RirCallArg::SharedStringConst(
                    self.string_literal_map[text.as_ref()],
                )))
            }
            CallArg::MutBorrow(place) if expected == RirPassMode::MutPlace => {
                self.plan_source_mut_place_arg(function, place, locals)
            }
            CallArg::MutBorrow(place) => self.plan_native_mut_borrow_arg(function, place),
        }
    }

    fn reusable_materializer(
        &mut self,
        function: FunctionId,
        ty: TypeId,
        position: RustRecipePosition,
    ) -> Result<rir::RirMaterializerId, RustTargetGap> {
        self.materializers.get(ty, position).ok_or_else(|| {
            Self::gap(
                RustTargetGapSite::Function(function),
                RustTargetGapKind::NonCopyValueRequired,
            )
        })
    }

    fn plan_air_owned_value(
        &mut self,
        function: FunctionId,
        owned: &air::OwnedValue<Operand>,
        position: RustRecipePosition,
        locals: &mut Vec<RirLocal>,
    ) -> Result<PlannedOwnedValue, RustTargetGap> {
        let source = match owned.source {
            air::ValueSource::Reusable => {
                let ty = self
                    .air
                    .operand_ty(&owned.value)
                    .expect("verified owned operand has type");
                RirOwnedSource::Reuse(self.reusable_materializer(function, ty, position)?)
            }
            air::ValueSource::TransferTemp { local } => RirOwnedSource::Transfer {
                local: RirLocalId::from_index(local.index()),
            },
        };
        let planned = match owned.source {
            air::ValueSource::Reusable => {
                self.plan_owned_operand(function, &owned.value, locals)?
            }
            air::ValueSource::TransferTemp { .. } => PlannedOwnedOperand {
                stmts: vec![],
                operand: RirOwnedOperand::Value(self.plan_operand(function, &owned.value)),
            },
        };
        Ok(PlannedOwnedValue {
            stmts: planned.stmts,
            value: RirOwnedValue {
                value: planned.operand,
                source,
            },
        })
    }

    fn dyn_borrow_for_place(&self, place: &Place) -> Option<rir::RirDynBorrow> {
        if !place.projection.is_empty() {
            return None;
        }
        let (source, carrier) = match place.root {
            air::PlaceRoot::DynBorrowParam(id) => {
                let decl = &self.air.dyn_borrow_params[id.index()];
                let carrier = self.dyn_surface_map[&decl.surface];
                (
                    rir::RirDynBorrowSource::Borrowed {
                        local: RirLocalId::from_index(decl.source.index()),
                        carrier,
                    },
                    carrier,
                )
            }
            air::PlaceRoot::Local(local) => {
                let carrier = self
                    .dyn_reborrows
                    .iter()
                    .rev()
                    .find_map(|(binding, carrier)| (*binding == local).then_some(*carrier))?;
                (
                    rir::RirDynBorrowSource::Reborrowed {
                        local: RirLocalId::from_index(local.index()),
                        carrier,
                    },
                    carrier,
                )
            }
            _ => return None,
        };
        Some(rir::RirDynBorrow {
            source,
            target: carrier,
            weakening: None,
        })
    }

    fn plan_owned_operand(
        &mut self,
        function: FunctionId,
        operand: &Operand,
        locals: &mut Vec<RirLocal>,
    ) -> Result<PlannedOwnedOperand, RustTargetGap> {
        let Operand::Place(place) = operand else {
            return Ok(PlannedOwnedOperand {
                stmts: vec![],
                operand: RirOwnedOperand::Value(self.plan_operand(function, operand)),
            });
        };
        if let Some(borrow) = self.dyn_borrow_for_place(place) {
            return Ok(PlannedOwnedOperand {
                stmts: vec![],
                operand: RirOwnedOperand::DynBorrow(borrow),
            });
        }
        let plan = self
            .access()
            .plan(function, PlaceAccessIntent::OwnedRead, place)
            .map_err(|gap| Self::access_gap(function, gap))?;
        if plan.dataref_plan().is_some() {
            let planned = self.plan_mut_place_arg(function, &plan, locals)?;
            return Ok(PlannedOwnedOperand {
                stmts: planned.stmts,
                operand: RirOwnedOperand::Access(planned.arg),
            });
        }
        if let Some(cell_id) = self.place_capture_cell(function, place) {
            let cell = self.capture_cell_ref(function, cell_id);
            let handle = match self.classify_capture_cell_storage(cell_id) {
                RirCellStorage::StackScoped => RirMutPlaceHandle::StackCell { cell },
                RirCellStorage::Heap => RirMutPlaceHandle::HeapCell { cell },
            };
            return Ok(PlannedOwnedOperand {
                stmts: vec![],
                operand: RirOwnedOperand::Access(RirMutPlaceArg::from_handle(
                    handle,
                    self.rir_place_steps(&plan, 0..plan.steps().len()),
                )),
            });
        }
        if let Some(borrow) = self.place_scoped_borrow(function, place) {
            let handle = RirMutPlaceHandle::ScopedPlaceCell {
                cell: self.scoped_place_cell_ref(function, borrow),
            };
            return Ok(PlannedOwnedOperand {
                stmts: vec![],
                operand: RirOwnedOperand::Access(RirMutPlaceArg::from_handle(
                    handle,
                    self.rir_place_steps(&plan, 0..plan.steps().len()),
                )),
            });
        }
        let physical = self.plan_place(function, &plan);
        if let RirPlaceRoot::Local(local) = physical.root
            && locals.get(local.index()).is_some_and(|local| {
                matches!(
                    local.binding,
                    RirLocalBinding::DirectPayload | RirLocalBinding::ScopedPlacePayload
                )
            })
        {
            return Ok(PlannedOwnedOperand {
                stmts: vec![],
                operand: RirOwnedOperand::Access(RirMutPlaceArg::from_handle(
                    RirMutPlaceHandle::Local { local },
                    self.rir_place_steps(&plan, 0..plan.steps().len()),
                )),
            });
        }
        let direct = plan.dataref_plan().is_none()
            && matches!(
                plan.root,
                PlaceAccessRoot::Local {
                    source_mut_param: false,
                    ..
                } | PlaceAccessRoot::LambdaCapture(_)
            );
        if direct {
            return Ok(PlannedOwnedOperand {
                stmts: vec![],
                operand: RirOwnedOperand::Value(self.plan_place_operand(function, &plan)),
            });
        }
        let planned = self.plan_mut_place_arg(function, &plan, locals)?;
        Ok(PlannedOwnedOperand {
            stmts: planned.stmts,
            operand: RirOwnedOperand::Access(planned.arg),
        })
    }

    fn plan_operand_read(
        &mut self,
        function: FunctionId,
        operand: &Operand,
        locals: &mut Vec<RirLocal>,
    ) -> Result<PlannedOperand, RustTargetGap> {
        match operand {
            Operand::Place(place) => self.lower_place_read(function, place, locals),
            Operand::Const(id) => Ok(PlannedOperand::from_operand(RirOperand::Const(
                self.const_map[id],
            ))),
        }
    }

    fn plan_operands_read(
        &mut self,
        function: FunctionId,
        operands: &[Operand],
        locals: &mut Vec<RirLocal>,
    ) -> Result<PlannedOperands, RustTargetGap> {
        let mut stmts = vec![];
        let mut planned = vec![];
        for operand in operands {
            let next = self.plan_operand_read(function, operand, locals)?;
            stmts.extend(next.stmts);
            planned.push(next.operand);
        }
        Ok(PlannedOperands {
            stmts,
            operands: planned,
        })
    }

    fn plan_operand(&self, function: FunctionId, operand: &Operand) -> RirOperand {
        match operand {
            Operand::Place(place) => {
                let plan = self
                    .access()
                    .plan(function, PlaceAccessIntent::ReadValue, place)
                    .expect("AIR verification rejects unreadable operand place");
                RirOperand::Place(self.plan_place(function, &plan))
            }
            Operand::Const(id) => RirOperand::Const(self.const_map[id]),
        }
    }

    fn lower_place_read(
        &mut self,
        function: FunctionId,
        place: &Place,
        locals: &mut Vec<RirLocal>,
    ) -> Result<PlannedOperand, RustTargetGap> {
        let plan = self
            .access()
            .plan(function, PlaceAccessIntent::ReadValue, place)
            .map_err(|gap| Self::access_gap(function, gap))?;
        let payload_binding = matches!(
            plan.root,
            PlaceAccessRoot::Local { local, .. }
                if locals.get(local.index()).is_some_and(|local| matches!(local.binding, RirLocalBinding::DirectPayload | RirLocalBinding::ScopedPlacePayload))
        );
        let direct = plan.dataref_plan().is_none()
            && !payload_binding
            && matches!(
                plan.root,
                PlaceAccessRoot::Local {
                    source_mut_param: false,
                    ..
                } | PlaceAccessRoot::LambdaCapture(_)
            );
        if direct {
            return Ok(PlannedOperand::from_operand(RirOperand::Place(
                self.plan_place(function, &plan),
            )));
        }
        self.lower_implicit_owned_read(function, place, locals)
    }

    fn plan_implicit_owned_value(
        &mut self,
        function: FunctionId,
        place: &Place,
        locals: &mut Vec<RirLocal>,
    ) -> Result<PlannedRValue, RustTargetGap> {
        let planned = self.plan_owned_operand(function, &Operand::Place(place.clone()), locals)?;
        let materializer =
            self.reusable_materializer(function, place.ty, RustRecipePosition::Value)?;
        Ok(PlannedRValue {
            stmts: planned.stmts,
            value: RirRValue::Materialize(RirOwnedValue {
                value: planned.operand,
                source: RirOwnedSource::Reuse(materializer),
            }),
        })
    }

    fn lower_implicit_owned_read(
        &mut self,
        function: FunctionId,
        place: &Place,
        locals: &mut Vec<RirLocal>,
    ) -> Result<PlannedOperand, RustTargetGap> {
        let planned = self.plan_implicit_owned_value(function, place, locals)?;
        let mut stmts = planned.stmts;
        let operand = self.rvalue_temp(planned.value, place.ty, locals, &mut stmts);
        Ok(PlannedOperand { stmts, operand })
    }

    fn lower_collection_loan_root(
        &mut self,
        function: FunctionId,
        index: usize,
        loan: &air::AirCollectionLoan,
        locals: &mut Vec<RirLocal>,
    ) -> Result<PlannedCollectionLoanRoot, RustTargetGap> {
        let plan = self
            .access()
            .collection_loan_plan(function, loan)
            .map_err(|gap| Self::access_gap(function, gap))?;
        let site = RustTargetGapSite::Statement(function, index);
        let root = self.plan_collection_access_from_plan(
            function,
            site.clone(),
            &plan.place,
            CollectionAccessOp::ShapeLoan,
            locals,
        )?;
        if !root.stmts.is_empty() {
            return Err(Self::gap(
                site,
                RustTargetGapKind::UnsupportedMutablePlaceProjection,
            ));
        }
        Ok(PlannedCollectionLoanRoot {
            root: root.access,
            root_kind: rir_collection_root_kind(plan.root_kind),
            mode: rir_collection_loan_mode(plan.mode),
        })
    }

    fn assign_target(plan: &PlaceAccessPlan, place: &Place) -> AssignTarget {
        if plan.dataref_plan().is_some() {
            return AssignTarget::DataRef;
        }
        match plan.root {
            PlaceAccessRoot::CaptureCell(cell) if place.projection.is_empty() => {
                AssignTarget::CaptureCell(cell)
            }
            PlaceAccessRoot::ScopedPlaceCell(borrow) if place.projection.is_empty() => {
                AssignTarget::ScopedPlaceCell(borrow)
            }
            PlaceAccessRoot::CaptureCell(_) | PlaceAccessRoot::ScopedPlaceCell(_) => {
                AssignTarget::ProjectedMutPlace
            }
            PlaceAccessRoot::Global(global) if !place.projection.is_empty() => {
                AssignTarget::ProjectedGlobal(global)
            }
            PlaceAccessRoot::Local {
                source_mut_param, ..
            } => AssignTarget::Assign { source_mut_param },
            PlaceAccessRoot::Global(_) | PlaceAccessRoot::LambdaCapture(_) => {
                AssignTarget::Assign {
                    source_mut_param: false,
                }
            }
        }
    }

    fn lower_place_write(
        &mut self,
        function: FunctionId,
        place: &Place,
        value: RirRValue,
        locals: &mut Vec<RirLocal>,
        stmts: &mut Vec<RirStmt>,
        initialized_cells: &mut [bool],
        possible_cells: &mut [bool],
        in_loop: bool,
    ) -> Result<(), RustTargetGap> {
        let plan = self
            .access()
            .plan(function, PlaceAccessIntent::Assign, place)
            .map_err(|gap| Self::access_gap(function, gap))?;
        if let Some(local) = place.root.local()
            && let Some(root) = locals.get(local.index())
            && matches!(
                root.binding,
                RirLocalBinding::DirectPayload | RirLocalBinding::ScopedPlacePayload
            )
        {
            stmts.push(RirStmt::MutPlaceSet {
                place: RirMutPlaceArg::from_handle(
                    RirMutPlaceHandle::Local {
                        local: RirLocalId::from_index(local.index()),
                    },
                    self.rir_place_steps(&plan, 0..plan.steps().len()),
                ),
                value,
            });
            return Ok(());
        }
        match Self::assign_target(&plan, place) {
            AssignTarget::CaptureCell(cell) => {
                self.lower_capture_cell_write(
                    function,
                    cell,
                    place.ty,
                    value,
                    locals,
                    stmts,
                    initialized_cells,
                    possible_cells,
                    in_loop,
                )?;
            }
            AssignTarget::ScopedPlaceCell(borrow) => {
                let value =
                    self.rvalue_short_region_value(function, value, place.ty, locals, stmts);
                stmts.push(RirStmt::ScopedPlaceCellSet {
                    cell: self.scoped_place_cell_ref(function, borrow),
                    value,
                });
            }
            AssignTarget::ProjectedMutPlace => {
                let planned = self.plan_mut_place_arg(function, &plan, locals)?;
                stmts.extend(planned.stmts);
                stmts.push(RirStmt::MutPlaceSet {
                    place: planned.arg,
                    value,
                });
            }
            AssignTarget::ProjectedGlobal(global) => {
                stmts.push(RirStmt::MutPlaceSet {
                    place: RirMutPlaceArg::from_handle(
                        RirMutPlaceHandle::Global {
                            global: self.global_map[&global],
                        },
                        self.rir_place_steps(&plan, 0..plan.steps().len()),
                    ),
                    value,
                });
            }
            AssignTarget::Assign { source_mut_param } => {
                let value = if source_mut_param {
                    self.rvalue_short_region_value(function, value, place.ty, locals, stmts)
                } else {
                    value
                };
                stmts.push(RirStmt::Assign {
                    dst: self.plan_place(function, &plan),
                    value,
                });
            }
            AssignTarget::DataRef => {
                self.lower_dataref_write(function, &plan, place, value, locals, stmts)?;
            }
        }
        Ok(())
    }

    fn lower_capture_cell_write(
        &self,
        function: FunctionId,
        cell: air::CaptureCellId,
        ty: TypeId,
        value: RirRValue,
        locals: &mut Vec<RirLocal>,
        stmts: &mut Vec<RirStmt>,
        initialized_cells: &mut [bool],
        possible_cells: &mut [bool],
        in_loop: bool,
    ) -> Result<(), RustTargetGap> {
        let cell_ref = self.capture_cell_ref(function, cell);
        let loop_local = matches!(
            self.air.capture_cells[cell.index()].lifetime,
            air::CaptureCellLifetime::Loop { .. }
        );
        if initialized_cells
            .get(cell.index())
            .copied()
            .unwrap_or(false)
        {
            let value = self.rvalue_short_region_value(function, value, ty, locals, stmts);
            stmts.push(RirStmt::CellSet {
                cell: cell_ref,
                value,
            });
        } else if (!loop_local && in_loop)
            || possible_cells.get(cell.index()).copied().unwrap_or(false)
        {
            return Err(Self::gap(
                RustTargetGapSite::Function(function),
                RustTargetGapKind::UnsupportedLambdaCell,
            ));
        } else {
            stmts.push(RirStmt::CellInit {
                cell: cell_ref,
                value,
            });
            if let Some(slot) = initialized_cells.get_mut(cell.index()) {
                *slot = true;
            }
            if let Some(slot) = possible_cells.get_mut(cell.index()) {
                *slot = true;
            }
        }
        Ok(())
    }

    fn lower_dataref_write(
        &mut self,
        function: FunctionId,
        plan: &PlaceAccessPlan,
        place: &Place,
        value: RirRValue,
        locals: &mut Vec<RirLocal>,
        stmts: &mut Vec<RirStmt>,
    ) -> Result<(), RustTargetGap> {
        let segment = self.final_dataref_segment(function, plan, locals, stmts)?;
        let value = self.rvalue_short_region_value(function, value, place.ty, locals, stmts);
        let suffix = self.rir_place_steps(
            plan,
            plan.dataref_plan().expect("dataref plan").remaining.clone(),
        );
        stmts.push(RirStmt::DataRefSet {
            object: segment.object,
            place: segment.place,
            suffix,
            value,
        });
        Ok(())
    }

    fn dataref_runtime_root_place(
        &mut self,
        function: FunctionId,
        root: PlaceAccessRoot,
        ty: TypeId,
        locals: &mut Vec<RirLocal>,
        stmts: &mut Vec<RirStmt>,
    ) -> Result<(TypeId, RirPlace), RustTargetGap> {
        let source = match root {
            PlaceAccessRoot::CaptureCell(cell) => Some(air::PlaceRoot::CaptureCell(cell)),
            PlaceAccessRoot::ScopedPlaceCell(borrow) => Some(air::PlaceRoot::ScopedBorrow(borrow)),
            PlaceAccessRoot::Local {
                local,
                source_mut_param: true,
                ..
            } => Some(air::PlaceRoot::Local(local)),
            PlaceAccessRoot::Global(global) => Some(air::PlaceRoot::Global(global)),
            PlaceAccessRoot::Local { local, .. } => {
                return Ok((
                    ty,
                    Self::rir_root_place(RirLocalId::from_index(local.index())),
                ));
            }
            PlaceAccessRoot::LambdaCapture(slot) => {
                let air::FunctionKind::Lambda(lambda) = self.air.function(function).kind else {
                    unreachable!("AIR verifier rejects capture roots outside lambdas")
                };
                let runtime = self.lambda_runtime_capture_slots[&(lambda, slot)];
                return Ok((
                    ty,
                    Self::rir_root_place(RirLocalId::from_index(
                        self.air.function(function).locals.len() + runtime,
                    )),
                ));
            }
        };
        let planned = self.lower_implicit_owned_read(
            function,
            &Place {
                root: source.expect("guarded dataref root"),
                projection: vec![],
                ty,
            },
            locals,
        )?;
        stmts.extend(planned.stmts);
        let RirOperand::Place(root) = planned.operand else {
            unreachable!("implicit owned read stages a local")
        };
        Ok((ty, root))
    }

    fn rir_root_place(local: RirLocalId) -> RirPlace {
        RirPlace::local(local, vec![])
    }

    fn dataref_mut_place_segment(
        &mut self,
        function: FunctionId,
        plan: &PlaceAccessPlan,
        locals: &mut Vec<RirLocal>,
        stmts: &mut Vec<RirStmt>,
    ) -> Result<DataRefSegment, RustTargetGap> {
        self.final_dataref_segment(function, plan, locals, stmts)
    }

    fn final_dataref_segment(
        &mut self,
        function: FunctionId,
        plan: &PlaceAccessPlan,
        locals: &mut Vec<RirLocal>,
        stmts: &mut Vec<RirStmt>,
    ) -> Result<DataRefSegment, RustTargetGap> {
        let dataref = plan.dataref_plan().expect("dataref places have a plan");
        if dataref.segments.iter().any(|segment| {
            self.materializers
                .get(
                    segment.storage_ty,
                    RustRecipePosition::StoredPayload(LambdaStorageFamily::DataRefProjection),
                )
                .is_none()
        }) {
            return Err(Self::gap(
                RustTargetGapSite::Function(function),
                RustTargetGapKind::UnsupportedPlaceProjection,
            ));
        }
        let mut current_place =
            self.dataref_plan_object_prefix(function, plan, dataref, locals, stmts)?;
        let (last, prefix) = dataref
            .segments
            .split_last()
            .expect("dataref plan has at least one segment");
        for (index, segment) in prefix.iter().enumerate() {
            self.append_dataref_object_prefix(plan, &mut current_place, segment);
            let next = self.dataref_segment(
                plan,
                current_place,
                segment,
                index == 0 && dataref.object_prefix_can_fail,
            );
            current_place =
                self.read_dataref_segment(function, next, segment.storage_ty, locals, stmts)?;
        }
        self.append_dataref_object_prefix(plan, &mut current_place, last);
        let segment = self.dataref_segment(
            plan,
            current_place,
            last,
            prefix.is_empty() && dataref.object_prefix_can_fail,
        );
        Ok(self.prepare_dataref_segment_object(function, segment, locals, stmts))
    }

    fn dataref_plan_object_prefix(
        &mut self,
        function: FunctionId,
        plan: &PlaceAccessPlan,
        dataref: &DataRefProjectionPlan,
        locals: &mut Vec<RirLocal>,
        stmts: &mut Vec<RirStmt>,
    ) -> Result<RirPlace, RustTargetGap> {
        let (_, mut current_place) = self.dataref_runtime_root_place(
            function,
            plan.root,
            plan.path().root().ty,
            locals,
            stmts,
        )?;
        let steps = self.rir_place_steps(plan, dataref.object_prefix.clone());
        current_place.projections.extend(steps);
        Ok(current_place)
    }

    fn append_dataref_object_prefix(
        &self,
        plan: &PlaceAccessPlan,
        place: &mut RirPlace,
        segment: &DataRefSegmentPlan,
    ) {
        let steps = self.rir_place_steps(plan, segment.object_prefix.clone());
        place.projections.extend(steps);
    }

    fn dataref_segment(
        &mut self,
        plan: &PlaceAccessPlan,
        object: RirPlace,
        segment: &DataRefSegmentPlan,
        object_must_materialize: bool,
    ) -> DataRefSegment {
        let storage = self.rir_place_steps(plan, segment.storage.clone());
        let materializer = self
            .materializers
            .get(
                segment.storage_ty,
                RustRecipePosition::StoredPayload(LambdaStorageFamily::DataRefProjection),
            )
            .expect("checked dataref projection materializer");
        DataRefSegment {
            object: RirOperand::Place(object),
            object_ty: segment.dataref_ty,
            object_must_materialize,
            place: self.intern_dataref_place(
                self.dataref_map[&segment.dataref],
                storage,
                materializer,
            ),
        }
    }

    fn intern_dataref_place(
        &mut self,
        dataref: RirDataRefId,
        storage: Vec<RirPlaceStep>,
        materializer: rir::RirMaterializerId,
    ) -> RirDataRefPlaceId {
        let key = (dataref, storage, materializer);
        if let Some(&id) = self.dataref_place_map.get(&key) {
            return id;
        }
        let id = RirDataRefPlaceId::from_index(self.dataref_places.len());
        self.dataref_places.push(RirDataRefPlace {
            dataref,
            storage: key.1.clone(),
            materializer,
        });
        self.dataref_place_map.insert(key, id);
        id
    }

    fn prepare_dataref_segment_object(
        &self,
        function: FunctionId,
        mut segment: DataRefSegment,
        locals: &mut Vec<RirLocal>,
        stmts: &mut Vec<RirStmt>,
    ) -> DataRefSegment {
        if segment.object_must_materialize || self.operand_uses_ctx(function, &segment.object) {
            segment.object = self.rvalue_temp(
                RirRValue::Use(segment.object),
                segment.object_ty,
                locals,
                stmts,
            );
        }
        segment
    }

    fn read_dataref_segment(
        &mut self,
        function: FunctionId,
        segment: DataRefSegment,
        storage_ty: TypeId,
        locals: &mut Vec<RirLocal>,
        stmts: &mut Vec<RirStmt>,
    ) -> Result<RirPlace, RustTargetGap> {
        let segment = self.prepare_dataref_segment_object(function, segment, locals, stmts);
        let materializer =
            self.reusable_materializer(function, storage_ty, RustRecipePosition::Value)?;
        let local = self.alloc_temp(locals, storage_ty);
        stmts.push(RirStmt::Init {
            local,
            value: RirRValue::Materialize(RirOwnedValue {
                value: RirOwnedOperand::Access(RirMutPlaceArg::dataref(
                    segment.object,
                    segment.place,
                    vec![],
                )),
                source: RirOwnedSource::Reuse(materializer),
            }),
        });
        Ok(Self::rir_root_place(local))
    }

    fn rvalue_short_region_value(
        &self,
        function: FunctionId,
        value: RirRValue,
        ty: TypeId,
        locals: &mut Vec<RirLocal>,
        stmts: &mut Vec<RirStmt>,
    ) -> RirRValue {
        match value {
            RirRValue::Use(operand) if !self.operand_uses_ctx(function, &operand) => {
                RirRValue::Use(operand)
            }
            value => RirRValue::TakeStaged(self.rvalue_temp(value, ty, locals, stmts)),
        }
    }

    fn operand_uses_ctx(&self, function: FunctionId, operand: &RirOperand) -> bool {
        matches!(
            operand,
            RirOperand::Place(place)
                if matches!(place.root, RirPlaceRoot::Global(_))
                    || self.rir_place_is_source_mut_place_param(function, place)
        )
    }

    fn rir_place_is_source_mut_place_param(&self, function: FunctionId, place: &RirPlace) -> bool {
        let RirPlaceRoot::Local(local) = place.root else {
            return false;
        };
        place.projections.is_empty()
            && self
                .air
                .function(function)
                .signature
                .params
                .iter()
                .any(|param| {
                    param.mode == ParamMode::MutBorrow && param.local_id.index() == local.index()
                })
    }

    fn rvalue_temp(
        &self,
        value: RirRValue,
        ty: TypeId,
        locals: &mut Vec<RirLocal>,
        stmts: &mut Vec<RirStmt>,
    ) -> RirOperand {
        RirOperand::Place(self.rvalue_temp_place(value, ty, locals, stmts))
    }

    fn rvalue_temp_place(
        &self,
        value: RirRValue,
        ty: TypeId,
        locals: &mut Vec<RirLocal>,
        stmts: &mut Vec<RirStmt>,
    ) -> RirPlace {
        let local = self.alloc_temp(locals, ty);
        stmts.push(RirStmt::Init { local, value });
        Self::rir_root_place(local)
    }

    fn place_capture_cell(
        &self,
        function: FunctionId,
        place: &Place,
    ) -> Option<air::CaptureCellId> {
        self.air.capture_cell_root(function, place.root)
    }

    fn capture_cell_ref(&self, function: FunctionId, cell: air::CaptureCellId) -> RirCellRef {
        let rir_cell = self.capture_cell_map[&cell];
        if self.air.capture_cells[cell.index()].owner == function {
            return RirCellRef::Owner(rir_cell);
        }
        let air::FunctionKind::Lambda(lambda) = self.air.function(function).kind else {
            unreachable!("AIR verifier rejects inaccessible capture cells")
        };
        let slot = self.air.lambdas[lambda.index()]
            .captures
            .iter()
            .enumerate()
            .find_map(|(slot, capture)| match capture {
                air::LambdaCaptureDecl::CaptureCell { cell: found, .. } if *found == cell => {
                    Some(air::LambdaCaptureSlotId::from_index(slot))
                }
                _ => None,
            })
            .expect("AIR verifier rejects uncaptured capture-cell use");
        let runtime = self.lambda_runtime_capture_slots[&(lambda, slot)];
        RirCellRef::Capture {
            cell: rir_cell,
            local: RirLocalId::from_index(self.air.function(function).locals.len() + runtime),
        }
    }

    fn scoped_place_cell_ref(
        &self,
        function: FunctionId,
        borrow: air::ScopedBorrowId,
    ) -> RirScopedPlaceCellRef {
        let cell = self.scoped_place_cell_map[&borrow];
        if self.air.scoped_borrows[borrow.index()].owner == function {
            return RirScopedPlaceCellRef::Owner(cell);
        }
        let air::FunctionKind::Lambda(lambda) = self.air.function(function).kind else {
            unreachable!("AIR verifier rejects inaccessible scoped borrows")
        };
        let slot = self.air.lambdas[lambda.index()]
            .captures
            .iter()
            .enumerate()
            .find_map(|(slot, capture)| match capture {
                air::LambdaCaptureDecl::ScopedBorrow { borrow: found, .. } if *found == borrow => {
                    Some(air::LambdaCaptureSlotId::from_index(slot))
                }
                _ => None,
            })
            .expect("AIR verifier rejects uncaptured scoped-borrow use");
        let runtime = self.lambda_runtime_capture_slots[&(lambda, slot)];
        RirScopedPlaceCellRef::Capture {
            cell,
            local: RirLocalId::from_index(self.air.function(function).locals.len() + runtime),
        }
    }

    fn place_scoped_borrow(
        &self,
        function: FunctionId,
        place: &Place,
    ) -> Option<air::ScopedBorrowId> {
        match place.root {
            air::PlaceRoot::ScopedBorrow(borrow) => Some(borrow),
            root => self.air.scoped_borrow_root(function, root),
        }
    }

    fn alloc_temp(&self, locals: &mut Vec<RirLocal>, ty: TypeId) -> RirLocalId {
        let index = locals.len();
        let id = RirLocalId::from_index(index);
        locals.push(RirLocal {
            id,
            ty: self.type_map[&ty],
            mutable: false,
            symbol: local_symbol(index, None),
            binding: RirLocalBinding::Value,
        });
        id
    }

    fn rir_place_steps(
        &self,
        plan: &PlaceAccessPlan,
        range: std::ops::Range<usize>,
    ) -> Vec<RirPlaceStep> {
        plan.steps()[range]
            .iter()
            .map(|step| self.rir_place_step(step))
            .collect()
    }

    fn collection_slot_step(
        &self,
        function: FunctionId,
        scope: &air::AirCollectionSlotScope,
        slot: &air::AirCollectionSlot,
    ) -> RirPlaceStep {
        let step = place_model::project_step(
            self.air,
            function,
            scope.root.ty,
            &air::Projection::Index(scope.index),
        )
        .expect("AIR verifies collection slot projection");
        debug_assert_eq!(step.ty(), slot.ty);
        self.rir_place_step(&step)
    }

    fn rir_place_step(&self, step: &place_model::ProjectionStep) -> RirPlaceStep {
        let source_ty = self.type_map[&step.source_ty()];
        let target_ty = self.type_map[&step.ty()];
        let kind = match step.kind() {
            place_model::ProjectionKind::Field(field) => {
                RirPlaceStepKind::StructField(RirFieldId::from_index(field.index()))
            }
            place_model::ProjectionKind::DataRefField(field) => {
                RirPlaceStepKind::DataRefField(RirFieldId::from_index(field.index()))
            }
            place_model::ProjectionKind::ExternField(field) => RirPlaceStepKind::ExternField(
                self.extern_storage_field(step.source_ty(), field)
                    .expect("AIR verification rejects unsupported extern field projection"),
            ),
            place_model::ProjectionKind::TupleField(index) => {
                RirPlaceStepKind::TupleField(RirFieldId::from_index(index as usize))
            }
            place_model::ProjectionKind::ArrayIndex(local) => {
                let TypeData::Array { len, .. } = self.air.type_arena.data(step.source_ty()) else {
                    unreachable!("AIR verification rejects invalid array projection")
                };
                RirPlaceStepKind::ArrayIndex {
                    index: RirLocalId::from_index(local.index()),
                    len: *len as u64,
                    elem_materializer: self
                        .materializers
                        .get(
                            step.ty(),
                            RustRecipePosition::StoredPayload(
                                LambdaStorageFamily::FixedArrayElement,
                            ),
                        )
                        .expect("AIR verification rejects unsupported array element"),
                }
            }
            place_model::ProjectionKind::ListIndex(local) => RirPlaceStepKind::ListIndex {
                index: RirLocalId::from_index(local.index()),
                elem_materializer: self
                    .materializers
                    .get(
                        step.ty(),
                        RustRecipePosition::StoredPayload(LambdaStorageFamily::ListElement),
                    )
                    .expect("AIR verification rejects unsupported list element"),
            },
            place_model::ProjectionKind::SliceIndex(local) => RirPlaceStepKind::SliceIndex {
                index: RirLocalId::from_index(local.index()),
                elem_materializer: self
                    .materializers
                    .get(step.ty(), RustRecipePosition::Value)
                    .expect("AIR verification rejects unsupported slice element"),
            },
        };
        RirPlaceStep {
            source_ty,
            target_ty,
            kind,
        }
    }

    fn inline_extern_storage_fields(
        &self,
        ext: air::ExternTypeId,
    ) -> impl Iterator<Item = (FieldId, &air::ExternFieldDecl, RirFieldId)> + '_ {
        self.air
            .extern_type(ext)
            .fields
            .iter()
            .enumerate()
            .filter(|(_, field)| !field.computed)
            .enumerate()
            .map(|(storage, (field, decl))| {
                (
                    FieldId::from_index(field),
                    decl,
                    RirFieldId::from_index(storage),
                )
            })
    }

    fn extern_storage_field(&self, source_ty: TypeId, field_id: FieldId) -> Option<RirFieldId> {
        let TypeData::Extern(extern_id) = self.air.type_arena.data(source_ty) else {
            return None;
        };
        if self.air.extern_type(*extern_id).rep != air::ExternRep::Inline {
            return None;
        }
        self.inline_extern_storage_fields(*extern_id)
            .find_map(|(field, _, storage)| (field == field_id).then_some(storage))
    }

    fn operand_ty(&self, operand: &Operand) -> TypeId {
        self.air
            .operand_ty(operand)
            .expect("verified AIR operand const should exist")
    }

    fn rust_copyable_rir_type(&self, ty: RirTypeId) -> bool {
        self.type_map
            .iter()
            .find_map(|(air_ty, mapped)| (*mapped == ty).then_some(*air_ty))
            .is_some_and(|ty| self.rust_copyable_air_type(ty))
    }

    fn rust_copyable_air_type(&self, ty: TypeId) -> bool {
        if matches!(self.air.type_arena.data(ty), TypeData::Function(_)) {
            return self.function_type_copyable[&ty];
        }
        self.air_policy().copyable(ty)
    }

    fn access(&self) -> PlaceAccessCx<'_> {
        PlaceAccessCx::new(self.air)
    }

    fn air_policy(&self) -> RustRepresentationPlan<'_> {
        RustRepresentationPlan::new(self.air, &self.classes)
    }

    fn plan_place(&self, function: FunctionId, plan: &PlaceAccessPlan) -> RirPlace {
        let root = match plan.root {
            PlaceAccessRoot::Local { local, .. } => {
                RirPlaceRoot::Local(RirLocalId::from_index(local.index()))
            }
            PlaceAccessRoot::Global(global) => RirPlaceRoot::Global(self.global_map[&global]),
            PlaceAccessRoot::LambdaCapture(slot) => {
                let air::FunctionKind::Lambda(lambda) = self.air.function(function).kind else {
                    unreachable!("AIR verifier rejects capture roots outside lambdas")
                };
                let runtime = self.lambda_runtime_capture_slots[&(lambda, slot)];
                RirPlaceRoot::Local(RirLocalId::from_index(
                    self.air.function(function).locals.len() + runtime,
                ))
            }
            PlaceAccessRoot::CaptureCell(_) | PlaceAccessRoot::ScopedPlaceCell(_) => {
                unreachable!("access plan requires a mutable-place wrapper")
            }
        };
        RirPlace {
            root,
            projections: self.rir_place_steps(plan, 0..plan.steps().len()),
        }
    }

    fn plan_place_operand(&self, function: FunctionId, plan: &PlaceAccessPlan) -> RirOperand {
        RirOperand::Place(self.plan_place(function, plan))
    }
}

fn format_supported(ty: &TypeData, spec: FormatSpec) -> bool {
    let scalar = matches!(
        ty,
        TypeData::Int | TypeData::Float | TypeData::Bool | TypeData::String | TypeData::Char
    );
    if !scalar {
        return false;
    }
    match spec.kind {
        FormatKind::Hex | FormatKind::HexUpper | FormatKind::Binary
            if !matches!(ty, TypeData::Int) =>
        {
            return false;
        }
        FormatKind::Exp | FormatKind::ExpUpper if !matches!(ty, TypeData::Float) => {
            return false;
        }
        _ => {}
    }
    if spec.precision.is_some() && !matches!(ty, TypeData::Float | TypeData::String) {
        return false;
    }
    spec.sign != FormatSign::Always || matches!(ty, TypeData::Int | TypeData::Float)
}

fn rir_param_escape(escape: ParamEscape) -> RirParamEscape {
    match escape {
        ParamEscape::NonEscaping => RirParamEscape::NonEscaping,
        ParamEscape::Escaping => RirParamEscape::Escaping,
    }
}

fn rir_function_value_escape(capability: FunctionValueCapability) -> Option<RirLambdaEscape> {
    match capability {
        FunctionValueCapability::Escaping => Some(RirLambdaEscape::Escaping),
        FunctionValueCapability::NonEscaping => Some(RirLambdaEscape::NonEscaping),
        FunctionValueCapability::NonFunction | FunctionValueCapability::Unknown => None,
    }
}

fn rir_cell_lifetime(lifetime: air::CaptureCellLifetime) -> RirCellLifetime {
    match lifetime {
        air::CaptureCellLifetime::Function => RirCellLifetime::Function,
        air::CaptureCellLifetime::Loop { loop_id } => RirCellLifetime::Loop {
            loop_id: RirLoopId::from_index(loop_id.index()),
        },
    }
}

fn rir_collection_root_kind(kind: air::AirCollectionRootKind) -> RirCollectionRootKind {
    match kind {
        air::AirCollectionRootKind::List => RirCollectionRootKind::List,
        air::AirCollectionRootKind::FixedArray => RirCollectionRootKind::FixedArray,
        air::AirCollectionRootKind::Slice => RirCollectionRootKind::Slice,
        air::AirCollectionRootKind::Map => RirCollectionRootKind::Map,
    }
}

fn rir_map_write_kind(kind: MapWriteKind) -> RirMapWriteKind {
    match kind {
        MapWriteKind::IndexedAssignment => RirMapWriteKind::IndexedAssignment,
        MapWriteKind::StructuralInsert => RirMapWriteKind::StructuralInsert,
    }
}

fn rir_collection_loan_mode(mode: air::AirCollectionLoanMode) -> RirCollectionLoanMode {
    match mode {
        air::AirCollectionLoanMode::ReadonlySequence => RirCollectionLoanMode::ReadonlySequence,
        air::AirCollectionLoanMode::MutableSequenceElement => {
            RirCollectionLoanMode::MutableSequenceElement
        }
        air::AirCollectionLoanMode::ReadonlyMap => RirCollectionLoanMode::ReadonlyMap,
        air::AirCollectionLoanMode::MutableMapValue => RirCollectionLoanMode::MutableMapValue,
    }
}

fn rir_core_enum_kind(kind: air::CoreEnumKind) -> RirCoreEnumKind {
    match kind {
        air::CoreEnumKind::Option => RirCoreEnumKind::Option,
        air::CoreEnumKind::Result => RirCoreEnumKind::Result,
    }
}

fn rir_enum_repr(repr: air::EnumRepr) -> RirEnumRepr {
    match repr {
        air::EnumRepr::Adt => RirEnumRepr::Adt,
        air::EnumRepr::RawInt => RirEnumRepr::RawInt,
        air::EnumRepr::RawString => RirEnumRepr::RawString,
    }
}

fn global_display(air: &air::Program, decl: &air::GlobalDecl) -> RirSymbol {
    let mut path = air.modules[decl.module.index()]
        .path
        .iter()
        .map(Ident::as_str)
        .filter(|segment| !segment.is_empty())
        .collect::<Vec<_>>();
    path.push(decl.name.as_str());
    RirSymbol::new(path.join("."))
}

fn global_slot_symbol(id: RirGlobalId, decl: &air::GlobalDecl) -> RirSymbol {
    RirSymbol::new(format!("g{}_{}", id.index(), sanitize(decl.name.as_str())))
}

fn function_symbol(
    prefix: &str,
    id: FunctionId,
    function: &air::Function,
    air: &air::Program,
    rir: &RirProgram,
    type_map: &HashMap<TypeId, RirTypeId>,
) -> Result<RirSymbol, RustTargetGap> {
    let name = sanitize(function.name.as_str());
    let mut symbol = match method_owner_name(function, air) {
        Some(owner) => format!("{prefix}_f{}_{}_{}", id.index(), sanitize(owner), name),
        None => format!("{prefix}_f{}_{}", id.index(), name),
    };
    if let Some(specialization) = &function.specialization {
        let mut parts = vec![];
        for ty in &specialization.type_args {
            let Some(&rir_ty) = type_map.get(ty) else {
                return Err(RustTargetGap {
                    site: RustTargetGapSite::Function(id),
                    kind: RustTargetGapKind::UnsupportedType,
                });
            };
            parts.push(type_suffix(rir, rir_ty));
        }
        parts.extend(specialization.const_args.iter().map(const_suffix));
        if !parts.is_empty() {
            symbol.push('_');
            symbol.push_str(&parts.join("_"));
        }
    }
    Ok(RirSymbol::new(symbol))
}

fn type_suffix(program: &RirProgram, ty: RirTypeId) -> String {
    match program.types[ty.index()] {
        RirType::Int => "int".to_string(),
        RirType::Float => "float".to_string(),
        RirType::Bool => "bool".to_string(),
        RirType::String => "string".to_string(),
        RirType::Char => "char".to_string(),
        RirType::Void => "void".to_string(),
        RirType::List(elem) => format!("list_{}", type_suffix(program, elem)),
        RirType::Option(inner) => format!("option_{}", type_suffix(program, inner)),
        RirType::Map { key, value } => format!(
            "map_{}_{}",
            type_suffix(program, key),
            type_suffix(program, value)
        ),
        RirType::Slice(elem) => format!("slice_{}", type_suffix(program, elem)),
        RirType::Lambda(sig) => format!("lambda_{}", sig.index()),
        RirType::Array { elem, len } => format!("array_{}_{}", len, type_suffix(program, elem)),
        RirType::Struct(id) => named_type_suffix(ty.index(), &program.structs[id.index()].display),
        RirType::Tuple(id) => named_type_suffix(ty.index(), &program.tuples[id.index()].display),
        RirType::DataRef(id) => {
            named_type_suffix(ty.index(), &program.datarefs[id.index()].display)
        }
        RirType::Flag(id) => named_type_suffix(ty.index(), &program.flags[id.index()].display),
        RirType::Enum(id) => {
            let enm = &program.enums[id.index()];
            if enm.core == Some(RirCoreEnumKind::Option) {
                let some = enm
                    .variants
                    .iter()
                    .find(|variant| variant.kind == RirVariantKind::Tuple);
                if let Some(field) = some.and_then(|variant| variant.fields.first()) {
                    return format!("option_{}", type_suffix(program, field.ty));
                }
            }
            named_type_suffix(ty.index(), &enm.display)
        }
    }
}

fn named_type_suffix(index: usize, display: &RirSymbol) -> String {
    format!(
        "t{}_{}",
        index,
        sanitize(display.as_str()).to_ascii_lowercase()
    )
}

fn const_suffix(value: &ConstValue) -> String {
    match value {
        ConstValue::Int(value) if *value < 0 => format!("n_neg_{}", value.unsigned_abs()),
        ConstValue::Int(value) => format!("n{value}"),
        ConstValue::Flag { flag, bits } => format!("flag{}_{}", flag.index(), bits),
        ConstValue::Float(value) => format!("f{:016x}", value.to_bits()),
        ConstValue::Bool(value) => value.to_string(),
        ConstValue::String(value) => sanitize(value).to_ascii_lowercase(),
        ConstValue::Char(value) => sanitize(&format!("c{}", *value as u32)),
        ConstValue::Nil => "nil".to_string(),
    }
}

fn method_owner_name<'a>(
    function: &'a air::Function,
    program: &'a air::Program,
) -> Option<&'a str> {
    if function.kind != air::FunctionKind::Method {
        return None;
    }
    if let Some(owner) = &function.owner {
        return Some(owner.name.as_str());
    }
    let receiver = function
        .signature
        .params
        .iter()
        .find(|param| param.role == air::ParamRole::Receiver)?;
    match program.type_arena.data(receiver.ty) {
        TypeData::Aggregate(aggregate) => Some(program.aggregate(*aggregate).name.as_str()),
        TypeData::Enum(enm) => Some(program.enum_decl(*enm).name.as_str()),
        _ => None,
    }
}

fn local_symbol(index: usize, name: Option<&Ident>) -> RirSymbol {
    match name {
        Some(name) => RirSymbol::new(format!("v{index}_{}", sanitize(name.as_str()))),
        None => RirSymbol::new(format!("v{index}")),
    }
}

fn scoped_symbol(text: &str, seen: &mut Vec<String>) -> RirSymbol {
    let base = sanitize(text);
    let mut symbol = base.clone();
    let mut index = 1;
    while seen.contains(&symbol) {
        symbol = format!("{base}_{index}");
        index += 1;
    }
    seen.push(symbol.clone());
    RirSymbol::new(symbol)
}

fn sanitize(text: &str) -> String {
    let mut out = String::new();
    for ch in text.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            out.push(ch);
        } else {
            out.push('_');
        }
    }
    if out.is_empty() || out == "_" || out.as_bytes()[0].is_ascii_digit() {
        out.insert(0, '_');
    }
    if is_rust_reserved_ident(&out) {
        out.insert_str(0, "r_");
    }
    out
}

fn is_rust_reserved_ident(text: &str) -> bool {
    matches!(
        text,
        "Self"
            | "abstract"
            | "as"
            | "async"
            | "await"
            | "become"
            | "box"
            | "break"
            | "const"
            | "continue"
            | "crate"
            | "do"
            | "dyn"
            | "else"
            | "enum"
            | "extern"
            | "false"
            | "final"
            | "fn"
            | "for"
            | "gen"
            | "if"
            | "impl"
            | "in"
            | "let"
            | "loop"
            | "macro"
            | "match"
            | "mod"
            | "move"
            | "mut"
            | "override"
            | "priv"
            | "pub"
            | "ref"
            | "return"
            | "self"
            | "static"
            | "struct"
            | "super"
            | "trait"
            | "true"
            | "try"
            | "type"
            | "typeof"
            | "union"
            | "unsafe"
            | "unsized"
            | "use"
            | "virtual"
            | "where"
            | "while"
            | "yield"
    )
}
