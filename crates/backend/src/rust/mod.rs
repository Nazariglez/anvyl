mod analysis;
pub mod cargo_job;
mod dataref_place;
pub mod emit;
mod mut_place;
mod native;
mod place;
mod place_access;
pub mod profile;
pub mod rep_policy;
pub mod rir;
#[cfg(test)]
mod source_job;
mod syntax;
mod target;
mod value;
mod write;

use std::{
    collections::{HashMap, HashSet},
    error::Error,
    fmt,
};

use anvyx_frontend::{
    air::{
        self, AggregateCtor, CallArg, Callee, ConstId, ConstValue, ExternId, FunctionId, GlobalId,
        LocalId, LocalKind, MapWriteKind, Mutability, Operand, ParamEscape, ParamMode, Place,
        Projection, RValue, TypeData, TypeId, TypePassClasses, VerifiedProgram,
    },
    ast::{FormatAlign, FormatKind, FormatSign, FormatSpec, Ident},
};
use anvyx_runtime::{RustPath, RustProviderSupport};

use self::{
    place_access::{
        DataRefProjectionPlan, DataRefSegmentPlan, PlaceAccessCx, PlaceAccessIntent,
        PlaceAccessPlan, PlaceAccessRoot, PlaceProjection, PlaceProjectionKind,
    },
    profile::{ProfileErrorKind, ProfileSite, RustBackendProfile, RustBackendProfileError},
    rep_policy::{AirRustRepPolicy, RustRepPolicy},
    rir::{
        RirCallArg, RirCallTarget, RirCellDecl, RirCellId, RirCellRef, RirCellStorage,
        RirCollectionLoanMode, RirCollectionLoanScope, RirCollectionRootKind, RirCollectionStorage,
        RirCollectionStorageId, RirCollectionStorageKind, RirConst, RirConstId, RirConstValue,
        RirCoreEnumKind, RirCtxPlan, RirDataRef, RirDataRefId, RirEnum, RirEnumId, RirEnumMatch,
        RirEnumMatchArm, RirEnumRepr, RirExtern, RirExternId, RirExternKind, RirExternParam,
        RirField, RirFieldId, RirFormatAlign, RirFormatKind, RirFormatSign, RirFormatSpec,
        RirFunction, RirFunctionId, RirGlobal, RirGlobalId, RirIf, RirLambda, RirLambdaCapture,
        RirLambdaCaptureArg, RirLambdaCaptureKind, RirLambdaEnvField, RirLambdaEnvFieldKind,
        RirLambdaEnvId, RirLambdaEnvLayout, RirLambdaEscape, RirLambdaId, RirLambdaParam,
        RirLambdaSig, RirLambdaSigId, RirLambdaSource, RirLambdaStorage, RirLocal, RirLocalId,
        RirLoop, RirLoopId, RirMapEntryMatch, RirMapWriteKind, RirMutPlaceAccess, RirMutPlaceArg,
        RirMutPlaceHandle, RirNativeExtern, RirOperand, RirOptionMatch, RirOptionSubject, RirParam,
        RirParamAbi, RirParamEscape, RirParamSemantic, RirPlace, RirPlaceRoot, RirProgram,
        RirProjection, RirRValue, RirRawEnumValue, RirReturn, RirScopedPlaceCellDecl,
        RirScopedPlaceCellId, RirScopedPlaceCellRef, RirStmt, RirStringifyHelper,
        RirStringifyHelperId, RirStringifyReq, RirStringifyReqId, RirStringifyReqKind, RirStruct,
        RirStructId, RirStructuredBlock, RirSymbol, RirTerm, RirTuple, RirTupleId, RirType,
        RirTypeId, RirVariant, RirVariantId, RirVariantKind, VerifiedRirProgram,
    },
};

#[cfg(test)]
mod tests;

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

pub struct RirPlan {
    program: RirProgram,
}

impl RirPlan {
    pub fn program(&self) -> &RirProgram {
        &self.program
    }

    pub fn verified(&self) -> VerifiedRirProgram<'_> {
        rir::verify(&self.program).expect("planner produced invalid RIR")
    }
}

pub fn plan(
    program: &VerifiedProgram<'_>,
    config: RustPlanConfig,
) -> Result<RirPlan, RustPlanError> {
    let mut cx = PlanCx::new(program, config);
    cx.check_support()?;
    let rir = cx.plan()?;
    rir::verify(&rir).map_err(RustPlanError::RirVerify)?;
    Ok(RirPlan { program: rir })
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RustPlanError {
    TargetGaps(Vec<RustTargetGap>),
    RirVerify(Vec<rir::RirVerifyError>),
}

impl fmt::Display for RustPlanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TargetGaps(gaps) => {
                writeln!(f, "Rust backend target gaps: {}", gaps.len())?;
                for gap in gaps.iter().take(8) {
                    writeln!(f, "  - {gap}")?;
                }
                if gaps.len() > 8 {
                    writeln!(f, "  - ... and {} more", gaps.len() - 8)?;
                }
                Ok(())
            }
            Self::RirVerify(errors) => {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustTargetGapSite {
    Entry,
    Type(TypeId),
    Const(ConstId),
    Module(usize),
    Global(GlobalId),
    Function(FunctionId),
    Extern(ExternId),
    Local(FunctionId, LocalId),
    Param(FunctionId, usize),
    Statement(FunctionId, usize),
    Terminator(FunctionId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustTargetGapKind {
    UnsupportedType,
    UnsupportedConst,
    UnsupportedModuleItem,
    UnsupportedFunctionKind,
    UnsupportedParamRole,
    UnsupportedParamMode,
    UnsupportedCallArgMode,
    UnsupportedReturnMode,
    UnsupportedLocalKind,
    UnsupportedPlaceProjection,
    UnsupportedTerminator,
    UnsupportedRValue,
    UnsupportedCallee,
    UnsupportedExtern,
    UnsupportedExternMember,
    UnsupportedEntry,
    UnsupportedRustAbi,
    UnsupportedLambdaValue,
    UnsupportedLambdaCall,
    UnsupportedLambdaCapture,
    UnsupportedLambdaCell,
    UnsupportedLambdaExternBoundary,
    UnsupportedGlobalType,
    UnsupportedGlobalAccess,
    UnsupportedGlobalBorrow,
    UnsupportedGlobalProjection,
    UnsupportedGlobalInitializer,
    UnsupportedGlobalValueRead,
    UnsupportedGlobalRooting,
    NonCopyValueRequired,
    UnsupportedStructuralStringify,
    UnsupportedContextBorrowAcrossCall,
    UnsupportedProviderNativeRepresentation,
    UnsupportedSliceView,
    UnsupportedMutablePlace,
    UnsupportedMutablePlaceProjection,
    UnsupportedMutablePlaceDataRef,
    UnsupportedMutablePlaceNativeBoundary,
    UnsupportedMapKey,
    UnsupportedMapValue,
}

impl fmt::Display for RustTargetGap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} at {:?}", self.kind, self.site)
    }
}

impl Error for RustTargetGap {}

impl From<RustBackendProfileError> for RustTargetGap {
    fn from(error: RustBackendProfileError) -> Self {
        Self {
            site: match error.site {
                ProfileSite::Entry => RustTargetGapSite::Entry,
                ProfileSite::Type(id) => RustTargetGapSite::Type(id),
                ProfileSite::Const(id) => RustTargetGapSite::Const(id),
                ProfileSite::Module(index) => RustTargetGapSite::Module(index),
                ProfileSite::Global(id) => RustTargetGapSite::Global(id),
                ProfileSite::Function(id) => RustTargetGapSite::Function(id),
                ProfileSite::Extern(id) => RustTargetGapSite::Extern(id),
                ProfileSite::Local(function, local) => RustTargetGapSite::Local(function, local),
                ProfileSite::Param(function, index) => RustTargetGapSite::Param(function, index),
                ProfileSite::Statement(function, index) => {
                    RustTargetGapSite::Statement(function, index)
                }
                ProfileSite::Terminator(function) => RustTargetGapSite::Terminator(function),
            },
            kind: match error.kind {
                ProfileErrorKind::UnsupportedType => RustTargetGapKind::UnsupportedType,
                ProfileErrorKind::UnsupportedConst => RustTargetGapKind::UnsupportedConst,
                ProfileErrorKind::UnsupportedModuleItem => RustTargetGapKind::UnsupportedModuleItem,
                ProfileErrorKind::UnsupportedFunctionKind => {
                    RustTargetGapKind::UnsupportedFunctionKind
                }
                ProfileErrorKind::UnsupportedParamRole => RustTargetGapKind::UnsupportedParamRole,
                ProfileErrorKind::UnsupportedParamMode => RustTargetGapKind::UnsupportedParamMode,
                ProfileErrorKind::UnsupportedCallArgMode => {
                    RustTargetGapKind::UnsupportedCallArgMode
                }
                ProfileErrorKind::UnsupportedReturnMode => RustTargetGapKind::UnsupportedReturnMode,
                ProfileErrorKind::UnsupportedLocalKind => RustTargetGapKind::UnsupportedLocalKind,
                ProfileErrorKind::UnsupportedPlaceProjection => {
                    RustTargetGapKind::UnsupportedPlaceProjection
                }
                ProfileErrorKind::UnsupportedTerminator => RustTargetGapKind::UnsupportedTerminator,
                ProfileErrorKind::UnsupportedRValue => RustTargetGapKind::UnsupportedRValue,
                ProfileErrorKind::UnsupportedCallee => RustTargetGapKind::UnsupportedCallee,
                ProfileErrorKind::UnsupportedExtern => RustTargetGapKind::UnsupportedExtern,
                ProfileErrorKind::UnsupportedExternMember => {
                    RustTargetGapKind::UnsupportedExternMember
                }
                ProfileErrorKind::UnsupportedEntry => RustTargetGapKind::UnsupportedEntry,
                ProfileErrorKind::UnsupportedRustAbi => RustTargetGapKind::UnsupportedRustAbi,
                ProfileErrorKind::UnsupportedLambdaValue => {
                    RustTargetGapKind::UnsupportedLambdaValue
                }
                ProfileErrorKind::UnsupportedLambdaCapture => {
                    RustTargetGapKind::UnsupportedLambdaCapture
                }
                ProfileErrorKind::UnsupportedLambdaCell => RustTargetGapKind::UnsupportedLambdaCell,
                ProfileErrorKind::UnsupportedLambdaExternBoundary => {
                    RustTargetGapKind::UnsupportedLambdaExternBoundary
                }
                ProfileErrorKind::UnsupportedGlobalType => RustTargetGapKind::UnsupportedGlobalType,
                ProfileErrorKind::UnsupportedGlobalAccess => {
                    RustTargetGapKind::UnsupportedGlobalAccess
                }
                ProfileErrorKind::UnsupportedGlobalBorrow => {
                    RustTargetGapKind::UnsupportedGlobalBorrow
                }
                ProfileErrorKind::UnsupportedGlobalProjection => {
                    RustTargetGapKind::UnsupportedGlobalProjection
                }
                ProfileErrorKind::UnsupportedGlobalInitializer => {
                    RustTargetGapKind::UnsupportedGlobalInitializer
                }
                ProfileErrorKind::UnsupportedGlobalValueRead => {
                    RustTargetGapKind::UnsupportedGlobalValueRead
                }
                ProfileErrorKind::UnsupportedGlobalRooting => {
                    RustTargetGapKind::UnsupportedGlobalRooting
                }
                ProfileErrorKind::UnsupportedMutablePlace => {
                    RustTargetGapKind::UnsupportedMutablePlace
                }
                ProfileErrorKind::UnsupportedMutablePlaceProjection => {
                    RustTargetGapKind::UnsupportedMutablePlaceProjection
                }
                ProfileErrorKind::UnsupportedMutablePlaceDataRef => {
                    RustTargetGapKind::UnsupportedMutablePlaceDataRef
                }
                ProfileErrorKind::UnsupportedMutablePlaceNativeBoundary => {
                    RustTargetGapKind::UnsupportedMutablePlaceNativeBoundary
                }
                ProfileErrorKind::UnsupportedMapKey => RustTargetGapKind::UnsupportedMapKey,
                ProfileErrorKind::UnsupportedMapValue => RustTargetGapKind::UnsupportedMapValue,
                ProfileErrorKind::NonCopyValueRequired => RustTargetGapKind::NonCopyValueRequired,
            },
        }
    }
}

struct PlanCx<'a> {
    verified: &'a VerifiedProgram<'a>,
    air: &'a air::Program,
    classes: TypePassClasses,
    config: RustPlanConfig,
    type_map: HashMap<TypeId, RirTypeId>,
    lambda_sig_map: HashMap<TypeId, RirLambdaSigId>,
    const_map: HashMap<ConstId, RirConstId>,
    function_map: HashMap<FunctionId, RirFunctionId>,
    global_map: HashMap<GlobalId, RirGlobalId>,
    function_lambda_map: HashMap<FunctionId, RirLambdaId>,
    lambda_map: HashMap<air::LambdaId, RirLambdaId>,
    function_type_copyable: HashMap<TypeId, bool>,
    function_type_shareable: HashMap<TypeId, bool>,
    lambda_runtime_capture_slots: HashMap<(air::LambdaId, air::LambdaCaptureSlotId), usize>,
    capture_cell_map: HashMap<air::CaptureCellId, RirCellId>,
    scoped_place_cell_map: HashMap<air::ScopedBorrowId, RirScopedPlaceCellId>,
    extern_map: HashMap<ExternId, RirExternId>,
    dataref_map: HashMap<air::AggregateId, RirDataRefId>,
    enum_map: HashMap<air::EnumId, RirEnumId>,
    tuple_map: HashMap<Vec<RirTypeId>, RirTupleId>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct KnownLambdaValue {
    lambda: RirLambdaId,
    ty: RirTypeId,
}

impl KnownLambdaValue {
    fn rvalue(self) -> RirRValue {
        RirRValue::Lambda {
            lambda: self.lambda,
            captures: vec![],
            ty: self.ty,
        }
    }
}

struct PlannedLambdaCaptures {
    captures: Vec<RirLambdaCapture>,
    env_fields: Vec<RirLambdaEnvField>,
}

struct PlannedRValue {
    stmts: Vec<RirStmt>,
    value: RirRValue,
    post_stmts: Vec<RirStmt>,
}

impl PlannedRValue {
    fn from_value(value: RirRValue) -> Self {
        Self {
            stmts: vec![],
            value,
            post_stmts: vec![],
        }
    }
}

struct PlannedOperand {
    stmts: Vec<RirStmt>,
    operand: RirOperand,
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

struct PlannedPlace {
    stmts: Vec<RirStmt>,
    place: RirPlace,
    post_stmts: Vec<RirStmt>,
}

struct PlannedCollectionLoanRoot {
    place: RirPlace,
    root_kind: RirCollectionRootKind,
    mode: RirCollectionLoanMode,
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
    dataref: RirDataRefId,
    projections: Vec<RirProjection>,
    ty: TypeId,
}

enum AssignTarget {
    CaptureCell(air::CaptureCellId),
    ScopedPlaceCell(air::ScopedBorrowId),
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
    fn new(program: &'a VerifiedProgram<'a>, config: RustPlanConfig) -> Self {
        let air = program.program();
        Self {
            verified: program,
            air,
            classes: TypePassClasses::analyze(air),
            config,
            type_map: HashMap::new(),
            lambda_sig_map: HashMap::new(),
            const_map: HashMap::new(),
            function_map: HashMap::new(),
            global_map: HashMap::new(),
            function_lambda_map: HashMap::new(),
            lambda_map: HashMap::new(),
            function_type_copyable: HashMap::new(),
            function_type_shareable: HashMap::new(),
            lambda_runtime_capture_slots: HashMap::new(),
            capture_cell_map: HashMap::new(),
            scoped_place_cell_map: HashMap::new(),
            extern_map: HashMap::new(),
            dataref_map: HashMap::new(),
            enum_map: HashMap::new(),
            tuple_map: HashMap::new(),
        }
    }

    fn check_support(&self) -> Result<(), RustPlanError> {
        RustBackendProfile::check_with_native_support(self.verified, &self.config.native_providers)
            .map_err(|errors| {
                RustPlanError::TargetGaps(errors.into_iter().map(RustTargetGap::from).collect())
            })
    }

    fn gap(site: RustTargetGapSite, kind: RustTargetGapKind) -> RustPlanError {
        RustPlanError::TargetGaps(vec![RustTargetGap { site, kind }])
    }

    fn plan(&mut self) -> Result<RirProgram, RustPlanError> {
        let mut program = RirProgram {
            ctx: RirCtxPlan::default(),
            ..RirProgram::default()
        };
        self.plan_types(&mut program)?;
        Self::plan_collection_storages(&mut program);
        self.plan_consts(&mut program);
        self.plan_externs(&mut program)?;
        self.plan_function_ids();
        self.plan_globals(&mut program)?;
        self.plan_function_type_capture_policy(&mut program);
        self.plan_cells(&mut program);
        self.plan_scoped_place_cells(&mut program);
        self.plan_lambdas(&mut program)?;
        self.check_lambda_sig_storage_support(&program)?;
        self.check_lambda_env_storage_support(&program)?;
        self.check_lambda_value_capture_cycles(&program)?;
        self.plan_function_type_copyability(&program);
        self.plan_stringify_helpers(&mut program)?;
        for index in 0..self.air.functions.len() {
            let id = FunctionId::from_index(index);
            program.functions.push(self.plan_function(id, &program)?);
        }
        program.entry = self.air.entry().map(|entry| self.function_map[&entry]);
        Ok(program)
    }

    fn plan_collection_storages(program: &mut RirProgram) {
        for (index, ty) in program.types.iter().enumerate() {
            let (kind, prefix) = match ty {
                RirType::List(elem_ty) => {
                    (RirCollectionStorageKind::List { elem_ty: *elem_ty }, "list")
                }
                RirType::Map { key, value } => (
                    RirCollectionStorageKind::Map {
                        key_ty: *key,
                        value_ty: *value,
                    },
                    "map",
                ),
                _ => continue,
            };
            let id = RirCollectionStorageId::from_index(program.collection_storages.len());
            program.collection_storages.push(RirCollectionStorage {
                id,
                value_ty: RirTypeId::from_index(index),
                kind,
                symbol: RirSymbol::new(format!("{prefix}_storage{index}")),
            });
        }
    }

    fn plan_types(&mut self, program: &mut RirProgram) -> Result<(), RustPlanError> {
        for index in 0..self.air.type_arena.len() {
            self.type_map
                .insert(TypeId::from_index(index), RirTypeId::from_index(index));
        }

        let mut aggregate_types = vec![];
        let mut extern_types = vec![];
        let mut enum_types = vec![];
        let mut dataref_types = vec![];
        let mut tuple_types = vec![];
        for (index, ty) in self.air.type_arena.iter().enumerate() {
            debug_assert_eq!(program.types.len(), index);
            let type_id = TypeId::from_index(index);
            let rir = match ty {
                TypeData::Int => RirType::Int,
                TypeData::Float => RirType::Float,
                TypeData::Bool => RirType::Bool,
                TypeData::String => RirType::String,
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
                TypeData::DataRef(aggregate) => {
                    let dataref_id = self.reserve_dataref(program, type_id, *aggregate)?;
                    dataref_types.push((type_id, *aggregate, dataref_id));
                    RirType::DataRef(dataref_id)
                }
                TypeData::Extern(ext) => {
                    let struct_id = self.reserve_extern_struct(program, type_id, *ext)?;
                    extern_types.push((type_id, *ext, struct_id));
                    RirType::Struct(struct_id)
                }
                TypeData::Array { elem, len } => RirType::Array {
                    elem: self.type_map[elem],
                    len: *len as u64,
                },
                TypeData::List(elem) => RirType::List(self.type_map[elem]),
                TypeData::Map { key, value, .. } if self.air_policy().map_supported(type_id) => {
                    RirType::Map {
                        key: self.type_map[key],
                        value: self.type_map[value],
                    }
                }
                TypeData::Optional(inner) => RirType::Option(self.type_map[inner]),
                TypeData::Tuple(elems) => {
                    let fields = elems
                        .iter()
                        .map(|elem| self.type_map[elem])
                        .collect::<Vec<_>>();
                    let tuple_id = self.intern_tuple(program, type_id, fields);
                    tuple_types.push((type_id, tuple_id));
                    RirType::Tuple(tuple_id)
                }
                TypeData::Slice(elem) => RirType::Slice(self.type_map[elem]),
                TypeData::Function(sig) => {
                    let sig = self.intern_lambda_sig(program, sig);
                    self.lambda_sig_map.insert(type_id, sig);
                    RirType::Lambda(sig)
                }
                TypeData::Any | TypeData::Map { .. } | TypeData::Dyn(_) => {
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
        for &(_, aggregate, dataref_id) in &dataref_types {
            self.fill_dataref(program, aggregate, dataref_id)?;
        }
        for &(type_id, enm, enum_id) in &enum_types {
            self.fill_enum(program, type_id, enm, enum_id)?;
        }
        let struct_types = aggregate_types
            .iter()
            .map(|(type_id, _, struct_id)| (*type_id, *struct_id))
            .chain(
                extern_types
                    .iter()
                    .map(|(type_id, _, struct_id)| (*type_id, *struct_id)),
            )
            .collect::<Vec<_>>();
        let enum_types = enum_types
            .iter()
            .map(|(type_id, _, enum_id)| (*type_id, *enum_id))
            .collect::<Vec<_>>();
        self.finalize_copyable_flags(program, &struct_types, &enum_types, &tuple_types);
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
                let semantic = rir::source_param_semantic(param.mode);
                RirLambdaParam {
                    ty: self.type_map[&param.ty],
                    semantic,
                    abi: RustRepPolicy::new(program).param_abi(semantic),
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
            copyable: true,
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
    ) -> Result<RirStructId, RustPlanError> {
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
            air_id: Some(aggregate),
            symbol: RirSymbol::new(format!(
                "{}T{}_{}",
                self.config.symbol_prefix,
                type_id.index(),
                sanitize(decl.name.as_str())
            )),
            display: RirSymbol::new(decl.name.as_str()),
            native_path: None,
            native_key: None,
            copyable: true,
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
    ) -> Result<(), RustPlanError> {
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
    ) -> Result<RirDataRefId, RustPlanError> {
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
            air_id: aggregate,
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
    ) -> Result<(), RustPlanError> {
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
    ) -> Result<RirStructId, RustPlanError> {
        let decl = self.air.extern_type(ext);
        if decl.rep != air::ExternRep::Inline {
            return Err(Self::gap(
                RustTargetGapSite::Type(type_id),
                RustTargetGapKind::UnsupportedType,
            ));
        }
        let id = RirStructId::from_index(program.structs.len());
        let native = self.native_type_binding(type_id, decl)?;
        program.structs.push(RirStruct {
            id,
            air_id: None,
            symbol: RirSymbol::new(format!(
                "{}T{}_{}",
                self.config.symbol_prefix,
                type_id.index(),
                sanitize(decl.name.as_str())
            )),
            display: RirSymbol::new(decl.name.as_str()),
            native_path: Some(native_path(&native.path)),
            native_key: Some(native.key.clone()),
            copyable: true,
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
    ) -> Result<(), RustPlanError> {
        let decl = self.air.extern_type(ext);
        let fields = match decl.constructor_fields() {
            Some(fields) => fields.map(|(_, field)| field).collect::<Vec<_>>(),
            None => decl.fields.iter().filter(|field| !field.computed).collect(),
        };
        let mut seen = vec![];
        let fields = fields
            .into_iter()
            .enumerate()
            .map(|(index, field)| {
                let Some(&ty) = self.type_map.get(&field.ty) else {
                    return Err(Self::gap(
                        RustTargetGapSite::Type(field.ty),
                        RustTargetGapKind::UnsupportedType,
                    ));
                };
                Ok(RirField {
                    id: RirFieldId::from_index(index),
                    symbol: scoped_symbol(field.name.as_str(), &mut seen),
                    ty,
                })
            })
            .collect::<Result<Vec<_>, RustPlanError>>()?;
        program.structs[struct_id.index()].fields = fields;
        Ok(())
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
            air_id: Some(enm),
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
            copyable: true,
            variants: vec![],
        });
        id
    }

    fn fill_enum(
        &self,
        program: &mut RirProgram,
        type_id: TypeId,
        enm: air::EnumId,
        enum_id: RirEnumId,
    ) -> Result<(), RustPlanError> {
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
            variants.push(RirVariant {
                id: RirVariantId::from_index(variant_index),
                symbol: scoped_symbol(variant.name.as_str(), &mut seen_variants),
                display: RirSymbol::new(variant.name.as_str()),
                kind,
                raw_value: variant.raw_value.as_ref().map(rir_raw_enum_value),
                fields,
            });
        }
        program.enums[enum_id.index()].variants = variants;
        Ok(())
    }

    fn finalize_copyable_flags(
        &self,
        program: &mut RirProgram,
        structs: &[(TypeId, RirStructId)],
        enums: &[(TypeId, RirEnumId)],
        tuples: &[(TypeId, RirTupleId)],
    ) {
        for &(type_id, id) in structs {
            program.structs[id.index()].copyable = self.rust_copyable_air_type(type_id);
        }
        for &(type_id, id) in enums {
            program.enums[id.index()].copyable = self.rust_copyable_air_type(type_id);
        }
        for &(type_id, id) in tuples {
            program.tuples[id.index()].copyable = self.rust_copyable_air_type(type_id);
        }

        while self.refine_copyable_flags(program, structs, enums, tuples) {}
    }

    fn refine_copyable_flags(
        &self,
        program: &mut RirProgram,
        structs: &[(TypeId, RirStructId)],
        enums: &[(TypeId, RirEnumId)],
        tuples: &[(TypeId, RirTupleId)],
    ) -> bool {
        let mut changed = false;
        for &(type_id, id) in structs {
            let copyable = self.rust_copyable_air_type(type_id)
                && Self::fields_copyable(program, &program.structs[id.index()].fields);
            changed |= set_if_changed(&mut program.structs[id.index()].copyable, copyable);
        }
        for &(type_id, id) in enums {
            let copyable = self.rust_copyable_air_type(type_id)
                && program.enums[id.index()]
                    .variants
                    .iter()
                    .all(|variant| Self::fields_copyable(program, &variant.fields));
            changed |= set_if_changed(&mut program.enums[id.index()].copyable, copyable);
        }
        for &(type_id, id) in tuples {
            let copyable = self.rust_copyable_air_type(type_id)
                && Self::fields_copyable(program, &program.tuples[id.index()].fields);
            changed |= set_if_changed(&mut program.tuples[id.index()].copyable, copyable);
        }
        changed
    }

    fn fields_copyable(program: &RirProgram, fields: &[RirField]) -> bool {
        let policy = RustRepPolicy::new(program);
        fields.iter().all(|field| policy.copyable(field.ty))
    }

    fn enum_field(
        &self,
        enum_ty: TypeId,
        ty: TypeId,
        index: usize,
    ) -> Result<RirField, RustPlanError> {
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

    fn plan_stringify_helpers(&self, program: &mut RirProgram) -> Result<(), RustPlanError> {
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

    fn require_stringify(&self, program: &mut RirProgram, ty: TypeId) -> Result<(), RustPlanError> {
        let rir_ty = self.type_map[&ty];
        let kind = match self.air.type_arena.data(ty) {
            TypeData::Int | TypeData::Float | TypeData::Bool | TypeData::String => return Ok(()),
            TypeData::Aggregate(aggregate) => {
                if program.stringify_reqs.iter().any(|req| req.ty == rir_ty) {
                    return Ok(());
                }
                self.classify_aggregate_stringify(program, ty, *aggregate)?
            }
            _ => {
                return Err(Self::gap(
                    RustTargetGapSite::Type(ty),
                    RustTargetGapKind::UnsupportedStructuralStringify,
                ));
            }
        };
        let id = RirStringifyReqId::from_index(program.stringify_reqs.len());
        program.stringify_reqs.push(RirStringifyReq {
            id,
            ty: rir_ty,
            kind,
        });
        Ok(())
    }

    fn classify_aggregate_stringify(
        &self,
        program: &mut RirProgram,
        ty: TypeId,
        aggregate: air::AggregateId,
    ) -> Result<RirStringifyReqKind, RustPlanError> {
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
                    mode: RirParamSemantic::SharedBorrow,
                }),
                ParamMode::Value if self.rust_copyable_air_type(ty) => {
                    Ok(RirStringifyReqKind::Override {
                        function: self.function_map[&function],
                        mode: RirParamSemantic::Value,
                    })
                }
                ParamMode::Value | ParamMode::MutBorrow => Err(Self::gap(
                    RustTargetGapSite::Function(function),
                    RustTargetGapKind::NonCopyValueRequired,
                )),
            };
        }
        let helper = self.require_structural_helper(program, ty, aggregate)?;
        Ok(RirStringifyReqKind::Structural(helper))
    }

    fn require_structural_helper(
        &self,
        program: &mut RirProgram,
        ty: TypeId,
        aggregate: air::AggregateId,
    ) -> Result<RirStringifyHelperId, RustPlanError> {
        let rir_ty = self.type_map[&ty];
        if let Some(helper) = program
            .stringify_helpers
            .iter()
            .find(|helper| helper.ty == rir_ty)
        {
            return Ok(helper.id);
        }
        let decl = self.air.aggregate(aggregate);
        if decl.kind != air::AggregateKind::Struct {
            return Err(Self::gap(
                RustTargetGapSite::Type(ty),
                RustTargetGapKind::UnsupportedStructuralStringify,
            ));
        }
        for field in &decl.fields {
            self.require_stringify(program, field.ty)?;
        }
        let id = RirStringifyHelperId::from_index(program.stringify_helpers.len());
        program.stringify_helpers.push(RirStringifyHelper {
            id,
            ty: rir_ty,
            symbol: RirSymbol::new(format!(
                "{}stringify_{}",
                self.config.symbol_prefix,
                type_suffix(program, rir_ty)
            )),
        });
        Ok(id)
    }

    fn plan_consts(&mut self, program: &mut RirProgram) {
        for index in 0..self.air.const_arena.len() {
            let air_id = ConstId::from_index(index);
            let konst = self.air.const_arena.get(air_id);
            let id = RirConstId::from_index(program.consts.len());
            let value = match &konst.value {
                ConstValue::Int(value) => RirConstValue::Int(*value),
                ConstValue::Float(value) => RirConstValue::Float(*value),
                ConstValue::Bool(value) => RirConstValue::Bool(*value),
                ConstValue::String(value) => RirConstValue::String(value.to_string()),
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

    fn plan_externs(&mut self, program: &mut RirProgram) -> Result<(), RustPlanError> {
        for index in 0..self.air.externs.len() {
            let air_id = ExternId::from_index(index);
            let decl = self.air.extern_decl(air_id);
            let id = RirExternId::from_index(program.externs.len());
            let native = self.native_extern(air_id, decl)?;
            let params = self.extern_params(decl, &native);
            let kind = Self::extern_kind(&native);
            program.externs.push(RirExtern {
                id,
                symbol: RirSymbol::new(format!(
                    "{}_extern_{}",
                    self.config.symbol_prefix,
                    sanitize(decl.name.as_str())
                )),
                kind,
                params,
                ret: self.type_map[&decl.return_type],
            });
            self.extern_map.insert(air_id, id);
        }
        Ok(())
    }

    fn extern_params(
        &self,
        decl: &air::ExternDecl,
        native: &native::ResolvedExtern<'_>,
    ) -> Vec<RirExternParam> {
        decl.call_params()
            .zip(&native.params)
            .map(|(param, abi)| RirExternParam {
                ty: self.type_map[&param.ty],
                semantic: abi.semantic,
                abi: abi.abi,
                escape: rir_param_escape(param.escape),
            })
            .collect()
    }

    fn extern_kind(native: &native::ResolvedExtern<'_>) -> RirExternKind {
        RirExternKind::Native(RirNativeExtern {
            path: native_path(&native.binding.path),
            abi: native.binding.abi.clone(),
        })
    }

    fn native_type_binding(
        &self,
        type_id: TypeId,
        decl: &air::ExternTypeDecl,
    ) -> Result<&anvyx_runtime::RustTypeBinding, RustPlanError> {
        let Some(binding) = &decl.binding else {
            return Err(Self::gap(
                RustTargetGapSite::Type(type_id),
                RustTargetGapKind::UnsupportedExtern,
            ));
        };
        native::type_binding(&self.config.native_providers, binding).ok_or_else(|| {
            Self::gap(
                RustTargetGapSite::Type(type_id),
                RustTargetGapKind::UnsupportedExtern,
            )
        })
    }

    fn native_extern(
        &self,
        id: ExternId,
        decl: &air::ExternDecl,
    ) -> Result<native::ResolvedExtern<'_>, RustPlanError> {
        native::resolve_extern(&self.config.native_providers, decl).map_err(|error| {
            let kind = match error {
                native::ResolveExternError::UnsupportedExtern => {
                    RustTargetGapKind::UnsupportedExtern
                }
                native::ResolveExternError::UnsupportedRustAbi => {
                    RustTargetGapKind::UnsupportedRustAbi
                }
            };
            Self::gap(RustTargetGapSite::Extern(id), kind)
        })
    }

    fn plan_function_ids(&mut self) {
        for index in 0..self.air.functions.len() {
            let air_id = FunctionId::from_index(index);
            self.function_map
                .insert(air_id, RirFunctionId::from_index(index));
        }
    }

    fn plan_globals(&mut self, program: &mut RirProgram) -> Result<(), RustPlanError> {
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
                air_id,
                module: decl.module,
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

    fn plan_scoped_place_cells(&mut self, program: &mut RirProgram) {
        for (index, borrow) in self.air.scoped_borrows.iter().enumerate() {
            let air_id = air::ScopedBorrowId::from_index(index);
            let id = RirScopedPlaceCellId::from_index(program.scoped_place_cells.len());
            self.scoped_place_cell_map.insert(air_id, id);
            let air::ScopedBorrowSource::SourceMutParam { local } = borrow.source;
            program.scoped_place_cells.push(RirScopedPlaceCellDecl {
                id,
                owner: self.function_map[&borrow.owner],
                source_local: RirLocalId::from_index(local.index()),
                payload_ty: self.type_map[&borrow.ty],
                symbol: RirSymbol::new(format!("__scoped{}", id.index())),
            });
        }
    }

    fn plan_lambdas(&mut self, program: &mut RirProgram) -> Result<(), RustPlanError> {
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
            let id = Self::push_zero_env_lambda(
                program,
                RirLambdaSource::Function(air_id),
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
            let id = Self::push_lambda(
                program,
                RirLambdaSource::Lambda(lambda),
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

    fn check_lambda_sig_storage_support(&self, program: &RirProgram) -> Result<(), RustPlanError> {
        let policy = RustRepPolicy::new(program);
        for sig in &program.lambda_sigs {
            if !policy.lambda_sig_has_heap_env(sig.id) || !policy.lambda_sig_needs_lifetime(sig.id)
            {
                continue;
            }
            let lambda = program
                .lambdas_for_sig(sig.id)
                .find(|lambda| matches!(lambda.storage, RirLambdaStorage::HeapEnv { .. }))
                .expect("heap-env signature must have a heap-env lambda");
            return Err(Self::gap(
                self.lambda_gap_site(lambda),
                RustTargetGapKind::UnsupportedLambdaCapture,
            ));
        }
        Ok(())
    }

    fn check_lambda_env_storage_support(&self, program: &RirProgram) -> Result<(), RustPlanError> {
        let policy = RustRepPolicy::new(program);
        for env in &program.lambda_envs {
            for field in &env.fields {
                let RirType::Lambda(sig) = program.types[field.ty.index()] else {
                    continue;
                };
                if policy.lambda_sig_needs_lifetime(sig) {
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
        match lambda.source {
            RirLambdaSource::Lambda(id) => {
                RustTargetGapSite::Function(self.air.lambdas[id.index()].owner)
            }
            RirLambdaSource::Function(id) => RustTargetGapSite::Function(id),
        }
    }

    fn check_lambda_value_capture_cycles(&self, program: &RirProgram) -> Result<(), RustPlanError> {
        for lambda in &program.lambdas {
            for capture in &lambda.captures {
                let Some(sig) = Self::value_capture_sig(program, capture) else {
                    continue;
                };
                let mut visited = vec![];
                if Self::lambda_sig_reaches_value_capture(program, sig, lambda.sig, &mut visited) {
                    return Err(Self::gap(
                        self.lambda_gap_site(lambda),
                        RustTargetGapKind::UnsupportedLambdaCapture,
                    ));
                }
            }
        }
        Ok(())
    }

    fn value_capture_sig(
        program: &RirProgram,
        capture: &RirLambdaCapture,
    ) -> Option<RirLambdaSigId> {
        if capture.abi != RirParamAbi::Value {
            return None;
        }
        match program.types[capture.ty.index()] {
            RirType::Lambda(sig) => Some(sig),
            _ => None,
        }
    }

    fn lambda_sig_reaches_value_capture(
        program: &RirProgram,
        from: RirLambdaSigId,
        target: RirLambdaSigId,
        visited: &mut Vec<RirLambdaSigId>,
    ) -> bool {
        if from == target {
            return true;
        }
        if visited.contains(&from) {
            return false;
        }
        visited.push(from);
        program.lambdas_for_sig(from).any(|lambda| {
            lambda
                .captures
                .iter()
                .filter_map(|capture| Self::value_capture_sig(program, capture))
                .any(|sig| Self::lambda_sig_reaches_value_capture(program, sig, target, visited))
        })
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

    fn plan_function_type_copyability(&mut self, program: &RirProgram) {
        let policy = RustRepPolicy::new(program);
        self.function_type_copyable.clear();
        self.function_type_shareable.clear();
        for (ty, sig) in &self.lambda_sig_map {
            let copyable = policy.lambda_sig_copyable(*sig);
            self.function_type_copyable.insert(*ty, copyable);
            self.function_type_shareable
                .insert(*ty, copyable || policy.lambda_sig_cloneable(*sig));
        }
    }

    fn plan_lambda_captures(
        &mut self,
        program: &RirProgram,
        lambda: air::LambdaId,
        decl: &air::LambdaDecl,
    ) -> Result<PlannedLambdaCaptures, RustPlanError> {
        let policy = RustRepPolicy::new(program);
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
            if !policy.supports_param(capture.ty, capture.semantic) {
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
    ) -> Result<Option<RirLambdaCapture>, RustPlanError> {
        let policy = RustRepPolicy::new(program);
        match capture {
            air::LambdaCaptureDecl::NoRuntime { .. } => Ok(None),
            air::LambdaCaptureDecl::ReadonlyLocal { ty, .. } => {
                if !self.rust_shareable_air_type(*ty) {
                    return Err(Self::gap(
                        RustTargetGapSite::Function(owner),
                        RustTargetGapKind::UnsupportedLambdaCapture,
                    ));
                }
                let semantic =
                    if escape == air::LambdaEscape::Escaping || self.rust_copyable_air_type(*ty) {
                        RirParamSemantic::Value
                    } else {
                        RirParamSemantic::SharedBorrow
                    };
                let ty = self.type_map[ty];
                Ok(Some(RirLambdaCapture {
                    ty,
                    semantic,
                    abi: policy.param_abi(semantic),
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
                    semantic: RirParamSemantic::ScopedPlaceCell,
                    abi: policy.param_abi(RirParamSemantic::ScopedPlaceCell),
                    kind: RirLambdaCaptureKind::ScopedPlaceCell {
                        cell: self.scoped_place_cell_map[borrow],
                    },
                }))
            }
            air::LambdaCaptureDecl::CaptureCell { cell, ty, .. } => {
                let ty = self.type_map[ty];
                let cell = self.capture_cell_map[cell];
                let storage = program.cells[cell.index()].storage;
                let (semantic, kind) = match storage {
                    RirCellStorage::StackScoped if escape == air::LambdaEscape::NonEscaping => (
                        RirParamSemantic::StackCell,
                        RirLambdaCaptureKind::StackCell { cell },
                    ),
                    RirCellStorage::Heap => (
                        RirParamSemantic::HeapCell,
                        RirLambdaCaptureKind::HeapCell { cell },
                    ),
                    RirCellStorage::StackScoped => {
                        return Err(Self::gap(
                            RustTargetGapSite::Function(owner),
                            RustTargetGapKind::UnsupportedLambdaCapture,
                        ));
                    }
                };
                Ok(Some(RirLambdaCapture {
                    ty,
                    semantic,
                    abi: policy.param_abi(semantic),
                    kind,
                }))
            }
        }
    }

    fn push_zero_env_lambda(
        program: &mut RirProgram,
        source: RirLambdaSource,
        function: RirFunctionId,
        sig: RirLambdaSigId,
        escape: RirLambdaEscape,
    ) -> RirLambdaId {
        Self::push_lambda(program, source, function, sig, escape, vec![], vec![])
    }

    fn push_lambda(
        program: &mut RirProgram,
        source: RirLambdaSource,
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
        program.lambdas.push(RirLambda {
            id,
            source,
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
        &self,
        air_id: FunctionId,
        program: &RirProgram,
    ) -> Result<RirFunction, RustPlanError> {
        let function = self.air.function(air_id);
        let mut locals = function
            .locals
            .iter()
            .enumerate()
            .map(|(index, local)| RirLocal {
                id: RirLocalId::from_index(index),
                ty: self.type_map[&local.ty],
                mutable: local.mutability == Mutability::Mutable,
                symbol: local_symbol(index, local.name.as_ref()),
                initialized: local.kind == LocalKind::Arg,
                payload_ref: false,
            })
            .collect::<Vec<_>>();
        for param in &function.signature.params {
            if let Some(local) = locals.get_mut(param.local_id.index()) {
                local.initialized = true;
            }
        }
        let policy = RustRepPolicy::new(program);
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
                    mutable: capture.semantic == RirParamSemantic::MutBorrow,
                    symbol: local_symbol(local.index(), None),
                    initialized: true,
                    payload_ref: false,
                });
                params.push(RirParam {
                    local,
                    ty: capture.ty,
                    semantic: capture.semantic,
                    abi: capture.abi,
                    escape: RirParamEscape::NonEscaping,
                });
                debug_assert_eq!(local.index(), function.locals.len() + index);
            }
        }
        params.extend(function.signature.params.iter().map(|param| {
            let ty = self.type_map[&param.ty];
            let semantic = rir::source_param_semantic(param.mode);
            RirParam {
                local: RirLocalId::from_index(param.local_id.index()),
                ty,
                semantic,
                abi: policy.param_abi(semantic),
                escape: rir_param_escape(param.escape),
            }
        }));
        let mut lambda_values = vec![None; locals.len()];
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
            &mut lambda_values,
            &mut initialized_cells,
            &mut possible_cells,
            false,
        )?;
        Ok(RirFunction {
            id: self.function_map[&air_id],
            air_id: Some(air_id),
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

    fn plan_air_block(
        &self,
        function: FunctionId,
        block: &air::AirBlock,
        locals: &mut Vec<RirLocal>,
        lambda_values: &mut Vec<Option<KnownLambdaValue>>,
        initialized_cells: &mut [bool],
        possible_cells: &mut [bool],
        in_loop: bool,
    ) -> Result<RirStructuredBlock, RustPlanError> {
        let mut stmts = vec![];
        for (index, stmt) in block.stmts.iter().enumerate() {
            stmts.extend(self.plan_air_stmt(
                function,
                index,
                stmt,
                locals,
                lambda_values,
                initialized_cells,
                possible_cells,
                in_loop,
            )?);
        }
        let (tail_stmts, term) = self.plan_air_tail(function, &block.tail, locals);
        stmts.extend(tail_stmts);
        Ok(RirStructuredBlock { stmts, term })
    }

    fn plan_global_root_value(
        &self,
        function: FunctionId,
        global: GlobalId,
        value: &RValue,
        locals: &mut Vec<RirLocal>,
        lambda_values: &mut Vec<Option<KnownLambdaValue>>,
    ) -> Result<(Vec<RirStmt>, RirRValue), RustPlanError> {
        let planned = self.plan_rvalue(function, value, locals, lambda_values)?;
        let mut stmts = planned.stmts;
        let value = RirRValue::Use(self.rvalue_short_region_operand(
            function,
            planned.value,
            self.air.globals[global.index()].ty,
            locals,
            &mut stmts,
        ));
        Ok((stmts, value))
    }

    fn plan_air_stmt(
        &self,
        function: FunctionId,
        index: usize,
        stmt: &air::AirStmt,
        locals: &mut Vec<RirLocal>,
        lambda_values: &mut Vec<Option<KnownLambdaValue>>,
        initialized_cells: &mut [bool],
        possible_cells: &mut [bool],
        in_loop: bool,
    ) -> Result<Vec<RirStmt>, RustPlanError> {
        match stmt {
            air::AirStmt::Init { local, value } => {
                let mut planned = self.plan_rvalue(function, value, locals, lambda_values)?;
                if locals[local.index()].mutable
                    && let RirRValue::SliceView { mutable, .. } = &mut planned.value
                {
                    *mutable = true;
                }
                let known = Self::known_lambda_rvalue(&planned.value);
                let mut stmts = planned.stmts;
                stmts.push(RirStmt::Init {
                    local: RirLocalId::from_index(local.index()),
                    value: planned.value,
                });
                stmts.extend(planned.post_stmts);
                Self::set_known_lambda(lambda_values, RirLocalId::from_index(local.index()), known);
                Ok(stmts)
            }
            air::AirStmt::Assign { dst, value } => {
                let planned = self.plan_rvalue(function, value, locals, lambda_values)?;
                let known = Self::known_lambda_rvalue(&planned.value);
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
                stmts.extend(planned.post_stmts);
                Self::set_place_known_lambda(lambda_values, dst, known);
                Ok(stmts)
            }
            air::AirStmt::Eval(value) => {
                let planned = self.plan_rvalue(function, value, locals, lambda_values)?;
                let mut stmts = planned.stmts;
                stmts.push(RirStmt::Eval(planned.value));
                stmts.extend(planned.post_stmts);
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
                let (mut stmts, value) =
                    self.plan_global_root_value(function, *global, value, locals, lambda_values)?;
                stmts.push(RirStmt::GlobalSetRoot {
                    global: self.global_map[global],
                    value,
                });
                Ok(stmts)
            }
            air::AirStmt::GlobalUpdateRoot { global, value } => {
                let (mut stmts, value) =
                    self.plan_global_root_value(function, *global, value, locals, lambda_values)?;
                let global_decl = &self.air.globals[global.index()];
                if matches!(
                    self.air.type_arena.data(global_decl.ty),
                    TypeData::List(_) | TypeData::Map { .. }
                ) {
                    stmts.push(RirStmt::MutPlaceSet {
                        place: RirMutPlaceArg::global(
                            self.global_map[global],
                            self.type_map[&global_decl.ty],
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
                let cond = self.plan_operand_read(function, &branch.cond, locals);
                let entry_lambdas = lambda_values.clone();
                let entry_cells = initialized_cells.to_vec();
                let entry_possible = possible_cells.to_vec();
                let mut then_lambdas = entry_lambdas.clone();
                let mut then_cells = entry_cells.clone();
                let mut then_possible = entry_possible.clone();
                let then_block = self.plan_air_block(
                    function,
                    &branch.then_block,
                    locals,
                    &mut then_lambdas,
                    &mut then_cells,
                    &mut then_possible,
                    in_loop,
                )?;
                let (else_block, else_lambdas, else_cells, else_possible) = match &branch.else_block
                {
                    Some(block) => {
                        let mut else_lambdas = entry_lambdas.clone();
                        let mut else_cells = entry_cells.clone();
                        let mut else_possible = entry_possible.clone();
                        let block = self.plan_air_block(
                            function,
                            block,
                            locals,
                            &mut else_lambdas,
                            &mut else_cells,
                            &mut else_possible,
                            in_loop,
                        )?;
                        (Some(block), else_lambdas, else_cells, else_possible)
                    }
                    None => (None, entry_lambdas, entry_cells, entry_possible),
                };
                Self::merge_known_lambdas(
                    lambda_values,
                    locals.len(),
                    [&then_lambdas, &else_lambdas],
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
                let entry_lambdas = lambda_values.clone();
                let entry_cells = initialized_cells.to_vec();
                let entry_possible = possible_cells.to_vec();
                let mut body_lambdas = entry_lambdas.clone();
                let mut body_cells = entry_cells.clone();
                let mut body_possible = entry_possible.clone();
                let body = self.plan_air_block(
                    function,
                    &loop_.body,
                    locals,
                    &mut body_lambdas,
                    &mut body_cells,
                    &mut body_possible,
                    true,
                )?;
                Self::merge_known_lambdas(
                    lambda_values,
                    locals.len(),
                    [&entry_lambdas, &body_lambdas],
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
                Ok(vec![RirStmt::Loop(RirLoop {
                    id: RirLoopId::from_index(loop_.id.index()),
                    body,
                })])
            }
            air::AirStmt::CollectionLoan(loan) => {
                let root = self.lower_collection_loan_root(function, loan);
                let body = self.plan_air_block(
                    function,
                    &loan.body,
                    locals,
                    lambda_values,
                    initialized_cells,
                    possible_cells,
                    in_loop,
                )?;
                Ok(vec![RirStmt::CollectionLoanScope(RirCollectionLoanScope {
                    root: root.place,
                    root_kind: root.root_kind,
                    mode: root.mode,
                    body,
                })])
            }
            air::AirStmt::CollectionSlotScope(scope) => self.plan_collection_slot_scope(
                function,
                scope,
                locals,
                lambda_values,
                initialized_cells,
                possible_cells,
                in_loop,
            ),
            air::AirStmt::EnumMatch(match_) => {
                let discr = self.lower_place_read(function, &match_.discr, locals);
                let RirOperand::Place(discr_place) = discr.operand else {
                    unreachable!("place read returns a place operand")
                };
                let mut stmts = discr.stmts;
                let entry_lambdas = lambda_values.clone();
                let entry_cells = initialized_cells.to_vec();
                let entry_possible = possible_cells.to_vec();
                let mut states = vec![];
                let mut cell_states = vec![];
                let mut possible_states = vec![];
                let arms = match_
                    .arms
                    .iter()
                    .map(|arm| {
                        let mut arm_lambdas = entry_lambdas.clone();
                        let mut arm_cells = entry_cells.clone();
                        let mut arm_possible = entry_possible.clone();
                        let block = self.plan_air_block(
                            function,
                            &arm.block,
                            locals,
                            &mut arm_lambdas,
                            &mut arm_cells,
                            &mut arm_possible,
                            in_loop,
                        )?;
                        states.push(arm_lambdas);
                        cell_states.push(arm_cells);
                        possible_states.push(arm_possible);
                        Ok(RirEnumMatchArm {
                            variant: RirVariantId::from_index(arm.variant.index()),
                            block,
                        })
                    })
                    .collect::<Result<Vec<_>, RustPlanError>>()?;
                let else_block = match &match_.else_block {
                    Some(block) => {
                        let mut else_lambdas = entry_lambdas.clone();
                        let mut else_cells = entry_cells.clone();
                        let mut else_possible = entry_possible.clone();
                        let block = self.plan_air_block(
                            function,
                            block,
                            locals,
                            &mut else_lambdas,
                            &mut else_cells,
                            &mut else_possible,
                            in_loop,
                        )?;
                        states.push(else_lambdas);
                        cell_states.push(else_cells);
                        possible_states.push(else_possible);
                        Some(block)
                    }
                    None => None,
                };
                if !states.is_empty() {
                    Self::merge_known_lambdas(lambda_values, locals.len(), states.iter());
                    for index in 0..initialized_cells.len() {
                        initialized_cells[index] = cell_states.iter().all(|state| state[index]);
                        possible_cells[index] = possible_states.iter().any(|state| state[index]);
                    }
                }
                stmts.push(RirStmt::EnumMatch(RirEnumMatch {
                    discr: discr_place,
                    arms,
                    else_block,
                }));
                Ok(stmts)
            }
            air::AirStmt::OptionalMatch(match_) => {
                let (mut stmts, subject) = self.plan_option_subject(function, match_, locals)?;
                let payload = match_
                    .payload
                    .map(|payload| RirLocalId::from_index(payload.index()));
                if match_.payload_ref
                    && let Some(payload) = payload
                    && let Some(local) = locals.get_mut(payload.index())
                {
                    local.payload_ref = true;
                }
                let entry_lambdas = lambda_values.clone();
                let entry_cells = initialized_cells.to_vec();
                let entry_possible = possible_cells.to_vec();
                let mut some_lambdas = entry_lambdas.clone();
                let mut some_cells = entry_cells.clone();
                let mut some_possible = entry_possible.clone();
                let some_block = self.plan_air_block(
                    function,
                    &match_.some_block,
                    locals,
                    &mut some_lambdas,
                    &mut some_cells,
                    &mut some_possible,
                    in_loop,
                )?;
                let mut none_lambdas = entry_lambdas;
                let mut none_cells = entry_cells;
                let mut none_possible = entry_possible;
                let none_block = self.plan_air_block(
                    function,
                    &match_.none_block,
                    locals,
                    &mut none_lambdas,
                    &mut none_cells,
                    &mut none_possible,
                    in_loop,
                )?;
                Self::merge_known_lambdas(
                    lambda_values,
                    locals.len(),
                    [&some_lambdas, &none_lambdas],
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
                    payload_ref: match_.payload_ref,
                    payload_escapes: match_.payload_escapes,
                    some_block,
                    none_block,
                }));
                Ok(stmts)
            }
            air::AirStmt::MapEntryMatch(match_) => {
                let mut key = self.plan_operand_read(function, &match_.key, locals);
                let payload = match_
                    .payload
                    .map(|payload| RirLocalId::from_index(payload.index()));
                if let Some(payload) = payload
                    && let Some(local) = locals.get_mut(payload.index())
                {
                    local.payload_ref = true;
                }
                let entry_lambdas = lambda_values.clone();
                let entry_cells = initialized_cells.to_vec();
                let entry_possible = possible_cells.to_vec();
                let mut some_lambdas = entry_lambdas.clone();
                let mut some_cells = entry_cells.clone();
                let mut some_possible = entry_possible.clone();
                let some_block = self.plan_air_block(
                    function,
                    &match_.some_block,
                    locals,
                    &mut some_lambdas,
                    &mut some_cells,
                    &mut some_possible,
                    in_loop,
                )?;
                let mut none_lambdas = entry_lambdas;
                let mut none_cells = entry_cells;
                let mut none_possible = entry_possible;
                let none_block = self.plan_air_block(
                    function,
                    &match_.none_block,
                    locals,
                    &mut none_lambdas,
                    &mut none_cells,
                    &mut none_possible,
                    in_loop,
                )?;
                Self::merge_known_lambdas(
                    lambda_values,
                    locals.len(),
                    [&some_lambdas, &none_lambdas],
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

    fn plan_option_subject(
        &self,
        function: FunctionId,
        match_: &air::AirOptionalMatch,
        locals: &mut Vec<RirLocal>,
    ) -> Result<(Vec<RirStmt>, RirOptionSubject), RustPlanError> {
        if !match_.payload_ref {
            let discr = self.lower_place_read(function, &match_.discr, locals);
            let RirOperand::Place(place) = discr.operand else {
                unreachable!("place read returns a place operand")
            };
            return Ok((discr.stmts, RirOptionSubject::Place(place)));
        }

        let plan = self
            .access()
            .plan(function, PlaceAccessIntent::PayloadAlias, &match_.discr)
            .map_err(|gap| Self::access_gap(function, gap))?;
        if plan.payload_alias_direct_place()
            && !Self::payload_ref_alias_subject(&match_.discr, locals)
        {
            let discr = self.lower_place_read(function, &match_.discr, locals);
            let RirOperand::Place(place) = discr.operand else {
                unreachable!("place read returns a place operand")
            };
            return Ok((discr.stmts, RirOptionSubject::Place(place)));
        }

        let planned = self.plan_mut_place_arg(function, &plan, locals)?;
        Ok((planned.stmts, RirOptionSubject::MutPlace(planned.arg)))
    }

    fn payload_ref_alias_subject(place: &Place, locals: &[RirLocal]) -> bool {
        let air::PlaceRoot::Local(local) = place.root else {
            return false;
        };
        locals
            .get(local.index())
            .is_some_and(|local| local.payload_ref)
    }

    fn set_known_lambda(
        lambda_values: &mut Vec<Option<KnownLambdaValue>>,
        local: RirLocalId,
        known: Option<KnownLambdaValue>,
    ) {
        if lambda_values.len() <= local.index() {
            lambda_values.resize(local.index() + 1, None);
        }
        lambda_values[local.index()] = known;
    }

    fn set_place_known_lambda(
        lambda_values: &mut Vec<Option<KnownLambdaValue>>,
        place: &Place,
        known: Option<KnownLambdaValue>,
    ) {
        let Some(local) = place.root.local() else {
            return;
        };
        Self::set_known_lambda(
            lambda_values,
            RirLocalId::from_index(local.index()),
            place.projection.is_empty().then_some(known).flatten(),
        );
    }

    fn merge_known_lambdas<'b>(
        lambda_values: &mut Vec<Option<KnownLambdaValue>>,
        len: usize,
        states: impl IntoIterator<Item = &'b Vec<Option<KnownLambdaValue>>>,
    ) {
        let states = states.into_iter().collect::<Vec<_>>();
        lambda_values.clear();
        lambda_values.resize(len, None);
        let Some(first_state) = states.first() else {
            return;
        };
        for (index, slot) in lambda_values.iter_mut().enumerate() {
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
        &self,
        function: FunctionId,
        tail: &air::AirTail,
        locals: &mut Vec<RirLocal>,
    ) -> (Vec<RirStmt>, RirTerm) {
        match tail {
            air::AirTail::None => (vec![], RirTerm::None),
            air::AirTail::Return(Some(value)) => {
                let planned = self.plan_operand_read(function, value, locals);
                (planned.stmts, RirTerm::Return(Some(planned.operand)))
            }
            air::AirTail::Return(None) => (vec![], RirTerm::Return(None)),
            air::AirTail::Unreachable => (vec![], RirTerm::Unreachable),
            air::AirTail::Break(id) => (vec![], RirTerm::Break(RirLoopId::from_index(id.index()))),
            air::AirTail::Continue(id) => {
                (vec![], RirTerm::Continue(RirLoopId::from_index(id.index())))
            }
        }
    }

    fn plan_rvalue(
        &self,
        function: FunctionId,
        value: &RValue,
        locals: &mut Vec<RirLocal>,
        lambda_values: &mut Vec<Option<KnownLambdaValue>>,
    ) -> Result<PlannedRValue, RustPlanError> {
        let planned = match value {
            RValue::Use(operand) => return self.plan_use(function, operand, locals, lambda_values),
            RValue::Unary { op, value, ty } => {
                let value = self.plan_operand_read(function, value, locals);
                PlannedRValue {
                    stmts: value.stmts,
                    value: RirRValue::Unary {
                        op: *op,
                        value: value.operand,
                        ty: self.type_map[ty],
                    },
                    post_stmts: vec![],
                }
            }
            RValue::Binary { op, lhs, rhs, ty } => {
                let lhs = self.plan_operand_read(function, lhs, locals);
                let rhs = self.plan_operand_read(function, rhs, locals);
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
                    post_stmts: vec![],
                }
            }
            RValue::SharedRefEq { lhs, rhs, negated } => {
                let lhs = self.plan_operand_read(function, lhs, locals);
                let rhs = self.plan_operand_read(function, rhs, locals);
                let mut stmts = lhs.stmts;
                stmts.extend(rhs.stmts);
                PlannedRValue {
                    stmts,
                    value: RirRValue::SharedRefEq {
                        lhs: lhs.operand,
                        rhs: rhs.operand,
                        negated: *negated,
                    },
                    post_stmts: vec![],
                }
            }
            RValue::Cast { value, target } => {
                let value = self.plan_operand_read(function, value, locals);
                PlannedRValue {
                    stmts: value.stmts,
                    value: RirRValue::Cast {
                        value: value.operand,
                        target: self.type_map[target],
                    },
                    post_stmts: vec![],
                }
            }
            RValue::OptionalSome { value, ty } => {
                let value = self.plan_operand_read(function, value, locals);
                PlannedRValue {
                    stmts: value.stmts,
                    value: RirRValue::OptionalSome {
                        value: value.operand,
                        ty: self.type_map[ty],
                    },
                    post_stmts: vec![],
                }
            }
            RValue::Call { callee, args } => {
                self.plan_call(function, callee, args, locals, lambda_values)?
            }
            RValue::Stringify { value, source_ty } => {
                let value = self.plan_operand_read(function, value, locals);
                PlannedRValue {
                    stmts: value.stmts,
                    value: RirRValue::Stringify {
                        value: value.operand,
                        source_ty: self.type_map[source_ty],
                    },
                    post_stmts: vec![],
                }
            }
            RValue::StringConcat { parts } => {
                let parts = self.plan_operands_read(function, parts, locals);
                PlannedRValue {
                    stmts: parts.stmts,
                    value: RirRValue::StringConcat {
                        parts: parts.operands,
                    },
                    post_stmts: vec![],
                }
            }
            RValue::Format { value, spec } => {
                let source_ty = self.type_map[&self.operand_ty(value)];
                let value = self.plan_operand_read(function, value, locals);
                PlannedRValue {
                    stmts: value.stmts,
                    value: RirRValue::Format {
                        value: value.operand,
                        source_ty,
                        spec: rir_format_spec(*spec),
                    },
                    post_stmts: vec![],
                }
            }
            RValue::Aggregate { kind, fields, ty } => {
                return self.plan_aggregate(function, kind, fields, *ty, locals);
            }
            RValue::Len { source } => {
                let source = self.lower_place_read(function, source, locals);
                let RirOperand::Place(source_place) = source.operand else {
                    unreachable!("place read returns a place operand")
                };
                PlannedRValue {
                    stmts: source.stmts,
                    value: RirRValue::Len {
                        source: source_place,
                    },
                    post_stmts: vec![],
                }
            }
            RValue::ListPush { list, value } => {
                let value = self.plan_operand_read(function, value, locals);
                let list = self.lower_structural_mutation_place(function, list);
                let mut stmts = value.stmts;
                stmts.extend(list.stmts);
                PlannedRValue {
                    stmts,
                    value: RirRValue::ListPush {
                        list: list.place,
                        value: value.operand,
                    },
                    post_stmts: list.post_stmts,
                }
            }
            RValue::SliceView {
                source,
                start,
                end,
                inclusive,
                ty,
            } => PlannedRValue::from_value(RirRValue::SliceView {
                source: self.plan_slice_view_source(function, source)?,
                start: RirLocalId::from_index(start.index()),
                end: RirLocalId::from_index(end.index()),
                inclusive: *inclusive,
                mutable: false,
                ty: self.type_map[ty],
            }),
            RValue::RangeListCopy {
                source,
                start,
                end,
                inclusive,
                ty,
            } => {
                let source = self.lower_place_read(function, source, locals);
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
                    post_stmts: vec![],
                }
            }
            RValue::MapGet { map, key, ty } => {
                let map = self.lower_place_read(function, map, locals);
                let key = self.plan_operand_read(function, key, locals);
                let RirOperand::Place(map_place) = map.operand else {
                    unreachable!("place read returns a place operand")
                };
                let mut stmts = map.stmts;
                stmts.extend(key.stmts);
                PlannedRValue {
                    stmts,
                    value: RirRValue::MapGet {
                        map: map_place,
                        key: key.operand,
                        ty: self.type_map[ty],
                    },
                    post_stmts: vec![],
                }
            }
            RValue::MapInsert {
                map,
                key,
                value,
                kind,
            } => {
                let key = self.plan_operand_read(function, key, locals);
                let value = self.plan_operand_read(function, value, locals);
                let map = match kind {
                    MapWriteKind::StructuralInsert => {
                        self.lower_structural_mutation_place(function, map)
                    }
                    MapWriteKind::IndexedAssignment => {
                        self.lower_collection_write_place(function, map)
                    }
                };
                let mut stmts = key.stmts;
                stmts.extend(value.stmts);
                stmts.extend(map.stmts);
                PlannedRValue {
                    stmts,
                    value: RirRValue::MapInsert {
                        map: map.place,
                        key: key.operand,
                        value: value.operand,
                        kind: rir_map_write_kind(*kind),
                    },
                    post_stmts: map.post_stmts,
                }
            }
            RValue::MapRemove { map, key, ty } => {
                let key = self.plan_operand_read(function, key, locals);
                let map = self.lower_structural_mutation_place(function, map);
                let mut stmts = key.stmts;
                stmts.extend(map.stmts);
                PlannedRValue {
                    stmts,
                    value: RirRValue::MapRemove {
                        map: map.place,
                        key: key.operand,
                        ty: self.type_map[ty],
                    },
                    post_stmts: map.post_stmts,
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
                let captures = self.plan_lambda_capture_args(function, captures, locals)?;
                PlannedRValue::from_value(RirRValue::Lambda {
                    lambda: self.lambda_map[lambda],
                    captures,
                    ty: self.type_map[ty],
                })
            }
            RValue::MapEntryAt { map, index, ty } => {
                let map = self.lower_place_read(function, map, locals);
                let RirOperand::Place(map_place) = map.operand else {
                    unreachable!("place read returns a place operand")
                };
                PlannedRValue {
                    stmts: map.stmts,
                    value: RirRValue::MapEntryAt {
                        map: map_place,
                        index: RirLocalId::from_index(index.index()),
                        ty: self.type_map[ty],
                    },
                    post_stmts: vec![],
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

    fn plan_lambda_capture_args(
        &self,
        function: FunctionId,
        captures: &[air::LambdaCaptureArg],
        locals: &mut Vec<RirLocal>,
    ) -> Result<Vec<RirLambdaCaptureArg>, RustPlanError> {
        let mut planned = vec![];
        for capture in captures {
            match capture {
                air::LambdaCaptureArg::NoRuntime => {}
                air::LambdaCaptureArg::ReadonlyLocal { value } => {
                    let value = self.plan_operand_read(function, value, locals);
                    if !value.stmts.is_empty() {
                        return Err(Self::gap(
                            RustTargetGapSite::Function(function),
                            RustTargetGapKind::UnsupportedLambdaCapture,
                        ));
                    }
                    planned.push(RirLambdaCaptureArg::Readonly {
                        value: value.operand,
                    });
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
        Ok(planned)
    }

    fn air_place_value_readable(&self, ty: TypeId) -> bool {
        self.rust_copyable_air_type(ty) || self.rust_shareable_air_type(ty)
    }

    fn known_lambda_rvalue(value: &RirRValue) -> Option<KnownLambdaValue> {
        let RirRValue::Lambda {
            lambda,
            captures,
            ty,
        } = value
        else {
            return None;
        };
        captures.is_empty().then_some(KnownLambdaValue {
            lambda: *lambda,
            ty: *ty,
        })
    }

    fn known_lambda_place(
        &self,
        place: &Place,
        lambda_values: &[Option<KnownLambdaValue>],
    ) -> Option<KnownLambdaValue> {
        if place.projection.is_empty()
            && matches!(self.air.type_arena.data(place.ty), TypeData::Function(_))
            && let Some(local) = place.root.local()
        {
            return lambda_values.get(local.index()).copied().flatten();
        }
        None
    }

    fn unbound_lambda_temp_place(&self, function: FunctionId, place: &Place) -> bool {
        if !place.projection.is_empty()
            || !matches!(self.air.type_arena.data(place.ty), TypeData::Function(_))
        {
            return false;
        }
        let Some(local) = place.root.local() else {
            return false;
        };
        self.air.function(function).locals[local.index()]
            .binding
            .is_none()
    }

    fn plan_use(
        &self,
        function: FunctionId,
        operand: &Operand,
        locals: &mut Vec<RirLocal>,
        lambda_values: &[Option<KnownLambdaValue>],
    ) -> Result<PlannedRValue, RustPlanError> {
        let Operand::Place(place) = operand else {
            return Ok(PlannedRValue::from_value(RirRValue::Use(
                self.plan_operand(function, operand),
            )));
        };
        if place.projection.is_empty() {
            if let Some(cell) = self.place_capture_cell(function, place) {
                return Ok(PlannedRValue::from_value(RirRValue::CellGetCopy {
                    cell: self.capture_cell_ref(function, cell),
                    ty: self.type_map[&place.ty],
                }));
            }
            if let Some(borrow) = self.place_scoped_borrow(function, place) {
                return Ok(PlannedRValue::from_value(RirRValue::ScopedPlaceCellGet {
                    cell: self.scoped_place_cell_ref(function, borrow),
                    ty: self.type_map[&place.ty],
                }));
            }
        }
        if self
            .access()
            .plan(function, PlaceAccessIntent::ReadValue, place)
            .expect("profile verifies readable places")
            .dataref_plan()
            .is_some()
        {
            let planned = self.lower_place_read(function, place, locals);
            return Ok(PlannedRValue {
                stmts: planned.stmts,
                value: RirRValue::Use(planned.operand),
                post_stmts: vec![],
            });
        }
        if !self.air_place_value_readable(place.ty) {
            if let Some(known) = self.known_lambda_place(place, lambda_values) {
                return Ok(PlannedRValue::from_value(known.rvalue()));
            }
            if !self.unbound_lambda_temp_place(function, place) {
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
        let decl = self.air.aggregate(*aggregate);
        Ok(PlannedRValue::from_value(RirRValue::Struct {
            ty: self.type_map[&place.ty],
            fields: decl
                .fields
                .iter()
                .enumerate()
                .map(|(index, field)| {
                    let mut field_place = place.clone();
                    field_place
                        .projection
                        .push(Projection::Field(air::FieldId::from_index(index)));
                    field_place.ty = field.ty;
                    RirOperand::Place(self.plan_place_in_function(function, &field_place))
                })
                .collect(),
        }))
    }

    fn plan_aggregate(
        &self,
        function: FunctionId,
        kind: &AggregateCtor,
        fields: &[Operand],
        ty: TypeId,
        locals: &mut Vec<RirLocal>,
    ) -> Result<PlannedRValue, RustPlanError> {
        let fields = self.plan_operands_read(function, fields, locals);
        let value = match kind {
            AggregateCtor::Struct(_) | AggregateCtor::Extern(_) => RirRValue::Struct {
                ty: self.type_map[&ty],
                fields: fields.operands,
            },
            AggregateCtor::EnumVariant { variant, .. } => RirRValue::EnumVariant {
                ty: self.type_map[&ty],
                variant: RirVariantId::from_index(variant.index()),
                fields: fields.operands,
            },
            AggregateCtor::Array => RirRValue::Array {
                ty: self.type_map[&ty],
                elems: fields.operands,
            },
            AggregateCtor::List => RirRValue::List {
                ty: self.type_map[&ty],
                elems: fields.operands,
            },
            AggregateCtor::Tuple => RirRValue::Tuple {
                ty: self.type_map[&ty],
                fields: fields.operands,
            },
            AggregateCtor::Map if fields.operands.len().is_multiple_of(2) => RirRValue::Map {
                ty: self.type_map[&ty],
                entries: fields
                    .operands
                    .chunks_exact(2)
                    .map(|entry| (entry[0].clone(), entry[1].clone()))
                    .collect(),
            },
            AggregateCtor::DataRef(_) => RirRValue::DataRefAlloc {
                ty: self.type_map[&ty],
                fields: fields.operands,
            },
            AggregateCtor::Map => {
                return Err(Self::gap(
                    RustTargetGapSite::Function(function),
                    RustTargetGapKind::UnsupportedRValue,
                ));
            }
        };
        Ok(PlannedRValue {
            stmts: fields.stmts,
            value,
            post_stmts: vec![],
        })
    }

    fn plan_call(
        &self,
        function_id: FunctionId,
        callee: &Callee,
        args: &[CallArg],
        locals: &mut Vec<RirLocal>,
        lambda_values: &mut Vec<Option<KnownLambdaValue>>,
    ) -> Result<PlannedRValue, RustPlanError> {
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
                        .map(|param| rir::source_param_semantic(param.mode))
                        .collect::<Vec<_>>(),
                )
            }
            Callee::Extern(id) => {
                let ext = self.air.extern_decl(*id);
                let native = self.native_extern(*id, ext)?;
                (
                    RirCallTarget::Extern(self.extern_map[id]),
                    self.type_map[&ext.return_type],
                    vec![],
                    native.params.iter().map(|param| param.semantic).collect(),
                )
            }
            Callee::Lambda(operand) => {
                let air_ty = self.operand_ty(operand);
                let TypeData::Function(sig_ty) = self.air.type_arena.data(air_ty) else {
                    return Err(Self::gap(
                        RustTargetGapSite::Function(function_id),
                        RustTargetGapKind::UnsupportedLambdaCall,
                    ));
                };
                let callee = self.plan_operand_read(function_id, operand, locals);
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
                        .map(|param| rir::source_param_semantic(param.mode))
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
            let planned = self.plan_arg(function_id, arg, expected, locals, lambda_values)?;
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
            post_stmts: vec![],
        })
    }

    fn plan_collection_slot_scope(
        &self,
        function: FunctionId,
        scope: &air::AirCollectionSlotScope,
        locals: &mut Vec<RirLocal>,
        lambda_values: &mut Vec<Option<KnownLambdaValue>>,
        initialized_cells: &mut [bool],
        possible_cells: &mut [bool],
        in_loop: bool,
    ) -> Result<Vec<RirStmt>, RustPlanError> {
        let body = self.plan_air_block(
            function,
            &scope.body,
            locals,
            lambda_values,
            initialized_cells,
            possible_cells,
            in_loop,
        )?;
        let (body, _) = self.collection_slot_block(function, scope, body, true)?;
        Ok(vec![RirStmt::CollectionSlotScope(body)])
    }

    fn collection_slot_block(
        &self,
        function: FunctionId,
        scope: &air::AirCollectionSlotScope,
        mut body: RirStructuredBlock,
        init: bool,
    ) -> Result<(RirStructuredBlock, bool), RustPlanError> {
        let mut stmts = if init {
            self.collection_slot_reads(function, scope, true)
        } else {
            vec![]
        };
        let mut first = init;
        let mut block_updates_slot = false;
        for stmt in std::mem::take(&mut body.stmts) {
            if first {
                first = false;
            } else {
                stmts.extend(self.collection_slot_reads(function, scope, false));
            }
            let (stmt, stmt_updates_slot) = self.collection_slot_stmt(function, scope, stmt)?;
            block_updates_slot |= stmt_updates_slot;
            stmts.push(stmt);
            if stmt_updates_slot {
                stmts.extend(self.collection_slot_writes(function, scope));
            }
        }
        body.stmts = stmts;
        Ok((body, block_updates_slot))
    }

    fn collection_slot_stmt(
        &self,
        function: FunctionId,
        scope: &air::AirCollectionSlotScope,
        stmt: RirStmt,
    ) -> Result<(RirStmt, bool), RustPlanError> {
        Ok(match stmt {
            RirStmt::If(mut branch) => {
                let (then_block, mut updates_slot) =
                    self.collection_slot_block(function, scope, branch.then_block, false)?;
                branch.then_block = then_block;
                if let Some(block) = branch.else_block {
                    let (block, else_updates_slot) =
                        self.collection_slot_block(function, scope, block, false)?;
                    updates_slot |= else_updates_slot;
                    branch.else_block = Some(block);
                }
                (RirStmt::If(branch), updates_slot)
            }
            RirStmt::Loop(mut loop_) => {
                let (body, updates_slot) =
                    self.collection_slot_block(function, scope, loop_.body, false)?;
                loop_.body = body;
                (RirStmt::Loop(loop_), updates_slot)
            }
            RirStmt::CollectionLoanScope(mut loan) => {
                let (body, updates_slot) =
                    self.collection_slot_block(function, scope, loan.body, false)?;
                loan.body = body;
                (RirStmt::CollectionLoanScope(loan), updates_slot)
            }
            RirStmt::CollectionSlotScope(block) => {
                let (block, updates_slot) =
                    self.collection_slot_block(function, scope, block, false)?;
                (RirStmt::CollectionSlotScope(block), updates_slot)
            }
            RirStmt::EnumMatch(mut match_) => {
                let mut updates_slot = false;
                for arm in &mut match_.arms {
                    let (block, arm_updates_slot) = self.collection_slot_block(
                        function,
                        scope,
                        std::mem::take(&mut arm.block),
                        false,
                    )?;
                    updates_slot |= arm_updates_slot;
                    arm.block = block;
                }
                if let Some(block) = match_.else_block {
                    let (block, else_updates_slot) =
                        self.collection_slot_block(function, scope, block, false)?;
                    updates_slot |= else_updates_slot;
                    match_.else_block = Some(block);
                }
                (RirStmt::EnumMatch(match_), updates_slot)
            }
            RirStmt::OptionMatch(mut match_) => {
                let (some_block, some_updates_slot) =
                    self.collection_slot_block(function, scope, match_.some_block, false)?;
                let (none_block, none_updates_slot) =
                    self.collection_slot_block(function, scope, match_.none_block, false)?;
                match_.some_block = some_block;
                match_.none_block = none_block;
                (
                    RirStmt::OptionMatch(match_),
                    some_updates_slot || none_updates_slot,
                )
            }
            RirStmt::MapEntryMatch(mut match_) => {
                let (some_block, some_updates_slot) =
                    self.collection_slot_block(function, scope, match_.some_block, false)?;
                let (none_block, none_updates_slot) =
                    self.collection_slot_block(function, scope, match_.none_block, false)?;
                match_.some_block = some_block;
                match_.none_block = none_block;
                (
                    RirStmt::MapEntryMatch(match_),
                    some_updates_slot || none_updates_slot,
                )
            }
            stmt => {
                let updates_slot = Self::direct_stmt_updates_collection_slot(scope, &stmt);
                (stmt, updates_slot)
            }
        })
    }

    fn direct_stmt_updates_collection_slot(
        scope: &air::AirCollectionSlotScope,
        stmt: &RirStmt,
    ) -> bool {
        match stmt {
            RirStmt::Assign { dst, .. } => Self::place_is_collection_slot(scope, dst),
            RirStmt::Eval(value)
            | RirStmt::Init { value, .. }
            | RirStmt::CellInit { value, .. }
            | RirStmt::CellSet { value, .. }
            | RirStmt::ScopedPlaceCellSet { value, .. } => {
                Self::rvalue_updates_collection_slot(scope, value)
            }
            _ => false,
        }
    }

    fn rvalue_updates_collection_slot(
        scope: &air::AirCollectionSlotScope,
        value: &RirRValue,
    ) -> bool {
        match value {
            RirRValue::Call { args, .. } => args
                .iter()
                .any(|arg| Self::call_arg_updates_collection_slot(scope, arg)),
            _ => false,
        }
    }

    fn call_arg_updates_collection_slot(
        scope: &air::AirCollectionSlotScope,
        arg: &RirCallArg,
    ) -> bool {
        match arg {
            RirCallArg::MutBorrow(place) => Self::place_is_collection_slot(scope, place),
            RirCallArg::MutPlace(arg) => match &arg.access {
                RirMutPlaceAccess::Handle(RirMutPlaceHandle::Local { local, ty }) => {
                    Self::place_is_collection_slot(
                        scope,
                        &RirPlace::local(*local, arg.projections.clone(), *ty),
                    )
                }
                _ => false,
            },
            _ => false,
        }
    }

    fn place_is_collection_slot(scope: &air::AirCollectionSlotScope, place: &RirPlace) -> bool {
        let RirPlaceRoot::Local(local) = place.root else {
            unreachable!("expected a local RIR place")
        };
        scope
            .slots
            .iter()
            .any(|slot| RirLocalId::from_index(slot.local.index()) == local)
    }

    fn collection_slot_reads(
        &self,
        function: FunctionId,
        scope: &air::AirCollectionSlotScope,
        init: bool,
    ) -> Vec<RirStmt> {
        scope
            .slots
            .iter()
            .map(|slot| {
                let local = RirLocalId::from_index(slot.local.index());
                let value = self.collection_slot_read(function, scope, slot);
                if init {
                    RirStmt::Init { local, value }
                } else {
                    RirStmt::Assign {
                        dst: RirPlace::local(local, vec![], self.type_map[&slot.ty]),
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
    ) -> Vec<RirStmt> {
        scope
            .slots
            .iter()
            .filter(|slot| slot.mutable)
            .map(|slot| self.collection_slot_write(function, scope, slot))
            .collect()
    }

    fn collection_slot_read(
        &self,
        function: FunctionId,
        scope: &air::AirCollectionSlotScope,
        slot: &air::AirCollectionSlot,
    ) -> RirRValue {
        match slot.kind {
            air::AirCollectionSlotKind::SequenceElement => RirRValue::Use(RirOperand::Place(
                self.collection_slot_place(function, scope, slot),
            )),
            air::AirCollectionSlotKind::MapValue => RirRValue::MapValueAt {
                map: self.plan_place_in_function(function, &scope.root),
                index: RirLocalId::from_index(scope.index.index()),
                ty: self.type_map[&slot.ty],
            },
        }
    }

    fn collection_slot_write(
        &self,
        function: FunctionId,
        scope: &air::AirCollectionSlotScope,
        slot: &air::AirCollectionSlot,
    ) -> RirStmt {
        let local = RirLocalId::from_index(slot.local.index());
        let value = RirOperand::Place(RirPlace::local(local, vec![], self.type_map[&slot.ty]));
        match slot.kind {
            air::AirCollectionSlotKind::SequenceElement => RirStmt::Assign {
                dst: self.collection_slot_place(function, scope, slot),
                value: RirRValue::Use(value),
            },
            air::AirCollectionSlotKind::MapValue => RirStmt::MapValueSet {
                map: self.plan_place_in_function(function, &scope.root),
                index: RirLocalId::from_index(scope.index.index()),
                value,
            },
        }
    }

    fn collection_slot_place(
        &self,
        function: FunctionId,
        scope: &air::AirCollectionSlotScope,
        slot: &air::AirCollectionSlot,
    ) -> RirPlace {
        match slot.kind {
            air::AirCollectionSlotKind::SequenceElement => {
                let mut place = self.plan_place_in_function(function, &scope.root);
                place
                    .projections
                    .push(RirProjection::Index(RirLocalId::from_index(
                        scope.index.index(),
                    )));
                place.ty = self.type_map[&slot.ty];
                place
            }
            air::AirCollectionSlotKind::MapValue => unreachable!("map value slots are not places"),
        }
    }

    fn moves_bound_noncopy_lambda(&self, function: FunctionId, operand: &Operand) -> bool {
        let Operand::Place(place) = operand else {
            return false;
        };
        let Some(local) = place.root.local() else {
            return false;
        };
        self.air.function(function).locals[local.index()]
            .binding
            .is_some()
            && !self.air_place_value_readable(place.ty)
    }

    fn plan_arg(
        &self,
        function: FunctionId,
        arg: &CallArg,
        expected: RirParamSemantic,
        locals: &mut Vec<RirLocal>,
        lambda_values: &mut Vec<Option<KnownLambdaValue>>,
    ) -> Result<PlannedCallArg, RustPlanError> {
        match arg {
            CallArg::Value(operand) if expected == RirParamSemantic::ScopedLambda => {
                let air_ty = self.operand_ty(operand);
                let TypeData::Function(_) = self.air.type_arena.data(air_ty) else {
                    return Err(Self::gap(
                        RustTargetGapSite::Function(function),
                        RustTargetGapKind::UnsupportedLambdaExternBoundary,
                    ));
                };
                let planned = self.plan_operand_read(function, operand, locals);
                Ok(PlannedCallArg {
                    stmts: planned.stmts,
                    arg: RirCallArg::ScopedLambda {
                        callee: planned.operand,
                        sig: self.lambda_sig_map[&air_ty],
                    },
                })
            }
            CallArg::Value(operand) => {
                if let Operand::Place(place) = operand
                    && !self.air_place_value_readable(place.ty)
                    && let Some(known) = self.known_lambda_place(place, lambda_values)
                {
                    let local = self.alloc_temp(locals, place.ty);
                    Self::set_known_lambda(lambda_values, local, Some(known));
                    return Ok(PlannedCallArg {
                        stmts: vec![RirStmt::Init {
                            local,
                            value: known.rvalue(),
                        }],
                        arg: RirCallArg::Value(RirOperand::Place(RirPlace::local(
                            local,
                            vec![],
                            known.ty,
                        ))),
                    });
                }
                if self.moves_bound_noncopy_lambda(function, operand) {
                    return Err(Self::gap(
                        RustTargetGapSite::Function(function),
                        RustTargetGapKind::NonCopyValueRequired,
                    ));
                }
                let planned = self.plan_operand_read(function, operand, locals);
                Ok(PlannedCallArg {
                    stmts: planned.stmts,
                    arg: RirCallArg::Value(planned.operand),
                })
            }
            CallArg::SharedBorrow(place) => {
                let planned = self.lower_place_read(function, place, locals);
                let RirOperand::Place(place) = planned.operand else {
                    unreachable!("place read returns a place operand")
                };
                Ok(PlannedCallArg {
                    stmts: planned.stmts,
                    arg: RirCallArg::SharedBorrow(place),
                })
            }
            CallArg::SharedStringConst(id) => Ok(PlannedCallArg::from_arg(
                RirCallArg::SharedStringConst(self.const_map[id]),
            )),
            CallArg::MutBorrow(place) if expected == RirParamSemantic::MutPlace => {
                self.plan_source_mut_place_arg(function, place, locals)
            }
            CallArg::MutBorrow(place) => self.plan_native_mut_borrow_arg(function, place),
        }
    }

    fn plan_operand_read(
        &self,
        function: FunctionId,
        operand: &Operand,
        locals: &mut Vec<RirLocal>,
    ) -> PlannedOperand {
        match operand {
            Operand::Place(place) => self.lower_place_read(function, place, locals),
            Operand::Const(id) => {
                PlannedOperand::from_operand(RirOperand::Const(self.const_map[id]))
            }
        }
    }

    fn plan_operands_read(
        &self,
        function: FunctionId,
        operands: &[Operand],
        locals: &mut Vec<RirLocal>,
    ) -> PlannedOperands {
        let mut stmts = vec![];
        let mut planned = vec![];
        for operand in operands {
            let next = self.plan_operand_read(function, operand, locals);
            stmts.extend(next.stmts);
            planned.push(next.operand);
        }
        PlannedOperands {
            stmts,
            operands: planned,
        }
    }

    fn plan_operand(&self, function: FunctionId, operand: &Operand) -> RirOperand {
        match operand {
            Operand::Place(place) => {
                RirOperand::Place(self.plan_place_in_function(function, place))
            }
            Operand::Const(id) => RirOperand::Const(self.const_map[id]),
        }
    }

    fn lower_place_read(
        &self,
        function: FunctionId,
        place: &Place,
        locals: &mut Vec<RirLocal>,
    ) -> PlannedOperand {
        let plan = self
            .access()
            .plan(function, PlaceAccessIntent::ReadValue, place)
            .expect("profile verifies readable places");
        if matches!(plan.root, PlaceAccessRoot::Global(_)) && plan.dataref_plan().is_none() {
            let mut stmts = vec![];
            let operand = self.rvalue_temp(
                RirRValue::Use(RirOperand::Place(
                    self.plan_place_in_function(function, place),
                )),
                place.ty,
                locals,
                &mut stmts,
            );
            return PlannedOperand { stmts, operand };
        }
        if plan.dataref_plan().is_none() {
            match plan.root {
                PlaceAccessRoot::CaptureCell(cell) => {
                    let root_ty = self.air.capture_cells[cell.index()].ty;
                    return self.lower_temp_root_read(
                        RirRValue::CellGetCopy {
                            cell: self.capture_cell_ref(function, cell),
                            ty: self.type_map[&root_ty],
                        },
                        root_ty,
                        place,
                        locals,
                    );
                }
                PlaceAccessRoot::ScopedPlaceCell(borrow) => {
                    let root_ty = self.air.scoped_borrows[borrow.index()].ty;
                    return self.lower_temp_root_read(
                        RirRValue::ScopedPlaceCellGet {
                            cell: self.scoped_place_cell_ref(function, borrow),
                            ty: self.type_map[&root_ty],
                        },
                        root_ty,
                        place,
                        locals,
                    );
                }
                PlaceAccessRoot::Local {
                    source_mut_param: true,
                    ..
                } => {
                    let mut stmts = vec![];
                    let operand = self.rvalue_temp(
                        RirRValue::Use(RirOperand::Place(
                            self.plan_place_in_function(function, place),
                        )),
                        place.ty,
                        locals,
                        &mut stmts,
                    );
                    return PlannedOperand { stmts, operand };
                }
                PlaceAccessRoot::Local { .. } | PlaceAccessRoot::LambdaCapture(_) => {
                    return PlannedOperand::from_operand(RirOperand::Place(
                        self.plan_place_in_function(function, place),
                    ));
                }
                PlaceAccessRoot::Global(_) => unreachable!("global roots returned above"),
            }
        }
        self.lower_dataref_place_read(function, &plan, locals)
    }

    fn lower_dataref_place_read(
        &self,
        function: FunctionId,
        plan: &PlaceAccessPlan,
        locals: &mut Vec<RirLocal>,
    ) -> PlannedOperand {
        let dataref = plan.dataref_plan().expect("dataref reads have a plan");
        let mut stmts = vec![];
        let mut current_place =
            self.dataref_plan_object_prefix(function, plan, dataref, locals, &mut stmts);
        for (index, segment) in dataref.segments.iter().enumerate() {
            let next = self.dataref_segment(
                current_place,
                segment,
                index == 0 && dataref.object_prefix_can_fail,
            );
            current_place = self.read_dataref_segment(function, next, locals, &mut stmts);
        }
        for projection in &dataref.remaining {
            current_place
                .projections
                .push(Self::rir_place_projection(projection));
            current_place.ty = self.type_map[&projection.ty];
        }
        PlannedOperand {
            stmts,
            operand: RirOperand::Place(current_place),
        }
    }

    fn lower_temp_root_read(
        &self,
        value: RirRValue,
        root_ty: TypeId,
        place: &Place,
        locals: &mut Vec<RirLocal>,
    ) -> PlannedOperand {
        let mut stmts = vec![];
        if place.projection.is_empty() {
            let operand = self.rvalue_temp(value, place.ty, locals, &mut stmts);
            return PlannedOperand { stmts, operand };
        }
        let root = self.rvalue_temp_place(value, root_ty, locals, &mut stmts);
        let RirPlaceRoot::Local(root_local) = root.root else {
            unreachable!("temporary roots are local")
        };
        PlannedOperand {
            stmts,
            operand: RirOperand::Place(RirPlace::local(
                root_local,
                place.projection.iter().map(Self::rir_projection).collect(),
                self.type_map[&place.ty],
            )),
        }
    }

    fn lower_structural_mutation_place(&self, function: FunctionId, place: &Place) -> PlannedPlace {
        let plan = self
            .access()
            .plan(function, PlaceAccessIntent::StructuralMutation, place)
            .expect("profile verifies structural mutations");
        if matches!(plan.root, PlaceAccessRoot::Global(_)) {
            return PlannedPlace {
                stmts: vec![],
                place: self.plan_access_place(function, &plan, place),
                post_stmts: vec![],
            };
        }
        PlannedPlace {
            stmts: vec![],
            place: self.plan_place_in_function(function, place),
            post_stmts: vec![],
        }
    }

    fn lower_collection_write_place(&self, function: FunctionId, place: &Place) -> PlannedPlace {
        let plan = self
            .access()
            .plan(function, PlaceAccessIntent::Assign, place)
            .expect("profile verifies collection writes");
        PlannedPlace {
            stmts: vec![],
            place: self.plan_access_place(function, &plan, place),
            post_stmts: vec![],
        }
    }

    fn lower_collection_loan_root(
        &self,
        function: FunctionId,
        loan: &air::AirCollectionLoan,
    ) -> PlannedCollectionLoanRoot {
        let plan = self
            .access()
            .collection_loan_plan(function, loan)
            .expect("profile verifies collection loan roots");
        PlannedCollectionLoanRoot {
            place: self.plan_access_place(function, &plan.place, &loan.root),
            root_kind: rir_collection_root_kind(plan.root_kind),
            mode: rir_collection_loan_mode(plan.mode),
        }
    }

    fn assign_target(plan: &PlaceAccessPlan, place: &Place) -> AssignTarget {
        if plan.dataref_plan().is_some() {
            return AssignTarget::DataRef;
        }
        match plan.root {
            PlaceAccessRoot::CaptureCell(cell) if place.projection.is_empty() => {
                AssignTarget::CaptureCell(cell)
            }
            PlaceAccessRoot::CaptureCell(_) => {
                unreachable!("profile rejects projected capture-cell places")
            }
            PlaceAccessRoot::ScopedPlaceCell(borrow) if place.projection.is_empty() => {
                AssignTarget::ScopedPlaceCell(borrow)
            }
            PlaceAccessRoot::ScopedPlaceCell(_) => {
                unreachable!("profile rejects projected scoped-borrow places")
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
        &self,
        function: FunctionId,
        place: &Place,
        value: RirRValue,
        locals: &mut Vec<RirLocal>,
        stmts: &mut Vec<RirStmt>,
        initialized_cells: &mut [bool],
        possible_cells: &mut [bool],
        in_loop: bool,
    ) -> Result<(), RustPlanError> {
        let plan = self
            .access()
            .plan(function, PlaceAccessIntent::Assign, place)
            .expect("profile verifies assignable places");
        if let Some(local) = place.root.local()
            && let Some(root) = locals.get(local.index())
            && root.payload_ref
        {
            stmts.push(RirStmt::MutPlaceSet {
                place: RirMutPlaceArg::projected(
                    RirMutPlaceHandle::Local {
                        local: RirLocalId::from_index(local.index()),
                        ty: root.ty,
                    },
                    place.projection.iter().map(Self::rir_projection).collect(),
                    self.type_map[&place.ty],
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
                    place,
                    value,
                    locals,
                    stmts,
                    initialized_cells,
                    possible_cells,
                    in_loop,
                )?;
            }
            AssignTarget::ScopedPlaceCell(borrow) => {
                let value = self.rvalue_temp(value, place.ty, locals, stmts);
                stmts.push(RirStmt::ScopedPlaceCellSet {
                    cell: self.scoped_place_cell_ref(function, borrow),
                    value: RirRValue::Use(value),
                });
            }
            AssignTarget::ProjectedGlobal(global) => {
                let global_decl = &self.air.globals[global.index()];
                stmts.push(RirStmt::MutPlaceSet {
                    place: RirMutPlaceArg::projected(
                        RirMutPlaceHandle::Global {
                            global: self.global_map[&global],
                            ty: self.type_map[&global_decl.ty],
                        },
                        place.projection.iter().map(Self::rir_projection).collect(),
                        self.type_map[&place.ty],
                    ),
                    value,
                });
            }
            AssignTarget::Assign { source_mut_param } => {
                let value = if source_mut_param {
                    RirRValue::Use(
                        self.rvalue_short_region_operand(function, value, place.ty, locals, stmts),
                    )
                } else {
                    value
                };
                stmts.push(RirStmt::Assign {
                    dst: self.plan_place_in_function(function, place),
                    value,
                });
            }
            AssignTarget::DataRef => {
                self.lower_dataref_write(function, &plan, place, value, locals, stmts);
            }
        }
        Ok(())
    }

    fn lower_capture_cell_write(
        &self,
        function: FunctionId,
        cell: air::CaptureCellId,
        place: &Place,
        value: RirRValue,
        locals: &mut Vec<RirLocal>,
        stmts: &mut Vec<RirStmt>,
        initialized_cells: &mut [bool],
        possible_cells: &mut [bool],
        in_loop: bool,
    ) -> Result<(), RustPlanError> {
        let cell_ref = self.capture_cell_ref(function, cell);
        if initialized_cells
            .get(cell.index())
            .copied()
            .unwrap_or(false)
        {
            let value = self.rvalue_short_region_operand(function, value, place.ty, locals, stmts);
            stmts.push(RirStmt::CellSet {
                cell: cell_ref,
                value: RirRValue::Use(value),
            });
        } else if in_loop || possible_cells.get(cell.index()).copied().unwrap_or(false) {
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
        &self,
        function: FunctionId,
        plan: &PlaceAccessPlan,
        place: &Place,
        value: RirRValue,
        locals: &mut Vec<RirLocal>,
        stmts: &mut Vec<RirStmt>,
    ) {
        let segment = self.final_dataref_segment(function, plan, locals, stmts);
        let value = self.rvalue_short_region_operand(function, value, place.ty, locals, stmts);
        stmts.push(RirStmt::DataRefSet {
            object: segment.object,
            dataref: segment.dataref,
            projections: segment.projections,
            value,
        });
    }

    fn plan_slice_view_source(
        &self,
        function: FunctionId,
        place: &Place,
    ) -> Result<RirPlace, RustPlanError> {
        if self
            .access()
            .plan(function, PlaceAccessIntent::SliceView, place)
            .is_err()
        {
            return Err(Self::gap(
                RustTargetGapSite::Function(function),
                RustTargetGapKind::UnsupportedSliceView,
            ));
        }
        Ok(self.plan_place_in_function(function, place))
    }

    fn dataref_runtime_root_place(
        &self,
        function: FunctionId,
        root: PlaceAccessRoot,
        ty: TypeId,
        locals: &mut Vec<RirLocal>,
        stmts: &mut Vec<RirStmt>,
    ) -> (TypeId, RirPlace) {
        match root {
            PlaceAccessRoot::CaptureCell(cell) => {
                let root = self.rvalue_temp_place(
                    RirRValue::CellGetCopy {
                        cell: self.capture_cell_ref(function, cell),
                        ty: self.type_map[&ty],
                    },
                    ty,
                    locals,
                    stmts,
                );
                (ty, root)
            }
            PlaceAccessRoot::ScopedPlaceCell(borrow) => {
                let root = self.rvalue_temp_place(
                    RirRValue::ScopedPlaceCellGet {
                        cell: self.scoped_place_cell_ref(function, borrow),
                        ty: self.type_map[&ty],
                    },
                    ty,
                    locals,
                    stmts,
                );
                (ty, root)
            }
            PlaceAccessRoot::Local {
                local,
                source_mut_param,
                ..
            } => {
                let root = self.rir_root_place(RirLocalId::from_index(local.index()), ty);
                if source_mut_param {
                    let root = self.rvalue_temp_place(
                        RirRValue::Use(RirOperand::Place(root)),
                        ty,
                        locals,
                        stmts,
                    );
                    (ty, root)
                } else {
                    (ty, root)
                }
            }
            PlaceAccessRoot::LambdaCapture(slot) => {
                let air::FunctionKind::Lambda(lambda) = self.air.function(function).kind else {
                    unreachable!("AIR verifier rejects capture roots outside lambdas")
                };
                let runtime = self.lambda_runtime_capture_slots[&(lambda, slot)];
                (
                    ty,
                    self.rir_root_place(
                        RirLocalId::from_index(self.air.function(function).locals.len() + runtime),
                        ty,
                    ),
                )
            }
            PlaceAccessRoot::Global(global) => {
                let root = self.rvalue_temp_place(
                    RirRValue::Use(RirOperand::Place(RirPlace::global(
                        self.global_map[&global],
                        vec![],
                        self.type_map[&ty],
                    ))),
                    ty,
                    locals,
                    stmts,
                );
                (ty, root)
            }
        }
    }

    fn rir_root_place(&self, local: RirLocalId, ty: TypeId) -> RirPlace {
        RirPlace::local(local, vec![], self.type_map[&ty])
    }

    fn dataref_mut_place_segment(
        &self,
        function: FunctionId,
        plan: &PlaceAccessPlan,
        locals: &mut Vec<RirLocal>,
        stmts: &mut Vec<RirStmt>,
    ) -> DataRefSegment {
        self.final_dataref_segment(function, plan, locals, stmts)
    }

    fn final_dataref_segment(
        &self,
        function: FunctionId,
        plan: &PlaceAccessPlan,
        locals: &mut Vec<RirLocal>,
        stmts: &mut Vec<RirStmt>,
    ) -> DataRefSegment {
        let dataref = plan.dataref_plan().expect("dataref places have a plan");
        let mut current_place =
            self.dataref_plan_object_prefix(function, plan, dataref, locals, stmts);
        let (last, prefix) = dataref
            .segments
            .split_last()
            .expect("dataref plan has at least one segment");
        for (index, segment) in prefix.iter().enumerate() {
            let next = self.dataref_segment(
                current_place,
                segment,
                index == 0 && dataref.object_prefix_can_fail,
            );
            current_place = self.read_dataref_segment(function, next, locals, stmts);
        }
        let segment = self.dataref_segment(
            current_place,
            last,
            prefix.is_empty() && dataref.object_prefix_can_fail,
        );
        self.prepare_dataref_segment_object(function, segment, locals, stmts)
    }

    fn dataref_plan_object_prefix(
        &self,
        function: FunctionId,
        plan: &PlaceAccessPlan,
        dataref: &DataRefProjectionPlan,
        locals: &mut Vec<RirLocal>,
        stmts: &mut Vec<RirStmt>,
    ) -> RirPlace {
        let (_, mut current_place) =
            self.dataref_runtime_root_place(function, plan.root, dataref.root_ty, locals, stmts);
        for projection in &dataref.object_prefix {
            current_place
                .projections
                .push(Self::rir_place_projection(projection));
            current_place.ty = self.type_map[&projection.ty];
        }
        current_place
    }

    fn dataref_segment(
        &self,
        object: RirPlace,
        segment: &DataRefSegmentPlan,
        object_must_materialize: bool,
    ) -> DataRefSegment {
        DataRefSegment {
            object: RirOperand::Place(object),
            object_ty: segment.dataref_ty,
            object_must_materialize,
            dataref: self.dataref_map[&segment.dataref],
            projections: segment
                .storage
                .iter()
                .map(Self::rir_place_projection)
                .collect(),
            ty: segment.storage_ty,
        }
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
        &self,
        function: FunctionId,
        segment: DataRefSegment,
        locals: &mut Vec<RirLocal>,
        stmts: &mut Vec<RirStmt>,
    ) -> RirPlace {
        let segment = self.prepare_dataref_segment_object(function, segment, locals, stmts);
        let local = self.alloc_temp(locals, segment.ty);
        stmts.push(RirStmt::Init {
            local,
            value: RirRValue::DataRefGet {
                object: segment.object,
                dataref: segment.dataref,
                projections: segment.projections,
                ty: self.type_map[&segment.ty],
            },
        });
        self.rir_root_place(local, segment.ty)
    }

    fn rvalue_short_region_operand(
        &self,
        function: FunctionId,
        value: RirRValue,
        ty: TypeId,
        locals: &mut Vec<RirLocal>,
        stmts: &mut Vec<RirStmt>,
    ) -> RirOperand {
        match value {
            RirRValue::Use(operand) if self.operand_uses_ctx(function, &operand) => {
                self.rvalue_temp(RirRValue::Use(operand), ty, locals, stmts)
            }
            RirRValue::Use(operand) => operand,
            value => self.rvalue_temp(value, ty, locals, stmts),
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
        self.rir_root_place(local, ty)
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
        self.air.scoped_borrow_root(function, place.root)
    }

    fn current_place_root(&self, function: FunctionId, place: &Place) -> (TypeId, RirLocalId) {
        match place.root {
            air::PlaceRoot::Local(local) => (
                self.air.function(function).locals[local.index()].ty,
                RirLocalId::from_index(local.index()),
            ),
            air::PlaceRoot::LambdaCapture(slot) => {
                let air::FunctionKind::Lambda(lambda) = self.air.function(function).kind else {
                    unreachable!("AIR verifier rejects capture roots outside lambdas")
                };
                let decl = &self.air.lambdas[lambda.index()].captures[slot.index()];
                let runtime = self.lambda_runtime_capture_slots[&(lambda, slot)];
                (
                    decl.ty(),
                    RirLocalId::from_index(self.air.function(function).locals.len() + runtime),
                )
            }
            air::PlaceRoot::ScopedBorrow(_)
            | air::PlaceRoot::CaptureCell(_)
            | air::PlaceRoot::Global(_) => {
                unreachable!("Rust backend profile rejects unsupported place roots")
            }
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
            initialized: false,
            payload_ref: false,
        });
        id
    }

    fn rir_place_projection(projection: &PlaceProjection) -> RirProjection {
        match projection.kind {
            PlaceProjectionKind::Field(field) | PlaceProjectionKind::DataRefField(field) => {
                RirProjection::Field(RirFieldId::from_index(field.index()))
            }
            PlaceProjectionKind::TupleField(index) => {
                RirProjection::TupleField(RirFieldId::from_index(index as usize))
            }
            PlaceProjectionKind::ArrayIndex(local)
            | PlaceProjectionKind::ListIndex(local)
            | PlaceProjectionKind::SliceIndex(local) => {
                RirProjection::Index(RirLocalId::from_index(local.index()))
            }
            PlaceProjectionKind::ExternField | PlaceProjectionKind::VariantField => {
                unreachable!("profile rejects unsupported projection")
            }
        }
    }

    fn rir_projection(projection: &Projection) -> RirProjection {
        match projection {
            Projection::Field(field) => RirProjection::Field(RirFieldId::from_index(field.index())),
            Projection::Index(local) => RirProjection::Index(RirLocalId::from_index(local.index())),
            Projection::TupleField(index) => {
                RirProjection::TupleField(RirFieldId::from_index(*index as usize))
            }
            Projection::VariantField { .. } => {
                unreachable!("profile rejects unsupported projection")
            }
        }
    }

    fn operand_ty(&self, operand: &Operand) -> TypeId {
        self.air
            .operand_ty(operand)
            .expect("verified AIR operand const should exist")
    }

    fn rust_copyable_air_type(&self, ty: TypeId) -> bool {
        if matches!(self.air.type_arena.data(ty), TypeData::Function(_)) {
            return self
                .function_type_copyable
                .get(&ty)
                .copied()
                .unwrap_or(true);
        }
        self.air_policy().copyable(ty)
    }

    fn rust_shareable_air_type(&self, ty: TypeId) -> bool {
        if matches!(self.air.type_arena.data(ty), TypeData::Function(_)) {
            return self
                .function_type_shareable
                .get(&ty)
                .copied()
                .unwrap_or_else(|| self.rust_copyable_air_type(ty));
        }
        self.air_policy().value_place_shareable(ty)
    }

    fn access(&self) -> PlaceAccessCx<'_> {
        PlaceAccessCx::new(self.air, &self.classes)
    }

    fn air_policy(&self) -> AirRustRepPolicy<'_> {
        AirRustRepPolicy::new(self.air, &self.classes)
    }

    fn plan_access_place(
        &self,
        function: FunctionId,
        plan: &PlaceAccessPlan,
        source: &Place,
    ) -> RirPlace {
        let root = match plan.root {
            PlaceAccessRoot::Global(global) => RirPlaceRoot::Global(self.global_map[&global]),
            PlaceAccessRoot::Local { local, .. } => {
                RirPlaceRoot::Local(RirLocalId::from_index(local.index()))
            }
            PlaceAccessRoot::CaptureCell(_)
            | PlaceAccessRoot::ScopedPlaceCell(_)
            | PlaceAccessRoot::LambdaCapture(_) => {
                return self.plan_place_in_function(function, source);
            }
        };
        RirPlace {
            root,
            projections: plan
                .projection
                .iter()
                .map(Self::rir_place_projection)
                .collect(),
            ty: self.type_map[&plan.ty],
        }
    }

    fn plan_place_in_function(&self, function: FunctionId, place: &Place) -> RirPlace {
        let root = match place.root {
            air::PlaceRoot::Global(global) => RirPlaceRoot::Global(self.global_map[&global]),
            air::PlaceRoot::Local(_)
            | air::PlaceRoot::LambdaCapture(_)
            | air::PlaceRoot::ScopedBorrow(_)
            | air::PlaceRoot::CaptureCell(_) => {
                let (_, local) = self.current_place_root(function, place);
                RirPlaceRoot::Local(local)
            }
        };
        RirPlace {
            root,
            projections: place.projection.iter().map(Self::rir_projection).collect(),
            ty: self.type_map[&place.ty],
        }
    }
}

fn set_if_changed(slot: &mut bool, value: bool) -> bool {
    let changed = *slot != value;
    *slot = value;
    changed
}

fn rir_param_escape(escape: ParamEscape) -> RirParamEscape {
    match escape {
        ParamEscape::NonEscaping => RirParamEscape::NonEscaping,
        ParamEscape::Escaping => RirParamEscape::Escaping,
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

fn rir_format_spec(spec: FormatSpec) -> RirFormatSpec {
    RirFormatSpec {
        fill: spec.fill,
        align: spec.align.map(|align| match align {
            FormatAlign::Left => RirFormatAlign::Left,
            FormatAlign::Right => RirFormatAlign::Right,
            FormatAlign::Center => RirFormatAlign::Center,
        }),
        sign: match spec.sign {
            FormatSign::Default => RirFormatSign::Default,
            FormatSign::Always => RirFormatSign::Always,
        },
        zero_pad: spec.zero_pad,
        width: spec.width,
        precision: spec.precision,
        kind: match spec.kind {
            FormatKind::Default => RirFormatKind::Default,
            FormatKind::Hex => RirFormatKind::Hex,
            FormatKind::HexUpper => RirFormatKind::HexUpper,
            FormatKind::Binary => RirFormatKind::Binary,
            FormatKind::Exp => RirFormatKind::Exp,
            FormatKind::ExpUpper => RirFormatKind::ExpUpper,
        },
    }
}

fn rir_core_enum_kind(kind: air::CoreEnumKind) -> RirCoreEnumKind {
    match kind {
        air::CoreEnumKind::Option => RirCoreEnumKind::Option,
    }
}

fn rir_enum_repr(repr: air::EnumRepr) -> RirEnumRepr {
    match repr {
        air::EnumRepr::Adt => RirEnumRepr::Adt,
        air::EnumRepr::RawInt => RirEnumRepr::RawInt,
        air::EnumRepr::RawString => RirEnumRepr::RawString,
    }
}

fn rir_raw_enum_value(value: &air::RawEnumValue) -> RirRawEnumValue {
    match value {
        air::RawEnumValue::Int(value) => RirRawEnumValue::Int(*value),
        air::RawEnumValue::String(value) => RirRawEnumValue::String(value.clone()),
    }
}

fn native_path(path: &RustPath) -> Vec<String> {
    let mut out = vec![path.crate_name.clone()];
    out.extend(path.segments.clone());
    out
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
) -> Result<RirSymbol, RustPlanError> {
    let name = sanitize(function.name.as_str());
    let mut symbol = match method_owner_name(function, air) {
        Some(owner) => format!("{prefix}_f{}_{}_{}", id.index(), sanitize(owner), name),
        None => format!("{prefix}_f{}_{}", id.index(), name),
    };
    if let Some(specialization) = &function.specialization {
        let mut parts = vec![];
        for ty in &specialization.type_args {
            let Some(&rir_ty) = type_map.get(ty) else {
                return Err(RustPlanError::TargetGaps(vec![RustTargetGap {
                    site: RustTargetGapSite::Function(id),
                    kind: RustTargetGapKind::UnsupportedType,
                }]));
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
        ConstValue::Float(value) => sanitize(&format!("f{value}")),
        ConstValue::Bool(value) => value.to_string(),
        ConstValue::String(value) => sanitize(value).to_ascii_lowercase(),
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
