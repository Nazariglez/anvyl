mod analysis;
pub mod cargo_job;
pub mod emit;
mod place;
pub mod profile;
pub mod rep_policy;
pub mod rir;
#[cfg(test)]
mod source_job;
mod syntax;
mod target;
mod value;
mod write;

use std::{collections::HashMap, error::Error, fmt};

use anvyx_frontend::{
    air::{
        self, AggregateCtor, CallArg, Callee, ConstId, ConstValue, ExternId, FunctionId, LocalId,
        LocalKind, Mutability, Operand, ParamEscape, ParamMode, Place, Projection, RValue,
        TypeData, TypeId, TypePassClasses, VerifiedProgram,
    },
    ast::{FormatAlign, FormatKind, FormatSign, FormatSpec, Ident},
};
use anvyx_runtime::{RustAbiSupport, RustExternBinding, RustPath, RustProviderSupport};

use self::{
    profile::{ProfileErrorKind, ProfileSite, RustBackendProfile, RustBackendProfileError},
    rep_policy::{AirRustRepPolicy, RustRepPolicy},
    rir::{
        RirCallArg, RirCallTarget, RirCellDecl, RirCellId, RirCellRef, RirCellStorage, RirConst,
        RirConstId, RirConstValue, RirCoreEnumKind, RirCtxPlan, RirDataRef, RirDataRefId, RirEnum,
        RirEnumId, RirEnumMatch, RirEnumMatchArm, RirEnumRepr, RirExtern, RirExternId,
        RirExternKind, RirExternParam, RirField, RirFieldId, RirFormatAlign, RirFormatKind,
        RirFormatSign, RirFormatSpec, RirFunction, RirFunctionId, RirIf, RirLambda,
        RirLambdaCapture, RirLambdaCaptureArg, RirLambdaCaptureKind, RirLambdaEscape, RirLambdaId,
        RirLambdaParam, RirLambdaSig, RirLambdaSigId, RirLambdaSource, RirLambdaStorage, RirLocal,
        RirLocalId, RirLoop, RirLoopId, RirNativeExtern, RirOperand, RirOptionMatch, RirParam,
        RirParamAbi, RirParamEscape, RirParamSemantic, RirPlace, RirProgram, RirProjection,
        RirRValue, RirRawEnumValue, RirReturn, RirStmt, RirStringifyHelper, RirStringifyHelperId,
        RirStringifyReq, RirStringifyReqId, RirStringifyReqKind, RirStruct, RirStructId,
        RirStructuredBlock, RirSymbol, RirTerm, RirTuple, RirTupleId, RirType, RirTypeId,
        RirVariant, RirVariantId, RirVariantKind, VerifiedRirProgram,
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
    UnsupportedPlaceRoot,
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
    NonCopyValueRequired,
    UnsupportedStructuralStringify,
    UnsupportedContextBorrowAcrossCall,
    UnsupportedProviderNativeRepresentation,
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
                ProfileErrorKind::UnsupportedPlaceRoot => RustTargetGapKind::UnsupportedPlaceRoot,
                ProfileErrorKind::UnsupportedTerminator => RustTargetGapKind::UnsupportedTerminator,
                ProfileErrorKind::UnsupportedRValue => RustTargetGapKind::UnsupportedRValue,
                ProfileErrorKind::UnsupportedCallee => RustTargetGapKind::UnsupportedCallee,
                ProfileErrorKind::UnsupportedExtern => RustTargetGapKind::UnsupportedExtern,
                ProfileErrorKind::UnsupportedExternMember => {
                    RustTargetGapKind::UnsupportedExternMember
                }
                ProfileErrorKind::UnsupportedEntry => RustTargetGapKind::UnsupportedEntry,
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
    function_lambda_map: HashMap<FunctionId, RirLambdaId>,
    lambda_map: HashMap<air::LambdaId, RirLambdaId>,
    function_type_copyable: HashMap<TypeId, bool>,
    lambda_runtime_capture_slots: HashMap<(air::LambdaId, air::LambdaCaptureSlotId), usize>,
    capture_cell_map: HashMap<air::CaptureCellId, RirCellId>,
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

struct PlannedCallArg {
    stmts: Vec<RirStmt>,
    arg: RirCallArg,
    post_stmts: Vec<RirStmt>,
}

struct DataRefSegment {
    object: RirOperand,
    dataref: RirDataRefId,
    projections: Vec<RirProjection>,
    ty: TypeId,
    next_index: usize,
}

impl PlannedCallArg {
    fn from_arg(arg: RirCallArg) -> Self {
        Self {
            stmts: vec![],
            arg,
            post_stmts: vec![],
        }
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
            function_lambda_map: HashMap::new(),
            lambda_map: HashMap::new(),
            function_type_copyable: HashMap::new(),
            lambda_runtime_capture_slots: HashMap::new(),
            capture_cell_map: HashMap::new(),
            extern_map: HashMap::new(),
            dataref_map: HashMap::new(),
            enum_map: HashMap::new(),
            tuple_map: HashMap::new(),
        }
    }

    fn check_support(&self) -> Result<(), RustPlanError> {
        RustBackendProfile::check(self.verified).map_err(|errors| {
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
        self.plan_consts(&mut program);
        self.plan_externs(&mut program)?;
        self.plan_function_ids();
        self.plan_cells(&mut program);
        self.plan_lambdas(&mut program)?;
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
                let semantic = rir::semantic_from_air(param.mode);
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
            let params = self.extern_params(program, decl);
            let kind = self.extern_kind(air_id, decl)?;
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

    fn extern_params(&self, program: &RirProgram, decl: &air::ExternDecl) -> Vec<RirExternParam> {
        let policy = RustRepPolicy::new(program);
        decl.call_params()
            .map(|param| {
                let ty = self.type_map[&param.ty];
                let semantic = rir::semantic_from_air(param.mode);
                RirExternParam {
                    ty,
                    semantic,
                    abi: policy.param_abi(semantic),
                }
            })
            .collect()
    }

    fn extern_kind(
        &self,
        air_id: ExternId,
        decl: &air::ExternDecl,
    ) -> Result<RirExternKind, RustPlanError> {
        if let Some(binding) = &decl.binding {
            let native = self.native_binding(binding).ok_or_else(|| {
                Self::gap(
                    RustTargetGapSite::Extern(air_id),
                    RustTargetGapKind::UnsupportedExtern,
                )
            })?;
            if native.abi.support != RustAbiSupport::Direct {
                return Err(Self::gap(
                    RustTargetGapSite::Extern(air_id),
                    RustTargetGapKind::UnsupportedRustAbi,
                ));
            }
            return Ok(RirExternKind::Native(RirNativeExtern {
                path: native_path(&native.path),
                abi: native.abi.clone(),
            }));
        }

        Err(Self::gap(
            RustTargetGapSite::Extern(air_id),
            RustTargetGapKind::UnsupportedExtern,
        ))
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
        self.config
            .native_providers
            .iter()
            .find(|provider| {
                provider.package == binding.package.as_str()
                    && provider.provider == binding.provider
            })
            .and_then(|provider| {
                provider
                    .modules
                    .iter()
                    .flat_map(|module| &module.types)
                    .find(|native| native.key == binding.key)
            })
            .ok_or_else(|| {
                Self::gap(
                    RustTargetGapSite::Type(type_id),
                    RustTargetGapKind::UnsupportedExtern,
                )
            })
    }

    fn native_binding(&self, binding: &air::ExternBindingDecl) -> Option<&RustExternBinding> {
        self.config
            .native_providers
            .iter()
            .find(|provider| {
                provider.package == binding.package.as_str()
                    && provider.provider == binding.provider
            })
            .and_then(|provider| {
                provider
                    .modules
                    .iter()
                    .flat_map(|module| &module.bindings)
                    .find(|native| native.key == binding.key)
            })
    }

    fn plan_function_ids(&mut self) {
        for index in 0..self.air.functions.len() {
            let air_id = FunctionId::from_index(index);
            self.function_map
                .insert(air_id, RirFunctionId::from_index(index));
        }
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
                storage: RirCellStorage::StackScoped,
                symbol: RirSymbol::new(format!("__cell{}", id.index())),
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
            if decl.escape == air::LambdaEscape::Escaping
                && decl.captures.iter().any(lambda_capture_has_runtime)
            {
                return Err(Self::gap(
                    RustTargetGapSite::Function(decl.owner),
                    RustTargetGapKind::UnsupportedLambdaCapture,
                ));
            }
            let captures = self.plan_lambda_captures(program, lambda, decl)?;
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
                captures,
            );
            self.lambda_map.insert(lambda, id);
        }
        Ok(())
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

    fn lambda_gap_site(&self, lambda: &RirLambda) -> RustTargetGapSite {
        match lambda.source {
            RirLambdaSource::Function(function) => RustTargetGapSite::Function(function),
            RirLambdaSource::Lambda(lambda) => {
                RustTargetGapSite::Function(self.air.lambdas[lambda.index()].owner)
            }
        }
    }

    fn plan_function_type_copyability(&mut self, program: &RirProgram) {
        let policy = RustRepPolicy::new(program);
        self.function_type_copyable.clear();
        for (ty, sig) in &self.lambda_sig_map {
            self.function_type_copyable
                .insert(*ty, policy.lambda_sig_copyable(*sig));
        }
    }

    fn plan_lambda_captures(
        &mut self,
        program: &RirProgram,
        lambda: air::LambdaId,
        decl: &air::LambdaDecl,
    ) -> Result<Vec<RirLambdaCapture>, RustPlanError> {
        let policy = RustRepPolicy::new(program);
        let mut captures = vec![];
        for (slot, capture) in decl.captures.iter().enumerate() {
            let Some(capture) = self.plan_lambda_capture(program, decl.owner, capture)? else {
                continue;
            };
            self.lambda_runtime_capture_slots.insert(
                (lambda, air::LambdaCaptureSlotId::from_index(slot)),
                captures.len(),
            );
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
        Ok(captures)
    }

    fn plan_lambda_capture(
        &self,
        program: &RirProgram,
        owner: FunctionId,
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
                let semantic = if self.rust_copyable_air_type(*ty) {
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
            air::LambdaCaptureDecl::ScopedLocal { ty, mutability, .. } => {
                if *mutability != Mutability::Mutable {
                    return Err(Self::gap(
                        RustTargetGapSite::Function(owner),
                        RustTargetGapKind::UnsupportedLambdaCapture,
                    ));
                }
                let ty = self.type_map[ty];
                let semantic = RirParamSemantic::MutBorrow;
                Ok(Some(RirLambdaCapture {
                    ty,
                    semantic,
                    abi: policy.param_abi(semantic),
                    kind: RirLambdaCaptureKind::Param,
                }))
            }
            air::LambdaCaptureDecl::ScopedBorrow { .. } => Err(Self::gap(
                RustTargetGapSite::Function(owner),
                RustTargetGapKind::UnsupportedLambdaCapture,
            )),
            air::LambdaCaptureDecl::CaptureCell { cell, ty, .. } => {
                let ty = self.type_map[ty];
                Ok(Some(RirLambdaCapture {
                    ty,
                    semantic: RirParamSemantic::StackCell,
                    abi: policy.param_abi(RirParamSemantic::StackCell),
                    kind: RirLambdaCaptureKind::StackCell {
                        cell: self.capture_cell_map[cell],
                    },
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
        Self::push_lambda(program, source, function, sig, escape, vec![])
    }

    fn push_lambda(
        program: &mut RirProgram,
        source: RirLambdaSource,
        function: RirFunctionId,
        sig: RirLambdaSigId,
        escape: RirLambdaEscape,
        captures: Vec<RirLambdaCapture>,
    ) -> RirLambdaId {
        let id = RirLambdaId::from_index(program.lambdas.len());
        let storage = if captures.is_empty() {
            RirLambdaStorage::ZeroEnv
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
            let semantic = rir::semantic_from_air(param.mode);
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
        for stmt in &block.stmts {
            stmts.extend(self.plan_air_stmt(
                function,
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

    fn plan_air_stmt(
        &self,
        function: FunctionId,
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
                let known = self.known_lambda_rvalue(&planned.value);
                let mut stmts = planned.stmts;
                stmts.push(RirStmt::Init {
                    local: RirLocalId::from_index(local.index()),
                    value: planned.value,
                });
                stmts.append(&mut planned.post_stmts);
                self.set_known_lambda(lambda_values, RirLocalId::from_index(local.index()), known);
                Ok(stmts)
            }
            air::AirStmt::Assign { dst, value } => {
                let mut planned = self.plan_rvalue(function, value, locals, lambda_values)?;
                let known = planned
                    .post_stmts
                    .is_empty()
                    .then(|| self.known_lambda_rvalue(&planned.value))
                    .flatten();
                let mut stmts = planned.stmts;
                let value = if planned.post_stmts.is_empty() {
                    planned.value
                } else {
                    let operand = self.rvalue_operand(planned.value, dst.ty, locals, &mut stmts);
                    stmts.append(&mut planned.post_stmts);
                    RirRValue::Use(operand)
                };
                self.lower_place_write(
                    function,
                    dst,
                    value,
                    locals,
                    &mut stmts,
                    initialized_cells,
                    possible_cells,
                    in_loop,
                )?;
                self.set_place_known_lambda(lambda_values, dst, known);
                Ok(stmts)
            }
            air::AirStmt::Eval(value) => {
                let mut planned = self.plan_rvalue(function, value, locals, lambda_values)?;
                let mut stmts = planned.stmts;
                stmts.push(RirStmt::Eval(planned.value));
                stmts.append(&mut planned.post_stmts);
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
                self.merge_known_lambdas(
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
                self.merge_known_lambdas(
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
                    self.merge_known_lambdas(lambda_values, locals.len(), states.iter());
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
                let discr = self.lower_place_read(function, &match_.discr, locals);
                let RirOperand::Place(discr_place) = discr.operand else {
                    unreachable!("place read returns a place operand")
                };
                let mut stmts = discr.stmts;
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
                self.merge_known_lambdas(
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
                    discr: discr_place,
                    payload,
                    payload_ref: match_.payload_ref,
                    payload_escapes: match_.payload_escapes,
                    some_block,
                    none_block,
                }));
                Ok(stmts)
            }
        }
    }

    fn set_known_lambda(
        &self,
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
        &self,
        lambda_values: &mut Vec<Option<KnownLambdaValue>>,
        place: &Place,
        known: Option<KnownLambdaValue>,
    ) {
        let Some(local) = place.root.local() else {
            return;
        };
        self.set_known_lambda(
            lambda_values,
            RirLocalId::from_index(local.index()),
            place.projection.is_empty().then_some(known).flatten(),
        );
    }

    fn merge_known_lambdas<'b>(
        &self,
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
                if self.place_crosses_dataref(function, list) {
                    return Err(Self::gap(
                        RustTargetGapSite::Function(function),
                        RustTargetGapKind::UnsupportedPlaceProjection,
                    ));
                }
                let value = self.plan_operand_read(function, value, locals);
                PlannedRValue {
                    stmts: value.stmts,
                    value: RirRValue::ListPush {
                        list: self.plan_place_in_function(function, list),
                        value: value.operand,
                    },
                    post_stmts: vec![],
                }
            }
            RValue::SliceView {
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
                    value: RirRValue::SliceView {
                        source: source_place,
                        start: RirLocalId::from_index(start.index()),
                        end: RirLocalId::from_index(end.index()),
                        inclusive: *inclusive,
                        ty: self.type_map[ty],
                    },
                    post_stmts: vec![],
                }
            }
            RValue::ListSlice {
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
                    value: RirRValue::ListSlice {
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
            RValue::MapInsert { map, key, value } => {
                if self.place_crosses_dataref(function, map) {
                    return Err(Self::gap(
                        RustTargetGapSite::Function(function),
                        RustTargetGapKind::UnsupportedPlaceProjection,
                    ));
                }
                let key = self.plan_operand_read(function, key, locals);
                let value = self.plan_operand_read(function, value, locals);
                let mut stmts = key.stmts;
                stmts.extend(value.stmts);
                PlannedRValue {
                    stmts,
                    value: RirRValue::MapInsert {
                        map: self.plan_place_in_function(function, map),
                        key: key.operand,
                        value: value.operand,
                    },
                    post_stmts: vec![],
                }
            }
            RValue::MapRemove { map, key, ty } => {
                if self.place_crosses_dataref(function, map) {
                    return Err(Self::gap(
                        RustTargetGapSite::Function(function),
                        RustTargetGapKind::UnsupportedPlaceProjection,
                    ));
                }
                let key = self.plan_operand_read(function, key, locals);
                PlannedRValue {
                    stmts: key.stmts,
                    value: RirRValue::MapRemove {
                        map: self.plan_place_in_function(function, map),
                        key: key.operand,
                        ty: self.type_map[ty],
                    },
                    post_stmts: vec![],
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
            RValue::ListPop { .. } | RValue::MapEntryAt { .. } => {
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
                air::LambdaCaptureArg::ScopedLocal { place } => {
                    planned.push(RirLambdaCaptureArg::Scoped {
                        place: self.plan_place_in_function(function, place),
                    });
                }
                air::LambdaCaptureArg::ScopedBorrow { .. } => {
                    return Err(Self::gap(
                        RustTargetGapSite::Function(function),
                        RustTargetGapKind::UnsupportedLambdaCapture,
                    ));
                }
                air::LambdaCaptureArg::CaptureCell { cell } => {
                    planned.push(RirLambdaCaptureArg::StackCell {
                        cell: self.capture_cell_ref(function, *cell),
                    });
                }
            }
        }
        Ok(planned)
    }

    fn air_place_value_readable(&self, ty: TypeId) -> bool {
        self.rust_copyable_air_type(ty) || self.rust_shareable_air_type(ty)
    }

    fn known_lambda_rvalue(&self, value: &RirRValue) -> Option<KnownLambdaValue> {
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
        if let Some(cell) = self.place_capture_cell(function, place) {
            if !place.projection.is_empty() {
                return Err(Self::gap(
                    RustTargetGapSite::Function(function),
                    RustTargetGapKind::UnsupportedPlaceProjection,
                ));
            }
            return Ok(PlannedRValue::from_value(RirRValue::CellGetCopy {
                cell: self.capture_cell_ref(function, cell),
                ty: self.type_map[&place.ty],
            }));
        }
        if self.place_crosses_dataref(function, place) {
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
        let (target, ty, callee_stmts) = match callee {
            Callee::Function(id) => {
                let function = self.air.function(*id);
                (
                    RirCallTarget::Function(self.function_map[id]),
                    self.type_map[&function.signature.return_type()],
                    vec![],
                )
            }
            Callee::Extern(id) => {
                let ext = self.air.extern_decl(*id);
                (
                    RirCallTarget::Extern(self.extern_map[id]),
                    self.type_map[&ext.return_type],
                    vec![],
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
                )
            }
        };
        let mut stmts = callee_stmts;
        let mut post_stmts = vec![];
        let mut planned_args = vec![];
        for arg in args {
            let planned = self.plan_arg(function_id, arg, locals, lambda_values)?;
            stmts.extend(planned.stmts);
            post_stmts.extend(planned.post_stmts);
            planned_args.push(planned.arg);
        }
        Ok(PlannedRValue {
            stmts,
            value: RirRValue::Call {
                callee: target,
                args: planned_args,
                ty,
            },
            post_stmts,
        })
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
        locals: &mut Vec<RirLocal>,
        lambda_values: &mut Vec<Option<KnownLambdaValue>>,
    ) -> Result<PlannedCallArg, RustPlanError> {
        match arg {
            CallArg::Value(operand) => {
                if let Operand::Place(place) = operand
                    && !self.air_place_value_readable(place.ty)
                    && let Some(known) = self.known_lambda_place(place, lambda_values)
                {
                    let local = self.alloc_temp(locals, place.ty);
                    self.set_known_lambda(lambda_values, local, Some(known));
                    return Ok(PlannedCallArg {
                        stmts: vec![RirStmt::Init {
                            local,
                            value: known.rvalue(),
                        }],
                        arg: RirCallArg::Value(RirOperand::Place(RirPlace {
                            local,
                            projections: vec![],
                            ty: known.ty,
                        })),
                        post_stmts: vec![],
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
                    post_stmts: vec![],
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
                    post_stmts: vec![],
                })
            }
            CallArg::SharedStringConst(id) => Ok(PlannedCallArg::from_arg(
                RirCallArg::SharedStringConst(self.const_map[id]),
            )),
            CallArg::MutBorrow(place) => {
                if !self.place_crosses_dataref(function, place) {
                    return Ok(PlannedCallArg::from_arg(RirCallArg::MutBorrow(
                        self.plan_place_in_function(function, place),
                    )));
                }
                let planned = self.lower_place_read(function, place, locals);
                let RirOperand::Place(temp_place) = planned.operand else {
                    unreachable!("place read returns a place operand")
                };
                locals[temp_place.local.index()].mutable = true;
                let mut post_stmts = vec![];
                let mut ignored_cells = vec![];
                let mut ignored_possible_cells = vec![];
                self.lower_place_write(
                    function,
                    place,
                    RirRValue::Use(RirOperand::Place(temp_place.clone())),
                    locals,
                    &mut post_stmts,
                    &mut ignored_cells,
                    &mut ignored_possible_cells,
                    false,
                )?;
                Ok(PlannedCallArg {
                    stmts: planned.stmts,
                    arg: RirCallArg::MutBorrow(temp_place),
                    post_stmts,
                })
            }
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
        if let Some(cell) = self.place_capture_cell(function, place) {
            if !place.projection.is_empty() {
                unreachable!("Rust backend profile rejects projected capture-cell places")
            }
            let local = self.alloc_temp(locals, place.ty);
            return PlannedOperand {
                stmts: vec![RirStmt::Init {
                    local,
                    value: RirRValue::CellGetCopy {
                        cell: self.capture_cell_ref(function, cell),
                        ty: self.type_map[&place.ty],
                    },
                }],
                operand: RirOperand::Place(RirPlace {
                    local,
                    projections: vec![],
                    ty: self.type_map[&place.ty],
                }),
            };
        }
        if !self.place_crosses_dataref(function, place) {
            return PlannedOperand::from_operand(RirOperand::Place(
                self.plan_place_in_function(function, place),
            ));
        }
        let mut stmts = vec![];
        let (mut current_ty, mut current_place) = self.root_place(function, place);
        let mut index = 0;
        while let Some(segment) =
            self.next_dataref_segment(place, &mut index, &mut current_ty, &mut current_place)
        {
            index = segment.next_index;
            current_ty = segment.ty;
            current_place = self.read_dataref_segment(segment, locals, &mut stmts);
        }
        PlannedOperand {
            stmts,
            operand: RirOperand::Place(current_place),
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
        if let Some(cell) = self.place_capture_cell(function, place) {
            if !place.projection.is_empty() {
                unreachable!("Rust backend profile rejects projected capture-cell places")
            }
            let cell_ref = self.capture_cell_ref(function, cell);
            if initialized_cells
                .get(cell.index())
                .copied()
                .unwrap_or(false)
            {
                stmts.push(RirStmt::CellSet {
                    cell: cell_ref,
                    value,
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
            return Ok(());
        }
        if !self.place_crosses_dataref(function, place) {
            stmts.push(RirStmt::Assign {
                dst: self.plan_place_in_function(function, place),
                value,
            });
            return Ok(());
        }
        let value = self.rvalue_operand(value, place.ty, locals, stmts);
        let (mut current_ty, mut current_place) = self.root_place(function, place);
        let mut index = 0;
        while let Some(segment) =
            self.next_dataref_segment(place, &mut index, &mut current_ty, &mut current_place)
        {
            if segment.next_index == place.projection.len() {
                stmts.push(RirStmt::DataRefSet {
                    object: segment.object,
                    dataref: segment.dataref,
                    projections: segment.projections,
                    value,
                });
                return Ok(());
            }
            index = segment.next_index;
            current_ty = segment.ty;
            current_place = self.read_dataref_segment(segment, locals, stmts);
        }
        unreachable!("dataref-crossing write has a dataref segment")
    }

    fn root_place(&self, function: FunctionId, place: &Place) -> (TypeId, RirPlace) {
        let (ty, local) = self.current_place_root(function, place);
        (
            ty,
            RirPlace {
                local,
                projections: vec![],
                ty: self.type_map[&ty],
            },
        )
    }

    fn next_dataref_segment(
        &self,
        place: &Place,
        index: &mut usize,
        current_ty: &mut TypeId,
        current_place: &mut RirPlace,
    ) -> Option<DataRefSegment> {
        while *index < place.projection.len() {
            if let TypeData::DataRef(aggregate) = self.air.type_arena.data(*current_ty) {
                let (projections, ty, next_index) =
                    self.dataref_projection_segment(*current_ty, &place.projection, *index);
                return Some(DataRefSegment {
                    object: RirOperand::Place(current_place.clone()),
                    dataref: self.dataref_map[aggregate],
                    projections,
                    ty,
                    next_index,
                });
            }
            let projection = &place.projection[*index];
            *current_ty = self.projected_ty(*current_ty, projection);
            current_place
                .projections
                .push(Self::rir_projection(projection));
            current_place.ty = self.type_map[&*current_ty];
            *index += 1;
        }
        None
    }

    fn read_dataref_segment(
        &self,
        segment: DataRefSegment,
        locals: &mut Vec<RirLocal>,
        stmts: &mut Vec<RirStmt>,
    ) -> RirPlace {
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
        RirPlace {
            local,
            projections: vec![],
            ty: self.type_map[&segment.ty],
        }
    }

    fn rvalue_operand(
        &self,
        value: RirRValue,
        ty: TypeId,
        locals: &mut Vec<RirLocal>,
        stmts: &mut Vec<RirStmt>,
    ) -> RirOperand {
        match value {
            RirRValue::Use(operand) => operand,
            value => {
                let local = self.alloc_temp(locals, ty);
                stmts.push(RirStmt::Init { local, value });
                RirOperand::Place(RirPlace {
                    local,
                    projections: vec![],
                    ty: self.type_map[&ty],
                })
            }
        }
    }

    fn dataref_projection_segment(
        &self,
        dataref_ty: TypeId,
        projections: &[Projection],
        start: usize,
    ) -> (Vec<RirProjection>, TypeId, usize) {
        let mut current_ty = dataref_ty;
        let mut index = start;
        let mut segment = vec![];
        while index < projections.len() {
            let projection = &projections[index];
            segment.push(Self::rir_projection(projection));
            current_ty = self.projected_ty(current_ty, projection);
            index += 1;
            if matches!(self.air.type_arena.data(current_ty), TypeData::DataRef(_))
                && index < projections.len()
            {
                break;
            }
        }
        (segment, current_ty, index)
    }

    fn place_crosses_dataref(&self, function: FunctionId, place: &Place) -> bool {
        let (mut ty, _) = self.current_place_root(function, place);
        for projection in &place.projection {
            if matches!(self.air.type_arena.data(ty), TypeData::DataRef(_)) {
                return true;
            }
            ty = self.projected_ty(ty, projection);
        }
        false
    }

    fn place_capture_cell(
        &self,
        function: FunctionId,
        place: &Place,
    ) -> Option<air::CaptureCellId> {
        match place.root {
            air::PlaceRoot::CaptureCell(cell) => Some(cell),
            air::PlaceRoot::LambdaCapture(slot) => {
                let air::FunctionKind::Lambda(lambda) = self.air.function(function).kind else {
                    return None;
                };
                match self.air.lambdas[lambda.index()].captures[slot.index()] {
                    air::LambdaCaptureDecl::CaptureCell { cell, .. } => Some(cell),
                    _ => None,
                }
            }
            _ => None,
        }
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
                    lambda_capture_ty(decl),
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

    fn projected_ty(&self, ty: TypeId, projection: &Projection) -> TypeId {
        match (self.air.type_arena.data(ty), projection) {
            (
                TypeData::Aggregate(aggregate) | TypeData::DataRef(aggregate),
                Projection::Field(field),
            ) => self.air.aggregate(*aggregate).fields[field.index()].ty,
            (
                TypeData::Array { elem, .. } | TypeData::List(elem) | TypeData::Slice(elem),
                Projection::Index(_),
            ) => *elem,
            (TypeData::Tuple(elems), Projection::TupleField(index)) => elems[*index as usize],
            _ => ty,
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
            return self.rust_copyable_air_type(ty);
        }
        self.air_policy().value_place_shareable(ty)
    }

    fn air_policy(&self) -> AirRustRepPolicy<'_> {
        AirRustRepPolicy::new(self.air, &self.classes)
    }

    fn plan_place_in_function(&self, function: FunctionId, place: &Place) -> RirPlace {
        let (_, root) = self.current_place_root(function, place);
        RirPlace {
            local: root,
            projections: place.projection.iter().map(Self::rir_projection).collect(),
            ty: self.type_map[&place.ty],
        }
    }
}

fn lambda_capture_has_runtime(capture: &air::LambdaCaptureDecl) -> bool {
    !matches!(capture, air::LambdaCaptureDecl::NoRuntime { .. })
}

fn lambda_capture_ty(capture: &air::LambdaCaptureDecl) -> TypeId {
    match capture {
        air::LambdaCaptureDecl::NoRuntime { ty, .. }
        | air::LambdaCaptureDecl::ReadonlyLocal { ty, .. }
        | air::LambdaCaptureDecl::ScopedLocal { ty, .. }
        | air::LambdaCaptureDecl::ScopedBorrow { ty, .. }
        | air::LambdaCaptureDecl::CaptureCell { ty, .. } => *ty,
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
