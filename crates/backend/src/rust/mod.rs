pub mod cargo_job;
pub mod emit;
pub mod profile;
pub mod rep_policy;
pub mod rir;
#[cfg(test)]
mod source_job;

use std::{collections::HashMap, error::Error, fmt};

use anvyx_frontend::{
    air::{
        self, AggregateCtor, CallArg, Callee, ConstId, ConstValue, ExternId, FunctionId, LocalId,
        LocalKind, Mutability, Operand, ParamMode, Place, Projection, RValue, TypeData, TypeId,
        TypePassClasses, VerifiedProgram,
    },
    ast::{FormatAlign, FormatKind, FormatSign, FormatSpec, Ident},
};
use anvyx_runtime::{RustAbiSupport, RustExternBinding, RustPath, RustProviderSupport};

use self::{
    profile::{ProfileErrorKind, ProfileSite, RustBackendProfile, RustBackendProfileError},
    rep_policy::{AirRustRepPolicy, RustRepPolicy},
    rir::{
        RirCallArg, RirCallTarget, RirConst, RirConstId, RirConstValue, RirCoreEnumKind,
        RirCtxPlan, RirDataRef, RirDataRefId, RirEnum, RirEnumId, RirEnumMatch, RirEnumMatchArm,
        RirEnumRepr, RirExtern, RirExternId, RirExternKind, RirExternParam, RirField, RirFieldId,
        RirFormatAlign, RirFormatKind, RirFormatSign, RirFormatSpec, RirFunction, RirFunctionId,
        RirIf, RirLocal, RirLocalId, RirLoop, RirLoopId, RirNativeExtern, RirOperand, RirParam,
        RirParamSemantic, RirPlace, RirProgram, RirProjection, RirRValue, RirRawEnumValue,
        RirReturn, RirStmt, RirStringifyHelper, RirStringifyHelperId, RirStringifyReq,
        RirStringifyReqId, RirStringifyReqKind, RirStruct, RirStructId, RirStructuredBlock,
        RirSymbol, RirTerm, RirType, RirTypeId, RirVariant, RirVariantId, RirVariantKind,
        VerifiedRirProgram,
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
    UnsupportedTerminator,
    UnsupportedRValue,
    UnsupportedCallee,
    UnsupportedExtern,
    UnsupportedExternMember,
    UnsupportedEntry,
    UnsupportedRustAbi,
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
                ProfileErrorKind::UnsupportedTerminator => RustTargetGapKind::UnsupportedTerminator,
                ProfileErrorKind::UnsupportedRValue => RustTargetGapKind::UnsupportedRValue,
                ProfileErrorKind::UnsupportedCallee => RustTargetGapKind::UnsupportedCallee,
                ProfileErrorKind::UnsupportedExtern => RustTargetGapKind::UnsupportedExtern,
                ProfileErrorKind::UnsupportedExternMember => {
                    RustTargetGapKind::UnsupportedExternMember
                }
                ProfileErrorKind::UnsupportedEntry => RustTargetGapKind::UnsupportedEntry,
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
    const_map: HashMap<ConstId, RirConstId>,
    function_map: HashMap<FunctionId, RirFunctionId>,
    extern_map: HashMap<ExternId, RirExternId>,
    dataref_map: HashMap<air::AggregateId, RirDataRefId>,
    enum_map: HashMap<air::EnumId, RirEnumId>,
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
            const_map: HashMap::new(),
            function_map: HashMap::new(),
            extern_map: HashMap::new(),
            dataref_map: HashMap::new(),
            enum_map: HashMap::new(),
        }
    }

    fn check_support(&self) -> Result<(), RustPlanError> {
        RustBackendProfile::check(self.verified).map_err(|errors| {
            RustPlanError::TargetGaps(errors.into_iter().map(RustTargetGap::from).collect())
        })
    }

    fn gap(&self, site: RustTargetGapSite, kind: RustTargetGapKind) -> RustPlanError {
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
                TypeData::Slice(elem) => RirType::Slice(self.type_map[elem]),
                TypeData::Any
                | TypeData::Map { .. }
                | TypeData::Tuple(_)
                | TypeData::Function(_)
                | TypeData::Dyn(_) => {
                    return Err(self.gap(
                        RustTargetGapSite::Type(type_id),
                        RustTargetGapKind::UnsupportedType,
                    ));
                }
            };
            program.types.push(rir);
        }
        for (type_id, aggregate, struct_id) in aggregate_types {
            self.fill_struct(program, type_id, aggregate, struct_id)?;
        }
        for (type_id, ext, struct_id) in extern_types {
            self.fill_extern_struct(program, type_id, ext, struct_id)?;
        }
        for (type_id, aggregate, dataref_id) in dataref_types {
            self.fill_dataref(program, type_id, aggregate, dataref_id)?;
        }
        for (type_id, enm, enum_id) in enum_types {
            self.fill_enum(program, type_id, enm, enum_id)?;
        }
        Ok(())
    }

    fn reserve_struct(
        &mut self,
        program: &mut RirProgram,
        type_id: TypeId,
        aggregate: air::AggregateId,
    ) -> Result<RirStructId, RustPlanError> {
        let decl = self.air.aggregate(aggregate);
        if decl.kind != air::AggregateKind::Struct {
            return Err(self.gap(
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
                return Err(self.gap(
                    RustTargetGapSite::Type(field.ty),
                    RustTargetGapKind::UnsupportedType,
                ));
            };
            if field.ty == type_id {
                return Err(self.gap(
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
        let copyable = self.rust_copyable_air_type(type_id)
            && fields
                .iter()
                .all(|field| RustRepPolicy::new(program).copyable(field.ty));
        let strukt = &mut program.structs[struct_id.index()];
        strukt.copyable = copyable;
        strukt.fields = fields;
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
            return Err(self.gap(
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
        _type_id: TypeId,
        aggregate: air::AggregateId,
        dataref_id: RirDataRefId,
    ) -> Result<(), RustPlanError> {
        let decl = self.air.aggregate(aggregate);
        let mut seen = vec![];
        let mut fields = vec![];
        for (index, field) in decl.fields.iter().enumerate() {
            let Some(&ty) = self.type_map.get(&field.ty) else {
                return Err(self.gap(
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
            return Err(self.gap(
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
        let mut seen = vec![];
        let fields = decl
            .fields
            .iter()
            .enumerate()
            .map(|(index, field)| {
                let Some(&ty) = self.type_map.get(&field.ty) else {
                    return Err(self.gap(
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
        let rir = &mut program.enums[enum_id.index()];
        rir.copyable = self.rust_copyable_air_type(type_id);
        rir.variants = variants;
        Ok(())
    }

    fn enum_field(
        &self,
        enum_ty: TypeId,
        ty: TypeId,
        index: usize,
    ) -> Result<RirField, RustPlanError> {
        let Some(&rir_ty) = self.type_map.get(&ty) else {
            return Err(self.gap(
                RustTargetGapSite::Type(ty),
                RustTargetGapKind::UnsupportedType,
            ));
        };
        let recursive = ty == enum_ty;
        if recursive {
            return Err(self.gap(
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
                return Err(self.gap(
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
                return Err(self.gap(
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
                ParamMode::Value | ParamMode::MutBorrow => Err(self.gap(
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
            return Err(self.gap(
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
            .into_iter()
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
                self.gap(
                    RustTargetGapSite::Extern(air_id),
                    RustTargetGapKind::UnsupportedExtern,
                )
            })?;
            if native.abi.support != RustAbiSupport::Direct {
                return Err(self.gap(
                    RustTargetGapSite::Extern(air_id),
                    RustTargetGapKind::UnsupportedRustAbi,
                ));
            }
            return Ok(RirExternKind::Native(RirNativeExtern {
                path: native_path(&native.path),
                abi: native.abi.clone(),
            }));
        }

        Err(self.gap(
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
            return Err(self.gap(
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
                self.gap(
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
            })
            .collect::<Vec<_>>();
        for param in &function.signature.params {
            if let Some(local) = locals.get_mut(param.local_id.index()) {
                local.initialized = true;
            }
        }
        let policy = RustRepPolicy::new(program);
        let params = function
            .signature
            .params
            .iter()
            .map(|param| {
                let ty = self.type_map[&param.ty];
                let semantic = rir::semantic_from_air(param.mode);
                RirParam {
                    local: RirLocalId::from_index(param.local_id.index()),
                    ty,
                    semantic,
                    abi: policy.param_abi(semantic),
                }
            })
            .collect();
        let body = self.plan_air_block(air_id, &function.body.block, &mut locals)?;
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
    ) -> Result<RirStructuredBlock, RustPlanError> {
        let mut stmts = vec![];
        for stmt in &block.stmts {
            stmts.extend(self.plan_air_stmt(function, stmt, locals)?);
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
    ) -> Result<Vec<RirStmt>, RustPlanError> {
        match stmt {
            air::AirStmt::Init { local, value } => {
                let mut planned = self.plan_rvalue(function, value, locals)?;
                let mut stmts = planned.stmts;
                stmts.push(RirStmt::Init {
                    local: RirLocalId::from_index(local.index()),
                    value: planned.value,
                });
                stmts.append(&mut planned.post_stmts);
                Ok(stmts)
            }
            air::AirStmt::Assign { dst, value } => {
                let mut planned = self.plan_rvalue(function, value, locals)?;
                let mut stmts = planned.stmts;
                let value = if planned.post_stmts.is_empty() {
                    planned.value
                } else {
                    let operand = self.rvalue_operand(planned.value, dst.ty, locals, &mut stmts);
                    stmts.append(&mut planned.post_stmts);
                    RirRValue::Use(operand)
                };
                self.lower_place_write(function, dst, value, locals, &mut stmts);
                Ok(stmts)
            }
            air::AirStmt::Eval(value) => {
                let mut planned = self.plan_rvalue(function, value, locals)?;
                let mut stmts = planned.stmts;
                stmts.push(RirStmt::Eval(planned.value));
                stmts.append(&mut planned.post_stmts);
                Ok(stmts)
            }
            air::AirStmt::If(branch) => {
                let cond = self.plan_operand_read(function, &branch.cond, locals);
                let mut stmts = cond.stmts;
                stmts.push(RirStmt::If(RirIf {
                    cond: cond.operand,
                    then_block: self.plan_air_block(function, &branch.then_block, locals)?,
                    else_block: branch
                        .else_block
                        .as_ref()
                        .map(|block| self.plan_air_block(function, block, locals))
                        .transpose()?,
                }));
                Ok(stmts)
            }
            air::AirStmt::Loop(loop_) => Ok(vec![RirStmt::Loop(RirLoop {
                id: RirLoopId::from_index(loop_.id.index()),
                body: self.plan_air_block(function, &loop_.body, locals)?,
            })]),
            air::AirStmt::EnumMatch(match_) => {
                let discr = self.lower_place_read(function, &match_.discr, locals);
                let RirOperand::Place(discr_place) = discr.operand else {
                    unreachable!("place read returns a place operand")
                };
                let mut stmts = discr.stmts;
                stmts.push(RirStmt::EnumMatch(RirEnumMatch {
                    discr: discr_place,
                    arms: match_
                        .arms
                        .iter()
                        .map(|arm| {
                            Ok(RirEnumMatchArm {
                                variant: RirVariantId::from_index(arm.variant.index()),
                                block: self.plan_air_block(function, &arm.block, locals)?,
                            })
                        })
                        .collect::<Result<Vec<_>, RustPlanError>>()?,
                    else_block: match_
                        .else_block
                        .as_ref()
                        .map(|block| self.plan_air_block(function, block, locals))
                        .transpose()?,
                }));
                Ok(stmts)
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
    ) -> Result<PlannedRValue, RustPlanError> {
        let planned = match value {
            RValue::Use(operand) => return self.plan_use(function, operand, locals),
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
            RValue::Call { callee, args } => self.plan_call(function, callee, args, locals)?,
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
                    return Err(self.gap(
                        RustTargetGapSite::Function(function),
                        RustTargetGapKind::UnsupportedPlaceProjection,
                    ));
                }
                let value = self.plan_operand_read(function, value, locals);
                PlannedRValue {
                    stmts: value.stmts,
                    value: RirRValue::ListPush {
                        list: self.plan_place(list),
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
                    return Err(self.gap(
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
                        map: self.plan_place(map),
                        key: key.operand,
                        value: value.operand,
                    },
                    post_stmts: vec![],
                }
            }
            RValue::MapRemove { map, key, ty } => {
                if self.place_crosses_dataref(function, map) {
                    return Err(self.gap(
                        RustTargetGapSite::Function(function),
                        RustTargetGapKind::UnsupportedPlaceProjection,
                    ));
                }
                let key = self.plan_operand_read(function, key, locals);
                PlannedRValue {
                    stmts: key.stmts,
                    value: RirRValue::MapRemove {
                        map: self.plan_place(map),
                        key: key.operand,
                        ty: self.type_map[ty],
                    },
                    post_stmts: vec![],
                }
            }
            RValue::ListPop { .. } | RValue::MapEntryAt { .. } | RValue::MakeClosure { .. } => {
                return Err(self.gap(
                    RustTargetGapSite::Function(function),
                    RustTargetGapKind::UnsupportedRValue,
                ));
            }
        };
        Ok(planned)
    }

    fn plan_use(
        &self,
        function: FunctionId,
        operand: &Operand,
        locals: &mut Vec<RirLocal>,
    ) -> Result<PlannedRValue, RustPlanError> {
        let Operand::Place(place) = operand else {
            return Ok(PlannedRValue::from_value(RirRValue::Use(
                self.plan_operand(operand),
            )));
        };
        if self.place_crosses_dataref(function, place) {
            let planned = self.lower_place_read(function, place, locals);
            return Ok(PlannedRValue {
                stmts: planned.stmts,
                value: RirRValue::Use(planned.operand),
                post_stmts: vec![],
            });
        }
        if !self.rust_copyable_air_type(place.ty)
            && !self.air_policy().value_place_shareable(place.ty)
        {
            return Err(self.gap(
                RustTargetGapSite::Function(function),
                RustTargetGapKind::NonCopyValueRequired,
            ));
        }
        let TypeData::Aggregate(aggregate) = self.air.type_arena.data(place.ty) else {
            return Ok(PlannedRValue::from_value(RirRValue::Use(
                self.plan_operand(operand),
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
                    RirOperand::Place(self.plan_place(&field_place))
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
            AggregateCtor::Tuple | AggregateCtor::Map => {
                return Err(self.gap(
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
    ) -> Result<PlannedRValue, RustPlanError> {
        let (target, ty) = match callee {
            Callee::Function(id) => {
                let function = self.air.function(*id);
                (
                    RirCallTarget::Function(self.function_map[id]),
                    self.type_map[&function.signature.return_type()],
                )
            }
            Callee::Extern(id) => {
                let ext = self.air.extern_decl(*id);
                (
                    RirCallTarget::Extern(self.extern_map[id]),
                    self.type_map[&ext.return_type],
                )
            }
            Callee::Closure(_) => {
                return Err(self.gap(
                    RustTargetGapSite::Function(function_id),
                    RustTargetGapKind::UnsupportedCallee,
                ));
            }
        };
        let mut stmts = vec![];
        let mut post_stmts = vec![];
        let mut planned_args = vec![];
        for arg in args {
            let planned = self.plan_arg(function_id, arg, locals);
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

    fn plan_arg(
        &self,
        function: FunctionId,
        arg: &CallArg,
        locals: &mut Vec<RirLocal>,
    ) -> PlannedCallArg {
        match arg {
            CallArg::Value(operand) => {
                let planned = self.plan_operand_read(function, operand, locals);
                PlannedCallArg {
                    stmts: planned.stmts,
                    arg: RirCallArg::Value(planned.operand),
                    post_stmts: vec![],
                }
            }
            CallArg::SharedBorrow(place) => {
                let planned = self.lower_place_read(function, place, locals);
                let RirOperand::Place(place) = planned.operand else {
                    unreachable!("place read returns a place operand")
                };
                PlannedCallArg {
                    stmts: planned.stmts,
                    arg: RirCallArg::SharedBorrow(place),
                    post_stmts: vec![],
                }
            }
            CallArg::SharedStringConst(id) => {
                PlannedCallArg::from_arg(RirCallArg::SharedStringConst(self.const_map[id]))
            }
            CallArg::MutBorrow(place) => {
                if !self.place_crosses_dataref(function, place) {
                    return PlannedCallArg::from_arg(RirCallArg::MutBorrow(self.plan_place(place)));
                }
                let planned = self.lower_place_read(function, place, locals);
                let RirOperand::Place(temp_place) = planned.operand else {
                    unreachable!("place read returns a place operand")
                };
                locals[temp_place.local.index()].mutable = true;
                let mut post_stmts = vec![];
                self.lower_place_write(
                    function,
                    place,
                    RirRValue::Use(RirOperand::Place(temp_place.clone())),
                    locals,
                    &mut post_stmts,
                );
                PlannedCallArg {
                    stmts: planned.stmts,
                    arg: RirCallArg::MutBorrow(temp_place),
                    post_stmts,
                }
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

    fn plan_operand(&self, operand: &Operand) -> RirOperand {
        match operand {
            Operand::Place(place) => RirOperand::Place(self.plan_place(place)),
            Operand::Const(id) => RirOperand::Const(self.const_map[id]),
        }
    }

    fn lower_place_read(
        &self,
        function: FunctionId,
        place: &Place,
        locals: &mut Vec<RirLocal>,
    ) -> PlannedOperand {
        if !self.place_crosses_dataref(function, place) {
            return PlannedOperand::from_operand(RirOperand::Place(self.plan_place(place)));
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
    ) {
        if !self.place_crosses_dataref(function, place) {
            stmts.push(RirStmt::Assign {
                dst: self.plan_place(place),
                value,
            });
            return;
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
                return;
            }
            index = segment.next_index;
            current_ty = segment.ty;
            current_place = self.read_dataref_segment(segment, locals, stmts);
        }
        unreachable!("dataref-crossing write has a dataref segment")
    }

    fn root_place(&self, function: FunctionId, place: &Place) -> (TypeId, RirPlace) {
        let ty = self.current_place_root_ty(function, place);
        (
            ty,
            RirPlace {
                local: RirLocalId::from_index(place.root.index()),
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
                .push(self.rir_projection(projection));
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
            segment.push(self.rir_projection(projection));
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
        let mut ty = self.current_place_root_ty(function, place);
        for projection in &place.projection {
            if matches!(self.air.type_arena.data(ty), TypeData::DataRef(_)) {
                return true;
            }
            ty = self.projected_ty(ty, projection);
        }
        false
    }

    fn current_place_root_ty(&self, function: FunctionId, place: &Place) -> TypeId {
        self.air.function(function).locals[place.root.index()].ty
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
        });
        id
    }

    fn rir_projection(&self, projection: &Projection) -> RirProjection {
        match projection {
            Projection::Field(field) => RirProjection::Field(RirFieldId::from_index(field.index())),
            Projection::Index(local) => RirProjection::Index(RirLocalId::from_index(local.index())),
            Projection::TupleField(_) | Projection::VariantField { .. } => {
                unreachable!("profile rejects unsupported projection")
            }
        }
    }

    fn operand_ty(&self, operand: &Operand) -> TypeId {
        match operand {
            Operand::Place(place) => place.ty,
            Operand::Const(id) => self.air.const_arena.get(*id).ty,
        }
    }

    fn rust_copyable_air_type(&self, ty: TypeId) -> bool {
        self.air_policy().copyable(ty)
    }

    fn air_policy(&self) -> AirRustRepPolicy<'_> {
        AirRustRepPolicy::new(self.air, &self.classes)
    }

    fn plan_place(&self, place: &Place) -> RirPlace {
        RirPlace {
            local: RirLocalId::from_index(place.root.index()),
            projections: place
                .projection
                .iter()
                .filter_map(|projection| match projection {
                    Projection::Field(field) => {
                        Some(RirProjection::Field(RirFieldId::from_index(field.index())))
                    }
                    Projection::Index(local) => {
                        Some(RirProjection::Index(RirLocalId::from_index(local.index())))
                    }
                    Projection::TupleField(_) | Projection::VariantField { .. } => None,
                })
                .collect(),
            ty: self.type_map[&place.ty],
        }
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
        RirType::Array { elem, len } => format!("array_{}_{}", len, type_suffix(program, elem)),
        RirType::Struct(id) => named_type_suffix(ty.index(), &program.structs[id.index()].display),
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
