pub mod cargo_job;
pub mod emit;
pub mod profile;
pub mod rir;
#[cfg(test)]
mod source_job;

use std::{collections::HashMap, error::Error, fmt};

use anvyx_frontend::{
    air::{
        self, AggregateCtor, CallArg, Callee, ConstId, ConstValue, ExternId, FunctionId, LocalId,
        LocalKind, Mutability, Operand, ParamMode, Place, Projection, RValue, TypeData, TypeId,
        TypePassClass, TypePassClasses, VerifiedProgram,
    },
    ast::{FormatAlign, FormatKind, FormatSign, FormatSpec, Ident},
};
use anvyx_runtime::{RustAbiSupport, RustExternBinding, RustProviderSupport};

use self::{
    profile::{ProfileErrorKind, ProfileSite, RustBackendProfile, RustBackendProfileError},
    rir::{
        RirCallArg, RirCallTarget, RirConst, RirConstId, RirConstValue, RirCoreEnumKind,
        RirCtxPlan, RirEnum, RirEnumId, RirEnumMatch, RirEnumMatchArm, RirExtern, RirExternId,
        RirExternKind, RirExternParam, RirField, RirFieldId, RirFormatAlign, RirFormatKind,
        RirFormatSign, RirFormatSpec, RirFunction, RirFunctionId, RirIf, RirLocal, RirLocalId,
        RirNativeExtern, RirOperand, RirParam, RirParamAbi, RirParamSemantic, RirPlace, RirProgram,
        RirProjection, RirRValue, RirReturn, RirStmt, RirStringifyHelper, RirStringifyHelperId,
        RirStringifyReq, RirStringifyReqId, RirStringifyReqKind, RirStruct, RirStructId,
        RirStructuredBlock, RirSymbol, RirTerm, RirType, RirTypeId, RirVariant, RirVariantId,
        RirVariantKind, VerifiedRirProgram,
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
        self.plan_consts(&mut program)?;
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
                TypeData::Slice(elem) => RirType::Slice(self.type_map[elem]),
                TypeData::Any
                | TypeData::Optional(_)
                | TypeData::Tuple(_)
                | TypeData::Map { .. }
                | TypeData::Function(_)
                | TypeData::Dyn(_)
                | TypeData::DataRef(_) => {
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
            if field.ty == type_id || matches!(program.types[ty.index()], RirType::Struct(_)) {
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
        let strukt = &mut program.structs[struct_id.index()];
        strukt.copyable = rust_copyable_air_type(&self.classes, type_id);
        strukt.fields = fields;
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
            native_path: Some(native_type_path(native)),
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
        program.enums.push(RirEnum {
            id,
            air_id: Some(enm),
            core: decl.core.map(rir_core_enum_kind),
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
                        .map(|(index, ty)| self.enum_field(program, type_id, *ty, index))
                        .collect::<Result<Vec<_>, _>>()?;
                    (RirVariantKind::Tuple, fields)
                }
                air::VariantShape::Struct(fields) => {
                    let mut seen_fields = vec![];
                    let fields = fields
                        .iter()
                        .enumerate()
                        .map(|(index, field)| {
                            self.enum_field(program, type_id, field.ty, index)
                                .map(|mut rir| {
                                    rir.symbol =
                                        scoped_symbol(field.name.as_str(), &mut seen_fields);
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
                fields,
            });
        }
        let rir = &mut program.enums[enum_id.index()];
        rir.copyable = rust_copyable_air_type(&self.classes, type_id);
        rir.variants = variants;
        Ok(())
    }

    fn enum_field(
        &self,
        program: &RirProgram,
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
        if ty == enum_ty || matches!(program.types[rir_ty.index()], RirType::Enum(_)) {
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
                ParamMode::Value if rust_copyable_air_type(&self.classes, ty) => {
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

    fn plan_consts(&mut self, program: &mut RirProgram) -> Result<(), RustPlanError> {
        for index in 0..self.air.const_arena.len() {
            let air_id = ConstId::from_index(index);
            let konst = self.air.const_arena.get(air_id);
            let id = RirConstId::from_index(program.consts.len());
            let value = match &konst.value {
                ConstValue::Int(value) => RirConstValue::Int(*value),
                ConstValue::Float(value) => RirConstValue::Float(*value),
                ConstValue::Bool(value) => RirConstValue::Bool(*value),
                ConstValue::String(value) => RirConstValue::String(value.to_string()),
                ConstValue::Nil => {
                    return Err(self.gap(
                        RustTargetGapSite::Const(air_id),
                        RustTargetGapKind::UnsupportedConst,
                    ));
                }
            };
            program.consts.push(RirConst {
                id,
                ty: self.type_map[&konst.ty],
                value,
            });
            self.const_map.insert(air_id, id);
        }
        Ok(())
    }

    fn plan_externs(&mut self, program: &mut RirProgram) -> Result<(), RustPlanError> {
        for index in 0..self.air.externs.len() {
            let air_id = ExternId::from_index(index);
            let decl = self.air.extern_decl(air_id);
            let id = RirExternId::from_index(program.externs.len());
            let params = self.extern_params(decl);
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

    fn extern_params(&self, decl: &air::ExternDecl) -> Vec<RirExternParam> {
        decl.call_params()
            .into_iter()
            .map(|param| RirExternParam {
                ty: self.type_map[&param.ty],
                semantic: rir::semantic_from_air(param.mode),
                abi: abi_for(param.mode),
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
                path: native_path(native),
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
        let params = function
            .signature
            .params
            .iter()
            .map(|param| RirParam {
                local: RirLocalId::from_index(param.local_id.index()),
                ty: self.type_map[&param.ty],
                semantic: rir::semantic_from_air(param.mode),
                abi: abi_for(param.mode),
            })
            .collect();
        let body = self.plan_air_block(air_id, &function.body.block)?;
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
    ) -> Result<RirStructuredBlock, RustPlanError> {
        Ok(RirStructuredBlock {
            stmts: block
                .stmts
                .iter()
                .map(|stmt| self.plan_air_stmt(function, stmt))
                .collect::<Result<Vec<_>, _>>()?,
            term: self.plan_air_tail(function, &block.tail)?,
        })
    }

    fn plan_air_stmt(
        &self,
        function: FunctionId,
        stmt: &air::AirStmt,
    ) -> Result<RirStmt, RustPlanError> {
        match stmt {
            air::AirStmt::Init { local, value } => Ok(RirStmt::Init {
                local: RirLocalId::from_index(local.index()),
                value: self.plan_rvalue(function, value)?,
            }),
            air::AirStmt::Assign { dst, value } => Ok(RirStmt::Assign {
                dst: self.plan_place(dst),
                value: self.plan_rvalue(function, value)?,
            }),
            air::AirStmt::Eval(value) => Ok(RirStmt::Eval(self.plan_rvalue(function, value)?)),
            air::AirStmt::If(branch) => Ok(RirStmt::If(RirIf {
                cond: self.plan_operand(&branch.cond),
                then_block: self.plan_air_block(function, &branch.then_block)?,
                else_block: branch
                    .else_block
                    .as_ref()
                    .map(|block| self.plan_air_block(function, block))
                    .transpose()?,
            })),
            air::AirStmt::EnumMatch(match_) => Ok(RirStmt::EnumMatch(RirEnumMatch {
                discr: self.plan_place(&match_.discr),
                arms: match_
                    .arms
                    .iter()
                    .map(|arm| {
                        Ok(RirEnumMatchArm {
                            variant: RirVariantId::from_index(arm.variant.index()),
                            block: self.plan_air_block(function, &arm.block)?,
                        })
                    })
                    .collect::<Result<Vec<_>, RustPlanError>>()?,
                else_block: match_
                    .else_block
                    .as_ref()
                    .map(|block| self.plan_air_block(function, block))
                    .transpose()?,
            })),
            air::AirStmt::Loop(_) => Err(self.gap(
                RustTargetGapSite::Function(function),
                RustTargetGapKind::UnsupportedTerminator,
            )),
        }
    }

    fn plan_air_tail(
        &self,
        function: FunctionId,
        tail: &air::AirTail,
    ) -> Result<RirTerm, RustPlanError> {
        Ok(match tail {
            air::AirTail::None => RirTerm::None,
            air::AirTail::Return(value) => {
                RirTerm::Return(value.as_ref().map(|value| self.plan_operand(value)))
            }
            air::AirTail::Unreachable => RirTerm::Unreachable,
            air::AirTail::Break(_) | air::AirTail::Continue(_) => {
                return Err(self.gap(
                    RustTargetGapSite::Function(function),
                    RustTargetGapKind::UnsupportedTerminator,
                ));
            }
        })
    }

    fn plan_rvalue(
        &self,
        function: FunctionId,
        value: &RValue,
    ) -> Result<RirRValue, RustPlanError> {
        match value {
            RValue::Use(operand) => self.plan_use(function, operand),
            RValue::Unary { op, value, ty } => Ok(RirRValue::Unary {
                op: *op,
                value: self.plan_operand(value),
                ty: self.type_map[ty],
            }),
            RValue::Binary { op, lhs, rhs, ty } => Ok(RirRValue::Binary {
                op: *op,
                lhs: self.plan_operand(lhs),
                rhs: self.plan_operand(rhs),
                ty: self.type_map[ty],
            }),
            RValue::Cast { value, target } => Ok(RirRValue::Cast {
                value: self.plan_operand(value),
                target: self.type_map[target],
            }),
            RValue::Call { callee, args } => self.plan_call(function, callee, args),
            RValue::Stringify { value, source_ty } => Ok(RirRValue::Stringify {
                value: self.plan_operand(value),
                source_ty: self.type_map[source_ty],
            }),
            RValue::StringConcat { parts } => Ok(RirRValue::StringConcat {
                parts: parts.iter().map(|part| self.plan_operand(part)).collect(),
            }),
            RValue::Format { value, spec } => Ok(RirRValue::Format {
                value: self.plan_operand(value),
                source_ty: self.type_map[&self.operand_ty(value)],
                spec: rir_format_spec(*spec),
            }),
            RValue::Aggregate { kind, fields, ty } => {
                self.plan_aggregate(function, kind, fields, *ty)
            }
            RValue::Len { source } => Ok(RirRValue::Len {
                source: self.plan_place(source),
            }),
            RValue::ListPush { list, value } => Ok(RirRValue::ListPush {
                list: self.plan_place(list),
                value: self.plan_operand(value),
            }),
            RValue::SliceView {
                source,
                start,
                end,
                inclusive,
                ty,
            } => Ok(RirRValue::SliceView {
                source: self.plan_place(source),
                start: RirLocalId::from_index(start.index()),
                end: RirLocalId::from_index(end.index()),
                inclusive: *inclusive,
                ty: self.type_map[ty],
            }),
            RValue::ListSlice {
                source,
                start,
                end,
                inclusive,
                ty,
            } => Ok(RirRValue::ListSlice {
                source: self.plan_place(source),
                start: RirLocalId::from_index(start.index()),
                end: RirLocalId::from_index(end.index()),
                inclusive: *inclusive,
                ty: self.type_map[ty],
            }),
            RValue::SharedRefEq { .. }
            | RValue::ListPop { .. }
            | RValue::MapGet { .. }
            | RValue::MapInsert { .. }
            | RValue::MapRemove { .. }
            | RValue::MapEntryAt { .. }
            | RValue::MakeClosure { .. } => Err(self.gap(
                RustTargetGapSite::Function(function),
                RustTargetGapKind::UnsupportedRValue,
            )),
        }
    }

    fn plan_use(
        &self,
        function: FunctionId,
        operand: &Operand,
    ) -> Result<RirRValue, RustPlanError> {
        let Operand::Place(place) = operand else {
            return Ok(RirRValue::Use(self.plan_operand(operand)));
        };
        if !rust_copyable_air_type(&self.classes, place.ty) {
            return Err(self.gap(
                RustTargetGapSite::Function(function),
                RustTargetGapKind::NonCopyValueRequired,
            ));
        }
        let TypeData::Aggregate(aggregate) = self.air.type_arena.data(place.ty) else {
            return Ok(RirRValue::Use(self.plan_operand(operand)));
        };
        let decl = self.air.aggregate(*aggregate);
        Ok(RirRValue::Struct {
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
        })
    }

    fn plan_aggregate(
        &self,
        function: FunctionId,
        kind: &AggregateCtor,
        fields: &[Operand],
        ty: TypeId,
    ) -> Result<RirRValue, RustPlanError> {
        match kind {
            AggregateCtor::Struct(_) | AggregateCtor::Extern(_) => Ok(RirRValue::Struct {
                ty: self.type_map[&ty],
                fields: fields
                    .iter()
                    .map(|field| self.plan_operand(field))
                    .collect(),
            }),
            AggregateCtor::EnumVariant { variant, .. } => Ok(RirRValue::EnumVariant {
                ty: self.type_map[&ty],
                variant: RirVariantId::from_index(variant.index()),
                fields: fields
                    .iter()
                    .map(|field| self.plan_operand(field))
                    .collect(),
            }),
            AggregateCtor::Array => Ok(RirRValue::Array {
                ty: self.type_map[&ty],
                elems: fields
                    .iter()
                    .map(|field| self.plan_operand(field))
                    .collect(),
            }),
            AggregateCtor::List => Ok(RirRValue::List {
                ty: self.type_map[&ty],
                elems: fields
                    .iter()
                    .map(|field| self.plan_operand(field))
                    .collect(),
            }),
            AggregateCtor::Tuple | AggregateCtor::Map | AggregateCtor::DataRef(_) => Err(self.gap(
                RustTargetGapSite::Function(function),
                RustTargetGapKind::UnsupportedRValue,
            )),
        }
    }

    fn plan_call(
        &self,
        function_id: FunctionId,
        callee: &Callee,
        args: &[CallArg],
    ) -> Result<RirRValue, RustPlanError> {
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
        Ok(RirRValue::Call {
            callee: target,
            args: args.iter().map(|arg| self.plan_arg(arg)).collect(),
            ty,
        })
    }

    fn plan_arg(&self, arg: &CallArg) -> RirCallArg {
        match arg {
            CallArg::Value(operand) => RirCallArg::Value(self.plan_operand(operand)),
            CallArg::SharedBorrow(place) => RirCallArg::SharedBorrow(self.plan_place(place)),
            CallArg::SharedStringConst(id) => RirCallArg::SharedStringConst(self.const_map[id]),
            CallArg::MutBorrow(place) => RirCallArg::MutBorrow(self.plan_place(place)),
        }
    }

    fn plan_operand(&self, operand: &Operand) -> RirOperand {
        match operand {
            Operand::Place(place) => RirOperand::Place(self.plan_place(place)),
            Operand::Const(id) => RirOperand::Const(self.const_map[id]),
        }
    }

    fn operand_ty(&self, operand: &Operand) -> TypeId {
        match operand {
            Operand::Place(place) => place.ty,
            Operand::Const(id) => self.air.const_arena.get(*id).ty,
        }
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

fn native_type_path(binding: &anvyx_runtime::RustTypeBinding) -> Vec<String> {
    let mut path = vec![binding.path.crate_name.clone()];
    path.extend(binding.path.segments.clone());
    path
}

fn native_path(binding: &RustExternBinding) -> Vec<String> {
    let mut path = vec![binding.path.crate_name.clone()];
    path.extend(binding.path.segments.clone());
    path
}

fn abi_for(mode: ParamMode) -> RirParamAbi {
    match mode {
        ParamMode::Value => RirParamAbi::Value,
        ParamMode::SharedBorrow => RirParamAbi::SharedBorrow,
        ParamMode::MutBorrow => RirParamAbi::MutBorrow,
    }
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
        RirType::Slice(elem) => format!("slice_{}", type_suffix(program, elem)),
        RirType::Array { elem, len } => format!("array_{}_{}", len, type_suffix(program, elem)),
        RirType::Struct(id) => named_type_suffix(ty.index(), &program.structs[id.index()].display),
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

fn rust_copyable_air_type(classes: &TypePassClasses, ty: TypeId) -> bool {
    classes.get(ty).is_some_and(|class| {
        matches!(
            class,
            TypePassClass::Immediate | TypePassClass::SmallCopyInline(_)
        )
    })
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
