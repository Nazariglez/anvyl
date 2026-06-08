use std::collections::BTreeSet;

use anvyx_frontend::air::{
    self, ParamMode, Program as AirProgram, TypeData, TypeId, TypePassClass, TypePassClasses,
    VariantShape,
};

use super::{
    rir::{
        RirDataRef, RirEnum, RirEnumId, RirField, RirLambdaSigId, RirParamAbi, RirParamSemantic,
        RirProgram, RirStruct, RirStructId, RirTuple, RirTupleId, RirType, RirTypeId,
    },
    target,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustValueRep {
    InlineCopy,
    InlineArray,
    InlineStruct,
    InlineEnum,
    RawIntEnum,
    RawStringEnum,
    HeapHandle,
    CowString,
    CowList,
    CowMap,
    Opaque,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustBorrowView {
    Ref,
    Str,
    Slice,
    TargetGap,
}

#[derive(Debug, Clone, Copy)]
pub struct AirRustRepPolicy<'a> {
    program: &'a AirProgram,
    classes: &'a TypePassClasses,
}

impl<'a> AirRustRepPolicy<'a> {
    pub fn new(program: &'a AirProgram, classes: &'a TypePassClasses) -> Self {
        Self { program, classes }
    }

    pub fn copyable(self, ty: TypeId) -> bool {
        if let TypeData::Optional(inner) = self.program.type_arena.data(ty) {
            return self.copyable(*inner);
        }
        if matches!(self.program.type_arena.data(ty), TypeData::DataRef(_)) {
            return false;
        }
        if matches!(self.program.type_arena.data(ty), TypeData::Function(_)) {
            return true;
        }
        self.classes.get(ty).is_some_and(|class| {
            matches!(
                class,
                TypePassClass::Immediate | TypePassClass::SmallCopyInline(_)
            )
        })
    }

    pub fn value_place_shareable(self, ty: TypeId) -> bool {
        self.copyable(ty) || self.shareable_value(ty)
    }

    pub fn value_from_ref_supported(self, ty: TypeId) -> bool {
        match self.program.type_arena.data(ty) {
            TypeData::Int
            | TypeData::Float
            | TypeData::Bool
            | TypeData::String
            | TypeData::DataRef(_)
            | TypeData::List(_)
            | TypeData::Map { .. }
            | TypeData::Function(_) => true,
            TypeData::Optional(inner) | TypeData::Array { elem: inner, .. } => {
                self.value_from_ref_supported(*inner)
            }
            TypeData::Tuple(elems) => elems
                .iter()
                .all(|elem| self.value_from_ref_supported(*elem)),
            TypeData::Aggregate(id) => self
                .program
                .aggregate(*id)
                .fields
                .iter()
                .all(|field| self.value_from_ref_supported(field.ty)),
            TypeData::Enum(id) => self.enum_value_from_ref_supported(*id),
            TypeData::Extern(id) => self
                .program
                .extern_type(*id)
                .fields
                .iter()
                .all(|field| self.value_from_ref_supported(field.ty)),
            TypeData::Void | TypeData::Any | TypeData::Slice(_) | TypeData::Dyn(_) => false,
        }
    }

    pub fn dataref_payload_supported(self, ty: TypeId) -> bool {
        match self.program.type_arena.data(ty) {
            TypeData::DataRef(_) => true,
            TypeData::Optional(inner) => self.dataref_payload_supported(*inner),
            TypeData::Tuple(elems) => elems
                .iter()
                .all(|elem| self.dataref_payload_supported(*elem)),
            TypeData::Aggregate(aggregate) => self
                .program
                .aggregate(*aggregate)
                .fields
                .iter()
                .all(|field| self.dataref_payload_supported(field.ty)),
            TypeData::Enum(enm) => self.program.enum_decl(*enm).variants.iter().all(|variant| {
                Self::variant_field_tys(variant).all(|ty| self.dataref_payload_supported(ty))
            }),
            TypeData::Int | TypeData::Float | TypeData::Bool | TypeData::String => true,
            TypeData::Void
            | TypeData::Any
            | TypeData::Array { .. }
            | TypeData::List(_)
            | TypeData::Map { .. }
            | TypeData::Slice(_)
            | TypeData::Extern(_)
            | TypeData::Function(_)
            | TypeData::Dyn(_) => false,
        }
    }

    pub fn list_supported(self, ty: TypeId) -> bool {
        let TypeData::List(elem) = self.program.type_arena.data(ty) else {
            return false;
        };
        self.value_place_shareable(*elem)
    }

    fn shareable_value(self, ty: TypeId) -> bool {
        match self.program.type_arena.data(ty) {
            TypeData::String | TypeData::DataRef(_) | TypeData::List(_) | TypeData::Map { .. } => {
                true
            }
            TypeData::Optional(inner) => self.value_place_shareable(*inner),
            TypeData::Tuple(elems) => elems.iter().all(|elem| self.value_place_shareable(*elem)),
            TypeData::Aggregate(id) => self
                .program
                .aggregate(*id)
                .fields
                .iter()
                .all(|field| self.value_place_shareable(field.ty)),
            TypeData::Enum(id) => self.enum_shareable(*id),
            _ => false,
        }
    }

    fn variant_field_tys(variant: &air::VariantDecl) -> Box<dyn Iterator<Item = TypeId> + '_> {
        match &variant.shape {
            VariantShape::Unit => Box::new(std::iter::empty()),
            VariantShape::Tuple(fields) => Box::new(fields.iter().copied()),
            VariantShape::Struct(fields) => Box::new(fields.iter().map(|field| field.ty)),
        }
    }

    fn enum_shareable(self, id: air::EnumId) -> bool {
        self.program.enum_decl(id).variants.iter().all(|variant| {
            let fields: &[TypeId] = match &variant.shape {
                VariantShape::Unit => &[],
                VariantShape::Tuple(fields) => fields,
                VariantShape::Struct(fields) => {
                    return fields
                        .iter()
                        .all(|field| self.value_place_shareable(field.ty));
                }
            };
            fields
                .iter()
                .all(|field| self.value_place_shareable(*field))
        })
    }

    fn enum_value_from_ref_supported(self, id: air::EnumId) -> bool {
        self.program.enum_decl(id).variants.iter().all(|variant| {
            let fields: &[TypeId] = match &variant.shape {
                VariantShape::Unit => &[],
                VariantShape::Tuple(fields) => fields,
                VariantShape::Struct(fields) => {
                    return fields
                        .iter()
                        .all(|field| self.value_from_ref_supported(field.ty));
                }
            };
            fields
                .iter()
                .all(|field| self.value_from_ref_supported(*field))
        })
    }

    pub fn map_supported(self, ty: TypeId) -> bool {
        let TypeData::Map { key, value, .. } = self.program.type_arena.data(ty) else {
            return false;
        };
        self.map_slot_supported(*key) && self.map_slot_supported(*value)
    }

    fn map_slot_supported(self, ty: TypeId) -> bool {
        matches!(
            self.program.type_arena.data(ty),
            TypeData::Int | TypeData::Float | TypeData::Bool | TypeData::String
        )
    }

    pub fn supports_param_mode(self, ty: TypeId, mode: ParamMode) -> bool {
        match mode {
            ParamMode::Value => match self.program.type_arena.data(ty) {
                TypeData::Optional(inner) => self.supports_param_mode(*inner, mode),
                TypeData::Tuple(elems) => elems
                    .iter()
                    .all(|elem| self.supports_param_mode(*elem, mode)),
                TypeData::Int
                | TypeData::Float
                | TypeData::Bool
                | TypeData::Void
                | TypeData::String
                | TypeData::Aggregate(_)
                | TypeData::DataRef(_)
                | TypeData::Enum(_)
                | TypeData::Extern(_)
                | TypeData::Array { .. }
                | TypeData::List(_)
                | TypeData::Map { .. } => true,
                TypeData::Function(_) => true,
                TypeData::Any | TypeData::Slice(_) | TypeData::Dyn(_) => false,
            },
            ParamMode::SharedBorrow => match self.program.type_arena.data(ty) {
                TypeData::Optional(inner) => self.supports_param_mode(*inner, mode),
                TypeData::Tuple(_) => true,
                TypeData::String
                | TypeData::Aggregate(_)
                | TypeData::DataRef(_)
                | TypeData::Enum(_)
                | TypeData::Extern(_)
                | TypeData::Array { .. }
                | TypeData::List(_)
                | TypeData::Map { .. } => true,
                TypeData::Int
                | TypeData::Float
                | TypeData::Bool
                | TypeData::Void
                | TypeData::Any
                | TypeData::Slice(_)
                | TypeData::Function(_)
                | TypeData::Dyn(_) => false,
            },
            ParamMode::MutBorrow => match self.program.type_arena.data(ty) {
                TypeData::Optional(inner) => self.supports_param_mode(*inner, mode),
                TypeData::Tuple(_) => true,
                TypeData::Int
                | TypeData::Float
                | TypeData::Bool
                | TypeData::String
                | TypeData::Aggregate(_)
                | TypeData::DataRef(_)
                | TypeData::Enum(_)
                | TypeData::Extern(_)
                | TypeData::Array { .. }
                | TypeData::List(_)
                | TypeData::Map { .. } => true,
                TypeData::Void
                | TypeData::Any
                | TypeData::Slice(_)
                | TypeData::Function(_)
                | TypeData::Dyn(_) => false,
            },
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct RustRepPolicy<'a> {
    program: &'a RirProgram,
}

impl<'a> RustRepPolicy<'a> {
    pub fn new(program: &'a RirProgram) -> Self {
        Self { program }
    }

    pub fn value_rep(self, ty: RirTypeId) -> RustValueRep {
        match self.ty(ty) {
            RirType::Int | RirType::Float | RirType::Bool | RirType::Void => {
                RustValueRep::InlineCopy
            }
            RirType::String => RustValueRep::CowString,
            RirType::Array { .. } => RustValueRep::InlineArray,
            RirType::List(_) => RustValueRep::CowList,
            RirType::Map { .. } => RustValueRep::CowMap,
            RirType::Option(_) => RustValueRep::InlineEnum,
            RirType::Slice(_) => RustValueRep::Opaque,
            RirType::Lambda(_) => RustValueRep::InlineCopy,
            RirType::Struct(_) | RirType::Tuple(_) => RustValueRep::InlineStruct,
            RirType::DataRef(_) => RustValueRep::HeapHandle,
            RirType::Enum(id) => self.enum_rep(id),
        }
    }

    pub fn cow_value(self, ty: RirTypeId) -> bool {
        matches!(
            self.value_rep(ty),
            RustValueRep::CowString | RustValueRep::CowList | RustValueRep::CowMap
        )
    }

    pub fn param_abi(self, semantic: RirParamSemantic) -> RirParamAbi {
        match semantic {
            RirParamSemantic::Value => RirParamAbi::Value,
            RirParamSemantic::SharedBorrow => RirParamAbi::SharedBorrow,
            RirParamSemantic::MutBorrow => RirParamAbi::MutBorrow,
            RirParamSemantic::MutPlace => RirParamAbi::MutPlace,
            RirParamSemantic::StackCell => RirParamAbi::StackCell,
        }
    }

    pub fn supports_param(self, ty: RirTypeId, semantic: RirParamSemantic) -> bool {
        self.ty_opt(ty)
            .is_some_and(|ty_data| self.supports_type_semantic(ty_data, semantic))
    }

    pub fn call_arg_abi(self, ty: RirTypeId, semantic: RirParamSemantic) -> Option<RirParamAbi> {
        self.supports_param(ty, semantic)
            .then(|| self.param_abi(semantic))
    }

    pub fn shareable_value(self, ty: RirTypeId) -> bool {
        self.copyable(ty)
            || match self.ty(ty) {
                RirType::String | RirType::DataRef(_) | RirType::List(_) | RirType::Map { .. } => {
                    true
                }
                RirType::Option(inner) | RirType::Array { elem: inner, .. } => {
                    self.shareable_value(inner)
                }
                RirType::Struct(id) => self.program.structs[id.index()]
                    .fields
                    .iter()
                    .all(|field| self.shareable_value(field.ty)),
                RirType::Tuple(id) => self.program.tuples[id.index()]
                    .fields
                    .iter()
                    .all(|field| self.shareable_value(field.ty)),
                RirType::Enum(id) => {
                    self.program.enums[id.index()]
                        .variants
                        .iter()
                        .all(|variant| {
                            variant
                                .fields
                                .iter()
                                .all(|field| self.shareable_value(field.ty))
                        })
                }
                _ => false,
            }
    }

    pub fn value_from_ref_supported(self, ty: RirTypeId) -> bool {
        match self.ty(ty) {
            RirType::Int
            | RirType::Float
            | RirType::Bool
            | RirType::String
            | RirType::DataRef(_)
            | RirType::List(_)
            | RirType::Map { .. } => true,
            RirType::Option(inner) | RirType::Array { elem: inner, .. } => {
                self.value_from_ref_supported(inner)
            }
            RirType::Struct(id) => self.program.structs[id.index()]
                .fields
                .iter()
                .all(|field| self.value_from_ref_supported(field.ty)),
            RirType::Tuple(id) => self.program.tuples[id.index()]
                .fields
                .iter()
                .all(|field| self.value_from_ref_supported(field.ty)),
            RirType::Enum(id) => self.program.enums[id.index()]
                .variants
                .iter()
                .all(|variant| {
                    variant
                        .fields
                        .iter()
                        .all(|field| self.value_from_ref_supported(field.ty))
                }),
            RirType::Slice(_) | RirType::Void => false,
            RirType::Lambda(_) => true,
        }
    }

    pub fn borrow_view(self, ty: RirTypeId) -> RustBorrowView {
        match self.ty(ty) {
            RirType::String => RustBorrowView::Str,
            RirType::Slice(_) => RustBorrowView::Slice,
            RirType::Void => RustBorrowView::TargetGap,
            _ => RustBorrowView::Ref,
        }
    }

    pub fn param_ty(self, ty: RirTypeId, abi: RirParamAbi) -> String {
        self.param_ty_with_lifetime(ty, abi, None)
    }

    pub fn capture_field_ty(self, ty: RirTypeId, abi: RirParamAbi) -> String {
        self.param_ty_with_lifetime(ty, abi, Some("'env"))
    }

    fn param_ty_with_lifetime(
        self,
        ty: RirTypeId,
        abi: RirParamAbi,
        lifetime: Option<&str>,
    ) -> String {
        let lifetime = lifetime.map_or(String::new(), |lifetime| format!("{lifetime} "));
        match abi {
            RirParamAbi::Value => self.rust_ty(ty),
            RirParamAbi::SharedBorrow => match self.borrow_view(ty) {
                RustBorrowView::Str => format!("&{lifetime}str"),
                _ => format!("&{lifetime}{}", self.rust_ty(ty)),
            },
            RirParamAbi::MutBorrow => format!("&{lifetime}mut {}", self.rust_ty(ty)),
            RirParamAbi::MutPlace => {
                let payload = self.rust_ty(ty);
                format!("{}<'_, 'cx, {payload}>", target::mut_place_ty())
            }
            RirParamAbi::StackCell => {
                let payload = self.rust_ty(ty);
                format!("&{lifetime}{}", target::stack_lambda_cell_ty(&payload))
            }
        }
    }

    pub fn dataref_storage_ty(self, dataref: &RirDataRef) -> String {
        let storage = dataref.storage_symbol();
        if self.dataref_cx_dependent(dataref) {
            format!("{storage}<'cx>")
        } else {
            storage
        }
    }

    pub fn lambda_sig_ty(self, id: RirLambdaSigId) -> String {
        let symbol = self.lambda_sig_symbol(id);
        if self.lambda_sig_needs_lifetime(id) {
            format!("{symbol}<'_>")
        } else {
            symbol
        }
    }

    pub fn lambda_sig_symbol(self, id: RirLambdaSigId) -> String {
        format!("LambdaSig{}", id.index())
    }

    pub fn lambda_sig_needs_lifetime(self, id: RirLambdaSigId) -> bool {
        self.program.lambdas_for_sig(id).any(|lambda| {
            lambda
                .captures
                .iter()
                .any(|capture| capture.abi != RirParamAbi::Value)
        })
    }

    pub fn lambda_sig_copyable(self, id: RirLambdaSigId) -> bool {
        self.lambda_sig_copyable_inner(id, &mut BTreeSet::new())
    }

    fn lambda_sig_copyable_inner(
        self,
        id: RirLambdaSigId,
        active: &mut BTreeSet<RirLambdaSigId>,
    ) -> bool {
        if !active.insert(id) {
            return false;
        }
        let copyable = self.program.lambdas_for_sig(id).all(|lambda| {
            lambda.captures.iter().all(|capture| match capture.abi {
                RirParamAbi::Value => self.copyable_inner(capture.ty, active),
                RirParamAbi::SharedBorrow | RirParamAbi::StackCell => true,
                RirParamAbi::MutBorrow | RirParamAbi::MutPlace => false,
            })
        });
        active.remove(&id);
        copyable
    }

    pub fn fields_cx_dependent(self, fields: &[RirField]) -> bool {
        fields.iter().any(|field| self.type_cx_dependent(field.ty))
    }

    pub fn struct_cx_dependent(self, strukt: &RirStruct) -> bool {
        self.fields_cx_dependent(&strukt.fields)
    }

    pub fn tuple_cx_dependent(self, tuple: &RirTuple) -> bool {
        self.fields_cx_dependent(&tuple.fields)
    }

    pub fn enum_cx_dependent(self, enm: &RirEnum) -> bool {
        enm.variants
            .iter()
            .any(|variant| self.fields_cx_dependent(&variant.fields))
    }

    pub fn rust_ty(self, ty: RirTypeId) -> String {
        match self.ty(ty) {
            RirType::Int => "i64".into(),
            RirType::Float => "f64".into(),
            RirType::Bool => "bool".into(),
            RirType::String => target::anv_string_ty(),
            RirType::Void => "()".into(),
            RirType::Struct(id) => self.named_ty(
                self.program.structs[id.index()].symbol.as_str(),
                self.type_cx_dependent(ty),
            ),
            RirType::DataRef(id) => {
                format!("{}<'cx>", self.program.datarefs[id.index()].symbol.as_str())
            }
            RirType::Enum(id) => self.named_ty(
                self.program.enums[id.index()].symbol.as_str(),
                self.type_cx_dependent(ty),
            ),
            RirType::Tuple(id) => self.named_ty(
                self.program.tuples[id.index()].symbol.as_str(),
                self.type_cx_dependent(ty),
            ),
            RirType::Array { elem, len } => format!("[{}; {len}]", self.rust_ty(elem)),
            RirType::List(elem) => target::anv_list_ty(self.rust_ty(elem)),
            RirType::Map { key, value } => {
                target::anv_map_ty(self.rust_ty(key), self.rust_ty(value))
            }
            RirType::Option(inner) => format!("Option<{}>", self.rust_ty(inner)),
            RirType::Slice(elem) => format!("&[{}]", self.rust_ty(elem)),
            RirType::Lambda(id) => self.lambda_sig_ty(id),
        }
    }

    pub fn dataref_cx_dependent(self, dataref: &RirDataRef) -> bool {
        dataref
            .fields
            .iter()
            .any(|field| self.type_cx_dependent(field.ty))
    }

    pub fn dataref_storage_tracked(self, dataref: &RirDataRef) -> bool {
        dataref.cycle_capable || self.dataref_cx_dependent(dataref)
    }

    pub fn type_cx_dependent(self, ty: RirTypeId) -> bool {
        match self.ty(ty) {
            RirType::DataRef(_) => true,
            RirType::Option(inner) | RirType::Array { elem: inner, .. } | RirType::List(inner) => {
                self.type_cx_dependent(inner)
            }
            RirType::Map { key, value } => {
                self.type_cx_dependent(key) || self.type_cx_dependent(value)
            }
            RirType::Struct(id) => self.program.structs[id.index()]
                .fields
                .iter()
                .any(|field| self.type_cx_dependent(field.ty)),
            RirType::Tuple(id) => self.program.tuples[id.index()]
                .fields
                .iter()
                .any(|field| self.type_cx_dependent(field.ty)),
            RirType::Enum(id) => self.program.enums[id.index()]
                .variants
                .iter()
                .any(|variant| {
                    variant
                        .fields
                        .iter()
                        .any(|field| self.type_cx_dependent(field.ty))
                }),
            _ => false,
        }
    }

    fn named_ty(self, symbol: &str, cx_dependent: bool) -> String {
        if cx_dependent {
            format!("{symbol}<'cx>")
        } else {
            symbol.into()
        }
    }

    pub fn copyable(self, ty: RirTypeId) -> bool {
        self.copyable_inner(ty, &mut BTreeSet::new())
    }

    fn copyable_inner(self, ty: RirTypeId, active: &mut BTreeSet<RirLambdaSigId>) -> bool {
        match self.ty(ty) {
            RirType::Int | RirType::Float | RirType::Bool | RirType::Void => true,
            RirType::String
            | RirType::DataRef(_)
            | RirType::List(_)
            | RirType::Map { .. }
            | RirType::Slice(_) => false,
            RirType::Lambda(id) => self.lambda_sig_copyable_inner(id, active),
            RirType::Option(inner) => self.copyable_inner(inner, active),
            RirType::Array { elem, .. } => self.copyable_inner(elem, active),
            RirType::Struct(id) => self.program.structs[id.index()].copyable,
            RirType::Enum(id) => self.program.enums[id.index()].copyable,
            RirType::Tuple(id) => self.program.tuples[id.index()].copyable,
        }
    }

    fn supports_type_semantic(self, ty: RirType, semantic: RirParamSemantic) -> bool {
        match semantic {
            RirParamSemantic::Value => match ty {
                RirType::Option(inner) => self.supports_param(inner, semantic),
                RirType::Int
                | RirType::Float
                | RirType::Bool
                | RirType::Void
                | RirType::String
                | RirType::Struct(_)
                | RirType::DataRef(_)
                | RirType::Enum(_)
                | RirType::Array { .. }
                | RirType::List(_)
                | RirType::Map { .. }
                | RirType::Lambda(_) => true,
                RirType::Tuple(id) => self.program.tuples[id.index()]
                    .fields
                    .iter()
                    .all(|field| self.supports_param(field.ty, semantic)),
                RirType::Slice(_) => false,
            },
            RirParamSemantic::SharedBorrow => match ty {
                RirType::Option(inner) => self.supports_param(inner, semantic),
                RirType::String
                | RirType::Struct(_)
                | RirType::Tuple(_)
                | RirType::DataRef(_)
                | RirType::Enum(_)
                | RirType::Array { .. }
                | RirType::List(_)
                | RirType::Map { .. } => true,
                RirType::Int
                | RirType::Float
                | RirType::Bool
                | RirType::Void
                | RirType::Slice(_)
                | RirType::Lambda(_) => false,
            },
            RirParamSemantic::MutBorrow => match ty {
                RirType::Option(inner) => self.supports_param(inner, semantic),
                RirType::Int
                | RirType::Float
                | RirType::Bool
                | RirType::String
                | RirType::Struct(_)
                | RirType::Tuple(_)
                | RirType::DataRef(_)
                | RirType::Enum(_)
                | RirType::Array { .. }
                | RirType::List(_)
                | RirType::Map { .. } => true,
                RirType::Void | RirType::Slice(_) | RirType::Lambda(_) => false,
            },
            RirParamSemantic::MutPlace | RirParamSemantic::StackCell => {
                !matches!(ty, RirType::Void)
            }
        }
    }

    fn enum_rep(self, id: RirEnumId) -> RustValueRep {
        let enum_ = &self.program.enums[id.index()];
        match enum_.raw_type {
            Some(ty) => match self.ty(ty) {
                RirType::String => RustValueRep::RawStringEnum,
                _ => RustValueRep::RawIntEnum,
            },
            None => RustValueRep::InlineEnum,
        }
    }

    fn ty(self, ty: RirTypeId) -> RirType {
        self.program.types[ty.index()]
    }

    fn ty_opt(self, ty: RirTypeId) -> Option<RirType> {
        self.program.types.get(ty.index()).copied()
    }
}

#[derive(Debug, Default, Clone)]
pub struct RustTracePlan {
    structs: BTreeSet<RirStructId>,
    enums: BTreeSet<RirEnumId>,
    tuples: BTreeSet<RirTupleId>,
    visited: BTreeSet<RirTypeId>,
}

impl RustTracePlan {
    pub fn build(program: &RirProgram) -> Self {
        let mut plan = Self::default();
        let policy = RustRepPolicy::new(program);
        for (index, ty) in program.types.iter().enumerate() {
            let id = RirTypeId::from_index(index);
            if matches!(
                ty,
                RirType::Struct(_) | RirType::Tuple(_) | RirType::Enum(_)
            ) && policy.type_cx_dependent(id)
            {
                plan.mark_type(program, id);
            }
        }
        for dataref in &program.datarefs {
            if policy.dataref_storage_tracked(dataref) {
                for field in &dataref.fields {
                    plan.mark_type(program, field.ty);
                }
            }
        }
        plan
    }

    pub fn needs_struct_trace(&self, id: RirStructId) -> bool {
        self.structs.contains(&id)
    }

    pub fn needs_enum_trace(&self, id: RirEnumId) -> bool {
        self.enums.contains(&id)
    }

    pub fn needs_tuple_trace(&self, id: RirTupleId) -> bool {
        self.tuples.contains(&id)
    }

    fn mark_type(&mut self, program: &RirProgram, ty: RirTypeId) {
        if !self.visited.insert(ty) {
            return;
        }
        match program.types[ty.index()] {
            RirType::Option(inner) => self.mark_type(program, inner),
            RirType::Struct(id) => {
                let strukt = &program.structs[id.index()];
                if strukt.native_path.is_some() {
                    return;
                }
                self.structs.insert(id);
                for field in &strukt.fields {
                    self.mark_type(program, field.ty);
                }
            }
            RirType::Tuple(id) => {
                self.tuples.insert(id);
                for field in &program.tuples[id.index()].fields {
                    self.mark_type(program, field.ty);
                }
            }
            RirType::Enum(id) => {
                self.enums.insert(id);
                for variant in &program.enums[id.index()].variants {
                    for field in &variant.fields {
                        self.mark_type(program, field.ty);
                    }
                }
            }
            RirType::Array { elem, .. } | RirType::List(elem) | RirType::Slice(elem) => {
                self.mark_type(program, elem);
            }
            RirType::Map { key, value } => {
                self.mark_type(program, key);
                self.mark_type(program, value);
            }
            RirType::Int
            | RirType::Float
            | RirType::Bool
            | RirType::String
            | RirType::Void
            | RirType::DataRef(_)
            | RirType::Lambda(_) => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use anvyx_frontend::{
        air::{self, FieldDecl, ParamMode, Program, TypeData, TypePassClasses},
        ast::Ident,
    };

    use super::{AirRustRepPolicy, RustBorrowView, RustRepPolicy, RustValueRep};
    use crate::rust::rir::{
        RirDataRef, RirDataRefId, RirField, RirFieldId, RirProgram, RirStruct, RirStructId,
        RirSymbol, RirType, RirTypeId,
    };

    #[test]
    fn policy_classifies_current_storage_reps() {
        let mut program = RirProgram::default();
        let int = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::Int);
        let string = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::String);
        let list = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::List(int));
        let array = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::Array { elem: int, len: 2 });
        let policy = RustRepPolicy::new(&program);

        assert_eq!(policy.value_rep(int), RustValueRep::InlineCopy);
        assert_eq!(policy.value_rep(string), RustValueRep::CowString);
        assert_eq!(policy.value_rep(list), RustValueRep::CowList);
        assert_eq!(policy.value_rep(array), RustValueRep::InlineArray);
    }

    #[test]
    fn air_policy_preserves_current_profile_param_modes() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let string = program.alloc_type(TypeData::String);
        let list = program.alloc_type(TypeData::List(int));
        let function = program.alloc_type(TypeData::Function(air::SignatureType::new(
            vec![],
            air::ReturnMode::Value(int),
        )));
        let classes = TypePassClasses::analyze(&program);
        let policy = AirRustRepPolicy::new(&program, &classes);

        assert!(policy.supports_param_mode(int, ParamMode::Value));
        assert!(policy.supports_param_mode(string, ParamMode::SharedBorrow));
        assert!(policy.supports_param_mode(string, ParamMode::Value));
        assert!(policy.supports_param_mode(list, ParamMode::Value));
        assert!(policy.supports_param_mode(list, ParamMode::MutBorrow));
        assert!(policy.copyable(function));
        assert!(policy.value_from_ref_supported(function));
    }

    #[test]
    fn policy_keeps_string_borrow_as_str_view() {
        let mut program = RirProgram::default();
        let string = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::String);
        let policy = RustRepPolicy::new(&program);

        assert_eq!(policy.borrow_view(string), RustBorrowView::Str);
        assert!(policy.cow_value(string));
    }

    #[test]
    fn policy_shares_structs_with_shareable_fields() {
        let mut program = RirProgram::default();
        let string = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::String);
        let label = RirTypeId::from_index(program.types.len());
        program
            .types
            .push(RirType::Struct(RirStructId::from_index(0)));
        program.structs.push(RirStruct {
            id: RirStructId::from_index(0),
            air_id: None,
            symbol: RirSymbol::new("Label"),
            display: RirSymbol::new("Label"),
            native_path: None,
            native_key: None,
            copyable: false,
            fields: vec![RirField {
                id: RirFieldId::from_index(0),
                symbol: RirSymbol::new("text"),
                ty: string,
            }],
        });
        let policy = RustRepPolicy::new(&program);

        assert!(!policy.copyable(label));
        assert!(policy.shareable_value(label));
    }

    #[test]
    fn policy_treats_datarefs_as_shared_handles_not_copy_payloads() {
        let mut program = RirProgram::default();
        let int = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::Int);
        let node = RirTypeId::from_index(program.types.len());
        program
            .types
            .push(RirType::DataRef(RirDataRefId::from_index(0)));
        program.datarefs.push(RirDataRef {
            id: RirDataRefId::from_index(0),
            air_id: air::AggregateId::from_index(0),
            symbol: RirSymbol::new("Node"),
            display: RirSymbol::new("Node"),
            cycle_capable: true,
            fields: vec![RirField {
                id: RirFieldId::from_index(0),
                symbol: RirSymbol::new("value"),
                ty: int,
            }],
        });
        let maybe_node = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::Option(node));
        let policy = RustRepPolicy::new(&program);

        assert_eq!(policy.value_rep(node), RustValueRep::HeapHandle);
        assert!(!policy.copyable(node));
        assert!(policy.shareable_value(node));
        assert!(policy.supports_param(node, crate::rust::rir::RirParamSemantic::Value));
        assert!(policy.supports_param(node, crate::rust::rir::RirParamSemantic::SharedBorrow));
        assert!(policy.supports_param(node, crate::rust::rir::RirParamSemantic::MutBorrow));
        assert_eq!(policy.borrow_view(node), RustBorrowView::Ref);
        assert_eq!(policy.rust_ty(node), "Node<'cx>");
        assert!(!policy.cow_value(node));
        assert!(!policy.copyable(maybe_node));
        assert!(policy.shareable_value(maybe_node));
    }

    #[test]
    fn air_policy_shares_aggregates_with_shareable_fields() {
        let mut program = Program::default();
        let string = program.alloc_type(TypeData::String);
        let module = program.alloc_module(air::Module {
            path: vec![],
            functions: vec![],
            aggregates: vec![],
            enums: vec![],
            extern_types: vec![],
            externs: vec![],
        });
        let aggregate = program.alloc_aggregate(air::AggregateDecl {
            name: Ident::new("Label"),
            module,
            kind: air::AggregateKind::Struct,
            type_args: vec![],
            const_args: vec![],
            fields: vec![FieldDecl {
                name: Ident::new("text"),
                ty: string,
            }],
            cycle_capable: false,
            stringify_override: None,
        });
        let label = program.alloc_type(TypeData::Aggregate(aggregate));
        let classes = TypePassClasses::analyze(&program);
        let policy = AirRustRepPolicy::new(&program, &classes);

        assert!(!policy.copyable(label));
        assert!(policy.value_place_shareable(label));
    }

    #[test]
    fn air_policy_treats_datarefs_as_shared_handles_not_copy_payloads() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let module = program.alloc_module(air::Module {
            path: vec![],
            functions: vec![],
            aggregates: vec![],
            enums: vec![],
            extern_types: vec![],
            externs: vec![],
        });
        let aggregate = program.alloc_aggregate(air::AggregateDecl {
            name: Ident::new("Node"),
            module,
            kind: air::AggregateKind::DataRef,
            type_args: vec![],
            const_args: vec![],
            fields: vec![FieldDecl {
                name: Ident::new("value"),
                ty: int,
            }],
            cycle_capable: true,
            stringify_override: None,
        });
        let node = program.alloc_type(TypeData::DataRef(aggregate));
        let maybe_node = program.alloc_type(TypeData::Optional(node));
        let classes = TypePassClasses::analyze(&program);
        let policy = AirRustRepPolicy::new(&program, &classes);

        assert!(!policy.copyable(node));
        assert!(policy.value_place_shareable(node));
        assert!(policy.supports_param_mode(node, ParamMode::Value));
        assert!(policy.supports_param_mode(node, ParamMode::SharedBorrow));
        assert!(policy.supports_param_mode(node, ParamMode::MutBorrow));
        assert!(!policy.copyable(maybe_node));
        assert!(policy.value_place_shareable(maybe_node));
    }
}
