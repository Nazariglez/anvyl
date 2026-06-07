use anvyx_frontend::{
    air::{
        self, AggregateCtor, AggregateId, AggregateKind, CallArg, Callee, ConstId, ConstValue,
        EnumId, ExternDecl, ExternId, Function, FunctionId, FunctionKind, Local, LocalId,
        LocalKind, Module, Mutability, Operand, Param, ParamMode, ParamRole, Place, PlaceRoot,
        Program, Projection, RValue, ReturnMode, TypeData, TypeId, TypePassClasses, VariantDecl,
        VariantShape, VerifiedProgram,
    },
    ast::{FormatKind, FormatSign, FormatSpec},
};

use super::rep_policy::AirRustRepPolicy;

#[derive(Debug, Clone, Copy, Default)]
pub struct RustBackendProfile;

impl RustBackendProfile {
    pub fn check(program: &VerifiedProgram<'_>) -> Result<(), Vec<RustBackendProfileError>> {
        let program = program.program();
        let mut cx = ProfileCx {
            program,
            classes: TypePassClasses::analyze(program),
            errors: vec![],
        };
        cx.check();
        if cx.errors.is_empty() {
            Ok(())
        } else {
            Err(cx.errors)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RustBackendProfileError {
    pub site: ProfileSite,
    pub kind: ProfileErrorKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProfileSite {
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
pub enum ProfileErrorKind {
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
    UnsupportedLambdaValue,
    UnsupportedLambdaCapture,
    UnsupportedLambdaCell,
    UnsupportedLambdaExternBoundary,
    NonCopyValueRequired,
}

struct ProfileCx<'a> {
    program: &'a Program,
    classes: TypePassClasses,
    errors: Vec<RustBackendProfileError>,
}

fn unsupported_place_root(root: PlaceRoot) -> ProfileErrorKind {
    match root {
        PlaceRoot::UpvalueCell(_) => ProfileErrorKind::UnsupportedLambdaCell,
        PlaceRoot::LambdaCapture(_) | PlaceRoot::ScopedBorrow(_) => {
            ProfileErrorKind::UnsupportedLambdaCapture
        }
        PlaceRoot::Global(_) => ProfileErrorKind::UnsupportedPlaceRoot,
        PlaceRoot::Local(_) => unreachable!("local roots are supported"),
    }
}

impl ProfileCx<'_> {
    fn check(&mut self) {
        self.check_entry();
        for index in 0..self.program.type_arena.len() {
            self.check_type(TypeId::from_index(index));
        }
        for index in 0..self.program.const_arena.len() {
            self.check_const(ConstId::from_index(index));
        }
        for (index, module) in self.program.modules.iter().enumerate() {
            self.check_module(index, module);
        }
        for index in 0..self.program.externs.len() {
            let id = ExternId::from_index(index);
            self.check_extern(id, self.program.extern_decl(id));
        }
        for index in 0..self.program.functions.len() {
            let id = FunctionId::from_index(index);
            self.check_function(id, self.program.function(id));
        }
    }

    fn check_entry(&mut self) {
        let Some(entry) = self.program.entry() else {
            return;
        };
        let function = self.program.function(entry);
        if !function.signature.params.is_empty() {
            self.push(ProfileSite::Entry, ProfileErrorKind::UnsupportedEntry);
        }
    }

    fn check_type(&mut self, id: TypeId) {
        self.check_type_ref(ProfileSite::Type(id), id);
    }

    fn aggregate_decl_supported(&self, aggregate: AggregateId) -> bool {
        let decl = self.program.aggregate(aggregate);
        decl.kind == AggregateKind::Struct
            && decl
                .fields
                .iter()
                .all(|field| self.inline_payload_supported(field.ty))
    }

    fn dataref_decl_supported(&self, aggregate: AggregateId) -> bool {
        let decl = self.program.aggregate(aggregate);
        decl.kind == AggregateKind::DataRef
            && decl
                .fields
                .iter()
                .all(|field| self.dataref_payload_supported(field.ty))
    }

    fn inline_payload_supported(&self, ty: TypeId) -> bool {
        match self.program.type_arena.data(ty) {
            TypeData::Int
            | TypeData::Float
            | TypeData::Bool
            | TypeData::String
            | TypeData::DataRef(_) => true,
            TypeData::Aggregate(aggregate) => self.aggregate_decl_supported(*aggregate),
            TypeData::Enum(enm) => self.enum_decl_supported(*enm),
            TypeData::Optional(inner) | TypeData::Array { elem: inner, .. } => {
                self.inline_payload_supported(*inner)
            }
            TypeData::Tuple(elems) => elems
                .iter()
                .all(|elem| self.inline_payload_supported(*elem)),
            _ => false,
        }
    }

    fn dataref_payload_supported(&self, ty: TypeId) -> bool {
        self.policy().dataref_payload_supported(ty)
    }

    fn enum_decl_supported(&self, enm: EnumId) -> bool {
        let decl = self.program.enum_decl(enm);
        decl.variants.iter().all(|variant| {
            variant_field_tys(variant).all(|ty| self.enum_field_supported(decl.core, ty))
        })
    }

    fn enum_field_supported(&self, core: Option<air::CoreEnumKind>, ty: TypeId) -> bool {
        match self.program.type_arena.data(ty) {
            TypeData::Aggregate(aggregate) => self.aggregate_decl_supported(*aggregate),
            TypeData::Enum(_) if core == Some(air::CoreEnumKind::Option) => true,
            TypeData::Enum(enm) => self.enum_decl_supported(*enm),
            TypeData::Optional(inner) | TypeData::Array { elem: inner, .. } => {
                self.inline_payload_supported(*inner)
            }
            TypeData::Tuple(elems) => elems
                .iter()
                .all(|elem| self.enum_field_supported(core, *elem)),
            TypeData::DataRef(_)
            | TypeData::Int
            | TypeData::Float
            | TypeData::Bool
            | TypeData::String => true,
            _ => false,
        }
    }

    fn extern_type_supported(&self, ext: air::ExternTypeId) -> bool {
        let decl = self.program.extern_type(ext);
        decl.rep == air::ExternRep::Inline
            && decl.fields.iter().all(|field| {
                matches!(
                    self.program.type_arena.data(field.ty),
                    TypeData::Int | TypeData::Float | TypeData::Bool | TypeData::String
                )
            })
    }

    fn check_const(&mut self, id: ConstId) {
        if !const_is_slice1(self.program, id) {
            self.push(ProfileSite::Const(id), ProfileErrorKind::UnsupportedConst);
        }
    }

    fn check_module(&mut self, index: usize, module: &Module) {
        for aggregate in &module.aggregates {
            let decl = self.program.aggregate(*aggregate);
            let supported = match decl.kind {
                AggregateKind::Struct => self.aggregate_decl_supported(*aggregate),
                AggregateKind::DataRef => self.dataref_decl_supported(*aggregate),
            };
            if !supported {
                self.push(
                    ProfileSite::Module(index),
                    ProfileErrorKind::UnsupportedModuleItem,
                );
            }
            for field in &decl.fields {
                self.check_type_ref(ProfileSite::Module(index), field.ty);
            }
        }
        for enm in &module.enums {
            if !self.enum_decl_supported(*enm) {
                self.push(
                    ProfileSite::Module(index),
                    ProfileErrorKind::UnsupportedModuleItem,
                );
            }
            for variant in &self.program.enum_decl(*enm).variants {
                for ty in variant_field_tys(variant) {
                    self.check_type_ref(ProfileSite::Module(index), ty);
                }
            }
        }
    }

    fn check_function(&mut self, id: FunctionId, function: &Function) {
        if !matches!(
            function.kind,
            FunctionKind::Normal | FunctionKind::Method | FunctionKind::Lambda(_)
        ) {
            self.push(
                ProfileSite::Function(id),
                ProfileErrorKind::UnsupportedFunctionKind,
            );
        }
        if let FunctionKind::Lambda(lambda) = function.kind {
            match self.program.lambdas.get(lambda.index()) {
                Some(decl) if decl.captures.is_empty() => {}
                Some(_) => self.push(
                    ProfileSite::Function(id),
                    ProfileErrorKind::UnsupportedLambdaCapture,
                ),
                None => self.push(
                    ProfileSite::Function(id),
                    ProfileErrorKind::UnsupportedLambdaValue,
                ),
            }
        }
        if matches!(function.signature.return_mode, ReturnMode::Place(_))
            || matches!(
                self.program
                    .type_arena
                    .data(function.signature.return_type()),
                TypeData::Slice(_)
            )
        {
            self.push(
                ProfileSite::Function(id),
                ProfileErrorKind::UnsupportedReturnMode,
            );
        }
        self.check_type_ref(ProfileSite::Function(id), function.signature.return_type());
        for (index, param) in function.signature.params.iter().enumerate() {
            self.check_param(id, function.kind, index, param);
        }
        for index in 0..function.locals.len() {
            let local = LocalId::from_index(index);
            self.check_local(id, local, &function.locals[index]);
        }
        self.check_air_block(id, &function.body.block);
    }

    fn check_air_block(&mut self, function: FunctionId, body: &air::AirBlock) {
        for (index, stmt) in body.stmts.iter().enumerate() {
            let site = ProfileSite::Statement(function, index);
            match stmt {
                air::AirStmt::Init { value, .. } | air::AirStmt::Eval(value) => {
                    self.check_mutating_rvalue(site, function, value);
                    self.check_rvalue(site, value);
                }
                air::AirStmt::Assign { dst, value } => {
                    self.check_rvalue(site, value);
                    self.check_place(site, dst);
                }
                air::AirStmt::If(branch) => {
                    self.check_operand(site, &branch.cond);
                    self.check_air_block(function, &branch.then_block);
                    if let Some(else_block) = &branch.else_block {
                        self.check_air_block(function, else_block);
                    }
                }
                air::AirStmt::EnumMatch(match_) => {
                    self.check_place(site, &match_.discr);
                    for arm in &match_.arms {
                        self.check_air_block(function, &arm.block);
                    }
                    if let Some(else_block) = &match_.else_block {
                        self.check_air_block(function, else_block);
                    }
                }
                air::AirStmt::OptionalMatch(match_) => {
                    self.check_place(site, &match_.discr);
                    if match_.payload_ref && self.place_crosses_dataref(site, &match_.discr) {
                        self.push(site, ProfileErrorKind::UnsupportedPlaceProjection);
                    }
                    if match_.payload.is_some()
                        && !match_.payload_ref
                        && let TypeData::Optional(inner) =
                            self.program.type_arena.data(match_.discr.ty)
                        && !self.policy().value_from_ref_supported(*inner)
                    {
                        self.push(site, ProfileErrorKind::NonCopyValueRequired);
                    }
                    self.check_air_block(function, &match_.some_block);
                    self.check_air_block(function, &match_.none_block);
                }
                air::AirStmt::Loop(loop_) => self.check_air_block(function, &loop_.body),
            }
        }
        if let air::AirTail::Return(Some(value)) = &body.tail {
            let site = ProfileSite::Terminator(function);
            self.check_operand(site, value);
        }
    }

    fn check_param(
        &mut self,
        function: FunctionId,
        kind: FunctionKind,
        index: usize,
        param: &Param,
    ) {
        let role_supported = matches!(
            (kind, index, param.role),
            (FunctionKind::Method, 0, ParamRole::Receiver) | (_, _, ParamRole::Normal)
        );
        if !role_supported {
            self.push(
                ProfileSite::Param(function, index),
                ProfileErrorKind::UnsupportedParamRole,
            );
        }
        if !self.supports_param_mode(param.ty, param.mode) {
            self.push(
                ProfileSite::Param(function, index),
                ProfileErrorKind::UnsupportedParamMode,
            );
        }
        self.check_type_ref(ProfileSite::Param(function, index), param.ty);
    }

    fn check_local(&mut self, function: FunctionId, local: LocalId, data: &Local) {
        if !matches!(
            data.kind,
            LocalKind::Arg | LocalKind::Temp | LocalKind::User | LocalKind::PatternBinding
        ) {
            self.push(
                ProfileSite::Local(function, local),
                ProfileErrorKind::UnsupportedLocalKind,
            );
        }
        self.check_type_ref(ProfileSite::Local(function, local), data.ty);
    }

    fn check_mutating_rvalue(&mut self, site: ProfileSite, function: FunctionId, value: &RValue) {
        let place = match value {
            RValue::ListPush { list, .. } => list,
            RValue::MapInsert { map, .. } | RValue::MapRemove { map, .. } => map,
            _ => return,
        };
        let Some(root) = place.root.local() else {
            self.push(site, unsupported_place_root(place.root));
            return;
        };
        if self
            .program
            .function(function)
            .locals
            .get(root.index())
            .is_some_and(|local| local.mutability != Mutability::Mutable)
        {
            self.push(site, ProfileErrorKind::UnsupportedRValue);
        }
    }

    fn check_rvalue(&mut self, site: ProfileSite, value: &RValue) {
        match value {
            RValue::FunctionRef { .. } => {}
            RValue::Use(operand) => {
                self.check_operand(site, operand);
                if self.non_shareable_value_operand(operand) {
                    self.push(site, ProfileErrorKind::NonCopyValueRequired);
                }
            }
            RValue::Unary { value, ty, .. } => {
                self.check_operand(site, value);
                self.check_type_ref(site, *ty);
            }
            RValue::Binary { lhs, rhs, ty, .. } => {
                self.check_operand(site, lhs);
                self.check_operand(site, rhs);
                self.check_type_ref(site, *ty);
            }
            RValue::Call { callee, args } => {
                self.check_callee(site, callee);
                for arg in args {
                    self.check_call_arg(site, arg);
                }
            }
            RValue::Cast { value, target } => {
                self.check_operand(site, value);
                self.check_type_ref(site, *target);
            }
            RValue::OptionalSome { value, ty } => {
                self.check_operand(site, value);
                self.check_type_ref(site, *ty);
                if self.non_shareable_value_operand(value) {
                    self.push(site, ProfileErrorKind::NonCopyValueRequired);
                }
            }
            RValue::Stringify { value, source_ty } => {
                self.check_operand(site, value);
                self.check_type_ref(site, *source_ty);
            }
            RValue::StringConcat { parts } => {
                for part in parts {
                    self.check_operand(site, part);
                    if !matches!(
                        self.program.type_arena.data(self.operand_ty(part)),
                        TypeData::String
                    ) {
                        self.push(site, ProfileErrorKind::UnsupportedRValue);
                    }
                }
            }
            RValue::Format { value, spec } => {
                self.check_operand(site, value);
                let ty = self.operand_ty(value);
                if !format_source_is_slice1(self.program.type_arena.data(ty), spec) {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                }
                self.check_type_ref(site, ty);
            }
            RValue::Aggregate { kind, fields, ty } => {
                self.check_aggregate_rvalue(site, kind, fields, *ty);
            }
            RValue::Len { source } => {
                self.check_place(site, source);
                if !matches!(
                    self.program.type_arena.data(source.ty),
                    TypeData::String
                        | TypeData::Array { .. }
                        | TypeData::List(_)
                        | TypeData::Slice(_)
                ) {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                }
            }
            RValue::ListPush { list, value } => {
                self.check_place(site, list);
                if self.place_crosses_dataref(site, list) {
                    self.push(site, ProfileErrorKind::UnsupportedPlaceProjection);
                }
                let TypeData::List(elem) = self.program.type_arena.data(list.ty) else {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                    return;
                };
                self.check_operand(site, value);
                if self.operand_ty(value) != *elem {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                }
                if self.non_shareable_value_operand(value) {
                    self.push(site, ProfileErrorKind::NonCopyValueRequired);
                }
            }
            RValue::SliceView {
                source,
                start,
                end,
                ty,
                ..
            } => self.check_slice_rvalue(site, source, *start, *end, *ty, false),
            RValue::ListSlice {
                source,
                start,
                end,
                ty,
                ..
            } => self.check_slice_rvalue(site, source, *start, *end, *ty, true),
            RValue::MapGet { map, key, ty } | RValue::MapRemove { map, key, ty } => {
                self.check_place(site, map);
                if matches!(value, RValue::MapRemove { .. })
                    && self.place_crosses_dataref(site, map)
                {
                    self.push(site, ProfileErrorKind::UnsupportedPlaceProjection);
                }
                self.check_operand(site, key);
                self.check_type_ref(site, *ty);
                let TypeData::Map {
                    key: expected_key,
                    value,
                    ..
                } = self.program.type_arena.data(map.ty)
                else {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                    return;
                };
                if self.operand_ty(key) != *expected_key {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                }
                if !matches!(self.program.type_arena.data(*ty), TypeData::Optional(inner) if *inner == *value)
                {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                }
            }
            RValue::MapInsert { map, key, value } => {
                self.check_place(site, map);
                if self.place_crosses_dataref(site, map) {
                    self.push(site, ProfileErrorKind::UnsupportedPlaceProjection);
                }
                self.check_operand(site, key);
                self.check_operand(site, value);
                let TypeData::Map {
                    key: expected_key,
                    value: expected_value,
                    ..
                } = self.program.type_arena.data(map.ty)
                else {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                    return;
                };
                if self.operand_ty(key) != *expected_key
                    || self.operand_ty(value) != *expected_value
                {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                }
            }
            RValue::SharedRefEq { lhs, rhs, .. } => {
                self.check_operand(site, lhs);
                self.check_operand(site, rhs);
                let lhs_ty = self.operand_ty(lhs);
                let rhs_ty = self.operand_ty(rhs);
                if lhs_ty != rhs_ty
                    || !matches!(self.program.type_arena.data(lhs_ty), TypeData::DataRef(_))
                {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                }
            }
            RValue::ListPop { list, .. } => {
                self.check_place(site, list);
                self.push(site, ProfileErrorKind::UnsupportedRValue);
            }
            RValue::MapEntryAt { map, .. } => {
                self.check_place(site, map);
                self.push(site, ProfileErrorKind::UnsupportedRValue);
            }
            RValue::MakeLambda { captures, .. } => {
                for capture in captures {
                    match capture {
                        air::LambdaCaptureArg::UpvalueCell { .. } => {
                            self.push(site, ProfileErrorKind::UnsupportedLambdaCell);
                        }
                        air::LambdaCaptureArg::ReadonlyLocal { value } => {
                            self.check_operand(site, value);
                            self.push(site, ProfileErrorKind::UnsupportedLambdaCapture);
                        }
                        air::LambdaCaptureArg::ScopedLocal { place }
                        | air::LambdaCaptureArg::ScopedBorrow { place } => {
                            self.check_place(site, place);
                            self.push(site, ProfileErrorKind::UnsupportedLambdaCapture);
                        }
                        air::LambdaCaptureArg::NoRuntime => {}
                    }
                }
            }
        }
    }

    fn check_aggregate_rvalue(
        &mut self,
        site: ProfileSite,
        kind: &AggregateCtor,
        fields: &[Operand],
        ty: TypeId,
    ) {
        match kind {
            AggregateCtor::Struct(aggregate) | AggregateCtor::DataRef(aggregate) => {
                let (expected_kind, type_matches) = match kind {
                    AggregateCtor::Struct(_) => (
                        AggregateKind::Struct,
                        matches!(self.program.type_arena.data(ty), TypeData::Aggregate(id) if id == aggregate),
                    ),
                    AggregateCtor::DataRef(_) => (
                        AggregateKind::DataRef,
                        matches!(self.program.type_arena.data(ty), TypeData::DataRef(id) if id == aggregate),
                    ),
                    _ => unreachable!(),
                };
                if !type_matches {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                }
                let decl = self.program.aggregate(*aggregate);
                if decl.kind != expected_kind || decl.fields.len() != fields.len() {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                }
                let expected = decl.fields.iter().map(|field| field.ty).collect::<Vec<_>>();
                self.check_value_fields(site, fields, expected);
            }
            AggregateCtor::Extern(ext) => {
                if !matches!(self.program.type_arena.data(ty), TypeData::Extern(id) if id == ext) {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                    return;
                }
                let decl = self.program.extern_type(*ext);
                if decl.rep != air::ExternRep::Inline {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                    return;
                }
                let Some(expected) = decl.constructor_fields() else {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                    return;
                };
                let expected = expected.map(|(_, field)| field.ty).collect::<Vec<_>>();
                if expected.len() != fields.len() {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                }
                self.check_value_fields(site, fields, expected);
            }
            AggregateCtor::EnumVariant { enum_id, variant } => {
                if !matches!(self.program.type_arena.data(ty), TypeData::Enum(id) if id == enum_id)
                {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                    return;
                }
                let Some(variant) = self
                    .program
                    .enum_decl(*enum_id)
                    .variants
                    .get(variant.index())
                else {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                    return;
                };
                if variant_field_count(variant) != fields.len() {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                }
                let expected = variant_field_tys(variant).collect::<Vec<_>>();
                self.check_value_fields(site, fields, expected);
            }
            AggregateCtor::Array => {
                let TypeData::Array { elem, len } = self.program.type_arena.data(ty) else {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                    return;
                };
                if *len != fields.len() {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                }
                self.check_value_fields(site, fields, std::iter::repeat_n(*elem, fields.len()));
            }
            AggregateCtor::List => {
                let TypeData::List(elem) = self.program.type_arena.data(ty) else {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                    return;
                };
                self.check_value_fields(site, fields, std::iter::repeat_n(*elem, fields.len()));
            }
            AggregateCtor::Map => {
                let TypeData::Map { key, value, .. } = self.program.type_arena.data(ty) else {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                    return;
                };
                if !fields.len().is_multiple_of(2) {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                }
                for entry in fields.chunks_exact(2) {
                    for (operand, expected) in [(&entry[0], *key), (&entry[1], *value)] {
                        self.check_operand(site, operand);
                        if self.operand_ty(operand) != expected {
                            self.push(site, ProfileErrorKind::UnsupportedRValue);
                        }
                    }
                }
            }
            AggregateCtor::Tuple => {
                let TypeData::Tuple(elems) = self.program.type_arena.data(ty) else {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                    return;
                };
                if elems.len() != fields.len() {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                }
                self.check_value_fields(site, fields, elems.iter().copied());
            }
        }
    }

    fn check_value_fields(
        &mut self,
        site: ProfileSite,
        fields: &[Operand],
        expected: impl IntoIterator<Item = TypeId>,
    ) {
        for (operand, expected) in fields.iter().zip(expected) {
            self.check_operand(site, operand);
            if self.operand_ty(operand) != expected {
                self.push(site, ProfileErrorKind::UnsupportedRValue);
            }
            if self.non_shareable_value_operand(operand) {
                self.push(site, ProfileErrorKind::NonCopyValueRequired);
            }
        }
    }

    fn check_slice_rvalue(
        &mut self,
        site: ProfileSite,
        source: &Place,
        start: LocalId,
        end: LocalId,
        ty: TypeId,
        owned: bool,
    ) {
        self.check_place(site, source);
        for local in [start, end] {
            let Some(data) = self.current_local(site, local) else {
                self.push(site, ProfileErrorKind::UnsupportedRValue);
                continue;
            };
            if !matches!(self.program.type_arena.data(data.ty), TypeData::Int) {
                self.push(site, ProfileErrorKind::UnsupportedRValue);
            }
        }
        match (
            owned,
            self.program.type_arena.data(source.ty),
            self.program.type_arena.data(ty),
        ) {
            (
                false,
                TypeData::Array {
                    elem: source_elem, ..
                }
                | TypeData::List(source_elem)
                | TypeData::Slice(source_elem),
                TypeData::Slice(elem),
            ) if source_elem == elem => {}
            (true, TypeData::List(source_elem), TypeData::List(elem)) if source_elem == elem => {
                if !self.policy().value_place_shareable(*elem) {
                    self.push(site, ProfileErrorKind::NonCopyValueRequired);
                }
            }
            _ => self.push(site, ProfileErrorKind::UnsupportedRValue),
        }
    }

    fn current_local(&self, site: ProfileSite, local: LocalId) -> Option<&Local> {
        let (ProfileSite::Statement(function, _) | ProfileSite::Terminator(function)) = site else {
            return None;
        };
        self.program.function(function).locals.get(local.index())
    }

    fn check_callee(&mut self, site: ProfileSite, callee: &Callee) {
        match callee {
            Callee::Function(_) => {}
            Callee::Extern(id) if self.program.extern_decl(*id).binding.is_some() => {}
            Callee::Extern(_) => {
                self.push(site, ProfileErrorKind::UnsupportedCallee);
            }
            Callee::Lambda(operand) => {
                self.check_operand(site, operand);
            }
        }
    }

    fn check_call_arg(&mut self, site: ProfileSite, arg: &CallArg) {
        match arg {
            CallArg::Value(operand) => {
                self.check_operand(site, operand);
                if self.non_shareable_value_operand(operand) {
                    self.push(site, ProfileErrorKind::NonCopyValueRequired);
                }
            }
            CallArg::SharedBorrow(place) => self.check_place(site, place),
            CallArg::SharedStringConst(_) => {}
            CallArg::MutBorrow(place) => {
                self.check_place(site, place);
                if !self.supports_param_mode(place.ty, ParamMode::MutBorrow) {
                    self.push(site, ProfileErrorKind::UnsupportedCallArgMode);
                }
            }
        }
    }

    fn non_shareable_value_operand(&self, operand: &Operand) -> bool {
        let Operand::Place(place) = operand else {
            return false;
        };
        self.non_copy_type(place.ty) && !self.policy().value_place_shareable(place.ty)
    }

    fn non_copy_type(&self, ty: TypeId) -> bool {
        !self.policy().copyable(ty)
    }

    fn operand_ty(&self, operand: &Operand) -> TypeId {
        self.program
            .operand_ty(operand)
            .expect("verified AIR operand const should exist")
    }

    fn supports_param_mode(&self, ty: TypeId, mode: ParamMode) -> bool {
        self.policy().supports_param_mode(ty, mode)
    }

    fn check_operand(&mut self, site: ProfileSite, operand: &Operand) {
        match operand {
            Operand::Place(place) => self.check_place(site, place),
            Operand::Const(id) => self.check_const_ref(site, *id),
        }
    }

    fn check_place(&mut self, site: ProfileSite, place: &Place) {
        let Some(root) = place.root.local() else {
            self.push(site, unsupported_place_root(place.root));
            return;
        };
        let Some(local) = self.current_local(site, root) else {
            self.push(site, ProfileErrorKind::UnsupportedPlaceProjection);
            return;
        };
        let mut ty = local.ty;
        for projection in &place.projection {
            let Some(next) = self.projected_ty(ty, projection) else {
                self.push(site, ProfileErrorKind::UnsupportedPlaceProjection);
                return;
            };
            ty = next;
        }
        if ty != place.ty {
            self.push(site, ProfileErrorKind::UnsupportedPlaceProjection);
        }
        self.check_type_ref(site, place.ty);
    }

    fn place_crosses_dataref(&self, site: ProfileSite, place: &Place) -> bool {
        let Some(root) = place.root.local() else {
            return false;
        };
        let Some(local) = self.current_local(site, root) else {
            return false;
        };
        let mut ty = local.ty;
        for projection in &place.projection {
            if matches!(self.program.type_arena.data(ty), TypeData::DataRef(_)) {
                return true;
            }
            let Some(next) = self.projected_ty(ty, projection) else {
                return false;
            };
            ty = next;
        }
        false
    }

    fn projected_ty(&self, ty: TypeId, projection: &Projection) -> Option<TypeId> {
        match (self.program.type_arena.data(ty), projection) {
            (
                TypeData::Aggregate(aggregate) | TypeData::DataRef(aggregate),
                Projection::Field(field),
            ) => self
                .program
                .aggregate(*aggregate)
                .fields
                .get(field.index())
                .map(|field| field.ty),
            (
                TypeData::Array { elem, .. } | TypeData::List(elem) | TypeData::Slice(elem),
                Projection::Index(_),
            ) => Some(*elem),
            (TypeData::Tuple(fields), Projection::TupleField(field)) => {
                fields.get(*field as usize).copied()
            }
            _ => None,
        }
    }

    fn check_extern(&mut self, id: ExternId, decl: &ExternDecl) {
        if decl.binding.is_none() {
            self.push(ProfileSite::Extern(id), ProfileErrorKind::UnsupportedExtern);
        }
        if decl
            .call_params()
            .any(|param| self.type_contains_function(param.ty))
            || self.type_contains_function(decl.return_type)
        {
            self.push(
                ProfileSite::Extern(id),
                ProfileErrorKind::UnsupportedLambdaExternBoundary,
            );
        }
    }

    fn type_contains_function(&self, ty: TypeId) -> bool {
        self.type_contains_function_inner(ty, &mut std::collections::HashSet::new())
    }

    fn type_contains_function_inner(
        &self,
        ty: TypeId,
        visited: &mut std::collections::HashSet<TypeId>,
    ) -> bool {
        if !visited.insert(ty) {
            return false;
        }
        match self.program.type_arena.data(ty) {
            TypeData::Function(_) => true,
            TypeData::Optional(inner)
            | TypeData::Array { elem: inner, .. }
            | TypeData::Slice(inner) => self.type_contains_function_inner(*inner, visited),
            TypeData::List(elem) => self.type_contains_function_inner(*elem, visited),
            TypeData::Map { key, value, .. } => {
                self.type_contains_function_inner(*key, visited)
                    || self.type_contains_function_inner(*value, visited)
            }
            TypeData::Tuple(elems) => elems
                .iter()
                .any(|elem| self.type_contains_function_inner(*elem, visited)),
            TypeData::Aggregate(aggregate) | TypeData::DataRef(aggregate) => self
                .program
                .aggregate(*aggregate)
                .fields
                .iter()
                .any(|field| self.type_contains_function_inner(field.ty, visited)),
            TypeData::Enum(enm) => self.program.enum_decl(*enm).variants.iter().any(|variant| {
                variant_field_tys(variant).any(|ty| self.type_contains_function_inner(ty, visited))
            }),
            TypeData::Extern(ext) => self
                .program
                .extern_type(*ext)
                .fields
                .iter()
                .any(|field| self.type_contains_function_inner(field.ty, visited)),
            _ => false,
        }
    }

    fn check_type_ref(&mut self, site: ProfileSite, ty: TypeId) {
        let ok = match self.program.type_arena.data(ty) {
            TypeData::Aggregate(aggregate) => {
                self.reject_function_container(site, ty)
                    || self.aggregate_decl_supported(*aggregate)
            }
            TypeData::DataRef(aggregate) => {
                self.reject_function_container(site, ty) || self.dataref_decl_supported(*aggregate)
            }
            TypeData::Enum(enm) => {
                self.reject_function_container(site, ty) || self.enum_decl_supported(*enm)
            }
            TypeData::Extern(ext) => {
                self.reject_function_container(site, ty) || self.extern_type_supported(*ext)
            }
            TypeData::Array { elem, .. } => {
                if self.reject_function_container(site, *elem) {
                    true
                } else {
                    self.check_type_ref(site, *elem);
                    !self.non_copy_type(*elem)
                }
            }
            TypeData::List(elem) => {
                if self.reject_function_container(site, *elem) {
                    true
                } else {
                    self.check_type_ref(site, *elem);
                    self.policy().list_supported(ty)
                }
            }
            TypeData::Slice(elem) => {
                if !self.reject_function_container(site, *elem) {
                    self.check_type_ref(site, *elem);
                }
                true
            }
            TypeData::Map { key, value, .. } => {
                let has_function = self.reject_function_container(site, *key)
                    | self.reject_function_container(site, *value);
                if has_function {
                    true
                } else {
                    self.check_type_ref(site, *key);
                    self.check_type_ref(site, *value);
                    self.policy().map_supported(ty)
                }
            }
            TypeData::Optional(inner) => {
                if !self.reject_function_container(site, *inner) {
                    self.check_type_ref(site, *inner);
                }
                true
            }
            TypeData::Tuple(elems) => {
                if elems
                    .iter()
                    .any(|elem| self.reject_function_container(site, *elem))
                {
                    true
                } else {
                    for elem in elems {
                        self.check_type_ref(site, *elem);
                    }
                    true
                }
            }
            TypeData::Function(sig) => {
                for param in &sig.params {
                    self.check_type_ref(site, param.ty);
                }
                self.check_type_ref(site, sig.ret.ty());
                true
            }
            ty => type_is_slice1(ty),
        };
        if !ok {
            self.push(site, ProfileErrorKind::UnsupportedType);
        }
    }

    fn reject_function_container(&mut self, site: ProfileSite, ty: TypeId) -> bool {
        let contains = self.type_contains_function(ty);
        if contains {
            self.push(site, ProfileErrorKind::UnsupportedLambdaValue);
        }
        contains
    }

    fn check_const_ref(&mut self, site: ProfileSite, id: ConstId) {
        if !const_is_slice1(self.program, id) {
            self.push(site, ProfileErrorKind::UnsupportedConst);
        }
    }

    fn policy(&self) -> AirRustRepPolicy<'_> {
        AirRustRepPolicy::new(self.program, &self.classes)
    }

    fn push(&mut self, site: ProfileSite, kind: ProfileErrorKind) {
        self.errors.push(RustBackendProfileError { site, kind });
    }
}

fn variant_field_tys(variant: &VariantDecl) -> impl Iterator<Item = TypeId> + '_ {
    let tuple = match &variant.shape {
        VariantShape::Tuple(fields) => fields.as_slice(),
        _ => &[],
    };
    let strukt = match &variant.shape {
        VariantShape::Struct(fields) => fields.as_slice(),
        _ => &[],
    };
    tuple
        .iter()
        .copied()
        .chain(strukt.iter().map(|field| field.ty))
}

fn variant_field_count(variant: &VariantDecl) -> usize {
    match &variant.shape {
        VariantShape::Unit => 0,
        VariantShape::Tuple(fields) => fields.len(),
        VariantShape::Struct(fields) => fields.len(),
    }
}

fn type_is_slice1(ty: &TypeData) -> bool {
    scalar_type_is_slice1(ty) || matches!(ty, TypeData::Void)
}

fn format_source_is_slice1(ty: &TypeData, spec: &FormatSpec) -> bool {
    if !scalar_type_is_slice1(ty) {
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
    if spec.sign == FormatSign::Always && !matches!(ty, TypeData::Int | TypeData::Float) {
        return false;
    }
    true
}

fn scalar_type_is_slice1(ty: &TypeData) -> bool {
    matches!(
        ty,
        TypeData::Int | TypeData::Float | TypeData::Bool | TypeData::String
    )
}

fn const_is_slice1(program: &Program, id: ConstId) -> bool {
    let konst = program.const_arena.get(id);
    matches!(
        (program.type_arena.data(konst.ty), &konst.value),
        (TypeData::Int, ConstValue::Int(_))
            | (TypeData::Float, ConstValue::Float(_))
            | (TypeData::Bool, ConstValue::Bool(_))
            | (TypeData::String, ConstValue::String(_))
            | (TypeData::Optional(_), ConstValue::Nil)
    )
}
