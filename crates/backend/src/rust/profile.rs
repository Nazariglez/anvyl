use std::collections::HashSet;

use anvyx_frontend::{
    air::{
        self, AggregateCtor, AggregateId, AggregateKind, CallArg, Callee, ConstId, ConstValue,
        EnumId, ExternDecl, ExternId, Function, FunctionId, FunctionKind, GlobalId, Local, LocalId,
        LocalKind, Module, Mutability, Operand, Param, ParamMode, ParamRole, Place, PlaceRoot,
        Program, Projection, RValue, ReturnMode, TypeData, TypeId, TypePassClasses, VariantDecl,
        VariantShape, VerifiedProgram,
    },
    ast::{FormatKind, FormatSign, FormatSpec},
};
use anvyx_runtime::RustProviderSupport;

use super::{
    dataref_mut_place::{
        DataRefMutPlace, DataRefMutPlaceSupport, classify as classify_dataref_mut_place,
        projected_ty as air_projected_ty,
    },
    mut_place::{
        direct_native_mut_borrow_supported,
        projected_mut_place_supported as mut_place_projection_supported,
    },
    native,
    rep_policy::{AirRustRepPolicy, RustMaterialization},
    rir,
};

#[derive(Debug, Clone, Copy, Default)]
pub struct RustBackendProfile;

impl RustBackendProfile {
    pub fn check_with_native_support(
        program: &VerifiedProgram<'_>,
        native_providers: &[RustProviderSupport],
    ) -> Result<(), Vec<RustBackendProfileError>> {
        let program = program.program();
        let mut cx = ProfileCx {
            program,
            native_providers,
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
    Global(GlobalId),
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
    UnsupportedTerminator,
    UnsupportedRValue,
    UnsupportedCallee,
    UnsupportedExtern,
    UnsupportedExternMember,
    UnsupportedEntry,
    UnsupportedRustAbi,
    UnsupportedLambdaValue,
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
    UnsupportedMutablePlace,
    UnsupportedCollectionLoan,
    UnsupportedMutablePlaceProjection,
    UnsupportedMutablePlaceDataRef,
    UnsupportedMutablePlaceNativeBoundary,
    UnsupportedMapKey,
    UnsupportedMapValue,
    NonCopyValueRequired,
}

struct ProfileCx<'a> {
    program: &'a Program,
    native_providers: &'a [RustProviderSupport],
    classes: TypePassClasses,
    errors: Vec<RustBackendProfileError>,
}

fn unsupported_place_root(root: PlaceRoot) -> ProfileErrorKind {
    match root {
        PlaceRoot::CaptureCell(_) => ProfileErrorKind::UnsupportedLambdaCell,
        PlaceRoot::LambdaCapture(_) | PlaceRoot::ScopedBorrow(_) => {
            ProfileErrorKind::UnsupportedLambdaCapture
        }
        PlaceRoot::Global(_) => unreachable!("global roots require use-site gap classification"),
        PlaceRoot::Local(_) => unreachable!("local roots are supported"),
    }
}

fn unsupported_global_place(place: &Place, root: ProfileErrorKind) -> ProfileErrorKind {
    if place.projection.is_empty() {
        root
    } else {
        ProfileErrorKind::UnsupportedGlobalProjection
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
        for index in 0..self.program.globals.len() {
            let id = GlobalId::from_index(index);
            self.check_global(id, &self.program.globals[index]);
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

    fn check_global(&mut self, id: GlobalId, decl: &air::GlobalDecl) {
        if let Some(gap) = self.global_payload_gap(decl.ty) {
            self.push(ProfileSite::Global(id), gap);
        }
    }

    fn global_payload_gap(&self, ty: TypeId) -> Option<ProfileErrorKind> {
        match self.policy().exact_root_global_materialization(ty) {
            RustMaterialization::Copy
            | RustMaterialization::Share
            | RustMaterialization::CloneHandle => None,
            RustMaterialization::CloneLambda
            | RustMaterialization::BorrowGuard
            | RustMaterialization::Gap => Some(self.global_payload_gap_kind(ty)),
        }
    }

    fn global_payload_gap_kind(&self, ty: TypeId) -> ProfileErrorKind {
        match self.program.type_arena.data(ty) {
            TypeData::Void | TypeData::Any | TypeData::Function(_) | TypeData::Dyn(_) => {
                ProfileErrorKind::UnsupportedGlobalType
            }
            TypeData::Optional(inner) | TypeData::Array { elem: inner, .. } => {
                self.global_payload_gap_kind(*inner)
            }
            TypeData::Tuple(elems) => self.first_global_payload_gap(elems.iter().copied()),
            TypeData::Aggregate(id) => self.first_global_payload_gap(
                self.program
                    .aggregate(*id)
                    .fields
                    .iter()
                    .map(|field| field.ty),
            ),
            TypeData::Enum(id) => self.first_global_payload_gap(
                self.program
                    .enum_decl(*id)
                    .variants
                    .iter()
                    .flat_map(variant_field_tys),
            ),
            TypeData::Int
            | TypeData::Float
            | TypeData::Bool
            | TypeData::String
            | TypeData::List(_)
            | TypeData::Map { .. }
            | TypeData::Slice(_)
            | TypeData::DataRef(_)
            | TypeData::Extern(_) => ProfileErrorKind::UnsupportedGlobalRooting,
        }
    }

    fn first_global_payload_gap(
        &self,
        fields: impl IntoIterator<Item = TypeId>,
    ) -> ProfileErrorKind {
        fields
            .into_iter()
            .find_map(|field| {
                self.global_payload_gap(field)
                    .map(|_| self.global_payload_gap_kind(field))
            })
            .unwrap_or(ProfileErrorKind::UnsupportedGlobalRooting)
    }

    fn global_supported(&self, global: GlobalId) -> bool {
        self.program
            .globals
            .get(global.index())
            .is_some_and(|decl| self.global_payload_gap(decl.ty).is_none())
    }

    fn global_root_set_supported(&self, global: GlobalId) -> bool {
        self.program
            .globals
            .get(global.index())
            .is_some_and(|decl| {
                decl.mutability == Mutability::Mutable && self.global_payload_gap(decl.ty).is_none()
            })
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
                .all(|field| self.policy().stored_payload_supported(field.ty))
    }

    fn dataref_decl_supported(&self, aggregate: AggregateId) -> bool {
        let decl = self.program.aggregate(aggregate);
        decl.kind == AggregateKind::DataRef
            && decl
                .fields
                .iter()
                .all(|field| self.policy().stored_payload_supported(field.ty))
    }

    fn enum_decl_supported(&self, enm: EnumId) -> bool {
        self.program.enum_decl(enm).variants.iter().all(|variant| {
            variant_field_tys(variant).all(|ty| self.policy().stored_payload_supported(ty))
        })
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
        match function.kind {
            FunctionKind::GlobalInit(global) => {
                if !self.global_supported(global) {
                    self.push(
                        ProfileSite::Function(id),
                        ProfileErrorKind::UnsupportedGlobalInitializer,
                    );
                }
            }
            FunctionKind::Normal | FunctionKind::Method | FunctionKind::Lambda(_) => {}
            FunctionKind::ExtendMethod | FunctionKind::Helper => {
                self.push(
                    ProfileSite::Function(id),
                    ProfileErrorKind::UnsupportedFunctionKind,
                );
            }
        }
        if let FunctionKind::Lambda(lambda) = function.kind {
            match self.program.lambdas.get(lambda.index()) {
                Some(decl) => self.check_lambda_decl(ProfileSite::Function(id), decl),
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

    fn check_lambda_decl(&mut self, site: ProfileSite, decl: &air::LambdaDecl) {
        for capture in &decl.captures {
            match capture {
                air::LambdaCaptureDecl::NoRuntime { .. } => {}
                air::LambdaCaptureDecl::ReadonlyLocal { ty, .. } => {
                    if !self.policy().value_place_shareable(*ty) || self.type_contains_slice(*ty) {
                        self.push(site, ProfileErrorKind::UnsupportedLambdaCapture);
                    }
                }
                air::LambdaCaptureDecl::ScopedLocal { .. } => {
                    self.push(site, ProfileErrorKind::UnsupportedLambdaCapture);
                }
                air::LambdaCaptureDecl::ScopedBorrow {
                    borrow,
                    ty,
                    mutability,
                    ..
                } => {
                    if decl.escape == air::LambdaEscape::Escaping {
                        self.push(site, ProfileErrorKind::UnsupportedLambdaCapture);
                    } else {
                        self.check_scoped_borrow_decl(site, *borrow, *ty, *mutability);
                    }
                }
                air::LambdaCaptureDecl::CaptureCell { ty, .. } => {
                    if self.type_contains_slice(*ty) {
                        self.push(site, ProfileErrorKind::UnsupportedLambdaCapture);
                    }
                }
            }
        }
    }

    fn check_air_block(&mut self, function: FunctionId, body: &air::AirBlock) {
        for (index, stmt) in body.stmts.iter().enumerate() {
            let site = ProfileSite::Statement(function, index);
            match stmt {
                air::AirStmt::Init { value, .. } | air::AirStmt::Eval(value) => {
                    self.check_mutating_rvalue(site, value);
                    self.check_rvalue(site, value);
                }
                air::AirStmt::Assign { dst, value } => {
                    self.check_rvalue(site, value);
                    if let PlaceRoot::Global(global) = dst.root
                        && !self.global_projected_place_supported(global, dst, true)
                    {
                        self.push(
                            site,
                            unsupported_global_place(
                                dst,
                                ProfileErrorKind::UnsupportedGlobalProjection,
                            ),
                        );
                        continue;
                    }
                    if !dst.projection.is_empty()
                        && !self.place_crosses_dataref(site, dst)
                        && (self.place_capture_cell(site, dst).is_some()
                            || self.place_scoped_borrow(site, dst).is_some())
                    {
                        self.push(site, ProfileErrorKind::UnsupportedMutablePlaceProjection);
                        continue;
                    }
                    self.check_place(site, dst);
                }
                air::AirStmt::GlobalEnsure { global } => {
                    if !self.global_supported(*global) {
                        self.push(site, ProfileErrorKind::UnsupportedGlobalAccess);
                    }
                }
                air::AirStmt::GlobalSetRoot { global, value, .. }
                | air::AirStmt::GlobalUpdateRoot { global, value } => {
                    self.check_rvalue(site, value);
                    if !self.global_root_set_supported(*global) {
                        self.push(site, ProfileErrorKind::UnsupportedGlobalRooting);
                    }
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
                    if match_.payload_ref
                        && !self.optional_payload_ref_discr_supported(site, &match_.discr)
                    {
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
                air::AirStmt::CollectionLoan(loan) => {
                    self.check_place(site, &loan.root);
                    self.check_air_block(function, &loan.body);
                }
                air::AirStmt::CollectionSlotScope(scope) => {
                    self.check_place(site, &scope.root);
                    self.check_air_block(function, &scope.body);
                }
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

    fn check_mutating_rvalue(&mut self, site: ProfileSite, value: &RValue) {
        let place = match value {
            RValue::ListPush { list, .. } => list,
            RValue::MapInsert { map, .. } | RValue::MapRemove { map, .. } => map,
            _ => return,
        };
        if matches!(place.root, PlaceRoot::Global(_)) {
            self.push(
                site,
                unsupported_global_place(place, ProfileErrorKind::UnsupportedGlobalRooting),
            );
            return;
        }
        if self
            .place_root_mutable(site, place)
            .is_some_and(|mutable| !mutable)
        {
            self.push(site, ProfileErrorKind::UnsupportedRValue);
        }
    }

    fn place_root_mutable(&mut self, site: ProfileSite, place: &Place) -> Option<bool> {
        match place.root {
            PlaceRoot::Local(root) => self
                .current_local(site, root)
                .map(|local| local.mutability == Mutability::Mutable),
            PlaceRoot::LambdaCapture(slot) => self
                .current_lambda_capture(site, slot)
                .map(|capture| matches!(capture, air::LambdaCaptureDecl::CaptureCell { .. })),
            PlaceRoot::CaptureCell(_) => Some(true),
            PlaceRoot::ScopedBorrow(_) => {
                self.push(site, unsupported_place_root(place.root));
                None
            }
            PlaceRoot::Global(global) => self.program.globals.get(global.index()).map_or_else(
                || {
                    self.push(site, ProfileErrorKind::UnsupportedGlobalAccess);
                    None
                },
                |global| Some(global.mutability == Mutability::Mutable),
            ),
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
                let expected = self.callee_param_semantics(callee);
                for (index, arg) in args.iter().enumerate() {
                    self.check_call_arg(site, arg, expected.get(index).copied());
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
                        | TypeData::Map { .. }
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
            } => self.check_range_rvalue(site, source, *start, *end, *ty, false),
            RValue::RangeListCopy {
                source,
                start,
                end,
                ty,
                ..
            } => self.check_range_rvalue(site, source, *start, *end, *ty, true),
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
            RValue::MapInsert {
                map,
                key,
                value,
                kind: _,
            } => {
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
            RValue::MapEntryAt { map, index, ty } => {
                self.check_place(site, map);
                self.check_type_ref(site, *ty);
                if self.current_local(site, *index).is_none() {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                }
            }
            RValue::MakeLambda {
                lambda, captures, ..
            } => {
                let decl = self.program.lambdas.get(lambda.index());
                for (index, capture) in captures.iter().enumerate() {
                    match capture {
                        air::LambdaCaptureArg::CaptureCell { .. } => {
                            if !decl.and_then(|decl| decl.captures.get(index)).is_some_and(
                                |capture| {
                                    matches!(capture, air::LambdaCaptureDecl::CaptureCell { .. })
                                },
                            ) {
                                self.push(site, ProfileErrorKind::UnsupportedLambdaCell);
                            }
                        }
                        air::LambdaCaptureArg::ReadonlyLocal { value } => {
                            self.check_operand(site, value);
                        }
                        air::LambdaCaptureArg::ScopedLocal { place } => {
                            self.check_place(site, place);
                            self.push(site, ProfileErrorKind::UnsupportedLambdaCapture);
                        }
                        air::LambdaCaptureArg::ScopedBorrow { place } => {
                            self.check_place(site, place);
                            if !decl.and_then(|decl| decl.captures.get(index)).is_some_and(
                                |capture| {
                                    matches!(
                                        capture,
                                        air::LambdaCaptureDecl::ScopedBorrow { borrow, .. }
                                            if Some(*borrow)
                                                == self.place_scoped_borrow(site, place)
                                    )
                                },
                            ) {
                                self.push(site, ProfileErrorKind::UnsupportedLambdaCapture);
                            }
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

    fn check_range_rvalue(
        &mut self,
        site: ProfileSite,
        source: &Place,
        start: LocalId,
        end: LocalId,
        ty: TypeId,
        copied: bool,
    ) {
        self.check_range_locals(site, source, start, end);
        let source_elem = match self.program.type_arena.data(source.ty) {
            TypeData::Array { elem, .. } | TypeData::List(elem) | TypeData::Slice(elem) => *elem,
            _ => {
                self.push(site, ProfileErrorKind::UnsupportedRValue);
                return;
            }
        };
        let result_elem = match (copied, self.program.type_arena.data(ty)) {
            (false, TypeData::Slice(elem)) | (true, TypeData::List(elem)) => *elem,
            _ => {
                self.push(site, ProfileErrorKind::UnsupportedRValue);
                return;
            }
        };
        if source_elem != result_elem {
            self.push(site, ProfileErrorKind::UnsupportedRValue);
        } else if copied && !self.policy().value_place_shareable(result_elem) {
            self.push(site, ProfileErrorKind::NonCopyValueRequired);
        }
    }

    fn optional_payload_ref_discr_supported(&self, site: ProfileSite, place: &Place) -> bool {
        let Some(function) = Self::current_function_id(site) else {
            return false;
        };
        let PlaceRoot::Local(local) = place.root else {
            return false;
        };
        if self.function_local_is_source_mut_place_param(function, local)
            || self.place_crosses_dataref(site, place)
        {
            return false;
        }
        let Some(mut ty) = self.current_local(site, local).map(|local| local.ty) else {
            return false;
        };
        for projection in &place.projection {
            match (self.program.type_arena.data(ty), projection) {
                (TypeData::Aggregate(_) | TypeData::Tuple(_), Projection::Field(_))
                | (TypeData::Tuple(_), Projection::TupleField(_))
                | (TypeData::Array { .. }, Projection::Index(_)) => {}
                _ => return false,
            }
            let Some(next) = air_projected_ty(self.program, ty, projection) else {
                return false;
            };
            ty = next;
        }
        true
    }

    fn check_range_locals(
        &mut self,
        site: ProfileSite,
        source: &Place,
        start: LocalId,
        end: LocalId,
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
    }

    fn projected_mut_place_supported(
        &self,
        site: ProfileSite,
        place: &Place,
        allow_collections: bool,
    ) -> bool {
        let root_ty = match place.root {
            PlaceRoot::Local(local) => self.current_local(site, local).map(|local| local.ty),
            PlaceRoot::LambdaCapture(slot) => self
                .current_lambda_capture(site, slot)
                .map(air::LambdaCaptureDecl::ty),
            PlaceRoot::CaptureCell(cell) => self
                .program
                .capture_cells
                .get(cell.index())
                .map(|cell| cell.ty),
            PlaceRoot::ScopedBorrow(borrow) => self
                .program
                .scoped_borrows
                .get(borrow.index())
                .map(|borrow| borrow.ty),
            PlaceRoot::Global(global) => self
                .program
                .globals
                .get(global.index())
                .map(|global| global.ty),
        };
        root_ty.is_some_and(|root_ty| {
            mut_place_projection_supported(self.program, root_ty, place, allow_collections)
        })
    }

    fn global_projected_place_supported(
        &self,
        global: GlobalId,
        place: &Place,
        allow_collections: bool,
    ) -> bool {
        let Some(decl) = self.program.globals.get(global.index()) else {
            return false;
        };
        decl.mutability == Mutability::Mutable
            && !place.projection.is_empty()
            && self.global_payload_gap(decl.ty).is_none()
            && mut_place_projection_supported(self.program, decl.ty, place, allow_collections)
    }

    fn current_local(&self, site: ProfileSite, local: LocalId) -> Option<&Local> {
        let (ProfileSite::Statement(function, _) | ProfileSite::Terminator(function)) = site else {
            return None;
        };
        self.program.function(function).locals.get(local.index())
    }

    fn current_lambda_capture(
        &self,
        site: ProfileSite,
        slot: air::LambdaCaptureSlotId,
    ) -> Option<&air::LambdaCaptureDecl> {
        let (ProfileSite::Statement(function, _) | ProfileSite::Terminator(function)) = site else {
            return None;
        };
        let FunctionKind::Lambda(lambda) = self.program.function(function).kind else {
            return None;
        };
        self.program
            .lambdas
            .get(lambda.index())?
            .captures
            .get(slot.index())
    }

    fn current_lambda_capture_ty(
        &self,
        site: ProfileSite,
        slot: air::LambdaCaptureSlotId,
    ) -> Option<TypeId> {
        self.current_lambda_capture(site, slot)
            .map(air::LambdaCaptureDecl::ty)
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

    fn callee_param_semantics(&self, callee: &Callee) -> Vec<rir::RirParamSemantic> {
        match callee {
            Callee::Function(id) => self
                .program
                .function(*id)
                .signature
                .params
                .iter()
                .map(|param| rir::source_param_semantic(param.mode))
                .collect(),
            Callee::Extern(id) => self.extern_param_semantics(*id),
            Callee::Lambda(operand) => match self.program.type_arena.data(self.operand_ty(operand))
            {
                TypeData::Function(sig) => sig
                    .params
                    .iter()
                    .map(|param| rir::source_param_semantic(param.mode))
                    .collect(),
                _ => vec![],
            },
        }
    }

    fn extern_param_semantics(&self, id: ExternId) -> Vec<rir::RirParamSemantic> {
        native::resolve_extern(self.native_providers, self.program.extern_decl(id))
            .map(|native| native.params.iter().map(|param| param.semantic).collect())
            .unwrap_or_default()
    }

    fn check_call_arg(
        &mut self,
        site: ProfileSite,
        arg: &CallArg,
        expected: Option<rir::RirParamSemantic>,
    ) {
        match arg {
            CallArg::Value(operand) => {
                self.check_operand(site, operand);
                if self.non_shareable_value_operand(operand) {
                    self.push(site, ProfileErrorKind::NonCopyValueRequired);
                }
            }
            CallArg::SharedBorrow(place) => {
                if matches!(place.root, PlaceRoot::Global(_)) && !place.projection.is_empty() {
                    self.push(
                        site,
                        unsupported_global_place(
                            place,
                            ProfileErrorKind::UnsupportedGlobalProjection,
                        ),
                    );
                    return;
                }
                self.check_place(site, place);
                if self.place_capture_cell(site, place).is_some() {
                    self.push(site, ProfileErrorKind::UnsupportedCallArgMode);
                }
            }
            CallArg::SharedStringConst(_) => {}
            CallArg::MutBorrow(place) => {
                let Some(expected) = expected else {
                    if matches!(place.root, PlaceRoot::Global(_)) {
                        self.push(
                            site,
                            unsupported_global_place(
                                place,
                                ProfileErrorKind::UnsupportedGlobalBorrow,
                            ),
                        );
                        return;
                    }
                    self.check_place(site, place);
                    self.push(site, ProfileErrorKind::UnsupportedCallArgMode);
                    return;
                };
                if matches!(place.root, PlaceRoot::Global(_))
                    && expected != rir::RirParamSemantic::MutPlace
                {
                    self.push(
                        site,
                        unsupported_global_place(place, ProfileErrorKind::UnsupportedGlobalBorrow),
                    );
                    return;
                }
                match expected {
                    rir::RirParamSemantic::MutPlace => {
                        if matches!(place.root, PlaceRoot::Global(_))
                            && place.projection.is_empty()
                            && self.place_root_mutable(site, place) != Some(true)
                        {
                            return;
                        }
                        if !place.projection.is_empty() {
                            match self.dataref_mut_place_support(site, place) {
                                DataRefMutPlaceSupport::Supported(_) => {}
                                DataRefMutPlaceSupport::UnsupportedDataRef => {
                                    self.push(
                                        site,
                                        ProfileErrorKind::UnsupportedMutablePlaceDataRef,
                                    );
                                    return;
                                }
                                DataRefMutPlaceSupport::Ordinary
                                    if self.projected_mut_place_supported(site, place, true) => {}
                                DataRefMutPlaceSupport::Ordinary => {
                                    self.push(
                                        site,
                                        ProfileErrorKind::UnsupportedMutablePlaceProjection,
                                    );
                                    return;
                                }
                            }
                        }
                        self.check_place(site, place);
                    }
                    rir::RirParamSemantic::MutBorrow => {
                        let Some(function) = Self::current_function_id(site) else {
                            self.push(
                                site,
                                ProfileErrorKind::UnsupportedMutablePlaceNativeBoundary,
                            );
                            return;
                        };
                        if !direct_native_mut_borrow_supported(self.program, function, place) {
                            self.push(
                                site,
                                ProfileErrorKind::UnsupportedMutablePlaceNativeBoundary,
                            );
                            return;
                        }
                        self.check_place(site, place);
                        if !self.supports_param_mode(place.ty, ParamMode::MutBorrow) {
                            self.push(site, ProfileErrorKind::UnsupportedCallArgMode);
                        }
                    }
                    rir::RirParamSemantic::Value
                    | rir::RirParamSemantic::SharedBorrow
                    | rir::RirParamSemantic::ScopedLambda
                    | rir::RirParamSemantic::StackCell
                    | rir::RirParamSemantic::HeapCell
                    | rir::RirParamSemantic::ScopedPlaceCell => {
                        self.push(site, ProfileErrorKind::UnsupportedCallArgMode);
                    }
                }
            }
        }
    }

    fn non_shareable_value_operand(&self, operand: &Operand) -> bool {
        let Operand::Place(place) = operand else {
            return false;
        };
        !matches!(self.program.type_arena.data(place.ty), TypeData::Slice(_))
            && self.non_copy_type(place.ty)
            && !self.policy().value_place_shareable(place.ty)
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
            Operand::Place(place) => {
                if matches!(place.root, PlaceRoot::Global(_)) && !place.projection.is_empty() {
                    self.push(
                        site,
                        unsupported_global_place(
                            place,
                            ProfileErrorKind::UnsupportedGlobalProjection,
                        ),
                    );
                    return;
                }
                self.check_place(site, place);
                if self.place_capture_cell(site, place).is_some()
                    && !self.policy().value_place_shareable(place.ty)
                {
                    self.push(site, ProfileErrorKind::UnsupportedLambdaCell);
                }
            }
            Operand::Const(id) => self.check_const_ref(site, *id),
        }
    }

    fn check_place(&mut self, site: ProfileSite, place: &Place) {
        if self.place_scoped_borrow(site, place).is_some() {
            let Some(ty) = self.check_scoped_borrow_place(site, place) else {
                return;
            };
            if ty != place.ty {
                self.push(site, ProfileErrorKind::UnsupportedPlaceProjection);
            }
            self.check_type_ref(site, place.ty);
            return;
        }
        let mut ty = match place.root {
            PlaceRoot::Local(root) => {
                let Some(local) = self.current_local(site, root) else {
                    self.push(site, ProfileErrorKind::UnsupportedPlaceProjection);
                    return;
                };
                if self.local_is_source_mut_place_param(site, root)
                    && !place.projection.is_empty()
                    && !self.place_crosses_dataref(site, place)
                    && !mut_place_projection_supported(self.program, local.ty, place, true)
                {
                    self.push(site, ProfileErrorKind::UnsupportedMutablePlaceProjection);
                    return;
                }
                local.ty
            }
            PlaceRoot::LambdaCapture(slot) => {
                let Some(ty) = self.current_lambda_capture_ty(site, slot) else {
                    self.push(site, ProfileErrorKind::UnsupportedLambdaCapture);
                    return;
                };
                if self.place_capture_cell(site, place).is_some()
                    && !place.projection.is_empty()
                    && !self.projected_mut_place_supported(site, place, false)
                    && self
                        .check_dataref_mut_place(
                            site,
                            place,
                            ProfileErrorKind::UnsupportedPlaceProjection,
                        )
                        .is_none()
                {
                    return;
                }
                ty
            }
            PlaceRoot::CaptureCell(cell) => {
                let Some(decl) = self.program.capture_cells.get(cell.index()) else {
                    self.push(site, ProfileErrorKind::UnsupportedPlaceProjection);
                    return;
                };
                if !place.projection.is_empty()
                    && !self.projected_mut_place_supported(site, place, false)
                    && self
                        .check_dataref_mut_place(
                            site,
                            place,
                            ProfileErrorKind::UnsupportedPlaceProjection,
                        )
                        .is_none()
                {
                    return;
                }
                decl.ty
            }
            PlaceRoot::ScopedBorrow(_) if !place.projection.is_empty() => {
                let Some(ty) = self.check_scoped_borrow_place(site, place) else {
                    return;
                };
                ty
            }
            PlaceRoot::ScopedBorrow(_) => {
                self.push(site, unsupported_place_root(place.root));
                return;
            }
            PlaceRoot::Global(global) => {
                let Some(decl) = self.program.globals.get(global.index()) else {
                    self.push(site, ProfileErrorKind::UnsupportedGlobalAccess);
                    return;
                };
                if self.global_payload_gap(decl.ty).is_some() {
                    self.push(site, ProfileErrorKind::UnsupportedGlobalValueRead);
                    return;
                }
                if !place.projection.is_empty()
                    && !mut_place_projection_supported(self.program, decl.ty, place, true)
                {
                    self.push(site, ProfileErrorKind::UnsupportedGlobalProjection);
                    return;
                }
                decl.ty
            }
        };
        for projection in &place.projection {
            let Some(next) = air_projected_ty(self.program, ty, projection) else {
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

    fn local_is_source_mut_place_param(&self, site: ProfileSite, local: LocalId) -> bool {
        Self::current_function_id(site)
            .is_some_and(|function| self.function_local_is_source_mut_place_param(function, local))
    }

    fn function_local_is_source_mut_place_param(
        &self,
        function: FunctionId,
        local: LocalId,
    ) -> bool {
        self.program
            .function(function)
            .signature
            .params
            .iter()
            .any(|param| param.local_id == local && param.mode == ParamMode::MutBorrow)
    }

    fn place_scoped_borrow(&self, site: ProfileSite, place: &Place) -> Option<air::ScopedBorrowId> {
        let function = Self::current_function_id(site)?;
        self.program.scoped_borrow_root(function, place.root)
    }

    fn check_scoped_borrow_decl(
        &mut self,
        site: ProfileSite,
        borrow: air::ScopedBorrowId,
        ty: TypeId,
        mutability: Mutability,
    ) {
        let Some(decl) = self.program.scoped_borrows.get(borrow.index()) else {
            self.push(site, ProfileErrorKind::UnsupportedLambdaCapture);
            return;
        };
        if decl.ty != ty || decl.mutability != mutability || mutability != Mutability::Mutable {
            self.push(site, ProfileErrorKind::UnsupportedLambdaCapture);
        }
        match decl.source {
            air::ScopedBorrowSource::SourceMutParam { local }
                if self.function_local_is_source_mut_place_param(decl.owner, local) => {}
            air::ScopedBorrowSource::SourceMutParam { .. } => {
                self.push(site, ProfileErrorKind::UnsupportedLambdaCapture);
            }
        }
    }

    fn check_scoped_borrow_place(&mut self, site: ProfileSite, place: &Place) -> Option<TypeId> {
        let function = Self::current_function_id(site)?;
        let borrow = self.place_scoped_borrow(site, place)?;
        let Some(decl) = self.program.scoped_borrows.get(borrow.index()) else {
            self.push(site, ProfileErrorKind::UnsupportedLambdaCapture);
            return None;
        };
        if matches!(place.root, PlaceRoot::ScopedBorrow(_)) && decl.owner != function {
            self.push(site, ProfileErrorKind::UnsupportedLambdaCapture);
            return None;
        }
        let projected_ty = if place.projection.is_empty() {
            None
        } else if self.projected_mut_place_supported(site, place, false) {
            place.projection.iter().try_fold(decl.ty, |ty, projection| {
                air_projected_ty(self.program, ty, projection)
            })
        } else {
            Some(
                self.check_dataref_mut_place(
                    site,
                    place,
                    ProfileErrorKind::UnsupportedMutablePlaceProjection,
                )?
                .ty,
            )
        };
        self.check_scoped_borrow_decl(site, borrow, decl.ty, decl.mutability);
        Some(projected_ty.unwrap_or(decl.ty))
    }

    fn current_function_id(site: ProfileSite) -> Option<FunctionId> {
        match site {
            ProfileSite::Function(function)
            | ProfileSite::Local(function, _)
            | ProfileSite::Param(function, _)
            | ProfileSite::Statement(function, _)
            | ProfileSite::Terminator(function) => Some(function),
            ProfileSite::Entry
            | ProfileSite::Type(_)
            | ProfileSite::Const(_)
            | ProfileSite::Module(_)
            | ProfileSite::Global(_)
            | ProfileSite::Extern(_) => None,
        }
    }

    fn place_capture_cell(&self, site: ProfileSite, place: &Place) -> Option<air::CaptureCellId> {
        let function = Self::current_function_id(site)?;
        self.program.capture_cell_root(function, place.root)
    }

    fn check_dataref_mut_place(
        &mut self,
        site: ProfileSite,
        place: &Place,
        ordinary: ProfileErrorKind,
    ) -> Option<DataRefMutPlace> {
        match self.dataref_mut_place_support(site, place) {
            DataRefMutPlaceSupport::Supported(supported) => Some(supported),
            DataRefMutPlaceSupport::UnsupportedDataRef => {
                self.push(site, ProfileErrorKind::UnsupportedMutablePlaceDataRef);
                None
            }
            DataRefMutPlaceSupport::Ordinary => {
                self.push(site, ordinary);
                None
            }
        }
    }

    fn dataref_mut_place_support(
        &self,
        site: ProfileSite,
        place: &Place,
    ) -> DataRefMutPlaceSupport {
        let Some(root_ty) = self.place_root_ty(site, place) else {
            return DataRefMutPlaceSupport::Ordinary;
        };
        classify_dataref_mut_place(self.program, root_ty, place)
    }

    fn place_root_ty(&self, site: ProfileSite, place: &Place) -> Option<TypeId> {
        match place.root {
            PlaceRoot::Local(root) => self.current_local(site, root).map(|local| local.ty),
            PlaceRoot::LambdaCapture(slot) => self.current_lambda_capture_ty(site, slot),
            PlaceRoot::CaptureCell(cell) => self
                .program
                .capture_cells
                .get(cell.index())
                .map(|decl| decl.ty),
            PlaceRoot::ScopedBorrow(borrow) => self
                .program
                .scoped_borrows
                .get(borrow.index())
                .map(|decl| decl.ty),
            PlaceRoot::Global(_) => None,
        }
    }

    fn place_crosses_dataref(&self, site: ProfileSite, place: &Place) -> bool {
        let Some(mut ty) = self.place_root_ty(site, place) else {
            return false;
        };
        for projection in &place.projection {
            if matches!(self.program.type_arena.data(ty), TypeData::DataRef(_)) {
                return true;
            }
            let Some(next) = air_projected_ty(self.program, ty, projection) else {
                return false;
            };
            ty = next;
        }
        false
    }

    fn check_extern(&mut self, id: ExternId, decl: &ExternDecl) {
        let site = ProfileSite::Extern(id);
        let native = match native::resolve_extern(self.native_providers, decl) {
            Ok(native) => Some(native),
            Err(native::ResolveExternError::UnsupportedExtern) => {
                self.push(site, ProfileErrorKind::UnsupportedExtern);
                None
            }
            Err(native::ResolveExternError::UnsupportedRustAbi) => {
                self.push(site, ProfileErrorKind::UnsupportedRustAbi);
                None
            }
        };
        if self.unsupported_lambda_extern_boundary(decl, native.as_ref()) {
            self.push(site, ProfileErrorKind::UnsupportedLambdaExternBoundary);
        }
    }

    fn unsupported_lambda_extern_boundary(
        &self,
        decl: &ExternDecl,
        native: Option<&native::ResolvedExtern<'_>>,
    ) -> bool {
        self.type_contains_function(decl.return_type)
            || decl.call_params().enumerate().any(|(index, param)| {
                self.type_contains_function(param.ty)
                    && !(self.type_is_function(param.ty)
                        && native.is_some_and(|native| {
                            param.escape == air::ParamEscape::NonEscaping
                                && native.params.get(index).is_some_and(|native_param| {
                                    native_param.semantic == rir::RirParamSemantic::ScopedLambda
                                })
                        }))
            })
    }

    fn type_is_function(&self, ty: TypeId) -> bool {
        matches!(self.program.type_arena.data(ty), TypeData::Function(_))
    }

    fn type_contains_slice(&self, ty: TypeId) -> bool {
        self.type_contains(ty, |data| matches!(data, TypeData::Slice(_)))
    }

    fn type_contains_function(&self, ty: TypeId) -> bool {
        self.type_contains(ty, |data| matches!(data, TypeData::Function(_)))
    }

    fn type_contains(&self, ty: TypeId, target: impl Fn(&TypeData) -> bool + Copy) -> bool {
        self.type_contains_inner(ty, &mut HashSet::new(), target)
    }

    fn type_contains_inner(
        &self,
        ty: TypeId,
        visited: &mut HashSet<TypeId>,
        target: impl Fn(&TypeData) -> bool + Copy,
    ) -> bool {
        if !visited.insert(ty) {
            return false;
        }
        let data = self.program.type_arena.data(ty);
        if target(data) {
            return true;
        }
        match data {
            TypeData::Optional(inner)
            | TypeData::Array { elem: inner, .. }
            | TypeData::Slice(inner)
            | TypeData::List(inner) => self.type_contains_inner(*inner, visited, target),
            TypeData::Map { key, value, .. } => {
                self.type_contains_inner(*key, visited, target)
                    || self.type_contains_inner(*value, visited, target)
            }
            TypeData::Tuple(elems) => elems
                .iter()
                .any(|elem| self.type_contains_inner(*elem, visited, target)),
            TypeData::Aggregate(aggregate) | TypeData::DataRef(aggregate) => self
                .program
                .aggregate(*aggregate)
                .fields
                .iter()
                .any(|field| self.type_contains_inner(field.ty, visited, target)),
            TypeData::Enum(enm) => self.program.enum_decl(*enm).variants.iter().any(|variant| {
                variant_field_tys(variant).any(|ty| self.type_contains_inner(ty, visited, target))
            }),
            TypeData::Extern(ext) => self
                .program
                .extern_type(*ext)
                .fields
                .iter()
                .any(|field| self.type_contains_inner(field.ty, visited, target)),
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
                if !self.reject_function_container(site, *elem) {
                    self.check_type_ref(site, *elem);
                }
                true
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
                    self.check_map_shape(site, *key, *value)
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
                if matches!(sig.ret, ReturnMode::Place(_)) {
                    self.push(site, ProfileErrorKind::UnsupportedReturnMode);
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

    fn check_map_shape(&mut self, site: ProfileSite, key: TypeId, value: TypeId) -> bool {
        let key_ok = self.policy().map_key_supported(key);
        let value_ok = self.policy().map_value_supported(value);
        if !key_ok {
            self.push(site, ProfileErrorKind::UnsupportedMapKey);
        }
        if !value_ok {
            self.push(site, ProfileErrorKind::UnsupportedMapValue);
        }
        key_ok && value_ok
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
