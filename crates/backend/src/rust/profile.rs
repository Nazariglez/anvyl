use std::collections::HashSet;

use anvyx_frontend::{
    air::{
        self, AggregateCtor, AggregateId, AggregateKind, CallArg, Callee, ConstId, ConstValue,
        EnumId, ExternDecl, ExternId, Function, FunctionId, FunctionKind, GlobalId, Local, LocalId,
        LocalKind, Module, Mutability, Operand, Param, ParamEscape, ParamMode, ParamRole, Place,
        PlaceRoot, Program, RValue, ReturnMode, TypeData, TypeId, TypePassClasses, VariantDecl,
        VariantShape, VerifiedProgram,
    },
    ast::{FormatKind, FormatSign, FormatSpec},
};
use anvyx_runtime::RustProviderSupport;

use super::{
    CollectionAccessOp, native,
    place_access::{
        PlaceAccessCx, PlaceAccessGapKind, PlaceAccessIntent, PlaceAccessPlan, PlaceAccessRoot,
    },
    rep_policy::{
        AirRustRepPolicy, LambdaStorageFamily, LambdaStorageGap, RustMaterialIntent,
        RustMaterialSource, RustMaterialization,
    },
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

fn profile_gap_kind(kind: PlaceAccessGapKind) -> ProfileErrorKind {
    match kind {
        PlaceAccessGapKind::PlaceProjection => ProfileErrorKind::UnsupportedPlaceProjection,
        PlaceAccessGapKind::GlobalAccess => ProfileErrorKind::UnsupportedGlobalAccess,
        PlaceAccessGapKind::GlobalBorrow => ProfileErrorKind::UnsupportedGlobalBorrow,
        PlaceAccessGapKind::GlobalProjection => ProfileErrorKind::UnsupportedGlobalProjection,
        PlaceAccessGapKind::GlobalValueRead => ProfileErrorKind::UnsupportedGlobalValueRead,
        PlaceAccessGapKind::GlobalRooting => ProfileErrorKind::UnsupportedGlobalRooting,
        PlaceAccessGapKind::GlobalType => ProfileErrorKind::UnsupportedGlobalType,
        PlaceAccessGapKind::MutablePlace => ProfileErrorKind::UnsupportedMutablePlace,
        PlaceAccessGapKind::MutablePlaceProjection => {
            ProfileErrorKind::UnsupportedMutablePlaceProjection
        }
        PlaceAccessGapKind::MutablePlaceDataRef => ProfileErrorKind::UnsupportedMutablePlaceDataRef,
        PlaceAccessGapKind::MutablePlaceNativeBoundary => {
            ProfileErrorKind::UnsupportedMutablePlaceNativeBoundary
        }
        PlaceAccessGapKind::SliceView => ProfileErrorKind::UnsupportedRValue,
        PlaceAccessGapKind::ReturnPlace => ProfileErrorKind::UnsupportedReturnMode,
    }
}

fn lambda_storage_gap_kind(gap: LambdaStorageGap) -> ProfileErrorKind {
    match gap {
        LambdaStorageGap::MapKeyEqualityHash => ProfileErrorKind::UnsupportedMapKey,
        LambdaStorageGap::GlobalRooting => ProfileErrorKind::UnsupportedGlobalRooting,
        LambdaStorageGap::ExternBoundary => ProfileErrorKind::UnsupportedLambdaExternBoundary,
        LambdaStorageGap::StorageImplementation
        | LambdaStorageGap::ProvenanceOrigin
        | LambdaStorageGap::Lifetime
        | LambdaStorageGap::Trace
        | LambdaStorageGap::UnsupportedType => ProfileErrorKind::UnsupportedLambdaValue,
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
        self.access().global_payload_gap(ty).map(profile_gap_kind)
    }

    fn global_supported(&self, global: GlobalId) -> bool {
        self.access().global_supported(global)
    }

    fn global_root_set_supported(&self, global: GlobalId) -> bool {
        self.access().global_root_set_supported(global)
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
            && decl.fields.iter().all(|field| {
                self.stored_payload_supported(field.ty)
                    || self
                        .policy()
                        .storage_supported(field.ty, LambdaStorageFamily::StructField)
                        .is_ok()
            })
    }

    fn dataref_decl_supported(&self, aggregate: AggregateId) -> bool {
        let decl = self.program.aggregate(aggregate);
        decl.kind == AggregateKind::DataRef
            && decl
                .fields
                .iter()
                .all(|field| self.stored_payload_supported(field.ty))
    }

    fn enum_decl_supported(&self, enm: EnumId) -> bool {
        self.program
            .enum_decl(enm)
            .variants
            .iter()
            .all(|variant| variant_field_tys(variant).all(|ty| self.stored_payload_supported(ty)))
    }

    fn extern_type_supported(&self, ext: air::ExternTypeId) -> bool {
        let decl = self.program.extern_type(ext);
        match decl.rep {
            air::ExternRep::Shared => true,
            air::ExternRep::Inline => {
                decl.fields.iter().all(|field| {
                    matches!(
                        self.program.type_arena.data(field.ty),
                        TypeData::Int | TypeData::Float | TypeData::Bool | TypeData::String
                    )
                }) && decl.variants.iter().all(|variant| {
                    variant_field_tys(variant).all(|ty| self.stored_payload_supported(ty))
                })
            }
        }
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
        if matches!(function.signature.return_mode, ReturnMode::Place(_)) {
            self.push(
                ProfileSite::Function(id),
                profile_gap_kind(PlaceAccessGapKind::ReturnPlace),
            );
        }
        if matches!(
            self.program
                .type_arena
                .data(function.signature.return_type()),
            TypeData::Slice(_)
        ) {
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
                    self.check_rvalue(site, value);
                }
                air::AirStmt::Assign { dst, value } => {
                    self.check_rvalue(site, value);
                    if self.place_uses_scoped_borrow_source_root(function, dst) {
                        self.push(site, ProfileErrorKind::UnsupportedLambdaCapture);
                    }
                    match self.access().plan(function, PlaceAccessIntent::Assign, dst) {
                        Ok(_) => self.check_type_ref(site, dst.ty),
                        Err(gap) => self.push(site, profile_gap_kind(gap)),
                    }
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
                        && self
                            .optional_payload_ref_discr_plan(site, &match_.discr)
                            .is_none()
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
                air::AirStmt::MapEntryMatch(match_) => {
                    self.check_operand(site, &match_.key);
                    match self
                        .access()
                        .plan(function, PlaceAccessIntent::MutPlaceArg, &match_.map)
                    {
                        Ok(_) => self.check_type_ref(site, match_.map.ty),
                        Err(gap) => self.push(site, profile_gap_kind(gap)),
                    }
                    self.check_air_block(function, &match_.some_block);
                    self.check_air_block(function, &match_.none_block);
                }
                air::AirStmt::Loop(loop_) => self.check_air_block(function, &loop_.body),
                air::AirStmt::CollectionLoan(loan) => {
                    if let Err(gap) = self.access().collection_loan_plan(function, loan) {
                        self.push(site, profile_gap_kind(gap));
                    }
                    self.check_place(site, &loan.root);
                    self.check_air_block(function, &loan.body);
                }
                air::AirStmt::CollectionSlotScope(scope) => {
                    self.check_collection_access(
                        site,
                        &scope.root,
                        CollectionAccessOp::slot(&scope.slots),
                    );
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

    fn check_structural_mutation(&mut self, site: ProfileSite, place: &Place) {
        self.check_collection_access(site, place, CollectionAccessOp::StructuralMutation);
    }

    fn check_collection_access(
        &mut self,
        site: ProfileSite,
        place: &Place,
        op: CollectionAccessOp,
    ) {
        let Some(function) = Self::current_function_id(site) else {
            if matches!(place.root, PlaceRoot::Global(_)) {
                self.push(site, ProfileErrorKind::UnsupportedGlobalRooting);
            }
            return;
        };
        let plan = match self.access().plan(function, op.intent(), place) {
            Ok(plan) => plan,
            Err(gap) => {
                self.push(site, profile_gap_kind(gap));
                return;
            }
        };
        if self.place_uses_scoped_borrow_source_root(function, place) {
            self.push(site, ProfileErrorKind::UnsupportedLambdaCapture);
        }
        if op.requires_mutable_root()
            && self
                .access_root_mutable(site, plan.root)
                .is_some_and(|mutable| !mutable)
        {
            self.push(site, ProfileErrorKind::UnsupportedRValue);
        }
    }

    fn access_root_mutable(&mut self, site: ProfileSite, root: PlaceAccessRoot) -> Option<bool> {
        match root {
            PlaceAccessRoot::Local { local, .. } => self
                .current_local(site, local)
                .map(|local| local.mutability == Mutability::Mutable),
            PlaceAccessRoot::CaptureCell(_) | PlaceAccessRoot::ScopedPlaceCell(_) => Some(true),
            PlaceAccessRoot::Global(global) => {
                self.program.globals.get(global.index()).map_or_else(
                    || {
                        self.push(site, ProfileErrorKind::UnsupportedGlobalAccess);
                        None
                    },
                    |global| Some(global.mutability == Mutability::Mutable),
                )
            }
            PlaceAccessRoot::LambdaCapture(_) => {
                self.push(site, ProfileErrorKind::UnsupportedLambdaCapture);
                None
            }
        }
    }

    fn check_rvalue(&mut self, site: ProfileSite, value: &RValue) {
        match value {
            RValue::FunctionRef { .. } => {}
            RValue::Use(operand) | RValue::FunctionValue { value: operand, .. } => {
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
                let reentry_boundary =
                    matches!(callee, Callee::Extern(_)) && self.has_retained_callbacks();
                for (index, arg) in args.iter().enumerate() {
                    self.check_call_arg(site, arg, expected.get(index).copied());
                    if reentry_boundary && Self::call_arg_exposes_runtime_borrow(arg) {
                        self.push(site, ProfileErrorKind::UnsupportedCallArgMode);
                    }
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
                if matches!(
                    self.program.type_arena.data(source.ty),
                    TypeData::Array { .. }
                        | TypeData::List(_)
                        | TypeData::Map { .. }
                        | TypeData::Slice(_)
                ) {
                    self.check_collection_access(site, source, CollectionAccessOp::Len);
                } else {
                    self.check_place(site, source);
                }
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
                self.check_structural_mutation(site, list);
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
                if matches!(value, RValue::MapRemove { .. }) {
                    self.check_structural_mutation(site, map);
                } else {
                    self.check_collection_access(site, map, CollectionAccessOp::MapGet);
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
                kind,
            } => {
                match kind {
                    air::MapWriteKind::StructuralInsert => {
                        self.check_structural_mutation(site, map);
                    }
                    air::MapWriteKind::IndexedAssignment => {
                        self.check_collection_access(
                            site,
                            map,
                            CollectionAccessOp::IndexedMapAssign,
                        );
                    }
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
                self.check_collection_access(site, map, CollectionAccessOp::MapEntryRead);
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

    fn optional_payload_ref_discr_plan(
        &self,
        site: ProfileSite,
        place: &Place,
    ) -> Option<PlaceAccessPlan> {
        let function = Self::current_function_id(site)?;
        self.access()
            .plan(function, PlaceAccessIntent::PayloadAlias, place)
            .ok()
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

    fn has_retained_callbacks(&self) -> bool {
        self.program.externs.iter().any(|ext| {
            ext.params.iter().any(|param| {
                param.escape == ParamEscape::Escaping
                    && matches!(
                        self.program.type_arena.data(param.ty),
                        TypeData::Function(_)
                    )
            })
        })
    }

    fn call_arg_exposes_runtime_borrow(arg: &CallArg) -> bool {
        matches!(arg, CallArg::SharedBorrow(_) | CallArg::MutBorrow(_))
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
                if matches!(place.root, PlaceRoot::Global(_)) {
                    let Some(function) = Self::current_function_id(site) else {
                        self.push(site, ProfileErrorKind::UnsupportedGlobalAccess);
                        return;
                    };
                    if let Err(gap) =
                        self.access()
                            .plan(function, PlaceAccessIntent::SharedBorrow, place)
                    {
                        self.push(site, profile_gap_kind(gap));
                        return;
                    }
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
                        self.check_mut_borrow_global(site, place);
                        return;
                    }
                    self.check_place(site, place);
                    self.push(site, ProfileErrorKind::UnsupportedCallArgMode);
                    return;
                };
                if matches!(place.root, PlaceRoot::Global(_))
                    && expected != rir::RirParamSemantic::MutPlace
                {
                    self.check_mut_borrow_global(site, place);
                    return;
                }
                match expected {
                    rir::RirParamSemantic::MutPlace => {
                        let Some(function) = Self::current_function_id(site) else {
                            self.push(site, ProfileErrorKind::UnsupportedMutablePlace);
                            return;
                        };
                        match self
                            .access()
                            .plan(function, PlaceAccessIntent::MutPlaceArg, place)
                        {
                            Ok(_) => self.check_type_ref(site, place.ty),
                            Err(gap) => self.push(site, profile_gap_kind(gap)),
                        }
                    }
                    rir::RirParamSemantic::MutBorrow => {
                        let Some(function) = Self::current_function_id(site) else {
                            self.push(
                                site,
                                ProfileErrorKind::UnsupportedMutablePlaceNativeBoundary,
                            );
                            return;
                        };
                        if let Err(gap) =
                            self.access()
                                .plan(function, PlaceAccessIntent::NativeMutBorrow, place)
                        {
                            self.push(site, profile_gap_kind(gap));
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
                    | rir::RirParamSemantic::EscapingLambda
                    | rir::RirParamSemantic::StackCell
                    | rir::RirParamSemantic::HeapCell
                    | rir::RirParamSemantic::ScopedPlaceCell => {
                        self.push(site, ProfileErrorKind::UnsupportedCallArgMode);
                    }
                }
            }
        }
    }

    fn check_mut_borrow_global(&mut self, site: ProfileSite, place: &Place) {
        let Some(function) = Self::current_function_id(site) else {
            self.push(site, ProfileErrorKind::UnsupportedGlobalBorrow);
            return;
        };
        if let Err(gap) = self
            .access()
            .plan(function, PlaceAccessIntent::MutBorrow, place)
        {
            self.push(site, profile_gap_kind(gap));
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
                if matches!(place.root, PlaceRoot::Global(_)) {
                    let Some(function) = Self::current_function_id(site) else {
                        self.push(site, ProfileErrorKind::UnsupportedGlobalAccess);
                        return;
                    };
                    if let Err(gap) =
                        self.access()
                            .plan(function, PlaceAccessIntent::ReadValue, place)
                    {
                        self.push(site, profile_gap_kind(gap));
                        return;
                    }
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
        let Some(function) = Self::current_function_id(site) else {
            self.push(site, ProfileErrorKind::UnsupportedPlaceProjection);
            return;
        };
        let plan = match self
            .access()
            .plan(function, PlaceAccessIntent::ReadValue, place)
        {
            Ok(plan) => plan,
            Err(gap) => {
                self.push(site, profile_gap_kind(gap));
                return;
            }
        };
        if let Some(borrow) = self.place_scoped_borrow(site, place) {
            let Some(decl) = self.program.scoped_borrows.get(borrow.index()) else {
                self.push(site, ProfileErrorKind::UnsupportedLambdaCapture);
                return;
            };
            if matches!(place.root, PlaceRoot::ScopedBorrow(_)) && decl.owner != function {
                self.push(site, ProfileErrorKind::UnsupportedLambdaCapture);
                return;
            }
            self.check_scoped_borrow_decl(site, borrow, decl.ty, decl.mutability);
        }
        if self.place_uses_scoped_borrow_source_root(function, place) {
            self.push(site, ProfileErrorKind::UnsupportedLambdaCapture);
        }
        if plan.ty != place.ty {
            self.push(site, ProfileErrorKind::UnsupportedPlaceProjection);
        }
        self.check_type_ref(site, place.ty);
    }

    fn place_uses_scoped_borrow_source_root(&self, function: FunctionId, place: &Place) -> bool {
        let PlaceRoot::Local(local) = place.root else {
            return false;
        };
        self.program.scoped_borrows.iter().any(|borrow| {
            borrow.owner == function
                && match &borrow.source {
                    air::ScopedBorrowSource::SourceMutParam { local: source }
                    | air::ScopedBorrowSource::VarSelf { local: source } => *source == local,
                    air::ScopedBorrowSource::PatternAlias { source } => {
                        source.root.local() == Some(local)
                    }
                }
        })
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
        match &decl.source {
            air::ScopedBorrowSource::SourceMutParam { local }
            | air::ScopedBorrowSource::VarSelf { local } => {
                if !self.function_local_is_source_mut_place_param(decl.owner, *local) {
                    self.push(site, ProfileErrorKind::UnsupportedLambdaCapture);
                }
            }
            air::ScopedBorrowSource::PatternAlias { source } => {
                let source_root_supported = source.root.local().is_some_and(|local| {
                    self.function_local_is_source_mut_place_param(decl.owner, local)
                });
                if !source_root_supported
                    || self
                        .access()
                        .plan(decl.owner, PlaceAccessIntent::MutPlaceArg, source)
                        .is_err()
                {
                    self.push(site, ProfileErrorKind::UnsupportedLambdaCapture);
                }
            }
        }
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
                            native.params.get(index).is_some_and(|native_param| {
                                matches!(
                                    (param.escape, native_param.semantic),
                                    (
                                        ParamEscape::NonEscaping,
                                        rir::RirParamSemantic::ScopedLambda
                                    ) | (
                                        ParamEscape::Escaping,
                                        rir::RirParamSemantic::EscapingLambda
                                    )
                                )
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
            TypeData::Extern(ext) => {
                let decl = self.program.extern_type(*ext);
                decl.fields
                    .iter()
                    .any(|field| self.type_contains_inner(field.ty, visited, target))
                    || decl.variants.iter().any(|variant| {
                        variant_field_tys(variant)
                            .any(|ty| self.type_contains_inner(ty, visited, target))
                    })
            }
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
                if self
                    .policy()
                    .storage_supported(ty, LambdaStorageFamily::UnknownOrigin)
                    .is_err()
                    && !self.reject_function_container(site, *elem)
                {
                    self.check_type_ref(site, *elem);
                }
                true
            }
            TypeData::Slice(elem) => {
                if !self.reject_function_container(site, *elem) {
                    self.check_type_ref(site, *elem);
                }
                true
            }
            TypeData::List(elem) => {
                if self
                    .policy()
                    .storage_supported(ty, LambdaStorageFamily::UnknownOrigin)
                    .is_err()
                    && self.reject_function_container(site, *elem)
                {
                    true
                } else {
                    self.check_type_ref(site, *elem);
                    self.policy().list_supported(ty)
                }
            }
            TypeData::Map { key, value, .. } => {
                self.check_type_ref(site, *key);
                self.check_type_ref(site, *value);
                self.check_map_shape(site, *key, *value)
            }
            TypeData::Optional(inner) => {
                if self
                    .policy()
                    .storage_supported(ty, LambdaStorageFamily::UnknownOrigin)
                    .is_err()
                    && !self.reject_function_container(site, *inner)
                {
                    self.check_type_ref(site, *inner);
                }
                true
            }
            TypeData::Tuple(elems) => {
                let supported = self
                    .policy()
                    .storage_supported(ty, LambdaStorageFamily::UnknownOrigin)
                    .is_ok();
                if !supported {
                    for elem in elems {
                        if !self.reject_function_container(site, *elem) {
                            self.check_type_ref(site, *elem);
                        }
                    }
                }
                true
            }
            TypeData::Function(sig) => {
                for param in &sig.params {
                    self.check_type_ref(site, param.ty);
                }
                if matches!(sig.ret, ReturnMode::Place(_)) {
                    self.push(site, profile_gap_kind(PlaceAccessGapKind::ReturnPlace));
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
        let key_gap = self
            .policy()
            .contains_function_payload(key)
            .then(|| {
                self.policy()
                    .storage_supported(key, LambdaStorageFamily::MapKey)
                    .err()
            })
            .flatten();
        let value_gap = self
            .policy()
            .contains_function_payload(value)
            .then(|| {
                self.policy()
                    .storage_supported(value, LambdaStorageFamily::MapValue)
                    .err()
            })
            .flatten();
        let key_ok = key_gap.is_none() && self.policy().map_key_supported(key);
        let value_ok = value_gap.is_none() && self.policy().map_value_supported(value);
        if let Some(gap) = key_gap {
            self.push(site, lambda_storage_gap_kind(gap));
        } else if !key_ok {
            self.push(site, ProfileErrorKind::UnsupportedMapKey);
        }
        if let Some(gap) = value_gap {
            self.push(site, lambda_storage_gap_kind(gap));
        } else if !value_ok {
            self.push(site, ProfileErrorKind::UnsupportedMapValue);
        }
        key_ok && value_ok
    }

    fn reject_function_container(&mut self, site: ProfileSite, ty: TypeId) -> bool {
        let Err(gap) = self
            .policy()
            .storage_supported(ty, LambdaStorageFamily::UnknownOrigin)
        else {
            return false;
        };
        if self.policy().contains_function_payload(ty) {
            self.push(site, lambda_storage_gap_kind(gap));
            true
        } else {
            false
        }
    }

    fn check_const_ref(&mut self, site: ProfileSite, id: ConstId) {
        if !const_is_slice1(self.program, id) {
            self.push(site, ProfileErrorKind::UnsupportedConst);
        }
    }

    fn stored_payload_supported(&self, ty: TypeId) -> bool {
        !matches!(
            self.policy().materialization_for(
                ty,
                RustMaterialSource::StoredPayload,
                RustMaterialIntent::Store,
            ),
            RustMaterialization::Gap
        )
    }

    fn policy(&self) -> AirRustRepPolicy<'_> {
        AirRustRepPolicy::new(self.program, &self.classes)
    }

    fn access(&self) -> PlaceAccessCx<'_> {
        PlaceAccessCx::new(self.program, &self.classes)
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
