use anvyx_frontend::{
    air::{
        self, AggregateCtor, AggregateId, AggregateKind, CallArg, Callee, ConstId, ConstValue,
        EnumId, ExternDecl, ExternId, ExternMember, Function, FunctionId, FunctionKind, Local,
        LocalId, LocalKind, Module, Mutability, Operand, Param, ParamMode, ParamRole, Place,
        Program, Projection, RValue, ReturnMode, TypeData, TypeId, TypePassClasses, VariantDecl,
        VariantShape, VerifiedProgram,
    },
    ast::{FormatKind, FormatSign, FormatSpec},
};

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
    UnsupportedTerminator,
    UnsupportedRValue,
    UnsupportedCallee,
    UnsupportedExtern,
    UnsupportedExternMember,
    UnsupportedEntry,
    NonCopyValueRequired,
}

struct ProfileCx<'a> {
    program: &'a Program,
    classes: TypePassClasses,
    errors: Vec<RustBackendProfileError>,
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
        let ok = match self.program.type_arena.data(id) {
            TypeData::Aggregate(aggregate) => self.aggregate_decl_supported(*aggregate),
            TypeData::Enum(enm) => self.enum_decl_supported(*enm),
            TypeData::Array { elem, .. } => !self.non_copy_type(*elem),
            TypeData::List(elem) | TypeData::Slice(elem) => {
                self.check_type_ref(ProfileSite::Type(id), *elem);
                true
            }
            ty => type_is_slice1(ty),
        };
        if !ok {
            self.push(ProfileSite::Type(id), ProfileErrorKind::UnsupportedType);
        }
    }

    fn aggregate_decl_supported(&self, aggregate: AggregateId) -> bool {
        let decl = self.program.aggregate(aggregate);
        decl.kind == AggregateKind::Struct
            && decl.fields.iter().all(|field| {
                matches!(
                    self.program.type_arena.data(field.ty),
                    TypeData::Int | TypeData::Float | TypeData::Bool | TypeData::String
                )
            })
    }

    fn enum_decl_supported(&self, enm: EnumId) -> bool {
        self.program.enum_decl(enm).variants.iter().all(|variant| {
            variant_field_tys(variant).into_iter().all(|ty| {
                match self.program.type_arena.data(ty) {
                    TypeData::Aggregate(aggregate) => self.aggregate_decl_supported(*aggregate),
                    TypeData::Int | TypeData::Float | TypeData::Bool | TypeData::String => true,
                    _ => false,
                }
            })
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
            if decl.kind != AggregateKind::Struct {
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
        if !module.extern_types.is_empty() {
            self.push(
                ProfileSite::Module(index),
                ProfileErrorKind::UnsupportedModuleItem,
            );
        }
    }

    fn check_function(&mut self, id: FunctionId, function: &Function) {
        if !matches!(function.kind, FunctionKind::Normal | FunctionKind::Method) {
            self.push(
                ProfileSite::Function(id),
                ProfileErrorKind::UnsupportedFunctionKind,
            );
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
                air::AirStmt::Loop(_) => self.push(site, ProfileErrorKind::UnsupportedTerminator),
            }
        }
        if let air::AirTail::Return(Some(value)) = &body.tail {
            let site = ProfileSite::Terminator(function);
            self.check_operand(site, value);
            if self.borrowed_string_param_operand(function, value) {
                self.push(site, ProfileErrorKind::NonCopyValueRequired);
            }
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
            LocalKind::Arg | LocalKind::Temp | LocalKind::User
        ) {
            self.push(
                ProfileSite::Local(function, local),
                ProfileErrorKind::UnsupportedLocalKind,
            );
        }
        self.check_type_ref(ProfileSite::Local(function, local), data.ty);
    }

    fn check_mutating_rvalue(&mut self, site: ProfileSite, function: FunctionId, value: &RValue) {
        if let RValue::ListPush { list, .. } = value
            && self
                .program
                .function(function)
                .locals
                .get(list.root.index())
                .is_some_and(|local| local.mutability != Mutability::Mutable)
        {
            self.push(site, ProfileErrorKind::UnsupportedRValue);
        }
    }

    fn check_rvalue(&mut self, site: ProfileSite, value: &RValue) {
        match value {
            RValue::Use(operand) => {
                self.check_operand(site, operand);
                if self.non_copy_value_operand(operand) {
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
            RValue::Stringify { value, source_ty } => {
                self.check_operand(site, value);
                if matches!(value, Operand::Place(_))
                    && matches!(self.program.type_arena.data(*source_ty), TypeData::String)
                {
                    self.push(site, ProfileErrorKind::NonCopyValueRequired);
                }
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
                let TypeData::List(elem) = self.program.type_arena.data(list.ty) else {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                    return;
                };
                self.check_operand(site, value);
                if self.operand_ty(value) != *elem {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                }
                if self.non_copy_value_operand(value) {
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
            RValue::SharedRefEq { .. }
            | RValue::ListPop { .. }
            | RValue::MapGet { .. }
            | RValue::MapInsert { .. }
            | RValue::MapRemove { .. }
            | RValue::MapEntryAt { .. }
            | RValue::MakeClosure { .. } => {
                self.push(site, ProfileErrorKind::UnsupportedRValue);
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
            AggregateCtor::Struct(aggregate) => {
                if !matches!(self.program.type_arena.data(ty), TypeData::Aggregate(id) if id == aggregate)
                {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                }
                let decl = self.program.aggregate(*aggregate);
                if decl.fields.len() != fields.len() {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                }
                for (field, operand) in decl.fields.iter().zip(fields) {
                    self.check_operand(site, operand);
                    if self.operand_ty(operand) != field.ty {
                        self.push(site, ProfileErrorKind::UnsupportedRValue);
                    }
                    if self.non_copy_value_operand(operand) {
                        self.push(site, ProfileErrorKind::NonCopyValueRequired);
                    }
                }
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
                let variant_fields = variant_field_tys(variant);
                if variant_fields.len() != fields.len() {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                }
                for (expected, operand) in variant_fields.into_iter().zip(fields) {
                    self.check_operand(site, operand);
                    if self.operand_ty(operand) != expected {
                        self.push(site, ProfileErrorKind::UnsupportedRValue);
                    }
                    if self.non_copy_value_operand(operand) {
                        self.push(site, ProfileErrorKind::NonCopyValueRequired);
                    }
                }
            }
            AggregateCtor::Array => {
                let TypeData::Array { elem, len } = self.program.type_arena.data(ty) else {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                    return;
                };
                if *len != fields.len() {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                }
                for operand in fields {
                    self.check_operand(site, operand);
                    if self.operand_ty(operand) != *elem {
                        self.push(site, ProfileErrorKind::UnsupportedRValue);
                    }
                    if self.non_copy_value_operand(operand) {
                        self.push(site, ProfileErrorKind::NonCopyValueRequired);
                    }
                }
            }
            AggregateCtor::List => {
                let TypeData::List(elem) = self.program.type_arena.data(ty) else {
                    self.push(site, ProfileErrorKind::UnsupportedRValue);
                    return;
                };
                for operand in fields {
                    self.check_operand(site, operand);
                    if self.operand_ty(operand) != *elem {
                        self.push(site, ProfileErrorKind::UnsupportedRValue);
                    }
                    if self.non_copy_value_operand(operand) {
                        self.push(site, ProfileErrorKind::NonCopyValueRequired);
                    }
                }
            }
            AggregateCtor::Tuple | AggregateCtor::Map | AggregateCtor::DataRef(_) => {
                self.push(site, ProfileErrorKind::UnsupportedRValue);
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
                if self.non_copy_type(*elem) {
                    self.push(site, ProfileErrorKind::NonCopyValueRequired);
                }
            }
            _ => self.push(site, ProfileErrorKind::UnsupportedRValue),
        }
    }

    fn current_local(&self, site: ProfileSite, local: LocalId) -> Option<&Local> {
        let ProfileSite::Statement(function, _) = site else {
            return None;
        };
        self.program.function(function).locals.get(local.index())
    }

    fn check_callee(&mut self, site: ProfileSite, callee: &Callee) {
        match callee {
            Callee::Function(_) => {}
            Callee::Extern(id) if extern_is_slice1_runtime(self.program, *id) => {}
            Callee::Extern(_) | Callee::Closure(_) => {
                self.push(site, ProfileErrorKind::UnsupportedCallee);
            }
        }
    }

    fn check_call_arg(&mut self, site: ProfileSite, arg: &CallArg) {
        match arg {
            CallArg::Value(operand) => {
                self.check_operand(site, operand);
                if self.non_copy_value_operand(operand) {
                    self.push(site, ProfileErrorKind::NonCopyValueRequired);
                }
            }
            CallArg::SharedBorrow(place) => self.check_place(site, place),
            CallArg::SharedStringConst(_) => {}
            CallArg::MutBorrow(place) => {
                self.check_place(site, place);
                self.push(site, ProfileErrorKind::UnsupportedCallArgMode);
            }
        }
    }

    fn non_copy_value_operand(&self, operand: &Operand) -> bool {
        matches!(operand, Operand::Place(place) if self.non_copy_type(place.ty))
    }

    fn borrowed_string_param_operand(&self, function: FunctionId, operand: &Operand) -> bool {
        let Operand::Place(place) = operand else {
            return false;
        };
        matches!(self.program.type_arena.data(place.ty), TypeData::String)
            && place.projection.is_empty()
            && self
                .program
                .function(function)
                .signature
                .params
                .iter()
                .any(|param| param.local_id == place.root && param.mode == ParamMode::SharedBorrow)
    }

    fn non_copy_type(&self, ty: TypeId) -> bool {
        !super::rust_copyable_air_type(&self.classes, ty)
    }

    fn operand_ty(&self, operand: &Operand) -> TypeId {
        match operand {
            Operand::Place(place) => place.ty,
            Operand::Const(id) => self.program.const_arena.get(*id).ty,
        }
    }

    fn supports_param_mode(&self, ty: TypeId, mode: ParamMode) -> bool {
        match mode {
            ParamMode::Value => matches!(
                self.program.type_arena.data(ty),
                TypeData::Int
                    | TypeData::Float
                    | TypeData::Bool
                    | TypeData::Void
                    | TypeData::Aggregate(_)
                    | TypeData::Enum(_)
                    | TypeData::Array { .. }
                    | TypeData::List(_)
            ),
            ParamMode::SharedBorrow => matches!(
                self.program.type_arena.data(ty),
                TypeData::String
                    | TypeData::Aggregate(_)
                    | TypeData::Enum(_)
                    | TypeData::Array { .. }
                    | TypeData::List(_)
            ),
            ParamMode::MutBorrow => false,
        }
    }

    fn check_operand(&mut self, site: ProfileSite, operand: &Operand) {
        match operand {
            Operand::Place(place) => self.check_place(site, place),
            Operand::Const(id) => self.check_const_ref(site, *id),
        }
    }

    fn check_place(&mut self, site: ProfileSite, place: &Place) {
        if !place
            .projection
            .iter()
            .all(|projection| matches!(projection, Projection::Field(_) | Projection::Index(_)))
        {
            self.push(site, ProfileErrorKind::UnsupportedPlaceProjection);
        }
        self.check_type_ref(site, place.ty);
    }

    fn check_extern(&mut self, id: ExternId, decl: &ExternDecl) {
        if decl.member != ExternMember::FreeFunction {
            self.push(
                ProfileSite::Extern(id),
                ProfileErrorKind::UnsupportedExternMember,
            );
        }
        if !extern_is_slice1_runtime(self.program, id) {
            self.push(ProfileSite::Extern(id), ProfileErrorKind::UnsupportedExtern);
        }
    }

    fn check_type_ref(&mut self, site: ProfileSite, ty: TypeId) {
        let ok = match self.program.type_arena.data(ty) {
            TypeData::Aggregate(aggregate) => self.aggregate_decl_supported(*aggregate),
            TypeData::Enum(enm) => self.enum_decl_supported(*enm),
            TypeData::Array { elem, .. } => {
                self.check_type_ref(site, *elem);
                !self.non_copy_type(*elem)
            }
            TypeData::List(elem) | TypeData::Slice(elem) => {
                self.check_type_ref(site, *elem);
                true
            }
            ty => type_is_slice1(ty),
        };
        if !ok {
            self.push(site, ProfileErrorKind::UnsupportedType);
        }
    }

    fn check_const_ref(&mut self, site: ProfileSite, id: ConstId) {
        if !const_is_slice1(self.program, id) {
            self.push(site, ProfileErrorKind::UnsupportedConst);
        }
    }

    fn push(&mut self, site: ProfileSite, kind: ProfileErrorKind) {
        self.errors.push(RustBackendProfileError { site, kind });
    }
}

fn variant_field_tys(variant: &VariantDecl) -> Vec<TypeId> {
    match &variant.shape {
        VariantShape::Unit => vec![],
        VariantShape::Tuple(fields) => fields.clone(),
        VariantShape::Struct(fields) => fields.iter().map(|field| field.ty).collect(),
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
    )
}

fn extern_is_slice1_runtime(program: &Program, id: ExternId) -> bool {
    let decl = program.extern_decl(id);
    if decl.member != ExternMember::FreeFunction {
        return false;
    }
    let module = program.module(decl.module);
    let [segment] = module.path.as_slice() else {
        return false;
    };
    if segment.as_str() != "core_runtime" {
        return false;
    }
    match decl.name.as_str() {
        "_println" => extern_signature_is(
            program,
            decl,
            &[(TypeData::String, ParamMode::SharedBorrow)],
            TypeData::Void,
        ),
        "_assert" => extern_signature_is(
            program,
            decl,
            &[
                (TypeData::Bool, ParamMode::Value),
                (TypeData::String, ParamMode::SharedBorrow),
            ],
            TypeData::Void,
        ),
        _ => false,
    }
}

fn extern_signature_is(
    program: &Program,
    decl: &ExternDecl,
    params: &[(TypeData, ParamMode)],
    ret: TypeData,
) -> bool {
    decl.params.len() == params.len()
        && decl.params.iter().zip(params).all(|(param, (ty, mode))| {
            program.type_arena.data(param.ty) == ty && param.mode == *mode
        })
        && program.type_arena.data(decl.return_type) == &ret
}
