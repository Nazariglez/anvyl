pub use super::typing::PrimitiveKind;
use super::{
    AggregateKind, ConstValue, ExternMember, Function, LocalKind, Mutability, ParamMode, ParamRole,
    ParamType, Program, ReturnMode, TypeData, VariantShape,
    body::{
        AggregateCtor, AirBlock, AirEnumMatch, AirIf, AirStmt, AirTail, CallArg, Callee, Operand,
        Place, Projection, RValue,
    },
    ids::*,
    typing::{self, PrimitiveTypes, supports_scalar_binary, supports_scalar_unary},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VerifyError {
    pub site: VerifySite,
    pub kind: VerifyErrorKind,
}

impl std::fmt::Display for VerifyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}: {:?}", self.site, self.kind)
    }
}

impl std::error::Error for VerifyError {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VerifySite {
    Program,
    Module(ModuleId),
    Type(TypeId),
    Const(ConstId),
    Aggregate(AggregateId),
    Enum(EnumId),
    ExternType(ExternTypeId),
    Extern(ExternId),
    Function(FunctionId),
    Statement {
        function: FunctionId,
        block: BlockId,
        index: usize,
    },
    Terminator {
        function: FunctionId,
        block: BlockId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VerifyErrorKind {
    BadReference(BadReference),
    BadFunction(BadFunction),
    BadPlace(BadPlace),
    BadCall(BadCall),
    BadType(BadType),
    BadConst(BadConst),
    BadModule(BadModule),
    BadRValue(BadRValue),
    BadStatement(BadStatement),
    BadExtern(BadExtern),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadExtern {
    ReceiverTypeMismatch { expected: TypeId, found: TypeId },
    OperatorOperandMismatch,
    MemberParamCountMismatch { expected: usize, found: usize },
    ReceiverModeMismatch,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadType {
    DuplicatePrimitive {
        kind: PrimitiveKind,
        first: TypeId,
        duplicate: TypeId,
    },
    EmptyDynContract,
    Recursive(TypeId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadConst {
    TypeMismatch { expected: TypeId, found: TypeId },
    NilMustBeOptional(TypeId),
    MissingPrimitive(PrimitiveKind),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadModule {
    DuplicateItem(ModuleItem),
    ItemWrongModule {
        item: ModuleItem,
        expected: ModuleId,
        found: ModuleId,
    },
    MissingItem(ModuleItem),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ModuleItem {
    Function(FunctionId),
    Aggregate(AggregateId),
    Enum(EnumId),
    ExternType(ExternTypeId),
    Extern(ExternId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadRValue {
    MissingPrimitive(PrimitiveKind),
    UnaryTypeMismatch {
        value: TypeId,
        result: TypeId,
    },
    BinaryTypeMismatch {
        lhs: TypeId,
        rhs: TypeId,
        result: TypeId,
    },
    UnsupportedBinaryOp(crate::ast::BinaryOp),
    CastMustConvertIntAndFloat {
        value: TypeId,
        target: TypeId,
    },
    StringConcatPartMustBeString(TypeId),
    StringifyOperandTypeMismatch {
        operand: TypeId,
        source: TypeId,
    },
    StringifyAnySource {
        source: TypeId,
    },
    StringifyVoidSource {
        source: TypeId,
    },
    AggregateCtorResultTypeMismatch {
        aggregate: AggregateId,
        expected: AggregateKind,
        found: TypeId,
    },
    AggregateCtorKindMismatch {
        aggregate: AggregateId,
        expected: AggregateKind,
        found: AggregateKind,
    },
    AggregateCtorFieldCountMismatch {
        aggregate: AggregateId,
        expected: usize,
        found: usize,
    },
    AggregateCtorFieldTypeMismatch {
        aggregate: AggregateId,
        field: usize,
        expected: TypeId,
        found: TypeId,
    },
    EnumCtorResultTypeMismatch {
        enum_id: EnumId,
        found: TypeId,
    },
    EnumCtorFieldCountMismatch {
        enum_id: EnumId,
        variant: VariantId,
        expected: usize,
        found: usize,
    },
    EnumCtorFieldTypeMismatch {
        enum_id: EnumId,
        variant: VariantId,
        field: usize,
        expected: TypeId,
        found: TypeId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadStatement {
    InitParamLocal(LocalId),
    InitTypeMismatch { expected: TypeId, found: TypeId },
    AssignTypeMismatch { expected: TypeId, found: TypeId },
    AssignImmutableLocal(LocalId),
    ReadUninitializedLocal(LocalId),
    AssignUninitializedLocal(LocalId),
    InitImmutableLocalTwice(LocalId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadReference {
    InvalidEntry(FunctionId),
    InvalidFunction(FunctionId),
    InvalidExtern(ExternId),
    InvalidExternType(ExternTypeId),
    InvalidAggregate(AggregateId),
    InvalidEnum(EnumId),
    InvalidType(TypeId),
    InvalidConst(ConstId),
    InvalidLocal(LocalId),
    InvalidField {
        aggregate: AggregateId,
        field: FieldId,
    },
    InvalidVariant {
        enum_id: EnumId,
        variant: VariantId,
    },
    InvalidModule(ModuleId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadFunction {
    ParamLocalOutOfRange {
        param: usize,
        total_locals: usize,
    },
    DuplicateParamLocal {
        first: LocalId,
        second: LocalId,
    },
    IfCondMustBeBool(TypeId),
    SwitchDiscriminantMustBeEnum(TypeId),
    DuplicateSwitchArm(VariantId),
    SwitchArmVariantMismatch {
        expected_enum: EnumId,
        variant: VariantId,
    },
    NonVoidFunctionMustReturnValue(TypeId),
    VoidFunctionMustReturnNone,
    ReturnedTypeMismatch {
        expected: TypeId,
        found: TypeId,
    },
    PlaceReturnMustReturnPlace,
    StringifyOverrideModuleMismatch {
        expected: ModuleId,
        found: ModuleId,
    },
    StringifyOverrideMissingReceiver,
    StringifyOverrideReceiverTypeMismatch {
        expected: AggregateId,
        found: TypeId,
    },
    StringifyOverrideReturnMustBeString(TypeId),
    LenSourceMustBeCountable(TypeId),
    ListElementTypeMismatch {
        expected: TypeId,
        found: TypeId,
    },
    MapKeyTypeMismatch {
        expected: TypeId,
        found: TypeId,
    },
    MapValueTypeMismatch {
        expected: TypeId,
        found: TypeId,
    },
    MapGetResultMustBeOptionalValue {
        expected_value: TypeId,
        found: TypeId,
    },
    ListPopResultMustBeOptionalElement {
        expected_elem: TypeId,
        found: TypeId,
    },
    ListSliceResultMustBeList {
        found: TypeId,
    },
    SliceIndexMustBeInt {
        which: &'static str,
        found: TypeId,
    },
    IndexTypeUnavailable,
    ParamLocalMustBeArg {
        param: usize,
        local: LocalId,
    },
    ParamLocalTypeMismatch {
        param: usize,
        expected: TypeId,
        found: TypeId,
    },
    MutBorrowParamLocalMustBeMutable {
        param: usize,
        local: LocalId,
    },
    BreakOutsideLoop(AirLoopId),
    ContinueOutsideLoop(AirLoopId),
    MatchNotExhaustive(EnumId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadPlace {
    FieldProjectionOnNonAggregate(TypeId),
    FieldProjectionKindMismatch {
        aggregate: AggregateId,
        expected: AggregateKind,
        found: AggregateKind,
    },
    TupleFieldOutOfRange {
        ty: TypeId,
        index: u16,
        len: usize,
    },
    TupleProjectionOnNonTuple(TypeId),
    VariantFieldOutOfRange {
        ty: TypeId,
        index: u16,
        len: usize,
    },
    VariantProjectionOnNonEnum(TypeId),
    IndexProjectionOnNonIndexable(TypeId),
    PlaceTypeMismatch {
        expected: TypeId,
        found: TypeId,
    },
    IndexLocalTypeMismatch {
        expected: TypeId,
        found: TypeId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadCall {
    ClosureCalleeMustBeFunction,
    ArityMismatch {
        expected: usize,
        found: usize,
    },
    ArgTypeMismatch {
        index: usize,
        expected: TypeId,
        found: TypeId,
    },
    ArgModeMismatch {
        index: usize,
        expected: ParamMode,
        found: ParamMode,
    },
    ArgAliasConflict {
        first: usize,
        second: usize,
    },
}

pub fn verify(program: &Program) -> Result<VerifiedProgram<'_>, Vec<VerifyError>> {
    let mut cx = VerifyCx::new(program);
    collect_errors(&mut cx);
    if cx.errors.is_empty() {
        Ok(VerifiedProgram { program })
    } else {
        Err(cx.errors)
    }
}

#[cfg(test)]
pub(crate) fn verify_structured_body(
    program: &Program,
    function_id: FunctionId,
    body: &super::AirBody,
) -> Result<(), Vec<VerifyError>> {
    let mut cx = VerifyCx::new(program);
    let mut state = LocalInit::new(program.function(function_id));
    verify_air_block(
        &mut cx,
        function_id,
        &body.block,
        &mut state,
        &mut Vec::new(),
    );
    if cx.errors.is_empty() {
        Ok(())
    } else {
        Err(cx.errors)
    }
}

pub struct VerifiedProgram<'a> {
    program: &'a Program,
}

impl std::fmt::Debug for VerifiedProgram<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("VerifiedProgram").finish()
    }
}

impl VerifiedProgram<'_> {
    pub fn program(&self) -> &Program {
        self.program
    }
}

struct VerifyCx<'a> {
    program: &'a Program,
    primitives: PrimitiveTypes,
    type_states: Vec<TypeState>,
    errors: Vec<VerifyError>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum TypeState {
    Unseen,
    Visiting,
    Done,
}

impl<'a> VerifyCx<'a> {
    fn new(program: &'a Program) -> Self {
        Self {
            program,
            primitives: PrimitiveTypes::scan(program),
            type_states: vec![TypeState::Unseen; program.type_arena.len()],
            errors: Vec::new(),
        }
    }

    fn push(&mut self, site: VerifySite, kind: VerifyErrorKind) {
        self.errors.push(VerifyError { site, kind });
    }

    fn term_site(function_id: FunctionId, block_id: BlockId) -> VerifySite {
        VerifySite::Terminator {
            function: function_id,
            block: block_id,
        }
    }

    fn stmt_site(function_id: FunctionId, block_id: BlockId, index: usize) -> VerifySite {
        VerifySite::Statement {
            function: function_id,
            block: block_id,
            index,
        }
    }

    fn has_type(&self, id: TypeId) -> bool {
        id.index() < self.program.type_arena.len()
    }

    fn has_aggregate(&self, id: AggregateId) -> bool {
        id.index() < self.program.aggregates.len()
    }

    fn has_enum(&self, id: EnumId) -> bool {
        id.index() < self.program.enums.len()
    }

    fn has_extern_type(&self, id: ExternTypeId) -> bool {
        id.index() < self.program.extern_types.len()
    }

    fn has_function(&self, id: FunctionId) -> bool {
        id.index() < self.program.functions.len()
    }

    fn has_extern(&self, id: ExternId) -> bool {
        id.index() < self.program.externs.len()
    }

    fn has_const(&self, id: ConstId) -> bool {
        id.index() < self.program.const_arena.len()
    }

    fn has_module(&self, id: ModuleId) -> bool {
        id.index() < self.program.modules.len()
    }

    fn verify_module_ref(&mut self, site: VerifySite, module: ModuleId) {
        if !self.has_module(module) {
            self.push(
                site,
                VerifyErrorKind::BadReference(BadReference::InvalidModule(module)),
            );
        }
    }

    fn verify_type_ref(&mut self, site: VerifySite, ty: TypeId) {
        if !self.has_type(ty) {
            self.push(
                site,
                VerifyErrorKind::BadReference(BadReference::InvalidType(ty)),
            );
            return;
        }
        match self.type_states[ty.index()] {
            TypeState::Done => return,
            TypeState::Visiting => {
                self.push(site, VerifyErrorKind::BadType(BadType::Recursive(ty)));
                return;
            }
            TypeState::Unseen => {}
        }
        self.type_states[ty.index()] = TypeState::Visiting;
        verify_type(self, ty);
        self.type_states[ty.index()] = TypeState::Done;
    }

    fn variant_belongs_to_enum(&self, enum_id: EnumId, variant: VariantId) -> bool {
        self.program
            .enums
            .get(enum_id.index())
            .is_some_and(|enm| variant.index() < enm.variants.len())
    }

    fn type_data(&self, ty: TypeId) -> Option<&TypeData> {
        self.program.type_arena.get(ty)
    }
}

#[derive(Clone)]
struct LocalInit {
    definite: Vec<bool>,
    possible: Vec<bool>,
}

impl LocalInit {
    fn new(function: &Function) -> Self {
        let mut state = Self {
            definite: vec![false; function.locals.len()],
            possible: vec![false; function.locals.len()],
        };
        for param in &function.signature.params {
            if param.local_id.index() < function.locals.len() {
                state.definite[param.local_id.index()] = true;
                state.possible[param.local_id.index()] = true;
            }
        }
        state
    }

    fn is_definite(&self, local: LocalId) -> bool {
        self.definite.get(local.index()).copied().unwrap_or(false)
    }

    fn is_possible(&self, local: LocalId) -> bool {
        self.possible.get(local.index()).copied().unwrap_or(false)
    }

    fn init(&mut self, local: LocalId) {
        if local.index() < self.definite.len() {
            self.definite[local.index()] = true;
            self.possible[local.index()] = true;
        }
    }

    fn join(states: impl IntoIterator<Item = Self>) -> Option<Self> {
        let mut states = states.into_iter();
        let mut joined = states.next()?;
        for state in states {
            for (left, right) in joined.definite.iter_mut().zip(state.definite) {
                *left &= right;
            }
            for (left, right) in joined.possible.iter_mut().zip(state.possible) {
                *left |= right;
            }
        }
        Some(joined)
    }
}

fn collect_errors(cx: &mut VerifyCx<'_>) {
    if let Some(entry) = cx.program.entry
        && !cx.has_function(entry)
    {
        cx.push(
            VerifySite::Program,
            VerifyErrorKind::BadReference(BadReference::InvalidEntry(entry)),
        );
    }

    for duplicate in cx.primitives.duplicates().to_vec() {
        cx.push(
            VerifySite::Type(duplicate.duplicate),
            VerifyErrorKind::BadType(BadType::DuplicatePrimitive {
                kind: duplicate.kind,
                first: duplicate.first,
                duplicate: duplicate.duplicate,
            }),
        );
    }

    for (id, _) in cx.program.type_arena.iter().enumerate() {
        let ty = TypeId::from_index(id);
        cx.verify_type_ref(VerifySite::Type(ty), ty);
    }

    for (id, _) in cx.program.modules.iter().enumerate() {
        verify_module(cx, ModuleId::from_index(id));
    }
    for (id, _) in cx.program.const_arena.iter().enumerate() {
        verify_const(cx, ConstId::from_index(id));
    }
    for (id, _) in cx.program.aggregates.iter().enumerate() {
        verify_aggregate(cx, AggregateId::from_index(id));
    }
    for (id, _) in cx.program.enums.iter().enumerate() {
        verify_enum(cx, EnumId::from_index(id));
    }
    for (id, _) in cx.program.extern_types.iter().enumerate() {
        verify_extern_type(cx, ExternTypeId::from_index(id));
    }
    for (id, _) in cx.program.externs.iter().enumerate() {
        verify_extern(cx, ExternId::from_index(id));
    }
    for (id, _) in cx.program.functions.iter().enumerate() {
        verify_function(cx, FunctionId::from_index(id));
    }
}

fn verify_module(cx: &mut VerifyCx<'_>, id: ModuleId) {
    let module = cx.program.module(id);
    let site = VerifySite::Module(id);
    verify_module_items(cx, &site, id, &module.functions, |cx, item| {
        cx.program
            .functions
            .get(item.index())
            .map(|decl| decl.module)
    });
    verify_module_items(cx, &site, id, &module.aggregates, |cx, item| {
        cx.program
            .aggregates
            .get(item.index())
            .map(|decl| decl.module)
    });
    verify_module_items(cx, &site, id, &module.enums, |cx, item| {
        cx.program.enums.get(item.index()).map(|decl| decl.module)
    });
    verify_module_items(cx, &site, id, &module.extern_types, |cx, item| {
        cx.program
            .extern_types
            .get(item.index())
            .map(|decl| decl.module)
    });
    verify_module_items(cx, &site, id, &module.externs, |cx, item| {
        cx.program.externs.get(item.index()).map(|decl| decl.module)
    });
}

fn verify_module_items<T>(
    cx: &mut VerifyCx<'_>,
    site: &VerifySite,
    module: ModuleId,
    items: &[T],
    owner: impl Fn(&VerifyCx<'_>, T) -> Option<ModuleId>,
) where
    T: Copy + Eq + std::hash::Hash + IntoModuleReference,
{
    let mut seen = std::collections::HashSet::new();
    for item in items {
        if !seen.insert(*item) {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadModule(BadModule::DuplicateItem((*item).module_item())),
            );
        }
        match owner(cx, *item) {
            Some(found) if found == module => {}
            Some(found) => cx.push(
                site.clone(),
                VerifyErrorKind::BadModule(BadModule::ItemWrongModule {
                    item: (*item).module_item(),
                    expected: module,
                    found,
                }),
            ),
            None => cx.push(site.clone(), (*item).invalid_reference()),
        }
    }
}

trait IntoModuleReference {
    fn invalid_reference(self) -> VerifyErrorKind;
    fn module_item(self) -> ModuleItem;
}

macro_rules! impl_module_ref {
    ($id:ty, $bad:ident, $item:ident) => {
        impl IntoModuleReference for $id {
            fn invalid_reference(self) -> VerifyErrorKind {
                VerifyErrorKind::BadReference(BadReference::$bad(self))
            }

            fn module_item(self) -> ModuleItem {
                ModuleItem::$item(self)
            }
        }
    };
}

impl_module_ref!(FunctionId, InvalidFunction, Function);
impl_module_ref!(AggregateId, InvalidAggregate, Aggregate);
impl_module_ref!(EnumId, InvalidEnum, Enum);
impl_module_ref!(ExternTypeId, InvalidExternType, ExternType);
impl_module_ref!(ExternId, InvalidExtern, Extern);

fn verify_const(cx: &mut VerifyCx<'_>, id: ConstId) {
    let konst = cx.program.const_data(id);
    let site = VerifySite::Const(id);
    if !cx.has_type(konst.ty) {
        cx.push(
            site,
            VerifyErrorKind::BadReference(BadReference::InvalidType(konst.ty)),
        );
        return;
    }
    let expected = match &konst.value {
        ConstValue::Int(_) => required_const_primitive(cx, site.clone(), PrimitiveKind::Int),
        ConstValue::Float(_) => required_const_primitive(cx, site.clone(), PrimitiveKind::Float),
        ConstValue::Bool(_) => required_const_primitive(cx, site.clone(), PrimitiveKind::Bool),
        ConstValue::String(_) => required_const_primitive(cx, site.clone(), PrimitiveKind::String),
        ConstValue::Nil => {
            if !matches!(cx.type_data(konst.ty), Some(TypeData::Optional(_))) {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadConst(BadConst::NilMustBeOptional(konst.ty)),
                );
            }
            None
        }
    };
    if let Some(expected) = expected
        && konst.ty != expected
    {
        cx.push(
            site,
            VerifyErrorKind::BadConst(BadConst::TypeMismatch {
                expected,
                found: konst.ty,
            }),
        );
    }
}

fn required_const_primitive(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    kind: PrimitiveKind,
) -> Option<TypeId> {
    require_primitive(cx, site, kind, |kind| {
        VerifyErrorKind::BadConst(BadConst::MissingPrimitive(kind))
    })
}

fn required_rvalue_primitive(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    kind: PrimitiveKind,
) -> Option<TypeId> {
    require_primitive(cx, site, kind, |kind| {
        VerifyErrorKind::BadRValue(BadRValue::MissingPrimitive(kind))
    })
}

fn require_primitive(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    kind: PrimitiveKind,
    missing: impl FnOnce(PrimitiveKind) -> VerifyErrorKind,
) -> Option<TypeId> {
    let ty = cx.primitives.get(kind);
    if ty.is_none() {
        cx.push(site, missing(kind));
    }
    ty
}

fn verify_decl_listed_once<T>(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    module: ModuleId,
    item: T,
    list: impl Fn(&super::Module) -> &[T],
) where
    T: Copy + Eq + IntoModuleReference,
{
    if !cx.has_module(module) {
        return;
    }
    let count = list(cx.program.module(module))
        .iter()
        .filter(|listed| **listed == item)
        .count();
    if count == 0 {
        cx.push(
            site,
            VerifyErrorKind::BadModule(BadModule::MissingItem(item.module_item())),
        );
    } else if count > 1 {
        cx.push(
            site,
            VerifyErrorKind::BadModule(BadModule::DuplicateItem(item.module_item())),
        );
    }
}

fn verify_aggregate(cx: &mut VerifyCx<'_>, id: AggregateId) {
    let agg = cx.program.aggregate(id);
    let site = VerifySite::Aggregate(id);
    cx.verify_module_ref(site.clone(), agg.module);
    verify_decl_listed_once(cx, site.clone(), agg.module, id, |m| &m.aggregates);
    for field in &agg.fields {
        cx.verify_type_ref(site.clone(), field.ty);
    }
    if let Some(function_id) = agg.stringify_override {
        if function_id.index() >= cx.program.functions.len() {
            cx.push(
                site,
                VerifyErrorKind::BadReference(BadReference::InvalidFunction(function_id)),
            );
            return;
        }
        let function = cx.program.function(function_id);
        if function.module != agg.module {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::StringifyOverrideModuleMismatch {
                    expected: agg.module,
                    found: function.module,
                }),
            );
        }
        match function.signature.params.first() {
            Some(param) if param.role == ParamRole::Receiver => {
                let matches_owner = cx.has_type(param.ty)
                    && match (agg.kind, cx.program.type_data(param.ty)) {
                        (AggregateKind::Struct, TypeData::Aggregate(owner))
                        | (AggregateKind::DataRef, TypeData::DataRef(owner)) => *owner == id,
                        _ => false,
                    };
                if !matches_owner {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadFunction(
                            BadFunction::StringifyOverrideReceiverTypeMismatch {
                                expected: id,
                                found: param.ty,
                            },
                        ),
                    );
                }
            }
            _ => cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::StringifyOverrideMissingReceiver),
            ),
        }
        if !cx.has_type(function.signature.return_type()) {
            cx.push(
                site,
                VerifyErrorKind::BadReference(BadReference::InvalidType(
                    function.signature.return_type(),
                )),
            );
            return;
        }
        if !matches!(function.signature.return_mode, ReturnMode::Value(_))
            || !matches!(
                cx.program.type_data(function.signature.return_type()),
                TypeData::String
            )
        {
            cx.push(
                site,
                VerifyErrorKind::BadFunction(BadFunction::StringifyOverrideReturnMustBeString(
                    function.signature.return_type(),
                )),
            );
        }
    }
}

fn verify_enum(cx: &mut VerifyCx<'_>, id: EnumId) {
    let enm = cx.program.enum_decl(id);
    cx.verify_module_ref(VerifySite::Enum(id), enm.module);
    verify_decl_listed_once(cx, VerifySite::Enum(id), enm.module, id, |m| &m.enums);
    for variant in &enm.variants {
        match &variant.shape {
            VariantShape::Unit => {}
            VariantShape::Tuple(types) => {
                for ty in types {
                    cx.verify_type_ref(VerifySite::Enum(id), *ty);
                }
            }
            VariantShape::Struct(fields) => {
                for field in fields {
                    cx.verify_type_ref(VerifySite::Enum(id), field.ty);
                }
            }
        }
    }
}

fn extern_owner_type(cx: &VerifyCx<'_>, owner: ExternTypeId) -> Option<TypeId> {
    cx.program
        .type_arena
        .iter()
        .enumerate()
        .find_map(|(index, ty)| {
            matches!(ty, TypeData::Extern(id) if *id == owner).then_some(TypeId::from_index(index))
        })
}

fn verify_receiver(cx: &mut VerifyCx<'_>, site: VerifySite, owner_ty: TypeId, receiver_ty: TypeId) {
    cx.verify_type_ref(site.clone(), receiver_ty);
    if owner_ty != receiver_ty {
        cx.push(
            site,
            VerifyErrorKind::BadExtern(BadExtern::ReceiverTypeMismatch {
                expected: owner_ty,
                found: receiver_ty,
            }),
        );
    }
}

fn verify_extern_type(cx: &mut VerifyCx<'_>, id: ExternTypeId) {
    let ty = cx.program.extern_type(id);
    let site = VerifySite::ExternType(id);
    cx.verify_module_ref(site.clone(), ty.module);
    verify_decl_listed_once(cx, site.clone(), ty.module, id, |m| &m.extern_types);
    let owner_ty = extern_owner_type(cx, id);
    for field in &ty.fields {
        cx.verify_type_ref(site.clone(), field.ty);
        if let Some(owner_ty) = owner_ty {
            verify_receiver(cx, site.clone(), owner_ty, field.get_receiver.ty);
            verify_receiver(cx, site.clone(), owner_ty, field.set_receiver.ty);
        } else {
            cx.verify_type_ref(site.clone(), field.get_receiver.ty);
            cx.verify_type_ref(site.clone(), field.set_receiver.ty);
        }
        if field.get_receiver.mode != ParamMode::SharedBorrow {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadExtern(BadExtern::ReceiverModeMismatch),
            );
        }
        if field.set_receiver.mode != ParamMode::MutBorrow {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadExtern(BadExtern::ReceiverModeMismatch),
            );
        }
    }
    for method in &ty.methods {
        if let Some(owner_ty) = owner_ty {
            verify_receiver(cx, site.clone(), owner_ty, method.receiver.ty);
        } else {
            cx.verify_type_ref(site.clone(), method.receiver.ty);
        }
        for param in &method.params {
            cx.verify_type_ref(site.clone(), param.ty);
        }
        cx.verify_type_ref(site.clone(), method.return_type);
    }
    for static_ in &ty.statics {
        for param in &static_.params {
            cx.verify_type_ref(site.clone(), param.ty);
        }
        cx.verify_type_ref(site.clone(), static_.return_type);
    }
    for op in &ty.operators {
        if let Some(owner_ty) = owner_ty {
            verify_receiver(cx, site.clone(), owner_ty, op.receiver.ty);
        } else {
            cx.verify_type_ref(site.clone(), op.receiver.ty);
        }
        if op.receiver.mode != ParamMode::SharedBorrow {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadExtern(BadExtern::ReceiverModeMismatch),
            );
        }
        match (&op.kind, &op.operand) {
            (super::ExternOp::Unary(_), Some(_)) | (super::ExternOp::Binary { .. }, None) => {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadExtern(BadExtern::OperatorOperandMismatch),
                );
            }
            _ => {}
        }
        if let Some(operand) = &op.operand {
            cx.verify_type_ref(site.clone(), operand.ty);
        }
        cx.verify_type_ref(site.clone(), op.return_type);
    }
}

fn verify_extern(cx: &mut VerifyCx<'_>, id: ExternId) {
    let ext = cx.program.extern_decl(id);
    let site = VerifySite::Extern(id);
    cx.verify_module_ref(site.clone(), ext.module);
    verify_decl_listed_once(cx, site.clone(), ext.module, id, |m| &m.externs);
    verify_extern_member(cx, site.clone(), &ext.member, &ext.params);
    for param in &ext.params {
        cx.verify_type_ref(site.clone(), param.ty);
    }
    cx.verify_type_ref(site, ext.return_type);
}

fn verify_receiver_mode(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    found: ParamMode,
    expected: ParamMode,
) {
    if found != expected {
        cx.push(
            site,
            VerifyErrorKind::BadExtern(BadExtern::ReceiverModeMismatch),
        );
    }
}

fn verify_member_param_count(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    found: usize,
    expected: usize,
) {
    if found != expected {
        cx.push(
            site,
            VerifyErrorKind::BadExtern(BadExtern::MemberParamCountMismatch { expected, found }),
        );
    }
}

fn verify_extern_member(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    member: &ExternMember,
    params: &[super::ExternParamDecl],
) {
    let expected_params = match member {
        ExternMember::FieldGetter { .. } | ExternMember::UnaryOperator { .. } => Some(0),
        ExternMember::FieldSetter { .. } | ExternMember::BinaryOperator { .. } => Some(1),
        ExternMember::FreeFunction
        | ExternMember::Init { .. }
        | ExternMember::Method { .. }
        | ExternMember::StaticMethod { .. } => None,
    };
    if let Some(expected) = expected_params {
        verify_member_param_count(cx, site.clone(), params.len(), expected);
    }
    let receiver_mode = match member {
        ExternMember::FieldGetter { receiver, .. }
        | ExternMember::UnaryOperator { receiver, .. }
        | ExternMember::BinaryOperator { receiver, .. } => {
            Some((receiver.mode, ParamMode::SharedBorrow))
        }
        ExternMember::FieldSetter { receiver, .. } => Some((receiver.mode, ParamMode::MutBorrow)),
        ExternMember::FreeFunction
        | ExternMember::Init { .. }
        | ExternMember::Method { .. }
        | ExternMember::StaticMethod { .. } => None,
    };
    if let Some((found, expected)) = receiver_mode {
        verify_receiver_mode(cx, site.clone(), found, expected);
    }
    match member {
        ExternMember::FreeFunction => {}
        ExternMember::FieldGetter {
            owner, receiver, ..
        }
        | ExternMember::FieldSetter { owner, receiver }
        | ExternMember::Method { owner, receiver }
        | ExternMember::UnaryOperator {
            owner, receiver, ..
        }
        | ExternMember::BinaryOperator {
            owner, receiver, ..
        } => {
            if !cx.has_extern_type(*owner) {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadReference(BadReference::InvalidExternType(*owner)),
                );
                cx.verify_type_ref(site, receiver.ty);
                return;
            }
            if let Some(owner_ty) = extern_owner_type(cx, *owner) {
                verify_receiver(cx, site, owner_ty, receiver.ty);
            } else {
                cx.verify_type_ref(site, receiver.ty);
            }
        }
        ExternMember::StaticMethod { owner } | ExternMember::Init { owner } => {
            if !cx.has_extern_type(*owner) {
                cx.push(
                    site,
                    VerifyErrorKind::BadReference(BadReference::InvalidExternType(*owner)),
                );
            }
        }
    }
}

fn verify_function(cx: &mut VerifyCx<'_>, id: FunctionId) {
    let func = cx.program.function(id);
    let site = VerifySite::Function(id);
    cx.verify_module_ref(site.clone(), func.module);
    verify_decl_listed_once(cx, site.clone(), func.module, id, |m| &m.functions);

    let mut seen_locals = std::collections::HashSet::new();
    for (i, param) in func.signature.params.iter().enumerate() {
        cx.verify_type_ref(site.clone(), param.ty);
        let is_out_of_range = param.local_id.index() >= func.locals.len();
        if is_out_of_range {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::ParamLocalOutOfRange {
                    param: i,
                    total_locals: func.locals.len(),
                }),
            );
        } else {
            let local = &func.locals[param.local_id.index()];
            if !seen_locals.insert(param.local_id) {
                let first_idx = func.signature.params[..i]
                    .iter()
                    .position(|p| p.local_id == param.local_id)
                    .unwrap();
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadFunction(BadFunction::DuplicateParamLocal {
                        first: func.signature.params[first_idx].local_id,
                        second: param.local_id,
                    }),
                );
            }
            if local.kind != LocalKind::Arg {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadFunction(BadFunction::ParamLocalMustBeArg {
                        param: i,
                        local: param.local_id,
                    }),
                );
            }
            if local.ty != param.ty {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadFunction(BadFunction::ParamLocalTypeMismatch {
                        param: i,
                        expected: param.ty,
                        found: local.ty,
                    }),
                );
            }
            if param.mode == ParamMode::MutBorrow && local.mutability != Mutability::Mutable {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadFunction(BadFunction::MutBorrowParamLocalMustBeMutable {
                        param: i,
                        local: param.local_id,
                    }),
                );
            }
        }
    }

    for local in &func.locals {
        cx.verify_type_ref(site.clone(), local.ty);
    }

    cx.verify_type_ref(site, func.signature.return_type());

    let mut state = LocalInit::new(func);
    if verify_air_block(cx, id, &func.body.block, &mut state, &mut Vec::new()).is_some()
        && !matches!(
            cx.type_data(func.signature.return_type()),
            Some(TypeData::Void)
        )
    {
        cx.push(
            VerifySite::Function(id),
            VerifyErrorKind::BadFunction(BadFunction::NonVoidFunctionMustReturnValue(
                func.signature.return_type(),
            )),
        );
    }
}

fn verify_air_block(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block: &AirBlock,
    state: &mut LocalInit,
    loops: &mut Vec<LoopCtx>,
) -> Option<LocalInit> {
    for (index, stmt) in block.stmts.iter().enumerate() {
        *state = verify_air_stmt(cx, function_id, index, stmt, state, loops)?;
    }
    verify_air_tail(cx, function_id, &block.tail, state, loops)
}

struct LoopCtx {
    id: AirLoopId,
    breaks: Vec<LocalInit>,
}

fn verify_air_stmt(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    index: usize,
    stmt: &AirStmt,
    state: &LocalInit,
    loops: &mut Vec<LoopCtx>,
) -> Option<LocalInit> {
    let block_id = BlockId::from_index(0);
    match stmt {
        AirStmt::Init { local, value } => {
            verify_air_rvalue_reads(cx, function_id, index, value, state);
            verify_init_stmt(cx, function_id, block_id, index, *local, value);
            let function = cx.program.function(function_id);
            if let Some(local_decl) = function.locals.get(local.index()) {
                if local_decl.mutability == Mutability::Immutable && state.is_possible(*local) {
                    cx.push(
                        VerifyCx::stmt_site(function_id, block_id, index),
                        VerifyErrorKind::BadStatement(BadStatement::InitImmutableLocalTwice(
                            *local,
                        )),
                    );
                }
                let mut next = state.clone();
                next.init(*local);
                return Some(next);
            }
            Some(state.clone())
        }
        AirStmt::Assign { dst, value } => {
            verify_air_rvalue_reads(cx, function_id, index, value, state);
            verify_air_place_read(cx, function_id, index, dst, state);
            if !state.is_definite(dst.root) {
                cx.push(
                    VerifyCx::stmt_site(function_id, block_id, index),
                    VerifyErrorKind::BadStatement(BadStatement::AssignUninitializedLocal(dst.root)),
                );
            }
            verify_assign_stmt(cx, function_id, block_id, index, dst, value);
            Some(state.clone())
        }
        AirStmt::Eval(value) => {
            verify_air_rvalue_reads(cx, function_id, index, value, state);
            verify_rvalue(cx, function_id, block_id, Some(index), value);
            Some(state.clone())
        }
        AirStmt::If(branch) => verify_air_if(cx, function_id, index, branch, state, loops),
        AirStmt::Loop(loop_) => {
            loops.push(LoopCtx {
                id: loop_.id,
                breaks: Vec::new(),
            });
            let mut body_state = state.clone();
            verify_air_block(cx, function_id, &loop_.body, &mut body_state, loops);
            let loop_ctx = loops.pop().unwrap();
            LocalInit::join(loop_ctx.breaks)
        }
        AirStmt::EnumMatch(match_) => {
            verify_air_match(cx, function_id, index, match_, state, loops)
        }
    }
}

fn verify_air_if(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    index: usize,
    branch: &AirIf,
    state: &LocalInit,
    loops: &mut Vec<LoopCtx>,
) -> Option<LocalInit> {
    let site = VerifyCx::stmt_site(function_id, BlockId::from_index(0), index);
    verify_air_operand_read(cx, function_id, index, &branch.cond, state);
    verify_operand(
        cx,
        function_id,
        BlockId::from_index(0),
        Some(index),
        &branch.cond,
    );
    if let Some(cond_ty) = operand_ty(cx, &branch.cond)
        && !cx.primitives.is_bool(cond_ty)
    {
        cx.push(
            site,
            VerifyErrorKind::BadFunction(BadFunction::IfCondMustBeBool(cond_ty)),
        );
    }

    let mut then_state = state.clone();
    let then_fallthrough =
        verify_air_block(cx, function_id, &branch.then_block, &mut then_state, loops);
    let else_fallthrough = match &branch.else_block {
        Some(else_block) => {
            let mut else_state = state.clone();
            verify_air_block(cx, function_id, else_block, &mut else_state, loops)
        }
        None => Some(state.clone()),
    };
    LocalInit::join([then_fallthrough, else_fallthrough].into_iter().flatten())
}

fn verify_air_match(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    index: usize,
    match_: &AirEnumMatch,
    state: &LocalInit,
    loops: &mut Vec<LoopCtx>,
) -> Option<LocalInit> {
    let site = VerifyCx::stmt_site(function_id, BlockId::from_index(0), index);
    verify_air_place_read(cx, function_id, index, &match_.discr, state);
    let discr_ty = verify_place(
        cx,
        function_id,
        BlockId::from_index(0),
        Some(index),
        &match_.discr,
    );
    let Some(expected_enum) = discr_ty.and_then(|ty| match cx.type_data(ty) {
        Some(TypeData::Enum(id)) if cx.has_enum(*id) => Some(*id),
        Some(TypeData::Enum(_) | TypeData::Optional(_)) | None => None,
        Some(_) => {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::SwitchDiscriminantMustBeEnum(ty)),
            );
            None
        }
    }) else {
        return Some(state.clone());
    };

    let mut seen = std::collections::HashSet::new();
    let mut fallthrough = Vec::new();
    for arm in &match_.arms {
        if !seen.insert(arm.variant) {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::DuplicateSwitchArm(arm.variant)),
            );
        }
        if !cx.variant_belongs_to_enum(expected_enum, arm.variant) {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::SwitchArmVariantMismatch {
                    expected_enum,
                    variant: arm.variant,
                }),
            );
        }
        let mut arm_state = state.clone();
        if let Some(state) = verify_air_block(cx, function_id, &arm.block, &mut arm_state, loops) {
            fallthrough.push(state);
        }
    }
    if let Some(else_block) = &match_.else_block {
        let mut else_state = state.clone();
        if let Some(state) = verify_air_block(cx, function_id, else_block, &mut else_state, loops) {
            fallthrough.push(state);
        }
    } else if seen.len() < cx.program.enum_decl(expected_enum).variants.len() {
        cx.push(
            site,
            VerifyErrorKind::BadFunction(BadFunction::MatchNotExhaustive(expected_enum)),
        );
    }
    LocalInit::join(fallthrough)
}

fn verify_air_tail(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    tail: &AirTail,
    state: &LocalInit,
    loops: &mut [LoopCtx],
) -> Option<LocalInit> {
    let site = VerifyCx::term_site(function_id, BlockId::from_index(0));
    match tail {
        AirTail::None => Some(state.clone()),
        AirTail::Return(value) => {
            if let Some(value) = value {
                verify_air_operand_read(cx, function_id, 0, value, state);
            }
            let function = cx.program.function(function_id);
            verify_return(
                cx,
                function_id,
                BlockId::from_index(0),
                site,
                function.signature.return_mode,
                value.as_ref(),
            );
            None
        }
        AirTail::Break(id) => {
            if let Some(loop_ctx) = loops.iter_mut().rev().find(|loop_ctx| loop_ctx.id == *id) {
                loop_ctx.breaks.push(state.clone());
            } else {
                cx.push(
                    site,
                    VerifyErrorKind::BadFunction(BadFunction::BreakOutsideLoop(*id)),
                );
            }
            None
        }
        AirTail::Continue(id) => {
            if !loops.iter().any(|loop_ctx| loop_ctx.id == *id) {
                cx.push(
                    site,
                    VerifyErrorKind::BadFunction(BadFunction::ContinueOutsideLoop(*id)),
                );
            }
            None
        }
        AirTail::Unreachable => None,
    }
}

fn verify_air_rvalue_reads(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    index: usize,
    value: &RValue,
    state: &LocalInit,
) {
    match value {
        RValue::Use(op)
        | RValue::Stringify { value: op, .. }
        | RValue::Unary { value: op, .. }
        | RValue::Cast { value: op, .. }
        | RValue::Format { value: op, .. } => {
            verify_air_operand_read(cx, function_id, index, op, state);
        }
        RValue::Binary { lhs, rhs, .. } | RValue::SharedRefEq { lhs, rhs, .. } => {
            verify_air_operand_read(cx, function_id, index, lhs, state);
            verify_air_operand_read(cx, function_id, index, rhs, state);
        }
        RValue::Aggregate { fields, .. } | RValue::StringConcat { parts: fields } => {
            for field in fields {
                verify_air_operand_read(cx, function_id, index, field, state);
            }
        }
        RValue::Call { callee, args } => {
            if let Callee::Closure(op) = callee {
                verify_air_operand_read(cx, function_id, index, op, state);
            }
            for arg in args {
                match arg {
                    CallArg::Value(op) => {
                        verify_air_operand_read(cx, function_id, index, op, state);
                    }
                    CallArg::SharedBorrow(place) | CallArg::MutBorrow(place) => {
                        verify_air_place_read(cx, function_id, index, place, state);
                    }
                    CallArg::SharedStringConst(_) => {}
                }
            }
        }
        RValue::Len { source } | RValue::ListPop { list: source, .. } => {
            verify_air_place_read(cx, function_id, index, source, state);
        }
        RValue::ListSlice {
            source, start, end, ..
        }
        | RValue::SliceView {
            source, start, end, ..
        } => {
            verify_air_place_read(cx, function_id, index, source, state);
            verify_air_local_read(cx, function_id, index, *start, state);
            verify_air_local_read(cx, function_id, index, *end, state);
        }
        RValue::ListPush { list, value } => {
            verify_air_place_read(cx, function_id, index, list, state);
            verify_air_operand_read(cx, function_id, index, value, state);
        }
        RValue::MapGet { map, key, .. } | RValue::MapRemove { map, key, .. } => {
            verify_air_place_read(cx, function_id, index, map, state);
            verify_air_operand_read(cx, function_id, index, key, state);
        }
        RValue::MapEntryAt {
            map, index: key, ..
        } => {
            verify_air_place_read(cx, function_id, index, map, state);
            verify_air_local_read(cx, function_id, index, *key, state);
        }
        RValue::MapInsert { map, key, value } => {
            verify_air_place_read(cx, function_id, index, map, state);
            verify_air_operand_read(cx, function_id, index, key, state);
            verify_air_operand_read(cx, function_id, index, value, state);
        }
        RValue::MakeClosure { captures, .. } => {
            for capture in captures {
                verify_air_operand_read(cx, function_id, index, capture, state);
            }
        }
    }
}

fn verify_air_operand_read(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    index: usize,
    op: &Operand,
    state: &LocalInit,
) {
    if let Operand::Place(place) = op {
        verify_air_place_read(cx, function_id, index, place, state);
    }
}

fn verify_air_place_read(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    index: usize,
    place: &Place,
    state: &LocalInit,
) {
    verify_air_local_read(cx, function_id, index, place.root, state);
    for projection in &place.projection {
        if let Projection::Index(local) = projection {
            verify_air_local_read(cx, function_id, index, *local, state);
        }
    }
}

fn verify_air_local_read(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    index: usize,
    local: LocalId,
    state: &LocalInit,
) {
    if !state.is_definite(local) {
        cx.push(
            VerifyCx::stmt_site(function_id, BlockId::from_index(0), index),
            VerifyErrorKind::BadStatement(BadStatement::ReadUninitializedLocal(local)),
        );
    }
}

fn verify_init_stmt(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    index: usize,
    local: LocalId,
    value: &RValue,
) {
    let function = cx.program.function(function_id);
    let site = VerifyCx::stmt_site(function_id, block_id, index);
    if local.index() >= function.locals.len() {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadReference(BadReference::InvalidLocal(local)),
        );
    } else {
        let target = &function.locals[local.index()];
        if function
            .signature
            .params
            .iter()
            .any(|param| param.local_id == local)
        {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadStatement(BadStatement::InitParamLocal(local)),
            );
        }
        cx.verify_type_ref(site.clone(), target.ty);
        if let Some(value_ty) = typing::rvalue_ty(cx.program, &cx.primitives, value)
            && value_ty != target.ty
        {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadStatement(BadStatement::InitTypeMismatch {
                    expected: target.ty,
                    found: value_ty,
                }),
            );
        }
    }
    verify_rvalue(cx, function_id, block_id, Some(index), value);
}

fn verify_assign_stmt(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    index: usize,
    dst: &Place,
    value: &RValue,
) {
    let function = cx.program.function(function_id);
    let site = VerifyCx::stmt_site(function_id, block_id, index);
    let dst_ty = verify_place(cx, function_id, block_id, Some(index), dst);
    if function
        .locals
        .get(dst.root.index())
        .is_some_and(|local| local.mutability == Mutability::Immutable)
    {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadStatement(BadStatement::AssignImmutableLocal(dst.root)),
        );
    }
    verify_rvalue(cx, function_id, block_id, Some(index), value);
    if let (Some(expected), Some(found)) =
        (dst_ty, typing::rvalue_ty(cx.program, &cx.primitives, value))
        && expected != found
    {
        cx.push(
            site,
            VerifyErrorKind::BadStatement(BadStatement::AssignTypeMismatch { expected, found }),
        );
    }
}

fn verify_return(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    site: VerifySite,
    ret: ReturnMode,
    value: Option<&Operand>,
) {
    let ret_ty = ret.ty();
    let ret_is_void = cx.primitives.void() == Some(ret_ty);
    match value {
        None => {
            if !ret_is_void {
                cx.push(
                    site,
                    VerifyErrorKind::BadFunction(BadFunction::NonVoidFunctionMustReturnValue(
                        ret_ty,
                    )),
                );
            }
        }
        Some(op) => {
            if ret_is_void {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadFunction(BadFunction::VoidFunctionMustReturnNone),
                );
            }
            if matches!(ret, ReturnMode::Place(_)) && !matches!(op, Operand::Place(_)) {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadFunction(BadFunction::PlaceReturnMustReturnPlace),
                );
            }
            verify_operand(cx, function_id, block_id, None, op);
            if let Some(op_ty) = operand_ty(cx, op)
                && op_ty != ret_ty
            {
                cx.push(
                    site,
                    VerifyErrorKind::BadFunction(BadFunction::ReturnedTypeMismatch {
                        expected: ret_ty,
                        found: op_ty,
                    }),
                );
            }
        }
    }
}

fn verify_stringify(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    stmt_index: Option<usize>,
    site: VerifySite,
    value: &Operand,
    source_ty: TypeId,
) {
    required_rvalue_primitive(cx, site.clone(), PrimitiveKind::String);
    cx.verify_type_ref(site.clone(), source_ty);
    match cx.type_data(source_ty) {
        Some(TypeData::Any) => cx.push(
            site.clone(),
            VerifyErrorKind::BadRValue(BadRValue::StringifyAnySource { source: source_ty }),
        ),
        Some(TypeData::Void) => cx.push(
            site.clone(),
            VerifyErrorKind::BadRValue(BadRValue::StringifyVoidSource { source: source_ty }),
        ),
        _ => {}
    }
    verify_operand(cx, function_id, block_id, stmt_index, value);
    if let Some(operand) = operand_ty(cx, value)
        && operand != source_ty
    {
        cx.push(
            site,
            VerifyErrorKind::BadRValue(BadRValue::StringifyOperandTypeMismatch {
                operand,
                source: source_ty,
            }),
        );
    }
}

fn verify_rvalue(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    stmt_index: Option<usize>,
    value: &RValue,
) {
    let stmt_idx = stmt_index.unwrap_or(0);
    let site = VerifyCx::stmt_site(function_id, block_id, stmt_idx);

    match value {
        RValue::Use(op) => {
            verify_operand(cx, function_id, block_id, stmt_index, op);
        }
        RValue::Stringify { value, source_ty } => {
            verify_stringify(
                cx,
                function_id,
                block_id,
                stmt_index,
                site,
                value,
                *source_ty,
            );
        }
        RValue::Unary {
            op: unary,
            value: op,
            ty,
        } => {
            verify_operand(cx, function_id, block_id, stmt_index, op);
            cx.verify_type_ref(site.clone(), *ty);
            if let Some(value_ty) = operand_ty(cx, op) {
                let valid = match (cx.primitives.scalar(value_ty), cx.primitives.scalar(*ty)) {
                    (Some(value), Some(result)) => supports_scalar_unary(*unary, value, result),
                    _ => false,
                };
                if !valid {
                    cx.push(
                        site,
                        VerifyErrorKind::BadRValue(BadRValue::UnaryTypeMismatch {
                            value: value_ty,
                            result: *ty,
                        }),
                    );
                }
            }
        }
        RValue::Binary { op, lhs, rhs, ty } => {
            verify_operand(cx, function_id, block_id, stmt_index, lhs);
            verify_operand(cx, function_id, block_id, stmt_index, rhs);
            cx.verify_type_ref(site.clone(), *ty);
            if matches!(
                op,
                crate::ast::BinaryOp::And
                    | crate::ast::BinaryOp::Or
                    | crate::ast::BinaryOp::Coalesce
            ) {
                cx.push(
                    site,
                    VerifyErrorKind::BadRValue(BadRValue::UnsupportedBinaryOp(*op)),
                );
            } else if let (Some(lhs_ty), Some(rhs_ty)) = (operand_ty(cx, lhs), operand_ty(cx, rhs))
            {
                let valid = match (
                    cx.primitives.scalar(lhs_ty),
                    cx.primitives.scalar(rhs_ty),
                    cx.primitives.scalar(*ty),
                ) {
                    (Some(lhs), Some(rhs), Some(result)) => {
                        supports_scalar_binary(*op, lhs, rhs, result)
                    }
                    _ => false,
                };
                if !valid {
                    cx.push(
                        site,
                        VerifyErrorKind::BadRValue(BadRValue::BinaryTypeMismatch {
                            lhs: lhs_ty,
                            rhs: rhs_ty,
                            result: *ty,
                        }),
                    );
                }
            }
        }
        RValue::SharedRefEq { lhs, rhs, .. } => {
            verify_operand(cx, function_id, block_id, stmt_index, lhs);
            verify_operand(cx, function_id, block_id, stmt_index, rhs);
            required_rvalue_primitive(cx, site, PrimitiveKind::Bool);
        }
        RValue::Cast { value: op, target } => {
            verify_operand(cx, function_id, block_id, stmt_index, op);
            cx.verify_type_ref(site.clone(), *target);
            if let Some(value_ty) = operand_ty(cx, op) {
                let valid = (cx.primitives.is_int(value_ty) && cx.primitives.is_float(*target))
                    || (cx.primitives.is_float(value_ty) && cx.primitives.is_int(*target));
                if !valid {
                    cx.push(
                        site,
                        VerifyErrorKind::BadRValue(BadRValue::CastMustConvertIntAndFloat {
                            value: value_ty,
                            target: *target,
                        }),
                    );
                }
            }
        }
        RValue::Aggregate { kind, fields, ty } => {
            cx.verify_type_ref(site.clone(), *ty);
            match kind {
                AggregateCtor::Struct(id) => {
                    verify_aggregate_ctor(
                        cx,
                        site.clone(),
                        *id,
                        AggregateKind::Struct,
                        *ty,
                        fields,
                    );
                }
                AggregateCtor::DataRef(id) => verify_aggregate_ctor(
                    cx,
                    site.clone(),
                    *id,
                    AggregateKind::DataRef,
                    *ty,
                    fields,
                ),
                AggregateCtor::EnumVariant { enum_id, variant } => {
                    verify_enum_ctor(cx, site.clone(), *enum_id, *variant, *ty, fields);
                }
                AggregateCtor::Tuple
                | AggregateCtor::List
                | AggregateCtor::Array
                | AggregateCtor::Map => {}
            }
            for field in fields {
                verify_operand(cx, function_id, block_id, stmt_index, field);
            }
        }
        RValue::Call { callee, args } => {
            verify_call(cx, function_id, block_id, stmt_index, callee, args);
        }
        RValue::StringConcat { parts } => {
            required_rvalue_primitive(cx, site.clone(), PrimitiveKind::String);
            for part in parts {
                verify_operand(cx, function_id, block_id, stmt_index, part);
                if let Some(ty) = operand_ty(cx, part)
                    && !matches!(cx.type_data(ty), Some(TypeData::String))
                {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadRValue(BadRValue::StringConcatPartMustBeString(ty)),
                    );
                }
            }
        }
        RValue::Format { value, .. } => {
            verify_operand(cx, function_id, block_id, stmt_index, value);
            required_rvalue_primitive(cx, site, PrimitiveKind::String);
        }
        RValue::Len { source } => {
            required_rvalue_primitive(cx, site.clone(), PrimitiveKind::Int);
            if let Some(source_ty) = verify_place(cx, function_id, block_id, stmt_index, source)
                && !matches!(
                    cx.type_data(source_ty),
                    Some(
                        TypeData::String
                            | TypeData::List(_)
                            | TypeData::Array { .. }
                            | TypeData::Map { .. },
                    )
                )
            {
                cx.push(
                    site,
                    VerifyErrorKind::BadFunction(BadFunction::LenSourceMustBeCountable(source_ty)),
                );
            }
        }
        RValue::ListPush { list, value } => {
            required_rvalue_primitive(cx, site.clone(), PrimitiveKind::Void);
            verify_place(cx, function_id, block_id, stmt_index, list);
            verify_operand(cx, function_id, block_id, stmt_index, value);
            if let Some(expected_elem) = list_elem_ty(cx, list)
                && let Some(value_ty) = operand_ty(cx, value)
                && value_ty != expected_elem
            {
                cx.push(
                    site,
                    VerifyErrorKind::BadFunction(BadFunction::ListElementTypeMismatch {
                        expected: expected_elem,
                        found: value_ty,
                    }),
                );
            }
        }
        RValue::ListPop { list, ty } => {
            verify_place(cx, function_id, block_id, stmt_index, list);
            cx.verify_type_ref(site.clone(), *ty);
            if let Some(expected_elem) = list_elem_ty(cx, list) {
                let valid = matches!(cx.type_data(*ty), Some(TypeData::Optional(inner)) if *inner == expected_elem);
                if !valid {
                    cx.push(
                        site,
                        VerifyErrorKind::BadFunction(
                            BadFunction::ListPopResultMustBeOptionalElement {
                                expected_elem,
                                found: *ty,
                            },
                        ),
                    );
                }
            }
        }
        RValue::ListSlice {
            source,
            start,
            end,
            ty,
            ..
        } => {
            verify_place(cx, function_id, block_id, stmt_index, source);
            cx.verify_type_ref(site.clone(), *ty);
            if let Some(expected_elem) = list_elem_ty(cx, source) {
                let valid = matches!(cx.type_data(*ty), Some(TypeData::List(inner)) if *inner == expected_elem);
                if !valid {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadFunction(BadFunction::ListSliceResultMustBeList {
                            found: *ty,
                        }),
                    );
                }
            }
            verify_slice_index(cx, function_id, block_id, stmt_idx, "start", *start);
            verify_slice_index(cx, function_id, block_id, stmt_idx, "end", *end);
        }
        RValue::MapGet { map, key, ty } | RValue::MapRemove { map, key, ty } => {
            verify_place(cx, function_id, block_id, stmt_index, map);
            verify_operand(cx, function_id, block_id, stmt_index, key);
            cx.verify_type_ref(site.clone(), *ty);
            if let Some((expected_key, expected_value)) = map_kv(cx, map) {
                verify_map_key(cx, &site, key, expected_key);
                verify_optional_map_value(cx, &site, *ty, expected_value);
            }
        }
        RValue::MapInsert { map, key, value } => {
            required_rvalue_primitive(cx, site.clone(), PrimitiveKind::Void);
            verify_place(cx, function_id, block_id, stmt_index, map);
            verify_operand(cx, function_id, block_id, stmt_index, key);
            verify_operand(cx, function_id, block_id, stmt_index, value);
            if let Some((expected_key, expected_value)) = map_kv(cx, map) {
                verify_map_key(cx, &site, key, expected_key);
                verify_map_value(cx, &site, value, expected_value);
            }
        }
        RValue::MapEntryAt { map, index, ty } => {
            verify_place(cx, function_id, block_id, stmt_index, map);
            verify_slice_index(cx, function_id, block_id, stmt_idx, "index", *index);
            cx.verify_type_ref(site, *ty);
        }
        RValue::SliceView {
            source,
            start,
            end,
            ty,
            inclusive: _,
        } => {
            verify_place(cx, function_id, block_id, stmt_index, source);
            verify_slice_index(cx, function_id, block_id, stmt_idx, "start", *start);
            verify_slice_index(cx, function_id, block_id, stmt_idx, "end", *end);
            cx.verify_type_ref(site, *ty);
        }
        RValue::MakeClosure { func, captures, ty } => {
            if !cx.has_function(*func) {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadReference(BadReference::InvalidFunction(*func)),
                );
            }
            for cap in captures {
                verify_operand(cx, function_id, block_id, stmt_index, cap);
            }
            cx.verify_type_ref(site, *ty);
        }
    }
}

fn verify_map_key(cx: &mut VerifyCx<'_>, site: &VerifySite, key: &Operand, expected: TypeId) {
    if let Some(found) = operand_ty(cx, key)
        && found != expected
    {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadFunction(BadFunction::MapKeyTypeMismatch { expected, found }),
        );
    }
}

fn verify_map_value(cx: &mut VerifyCx<'_>, site: &VerifySite, value: &Operand, expected: TypeId) {
    if let Some(found) = operand_ty(cx, value)
        && found != expected
    {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadFunction(BadFunction::MapValueTypeMismatch { expected, found }),
        );
    }
}

fn verify_optional_map_value(
    cx: &mut VerifyCx<'_>,
    site: &VerifySite,
    found: TypeId,
    expected_value: TypeId,
) {
    let valid =
        matches!(cx.type_data(found), Some(TypeData::Optional(inner)) if *inner == expected_value);
    if !valid {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadFunction(BadFunction::MapGetResultMustBeOptionalValue {
                expected_value,
                found,
            }),
        );
    }
}

fn verify_operand(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    stmt_index: Option<usize>,
    op: &Operand,
) {
    let site = VerifyCx::stmt_site(function_id, block_id, stmt_index.unwrap_or(0));
    match op {
        Operand::Place(place) => {
            verify_place(cx, function_id, block_id, stmt_index, place);
        }
        Operand::Const(id) => {
            if !cx.has_const(*id) {
                cx.push(
                    site,
                    VerifyErrorKind::BadReference(BadReference::InvalidConst(*id)),
                );
            }
        }
    }
}

fn verify_place(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    stmt_index: Option<usize>,
    place: &Place,
) -> Option<TypeId> {
    let function = cx.program.function(function_id);
    let stmt_idx = stmt_index.unwrap_or(0);
    let site = VerifyCx::stmt_site(function_id, block_id, stmt_idx);

    if place.root.index() >= function.locals.len() {
        cx.push(
            site,
            VerifyErrorKind::BadReference(BadReference::InvalidLocal(place.root)),
        );
        return None;
    }

    let mut current_ty = function.locals[place.root.index()].ty;
    if !cx.has_type(current_ty) {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadReference(BadReference::InvalidType(current_ty)),
        );
        return None;
    }
    if !cx.has_type(place.ty) {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadReference(BadReference::InvalidType(place.ty)),
        );
        return None;
    }

    for proj in &place.projection {
        let Some(data) = cx.type_data(current_ty) else {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadReference(BadReference::InvalidType(current_ty)),
            );
            return None;
        };
        match proj {
            Projection::Field(field_id) => match data {
                TypeData::Aggregate(agg_id) | TypeData::DataRef(agg_id) => {
                    let Some(agg) = cx.program.aggregates.get(agg_id.index()) else {
                        cx.push(
                            site.clone(),
                            VerifyErrorKind::BadReference(BadReference::InvalidAggregate(*agg_id)),
                        );
                        return None;
                    };
                    let expected = match data {
                        TypeData::Aggregate(_) => AggregateKind::Struct,
                        TypeData::DataRef(_) => AggregateKind::DataRef,
                        _ => unreachable!(),
                    };
                    if agg.kind != expected {
                        cx.push(
                            site.clone(),
                            VerifyErrorKind::BadPlace(BadPlace::FieldProjectionKindMismatch {
                                aggregate: *agg_id,
                                expected,
                                found: agg.kind,
                            }),
                        );
                        return None;
                    }
                    let Some(field) = agg.fields.get(field_id.index()) else {
                        cx.push(
                            site.clone(),
                            VerifyErrorKind::BadReference(BadReference::InvalidField {
                                aggregate: *agg_id,
                                field: *field_id,
                            }),
                        );
                        return None;
                    };
                    current_ty = field.ty;
                }
                _ => {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadPlace(BadPlace::FieldProjectionOnNonAggregate(
                            current_ty,
                        )),
                    );
                    return None;
                }
            },
            Projection::TupleField(index) => match data {
                TypeData::Tuple(elems) => {
                    let Some(ty) = elems.get(*index as usize) else {
                        cx.push(
                            site.clone(),
                            VerifyErrorKind::BadPlace(BadPlace::TupleFieldOutOfRange {
                                ty: current_ty,
                                index: *index,
                                len: elems.len(),
                            }),
                        );
                        return None;
                    };
                    current_ty = *ty;
                }
                _ => {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadPlace(BadPlace::TupleProjectionOnNonTuple(current_ty)),
                    );
                    return None;
                }
            },
            Projection::VariantField {
                enum_id,
                variant,
                field,
            } => match data {
                TypeData::Enum(eid) if *eid == *enum_id => {
                    let Some(enm) = cx.program.enums.get(enum_id.index()) else {
                        cx.push(
                            site.clone(),
                            VerifyErrorKind::BadReference(BadReference::InvalidEnum(*enum_id)),
                        );
                        return None;
                    };
                    let Some(shape) = enm.variants.get(variant.index()).map(|decl| &decl.shape)
                    else {
                        cx.push(
                            site.clone(),
                            VerifyErrorKind::BadReference(BadReference::InvalidVariant {
                                enum_id: *enum_id,
                                variant: *variant,
                            }),
                        );
                        return None;
                    };
                    let Some(ty) = variant_field_ty(shape, *field as usize) else {
                        cx.push(
                            site.clone(),
                            VerifyErrorKind::BadPlace(BadPlace::VariantFieldOutOfRange {
                                ty: current_ty,
                                index: *field,
                                len: variant_field_count(shape),
                            }),
                        );
                        return None;
                    };
                    current_ty = ty;
                }
                _ => {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadPlace(BadPlace::VariantProjectionOnNonEnum(current_ty)),
                    );
                    return None;
                }
            },
            Projection::Index(local) => {
                let Some(index_local) = function.locals.get(local.index()) else {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadReference(BadReference::InvalidLocal(*local)),
                    );
                    return None;
                };
                match data {
                    TypeData::List(elem) | TypeData::Array { elem, .. } => {
                        let Some(int_ty) = cx.primitives.int() else {
                            cx.push(
                                site.clone(),
                                VerifyErrorKind::BadFunction(BadFunction::IndexTypeUnavailable),
                            );
                            return None;
                        };
                        if index_local.ty != int_ty {
                            cx.push(
                                site.clone(),
                                VerifyErrorKind::BadPlace(BadPlace::IndexLocalTypeMismatch {
                                    expected: int_ty,
                                    found: index_local.ty,
                                }),
                            );
                            return None;
                        }
                        current_ty = *elem;
                    }
                    TypeData::Map { key, value, .. } => {
                        if index_local.ty != *key {
                            cx.push(
                                site.clone(),
                                VerifyErrorKind::BadPlace(BadPlace::IndexLocalTypeMismatch {
                                    expected: *key,
                                    found: index_local.ty,
                                }),
                            );
                            return None;
                        }
                        current_ty = *value;
                    }
                    _ => {
                        cx.push(
                            site.clone(),
                            VerifyErrorKind::BadPlace(BadPlace::IndexProjectionOnNonIndexable(
                                current_ty,
                            )),
                        );
                        return None;
                    }
                }
            }
        }
    }

    if current_ty != place.ty {
        cx.push(
            site,
            VerifyErrorKind::BadPlace(BadPlace::PlaceTypeMismatch {
                expected: current_ty,
                found: place.ty,
            }),
        );
        return None;
    }

    Some(current_ty)
}

fn verify_aggregate_ctor(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    aggregate_id: AggregateId,
    expected_kind: AggregateKind,
    ty: TypeId,
    fields: &[Operand],
) {
    let Some(aggregate) = cx.program.aggregates.get(aggregate_id.index()) else {
        cx.push(
            site,
            VerifyErrorKind::BadReference(BadReference::InvalidAggregate(aggregate_id)),
        );
        return;
    };

    let expected_ty = match expected_kind {
        AggregateKind::Struct => TypeData::Aggregate(aggregate_id),
        AggregateKind::DataRef => TypeData::DataRef(aggregate_id),
    };
    if cx.type_data(ty) != Some(&expected_ty) {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadRValue(BadRValue::AggregateCtorResultTypeMismatch {
                aggregate: aggregate_id,
                expected: expected_kind,
                found: ty,
            }),
        );
    }
    if aggregate.kind != expected_kind {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadRValue(BadRValue::AggregateCtorKindMismatch {
                aggregate: aggregate_id,
                expected: expected_kind,
                found: aggregate.kind,
            }),
        );
    }
    if fields.len() != aggregate.fields.len() {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadRValue(BadRValue::AggregateCtorFieldCountMismatch {
                aggregate: aggregate_id,
                expected: aggregate.fields.len(),
                found: fields.len(),
            }),
        );
    }
    for (index, (operand, field)) in fields.iter().zip(&aggregate.fields).enumerate() {
        if let Some(found) = operand_ty(cx, operand)
            && found != field.ty
        {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadRValue(BadRValue::AggregateCtorFieldTypeMismatch {
                    aggregate: aggregate_id,
                    field: index,
                    expected: field.ty,
                    found,
                }),
            );
        }
    }
}

fn verify_enum_ctor(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    enum_id: EnumId,
    variant: VariantId,
    ty: TypeId,
    fields: &[Operand],
) {
    let Some(enm) = cx.program.enums.get(enum_id.index()) else {
        cx.push(
            site,
            VerifyErrorKind::BadReference(BadReference::InvalidEnum(enum_id)),
        );
        return;
    };
    let Some(variant_decl) = enm.variants.get(variant.index()) else {
        cx.push(
            site,
            VerifyErrorKind::BadReference(BadReference::InvalidVariant { enum_id, variant }),
        );
        return;
    };
    if cx.type_data(ty) != Some(&TypeData::Enum(enum_id)) {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadRValue(BadRValue::EnumCtorResultTypeMismatch {
                enum_id,
                found: ty,
            }),
        );
    }
    let expected_len = variant_field_count(&variant_decl.shape);
    if fields.len() != expected_len {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadRValue(BadRValue::EnumCtorFieldCountMismatch {
                enum_id,
                variant,
                expected: expected_len,
                found: fields.len(),
            }),
        );
    }
    for (index, operand) in fields.iter().enumerate() {
        let Some(expected_ty) = variant_field_ty(&variant_decl.shape, index) else {
            break;
        };
        if let Some(found) = operand_ty(cx, operand)
            && found != expected_ty
        {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadRValue(BadRValue::EnumCtorFieldTypeMismatch {
                    enum_id,
                    variant,
                    field: index,
                    expected: expected_ty,
                    found,
                }),
            );
        }
    }
}

fn variant_field_count(shape: &VariantShape) -> usize {
    match shape {
        VariantShape::Unit => 0,
        VariantShape::Tuple(fields) => fields.len(),
        VariantShape::Struct(fields) => fields.len(),
    }
}

fn variant_field_ty(shape: &VariantShape, index: usize) -> Option<TypeId> {
    match shape {
        VariantShape::Unit => None,
        VariantShape::Tuple(fields) => fields.get(index).copied(),
        VariantShape::Struct(fields) => fields.get(index).map(|field| field.ty),
    }
}

fn verify_call(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    stmt_index: Option<usize>,
    callee: &Callee,
    args: &[CallArg],
) {
    let site = VerifyCx::stmt_site(function_id, block_id, stmt_index.unwrap_or(0));

    if let Callee::Closure(op) = callee {
        verify_operand(cx, function_id, block_id, stmt_index, op);
    }
    for (arg_index, arg) in args.iter().enumerate() {
        verify_call_arg(cx, function_id, block_id, stmt_index, arg_index, arg);
    }

    match callee {
        Callee::Function(id) => {
            if !cx.has_function(*id) {
                cx.push(
                    site,
                    VerifyErrorKind::BadReference(BadReference::InvalidFunction(*id)),
                );
                return;
            }
            let params: Vec<ParamType> = cx
                .program
                .function(*id)
                .signature
                .params
                .iter()
                .map(|p| ParamType {
                    ty: p.ty,
                    mode: p.mode,
                })
                .collect();
            verify_call_args(cx, &site, args, &params);
        }
        Callee::Extern(id) => {
            if !cx.has_extern(*id) {
                cx.push(
                    site,
                    VerifyErrorKind::BadReference(BadReference::InvalidExtern(*id)),
                );
                return;
            }
            let params: Vec<ParamType> = cx
                .program
                .extern_decl(*id)
                .params
                .iter()
                .map(|p| ParamType {
                    ty: p.ty,
                    mode: p.mode,
                })
                .collect();
            verify_call_args(cx, &site, args, &params);
        }
        Callee::Closure(op) => {
            if let Some(ty) = operand_ty(cx, op) {
                match cx.type_data(ty).cloned() {
                    Some(TypeData::Function(sig)) => verify_call_args(cx, &site, args, &sig.params),
                    _ => cx.push(
                        site,
                        VerifyErrorKind::BadCall(BadCall::ClosureCalleeMustBeFunction),
                    ),
                }
            }
        }
    }
}

fn verify_call_arg(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    stmt_index: Option<usize>,
    arg_index: usize,
    arg: &CallArg,
) {
    match arg {
        CallArg::Value(op) => verify_operand(cx, function_id, block_id, stmt_index, op),
        CallArg::SharedBorrow(place) | CallArg::MutBorrow(place) => {
            verify_place(cx, function_id, block_id, stmt_index, place);
        }
        CallArg::SharedStringConst(id) => {
            let site = VerifyCx::stmt_site(function_id, block_id, stmt_index.unwrap_or(0));
            let Some(konst) = cx.program.const_arena.get_checked(*id) else {
                cx.push(
                    site,
                    VerifyErrorKind::BadReference(BadReference::InvalidConst(*id)),
                );
                return;
            };
            if !matches!(cx.program.type_arena.data(konst.ty), TypeData::String)
                || !matches!(konst.value, ConstValue::String(_))
            {
                cx.push(
                    site,
                    VerifyErrorKind::BadCall(BadCall::ArgTypeMismatch {
                        index: arg_index,
                        expected: konst.ty,
                        found: konst.ty,
                    }),
                );
            }
        }
    }
}

fn verify_call_args(
    cx: &mut VerifyCx<'_>,
    site: &VerifySite,
    args: &[CallArg],
    expected: &[ParamType],
) {
    if args.len() != expected.len() {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadCall(BadCall::ArityMismatch {
                expected: expected.len(),
                found: args.len(),
            }),
        );
    }
    for first in 0..args.len() {
        for second in first + 1..args.len() {
            if call_args_conflict(&args[first], &args[second]) {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadCall(BadCall::ArgAliasConflict { first, second }),
                );
            }
        }
    }
    for (i, (arg, expected_param)) in args.iter().zip(expected.iter()).enumerate() {
        if arg.mode() != expected_param.mode {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadCall(BadCall::ArgModeMismatch {
                    index: i,
                    expected: expected_param.mode,
                    found: arg.mode(),
                }),
            );
        }
        if let Some(found_ty) = typing::call_arg_ty(cx.program, arg)
            && found_ty != expected_param.ty
        {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadCall(BadCall::ArgTypeMismatch {
                    index: i,
                    expected: expected_param.ty,
                    found: found_ty,
                }),
            );
        }
    }
}

fn call_args_conflict(left: &CallArg, right: &CallArg) -> bool {
    let borrow_conflict = matches!(
        (left.mode(), right.mode()),
        (ParamMode::SharedBorrow, ParamMode::MutBorrow)
            | (
                ParamMode::MutBorrow,
                ParamMode::SharedBorrow | ParamMode::MutBorrow
            )
    );
    borrow_conflict
        && left
            .place()
            .zip(right.place())
            .is_some_and(|(left, right)| left.may_overlap(right))
}

fn verify_slice_index(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    stmt_idx: usize,
    which: &'static str,
    local: LocalId,
) {
    let site = VerifyCx::stmt_site(function_id, block_id, stmt_idx);
    let Some(local) = cx.program.function(function_id).locals.get(local.index()) else {
        cx.push(
            site,
            VerifyErrorKind::BadReference(BadReference::InvalidLocal(local)),
        );
        return;
    };
    if !cx.primitives.is_int(local.ty) {
        cx.push(
            site,
            VerifyErrorKind::BadFunction(BadFunction::SliceIndexMustBeInt {
                which,
                found: local.ty,
            }),
        );
    }
}

fn verify_type(cx: &mut VerifyCx<'_>, id: TypeId) {
    let Some(data) = cx.type_data(id).cloned() else {
        return;
    };
    let site = VerifySite::Type(id);
    match data {
        TypeData::Int
        | TypeData::Float
        | TypeData::Bool
        | TypeData::String
        | TypeData::Void
        | TypeData::Any => {}
        TypeData::Optional(inner) | TypeData::List(inner) | TypeData::Slice(inner) => {
            cx.verify_type_ref(site, inner);
        }
        TypeData::Tuple(items) => {
            for item in items {
                cx.verify_type_ref(site.clone(), item);
            }
        }
        TypeData::Array { elem, .. } => {
            cx.verify_type_ref(site, elem);
        }
        TypeData::Map { key, value, .. } => {
            cx.verify_type_ref(site.clone(), key);
            cx.verify_type_ref(site, value);
        }
        TypeData::Function(sig) => {
            for param in &sig.params {
                cx.verify_type_ref(site.clone(), param.ty);
            }
            cx.verify_type_ref(site, sig.ret.ty());
        }
        TypeData::Dyn(contract) => {
            if contract.display_name.is_empty() || contract.method_table_key.is_empty() {
                cx.push(site, VerifyErrorKind::BadType(BadType::EmptyDynContract));
            }
        }
        TypeData::Aggregate(agg_id) | TypeData::DataRef(agg_id) => {
            if !cx.has_aggregate(agg_id) {
                cx.push(
                    VerifySite::Type(id),
                    VerifyErrorKind::BadReference(BadReference::InvalidAggregate(agg_id)),
                );
            }
        }
        TypeData::Enum(enum_id) => {
            if !cx.has_enum(enum_id) {
                cx.push(
                    VerifySite::Type(id),
                    VerifyErrorKind::BadReference(BadReference::InvalidEnum(enum_id)),
                );
            }
        }
        TypeData::Extern(ext_id) => {
            if !cx.has_extern_type(ext_id) {
                cx.push(
                    VerifySite::Type(id),
                    VerifyErrorKind::BadReference(BadReference::InvalidExternType(ext_id)),
                );
            }
        }
    }
}

fn operand_ty(cx: &VerifyCx<'_>, op: &Operand) -> Option<TypeId> {
    typing::operand_ty(cx.program, op)
}

fn list_elem_ty(cx: &VerifyCx<'_>, place: &Place) -> Option<TypeId> {
    typing::list_elem_ty(cx.program, place.ty)
}

fn map_kv(cx: &VerifyCx<'_>, place: &Place) -> Option<(TypeId, TypeId)> {
    typing::map_kv(cx.program, place.ty)
}
