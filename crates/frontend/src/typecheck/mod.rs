use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

pub(crate) use self::{
    annotation::DeprecatedUseKind,
    decls::*,
    generic::*,
    result::*,
    semantic_use::*,
    surface::*,
    type_ops::type_closure_facts,
    type_refs::{GenericTypeContext, TypeRefError},
};
use self::{
    body::{
        CallableBody, CallableTemplate, CallableTemplateEnv, check_block_checked,
        check_block_checked_with_hint, check_module_bodies, check_stmts,
        collect_callable_templates, push_source_scope, register_declarations,
    },
    closure::{ClosureClassifier, ClosureScopeState},
    const_term::ConstTerm,
    decl_validate::{
        check_decl_param_order, check_finite_size_cycles, check_infer_return_decls,
        generic_param_type_error, validate_public_value_surfaces,
    },
    dyn_infer::DynInference,
    infer::{
        LocalTypeId, Solver, SolverFinalizeError, SolverRelationError, SourceExprTypes, TypeHandle,
    },
    literal::{
        check_array_fill_hint, check_array_lit_hint, check_inferred_enum_hint, check_map_lit_hint,
        check_range_expr, check_string_interp, check_struct_lit_hint,
        check_tuple_checked_with_hint, type_from_lit,
    },
    pattern::{
        PatternBindMode, PatternContext, PatternRoot, PatternRootInput, check_pattern_scrutinee,
        mode_for_head,
    },
    place::{AliasAltGroupId, PlaceAccess, PlaceIdentity, PlaceRoot, PlaceUseFacts, check_place},
    postfix::{
        PostfixStep, check_index_expr, check_postfix_chain, check_tuple_index,
        collect_postfix_chain,
    },
    type_ops::type_contains_dyn_value,
    type_refs::{LocalTypeScopes, TypeRefResolver},
};
use crate::{
    ast::*,
    config::{CompilationContext, PredicateError},
    externs::{
        RawExterns,
        catalog::{
            ExternCatalog, ExternCatalogError, ExternField, ExternFieldRef, ExternType,
            ExternTypeId,
        },
    },
    lint::LintEvent,
    resolve::ResolveResult,
    source::SourceId,
    span::{SourceSpan, Span},
};

mod annotation;
mod body;
mod closure;
mod const_eval;
mod const_term;
mod contracts;
mod control_flow;
mod convert;
mod decl_validate;
mod decls;
mod dyn_infer;
mod enum_variant;
mod extern_boundary;
mod extern_ops;
mod field_check;
mod generic;
mod generic_bind;
mod globals;
mod infer;
mod literal;
mod match_coverage;
mod member;
mod pattern;
mod place;
mod postfix;
mod result;
mod semantic_use;
mod surface;
mod type_ops;
mod type_refs;

#[cfg(test)]
mod tests;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ConstDiagnostic {
    Value(ConstValue),
    Name(Ident),
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MemberAccessKind {
    Field,
    Method,
}

impl MemberAccessKind {
    pub(crate) fn diagnostic_name(self) -> &'static str {
        match self {
            Self::Field => "field",
            Self::Method => "method",
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct TypecheckConfig {
    pub(crate) context: CompilationContext,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum VariantShape {
    Unit,
    Tuple,
    Struct,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum GenericParamKind {
    Type,
    Const,
}

impl GenericParamKind {
    pub(crate) fn keyword(self) -> &'static str {
        match self {
            Self::Type => "type",
            Self::Const => "const",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct CompileWarning {
    pub(crate) message: String,
    pub(crate) span: SourceSpan,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TryCarrierKind {
    Result,
    Option,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum DynContainerConversionKind {
    Collection,
    FixedArray,
    Slice,
    DynamicWeakening,
    MapValue,
}

impl TryCarrierKind {
    pub(crate) fn label(self) -> &'static str {
        match self {
            Self::Result => "Result",
            Self::Option => "Option",
        }
    }

    pub(crate) fn any_label() -> &'static str {
        "Result or Option"
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TypeError {
    Decl(DeclError),
    ExternCatalog(ExternCatalogError),
    UndefinedVariable {
        name: Ident,
        span: Option<SourceSpan>,
    },
    TypeMismatch {
        expected: Type,
        found: Type,
        span: Option<SourceSpan>,
    },
    ConstMismatch {
        expected: ConstDiagnostic,
        found: ConstDiagnostic,
        span: Option<SourceSpan>,
    },
    RecursiveInference {
        span: Option<SourceSpan>,
    },
    CannotInferType {
        span: Option<SourceSpan>,
    },
    CannotInferEnum {
        span: Option<SourceSpan>,
    },
    NamedFunctionCapture {
        name: Ident,
        span: Option<SourceSpan>,
    },
    AllNilArrayLiteral {
        span: Option<SourceSpan>,
    },
    ArrayFillLengthNotConst {
        span: Option<SourceSpan>,
    },
    InferReturnNonGeneric {
        span: Option<SourceSpan>,
    },
    InferReturnExtern {
        span: Option<SourceSpan>,
    },
    InferReturnValue {
        span: Option<SourceSpan>,
    },
    InferReturnMismatch {
        expected: Type,
        found: Type,
        span: Option<SourceSpan>,
    },
    InferReturnRecursive {
        span: Option<SourceSpan>,
    },
    UnsupportedPlaceReturn {
        message: &'static str,
        span: Option<SourceSpan>,
    },
    UnknownType {
        qualifier: Option<Ident>,
        name: Ident,
        span: Option<SourceSpan>,
    },
    TypeUsedAsValue {
        ty: Type,
        span: Option<SourceSpan>,
    },
    CannotInferConst {
        span: Option<SourceSpan>,
    },
    NotCallable {
        ty: Type,
        span: Option<SourceSpan>,
    },
    WrongArgCount {
        expected: usize,
        found: usize,
        span: Option<SourceSpan>,
    },
    WrongArgRange {
        min: usize,
        max: usize,
        found: usize,
        span: Option<SourceSpan>,
    },
    LambdaParamCountMismatch {
        expected: usize,
        found: usize,
        span: Option<SourceSpan>,
    },
    RequiredParamAfterDefault {
        name: Ident,
        span: Option<SourceSpan>,
    },
    EnumVariantArgCount {
        enum_name: Ident,
        variant: Ident,
        expected: usize,
        found: usize,
        span: Option<SourceSpan>,
    },
    DuplicateName {
        name: Ident,
        span: Option<SourceSpan>,
    },
    ImmutableAssignment {
        name: Ident,
        span: Option<SourceSpan>,
    },
    ConstAssignment {
        name: Ident,
        span: Option<SourceSpan>,
    },
    VarArgNonLvalue {
        span: Option<SourceSpan>,
    },
    VarArgImmutableBinding {
        name: Ident,
        span: Option<SourceSpan>,
    },
    MutatingMethodImmutableReceiver {
        name: Ident,
        span: Option<SourceSpan>,
    },
    MutableAlias {
        span: Option<SourceSpan>,
    },
    InvalidFormatSpec {
        reason: &'static str,
        span: Option<SourceSpan>,
    },
    NonEscapingCallbackEscapes {
        name: Ident,
        help: Option<String>,
        span: Option<SourceSpan>,
    },
    BorrowedCaptureEscapes {
        name: Ident,
        origin: CaptureStorageOrigin,
        span: Option<SourceSpan>,
    },
    RequiresMutablePlace {
        name: Ident,
        span: Option<SourceSpan>,
    },
    VarPatternRequiresMutablePlace {
        span: Option<SourceSpan>,
    },
    InvalidOperand {
        op: String,
        operand_type: Type,
        span: Option<SourceSpan>,
    },
    MissingReturn {
        expected: Type,
        span: Option<SourceSpan>,
    },
    IfWithoutElseValue {
        span: Option<SourceSpan>,
    },
    IfConditionNotBool {
        found: Type,
        span: Option<SourceSpan>,
    },
    TernaryConditionNotBool {
        found: Type,
        span: Option<SourceSpan>,
    },
    WhileConditionNotBool {
        found: Type,
        span: Option<SourceSpan>,
    },
    BreakOutsideLoop {
        span: Option<SourceSpan>,
    },
    ContinueOutsideLoop {
        span: Option<SourceSpan>,
    },
    ReturnInsideDefer {
        span: Option<SourceSpan>,
    },
    BreakInsideDefer {
        span: Option<SourceSpan>,
    },
    ContinueInsideDefer {
        span: Option<SourceSpan>,
    },
    TryOnInvalidCarrier {
        expected: TryCarrierKind,
        found: Type,
        span: Option<SourceSpan>,
    },
    TryOutsideCarrierFunction {
        found: Option<Type>,
        span: Option<SourceSpan>,
    },
    TryResultErrorMismatch {
        expected: Type,
        found: Type,
        span: Option<SourceSpan>,
    },
    TryInsideDefer {
        span: Option<SourceSpan>,
    },
    ForIterableNotSupported {
        found: Type,
        span: Option<SourceSpan>,
    },
    ForVarRequiresMutableIterable {
        span: Option<SourceSpan>,
    },
    ForMutableMapKey {
        span: Option<SourceSpan>,
    },
    ForMutableMapEntry {
        span: Option<SourceSpan>,
    },
    ForIterationModifier {
        message: &'static str,
        span: Option<SourceSpan>,
    },
    InfiniteSize {
        name: Ident,
        span: Option<SourceSpan>,
    },
    NotEquatable {
        ty: Type,
        span: Option<SourceSpan>,
    },
    UnsupportedPattern {
        pattern: &'static str,
        span: Option<SourceSpan>,
    },
    TuplePatternArityMismatch {
        expected: usize,
        found: usize,
        span: Option<SourceSpan>,
    },
    TuplePatternOnNonTuple {
        ty: Type,
        span: Option<SourceSpan>,
    },
    OrPatternBindingMismatch {
        span: Option<SourceSpan>,
    },
    OrPatternBindingTypeMismatch {
        name: Ident,
        expected: Type,
        found: Type,
        span: Option<SourceSpan>,
    },
    EmptyMatch {
        span: Option<SourceSpan>,
    },
    NonExhaustiveMatch {
        span: Option<SourceSpan>,
    },
    UnsupportedMatchScrutinee {
        found: Type,
        span: Option<SourceSpan>,
    },
    InvalidLiteralPattern {
        expected: Type,
        found: Type,
        span: Option<SourceSpan>,
    },
    OptionalPatternOnNonOptional {
        span: Option<SourceSpan>,
    },
    OptionalChainingOnNonOptional {
        span: Option<SourceSpan>,
    },
    NestedOptionalPattern {
        span: Option<SourceSpan>,
    },
    MatchArmTypeMismatch {
        expected: Type,
        found: Type,
        span: Option<SourceSpan>,
    },
    RequiresUnwrappingPattern {
        span: Option<SourceSpan>,
    },
    IrrefutableLetElse {
        span: Option<SourceSpan>,
    },
    LetElseMustDiverge {
        span: Option<SourceSpan>,
    },
    MemberAccessOnNonAggregate {
        ty: Type,
        member: Ident,
        kind: MemberAccessKind,
        span: Option<SourceSpan>,
    },
    UnknownMember {
        ty: Type,
        member: Ident,
        kind: MemberAccessKind,
        span: Option<SourceSpan>,
    },
    AmbiguousPromotedField {
        ty: Type,
        member: Ident,
        candidates: Vec<Vec<Ident>>,
        span: Option<SourceSpan>,
    },
    AmbiguousPromotedMethod {
        ty: Type,
        member: Ident,
        candidates: Vec<Vec<Ident>>,
        span: Option<SourceSpan>,
    },
    PromotedFieldNotStored {
        ty: Type,
        field: Ident,
        paths: Vec<Vec<Ident>>,
        span: Option<SourceSpan>,
    },
    DuplicateProjectionTarget {
        source: Type,
        target: Type,
        paths: Vec<Vec<Ident>>,
        span: Option<SourceSpan>,
    },
    ChainedProjection {
        source: Type,
        target: Type,
        via: Vec<Ident>,
        span: Option<SourceSpan>,
    },
    MissingProjection {
        source: Type,
        target: Type,
        paths: Vec<Vec<Ident>>,
        span: Option<SourceSpan>,
    },
    InstanceMethodOnType {
        ty: Type,
        method: Ident,
        span: Option<SourceSpan>,
    },
    StaticMethodOnValue {
        ty: Type,
        method: Ident,
        span: Option<SourceSpan>,
    },
    ReadonlyMethodMutation {
        span: Option<SourceSpan>,
    },
    UnknownIntrinsic {
        name: Ident,
        span: Option<SourceSpan>,
    },
    IntrinsicArgCount {
        name: Ident,
        expected: usize,
        found: usize,
        span: Option<SourceSpan>,
    },
    IntrinsicExpectedIdent {
        name: Ident,
        span: Option<SourceSpan>,
    },
    IntrinsicExpectedString {
        name: Ident,
        span: Option<SourceSpan>,
    },
    UnknownIntrinsicValue {
        predicate: Ident,
        value: Ident,
        span: Option<SourceSpan>,
    },
    CompileError {
        message: String,
        span: Option<SourceSpan>,
    },
    MethodGenericShadow {
        owner_kind: AggregateKind,
        method_param: GenericParamKind,
        owner_param: GenericParamKind,
        name: Ident,
        span: Option<SourceSpan>,
    },
    TupleIndexOnNonTuple {
        ty: Type,
        index: u32,
        span: Option<SourceSpan>,
    },
    TupleIndexOutOfBounds {
        index: u32,
        len: usize,
        span: Option<SourceSpan>,
    },
    IndexNotInt {
        found: Type,
        span: Option<SourceSpan>,
    },
    IndexOnNonIndexable {
        found: Type,
        span: Option<SourceSpan>,
    },
    RangeIndexNotInt {
        found: Type,
        span: Option<SourceSpan>,
    },
    RangeIndexUnsupported {
        found: Type,
        span: Option<SourceSpan>,
    },
    NonKeyableMapKey {
        ty: Type,
        field: Option<Ident>,
        span: Option<SourceSpan>,
    },
    DuplicateMapKey {
        span: Option<SourceSpan>,
    },
    UndefinedModuleMember {
        module: ModuleScope,
        name: Ident,
        span: Option<SourceSpan>,
    },
    PrivateModuleMember {
        module: ModuleScope,
        name: Ident,
        span: Option<SourceSpan>,
    },
    AmbiguousExtendMethod {
        receiver: Type,
        name: Ident,
        span: Option<SourceSpan>,
    },
    DuplicateField {
        name: Ident,
        span: Option<SourceSpan>,
    },
    MissingField {
        name: Ident,
        span: Option<SourceSpan>,
    },
    UnknownVariantField {
        enum_name: Ident,
        variant: Ident,
        field: Ident,
        span: Option<SourceSpan>,
    },
    MissingVariantField {
        enum_name: Ident,
        variant: Ident,
        field: Ident,
        span: Option<SourceSpan>,
    },
    InvalidStructLiteral {
        name: Ident,
        kind: String,
        span: Option<SourceSpan>,
    },
    UnknownStructLiteral {
        qualifier: Option<Ident>,
        name: Ident,
        span: Option<SourceSpan>,
    },
    UnknownEnumVariant {
        enum_name: Ident,
        variant: Ident,
        span: Option<SourceSpan>,
    },
    EnumPatternTypeMismatch {
        expected: Type,
        found: Type,
        span: Option<SourceSpan>,
    },
    EnumVariantShapeMismatch {
        enum_name: Ident,
        variant: Ident,
        expected: VariantShape,
        span: Option<SourceSpan>,
    },
    GenericArity(ArityError),
    UnboundGenericParam {
        name: Ident,
        span: Option<SourceSpan>,
    },
    UnknownConst {
        name: Ident,
        span: Option<SourceSpan>,
    },
    RuntimeGlobalInConstPosition {
        global: GlobalKey,
        span: Option<SourceSpan>,
    },
    ConstCycle {
        name: Ident,
        span: Option<SourceSpan>,
    },
    NonConstExpression {
        span: Option<SourceSpan>,
    },
    GenericFieldDefault {
        span: Option<SourceSpan>,
    },
    ConstTypeMismatch {
        expected: Type,
        found: Type,
        span: Option<SourceSpan>,
    },
    InvalidConstCast {
        from: Type,
        to: Type,
        span: Option<SourceSpan>,
    },
    InvalidCast {
        from: Type,
        to: Type,
        span: Option<SourceSpan>,
    },
    ConstDivisionByZero {
        span: Option<SourceSpan>,
    },
    ConstOverflow {
        span: Option<SourceSpan>,
    },
    ExpectedIntConst {
        found: Type,
        span: Option<SourceSpan>,
    },
    NegativeArrayLength {
        value: i64,
        span: Option<SourceSpan>,
    },
    GenericArgKindMismatch {
        expected: &'static str,
        span: Option<SourceSpan>,
    },
    ExternAnyEscape {
        span: Option<SourceSpan>,
    },
    AnyOutsideExternBoundary {
        span: Option<SourceSpan>,
    },
    ContractUnsatisfied {
        ty: Type,
        contract: String,
        detail: String,
        span: Option<SourceSpan>,
    },
    DynamicMethodMissing {
        contract: Ident,
        method: Ident,
        span: Option<SourceSpan>,
    },
    BorrowedDynReassign {
        name: Ident,
        span: Option<SourceSpan>,
    },
    DynContainerConversion {
        kind: DynContainerConversionKind,
        span: Option<SourceSpan>,
    },
    DuplicateGenericParam {
        name: Ident,
        span: Option<SourceSpan>,
    },
}

impl From<SolverFinalizeError> for TypeError {
    fn from(error: SolverFinalizeError) -> Self {
        match error {
            SolverFinalizeError::UnresolvedType { span } => Self::CannotInferType { span },
            SolverFinalizeError::UnresolvedConst { span } => Self::CannotInferConst { span },
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BindingMutability {
    Readonly,
    Mutable,
}

impl BindingMutability {
    fn from_bool(mutable: bool) -> Self {
        if mutable {
            Self::Mutable
        } else {
            Self::Readonly
        }
    }

    fn place_access(self) -> PlaceAccess {
        match self {
            Self::Readonly => PlaceAccess::Immutable,
            Self::Mutable => PlaceAccess::Mutable,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct LocalBindingKind {
    mutability: BindingMutability,
    storage: CaptureStorageOrigin,
}

impl LocalBindingKind {
    fn immutable() -> Self {
        Self::from_mutable(false)
    }

    fn constant() -> Self {
        Self {
            mutability: BindingMutability::Readonly,
            storage: CaptureStorageOrigin::Const,
        }
    }

    fn borrowed_self() -> Self {
        Self {
            mutability: BindingMutability::Mutable,
            storage: CaptureStorageOrigin::VarSelf,
        }
    }

    fn pattern_alias(context: PatternContext) -> Self {
        let storage = match context {
            PatternContext::For => CaptureStorageOrigin::ForVarAlias,
            _ => CaptureStorageOrigin::PatternAlias,
        };
        Self {
            mutability: BindingMutability::Mutable,
            storage,
        }
    }

    fn downcast_alias() -> Self {
        Self {
            mutability: BindingMutability::Mutable,
            storage: CaptureStorageOrigin::MutableDowncastAlias,
        }
    }

    fn readonly_self() -> Self {
        Self {
            mutability: BindingMutability::Readonly,
            storage: CaptureStorageOrigin::ReadonlySelf,
        }
    }

    fn from_param(mutable: bool, ty: &Type) -> Self {
        let mutability = BindingMutability::from_bool(mutable);
        let storage = if mutable && matches!(ty, Type::Dyn(_)) {
            CaptureStorageOrigin::DynView
        } else if mutable {
            CaptureStorageOrigin::BorrowedParam
        } else {
            CaptureStorageOrigin::Owned
        };
        Self {
            mutability,
            storage,
        }
    }

    fn from_mutable(mutable: bool) -> Self {
        Self {
            mutability: BindingMutability::from_bool(mutable),
            storage: CaptureStorageOrigin::Owned,
        }
    }

    fn requires_runtime_capture(self) -> bool {
        !matches!(self.storage, CaptureStorageOrigin::Const)
    }

    fn place_access(self) -> PlaceAccess {
        match self.storage {
            CaptureStorageOrigin::Owned
            | CaptureStorageOrigin::BorrowedParam
            | CaptureStorageOrigin::VarSelf => self.mutability.place_access(),
            CaptureStorageOrigin::DynView => PlaceAccess::DynView,
            CaptureStorageOrigin::Const => PlaceAccess::Const,
            CaptureStorageOrigin::PatternAlias
            | CaptureStorageOrigin::MutableDowncastAlias
            | CaptureStorageOrigin::ForVarAlias => PlaceAccess::Mutable,
            CaptureStorageOrigin::ReadonlySelf => PlaceAccess::ReadonlySelf,
        }
    }
}

#[derive(Clone)]
struct VarInfo {
    binding_id: BindingId,
    type_id: LocalTypeId,
    kind: LocalBindingKind,
    const_value: Option<ConstValue>,
    local_const: Option<LocalConstId>,
    alias: Option<Box<place::AliasTarget>>,
}

#[derive(Clone)]
struct LocalCallableInfo {
    binding_id: BindingId,
    type_id: LocalTypeId,
    callee: CallableRef,
}

#[derive(Clone)]
enum LocalSymbol {
    Value(VarInfo),
    Callable(Box<LocalCallableInfo>),
}

enum LocalSymbolLookup {
    Found(LocalSymbol, usize),
    Blocked(Box<TypeError>),
    Missing,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct LocalConstId(u32);

#[derive(Clone, Copy)]
struct LocalConstInfo {
    binding_id: BindingId,
    type_id: LocalTypeId,
    id: LocalConstId,
}

impl LocalConstInfo {
    fn symbol(self) -> LocalSymbol {
        LocalSymbol::Value(VarInfo {
            binding_id: self.binding_id,
            type_id: self.type_id,
            kind: LocalBindingKind::constant(),
            const_value: None,
            local_const: Some(self.id),
            alias: None,
        })
    }
}

struct LocalValue {
    info: VarInfo,
    source_depth: usize,
}

struct LocalPlaceAccess {
    access: PlaceAccess,
    facts: PlaceUseFacts,
    identity: PlaceIdentity,
    accepts_extern_any: bool,
}

#[derive(Clone)]
struct NamedFunctionFrame {
    value_floor: usize,
}

impl LocalCallableInfo {
    fn has_generics(&self) -> bool {
        !self.callee.def.sig.owner_generics.is_empty() || !self.callee.def.sig.generics.is_empty()
    }

    fn value_error(&self, name: Ident, span: Option<SourceSpan>) -> Option<TypeError> {
        if self.has_generics() {
            Some(TypeError::UndefinedVariable { name, span })
        } else if self.callee.def.sig.ret.is_infer() {
            Some(TypeError::InferReturnValue { span })
        } else {
            None
        }
    }

    fn value_view(&self) -> VarInfo {
        VarInfo {
            binding_id: self.binding_id,
            type_id: self.type_id,
            kind: LocalBindingKind::immutable(),
            const_value: None,
            local_const: None,
            alias: None,
        }
    }
}

impl LocalSymbol {
    fn requires_runtime_capture(&self) -> bool {
        match self {
            Self::Value(info) => info.kind.requires_runtime_capture(),
            Self::Callable(_) => false,
        }
    }

    fn value_error(&self, name: Ident, span: Option<SourceSpan>) -> Option<TypeError> {
        match self {
            Self::Value(_) => None,
            Self::Callable(info) => info.value_error(name, span),
        }
    }

    fn value_view(&self) -> VarInfo {
        match self {
            Self::Value(info) => info.clone(),
            Self::Callable(info) => info.value_view(),
        }
    }
}

#[derive(Clone)]
enum ReturnMode {
    Explicit {
        ret: ReturnSpec,
        source: Option<PlaceIdentity>,
    },
    Infer {
        access: ReturnAccess,
        source: Option<PlaceIdentity>,
        candidates: Vec<(Span, TypeHandle)>,
    },
}

struct ReturnFrame {
    mode: ReturnMode,
}

#[derive(Clone)]
struct ScopeState {
    scopes: Vec<HashMap<Ident, LocalSymbol>>,
    local_type_scopes: LocalTypeScopes,
    closure: ClosureScopeState,
}

#[derive(Clone)]
struct ActiveMutDowncastRoot {
    identity: PlaceIdentity,
    allowed: Ident,
}

struct TypeChecker {
    solver: Solver,
    calls: CallMap,
    extern_uses: ExternUseMap,
    member_paths: MemberPathMap,
    argument_projections: ArgumentProjectionMap,
    contract_witnesses: ContractWitnessMap,
    witness_keys: HashMap<ContractWitnessKey, WitnessId>,
    dyn_conversions: DynConversionMap,
    dyn_weakenings: DynWeakeningMap,
    dyn_calls: DynCallMap,
    dyn_downcasts: DynDowncastMap,
    global_accesses: GlobalAccessMap,
    closure: ClosureClassifier,
    global_types: HashMap<GlobalKey, LocalTypeId>,
    active_mut_downcast_roots: Vec<ActiveMutDowncastRoot>,
    dyn_infer_registered_modules: HashSet<ModuleScope>,
    dyn_infer: DynInference,
    used_imports: HashSet<ImportId>,
    next_witness_id: u32,
    decls: DeclarationIndex,
    externs: ExternCatalog,
    promoted_surfaces: HashMap<CanonicalTypeKey, PromotedSurface>,
    scopes: Vec<HashMap<Ident, LocalSymbol>>,
    local_type_scopes: LocalTypeScopes,
    named_function_frames: Vec<NamedFunctionFrame>,
    returns: Vec<ReturnFrame>,
    loop_depth: usize,
    defer_depth: usize,
    global_initializer_depth: usize,
    discard_depth: usize,
    errors: Vec<TypeError>,
    warnings: Vec<CompileWarning>,
    lint_events: Vec<LintEvent>,
    config: TypecheckConfig,
    current_module: ModuleScope,
    module_sources: HashMap<ModuleScope, SourceId>,
    module_programs: HashMap<ModuleScope, Rc<Program>>,
    type_substs: Vec<TypeSubst>,
    const_substs: Vec<ConstSubst>,
    generic_contexts: Vec<GenericTypeContext>,
    generic_owner_frames: Vec<GenericOwnerFrame>,
    local_callables: HashMap<CallableId, LocalCallableInfo>,
    callable_templates: HashMap<CallableId, CallableTemplate>,
    specializations: HashMap<SpecializationKey, SpecializationState>,
    consts: HashMap<(ModuleScope, Ident), const_eval::ConstEntry>,
    local_consts: Vec<const_eval::LocalConstEntry>,
    next_alias_alt_group: u32,
    next_binding_id: u32,
}

impl TypeChecker {
    fn new(decls: DeclarationIndex, externs: ExternCatalog, config: TypecheckConfig) -> Self {
        let core_option = decls.core_option_key();
        Self {
            solver: Solver::new(core_option),
            calls: HashMap::new(),
            extern_uses: HashMap::new(),
            member_paths: HashMap::new(),
            argument_projections: HashMap::new(),
            contract_witnesses: HashMap::new(),
            witness_keys: HashMap::new(),
            dyn_conversions: HashMap::new(),
            dyn_weakenings: HashMap::new(),
            dyn_calls: HashMap::new(),
            dyn_downcasts: HashMap::new(),
            global_accesses: HashMap::new(),
            closure: ClosureClassifier::default(),
            global_types: HashMap::new(),
            active_mut_downcast_roots: vec![],
            dyn_infer_registered_modules: HashSet::new(),
            dyn_infer: DynInference::default(),
            used_imports: HashSet::new(),
            next_witness_id: 0,
            decls,
            externs,
            promoted_surfaces: HashMap::new(),
            scopes: vec![],
            local_type_scopes: LocalTypeScopes::default(),
            named_function_frames: vec![],
            returns: vec![],
            loop_depth: 0,
            defer_depth: 0,
            global_initializer_depth: 0,
            discard_depth: 0,
            errors: vec![],
            warnings: vec![],
            lint_events: vec![],
            config,
            current_module: ModuleScope::Root,
            module_sources: HashMap::new(),
            module_programs: HashMap::new(),
            type_substs: vec![],
            const_substs: vec![],
            generic_contexts: vec![],
            generic_owner_frames: vec![],
            local_callables: HashMap::new(),
            callable_templates: HashMap::new(),
            specializations: HashMap::new(),
            consts: HashMap::new(),
            local_consts: vec![],
            next_alias_alt_group: 0,
            next_binding_id: 0,
        }
    }

    fn source_id(&self) -> SourceId {
        self.module_sources
            .get(&self.current_module)
            .copied()
            .expect("source module has no SourceId")
    }

    fn source_span(&self, span: Span) -> SourceSpan {
        SourceSpan::from_byte_span(self.source_id(), span)
    }

    fn error_span(&self, span: Span) -> Option<SourceSpan> {
        self.module_error_span(&self.current_module, span)
    }

    fn module_error_span(&self, module: &ModuleScope, span: Span) -> Option<SourceSpan> {
        self.module_sources
            .get(module)
            .map(|source| SourceSpan::from_byte_span(*source, span))
    }

    fn with_current_module<R>(
        &mut self,
        module: &ModuleScope,
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        let previous = std::mem::replace(&mut self.current_module, module.clone());
        let ret = f(self);
        self.current_module = previous;
        ret
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
        self.local_type_scopes.push_scope();
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
        self.closure.exit_scope(self.scopes.len());
        self.local_type_scopes.pop_scope();
    }

    fn closure_scope_state_from(
        &self,
        scopes: &[HashMap<Ident, LocalSymbol>],
    ) -> ClosureScopeState {
        self.closure
            .scope_state_for_bindings(scopes.iter().enumerate().flat_map(|(depth, scope)| {
                scope.iter().map(move |(name, symbol)| match symbol {
                    LocalSymbol::Value(info) => {
                        (info.binding_id, *name, info.type_id, info.kind, depth)
                    }
                    LocalSymbol::Callable(info) => (
                        info.binding_id,
                        *name,
                        info.type_id,
                        LocalBindingKind::immutable(),
                        depth,
                    ),
                })
            }))
    }

    fn scope_state_from(&self, scopes: Vec<HashMap<Ident, LocalSymbol>>) -> ScopeState {
        let closure = self.closure_scope_state_from(&scopes);
        ScopeState {
            scopes,
            local_type_scopes: self.local_type_scopes.clone(),
            closure,
        }
    }

    fn take_scope_state(&mut self) -> ScopeState {
        ScopeState {
            scopes: std::mem::take(&mut self.scopes),
            local_type_scopes: std::mem::take(&mut self.local_type_scopes),
            closure: self
                .closure
                .replace_scope_state(ClosureScopeState::default()),
        }
    }

    fn restore_scope_state(&mut self, state: ScopeState) {
        self.scopes = state.scopes;
        self.local_type_scopes = state.local_type_scopes;
        self.closure.restore_scope_state(state.closure);
    }

    fn replace_scopes(&mut self, scopes: Vec<HashMap<Ident, LocalSymbol>>) {
        let closure = self.closure_scope_state_from(&scopes);
        self.scopes = scopes;
        self.closure.restore_scope_state(closure);
    }

    fn replace_scope_state(&mut self, state: ScopeState) -> ScopeState {
        ScopeState {
            scopes: std::mem::replace(&mut self.scopes, state.scopes),
            local_type_scopes: std::mem::replace(
                &mut self.local_type_scopes,
                state.local_type_scopes,
            ),
            closure: self.closure.replace_scope_state(state.closure),
        }
    }

    fn fresh_alias_alt_group(&mut self) -> AliasAltGroupId {
        let id = self.next_alias_alt_group;
        self.next_alias_alt_group += 1;
        AliasAltGroupId::new(id)
    }

    fn fresh_binding_id(&mut self) -> BindingId {
        let id = BindingId(self.next_binding_id);
        self.next_binding_id += 1;
        id
    }

    fn define_closure_binding(
        &mut self,
        binding_id: BindingId,
        name: Ident,
        type_id: LocalTypeId,
        kind: LocalBindingKind,
    ) {
        let Some(scope_depth) = self.scopes.len().checked_sub(1) else {
            return;
        };
        self.closure
            .define_binding(binding_id, name, type_id, kind, scope_depth);
    }

    fn define(&mut self, name: Ident, ty: Type, mutable: bool) {
        self.define_value(name, ty, LocalBindingKind::from_mutable(mutable), None);
    }

    fn define_pattern_binding_from_handle(
        &mut self,
        name: Ident,
        handle: &TypeHandle,
        mutable: bool,
    ) {
        self.define_shadowing_value_from_handle(
            name,
            handle,
            LocalBindingKind::from_mutable(mutable),
            None,
            None,
        );
    }

    fn define_alias_binding_from_handle(
        &mut self,
        name: Ident,
        handle: &TypeHandle,
        target: place::AliasTarget,
        context: PatternContext,
    ) {
        self.define_shadowing_value_from_handle(
            name,
            handle,
            LocalBindingKind::pattern_alias(context),
            None,
            Some(target),
        );
    }

    fn define_downcast_alias_from_handle(
        &mut self,
        name: Ident,
        handle: &TypeHandle,
        target: place::AliasTarget,
    ) {
        self.define_shadowing_value_from_handle(
            name,
            handle,
            LocalBindingKind::downcast_alias(),
            None,
            Some(target),
        );
    }

    fn define_const(&mut self, name: Ident, ty: Type, value: ConstValue) {
        self.define_value(name, ty, LocalBindingKind::constant(), Some(value));
    }

    fn define_value(
        &mut self,
        name: Ident,
        ty: Type,
        kind: LocalBindingKind,
        const_value: Option<ConstValue>,
    ) -> LocalTypeId {
        self.define_value_with_alias(name, ty, kind, const_value, None)
    }

    fn define_value_with_alias(
        &mut self,
        name: Ident,
        ty: Type,
        kind: LocalBindingKind,
        const_value: Option<ConstValue>,
        alias: Option<place::AliasTarget>,
    ) -> LocalTypeId {
        let binding_id = self.fresh_binding_id();
        let type_id = self.solver.alloc_local_type(&ty);
        let inserted = self.define_local_symbol(
            name,
            LocalSymbol::Value(VarInfo {
                binding_id,
                type_id,
                kind,
                const_value,
                local_const: None,
                alias: alias.map(Box::new),
            }),
        );
        if inserted {
            self.define_closure_binding(binding_id, name, type_id, kind);
        }
        type_id
    }

    fn define_local_symbol(&mut self, name: Ident, symbol: LocalSymbol) -> bool {
        let Some(scope) = self.scopes.last() else {
            return false;
        };
        if scope.contains_key(&name) {
            self.errors
                .push(TypeError::DuplicateName { name, span: None });
            return false;
        }
        self.scopes
            .last_mut()
            .expect("scope exists")
            .insert(name, symbol);
        true
    }

    fn define_shadowing_value_from_handle(
        &mut self,
        name: Ident,
        handle: &TypeHandle,
        kind: LocalBindingKind,
        const_value: Option<ConstValue>,
        alias: Option<place::AliasTarget>,
    ) {
        let type_id = self.solver.alloc_local_type_from_handle(handle);
        self.define_shadowing_local(name, type_id, kind, const_value, alias);
    }

    fn define_shadowing_local(
        &mut self,
        name: Ident,
        type_id: LocalTypeId,
        kind: LocalBindingKind,
        const_value: Option<ConstValue>,
        alias: Option<place::AliasTarget>,
    ) {
        let binding_id = self.fresh_binding_id();
        let Some(scope) = self.scopes.last_mut() else {
            return;
        };
        scope.insert(
            name,
            LocalSymbol::Value(VarInfo {
                binding_id,
                type_id,
                kind,
                const_value,
                local_const: None,
                alias: alias.map(Box::new),
            }),
        );
        self.define_closure_binding(binding_id, name, type_id, kind);
    }

    fn local_binding_id(&self, name: Ident) -> Option<BindingId> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| match scope.get(&name) {
                Some(LocalSymbol::Value(info)) => Some(info.binding_id),
                Some(LocalSymbol::Callable(info)) => Some(info.binding_id),
                _ => None,
            })
    }

    fn define_local_callable(&mut self, name: Ident, callee: CallableRef, surface_ty: Type) {
        let binding_id = self.fresh_binding_id();
        let type_id = self.solver.alloc_local_type(&surface_ty);
        let info = LocalCallableInfo {
            binding_id,
            type_id,
            callee,
        };
        self.local_callables
            .insert(info.callee.def.id.clone(), info.clone());
        if self.define_local_symbol(name, LocalSymbol::Callable(Box::new(info))) {
            self.define_closure_binding(binding_id, name, type_id, LocalBindingKind::immutable());
        }
    }

    fn lookup(&self, name: Ident) -> Option<VarInfo> {
        self.lookup_with_depth(name).map(|(info, _)| info)
    }

    fn lookup_with_depth(&self, name: Ident) -> Option<(VarInfo, usize)> {
        self.lookup_local_symbol(name)
            .map(|(symbol, depth)| (symbol.value_view(), depth))
    }

    fn lookup_local_symbol(&self, name: Ident) -> Option<(&LocalSymbol, usize)> {
        for (depth, scope) in self.scopes.iter().enumerate().rev() {
            if let Some(symbol) = scope.get(&name) {
                return Some((symbol, depth));
            }
        }
        None
    }

    fn local_callable(&self, id: &CallableId) -> Option<LocalCallableInfo> {
        self.local_callables.get(id).cloned()
    }

    fn lookup_local_symbol_checked(&self, name: Ident, span: Span) -> LocalSymbolLookup {
        let Some((symbol, depth)) = self
            .lookup_local_symbol(name)
            .map(|(symbol, depth)| (symbol.clone(), depth))
        else {
            return LocalSymbolLookup::Missing;
        };
        if self.blocks_named_capture(&symbol, depth) {
            return LocalSymbolLookup::Blocked(Box::new(TypeError::NamedFunctionCapture {
                name,
                span: self.error_span(span),
            }));
        }
        LocalSymbolLookup::Found(symbol, depth)
    }

    fn lookup_local_value_checked(
        &mut self,
        name: Ident,
        span: Span,
    ) -> Result<Option<LocalValue>, ()> {
        match self.lookup_local_symbol_checked(name, span) {
            LocalSymbolLookup::Found(symbol, depth) => {
                if let Some(error) = symbol.value_error(name, self.error_span(span)) {
                    self.push_error(error);
                    Err(())
                } else {
                    Ok(Some(self.local_value_from_info(symbol.value_view(), depth)))
                }
            }
            LocalSymbolLookup::Blocked(error) => {
                self.push_error(*error);
                Err(())
            }
            LocalSymbolLookup::Missing => Ok(None),
        }
    }

    fn local_value_from_info(&self, info: VarInfo, depth: usize) -> LocalValue {
        LocalValue {
            info,
            source_depth: depth,
        }
    }

    fn blocks_named_capture(&self, symbol: &LocalSymbol, depth: usize) -> bool {
        symbol.requires_runtime_capture()
            && self
                .named_function_frames
                .last()
                .is_some_and(|frame| depth > 0 && depth < frame.value_floor)
    }

    fn enter_named_function(&mut self) {
        self.named_function_frames.push(NamedFunctionFrame {
            value_floor: self.scopes.len(),
        });
    }

    fn exit_named_function(&mut self) {
        self.named_function_frames.pop();
    }

    fn local_value_access(&self, value: &LocalValue) -> LocalPlaceAccess {
        let alias = value.info.alias.as_deref();
        let access = alias.map_or_else(|| value.info.kind.place_access(), |alias| alias.access);
        let (facts, identity, accepts_extern_any) = match alias {
            Some(alias) => (
                alias.facts.clone(),
                alias.identity.clone(),
                alias.accepts_extern_any,
            ),
            None => (
                PlaceUseFacts::default(),
                PlaceIdentity::root(PlaceRoot::Local(value.info.type_id)),
                false,
            ),
        };
        LocalPlaceAccess {
            access,
            facts,
            identity,
            accepts_extern_any,
        }
    }

    fn type_handle(&self, ty: &Type) -> TypeHandle {
        self.solver.concrete_type(ty)
    }

    fn local_handle(&self, id: LocalTypeId) -> TypeHandle {
        self.solver.local_handle(id)
    }

    fn set_type(&mut self, id: ExprId, ty: Type, span: Span) -> TypeHandle {
        let span = self.error_span(span);
        if matches!(ty, Type::Infer) {
            self.set_poison_type(id, span)
        } else {
            self.solver.set_expr_type_from_type(id, span, &ty);
            self.solver.expr_handle(id)
        }
    }

    fn set_poison_type(&mut self, id: ExprId, span: Option<SourceSpan>) -> TypeHandle {
        self.solver.poison_expr_type(id, span)
    }

    fn handle_is_poison(&self, handle: &TypeHandle) -> bool {
        self.solver.handle_is_poison(handle)
    }

    pub(super) fn checked_is_poison(&self, checked: &CheckedType) -> bool {
        self.handle_is_poison(&checked.handle)
    }

    fn set_nil_type(&mut self, id: ExprId, span: Span) -> TypeHandle {
        self.solver.nil_expr_type(id, self.error_span(span))
    }

    fn fresh_nil_handle(&mut self, span: Span) -> TypeHandle {
        self.solver.fresh_nil_handle(self.error_span(span))
    }

    fn set_type_from_handle(&mut self, id: ExprId, span: Span, handle: &TypeHandle) -> TypeHandle {
        self.solver
            .set_expr_type_from_handle(id, self.error_span(span), handle)
    }

    fn fresh_temp_handle(&mut self, span: Span) -> TypeHandle {
        self.solver.fresh_temp_handle(self.error_span(span))
    }

    fn array_handle(&mut self, elem: &TypeHandle, len: &ArrayLen) -> TypeHandle {
        self.solver.array_handle(elem, len)
    }

    fn list_handle(&mut self, elem: &TypeHandle) -> TypeHandle {
        self.solver.list_handle(elem)
    }

    fn map_handle(&mut self, key: &TypeHandle, value: &TypeHandle) -> TypeHandle {
        self.solver.map_handle(key, value)
    }

    fn tuple_handle(&mut self, elems: Vec<TypeHandle>) -> TypeHandle {
        self.solver.tuple_handle(elems)
    }

    fn handle_type(&self, handle: &TypeHandle) -> Type {
        self.solver.handle_to_type(handle)
    }

    fn solve_constraints(&mut self) -> bool {
        let errors = self.solver.solve_pending();
        let has_errors = !errors.is_empty();
        for error in errors {
            self.push_solver_error(error);
        }
        has_errors
    }

    fn solve_dyn_inference(&mut self) {
        let mut dyn_infer = std::mem::take(&mut self.dyn_infer);
        dyn_infer.solve(self);
        self.dyn_infer = dyn_infer;
    }

    fn push_solver_error(&mut self, error: SolverRelationError) {
        let err = match error {
            SolverRelationError::TypeMismatch {
                expected,
                found,
                span,
            } => TypeError::TypeMismatch {
                expected,
                found,
                span,
            },
            SolverRelationError::ConstMismatch {
                expected,
                found,
                span,
            } => TypeError::ConstMismatch {
                expected,
                found,
                span,
            },
            SolverRelationError::RecursiveInference { span } => {
                TypeError::RecursiveInference { span }
            }
        };
        self.push_error(err);
    }

    fn push_finalize_errors(&mut self, errors: Vec<SolverFinalizeError>) -> bool {
        let has_errors = !errors.is_empty();
        for error in errors {
            self.push_error_once(TypeError::from(error));
        }
        has_errors
    }

    fn expr_types(&self) -> HashMap<ExprId, (Span, Type)> {
        self.solver.expr_types_to_types()
    }

    fn record_call(&mut self, expr_id: ExprId, target: CallTarget) {
        self.calls.insert(expr_id, target);
    }

    fn record_extern_use(&mut self, expr_id: ExprId, target: ExternUseTarget) {
        self.extern_uses.entry(expr_id).or_default().push(target);
    }

    pub(crate) fn record_member_path(&mut self, fact: MemberPathFact) {
        self.member_paths.insert(fact.expr_id, fact);
    }

    pub(crate) fn record_argument_projection(&mut self, fact: ArgumentProjectionFact) {
        self.argument_projections
            .insert((fact.call_id, fact.arg_index), fact);
    }

    fn record_contract_witness(&mut self, key: ContractWitnessKey, span: Span) -> WitnessId {
        let origins = key
            .slots
            .iter()
            .filter_map(|slot| match &slot.target {
                WitnessSlotTarget::Extend { extend, .. } => self
                    .decls
                    .extend(extend)
                    .map(|schema| schema.origin.clone()),
                WitnessSlotTarget::Direct { .. }
                | WitnessSlotTarget::Extern { .. }
                | WitnessSlotTarget::Promoted { .. } => None,
            })
            .collect::<Vec<_>>();
        for origin in origins {
            self.mark_activation_imports_used(&origin);
        }
        if let Some(id) = self.witness_keys.get(&key) {
            return *id;
        }
        let id = WitnessId(self.next_witness_id);
        self.next_witness_id += 1;
        let fact = ContractWitnessFact {
            id,
            key: key.clone(),
            span: self.source_span(span),
        };
        self.witness_keys.insert(key, id);
        self.contract_witnesses.insert(id, fact);
        id
    }

    fn record_dyn_conversion(&mut self, fact: DynConversionFact) {
        self.dyn_conversions.insert(fact.expr_id, fact);
    }

    fn record_dyn_weakening(&mut self, fact: DynWeakeningFact) {
        self.dyn_weakenings.insert(fact.expr_id, fact);
    }

    fn record_dyn_call(&mut self, fact: DynCallFact) {
        self.dyn_calls.insert(fact.call_id, fact);
    }

    fn record_dyn_downcast(&mut self, fact: DynDowncastFact) {
        self.dyn_downcasts.insert(fact.expr_id, fact);
    }

    fn should_register_dyn_infer_params(&mut self) -> bool {
        self.dyn_infer_registered_modules
            .insert(self.current_module.clone())
    }

    pub(super) fn check_mut_downcast_root_use(
        &mut self,
        root_name: Option<Ident>,
        identity: &PlaceIdentity,
        span: Span,
    ) {
        let Some(root_name) = root_name else {
            return;
        };
        if self
            .active_mut_downcast_roots
            .iter()
            .any(|root| root.allowed != root_name && root.identity.conflicts_with(identity))
        {
            self.push_error(TypeError::CompileError {
                message: "dynamic root cannot be used while a mutable downcast binding is live"
                    .to_string(),
                span: self.error_span(span),
            });
        }
    }

    fn extern_type_id(&self, ty: &Type) -> Option<ExternTypeId> {
        let key = self.decls.key_for_type(ty)?;
        (key.kind == NominalKind::Extern)
            .then(|| self.externs.type_by_nominal(&key))
            .flatten()
    }

    fn extern_type(&self, owner: ExternTypeId) -> &ExternType {
        self.externs.ty(owner)
    }

    fn try_carrier_parts(&self, ty: &Type) -> Option<TryCarrier> {
        let Type::Nominal(nominal) = ty else {
            return None;
        };
        let key = self.decls.key_for_type(ty)?;
        let (kind, success, error) = if self.decls.core_result_key().as_ref() == Some(&key) {
            let [success, error] = nominal.type_args.as_slice() else {
                return None;
            };
            (TryCarrierKind::Result, success, Some(error.clone()))
        } else {
            (
                TryCarrierKind::Option,
                self.decls.semantic_option_inner(ty)?,
                None,
            )
        };
        Some(TryCarrier {
            kind,
            nominal: nominal.clone(),
            success: success.clone(),
            error,
        })
    }

    fn extern_field(
        &self,
        owner: ExternTypeId,
        name: Ident,
    ) -> Option<(ExternFieldRef, &ExternField)> {
        self.externs.field(owner, name)
    }

    fn push_return_frame(&mut self, mode: ReturnMode) {
        self.returns.push(ReturnFrame { mode });
    }

    fn pop_return_frame(&mut self) -> Option<ReturnFrame> {
        self.returns.pop()
    }

    fn return_mode(&self) -> Option<&ReturnMode> {
        self.returns.last().map(|frame| &frame.mode)
    }

    fn return_type(&self) -> Option<&Type> {
        match self.return_mode()? {
            ReturnMode::Explicit { ret, .. } => Some(&ret.ty),
            ReturnMode::Infer { .. } => None,
        }
    }

    fn push_inferred_return(&mut self, span: Span, handle: TypeHandle) {
        let Some(frame) = self.returns.last_mut() else {
            return;
        };
        let ReturnMode::Infer { candidates, .. } = &mut frame.mode else {
            return;
        };
        candidates.push((span, handle));
    }

    fn push_error(&mut self, err: TypeError) {
        self.errors.push(err);
    }

    fn push_warning(&mut self, warning: CompileWarning) {
        if !self.warnings.contains(&warning) {
            self.warnings.push(warning);
        }
    }

    pub(super) fn push_lint_event(&mut self, event: LintEvent) {
        if !self.lint_events.contains(&event) {
            self.lint_events.push(event);
        }
    }

    fn push_error_once(&mut self, err: TypeError) {
        if !self.errors.contains(&err) {
            self.push_error(err);
        }
    }

    fn push_unbound_generic_errors(&mut self, names: Vec<Ident>, span: Span) {
        for name in names {
            self.push_error(TypeError::UnboundGenericParam {
                name,
                span: self.error_span(span),
            });
        }
    }

    fn enter_global_initializer(&mut self) {
        self.global_initializer_depth += 1;
    }

    fn exit_global_initializer(&mut self) {
        self.global_initializer_depth = self
            .global_initializer_depth
            .checked_sub(1)
            .expect("global initializer depth underflow");
    }

    fn in_global_initializer(&self) -> bool {
        self.global_initializer_depth > 0
    }

    fn push_type_subst(&mut self, subst: TypeSubst) {
        self.type_substs.push(subst);
    }

    fn pop_type_subst(&mut self) {
        self.type_substs.pop();
    }

    fn push_const_subst(&mut self, subst: ConstSubst) {
        self.const_substs.push(subst);
    }

    fn pop_const_subst(&mut self) {
        self.const_substs.pop();
    }

    fn generic_context(
        &mut self,
        type_params: &[TypeParam],
        const_params: &[ConstParam],
        span: Span,
    ) -> GenericTypeContext {
        match GenericTypeContext::try_from_params(type_params, const_params) {
            Ok(generics) => generics,
            Err(error) => {
                self.push_error(generic_param_type_error(error, self.error_span(span)));
                GenericTypeContext::default()
            }
        }
    }

    fn extended_generic_context(
        &mut self,
        owner: &GenericTypeContext,
        type_params: &[TypeParam],
        const_params: &[ConstParam],
        span: Span,
    ) -> GenericTypeContext {
        match owner.try_with_shadowing_params(type_params, const_params) {
            Ok(generics) => generics,
            Err(error) => {
                self.push_error(generic_param_type_error(error, self.error_span(span)));
                owner.clone()
            }
        }
    }

    fn resolve_generic_bounds_for_tc(&mut self, generics: &mut GenericParams, span: Span) {
        for param in &mut generics.type_params {
            param.bounds = std::mem::take(&mut param.bounds)
                .into_iter()
                .filter_map(
                    |bound| match self.resolve_type_for_tc_at(&Type::Dyn(bound), span) {
                        Type::Dyn(bound)
                            if !matches!(bound, ContractRef::Infer | ContractRef::Hole(_)) =>
                        {
                            Some(bound)
                        }
                        _ => None,
                    },
                )
                .collect();
        }
    }

    fn resolved_value(value: ResolvedValue) -> (ModuleScope, Ident, ValueDecl) {
        (value.module, value.name, value.decl)
    }

    fn exported_value_in_module(
        &self,
        scope: &ModuleScope,
        name: Ident,
    ) -> Option<(ModuleScope, Ident, ValueDecl)> {
        self.decls
            .exported_value(scope, name)
            .map(Self::resolved_value)
    }

    fn current_module_value(&self, name: Ident) -> Option<(ModuleScope, Ident, ValueDecl)> {
        self.decls
            .local_value(&self.current_module, name)
            .map(Self::resolved_value)
    }

    fn finish_resolved_type(&mut self, finalized: Type, span: Span) -> Type {
        self.validate_escaping_parameter_types(&finalized, span);
        self.validate_nominal_uses(&finalized, span);
        let substituted = match self.type_substs.last().cloned() {
            Some(ts) => {
                let cs = self.const_substs.last().cloned().unwrap_or_default();
                self.substitute_checked(&finalized, &ts, &cs, span)
            }
            None => finalized,
        };
        let ty = self.normalize_type_consts(&substituted, span);
        self.reject_user_any_type(&ty, span);
        self.validate_type_return_specs(&ty, span);
        ty
    }

    fn mark_import_used(&mut self, import: Option<ImportId>) {
        if let Some(import) = import {
            self.used_imports.insert(import);
        }
    }

    pub(super) fn mark_activation_imports_used(&mut self, module: &ModuleScope) {
        self.used_imports.extend(
            self.decls
                .active_import_ids(&self.current_module, module)
                .iter()
                .cloned(),
        );
    }

    fn imported_value(&mut self, name: Ident) -> Option<(ModuleScope, Ident, ValueDecl)> {
        let (value, import) = self
            .decls
            .imported_value_with_import(&self.current_module, name)?;
        self.mark_import_used(import);
        Some(Self::resolved_value(value))
    }

    fn lookup_named_value(&mut self, name: Ident) -> Option<(ModuleScope, Ident, ValueDecl)> {
        if self
            .lookup_local_symbol(name)
            .is_some_and(|(_, depth)| depth > 0)
        {
            return None;
        }
        self.current_module_value(name)
            .or_else(|| self.imported_value(name))
    }

    fn lookup_module_alias(&mut self, name: Ident) -> Option<ModuleScope> {
        let (module, import) = self
            .decls
            .imported_module_with_import(&self.current_module, name)?;
        self.mark_import_used(import);
        Some(module)
    }

    fn visible_type_subject(&mut self, name: Ident, span: Span) -> Option<Type> {
        if let Some(ty) = self.substituted_type_param(name) {
            return Some(ty);
        }
        if self.local_type_scopes.visible(name, None).is_some() {
            let ty = self.resolve_type_for_tc_at(&Type::UnresolvedName(name), span);
            return (!matches!(ty, Type::Infer)).then_some(ty);
        }
        let (binding, import) = self.decls.resolve_visible_type_binding_with_import(
            &self.current_module,
            None,
            name,
        )?;
        self.mark_import_used(import);
        match binding {
            TypeBinding::Nominal(key) => {
                self.warn_extern_type_deprecated(&key, span);
                Some(nominal_type(&key))
            }
            TypeBinding::Alias(_) | TypeBinding::Contract(_) => {
                let ty = self.resolve_type_binding_for_tc_at(binding, &[], span, name);
                (!matches!(ty, Type::Infer)).then_some(ty)
            }
        }
    }

    fn func_type_from_sig(&mut self, params: &[Param], ret: &ReturnSpec, span: Span) -> Type {
        self.callable_type_from_sig(params, ret, span, false)
    }

    fn callable_type_from_sig(
        &mut self,
        params: &[Param],
        ret: &ReturnSpec,
        span: Span,
        exported: bool,
    ) -> Type {
        let resolved_params = self.resolve_callable_params(params, exported);
        let resolved_ret = ret.with_ty(self.resolve_type_for_tc_at(&ret.ty, span));
        Type::func(resolved_params, resolved_ret)
    }

    fn resolve_callable_params(&mut self, params: &[Param], exported: bool) -> Vec<FuncParam> {
        params
            .iter()
            .map(|p| {
                let ty = self.resolve_callable_param_type(&p.ty, p.ty_span, exported);
                self.validate_func_param_escape(
                    p.escape,
                    matches!(p.mutability, Mutability::Mutable),
                    p.cast_accept,
                    &ty,
                    p.ty_span,
                );
                FuncParam::new(
                    ty,
                    matches!(p.mutability, Mutability::Mutable),
                    p.cast_accept,
                    p.escape,
                )
            })
            .collect()
    }

    fn finish(&mut self) -> Option<(SourceExprTypes, TypecheckFacts)> {
        self.solve_constraints();
        self.solve_dyn_inference();
        let escape_events = self.closure.take_escape_events();
        self.push_escape_events(escape_events);
        if !self.errors.is_empty() {
            return None;
        }

        let (types, finalize_errors) = self.solver.finalize_expr_types();
        let has_finalize_errors = self.push_finalize_errors(finalize_errors);
        if !has_finalize_errors {
            for error in self.result_closure_errors(&types) {
                self.push_error_once(error);
            }
        }
        if !self.errors.is_empty() {
            return None;
        }
        let mut facts = self.closure.finish(|id| self.solver.local_type_to_type(id));
        facts.import_records = self.decls.import_records().to_vec();
        facts.used_imports.clone_from(self.decls.used_imports());
        facts.used_imports.extend(self.used_imports.clone());
        Some((types, facts))
    }

    fn into_output(mut self) -> TypecheckOutput {
        let facts = self.finish().map(|(_, facts)| facts);
        let errors = std::mem::take(&mut self.errors);
        let warnings = std::mem::take(&mut self.warnings);
        let lint_events = std::mem::take(&mut self.lint_events);
        match facts {
            Some(facts) if errors.is_empty() => {
                TypecheckOutput::success(warnings, lint_events, facts)
            }
            _ => TypecheckOutput::failed(errors, warnings, lint_events),
        }
    }

    fn result_closure_errors(&self, types: &SourceExprTypes) -> Vec<TypeError> {
        let mut errors = vec![];
        for (span, ty) in types.values() {
            push_type_closure_error(&mut errors, ty, *span);
        }
        for (id, target) in &self.calls {
            let span = types.get(id).and_then(|(span, _)| *span);
            push_call_target_closure_error(&mut errors, target, span);
        }
        self.externs.for_each_resolved_ty(|ty, site| {
            push_extern_ty_closure_error(&mut errors, ty, extern_site_span(site));
        });
        errors
    }

    pub(crate) fn check_generic_bounds(
        &mut self,
        generics: &GenericParams,
        args: &GenericArgs,
        span: Span,
    ) -> bool {
        let before = self.errors.len();
        for (param, arg) in generics.type_params.iter().zip(&args.type_args) {
            for bound in &param.bounds {
                if self.type_satisfies_bound(arg, bound, span) {
                    continue;
                }
                self.push_error(TypeError::CompileError {
                    message: format!("type '{arg}' does not satisfy contract bound '{bound}'"),
                    span: self.error_span(span),
                });
            }
        }
        self.errors.len() == before
    }

    fn type_satisfies_bound(&mut self, ty: &Type, bound: &ContractRef, span: Span) -> bool {
        match ty {
            Type::Dyn(source) => {
                contracts::contract_ref_subset(&self.decls, &self.current_module, source, bound)
            }
            Type::Var(id) => self.type_param_satisfies_bound(*id, bound),
            Type::Infer => true,
            _ => contracts::match_contract(self, ty, bound, span).is_ok(),
        }
    }

    fn type_param_satisfies_bound(&self, id: TypeVarId, bound: &ContractRef) -> bool {
        let Some(bounds) = self.type_param_bounds(id) else {
            return false;
        };
        let source = contract_ref_from_bounds(bounds);
        contracts::contract_ref_subset(&self.decls, &self.current_module, &source, bound)
    }

    fn type_param_bounds(&self, id: TypeVarId) -> Option<&[ContractRef]> {
        self.generic_owner_frames
            .iter()
            .rev()
            .find_map(|frame| frame.params.type_param_bounds(id))
    }

    fn validate_nominal_args(
        &mut self,
        decls: &DeclarationIndex,
        key: &NominalKey,
        generics: &GenericParams,
        args: &GenericArgs,
        span: Span,
    ) {
        let error_count = self.errors.len();
        for term in &args.const_args {
            self.require_usize_const(term.clone(), span, false);
        }
        if self.errors.len() != error_count {
            return;
        }
        if !self.check_generic_bounds(generics, args, span) {
            return;
        }
        let (type_subst, const_subst) = generics.substitutions(args);
        match key.kind {
            NominalKind::Struct | NominalKind::DataRef => {
                if let Some(schema) = decls.aggregate(key).cloned() {
                    for field in schema.fields.values() {
                        self.substitute_checked(&field.ty, &type_subst, &const_subst, span);
                    }
                }
            }
            NominalKind::Enum => {
                if let Some(variants) = decls.enum_schema(key).map(|schema| schema.variants.clone())
                {
                    for variant in variants.values() {
                        match &variant.payload {
                            VariantPayload::Unit => {}
                            VariantPayload::Tuple(params) => {
                                for param in params {
                                    self.substitute_checked(param, &type_subst, &const_subst, span);
                                }
                            }
                            VariantPayload::Struct(fields) => {
                                for field in fields.values() {
                                    self.substitute_checked(
                                        &field.ty,
                                        &type_subst,
                                        &const_subst,
                                        span,
                                    );
                                }
                            }
                        }
                    }
                }
            }
            NominalKind::Extern => {}
        }
    }
}

fn contract_ref_from_bounds(bounds: &[ContractRef]) -> ContractRef {
    match bounds {
        [bound] => bound.clone(),
        _ => ContractRef::Intersection(bounds.to_vec()),
    }
}

impl TypeChecker {
    pub(super) fn optional_chain_inner_type(&mut self, ty: &Type, span: Span) -> Type {
        if matches!(ty, Type::Infer) {
            return Type::Infer;
        }
        if let Some(inner) = self.decls.semantic_option_inner(ty) {
            return inner.clone();
        }
        self.push_error(TypeError::OptionalChainingOnNonOptional {
            span: self.error_span(span),
        });
        ty.clone()
    }

    pub(super) fn optional_chain_result_type(&self, ty: Type) -> Type {
        if matches!(ty, Type::Infer | Type::Void) || self.decls.semantic_option_inner(&ty).is_some()
        {
            return ty;
        }
        self.decls.semantic_option_of(ty)
    }
}

pub(crate) fn check_with_modules(
    program: &Program,
    resolved: &ResolveResult,
    externs: RawExterns,
    config: TypecheckConfig,
) -> TypecheckOutput {
    match typechecker_for_modules(program, resolved, externs, config) {
        Ok(tc) => tc.into_output(),
        Err(errors) => TypecheckOutput::failed(errors, vec![], vec![]),
    }
}

fn typechecker_for_modules(
    program: &Program,
    resolved: &ResolveResult,
    externs: RawExterns,
    config: TypecheckConfig,
) -> Result<TypeChecker, Vec<TypeError>> {
    let mut decls = DeclarationIndex::from_root_and_modules(program, resolved, &externs);
    if decls.has_errors() {
        return Err(decl_errors(decls.errors()));
    }
    let catalog = crate::externs::catalog::build_catalog(externs, &decls).map_err(|errors| {
        errors
            .into_iter()
            .map(TypeError::ExternCatalog)
            .collect::<Vec<_>>()
    })?;
    decls.sync_extern_headers(&catalog);

    let mut tc = TypeChecker::new(decls, catalog, config);
    let root_scope = ModuleScope::from_module_id(&resolved.root);
    let root_source = resolved.root_source;
    tc.current_module = root_scope.clone();
    tc.module_sources.insert(root_scope.clone(), root_source);
    tc.with_current_module(&root_scope, |tc| {
        tc.collect_const_decls(&root_scope, program);
        collect_callable_templates(&root_scope, program, tc);
    });

    let mut module_bodies = vec![];
    for group in &resolved.module_groups {
        for module in group {
            if module.key == resolved.root {
                continue;
            }
            let scope = ModuleScope::from_module_id(&module.key);
            let program = Rc::new(module.program.clone());
            tc.module_sources.insert(scope.clone(), module.source);
            tc.module_programs
                .insert(scope.clone(), Rc::clone(&program));
            tc.with_current_module(&scope, |tc| {
                tc.collect_const_decls(&scope, program.as_ref());
                collect_callable_templates(&scope, program.as_ref(), tc);
            });
            module_bodies.push((scope, program));
        }
    }

    tc.with_current_module(&root_scope, |tc| tc.eval_module_consts(&root_scope));
    tc.finalize_declarations();
    tc.seed_global_types();
    check_finite_size_cycles(&mut tc);
    tc.with_current_module(&root_scope, |tc| {
        check_decl_param_order(program, tc);
        check_infer_return_decls(program, tc);
    });
    for (module, program) in &module_bodies {
        tc.with_current_module(module, |tc| {
            check_decl_param_order(program.as_ref(), tc);
            check_infer_return_decls(program.as_ref(), tc);
        });
    }
    globals::check_global_initializers(&root_scope, program, &mut tc);
    for (module, program) in &module_bodies {
        globals::check_global_initializers(module, program.as_ref(), &mut tc);
    }
    tc.sync_global_types();
    validate_public_value_surfaces(&tc.decls, &mut tc.errors);
    if !tc.errors.is_empty() {
        return Ok(tc);
    }
    push_source_scope(&mut tc);
    register_declarations(program, &mut tc);
    check_stmts(&program.stmts, &mut tc);
    tc.pop_scope();

    for (module, program) in module_bodies {
        check_module_bodies(&module, program.as_ref(), &mut tc);
    }

    Ok(tc)
}

fn decl_errors(errors: &[DeclError]) -> Vec<TypeError> {
    errors.iter().cloned().map(TypeError::Decl).collect()
}

#[derive(Clone)]
struct CheckedType {
    ty: Type,
    handle: TypeHandle,
    contains_extern_any: bool,
}

#[derive(Clone)]
struct TryCarrier {
    kind: TryCarrierKind,
    nominal: NominalType,
    success: Type,
    error: Option<Type>,
}

impl TryCarrier {
    fn operand_handle(&self, success: TypeHandle, tc: &mut TypeChecker) -> TypeHandle {
        let mut args = vec![success];
        if let Some(error) = &self.error {
            args.push(tc.type_handle(error));
        }
        tc.solver.nominal_handle(&self.nominal, args)
    }

    fn validate_residual(&self, operand: &Self, span: Span, tc: &mut TypeChecker) -> bool {
        let (Some(expected), Some(found)) = (&self.error, &operand.error) else {
            return true;
        };
        if expected == found {
            return true;
        }
        tc.push_error(TypeError::TryResultErrorMismatch {
            expected: expected.clone(),
            found: found.clone(),
            span: tc.error_span(span),
        });
        false
    }
}

struct TryOperandHint {
    success: TypeHandle,
    operand_expected: TypeHandle,
    success_matches_result_error: bool,
}

fn checked_type(ty: Type, tc: &TypeChecker) -> CheckedType {
    CheckedType {
        handle: tc.type_handle(&ty),
        ty,
        contains_extern_any: false,
    }
}

fn checked_void(tc: &TypeChecker) -> CheckedType {
    checked_type(Type::Void, tc)
}

fn join_checked(
    left: CheckedType,
    left_span: Span,
    right: CheckedType,
    right_span: Span,
    tc: &mut TypeChecker,
) -> CheckedType {
    match (left.ty.is_void(), right.ty.is_void()) {
        (true, true) => return checked_void(tc),
        (true, false) => {
            tc.push_error(TypeError::TypeMismatch {
                expected: right.ty,
                found: Type::Void,
                span: tc.error_span(left_span),
            });
            return checked_void(tc);
        }
        (false, true) => {
            tc.push_error(TypeError::TypeMismatch {
                expected: left.ty,
                found: Type::Void,
                span: tc.error_span(right_span),
            });
            return checked_void(tc);
        }
        (false, false) => {}
    }
    let result = tc.fresh_temp_handle(right_span);
    let contains_extern_any = left.contains_extern_any || right.contains_extern_any;
    tc.expect_assignable(left_span, left.handle, result.clone());
    tc.expect_assignable(right_span, right.handle, result.clone());
    tc.solve_constraints();
    CheckedType {
        ty: tc.handle_type(&result),
        handle: result,
        contains_extern_any,
    }
}

fn check_expr_checked(expr: &ExprNode, tc: &mut TypeChecker) -> CheckedType {
    check_expr_checked_with_hint(expr, None, tc)
}

fn reject_if_without_else_value(expr: &ExprNode, tc: &mut TypeChecker) {
    if let ExprKind::If(node) = &expr.node.kind
        && node.node.else_block.is_none()
    {
        tc.push_error(TypeError::IfWithoutElseValue {
            span: tc.error_span(expr.span),
        });
    }
}

pub(in crate::typecheck) fn check_value_expr_checked_with_hint(
    expr: &ExprNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let checked = check_expr_checked_with_hint(expr, expected, tc);
    reject_if_without_else_value(expr, tc);
    checked
}

fn checked_from_type(expr: &ExprNode, ty: Type, tc: &mut TypeChecker) -> CheckedType {
    if matches!(ty, Type::Infer) {
        return checked_poison(expr, tc);
    }
    let handle = tc.set_type(expr.node.id, ty.clone(), expr.span);
    CheckedType {
        ty,
        handle,
        contains_extern_any: false,
    }
}

fn checked_poison(expr: &ExprNode, tc: &mut TypeChecker) -> CheckedType {
    let handle = tc.set_poison_type(expr.node.id, tc.error_span(expr.span));
    CheckedType {
        ty: Type::Infer,
        handle,
        contains_extern_any: false,
    }
}

fn checked_from_handle(expr: &ExprNode, handle: TypeHandle, tc: &mut TypeChecker) -> CheckedType {
    let handle = tc.set_type_from_handle(expr.node.id, expr.span, &handle);
    let ty = tc.handle_type(&handle);
    CheckedType {
        ty,
        handle,
        contains_extern_any: false,
    }
}

fn checked_from_checked(
    expr: &ExprNode,
    checked: CheckedType,
    tc: &mut TypeChecker,
) -> CheckedType {
    let handle = tc.set_type_from_handle(expr.node.id, expr.span, &checked.handle);
    let ty = tc.handle_type(&handle);
    CheckedType {
        ty,
        handle,
        contains_extern_any: checked.contains_extern_any,
    }
}

fn solve_and_checked_from_handle(
    expr: &ExprNode,
    handle: TypeHandle,
    tc: &mut TypeChecker,
) -> CheckedType {
    tc.solve_constraints();
    checked_from_handle(expr, handle, tc)
}

fn check_expected(expr: &ExprNode, expected: TypeHandle, tc: &mut TypeChecker) -> CheckedType {
    let checked = check_value_expr_checked_with_hint(expr, Some(expected.clone()), tc);
    tc.expect_assignable_expr(expr.span, expr.node.id, checked.handle.clone(), expected);
    checked
}

pub(in crate::typecheck) fn validate_const_expr_type(
    expr: &ExprNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> Result<Type, TypeError> {
    let error_count = tc.errors.len();
    let checked = check_value_expr_checked_with_hint(expr, expected.clone(), tc);
    if let Some(expected) = expected {
        tc.expect_assignable_expr(expr.span, expr.node.id, checked.handle, expected);
        tc.solve_constraints();
    }
    if tc.errors.len() == error_count {
        return Ok(checked.ty);
    }
    let error = tc.errors[error_count].clone();
    tc.errors.truncate(error_count);
    Err(error)
}

fn check_arg_count(
    args: &[ExprNode],
    expected: usize,
    call_span: Span,
    tc: &mut TypeChecker,
) -> bool {
    check_arg_range(args, expected, expected, call_span, tc)
}

fn check_arg_range(
    args: &[ExprNode],
    min: usize,
    max: usize,
    call_span: Span,
    tc: &mut TypeChecker,
) -> bool {
    if (min..=max).contains(&args.len()) {
        return true;
    }

    if min == max {
        tc.push_error(TypeError::WrongArgCount {
            expected: max,
            found: args.len(),
            span: tc.error_span(call_span),
        });
    } else {
        tc.push_error(TypeError::WrongArgRange {
            min,
            max,
            found: args.len(),
            span: tc.error_span(call_span),
        });
    }
    for arg in args {
        check_expr_checked(arg, tc);
    }
    false
}

fn check_expr_checked_with_hint(
    expr: &ExprNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    match &expr.node.kind {
        ExprKind::Lit(Lit::Nil) => match expected {
            Some(expected) => {
                let nil = tc.fresh_nil_handle(expr.span);
                tc.expect_assignable_expr(expr.span, expr.node.id, nil, expected.clone());
                checked_from_handle(expr, expected, tc)
            }
            None => {
                let handle = tc.set_nil_type(expr.node.id, expr.span);
                CheckedType {
                    ty: Type::Infer,
                    handle,
                    contains_extern_any: false,
                }
            }
        },
        ExprKind::Lit(lit) => checked_from_type(expr, type_from_lit(lit), tc),
        ExprKind::TypeSubject(ty) => {
            if let Some(ty) = tc.resolve_type_subject(ty, expr.span) {
                tc.push_error(TypeError::TypeUsedAsValue {
                    ty,
                    span: tc.error_span(expr.span),
                });
            }
            checked_from_type(expr, Type::Infer, tc)
        }
        ExprKind::Ident(name) => match tc.lookup_local_symbol_checked(*name, expr.span) {
            LocalSymbolLookup::Found(LocalSymbol::Value(ref info), depth) => {
                let value = tc.local_value_from_info(info.clone(), depth);
                tc.record_local_read(expr.node.id, &value);
                let access = tc.local_value_access(&value);
                tc.check_mut_downcast_root_use(Some(*name), &access.identity, expr.span);
                if let Some((_, value_name, value)) = tc.lookup_named_value(*name)
                    && !matches!(value, ValueDecl::Global(_))
                {
                    tc.warn_named_value_deprecated(&value, value_name, expr.span);
                }
                let fallback = tc.solver.local_type_to_type(info.type_id);
                if fallback != Type::Infer || info.const_value.is_some() {
                    checked_from_handle(expr, tc.local_handle(info.type_id), tc)
                } else {
                    match tc.eval_visible_const(*name, expr.span) {
                        Some(Ok(value)) => {
                            checked_from_type(expr, const_eval::const_type(&value), tc)
                        }
                        Some(Err(err)) => {
                            tc.push_error(err);
                            checked_from_type(expr, Type::Infer, tc)
                        }
                        None => checked_from_handle(expr, tc.local_handle(info.type_id), tc),
                    }
                }
            }
            LocalSymbolLookup::Found(LocalSymbol::Callable(info), _) => {
                match info.value_error(*name, tc.error_span(expr.span)) {
                    Some(error) => {
                        tc.push_error(error);
                        checked_from_type(expr, Type::Infer, tc)
                    }
                    None => checked_from_handle(expr, tc.local_handle(info.type_id), tc),
                }
            }
            LocalSymbolLookup::Blocked(error) => {
                tc.push_error(*error);
                checked_from_type(expr, Type::Infer, tc)
            }
            LocalSymbolLookup::Missing => match tc.lookup_named_value(*name) {
                Some((module, value_name, value)) => {
                    tc.warn_named_value_deprecated(&value, value_name, expr.span);
                    if let ValueDecl::Global(sig) = &value {
                        let checked = checked_from_handle(expr, tc.global_handle(&sig.key), tc);
                        let value = place::global_value(sig, checked);
                        place::record_value_read(expr.node.id, &value, tc);
                        return value.checked;
                    }
                    if let Some(callee) = tc.decls.callable_for_value(&ResolvedValue {
                        module: module.clone(),
                        name: value_name,
                        visibility: Visibility::Private,
                        decl: value.clone(),
                    }) && callee.def.sig.ret.is_infer()
                    {
                        tc.push_error(TypeError::InferReturnValue {
                            span: tc.error_span(expr.span),
                        });
                        return checked_from_type(expr, Type::Infer, tc);
                    }
                    match value {
                        ValueDecl::Const(_) => match tc.eval_visible_const(*name, expr.span) {
                            Some(Ok(value)) => {
                                checked_from_type(expr, const_eval::const_type(&value), tc)
                            }
                            Some(Err(err)) => {
                                tc.push_error(err);
                                checked_from_type(expr, Type::Infer, tc)
                            }
                            None => checked_from_type(expr, Type::Infer, tc),
                        },
                        ValueDecl::Func(sig) => checked_from_type(expr, sig.ty, tc),
                        ValueDecl::Global(_) => unreachable!("global handled above"),
                    }
                }
                None => {
                    if let Some(ty) = tc.visible_type_subject(*name, expr.span) {
                        tc.push_error(TypeError::TypeUsedAsValue {
                            ty,
                            span: tc.error_span(expr.span),
                        });
                    } else {
                        tc.push_error(TypeError::UndefinedVariable {
                            name: *name,
                            span: tc.error_span(expr.span),
                        });
                    }
                    checked_from_type(expr, Type::Infer, tc)
                }
            },
        },
        ExprKind::Binary(bin_node) => {
            checked_from_checked(expr, check_binary(expr.node.id, bin_node, expected, tc), tc)
        }
        ExprKind::Unary(unary_node) => {
            checked_from_checked(expr, check_unary(expr.node.id, unary_node, tc), tc)
        }
        ExprKind::Try(try_node) => {
            checked_from_checked(expr, check_try(try_node, expected, tc), tc)
        }
        ExprKind::Block(block_node) => {
            let checked = check_block_checked_with_hint(block_node, expected, tc);
            if let Some(tail) = &block_node.node.tail {
                tc.closure.copy_expr_flow(tail.node.id, expr.node.id);
            }
            checked_from_checked(expr, checked, tc)
        }
        ExprKind::If(if_node) => {
            let checked = check_if_checked_with_hint(if_node, expected, tc);
            if let Some(tail) = &if_node.node.then_block.node.tail {
                tc.closure.copy_expr_flow(tail.node.id, expr.node.id);
            }
            if let Some(else_block) = &if_node.node.else_block
                && let Some(tail) = &else_block.node.tail
            {
                tc.closure.copy_expr_flow(tail.node.id, expr.node.id);
            }
            checked_from_checked(expr, checked, tc)
        }
        ExprKind::Ternary(ternary_node) => {
            let checked = check_ternary_checked_with_hint(ternary_node, expected, tc);
            tc.closure
                .copy_expr_flow(ternary_node.node.then_expr.node.id, expr.node.id);
            tc.closure
                .copy_expr_flow(ternary_node.node.else_expr.node.id, expr.node.id);
            checked_from_checked(expr, checked, tc)
        }
        ExprKind::Assign(assign_node) => {
            check_assign(expr.node.id, assign_node, tc);
            checked_from_type(expr, Type::Void, tc)
        }
        ExprKind::StructLiteral(lit) => check_struct_lit_hint(expr, lit, expected, tc),
        ExprKind::InferredEnum(node) => check_inferred_enum_hint(expr, node, expected, tc),
        ExprKind::Field(_) | ExprKind::Call(_) => {
            let chain = collect_postfix_chain(expr).expect("postfix chain");
            check_postfix_chain(&chain, expr, expected.as_ref(), tc)
        }
        ExprKind::Tuple(elems) => check_tuple_checked_with_hint(expr, elems, expected, tc),
        ExprKind::TupleIndex(node) => check_tuple_index(expr, node, tc),
        ExprKind::Index(node) => check_index_expr(expr, node, tc),
        ExprKind::ArrayLiteral(lit) => check_array_lit_hint(expr, lit, expected, tc),
        ExprKind::ArrayFill(fill) => check_array_fill_hint(expr, fill, expected, tc),
        ExprKind::IfLet(if_let_node) => {
            let checked = pattern::check_if_let_checked_with_hint(if_let_node, expected, tc);
            if let Some(tail) = &if_let_node.node.then_block.node.tail {
                tc.closure.copy_expr_flow(tail.node.id, expr.node.id);
            }
            if let Some(else_block) = &if_let_node.node.else_block
                && let Some(tail) = &else_block.node.tail
            {
                tc.closure.copy_expr_flow(tail.node.id, expr.node.id);
            }
            checked_from_checked(expr, checked, tc)
        }
        ExprKind::Match(match_node) => {
            let checked = check_match_checked_with_hint(match_node, expected, tc);
            for arm in &match_node.node.arms {
                tc.closure
                    .copy_expr_flow(arm.node.body.node.id, expr.node.id);
            }
            checked_from_checked(expr, checked, tc)
        }
        ExprKind::StringInterp(parts) => check_string_interp(expr, parts, tc),
        ExprKind::MapLiteral(lit) => check_map_lit_hint(expr, lit, expected, tc),
        ExprKind::IntrinsicCall(call) => check_intrinsic_call(expr, call, tc),
        ExprKind::Range(range) => check_range_expr(expr, range, expected, tc),
        ExprKind::Cast(cast) => convert::check_cast_expr(expr, cast, tc),
        ExprKind::ExactDowncast(downcast) => check_exact_downcast_expr(expr, downcast, tc),
        ExprKind::Lambda(lambda) => closure::check_lambda_expr(expr, lambda, expected.as_ref(), tc),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum IntrinsicKind {
    Predicate,
    Warning,
    Error,
    Log,
    File,
    Line,
    Function,
}

fn intrinsic_kind(name: Ident) -> Option<IntrinsicKind> {
    Some(match name.as_str() {
        "profile" | "os" | "arch" | "feature" => IntrinsicKind::Predicate,
        "warning" => IntrinsicKind::Warning,
        "error" => IntrinsicKind::Error,
        "log" => IntrinsicKind::Log,
        "file" => IntrinsicKind::File,
        "line" => IntrinsicKind::Line,
        "function" => IntrinsicKind::Function,
        _ => return None,
    })
}

fn check_intrinsic_call(
    expr: &ExprNode,
    call: &IntrinsicCallNode,
    tc: &mut TypeChecker,
) -> CheckedType {
    let name = call.node.name;
    let Some(kind) = intrinsic_kind(name) else {
        tc.push_error(TypeError::UnknownIntrinsic {
            name,
            span: tc.error_span(call.span),
        });
        return checked_from_type(expr, Type::Infer, tc);
    };

    match kind {
        IntrinsicKind::Predicate => {
            if let Some(arg) = intrinsic_ident_arg(name, &call.node.args, call.span, tc) {
                eval_intrinsic_predicate(name, arg, call.span, tc);
            }
            checked_from_type(expr, Type::Bool, tc)
        }
        IntrinsicKind::Warning | IntrinsicKind::Log | IntrinsicKind::Error => {
            let Some(message) = intrinsic_string_arg(name, &call.node.args, call.span, tc) else {
                return checked_from_type(expr, Type::Void, tc);
            };
            if kind == IntrinsicKind::Error {
                tc.push_error(TypeError::CompileError {
                    message,
                    span: tc.error_span(call.span),
                });
            } else {
                tc.push_warning(CompileWarning {
                    message,
                    span: tc.source_span(call.span),
                });
            }
            checked_from_type(expr, Type::Void, tc)
        }
        IntrinsicKind::File | IntrinsicKind::Function => {
            check_intrinsic_arg_count(name, &call.node.args, 0, call.span, tc);
            checked_from_type(expr, Type::String, tc)
        }
        IntrinsicKind::Line => {
            check_intrinsic_arg_count(name, &call.node.args, 0, call.span, tc);
            checked_from_type(expr, Type::Int, tc)
        }
    }
}

fn intrinsic_ident_arg(
    name: Ident,
    args: &[ExprNode],
    span: Span,
    tc: &mut TypeChecker,
) -> Option<Ident> {
    if !check_intrinsic_arg_count(name, args, 1, span, tc) {
        return None;
    }
    let ExprKind::Ident(arg) = args[0].node.kind else {
        tc.push_error(TypeError::IntrinsicExpectedIdent {
            name,
            span: tc.error_span(args[0].span),
        });
        return None;
    };
    Some(arg)
}

fn intrinsic_string_arg(
    name: Ident,
    args: &[ExprNode],
    span: Span,
    tc: &mut TypeChecker,
) -> Option<String> {
    if !check_intrinsic_arg_count(name, args, 1, span, tc) {
        return None;
    }
    let ExprKind::Lit(Lit::String(message)) = &args[0].node.kind else {
        tc.push_error(TypeError::IntrinsicExpectedString {
            name,
            span: tc.error_span(args[0].span),
        });
        return None;
    };
    Some(message.clone())
}

fn check_intrinsic_arg_count(
    name: Ident,
    args: &[ExprNode],
    expected: usize,
    span: Span,
    tc: &mut TypeChecker,
) -> bool {
    if args.len() == expected {
        return true;
    }
    tc.push_error(TypeError::IntrinsicArgCount {
        name,
        expected,
        found: args.len(),
        span: tc.error_span(span),
    });
    false
}

fn eval_intrinsic_predicate(
    name: Ident,
    arg: Ident,
    span: Span,
    tc: &mut TypeChecker,
) -> Option<bool> {
    match tc
        .config
        .context
        .eval_predicate(name.as_str(), arg.as_str())
    {
        Ok(value) => Some(value),
        Err(PredicateError::UnknownValue) => {
            tc.push_error(TypeError::UnknownIntrinsicValue {
                predicate: name,
                value: arg,
                span: tc.error_span(span),
            });
            None
        }
        Err(PredicateError::UnknownPredicate) => None,
    }
}

fn intrinsic_bool_value(expr: &ExprNode, tc: &TypeChecker) -> Option<bool> {
    let ExprKind::IntrinsicCall(call) = &expr.node.kind else {
        return None;
    };
    let name = call.node.name;
    if intrinsic_kind(name) != Some(IntrinsicKind::Predicate) || call.node.args.len() != 1 {
        return None;
    }
    let ExprKind::Ident(arg) = call.node.args[0].node.kind else {
        return None;
    };
    tc.config
        .context
        .eval_predicate(name.as_str(), arg.as_str())
        .ok()
}

fn try_operand_hint(
    enclosing: &TryCarrier,
    expected: Option<TypeHandle>,
    span: Span,
    tc: &mut TypeChecker,
) -> TryOperandHint {
    let expected_success = expected.as_ref().map(|handle| tc.handle_type(handle));
    let optional_context = matches!(enclosing.kind, TryCarrierKind::Option)
        && expected_success
            .as_ref()
            .is_some_and(|ty| tc.decls.semantic_option_inner(ty).is_some());
    let success_matches_result_error = enclosing
        .error
        .as_ref()
        .is_some_and(|error| expected_success.as_ref() == Some(error));
    let success = if optional_context {
        tc.fresh_temp_handle(span)
    } else {
        expected.unwrap_or_else(|| tc.fresh_temp_handle(span))
    };

    TryOperandHint {
        operand_expected: enclosing.operand_handle(success.clone(), tc),
        success,
        success_matches_result_error,
    }
}

fn try_operand_recovery_ty(
    expr: &ExprNode,
    operand_ty: &Type,
    operand: &TypeHandle,
    tc: &TypeChecker,
) -> Option<Type> {
    if operand_ty != &Type::Infer {
        return None;
    }
    let partial = tc.solver.handle_to_partial_type(operand);
    if tc.try_carrier_parts(&partial).is_some() {
        return Some(partial);
    }
    try_operand_field_carrier_ty(expr, tc)
}

fn try_operand_field_carrier_ty(expr: &ExprNode, tc: &TypeChecker) -> Option<Type> {
    let chain = collect_postfix_chain(expr)?;
    let PostfixStep::Field { node, id } = chain.steps.first()? else {
        return None;
    };
    let field = tc.solver.expr_handle(*id);
    let carrier_ty = tc.solver.handle_to_partial_type(&field);
    let key = tc.decls.key_for_type(&carrier_ty)?;
    let schema = tc.decls.enum_schema(&key)?;
    schema.variants.get(&node.node.field)?;

    let Type::Nominal(mut nominal) = carrier_ty else {
        return None;
    };
    if nominal.type_args.len() != schema.generics.type_params.len() {
        nominal.type_args = vec![Type::Infer; schema.generics.type_params.len()];
    }
    if nominal.const_args.len() != schema.generics.const_params.len() {
        return None;
    }

    let ty = Type::Nominal(nominal);
    tc.try_carrier_parts(&ty)?;
    Some(ty)
}

fn take_try_operand_error(
    start: usize,
    end: usize,
    operand_span: Span,
    try_span: Span,
    enclosing: &TryCarrier,
    hint: &TryOperandHint,
    found_ty: Option<&Type>,
    tc: &mut TypeChecker,
) -> Option<TypeError> {
    let end = end.min(tc.errors.len());
    let root_span = tc.error_span(operand_span);
    if let Some((index, error)) = (start..end).find_map(|index| {
        let TypeError::TypeMismatch {
            expected,
            found,
            span,
        } = &tc.errors[index]
        else {
            return None;
        };
        try_operand_error_from_mismatch(
            expected, found, *span, root_span, try_span, enclosing, hint, tc,
        )
        .map(|error| (index, error))
    }) {
        tc.errors.remove(index);
        return Some(error);
    }

    let found = try_carrier_mismatch_ty(found_ty?, enclosing, tc)?;
    remove_root_try_operand_error(start, end, operand_span, enclosing, tc);
    Some(try_invalid_carrier_error(enclosing, found, try_span, tc))
}

fn try_operand_error_from_mismatch(
    expected: &Type,
    found: &Type,
    mismatch_span: Option<SourceSpan>,
    root_span: Option<SourceSpan>,
    try_span: Span,
    enclosing: &TryCarrier,
    hint: &TryOperandHint,
    tc: &TypeChecker,
) -> Option<TypeError> {
    if let Some(error) = &enclosing.error
        && expected == error
        && !hint.success_matches_result_error
    {
        return Some(TypeError::TryResultErrorMismatch {
            expected: error.clone(),
            found: found.clone(),
            span: tc.error_span(try_span),
        });
    }

    if mismatch_span != root_span {
        return None;
    }
    let found = try_carrier_mismatch_ty(found, enclosing, tc)?;
    Some(try_invalid_carrier_error(enclosing, found, try_span, tc))
}

fn try_carrier_mismatch_ty(found: &Type, enclosing: &TryCarrier, tc: &TypeChecker) -> Option<Type> {
    let found_carrier = tc.try_carrier_parts(found)?;
    (found_carrier.kind != enclosing.kind).then(|| found.clone())
}

fn remove_root_try_operand_error(
    start: usize,
    end: usize,
    span: Span,
    enclosing: &TryCarrier,
    tc: &mut TypeChecker,
) {
    let end = end.min(tc.errors.len());
    let root_span = tc.error_span(span);
    let index = (start..end).find(|index| match &tc.errors[*index] {
        TypeError::TypeMismatch {
            expected,
            found,
            span,
        } if *span == root_span => {
            matches!(found, Type::Infer)
                || matches!(expected, Type::Infer)
                || try_carrier_mismatch_ty(found, enclosing, tc).is_some()
                || try_carrier_mismatch_ty(expected, enclosing, tc).is_some()
        }
        TypeError::CannotInferType { span } | TypeError::UnboundGenericParam { span, .. } => {
            *span == root_span
        }
        _ => false,
    });
    if let Some(index) = index {
        tc.errors.remove(index);
    }
}

fn try_invalid_carrier_error(
    enclosing: &TryCarrier,
    found: Type,
    span: Span,
    tc: &TypeChecker,
) -> TypeError {
    TypeError::TryOnInvalidCarrier {
        expected: enclosing.kind,
        found,
        span: tc.error_span(span),
    }
}

fn push_try_invalid_carrier(enclosing: &TryCarrier, found: Type, span: Span, tc: &mut TypeChecker) {
    tc.push_error(try_invalid_carrier_error(enclosing, found, span, tc));
}

fn check_try(
    try_node: &TryNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let return_ty = tc.return_type().cloned();
    let enclosing = return_ty.as_ref().and_then(|ty| tc.try_carrier_parts(ty));

    if tc.in_global_initializer() {
        tc.push_error(TypeError::CompileError {
            message: "try is not allowed in runtime global initializers".to_string(),
            span: tc.error_span(try_node.span),
        });
        check_value_expr_checked_with_hint(&try_node.node.expr, None, tc);
        return checked_type(Type::Infer, tc);
    }

    if tc.in_defer() {
        tc.push_error(TypeError::TryInsideDefer {
            span: tc.error_span(try_node.span),
        });
    }
    if enclosing.is_none() {
        tc.push_error(TypeError::TryOutsideCarrierFunction {
            found: return_ty.clone(),
            span: tc.error_span(try_node.span),
        });
    }

    let hint = enclosing
        .as_ref()
        .map(|enclosing| try_operand_hint(enclosing, expected, try_node.span, tc));
    let operand_error_start = tc.errors.len();
    let operand = check_value_expr_checked_with_hint(
        &try_node.node.expr,
        hint.as_ref().map(|hint| hint.operand_expected.clone()),
        tc,
    );
    tc.solve_constraints();
    let operand_ty = tc.handle_type(&operand.handle);
    let operand_error_end = tc.errors.len();
    let operand_recovery_ty =
        try_operand_recovery_ty(&try_node.node.expr, &operand_ty, &operand.handle, tc);

    if operand_ty == Type::Infer {
        if operand_error_start != operand_error_end {
            if let (Some(enclosing), Some(hint)) = (&enclosing, &hint)
                && let Some(error) = take_try_operand_error(
                    operand_error_start,
                    operand_error_end,
                    try_node.node.expr.span,
                    try_node.span,
                    enclosing,
                    hint,
                    operand_recovery_ty.as_ref(),
                    tc,
                )
            {
                tc.push_error(error);
            }
            return checked_type(Type::Infer, tc);
        }
        if let (Some(enclosing), Some(found)) = (&enclosing, operand_recovery_ty.as_ref())
            && let Some(found) = try_carrier_mismatch_ty(found, enclosing, tc)
        {
            push_try_invalid_carrier(enclosing, found, try_node.span, tc);
            return checked_type(Type::Infer, tc);
        }
        if let Some(hint) = &hint {
            let ty = tc.handle_type(&hint.success);
            return CheckedType {
                contains_extern_any: type_closure_facts(&ty).contains_any,
                handle: hint.success.clone(),
                ty,
            };
        }
    }

    let Some(enclosing) = enclosing else {
        return checked_type(Type::Infer, tc);
    };
    let Some(operand_carrier) = tc.try_carrier_parts(&operand_ty) else {
        push_try_invalid_carrier(&enclosing, operand_ty, try_node.span, tc);
        return checked_type(Type::Infer, tc);
    };
    if operand_carrier.kind != enclosing.kind {
        remove_root_try_operand_error(
            operand_error_start,
            operand_error_end,
            try_node.node.expr.span,
            &enclosing,
            tc,
        );
        push_try_invalid_carrier(&enclosing, operand_ty, try_node.span, tc);
        return checked_type(Type::Infer, tc);
    }
    if !enclosing.validate_residual(&operand_carrier, try_node.span, tc) {
        return checked_type(Type::Infer, tc);
    }

    let mut checked = checked_type(operand_carrier.success.clone(), tc);
    checked.contains_extern_any = type_closure_facts(&checked.ty).contains_any;
    checked
}

fn check_binary(
    expr_id: ExprId,
    bin: &BinaryNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    if bin.node.op == BinaryOp::Coalesce {
        return check_coalesce(&bin.node.left, &bin.node.right, bin.span, expected, tc);
    }

    let left = check_expr_checked(&bin.node.left, tc);
    let right = check_expr_checked(&bin.node.right, tc);
    check_binary_checked(
        expr_id,
        bin.node.op,
        &bin.node.left,
        left,
        &bin.node.right,
        right,
        bin.span,
        tc,
    )
}

fn check_coalesce(
    left_expr: &ExprNode,
    right_expr: &ExprNode,
    span: Span,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let expected_ty = expected.as_ref().map(|handle| tc.handle_type(handle));
    let left_expected = expected_ty
        .as_ref()
        .map(|ty| tc.decls.semantic_option_of(ty.clone()))
        .map(|ty| tc.type_handle(&ty));
    let left = check_value_expr_checked_with_hint(left_expr, left_expected, tc);
    let Some(inner) = tc.decls.semantic_option_inner(&left.ty).cloned() else {
        if matches!(left.ty, Type::Infer) {
            let mut right = check_expr_checked(right_expr, tc);
            right.contains_extern_any |= left.contains_extern_any;
            return checked_from_checked(right_expr, right, tc);
        }
        tc.push_error(TypeError::InvalidOperand {
            op: BinaryOp::Coalesce.to_string(),
            operand_type: left.ty,
            span: tc.error_span(span),
        });
        check_expr_checked(right_expr, tc);
        return checked_type(Type::Infer, tc);
    };

    let mut right = check_expr_checked(right_expr, tc);
    right.contains_extern_any |= left.contains_extern_any;
    if tc.decls.semantic_option_inner(&right.ty) == Some(&inner) {
        return right;
    }

    let result = tc.type_handle(&inner);
    tc.expect_assignable_expr(
        right_expr.span,
        right_expr.node.id,
        right.handle,
        result.clone(),
    );
    tc.solve_constraints();
    CheckedType {
        ty: tc.handle_type(&result),
        handle: result,
        contains_extern_any: right.contains_extern_any,
    }
}

fn check_binary_checked(
    expr_id: ExprId,
    op: BinaryOp,
    left_expr: &ExprNode,
    left: CheckedType,
    right_expr: &ExprNode,
    right: CheckedType,
    span: Span,
    tc: &mut TypeChecker,
) -> CheckedType {
    if tc.checked_is_poison(&left) || tc.checked_is_poison(&right) {
        return checked_type(Type::Infer, tc);
    }
    match builtin_binary_type(op, &left.ty, &right.ty, tc) {
        Ok(ty) => checked_type(ty, tc),
        Err(failure) => {
            extern_ops::check_binary(expr_id, op, left_expr, &left, right_expr, &right, span, tc)
                .unwrap_or_else(|| checked_type(emit_binary_failure(failure, span, tc), tc))
        }
    }
}

#[derive(Debug)]
enum BinaryTypeFailure {
    InvalidOperand {
        op: String,
        operand_type: Type,
        fallback: Type,
    },
    TypeMismatch {
        expected: Type,
        found: Type,
        fallback: Type,
    },
    NotEquatable {
        ty: Type,
        fallback: Type,
    },
}

fn equatable_type(ty: &Type, tc: &TypeChecker) -> bool {
    !matches!(ty, Type::Slice { .. })
        && !type_contains_dyn_value(ty, &tc.decls, &mut HashSet::new())
}

fn builtin_binary_type(
    op: BinaryOp,
    left_ty: &Type,
    right_ty: &Type,
    tc: &TypeChecker,
) -> Result<Type, BinaryTypeFailure> {
    let same = left_ty == right_ty;
    match op {
        BinaryOp::Add => {
            let string_pair = left_ty.is_str() && right_ty.is_str();
            let string_lhs = left_ty.is_str() && right_ty.is_stringable();
            let string_rhs = right_ty.is_str() && left_ty.is_stringable();
            if string_pair || string_lhs || string_rhs {
                return Ok(Type::String);
            }
            if left_ty.is_num() && same {
                return Ok(left_ty.clone());
            }
            Err(BinaryTypeFailure::InvalidOperand {
                op: op.to_string(),
                operand_type: right_ty.clone(),
                fallback: Type::Infer,
            })
        }
        BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => {
            if left_ty.is_num() && same {
                return Ok(left_ty.clone());
            }
            Err(BinaryTypeFailure::InvalidOperand {
                op: op.to_string(),
                operand_type: right_ty.clone(),
                fallback: Type::Infer,
            })
        }
        BinaryOp::Eq | BinaryOp::NotEq => {
            let extern_eq = same && tc.extern_type_id(left_ty).is_some();
            if extern_eq {
                Err(BinaryTypeFailure::InvalidOperand {
                    op: op.to_string(),
                    operand_type: right_ty.clone(),
                    fallback: Type::Infer,
                })
            } else if same && equatable_type(left_ty, tc) {
                Ok(Type::Bool)
            } else if same {
                Err(BinaryTypeFailure::NotEquatable {
                    ty: left_ty.clone(),
                    fallback: Type::Infer,
                })
            } else {
                Err(BinaryTypeFailure::TypeMismatch {
                    expected: left_ty.clone(),
                    found: right_ty.clone(),
                    fallback: Type::Bool,
                })
            }
        }
        BinaryOp::LessThan
        | BinaryOp::GreaterThan
        | BinaryOp::LessThanEq
        | BinaryOp::GreaterThanEq => {
            if (left_ty.is_num() || left_ty.is_str()) && same {
                Ok(Type::Bool)
            } else if same {
                Err(BinaryTypeFailure::InvalidOperand {
                    op: op.to_string(),
                    operand_type: left_ty.clone(),
                    fallback: Type::Bool,
                })
            } else {
                Err(BinaryTypeFailure::TypeMismatch {
                    expected: left_ty.clone(),
                    found: right_ty.clone(),
                    fallback: Type::Bool,
                })
            }
        }
        BinaryOp::And | BinaryOp::Or => {
            if left_ty.is_bool() && right_ty.is_bool() {
                Ok(Type::Bool)
            } else {
                let operand_type = if left_ty.is_bool() { right_ty } else { left_ty };
                Err(BinaryTypeFailure::InvalidOperand {
                    op: op.to_string(),
                    operand_type: operand_type.clone(),
                    fallback: Type::Bool,
                })
            }
        }
        BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::Xor | BinaryOp::Shl | BinaryOp::Shr => {
            if left_ty.is_int() && right_ty.is_int() {
                Ok(Type::Int)
            } else if same {
                Err(BinaryTypeFailure::InvalidOperand {
                    op: op.to_string(),
                    operand_type: left_ty.clone(),
                    fallback: Type::Int,
                })
            } else {
                Err(BinaryTypeFailure::TypeMismatch {
                    expected: Type::Int,
                    found: if left_ty.is_int() {
                        right_ty.clone()
                    } else {
                        left_ty.clone()
                    },
                    fallback: Type::Int,
                })
            }
        }
        BinaryOp::Coalesce => unreachable!("coalesce is checked before builtin binary dispatch"),
    }
}

fn emit_binary_failure(failure: BinaryTypeFailure, span: Span, tc: &mut TypeChecker) -> Type {
    match failure {
        BinaryTypeFailure::InvalidOperand {
            op,
            operand_type,
            fallback,
        } => {
            tc.push_error(TypeError::InvalidOperand {
                op,
                operand_type,
                span: tc.error_span(span),
            });
            fallback
        }
        BinaryTypeFailure::TypeMismatch {
            expected,
            found,
            fallback,
        } => {
            tc.push_error(TypeError::TypeMismatch {
                expected,
                found,
                span: tc.error_span(span),
            });
            fallback
        }
        BinaryTypeFailure::NotEquatable { ty, fallback } => {
            tc.push_error(TypeError::NotEquatable {
                ty,
                span: tc.error_span(span),
            });
            fallback
        }
    }
}

fn check_unary(expr_id: ExprId, unary: &UnaryNode, tc: &mut TypeChecker) -> CheckedType {
    let operand = check_expr_checked(&unary.node.expr, tc);
    if tc.checked_is_poison(&operand) {
        return checked_type(Type::Infer, tc);
    }
    match builtin_unary_type(unary.node.op, &operand.ty) {
        Ok(ty) => checked_type(ty, tc),
        Err(failure) => extern_ops::check_unary(expr_id, unary.node.op, &operand, tc)
            .unwrap_or_else(|| checked_type(emit_unary_failure(failure, unary.span, tc), tc)),
    }
}

#[derive(Debug)]
struct UnaryTypeFailure {
    op: String,
    operand_type: Type,
}

fn builtin_unary_type(op: UnaryOp, operand_ty: &Type) -> Result<Type, UnaryTypeFailure> {
    match op {
        UnaryOp::Neg => {
            if operand_ty.is_num() {
                Ok(operand_ty.clone())
            } else {
                Err(UnaryTypeFailure {
                    op: op.to_string(),
                    operand_type: operand_ty.clone(),
                })
            }
        }
        UnaryOp::Not => {
            if operand_ty.is_bool() {
                Ok(Type::Bool)
            } else {
                Err(UnaryTypeFailure {
                    op: op.to_string(),
                    operand_type: operand_ty.clone(),
                })
            }
        }
        UnaryOp::BitNot => {
            if operand_ty.is_int() {
                Ok(Type::Int)
            } else {
                Err(UnaryTypeFailure {
                    op: op.to_string(),
                    operand_type: operand_ty.clone(),
                })
            }
        }
    }
}

fn emit_unary_failure(failure: UnaryTypeFailure, span: Span, tc: &mut TypeChecker) -> Type {
    tc.push_error(TypeError::InvalidOperand {
        op: failure.op,
        operand_type: failure.operand_type,
        span: tc.error_span(span),
    });
    Type::Infer
}

#[derive(Clone, Copy)]
enum ConditionKind {
    If,
    Ternary,
    While,
}

fn condition_not_bool(kind: ConditionKind, found: Type, span: Option<SourceSpan>) -> TypeError {
    match kind {
        ConditionKind::If => TypeError::IfConditionNotBool { found, span },
        ConditionKind::Ternary => TypeError::TernaryConditionNotBool { found, span },
        ConditionKind::While => TypeError::WhileConditionNotBool { found, span },
    }
}

fn check_bool_condition(kind: ConditionKind, cond: CheckedType, span: Span, tc: &mut TypeChecker) {
    if cond.ty.is_bool() {
        return;
    }
    if cond.ty == Type::Infer {
        let bool_handle = tc.type_handle(&Type::Bool);
        tc.expect_assignable(span, cond.handle, bool_handle);
    } else {
        tc.push_error(condition_not_bool(kind, cond.ty, tc.error_span(span)));
    }
}

struct CheckedBranch {
    checked: CheckedType,
    span: Span,
    diverges: bool,
}

fn checked_branch_against_expected(
    branch: CheckedBranch,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let Some(expected) = expected else {
        return branch.checked;
    };
    tc.expect_assignable(branch.span, branch.checked.handle, expected.clone());
    tc.solve_constraints();
    CheckedType {
        ty: tc.handle_type(&expected),
        handle: expected,
        contains_extern_any: branch.checked.contains_extern_any,
    }
}

fn join_branches_with_hint(
    expected: Option<TypeHandle>,
    left: CheckedBranch,
    right: CheckedBranch,
    tc: &mut TypeChecker,
) -> CheckedType {
    match (left.diverges, right.diverges) {
        (true, true) => checked_void(tc),
        (true, false) => checked_branch_against_expected(right, expected, tc),
        (false, true) => checked_branch_against_expected(left, expected, tc),
        (false, false) => {
            if let Some(expected) = expected {
                let contains_extern_any =
                    left.checked.contains_extern_any || right.checked.contains_extern_any;
                tc.expect_assignable(left.span, left.checked.handle, expected.clone());
                tc.expect_assignable(right.span, right.checked.handle, expected.clone());
                tc.solve_constraints();
                return CheckedType {
                    ty: tc.handle_type(&expected),
                    handle: expected,
                    contains_extern_any,
                };
            }

            join_checked(left.checked, left.span, right.checked, right.span, tc)
        }
    }
}

fn check_if_checked_with_hint(
    if_node: &IfNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let cond = check_expr_checked(&if_node.node.cond, tc);
    check_bool_condition(ConditionKind::If, cond, if_node.node.cond.span, tc);
    let known_cond = intrinsic_bool_value(&if_node.node.cond, tc);
    let Some(else_block) = &if_node.node.else_block else {
        if known_cond != Some(false) {
            closure::check_closure_flow_branch(tc, |tc| {
                check_block_checked(&if_node.node.then_block, tc)
            });
        }
        return checked_void(tc);
    };
    if known_cond == Some(true) {
        return check_block_checked_with_hint(&if_node.node.then_block, expected, tc);
    }
    if known_cond == Some(false) {
        return check_block_checked_with_hint(else_block, expected, tc);
    }
    let (then, else_checked) = closure::check_closure_flow_branches(
        tc,
        |tc| check_block_checked_with_hint(&if_node.node.then_block, expected.clone(), tc),
        |tc| check_block_checked_with_hint(else_block, expected.clone(), tc),
    );
    join_branches_with_hint(
        expected,
        CheckedBranch {
            checked: then,
            span: if_node.node.then_block.span,
            diverges: control_flow::block_diverges(&if_node.node.then_block),
        },
        CheckedBranch {
            checked: else_checked,
            span: else_block.span,
            diverges: control_flow::block_diverges(else_block),
        },
        tc,
    )
}

fn check_ternary_checked_with_hint(
    ternary_node: &TernaryNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let node = &ternary_node.node;
    let cond = check_expr_checked(&node.cond, tc);
    check_bool_condition(ConditionKind::Ternary, cond, node.cond.span, tc);
    let known_cond = intrinsic_bool_value(&node.cond, tc);
    if known_cond == Some(true) {
        return check_value_expr_checked_with_hint(&node.then_expr, expected, tc);
    }
    if known_cond == Some(false) {
        return check_value_expr_checked_with_hint(&node.else_expr, expected, tc);
    }
    let (then, else_checked) = closure::check_closure_flow_branches(
        tc,
        |tc| check_value_expr_checked_with_hint(&node.then_expr, expected.clone(), tc),
        |tc| check_value_expr_checked_with_hint(&node.else_expr, expected.clone(), tc),
    );
    join_branches_with_hint(
        expected,
        CheckedBranch {
            checked: then,
            span: node.then_expr.span,
            diverges: control_flow::expr_diverges(&node.then_expr),
        },
        CheckedBranch {
            checked: else_checked,
            span: node.else_expr.span,
            diverges: control_flow::expr_diverges(&node.else_expr),
        },
        tc,
    )
}

fn expected_assignable_type(expected: Option<&TypeHandle>, tc: &TypeChecker) -> Option<Type> {
    let ty = expected.map(|handle| tc.handle_type(handle))?;
    Some(tc.decls.semantic_option_inner(&ty).unwrap_or(&ty).clone())
}

fn check_exact_downcast_expr(
    expr: &ExprNode,
    downcast: &ExactDowncastNode,
    tc: &mut TypeChecker,
) -> CheckedType {
    let _target = tc.resolve_downcast_target_type_at(&downcast.node.target, downcast.span);
    check_value_expr_checked_with_hint(&downcast.node.expr, None, tc);
    tc.push_error(TypeError::CompileError {
        message: "exact downcast is only supported in conditional bindings".to_string(),
        span: tc.error_span(downcast.span),
    });
    checked_from_type(expr, Type::Infer, tc)
}

fn sync_assigned_flow(
    target: &ExprNode,
    value: &ExprNode,
    function_value: bool,
    tc: &mut TypeChecker,
) {
    let ExprKind::Ident(name) = target.node.kind else {
        tc.record_escaping_use(value);
        return;
    };
    if tc.lookup_local_symbol(name).is_none() {
        tc.record_escaping_use(value);
        return;
    }

    let Some(binding_id) = tc.local_binding_id(name) else {
        tc.record_escaping_use(value);
        return;
    };
    tc.closure
        .assign_local_or_use(binding_id, value.node.id, function_value, value.span);
}

fn check_assign(expr_id: ExprId, assign: &AssignNode, tc: &mut TypeChecker) {
    let target = check_place(&assign.node.target, tc);
    if let Some(error) = target.value.access.assign_error(
        assignment_target_name(&assign.node.target),
        tc.error_span(assign.node.target.span),
    ) {
        tc.push_error(error);
    }

    match assign_op_to_binary_op(assign.node.op) {
        None => {
            let value = check_expr_checked_with_hint(
                &assign.node.value,
                Some(target.checked().handle.clone()),
                tc,
            );
            let function_value = matches!(value.ty, Type::Func { .. });
            if !target.accepts_extern_any() {
                tc.reject_extern_any_escape(&value, assign.node.value.span);
            }
            if !target.checked().ty.is_void() && !value.ty.is_void() {
                tc.expect_assignable_expr(
                    assign.node.value.span,
                    assign.node.value.node.id,
                    value.handle,
                    target.checked().handle.clone(),
                );
            }
            if target.value.access.can_assign() {
                sync_assigned_flow(&assign.node.target, &assign.node.value, function_value, tc);
                place::record_write(assign.node.target.node.id, &target, tc);
            }
        }
        Some(op) => {
            let value = check_expr_checked(&assign.node.value, tc);
            let target_value = CheckedType {
                ty: target.checked().ty.clone(),
                handle: target.checked().handle.clone(),
                contains_extern_any: target.checked().contains_extern_any,
            };
            let result = check_binary_checked(
                expr_id,
                op,
                &assign.node.target,
                target_value,
                &assign.node.value,
                value,
                assign.span,
                tc,
            );
            if !target.checked().ty.is_void() && !result.ty.is_void() {
                tc.expect_assignable_expr(
                    assign.node.value.span,
                    assign.node.value.node.id,
                    result.handle,
                    target.checked().handle.clone(),
                );
            }
            if target.value.access.can_assign() {
                place::record_compound_write(assign.node.target.node.id, &target, tc);
            }
        }
    }
}

fn assignment_target_name(expr: &ExprNode) -> Ident {
    match &expr.node.kind {
        ExprKind::Ident(name) => *name,
        ExprKind::Field(field) => field.node.field,
        ExprKind::Index(index) => assignment_target_name(&index.node.target),
        ExprKind::TupleIndex(index) => assignment_target_name(&index.node.target),
        _ => Ident::new("<target>"),
    }
}

fn assign_op_to_binary_op(op: AssignOp) -> Option<BinaryOp> {
    match op {
        AssignOp::Assign => None,
        AssignOp::AddAssign => Some(BinaryOp::Add),
        AssignOp::SubAssign => Some(BinaryOp::Sub),
        AssignOp::MulAssign => Some(BinaryOp::Mul),
        AssignOp::DivAssign => Some(BinaryOp::Div),
        AssignOp::XorAssign => Some(BinaryOp::Xor),
        AssignOp::BitAndAssign => Some(BinaryOp::BitAnd),
        AssignOp::BitOrAssign => Some(BinaryOp::BitOr),
        AssignOp::ShlAssign => Some(BinaryOp::Shl),
        AssignOp::ShrAssign => Some(BinaryOp::Shr),
    }
}

fn check_match_checked_with_hint(
    match_node: &MatchNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let node = &match_node.node;
    let mode = mode_for_head(node.head);
    let scrutinee = check_pattern_scrutinee(&node.scrutinee, mode, tc);
    if node.arms.is_empty() {
        tc.push_error(TypeError::EmptyMatch {
            span: tc.error_span(match_node.span),
        });
        return checked_void(tc);
    }
    let mut arms = Vec::with_capacity(node.arms.len());
    let mut outcomes = Vec::with_capacity(node.arms.len());
    let flow = tc.closure.closure_flow_snapshot();
    let mut arm_flows = Vec::with_capacity(node.arms.len());
    for arm in &node.arms {
        tc.closure.restore_closure_flow(&flow);
        tc.push_scope();
        let outcome = pattern::check_place_at(
            &arm.node.pattern,
            scrutinee.pattern_place(
                scrutinee.checked.handle.clone(),
                scrutinee.checked.ty.clone(),
            ),
            mode,
            node.scrutinee.node.id,
            PatternContext::Match,
            tc,
        );
        let body = check_expr_checked_with_hint(&arm.node.body, expected.clone(), tc);
        if let Some(expected) = expected.as_ref() {
            let expected_ty = tc.handle_type(expected);
            if !body.ty.is_void() && !matches!(body.ty, Type::Infer) && body.ty != expected_ty {
                tc.push_error(TypeError::MatchArmTypeMismatch {
                    expected: expected_ty,
                    found: body.ty.clone(),
                    span: tc.error_span(arm.node.body.span),
                });
            }
        }
        tc.pop_scope();
        arm_flows.push(tc.closure.closure_flow_snapshot());
        outcomes.push(outcome);
        arms.push((arm.node.body.span, body));
    }
    tc.closure.restore_closure_flow(&flow);
    for flow in arm_flows {
        let current = tc.closure.closure_flow_snapshot();
        tc.closure.join_closure_flow_snapshots(&current, &flow);
    }
    match_coverage::check(&scrutinee.checked.ty, &outcomes, match_node.span, tc);
    if arms[0].1.ty.is_void() {
        return checked_void(tc);
    }
    let result = tc.fresh_temp_handle(arms[0].0);
    let contains_extern_any = arms
        .iter()
        .any(|(_, arm)| !arm.ty.is_void() && arm.contains_extern_any);
    for (span, arm) in arms {
        if !arm.ty.is_void() {
            tc.expect_assignable(span, arm.handle, result.clone());
        }
    }
    tc.solve_constraints();
    CheckedType {
        ty: tc.handle_type(&result),
        handle: result,
        contains_extern_any,
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct CallTargetClosureFacts {
    pub(crate) types: type_ops::TypeClosureFacts,
    pub(crate) consts: ConstArgClosureFacts,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(crate) struct ConstArgClosureFacts {
    pub(crate) contains_unresolved: bool,
    pub(crate) contains_infer: bool,
}

impl CallTargetClosureFacts {
    pub(crate) fn contains_unresolved_const(&self) -> bool {
        self.types.contains_unresolved_const || self.consts.contains_unresolved
    }

    fn is_empty(&self) -> bool {
        !self.types.infer.contains_type
            && !self.types.infer.contains_return
            && self.types.first_unresolved.is_none()
            && !self.contains_unresolved_const()
            && !self.consts.contains_infer
    }
}

pub(crate) fn call_target_closure_facts(target: &CallTarget) -> CallTargetClosureFacts {
    let mut facts = CallTargetClosureFacts::default();
    for ty in &target.args.type_args {
        let ty_facts = type_closure_facts(ty);
        facts.types.infer.contains_type |= ty_facts.infer.contains_type;
        facts.types.infer.contains_return |= ty_facts.infer.contains_return;
        facts.types.first_unresolved = facts.types.first_unresolved.or(ty_facts.first_unresolved);
        facts.types.contains_unresolved_const |= ty_facts.contains_unresolved_const;
    }
    for arg in &target.args.const_args {
        match arg {
            ConstTerm::Name(_) => facts.consts.contains_unresolved = true,
            ConstTerm::ArrayInfer | ConstTerm::Infer(_) => facts.consts.contains_infer = true,
            ConstTerm::Value(_) | ConstTerm::Param(_) => {}
        }
    }
    facts
}

fn extern_site_span(site: crate::externs::RawExternSite) -> Option<SourceSpan> {
    site.span
}

fn push_extern_ty_closure_error(
    errors: &mut Vec<TypeError>,
    ty: &crate::externs::catalog::ResolvedExternTy,
    span: Option<SourceSpan>,
) {
    push_type_closure_error(errors, &ty.ty, span);
}

fn push_call_target_closure_error(
    errors: &mut Vec<TypeError>,
    target: &CallTarget,
    span: Option<SourceSpan>,
) {
    let facts = call_target_closure_facts(target);
    if facts.is_empty() {
        return;
    }
    for ty in &target.args.type_args {
        push_type_closure_error(errors, ty, span);
    }
    if facts.contains_unresolved_const() || facts.consts.contains_infer {
        errors.push(TypeError::CannotInferConst { span });
    }
}

fn push_type_closure_error(errors: &mut Vec<TypeError>, ty: &Type, span: Option<SourceSpan>) {
    let facts = type_closure_facts(ty);
    if let Some(unresolved) = facts.first_unresolved {
        errors.push(TypeError::UnknownType {
            qualifier: unresolved.qualifier,
            name: unresolved.name,
            span,
        });
    } else if facts.infer.contains_return {
        errors.push(TypeError::InferReturnValue { span });
    } else if facts.infer.contains_type {
        errors.push(TypeError::CannotInferType { span });
    } else if facts.contains_unresolved_const {
        errors.push(TypeError::CannotInferConst { span });
    }
}
