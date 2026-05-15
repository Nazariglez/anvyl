use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    rc::Rc,
};

use self::{
    closure::{
        BorrowedCapture, ClosureClassifier, ClosureScopeState, EscapeEvent, NonEscapingCallback,
    },
    const_term::ConstTerm,
    dyn_infer::DynInference,
    generic_bind::bind_exact_generic_args,
    infer::{
        GenericSolverSeeds, GenericSolverVars, LocalTypeId, Solver, SolverFinalizeError,
        SolverRelationError, SourceExprTypes, TypeHandle,
    },
    pattern::{PatternBindMode, PatternContext, PatternRoot, PatternRootInput},
    place::{
        AliasAltGroupId, PlaceAccess, PlaceIdentity, PlaceRoot, PlaceUseFacts,
        check_alias_scrutinee, check_place,
    },
    postfix::{PostfixStep, check_postfix_chain, collect_postfix_chain},
    type_ops::{TypeFolder, TypeVisitor, type_depends_on_generics},
    type_refs::{
        FinalizedTypeRef, GenericParamError, LocalTypeAlias, LocalTypeScopes, TypeRefResolver,
        TypeRefWarning, TypeRefWarningKind,
    },
};
pub(crate) use self::{
    decls::*,
    generic::*,
    result::*,
    semantic_use::*,
    surface::*,
    type_ops::type_closure_facts,
    type_refs::{GenericTypeContext, TypeRefError},
};
use crate::{
    ast::*,
    config::{CompilationContext, LintConfig, LintLevel, PredicateError},
    externs::{
        RawExterns,
        catalog::{
            ExternCatalog, ExternCatalogError, ExternField, ExternFieldRef, ExternType,
            ExternTypeId,
        },
    },
    resolve::ResolveResult,
    source::SourceId,
    span::{SourceSpan, Span},
};

mod annotation;
mod closure;
mod const_eval;
mod const_term;
mod contracts;
mod control_flow;
mod convert;
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

impl From<MemberAccessKind> for DeprecatedUseKind {
    fn from(kind: MemberAccessKind) -> Self {
        match kind {
            MemberAccessKind::Field => Self::Field,
            MemberAccessKind::Method => Self::Method,
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct TypecheckConfig {
    pub(crate) lint: LintConfig,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum DeprecatedUseKind {
    Function,
    ExternFunction,
    Const,
    Global,
    ExternType,
    TypeAlias,
    Contract,
    Struct,
    DataRef,
    Enum,
    EnumVariant,
    Field,
    Method,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TypeWarning {
    DeprecatedAccess {
        kind: DeprecatedUseKind,
        name: Ident,
        reason: Option<String>,
        span: SourceSpan,
    },
    InternalAccess {
        kind: MemberAccessKind,
        name: Ident,
        owner: Type,
        reason: Option<String>,
        span: SourceSpan,
    },
    CompileMessage {
        message: String,
        span: SourceSpan,
    },
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
    InternalAccess {
        kind: MemberAccessKind,
        name: Ident,
        owner: Type,
        reason: Option<String>,
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
enum CallableTemplateEnv {
    SourceModule,
    Local(ScopeState),
}

struct NamedFunctionFrame {
    value_floor: usize,
}

#[derive(Clone, Default)]
struct GenericOwnerFrame {
    params: GenericParams,
    args: GenericArgs,
    generics: GenericTypeContext,
}

struct SourceFuncSig {
    owner_generics: GenericParams,
    owner_args: GenericArgs,
    generics: GenericParams,
    generic_context: GenericTypeContext,
    params: Vec<FuncParam>,
    required_params: usize,
    ret: ReturnSpec,
    surface_ty: Type,
}

struct LocalFuncDecl {
    id: CallableId,
    sig: SourceFuncSig,
    func: FuncNode,
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
struct CallableTemplate {
    span: Span,
    mode: MethodMode,
    generics: GenericTypeContext,
    env: CallableTemplateEnv,
    params: Vec<Param>,
    ret: ReturnSpec,
    ret_span: Span,
    body: BlockNode,
}

#[derive(Clone, Copy)]
struct ControlFlowFrame {
    loops: usize,
    defers: usize,
    global_initializers: usize,
}

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

#[derive(Clone, Copy)]
enum ExplicitCast {
    Identity,
    Builtin,
    CastFrom { escape: EscapeMode },
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
    warnings: Vec<TypeWarning>,
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

struct ConstNormalizer<'tc> {
    tc: &'tc mut TypeChecker,
    span: Span,
}

impl TypeFolder for ConstNormalizer<'_> {
    fn fold_const_arg(&mut self, arg: &ConstArg) -> ConstArg {
        self.tc.normalize_const_arg(arg, self.span)
    }

    fn fold_array_len(&mut self, len: ArrayLen) -> ArrayLen {
        self.tc.normalize_array_len(len, self.span)
    }
}

struct CheckedSubstituter<'a, 'tc> {
    tc: &'tc mut TypeChecker,
    span: Span,
    types: &'a TypeSubst,
    consts: &'a ConstSubst,
}

impl TypeFolder for CheckedSubstituter<'_, '_> {
    fn fold_var(&mut self, id: TypeVarId) -> Type {
        self.types.get(&id).cloned().unwrap_or(Type::Var(id))
    }

    fn fold_const_arg(&mut self, arg: &ConstArg) -> ConstArg {
        match arg {
            ConstArg::Param(id) => self
                .consts
                .get(id)
                .and_then(ConstTerm::to_arg_no_infer)
                .unwrap_or_else(|| arg.clone()),
            ConstArg::Value(_) | ConstArg::Name(_) => arg.clone(),
        }
    }

    fn fold_array_len(&mut self, len: ArrayLen) -> ArrayLen {
        match len {
            ArrayLen::Param(id) => match self.consts.get(&id).cloned() {
                Some(term) => self
                    .tc
                    .array_len_from_term(term, self.span)
                    .unwrap_or(ArrayLen::Infer),
                None => ArrayLen::Param(id),
            },
            other => self
                .tc
                .array_len_from_term(ConstTerm::from_array_len(other), self.span)
                .unwrap_or(ArrayLen::Infer),
        }
    }
}

impl TypeChecker {
    fn new(decls: DeclarationIndex, externs: ExternCatalog, config: TypecheckConfig) -> Self {
        Self {
            solver: Solver::default(),
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

    fn mark_non_escaping_callback_binding(&mut self, name: Ident, origin: NonEscapingCallback) {
        let Some(binding_id) = self.local_binding_id(name) else {
            return;
        };
        self.closure.add_binding_callback(binding_id, origin);
    }

    fn record_escaping_use(&mut self, expr: &ExprNode) {
        self.closure.record_escaping_use(expr.node.id, expr.span);
    }

    fn push_escape_events(&mut self, events: Vec<EscapeEvent>) {
        for event in events {
            match event {
                EscapeEvent::Callback { origin, span } => {
                    self.push_non_escaping_callback_escape(&origin, span);
                }
                EscapeEvent::Borrowed { capture, span } => {
                    self.push_borrowed_escaping_capture(&capture, span);
                }
            }
        }
    }

    fn check_argument_escape(&mut self, arg: &ExprNode, escape: EscapeMode) {
        if escape.is_escaping() {
            self.record_escaping_use(arg);
        }
    }

    fn record_aggregate_elem_flow(&mut self, aggregate: ExprId, elem: &ExprNode) {
        self.record_escaping_use(elem);
        self.closure.copy_expr_flow(elem.node.id, aggregate);
    }

    fn mark_non_escaping_callback_param(
        &mut self,
        name: Ident,
        type_id: LocalTypeId,
        param: &FuncParam,
        source_ty: Option<&Type>,
    ) {
        if param.escape.is_escaping() || !matches!(param.ty, Type::Func { .. }) {
            return;
        }
        let ty = source_ty.unwrap_or(&param.ty);
        let help = Some(format!("mark the parameter as `{name}: escaping {ty}`"));
        self.mark_non_escaping_callback_binding(
            name,
            NonEscapingCallback {
                id: type_id,
                name,
                help,
            },
        );
    }

    fn push_non_escaping_callback_escape(&mut self, origin: &NonEscapingCallback, span: Span) {
        if !self.closure.record_non_escaping_callback_escape(origin.id) {
            return;
        }
        let help = origin.help.clone().or_else(|| {
            let ty = self.solver.local_type_to_type(origin.id);
            matches!(ty, Type::Func { .. })
                .then(|| format!("mark the parameter as `{}: escaping {ty}`", origin.name))
        });
        self.push_error(TypeError::NonEscapingCallbackEscapes {
            name: origin.name,
            help,
            span: self.error_span(span),
        });
    }

    fn record_local_read(&mut self, expr: ExprId, value: &LocalValue) {
        self.closure
            .record_local_read(expr, value.info.binding_id, value.source_depth);
    }

    fn push_borrowed_escaping_capture(&mut self, capture: &BorrowedCapture, span: Span) {
        if !self.closure.record_borrowed_escaping_capture(capture.id) {
            return;
        }
        self.push_error(TypeError::BorrowedCaptureEscapes {
            name: capture.name,
            origin: capture.origin,
            span: self.error_span(span),
        });
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
                    Ok(Some(self.local_value_from_info(
                        name,
                        symbol.value_view(),
                        depth,
                    )))
                }
            }
            LocalSymbolLookup::Blocked(error) => {
                self.push_error(*error);
                Err(())
            }
            LocalSymbolLookup::Missing => Ok(None),
        }
    }

    fn local_value_from_info(&self, _name: Ident, info: VarInfo, depth: usize) -> LocalValue {
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

    fn enter_lambda(&mut self, expr_id: ExprId) {
        self.closure.mark_lambda_non_escaping(expr_id);
        self.closure.enter_lambda(expr_id, self.scopes.len());
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
            self.solver.error_expr_type(id, span)
        } else {
            self.solver.set_expr_type_from_type(id, span, &ty);
            self.solver.expr_handle(id)
        }
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

    fn expect_assignable(&mut self, span: Span, from: TypeHandle, to: TypeHandle) {
        convert::expect_assignable(self, span, None, from, to);
    }

    fn expect_assignable_expr(
        &mut self,
        span: Span,
        expr_id: ExprId,
        from: TypeHandle,
        to: TypeHandle,
    ) {
        convert::expect_assignable(self, span, Some(expr_id), from, to);
    }

    fn expect_equal(&mut self, span: Span, left: TypeHandle, right: TypeHandle) {
        self.solver
            .add_handle_equal(self.error_span(span), left, right);
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

    pub(crate) fn record_global_access(&mut self, fact: GlobalAccessFact) {
        self.global_accesses.insert(fact.expr_id, fact);
    }

    fn seed_global_types(&mut self) {
        let globals = self
            .decls
            .values()
            .filter_map(|value| match &value.decl {
                ValueDecl::Global(sig) => {
                    Some((sig.key.clone(), sig.ty.clone(), sig.initializer_span))
                }
                ValueDecl::Func(_) | ValueDecl::Const(_) => None,
            })
            .collect::<Vec<_>>();

        for (key, ty, span) in globals {
            let id = if matches!(ty, Type::Infer) {
                self.solver.alloc_fresh_local_type(Some(span))
            } else {
                self.solver.alloc_local_type(&ty)
            };
            self.global_types.insert(key, id);
        }
    }

    fn global_handle(&self, key: &GlobalKey) -> TypeHandle {
        let id = *self
            .global_types
            .get(key)
            .expect("global type was not seeded");
        self.local_handle(id)
    }

    fn global_checked(&self, sig: &GlobalSig) -> CheckedType {
        let handle = self.global_handle(&sig.key);
        CheckedType {
            ty: self.handle_type(&handle),
            handle,
            contains_extern_any: false,
        }
    }

    fn sync_global_types(&mut self) {
        let globals = self
            .global_types
            .iter()
            .map(|(key, id)| (key.clone(), *id))
            .collect::<Vec<_>>();

        for (key, id) in globals {
            let ty = self.solver.local_type_to_type(id);
            if let Some(sig) = self.decls.global(&key) {
                if type_closure_facts(&ty).contains_any {
                    self.push_error_once(TypeError::AnyOutsideExternBoundary {
                        span: Some(sig.span),
                    });
                }
                let mut errors = vec![];
                push_type_closure_error(&mut errors, &ty, Some(sig.initializer_span));
                for error in errors {
                    self.push_error_once(error);
                }
            }
            self.decls.set_global_type(&key, ty.clone());
            self.solver.set_local_type_from_type(id, &ty);
        }
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

    fn promoted_surface_for(&mut self, receiver: &Type) -> Option<PromotedSurface> {
        let key = CanonicalTypeKey(receiver.clone());
        if let Some(surface) = self.promoted_surfaces.get(&key) {
            return Some(surface.clone());
        }
        let (surface, errors) = self.decls.promoted_surface_for(receiver, &self.externs)?;
        for error in errors {
            self.push_error_once(TypeError::Decl(error));
        }
        self.promoted_surfaces.insert(key, surface.clone());
        Some(surface)
    }

    fn reject_extern_any_escape(&mut self, checked: &CheckedType, span: Span) {
        self.reject_extern_any_escape_fact(checked.contains_extern_any, span);
    }

    fn reject_extern_any_escape_fact(&mut self, contains_extern_any: bool, span: Span) {
        if contains_extern_any {
            self.push_error(TypeError::ExternAnyEscape {
                span: self.error_span(span),
            });
        }
    }

    fn reject_user_any_type(&mut self, ty: &Type, span: Span) -> bool {
        if !type_closure_facts(ty).contains_any {
            return false;
        }
        self.push_error(TypeError::AnyOutsideExternBoundary {
            span: self.error_span(span),
        });
        true
    }

    fn reject_dyn_implicit_format(&mut self, ty: &Type, span: Span) -> bool {
        if !type_contains_dyn_value(ty, &self.decls, &mut HashSet::new()) {
            return false;
        }
        self.push_error(TypeError::CompileError {
            message: "dynamic values cannot be implicitly formatted".to_string(),
            span: self.error_span(span),
        });
        true
    }

    fn validate_map_key_type(&mut self, ty: &Type, span: Span) {
        if let Some(err) = map_key_type_error(&self.decls, ty, self.error_span(span)) {
            self.push_error(err);
        }
    }

    fn validate_map_key_type_in(&mut self, decls: &DeclarationIndex, ty: &Type, span: Span) {
        if let Some(err) = map_key_type_error(decls, ty, self.error_span(span)) {
            self.push_error(err);
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
                self.decls.core_option_inner(ty)?,
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

    fn push_warning(&mut self, warning: TypeWarning) {
        if !self.warnings.contains(&warning) {
            self.warnings.push(warning);
        }
    }

    fn check_matched_field_access_policy(
        &mut self,
        owner: &field_check::FieldOwner,
        name: Ident,
        policy: &annotation::AccessPolicy,
        span: Span,
    ) {
        match owner {
            field_check::FieldOwner::Nominal(owner_ty) => {
                if let Some(key) = self.decls.key_for_type(owner_ty) {
                    self.check_access_policy(
                        policy,
                        MemberAccessKind::Field,
                        name,
                        owner_ty,
                        &key.module,
                        span,
                    );
                }
            }
            field_check::FieldOwner::Variant { key, .. } => {
                self.check_access_policy(
                    policy,
                    MemberAccessKind::Field,
                    name,
                    &nominal_type(key),
                    &key.module,
                    span,
                );
            }
        }
    }

    fn warn_named_value_deprecated(&mut self, value: &ValueDecl, name: Ident, span: Span) {
        if let Some(kind) = value.deprecated_kind() {
            self.warn_deprecated(value.policy(), kind, name, span);
        }
    }

    fn warn_named_const_deprecated(&mut self, name: Ident, span: Span) {
        let Some((_, _, ValueDecl::Const(sig))) = self.lookup_named_value(name) else {
            return;
        };
        self.warn_deprecated(&sig.policy, DeprecatedUseKind::Const, name, span);
    }

    fn warn_extern_type_deprecated(&mut self, key: &NominalKey, span: Span) {
        if key.kind != NominalKind::Extern {
            return;
        }
        let reason = match self.decls.extern_type_policy(key) {
            Some(policy) if policy.has_deprecated() => {
                policy.deprecated_reason().map(str::to_string)
            }
            _ => return,
        };
        self.push_warning(TypeWarning::DeprecatedAccess {
            kind: DeprecatedUseKind::ExternType,
            name: key.name,
            reason,
            span: self.source_span(span),
        });
    }

    fn warn_deprecated(
        &mut self,
        policy: &annotation::AccessPolicy,
        kind: DeprecatedUseKind,
        name: Ident,
        span: Span,
    ) {
        if let Some(warning) = deprecated_access_warning(policy, kind, name, self.source_span(span))
        {
            self.push_warning(warning);
        }
    }

    fn optional_chain_inner_type(&mut self, ty: &Type, span: Span) -> Type {
        if matches!(ty, Type::Infer) {
            return Type::Infer;
        }
        if let Some(inner) = self.decls.core_option_inner(ty) {
            return inner.clone();
        }
        self.push_error(TypeError::OptionalChainingOnNonOptional {
            span: self.error_span(span),
        });
        ty.clone()
    }

    fn optional_chain_result_type(&self, ty: Type) -> Type {
        if matches!(ty, Type::Infer | Type::Void) || self.decls.core_option_inner(&ty).is_some() {
            return ty;
        }
        self.decls.core_option_of(ty).unwrap_or(Type::Infer)
    }

    fn check_access_policy(
        &mut self,
        policy: &annotation::AccessPolicy,
        kind: MemberAccessKind,
        name: Ident,
        owner: &Type,
        origin: &ModuleScope,
        span: Span,
    ) {
        emit_access_policy(
            policy,
            kind,
            name,
            owner,
            origin,
            span,
            &mut AccessPolicyOutput {
                source: self.source_id(),
                current_module: &self.current_module,
                config: &self.config,
                warnings: &mut self.warnings,
                errors: &mut self.errors,
            },
        );
    }

    fn check_stored_field_path_access(&mut self, owner: &Type, path: &[Ident], span: Span) {
        let mut owner = owner.clone();
        for name in path {
            let Some(key) = self.decls.key_for_type(&owner) else {
                return;
            };
            let Some(aggregate) = self.decls.aggregate(&key) else {
                return;
            };
            let Some(field) = aggregate.fields.get(name) else {
                return;
            };
            let policy = field.policy.clone();
            let field_ty = substitute_aggregate_member(&owner, &aggregate.generics, &field.ty);
            let origin = key.module;
            self.check_access_policy(
                &policy,
                MemberAccessKind::Field,
                *name,
                &owner,
                &origin,
                span,
            );
            owner = field_ty;
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

    fn enter_loop(&mut self) {
        self.loop_depth += 1;
    }

    fn exit_loop(&mut self) {
        self.loop_depth = self
            .loop_depth
            .checked_sub(1)
            .expect("loop depth underflow");
    }

    fn in_loop(&self) -> bool {
        self.loop_depth > 0
    }

    fn enter_defer(&mut self) {
        self.defer_depth += 1;
    }

    fn exit_defer(&mut self) {
        self.defer_depth = self
            .defer_depth
            .checked_sub(1)
            .expect("defer depth underflow");
    }

    fn in_defer(&self) -> bool {
        self.defer_depth > 0
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

    fn enter_function_control_flow(&mut self) -> ControlFlowFrame {
        let frame = ControlFlowFrame {
            loops: self.loop_depth,
            defers: self.defer_depth,
            global_initializers: self.global_initializer_depth,
        };
        self.loop_depth = 0;
        self.defer_depth = 0;
        self.global_initializer_depth = 0;
        frame
    }

    fn exit_function_control_flow(&mut self, frame: ControlFlowFrame) {
        self.loop_depth = frame.loops;
        self.defer_depth = frame.defers;
        self.global_initializer_depth = frame.global_initializers;
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

    fn push_generic_context(&mut self, generics: GenericTypeContext) {
        self.generic_contexts.push(generics);
    }

    fn pop_generic_context(&mut self) {
        self.generic_contexts.pop();
    }

    fn push_generic_owner_frame(&mut self, frame: GenericOwnerFrame) {
        self.generic_owner_frames.push(frame);
    }

    fn pop_generic_owner_frame(&mut self) {
        self.generic_owner_frames.pop();
    }

    fn visible_generic_owner(&self) -> GenericOwnerFrame {
        self.generic_owner_frames
            .last()
            .cloned()
            .unwrap_or_default()
    }

    fn substituted_type_param(&self, name: Ident) -> Option<Type> {
        let id = self
            .generic_contexts
            .iter()
            .rev()
            .find_map(|ctx| ctx.type_param(name))?;
        self.type_substs
            .iter()
            .rev()
            .find_map(|subst| subst.get(&id).cloned())
            .or(Some(Type::Var(id)))
    }

    fn store_callable_template(&mut self, id: CallableId, template: CallableTemplate) {
        self.callable_templates.insert(id, template);
    }

    fn callable_template(&self, id: &CallableId) -> Option<&CallableTemplate> {
        self.callable_templates.get(id)
    }

    fn specialization(&self, key: &SpecializationKey) -> Option<&SpecializationState> {
        self.specializations.get(key)
    }

    fn store_specialization(&mut self, key: SpecializationKey, state: SpecializationState) {
        self.specializations.insert(key, state);
    }

    fn closure_fact_snapshot(&self) -> TypecheckFacts {
        self.closure
            .fact_snapshot(|id| self.solver.local_type_to_type(id))
    }

    fn specialization_facts(&self) -> SpecializedBodyFacts {
        SpecializedBodyFacts {
            types: self.expr_types(),
            calls: self.calls.clone(),
            extern_uses: self.extern_uses.clone(),
            member_paths: self.member_paths.clone(),
            argument_projections: self.argument_projections.clone(),
            contract_witnesses: self.contract_witnesses.clone(),
            dyn_conversions: self.dyn_conversions.clone(),
            dyn_weakenings: self.dyn_weakenings.clone(),
            dyn_calls: self.dyn_calls.clone(),
            dyn_downcasts: self.dyn_downcasts.clone(),
            global_accesses: self.global_accesses.clone(),
            closure: self.closure_fact_snapshot(),
        }
    }

    fn restore_specialization(&mut self, facts: SpecializedBodyFacts) {
        for (id, (span, ty)) in facts.types {
            self.set_type(id, ty, span);
        }
        self.calls.extend(facts.calls);
        self.extern_uses.extend(facts.extern_uses);
        for fact in facts.member_paths.into_values() {
            self.record_member_path(fact);
        }
        for fact in facts.argument_projections.into_values() {
            self.record_argument_projection(fact);
        }
        for fact in facts.contract_witnesses.into_values() {
            self.next_witness_id = self.next_witness_id.max(fact.id.0 + 1);
            self.witness_keys.insert(fact.key.clone(), fact.id);
            self.contract_witnesses.insert(fact.id, fact);
        }
        self.dyn_conversions.extend(facts.dyn_conversions);
        self.dyn_weakenings.extend(facts.dyn_weakenings);
        self.dyn_calls.extend(facts.dyn_calls);
        self.dyn_downcasts.extend(facts.dyn_downcasts);
        self.global_accesses.extend(facts.global_accesses);
        self.closure.extend_facts(facts.closure);
    }

    fn resolved_value(value: ResolvedValue) -> (ModuleScope, Ident, ValueDecl) {
        (value.module, value.name, value.decl)
    }

    fn extend_visible(&self, extend: &ExtendSchema) -> bool {
        Self::extend_visible_in(&self.decls, &self.current_module, extend)
    }

    fn extend_visible_in(
        decls: &DeclarationIndex,
        current_module: &ModuleScope,
        extend: &ExtendSchema,
    ) -> bool {
        extend.origin == *current_module
            || (extend.exported && decls.imports_module(current_module, &extend.origin))
    }

    fn find_extend_method(&self, receiver: &Type, name: Ident) -> Option<ExtendMethodMatch<'_>> {
        self.decls
            .find_instance_extend_method(receiver, name, |ext| self.extend_visible(ext))
    }

    fn find_static_extend_method(
        &self,
        target: &Type,
        name: Ident,
    ) -> Option<ExtendMethodMatch<'_>> {
        self.decls
            .find_static_extend_method(target, name, |ext| self.extend_visible(ext))
    }

    fn explicit_cast_conversion(&self, source: &Type, target: &Type) -> Option<ExplicitCast> {
        if source == target {
            return Some(ExplicitCast::Identity);
        }
        if builtin_numeric_cast(source, target) {
            return Some(ExplicitCast::Builtin);
        }
        self.cast_from_conversion_escape(source, target)
            .map(|escape| ExplicitCast::CastFrom { escape })
    }

    fn cast_from_conversion_escape(&self, source: &Type, target: &Type) -> Option<EscapeMode> {
        match self
            .decls
            .find_cast_conversion(source, target, |ext| self.extend_visible(ext))
        {
            Some(CastConversionMatch::Match { escape }) => Some(escape),
            Some(CastConversionMatch::Ambiguous) | None => None,
        }
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

    fn resolve_type_subject(&mut self, ty: &Type, span: Span) -> Option<Type> {
        let ty = self.resolve_type_for_tc_at(ty, span);
        (!matches!(ty, Type::Infer)).then_some(ty)
    }

    fn resolve_type_for_tc_at(&mut self, ty: &Type, span: Span) -> Type {
        let generics = self.generic_contexts.last().cloned().unwrap_or_default();
        let resolver = TypeRefResolver::with_local_types(&self.decls, &self.local_type_scopes);
        let result = resolver.finalize_at(&self.current_module, &generics, ty, Some(span));
        let ty = self.finish_type_ref_result(result, span);
        self.reject_source_dyn_contracts(ty, span)
    }

    fn resolve_downcast_target_type_at(&mut self, ty: &Type, span: Span) -> Type {
        let generics = self.generic_contexts.last().cloned().unwrap_or_default();
        let resolver = TypeRefResolver::with_local_types(&self.decls, &self.local_type_scopes);
        match resolver.finalize_at(&self.current_module, &generics, ty, Some(span)) {
            Ok(finalized) => {
                self.push_type_ref_warnings(finalized.warnings);
                let ty = self.reject_source_dyn_contracts(finalized.ty, span);
                if matches!(ty, Type::Infer) {
                    Type::Infer
                } else if type_depends_on_generics(&ty) {
                    self.push_error(TypeError::CompileError {
                        message:
                            "exact downcast target must be a fully concrete runtime-identifiable type"
                                .to_string(),
                        span: self.error_span(span),
                    });
                    Type::Infer
                } else {
                    self.finish_resolved_type(ty, span)
                }
            }
            Err(error) => {
                self.push_error_once(type_ref_error(error, self.error_span(span)));
                Type::Infer
            }
        }
    }

    fn resolve_callable_param_type(&mut self, ty: &Type, span: Span, exported: bool) -> Type {
        if matches!(ty, Type::Dyn(ContractRef::Infer)) {
            let generics = self.generic_contexts.last().cloned().unwrap_or_default();
            let resolver = TypeRefResolver::with_local_types(&self.decls, &self.local_type_scopes);
            let result = resolver.finalize_at(&self.current_module, &generics, ty, Some(span));
            let ty = self.finish_type_ref_result(result, span);
            return self.dyn_infer.assign_holes(
                &self.current_module,
                &ty,
                self.source_span(span),
                exported,
            );
        }
        self.resolve_type_for_tc_at(ty, span)
    }

    fn reject_source_dyn_contracts(&mut self, ty: Type, span: Span) -> Type {
        if type_contains_anonymous_contract(&ty) {
            self.push_error(TypeError::CompileError {
                message: "anonymous dynamic contract syntax is not supported; declare a named contract or use dyn _ in a callable parameter".to_string(),
                span: self.error_span(span),
            });
            return Type::Infer;
        }
        self.reject_raw_dyn_infer(ty, span)
    }

    fn reject_raw_dyn_infer(&mut self, ty: Type, span: Span) -> Type {
        if !DynInference::has_raw_hole(&ty) {
            return ty;
        }
        let message = if type_contains_raw_dyn_infer_func(&ty) {
            "inferred dynamic contracts are not allowed in nested function types because they have no body that can own inference"
        } else {
            "inferred dynamic contracts are only allowed as direct parameters of callables with bodies"
        };
        self.push_error(TypeError::CompileError {
            message: message.to_string(),
            span: self.error_span(span),
        });
        Type::Infer
    }

    fn resolve_type_binding_for_tc_at(
        &mut self,
        binding: TypeBinding,
        args: &[GenericArg],
        span: Span,
        use_name: Ident,
    ) -> Type {
        let generics = self.generic_contexts.last().cloned().unwrap_or_default();
        let resolver = TypeRefResolver::with_local_types(&self.decls, &self.local_type_scopes);
        let result = resolver.finalize_type_binding_at(
            &self.current_module,
            &generics,
            binding,
            args,
            Some(span),
            use_name,
        );
        let ty = self.finish_type_ref_result(result, span);
        self.reject_source_dyn_contracts(ty, span)
    }

    fn finish_type_ref_result(
        &mut self,
        result: Result<FinalizedTypeRef, TypeRefError>,
        span: Span,
    ) -> Type {
        match result {
            Ok(finalized) => {
                self.push_type_ref_warnings(finalized.warnings);
                self.finish_resolved_type(finalized.ty, span)
            }
            Err(error) => {
                self.push_error_once(type_ref_error(error, self.error_span(span)));
                Type::Infer
            }
        }
    }

    fn resolve_module_alias_target_for_tc_at(
        &mut self,
        key: &TypeAliasKey,
        span: Span,
        use_name: Ident,
    ) -> Type {
        let resolver = TypeRefResolver::with_local_types(&self.decls, &self.local_type_scopes);
        let result = resolver.finalize_module_alias_target_at(key, Some(span), use_name);
        let ty = self.finish_type_ref_result(result, span);
        self.reject_source_dyn_contracts(ty, span)
    }

    fn resolve_local_alias_target_for_tc_at(
        &mut self,
        alias: &LocalTypeAlias,
        span: Span,
        use_name: Ident,
    ) -> Type {
        let resolver = TypeRefResolver::with_local_types(&self.decls, &self.local_type_scopes);
        let result = resolver.finalize_local_alias_target_at(alias, Some(span), use_name);
        let ty = self.finish_type_ref_result(result, span);
        self.reject_source_dyn_contracts(ty, span)
    }

    fn push_type_ref_warnings(&mut self, warnings: Vec<TypeRefWarning>) {
        for warning in warnings {
            let kind = match warning.kind {
                TypeRefWarningKind::TypeAlias => DeprecatedUseKind::TypeAlias,
                TypeRefWarningKind::Contract => DeprecatedUseKind::Contract,
            };
            self.push_warning(TypeWarning::DeprecatedAccess {
                kind,
                name: warning.name,
                reason: warning.reason,
                span: self.source_span(warning.span),
            });
        }
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

    fn validate_type_return_specs(&mut self, ty: &Type, span: Span) {
        ReturnSpecValidator { tc: self, span }.visit_type(ty);
    }

    fn validate_func_param_escape(
        &mut self,
        escape: EscapeMode,
        mutable: bool,
        cast_accept: bool,
        ty: &Type,
        span: Span,
    ) {
        let span = self.error_span(span);
        validate_param_escape(&mut self.errors, escape, mutable, cast_accept, ty, span);
    }

    fn validate_escaping_parameter_types(&mut self, ty: &Type, span: Span) {
        match ty {
            Type::Func { params, ret } => {
                for param in params {
                    self.validate_func_param_escape(
                        param.escape,
                        param.mutable,
                        param.cast_accept,
                        &param.ty,
                        span,
                    );
                    self.validate_escaping_parameter_types(&param.ty, span);
                }
                self.validate_escaping_parameter_types(&ret.ty, span);
            }
            Type::Tuple(elems) => {
                for elem in elems {
                    self.validate_escaping_parameter_types(elem, span);
                }
            }
            Type::Nominal(nominal) => {
                for arg in &nominal.type_args {
                    self.validate_escaping_parameter_types(arg, span);
                }
            }
            Type::UnresolvedNominal { generic_args, .. } => {
                for arg in generic_args {
                    if let GenericArg::Type(ty) = arg {
                        self.validate_escaping_parameter_types(ty, span);
                    }
                }
            }
            Type::List { elem } | Type::Array { elem, .. } | Type::Slice { elem } => {
                self.validate_escaping_parameter_types(elem, span);
            }
            Type::Map { key, value } => {
                self.validate_escaping_parameter_types(key, span);
                self.validate_escaping_parameter_types(value, span);
            }
            Type::Dyn(contract) => self.validate_escaping_contract_params(contract, span),
            Type::Infer
            | Type::InferReturn
            | Type::Any
            | Type::Int
            | Type::Float
            | Type::Bool
            | Type::String
            | Type::Void
            | Type::Var(_)
            | Type::UnresolvedName(_) => {}
        }
    }

    fn validate_escaping_contract_params(&mut self, contract: &ContractRef, span: Span) {
        match contract {
            ContractRef::Anonymous(surface) => {
                for req in &surface.requirements {
                    for param in &req.params {
                        self.validate_func_param_escape(
                            param.escape,
                            param.mutable,
                            false,
                            &param.ty,
                            span,
                        );
                        self.validate_escaping_parameter_types(&param.ty, span);
                    }
                    self.validate_escaping_parameter_types(&req.ret.ty, span);
                }
            }
            ContractRef::Intersection(contracts) => {
                for contract in contracts {
                    self.validate_escaping_contract_params(contract, span);
                }
            }
            ContractRef::Named { .. } | ContractRef::Infer | ContractRef::Hole(_) => {}
        }
    }

    fn normalize_type_consts(&mut self, ty: &Type, span: Span) -> Type {
        ConstNormalizer { tc: self, span }.fold_type(ty)
    }

    fn substitute_checked(
        &mut self,
        ty: &Type,
        types: &TypeSubst,
        consts: &ConstSubst,
        span: Span,
    ) -> Type {
        CheckedSubstituter {
            tc: self,
            span,
            types,
            consts,
        }
        .fold_type(ty)
    }

    fn eval_const_term(&mut self, term: ConstTerm, span: Span) -> Option<ConstTerm> {
        self.eval_const_term_inner(term, span, true)
    }

    fn eval_const_term_inner(
        &mut self,
        term: ConstTerm,
        span: Span,
        warn_deprecated: bool,
    ) -> Option<ConstTerm> {
        match term {
            ConstTerm::Value(_) => Some(term),
            ConstTerm::Name(name) => {
                if warn_deprecated {
                    self.warn_named_const_deprecated(name, span);
                }
                match self.lookup_visible_const_name(name, span) {
                    const_eval::ConstNameLookup::Value(value) => Some(ConstTerm::Value(value)),
                    const_eval::ConstNameLookup::RuntimeGlobal(global) => {
                        self.push_error_once(TypeError::RuntimeGlobalInConstPosition {
                            global,
                            span: self.error_span(span),
                        });
                        None
                    }
                    const_eval::ConstNameLookup::Error(error) => {
                        self.push_error(error);
                        None
                    }
                    const_eval::ConstNameLookup::NotConstLocal => {
                        self.push_error_once(TypeError::NonConstExpression {
                            span: self.error_span(span),
                        });
                        None
                    }
                    const_eval::ConstNameLookup::Missing => {
                        self.push_error_once(TypeError::UnknownConst {
                            name,
                            span: self.error_span(span),
                        });
                        None
                    }
                }
            }
            ConstTerm::Param(id) => match self
                .const_substs
                .last()
                .and_then(|subst| subst.get(&id).cloned())
            {
                Some(term) => self.eval_const_term_inner(term, span, warn_deprecated),
                None => Some(ConstTerm::Param(id)),
            },
            ConstTerm::ArrayInfer | ConstTerm::Infer(_) => None,
        }
    }

    fn require_usize_const(&mut self, term: ConstTerm, span: Span) -> Option<usize> {
        self.require_usize_const_inner(term, span, true)
    }

    fn require_usize_const_inner(
        &mut self,
        term: ConstTerm,
        span: Span,
        warn_deprecated: bool,
    ) -> Option<usize> {
        match self.eval_const_term_inner(term, span, warn_deprecated)? {
            ConstTerm::Value(value) => match const_eval::const_usize(&value, self.error_span(span))
            {
                Ok(value) => Some(value),
                Err(err) => {
                    self.push_error(err);
                    None
                }
            },
            ConstTerm::Name(name) => {
                self.push_error(TypeError::UnknownConst {
                    name,
                    span: self.error_span(span),
                });
                None
            }
            ConstTerm::Param(_) | ConstTerm::ArrayInfer | ConstTerm::Infer(_) => None,
        }
    }

    fn array_len_from_term(&mut self, term: ConstTerm, span: Span) -> Option<ArrayLen> {
        match term {
            ConstTerm::ArrayInfer => Some(ArrayLen::Infer),
            ConstTerm::Param(id) => match self
                .const_substs
                .last()
                .and_then(|subst| subst.get(&id).cloned())
            {
                Some(term) => self.array_len_from_term(term, span),
                None => Some(ArrayLen::Param(id)),
            },
            ConstTerm::Value(_) | ConstTerm::Name(_) => {
                self.require_usize_const(term, span).map(ArrayLen::Fixed)
            }
            ConstTerm::Infer(_) => None,
        }
    }

    fn normalize_const_arg(&mut self, arg: &ConstArg, span: Span) -> ConstArg {
        let Some(term) = self.eval_const_term(ConstTerm::from_arg(arg), span) else {
            return arg.clone();
        };
        term.to_arg_no_infer().unwrap_or_else(|| arg.clone())
    }

    fn normalize_array_len(&mut self, len: ArrayLen, span: Span) -> ArrayLen {
        self.array_len_from_term(ConstTerm::from_array_len(len), span)
            .unwrap_or(ArrayLen::Infer)
    }

    fn imported_value(&self, name: Ident) -> Option<(ModuleScope, Ident, ValueDecl)> {
        self.decls
            .imported_value(&self.current_module, name)
            .map(Self::resolved_value)
    }

    fn lookup_named_value(&self, name: Ident) -> Option<(ModuleScope, Ident, ValueDecl)> {
        if self
            .lookup_local_symbol(name)
            .is_some_and(|(_, depth)| depth > 0)
        {
            return None;
        }
        self.current_module_value(name)
            .or_else(|| self.imported_value(name))
    }

    fn lookup_module_alias(&self, name: Ident) -> Option<ModuleScope> {
        self.decls.imported_module(&self.current_module, name)
    }

    fn visible_type_subject(&mut self, name: Ident, span: Span) -> Option<Type> {
        if let Some(ty) = self.substituted_type_param(name) {
            return Some(ty);
        }
        if self.local_type_scopes.visible(name, None).is_some() {
            let ty = self.resolve_type_for_tc_at(&Type::UnresolvedName(name), span);
            return (!matches!(ty, Type::Infer)).then_some(ty);
        }
        let binding = self
            .decls
            .resolve_visible_type_binding(&self.current_module, None, name)?;
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
        let resolved_params = self.resolve_callable_params(params, span, exported);
        let resolved_ret = ret.with_ty(self.resolve_type_for_tc_at(&ret.ty, span));
        Type::Func {
            params: resolved_params,
            ret: Box::new(resolved_ret),
        }
    }

    fn resolve_callable_params(
        &mut self,
        params: &[Param],
        span: Span,
        exported: bool,
    ) -> Vec<FuncParam> {
        params
            .iter()
            .map(|p| {
                let ty = self.resolve_callable_param_type(&p.ty, span, exported);
                self.validate_func_param_escape(
                    p.escape,
                    matches!(p.mutability, Mutability::Mutable),
                    p.cast_accept,
                    &ty,
                    span,
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

    fn finish(&mut self) -> Result<(SourceExprTypes, TypecheckFacts), Vec<TypeError>> {
        self.solve_constraints();
        self.solve_dyn_inference();
        let facts = self.closure.finish(|id| self.solver.local_type_to_type(id));
        let escape_events = self.closure.take_escape_events();
        self.push_escape_events(escape_events);
        if !self.errors.is_empty() {
            return Err(std::mem::take(&mut self.errors));
        }

        let (types, finalize_errors) = self.solver.finalize_expr_types();
        let has_finalize_errors = self.push_finalize_errors(finalize_errors);
        if !has_finalize_errors {
            for error in self.result_closure_errors(&types) {
                self.push_error_once(error);
            }
        }
        if self.errors.is_empty() {
            Ok((types, facts))
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    fn into_result(mut self) -> Result<TypecheckResult, Vec<TypeError>> {
        let (_, facts) = self.finish()?;
        Ok(TypecheckResult::new(self.warnings, facts))
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

    fn nominal_generics(&self, key: &NominalKey) -> Option<GenericParams> {
        self.nominal_generics_in(&self.decls, key)
    }

    fn nominal_generics_in(
        &self,
        decls: &DeclarationIndex,
        key: &NominalKey,
    ) -> Option<GenericParams> {
        match key.kind {
            NominalKind::Struct | NominalKind::DataRef => {
                decls.aggregate(key).map(|schema| schema.generics.clone())
            }
            NominalKind::Enum => decls.enum_schema(key).map(|schema| schema.generics.clone()),
            NominalKind::Extern => Some(GenericParams::default()),
        }
    }

    fn validate_nominal_uses(&mut self, ty: &Type, span: Span) {
        match ty {
            Type::Nominal(nominal) => {
                for arg in &nominal.type_args {
                    self.validate_nominal_uses(arg, span);
                }
                let Some(key) = self.decls.key_for_type(ty) else {
                    return;
                };
                self.warn_extern_type_deprecated(&key, span);
                let Some(generics) = self.nominal_generics(&key) else {
                    return;
                };
                let args = nominal_generic_args(ty).expect("nominal type");
                let decls = self.decls.clone();
                self.validate_nominal_args(&decls, &key, &generics, &args, span);
            }
            Type::Func { params, ret } => {
                for param in params {
                    self.validate_nominal_uses(&param.ty, span);
                }
                self.validate_nominal_uses(&ret.ty, span);
            }
            Type::Dyn(contract) => self.validate_contract_ref_uses(contract, span),
            Type::Tuple(elems) => {
                for elem in elems {
                    self.validate_nominal_uses(elem, span);
                }
            }
            Type::List { elem } | Type::Slice { elem } | Type::Array { elem, .. } => {
                self.validate_nominal_uses(elem, span);
            }
            Type::Map { key, value } => {
                self.validate_nominal_uses(key, span);
                self.validate_map_key_type(key, span);
                self.validate_nominal_uses(value, span);
            }
            Type::Infer
            | Type::InferReturn
            | Type::Any
            | Type::Int
            | Type::Float
            | Type::Bool
            | Type::String
            | Type::Void
            | Type::Var(_)
            | Type::UnresolvedName(_)
            | Type::UnresolvedNominal { .. } => {}
        }
    }

    fn validate_contract_ref_uses(&mut self, contract: &ContractRef, span: Span) {
        if let Some(name) = contract_surface_conflict(&self.decls, &self.current_module, contract) {
            self.push_error(TypeError::CompileError {
                message: format!("conflicting contract requirement '{name}'"),
                span: self.error_span(span),
            });
        }
        match contract {
            ContractRef::Anonymous(surface) => {
                for req in &surface.requirements {
                    for param in &req.params {
                        self.validate_nominal_uses(&param.ty, span);
                    }
                    self.validate_nominal_uses(&req.ret.ty, span);
                }
            }
            ContractRef::Intersection(contracts) => {
                for contract in contracts {
                    self.validate_contract_ref_uses(contract, span);
                }
            }
            ContractRef::Named { .. } | ContractRef::Infer | ContractRef::Hole(_) => {}
        }
    }

    fn validate_contract_surface(
        &mut self,
        decls: &DeclarationIndex,
        contract: &ContractRef,
        span: Span,
    ) {
        if let Some(name) = contract_surface_conflict(decls, &self.current_module, contract) {
            self.push_error(TypeError::CompileError {
                message: format!("conflicting contract requirement '{name}'"),
                span: self.error_span(span),
            });
        }
    }

    fn finalize_declarations(&mut self) {
        let saved_module = self.current_module.clone();
        let mut decls = std::mem::take(&mut self.decls);
        let lookup = decls.clone();
        let generic_errors = decls.map_canonical_type_uses(|site, ty| {
            self.current_module = site.module.clone();
            let span = site.span;
            let ty = self.finalize_decl_type(&lookup, site, ty);
            let ty = self.normalize_type_consts(&ty, span);
            self.reject_user_any_type(&ty, span);
            ty
        });
        for error in generic_errors {
            let source = self.module_sources.get(&error.module).copied();
            self.push_error(generic_param_decl_type_error(error, source));
        }
        validate_type_alias_decls(&decls, &mut self.errors);
        contracts::finalize_contracts(&mut decls, &mut self.errors, &mut self.warnings);
        validate_public_contract_types(&decls, &mut self.errors);
        validate_dyn_infer_decls(&decls, &mut self.errors);
        validate_extend_decls(&decls, &mut self.errors);
        for error in decls.build_projection_entries() {
            self.push_error(TypeError::Decl(error));
        }
        for error in decls.build_promoted_surfaces(&self.externs) {
            self.push_error(TypeError::Decl(error));
        }
        self.validate_final_decl_type_uses(&mut decls);
        self.current_module = saved_module;
        self.decls = decls;
    }

    fn validate_final_decl_type_uses(&mut self, decls: &mut DeclarationIndex) {
        let validation = decls.clone();
        self.decls = validation.clone();
        let _ = decls.map_canonical_type_uses(|site, ty| {
            self.current_module = site.module;
            self.push_generic_owner_frame(GenericOwnerFrame {
                params: GenericParams {
                    type_params: site.type_params,
                    const_params: vec![],
                },
                ..GenericOwnerFrame::default()
            });
            self.validate_nominal_uses_in(&validation, &ty, site.span);
            self.pop_generic_owner_frame();
            ty
        });
    }

    fn finalize_decl_type(
        &mut self,
        decls: &DeclarationIndex,
        site: DeclTypeSite,
        ty: Type,
    ) -> Type {
        let resolver = TypeRefResolver::module_only(decls);
        match resolver.finalize_at(&site.module, &site.generics, &ty, Some(site.span)) {
            Ok(finalized) => {
                self.push_type_ref_warnings(finalized.warnings);
                finalized.ty
            }
            Err(TypeRefError::Unknown { qualifier, name }) => {
                self.push_error(TypeError::Decl(DeclError::UnknownType {
                    module: site.module,
                    qualifier,
                    name,
                    span: Some(self.source_span(site.span)),
                }));
                Type::Infer
            }
            Err(error) => {
                self.push_error(type_ref_error(error, self.error_span(site.span)));
                Type::Infer
            }
        }
    }

    fn validate_nominal_uses_in(&mut self, decls: &DeclarationIndex, ty: &Type, span: Span) {
        match ty {
            Type::Nominal(nominal) => {
                for arg in &nominal.type_args {
                    self.validate_nominal_uses_in(decls, arg, span);
                }
                let Some(key) = decls.key_for_type(ty) else {
                    return;
                };
                let Some(generics) = self.nominal_generics_in(decls, &key) else {
                    return;
                };
                let args = nominal_generic_args(ty).expect("nominal type");
                self.validate_nominal_args(decls, &key, &generics, &args, span);
            }
            Type::Func { params, ret } => {
                for param in params {
                    self.validate_func_param_escape(
                        param.escape,
                        param.mutable,
                        param.cast_accept,
                        &param.ty,
                        span,
                    );
                    self.validate_nominal_uses_in(decls, &param.ty, span);
                }
                self.validate_nominal_uses_in(decls, &ret.ty, span);
            }
            Type::Dyn(contract) => self.validate_contract_ref_uses_in(decls, contract, span),
            Type::Tuple(elems) => {
                for elem in elems {
                    self.validate_nominal_uses_in(decls, elem, span);
                }
            }
            Type::List { elem } | Type::Slice { elem } | Type::Array { elem, .. } => {
                self.validate_nominal_uses_in(decls, elem, span);
            }
            Type::Map { key, value } => {
                self.validate_nominal_uses_in(decls, key, span);
                self.validate_map_key_type_in(decls, key, span);
                self.validate_nominal_uses_in(decls, value, span);
            }
            Type::Infer
            | Type::InferReturn
            | Type::Any
            | Type::Int
            | Type::Float
            | Type::Bool
            | Type::String
            | Type::Void
            | Type::Var(_)
            | Type::UnresolvedName(_)
            | Type::UnresolvedNominal { .. } => {}
        }
    }

    fn validate_contract_ref_uses_in(
        &mut self,
        decls: &DeclarationIndex,
        contract: &ContractRef,
        span: Span,
    ) {
        self.validate_contract_surface(decls, contract, span);
        match contract {
            ContractRef::Anonymous(surface) => {
                for req in &surface.requirements {
                    for param in &req.params {
                        self.validate_func_param_escape(
                            param.escape,
                            param.mutable,
                            false,
                            &param.ty,
                            span,
                        );
                        self.validate_nominal_uses_in(decls, &param.ty, span);
                    }
                    self.validate_nominal_uses_in(decls, &req.ret.ty, span);
                }
            }
            ContractRef::Intersection(contracts) => {
                for contract in contracts {
                    self.validate_contract_ref_uses_in(decls, contract, span);
                }
            }
            ContractRef::Named { .. } | ContractRef::Infer | ContractRef::Hole(_) => {}
        }
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
            self.require_usize_const_inner(term.clone(), span, false);
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

fn contract_surface_conflict(
    decls: &DeclarationIndex,
    module: &ModuleScope,
    contract: &ContractRef,
) -> Option<Ident> {
    match contracts::requirements_for_ref(decls, module, contract) {
        Err(contracts::ContractSetError::ConflictingRequirement(name)) => Some(name),
        Ok(_) | Err(contracts::ContractSetError::UnknownContract) => None,
    }
}

fn map_key_type_error(
    decls: &DeclarationIndex,
    ty: &Type,
    span: Option<SourceSpan>,
) -> Option<TypeError> {
    let err = decls.map_key_error(ty)?;
    Some(TypeError::NonKeyableMapKey {
        ty: err.ty,
        field: err.field,
        span,
    })
}

fn deprecated_access_warning(
    policy: &annotation::AccessPolicy,
    kind: DeprecatedUseKind,
    name: Ident,
    span: SourceSpan,
) -> Option<TypeWarning> {
    policy
        .has_deprecated()
        .then(|| TypeWarning::DeprecatedAccess {
            kind,
            name,
            reason: policy.deprecated_reason().map(str::to_string),
            span,
        })
}

struct AccessPolicyOutput<'a> {
    source: SourceId,
    current_module: &'a ModuleScope,
    config: &'a TypecheckConfig,
    warnings: &'a mut Vec<TypeWarning>,
    errors: &'a mut Vec<TypeError>,
}

fn emit_access_policy(
    policy: &annotation::AccessPolicy,
    kind: MemberAccessKind,
    name: Ident,
    owner: &Type,
    origin: &ModuleScope,
    span: Span,
    out: &mut AccessPolicyOutput<'_>,
) {
    if let Some(warning) = deprecated_access_warning(
        policy,
        DeprecatedUseKind::from(kind),
        name,
        SourceSpan::from_byte_span(out.source, span),
    ) {
        out.warnings.push(warning);
    }

    if !policy.has_internal()
        || origin == out.current_module
        || out.config.lint.internal_access == LintLevel::Allow
    {
        return;
    }

    let reason = policy.internal_reason().map(str::to_string);
    match out.config.lint.internal_access {
        LintLevel::Allow => unreachable!("allow returned before diagnostic emission"),
        LintLevel::Warn => out.warnings.push(TypeWarning::InternalAccess {
            kind,
            name,
            owner: owner.clone(),
            reason,
            span: SourceSpan::from_byte_span(out.source, span),
        }),
        LintLevel::Error => out.errors.push(TypeError::InternalAccess {
            kind,
            name,
            owner: owner.clone(),
            reason,
            span: Some(SourceSpan::from_byte_span(out.source, span)),
        }),
    }
}

pub(crate) fn check_with_modules(
    program: &Program,
    resolved: &ResolveResult,
    externs: RawExterns,
    config: TypecheckConfig,
) -> Result<TypecheckResult, Vec<TypeError>> {
    typechecker_for_modules(program, resolved, externs, config)?.into_result()
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
        collect_callable_templates(root_scope.clone(), program, tc);
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
                collect_callable_templates(scope.clone(), program.as_ref(), tc);
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
        return Err(tc.errors);
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

fn check_finite_size_cycles(tc: &mut TypeChecker) {
    let graph = finite_size_graph(&tc.decls);
    let mut visiting = HashSet::new();
    let mut visited = HashSet::new();
    let mut reported = HashSet::new();
    let mut keys = graph.keys().cloned().collect::<Vec<_>>();
    keys.sort_by_key(nominal_key_sort_key);
    for key in keys {
        check_finite_size_key(&key, &graph, &mut visiting, &mut visited, &mut reported, tc);
    }
}

fn check_finite_size_key(
    key: &NominalKey,
    graph: &HashMap<NominalKey, Vec<NominalKey>>,
    visiting: &mut HashSet<NominalKey>,
    visited: &mut HashSet<NominalKey>,
    reported: &mut HashSet<NominalKey>,
    tc: &mut TypeChecker,
) {
    if visited.contains(key) {
        return;
    }
    if !visiting.insert(key.clone()) {
        if reported.insert(key.clone()) {
            let span = tc
                .decls
                .type_span(key)
                .and_then(|span| tc.module_error_span(&key.module, span));
            tc.push_error(TypeError::InfiniteSize {
                name: key.name,
                span,
            });
        }
        return;
    }
    if let Some(edges) = graph.get(key) {
        for edge in edges {
            check_finite_size_key(edge, graph, visiting, visited, reported, tc);
        }
    }
    visiting.remove(key);
    visited.insert(key.clone());
}

fn finite_size_graph(decls: &DeclarationIndex) -> HashMap<NominalKey, Vec<NominalKey>> {
    let mut graph = HashMap::new();
    for (key, schema) in decls.aggregates() {
        let mut edges = vec![];
        for field in schema.fields.values() {
            finite_size_edges(&field.ty, decls, &mut edges);
        }
        sort_finite_size_edges(&mut edges);
        graph.insert(key.clone(), edges);
    }
    for (key, schema) in decls.enums() {
        let mut edges = vec![];
        for variant in schema.variants.values() {
            match &variant.payload {
                VariantPayload::Unit => {}
                VariantPayload::Tuple(types) => {
                    for ty in types {
                        finite_size_edges(ty, decls, &mut edges);
                    }
                }
                VariantPayload::Struct(fields) => {
                    for field in fields.values() {
                        finite_size_edges(&field.ty, decls, &mut edges);
                    }
                }
            }
        }
        sort_finite_size_edges(&mut edges);
        graph.insert(key.clone(), edges);
    }
    graph
}

fn sort_finite_size_edges(edges: &mut Vec<NominalKey>) {
    edges.sort_by_key(nominal_key_sort_key);
    edges.dedup();
}

fn nominal_key_sort_key(key: &NominalKey) -> String {
    format!("{:?}:{:?}:{}", key.module, key.kind, key.name)
}

fn finite_size_edges(ty: &Type, decls: &DeclarationIndex, edges: &mut Vec<NominalKey>) {
    match ty {
        Type::Nominal(nominal) => {
            if let Some(key) = decls.key_for_type(ty)
                && matches!(key.kind, NominalKind::Struct | NominalKind::Enum)
            {
                edges.push(key);
            }
            if !matches!(nominal.kind, NominalKind::DataRef | NominalKind::Extern) {
                for arg in &nominal.type_args {
                    finite_size_edges(arg, decls, edges);
                }
            }
        }
        Type::Array { elem, .. } => finite_size_edges(elem, decls, edges),
        Type::Tuple(types) => {
            for ty in types {
                finite_size_edges(ty, decls, edges);
            }
        }
        Type::Infer
        | Type::InferReturn
        | Type::Any
        | Type::Int
        | Type::Float
        | Type::Bool
        | Type::String
        | Type::Void
        | Type::Func { .. }
        | Type::Dyn(_)
        | Type::Var(_)
        | Type::UnresolvedName(_)
        | Type::UnresolvedNominal { .. }
        | Type::List { .. }
        | Type::Map { .. }
        | Type::Slice { .. } => {}
    }
}

fn generic_param_decl_type_error(
    error: GenericContextError,
    source: Option<SourceId>,
) -> TypeError {
    TypeError::Decl(DeclError::DuplicateGenericParam {
        module: error.module,
        name: error.error.name(),
        span: source.map(|source| SourceSpan {
            source,
            span: error.span,
        }),
    })
}

fn generic_param_type_error(error: GenericParamError, span: Option<SourceSpan>) -> TypeError {
    TypeError::DuplicateGenericParam {
        name: error.name(),
        span,
    }
}

fn validate_dyn_infer_decls(decls: &DeclarationIndex, errors: &mut Vec<TypeError>) {
    for (_key, aggregate) in decls.aggregates() {
        for field in aggregate.fields.values() {
            push_invalid_dyn_infer_decl(&field.ty, field.span, errors);
        }
    }
    for (_key, schema) in decls.enums() {
        for variant in schema.variants.values() {
            match &variant.payload {
                VariantPayload::Unit => {}
                VariantPayload::Tuple(types) => {
                    for ty in types {
                        push_invalid_dyn_infer_decl(ty, None, errors);
                    }
                }
                VariantPayload::Struct(fields) => {
                    for field in fields.values() {
                        push_invalid_dyn_infer_decl(&field.ty, field.span, errors);
                    }
                }
            }
        }
    }
    for alias in decls.type_aliases() {
        push_invalid_dyn_infer_decl(&alias.def.aliased, Some(alias.def.span), errors);
    }
    for value in decls.values() {
        match &value.decl {
            ValueDecl::Const(sig) => push_invalid_dyn_infer_decl(&sig.ty, None, errors),
            ValueDecl::Global(sig) => {
                push_invalid_dyn_infer_decl(&sig.ty, Some(sig.initializer_span), errors);
            }
            ValueDecl::Func(sig) if sig.kind == CallableKind::ExternFunction => {
                push_invalid_dyn_infer_decl(&sig.ty, None, errors);
            }
            ValueDecl::Func(_) => {}
        }
    }
}

fn push_invalid_dyn_infer_decl(ty: &Type, span: Option<SourceSpan>, errors: &mut Vec<TypeError>) {
    if DynInference::has_raw_hole(ty) {
        errors.push(TypeError::CompileError {
            message: "inferred dynamic contracts are only allowed as direct parameters of callables with bodies"
                .to_string(),
            span,
        });
    }
}

fn validate_type_alias_decls(decls: &DeclarationIndex, errors: &mut Vec<TypeError>) {
    for alias in decls.type_aliases() {
        validate_type_alias_def(
            decls,
            &alias.def,
            &alias.def.aliased,
            matches!(alias.visibility, Visibility::Public),
            errors,
        );
    }
}

fn validate_public_value_surfaces(decls: &DeclarationIndex, errors: &mut Vec<TypeError>) {
    for value in decls.values() {
        if !matches!(value.visibility, Visibility::Public) {
            continue;
        }
        if let Some(ty) = private_exposed_type(decls, value.decl.ty()) {
            errors.push(TypeError::Decl(DeclError::PublicValuePrivateType {
                kind: value.decl.public_kind(),
                name: value.name,
                ty,
                span: value.decl.diagnostic_span(),
            }));
        }
    }
}

fn validate_public_contract_types(decls: &DeclarationIndex, errors: &mut Vec<TypeError>) {
    for contract in decls.contracts() {
        if !matches!(contract.visibility, Visibility::Public) {
            continue;
        }
        for (include, span) in &contract.includes {
            if private_included_contract(decls, &contract.key.module, include).is_some() {
                errors.push(TypeError::Decl(DeclError::PublicContractPrivateType {
                    name: contract.key.name,
                    ty: Type::Dyn(include.clone()),
                    span: Some(*span),
                }));
            }
        }
        for req in &contract.requirements {
            let exposed = req
                .params
                .iter()
                .find_map(|param| private_exposed_type(decls, &param.ty))
                .or_else(|| private_exposed_type(decls, &req.ret.ty));
            if let Some(ty) = exposed {
                errors.push(TypeError::Decl(DeclError::PublicContractPrivateType {
                    name: contract.key.name,
                    ty,
                    span: req.span,
                }));
            }
        }
    }
}

fn validate_type_alias_def(
    decls: &DeclarationIndex,
    alias: &TypeAliasDef,
    target: &Type,
    public: bool,
    errors: &mut Vec<TypeError>,
) {
    if matches!(target, Type::Infer) {
        return;
    }
    push_unused_alias_params(&alias.generics, target, alias.span, errors);
    if public && let Some(ty) = private_exposed_type(decls, target) {
        errors.push(TypeError::Decl(DeclError::PublicAliasPrivateType {
            name: alias.name,
            ty,
            span: Some(alias.span),
        }));
    }
}

fn push_unused_alias_params(
    generics: &GenericParams,
    ty: &Type,
    span: SourceSpan,
    errors: &mut Vec<TypeError>,
) {
    let facts = target_facts(ty);
    for param in &generics.type_params {
        if !facts.type_params.contains(&param.id) {
            errors.push(TypeError::Decl(DeclError::UnusedAliasTypeParam {
                name: param.name,
                span: Some(span),
            }));
        }
    }
    for param in &generics.const_params {
        if !facts.const_params.contains(&param.id) {
            errors.push(TypeError::Decl(DeclError::UnusedAliasConstParam {
                name: param.name,
                span: Some(span),
            }));
        }
    }
}

fn private_exposed_type(decls: &DeclarationIndex, ty: &Type) -> Option<Type> {
    match ty {
        Type::Nominal(nominal) => {
            let key = decls.key_for_type(ty)?;
            let exported = decls
                .exported_nominal_type(&key.module, key.name)
                .is_some_and(|exported| exported == key);
            if !exported {
                return Some(ty.clone());
            }
            nominal
                .type_args
                .iter()
                .find_map(|ty| private_exposed_type(decls, ty))
        }
        Type::Func { params, ret } => params
            .iter()
            .find_map(|param| private_exposed_type(decls, &param.ty))
            .or_else(|| private_exposed_type(decls, &ret.ty)),
        Type::Dyn(contract) => private_contract_type(decls, contract),
        Type::Tuple(elems) => elems.iter().find_map(|ty| private_exposed_type(decls, ty)),
        Type::List { elem } | Type::Slice { elem } | Type::Array { elem, .. } => {
            private_exposed_type(decls, elem)
        }
        Type::Map { key, value } => {
            private_exposed_type(decls, key).or_else(|| private_exposed_type(decls, value))
        }
        Type::Infer
        | Type::InferReturn
        | Type::Any
        | Type::Int
        | Type::Float
        | Type::Bool
        | Type::String
        | Type::Void
        | Type::Var(_)
        | Type::UnresolvedName(_)
        | Type::UnresolvedNominal { .. } => None,
    }
}

fn private_included_contract(
    decls: &DeclarationIndex,
    module: &ModuleScope,
    contract: &ContractRef,
) -> Option<ContractKey> {
    match contract {
        ContractRef::Named { .. } => {
            let resolver = TypeRefResolver::module_only(decls);
            let key = resolver.resolve_contract_ref(module, contract).ok()?;
            let exported = matches!(
                decls.exported_type_binding(&key.module, key.name),
                Some(TypeBinding::Contract(exported)) if exported == key
            );
            (!exported).then_some(key)
        }
        ContractRef::Intersection(contracts) => contracts
            .iter()
            .find_map(|contract| private_included_contract(decls, module, contract)),
        ContractRef::Anonymous(_) | ContractRef::Infer | ContractRef::Hole(_) => None,
    }
}

fn private_contract_type(decls: &DeclarationIndex, contract: &ContractRef) -> Option<Type> {
    match contract {
        ContractRef::Named { name, origin, .. } => {
            let module = origin
                .as_ref()
                .map_or(ModuleScope::Root, ModuleScope::from_nominal_origin);
            let key = ContractKey {
                module,
                name: *name,
            };
            let exported = matches!(
                decls.exported_type_binding(&key.module, key.name),
                Some(TypeBinding::Contract(exported)) if exported == key
            );
            (!exported).then(|| Type::Dyn(contract.clone()))
        }
        ContractRef::Anonymous(surface) => surface.requirements.iter().find_map(|req| {
            req.params
                .iter()
                .find_map(|param| private_exposed_type(decls, &param.ty))
                .or_else(|| private_exposed_type(decls, &req.ret.ty))
        }),
        ContractRef::Intersection(contracts) => contracts
            .iter()
            .find_map(|contract| private_contract_type(decls, contract)),
        ContractRef::Infer | ContractRef::Hole(_) => None,
    }
}

fn validate_extend_decls(decls: &DeclarationIndex, errors: &mut Vec<TypeError>) {
    validate_duplicate_extend_methods(decls, errors);
    for extend in decls.extends() {
        if matches!(extend.target, Type::Infer) {
            continue;
        }
        let facts = target_facts(&extend.target);
        if unsupported_extend_target(&extend.target, &facts) {
            errors.push(TypeError::Decl(DeclError::UnsupportedExtendTarget {
                ty: extend.target.clone(),
                span: Some(extend.span),
            }));
        }
        for param in &extend.generics.type_params {
            if !facts.type_params.contains(&param.id) {
                errors.push(TypeError::Decl(DeclError::UnusedExtendTypeParam {
                    name: param.name,
                    span: Some(extend.span),
                }));
            }
        }
        for param in &extend.generics.const_params {
            if !facts.const_params.contains(&param.id) {
                errors.push(TypeError::Decl(DeclError::UnusedExtendConstParam {
                    name: param.name,
                    span: Some(extend.span),
                }));
            }
        }
        validate_extend_method_conflicts(decls, extend, errors);
        validate_cast_froms(decls, extend, errors);
    }
}

fn validate_duplicate_extend_methods(decls: &DeclarationIndex, errors: &mut Vec<TypeError>) {
    let extends = decls.extends().collect::<Vec<_>>();
    for (index, extend) in extends.iter().enumerate() {
        for prior in &extends[..index] {
            if prior.origin != extend.origin
                || !same_extend_target(
                    &prior.target,
                    &prior.generics,
                    &extend.target,
                    &extend.generics,
                )
            {
                continue;
            }
            for method_key in extend.methods.keys() {
                if prior.methods.contains_key(method_key) {
                    errors.push(TypeError::Decl(DeclError::DuplicateExtendMethod {
                        name: method_key.name,
                        surface: method_key.surface,
                        span: Some(extend.span),
                    }));
                }
            }
        }
    }
}

fn validate_param_escape(
    errors: &mut Vec<TypeError>,
    escape: EscapeMode,
    mutable: bool,
    cast_accept: bool,
    ty: &Type,
    span: Option<SourceSpan>,
) {
    if !escape.is_escaping() {
        return;
    }
    if mutable {
        errors.push(TypeError::CompileError {
            message: "`escaping` cannot be combined with `var`".to_string(),
            span,
        });
    }
    if cast_accept {
        errors.push(TypeError::CompileError {
            message: "`escaping` cannot be combined with `as`".to_string(),
            span,
        });
    }
    if !matches!(ty, Type::Func { .. }) {
        errors.push(TypeError::CompileError {
            message: "`escaping` is only valid on function-typed parameters".to_string(),
            span,
        });
    }
}

fn validate_cast_froms(
    decls: &DeclarationIndex,
    extend: &ExtendSchema,
    errors: &mut Vec<TypeError>,
) {
    for cast in &extend.cast_froms {
        validate_cast_from_param(cast, errors);
        if same_extend_target(
            &cast.param.ty,
            &extend.generics,
            &extend.target,
            &extend.generics,
        ) {
            errors.push(TypeError::Decl(DeclError::PointlessCastFrom {
                ty: cast.param.ty.clone(),
                span: Some(cast.span),
            }));
        }
        if let Some(ret) = &cast.ret
            && !same_extend_target(&ret.ty, &extend.generics, &extend.target, &extend.generics)
        {
            errors.push(TypeError::Decl(DeclError::CastFromReturnMismatch {
                expected: extend.target.clone(),
                found: ret.ty.clone(),
                span: Some(cast.span),
            }));
        }
        if has_duplicate_cast_from(decls, extend, cast) {
            errors.push(TypeError::Decl(DeclError::DuplicateCastFrom {
                target: extend.target.clone(),
                source: cast.param.ty.clone(),
                span: Some(cast.span),
            }));
        }
    }
}

fn validate_cast_from_param(cast: &CastConversionSchema, errors: &mut Vec<TypeError>) {
    validate_param_escape(
        errors,
        cast.param.escape,
        cast.param.mutable,
        cast.param.cast_accept,
        &cast.param.ty,
        Some(cast.span),
    );
}

fn has_duplicate_cast_from(
    decls: &DeclarationIndex,
    extend: &ExtendSchema,
    cast: &CastConversionSchema,
) -> bool {
    for other_extend in decls.extends() {
        if other_extend.id == extend.id {
            for other in &other_extend.cast_froms {
                if std::ptr::eq(other, cast) {
                    return false;
                }
                if same_extend_target(
                    &other.param.ty,
                    &other_extend.generics,
                    &cast.param.ty,
                    &extend.generics,
                ) {
                    return true;
                }
            }
            continue;
        }
        if other_extend.origin != extend.origin
            || !same_extend_target(
                &other_extend.target,
                &other_extend.generics,
                &extend.target,
                &extend.generics,
            )
        {
            continue;
        }
        if other_extend.cast_froms.iter().any(|other| {
            same_extend_target(
                &other.param.ty,
                &other_extend.generics,
                &cast.param.ty,
                &extend.generics,
            )
        }) {
            return true;
        }
    }
    false
}

fn unsupported_extend_target(ty: &Type, facts: &TargetFacts) -> bool {
    matches!(ty, Type::Void | Type::Func { .. } | Type::InferReturn) || facts.contains_void
}

fn validate_extend_method_conflicts(
    decls: &DeclarationIndex,
    extend: &ExtendSchema,
    errors: &mut Vec<TypeError>,
) {
    let Some(key) = decls.key_for_type(&extend.target) else {
        return;
    };
    if key.module != extend.origin {
        return;
    }
    if let Some(aggregate) = decls.aggregate(&key) {
        for method_key in extend.methods.keys() {
            if aggregate.methods.contains_key(method_key) {
                push_extend_method_conflict(errors, extend, *method_key);
            }
        }
    }
    if let Some(enum_schema) = decls.enum_schema(&key) {
        for method_key in extend.methods.keys() {
            if method_key.surface == MethodSurface::Static
                && enum_schema.variants.contains_key(&method_key.name)
            {
                push_extend_method_conflict(errors, extend, *method_key);
            }
        }
    }
}

fn push_extend_method_conflict(
    errors: &mut Vec<TypeError>,
    extend: &ExtendSchema,
    method_key: MethodKey,
) {
    errors.push(TypeError::Decl(DeclError::ExtendMethodConflict {
        ty: extend.target.clone(),
        name: method_key.name,
        surface: method_key.surface,
        span: Some(extend.span),
    }));
}

#[derive(Default)]
struct TargetFacts {
    contains_void: bool,
    type_params: HashSet<TypeVarId>,
    const_params: HashSet<ConstParamId>,
}

impl TypeVisitor for TargetFacts {
    fn visit_type_leaf(&mut self, ty: &Type) -> bool {
        match ty {
            Type::Void => self.contains_void = true,
            Type::Var(id) => {
                self.type_params.insert(*id);
            }
            _ => {}
        }
        false
    }

    fn visit_const_arg(&mut self, arg: &ConstArg) -> bool {
        if let ConstArg::Param(id) = arg {
            self.const_params.insert(*id);
        }
        false
    }

    fn visit_array_len(&mut self, len: ArrayLen) -> bool {
        if let ArrayLen::Param(id) = len {
            self.const_params.insert(id);
        }
        false
    }
}

fn target_facts(ty: &Type) -> TargetFacts {
    let mut facts = TargetFacts::default();
    facts.visit_type(ty);
    facts
}

fn type_ref_error(error: TypeRefError, span: Option<SourceSpan>) -> TypeError {
    match error {
        TypeRefError::Unknown { qualifier, name } => TypeError::UnknownType {
            qualifier,
            name,
            span,
        },
        TypeRefError::GenericArity { expected, found } => {
            TypeError::GenericArity(ArityError::TypeArgs { expected, found })
        }
        TypeRefError::GenericArgKindMismatch { expected } => {
            TypeError::GenericArgKindMismatch { expected, span }
        }
        TypeRefError::AliasCycle { name } => TypeError::CompileError {
            message: format!("type alias '{name}' depends on itself"),
            span,
        },
        TypeRefError::ContractAsType { name } => TypeError::CompileError {
            message: format!(
                "contract '{name}' is not a concrete type; use 'dyn {name}' or a generic bound"
            ),
            span,
        },
        TypeRefError::UnknownContract { qualifier, name } => TypeError::CompileError {
            message: match qualifier {
                Some(qualifier) => format!("unknown contract '{qualifier}.{name}'"),
                None => format!("unknown contract '{name}'"),
            },
            span,
        },
        TypeRefError::DuplicateContractRequirement { name } => TypeError::CompileError {
            message: format!("duplicate contract requirement '{name}'"),
            span,
        },
        TypeRefError::ConflictingContractRequirement { name } => TypeError::CompileError {
            message: format!("conflicting contract requirement '{name}'"),
            span,
        },
        TypeRefError::UnsupportedContractComposition => TypeError::CompileError {
            message: "inferred dynamic contracts are not supported yet".to_string(),
            span,
        },
    }
}

struct ReturnSpecValidator<'a> {
    tc: &'a mut TypeChecker,
    span: Span,
}

impl TypeVisitor for ReturnSpecValidator<'_> {
    fn visit_type_leaf(&mut self, ty: &Type) -> bool {
        if let Type::Func { params, ret } = ty {
            validate_return_spec(
                ret,
                false,
                has_mutable_func_param(params),
                self.span,
                self.tc,
            );
        }
        false
    }

    fn visit_contract_ref_leaf(&mut self, contract: &ContractRef) -> bool {
        if let ContractRef::Anonymous(surface) = contract {
            for req in &surface.requirements {
                validate_unsupported_return_spec(
                    &req.ret,
                    "contract requirements cannot return mutable places",
                    self.span,
                    self.tc,
                );
            }
        }
        false
    }
}

fn has_generics(type_params: &[TypeParam], const_params: &[ConstParam]) -> bool {
    !type_params.is_empty() || !const_params.is_empty()
}

fn is_generic(func: &Func) -> bool {
    has_generics(&func.type_params, &func.const_params)
}

fn method_sig_is_generic(sig: &MethodSig) -> bool {
    has_generics(&sig.type_params, &sig.const_params)
}

fn check_infer_return_decls(program: &Program, tc: &mut TypeChecker) {
    for stmt in &program.stmts {
        match &stmt.node {
            Stmt::Func(func_node) => {
                let func = &func_node.node;
                validate_return_spec(
                    &func.ret,
                    is_generic(func),
                    has_mutable_param(&func.params),
                    func_node.span,
                    tc,
                );
            }
            Stmt::ExternFunc(func_node) => {
                validate_unsupported_return_spec(
                    &func_node.node.ret,
                    "extern functions cannot return mutable places",
                    func_node.span,
                    tc,
                );
            }
            Stmt::Aggregate(agg_node) => {
                let agg = &agg_node.node;
                let owner_is_generic = has_generics(&agg.type_params, &agg.const_params);
                for method in &agg.methods {
                    validate_return_spec(
                        &method.sig.ret,
                        owner_is_generic || method_sig_is_generic(&method.sig),
                        method_has_mutable_input(&method.sig),
                        agg_node.span,
                        tc,
                    );
                }
            }
            Stmt::Extend(extend_node) => {
                let extend = &extend_node.node;
                let owner_is_generic = has_generics(&extend.type_params, &extend.const_params);
                for method in &extend.methods {
                    validate_return_spec(
                        &method.node.sig.ret,
                        owner_is_generic || method_sig_is_generic(&method.node.sig),
                        method_has_mutable_input(&method.node.sig),
                        method.span,
                        tc,
                    );
                }
                for cast in &extend.cast_froms {
                    if let Some(ret) = &cast.node.ret {
                        validate_unsupported_return_spec(
                            ret,
                            "cast from declarations cannot return mutable places",
                            cast.span,
                            tc,
                        );
                    }
                }
            }
            Stmt::Contract(contract_node) => {
                for req in &contract_node.node.requirements {
                    validate_unsupported_return_spec(
                        &req.node.sig.ret,
                        "contract requirements cannot return mutable places",
                        req.span,
                        tc,
                    );
                }
            }
            _ => {}
        }
    }
}

fn has_mutable_param(params: &[Param]) -> bool {
    params
        .iter()
        .any(|param| matches!(param.mutability, Mutability::Mutable))
}

fn has_mutable_func_param(params: &[FuncParam]) -> bool {
    params.iter().any(|param| param.mutable)
}

fn method_has_mutable_input(sig: &MethodSig) -> bool {
    matches!(sig.receiver, Some(MethodReceiver::Var)) || has_mutable_param(&sig.params)
}

fn validate_return_spec(
    ret: &ReturnSpec,
    generic: bool,
    first_input_mutable: bool,
    span: Span,
    tc: &mut TypeChecker,
) {
    if ret.is_infer() && !generic {
        tc.push_error(TypeError::InferReturnNonGeneric {
            span: tc.error_span(span),
        });
    }
    validate_place_return_spec(ret, first_input_mutable, span, tc);
}

fn validate_unsupported_return_spec(
    ret: &ReturnSpec,
    place_message: &'static str,
    span: Span,
    tc: &mut TypeChecker,
) {
    if ret.is_infer() {
        tc.push_error(TypeError::InferReturnExtern {
            span: tc.error_span(span),
        });
    }
    if ret.is_place() {
        tc.push_error(TypeError::UnsupportedPlaceReturn {
            message: place_message,
            span: tc.error_span(span),
        });
    }
}

fn validate_place_return_spec(
    ret: &ReturnSpec,
    first_input_mutable: bool,
    span: Span,
    tc: &mut TypeChecker,
) {
    if !ret.is_place() {
        return;
    }
    if ret.is_void() {
        tc.push_error(TypeError::UnsupportedPlaceReturn {
            message: "mutable place returns cannot return void",
            span: tc.error_span(span),
        });
    }
    if !first_input_mutable {
        tc.push_error(TypeError::UnsupportedPlaceReturn {
            message: "mutable place returns require a first mutable input",
            span: tc.error_span(span),
        });
    }
}

fn collect_callable_templates(module: ModuleScope, program: &Program, tc: &mut TypeChecker) {
    let mut extend_index = 0;

    for stmt in &program.stmts {
        match &stmt.node {
            Stmt::Func(func_node) => {
                let func = &func_node.node;
                if !is_generic(func) {
                    continue;
                }
                let generics =
                    tc.generic_context(&func.type_params, &func.const_params, func_node.span);
                tc.store_callable_template(
                    CallableId::function(module.clone(), func.name),
                    CallableTemplate {
                        span: func_node.span,
                        mode: MethodMode::Static,
                        generics,
                        env: CallableTemplateEnv::SourceModule,
                        params: func.params.clone(),
                        ret: func.ret.clone(),
                        ret_span: func_node.span,
                        body: func.body.clone(),
                    },
                );
            }
            Stmt::Aggregate(agg_node) => {
                let agg = &agg_node.node;
                let owner = NominalKey {
                    module: module.clone(),
                    kind: agg.kind.into(),
                    name: agg.name,
                };
                let owner_is_generic = has_generics(&agg.type_params, &agg.const_params);
                let has_generic_method = agg
                    .methods
                    .iter()
                    .any(|method| method_sig_is_generic(&method.sig));
                if !owner_is_generic && !has_generic_method {
                    continue;
                }
                let owner_generics =
                    tc.generic_context(&agg.type_params, &agg.const_params, agg_node.span);
                for method in &agg.methods {
                    let method_is_generic = method_sig_is_generic(&method.sig);
                    if !owner_is_generic && !method_is_generic {
                        continue;
                    }
                    let generics = tc.extended_generic_context(
                        &owner_generics,
                        &method.sig.type_params,
                        &method.sig.const_params,
                        agg_node.span,
                    );
                    let mode = MethodMode::from_receiver(method.sig.receiver);
                    tc.store_callable_template(
                        CallableId::aggregate_method(
                            owner.clone(),
                            method.sig.name,
                            mode.surface(),
                        ),
                        CallableTemplate {
                            span: agg_node.span,
                            mode,
                            generics,
                            env: CallableTemplateEnv::SourceModule,
                            params: method.sig.params.clone(),
                            ret: method.sig.ret.clone(),
                            ret_span: agg_node.span,
                            body: method.body.clone(),
                        },
                    );
                }
            }
            Stmt::Extend(extend_node) => {
                let extend = &extend_node.node;
                let extend_id = ExtendId {
                    module: module.clone(),
                    index: extend_index,
                };
                extend_index += 1;
                let owner_is_generic = has_generics(&extend.type_params, &extend.const_params);
                let has_generic_method = extend
                    .methods
                    .iter()
                    .any(|method| method_sig_is_generic(&method.node.sig));
                if !owner_is_generic && !has_generic_method {
                    continue;
                }
                let owner_generics =
                    tc.generic_context(&extend.type_params, &extend.const_params, extend_node.span);
                for method_node in &extend.methods {
                    let method = &method_node.node;
                    let method_is_generic = method_sig_is_generic(&method.sig);
                    if !owner_is_generic && !method_is_generic {
                        continue;
                    }
                    let mode = MethodMode::from_receiver(method.sig.receiver);
                    let generics = tc.extended_generic_context(
                        &owner_generics,
                        &method.sig.type_params,
                        &method.sig.const_params,
                        method_node.span,
                    );
                    tc.store_callable_template(
                        CallableId::extend_method(
                            extend_id.clone(),
                            method.sig.name,
                            mode.surface(),
                        ),
                        CallableTemplate {
                            span: method_node.span,
                            mode,
                            generics,
                            env: CallableTemplateEnv::SourceModule,
                            params: method.sig.params.clone(),
                            ret: method.sig.ret.clone(),
                            ret_span: method_node.span,
                            body: method.body.clone(),
                        },
                    );
                }
            }
            _ => {}
        }
    }
}

fn type_contains_anonymous_contract(ty: &Type) -> bool {
    struct AnonymousContractVisitor;

    impl TypeVisitor for AnonymousContractVisitor {
        fn visit_contract_ref_leaf(&mut self, contract: &ContractRef) -> bool {
            matches!(contract, ContractRef::Anonymous(_))
        }
    }

    let mut visitor = AnonymousContractVisitor;
    visitor.visit_type(ty)
}

fn type_contains_raw_dyn_infer_func(ty: &Type) -> bool {
    struct RawDynInferFunc;

    impl TypeVisitor for RawDynInferFunc {
        fn visit_type(&mut self, ty: &Type) -> bool {
            match ty {
                Type::Func { params, ret } => {
                    params
                        .iter()
                        .any(|param| DynInference::has_raw_hole(&param.ty))
                        || DynInference::has_raw_hole(&ret.ty)
                }
                _ => self.visit_type_children(ty),
            }
        }
    }

    let mut visitor = RawDynInferFunc;
    visitor.visit_type(ty)
}

fn push_source_scope(tc: &mut TypeChecker) {
    tc.push_scope();
    register_builtins(tc);
}

fn register_builtins(tc: &mut TypeChecker) {
    let builtins = [
        ("println", vec![FuncParam::immut(Type::Any)], Type::Void),
        ("assert", vec![FuncParam::immut(Type::Bool)], Type::Void),
        (
            "assert_msg",
            vec![FuncParam::immut(Type::Bool), FuncParam::immut(Type::String)],
            Type::Void,
        ),
    ];

    for (name, params, ret) in builtins {
        tc.define(
            Ident::new(name),
            Type::Func {
                params,
                ret: Box::new(ReturnSpec::value(ret)),
            },
            false,
        );
    }
}

fn register_declarations(program: &Program, tc: &mut TypeChecker) {
    let extern_functions = tc
        .externs
        .functions_in_scope(&tc.current_module)
        .map(|function| (function.key.name, function.signature.to_func_type()))
        .collect::<Vec<_>>();
    for (name, ty) in extern_functions {
        tc.define(name, ty, false);
    }

    let register_dyn_infer = tc.should_register_dyn_infer_params();

    for stmt in &program.stmts {
        match &stmt.node {
            Stmt::Func(func_node) => {
                let func = &func_node.node;
                if is_generic(func) {
                    continue;
                }
                let func_ty = if register_dyn_infer {
                    let func_ty = tc.callable_type_from_sig(
                        &func.params,
                        &func.ret,
                        func_node.span,
                        matches!(func.visibility, Visibility::Public),
                    );
                    tc.decls
                        .set_func_type(&tc.current_module, func.name, func_ty.clone());
                    func_ty
                } else {
                    tc.decls
                        .local_value(&tc.current_module, func.name)
                        .map_or_else(
                            || {
                                debug_assert!(
                                    false,
                                    "registered function missing declaration type"
                                );
                                Type::Infer
                            },
                            |value| value.decl.ty().clone(),
                        )
                };
                tc.define(func.name, func_ty, false);
            }
            Stmt::Aggregate(_) | Stmt::Enum(_) => {}
            Stmt::ExternFunc(_) | Stmt::ExternType(_) => {}
            Stmt::Const(const_node) => {
                let c = &const_node.node;
                let ty = match &c.ty {
                    Some(t) => tc.resolve_type_for_tc_at(t, const_node.span),
                    None => Type::Infer,
                };
                tc.define_value(c.name, ty, LocalBindingKind::constant(), None);
            }
            _ => {}
        }
    }

    if register_dyn_infer {
        register_callable_dyn_infer_params(program, tc);
    }
}

fn register_callable_dyn_infer_params(program: &Program, tc: &mut TypeChecker) {
    let module = tc.current_module.clone();
    let mut extend_index = 0;

    for stmt in &program.stmts {
        let exported = matches!(stmt_visibility(stmt), Visibility::Public);
        match &stmt.node {
            Stmt::Func(func_node)
                if is_generic(&func_node.node)
                    && callable_sig_has_raw_dyn_infer(
                        &func_node.node.params,
                        &func_node.node.ret,
                    ) =>
            {
                let sig = source_func_sig(&func_node.node, func_node.span, tc);
                tc.decls
                    .set_func_type(&module, func_node.node.name, sig.surface_ty);
            }
            Stmt::Aggregate(agg_node) => {
                register_aggregate_method_dyn_infer_params(agg_node, &module, exported, tc);
            }
            Stmt::Extend(extend_node) => {
                let id = ExtendId {
                    module: module.clone(),
                    index: extend_index,
                };
                extend_index += 1;
                register_extend_method_dyn_infer_params(extend_node, &id, exported, tc);
            }
            _ => {}
        }
    }
}

fn register_aggregate_method_dyn_infer_params(
    agg_node: &AggregateDeclNode,
    module: &ModuleScope,
    exported: bool,
    tc: &mut TypeChecker,
) {
    let agg = &agg_node.node;
    let key = NominalKey {
        module: module.clone(),
        kind: agg.kind.into(),
        name: agg.name,
    };
    let owner_generics = tc.generic_context(&agg.type_params, &agg.const_params, agg_node.span);
    for method in &agg.methods {
        if !callable_sig_has_raw_dyn_infer(&method.sig.params, &method.sig.ret) {
            continue;
        }
        let mode = MethodMode::from_receiver(method.sig.receiver);
        let generics = tc.extended_generic_context(
            &owner_generics,
            &method.sig.type_params,
            &method.sig.const_params,
            agg_node.span,
        );
        let (params, ret) = resolve_callable_sig_types(
            &method.sig.params,
            &method.sig.ret,
            generics,
            agg_node.span,
            exported,
            tc,
        );
        let Some(schema) = tc.decls.aggregate_mut(&key) else {
            continue;
        };
        let Some(method_schema) = schema
            .methods
            .get_mut(&MethodKey::new(method.sig.name, mode.surface()))
        else {
            continue;
        };
        method_schema.params = params;
        method_schema.ret = ret;
    }
}

fn register_extend_method_dyn_infer_params(
    extend_node: &ExtendDeclNode,
    id: &ExtendId,
    exported: bool,
    tc: &mut TypeChecker,
) {
    let extend = &extend_node.node;
    let owner_generics =
        tc.generic_context(&extend.type_params, &extend.const_params, extend_node.span);
    for method_node in &extend.methods {
        let method = &method_node.node;
        if !callable_sig_has_raw_dyn_infer(&method.sig.params, &method.sig.ret) {
            continue;
        }
        let mode = MethodMode::from_receiver(method.sig.receiver);
        let generics = tc.extended_generic_context(
            &owner_generics,
            &method.sig.type_params,
            &method.sig.const_params,
            method_node.span,
        );
        let (params, ret) = resolve_callable_sig_types(
            &method.sig.params,
            &method.sig.ret,
            generics,
            method_node.span,
            exported,
            tc,
        );
        let Some(extend) = tc.decls.extend_mut(id) else {
            continue;
        };
        let Some(method_schema) = extend
            .methods
            .get_mut(&MethodKey::new(method.sig.name, mode.surface()))
        else {
            continue;
        };
        method_schema.params = params;
        method_schema.ret = ret;
    }
}

fn callable_sig_has_raw_dyn_infer(params: &[Param], ret: &ReturnSpec) -> bool {
    params
        .iter()
        .any(|param| DynInference::has_raw_hole(&param.ty))
        || DynInference::has_raw_hole(&ret.ty)
}

fn resolve_callable_sig_types(
    params: &[Param],
    ret: &ReturnSpec,
    generics: GenericTypeContext,
    span: Span,
    exported: bool,
    tc: &mut TypeChecker,
) -> (Vec<FuncParam>, ReturnSpec) {
    tc.push_generic_context(generics);
    let params = tc.resolve_callable_params(params, span, exported);
    let ret = ret.with_ty(tc.resolve_type_for_tc_at(&ret.ty, span));
    tc.pop_generic_context();
    (params, ret)
}

fn check_stmts(stmts: &[StmtNode], tc: &mut TypeChecker) {
    for stmt in stmts {
        check_stmt(stmt, None, tc);
    }
}

fn source_func_sig(func: &Func, span: Span, tc: &mut TypeChecker) -> SourceFuncSig {
    let owner = tc.visible_generic_owner();
    let mut generics = generic_params(&func.type_params, &func.const_params);
    let generic_context =
        tc.extended_generic_context(&owner.generics, &func.type_params, &func.const_params, span);
    check_param_order(&func.params, span, tc);
    validate_return_spec(
        &func.ret,
        !generics.is_empty() || !owner.params.is_empty(),
        has_mutable_param(&func.params),
        span,
        tc,
    );

    tc.push_generic_context(generic_context.clone());
    tc.resolve_generic_bounds_for_tc(&mut generics, span);
    let exported = matches!(func.visibility, Visibility::Public);
    let params = tc.resolve_callable_params(&func.params, span, exported);
    let ret = func
        .ret
        .with_ty(tc.resolve_type_for_tc_at(&func.ret.ty, span));
    tc.pop_generic_context();

    SourceFuncSig {
        owner_generics: owner.params,
        owner_args: owner.args,
        generics,
        generic_context,
        required_params: required_param_count(&func.params),
        surface_ty: Type::Func {
            params: params.clone(),
            ret: Box::new(ret.clone()),
        },
        params,
        ret,
    }
}

fn register_local_type_aliases(stmts: &[StmtNode], tc: &mut TypeChecker) {
    for stmt in stmts {
        let Stmt::TypeAlias(alias_node) = &stmt.node else {
            continue;
        };
        let alias = &alias_node.node;
        let owner = tc.generic_contexts.last().cloned().unwrap_or_default();
        let generic_context = tc.extended_generic_context(
            &owner,
            &alias.type_params,
            &alias.const_params,
            alias_node.span,
        );
        let mut errors = vec![];
        let policy = annotation::normalize_annotations(
            tc.source_id(),
            &alias.annotations,
            annotation::AnnotationTarget::TypeAlias,
            &mut errors,
        );
        tc.errors.extend(errors.into_iter().map(TypeError::Decl));
        let local = LocalTypeAlias {
            key: alias_node.span,
            def: TypeAliasDef {
                module: tc.current_module.clone(),
                name: alias.name,
                generics: generic_params(&alias.type_params, &alias.const_params),
                generic_context,
                aliased: alias.aliased.clone(),
                policy,
                span: tc.source_span(alias_node.span),
            },
            visible_depth: tc.local_type_scopes.depth(),
        };
        if !tc.local_type_scopes.insert(local) {
            tc.push_error(TypeError::Decl(DeclError::DuplicateType {
                module: tc.current_module.clone(),
                name: alias.name,
                span: tc.error_span(alias_node.span),
            }));
        }
    }
}

fn register_block_declarations(
    stmts: &[StmtNode],
    tc: &mut TypeChecker,
) -> Vec<Option<LocalConstInfo>> {
    let mut declarations = vec![None; stmts.len()];
    let mut funcs = vec![];
    register_local_type_aliases(stmts, tc);
    let mut sig_env = tc.scopes.clone();
    add_callable_decl_placeholders(stmts, &mut sig_env, tc);
    for (index, stmt) in stmts.iter().enumerate() {
        match &stmt.node {
            Stmt::Func(func_node) => {
                let func = &func_node.node;
                let module = tc.current_module.clone();
                let env = CallableTemplateEnv::Local(tc.scope_state_from(sig_env.clone()));
                let sig = with_callable_body_env(&module, &env, tc, |tc| {
                    source_func_sig(func, func_node.span, tc)
                });
                let id = CallableId::local_function(
                    tc.current_module.clone(),
                    func.name,
                    func_node.span,
                );
                funcs.push(LocalFuncDecl {
                    id,
                    sig,
                    func: func_node.clone(),
                });
            }
            Stmt::Const(const_node) => {
                let info = tc.declare_local_const(
                    const_node,
                    CallableTemplateEnv::Local(tc.scope_state_from(sig_env.clone())),
                );
                add_env_symbol(const_node.node.name, info.symbol(), &mut sig_env);
                declarations[index] = Some(info);
            }
            _ => add_stmt_capture_blockers(&stmt.node, &mut sig_env, tc),
        }
    }

    for decl in &funcs {
        let func = &decl.func.node;
        let callee = CallableRef {
            def: CallableDef {
                id: decl.id.clone(),
                sig: CallableSig {
                    owner_generics: decl.sig.owner_generics.clone(),
                    generics: decl.sig.generics.clone(),
                    params: decl.sig.params.clone(),
                    required_params: decl.sig.required_params,
                    ret: decl.sig.ret.clone(),
                },
            },
            receiver_ty: None,
            owner_args: decl.sig.owner_args.clone(),
        };
        tc.define_local_callable(func.name, callee, decl.sig.surface_ty.clone());
    }

    let mut funcs = funcs.into_iter();
    let mut env = tc.scopes.clone();
    for (stmt, local_const) in stmts.iter().zip(declarations.iter().copied()) {
        match &stmt.node {
            Stmt::Func(_) => {
                let decl = funcs.next().expect("function declaration was collected");
                store_local_callable_template(decl, env.clone(), tc);
            }
            Stmt::Const(const_node) => {
                let Some(info) = local_const else {
                    continue;
                };
                tc.set_local_const_env(
                    info.id,
                    CallableTemplateEnv::Local(tc.scope_state_from(env.clone())),
                );
                add_env_symbol(const_node.node.name, info.symbol(), &mut env);
            }
            _ => add_stmt_capture_blockers(&stmt.node, &mut env, tc),
        }
    }

    declarations
}

fn add_stmt_capture_blockers(
    stmt: &Stmt,
    env: &mut [HashMap<Ident, LocalSymbol>],
    tc: &mut TypeChecker,
) {
    match stmt {
        Stmt::Binding(binding) => add_pattern_capture_blockers(
            &binding.node.pattern,
            LocalBindingKind::from_mutable(matches!(binding.node.mutability, Mutability::Mutable)),
            env,
            tc,
        ),
        Stmt::LetElse(let_else) => add_pattern_capture_blockers(
            &let_else.node.pattern,
            LocalBindingKind::from_mutable(matches!(let_else.node.head, PatternHead::Var)),
            env,
            tc,
        ),
        _ => {}
    }
}

fn add_callable_decl_placeholders(
    stmts: &[StmtNode],
    env: &mut [HashMap<Ident, LocalSymbol>],
    tc: &mut TypeChecker,
) {
    for stmt in stmts {
        let Stmt::Func(func_node) = &stmt.node else {
            continue;
        };
        let func = &func_node.node;
        let id = CallableId::local_function(tc.current_module.clone(), func.name, func_node.span);
        let callee = CallableRef {
            def: CallableDef {
                id,
                sig: CallableSig {
                    owner_generics: GenericParams::default(),
                    generics: GenericParams::default(),
                    params: vec![],
                    required_params: 0,
                    ret: ReturnSpec::value(Type::Infer),
                },
            },
            receiver_ty: None,
            owner_args: GenericArgs::default(),
        };
        let binding_id = tc.fresh_binding_id();
        let type_id = tc.solver.alloc_local_type(&Type::Infer);
        add_env_symbol(
            func.name,
            LocalSymbol::Callable(Box::new(LocalCallableInfo {
                binding_id,
                type_id,
                callee,
            })),
            env,
        );
    }
}

fn store_local_callable_template(
    decl: LocalFuncDecl,
    env: Vec<HashMap<Ident, LocalSymbol>>,
    tc: &mut TypeChecker,
) {
    let has_template = is_generic(&decl.func.node)
        || decl.func.node.ret.is_infer()
        || !decl.sig.owner_generics.is_empty();
    if !has_template {
        return;
    }
    tc.store_callable_template(
        decl.id,
        CallableTemplate {
            span: decl.func.span,
            mode: MethodMode::Static,
            generics: decl.sig.generic_context,
            env: CallableTemplateEnv::Local(tc.scope_state_from(env)),
            params: decl.func.node.params.clone(),
            ret: decl.func.node.ret.clone(),
            ret_span: decl.func.span,
            body: decl.func.node.body.clone(),
        },
    );
}

fn add_capture_blocker(
    name: Ident,
    kind: LocalBindingKind,
    env: &mut [HashMap<Ident, LocalSymbol>],
    tc: &mut TypeChecker,
) {
    debug_assert!(kind.requires_runtime_capture());
    let binding_id = tc.fresh_binding_id();
    let type_id = tc.solver.alloc_local_type(&Type::Infer);
    add_env_symbol(
        name,
        LocalSymbol::Value(VarInfo {
            binding_id,
            type_id,
            kind,
            const_value: None,
            local_const: None,
            alias: None,
        }),
        env,
    );
}

fn add_env_symbol(name: Ident, symbol: LocalSymbol, env: &mut [HashMap<Ident, LocalSymbol>]) {
    if let Some(scope) = env.last_mut() {
        scope.insert(name, symbol);
    }
}

fn add_pattern_capture_blockers(
    pattern: &PatternNode,
    kind: LocalBindingKind,
    env: &mut [HashMap<Ident, LocalSymbol>],
    tc: &mut TypeChecker,
) {
    match &pattern.node {
        Pattern::Ident(name) => add_capture_blocker(*name, kind, env, tc),
        Pattern::Tuple(fields)
        | Pattern::EnumTuple { fields, .. }
        | Pattern::InferredEnumTuple { fields, .. }
        | Pattern::Or(fields) => {
            for field in fields {
                add_pattern_capture_blockers(field, kind, env, tc);
            }
        }
        Pattern::Struct { fields, .. }
        | Pattern::EnumStruct { fields, .. }
        | Pattern::InferredEnumStruct { fields, .. } => {
            for (_, field) in fields {
                add_pattern_capture_blockers(field, kind, env, tc);
            }
        }
        Pattern::Optional(inner) => add_pattern_capture_blockers(inner, kind, env, tc),
        Pattern::Wildcard
        | Pattern::EnumUnit { .. }
        | Pattern::InferredEnumUnit { .. }
        | Pattern::Range { .. }
        | Pattern::Lit(_)
        | Pattern::Rest
        | Pattern::Nil => {}
    }
}

fn check_decl_param_order(program: &Program, tc: &mut TypeChecker) {
    for stmt in &program.stmts {
        match &stmt.node {
            Stmt::Func(func) => check_param_order(&func.node.params, func.span, tc),
            Stmt::Aggregate(agg) => {
                for method in &agg.node.methods {
                    check_param_order(&method.sig.params, agg.span, tc);
                }
            }
            Stmt::Extend(extend) => {
                for method in &extend.node.methods {
                    check_param_order(&method.node.sig.params, method.span, tc);
                }
            }
            _ => {}
        }
    }
}

fn check_param_order(params: &[Param], span: Span, tc: &mut TypeChecker) {
    let mut saw_default = false;
    for param in params {
        if param.default.is_some() {
            saw_default = true;
        } else if saw_default {
            tc.push_error(TypeError::RequiredParamAfterDefault {
                name: param.name,
                span: tc.error_span(span),
            });
        }
    }
}

fn check_aggregate_decl(agg_node: &AggregateDeclNode, tc: &mut TypeChecker) {
    let agg = &agg_node.node;
    let key = NominalKey {
        module: tc.current_module.clone(),
        kind: agg.kind.into(),
        name: agg.name,
    };
    let Some(schema) = tc.decls.aggregate(&key).cloned() else {
        return;
    };

    check_aggregate_field_defaults(&agg.fields, &schema.fields, tc);
    check_method_generic_shadows(agg, agg_node.span, tc);
    check_aggregate_method_bodies(agg, agg_node.span, &key, &schema, tc);
}

fn check_aggregate_field_defaults(
    fields: &[StructField],
    schema: &HashMap<Ident, FieldSchema>,
    tc: &mut TypeChecker,
) {
    for field in fields {
        let Some(default) = &field.default else {
            continue;
        };
        let Some(schema) = schema.get(&field.name) else {
            continue;
        };
        if type_depends_on_generics(&schema.ty) {
            tc.push_error(TypeError::GenericFieldDefault {
                span: tc.error_span(default.span),
            });
            continue;
        }
        let expected = tc.type_handle(&schema.ty);
        if let Err(error) = validate_const_expr_type(default, Some(expected), tc) {
            tc.push_error(error);
            continue;
        }
        if matches!(default.node.kind, ExprKind::Lit(Lit::Nil))
            && tc.decls.core_option_inner(&schema.ty).is_some()
        {
            continue;
        }
        if empty_heap_collection_default(default, &schema.ty) {
            continue;
        }
        if let Err(error) = tc.eval_const_expr(default, false) {
            tc.push_error(error);
        }
    }
}

fn empty_heap_collection_default(expr: &ExprNode, ty: &Type) -> bool {
    match (&expr.node.kind, ty) {
        (ExprKind::ArrayLiteral(lit), Type::List { .. }) => lit.node.elements.is_empty(),
        (ExprKind::MapLiteral(lit), Type::Map { .. }) => lit.node.entries.is_empty(),
        _ => false,
    }
}

fn check_module_bodies(module: &ModuleScope, program: &Program, tc: &mut TypeChecker) {
    with_source_module_scope(module, tc, |tc| check_stmts(&program.stmts, tc));
}

fn check_stmt(stmt: &StmtNode, local_const: Option<LocalConstInfo>, tc: &mut TypeChecker) {
    match &stmt.node {
        Stmt::Func(func_node) => {
            let func = &func_node.node;
            let id =
                CallableId::local_function(tc.current_module.clone(), func.name, func_node.span);
            let local = tc.local_callable(&id);
            if is_generic(func) && local.is_none() {
                return;
            }
            if let Some(info) = local
                && tc.callable_template(&info.callee.def.id).is_some()
            {
                return;
            }
            check_func(func_node, tc);
        }
        Stmt::Binding(binding_node) => {
            check_binding(binding_node, tc);
        }
        Stmt::Return(ret_node) => {
            check_return(ret_node, tc);
        }
        Stmt::Expr(expr_node) => {
            tc.discard_depth += 1;
            check_expr_checked(expr_node, tc);
            tc.discard_depth -= 1;
        }
        Stmt::While(while_node) => {
            check_while(while_node, tc);
        }
        Stmt::WhileLet(while_let_node) => {
            check_while_let(while_let_node, tc);
        }
        Stmt::For(for_node) => {
            check_for(for_node, tc);
        }
        Stmt::Break => {
            check_break(stmt.span, tc);
        }
        Stmt::Continue => {
            check_continue(stmt.span, tc);
        }
        Stmt::Extend(extend_node) => {
            check_extend(extend_node, tc);
        }
        Stmt::Aggregate(agg_node) => {
            check_aggregate_decl(agg_node, tc);
        }
        Stmt::Enum(_) | Stmt::Contract(_) => {}
        Stmt::Const(const_node) => {
            if tc.scopes.len() > 1 {
                match local_const {
                    Some(info) => {
                        if tc.define_local_symbol(const_node.node.name, info.symbol()) {
                            tc.define_closure_binding(
                                info.binding_id,
                                const_node.node.name,
                                info.type_id,
                                LocalBindingKind::constant(),
                            );
                        }
                        if let Err(err) = tc.eval_local_const(info.id, const_node.span) {
                            tc.push_error(err);
                        }
                    }
                    None => check_const(const_node, tc),
                }
            }
        }
        Stmt::Global(_) => {}
        Stmt::TypeAlias(alias_node) => {
            check_type_alias(alias_node, tc);
        }
        Stmt::LetElse(let_else_node) => {
            check_let_else(let_else_node, tc);
        }
        Stmt::Defer(defer_node) => {
            check_defer(defer_node, tc);
        }
        Stmt::Import(_) | Stmt::ExternFunc(_) | Stmt::ExternType(_) => {}
    }
}

fn check_defer(defer_node: &DeferNode, tc: &mut TypeChecker) {
    if tc.in_global_initializer() {
        tc.push_error(TypeError::CompileError {
            message: "defer is not allowed in runtime global initializers".to_string(),
            span: tc.error_span(defer_node.span),
        });
        check_defer_body(&defer_node.node.body, tc);
        return;
    }

    tc.enter_defer();
    check_defer_body(&defer_node.node.body, tc);
    tc.exit_defer();
}

fn check_defer_body(body: &DeferBody, tc: &mut TypeChecker) {
    match body {
        DeferBody::Expr(expr) => {
            check_expr_checked(expr, tc);
        }
        DeferBody::Block(block) => {
            check_block_checked(block, tc);
        }
    }
}

fn check_func(func_node: &FuncNode, tc: &mut TypeChecker) {
    let func = &func_node.node;
    let id = CallableId::local_function(tc.current_module.clone(), func.name, func_node.span);
    let local = tc.local_callable(&id);
    let (param_types, ret) = match local.as_ref() {
        Some(info) => (&info.callee.def.sig.params, &info.callee.def.sig.ret),
        None => {
            let func_ty = match tc.lookup(func.name) {
                Some(info) => tc.solver.local_type_to_type(info.type_id),
                None => tc.func_type_from_sig(&func.params, &func.ret, func_node.span),
            };
            let Type::Func { params, ret } = func_ty else {
                return;
            };
            check_func_body(
                None,
                &func.params,
                &params,
                ret.as_ref().clone(),
                &func.body,
                func_node.span,
                &[],
                tc,
            );
            return;
        }
    };
    check_func_body(
        None,
        &func.params,
        param_types,
        ret.clone(),
        &func.body,
        func_node.span,
        &[],
        tc,
    );
}

fn check_extend(extend_node: &ExtendDeclNode, tc: &mut TypeChecker) {
    let extend = &extend_node.node;
    let owner_is_generic = has_generics(&extend.type_params, &extend.const_params);
    if owner_is_generic {
        return;
    }

    let self_ty = tc.resolve_type_for_tc_at(&extend.ty, extend_node.span);
    for method_node in &extend.methods {
        let method = &method_node.node;
        if method_sig_is_generic(&method.sig) {
            continue;
        }
        let mode = MethodMode::from_receiver(method.sig.receiver);
        let params = &method.sig.params;
        let key = MethodKey::new(method.sig.name, mode.surface());
        let Some((param_types, ret_ty)) = tc
            .decls
            .extends()
            .find(|extend| {
                extend.origin == tc.current_module && extend.span.byte() == extend_node.span
            })
            .and_then(|extend| extend.methods.get(&key))
            .map(|method| (method.params.clone(), method.ret.clone()))
        else {
            continue;
        };
        check_func_body(
            mode.receiver().map(|receiver| (receiver, self_ty.clone())),
            params,
            &param_types,
            ret_ty,
            &method.body,
            extend_node.span,
            &[],
            tc,
        );
    }
    let cast_schemas = tc
        .decls
        .extends()
        .find(|extend| extend.origin == tc.current_module && extend.span.byte() == extend_node.span)
        .map(|extend| extend.cast_froms.clone())
        .unwrap_or_default();
    for (cast, schema) in extend.cast_froms.iter().zip(cast_schemas) {
        check_func_body(
            None,
            std::slice::from_ref(&cast.node.param),
            std::slice::from_ref(&schema.param),
            ReturnSpec::value(self_ty.clone()),
            &cast.node.body,
            cast.span,
            &[],
            tc,
        );
    }
}

fn check_aggregate_method_bodies(
    agg: &StructDecl,
    span: Span,
    key: &NominalKey,
    schema: &AggregateSchema,
    tc: &mut TypeChecker,
) {
    if !schema.generics.is_empty() {
        return;
    }

    let self_ty = nominal_type(key);
    for method in &agg.methods {
        if method_sig_is_generic(&method.sig) {
            continue;
        }
        let mode = MethodMode::from_receiver(method.sig.receiver);
        let Some(method_schema) = schema
            .methods
            .get(&MethodKey::new(method.sig.name, mode.surface()))
        else {
            continue;
        };
        check_func_body(
            method_schema
                .mode
                .receiver()
                .map(|receiver| (receiver, self_ty.clone())),
            &method.sig.params,
            &method_schema.params,
            method_schema.ret.clone(),
            &method.body,
            span,
            &[],
            tc,
        );
    }
}

fn check_method_generic_shadows(agg: &StructDecl, span: Span, tc: &mut TypeChecker) {
    let mut owner_params = HashMap::new();
    owner_params.extend(
        agg.type_params
            .iter()
            .map(|param| (param.name, GenericParamKind::Type)),
    );
    owner_params.extend(
        agg.const_params
            .iter()
            .map(|param| (param.name, GenericParamKind::Const)),
    );

    for method in &agg.methods {
        for param in &method.sig.type_params {
            check_method_generic_shadow(
                agg.kind,
                &owner_params,
                GenericParamKind::Type,
                param.name,
                span,
                tc,
            );
        }
        for param in &method.sig.const_params {
            check_method_generic_shadow(
                agg.kind,
                &owner_params,
                GenericParamKind::Const,
                param.name,
                span,
                tc,
            );
        }
    }
}

fn check_method_generic_shadow(
    owner_kind: AggregateKind,
    owner_params: &HashMap<Ident, GenericParamKind>,
    method_param: GenericParamKind,
    name: Ident,
    span: Span,
    tc: &mut TypeChecker,
) {
    let Some(owner_param) = owner_params.get(&name).copied() else {
        return;
    };
    tc.push_error(TypeError::MethodGenericShadow {
        owner_kind,
        method_param,
        owner_param,
        name,
        span: tc.error_span(span),
    });
}

enum CallableBody<'a> {
    Block(&'a BlockNode),
    Expr(&'a ExprNode),
}

impl CallableBody<'_> {
    fn span(&self) -> Span {
        match self {
            Self::Block(block) => block.span,
            Self::Expr(expr) => expr.span,
        }
    }

    fn diverges(&self) -> bool {
        match self {
            Self::Block(block) => control_flow::block_diverges(block),
            Self::Expr(expr) => control_flow::expr_diverges(expr),
        }
    }

    fn value_expr_id(&self) -> Option<ExprId> {
        self.value_expr().map(|expr| expr.node.id)
    }

    fn value_expr(&self) -> Option<&ExprNode> {
        match self {
            Self::Block(block) => block.node.tail.as_deref(),
            Self::Expr(expr) => Some(expr),
        }
    }

    fn check_with_hint(&self, expected: Option<TypeHandle>, tc: &mut TypeChecker) -> CheckedType {
        match self {
            Self::Block(block) => check_block_checked_with_hint(block, expected, tc),
            Self::Expr(expr) => check_expr_checked_with_hint(expr, expected, tc),
        }
    }
}

fn check_callable_body_with_return(
    body: CallableBody<'_>,
    expected_ret: Option<&ReturnSpec>,
    source: Option<&PlaceIdentity>,
    callable_span: Span,
    tc: &mut TypeChecker,
) -> CheckedType {
    if expected_ret.is_some_and(ReturnSpec::is_place) {
        return check_callable_body_place_return(body, expected_ret, source, callable_span, tc);
    }

    let expected = expected_ret.map(|ret| tc.type_handle(&ret.ty));
    let checked = body.check_with_hint(expected, tc);
    finish_callable_body_value_return(body, &checked, expected_ret, callable_span, tc);
    checked
}

fn check_callable_body_place_return(
    body: CallableBody<'_>,
    expected_ret: Option<&ReturnSpec>,
    source: Option<&PlaceIdentity>,
    callable_span: Span,
    tc: &mut TypeChecker,
) -> CheckedType {
    match body {
        CallableBody::Block(block) => {
            tc.push_scope();
            let declarations = register_block_declarations(&block.node.stmts, tc);
            for (stmt, local_const) in block.node.stmts.iter().zip(declarations) {
                check_stmt(stmt, local_const, tc);
            }
            let checked = match &block.node.tail {
                Some(expr) => check_tail_place_return(expr, expected_ret, source, tc),
                None => checked_void(tc),
            };
            finish_missing_place_return(
                &checked,
                control_flow::block_diverges(block),
                expected_ret,
                callable_span,
                tc,
            );
            tc.pop_scope();
            checked
        }
        CallableBody::Expr(expr) => check_tail_place_return(expr, expected_ret, source, tc),
    }
}

fn check_tail_place_return(
    expr: &ExprNode,
    expected_ret: Option<&ReturnSpec>,
    source: Option<&PlaceIdentity>,
    tc: &mut TypeChecker,
) -> CheckedType {
    if let ExprKind::Block(block) = &expr.node.kind {
        return check_callable_body_place_return(
            CallableBody::Block(block),
            expected_ret,
            source,
            expr.span,
            tc,
        );
    }

    let ret = expected_ret
        .cloned()
        .unwrap_or_else(|| ReturnSpec::place(Type::InferReturn));
    let checked = check_return_expr(expr, ret, source.cloned(), tc);
    if expected_ret.is_none() {
        tc.push_inferred_return(expr.span, checked.handle.clone());
    }
    checked
}

fn finish_missing_place_return(
    checked: &CheckedType,
    diverges: bool,
    expected_ret: Option<&ReturnSpec>,
    callable_span: Span,
    tc: &mut TypeChecker,
) {
    if checked.ty.is_void() && !diverges {
        match expected_ret {
            Some(ret) if !ret.ty.is_void() => tc.push_error(TypeError::MissingReturn {
                expected: ret.ty.clone(),
                span: tc.error_span(callable_span),
            }),
            None => tc.push_inferred_return(callable_span, tc.type_handle(&Type::Void)),
            _ => {}
        }
    }
}

fn finish_callable_body_value_return(
    body: CallableBody<'_>,
    checked: &CheckedType,
    expected_ret: Option<&ReturnSpec>,
    callable_span: Span,
    tc: &mut TypeChecker,
) {
    match expected_ret {
        Some(ret) => {
            if !checked.ty.is_void() {
                if let Some(expr) = body.value_expr() {
                    tc.record_escaping_use(expr);
                }
                tc.reject_extern_any_escape(checked, body.span());
                let ret_handle = tc.type_handle(&ret.ty);
                match body.value_expr_id() {
                    Some(expr_id) => tc.expect_assignable_expr(
                        body.span(),
                        expr_id,
                        checked.handle.clone(),
                        ret_handle,
                    ),
                    None => tc.expect_assignable(body.span(), checked.handle.clone(), ret_handle),
                }
            } else if !ret.ty.is_void() && !body.diverges() {
                tc.push_error(TypeError::MissingReturn {
                    expected: ret.ty.clone(),
                    span: tc.error_span(callable_span),
                });
            }
        }
        None => {
            if !checked.ty.is_void() {
                if let Some(expr) = body.value_expr() {
                    tc.record_escaping_use(expr);
                }
                tc.reject_extern_any_escape(checked, body.span());
                tc.push_inferred_return(body.span(), checked.handle.clone());
            } else if !body.diverges() {
                tc.push_inferred_return(callable_span, tc.type_handle(&Type::Void));
            }
        }
    }
}

fn check_func_body(
    self_binding: Option<(MethodReceiver, Type)>,
    params: &[Param],
    param_types: &[FuncParam],
    ret: ReturnSpec,
    body: &BlockNode,
    span: Span,
    const_bindings: &[(Ident, ConstValue)],
    tc: &mut TypeChecker,
) -> Option<Type> {
    check_param_default_values(params, param_types, tc);
    let flow = tc.enter_function_control_flow();
    tc.enter_named_function();
    tc.push_scope();
    for (name, value) in const_bindings {
        tc.define_const(*name, const_eval::const_type(value), value.clone());
    }
    let infer_return = ret.is_infer();
    let mut source = None;
    if let Some((receiver, self_ty)) = self_binding {
        let kind = match receiver {
            MethodReceiver::Var => LocalBindingKind::borrowed_self(),
            MethodReceiver::Value => LocalBindingKind::readonly_self(),
        };
        let type_id = tc.define_value(Ident::new("self"), self_ty, kind, None);
        if matches!(receiver, MethodReceiver::Var) {
            source = Some(PlaceIdentity::root(PlaceRoot::Local(type_id)));
        }
    }
    for (param, param_ty) in params.iter().zip(param_types.iter()) {
        let mutable = matches!(param.mutability, Mutability::Mutable);
        let kind = LocalBindingKind::from_param(mutable, &param_ty.ty);
        let type_id = tc.define_value(param.name, param_ty.ty.clone(), kind, None);
        tc.mark_non_escaping_callback_param(param.name, type_id, param_ty, Some(&param.ty));
        if source.is_none() && mutable {
            source = Some(PlaceIdentity::root(PlaceRoot::Local(type_id)));
        }
    }
    let return_mode = if infer_return {
        ReturnMode::Infer {
            access: ret.access,
            source: source.clone(),
            candidates: vec![],
        }
    } else {
        ReturnMode::Explicit {
            ret: ret.clone(),
            source: source.clone(),
        }
    };
    tc.push_return_frame(return_mode);
    let expected_ret = (!infer_return).then_some(&ret);
    check_callable_body_with_return(
        CallableBody::Block(body),
        expected_ret,
        source.as_ref(),
        span,
        tc,
    );
    let frame = tc.pop_return_frame();
    let inferred_ret = frame.and_then(|frame| infer_return_type(frame, tc));
    tc.pop_scope();
    tc.exit_named_function();
    tc.exit_function_control_flow(flow);
    inferred_ret
}

fn infer_return_type(frame: ReturnFrame, tc: &mut TypeChecker) -> Option<Type> {
    let ReturnMode::Infer { candidates, .. } = frame.mode else {
        return None;
    };
    let mut candidates = candidates.into_iter();
    let (_, first) = candidates.next()?;
    tc.solve_constraints();
    let inferred = tc.handle_type(&first);
    for (span, candidate) in candidates {
        let found = tc.handle_type(&candidate);
        if inferred != found && !matches!(inferred, Type::Infer) && !matches!(found, Type::Infer) {
            tc.push_error(TypeError::InferReturnMismatch {
                expected: inferred.clone(),
                found,
                span: tc.error_span(span),
            });
        }
    }
    Some(inferred)
}

fn check_param_default_values(params: &[Param], param_types: &[FuncParam], tc: &mut TypeChecker) {
    for (param, param_ty) in params.iter().zip(param_types) {
        let Some(default) = &param.default else {
            continue;
        };
        let expected = tc.type_handle(&param_ty.ty);
        match validate_const_expr_type(default, Some(expected), tc) {
            Ok(_) => {
                if let Err(error) = tc.eval_const_expr(default, false) {
                    tc.push_error(error);
                }
            }
            Err(error) => tc.push_error(error),
        }
    }
}

fn with_callable_body_env<R>(
    module: &ModuleScope,
    env: &CallableTemplateEnv,
    tc: &mut TypeChecker,
    f: impl FnOnce(&mut TypeChecker) -> R,
) -> R {
    match env {
        CallableTemplateEnv::SourceModule => with_source_module_scope(module, tc, f),
        CallableTemplateEnv::Local(state) => {
            let previous_module = std::mem::replace(&mut tc.current_module, module.clone());
            let previous_state = tc.replace_scope_state(state.clone());
            let ret = f(tc);
            tc.restore_scope_state(previous_state);
            tc.current_module = previous_module;
            ret
        }
    }
}

fn check_specialized_callable_body(
    callee: &CallableRef,
    param_types: &[FuncParam],
    ret: ReturnSpec,
    args: &GenericArgs,
    type_subst: TypeSubst,
    const_subst: ConstSubst,
    const_bindings: Vec<(Ident, ConstValue)>,
    tc: &mut TypeChecker,
) -> Option<Type> {
    if args.is_empty()
        || matches!(
            callee.def.id.kind,
            CallableKind::ExternFunction | CallableKind::EnumVariant
        )
    {
        return None;
    }

    let template = tc.callable_template(&callee.def.id).cloned()?;
    let inferred = template.ret.is_infer();
    let key = specialization_key(callee.def.id.clone(), args);
    match tc.specialization(&key).cloned() {
        Some(SpecializationState::InProgress) if inferred => {
            tc.push_error(TypeError::InferReturnRecursive {
                span: tc.error_span(template.ret_span),
            });
            return Some(Type::Infer);
        }
        Some(SpecializationState::InProgress) => return None,
        Some(SpecializationState::Done(body)) => {
            tc.restore_specialization(body.facts);
            return body.inferred_ret;
        }
        None => {}
    }

    let receiver = template.mode.receiver().zip(callee.receiver_ty.clone());

    let owner_frame = GenericOwnerFrame {
        params: combined_callable_params(callee),
        args: args.clone(),
        generics: template.generics.clone(),
    };
    check_with_specialization(key, type_subst, const_subst, owner_frame, tc, |tc| {
        with_callable_body_env(&callee.def.id.module, &template.env, tc, |tc| {
            check_func_body(
                receiver,
                &template.params,
                param_types,
                ret.clone(),
                &template.body,
                template.span,
                &const_bindings,
                tc,
            )
        })
    })
}

fn combined_callable_params(callee: &CallableRef) -> GenericParams {
    let mut params = callee.def.sig.owner_generics.clone();
    params
        .type_params
        .extend(callee.def.sig.generics.type_params.clone());
    params
        .const_params
        .extend(callee.def.sig.generics.const_params.clone());
    params
}

fn check_with_specialization(
    key: SpecializationKey,
    type_subst: TypeSubst,
    const_subst: ConstSubst,
    owner_frame: GenericOwnerFrame,
    tc: &mut TypeChecker,
    check_body: impl FnOnce(&mut TypeChecker) -> Option<Type>,
) -> Option<Type> {
    tc.solve_constraints();
    let old_facts = tc.specialization_facts();
    tc.store_specialization(key.clone(), SpecializationState::InProgress);
    tc.push_type_subst(type_subst);
    tc.push_const_subst(const_subst);
    tc.push_generic_context(owner_frame.generics.clone());
    tc.push_generic_owner_frame(owner_frame);
    let inferred_ret = check_body(tc);
    tc.solve_constraints();
    tc.pop_generic_owner_frame();
    tc.pop_generic_context();
    tc.pop_const_subst();
    tc.pop_type_subst();
    tc.store_specialization(
        key,
        SpecializationState::Done(Box::new(SpecializedBody {
            facts: specialized_body_facts(&old_facts, &tc.specialization_facts()),
            inferred_ret: inferred_ret.clone(),
        })),
    );
    inferred_ret
}

fn const_param_bindings(params: &GenericParams, args: &GenericArgs) -> Vec<(Ident, ConstValue)> {
    params
        .const_params
        .iter()
        .zip(&args.const_args)
        .filter_map(|(param, term)| match term {
            ConstTerm::Value(value) => Some((param.name, value.clone())),
            ConstTerm::Name(_)
            | ConstTerm::Param(_)
            | ConstTerm::ArrayInfer
            | ConstTerm::Infer(_) => None,
        })
        .collect()
}

fn callable_const_bindings(
    owner_params: &GenericParams,
    owner_args: &GenericArgs,
    callable_params: &GenericParams,
    callable_args: &GenericArgs,
) -> Vec<(Ident, ConstValue)> {
    let mut bindings = const_param_bindings(owner_params, owner_args);
    bindings.extend(const_param_bindings(callable_params, callable_args));
    bindings
}

fn specialized_body_facts(
    old: &SpecializedBodyFacts,
    current: &SpecializedBodyFacts,
) -> SpecializedBodyFacts {
    SpecializedBodyFacts {
        types: map_delta(&old.types, &current.types),
        calls: map_delta(&old.calls, &current.calls),
        extern_uses: map_delta(&old.extern_uses, &current.extern_uses),
        member_paths: map_delta(&old.member_paths, &current.member_paths),
        argument_projections: map_delta(&old.argument_projections, &current.argument_projections),
        contract_witnesses: map_delta(&old.contract_witnesses, &current.contract_witnesses),
        dyn_conversions: map_delta(&old.dyn_conversions, &current.dyn_conversions),
        dyn_weakenings: map_delta(&old.dyn_weakenings, &current.dyn_weakenings),
        dyn_calls: map_delta(&old.dyn_calls, &current.dyn_calls),
        dyn_downcasts: map_delta(&old.dyn_downcasts, &current.dyn_downcasts),
        global_accesses: map_delta(&old.global_accesses, &current.global_accesses),
        closure: current.closure.delta_since(&old.closure),
    }
}

fn map_delta<K, V>(old: &HashMap<K, V>, current: &HashMap<K, V>) -> HashMap<K, V>
where
    K: Copy + Eq + Hash,
    V: Clone + PartialEq,
{
    current
        .iter()
        .filter_map(|(id, item)| match old.get(id) {
            Some(old_item) if old_item == item => None,
            _ => Some((*id, item.clone())),
        })
        .collect()
}

fn specialization_key(id: CallableId, args: &GenericArgs) -> SpecializationKey {
    SpecializationKey {
        target: id,
        args: args.clone(),
    }
}

fn with_global_scope<R>(tc: &mut TypeChecker, f: impl FnOnce(&mut TypeChecker) -> R) -> R {
    let state = tc.take_scope_state();
    tc.replace_scopes(state.scopes.first().cloned().into_iter().collect());
    let ret = f(tc);
    tc.restore_scope_state(state);
    ret
}

fn with_source_module_scope<R>(
    module: &ModuleScope,
    tc: &mut TypeChecker,
    f: impl FnOnce(&mut TypeChecker) -> R,
) -> R {
    let previous_module = std::mem::replace(&mut tc.current_module, module.clone());
    let ret = match module {
        ModuleScope::Root => with_global_scope(tc, f),
        ModuleScope::Named(_) | ModuleScope::Package(_) => {
            let state = tc.take_scope_state();
            tc.replace_scopes(vec![]);
            push_source_scope(tc);
            if let Some(program) = tc.module_programs.get(module).map(Rc::clone) {
                register_declarations(program.as_ref(), tc);
                tc.eval_module_consts(module);
            }
            let ret = f(tc);
            tc.restore_scope_state(state);
            ret
        }
    };
    tc.current_module = previous_module;
    ret
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

fn check_block_checked(block: &BlockNode, tc: &mut TypeChecker) -> CheckedType {
    check_block_checked_with_hint(block, None, tc)
}

fn check_block_checked_with_hint(
    block: &BlockNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    tc.push_scope();
    let declarations = register_block_declarations(&block.node.stmts, tc);
    for (stmt, local_const) in block.node.stmts.iter().zip(declarations) {
        check_stmt(stmt, local_const, tc);
    }
    let checked = match &block.node.tail {
        Some(expr) => check_expr_checked_with_hint(expr, expected, tc),
        None => checked_void(tc),
    };
    tc.pop_scope();
    checked
}

fn mode_for_head(head: PatternHead) -> PatternBindMode {
    match head {
        PatternHead::Let => PatternBindMode::Owned { mutable: false },
        PatternHead::Var => PatternBindMode::Alias,
    }
}

fn mode_for_binding(binding: &Binding) -> PatternBindMode {
    match binding.mutability {
        Mutability::Immutable => PatternBindMode::Owned { mutable: false },
        Mutability::Mutable if matches!(binding.pattern.node, Pattern::Ident(_)) => {
            PatternBindMode::Owned { mutable: true }
        }
        Mutability::Mutable => PatternBindMode::Alias,
    }
}

struct PatternScrutinee {
    checked: CheckedType,
    access: PlaceAccess,
    facts: PlaceUseFacts,
    identity: PlaceIdentity,
    accepts_extern_any: bool,
}

impl PatternScrutinee {
    fn owned(checked: CheckedType) -> Self {
        Self {
            checked,
            access: PlaceAccess::Mutable,
            facts: PlaceUseFacts::default(),
            identity: PlaceIdentity::unknown(),
            accepts_extern_any: false,
        }
    }

    fn alias(place: place::CheckedPlace) -> Self {
        let accepts_extern_any = place.accepts_extern_any();
        let place::PlaceValue {
            checked,
            access,
            facts,
            identity,
            ..
        } = place.value;
        Self {
            checked,
            access,
            facts,
            identity,
            accepts_extern_any,
        }
    }

    fn pattern_place(
        &self,
        expected_handle: TypeHandle,
        expected_ty: Type,
    ) -> pattern::PatternPlace {
        pattern::PatternPlace {
            expected_handle,
            expected_ty,
            access: self.access,
            facts: self.facts.clone(),
            identity: self.identity.clone(),
            accepts_extern_any: self.accepts_extern_any,
        }
    }
}

fn check_pattern_scrutinee(
    expr: &ExprNode,
    mode: PatternBindMode,
    tc: &mut TypeChecker,
) -> PatternScrutinee {
    match mode {
        PatternBindMode::Owned { .. } => {
            PatternScrutinee::owned(check_value_expr_checked_with_hint(expr, None, tc))
        }
        PatternBindMode::Alias => PatternScrutinee::alias(check_alias_scrutinee(expr, tc)),
    }
}

fn refined_binding_type(annot: &Type, value: &Type, tc: &TypeChecker) -> Type {
    if let Some(annot_inner) = tc.decls.core_option_inner(annot) {
        let value_inner = tc.decls.core_option_inner(value).unwrap_or(value);
        let inner = refined_binding_type(annot_inner, value_inner, tc);
        return tc
            .decls
            .core_option_of(inner)
            .unwrap_or_else(|| annot.clone());
    }
    match (annot, value) {
        (
            Type::Array { elem, len },
            Type::Array {
                elem: value_elem,
                len: value_len,
            },
        ) => Type::Array {
            elem: Box::new(refined_binding_type(elem, value_elem, tc)),
            len: if matches!(len, ArrayLen::Infer) {
                *value_len
            } else {
                *len
            },
        },
        (Type::List { elem }, Type::List { elem: value_elem }) => Type::List {
            elem: Box::new(refined_binding_type(elem, value_elem, tc)),
        },
        (Type::Slice { elem }, Type::Slice { elem: value_elem }) => Type::Slice {
            elem: Box::new(refined_binding_type(elem, value_elem, tc)),
        },
        (
            Type::Map { key, value },
            Type::Map {
                key: value_key,
                value: value_value,
            },
        ) => Type::Map {
            key: Box::new(refined_binding_type(key, value_key, tc)),
            value: Box::new(refined_binding_type(value, value_value, tc)),
        },
        (Type::Tuple(types), Type::Tuple(value_types)) if types.len() == value_types.len() => {
            Type::Tuple(
                types
                    .iter()
                    .zip(value_types)
                    .map(|(ty, value_ty)| refined_binding_type(ty, value_ty, tc))
                    .collect(),
            )
        }
        _ => annot.clone(),
    }
}

fn check_binding(binding_node: &BindingNode, tc: &mut TypeChecker) {
    let binding = &binding_node.node;
    let mode = mode_for_binding(binding);
    let value_ty = match &binding.ty {
        Some(annot) => {
            let annot_ty = tc.resolve_type_for_tc_at(annot, binding_node.span);
            let annot_handle = tc.type_handle(&annot_ty);
            let value = match mode {
                PatternBindMode::Owned { .. } => {
                    PatternScrutinee::owned(check_value_expr_checked_with_hint(
                        &binding.value,
                        Some(annot_handle.clone()),
                        tc,
                    ))
                }
                PatternBindMode::Alias => {
                    PatternScrutinee::alias(check_alias_scrutinee(&binding.value, tc))
                }
            };
            tc.reject_extern_any_escape(&value.checked, binding.value.span);
            tc.expect_assignable_expr(
                binding.value.span,
                binding.value.node.id,
                value.checked.handle.clone(),
                annot_handle,
            );
            tc.solve_constraints();
            let value_ty = value.checked.ty.clone();
            let binding_ty = refined_binding_type(&annot_ty, &value.checked.ty, tc);
            let binding_handle = tc.type_handle(&binding_ty);
            pattern::check_place_at(
                &binding.pattern,
                value.pattern_place(binding_handle, binding_ty),
                mode,
                binding.value.node.id,
                PatternContext::Binding,
                tc,
            );
            value_ty
        }
        None => {
            let value = check_pattern_scrutinee(&binding.value, mode, tc);
            tc.reject_extern_any_escape(&value.checked, binding.value.span);
            tc.reject_user_any_type(&value.checked.ty, binding_node.span);
            let value_ty = value.checked.ty.clone();
            pattern::check_place_at(
                &binding.pattern,
                value.pattern_place(value.checked.handle.clone(), value.checked.ty.clone()),
                mode,
                binding.value.node.id,
                PatternContext::Binding,
                tc,
            );
            value_ty
        }
    };

    let function_value = matches!(value_ty, Type::Func { .. });
    let binding_id = simple_owned_binding_name(binding).and_then(|name| tc.local_binding_id(name));
    tc.closure.bind_local(
        binding_id,
        binding.value.node.id,
        function_value,
        binding.value.span,
    );
}

fn simple_owned_binding_name(binding: &Binding) -> Option<Ident> {
    if !matches!(mode_for_binding(binding), PatternBindMode::Owned { .. }) {
        return None;
    }
    let Pattern::Ident(name) = &binding.pattern.node else {
        return None;
    };
    Some(*name)
}

fn check_type_alias(alias_node: &TypeAliasDeclNode, tc: &mut TypeChecker) {
    let Some(local) = tc.local_type_scopes.by_key(alias_node.span).cloned() else {
        return;
    };
    tc.push_generic_context(local.def.generic_context.clone());
    let aliased = tc.resolve_type_for_tc_at(&local.def.aliased, alias_node.span);
    tc.pop_generic_context();
    validate_type_alias_def(&tc.decls, &local.def, &aliased, false, &mut tc.errors);
}

fn check_const(const_node: &ConstDeclNode, tc: &mut TypeChecker) {
    let c = &const_node.node;
    let value = match tc.eval_const_expr(&c.value, true) {
        Ok(value) => value,
        Err(err) => {
            tc.push_error(err);
            return;
        }
    };
    let value_ty = const_eval::const_type(&value);
    let ty = match &c.ty {
        Some(annot) => {
            let annot_ty = tc.resolve_type_for_tc_at(annot, const_node.span);
            tc.reject_user_any_type(&annot_ty, const_node.span);
            if annot_ty != value_ty {
                tc.push_error(TypeError::ConstTypeMismatch {
                    expected: annot_ty.clone(),
                    found: value_ty,
                    span: tc.error_span(const_node.span),
                });
            }
            annot_ty
        }
        None => value_ty,
    };
    tc.define_const(c.name, ty, value);
}

fn check_return(ret_node: &ReturnNode, tc: &mut TypeChecker) {
    let ret = &ret_node.node;
    if tc.in_global_initializer() {
        tc.push_error(TypeError::CompileError {
            message: "return is not allowed in runtime global initializers".to_string(),
            span: tc.error_span(ret_node.span),
        });
        check_discarded_return_value(ret, tc);
        return;
    }

    if tc.in_defer() {
        tc.push_error(TypeError::ReturnInsideDefer {
            span: tc.error_span(ret_node.span),
        });
        check_discarded_return_value(ret, tc);
        return;
    }

    match (&ret.value, tc.return_mode()) {
        (Some(expr), Some(ReturnMode::Explicit { ret, source })) => {
            check_return_expr(expr, ret.clone(), source.clone(), tc);
        }
        (Some(expr), Some(ReturnMode::Infer { access, source, .. })) => {
            let ret = ReturnSpec {
                access: *access,
                ty: Type::InferReturn,
            };
            let actual = check_return_expr(expr, ret, source.clone(), tc);
            tc.push_inferred_return(expr.span, actual.handle);
        }
        (Some(expr), None) => {
            let actual = check_value_expr_checked_with_hint(expr, None, tc);
            tc.record_escaping_use(expr);
            tc.reject_extern_any_escape(&actual, expr.span);
        }
        (None, Some(ReturnMode::Explicit { ret, .. })) if !ret.ty.is_void() => {
            tc.push_error(TypeError::MissingReturn {
                expected: ret.ty.clone(),
                span: tc.error_span(ret_node.span),
            });
        }
        (None, Some(ReturnMode::Infer { .. })) => {
            tc.push_inferred_return(ret_node.span, tc.type_handle(&Type::Void));
        }
        (None, _) => {}
    }
}

fn check_discarded_return_value(ret: &Return, tc: &mut TypeChecker) {
    if let Some(expr) = &ret.value {
        let actual = check_value_expr_checked_with_hint(expr, None, tc);
        tc.record_escaping_use(expr);
        tc.reject_extern_any_escape(&actual, expr.span);
    }
}

fn check_return_expr(
    expr: &ExprNode,
    ret: ReturnSpec,
    source: Option<PlaceIdentity>,
    tc: &mut TypeChecker,
) -> CheckedType {
    if ret.is_place() {
        if let Some(checked) = check_branch_place_return_expr(expr, &ret, source.as_ref(), tc) {
            return checked_from_checked(expr, checked, tc);
        }
        let place = check_place(expr, tc);
        validate_place_return_expr(&place.value, source.as_ref(), expr.span, tc);
        let checked = place.into_checked();
        tc.reject_extern_any_escape(&checked, expr.span);
        if !matches!(ret.ty, Type::InferReturn) {
            let expected = tc.type_handle(&ret.ty);
            tc.expect_assignable_expr(expr.span, expr.node.id, checked.handle.clone(), expected);
        }
        return checked;
    }

    let expected = (!matches!(ret.ty, Type::InferReturn)).then(|| tc.type_handle(&ret.ty));
    let actual = check_value_expr_checked_with_hint(expr, expected.clone(), tc);
    tc.record_escaping_use(expr);
    tc.reject_extern_any_escape(&actual, expr.span);
    if let Some(expected) = expected {
        tc.expect_assignable_expr(expr.span, expr.node.id, actual.handle.clone(), expected);
    }
    actual
}

fn check_branch_place_return_expr(
    expr: &ExprNode,
    ret: &ReturnSpec,
    source: Option<&PlaceIdentity>,
    tc: &mut TypeChecker,
) -> Option<CheckedType> {
    let expected = place_return_expected_handle(ret, tc);
    match &expr.node.kind {
        ExprKind::Block(block) => Some(check_callable_body_place_return(
            CallableBody::Block(block),
            Some(ret),
            source,
            block.span,
            tc,
        )),
        ExprKind::If(if_node) => {
            let cond = check_expr_checked(&if_node.node.cond, tc);
            check_bool_condition(ConditionKind::If, cond, if_node.node.cond.span, tc);
            let then_checked = check_callable_body_place_return(
                CallableBody::Block(&if_node.node.then_block),
                Some(ret),
                source,
                if_node.node.then_block.span,
                tc,
            );
            let Some(else_block) = &if_node.node.else_block else {
                tc.push_error(TypeError::MissingReturn {
                    expected: ret.ty.clone(),
                    span: tc.error_span(expr.span),
                });
                return Some(if then_checked.ty.is_void() {
                    diverged_place_return(ret, tc)
                } else {
                    then_checked
                });
            };
            let else_checked = check_callable_body_place_return(
                CallableBody::Block(else_block),
                Some(ret),
                source,
                else_block.span,
                tc,
            );
            Some(join_place_return_branches(
                ret,
                expected,
                place_return_branch(
                    then_checked,
                    if_node.node.then_block.span,
                    control_flow::block_diverges(&if_node.node.then_block),
                ),
                place_return_branch(
                    else_checked,
                    else_block.span,
                    control_flow::block_diverges(else_block),
                ),
                tc,
            ))
        }
        ExprKind::Ternary(ternary) => {
            let cond = check_expr_checked(&ternary.node.cond, tc);
            check_bool_condition(ConditionKind::Ternary, cond, ternary.node.cond.span, tc);
            let then_checked =
                check_return_expr(&ternary.node.then_expr, ret.clone(), source.cloned(), tc);
            let else_checked =
                check_return_expr(&ternary.node.else_expr, ret.clone(), source.cloned(), tc);
            Some(join_place_return_branches(
                ret,
                expected,
                place_return_branch(
                    then_checked,
                    ternary.node.then_expr.span,
                    control_flow::expr_diverges(&ternary.node.then_expr),
                ),
                place_return_branch(
                    else_checked,
                    ternary.node.else_expr.span,
                    control_flow::expr_diverges(&ternary.node.else_expr),
                ),
                tc,
            ))
        }
        ExprKind::Match(match_node) => {
            let node = &match_node.node;
            let mode = mode_for_head(node.head);
            let scrutinee = check_pattern_scrutinee(&node.scrutinee, mode, tc);
            if node.arms.is_empty() {
                tc.push_error(TypeError::EmptyMatch {
                    span: tc.error_span(match_node.span),
                });
                return Some(checked_void(tc));
            }

            let mut joined = None;
            let mut outcomes = Vec::with_capacity(node.arms.len());
            for arm in &node.arms {
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
                let checked = check_return_expr(&arm.node.body, ret.clone(), source.cloned(), tc);
                tc.pop_scope();
                outcomes.push(outcome);
                let branch = place_return_branch(
                    checked,
                    arm.node.body.span,
                    control_flow::expr_diverges(&arm.node.body),
                );
                joined = Some(match joined {
                    Some(previous) => {
                        join_place_return_branch(ret, expected.clone(), previous, branch, tc)
                    }
                    None => branch,
                });
            }
            match_coverage::check(&scrutinee.checked.ty, &outcomes, match_node.span, tc);
            Some(match joined {
                Some(branch) => finish_place_return_branch(ret, expected, branch, tc),
                None => checked_void(tc),
            })
        }
        _ => None,
    }
}

fn place_return_expected_handle(ret: &ReturnSpec, tc: &TypeChecker) -> Option<TypeHandle> {
    (!matches!(ret.ty, Type::InferReturn)).then(|| tc.type_handle(&ret.ty))
}

fn place_return_branch(checked: CheckedType, span: Span, diverges: bool) -> CheckedBranch {
    CheckedBranch {
        diverges: diverges && checked.ty.is_void(),
        checked,
        span,
    }
}

fn join_place_return_branch(
    ret: &ReturnSpec,
    expected: Option<TypeHandle>,
    left: CheckedBranch,
    right: CheckedBranch,
    tc: &mut TypeChecker,
) -> CheckedBranch {
    let diverges = left.diverges && right.diverges;
    let span = right.span;
    let checked = join_branches_with_hint(expected, left, right, tc);
    let checked = if diverges {
        diverged_place_return(ret, tc)
    } else {
        checked
    };
    CheckedBranch {
        checked,
        span,
        diverges,
    }
}

fn join_place_return_branches(
    ret: &ReturnSpec,
    expected: Option<TypeHandle>,
    left: CheckedBranch,
    right: CheckedBranch,
    tc: &mut TypeChecker,
) -> CheckedType {
    let branch = join_place_return_branch(ret, expected, left, right, tc);
    if branch.diverges {
        diverged_place_return(ret, tc)
    } else {
        branch.checked
    }
}

fn finish_place_return_branch(
    ret: &ReturnSpec,
    expected: Option<TypeHandle>,
    branch: CheckedBranch,
    tc: &mut TypeChecker,
) -> CheckedType {
    if branch.diverges {
        diverged_place_return(ret, tc)
    } else {
        checked_branch_against_expected(branch, expected, tc)
    }
}

fn diverged_place_return(ret: &ReturnSpec, tc: &TypeChecker) -> CheckedType {
    let ty = if matches!(ret.ty, Type::InferReturn) {
        Type::Infer
    } else {
        ret.ty.clone()
    };
    checked_type(ty, tc)
}

fn validate_place_return_expr(
    value: &place::PlaceValue,
    source: Option<&PlaceIdentity>,
    span: Span,
    tc: &mut TypeChecker,
) {
    if !value.access.can_mut_borrow() {
        tc.push_error(TypeError::UnsupportedPlaceReturn {
            message: "mutable place return requires a mutable place",
            span: tc.error_span(span),
        });
    }
    if value.identity.is_indexed_derived() {
        tc.push_error(TypeError::UnsupportedPlaceReturn {
            message: "mutable place return cannot return an indexed place",
            span: tc.error_span(span),
        });
    }
    let Some(source) = source else {
        tc.push_error(TypeError::UnsupportedPlaceReturn {
            message: "mutable place return must derive from the first mutable input",
            span: tc.error_span(span),
        });
        return;
    };
    if !value.identity.derives_from(source) {
        tc.push_error(TypeError::UnsupportedPlaceReturn {
            message: "mutable place return must derive from the first mutable input",
            span: tc.error_span(span),
        });
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
    let handle = tc.set_type(expr.node.id, ty.clone(), expr.span);
    CheckedType {
        ty,
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
                let value = tc.local_value_from_info(*name, info.clone(), depth);
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
            let checked = check_if_let_checked_with_hint(if_let_node, expected, tc);
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
        ExprKind::Cast(cast) => check_cast_expr(expr, cast, tc),
        ExprKind::ExactDowncast(downcast) => check_exact_downcast_expr(expr, downcast, tc),
        ExprKind::Lambda(lambda) => check_lambda_expr(expr, lambda, expected, tc),
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
                tc.push_warning(TypeWarning::CompileMessage {
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

fn type_from_lit(lit: &Lit) -> Type {
    match lit {
        Lit::Int(_) => Type::Int,
        Lit::Float(_) => Type::Float,
        Lit::Bool(_) => Type::Bool,
        Lit::String(_) => Type::String,
        Lit::Nil => Type::Infer,
    }
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
            .is_some_and(|ty| tc.decls.core_option_inner(ty).is_some());
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
        .and_then(|ty| tc.decls.core_option_of(ty.clone()))
        .map(|ty| tc.type_handle(&ty));
    let left = check_value_expr_checked_with_hint(left_expr, left_expected, tc);
    let Some(inner) = tc.decls.core_option_inner(&left.ty).cloned() else {
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
    if tc.decls.core_option_inner(&right.ty) == Some(&inner) {
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

fn type_contains_dyn_value(
    ty: &Type,
    decls: &DeclarationIndex,
    seen: &mut HashSet<NominalKey>,
) -> bool {
    match ty {
        Type::Dyn(_) => true,
        Type::Tuple(elems) => elems
            .iter()
            .any(|elem| type_contains_dyn_value(elem, decls, seen)),
        Type::List { elem } | Type::Array { elem, .. } | Type::Slice { elem } => {
            type_contains_dyn_value(elem, decls, seen)
        }
        Type::Map { key, value } => {
            type_contains_dyn_value(key, decls, seen) || type_contains_dyn_value(value, decls, seen)
        }
        Type::Nominal(_) => nominal_contains_dyn_value(ty, decls, seen),
        Type::Func { .. }
        | Type::Infer
        | Type::InferReturn
        | Type::Any
        | Type::Int
        | Type::Float
        | Type::Bool
        | Type::String
        | Type::Void
        | Type::Var(_)
        | Type::UnresolvedName(_)
        | Type::UnresolvedNominal { .. } => false,
    }
}

fn nominal_contains_dyn_value(
    ty: &Type,
    decls: &DeclarationIndex,
    seen: &mut HashSet<NominalKey>,
) -> bool {
    let Some(key) = decls.key_for_type(ty) else {
        return false;
    };
    if !seen.insert(key.clone()) {
        return false;
    }
    let contains = match key.kind {
        NominalKind::Struct | NominalKind::DataRef => decls.aggregate(&key).is_some_and(|agg| {
            agg.fields.values().any(|field| {
                let field_ty = substitute_aggregate_member(ty, &agg.generics, &field.ty);
                type_contains_dyn_value(&field_ty, decls, seen)
            })
        }),
        NominalKind::Enum => decls.enum_schema(&key).is_some_and(|schema| {
            let Some(nominal) = ty.as_nominal() else {
                return false;
            };
            let args = GenericArgs {
                type_args: nominal.type_args.clone(),
                const_args: ConstTerm::from_args(&nominal.const_args),
            };
            let (type_subst, const_subst) = schema.generics.substitutions(&args);
            schema
                .variants
                .values()
                .any(|variant| match &variant.payload {
                    VariantPayload::Unit => false,
                    VariantPayload::Tuple(types) => types.iter().any(|ty| {
                        let ty = substitute(ty, &type_subst, &const_subst);
                        type_contains_dyn_value(&ty, decls, seen)
                    }),
                    VariantPayload::Struct(fields) => fields.values().any(|field| {
                        let ty = substitute(&field.ty, &type_subst, &const_subst);
                        type_contains_dyn_value(&ty, decls, seen)
                    }),
                })
        }),
        NominalKind::Extern => false,
    };
    seen.remove(&key);
    contains
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

fn check_closure_flow_branch<R>(
    tc: &mut TypeChecker,
    check: impl FnOnce(&mut TypeChecker) -> R,
) -> R {
    let flow = tc.closure.closure_flow_snapshot();
    let ret = check(tc);
    let branch_flow = tc.closure.closure_flow_snapshot();
    tc.closure.join_closure_flow_snapshots(&flow, &branch_flow);
    ret
}

fn check_closure_flow_branches<R>(
    tc: &mut TypeChecker,
    left: impl FnOnce(&mut TypeChecker) -> R,
    right: impl FnOnce(&mut TypeChecker) -> R,
) -> (R, R) {
    let flow = tc.closure.closure_flow_snapshot();
    let left_ret = left(tc);
    let left_flow = tc.closure.closure_flow_snapshot();
    tc.closure.restore_closure_flow(&flow);
    let right_ret = right(tc);
    let right_flow = tc.closure.closure_flow_snapshot();
    tc.closure
        .join_closure_flow_snapshots(&left_flow, &right_flow);
    (left_ret, right_ret)
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
            check_closure_flow_branch(tc, |tc| check_block_checked(&if_node.node.then_block, tc));
        }
        return checked_void(tc);
    };
    if known_cond == Some(true) {
        return check_block_checked_with_hint(&if_node.node.then_block, expected, tc);
    }
    if known_cond == Some(false) {
        return check_block_checked_with_hint(else_block, expected, tc);
    }
    let (then, else_checked) = check_closure_flow_branches(
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
    let (then, else_checked) = check_closure_flow_branches(
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

#[derive(Clone, Copy, PartialEq, Eq)]
enum CollectionLiteralKind {
    Array,
    List,
}

fn expected_assignable_type(expected: Option<&TypeHandle>, tc: &TypeChecker) -> Option<Type> {
    let ty = expected.map(|handle| tc.handle_type(handle))?;
    Some(tc.decls.core_option_inner(&ty).unwrap_or(&ty).clone())
}

fn check_lambda_expr(
    expr: &ExprNode,
    lambda: &LambdaNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let expected_func = expected_assignable_type(expected.as_ref(), tc).and_then(|ty| match ty {
        Type::Func { params, ret } => Some((params, *ret)),
        _ => None,
    });
    if let Some((params, _)) = &expected_func
        && params.len() != lambda.node.params.len()
    {
        tc.push_error(TypeError::LambdaParamCountMismatch {
            expected: params.len(),
            found: lambda.node.params.len(),
            span: tc.error_span(lambda.span),
        });
        return checked_from_type(expr, Type::Infer, tc);
    }

    let params = lambda
        .node
        .params
        .iter()
        .enumerate()
        .map(|(index, param)| {
            let ty = match &param.ty {
                Some(ty) => tc.resolve_callable_param_type(ty, lambda.span, false),
                None => expected_func
                    .as_ref()
                    .and_then(|(params, _)| params.get(index))
                    .map_or_else(
                        || {
                            tc.push_error(TypeError::CannotInferType {
                                span: tc.error_span(lambda.span),
                            });
                            Type::Infer
                        },
                        |param| param.ty.clone(),
                    ),
            };
            tc.validate_func_param_escape(
                param.escape,
                param.mutable,
                param.cast_accept,
                &ty,
                lambda.span,
            );
            FuncParam::new(ty, param.mutable, param.cast_accept, param.escape)
        })
        .collect::<Vec<_>>();

    let explicit_ret = lambda
        .node
        .ret_type
        .as_ref()
        .map(|ret| ret.with_ty(tc.resolve_type_for_tc_at(&ret.ty, lambda.span)));
    if let Some(ret) = &explicit_ret {
        validate_return_spec(ret, false, has_mutable_func_param(&params), lambda.span, tc);
    }
    let expected_ret = explicit_ret.or_else(|| expected_func.as_ref().map(|(_, ret)| ret.clone()));

    let flow = tc.enter_function_control_flow();
    tc.enter_lambda(expr.node.id);
    tc.push_scope();
    let mut source = None;
    for (param, param_ty) in lambda.node.params.iter().zip(&params) {
        let kind = LocalBindingKind::from_param(param.mutable, &param_ty.ty);
        let type_id = tc.define_value(param.name, param_ty.ty.clone(), kind, None);
        tc.mark_non_escaping_callback_param(param.name, type_id, param_ty, param.ty.as_ref());
        if source.is_none() && param.mutable {
            source = Some(PlaceIdentity::root(PlaceRoot::Local(type_id)));
        }
    }
    let return_mode = match &expected_ret {
        Some(ret) => ReturnMode::Explicit {
            ret: ret.clone(),
            source: source.clone(),
        },
        None => ReturnMode::Infer {
            access: ReturnAccess::Value,
            source: None,
            candidates: vec![],
        },
    };
    tc.push_return_frame(return_mode);

    check_callable_body_with_return(
        CallableBody::Expr(&lambda.node.body),
        expected_ret.as_ref(),
        source.as_ref(),
        lambda.span,
        tc,
    );

    let frame = tc.pop_return_frame();
    let inferred_ret = frame.and_then(|frame| infer_return_type(frame, tc));
    tc.pop_scope();
    tc.closure.exit_lambda();
    tc.closure.drain_escape_events(expr.span);
    tc.exit_function_control_flow(flow);
    tc.closure.lambda_value(expr.node.id);

    let ret = expected_ret
        .or_else(|| inferred_ret.map(ReturnSpec::value))
        .unwrap_or_else(|| ReturnSpec::value(Type::Infer));
    checked_from_type(
        expr,
        Type::Func {
            params,
            ret: Box::new(ret),
        },
        tc,
    )
}

fn expected_collection(
    expected: Option<&TypeHandle>,
    tc: &TypeChecker,
) -> Option<(TypeHandle, CollectionLiteralKind)> {
    match expected_assignable_type(expected, tc)? {
        Type::Array { elem, .. } | Type::Slice { elem } => {
            Some((tc.type_handle(&elem), CollectionLiteralKind::Array))
        }
        Type::List { elem } => Some((tc.type_handle(&elem), CollectionLiteralKind::List)),
        _ => None,
    }
}

fn expected_map(
    expected: Option<&TypeHandle>,
    tc: &TypeChecker,
) -> Option<(TypeHandle, TypeHandle)> {
    let Type::Map { key, value } = expected_assignable_type(expected, tc)? else {
        return None;
    };
    Some((tc.type_handle(&key), tc.type_handle(&value)))
}

fn expected_range_bound(expected: Option<&TypeHandle>, tc: &TypeChecker) -> Option<TypeHandle> {
    let expected = expected_assignable_type(expected, tc)?;
    let inner = tc.decls.core_range_inner(&expected)?;
    Some(tc.type_handle(inner))
}

fn collection_literal_handle(
    kind: CollectionLiteralKind,
    elem: TypeHandle,
    len: ArrayLen,
    tc: &mut TypeChecker,
) -> TypeHandle {
    match kind {
        CollectionLiteralKind::Array => tc.array_handle(&elem, &len),
        CollectionLiteralKind::List => tc.list_handle(&elem),
    }
}

fn contains_nil(elements: &[ExprNode]) -> bool {
    elements
        .iter()
        .any(|element| matches!(element.node.kind, ExprKind::Lit(Lit::Nil)))
}

fn option_elem_handle(elem: TypeHandle, tc: &mut TypeChecker) -> TypeHandle {
    let ty = tc.handle_type(&elem);
    if tc.decls.core_option_inner(&ty).is_some() {
        return elem;
    }
    let Some(option_ty) = tc.decls.core_option_of(ty) else {
        return elem;
    };
    tc.type_handle(&option_ty)
}

fn builtin_numeric_cast(source: &Type, target: &Type) -> bool {
    matches!(
        (source, target),
        (Type::Int, Type::Float) | (Type::Float, Type::Int)
    )
}

fn check_cast_expr(expr: &ExprNode, cast: &CastNode, tc: &mut TypeChecker) -> CheckedType {
    let target = tc.resolve_type_for_tc_at(&cast.node.target, cast.span);
    let checked = check_value_expr_checked_with_hint(&cast.node.expr, None, tc);
    let from = checked.ty;
    let conversion = tc.explicit_cast_conversion(&from, &target);
    match conversion {
        Some(ExplicitCast::Identity) => tc
            .closure
            .copy_expr_flow(cast.node.expr.node.id, expr.node.id),
        Some(ExplicitCast::CastFrom { escape }) => {
            tc.check_argument_escape(&cast.node.expr, escape);
        }
        Some(ExplicitCast::Builtin) | None => {}
    }
    let ty = if conversion.is_some() || matches!(from, Type::Infer) || matches!(target, Type::Infer)
    {
        target
    } else {
        tc.push_error(TypeError::InvalidCast {
            from,
            to: target,
            span: tc.error_span(cast.span),
        });
        Type::Infer
    };
    let mut casted = checked_from_type(expr, ty, tc);
    casted.contains_extern_any = checked.contains_extern_any;
    casted
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

fn check_range_expr(
    expr: &ExprNode,
    range: &RangeNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let bound = expected_range_bound(expected.as_ref(), tc)
        .unwrap_or_else(|| tc.fresh_temp_handle(range.span));
    let (kind, contains_extern_any) = match &range.node {
        Range::Bounded {
            start,
            end,
            inclusive,
        } => {
            let start = check_expected(start, bound.clone(), tc);
            let end = check_expected(end, bound.clone(), tc);
            let kind = if *inclusive {
                CoreRangeKind::Inclusive
            } else {
                CoreRangeKind::Exclusive
            };
            (kind, start.contains_extern_any || end.contains_extern_any)
        }
        Range::From { start } => {
            let start = check_expected(start, bound.clone(), tc);
            (CoreRangeKind::From, start.contains_extern_any)
        }
        Range::To { end, inclusive } => {
            let end = check_expected(end, bound.clone(), tc);
            let kind = if *inclusive {
                CoreRangeKind::ToInclusive
            } else {
                CoreRangeKind::To
            };
            (kind, end.contains_extern_any)
        }
    };
    tc.solve_constraints();
    let ty = tc
        .decls
        .core_range_of(kind, tc.handle_type(&bound))
        .expect("core range declaration is available");
    let mut checked = checked_from_type(expr, ty, tc);
    checked.contains_extern_any = contains_extern_any;
    checked
}

fn check_string_interp(expr: &ExprNode, parts: &[StringPart], tc: &mut TypeChecker) -> CheckedType {
    let mut contains_extern_any = false;
    for part in parts {
        let StringPart::Expr(inner, spec) = part else {
            continue;
        };
        let checked = check_value_expr_checked_with_hint(inner, None, tc);
        tc.reject_dyn_implicit_format(&checked.ty, inner.span);
        if let Some(spec) = spec {
            validate_format_spec(&checked.ty, &spec.node, spec.span, tc);
        }
        contains_extern_any |= checked.contains_extern_any;
    }
    let mut checked = checked_from_type(expr, Type::String, tc);
    checked.contains_extern_any = contains_extern_any;
    checked
}

fn validate_format_spec(ty: &Type, spec: &FormatSpec, span: Span, tc: &mut TypeChecker) {
    if matches!(ty, Type::Infer) {
        return;
    }
    let reason = match spec.kind {
        FormatKind::Hex | FormatKind::HexUpper | FormatKind::Binary if !ty.is_int() => {
            Some("integer format requires int")
        }
        FormatKind::Exp | FormatKind::ExpUpper if !ty.is_float() => {
            Some("scientific format requires float")
        }
        _ if spec.precision.is_some() && !(ty.is_float() || ty.is_str()) => {
            Some("precision requires float or string")
        }
        _ if spec.sign == FormatSign::Always && !(ty.is_int() || ty.is_float()) => {
            Some("sign requires numeric type")
        }
        _ => None,
    };
    if let Some(reason) = reason {
        tc.push_error(TypeError::InvalidFormatSpec {
            reason,
            span: tc.error_span(span),
        });
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum LiteralMapKey {
    Int(i64),
    Bool(bool),
    String(String),
    Tuple(Vec<LiteralMapKey>),
}

fn literal_map_key(expr: &ExprNode) -> Option<LiteralMapKey> {
    match &expr.node.kind {
        ExprKind::Lit(Lit::Int(value)) => Some(LiteralMapKey::Int(*value)),
        ExprKind::Lit(Lit::Bool(value)) => Some(LiteralMapKey::Bool(*value)),
        ExprKind::Lit(Lit::String(value)) => Some(LiteralMapKey::String(value.clone())),
        ExprKind::Tuple(elems) => elems
            .iter()
            .map(literal_map_key)
            .collect::<Option<Vec<_>>>()
            .map(LiteralMapKey::Tuple),
        _ => None,
    }
}

fn check_duplicate_map_keys(lit: &MapLiteralNode, tc: &mut TypeChecker) {
    let mut keys = HashSet::new();
    for (key, _) in &lit.node.entries {
        let Some(key_value) = literal_map_key(key) else {
            continue;
        };
        if !keys.insert(key_value) {
            tc.push_error(TypeError::DuplicateMapKey {
                span: tc.error_span(key.span),
            });
        }
    }
}

fn check_map_lit_hint(
    expr: &ExprNode,
    lit: &MapLiteralNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    check_duplicate_map_keys(lit, tc);
    let (key, value, has_hint) = match expected_map(expected.as_ref(), tc) {
        Some((key, value)) => (key, value, true),
        None => (
            tc.fresh_temp_handle(lit.span),
            tc.fresh_temp_handle(lit.span),
            false,
        ),
    };

    if lit.node.entries.is_empty() && !has_hint {
        tc.push_error(TypeError::CannotInferType {
            span: tc.error_span(lit.span),
        });
        return checked_from_type(expr, Type::Infer, tc);
    }

    let mut contains_extern_any = false;
    for (key_expr, value_expr) in &lit.node.entries {
        let key_checked = check_expected(key_expr, key.clone(), tc);
        tc.record_aggregate_elem_flow(expr.node.id, key_expr);
        let value_checked = check_expected(value_expr, value.clone(), tc);
        tc.record_aggregate_elem_flow(expr.node.id, value_expr);
        contains_extern_any |= key_checked.contains_extern_any || value_checked.contains_extern_any;
    }

    let map = tc.map_handle(&key, &value);
    let mut checked = solve_and_checked_from_handle(expr, map, tc);
    if !has_hint && let Type::Map { key, .. } = &checked.ty {
        tc.validate_map_key_type(key, lit.span);
    }
    checked.contains_extern_any = contains_extern_any;
    checked
}

fn check_array_lit_hint(
    expr: &ExprNode,
    lit: &ArrayLiteralNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let expected = expected.as_ref();
    let expected_collection = expected_collection(expected, tc);
    let has_nil = contains_nil(&lit.node.elements);
    if expected_collection.is_none()
        && !lit.node.elements.is_empty()
        && lit
            .node
            .elements
            .iter()
            .all(|element| matches!(element.node.kind, ExprKind::Lit(Lit::Nil)))
    {
        tc.push_error(TypeError::AllNilArrayLiteral {
            span: tc.error_span(lit.span),
        });
    }
    let (elem, kind) = expected_collection
        .unwrap_or_else(|| (tc.fresh_temp_handle(lit.span), CollectionLiteralKind::Array));
    let elem = if has_nil {
        option_elem_handle(elem, tc)
    } else {
        elem
    };
    let array = collection_literal_handle(
        kind,
        elem.clone(),
        ArrayLen::Fixed(lit.node.elements.len()),
        tc,
    );
    let mut contains_extern_any = false;
    for value in &lit.node.elements {
        let checked = check_expected(value, elem.clone(), tc);
        tc.record_aggregate_elem_flow(expr.node.id, value);
        contains_extern_any |= checked.contains_extern_any;
    }
    let mut checked = solve_and_checked_from_handle(expr, array, tc);
    checked.contains_extern_any = contains_extern_any;
    checked
}

fn check_array_fill_hint(
    expr: &ExprNode,
    fill: &ArrayFillNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let len = match tc.eval_const_expr(&fill.node.len, true) {
        Ok(const_value) => {
            match const_eval::const_usize(&const_value, tc.error_span(fill.node.len.span)) {
                Ok(len) => ArrayLen::Fixed(len),
                Err(err) => {
                    tc.push_error(err);
                    ArrayLen::Infer
                }
            }
        }
        Err(TypeError::NonConstExpression { .. }) => {
            tc.push_error(TypeError::ArrayFillLengthNotConst {
                span: tc.error_span(fill.node.len.span),
            });
            ArrayLen::Infer
        }
        Err(err) => {
            tc.push_error(err);
            ArrayLen::Infer
        }
    };
    let (elem, kind) = expected_collection(expected.as_ref(), tc).unwrap_or_else(|| {
        (
            tc.fresh_temp_handle(fill.node.value.span),
            CollectionLiteralKind::Array,
        )
    });
    let value = check_expected(&fill.node.value, elem.clone(), tc);
    tc.record_aggregate_elem_flow(expr.node.id, &fill.node.value);
    let array = collection_literal_handle(kind, elem, len, tc);
    let mut checked = solve_and_checked_from_handle(expr, array, tc);
    checked.contains_extern_any = value.contains_extern_any;
    checked
}

fn tuple_hints(
    elems: &[ExprNode],
    expected: Option<&TypeHandle>,
    tc: &mut TypeChecker,
) -> Vec<TypeHandle> {
    match expected_assignable_type(expected, tc) {
        Some(Type::Tuple(types)) if types.len() == elems.len() => {
            types.iter().map(|ty| tc.type_handle(ty)).collect()
        }
        _ => elems
            .iter()
            .map(|elem| tc.fresh_temp_handle(elem.span))
            .collect(),
    }
}

fn check_tuple_checked_with_hint(
    expr: &ExprNode,
    elems: &[ExprNode],
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let hints = tuple_hints(elems, expected.as_ref(), tc);
    let mut contains_extern_any = false;
    for (elem, hint) in elems.iter().zip(&hints) {
        let checked = check_expected(elem, hint.clone(), tc);
        tc.record_aggregate_elem_flow(expr.node.id, elem);
        contains_extern_any |= checked.contains_extern_any;
    }
    let tuple = tc.tuple_handle(hints);
    let mut checked = solve_and_checked_from_handle(expr, tuple, tc);
    checked.contains_extern_any = contains_extern_any;
    checked
}

fn check_tuple_index(expr: &ExprNode, node: &TupleIndexNode, tc: &mut TypeChecker) -> CheckedType {
    let target = check_expr_checked(&node.node.target, tc);
    tc.closure
        .copy_place_identity(node.node.target.node.id, expr.node.id);
    check_tuple_index_access(expr, node, &target, tc)
}

fn check_tuple_index_access(
    expr: &ExprNode,
    node: &TupleIndexNode,
    target: &CheckedType,
    tc: &mut TypeChecker,
) -> CheckedType {
    let Type::Tuple(elems) = &target.ty else {
        tc.push_error(TypeError::TupleIndexOnNonTuple {
            ty: target.ty.clone(),
            index: node.node.index,
            span: tc.error_span(node.span),
        });
        return checked_from_type(expr, Type::Infer, tc);
    };

    let Some(elem_ty) = elems.get(node.node.index as usize).cloned() else {
        tc.push_error(TypeError::TupleIndexOutOfBounds {
            index: node.node.index,
            len: elems.len(),
            span: tc.error_span(node.span),
        });
        return checked_from_type(expr, Type::Infer, tc);
    };

    let mut checked = checked_from_type(expr, elem_ty, tc);
    checked.contains_extern_any = target.contains_extern_any;
    checked
}

struct CheckedIndex {
    read_ty: Type,
    write_ty: Type,
    contains_extern_any: bool,
}

impl CheckedIndex {
    fn new(read_ty: Type, write_ty: Type, target: &CheckedType, index: &CheckedType) -> Self {
        Self {
            read_ty,
            write_ty,
            contains_extern_any: target.contains_extern_any || index.contains_extern_any,
        }
    }

    fn same(ty: Type, target: &CheckedType, index: &CheckedType) -> Self {
        Self::new(ty.clone(), ty, target, index)
    }

    fn infer(target: &CheckedType, index: &CheckedType) -> Self {
        Self::same(Type::Infer, target, index)
    }
}

fn check_index_expr(expr: &ExprNode, node: &IndexNode, tc: &mut TypeChecker) -> CheckedType {
    let target = check_expr_checked(&node.node.target, tc);
    tc.closure
        .copy_place_identity(node.node.target.node.id, expr.node.id);
    let indexed = check_index_access(node, &target, tc);
    let mut checked = checked_from_type(expr, indexed.read_ty, tc);
    checked.contains_extern_any = indexed.contains_extern_any;
    checked
}

fn check_index_access(
    node: &IndexNode,
    target: &CheckedType,
    tc: &mut TypeChecker,
) -> CheckedIndex {
    if node.node.safe || has_safe_postfix(&node.node.target) {
        let inner = tc.optional_chain_inner_type(&target.ty, node.span);
        let inner_target = CheckedType {
            ty: inner,
            handle: target.handle.clone(),
            contains_extern_any: target.contains_extern_any,
        };
        let indexed = check_index_access_inner(node, &inner_target, tc);
        return CheckedIndex {
            read_ty: tc.optional_chain_result_type(indexed.read_ty),
            write_ty: Type::Infer,
            contains_extern_any: indexed.contains_extern_any,
        };
    }

    check_index_access_inner(node, target, tc)
}

fn has_safe_postfix(expr: &ExprNode) -> bool {
    match &expr.node.kind {
        ExprKind::Field(field) => field.node.safe || has_safe_postfix(&field.node.target),
        ExprKind::Call(call) => call.node.safe || has_safe_postfix(&call.node.func),
        ExprKind::Index(index) => index.node.safe || has_safe_postfix(&index.node.target),
        _ => false,
    }
}

fn check_index_access_inner(
    node: &IndexNode,
    target: &CheckedType,
    tc: &mut TypeChecker,
) -> CheckedIndex {
    if matches!(node.node.index.node.kind, ExprKind::Range(_)) {
        return check_range_index_access(node, target, tc);
    }

    match &target.ty {
        Type::List { elem } | Type::Array { elem, .. } | Type::Slice { elem } => {
            let index = check_value_expr_checked_with_hint(
                &node.node.index,
                Some(tc.type_handle(&Type::Int)),
                tc,
            );
            if !matches!(index.ty, Type::Infer | Type::Int) {
                tc.push_error(TypeError::IndexNotInt {
                    found: index.ty.clone(),
                    span: tc.error_span(node.node.index.span),
                });
            }
            CheckedIndex::same((**elem).clone(), target, &index)
        }
        Type::Map { key, value } => {
            let key_handle = tc.type_handle(key);
            let index =
                check_value_expr_checked_with_hint(&node.node.index, Some(key_handle.clone()), tc);
            tc.expect_assignable(node.node.index.span, index.handle.clone(), key_handle);
            tc.solve_constraints();
            let value = (**value).clone();
            CheckedIndex::new(
                tc.decls
                    .core_option_of(value.clone())
                    .unwrap_or(Type::Infer),
                value,
                target,
                &index,
            )
        }
        Type::Infer => {
            let index = check_expr_checked(&node.node.index, tc);
            CheckedIndex::infer(target, &index)
        }
        found => {
            let index = check_expr_checked(&node.node.index, tc);
            if !found.is_void() {
                tc.push_error(TypeError::IndexOnNonIndexable {
                    found: found.clone(),
                    span: tc.error_span(node.span),
                });
            }
            CheckedIndex::infer(target, &index)
        }
    }
}

fn check_range_index_access(
    node: &IndexNode,
    target: &CheckedType,
    tc: &mut TypeChecker,
) -> CheckedIndex {
    let index = check_expr_checked(&node.node.index, tc);
    match &target.ty {
        Type::List { elem } | Type::Array { elem, .. } | Type::Slice { elem } => {
            if !matches!(
                tc.decls.core_range_inner(&index.ty),
                Some(Type::Int | Type::Infer)
            ) {
                tc.push_error(TypeError::RangeIndexNotInt {
                    found: index.ty.clone(),
                    span: tc.error_span(node.node.index.span),
                });
            }
            CheckedIndex::new(
                Type::Slice { elem: elem.clone() },
                Type::Infer,
                target,
                &index,
            )
        }
        Type::Infer => CheckedIndex::infer(target, &index),
        found => {
            tc.push_error(TypeError::RangeIndexUnsupported {
                found: found.clone(),
                span: tc.error_span(node.span),
            });
            CheckedIndex::infer(target, &index)
        }
    }
}

struct NominalLiteralSolver {
    vars: GenericSolverVars,
}

impl NominalLiteralSolver {
    fn new(
        generics: &GenericParams,
        args: &[GenericArg],
        span: Span,
        tc: &mut TypeChecker,
    ) -> Option<Self> {
        let seeds = if args.is_empty() {
            GenericSolverSeeds::default()
        } else {
            let args = bind_exact_generic_args(tc, generics, args, span)?;
            GenericSolverSeeds::from_args(generics, &args)
        };
        Some(Self::from_seeds(generics, &seeds, span, tc))
    }

    fn without_args(generics: &GenericParams, span: Span, tc: &mut TypeChecker) -> Self {
        Self::from_seeds(generics, &GenericSolverSeeds::default(), span, tc)
    }

    fn from_seeds(
        generics: &GenericParams,
        seeds: &GenericSolverSeeds,
        span: Span,
        tc: &mut TypeChecker,
    ) -> Self {
        Self {
            vars: tc
                .solver
                .generic_solver_vars(generics, seeds, tc.error_span(span)),
        }
    }

    fn bind_expected(
        &self,
        key: &NominalKey,
        generics: &GenericParams,
        expected: Option<&Type>,
        span: Span,
        tc: &mut TypeChecker,
    ) -> bool {
        let Some(expected) = expected else {
            return true;
        };
        if tc.decls.key_for_type(expected).as_ref() != Some(key) {
            return true;
        }
        let template = nominal_literal_type(key, generics, None);
        let template = tc.solver.instantiate_generic_type(&template, &self.vars);
        let expected = tc.type_handle(expected);
        tc.expect_equal(span, template, expected);
        !tc.solve_constraints()
    }

    fn instantiate(&self, ty: &Type, tc: &mut TypeChecker) -> TypeHandle {
        tc.solver.instantiate_generic_type(ty, &self.vars)
    }

    fn finalize(
        &self,
        key: &NominalKey,
        generics: &GenericParams,
        span: Span,
        tc: &mut TypeChecker,
    ) -> Option<Type> {
        let args = match tc.solver.finalize_generic_args(generics, &self.vars) {
            Ok(args) => args,
            Err(unbound) => {
                tc.push_unbound_generic_errors(unbound, span);
                return None;
            }
        };
        if !tc.check_generic_bounds(generics, &args, span) {
            return None;
        }
        Some(nominal_literal_type(key, generics, Some(&args)))
    }
}

fn check_struct_lit_hint(
    expr: &ExprNode,
    lit: &StructLiteralNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    if let Some(checked) = check_enum_struct_variant_lit(expr, lit, expected.clone(), tc) {
        return checked;
    }

    let Some(target) = resolve_struct_target(lit, tc) else {
        check_unknown_nominal_fields(&lit.node.fields, tc);
        return checked_from_type(expr, Type::Infer, tc);
    };
    let key = target.key.clone();

    if key.kind == NominalKind::Extern {
        return check_extern_lit(expr, lit, &key, expected, tc);
    }

    let valid_literal_target = matches!(key.kind, NominalKind::Struct | NominalKind::DataRef);
    if !valid_literal_target {
        let kind = match key.kind {
            NominalKind::Enum => "enum",
            NominalKind::Extern => "extern",
            NominalKind::Struct | NominalKind::DataRef => unreachable!(),
        };
        tc.push_error(TypeError::InvalidStructLiteral {
            name: key.name,
            kind: kind.to_string(),
            span: tc.error_span(lit.span),
        });
        check_unknown_nominal_fields(&lit.node.fields, tc);
        return checked_from_type(expr, Type::Infer, tc);
    }

    let agg = tc
        .decls
        .aggregate(&key)
        .expect("aggregate exists for resolved key")
        .clone();
    let kind = match key.kind {
        NominalKind::Struct => DeprecatedUseKind::Struct,
        NominalKind::DataRef => DeprecatedUseKind::DataRef,
        NominalKind::Enum | NominalKind::Extern => unreachable!("aggregate key checked above"),
    };
    tc.warn_deprecated(&agg.policy, kind, key.name, lit.span);
    let expected_ty = expected.as_ref().map(|handle| tc.handle_type(handle));
    let inf = match &target.seeds {
        Some(seeds) => NominalLiteralSolver::from_seeds(&agg.generics, seeds, lit.span, tc),
        None => {
            let Some(inf) =
                NominalLiteralSolver::new(&agg.generics, &lit.node.generic_args, lit.span, tc)
            else {
                check_unknown_nominal_fields(&lit.node.fields, tc);
                return checked_from_type(expr, Type::Infer, tc);
            };
            inf
        }
    };
    let expected_ok = inf.bind_expected(&key, &agg.generics, expected_ty.as_ref(), lit.span, tc);
    let field_check = check_nominal_fields(
        expr.node.id,
        &lit.node.fields,
        &agg.fields,
        nominal_type(&key),
        lit.span,
        &inf,
        tc,
    );
    if !expected_ok || field_check.failed {
        return checked_from_type(expr, Type::Infer, tc);
    }
    let Some(ty) = inf.finalize(&key, &agg.generics, lit.span, tc) else {
        return checked_from_type(expr, Type::Infer, tc);
    };
    tc.reject_user_any_type(&ty, lit.span);
    let handle = tc.type_handle(&ty);
    let mut checked = solve_and_checked_from_handle(expr, handle, tc);
    checked.contains_extern_any = field_check.contains_extern_any;
    checked
}

fn check_enum_struct_variant_lit(
    expr: &ExprNode,
    lit: &StructLiteralNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> Option<CheckedType> {
    let qualifier = lit.node.qualifier?;
    let subject = tc.visible_type_subject(qualifier, lit.span)?;
    let key = tc
        .decls
        .key_for_type(&subject)
        .filter(|key| key.kind == NominalKind::Enum)?;
    let Some(resolved) = enum_variant::resolve_use(tc, &key, lit.node.name, lit.span) else {
        check_unknown_nominal_fields(&lit.node.fields, tc);
        return Some(checked_from_type(expr, Type::Infer, tc));
    };
    let VariantPayload::Struct(fields) = &resolved.schema.payload else {
        enum_variant::push_shape_mismatch(tc, &resolved, VariantShape::Struct, lit.span);
        check_unknown_nominal_fields(&lit.node.fields, tc);
        return Some(checked_from_type(expr, Type::Infer, tc));
    };

    if !lit.node.generic_args.is_empty() {
        tc.push_error(TypeError::GenericArity(ArityError::TypeArgs {
            expected: 0,
            found: lit.node.generic_args.len(),
        }));
        check_unknown_nominal_fields(&lit.node.fields, tc);
        return Some(checked_from_type(expr, Type::Infer, tc));
    }

    let expected_ty = expected.as_ref().map(|handle| tc.handle_type(handle));
    let inf = NominalLiteralSolver::without_args(&resolved.generics, lit.span, tc);
    let expected_ok =
        inf.bind_expected(&key, &resolved.generics, expected_ty.as_ref(), lit.span, tc);
    let field_check = check_variant_literal_fields(
        expr.node.id,
        &lit.node.fields,
        fields,
        &key,
        lit.node.name,
        lit.span,
        &inf,
        tc,
    );
    if !expected_ok || field_check.failed {
        return Some(checked_from_type(expr, Type::Infer, tc));
    }
    let Some(ty) = inf.finalize(&key, &resolved.generics, lit.span, tc) else {
        return Some(checked_from_type(expr, Type::Infer, tc));
    };
    tc.reject_user_any_type(&ty, lit.span);
    let handle = tc.type_handle(&ty);
    let mut checked = solve_and_checked_from_handle(expr, handle, tc);
    checked.contains_extern_any = field_check.contains_extern_any;
    Some(checked)
}

fn check_extern_lit(
    expr: &ExprNode,
    lit: &StructLiteralNode,
    key: &NominalKey,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let Some(owner) = tc.externs.type_by_nominal(key) else {
        tc.push_error(TypeError::InvalidStructLiteral {
            name: key.name,
            kind: "extern".to_string(),
            span: tc.error_span(lit.span),
        });
        check_unknown_nominal_fields(&lit.node.fields, tc);
        return checked_from_type(expr, Type::Infer, tc);
    };

    if !lit.node.generic_args.is_empty() {
        tc.push_error(TypeError::GenericArity(ArityError::TypeArgs {
            expected: 0,
            found: lit.node.generic_args.len(),
        }));
        check_unknown_nominal_fields(&lit.node.fields, tc);
        return checked_from_type(expr, Type::Infer, tc);
    }

    let Some(init) = tc.externs.init(owner).cloned() else {
        tc.push_error(TypeError::InvalidStructLiteral {
            name: key.name,
            kind: "extern".to_string(),
            span: tc.error_span(lit.span),
        });
        check_unknown_nominal_fields(&lit.node.fields, tc);
        return checked_from_type(expr, Type::Infer, tc);
    };

    let expected_ty = expected.as_ref().map(|handle| tc.handle_type(handle));
    if let Some(expected_ty) = expected_ty.as_ref()
        && tc.decls.key_for_type(expected_ty).as_ref() == Some(key)
    {
        let expected = tc.type_handle(expected_ty);
        let actual = tc.type_handle(&nominal_type(key));
        tc.expect_equal(lit.span, actual, expected);
    }

    let fields_failed = check_extern_literal_fields(
        expr.node.id,
        &lit.node.fields,
        owner,
        &init.field_init,
        lit.span,
        tc,
    );
    if fields_failed {
        return checked_from_type(expr, Type::Infer, tc);
    }

    tc.record_extern_use(expr.node.id, ExternUseTarget::Init(owner));
    let ty = nominal_type(key);
    let handle = tc.type_handle(&ty);
    solve_and_checked_from_handle(expr, handle, tc)
}

fn check_extern_literal_fields(
    aggregate: ExprId,
    fields: &[(Ident, ExprNode)],
    owner: ExternTypeId,
    explicit_init: &[Ident],
    span: Span,
    tc: &mut TypeChecker,
) -> bool {
    let owner_ty = nominal_type(&tc.extern_type(owner).nominal);
    let mut seen = HashMap::new();
    let mut failed = false;
    for (name, value) in fields {
        let duplicate = seen.insert(*name, value.span).is_some();
        if duplicate {
            tc.push_error(TypeError::DuplicateField {
                name: *name,
                span: tc.error_span(value.span),
            });
            failed = true;
        }

        let Some((_, field)) = tc.extern_field(owner, *name) else {
            tc.push_error(TypeError::UnknownMember {
                ty: owner_ty.clone(),
                member: *name,
                kind: MemberAccessKind::Field,
                span: tc.error_span(value.span),
            });
            check_expr_checked(value, tc);
            failed = true;
            continue;
        };

        let field_ty = field.ty.clone();
        let allowed = if explicit_init.is_empty() {
            !field.computed
        } else {
            explicit_init.contains(name)
        };
        if !allowed {
            tc.push_error(TypeError::ImmutableAssignment {
                name: *name,
                span: tc.error_span(value.span),
            });
            failed = true;
        }
        let hint = tc.type_handle(&field_ty.ty);
        let checked = check_expr_checked_with_hint(value, Some(hint), tc);
        if !duplicate && allowed {
            tc.record_aggregate_elem_flow(aggregate, value);
        }
        failed |= !extern_boundary::check_checked_value(value, &checked, &field_ty, tc);
    }

    for name in required_extern_literal_fields(owner, explicit_init, tc) {
        if !seen.contains_key(&name) {
            tc.push_error(TypeError::MissingField {
                name,
                span: tc.error_span(span),
            });
            failed = true;
        }
    }
    failed
}

fn required_extern_literal_fields(
    owner: ExternTypeId,
    explicit_init: &[Ident],
    tc: &TypeChecker,
) -> Vec<Ident> {
    if !explicit_init.is_empty() {
        return explicit_init.to_vec();
    }
    tc.extern_type(owner)
        .fields
        .iter()
        .filter(|field| !field.computed)
        .map(|field| field.name)
        .collect()
}

fn check_inferred_enum_hint(
    expr: &ExprNode,
    node: &InferredEnumNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let Some(expected) = expected else {
        return cannot_infer_inferred_enum(expr, node, tc);
    };
    let expected_ty = tc.handle_type(&expected);
    let expected_key = tc
        .decls
        .key_for_type(&expected_ty)
        .filter(|key| key.kind == NominalKind::Enum);
    let Some(key) = expected_key else {
        return cannot_infer_inferred_enum(expr, node, tc);
    };

    if tc.decls.enum_schema(&key).is_none() {
        return checked_from_type(expr, Type::Infer, tc);
    }
    let Some(resolved) = enum_variant::resolve_use(tc, &key, node.node.variant, node.span) else {
        check_inferred_enum_args(&node.node.args, tc);
        return checked_from_type(expr, Type::Infer, tc);
    };
    let generics = resolved.generics.clone();
    let variant = &resolved.schema;

    let inf = NominalLiteralSolver::without_args(&generics, node.span, tc);
    if !inf.bind_expected(&key, &generics, Some(&expected_ty), node.span, tc) {
        check_inferred_enum_args(&node.node.args, tc);
        return checked_from_type(expr, Type::Infer, tc);
    }

    let mut contains_extern_any = false;
    match (&variant.payload, &node.node.args) {
        (VariantPayload::Unit, InferredEnumArgs::Unit) => {}
        (VariantPayload::Unit, args) => {
            enum_variant::push_shape_mismatch(
                tc,
                &resolved,
                inferred_enum_arg_shape(args),
                node.span,
            );
            check_inferred_enum_args(args, tc);
            return checked_from_type(expr, Type::Infer, tc);
        }
        (VariantPayload::Tuple(params), InferredEnumArgs::Tuple(args)) => {
            if params.len() != args.len() {
                enum_variant::push_arg_count_mismatch(
                    tc,
                    key.name,
                    node.node.variant,
                    params.len(),
                    args.len(),
                    node.span,
                );
                check_exprs_without_hint(args, tc);
                return checked_from_type(expr, Type::Infer, tc);
            }
            let mut failed = false;
            for (arg, param) in args.iter().zip(params) {
                let hint = inf.instantiate(param, tc);
                let checked = check_expected(arg, hint, tc);
                tc.record_aggregate_elem_flow(expr.node.id, arg);
                contains_extern_any |= checked.contains_extern_any;
                failed |= tc.solve_constraints();
            }
            if failed || inf.finalize(&key, &generics, node.span, tc).is_none() {
                return checked_from_type(expr, Type::Infer, tc);
            }
        }
        (VariantPayload::Tuple(_), args) => {
            enum_variant::push_shape_mismatch(
                tc,
                &resolved,
                inferred_enum_arg_shape(args),
                node.span,
            );
            check_inferred_enum_args(args, tc);
            return checked_from_type(expr, Type::Infer, tc);
        }
        (VariantPayload::Struct(fields), InferredEnumArgs::Struct(args)) => {
            let field_check = check_variant_literal_fields(
                expr.node.id,
                args,
                fields,
                &key,
                node.node.variant,
                node.span,
                &inf,
                tc,
            );
            contains_extern_any |= field_check.contains_extern_any;
            if field_check.failed || inf.finalize(&key, &generics, node.span, tc).is_none() {
                return checked_from_type(expr, Type::Infer, tc);
            }
        }
        (VariantPayload::Struct(_), args) => {
            enum_variant::push_shape_mismatch(
                tc,
                &resolved,
                inferred_enum_arg_shape(args),
                node.span,
            );
            check_inferred_enum_args(args, tc);
            return checked_from_type(expr, Type::Infer, tc);
        }
    }

    let mut checked = solve_and_checked_from_handle(expr, expected, tc);
    checked.contains_extern_any = contains_extern_any;
    checked
}

fn cannot_infer_inferred_enum(
    expr: &ExprNode,
    node: &InferredEnumNode,
    tc: &mut TypeChecker,
) -> CheckedType {
    tc.push_error(TypeError::CannotInferEnum {
        span: tc.error_span(node.span),
    });
    check_inferred_enum_args(&node.node.args, tc);
    checked_from_type(expr, Type::Infer, tc)
}

#[derive(Default)]
struct NominalFieldCheck {
    failed: bool,
    contains_extern_any: bool,
}

fn check_nominal_fields(
    aggregate: ExprId,
    fields: &[(Ident, ExprNode)],
    schema: &HashMap<Ident, FieldSchema>,
    owner_ty: Type,
    span: Span,
    inf: &NominalLiteralSolver,
    tc: &mut TypeChecker,
) -> NominalFieldCheck {
    check_expr_fields(
        aggregate,
        fields,
        schema,
        field_check::FieldOwner::Nominal(owner_ty),
        field_check::MissingFields::AllowDefaults,
        span,
        inf,
        tc,
    )
}

fn check_variant_literal_fields(
    aggregate: ExprId,
    fields: &[(Ident, ExprNode)],
    schema: &HashMap<Ident, FieldSchema>,
    key: &NominalKey,
    variant: Ident,
    span: Span,
    inf: &NominalLiteralSolver,
    tc: &mut TypeChecker,
) -> NominalFieldCheck {
    check_expr_fields(
        aggregate,
        fields,
        schema,
        field_check::FieldOwner::Variant {
            key: key.clone(),
            variant,
        },
        field_check::MissingFields::AllowDefaults,
        span,
        inf,
        tc,
    )
}

fn check_expr_fields(
    aggregate: ExprId,
    fields: &[(Ident, ExprNode)],
    schema: &HashMap<Ident, FieldSchema>,
    owner: field_check::FieldOwner,
    missing: field_check::MissingFields,
    span: Span,
    inf: &NominalLiteralSolver,
    tc: &mut TypeChecker,
) -> NominalFieldCheck {
    let uses = fields
        .iter()
        .enumerate()
        .map(|(index, (name, value))| field_check::FieldUse {
            name: *name,
            span: value.span,
            index,
        })
        .collect::<Vec<_>>();
    let shape = field_check::check(&uses, schema, &owner, missing, Some(span), tc);
    let valid = shape
        .fields
        .iter()
        .map(|field| field.index)
        .collect::<HashSet<_>>();
    for (index, (_, value)) in fields.iter().enumerate() {
        if !valid.contains(&index) {
            check_expr_checked(value, tc);
        }
    }
    let mut check = NominalFieldCheck {
        failed: shape.failed,
        contains_extern_any: false,
    };
    for field in shape.fields {
        let value = &fields[field.index].1;
        tc.check_matched_field_access_policy(&owner, field.name, &field.policy, value.span);
        let hint = inf.instantiate(&field.ty, tc);
        let checked = check_expr_checked_with_hint(value, Some(hint.clone()), tc);
        tc.record_aggregate_elem_flow(aggregate, value);
        check.contains_extern_any |= checked.contains_extern_any;
        tc.expect_assignable_expr(value.span, value.node.id, checked.handle, hint);
        check.failed |= tc.solve_constraints();
    }
    check
}

fn check_unknown_nominal_fields(fields: &[(Ident, ExprNode)], tc: &mut TypeChecker) {
    for (_, value) in fields {
        check_expr_checked(value, tc);
    }
}

fn check_inferred_enum_args(args: &InferredEnumArgs, tc: &mut TypeChecker) {
    match args {
        InferredEnumArgs::Unit => {}
        InferredEnumArgs::Tuple(args) => check_exprs_without_hint(args, tc),
        InferredEnumArgs::Struct(fields) => check_unknown_nominal_fields(fields, tc),
    }
}

fn check_exprs_without_hint(args: &[ExprNode], tc: &mut TypeChecker) {
    for arg in args {
        check_expr_checked(arg, tc);
    }
}

fn inferred_enum_arg_shape(args: &InferredEnumArgs) -> VariantShape {
    match args {
        InferredEnumArgs::Unit => VariantShape::Unit,
        InferredEnumArgs::Tuple(_) => VariantShape::Tuple,
        InferredEnumArgs::Struct(_) => VariantShape::Struct,
    }
}

fn nominal_literal_type(
    key: &NominalKey,
    generics: &GenericParams,
    args: Option<&GenericArgs>,
) -> Type {
    if let Some(args) = args {
        let const_args = ConstTerm::to_args_no_infer(&args.const_args)
            .expect("nominal literal finalization must not produce inference const terms");
        return nominal_type_with_args(key, &args.type_args, &const_args);
    }

    let type_args = generics
        .type_params
        .iter()
        .map(|param| Type::Var(param.id))
        .collect::<Vec<_>>();
    let const_args = generics
        .const_params
        .iter()
        .map(|param| ConstArg::Param(param.id))
        .collect::<Vec<_>>();
    nominal_type_with_args(key, &type_args, &const_args)
}

struct StructLiteralTarget {
    key: NominalKey,
    seeds: Option<GenericSolverSeeds>,
}

fn resolve_struct_target(
    lit: &StructLiteralNode,
    tc: &mut TypeChecker,
) -> Option<StructLiteralTarget> {
    if lit.node.qualifier.is_none()
        && let Some(alias) = tc.local_type_scopes.visible(lit.node.name, None).cloned()
    {
        let expanded = if lit.node.generic_args.is_empty() {
            tc.resolve_local_alias_target_for_tc_at(&alias, lit.span, lit.node.name)
        } else {
            let ty = Type::UnresolvedNominal {
                qualifier: None,
                name: lit.node.name,
                generic_args: lit.node.generic_args.clone(),
            };
            tc.resolve_type_for_tc_at(&ty, lit.span)
        };
        return struct_literal_target_from_expanded(lit, expanded, tc);
    }

    let Some(binding) = tc.decls.resolve_visible_type_binding(
        &tc.current_module,
        lit.node.qualifier,
        lit.node.name,
    ) else {
        tc.push_error(TypeError::UnknownStructLiteral {
            qualifier: lit.node.qualifier,
            name: lit.node.name,
            span: tc.error_span(lit.span),
        });
        return None;
    };
    match binding {
        TypeBinding::Nominal(key) => Some(StructLiteralTarget { key, seeds: None }),
        TypeBinding::Alias(key) => {
            let expanded = if lit.node.generic_args.is_empty() {
                tc.resolve_module_alias_target_for_tc_at(&key, lit.span, lit.node.name)
            } else {
                let ty = Type::UnresolvedNominal {
                    qualifier: lit.node.qualifier,
                    name: lit.node.name,
                    generic_args: lit.node.generic_args.clone(),
                };
                tc.resolve_type_for_tc_at(&ty, lit.span)
            };
            struct_literal_target_from_expanded(lit, expanded, tc)
        }
        TypeBinding::Contract(_) => {
            tc.resolve_type_for_tc_at(
                &Type::UnresolvedNominal {
                    qualifier: lit.node.qualifier,
                    name: lit.node.name,
                    generic_args: lit.node.generic_args.clone(),
                },
                lit.span,
            );
            None
        }
    }
}

fn struct_literal_target_from_expanded(
    lit: &StructLiteralNode,
    expanded: Type,
    tc: &mut TypeChecker,
) -> Option<StructLiteralTarget> {
    if matches!(expanded, Type::Infer) {
        return None;
    }
    let Some(key) = tc.decls.key_for_type(&expanded) else {
        tc.push_error(TypeError::InvalidStructLiteral {
            name: lit.node.name,
            kind: expanded.to_string(),
            span: tc.error_span(lit.span),
        });
        return None;
    };
    let seeds = tc
        .decls
        .nominal_generics(&key)
        .map(|generics| literal_target_seeds(&generics, &expanded));
    Some(StructLiteralTarget { key, seeds })
}

fn literal_target_seeds(generics: &GenericParams, expanded: &Type) -> GenericSolverSeeds {
    let Some(args) = nominal_generic_args(expanded) else {
        return GenericSolverSeeds::default();
    };
    let mut seeds = GenericSolverSeeds::default();
    for (param, ty) in generics.type_params.iter().zip(args.type_args) {
        if !type_depends_on_generics(&ty) {
            seeds.type_args.insert(param.id, ty);
        }
    }
    for (param, term) in generics.const_params.iter().zip(args.const_args) {
        if !matches!(term, ConstTerm::Param(_)) {
            seeds.const_args.insert(param.id, term);
        }
    }
    seeds
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

fn check_while(while_node: &WhileNode, tc: &mut TypeChecker) {
    let cond = check_expr_checked(&while_node.node.cond, tc);
    check_bool_condition(ConditionKind::While, cond, while_node.node.cond.span, tc);
    check_loop_body(&while_node.node.body, tc);
}

fn check_loop_body(body: &BlockNode, tc: &mut TypeChecker) {
    tc.closure.enter_loop_flow();
    tc.enter_loop();
    check_block_checked(body, tc);
    tc.exit_loop();
    tc.closure.exit_loop_flow();
}

fn check_for(for_node: &ForNode, tc: &mut TypeChecker) {
    let node = &for_node.node;
    let source = check_place(&node.iterable, tc);
    place::record_value_read(node.iterable.node.id, &source.value, tc);
    let iterable_ty = source.value.checked.ty.clone();
    check_for_modifiers(node, &iterable_ty, tc);

    let slots = for_slots(&node.bindings, &iterable_ty, node.iterable.span, tc);
    let roots = node
        .bindings
        .iter()
        .zip(slots)
        .map(|(binding, slot)| for_slot_root(binding, slot, &source, &node.iterable, tc))
        .collect();

    tc.push_scope();
    pattern::check_roots(roots, PatternContext::For, tc);
    check_loop_body(&node.body, tc);
    tc.pop_scope();
}

enum ForSlot {
    Owned(Type),
    Item(Type),
}

fn for_slots(
    bindings: &[ForBinding],
    iterable_ty: &Type,
    iterable_span: Span,
    tc: &mut TypeChecker,
) -> Vec<ForSlot> {
    let range_inner = tc.decls.core_range_inner(iterable_ty).cloned();
    match (bindings, iterable_ty, range_inner) {
        ([_], Type::List { elem } | Type::Array { elem, .. } | Type::Slice { elem }, _) => {
            vec![ForSlot::Item((**elem).clone())]
        }
        ([binding], Type::Map { key, value }, _) => {
            if binding.mutable {
                tc.push_error(TypeError::ForMutableMapEntry {
                    span: tc.error_span(binding.pattern.span),
                });
            }
            vec![ForSlot::Owned(Type::Tuple(vec![
                (**key).clone(),
                (**value).clone(),
            ]))]
        }
        ([_], Type::Infer, _) => infer_for_slots(1),
        ([_], _, Some(inner)) => vec![ForSlot::Owned(inner)],
        ([_], _, None) => unsupported_for_slots(1, iterable_ty, iterable_span, tc),

        ([_, _], Type::List { elem } | Type::Array { elem, .. } | Type::Slice { elem }, _) => {
            vec![ForSlot::Owned(Type::Int), ForSlot::Item((**elem).clone())]
        }
        ([first, _], Type::Map { key, value }, _) => {
            if first.mutable {
                tc.push_error(TypeError::ForMutableMapKey {
                    span: tc.error_span(first.pattern.span),
                });
            }
            vec![
                ForSlot::Owned((**key).clone()),
                ForSlot::Item((**value).clone()),
            ]
        }
        ([_, _], Type::Infer, _) => infer_for_slots(2),
        ([_, _], _, Some(inner)) => vec![ForSlot::Owned(Type::Int), ForSlot::Owned(inner)],
        ([_, _], _, None) => unsupported_for_slots(2, iterable_ty, iterable_span, tc),

        (bindings, _, _) => infer_for_slots(bindings.len()),
    }
}

fn infer_for_slots(count: usize) -> Vec<ForSlot> {
    (0..count).map(|_| ForSlot::Owned(Type::Infer)).collect()
}

fn unsupported_for_slots(
    count: usize,
    iterable_ty: &Type,
    iterable_span: Span,
    tc: &mut TypeChecker,
) -> Vec<ForSlot> {
    tc.push_error(TypeError::ForIterableNotSupported {
        found: iterable_ty.clone(),
        span: tc.error_span(iterable_span),
    });
    infer_for_slots(count)
}

fn for_slot_root<'a>(
    binding: &'a ForBinding,
    slot: ForSlot,
    source: &place::CheckedPlace,
    iterable: &ExprNode,
    tc: &mut TypeChecker,
) -> PatternRoot<'a> {
    match slot {
        ForSlot::Owned(ty) => owned_for_root(binding, ty),
        ForSlot::Item(ty) if binding.mutable => alias_for_root(binding, ty, source, iterable, tc),
        ForSlot::Item(ty) => owned_for_root(binding, ty),
    }
}

fn owned_for_root(binding: &ForBinding, ty: Type) -> PatternRoot<'_> {
    PatternRoot {
        pattern: &binding.pattern,
        input: PatternRootInput::Owned(ty),
        mode: PatternBindMode::Owned {
            mutable: binding.mutable,
        },
    }
}

fn alias_for_root<'a>(
    binding: &'a ForBinding,
    ty: Type,
    source: &place::CheckedPlace,
    iterable: &ExprNode,
    tc: &mut TypeChecker,
) -> PatternRoot<'a> {
    let access = place::projected_field_access(source.value.access);
    let access = if access.can_assign() {
        access
    } else {
        tc.push_error(TypeError::ForVarRequiresMutableIterable {
            span: tc.error_span(iterable.span),
        });
        PlaceAccess::Mutable
    };
    let place = pattern::PatternPlace {
        expected_handle: tc.type_handle(&ty),
        expected_ty: ty,
        access,
        facts: source.value.facts.clone(),
        identity: source.value.identity.clone().index(),
        accepts_extern_any: source.accepts_extern_any(),
    };
    PatternRoot {
        pattern: &binding.pattern,
        input: PatternRootInput::Place(Box::new(place), iterable.node.id),
        mode: PatternBindMode::Alias,
    }
}

fn check_for_modifiers(node: &For, iterable_ty: &Type, tc: &mut TypeChecker) {
    let range_kind = tc.decls.core_range_kind(iterable_ty);

    if node.reversed {
        if matches!(iterable_ty, Type::Map { .. }) {
            push_for_modifier_error(
                tc,
                "rev is not supported for map iteration",
                node.iterable.span,
            );
        } else if matches!(
            range_kind,
            Some(CoreRangeKind::From | CoreRangeKind::To | CoreRangeKind::ToInclusive)
        ) {
            push_for_modifier_error(
                tc,
                "reverse is not supported for open-ended ranges",
                node.iterable.span,
            );
        }
    }

    if let Some(step) = &node.step {
        if matches!(iterable_ty, Type::Map { .. }) {
            push_for_modifier_error(tc, "step is not supported for map iteration", step.span);
            check_expr_checked(step, tc);
        } else {
            let step_checked = check_expr_checked(step, tc);
            let step_is_int = matches!(step_checked.ty, Type::Int | Type::Infer);
            let range_is_int = matches!(
                tc.decls.core_range_inner(iterable_ty),
                Some(Type::Int | Type::Infer)
            );
            if range_kind.is_some() && (!range_is_int || !step_is_int) {
                push_for_modifier_error(tc, "step is only supported for integer ranges", step.span);
            }
            let int = tc.type_handle(&Type::Int);
            tc.expect_assignable(step.span, step_checked.handle, int);
        }
    }
}

fn push_for_modifier_error(tc: &mut TypeChecker, message: &'static str, span: Span) {
    tc.push_error(TypeError::ForIterationModifier {
        message,
        span: tc.error_span(span),
    });
}

fn check_break(span: Span, tc: &mut TypeChecker) {
    if tc.in_defer() {
        tc.push_error(TypeError::BreakInsideDefer {
            span: tc.error_span(span),
        });
    } else if !tc.in_loop() {
        tc.push_error(TypeError::BreakOutsideLoop {
            span: tc.error_span(span),
        });
    }
}

fn check_continue(span: Span, tc: &mut TypeChecker) {
    if tc.in_defer() {
        tc.push_error(TypeError::ContinueInsideDefer {
            span: tc.error_span(span),
        });
    } else if !tc.in_loop() {
        tc.push_error(TypeError::ContinueOutsideLoop {
            span: tc.error_span(span),
        });
    }
}

fn check_let_else(let_else_node: &LetElseNode, tc: &mut TypeChecker) {
    let node = &let_else_node.node;
    let mode = mode_for_head(node.head);
    let value = check_pattern_scrutinee(&node.value, mode, tc);
    tc.push_scope();
    check_block_checked(&node.else_block, tc);
    tc.pop_scope();
    if !control_flow::block_diverges(&node.else_block) {
        tc.push_error(TypeError::LetElseMustDiverge {
            span: tc.error_span(node.else_block.span),
        });
    }
    pattern::check_place_at(
        &node.pattern,
        value.pattern_place(value.checked.handle.clone(), value.checked.ty.clone()),
        mode,
        node.value.node.id,
        PatternContext::LetElse,
        tc,
    );
}

fn check_while_let(while_let_node: &WhileLetNode, tc: &mut TypeChecker) {
    let node = &while_let_node.node;
    let mode = mode_for_head(node.head);
    let value = check_pattern_scrutinee(&node.value, mode, tc);
    tc.push_scope();
    pattern::check_place_at(
        &node.pattern,
        value.pattern_place(value.checked.handle.clone(), value.checked.ty.clone()),
        mode,
        node.value.node.id,
        PatternContext::WhileLet,
        tc,
    );
    check_loop_body(&node.body, tc);
    tc.pop_scope();
}

fn check_if_let_exact_downcast(
    if_let_node: &IfLetNode,
    downcast: &ExactDowncastNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let node = &if_let_node.node;
    let binding = exact_downcast_binding(node, tc);
    let target = tc.resolve_downcast_target_type_at(&downcast.node.target, downcast.span);
    let target = runtime_downcast_target(tc, target, downcast.span);
    let source = check_place(&downcast.node.expr, tc);
    let source_contract = match &source.checked().ty {
        Type::Dyn(contract) => Some(contract.clone()),
        Type::Infer => None,
        _ => {
            tc.push_error(TypeError::CompileError {
                message: "exact downcast source must be a dynamic value".to_string(),
                span: tc.error_span(downcast.node.expr.span),
            });
            None
        }
    };
    let mut source_valid = source_contract.is_some();

    match binding {
        Some(binding) if binding.mutable => {
            if let Some(error) = source
                .value
                .access
                .mut_borrow_error(binding.name, tc.error_span(downcast.node.expr.span))
            {
                tc.push_error(error);
                source_valid = false;
            }
        }
        Some(_) if matches!(source.value.access, PlaceAccess::NotPlace) => {
            tc.push_error(TypeError::CompileError {
                message: "exact downcast source must be a dynamic place".to_string(),
                span: tc.error_span(downcast.node.expr.span),
            });
            source_valid = false;
        }
        _ => {}
    }

    let binding_ty = target.clone().unwrap_or(Type::Infer);
    checked_from_type(&node.value, binding_ty.clone(), tc);
    let Some(binding) = binding else {
        return check_downcast_branches(node, None, binding_ty, expected, tc);
    };
    let Some(target) = target else {
        return check_downcast_branches(node, Some(binding), binding_ty, expected, tc);
    };
    if !source_valid {
        return check_downcast_branches(node, Some(binding), binding_ty, expected, tc);
    }
    let source_contract = source_contract.expect("valid downcast source has contract");

    if let Some(source) =
        contracts::contract_set_key_for_ref(&tc.decls, &tc.current_module, &source_contract)
    {
        tc.record_dyn_downcast(DynDowncastFact {
            expr_id: node.value.node.id,
            source_id: downcast.node.expr.node.id,
            source,
            target: target.clone(),
            mutable: binding.mutable,
            span: tc.source_span(node.value.span),
        });
    } else if let Some(hole) = dyn_infer::hole_id(&source_contract) {
        tc.dyn_infer.add_downcast(
            tc.current_module.clone(),
            node.value.node.id,
            downcast.node.expr.node.id,
            hole,
            target.clone(),
            binding.mutable,
            tc.source_span(node.value.span),
        );
    }

    let then_expected = expected.clone();
    check_if_let_branches(node, expected, tc, |tc| {
        tc.push_scope();
        let handle = tc.type_handle(&target);
        if binding.mutable {
            let alias = place::AliasTarget {
                access: PlaceAccess::Mutable,
                identity: source.value.identity.clone(),
                facts: source.value.facts.clone(),
                accepts_extern_any: source.accepts_extern_any(),
            };
            tc.define_downcast_alias_from_handle(binding.name, &handle, alias);
            tc.active_mut_downcast_roots.push(ActiveMutDowncastRoot {
                identity: source.value.identity.clone(),
                allowed: binding.name,
            });
        } else {
            tc.define_pattern_binding_from_handle(binding.name, &handle, false);
        }

        let then = check_block_checked_with_hint(&node.then_block, then_expected, tc);
        if binding.mutable {
            tc.active_mut_downcast_roots.pop();
        }
        tc.pop_scope();
        then
    })
}

#[derive(Clone, Copy)]
struct ExactDowncastBinding {
    name: Ident,
    mutable: bool,
}

fn exact_downcast_binding(node: &IfLet, tc: &mut TypeChecker) -> Option<ExactDowncastBinding> {
    match node.pattern.node {
        Pattern::Ident(name) => Some(ExactDowncastBinding {
            name,
            mutable: matches!(node.head, PatternHead::Var),
        }),
        _ => {
            tc.push_error(TypeError::CompileError {
                message: "exact downcast currently binds a single identifier".to_string(),
                span: tc.error_span(node.pattern.span),
            });
            None
        }
    }
}

fn runtime_downcast_target(tc: &mut TypeChecker, target: Type, span: Span) -> Option<Type> {
    match &target {
        Type::Dyn(_) => {
            tc.push_error(TypeError::CompileError {
                message: "downcast tests the stored concrete type; use a wider dynamic type at the conversion site instead of downcasting to another contract".to_string(),
                span: tc.error_span(span),
            });
            return None;
        }
        Type::Infer => return None,
        _ => {}
    }
    let facts = type_closure_facts(&target);
    if facts.first_unresolved.is_some()
        || facts.infer.contains_type
        || facts.infer.contains_return
        || facts.contains_unresolved_const
        || type_depends_on_generics(&target)
    {
        tc.push_error(TypeError::CompileError {
            message: "exact downcast target must be a fully concrete runtime-identifiable type"
                .to_string(),
            span: tc.error_span(span),
        });
        return None;
    }
    if tc.decls.key_for_type(&target).is_some() {
        Some(target)
    } else {
        tc.push_error(TypeError::CompileError {
            message: "exact downcast target must be a concrete nominal type".to_string(),
            span: tc.error_span(span),
        });
        None
    }
}

fn check_downcast_branches(
    node: &IfLet,
    binding: Option<ExactDowncastBinding>,
    binding_ty: Type,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let then_expected = expected.clone();
    check_if_let_branches(node, expected, tc, |tc| {
        tc.push_scope();
        if let Some(binding) = binding {
            let handle = tc.type_handle(&binding_ty);
            tc.define_pattern_binding_from_handle(binding.name, &handle, binding.mutable);
        }
        let then = check_block_checked_with_hint(&node.then_block, then_expected, tc);
        tc.pop_scope();
        then
    })
}

fn check_if_let_branches(
    node: &IfLet,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
    then: impl FnOnce(&mut TypeChecker) -> CheckedType,
) -> CheckedType {
    let Some(else_block) = &node.else_block else {
        check_closure_flow_branch(tc, then);
        return checked_void(tc);
    };
    let (then, else_checked) = check_closure_flow_branches(tc, then, |tc| {
        check_block_checked_with_hint(else_block, expected, tc)
    });
    join_checked(
        then,
        node.then_block.span,
        else_checked,
        else_block.span,
        tc,
    )
}

fn check_if_let_checked_with_hint(
    if_let_node: &IfLetNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let node = &if_let_node.node;
    if let ExprKind::ExactDowncast(downcast) = &node.value.node.kind {
        return check_if_let_exact_downcast(if_let_node, downcast, expected, tc);
    }

    let mode = mode_for_head(node.head);
    let value = check_pattern_scrutinee(&node.value, mode, tc);
    let then_expected = expected.clone();
    check_if_let_branches(node, expected, tc, |tc| {
        tc.push_scope();
        pattern::check_place_at(
            &node.pattern,
            value.pattern_place(value.checked.handle.clone(), value.checked.ty.clone()),
            mode,
            node.value.node.id,
            PatternContext::IfLet,
            tc,
        );
        let then = check_block_checked_with_hint(&node.then_block, then_expected, tc);
        tc.pop_scope();
        then
    })
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
