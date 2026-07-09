use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

pub(crate) use self::{
    annotation::DeprecatedUseKind,
    const_term::ConstTerm,
    decls::*,
    generic::*,
    infer::SemanticLocalId,
    result::*,
    semantic_use::*,
    surface::*,
    type_ops::{type_closure_facts, type_has_unfinished_facts},
    type_refs::{
        GenericTypeContext, TypeRefError, TypeRefResolver, extend_generic_context_with_params,
        generic_context_from_params,
    },
};
use self::{
    body::{
        CallableBody, CallableTemplate, CallableTemplateEnv, check_block_checked,
        check_block_checked_with_hint, check_module_bodies, check_specialized_callable_body,
        check_stmts, collect_callable_templates, register_declarations,
    },
    closure::{ClosureClassifier, ClosureScopeState, FunctionFlowMode},
    decl_validate::{
        check_finite_size_cycles, check_infer_return_decls, generic_param_type_error,
        method_sig_is_generic,
    },
    defaults::{check_decl_param_defaults, check_decl_param_order},
    dyn_infer::DynInference,
    infer::{Solver, SolverFinalizeError, SolverRelationError, SourceExprTypes, TypeHandle},
    literal::{
        check_array_fill_hint, check_array_lit_hint, check_inferred_enum_hint, check_map_lit_hint,
        check_range_expr, check_string_interp, check_struct_lit_hint,
        check_tuple_checked_with_hint, type_from_lit,
    },
    pattern::{PatternBindMode, PatternContext, PatternRoot, PatternRootInput},
    place::{AliasAltGroupId, PlaceAccess, PlaceIdentity, PlaceRoot, PlaceUseFacts, check_place},
    postfix::{PostfixStep, check_map_key, check_postfix_chain, collect_postfix_chain},
    type_ops::{contains_borrowed_slice_view, type_contains_dyn_value},
    type_refs::LocalTypeScopes,
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
mod collection_loan;
mod const_eval;
mod const_term;
mod contracts;
mod control_flow;
mod convert;
mod decl_validate;
mod decls;
mod defaults;
mod downcast;
mod dyn_infer;
mod enum_variant;
mod extend_target;
mod extern_boundary;
mod extern_ops;
mod field_check;
mod generic;
mod generic_bind;
mod globals;
mod infer;
mod iterator_plan;
mod literal;
mod match_check;
mod match_coverage;
mod member;
mod pattern;
mod place;
mod postfix;
mod projection;
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

pub(crate) enum ResolvedNominal<'a> {
    Aggregate(&'a AggregateSchema),
    Enum {
        key: NominalKey,
        schema: &'a EnumSchema,
    },
    Extern {
        id: ExternTypeId,
        ty: &'a ExternType,
    },
}

impl<'a> ResolvedNominal<'a> {
    pub(crate) fn key(&self) -> &NominalKey {
        match self {
            Self::Aggregate(schema) => &schema.key,
            Self::Enum { key, .. } => key,
            Self::Extern { ty, .. } => &ty.nominal,
        }
    }

    pub(crate) fn surface_ty(&self) -> Type {
        nominal_type(self.key())
    }

    pub(crate) fn variants(&self) -> Option<&'a NamedSchemas<VariantSchema>> {
        match self {
            Self::Enum { schema, .. } => Some(&schema.variants),
            Self::Aggregate(_) | Self::Extern { .. } => None,
        }
    }
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
    IteratorPlanAsValue {
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
    RefArgNonLvalue {
        span: Option<SourceSpan>,
    },
    RefArgImmutableBinding {
        name: Ident,
        span: Option<SourceSpan>,
    },
    SequenceStructuralMutationDuringLoan {
        span: Option<SourceSpan>,
    },
    MapStructuralMutationDuringLoan {
        span: Option<SourceSpan>,
    },
    ActiveCollectionRebind {
        span: Option<SourceSpan>,
    },
    ActiveCollectionMutableArg {
        span: Option<SourceSpan>,
    },
    StoredSliceLocal {
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
    UnknownFunctionValueEscapes {
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
    RefPatternRequiresMutablePlace {
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
    MatchArmTypeMismatch {
        expected: Type,
        found: Type,
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
    ForRefRequiresMutableIterable {
        span: Option<SourceSpan>,
    },
    ForMutableMapKey {
        span: Option<SourceSpan>,
    },
    ForMutableMapEntry {
        span: Option<SourceSpan>,
    },
    RefutableForPattern {
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
    UnsupportedOptionalPayloadPattern {
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
    AmbiguousProjection {
        source: Type,
        target: Type,
        paths: Vec<Vec<Ident>>,
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
    ArrayIndexOutOfBounds {
        index: i64,
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
    InvalidDefaultExpression {
        kind: &'static str,
        span: Option<SourceSpan>,
    },
    DefaultReferencesParameter {
        name: Ident,
        span: Option<SourceSpan>,
    },
    DefaultReferencesSelf {
        span: Option<SourceSpan>,
    },
    DefaultReferencesField {
        name: Ident,
        span: Option<SourceSpan>,
    },
    RefParamDefault {
        name: Ident,
        span: Option<SourceSpan>,
    },
    ConstTypeMismatch {
        expected: Type,
        found: Type,
        span: Option<SourceSpan>,
    },
    RawEnumExpectedIntValue {
        found: Type,
        span: Option<SourceSpan>,
    },
    RawEnumExpectedStringValue {
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
    AmbiguousCast {
        from: Type,
        to: Type,
        span: Option<SourceSpan>,
    },
    RawEnumWrongRawCast {
        enum_ty: Type,
        expected: Type,
        found: Type,
        span: Option<SourceSpan>,
    },
    NonRawEnumRawCast {
        enum_ty: Type,
        raw_ty: Type,
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
impl From<Box<TypeError>> for TypeError {
    fn from(error: Box<TypeError>) -> Self {
        *error
    }
}

impl TypeError {
    pub(crate) fn span(&self) -> Option<SourceSpan> {
        match self {
            TypeError::Decl(error) => error.span(),
            TypeError::ExternCatalog(error) => error.span(),
            TypeError::UndefinedVariable { span, .. }
            | TypeError::TypeMismatch { span, .. }
            | TypeError::ConstMismatch { span, .. }
            | TypeError::RecursiveInference { span, .. }
            | TypeError::CannotInferType { span, .. }
            | TypeError::CannotInferEnum { span, .. }
            | TypeError::NamedFunctionCapture { span, .. }
            | TypeError::AllNilArrayLiteral { span, .. }
            | TypeError::ArrayFillLengthNotConst { span, .. }
            | TypeError::InferReturnNonGeneric { span, .. }
            | TypeError::InferReturnExtern { span, .. }
            | TypeError::InferReturnValue { span, .. }
            | TypeError::InferReturnMismatch { span, .. }
            | TypeError::InferReturnRecursive { span, .. }
            | TypeError::UnsupportedPlaceReturn { span, .. }
            | TypeError::UnknownType { span, .. }
            | TypeError::TypeUsedAsValue { span, .. }
            | TypeError::IteratorPlanAsValue { span, .. }
            | TypeError::CannotInferConst { span, .. }
            | TypeError::NotCallable { span, .. }
            | TypeError::WrongArgCount { span, .. }
            | TypeError::WrongArgRange { span, .. }
            | TypeError::LambdaParamCountMismatch { span, .. }
            | TypeError::RequiredParamAfterDefault { span, .. }
            | TypeError::EnumVariantArgCount { span, .. }
            | TypeError::DuplicateName { span, .. }
            | TypeError::ImmutableAssignment { span, .. }
            | TypeError::ConstAssignment { span, .. }
            | TypeError::RefArgNonLvalue { span, .. }
            | TypeError::RefArgImmutableBinding { span, .. }
            | TypeError::SequenceStructuralMutationDuringLoan { span, .. }
            | TypeError::MapStructuralMutationDuringLoan { span, .. }
            | TypeError::ActiveCollectionRebind { span, .. }
            | TypeError::ActiveCollectionMutableArg { span, .. }
            | TypeError::StoredSliceLocal { span, .. }
            | TypeError::MutatingMethodImmutableReceiver { span, .. }
            | TypeError::MutableAlias { span, .. }
            | TypeError::InvalidFormatSpec { span, .. }
            | TypeError::NonEscapingCallbackEscapes { span, .. }
            | TypeError::UnknownFunctionValueEscapes { span }
            | TypeError::BorrowedCaptureEscapes { span, .. }
            | TypeError::RequiresMutablePlace { span, .. }
            | TypeError::RefPatternRequiresMutablePlace { span, .. }
            | TypeError::ForRefRequiresMutableIterable { span, .. }
            | TypeError::ForMutableMapKey { span, .. }
            | TypeError::ForMutableMapEntry { span, .. }
            | TypeError::RefutableForPattern { span, .. }
            | TypeError::InvalidOperand { span, .. }
            | TypeError::MissingReturn { span, .. }
            | TypeError::MatchArmTypeMismatch { span, .. }
            | TypeError::IfWithoutElseValue { span, .. }
            | TypeError::IfConditionNotBool { span, .. }
            | TypeError::TernaryConditionNotBool { span, .. }
            | TypeError::WhileConditionNotBool { span, .. }
            | TypeError::BreakOutsideLoop { span, .. }
            | TypeError::ContinueOutsideLoop { span, .. }
            | TypeError::ReturnInsideDefer { span, .. }
            | TypeError::BreakInsideDefer { span, .. }
            | TypeError::ContinueInsideDefer { span, .. }
            | TypeError::TryOnInvalidCarrier { span, .. }
            | TypeError::TryOutsideCarrierFunction { span, .. }
            | TypeError::TryResultErrorMismatch { span, .. }
            | TypeError::TryInsideDefer { span, .. }
            | TypeError::ForIterableNotSupported { span, .. }
            | TypeError::ForIterationModifier { span, .. }
            | TypeError::InfiniteSize { span, .. }
            | TypeError::NotEquatable { span, .. }
            | TypeError::UnsupportedPattern { span, .. }
            | TypeError::TuplePatternArityMismatch { span, .. }
            | TypeError::TuplePatternOnNonTuple { span, .. }
            | TypeError::OrPatternBindingMismatch { span, .. }
            | TypeError::OrPatternBindingTypeMismatch { span, .. }
            | TypeError::EmptyMatch { span, .. }
            | TypeError::NonExhaustiveMatch { span, .. }
            | TypeError::UnsupportedMatchScrutinee { span, .. }
            | TypeError::InvalidLiteralPattern { span, .. }
            | TypeError::OptionalPatternOnNonOptional { span, .. }
            | TypeError::OptionalChainingOnNonOptional { span, .. }
            | TypeError::NestedOptionalPattern { span, .. }
            | TypeError::UnsupportedOptionalPayloadPattern { span, .. }
            | TypeError::RequiresUnwrappingPattern { span, .. }
            | TypeError::IrrefutableLetElse { span, .. }
            | TypeError::LetElseMustDiverge { span, .. }
            | TypeError::MemberAccessOnNonAggregate { span, .. }
            | TypeError::UnknownMember { span, .. }
            | TypeError::AmbiguousPromotedField { span, .. }
            | TypeError::AmbiguousPromotedMethod { span, .. }
            | TypeError::PromotedFieldNotStored { span, .. }
            | TypeError::AmbiguousProjection { span, .. }
            | TypeError::MissingProjection { span, .. }
            | TypeError::InstanceMethodOnType { span, .. }
            | TypeError::StaticMethodOnValue { span, .. }
            | TypeError::ReadonlyMethodMutation { span, .. }
            | TypeError::UnknownIntrinsic { span, .. }
            | TypeError::IntrinsicArgCount { span, .. }
            | TypeError::IntrinsicExpectedIdent { span, .. }
            | TypeError::IntrinsicExpectedString { span, .. }
            | TypeError::UnknownIntrinsicValue { span, .. }
            | TypeError::CompileError { span, .. }
            | TypeError::MethodGenericShadow { span, .. }
            | TypeError::TupleIndexOnNonTuple { span, .. }
            | TypeError::TupleIndexOutOfBounds { span, .. }
            | TypeError::ArrayIndexOutOfBounds { span, .. }
            | TypeError::IndexNotInt { span, .. }
            | TypeError::IndexOnNonIndexable { span, .. }
            | TypeError::RangeIndexNotInt { span, .. }
            | TypeError::RangeIndexUnsupported { span, .. }
            | TypeError::NonKeyableMapKey { span, .. }
            | TypeError::DuplicateMapKey { span, .. }
            | TypeError::UndefinedModuleMember { span, .. }
            | TypeError::PrivateModuleMember { span, .. }
            | TypeError::AmbiguousExtendMethod { span, .. }
            | TypeError::DuplicateField { span, .. }
            | TypeError::MissingField { span, .. }
            | TypeError::UnknownVariantField { span, .. }
            | TypeError::MissingVariantField { span, .. }
            | TypeError::InvalidStructLiteral { span, .. }
            | TypeError::UnknownStructLiteral { span, .. }
            | TypeError::UnknownEnumVariant { span, .. }
            | TypeError::EnumPatternTypeMismatch { span, .. }
            | TypeError::EnumVariantShapeMismatch { span, .. }
            | TypeError::UnboundGenericParam { span, .. }
            | TypeError::UnknownConst { span, .. }
            | TypeError::RuntimeGlobalInConstPosition { span, .. }
            | TypeError::ConstCycle { span, .. }
            | TypeError::NonConstExpression { span, .. }
            | TypeError::InvalidDefaultExpression { span, .. }
            | TypeError::DefaultReferencesParameter { span, .. }
            | TypeError::DefaultReferencesSelf { span, .. }
            | TypeError::DefaultReferencesField { span, .. }
            | TypeError::RefParamDefault { span, .. }
            | TypeError::ConstTypeMismatch { span, .. }
            | TypeError::RawEnumExpectedIntValue { span, .. }
            | TypeError::RawEnumExpectedStringValue { span, .. }
            | TypeError::InvalidConstCast { span, .. }
            | TypeError::InvalidCast { span, .. }
            | TypeError::AmbiguousCast { span, .. }
            | TypeError::RawEnumWrongRawCast { span, .. }
            | TypeError::NonRawEnumRawCast { span, .. }
            | TypeError::ConstDivisionByZero { span, .. }
            | TypeError::ConstOverflow { span, .. }
            | TypeError::ExpectedIntConst { span, .. }
            | TypeError::NegativeArrayLength { span, .. }
            | TypeError::GenericArgKindMismatch { span, .. }
            | TypeError::ExternAnyEscape { span, .. }
            | TypeError::AnyOutsideExternBoundary { span, .. }
            | TypeError::ContractUnsatisfied { span, .. }
            | TypeError::DynamicMethodMissing { span, .. }
            | TypeError::BorrowedDynReassign { span, .. }
            | TypeError::DynContainerConversion { span, .. }
            | TypeError::DuplicateGenericParam { span, .. } => *span,
            TypeError::GenericArity(_) => None,
        }
    }
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
            storage: CaptureStorageOrigin::RefSelf,
        }
    }

    fn pattern_alias(context: PatternContext) -> Self {
        let storage = match context {
            PatternContext::For => CaptureStorageOrigin::ForRefAlias,
            _ => CaptureStorageOrigin::PatternAlias,
        };
        Self {
            mutability: BindingMutability::Mutable,
            storage,
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
        } else if mutable || contains_borrowed_slice_view(ty) {
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
        self.storage.requires_runtime_capture()
    }

    fn is_const(self) -> bool {
        matches!(self.storage, CaptureStorageOrigin::Const)
    }

    fn is_air_local(self) -> bool {
        self.storage.is_air_local()
    }

    fn place_access(self) -> PlaceAccess {
        match self.storage {
            CaptureStorageOrigin::Owned
            | CaptureStorageOrigin::BorrowedParam
            | CaptureStorageOrigin::RefSelf => self.mutability.place_access(),
            CaptureStorageOrigin::DynView => PlaceAccess::DynView,
            CaptureStorageOrigin::Const => PlaceAccess::Const,
            CaptureStorageOrigin::PatternAlias | CaptureStorageOrigin::ForRefAlias => {
                PlaceAccess::Mutable
            }
            CaptureStorageOrigin::ReadonlySelf => PlaceAccess::ReadonlySelf,
        }
    }
}

#[derive(Clone)]
struct VarInfo {
    binding_id: BindingId,
    type_id: SemanticLocalId,
    kind: LocalBindingKind,
    const_value: Option<ConstValue>,
    local_const: Option<LocalConstId>,
    alias: Option<Box<place::AliasTarget>>,
}

#[derive(Clone)]
struct LocalCallableInfo {
    binding_id: BindingId,
    type_id: SemanticLocalId,
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

#[derive(Clone, Copy)]
struct DefinedLocal {
    binding_id: BindingId,
    type_id: SemanticLocalId,
}

#[derive(Clone, Copy)]
enum NameSubjectMode {
    Value,
    Place,
    PostfixBase,
    Const,
}

enum ResolvedIdentSubject {
    Local(LocalSymbol, usize),
    Blocked(Box<TypeError>),
    Named(ModuleScope, Ident, Box<ValueDecl>),
    Module(ModuleScope),
    Type(Type),
    Missing,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct LocalConstId(u32);

#[derive(Clone, Copy)]
struct LocalConstInfo {
    binding_id: BindingId,
    type_id: SemanticLocalId,
    id: LocalConstId,
}

impl VarInfo {
    fn new(binding_id: BindingId, type_id: SemanticLocalId, kind: LocalBindingKind) -> Self {
        Self {
            binding_id,
            type_id,
            kind,
            const_value: None,
            local_const: None,
            alias: None,
        }
    }

    fn with_const_value(mut self, const_value: Option<ConstValue>) -> Self {
        self.const_value = const_value;
        self
    }

    fn with_local_const(mut self, id: LocalConstId) -> Self {
        self.local_const = Some(id);
        self
    }

    fn with_alias(mut self, alias: Option<place::AliasTarget>) -> Self {
        self.alias = alias.map(Box::new);
        self
    }
}

impl LocalCallableInfo {
    fn new(binding_id: BindingId, type_id: SemanticLocalId, callee: CallableRef) -> Self {
        Self {
            binding_id,
            type_id,
            callee,
        }
    }
}

impl LocalConstInfo {
    fn symbol(self) -> LocalSymbol {
        LocalSymbol::value(
            VarInfo::new(self.binding_id, self.type_id, LocalBindingKind::constant())
                .with_local_const(self.id),
        )
    }
}

struct LocalValue {
    info: VarInfo,
    source_depth: usize,
}

#[derive(Clone)]
struct SourceModuleFactsInput {
    scope: ModuleScope,
    source: SourceId,
    program: Rc<Program>,
}

struct SemanticMethodLikeInput<'a> {
    name: Ident,
    receiver: Option<MethodReceiver>,
    source_params: &'a [Param],
    body_span: Span,
    span: Span,
    callable: &'a CallableRef,
    args: GenericArgs,
    param_types: Vec<FuncParam>,
    ret: ReturnSpec,
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
        } else if self.callee.def.sig.ret.is_iter() {
            Some(TypeError::IteratorPlanAsValue { span })
        } else {
            None
        }
    }

    fn value_view(&self) -> VarInfo {
        VarInfo::new(self.binding_id, self.type_id, LocalBindingKind::immutable())
    }
}

impl LocalSymbol {
    fn value(info: VarInfo) -> Self {
        Self::Value(info)
    }

    fn callable(info: LocalCallableInfo) -> Self {
        Self::Callable(Box::new(info))
    }

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
        candidates: Vec<ReturnCandidate>,
    },
}

#[derive(Clone)]
enum ReturnCandidate {
    Value { span: Span, handle: TypeHandle },
    Iter { span: Span },
}

struct ReturnFrame {
    mode: ReturnMode,
}

#[derive(Clone)]
struct ScopeState {
    scopes: Vec<HashMap<Ident, LocalSymbol>>,
    local_type_scopes: LocalTypeScopes,
    closure: ClosureScopeState,
    active_collection_loans: Vec<collection_loan::ActiveCollectionLoan>,
}

const MUT_ALIAS_ROOT_MESSAGE: &str = "place cannot be used while a mutable alias binding is live";

#[derive(Clone)]
struct ActiveMutAliasRoot {
    identity: PlaceIdentity,
    allowed: Ident,
    scope_depth: usize,
    message: &'static str,
}

struct TypeChecker {
    solver: Solver,
    semantic_facts: SemanticFactMaps,
    expr_places: HashMap<ExprId, place::PlaceValue>,
    closure: ClosureClassifier,
    global_types: HashMap<GlobalKey, SemanticLocalId>,
    active_mut_alias_roots: Vec<ActiveMutAliasRoot>,
    active_collection_loans: Vec<collection_loan::ActiveCollectionLoan>,
    dyn_infer_registered_modules: HashSet<ModuleScope>,
    dyn_infer: DynInference,
    used_imports: HashSet<ImportId>,
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
    deferred_expected_return_depth: usize,
    errors: Vec<TypeError>,
    warnings: Vec<CompileWarning>,
    lint_events: Vec<LintEvent>,
    config: TypecheckConfig,
    current_module: ModuleScope,
    module_sources: HashMap<ModuleScope, SourceId>,
    module_programs: HashMap<ModuleScope, Rc<Program>>,
    source_modules: Vec<SourceModuleFactsInput>,
    type_substs: Vec<TypeSubst>,
    const_substs: Vec<ConstSubst>,
    generic_contexts: Vec<GenericTypeContext>,
    generic_owner_frames: Vec<GenericOwnerFrame>,
    active_bodies: Vec<BodyInstanceKey>,
    iter_return_sigs: HashMap<BodyInstanceKey, Type>,
    local_def_bodies: HashMap<SemanticLocalId, BodyInstanceKey>,
    local_callables: HashMap<CallableId, LocalCallableInfo>,
    callable_templates: HashMap<CallableId, CallableTemplate>,
    specializations: HashMap<CallableInstanceKey, SpecializationState>,
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
            semantic_facts: SemanticFactMaps::default(),
            expr_places: HashMap::new(),
            closure: ClosureClassifier::default(),
            global_types: HashMap::new(),
            active_mut_alias_roots: vec![],
            active_collection_loans: vec![],
            dyn_infer_registered_modules: HashSet::new(),
            dyn_infer: DynInference::default(),
            used_imports: HashSet::new(),
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
            deferred_expected_return_depth: 0,
            errors: vec![],
            warnings: vec![],
            lint_events: vec![],
            config,
            current_module: ModuleScope::Root,
            module_sources: HashMap::new(),
            module_programs: HashMap::new(),
            source_modules: vec![],
            type_substs: vec![],
            const_substs: vec![],
            generic_contexts: vec![],
            generic_owner_frames: vec![],
            active_bodies: vec![],
            iter_return_sigs: HashMap::new(),
            local_def_bodies: HashMap::new(),
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

    fn current_body(&self) -> BodyInstanceKey {
        self.active_bodies
            .last()
            .cloned()
            .unwrap_or_else(|| BodyInstanceKey::Module(self.current_module.clone()))
    }

    fn record_current_iter_return_sig(&mut self, ty: Type) {
        self.iter_return_sigs.insert(self.current_body(), ty);
    }

    fn iter_return_sig(&self, body: &BodyInstanceKey) -> Option<&Type> {
        self.iter_return_sigs.get(body)
    }

    fn with_deferred_expected_returns<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        self.deferred_expected_return_depth += 1;
        let ret = f(self);
        self.deferred_expected_return_depth = self
            .deferred_expected_return_depth
            .checked_sub(1)
            .expect("deferred expected return depth underflow");
        ret
    }

    fn expected_returns_deferred(&self) -> bool {
        self.deferred_expected_return_depth > 0
    }

    fn probe_compatibility_without_effects<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        let error_count = self.errors.len();
        let warning_count = self.warnings.len();
        let lint_count = self.lint_events.len();
        let used_imports = self.used_imports.clone();
        let promoted_surfaces = self.promoted_surfaces.clone();

        let ret = f(self);

        self.errors.truncate(error_count);
        self.warnings.truncate(warning_count);
        self.lint_events.truncate(lint_count);
        self.used_imports = used_imports;
        self.promoted_surfaces = promoted_surfaces;
        ret
    }

    fn with_body_instance<R>(&mut self, key: BodyInstanceKey, f: impl FnOnce(&mut Self) -> R) -> R {
        let depth = self.active_bodies.len();
        self.active_bodies.push(key);
        let ret = f(self);
        let popped = self.active_bodies.pop();
        debug_assert!(popped.is_some());
        debug_assert_eq!(self.active_bodies.len(), depth);
        ret
    }

    fn current_expr_site(&self, expr: ExprId) -> SemanticExprSite {
        SemanticExprSite {
            body: self.current_body(),
            expr,
        }
    }

    fn can_record_local_facts(&self) -> bool {
        let body = self.current_body();
        match body {
            BodyInstanceKey::Lambda(_) | BodyInstanceKey::Global(_) => true,
            BodyInstanceKey::Callable(key) => {
                key.target.parent.is_none() && matches!(key.target.kind, CallableKind::Function)
                    || matches!(
                        (&key.target.parent, key.target.kind),
                        (
                            Some(CallableParent::Nominal(_)),
                            CallableKind::InstanceMethod | CallableKind::StaticMethod,
                        ) | (
                            Some(CallableParent::Extend(_)),
                            CallableKind::ExtendMethod(_),
                        )
                    )
            }
            BodyInstanceKey::Module(_) | BodyInstanceKey::CastFrom(_) => false,
        }
    }

    fn record_local_def(
        &mut self,
        id: SemanticLocalId,
        binding_id: Option<BindingId>,
        name: Ident,
        span: Option<Span>,
        mutable: bool,
        kind: LocalDefKind,
    ) {
        if !self.can_record_local_facts() {
            return;
        }
        let body = self.current_body();
        let span = span.map(|span| self.source_span(span));
        self.semantic_facts.record_local_def(
            body.clone(),
            LocalDefFact {
                id,
                binding_id,
                name,
                span,
                ty: Type::Infer,
                mutable,
                kind,
            },
        );
        self.local_def_bodies.insert(id, body);
    }

    fn record_binding_def(&mut self, span: Span, local: SemanticLocalId) {
        if !self.can_record_local_facts() {
            return;
        }
        let body = self.current_body();
        self.semantic_facts
            .record_binding_def(body, self.source_span(span), local);
    }

    fn record_param_def(&mut self, index: usize, local: SemanticLocalId) {
        if !self.can_record_local_facts() {
            return;
        }
        self.semantic_facts
            .record_param_def(self.current_body(), index, local);
    }

    fn record_local_use(&mut self, expr_id: ExprId, local: SemanticLocalId, mode: LocalUseMode) {
        if !self.can_record_local_facts() {
            return;
        }
        let body = self.current_body();
        let def_body = self.local_def_bodies.get(&local);
        let binding_id = def_body
            .and_then(|body| self.semantic_facts.body(body))
            .and_then(|facts| facts.locals.defs.get(&local))
            .and_then(|def| def.binding_id);
        if def_body != Some(&body) && binding_id.is_none() {
            return;
        }
        self.semantic_facts.record_local_use(
            body,
            LocalUseFact {
                expr_id,
                local,
                binding_id,
                mode,
            },
        );
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
        self.local_type_scopes.push_scope();
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
        self.active_mut_alias_roots
            .retain(|root| root.scope_depth <= self.scopes.len());
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
            active_collection_loans: vec![],
        }
    }

    fn take_scope_state(&mut self) -> ScopeState {
        ScopeState {
            scopes: std::mem::take(&mut self.scopes),
            local_type_scopes: std::mem::take(&mut self.local_type_scopes),
            closure: self
                .closure
                .replace_scope_state(ClosureScopeState::default()),
            active_collection_loans: std::mem::take(&mut self.active_collection_loans),
        }
    }

    fn restore_scope_state(&mut self, state: ScopeState) {
        self.scopes = state.scopes;
        self.local_type_scopes = state.local_type_scopes;
        self.closure.restore_scope_state(state.closure);
        self.active_collection_loans = state.active_collection_loans;
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
            active_collection_loans: std::mem::replace(
                &mut self.active_collection_loans,
                state.active_collection_loans,
            ),
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
        type_id: SemanticLocalId,
        kind: LocalBindingKind,
    ) {
        let Some(scope_depth) = self.scopes.len().checked_sub(1) else {
            return;
        };
        self.closure
            .define_binding(binding_id, name, type_id, kind, scope_depth);
    }

    fn define(&mut self, name: Ident, ty: &Type, mutable: bool) {
        self.define_value(name, ty, LocalBindingKind::from_mutable(mutable), None);
    }

    fn define_pattern_binding_from_handle(
        &mut self,
        name: Ident,
        handle: &TypeHandle,
        mutable: bool,
        span: Option<Span>,
    ) -> SemanticLocalId {
        let local = self
            .define_shadowing_value_from_handle(
                name,
                handle,
                LocalBindingKind::from_mutable(mutable),
                None,
                None,
            )
            .expect("pattern binding requires an active local scope");
        if let Some(span) = span {
            self.record_local_def(
                local.type_id,
                Some(local.binding_id),
                name,
                Some(span),
                mutable,
                LocalDefKind::Binding,
            );
            self.record_binding_def(span, local.type_id);
        }
        local.type_id
    }

    fn define_ref_alias_binding_from_handle(
        &mut self,
        name: Ident,
        handle: &TypeHandle,
        target: place::AliasTarget,
        context: PatternContext,
        span: Option<Span>,
    ) {
        if matches!(
            context,
            PatternContext::IfLet | PatternContext::WhileLet | PatternContext::Match
        ) {
            self.push_mut_alias_root(target.identity.clone(), name, MUT_ALIAS_ROOT_MESSAGE);
        }
        self.define_alias_binding_from_handle(name, handle, target, context, span);
    }

    fn define_alias_binding_from_handle(
        &mut self,
        name: Ident,
        handle: &TypeHandle,
        target: place::AliasTarget,
        context: PatternContext,
        span: Option<Span>,
    ) {
        let Some(id) = self.define_shadowing_value_from_handle(
            name,
            handle,
            LocalBindingKind::pattern_alias(context),
            None,
            Some(target),
        ) else {
            return;
        };
        if let Some(span) = span {
            self.record_local_def(
                id.type_id,
                Some(id.binding_id),
                name,
                Some(span),
                true,
                LocalDefKind::Binding,
            );
            self.record_binding_def(span, id.type_id);
        }
    }

    fn push_mut_alias_root(
        &mut self,
        identity: PlaceIdentity,
        allowed: Ident,
        message: &'static str,
    ) {
        self.active_mut_alias_roots.push(ActiveMutAliasRoot {
            identity,
            allowed,
            scope_depth: self.scopes.len(),
            message,
        });
    }

    fn define_const(&mut self, name: Ident, ty: &Type, value: ConstValue) {
        self.define_value(name, ty, LocalBindingKind::constant(), Some(value));
    }

    fn define_value(
        &mut self,
        name: Ident,
        ty: &Type,
        kind: LocalBindingKind,
        const_value: Option<ConstValue>,
    ) -> Option<DefinedLocal> {
        self.define_value_with_alias(name, ty, kind, const_value, None)
    }

    fn define_value_with_alias(
        &mut self,
        name: Ident,
        ty: &Type,
        kind: LocalBindingKind,
        const_value: Option<ConstValue>,
        alias: Option<place::AliasTarget>,
    ) -> Option<DefinedLocal> {
        let binding_id = self.fresh_binding_id();
        let type_id = self.solver.alloc_local_type(ty);
        let info = VarInfo::new(binding_id, type_id, kind)
            .with_const_value(const_value)
            .with_alias(alias);
        let inserted = self.define_local_symbol(name, LocalSymbol::value(info));
        if !inserted {
            return None;
        }
        self.define_closure_binding(binding_id, name, type_id, kind);
        Some(DefinedLocal {
            binding_id,
            type_id,
        })
    }

    fn define_local_symbol(&mut self, name: Ident, symbol: LocalSymbol) -> bool {
        let Some(scope) = self.scopes.last_mut() else {
            return false;
        };
        if scope.contains_key(&name) {
            self.errors
                .push(TypeError::DuplicateName { name, span: None });
            return false;
        }
        scope.insert(name, symbol);
        true
    }

    fn define_shadowing_value_from_handle(
        &mut self,
        name: Ident,
        handle: &TypeHandle,
        kind: LocalBindingKind,
        const_value: Option<ConstValue>,
        alias: Option<place::AliasTarget>,
    ) -> Option<DefinedLocal> {
        let type_id = self.solver.alloc_local_type_from_handle(handle);
        self.define_shadowing_local(name, type_id, kind, const_value, alias)
    }

    fn define_shadowing_local(
        &mut self,
        name: Ident,
        type_id: SemanticLocalId,
        kind: LocalBindingKind,
        const_value: Option<ConstValue>,
        alias: Option<place::AliasTarget>,
    ) -> Option<DefinedLocal> {
        let binding_id = self.fresh_binding_id();
        let scope = self.scopes.last_mut()?;
        let info = VarInfo::new(binding_id, type_id, kind)
            .with_const_value(const_value)
            .with_alias(alias);
        scope.insert(name, LocalSymbol::value(info));
        self.define_closure_binding(binding_id, name, type_id, kind);
        Some(DefinedLocal {
            binding_id,
            type_id,
        })
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

    fn local_binding_is_function_value(&self, name: Ident) -> bool {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| match scope.get(&name) {
                Some(LocalSymbol::Value(info)) => Some(matches!(
                    self.solver.local_type_to_type(info.type_id),
                    Type::Func { .. }
                )),
                Some(LocalSymbol::Callable(_)) => Some(true),
                None => None,
            })
            .unwrap_or(false)
    }

    fn define_local_callable(&mut self, name: Ident, callee: CallableRef, surface_ty: &Type) {
        let binding_id = self.fresh_binding_id();
        let type_id = self.solver.alloc_local_type(surface_ty);
        let info = LocalCallableInfo::new(binding_id, type_id, callee);
        if !self.define_local_symbol(name, LocalSymbol::callable(info.clone())) {
            return;
        }
        self.local_callables
            .insert(info.callee.def.id.clone(), info);
        self.define_closure_binding(binding_id, name, type_id, LocalBindingKind::immutable());
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

    fn local_value_from_info(info: VarInfo, depth: usize) -> LocalValue {
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

    fn local_value_access(value: &LocalValue) -> LocalPlaceAccess {
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

    fn local_place_value(
        &mut self,
        expr: &ExprNode,
        name: Ident,
        value: &LocalValue,
        mode: Option<LocalUseMode>,
    ) -> (place::PlaceValue, bool) {
        let checked = checked_from_handle(expr, &Self::local_handle(value.info.type_id), self);
        self.record_local_read(expr.node.id, value);
        if let Some(mode) = mode
            && value.info.kind.is_air_local()
        {
            self.record_local_use(expr.node.id, value.info.type_id, mode);
        }
        let access = Self::local_value_access(value);
        let mut place = place::PlaceValue::new(checked, access.access, access.facts);
        place.identity = access.identity;
        place.root_local = Some(value.info.type_id);
        place.root_binding = Some(value.info.binding_id);
        place.root_source_depth = Some(value.source_depth);
        place.root_name = Some(name);
        (place, access.accepts_extern_any)
    }

    fn type_handle(ty: &Type) -> TypeHandle {
        Solver::concrete_type(ty)
    }

    fn local_handle(id: SemanticLocalId) -> TypeHandle {
        Solver::local_handle(id)
    }

    fn set_type(&mut self, id: ExprId, ty: &Type, span: Span) -> TypeHandle {
        let span = self.error_span(span);
        if matches!(ty, Type::Infer) {
            self.set_poison_type(id, span)
        } else {
            self.solver.set_expr_type_from_type(id, span, ty);
            let handle = Solver::expr_handle(id);
            self.record_expr_type(id, span, &handle);
            handle
        }
    }

    fn set_poison_type(&mut self, id: ExprId, span: Option<SourceSpan>) -> TypeHandle {
        let handle = self.solver.poison_expr_type(id, span);
        self.record_expr_type(id, span, &handle);
        handle
    }

    fn handle_is_poison(&self, handle: &TypeHandle) -> bool {
        self.solver.handle_is_poison(handle)
    }

    pub(super) fn checked_is_poison(&self, checked: &CheckedType) -> bool {
        self.handle_is_poison(&checked.handle)
    }

    fn set_nil_type(&mut self, id: ExprId, span: Span) -> TypeHandle {
        let span = self.error_span(span);
        let handle = self.solver.nil_expr_type(id, span);
        self.record_expr_type(id, span, &handle);
        handle
    }

    fn fresh_nil_handle(&mut self, span: Span) -> TypeHandle {
        self.solver.fresh_nil_handle(self.error_span(span))
    }

    fn set_type_from_handle(&mut self, id: ExprId, span: Span, handle: &TypeHandle) -> TypeHandle {
        let span = self.error_span(span);
        let handle = self.solver.set_expr_type_from_handle(id, span, handle);
        self.record_expr_type(id, span, &handle);
        handle
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

    fn finalize_handle_type(&mut self, handle: &TypeHandle) -> (Type, bool) {
        let (ty, errors) = self.solver.finalize_handle_to_type(handle);
        let has_errors = self.push_finalize_errors(errors);
        (ty, has_errors)
    }

    fn compile_error(&self, span: Span, message: impl Into<String>) -> TypeError {
        TypeError::compile(self.error_span(span), message)
    }

    fn push_compile_error(&mut self, span: Span, message: impl Into<String>) {
        let error = self.compile_error(span, message);
        self.push_error(error);
    }

    fn record_expr_type(&mut self, expr_id: ExprId, span: Option<SourceSpan>, handle: &TypeHandle) {
        let site = self.current_expr_site(expr_id);
        let handle = self.solver.snapshot_handle(handle);
        self.semantic_facts.record_expr_type(site, span, handle);
    }

    fn record_call(&mut self, expr_id: ExprId, target: CallTarget) {
        let site = self.current_expr_site(expr_id);
        self.semantic_facts.record_call(site, target);
    }

    fn record_const_value(&mut self, expr_id: ExprId, value: ConstValue) {
        let site = self.current_expr_site(expr_id);
        self.semantic_facts.record_const_value(site, value);
    }

    pub(super) fn record_function_value(&mut self, expr_id: ExprId, fact: FunctionValueFact) {
        let site = self.current_expr_site(expr_id);
        self.semantic_facts.record_function_value(site, fact);
    }

    pub(super) fn function_value_kind_for_callee(callee: &CallableRef) -> FunctionValueKind {
        FunctionValueKind::Named(CallableInstanceKey {
            target: callee.def.id.clone(),
            args: callee.owner_args.clone(),
        })
    }

    pub(super) fn type_carries_function_value(&self, ty: &Type) -> bool {
        type_carries_function_value(&self.decls, ty)
    }

    pub(super) fn function_flow_mode(
        &self,
        value_ty: &Type,
        storage_ty: &Type,
        direct_local: bool,
    ) -> FunctionFlowMode {
        if direct_local || matches!(storage_ty, Type::Func { .. }) {
            return FunctionFlowMode::Direct;
        }
        let function_value =
            !matches!(value_ty, Type::Infer) && self.type_carries_function_value(value_ty);
        let function_storage =
            !matches!(storage_ty, Type::Infer) && self.type_carries_function_value(storage_ty);
        if function_value || function_storage {
            FunctionFlowMode::Stored
        } else {
            FunctionFlowMode::None
        }
    }

    pub(super) fn record_function_value_expr(
        &mut self,
        expr_id: ExprId,
        ty: &Type,
        kind: FunctionValueKind,
    ) {
        let is_function = matches!(ty, Type::Func { .. });
        if !self.type_carries_function_value(ty) || type_has_unfinished_facts(ty) {
            return;
        }
        let decls = &self.decls;
        let solver = &self.solver;
        self.closure
            .record_function_value_origin(expr_id, &kind, |id| {
                type_carries_function_value(decls, &solver.local_type_to_type(id))
            });
        if is_function {
            self.record_function_value(
                expr_id,
                FunctionValueFact {
                    expr: expr_id,
                    ty: ty.clone(),
                    kind,
                },
            );
        }
    }

    pub(super) fn call_return_function_value_origin(&self, expr_id: ExprId) -> FunctionValueOrigin {
        self.semantic_facts
            .body(&self.current_body())
            .and_then(|facts| facts.calls.get(&expr_id))
            .map_or(FunctionValueOrigin::CallReturn, |target| {
                if matches!(target.id.kind, CallableKind::ExternFunction) {
                    FunctionValueOrigin::CallReturn
                } else {
                    FunctionValueOrigin::SourceCallReturn
                }
            })
    }

    pub(super) fn record_call_return_function_value(&mut self, expr: &ExprNode, ty: &Type) {
        if matches!(expr.node.kind, ExprKind::Call(_)) {
            let origin = self.call_return_function_value_origin(expr.node.id);
            self.record_function_value_expr(expr.node.id, ty, FunctionValueKind::Storage(origin));
        }
    }

    pub(super) fn record_function_value_call(
        &mut self,
        expr_id: ExprId,
        fact: FunctionValueCallFact,
    ) {
        let site = self.current_expr_site(expr_id);
        self.semantic_facts.record_function_value_call(site, fact);
    }

    pub(super) fn record_default_args(
        &mut self,
        call: ExprId,
        callee: &CallTarget,
        provided: usize,
        params: &[FuncParam],
        default_sites: &[Option<ParamDefaultSite>],
    ) {
        if provided >= params.len() {
            return;
        }
        let body = self.current_body();
        let callee = CallableInstanceKey {
            target: callee.id.clone(),
            args: callee.args.clone(),
        };
        for (index, param) in params.iter().enumerate().skip(provided) {
            let Some(site) = default_sites.get(index).copied().flatten() else {
                continue;
            };
            self.semantic_facts.record_default_arg(
                body.clone(),
                DefaultArgFact {
                    call,
                    callee: callee.clone(),
                    param_index: index,
                    default: DefaultExprSite {
                        expr: site.expr_id,
                        source: site.source,
                    },
                    ty: param.ty.clone(),
                },
            );
        }
    }

    pub(super) fn record_default_field(
        &mut self,
        aggregate: ExprId,
        owner: Type,
        owner_key: NominalKey,
        field: Ident,
        slot: usize,
        default: &FieldDefault,
        ty: Type,
    ) {
        self.semantic_facts.record_default_field(
            self.current_body(),
            DefaultFieldFact {
                aggregate,
                owner,
                owner_key,
                field,
                slot,
                default: DefaultExprSite {
                    expr: default.expr_id,
                    source: default.span.source(),
                },
                ty,
            },
        );
    }

    fn record_extern_use(&mut self, expr_id: ExprId, target: ExternUseTarget) {
        let site = self.current_expr_site(expr_id);
        self.semantic_facts.record_extern_use(site, target);
    }

    fn record_stringify(&mut self, expr_id: ExprId, arg: ExprId) {
        self.semantic_facts
            .record_stringify(self.current_expr_site(expr_id), arg);
    }

    pub(crate) fn record_member_path(&mut self, fact: MemberPathFact) {
        self.semantic_facts
            .record_member_path(self.current_body(), fact);
    }

    fn record_expected_projection(&mut self, fact: ExpectedProjectionFact) {
        self.semantic_facts
            .record_expected_projection(self.current_body(), fact);
    }

    fn record_expr_place(&mut self, expr_id: ExprId, value: &place::PlaceValue) {
        self.expr_places.insert(expr_id, value.clone());
    }

    fn expr_place(&self, expr_id: ExprId) -> Option<place::PlaceValue> {
        self.expr_places.get(&expr_id).cloned()
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
        self.semantic_facts
            .record_contract_witness(key, self.source_span(span))
    }

    fn record_dyn_conversion_at(
        &mut self,
        site: SemanticExprSite,
        witness: WitnessId,
        span: SourceSpan,
    ) {
        let expr_id = site.expr;
        self.semantic_facts.record_dyn_conversion(
            site.body,
            DynConversionFact {
                expr_id,
                witness,
                span,
            },
        );
    }

    fn record_dyn_weakening_at(
        &mut self,
        site: SemanticExprSite,
        source: ContractSetKey,
        target: ContractSetKey,
        span: SourceSpan,
    ) {
        let expr_id = site.expr;
        self.semantic_facts.record_dyn_weakening(
            site.body,
            DynWeakeningFact {
                expr_id,
                source,
                target,
                span,
            },
        );
    }

    fn record_resolved_dyn_call(
        &mut self,
        site: SemanticExprSite,
        receiver: &SemanticExprSite,
        contract: ContractSetKey,
        method: Ident,
        arg_count: usize,
        requires_mutable: bool,
        span: SourceSpan,
    ) {
        let call_id = site.expr;
        self.semantic_facts.record_dyn_call(
            site.body,
            DynCallFact {
                call_id,
                receiver_id: receiver.expr,
                contract,
                method,
                arg_count,
                requires_mutable,
                span,
            },
        );
    }

    fn record_resolved_dyn_downcast(
        &mut self,
        site: SemanticExprSite,
        source: &SemanticExprSite,
        source_contract: ContractSetKey,
        target: Type,
        mutable: bool,
        span: SourceSpan,
    ) {
        let expr_id = site.expr;
        self.semantic_facts.record_dyn_downcast(
            site.body,
            DynDowncastFact {
                expr_id,
                source_id: source.expr,
                source: source_contract,
                target,
                mutable,
                span,
            },
        );
    }

    fn should_register_dyn_infer_params(&mut self) -> bool {
        self.dyn_infer_registered_modules
            .insert(self.current_module.clone())
    }

    pub(super) fn check_mut_alias_root_use(
        &mut self,
        root_name: Option<Ident>,
        identity: &PlaceIdentity,
        span: Span,
    ) {
        let Some(root_name) = root_name else {
            return;
        };
        let allowed_depth = self
            .active_mut_alias_roots
            .iter()
            .filter(|root| root.allowed == root_name && root.identity.conflicts_with(identity))
            .map(|root| root.scope_depth)
            .max();
        if let Some(root) = self.active_mut_alias_roots.iter().find(|root| {
            root.identity.conflicts_with(identity)
                && root.allowed != root_name
                && allowed_depth.is_none_or(|depth| depth <= root.scope_depth)
        }) {
            self.push_compile_error(span, root.message);
        }
    }

    fn extern_type_id(&self, ty: &Type) -> Option<ExternTypeId> {
        match self.resolve_nominal(ty)? {
            ResolvedNominal::Extern { id, .. } => Some(id),
            ResolvedNominal::Aggregate(_) | ResolvedNominal::Enum { .. } => None,
        }
    }

    fn extern_type(&self, owner: ExternTypeId) -> &ExternType {
        self.externs.ty(owner)
    }

    fn resolve_nominal(&self, ty: &Type) -> Option<ResolvedNominal<'_>> {
        let key = self.decls.key_for_type(ty)?;
        if key.kind == NominalKind::Extern {
            let id = self.externs.type_by_nominal(&key)?;
            return Some(ResolvedNominal::Extern {
                id,
                ty: self.externs.ty(id),
            });
        }
        match key.kind {
            NominalKind::Struct | NominalKind::DataRef => {
                self.decls.aggregate(&key).map(ResolvedNominal::Aggregate)
            }
            NominalKind::Enum => self
                .decls
                .enum_schema(&key)
                .map(|schema| ResolvedNominal::Enum { key, schema }),
            NominalKind::Extern => None,
        }
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

    fn return_type(&self) -> Option<Type> {
        match self.return_mode()? {
            ReturnMode::Explicit { ret, .. } => Some(ret.ty()),
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
        candidates.push(ReturnCandidate::Value { span, handle });
    }

    fn push_inferred_iter_return(&mut self, span: Span) {
        let Some(frame) = self.returns.last_mut() else {
            return;
        };
        let ReturnMode::Infer { candidates, .. } = &mut frame.mode else {
            return;
        };
        candidates.push(ReturnCandidate::Iter { span });
    }

    fn push_error(&mut self, err: impl Into<TypeError>) {
        self.errors.push(err.into());
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

    fn with_global_initializer_body<R>(
        &mut self,
        key: GlobalKey,
        f: impl FnOnce(&mut Self) -> R,
    ) -> R {
        self.global_initializer_depth += 1;
        let ret = self.with_body_instance(BodyInstanceKey::Global(key), f);
        self.global_initializer_depth = self
            .global_initializer_depth
            .checked_sub(1)
            .expect("global initializer depth underflow");
        ret
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
        let source_span = self.error_span(span);
        generic_context_from_params(type_params, const_params, |error| {
            self.push_error(generic_param_type_error(error, source_span));
        })
    }

    fn extended_generic_context(
        &mut self,
        owner: &GenericTypeContext,
        type_params: &[TypeParam],
        const_params: &[ConstParam],
        span: Span,
    ) -> GenericTypeContext {
        let source_span = self.error_span(span);
        extend_generic_context_with_params(owner, type_params, const_params, |error| {
            self.push_error(generic_param_type_error(error, source_span));
        })
    }

    fn resolve_generic_bounds_for_tc(&mut self, generics: &mut GenericParams, span: Span) {
        map_generic_param_bounds(generics, |bound| self.resolve_type_for_tc_at(&bound, span));
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
        if self.lookup_local_symbol(name).is_some() {
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

    fn resolve_ident_subject(
        &mut self,
        name: Ident,
        span: Span,
        mode: NameSubjectMode,
    ) -> ResolvedIdentSubject {
        match self.lookup_local_symbol_checked(name, span) {
            LocalSymbolLookup::Found(symbol, depth) => {
                return ResolvedIdentSubject::Local(symbol, depth);
            }
            LocalSymbolLookup::Blocked(error) => return ResolvedIdentSubject::Blocked(error),
            LocalSymbolLookup::Missing => {}
        }

        if !matches!(mode, NameSubjectMode::Const)
            && let Some((module, name, value)) = self.lookup_named_value(name)
        {
            return ResolvedIdentSubject::Named(module, name, Box::new(value));
        }

        if matches!(mode, NameSubjectMode::PostfixBase) {
            if let Some(scope) = self.lookup_module_alias(name) {
                return ResolvedIdentSubject::Module(scope);
            }
            if let Some(ty) = self.visible_type_subject(name, span) {
                return ResolvedIdentSubject::Type(ty);
            }
        }

        ResolvedIdentSubject::Missing
    }

    fn visible_type_subject(&mut self, name: Ident, span: Span) -> Option<Type> {
        if let Some(ty) = self.substituted_type_param(name) {
            return Some(ty);
        }
        if self.local_type_scopes.visible(name, None).is_some() {
            let ty = self.resolve_type_for_tc_at(&Type::UnresolvedName(name), span);
            return (!matches!(ty, Type::Infer)).then_some(ty);
        }
        let (binding, import) = self
            .decls
            .visible_type_binding_with_import(&self.current_module, name)?;
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
        let resolved_ret = ret.with_ty(self.resolve_type_for_tc_at(&ret.ty(), span));
        Type::func(resolved_params, resolved_ret)
    }

    fn resolve_callable_params(&mut self, params: &[Param], exported: bool) -> Vec<FuncParam> {
        params
            .iter()
            .map(|p| {
                let ty = self.resolve_callable_param_type(&p.ty, p.ty_span, exported);
                if let Some(error) =
                    collection_loan::stored_nested_slice_error(&ty, self.error_span(p.ty_span))
                {
                    self.push_error(error);
                }
                self.validate_func_param_escape(
                    p.escape,
                    matches!(p.mutability, Mutability::Mutable),
                    p.cast_accept,
                    &ty,
                    p.ty_span,
                );
                p.clone().with_ty(ty).func_param()
            })
            .collect()
    }

    fn build_semantic_declarations(&self) -> SemanticDeclarations {
        let mut facts = SemanticDeclarations::default();
        for module in &self.source_modules {
            facts.modules.push(SemanticModuleFact {
                module: module.scope.clone(),
                source: module.source,
            });

            for stmt in &module.program.stmts {
                match &stmt.node {
                    Stmt::Func(func_node) => {
                        let func = &func_node.node;
                        let id = CallableId::function(module.scope.clone(), func.name);
                        let value = self
                            .decls
                            .local_value(&module.scope, func.name)
                            .expect("source function missing declaration");
                        let callable = self
                            .decls
                            .callable_for_value(&value)
                            .expect("source function declaration is not callable");
                        assert_eq!(callable.def.id, id);
                        assert!(callable.def.sig.owner_generics.is_empty());
                        assert!(callable.def.sig.required_params <= callable.def.sig.params.len());
                        assert_eq!(func.params.len(), callable.def.sig.params.len());
                        let instances = self.callable_fact_instances(
                            &id,
                            !callable.def.sig.generics.is_empty(),
                            &callable.def.sig.params,
                            &callable.def.sig.ret,
                        );
                        for (args, params, return_ty) in instances {
                            facts.functions.push(Self::semantic_function_fact(
                                module, func_node, &callable, args, params, return_ty,
                            ));
                        }
                    }
                    Stmt::Extend(extend_node) => {
                        self.push_extend_method_facts(module, extend_node, &mut facts);
                    }
                    Stmt::Aggregate(agg_node) => {
                        let agg = &agg_node.node;
                        let owner = NominalKey {
                            module: module.scope.clone(),
                            kind: agg.kind.into(),
                            name: agg.name,
                        };
                        let Some(schema) = self.decls.aggregate(&owner) else {
                            continue;
                        };
                        for method in &agg.methods {
                            let mode = MethodMode::from_receiver(method.sig.receiver);
                            let Some(method_schema) = schema
                                .methods
                                .get(&MethodKey::new(method.sig.name, mode.surface()))
                            else {
                                continue;
                            };
                            let id = CallableId::aggregate_method(
                                owner.clone(),
                                method.sig.name,
                                mode.surface(),
                            );
                            let instances = self.callable_fact_instances(
                                &id,
                                !schema.generics.is_empty() || method_sig_is_generic(&method.sig),
                                &method_schema.params,
                                &method_schema.ret,
                            );
                            for (args, params, return_ty) in instances {
                                let owner_const_args = ConstTerm::to_args_no_infer(
                                    &args.const_args[..schema.generics.const_params.len()],
                                )
                                .expect("generic method instance has unresolved owner const args");
                                let self_ty = nominal_type_with_args(
                                    &owner,
                                    &args.type_args[..schema.generics.type_params.len()],
                                    &owner_const_args,
                                );
                                let callable = match mode {
                                    MethodMode::Instance { .. } => {
                                        self.decls.callable_for_aggregate_method(
                                            schema,
                                            method.sig.name,
                                            method_schema,
                                            self_ty.clone(),
                                        )
                                    }
                                    MethodMode::Static => {
                                        self.decls.callable_for_aggregate_static_method(
                                            schema,
                                            method.sig.name,
                                            method_schema,
                                            Some(&self_ty),
                                        )
                                    }
                                };
                                debug_assert_eq!(callable.def.id, id);
                                facts.functions.push(Self::semantic_method_fact(
                                    module,
                                    method,
                                    agg_node.span,
                                    &callable,
                                    args,
                                    params,
                                    return_ty,
                                ));
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        facts.validate();
        facts
    }

    fn callable_fact_instances(
        &self,
        id: &CallableId,
        generic: bool,
        params: &[FuncParam],
        ret: &ReturnSpec,
    ) -> Vec<(GenericArgs, Vec<FuncParam>, ReturnSpec)> {
        if !generic {
            return vec![(GenericArgs::default(), params.to_vec(), ret.clone())];
        }
        let mut instances = self
            .specializations
            .iter()
            .filter_map(|(key, state)| match state {
                SpecializationState::Done(body)
                    if key.target == *id && generic_args_are_concrete(&key.args) =>
                {
                    Some((
                        key.args.clone(),
                        body.params.clone(),
                        body.inferred_ret.clone().unwrap_or_else(|| {
                            if ret.is_iter() {
                                ReturnSpec::iter()
                            } else {
                                match ret.access() {
                                    ReturnAccess::Value => {
                                        ReturnSpec::value(body.return_ty.clone())
                                    }
                                    ReturnAccess::Place => {
                                        ReturnSpec::place(body.return_ty.clone())
                                    }
                                }
                            }
                        }),
                    ))
                }
                SpecializationState::InProgress | SpecializationState::Done(_) => None,
            })
            .collect::<Vec<_>>();
        instances.sort_by_key(|(args, ..)| format!("{args:?}"));
        instances
    }

    fn push_extend_method_facts(
        &self,
        module: &SourceModuleFactsInput,
        extend_node: &ExtendDeclNode,
        facts: &mut SemanticDeclarations,
    ) {
        let Some(schema) = self
            .decls
            .extends()
            .find(|schema| schema.origin == module.scope && schema.span.byte() == extend_node.span)
        else {
            return;
        };
        for method_node in &extend_node.node.methods {
            let method = &method_node.node;
            let mode = MethodMode::from_receiver(method.sig.receiver);
            let Some(method_schema) = schema
                .methods
                .get(&MethodKey::new(method.sig.name, mode.surface()))
            else {
                continue;
            };
            let id = CallableId::extend_method(schema.id.clone(), method.sig.name, mode.surface());
            let instances = self.callable_fact_instances(
                &id,
                !schema.generics.is_empty() || method_sig_is_generic(&method.sig),
                &method_schema.params,
                &method_schema.ret,
            );
            for (args, params, return_ty) in instances {
                let owner_args = extend_owner_args(schema, &args);
                let callable = match mode {
                    MethodMode::Instance { .. } => self.decls.callable_for_extend_method(
                        self_ty_for_extend(schema, &owner_args),
                        schema,
                        method.sig.name,
                        method_schema,
                        owner_args,
                    ),
                    MethodMode::Static => self.decls.callable_for_static_extend_method(
                        schema,
                        method.sig.name,
                        method_schema,
                        owner_args,
                    ),
                };
                facts.functions.push(Self::semantic_extend_method_fact(
                    module,
                    method,
                    method_node.span,
                    &callable,
                    args,
                    params,
                    return_ty,
                ));
            }
        }
    }

    fn semantic_function_fact(
        module: &SourceModuleFactsInput,
        func_node: &FuncNode,
        callable: &CallableRef,
        args: GenericArgs,
        param_types: Vec<FuncParam>,
        ret: ReturnSpec,
    ) -> SemanticFunctionInstanceFact {
        let func = &func_node.node;
        let params = func
            .params
            .iter()
            .zip(param_types)
            .map(|(source, sig)| SemanticParamSigFact {
                name: source.name,
                span: SourceSpan::from_byte_span(module.source, source.ty_span),
                ty: sig.ty,
                mutable: sig.mutable,
                escape: sig.escape,
            })
            .collect();
        Self::semantic_callable_fact(
            module,
            callable,
            args,
            func.name,
            func_node.span,
            func.body.span,
            params,
            ret,
        )
    }

    fn semantic_method_fact(
        module: &SourceModuleFactsInput,
        method: &Method,
        span: Span,
        callable: &CallableRef,
        args: GenericArgs,
        param_types: Vec<FuncParam>,
        ret: ReturnSpec,
    ) -> SemanticFunctionInstanceFact {
        Self::semantic_method_like_fact(
            module,
            SemanticMethodLikeInput {
                name: method.sig.name,
                receiver: method.sig.receiver,
                source_params: &method.sig.params,
                body_span: method.body.span,
                span,
                callable,
                args,
                param_types,
                ret,
            },
        )
    }

    fn semantic_extend_method_fact(
        module: &SourceModuleFactsInput,
        method: &ExtendMethod,
        span: Span,
        callable: &CallableRef,
        args: GenericArgs,
        param_types: Vec<FuncParam>,
        ret: ReturnSpec,
    ) -> SemanticFunctionInstanceFact {
        Self::semantic_method_like_fact(
            module,
            SemanticMethodLikeInput {
                name: method.sig.name,
                receiver: method.sig.receiver,
                source_params: &method.sig.params,
                body_span: method.body.span,
                span,
                callable,
                args,
                param_types,
                ret,
            },
        )
    }

    fn semantic_method_like_fact(
        module: &SourceModuleFactsInput,
        input: SemanticMethodLikeInput<'_>,
    ) -> SemanticFunctionInstanceFact {
        let mut params = vec![];
        if let Some(receiver_ty) = &input.callable.receiver_ty {
            params.push(SemanticParamSigFact {
                name: Ident::new("self"),
                span: SourceSpan::from_byte_span(module.source, input.span),
                ty: receiver_ty.clone(),
                mutable: matches!(input.receiver, Some(MethodReceiver::Ref)),
                escape: EscapeMode::NonEscaping,
            });
        }
        params.extend(
            input
                .source_params
                .iter()
                .zip(input.param_types)
                .map(|(source, sig)| SemanticParamSigFact {
                    name: source.name,
                    span: SourceSpan::from_byte_span(module.source, source.ty_span),
                    ty: sig.ty,
                    mutable: sig.mutable,
                    escape: sig.escape,
                }),
        );
        Self::semantic_callable_fact(
            module,
            input.callable,
            input.args,
            input.name,
            input.span,
            input.body_span,
            params,
            input.ret,
        )
    }

    fn semantic_callable_fact(
        module: &SourceModuleFactsInput,
        callable: &CallableRef,
        args: GenericArgs,
        name: Ident,
        span: Span,
        body_span: Span,
        params: Vec<SemanticParamSigFact>,
        ret: ReturnSpec,
    ) -> SemanticFunctionInstanceFact {
        let body = BodyInstanceKey::Callable(CallableInstanceKey {
            target: callable.def.id.clone(),
            args: args.clone(),
        });
        SemanticFunctionInstanceFact {
            id: callable.def.id.clone(),
            args,
            body,
            module: module.scope.clone(),
            name,
            span: SourceSpan::from_byte_span(module.source, span),
            body_span: SourceSpan::from_byte_span(module.source, body_span),
            params,
            ret,
            is_stringify_override: callable.is_stringify_override,
        }
    }

    fn finish(&mut self) -> Option<SemanticCheckOutput> {
        self.solve_constraints();
        self.solve_dyn_inference();
        let escape_events = self.closure.take_escape_events();
        self.push_escape_events(escape_events);
        if !self.errors.is_empty() {
            return None;
        }

        let mut types;
        let mut has_type_errors;
        let mut has_stringify_errors;
        loop {
            let (current_types, finalize_errors) = self.solver.finalize_expr_types();
            types = current_types;
            let has_finalize_errors = self.push_finalize_errors(finalize_errors);
            let has_local_finalize_errors = self.finish_semantic_local_defs();
            let has_expr_finalize_errors = self.finish_semantic_expr_types();
            has_type_errors =
                has_finalize_errors || has_local_finalize_errors || has_expr_finalize_errors;
            has_stringify_errors = false;
            if !has_type_errors {
                self.semantic_facts.finish_match_plans();
                self.semantic_facts.finish_stringifies();
                has_stringify_errors = self.finish_stringify_types();
            }
            if has_type_errors || has_stringify_errors {
                break;
            }
            if !self.ensure_stringify_override_specializations() {
                break;
            }
            self.solve_constraints();
            self.solve_dyn_inference();
            let escape_events = self.closure.take_escape_events();
            self.push_escape_events(escape_events);
            if !self.errors.is_empty() {
                return None;
            }
        }
        if !has_type_errors && !has_stringify_errors {
            for error in self.result_closure_errors(&types) {
                self.push_error_once(error);
            }
        }
        if !self.errors.is_empty() {
            return None;
        }
        let mut facts = self.closure.finish(|id| self.solver.local_type_to_type(id));
        let body_facts = self.semantic_facts.flattened_body_facts();
        facts.iter_runtime_checks = body_facts.iter_runtime_checks;
        facts.import_records = self.decls.import_records().to_vec();
        facts.used_imports.clone_from(self.decls.used_imports());
        facts.used_imports.extend(self.used_imports.clone());
        self.semantic_facts.validate_finished();
        let declaration_facts = self.build_semantic_declarations();
        Some(SemanticCheckOutput {
            warnings: std::mem::take(&mut self.warnings),
            lint_events: std::mem::take(&mut self.lint_events),
            public_facts: facts,
            source_types: types,
            program: SemanticProgram {
                facts: self.semantic_facts.clone(),
                declaration_facts,
                declarations: self.decls.clone(),
                externs: self.externs.clone(),
            },
        })
    }

    fn into_semantic_result(mut self) -> Result<SemanticCheckOutput, TypecheckFailure> {
        let semantic = self.finish();
        let errors = std::mem::take(&mut self.errors);
        let warnings = std::mem::take(&mut self.warnings);
        let lint_events = std::mem::take(&mut self.lint_events);
        let diagnostic_context = TypeDiagnosticContext::from_decls(&self.decls);
        match semantic {
            Some(semantic) if errors.is_empty() => Ok(semantic),
            _ => Err(TypecheckFailure {
                errors,
                warnings,
                lint_events,
                diagnostic_context: Box::new(diagnostic_context),
            }),
        }
    }

    fn finish_semantic_local_defs(&mut self) -> bool {
        let records = self
            .local_def_bodies
            .iter()
            .map(|(local, body)| (*local, body.clone()))
            .collect::<Vec<_>>();
        let mut has_errors = false;
        for (local, body) in records {
            let (ty, errors) = self.finalize_handle_type(&Solver::local_handle(local));
            has_errors |= errors;
            self.semantic_facts.finish_local_def(&body, local, ty);
        }
        has_errors
    }

    fn finish_semantic_expr_types(&mut self) -> bool {
        let records = self
            .semantic_facts
            .bodies
            .iter()
            .flat_map(|(body_key, body)| {
                body.expr_types
                    .iter()
                    .map(|(expr, fact)| (body_key.clone(), *expr, fact.handle.clone()))
            })
            .collect::<Vec<_>>();
        let mut has_errors = false;
        for (body, expr, handle) in records {
            let (ty, errors) = self.finalize_handle_type(&handle);
            has_errors |= errors;
            self.semantic_facts.finish_expr_type(&body, expr, ty);
        }
        has_errors
    }

    fn finish_stringify_types(&mut self) -> bool {
        let records = self
            .semantic_facts
            .bodies
            .values()
            .flat_map(|body| {
                body.stringifies.values().map(|fact| {
                    let span = body.expr_types.get(&fact.arg).and_then(|arg| arg.span);
                    (fact.source_ty.clone(), span)
                })
            })
            .collect::<Vec<_>>();
        let mut has_errors = false;
        for (ty, span) in records {
            if !type_closure_facts(&ty).contains_any {
                continue;
            }
            self.push_error_once(TypeError::AnyOutsideExternBoundary { span });
            has_errors = true;
        }
        has_errors
    }

    fn ensure_stringify_override_specializations(&mut self) -> bool {
        let types = self
            .semantic_facts
            .bodies
            .values()
            .flat_map(|body| body.stringifies.values().map(|fact| fact.source_ty.clone()))
            .collect::<Vec<_>>();
        let mut seen = HashSet::new();
        let mut added = false;
        for ty in types {
            added |= self.ensure_type_stringify_override_specializations(&ty, &mut seen);
        }
        added
    }

    fn ensure_type_stringify_override_specializations(
        &mut self,
        ty: &Type,
        seen: &mut HashSet<Type>,
    ) -> bool {
        if !seen.insert(ty.clone()) {
            return false;
        }
        let mut added = false;
        match ty {
            Type::Optional { inner } | Type::List { elem: inner } | Type::Slice { elem: inner } => {
                added |= self.ensure_type_stringify_override_specializations(inner, seen);
            }
            Type::Array { elem, .. } => {
                added |= self.ensure_type_stringify_override_specializations(elem, seen);
            }
            Type::Map { key, value } => {
                added |= self.ensure_type_stringify_override_specializations(key, seen);
                added |= self.ensure_type_stringify_override_specializations(value, seen);
            }
            Type::Tuple(items) => {
                for item in items {
                    added |= self.ensure_type_stringify_override_specializations(item, seen);
                }
            }
            Type::Nominal(_) => {
                added |= self.ensure_nominal_stringify_override_specialization(ty, seen);
            }
            Type::Func { .. }
            | Type::Infer
            | Type::InferReturn
            | Type::Any
            | Type::Int
            | Type::Float
            | Type::Bool
            | Type::String
            | Type::Void
            | Type::Dyn(_)
            | Type::Var(_)
            | Type::UnresolvedName(_)
            | Type::UnresolvedNominal { .. } => {}
        }
        added
    }

    fn ensure_nominal_stringify_override_specialization(
        &mut self,
        ty: &Type,
        seen: &mut HashSet<Type>,
    ) -> bool {
        let Some(owner) = self.decls.key_for_type(ty) else {
            return false;
        };
        if let Some(aggregate) = self.decls.aggregate(&owner).cloned() {
            if let Some(method) = aggregate.stringify_override().cloned() {
                return self.ensure_stringify_override_specialization(ty, &aggregate, &method);
            }

            let mut added = false;
            for field in aggregate.fields.values() {
                let field_ty = substitute_aggregate_member(ty, &aggregate.generics, &field.ty);
                added |= self.ensure_type_stringify_override_specializations(&field_ty, seen);
            }
            return added;
        }

        let Some(schema) = self.decls.enum_schema(&owner).cloned() else {
            return false;
        };
        let mut added = false;
        for variant in schema.variants.values() {
            variant.payload.for_each_type(|payload_ty| {
                let payload_ty = substitute_aggregate_member(ty, &schema.generics, payload_ty);
                added |= self.ensure_type_stringify_override_specializations(&payload_ty, seen);
            });
        }
        added
    }

    fn ensure_stringify_override_specialization(
        &mut self,
        ty: &Type,
        aggregate: &AggregateSchema,
        method: &MethodSchema,
    ) -> bool {
        let Some(args) = nominal_generic_args(ty) else {
            return false;
        };
        if args.is_empty() {
            return false;
        }
        let callable = self.decls.callable_for_aggregate_method(
            aggregate,
            Ident::new("to_string"),
            method,
            ty.clone(),
        );
        let key = CallableInstanceKey {
            target: callable.def.id.clone(),
            args: args.clone(),
        };
        if self.specializations.contains_key(&key) {
            return false;
        }
        debug_assert!(method.generics.is_empty());
        debug_assert_eq!(args.type_args.len(), aggregate.generics.type_params.len());
        debug_assert_eq!(args.const_args.len(), aggregate.generics.const_params.len());
        let generics = combined_callable_params(&callable);
        let (type_subst, const_subst) = generics.substitutions(&args);
        let const_bindings = callable_const_bindings(
            &callable.def.sig.owner_generics,
            &args,
            &callable.def.sig.generics,
            &GenericArgs::default(),
        );
        check_specialized_callable_body(
            &callable,
            &[],
            &method.ret,
            &args,
            type_subst,
            const_subst,
            &const_bindings,
            self,
        );
        true
    }

    fn result_closure_errors(&self, types: &SourceExprTypes) -> Vec<TypeError> {
        let mut errors = vec![];
        for (span, ty) in types.values() {
            push_type_closure_error(&mut errors, ty, *span);
        }
        for body in self.semantic_facts.bodies.values() {
            for (id, target) in &body.calls {
                let span = types.get(id).and_then(|(span, _)| *span);
                push_call_target_closure_error(&mut errors, target, span);
            }
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
                self.push_compile_error(
                    span,
                    format!("type '{arg}' does not satisfy contract bound '{bound}'"),
                );
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
    pub(super) fn core_option_or_infer(&mut self, inner: Type, span: Span) -> Type {
        match self.decls.core_option_of(inner) {
            Some(ty) => ty,
            None => {
                self.push_compile_error(span, "optional features require the core Option type");
                Type::Infer
            }
        }
    }

    pub(super) fn option_inner_or_error(&mut self, ty: &Type, span: Span) -> Type {
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

    pub(super) fn wrap_option_if_needed(&mut self, ty: Type, span: Span) -> Type {
        if matches!(ty, Type::Infer | Type::Void) || self.decls.semantic_option_inner(&ty).is_some()
        {
            return ty;
        }
        self.core_option_or_infer(ty, span)
    }

    pub(super) fn same_option_payload(&self, left: &Type, right: &Type) -> bool {
        match (
            self.decls.semantic_option_inner(left),
            self.decls.semantic_option_inner(right),
        ) {
            (Some(left), Some(right)) => left == right,
            _ => false,
        }
    }

    pub(super) fn optional_chain_inner_type(&mut self, ty: &Type, span: Span) -> Type {
        self.option_inner_or_error(ty, span)
    }

    pub(super) fn optional_chain_result_type(&mut self, ty: Type, span: Span) -> Type {
        self.wrap_option_if_needed(ty, span)
    }
}

#[cfg(test)]
pub(crate) fn check_with_modules(
    program: &Program,
    resolved: &ResolveResult,
    externs: RawExterns,
    config: TypecheckConfig,
) -> TypecheckOutput {
    match check_semantic_with_modules(program, resolved, externs, config) {
        Ok(mut semantic) => {
            let warnings = std::mem::take(&mut semantic.warnings);
            let lint_events = std::mem::take(&mut semantic.lint_events);
            let diagnostic_context =
                TypeDiagnosticContext::from_decls(&semantic.program.declarations);
            TypecheckOutput::success(
                warnings,
                lint_events,
                diagnostic_context,
                TypecheckFacts::from_semantic(semantic),
            )
        }
        Err(failure) => TypecheckOutput::failed(
            failure.errors,
            failure.warnings,
            failure.lint_events,
            *failure.diagnostic_context,
        ),
    }
}

pub(crate) fn check_semantic_with_modules(
    program: &Program,
    resolved: &ResolveResult,
    externs: RawExterns,
    config: TypecheckConfig,
) -> Result<SemanticCheckOutput, TypecheckFailure> {
    typechecker_for_modules(program, resolved, externs, config)?.into_semantic_result()
}

fn typechecker_for_modules(
    program: &Program,
    resolved: &ResolveResult,
    externs: RawExterns,
    config: TypecheckConfig,
) -> Result<TypeChecker, TypecheckFailure> {
    let mut decls = DeclarationIndex::from_root_and_modules(program, resolved, &externs);
    if decls.has_errors() {
        return Err(TypecheckFailure::errors(decl_errors(decls.errors())));
    }
    let catalog = match crate::externs::catalog::build_catalog(externs, &mut decls) {
        Ok(catalog) => catalog,
        Err(errors) => {
            let diagnostic_context = TypeDiagnosticContext::from_decls(&decls);
            return Err(TypecheckFailure {
                errors: errors.into_iter().map(TypeError::ExternCatalog).collect(),
                warnings: vec![],
                lint_events: vec![],
                diagnostic_context: Box::new(diagnostic_context),
            });
        }
    };
    decls.sync_extern_headers(&catalog);

    let mut tc = TypeChecker::new(decls, catalog, config);
    let root_scope = ModuleScope::from_module_id(&resolved.root);
    tc.current_module = root_scope.clone();

    let mut module_bodies = vec![];
    for source_module in DeclarationIndex::source_modules(program, resolved) {
        let scope = source_module.scope;
        let source = source_module.source;
        let program = Rc::new(source_module.program.clone());
        tc.module_sources.insert(scope.clone(), source);
        tc.source_modules.push(SourceModuleFactsInput {
            scope: scope.clone(),
            source,
            program: Rc::clone(&program),
        });
        tc.module_programs
            .insert(scope.clone(), Rc::clone(&program));
        if scope != root_scope {
            module_bodies.push((scope.clone(), Rc::clone(&program)));
        }
        tc.with_current_module(&scope, |tc| {
            tc.collect_const_decls(&scope, program.as_ref());
            collect_callable_templates(&scope, program.as_ref(), tc);
        });
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
    if !tc.errors.is_empty() {
        return Ok(tc);
    }
    tc.push_scope();
    register_declarations(program, &mut tc);
    check_decl_param_defaults(program, &mut tc);
    if tc.errors.is_empty() {
        check_stmts(&program.stmts, &mut tc);
    }
    tc.pop_scope();

    for (module, program) in module_bodies {
        check_module_bodies(&module, program.as_ref(), &mut tc);
    }

    Ok(tc)
}

fn extend_owner_args(schema: &ExtendSchema, args: &GenericArgs) -> GenericArgs {
    let owner_len = schema.generics.type_params.len();
    let const_len = schema.generics.const_params.len();
    GenericArgs {
        type_args: args.type_args[..owner_len].to_vec(),
        const_args: args.const_args[..const_len].to_vec(),
    }
}

fn self_ty_for_extend(schema: &ExtendSchema, owner_args: &GenericArgs) -> Type {
    let (type_subst, const_subst) = schema.generics.substitutions(owner_args);
    substitute(&schema.target, &type_subst, &const_subst)
}

fn decl_errors(errors: &[DeclError]) -> Vec<TypeError> {
    errors.iter().cloned().map(TypeError::Decl).collect()
}

impl TypeError {
    fn compile(span: Option<SourceSpan>, message: impl Into<String>) -> Self {
        Self::CompileError {
            message: message.into(),
            span,
        }
    }
}

#[derive(Clone)]
struct CheckedType {
    ty: Type,
    handle: TypeHandle,
    contains_extern_any: bool,
}

impl CheckedType {
    fn new(ty: Type, handle: TypeHandle) -> Self {
        Self {
            ty,
            handle,
            contains_extern_any: false,
        }
    }

    fn with_extern_any(mut self, contains_extern_any: bool) -> Self {
        self.contains_extern_any = contains_extern_any;
        self
    }

    fn with_handle(&self, ty: Type, handle: TypeHandle) -> Self {
        Self {
            ty,
            handle,
            contains_extern_any: self.contains_extern_any,
        }
    }

    fn union_extern_any(&self, other: &Self) -> bool {
        self.contains_extern_any || other.contains_extern_any
    }
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
            args.push(TypeChecker::type_handle(error));
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

fn checked_type(ty: Type) -> CheckedType {
    let handle = TypeChecker::type_handle(&ty);
    CheckedType::new(ty, handle)
}

fn checked_void() -> CheckedType {
    checked_type(Type::Void)
}

fn join_checked(
    left: CheckedType,
    left_span: Span,
    right: CheckedType,
    right_span: Span,
    tc: &mut TypeChecker,
) -> CheckedType {
    match (left.ty.is_void(), right.ty.is_void()) {
        (true, true) => return checked_void(),
        (true, false) => {
            tc.push_error(TypeError::TypeMismatch {
                expected: right.ty,
                found: Type::Void,
                span: tc.error_span(left_span),
            });
            return checked_void();
        }
        (false, true) => {
            tc.push_error(TypeError::TypeMismatch {
                expected: left.ty,
                found: Type::Void,
                span: tc.error_span(right_span),
            });
            return checked_void();
        }
        (false, false) => {}
    }
    let result = tc.fresh_temp_handle(right_span);
    let contains_extern_any = left.union_extern_any(&right);
    tc.expect_assignable(left_span, left.handle, result.clone());
    tc.expect_assignable(right_span, right.handle, result.clone());
    tc.solve_constraints();
    CheckedType::new(tc.handle_type(&result), result).with_extern_any(contains_extern_any)
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
    let expected_void = expected
        .as_ref()
        .is_some_and(|expected| tc.handle_type(expected).is_void());
    let checked = check_expr_checked_with_hint(expr, expected, tc);
    if !expected_void {
        reject_if_without_else_value(expr, tc);
    }
    checked
}

fn checked_from_type(expr: &ExprNode, ty: Type, tc: &mut TypeChecker) -> CheckedType {
    if matches!(ty, Type::Infer) {
        return checked_poison(expr, tc);
    }
    let handle = tc.set_type(expr.node.id, &ty, expr.span);
    CheckedType::new(ty, handle)
}

fn checked_poison(expr: &ExprNode, tc: &mut TypeChecker) -> CheckedType {
    let handle = tc.set_poison_type(expr.node.id, tc.error_span(expr.span));
    CheckedType::new(Type::Infer, handle)
}

fn checked_from_handle(expr: &ExprNode, handle: &TypeHandle, tc: &mut TypeChecker) -> CheckedType {
    let handle = tc.set_type_from_handle(expr.node.id, expr.span, handle);
    let ty = tc.handle_type(&handle);
    CheckedType::new(ty, handle)
}

fn checked_from_checked(
    expr: &ExprNode,
    checked: &CheckedType,
    tc: &mut TypeChecker,
) -> CheckedType {
    let handle = tc.set_type_from_handle(expr.node.id, expr.span, &checked.handle);
    let ty = tc.handle_type(&handle);
    checked.with_handle(ty, handle)
}

fn solve_and_checked_from_handle(
    expr: &ExprNode,
    handle: &TypeHandle,
    tc: &mut TypeChecker,
) -> CheckedType {
    tc.solve_constraints();
    checked_from_handle(expr, handle, tc)
}

pub(in crate::typecheck) fn check_expected_value_expr(
    expr: &ExprNode,
    expected: TypeHandle,
    tc: &mut TypeChecker,
) -> CheckedType {
    check_expected_value_expr_inner(
        expr,
        expected,
        projection::ExpectedProjectionMode::Assignable,
        true,
        tc,
    )
}

fn check_expected_value_expr_deferred(
    expr: &ExprNode,
    expected: TypeHandle,
    tc: &mut TypeChecker,
) -> CheckedType {
    tc.with_deferred_expected_returns(|tc| {
        check_expected_value_expr_inner(
            expr,
            expected,
            projection::ExpectedProjectionMode::Assignable,
            false,
            tc,
        )
    })
}

fn check_expected_value_expr_inner(
    expr: &ExprNode,
    expected: TypeHandle,
    mode: projection::ExpectedProjectionMode,
    enforce: bool,
    tc: &mut TypeChecker,
) -> CheckedType {
    let hint = (!matches!(expr.node.kind, ExprKind::Ident(_))).then(|| expected.clone());
    let mut checked = check_value_expr_checked_with_hint(expr, hint, tc);
    let target = tc.handle_type(&expected);
    let source = checked.ty.clone();
    let accepted = match projection::expected_projection(tc, expr.span, &source, &target, mode) {
        projection::ExpectedProjectionDecision::SourceAccepted
        | projection::ExpectedProjectionDecision::NotNeeded => true,
        projection::ExpectedProjectionDecision::Project(projection) => {
            checked = projection::apply_value_projection(tc, expr, &checked, &source, projection);
            true
        }
        projection::ExpectedProjectionDecision::Failed => false,
    };
    if accepted && enforce {
        tc.expect_assignable_expr(expr.span, expr.node.id, checked.handle.clone(), expected);
    }
    checked
}

fn check_unprojected_expected(
    expr: &ExprNode,
    expected: TypeHandle,
    tc: &mut TypeChecker,
) -> CheckedType {
    let checked = check_value_expr_checked_with_hint(expr, Some(expected.clone()), tc);
    tc.expect_assignable_expr(expr.span, expr.node.id, checked.handle.clone(), expected);
    checked
}

pub(in crate::typecheck) fn validate_const_expr_type(
    expr: &ExprNode,
    expected: Option<TypeHandle>,
    tc: &mut TypeChecker,
) -> Result<Type, Box<TypeError>> {
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
    Err(Box::new(error))
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
                checked_from_handle(expr, &expected, tc)
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
        ExprKind::IterSource(iter) => {
            check_expr_checked(&iter.node.source, tc);
            tc.push_error(TypeError::IteratorPlanAsValue {
                span: tc.error_span(expr.span),
            });
            checked_from_type(expr, Type::Infer, tc)
        }
        ExprKind::Ident(name) => {
            match tc.resolve_ident_subject(*name, expr.span, NameSubjectMode::Value) {
                ResolvedIdentSubject::Local(LocalSymbol::Value(ref info), depth) => {
                    if let Some(checked) = tc.check_local_const_value_expr(expr, *name, info) {
                        return checked;
                    }
                    let value = TypeChecker::local_value_from_info(info.clone(), depth);
                    let (place, _) =
                        tc.local_place_value(expr, *name, &value, Some(LocalUseMode::Read));
                    tc.check_mut_alias_root_use(Some(*name), &place.identity, expr.span);
                    let checked = place.checked.clone();
                    tc.record_expr_place(expr.node.id, &place);
                    tc.record_function_value_expr(
                        expr.node.id,
                        &checked.ty,
                        FunctionValueKind::Storage(FunctionValueOrigin::KnownLocal),
                    );
                    checked
                }
                ResolvedIdentSubject::Local(LocalSymbol::Callable(info), _) => {
                    match info.value_error(*name, tc.error_span(expr.span)) {
                        Some(error) => {
                            tc.push_error(error);
                            checked_from_type(expr, Type::Infer, tc)
                        }
                        None => {
                            let checked = checked_from_handle(
                                expr,
                                &TypeChecker::local_handle(info.type_id),
                                tc,
                            );
                            tc.record_function_value_expr(
                                expr.node.id,
                                &checked.ty,
                                TypeChecker::function_value_kind_for_callee(&info.callee),
                            );
                            checked
                        }
                    }
                }
                ResolvedIdentSubject::Blocked(error) => {
                    tc.push_error(*error);
                    checked_from_type(expr, Type::Infer, tc)
                }
                ResolvedIdentSubject::Named(module, value_name, value) => {
                    tc.warn_named_value_deprecated(value.as_ref(), value_name, expr.span);
                    match value.as_ref() {
                        ValueDecl::Const(_) => {
                            let value =
                                tc.eval_top_const(&module, value_name, tc.error_span(expr.span));
                            tc.check_const_value_expr(expr, value)
                        }
                        ValueDecl::Global(sig) => {
                            let checked =
                                checked_from_handle(expr, &tc.global_handle(&sig.key), tc);
                            let value = place::global_value(sig, expr.node.id, checked);
                            tc.record_expr_place(expr.node.id, &value);
                            place::record_value_read(expr.node.id, &value, tc);
                            tc.record_function_value_expr(
                                expr.node.id,
                                &value.checked.ty,
                                FunctionValueKind::Storage(FunctionValueOrigin::GlobalRoot),
                            );
                            value.checked
                        }
                        ValueDecl::Func(sig) => {
                            let resolved = ResolvedValue {
                                module,
                                name: value_name,
                                decl: ValueDecl::Func(sig.clone()),
                            };
                            if let Some(callee) = tc.decls.callable_for_value(&resolved) {
                                if callee.def.sig.ret.is_infer() {
                                    tc.push_error(TypeError::InferReturnValue {
                                        span: tc.error_span(expr.span),
                                    });
                                    return checked_from_type(expr, Type::Infer, tc);
                                }
                                if callee.def.sig.ret.is_iter() {
                                    tc.push_error(TypeError::IteratorPlanAsValue {
                                        span: tc.error_span(expr.span),
                                    });
                                    return checked_from_type(expr, Type::Infer, tc);
                                }
                                tc.record_function_value_expr(
                                    expr.node.id,
                                    &sig.ty,
                                    TypeChecker::function_value_kind_for_callee(&callee),
                                );
                            }
                            checked_from_type(expr, sig.ty.clone(), tc)
                        }
                    }
                }
                ResolvedIdentSubject::Missing => {
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
                ResolvedIdentSubject::Module(_) | ResolvedIdentSubject::Type(_) => unreachable!(),
            }
        }
        ExprKind::Binary(bin_node) => checked_from_checked(
            expr,
            &check_binary(expr.node.id, bin_node, expected.as_ref(), tc),
            tc,
        ),
        ExprKind::Unary(unary_node) => {
            checked_from_checked(expr, &check_unary(expr.node.id, unary_node, tc), tc)
        }
        ExprKind::Try(try_node) => {
            checked_from_checked(expr, &check_try(try_node, expected, tc), tc)
        }
        ExprKind::Block(block_node) => {
            let checked = check_block_checked_with_hint(block_node, expected, tc);
            if let Some(tail) = &block_node.node.tail {
                tc.closure.copy_expr_flow(tail.node.id, expr.node.id);
            }
            checked_from_checked(expr, &checked, tc)
        }
        ExprKind::If(if_node) => {
            let checked =
                control_flow::check_if_checked_with_hint(if_node, expected, expr.node.id, tc);
            checked_from_checked(expr, &checked, tc)
        }
        ExprKind::Ternary(ternary_node) => {
            let checked = control_flow::check_ternary_checked_with_hint(
                ternary_node,
                expected,
                expr.node.id,
                tc,
            );
            checked_from_checked(expr, &checked, tc)
        }
        ExprKind::Assign(assign_node) => {
            check_assign(expr.node.id, assign_node, tc);
            checked_from_type(expr, Type::Void, tc)
        }
        ExprKind::StructLiteral(lit) => check_struct_lit_hint(expr, lit, expected.as_ref(), tc),
        ExprKind::InferredEnum(node) => check_inferred_enum_hint(expr, node, expected, tc),
        ExprKind::Field(_) | ExprKind::Call(_) | ExprKind::Index(_) | ExprKind::TupleIndex(_) => {
            let chain = collect_postfix_chain(expr).expect("postfix chain");
            check_postfix_chain(&chain, expr, expected.as_ref(), tc)
        }
        ExprKind::Tuple(elems) => check_tuple_checked_with_hint(expr, elems, expected.as_ref(), tc),
        ExprKind::ArrayLiteral(lit) => check_array_lit_hint(expr, lit, expected.as_ref(), tc),
        ExprKind::ArrayFill(fill) => check_array_fill_hint(expr, fill, expected.as_ref(), tc),
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
            checked_from_checked(expr, &checked, tc)
        }
        ExprKind::Match(match_node) => {
            let checked =
                control_flow::check_match_checked_with_hint(match_node, expected, expr.node.id, tc);
            checked_from_checked(expr, &checked, tc)
        }
        ExprKind::StringInterp(parts) => check_string_interp(expr, parts, tc),
        ExprKind::MapLiteral(lit) => check_map_lit_hint(expr, lit, expected.as_ref(), tc),
        ExprKind::IntrinsicCall(call) => check_intrinsic_call(expr, call, tc),
        ExprKind::Range(range) => check_range_expr(expr, range, expected.as_ref(), tc),
        ExprKind::Cast(cast) => convert::check_cast_expr(expr, cast, tc),
        ExprKind::ExactDowncast(downcast) => {
            downcast::check_expr(expr, downcast, expected.as_ref(), tc)
        }
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
    Stringify,
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
        "stringify" => IntrinsicKind::Stringify,
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
                tc.push_compile_error(call.span, message);
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
        IntrinsicKind::Stringify => check_stringify_intrinsic(expr, call, tc),
    }
}

fn check_stringify_intrinsic(
    expr: &ExprNode,
    call: &IntrinsicCallNode,
    tc: &mut TypeChecker,
) -> CheckedType {
    if !check_intrinsic_arg_count(call.node.name, &call.node.args, 1, call.span, tc) {
        return checked_from_type(expr, Type::String, tc);
    }
    let arg = &call.node.args[0];
    let checked = check_value_expr_checked_with_hint(arg, None, tc);
    check_default_stringify_conversion(&checked, arg.span, tc);
    tc.record_stringify(expr.node.id, arg.node.id);
    checked_from_type(expr, Type::String, tc)
}

fn check_default_stringify_conversion(checked: &CheckedType, span: Span, tc: &mut TypeChecker) {
    if checked.contains_extern_any {
        tc.reject_extern_any_escape(checked, span);
        return;
    }
    tc.reject_user_any_type(&checked.ty, span);
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
    let field = Solver::expr_handle(*id);
    let carrier_ty = tc.solver.handle_to_partial_type(&field);
    let key = tc.decls.key_for_type(&carrier_ty)?;
    let schema = tc.decls.enum_schema(&key)?;
    schema.variants.get(node.node.field)?;

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
    let return_ty = tc.return_type();
    let enclosing = return_ty.as_ref().and_then(|ty| tc.try_carrier_parts(ty));

    if tc.in_global_initializer() {
        tc.push_compile_error(
            try_node.span,
            "try is not allowed in runtime global initializers",
        );
        check_value_expr_checked_with_hint(&try_node.node.expr, None, tc);
        return checked_type(Type::Infer);
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
            return checked_type(Type::Infer);
        }
        if let (Some(enclosing), Some(found)) = (&enclosing, operand_recovery_ty.as_ref())
            && let Some(found) = try_carrier_mismatch_ty(found, enclosing, tc)
        {
            push_try_invalid_carrier(enclosing, found, try_node.span, tc);
            return checked_type(Type::Infer);
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
        return checked_type(Type::Infer);
    };
    let Some(operand_carrier) = tc.try_carrier_parts(&operand_ty) else {
        push_try_invalid_carrier(&enclosing, operand_ty, try_node.span, tc);
        return checked_type(Type::Infer);
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
        return checked_type(Type::Infer);
    }
    if !enclosing.validate_residual(&operand_carrier, try_node.span, tc) {
        return checked_type(Type::Infer);
    }

    let mut checked = checked_type(operand_carrier.success.clone());
    checked.contains_extern_any = type_closure_facts(&checked.ty).contains_any;
    checked
}

fn check_binary(
    expr_id: ExprId,
    bin: &BinaryNode,
    expected: Option<&TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    if bin.node.op == BinaryOp::Coalesce {
        return check_coalesce(
            expr_id,
            &bin.node.left,
            &bin.node.right,
            bin.span,
            expected,
            tc,
        );
    }
    if let Some(checked) = check_nil_equality(expr_id, bin, tc) {
        return checked;
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

fn check_nil_equality(
    expr_id: ExprId,
    bin: &BinaryNode,
    tc: &mut TypeChecker,
) -> Option<CheckedType> {
    if !matches!(bin.node.op, BinaryOp::Eq | BinaryOp::NotEq) {
        return None;
    }

    let left_nil = is_nil_lit(&bin.node.left);
    let right_nil = is_nil_lit(&bin.node.right);
    if left_nil == right_nil {
        return None;
    }

    let (value_expr, nil_expr) = if left_nil {
        (&bin.node.right, &bin.node.left)
    } else {
        (&bin.node.left, &bin.node.right)
    };

    let value = check_expr_checked(value_expr, tc);
    if tc.checked_is_poison(&value) {
        check_expr_checked(nil_expr, tc);
        return Some(checked_type(Type::Infer));
    }

    if tc.decls.semantic_option_inner(&value.ty).is_some() {
        check_value_expr_checked_with_hint(nil_expr, Some(value.handle), tc);
        return Some(checked_type(Type::Bool));
    }
    if matches!(value.ty, Type::Infer) {
        check_expr_checked(nil_expr, tc);
        return Some(checked_type(Type::Bool));
    }

    let nil = check_expr_checked(nil_expr, tc);
    let (left, right) = if left_nil { (nil, value) } else { (value, nil) };
    Some(check_binary_checked(
        expr_id,
        bin.node.op,
        &bin.node.left,
        left,
        &bin.node.right,
        right,
        bin.span,
        tc,
    ))
}

fn is_nil_lit(expr: &ExprNode) -> bool {
    matches!(expr.node.kind, ExprKind::Lit(Lit::Nil))
}

fn type_carries_function_value(decls: &DeclarationIndex, ty: &Type) -> bool {
    matches!(ty, Type::Func { .. } | Type::Infer)
        || decls
            .semantic_option_inner(ty)
            .is_some_and(|inner| type_carries_function_value(decls, inner))
}

fn check_coalesce(
    expr_id: ExprId,
    left_expr: &ExprNode,
    right_expr: &ExprNode,
    span: Span,
    expected: Option<&TypeHandle>,
    tc: &mut TypeChecker,
) -> CheckedType {
    let expected_ty = expected.map(|handle| tc.handle_type(handle));
    let left_expected = expected_ty
        .as_ref()
        .map(|ty| tc.core_option_or_infer(ty.clone(), span))
        .map(|ty| TypeChecker::type_handle(&ty));
    let left = check_value_expr_checked_with_hint(left_expr, left_expected, tc);
    let Some(inner) = tc.decls.semantic_option_inner(&left.ty).cloned() else {
        if matches!(left.ty, Type::Infer) {
            let mut right = check_expr_checked(right_expr, tc);
            right.contains_extern_any |= left.contains_extern_any;
            tc.closure
                .copy_coalesce_selected_flow(left_expr.node.id, right_expr.node.id, expr_id);
            return checked_from_checked(right_expr, &right, tc);
        }
        tc.push_error(TypeError::InvalidOperand {
            op: BinaryOp::Coalesce.to_string(),
            operand_type: left.ty,
            span: tc.error_span(span),
        });
        check_expr_checked(right_expr, tc);
        return checked_type(Type::Infer);
    };

    let mut right = check_expr_checked(right_expr, tc);
    right.contains_extern_any |= left.contains_extern_any;
    tc.closure
        .copy_coalesce_selected_flow(left_expr.node.id, right_expr.node.id, expr_id);
    if tc.same_option_payload(&left.ty, &right.ty) {
        return right;
    }

    let result = TypeChecker::type_handle(&inner);
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
        return checked_type(Type::Infer);
    }

    if op == BinaryOp::Add && (left.ty.is_str() || right.ty.is_str()) {
        record_binary_stringify_conversions(left_expr, &left, right_expr, &right, tc);
        return checked_type(Type::String);
    }

    if matches!(op, BinaryOp::Eq | BinaryOp::NotEq) {
        return check_equality(expr_id, op, left_expr, left, right_expr, right, span, tc);
    }

    if let (Some(lhs), Some(rhs)) = (left.ty.scalar_kind(), right.ty.scalar_kind())
        && let Some(result) = op.scalar_result(lhs, rhs)
    {
        return checked_type(type_from_scalar(result));
    }

    extern_ops::check_binary(expr_id, op, left_expr, &left, right_expr, &right, span, tc)
        .unwrap_or_else(|| checked_type(emit_binary_failure(op, &left.ty, &right.ty, span, tc)))
}

fn check_equality(
    expr_id: ExprId,
    op: BinaryOp,
    left_expr: &ExprNode,
    left: CheckedType,
    right_expr: &ExprNode,
    right: CheckedType,
    span: Span,
    tc: &mut TypeChecker,
) -> CheckedType {
    if left.ty != right.ty {
        return extern_ops::check_binary(
            expr_id, op, left_expr, &left, right_expr, &right, span, tc,
        )
        .unwrap_or_else(|| {
            tc.push_error(TypeError::TypeMismatch {
                expected: left.ty,
                found: right.ty,
                span: tc.error_span(span),
            });
            checked_type(Type::Bool)
        });
    }

    if tc.extern_type_id(&left.ty).is_some() {
        return extern_ops::check_binary(
            expr_id, op, left_expr, &left, right_expr, &right, span, tc,
        )
        .unwrap_or_else(|| {
            tc.push_error(TypeError::InvalidOperand {
                op: op.to_string(),
                operand_type: right.ty,
                span: tc.error_span(span),
            });
            checked_type(Type::Infer)
        });
    }

    if equatable_type(&left.ty, tc) {
        checked_type(Type::Bool)
    } else {
        tc.push_error(TypeError::NotEquatable {
            ty: left.ty,
            span: tc.error_span(span),
        });
        checked_type(Type::Infer)
    }
}

fn record_binary_stringify_conversions(
    left_expr: &ExprNode,
    left: &CheckedType,
    right_expr: &ExprNode,
    right: &CheckedType,
    tc: &mut TypeChecker,
) {
    match (left.ty.is_str(), right.ty.is_str()) {
        (true, false) => {
            check_default_stringify_conversion(right, right_expr.span, tc);
            tc.record_stringify(right_expr.node.id, right_expr.node.id);
        }
        (false, true) => {
            check_default_stringify_conversion(left, left_expr.span, tc);
            tc.record_stringify(left_expr.node.id, left_expr.node.id);
        }
        _ => {}
    }
}

fn emit_binary_failure(
    op: BinaryOp,
    left: &Type,
    right: &Type,
    span: Span,
    tc: &mut TypeChecker,
) -> Type {
    let same = left == right;
    match op {
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => {
            invalid_operand(op, right.clone(), Type::Infer, span, tc)
        }
        BinaryOp::LessThan
        | BinaryOp::GreaterThan
        | BinaryOp::LessThanEq
        | BinaryOp::GreaterThanEq => {
            if same {
                invalid_operand(op, left.clone(), Type::Bool, span, tc)
            } else {
                type_mismatch(left.clone(), right.clone(), Type::Bool, span, tc)
            }
        }
        BinaryOp::And | BinaryOp::Or => {
            let operand_type = if left.is_bool() { right } else { left };
            invalid_operand(op, operand_type.clone(), Type::Bool, span, tc)
        }
        BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::Xor | BinaryOp::Shl | BinaryOp::Shr => {
            if same {
                invalid_operand(op, left.clone(), Type::Int, span, tc)
            } else {
                let found = if left.is_int() { right } else { left };
                type_mismatch(Type::Int, found.clone(), Type::Int, span, tc)
            }
        }
        BinaryOp::Eq | BinaryOp::NotEq | BinaryOp::Coalesce => Type::Infer,
    }
}

fn invalid_operand(
    op: BinaryOp,
    operand_type: Type,
    fallback: Type,
    span: Span,
    tc: &mut TypeChecker,
) -> Type {
    tc.push_error(TypeError::InvalidOperand {
        op: op.to_string(),
        operand_type,
        span: tc.error_span(span),
    });
    fallback
}

fn type_mismatch(
    expected: Type,
    found: Type,
    fallback: Type,
    span: Span,
    tc: &mut TypeChecker,
) -> Type {
    tc.push_error(TypeError::TypeMismatch {
        expected,
        found,
        span: tc.error_span(span),
    });
    fallback
}

fn check_unary(expr_id: ExprId, unary: &UnaryNode, tc: &mut TypeChecker) -> CheckedType {
    let operand = check_expr_checked(&unary.node.expr, tc);
    if tc.checked_is_poison(&operand) {
        return checked_type(Type::Infer);
    }
    if let Some(value) = operand.ty.scalar_kind()
        && let Some(result) = unary.node.op.scalar_result(value)
    {
        return checked_type(type_from_scalar(result));
    }
    extern_ops::check_unary(expr_id, unary.node.op, &operand, tc).unwrap_or_else(|| {
        tc.push_error(TypeError::InvalidOperand {
            op: unary.node.op.to_string(),
            operand_type: operand.ty,
            span: tc.error_span(unary.span),
        });
        checked_type(Type::Infer)
    })
}

fn equatable_type(ty: &Type, tc: &TypeChecker) -> bool {
    !matches!(ty, Type::Slice { .. })
        && !type_contains_dyn_value(ty, &tc.decls, &mut HashSet::new())
}

fn type_from_scalar(scalar: ScalarKind) -> Type {
    match scalar {
        ScalarKind::Int => Type::Int,
        ScalarKind::Float => Type::Float,
        ScalarKind::Bool => Type::Bool,
        ScalarKind::String => Type::String,
    }
}

fn expected_assignable_type(expected: Option<&TypeHandle>, tc: &TypeChecker) -> Option<Type> {
    let ty = expected.map(|handle| tc.handle_type(handle))?;
    Some(tc.decls.semantic_option_inner(&ty).unwrap_or(&ty).clone())
}

fn sync_assigned_flow(
    target: &ExprNode,
    value: &ExprNode,
    mode: FunctionFlowMode,
    tc: &mut TypeChecker,
) {
    let ExprKind::Ident(name) = target.node.kind else {
        tc.record_assignment_escape(value);
        return;
    };
    if tc.lookup_local_symbol(name).is_none() {
        tc.record_assignment_escape(value);
        return;
    }

    let Some(binding_id) = tc.local_binding_id(name) else {
        tc.record_assignment_escape(value);
        return;
    };
    tc.closure
        .assign_local_or_use(binding_id, value.node.id, mode, value.span);
}

fn check_assign(expr_id: ExprId, assign: &AssignNode, tc: &mut TypeChecker) {
    if assign.node.op == AssignOp::Assign && check_simple_index_assignment(assign, tc) {
        return;
    }

    let target = check_assignment_place(&assign.node.target, tc);
    if assign.node.op == AssignOp::Assign {
        check_simple_assignment(assign, &target, tc);
        return;
    }

    if let Some(error) = target.value.access.assign_error(
        assignment_target_name(&assign.node.target),
        tc.error_span(assign.node.target.span),
    ) {
        tc.push_error(error);
    }
    if let Some(error) = collection_loan::root_rebind_error(
        &tc.active_collection_loans,
        &target.value.identity,
        tc.error_span(assign.node.target.span),
    ) {
        tc.push_error(error);
    }

    let Some(op) = assign_op_to_binary_op(assign.node.op) else {
        return;
    };
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

fn check_assignment_place(target: &ExprNode, tc: &mut TypeChecker) -> place::CheckedPlace {
    let ExprKind::Index(index) = &target.node.kind else {
        return check_place(target, tc);
    };
    let receiver = check_place(&index.node.target, tc);
    place::check_indexed_place(target, index, &receiver, tc)
}

fn check_simple_index_assignment(assign: &AssignNode, tc: &mut TypeChecker) -> bool {
    let ExprKind::Index(index) = &assign.node.target.node.kind else {
        return false;
    };
    if index.node.safe || matches!(index.node.index.node.kind, ExprKind::Range(_)) {
        return false;
    }

    let target = check_place(&index.node.target, tc);
    let Type::Map { key, value } = target.checked().ty.clone() else {
        let target = place::check_indexed_place(&assign.node.target, index, &target, tc);
        check_simple_assignment(assign, &target, tc);
        return true;
    };

    check_map_index_assignment(assign, index, &target, &key, &value, tc);
    true
}

fn check_map_index_assignment(
    assign: &AssignNode,
    index: &IndexNode,
    target: &place::CheckedPlace,
    key: &Type,
    value: &Type,
    tc: &mut TypeChecker,
) {
    check_map_key(index, key, tc);
    if let Some(error) = target.value.access.assign_error(
        assignment_target_name(&index.node.target),
        tc.error_span(index.node.target.span),
    ) {
        tc.push_error(error);
    }
    let projected_identity = target.value.identity.clone().index();
    if let Some(error) = collection_loan::root_rebind_error(
        &tc.active_collection_loans,
        &projected_identity,
        tc.error_span(assign.node.target.span),
    ) {
        tc.push_error(error);
    }

    let value_handle = TypeChecker::type_handle(value);
    let value = check_expected_value_expr(&assign.node.value, value_handle, tc);
    if !target.accepts_extern_any() {
        tc.reject_extern_any_escape(&value, assign.node.value.span);
    }
    if target.value.access.can_assign() {
        tc.record_assignment_escape(&assign.node.value);
        place::record_projected_write(assign.node.target.node.id, target, tc);
    }
}

fn check_simple_assignment(
    assign: &AssignNode,
    target: &place::CheckedPlace,
    tc: &mut TypeChecker,
) {
    if let Some(error) = target.value.access.assign_error(
        assignment_target_name(&assign.node.target),
        tc.error_span(assign.node.target.span),
    ) {
        tc.push_error(error);
    }
    if let Some(error) = collection_loan::root_rebind_error(
        &tc.active_collection_loans,
        &target.value.identity,
        tc.error_span(assign.node.target.span),
    ) {
        tc.push_error(error);
    }
    let value = check_expected_value_expr(&assign.node.value, target.checked().handle.clone(), tc);
    let flow_mode = tc.function_flow_mode(&value.ty, &target.checked().ty, false);
    if flow_mode != FunctionFlowMode::None {
        tc.record_call_return_function_value(&assign.node.value, &target.checked().ty);
    }
    if !target.accepts_extern_any() {
        tc.reject_extern_any_escape(&value, assign.node.value.span);
    }
    if target.value.access.can_assign() {
        sync_assigned_flow(&assign.node.target, &assign.node.value, flow_mode, tc);
        place::record_write(assign.node.target.node.id, target, tc);
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
    pub(crate) fn add_type(&mut self, ty: &Type) {
        let facts = type_closure_facts(ty);
        self.types.infer.contains_type |= facts.infer.contains_type;
        self.types.infer.contains_return |= facts.infer.contains_return;
        self.types.first_unresolved = self.types.first_unresolved.or(facts.first_unresolved);
        self.types.contains_unresolved_const |= facts.contains_unresolved_const;
    }

    pub(crate) fn add_const_term(&mut self, arg: &ConstTerm) {
        match arg {
            ConstTerm::Name(_) => self.consts.contains_unresolved = true,
            ConstTerm::ArrayInfer | ConstTerm::Infer(_) => self.consts.contains_infer = true,
            ConstTerm::Value(_) | ConstTerm::Param(_) => {}
        }
    }

    pub(crate) fn add_generic_args(&mut self, args: &GenericArgs) {
        for ty in &args.type_args {
            self.add_type(ty);
        }
        for arg in &args.const_args {
            self.add_const_term(arg);
        }
    }

    pub(crate) fn contains_unresolved_const(&self) -> bool {
        self.types.contains_unresolved_const || self.consts.contains_unresolved
    }

    pub(crate) fn is_empty(&self) -> bool {
        !self.types.infer.contains_type
            && !self.types.infer.contains_return
            && self.types.first_unresolved.is_none()
            && !self.contains_unresolved_const()
            && !self.consts.contains_infer
    }

    fn push_errors(&self, errors: &mut Vec<TypeError>, span: Option<SourceSpan>) {
        if self.is_empty() {
            return;
        }
        if let Some(unresolved) = self.types.first_unresolved {
            errors.push(TypeError::UnknownType {
                qualifier: unresolved.qualifier,
                name: unresolved.name,
                span,
            });
        } else if self.types.infer.contains_return {
            errors.push(TypeError::InferReturnValue { span });
        } else if self.types.infer.contains_type {
            errors.push(TypeError::CannotInferType { span });
        } else if self.contains_unresolved_const() || self.consts.contains_infer {
            errors.push(TypeError::CannotInferConst { span });
        }
    }
}

pub(crate) fn call_target_closure_facts(target: &CallTarget) -> CallTargetClosureFacts {
    let mut facts = CallTargetClosureFacts::default();
    facts.add_generic_args(&target.args);
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
    facts.push_errors(errors, span);
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
