// exports body type used in Function
use anvyx_externs::{
    ExternBindingKey, ExternEffects, ExternTypeExpr, ExternTypeKey, ProviderId, ProviderPackageKey,
};

pub use super::body::AirBody;
use super::{
    body::Place,
    ids::*,
    types::{
        BinaryOp, ConstValue, ParamEscape, ParamMode, ParamType, ReturnMode, SignatureType, UnaryOp,
    },
};
use crate::{
    ast::{ExprId, Ident},
    span::SourceSpan,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ContractSurfaceDecl {
    pub display_name: String,
    pub slots: Vec<ContractSlotDecl>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ContractSlotDecl {
    pub id: ContractSlotId,
    pub name: Ident,
    pub receiver: ContractReceiver,
    pub params: Vec<ContractParamDecl>,
    pub ret: ContractReturnDecl,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ContractReceiver {
    Value,
    Ref,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ContractParamDecl {
    pub ty: TypeId,
    pub mode: ParamMode,
    pub cast_accept: bool,
    pub escape: ParamEscape,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ContractReturnDecl {
    Value(TypeId),
    Place(TypeId),
    Iter,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ContractWitnessDecl {
    pub key: ContractWitnessKey,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ContractWitnessKey {
    pub concrete_ty: TypeId,
    pub surface: ContractSurfaceId,
    pub slots: Vec<ContractWitnessSlotDecl>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ContractWitnessSlotDecl {
    pub slot: ContractSlotId,
    pub receiver: ParamMode,
    pub target: ContractWitnessTarget,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ContractWitnessTarget {
    Function {
        function: FunctionId,
    },
    IteratorFunction {
        function: FunctionId,
    },
    Extern {
        function: ExternId,
    },
    Promoted {
        fields: Vec<FieldId>,
        target: Box<ContractWitnessTarget>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ContractWeakeningDecl {
    pub source: ContractSurfaceId,
    pub target: ContractSurfaceId,
    pub target_to_source: Vec<ContractSlotId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Module {
    pub path: Vec<Ident>,
    pub functions: Vec<FunctionId>,
    pub globals: Vec<GlobalId>,
    pub aggregates: Vec<AggregateId>,
    pub enums: Vec<EnumId>,
    pub flags: Vec<FlagId>,
    pub extern_types: Vec<ExternTypeId>,
    pub externs: Vec<ExternId>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Ident,
    pub module: ModuleId,
    pub kind: FunctionKind,
    pub owner: Option<FunctionOwner>,
    pub specialization: Option<FunctionSpecialization>,
    pub signature: Signature,
    pub locals: Vec<Local>,
    pub body: AirBody,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSpecialization {
    pub type_args: Vec<TypeId>,
    pub const_args: Vec<ConstValue>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionOwner {
    pub name: Ident,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionKind {
    Normal,
    Method,
    ExtendMethod,
    Lambda(LambdaId),
    Helper,
    GlobalInit(GlobalId),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LambdaDecl {
    pub source: ExprId,
    pub module: ModuleId,
    pub owner: FunctionId,
    pub body: FunctionId,
    pub signature: SignatureType,
    pub escape: LambdaEscape,
    pub captures: Vec<LambdaCaptureDecl>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CaptureLocalSource {
    pub owner: FunctionId,
    pub local: LocalId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LambdaCaptureDecl {
    NoRuntime {
        binding: BindingId,
        ty: TypeId,
    },
    ReadonlyLocal {
        binding: BindingId,
        source: CaptureLocalSource,
        ty: TypeId,
    },
    ScopedLocal {
        binding: BindingId,
        source: CaptureLocalSource,
        ty: TypeId,
        mutability: Mutability,
    },
    ScopedBorrow {
        binding: BindingId,
        borrow: ScopedBorrowId,
        ty: TypeId,
        mutability: Mutability,
    },
    CaptureCell {
        binding: BindingId,
        cell: CaptureCellId,
        ty: TypeId,
    },
}

impl LambdaCaptureDecl {
    pub fn binding(&self) -> BindingId {
        match self {
            Self::NoRuntime { binding, .. }
            | Self::ReadonlyLocal { binding, .. }
            | Self::ScopedLocal { binding, .. }
            | Self::ScopedBorrow { binding, .. }
            | Self::CaptureCell { binding, .. } => *binding,
        }
    }

    pub fn ty(&self) -> TypeId {
        match self {
            Self::NoRuntime { ty, .. }
            | Self::ReadonlyLocal { ty, .. }
            | Self::ScopedLocal { ty, .. }
            | Self::ScopedBorrow { ty, .. }
            | Self::CaptureCell { ty, .. } => *ty,
        }
    }

    pub fn mutability(&self) -> Mutability {
        match self {
            Self::ScopedLocal { mutability, .. } | Self::ScopedBorrow { mutability, .. } => {
                *mutability
            }
            Self::CaptureCell { .. } => Mutability::Mutable,
            Self::NoRuntime { .. } | Self::ReadonlyLocal { .. } => Mutability::Immutable,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LambdaEscape {
    NonEscaping,
    Escaping,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Signature {
    pub params: Vec<Param>,
    pub return_mode: ReturnMode,
}

impl Signature {
    pub fn new(params: Vec<Param>, return_type: TypeId) -> Self {
        Self::with_return_mode(params, ReturnMode::Value(return_type))
    }

    pub fn with_return_mode(params: Vec<Param>, return_mode: ReturnMode) -> Self {
        Self {
            params,
            return_mode,
        }
    }

    pub fn return_type(&self) -> TypeId {
        self.return_mode.ty()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: Option<Ident>,
    pub ty: TypeId,
    pub mode: ParamMode,
    pub escape: ParamEscape,
    pub role: ParamRole,
    pub local_id: LocalId,
}

impl Param {
    pub fn param_type(&self) -> ParamType {
        ParamType {
            ty: self.ty,
            mode: self.mode,
            escape: self.escape,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamRole {
    Normal,
    Receiver,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Local {
    pub name: Option<Ident>,
    pub binding: Option<BindingId>,
    pub ty: TypeId,
    pub mutability: Mutability,
    pub kind: LocalKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mutability {
    Immutable,
    Mutable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LocalKind {
    Arg,
    Return,
    Temp,
    User,
    PatternBinding,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DynBorrowParamDecl {
    pub owner: FunctionId,
    pub source: LocalId,
    pub ty: TypeId,
    pub surface: ContractSurfaceId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScopedBorrowDecl {
    pub owner: FunctionId,
    pub binding: BindingId,
    pub source: ScopedBorrowSource,
    pub ty: TypeId,
    pub mutability: Mutability,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScopedBorrowSource {
    SourceMutParam { local: LocalId },
    RefSelf { local: LocalId },
    PatternAlias { source: Place },
    ForRefAlias { source: Place },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CaptureCellDecl {
    pub binding: BindingId,
    pub owner: FunctionId,
    pub source_local: LocalId,
    pub ty: TypeId,
    pub lifetime: CaptureCellLifetime,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CaptureCellLifetime {
    Function,
    Loop { loop_id: AirLoopId },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalDecl {
    pub name: Ident,
    pub module: ModuleId,
    pub ty: TypeId,
    pub mutability: Mutability,
    pub init: FunctionId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AggregateDecl {
    pub name: Ident,
    pub module: ModuleId,
    pub kind: AggregateKind,
    pub type_args: Vec<TypeId>,
    pub const_args: Vec<String>,
    pub fields: Vec<FieldDecl>,
    pub cycle_capable: bool,
    pub stringify_override: Option<FunctionId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldDecl {
    pub name: Ident,
    pub ty: TypeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AggregateKind {
    Struct,
    DataRef,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EnumRepr {
    Adt,
    RawInt,
    RawString,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RawEnumValue {
    Int(i64),
    String(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumDecl {
    pub name: Ident,
    pub module: ModuleId,
    pub type_args: Vec<TypeId>,
    pub const_args: Vec<String>,
    pub core: Option<CoreEnumKind>,
    pub repr: EnumRepr,
    pub raw_type: Option<TypeId>,
    pub variants: Vec<VariantDecl>,
}

impl EnumDecl {
    pub fn is_unit_only(&self) -> bool {
        self.variants
            .iter()
            .all(|variant| matches!(variant.shape, VariantShape::Unit))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FlagDecl {
    pub name: Ident,
    pub module: ModuleId,
    pub known_bits: i64,
    pub members: Vec<FlagMemberDecl>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FlagMemberDecl {
    pub id: FlagMemberId,
    pub name: Ident,
    pub value: i64,
    pub atomic: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CoreEnumKind {
    Option,
    Result,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariantDecl {
    pub name: Ident,
    pub shape: VariantShape,
    pub raw_value: Option<RawEnumValue>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VariantShape {
    Unit,
    Tuple(Vec<TypeId>),
    Struct(Vec<FieldDecl>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternDecl {
    pub name: Ident,
    pub span: Option<SourceSpan>,
    pub module: ModuleId,
    pub member: ExternMember,
    pub params: Vec<ExternParamDecl>,
    pub return_type: TypeId,
    pub abi: ExternAbi,
    pub binding: Option<ExternBindingDecl>,
    pub effects: ExternEffects,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternAbi {
    pub params: Vec<ExternTypeExpr>,
    pub ret: ExternTypeExpr,
}

impl Default for ExternAbi {
    fn default() -> Self {
        Self {
            params: vec![],
            ret: ExternTypeExpr::Void,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternBindingDecl {
    pub package: ProviderPackageKey,
    pub provider: ProviderId,
    pub key: ExternBindingKey,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternTypeBindingDecl {
    pub package: ProviderPackageKey,
    pub provider: ProviderId,
    pub key: ExternTypeKey,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternParamDecl {
    pub ty: TypeId,
    pub mode: ParamMode,
    pub escape: ParamEscape,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExternReceiverDecl {
    pub ty: TypeId,
    pub mode: ParamMode,
}

impl ExternDecl {
    pub fn call_params(&self) -> impl Iterator<Item = ParamType> + '_ {
        self.receiver_param()
            .into_iter()
            .chain(self.params.iter().map(ExternParamDecl::param_type))
    }

    pub fn call_arity(&self) -> usize {
        self.params.len() + usize::from(self.receiver_param().is_some())
    }

    pub fn call_param(&self, index: usize) -> Option<ParamType> {
        match self.receiver_param() {
            Some(receiver) if index == 0 => Some(receiver),
            Some(_) => self.params.get(index - 1).map(ExternParamDecl::param_type),
            None => self.params.get(index).map(ExternParamDecl::param_type),
        }
    }

    fn receiver_param(&self) -> Option<ParamType> {
        match &self.member {
            ExternMember::FieldGetter { receiver, .. }
            | ExternMember::FieldSetter { receiver, .. }
            | ExternMember::Method { receiver, .. }
            | ExternMember::UnaryOperator { receiver, .. }
            | ExternMember::BinaryOperator { receiver, .. } => Some(receiver.param_type()),
            ExternMember::FreeFunction
            | ExternMember::StaticMethod { .. }
            | ExternMember::Init { .. } => None,
        }
    }
}

impl ExternParamDecl {
    pub fn param_type(&self) -> ParamType {
        ParamType {
            ty: self.ty,
            mode: self.mode,
            escape: self.escape,
        }
    }
}

impl ExternReceiverDecl {
    pub fn param_type(&self) -> ParamType {
        ParamType {
            ty: self.ty,
            mode: self.mode,
            escape: ParamEscape::NonEscaping,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExternMember {
    FreeFunction,
    FieldGetter {
        owner: ExternTypeId,
        receiver: ExternReceiverDecl,
        computed: bool,
    },
    FieldSetter {
        owner: ExternTypeId,
        receiver: ExternReceiverDecl,
    },
    Method {
        owner: ExternTypeId,
        receiver: ExternReceiverDecl,
    },
    StaticMethod {
        owner: ExternTypeId,
    },
    Init {
        owner: ExternTypeId,
    },
    UnaryOperator {
        owner: ExternTypeId,
        receiver: ExternReceiverDecl,
        op: UnaryOp,
    },
    BinaryOperator {
        owner: ExternTypeId,
        receiver: ExternReceiverDecl,
        op: BinaryOp,
        self_on_right: bool,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternTypeDecl {
    pub name: Ident,
    pub module: ModuleId,
    pub binding: Option<ExternTypeBindingDecl>,
    pub type_args: Vec<TypeId>,
    pub const_args: Vec<String>,
    pub rep: ExternRep,
    pub layout: Option<anvyx_externs::ExternLayout>,
    pub materialization: Option<anvyx_externs::ExternMaterialization>,
    pub owns_heap_edges: Option<bool>,
    pub has_init: bool,
    pub init_args: Vec<ExternInitArgDecl>,
    pub fields: Vec<ExternFieldDecl>,
    pub variants: Vec<VariantDecl>,
    pub variant_abis: Vec<ExternVariantAbiDecl>,
    pub methods: Vec<ExternMethodDecl>,
    pub statics: Vec<ExternStaticDecl>,
    pub operators: Vec<ExternOpDecl>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternInitArgDecl {
    pub field: FieldId,
    pub param: usize,
    pub presence: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternVariantAbiDecl {
    pub fields: Vec<ExternTypeExpr>,
}

impl ExternTypeDecl {
    pub fn required_init_fields(
        &self,
    ) -> Option<impl Iterator<Item = (&ExternInitArgDecl, &ExternFieldDecl)>> {
        self.has_init.then(|| {
            self.init_args
                .iter()
                .filter(|arg| !arg.presence)
                .map(|arg| (arg, &self.fields[arg.field.index()]))
        })
    }

    pub fn presence_init_fields(
        &self,
    ) -> Option<impl Iterator<Item = (&ExternInitArgDecl, &ExternFieldDecl)>> {
        self.has_init.then(|| {
            self.init_args
                .iter()
                .filter(|arg| arg.presence)
                .map(|arg| (arg, &self.fields[arg.field.index()]))
        })
    }

    pub fn constructor_fields(&self) -> Option<impl Iterator<Item = (FieldId, &ExternFieldDecl)>> {
        self.required_init_fields()
            .map(|fields| fields.map(|(arg, field)| (arg.field, field)))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExternRep {
    Shared,
    Inline,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternFieldDecl {
    pub name: Ident,
    pub ty: TypeId,
    pub abi: ExternTypeExpr,
    pub get_receiver: ExternReceiverDecl,
    pub set_receiver: ExternReceiverDecl,
    pub computed: bool,
    pub readable: bool,
    pub writable: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternMethodDecl {
    pub name: Ident,
    pub receiver: ExternReceiverDecl,
    pub params: Vec<ExternParamDecl>,
    pub return_type: TypeId,
    pub abi: ExternAbi,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternStaticDecl {
    pub name: Ident,
    pub params: Vec<ExternParamDecl>,
    pub return_type: TypeId,
    pub abi: ExternAbi,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternOpDecl {
    pub kind: ExternOp,
    pub receiver: ExternReceiverDecl,
    pub operand: Option<ExternParamDecl>,
    pub return_type: TypeId,
    pub abi: ExternAbi,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExternOp {
    Binary { op: BinaryOp, self_on_right: bool },
    Unary(UnaryOp),
}
