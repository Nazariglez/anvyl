use std::{error::Error, fmt};

use anvyx_frontend::{
    air::{self, FunctionId},
    ast::{BinaryOp, UnaryOp},
};
use anvyx_runtime::{ExternTypeExpr, ExternTypeKey, RustExternAbi, RustParamAbi, RustReturnAbi};

macro_rules! rir_id {
    ($name:ident) => {
        #[repr(transparent)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
        pub struct $name(pub u32);

        impl $name {
            pub const fn from_index(index: usize) -> Self {
                Self(index as u32)
            }

            pub const fn index(self) -> usize {
                self.0 as usize
            }
        }
    };
}

rir_id!(RirFunctionId);
rir_id!(RirExternId);
rir_id!(RirTypeId);
rir_id!(RirConstId);
rir_id!(RirLocalId);
rir_id!(RirStructId);
rir_id!(RirEnumId);
rir_id!(RirVariantId);
rir_id!(RirFieldId);
rir_id!(RirStringifyHelperId);
rir_id!(RirStringifyReqId);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirSymbol(pub String);

impl RirSymbol {
    pub fn new(text: impl Into<String>) -> Self {
        Self(text.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct RirProgram {
    pub ctx: RirCtxPlan,
    pub functions: Vec<RirFunction>,
    pub externs: Vec<RirExtern>,
    pub types: Vec<RirType>,
    pub structs: Vec<RirStruct>,
    pub enums: Vec<RirEnum>,
    pub stringify_reqs: Vec<RirStringifyReq>,
    pub stringify_helpers: Vec<RirStringifyHelper>,
    pub consts: Vec<RirConst>,
    pub entry: Option<RirFunctionId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirStruct {
    pub id: RirStructId,
    pub air_id: Option<air::AggregateId>,
    pub symbol: RirSymbol,
    pub display: RirSymbol,
    pub native_path: Option<Vec<String>>,
    pub native_key: Option<ExternTypeKey>,
    pub copyable: bool,
    pub fields: Vec<RirField>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirField {
    pub id: RirFieldId,
    pub symbol: RirSymbol,
    pub ty: RirTypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirEnum {
    pub id: RirEnumId,
    pub air_id: Option<air::EnumId>,
    pub core: Option<RirCoreEnumKind>,
    pub symbol: RirSymbol,
    pub display: RirSymbol,
    pub copyable: bool,
    pub variants: Vec<RirVariant>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirCoreEnumKind {
    Option,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirVariant {
    pub id: RirVariantId,
    pub symbol: RirSymbol,
    pub display: RirSymbol,
    pub kind: RirVariantKind,
    pub fields: Vec<RirField>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirVariantKind {
    Unit,
    Tuple,
    Struct,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirStringifyReq {
    pub id: RirStringifyReqId,
    pub ty: RirTypeId,
    pub kind: RirStringifyReqKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirStringifyReqKind {
    Structural(RirStringifyHelperId),
    Override {
        function: RirFunctionId,
        mode: RirParamSemantic,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirStringifyHelper {
    pub id: RirStringifyHelperId,
    pub ty: RirTypeId,
    pub symbol: RirSymbol,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirCtxPlan {
    pub symbol: RirSymbol,
}

impl Default for RirCtxPlan {
    fn default() -> Self {
        Self {
            symbol: RirSymbol::new("AnvCtx"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RirFunction {
    pub id: RirFunctionId,
    pub air_id: Option<FunctionId>,
    pub symbol: RirSymbol,
    pub params: Vec<RirParam>,
    pub ret: RirReturn,
    pub locals: Vec<RirLocal>,
    pub body: RirStructuredBlock,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RirParam {
    pub local: RirLocalId,
    pub ty: RirTypeId,
    pub semantic: RirParamSemantic,
    pub abi: RirParamAbi,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirParamSemantic {
    Value,
    SharedBorrow,
    MutBorrow,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirParamAbi {
    Value,
    SharedBorrow,
    MutBorrow,
    PlaceHandle,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RirReturn {
    pub ty: RirTypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirLocal {
    pub id: RirLocalId,
    pub ty: RirTypeId,
    pub mutable: bool,
    pub symbol: RirSymbol,
    pub initialized: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RirStmt {
    Init { local: RirLocalId, value: RirRValue },
    Assign { dst: RirPlace, value: RirRValue },
    Eval(RirRValue),
    If(RirIf),
    EnumMatch(RirEnumMatch),
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct RirStructuredBlock {
    pub stmts: Vec<RirStmt>,
    pub term: RirTerm,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RirIf {
    pub cond: RirOperand,
    pub then_block: RirStructuredBlock,
    pub else_block: Option<RirStructuredBlock>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RirEnumMatch {
    pub discr: RirPlace,
    pub arms: Vec<RirEnumMatchArm>,
    pub else_block: Option<RirStructuredBlock>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RirEnumMatchArm {
    pub variant: RirVariantId,
    pub block: RirStructuredBlock,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RirRValue {
    Use(RirOperand),
    Struct {
        ty: RirTypeId,
        fields: Vec<RirOperand>,
    },
    Array {
        ty: RirTypeId,
        elems: Vec<RirOperand>,
    },
    List {
        ty: RirTypeId,
        elems: Vec<RirOperand>,
    },
    EnumVariant {
        ty: RirTypeId,
        variant: RirVariantId,
        fields: Vec<RirOperand>,
    },
    Unary {
        op: UnaryOp,
        value: RirOperand,
        ty: RirTypeId,
    },
    Binary {
        op: BinaryOp,
        lhs: RirOperand,
        rhs: RirOperand,
        ty: RirTypeId,
    },
    Cast {
        value: RirOperand,
        target: RirTypeId,
    },
    Call {
        callee: RirCallTarget,
        args: Vec<RirCallArg>,
        ty: RirTypeId,
    },
    Stringify {
        value: RirOperand,
        source_ty: RirTypeId,
    },
    StringConcat {
        parts: Vec<RirOperand>,
    },
    Format {
        value: RirOperand,
        source_ty: RirTypeId,
        spec: RirFormatSpec,
    },
    Len {
        source: RirPlace,
    },
    ListPush {
        list: RirPlace,
        value: RirOperand,
    },
    SliceView {
        source: RirPlace,
        start: RirLocalId,
        end: RirLocalId,
        inclusive: bool,
        ty: RirTypeId,
    },
    ListSlice {
        source: RirPlace,
        start: RirLocalId,
        end: RirLocalId,
        inclusive: bool,
        ty: RirTypeId,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct RirFormatSpec {
    pub fill: char,
    pub align: Option<RirFormatAlign>,
    pub sign: RirFormatSign,
    pub zero_pad: bool,
    pub width: Option<u32>,
    pub precision: Option<u32>,
    pub kind: RirFormatKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirFormatAlign {
    Left,
    Right,
    Center,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum RirFormatSign {
    #[default]
    Default,
    Always,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum RirFormatKind {
    #[default]
    Default,
    Hex,
    HexUpper,
    Binary,
    Exp,
    ExpUpper,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirCallTarget {
    Function(RirFunctionId),
    Extern(RirExternId),
}

#[derive(Debug, Clone, PartialEq)]
pub enum RirCallArg {
    Value(RirOperand),
    SharedBorrow(RirPlace),
    SharedStringConst(RirConstId),
    MutBorrow(RirPlace),
}

impl RirCallArg {
    pub fn semantic(&self) -> RirParamSemantic {
        match self {
            Self::Value(_) => RirParamSemantic::Value,
            Self::SharedBorrow(_) | Self::SharedStringConst(_) => RirParamSemantic::SharedBorrow,
            Self::MutBorrow(_) => RirParamSemantic::MutBorrow,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirPlace {
    pub local: RirLocalId,
    pub projections: Vec<RirProjection>,
    pub ty: RirTypeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirProjection {
    Field(RirFieldId),
    Index(RirLocalId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirOperand {
    Place(RirPlace),
    Const(RirConstId),
}

#[derive(Debug, Clone, PartialEq, Default)]
pub enum RirTerm {
    #[default]
    None,
    Return(Option<RirOperand>),
    Unreachable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirType {
    Int,
    Float,
    Bool,
    String,
    Void,
    Struct(RirStructId),
    Enum(RirEnumId),
    Array { elem: RirTypeId, len: u64 },
    List(RirTypeId),
    Slice(RirTypeId),
}

#[derive(Debug, Clone, PartialEq)]
pub struct RirConst {
    pub id: RirConstId,
    pub ty: RirTypeId,
    pub value: RirConstValue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RirConstValue {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct RirExtern {
    pub id: RirExternId,
    pub symbol: RirSymbol,
    pub kind: RirExternKind,
    pub params: Vec<RirExternParam>,
    pub ret: RirTypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirExternKind {
    Native(RirNativeExtern),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirNativeExtern {
    pub path: Vec<String>,
    pub abi: RustExternAbi,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RirExternParam {
    pub ty: RirTypeId,
    pub semantic: RirParamSemantic,
    pub abi: RirParamAbi,
}

#[derive(Debug, Clone, Copy)]
pub struct VerifiedRirProgram<'a> {
    program: &'a RirProgram,
}

impl<'a> VerifiedRirProgram<'a> {
    pub fn program(&self) -> &'a RirProgram {
        self.program
    }
}

pub fn verify(program: &RirProgram) -> Result<VerifiedRirProgram<'_>, Vec<RirVerifyError>> {
    let mut cx = VerifyCx {
        program,
        errors: vec![],
        initialized: vec![],
        possibly_initialized: vec![],
    };
    cx.check();
    if cx.errors.is_empty() {
        Ok(VerifiedRirProgram { program })
    } else {
        Err(cx.errors)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RirVerifyError {
    pub site: RirVerifySite,
    pub kind: RirVerifyErrorKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RirVerifySite {
    Program,
    Context,
    Type(RirTypeId),
    Const(RirConstId),
    Extern(RirExternId),
    Function(RirFunctionId),
    Param(RirFunctionId, usize),
    Local(RirFunctionId, RirLocalId),
    Statement(RirFunctionId, usize),
    RValue(RirFunctionId, usize),
    CallArg(RirFunctionId, usize, usize),
    Terminator(RirFunctionId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RirVerifyErrorKind {
    BadId,
    TypeMismatch {
        expected: RirTypeId,
        found: RirTypeId,
    },
    ConstTypeMismatch,
    VoidConst,
    DuplicateSymbol,
    ReturnValueRequired,
    UnexpectedReturnValue,
    ImmutableAssign,
    ParamLocalMissing,
    ParamLocalNotInitialized,
    InitParamLocal,
    UninitializedLocal(RirLocalId),
    UnsupportedAbi,
    UnsupportedRValueType,
    NonCopyValueRequired,
    FieldCount {
        expected: usize,
        found: usize,
    },
    CallArgCount {
        expected: usize,
        found: usize,
    },
    CallArgMode,
    DuplicateMatchArm,
    MatchNotExhaustive,
}

impl fmt::Display for RirVerifyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}: {:?}", self.site, self.kind)
    }
}

impl Error for RirVerifyError {}

struct VerifyCx<'a> {
    program: &'a RirProgram,
    errors: Vec<RirVerifyError>,
    initialized: Vec<bool>,
    possibly_initialized: Vec<bool>,
}

fn native_extern_signature_ok(
    program: &RirProgram,
    native: &RirNativeExtern,
    ext: &RirExtern,
    void: Option<RirTypeId>,
) -> bool {
    !native.path.is_empty()
        && native.path.iter().all(|segment| !segment.is_empty())
        && native.abi.support == anvyx_runtime::RustAbiSupport::Direct
        && native.abi.params.len() == ext.params.len()
        && native
            .abi
            .params
            .iter()
            .zip(&ext.params)
            .all(|(abi, param)| native_param_abi_ok(program, abi, *param))
        && native_return_abi_ok(program, &native.abi.ret, ext.ret, void)
}

fn native_param_abi_ok(program: &RirProgram, abi: &RustParamAbi, param: RirExternParam) -> bool {
    match abi {
        RustParamAbi::Value(ty) => {
            param.abi == RirParamAbi::Value && rir_type_matches_extern(program, param.ty, ty)
        }
        RustParamAbi::Borrow(ty) => {
            param.abi == RirParamAbi::SharedBorrow && rir_type_matches_extern(program, param.ty, ty)
        }
        RustParamAbi::MutBorrow(ty) => {
            param.abi == RirParamAbi::MutBorrow && rir_type_matches_extern(program, param.ty, ty)
        }
        RustParamAbi::Option(_) | RustParamAbi::List(_) => false,
    }
}

fn native_return_abi_ok(
    program: &RirProgram,
    abi: &RustReturnAbi,
    ret: RirTypeId,
    void: Option<RirTypeId>,
) -> bool {
    match abi {
        RustReturnAbi::Void => Some(ret) == void,
        RustReturnAbi::Value(ty) => rir_type_matches_extern(program, ret, ty),
        RustReturnAbi::Option(inner) => rir_type_matches_option(program, ret, inner),
        RustReturnAbi::List(_) => false,
    }
}

fn rir_type_matches_extern(program: &RirProgram, id: RirTypeId, expected: &ExternTypeExpr) -> bool {
    let Some(found) = program.types.get(id.index()) else {
        return false;
    };
    match (found, expected) {
        (RirType::Void, ExternTypeExpr::Void)
        | (RirType::Bool, ExternTypeExpr::Bool)
        | (RirType::Int, ExternTypeExpr::Int)
        | (RirType::Float, ExternTypeExpr::Float)
        | (RirType::String, ExternTypeExpr::String) => true,
        (RirType::Struct(struct_id), ExternTypeExpr::Named { module, name, args }) => {
            let strukt = &program.structs[struct_id.index()];
            args.is_empty()
                && strukt.native_key.as_ref().is_some_and(|key| {
                    module.as_ref().is_none_or(|module| key.module == *module) && key.name == *name
                })
        }
        _ => false,
    }
}

fn rir_type_matches_option(program: &RirProgram, id: RirTypeId, inner: &RustReturnAbi) -> bool {
    let Some(RirType::Enum(enum_id)) = program.types.get(id.index()) else {
        return false;
    };
    let enm = &program.enums[enum_id.index()];
    if enm.core != Some(RirCoreEnumKind::Option) {
        return false;
    }
    let [none, some] = enm.variants.as_slice() else {
        return false;
    };
    if none.kind != RirVariantKind::Unit || some.kind != RirVariantKind::Tuple {
        return false;
    }
    let [field] = some.fields.as_slice() else {
        return false;
    };
    match inner {
        RustReturnAbi::Value(ty) => rir_type_matches_extern(program, field.ty, ty),
        RustReturnAbi::Void | RustReturnAbi::Option(_) | RustReturnAbi::List(_) => false,
    }
}

impl VerifyCx<'_> {
    fn check(&mut self) {
        if let Some(entry) = self.program.entry {
            self.check_function_id(RirVerifySite::Program, entry);
        }
        if self.program.ctx.symbol.as_str().is_empty() {
            self.push(RirVerifySite::Context, RirVerifyErrorKind::BadId);
        }
        for (index, ty) in self.program.types.iter().enumerate() {
            let site = RirVerifySite::Type(RirTypeId::from_index(index));
            self.check_type_id(site, RirTypeId::from_index(index));
            match ty {
                RirType::Struct(id) => self.check_struct_id(site, *id),
                RirType::Enum(id) => self.check_enum_id(site, *id),
                RirType::Array { elem, .. } | RirType::List(elem) | RirType::Slice(elem) => {
                    self.check_type_id(site, *elem);
                }
                _ => {}
            }
        }
        self.check_structs();
        self.check_enums();
        self.check_stringify_helpers();
        for (index, konst) in self.program.consts.iter().enumerate() {
            let id = RirConstId::from_index(index);
            if konst.id != id {
                self.push(RirVerifySite::Const(id), RirVerifyErrorKind::BadId);
            }
            self.check_const(id, konst);
        }
        for (index, ext) in self.program.externs.iter().enumerate() {
            let id = RirExternId::from_index(index);
            if ext.id != id {
                self.push(RirVerifySite::Extern(id), RirVerifyErrorKind::BadId);
            }
            self.check_extern(id, ext);
        }
        for (index, function) in self.program.functions.iter().enumerate() {
            let id = RirFunctionId::from_index(index);
            if function.id != id {
                self.push(RirVerifySite::Function(id), RirVerifyErrorKind::BadId);
            }
            self.check_function(id, function);
        }
    }

    fn check_structs(&mut self) {
        let mut symbols = Vec::new();
        for (index, strukt) in self.program.structs.iter().enumerate() {
            let id = RirStructId::from_index(index);
            let site = RirVerifySite::Type(RirTypeId::from_index(index));
            if strukt.id != id {
                self.push(site, RirVerifyErrorKind::BadId);
            }
            if strukt.symbol.as_str().is_empty()
                || symbols.iter().any(|symbol| symbol == &strukt.symbol)
            {
                self.push(site, RirVerifyErrorKind::DuplicateSymbol);
            }
            symbols.push(strukt.symbol.clone());
            let mut field_symbols = Vec::new();
            for (field_index, field) in strukt.fields.iter().enumerate() {
                if field.id != RirFieldId::from_index(field_index) {
                    self.push(site, RirVerifyErrorKind::BadId);
                }
                if field.symbol.as_str().is_empty()
                    || field_symbols.iter().any(|symbol| symbol == &field.symbol)
                {
                    self.push(site, RirVerifyErrorKind::DuplicateSymbol);
                }
                field_symbols.push(field.symbol.clone());
                self.check_type_id(site, field.ty);
                if strukt.copyable && !self.inherently_copyable_type(field.ty) {
                    self.push(site, RirVerifyErrorKind::NonCopyValueRequired);
                }
            }
        }
    }

    fn check_enums(&mut self) {
        let mut symbols = Vec::new();
        for (index, enm) in self.program.enums.iter().enumerate() {
            let id = RirEnumId::from_index(index);
            let site = RirVerifySite::Type(RirTypeId::from_index(index));
            if enm.id != id {
                self.push(site, RirVerifyErrorKind::BadId);
            }
            if enm.symbol.as_str().is_empty() || symbols.iter().any(|symbol| symbol == &enm.symbol)
            {
                self.push(site, RirVerifyErrorKind::DuplicateSymbol);
            }
            symbols.push(enm.symbol.clone());
            let mut variant_symbols = Vec::new();
            for (variant_index, variant) in enm.variants.iter().enumerate() {
                if variant.id != RirVariantId::from_index(variant_index) {
                    self.push(site, RirVerifyErrorKind::BadId);
                }
                if variant.symbol.as_str().is_empty()
                    || variant_symbols
                        .iter()
                        .any(|symbol| symbol == &variant.symbol)
                {
                    self.push(site, RirVerifyErrorKind::DuplicateSymbol);
                }
                variant_symbols.push(variant.symbol.clone());
                if variant.kind == RirVariantKind::Unit && !variant.fields.is_empty() {
                    self.push(
                        site,
                        RirVerifyErrorKind::FieldCount {
                            expected: 0,
                            found: variant.fields.len(),
                        },
                    );
                }
                let mut field_symbols = Vec::new();
                for (field_index, field) in variant.fields.iter().enumerate() {
                    if field.id != RirFieldId::from_index(field_index) {
                        self.push(site, RirVerifyErrorKind::BadId);
                    }
                    if field.symbol.as_str().is_empty()
                        || field_symbols.iter().any(|symbol| symbol == &field.symbol)
                    {
                        self.push(site, RirVerifyErrorKind::DuplicateSymbol);
                    }
                    field_symbols.push(field.symbol.clone());
                    self.check_type_id(site, field.ty);
                    if matches!(self.ty(field.ty), Some(RirType::Void)) {
                        self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    }
                    if enm.copyable && !self.copyable_type(field.ty) {
                        self.push(site, RirVerifyErrorKind::NonCopyValueRequired);
                    }
                }
            }
        }
    }

    fn check_stringify_helpers(&mut self) {
        let mut symbols = Vec::new();
        let mut tys = Vec::new();
        for (index, req) in self.program.stringify_reqs.iter().enumerate() {
            let site = RirVerifySite::Type(req.ty);
            if req.id != RirStringifyReqId::from_index(index) {
                self.push(site, RirVerifyErrorKind::BadId);
            }
            self.check_type_id(site, req.ty);
            match (req.kind, self.ty(req.ty)) {
                (RirStringifyReqKind::Structural(helper), Some(RirType::Struct(_))) => {
                    self.check_stringify_helper_id(site, helper);
                    if self
                        .program
                        .stringify_helpers
                        .get(helper.index())
                        .is_none_or(|helper| helper.ty != req.ty)
                    {
                        self.push(
                            site,
                            RirVerifyErrorKind::TypeMismatch {
                                expected: req.ty,
                                found: self
                                    .program
                                    .stringify_helpers
                                    .get(helper.index())
                                    .map_or(req.ty, |helper| helper.ty),
                            },
                        );
                    }
                }
                (RirStringifyReqKind::Override { function, mode }, Some(RirType::Struct(_))) => {
                    self.check_function_id(site, function);
                    let Some(function) = self.program.functions.get(function.index()) else {
                        continue;
                    };
                    if function.ret.ty != self.type_id(RirType::String).unwrap_or(req.ty) {
                        self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    }
                    if function.params.len() != 1 {
                        self.push(
                            site,
                            RirVerifyErrorKind::CallArgCount {
                                expected: 1,
                                found: function.params.len(),
                            },
                        );
                        continue;
                    }
                    let param = &function.params[0];
                    if param.ty != req.ty || param.semantic != mode {
                        self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    }
                    if !matches!(
                        mode,
                        RirParamSemantic::Value | RirParamSemantic::SharedBorrow
                    ) {
                        self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    }
                    if mode == RirParamSemantic::Value && !self.copyable_type(req.ty) {
                        self.push(site, RirVerifyErrorKind::NonCopyValueRequired);
                    }
                }
                _ => self.push(site, RirVerifyErrorKind::UnsupportedRValueType),
            }
        }
        for (index, helper) in self.program.stringify_helpers.iter().enumerate() {
            let site = RirVerifySite::Type(helper.ty);
            if helper.id != RirStringifyHelperId::from_index(index) {
                self.push(site, RirVerifyErrorKind::BadId);
            }
            self.check_type_id(site, helper.ty);
            if helper.symbol.as_str().is_empty()
                || symbols.iter().any(|symbol| symbol == &helper.symbol)
            {
                self.push(site, RirVerifyErrorKind::DuplicateSymbol);
            }
            symbols.push(helper.symbol.clone());
            if tys.contains(&helper.ty) {
                self.push(site, RirVerifyErrorKind::DuplicateSymbol);
            }
            tys.push(helper.ty);
            match self.ty(helper.ty) {
                Some(RirType::Struct(struct_id)) => {
                    let Some(strukt) = self.program.structs.get(struct_id.index()) else {
                        self.push(site, RirVerifyErrorKind::BadId);
                        continue;
                    };
                    for field in &strukt.fields {
                        match self.ty(field.ty) {
                            Some(
                                RirType::Int | RirType::Float | RirType::Bool | RirType::String,
                            ) => {}
                            Some(RirType::Struct(_))
                                if self.stringify_req(field.ty).is_some_and(|req| {
                                    matches!(req.kind, RirStringifyReqKind::Structural(helper)
                                        if self.program.stringify_helpers.get(helper.index()).is_some_and(|helper| helper.ty == field.ty))
                                }) => {}
                            Some(_) | None => {
                                self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                            }
                        }
                    }
                }
                _ => self.push(site, RirVerifyErrorKind::UnsupportedRValueType),
            }
        }
    }

    fn check_const(&mut self, id: RirConstId, konst: &RirConst) {
        self.check_type_id(RirVerifySite::Const(id), konst.ty);
        if matches!(self.ty(konst.ty), Some(RirType::Void)) {
            self.push(RirVerifySite::Const(id), RirVerifyErrorKind::VoidConst);
        }
        let ok = matches!(
            (self.ty(konst.ty), &konst.value),
            (Some(RirType::Int), RirConstValue::Int(_))
                | (Some(RirType::Float), RirConstValue::Float(_))
                | (Some(RirType::Bool), RirConstValue::Bool(_))
                | (Some(RirType::String), RirConstValue::String(_))
        );
        if !ok {
            self.push(
                RirVerifySite::Const(id),
                RirVerifyErrorKind::ConstTypeMismatch,
            );
        }
    }

    fn check_extern(&mut self, id: RirExternId, ext: &RirExtern) {
        self.check_type_id(RirVerifySite::Extern(id), ext.ret);
        self.check_extern_signature(id, ext);
        for param in &ext.params {
            self.check_type_id(RirVerifySite::Extern(id), param.ty);
            self.check_abi(
                RirVerifySite::Extern(id),
                param.ty,
                param.semantic,
                param.abi,
            );
        }
    }

    fn check_extern_signature(&mut self, id: RirExternId, ext: &RirExtern) {
        let site = RirVerifySite::Extern(id);
        let void = self.type_id(RirType::Void);
        let ok = match &ext.kind {
            RirExternKind::Native(native) => {
                native_extern_signature_ok(self.program, native, ext, void)
            }
        };
        if !ok {
            self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
        }
    }

    fn check_function(&mut self, id: RirFunctionId, function: &RirFunction) {
        self.check_type_id(RirVerifySite::Function(id), function.ret.ty);
        for (index, param) in function.params.iter().enumerate() {
            let site = RirVerifySite::Param(id, index);
            self.check_local_id(site, function, param.local);
            self.check_type_id(site, param.ty);
            self.check_abi(site, param.ty, param.semantic, param.abi);
            match function.locals.get(param.local.index()) {
                Some(local) if local.ty != param.ty => self.push(
                    site,
                    RirVerifyErrorKind::TypeMismatch {
                        expected: param.ty,
                        found: local.ty,
                    },
                ),
                Some(local) if !local.initialized => {
                    self.push(site, RirVerifyErrorKind::ParamLocalNotInitialized);
                }
                Some(_) => {}
                None => self.push(site, RirVerifyErrorKind::ParamLocalMissing),
            }
        }
        for (index, local) in function.locals.iter().enumerate() {
            let local_id = RirLocalId::from_index(index);
            if local.id != local_id {
                self.push(
                    RirVerifySite::Local(id, local_id),
                    RirVerifyErrorKind::BadId,
                );
            }
            self.check_type_id(RirVerifySite::Local(id, local_id), local.ty);
        }
        let previous_initialized = std::mem::take(&mut self.initialized);
        let previous_possible = std::mem::take(&mut self.possibly_initialized);
        self.initialized = function
            .locals
            .iter()
            .map(|local| local.initialized)
            .collect();
        self.possibly_initialized.clone_from(&self.initialized);
        for param in &function.params {
            if let Some(initialized) = self.initialized.get_mut(param.local.index()) {
                *initialized = true;
            }
            if let Some(possible) = self.possibly_initialized.get_mut(param.local.index()) {
                *possible = true;
            }
        }
        for (stmt_index, stmt) in function.body.stmts.iter().enumerate() {
            self.check_stmt(id, function, stmt_index, stmt);
        }
        self.check_term(id, function, &function.body.term);
        if self.structured_block_falls_through(&function.body)
            && !matches!(self.ty(function.ret.ty), Some(RirType::Void))
        {
            self.push(
                RirVerifySite::Terminator(id),
                RirVerifyErrorKind::ReturnValueRequired,
            );
        }
        self.initialized = previous_initialized;
        self.possibly_initialized = previous_possible;
    }

    fn check_stmt(
        &mut self,
        function_id: RirFunctionId,
        function: &RirFunction,
        index: usize,
        stmt: &RirStmt,
    ) {
        let site = RirVerifySite::Statement(function_id, index);
        match stmt {
            RirStmt::Init { local, value } => {
                self.check_local_id(site, function, *local);
                if function.params.iter().any(|param| param.local == *local) {
                    self.push(site, RirVerifyErrorKind::InitParamLocal);
                }
                if self
                    .possibly_initialized
                    .get(local.index())
                    .copied()
                    .unwrap_or(false)
                {
                    self.push(site, RirVerifyErrorKind::InitParamLocal);
                }
                if let Some(expected) = function.locals.get(local.index()).map(|local| local.ty) {
                    self.check_rvalue(function_id, function, index, value, Some(expected));
                }
                if let Some(initialized) = self.initialized.get_mut(local.index()) {
                    *initialized = true;
                }
                if let Some(possible) = self.possibly_initialized.get_mut(local.index()) {
                    *possible = true;
                }
            }
            RirStmt::Assign { dst, value } => {
                self.check_place(site, function, dst);
                if let Some(local) = function.locals.get(dst.local.index())
                    && !local.mutable
                {
                    self.push(site, RirVerifyErrorKind::ImmutableAssign);
                }
                self.check_rvalue(function_id, function, index, value, Some(dst.ty));
            }
            RirStmt::Eval(value) => {
                self.check_rvalue(function_id, function, index, value, None);
            }
            RirStmt::If(branch) => {
                if let Some(bool_ty) = self.type_id(RirType::Bool) {
                    let found = self.value_operand_ty(site, function, &branch.cond);
                    if found != Some(bool_ty) {
                        self.push(
                            site,
                            RirVerifyErrorKind::TypeMismatch {
                                expected: bool_ty,
                                found: found.unwrap_or(bool_ty),
                            },
                        );
                    }
                }
                let entry_definite = self.initialized.clone();
                let entry_possible = self.possibly_initialized.clone();
                let then_state = self.check_structured_block(
                    function_id,
                    function,
                    &branch.then_block,
                    entry_definite.clone(),
                    entry_possible.clone(),
                );
                let else_state = branch.else_block.as_ref().map_or(
                    Some((entry_definite.clone(), entry_possible.clone())),
                    |else_block| {
                        self.check_structured_block(
                            function_id,
                            function,
                            else_block,
                            entry_definite.clone(),
                            entry_possible.clone(),
                        )
                    },
                );
                self.merge_structured_states([then_state, else_state]);
            }
            RirStmt::EnumMatch(match_) => {
                self.check_place(site, function, &match_.discr);
                let enum_id = match self.ty(match_.discr.ty) {
                    Some(RirType::Enum(id)) => Some(id),
                    _ => {
                        self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                        None
                    }
                };
                let variant_count = enum_id
                    .and_then(|id| self.program.enums.get(id.index()))
                    .map(|enm| enm.variants.len());
                let mut seen = Vec::new();
                let entry_definite = self.initialized.clone();
                let entry_possible = self.possibly_initialized.clone();
                let mut states = vec![];
                for arm in &match_.arms {
                    if variant_count.is_none_or(|len| arm.variant.index() >= len) {
                        self.push(site, RirVerifyErrorKind::BadId);
                    } else if seen.contains(&arm.variant) {
                        self.push(site, RirVerifyErrorKind::DuplicateMatchArm);
                    } else {
                        seen.push(arm.variant);
                    }
                    states.push(self.check_structured_block(
                        function_id,
                        function,
                        &arm.block,
                        entry_definite.clone(),
                        entry_possible.clone(),
                    ));
                }
                if let Some(else_block) = &match_.else_block {
                    states.push(self.check_structured_block(
                        function_id,
                        function,
                        else_block,
                        entry_definite.clone(),
                        entry_possible.clone(),
                    ));
                } else if variant_count.is_some_and(|len| seen.len() < len) {
                    self.push(site, RirVerifyErrorKind::MatchNotExhaustive);
                }
                self.merge_structured_states(states);
            }
        }
    }

    fn check_structured_block(
        &mut self,
        function_id: RirFunctionId,
        function: &RirFunction,
        body: &RirStructuredBlock,
        definite: Vec<bool>,
        possible: Vec<bool>,
    ) -> Option<(Vec<bool>, Vec<bool>)> {
        let outer_definite = std::mem::replace(&mut self.initialized, definite);
        let outer_possible = std::mem::replace(&mut self.possibly_initialized, possible);
        for (index, stmt) in body.stmts.iter().enumerate() {
            self.check_stmt(function_id, function, index, stmt);
        }
        self.check_term(function_id, function, &body.term);
        let falls_through = self.structured_block_falls_through(body);
        let result =
            falls_through.then(|| (self.initialized.clone(), self.possibly_initialized.clone()));
        self.initialized = outer_definite;
        self.possibly_initialized = outer_possible;
        result
    }

    fn structured_block_falls_through(&self, block: &RirStructuredBlock) -> bool {
        for stmt in &block.stmts {
            if !self.stmt_falls_through(stmt) {
                return false;
            }
        }
        matches!(block.term, RirTerm::None)
    }

    fn stmt_falls_through(&self, stmt: &RirStmt) -> bool {
        match stmt {
            RirStmt::If(branch) => {
                branch
                    .else_block
                    .as_ref()
                    .is_none_or(|block| self.structured_block_falls_through(block))
                    || self.structured_block_falls_through(&branch.then_block)
            }
            RirStmt::EnumMatch(match_) => {
                let arm_falls = match_
                    .arms
                    .iter()
                    .any(|arm| self.structured_block_falls_through(&arm.block));
                match &match_.else_block {
                    Some(block) => arm_falls || self.structured_block_falls_through(block),
                    None => arm_falls || !self.enum_match_is_exhaustive(match_),
                }
            }
            RirStmt::Init { .. } | RirStmt::Assign { .. } | RirStmt::Eval(_) => true,
        }
    }

    fn enum_match_is_exhaustive(&self, match_: &RirEnumMatch) -> bool {
        let Some(RirType::Enum(enum_id)) = self.ty(match_.discr.ty) else {
            return false;
        };
        let Some(enm) = self.program.enums.get(enum_id.index()) else {
            return false;
        };
        let mut seen = Vec::new();
        for arm in &match_.arms {
            if arm.variant.index() >= enm.variants.len() || seen.contains(&arm.variant) {
                return false;
            }
            seen.push(arm.variant);
        }
        seen.len() == enm.variants.len()
    }

    fn merge_structured_states(
        &mut self,
        states: impl IntoIterator<Item = Option<(Vec<bool>, Vec<bool>)>>,
    ) {
        let mut states = states.into_iter().flatten();
        let Some((mut definite, mut possible)) = states.next() else {
            return;
        };
        for (next_definite, next_possible) in states {
            definite = definite
                .iter()
                .zip(&next_definite)
                .map(|(lhs, rhs)| *lhs && *rhs)
                .collect();
            possible = possible
                .iter()
                .zip(&next_possible)
                .map(|(lhs, rhs)| *lhs || *rhs)
                .collect();
        }
        self.initialized = definite;
        self.possibly_initialized = possible;
    }

    fn check_rvalue(
        &mut self,
        function_id: RirFunctionId,
        function: &RirFunction,
        index: usize,
        value: &RirRValue,
        expected: Option<RirTypeId>,
    ) {
        let site = RirVerifySite::RValue(function_id, index);
        let found = match value {
            RirRValue::Use(operand) => self.value_operand_ty(site, function, operand),
            RirRValue::Struct { ty, fields } => {
                self.check_type_id(site, *ty);
                let Some(RirType::Struct(struct_id)) = self.ty(*ty) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                };
                let Some(strukt) = self.program.structs.get(struct_id.index()).cloned() else {
                    self.push(site, RirVerifyErrorKind::BadId);
                    return;
                };
                if fields.len() != strukt.fields.len() {
                    self.push(
                        site,
                        RirVerifyErrorKind::FieldCount {
                            expected: strukt.fields.len(),
                            found: fields.len(),
                        },
                    );
                }
                for (field, operand) in strukt.fields.iter().zip(fields) {
                    let found = self.value_operand_ty(site, function, operand);
                    if found != Some(field.ty) {
                        self.push(
                            site,
                            RirVerifyErrorKind::TypeMismatch {
                                expected: field.ty,
                                found: found.unwrap_or(field.ty),
                            },
                        );
                    }
                }
                Some(*ty)
            }
            RirRValue::Array { ty, elems } => {
                self.check_type_id(site, *ty);
                let Some(RirType::Array { elem, len }) = self.ty(*ty) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                };
                if elems.len() != len as usize {
                    self.push(
                        site,
                        RirVerifyErrorKind::FieldCount {
                            expected: len as usize,
                            found: elems.len(),
                        },
                    );
                }
                for elem_value in elems {
                    let found = self.value_operand_ty(site, function, elem_value);
                    if found != Some(elem) {
                        self.push(
                            site,
                            RirVerifyErrorKind::TypeMismatch {
                                expected: elem,
                                found: found.unwrap_or(elem),
                            },
                        );
                    }
                }
                Some(*ty)
            }
            RirRValue::List { ty, elems } => {
                self.check_type_id(site, *ty);
                let Some(RirType::List(elem)) = self.ty(*ty) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                };
                for elem_value in elems {
                    let found = self.value_operand_ty(site, function, elem_value);
                    if found != Some(elem) {
                        self.push(
                            site,
                            RirVerifyErrorKind::TypeMismatch {
                                expected: elem,
                                found: found.unwrap_or(elem),
                            },
                        );
                    }
                }
                Some(*ty)
            }
            RirRValue::EnumVariant {
                ty,
                variant,
                fields,
            } => {
                self.check_type_id(site, *ty);
                let Some(RirType::Enum(enum_id)) = self.ty(*ty) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                };
                let Some(enm) = self.program.enums.get(enum_id.index()) else {
                    self.push(site, RirVerifyErrorKind::BadId);
                    return;
                };
                let Some(variant) = enm.variants.get(variant.index()).cloned() else {
                    self.push(site, RirVerifyErrorKind::BadId);
                    return;
                };
                if fields.len() != variant.fields.len() {
                    self.push(
                        site,
                        RirVerifyErrorKind::FieldCount {
                            expected: variant.fields.len(),
                            found: fields.len(),
                        },
                    );
                }
                for (field, operand) in variant.fields.iter().zip(fields) {
                    let found = self.value_operand_ty(site, function, operand);
                    if found != Some(field.ty) {
                        self.push(
                            site,
                            RirVerifyErrorKind::TypeMismatch {
                                expected: field.ty,
                                found: found.unwrap_or(field.ty),
                            },
                        );
                    }
                }
                Some(*ty)
            }
            RirRValue::Unary { op, value, ty } => {
                let value_ty = self.operand_ty(site, function, value);
                self.check_type_id(site, *ty);
                if !self.unary_ok(*op, value_ty, *ty) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                Some(*ty)
            }
            RirRValue::Binary { op, lhs, rhs, ty } => {
                let lhs_ty = self.operand_ty(site, function, lhs);
                let rhs_ty = self.operand_ty(site, function, rhs);
                self.check_type_id(site, *ty);
                if !self.binary_ok(*op, lhs_ty, rhs_ty, *ty) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                Some(*ty)
            }
            RirRValue::Cast { value, target } => {
                let value_ty = self.operand_ty(site, function, value);
                self.check_type_id(site, *target);
                if !self.cast_ok(value_ty, *target) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                Some(*target)
            }
            RirRValue::Stringify { value, source_ty } => {
                let value_ty = self.operand_ty(site, function, value);
                self.check_type_id(site, *source_ty);
                if value_ty != Some(*source_ty) || !self.stringify_ok(value, *source_ty) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                self.type_id(RirType::String)
            }
            RirRValue::StringConcat { parts } => {
                for part in parts {
                    let ty = self.operand_ty(site, function, part);
                    if ty.is_none_or(|ty| self.ty(ty) != Some(RirType::String)) {
                        self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    }
                }
                self.type_id(RirType::String)
            }
            RirRValue::Format {
                value,
                source_ty,
                spec,
            } => {
                let value_ty = self.operand_ty(site, function, value);
                self.check_type_id(site, *source_ty);
                if value_ty != Some(*source_ty) || !self.format_ok(*source_ty, *spec) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                self.type_id(RirType::String)
            }
            RirRValue::Call { callee, args, ty } => {
                self.check_type_id(site, *ty);
                self.check_call(function_id, function, index, *callee, args, *ty);
                Some(*ty)
            }
            RirRValue::Len { source } => {
                self.check_place(site, function, source);
                if !matches!(
                    self.ty(source.ty),
                    Some(
                        RirType::String
                            | RirType::Array { .. }
                            | RirType::List(_)
                            | RirType::Slice(_)
                    )
                ) {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                }
                self.type_id(RirType::Int)
            }
            RirRValue::ListPush { list, value } => {
                self.check_place(site, function, list);
                if function
                    .locals
                    .get(list.local.index())
                    .is_some_and(|local| !local.mutable)
                {
                    self.push(site, RirVerifyErrorKind::ImmutableAssign);
                }
                let Some(RirType::List(elem)) = self.ty(list.ty) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                };
                let found = self.value_operand_ty(site, function, value);
                if found != Some(elem) {
                    self.push(
                        site,
                        RirVerifyErrorKind::TypeMismatch {
                            expected: elem,
                            found: found.unwrap_or(elem),
                        },
                    );
                }
                self.type_id(RirType::Void)
            }
            RirRValue::SliceView {
                source,
                start,
                end,
                ty,
                ..
            } => {
                self.check_slice_range(site, function, source, *start, *end);
                let Some(RirType::Slice(elem)) = self.ty(*ty) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                };
                match self.ty(source.ty) {
                    Some(
                        RirType::Array {
                            elem: source_elem, ..
                        }
                        | RirType::List(source_elem)
                        | RirType::Slice(source_elem),
                    ) if source_elem == elem => {}
                    _ => self.push(site, RirVerifyErrorKind::UnsupportedRValueType),
                }
                Some(*ty)
            }
            RirRValue::ListSlice {
                source,
                start,
                end,
                ty,
                ..
            } => {
                self.check_slice_range(site, function, source, *start, *end);
                let Some(RirType::List(elem)) = self.ty(*ty) else {
                    self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    return;
                };
                match self.ty(source.ty) {
                    Some(RirType::List(source_elem)) if source_elem == elem => {
                        if !self.copyable_type(elem) {
                            self.push(site, RirVerifyErrorKind::NonCopyValueRequired);
                        }
                    }
                    _ => self.push(site, RirVerifyErrorKind::UnsupportedRValueType),
                }
                Some(*ty)
            }
        };
        if let (Some(expected), Some(found)) = (expected, found)
            && expected != found
        {
            self.push(site, RirVerifyErrorKind::TypeMismatch { expected, found });
        }
    }

    fn check_call(
        &mut self,
        function_id: RirFunctionId,
        function: &RirFunction,
        stmt: usize,
        callee: RirCallTarget,
        args: &[RirCallArg],
        ret: RirTypeId,
    ) {
        let (expected, callee_ret) = match callee {
            RirCallTarget::Function(id) => {
                self.check_function_id(RirVerifySite::RValue(function_id, stmt), id);
                match self.program.functions.get(id.index()) {
                    Some(function) => (
                        function
                            .params
                            .iter()
                            .map(|param| (param.ty, param.semantic))
                            .collect::<Vec<_>>(),
                        function.ret.ty,
                    ),
                    None => return,
                }
            }
            RirCallTarget::Extern(id) => {
                self.check_extern_id(RirVerifySite::RValue(function_id, stmt), id);
                match self.program.externs.get(id.index()) {
                    Some(ext) => (
                        ext.params
                            .iter()
                            .map(|param| (param.ty, param.semantic))
                            .collect::<Vec<_>>(),
                        ext.ret,
                    ),
                    None => return,
                }
            }
        };
        if expected.len() != args.len() {
            self.push(
                RirVerifySite::RValue(function_id, stmt),
                RirVerifyErrorKind::CallArgCount {
                    expected: expected.len(),
                    found: args.len(),
                },
            );
            return;
        }
        if ret != callee_ret {
            self.push(
                RirVerifySite::RValue(function_id, stmt),
                RirVerifyErrorKind::TypeMismatch {
                    expected: callee_ret,
                    found: ret,
                },
            );
        }
        for (index, (arg, (ty, mode))) in args.iter().zip(expected).enumerate() {
            let site = RirVerifySite::CallArg(function_id, stmt, index);
            if arg.semantic() != mode {
                self.push(site, RirVerifyErrorKind::CallArgMode);
            }
            let found = match arg {
                RirCallArg::Value(operand) => self.value_operand_ty(site, function, operand),
                RirCallArg::SharedBorrow(place) | RirCallArg::MutBorrow(place) => {
                    self.check_place(site, function, place);
                    Some(place.ty)
                }
                RirCallArg::SharedStringConst(id) => {
                    self.check_const_id(site, *id);
                    self.program.consts.get(id.index()).map(|konst| konst.ty)
                }
            };
            if let Some(found) = found
                && found != ty
            {
                self.push(
                    site,
                    RirVerifyErrorKind::TypeMismatch {
                        expected: ty,
                        found,
                    },
                );
            }
        }
    }

    fn check_term(&mut self, function_id: RirFunctionId, function: &RirFunction, term: &RirTerm) {
        let site = RirVerifySite::Terminator(function_id);
        match term {
            RirTerm::Return(None) if !matches!(self.ty(function.ret.ty), Some(RirType::Void)) => {
                self.push(site, RirVerifyErrorKind::ReturnValueRequired);
            }
            RirTerm::Return(Some(_)) if matches!(self.ty(function.ret.ty), Some(RirType::Void)) => {
                self.push(site, RirVerifyErrorKind::UnexpectedReturnValue);
            }
            RirTerm::Return(Some(operand)) => {
                if let Some(found) = self.operand_ty(site, function, operand) {
                    if found != function.ret.ty {
                        self.push(
                            site,
                            RirVerifyErrorKind::TypeMismatch {
                                expected: function.ret.ty,
                                found,
                            },
                        );
                    }
                    if matches!(self.ty(found), Some(RirType::Slice(_))) {
                        self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    }
                }
            }
            RirTerm::None | RirTerm::Return(None) | RirTerm::Unreachable => {}
        }
    }

    fn check_abi(
        &mut self,
        site: RirVerifySite,
        ty: RirTypeId,
        semantic: RirParamSemantic,
        abi: RirParamAbi,
    ) {
        let supported = matches!(
            (semantic, abi, self.ty(ty)),
            (
                RirParamSemantic::Value,
                RirParamAbi::Value,
                Some(
                    RirType::Int
                        | RirType::Float
                        | RirType::Bool
                        | RirType::Void
                        | RirType::Struct(_)
                        | RirType::Enum(_)
                        | RirType::Array { .. }
                        | RirType::List(_)
                ),
            ) | (
                RirParamSemantic::SharedBorrow,
                RirParamAbi::SharedBorrow,
                Some(
                    RirType::String
                        | RirType::Struct(_)
                        | RirType::Enum(_)
                        | RirType::Array { .. }
                        | RirType::List(_)
                ),
            )
        );
        if !supported {
            self.push(site, RirVerifyErrorKind::UnsupportedAbi);
        }
    }

    fn unary_ok(&self, op: UnaryOp, value: Option<RirTypeId>, ret: RirTypeId) -> bool {
        match (op, value.and_then(|ty| self.ty(ty)), self.ty(ret)) {
            (
                UnaryOp::Neg,
                Some(RirType::Int | RirType::Float),
                Some(RirType::Int | RirType::Float),
            ) => value == Some(ret),
            (UnaryOp::Not, Some(RirType::Bool), Some(RirType::Bool)) => true,
            (UnaryOp::BitNot, Some(RirType::Int), Some(RirType::Int)) => true,
            _ => false,
        }
    }

    fn binary_ok(
        &self,
        op: BinaryOp,
        lhs: Option<RirTypeId>,
        rhs: Option<RirTypeId>,
        ret: RirTypeId,
    ) -> bool {
        if lhs != rhs {
            return false;
        }
        match (op, lhs.and_then(|ty| self.ty(ty)), self.ty(ret)) {
            (
                BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem,
                Some(RirType::Int | RirType::Float),
                Some(RirType::Int | RirType::Float),
            ) => lhs == Some(ret),
            (
                BinaryOp::Eq
                | BinaryOp::NotEq
                | BinaryOp::LessThan
                | BinaryOp::GreaterThan
                | BinaryOp::LessThanEq
                | BinaryOp::GreaterThanEq,
                Some(RirType::Int | RirType::Float | RirType::Bool),
                Some(RirType::Bool),
            ) => true,
            (BinaryOp::And | BinaryOp::Or, Some(RirType::Bool), Some(RirType::Bool)) => true,
            (
                BinaryOp::Xor | BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::Shl | BinaryOp::Shr,
                Some(RirType::Int),
                Some(RirType::Int),
            ) => true,
            _ => false,
        }
    }

    fn cast_ok(&self, value: Option<RirTypeId>, ret: RirTypeId) -> bool {
        matches!(
            (value.and_then(|ty| self.ty(ty)), self.ty(ret)),
            (
                Some(RirType::Int | RirType::Float),
                Some(RirType::Int | RirType::Float)
            )
        )
    }

    fn stringify_ok(&self, value: &RirOperand, source_ty: RirTypeId) -> bool {
        match self.ty(source_ty) {
            Some(RirType::Int | RirType::Float | RirType::Bool) => true,
            Some(RirType::String) => matches!(value, RirOperand::Const(_)),
            Some(RirType::Struct(_)) => {
                matches!(value, RirOperand::Place(_))
                    && matches!(
                        self.stringify_req(source_ty).map(|req| req.kind),
                        Some(
                            RirStringifyReqKind::Structural(_)
                                | RirStringifyReqKind::Override { .. }
                        )
                    )
            }
            Some(
                RirType::Void
                | RirType::Enum(_)
                | RirType::Array { .. }
                | RirType::List(_)
                | RirType::Slice(_),
            )
            | None => false,
        }
    }

    fn format_ok(&self, source_ty: RirTypeId, spec: RirFormatSpec) -> bool {
        let Some(ty) = self.ty(source_ty) else {
            return false;
        };
        match ty {
            RirType::Int | RirType::Float | RirType::Bool | RirType::String => {}
            RirType::Void
            | RirType::Struct(_)
            | RirType::Enum(_)
            | RirType::Array { .. }
            | RirType::List(_)
            | RirType::Slice(_) => {
                return false;
            }
        }
        match spec.kind {
            RirFormatKind::Hex | RirFormatKind::HexUpper | RirFormatKind::Binary
                if ty != RirType::Int =>
            {
                return false;
            }
            RirFormatKind::Exp | RirFormatKind::ExpUpper if ty != RirType::Float => {
                return false;
            }
            _ => {}
        }
        if spec.precision.is_some() && !matches!(ty, RirType::Float | RirType::String) {
            return false;
        }
        if spec.sign == RirFormatSign::Always && !matches!(ty, RirType::Int | RirType::Float) {
            return false;
        }
        true
    }

    fn operand_ty(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        operand: &RirOperand,
    ) -> Option<RirTypeId> {
        match operand {
            RirOperand::Place(place) => {
                self.check_place(site, function, place);
                Some(place.ty)
            }
            RirOperand::Const(id) => {
                self.check_const_id(site, *id);
                self.program.consts.get(id.index()).map(|konst| konst.ty)
            }
        }
    }

    fn value_operand_ty(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        operand: &RirOperand,
    ) -> Option<RirTypeId> {
        let ty = self.operand_ty(site, function, operand);
        if matches!(operand, RirOperand::Place(_)) && ty.is_some_and(|ty| !self.copyable_type(ty)) {
            self.push(site, RirVerifyErrorKind::NonCopyValueRequired);
        }
        ty
    }

    fn check_slice_range(
        &mut self,
        site: RirVerifySite,
        function: &RirFunction,
        source: &RirPlace,
        start: RirLocalId,
        end: RirLocalId,
    ) {
        self.check_place(site, function, source);
        for local in [start, end] {
            let Some(data) = function.locals.get(local.index()) else {
                self.push(site, RirVerifyErrorKind::BadId);
                continue;
            };
            if self.ty(data.ty) != Some(RirType::Int) {
                self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
            }
            if !self
                .initialized
                .get(local.index())
                .copied()
                .unwrap_or(false)
            {
                self.push(site, RirVerifyErrorKind::UninitializedLocal(local));
            }
        }
    }

    fn check_place(&mut self, site: RirVerifySite, function: &RirFunction, place: &RirPlace) {
        self.check_local_id(site, function, place.local);
        self.check_type_id(site, place.ty);
        let mut current = match function.locals.get(place.local.index()) {
            Some(local) => local.ty,
            None => return,
        };
        for projection in &place.projections {
            match projection {
                RirProjection::Field(field_id) => {
                    let Some(RirType::Struct(struct_id)) = self.ty(current) else {
                        self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                        return;
                    };
                    let Some(strukt) = self.program.structs.get(struct_id.index()) else {
                        self.push(site, RirVerifyErrorKind::BadId);
                        return;
                    };
                    let Some(field) = strukt.fields.get(field_id.index()) else {
                        self.push(site, RirVerifyErrorKind::BadId);
                        return;
                    };
                    current = field.ty;
                }
                RirProjection::Index(local) => {
                    let Some(RirType::Array { elem, .. } | RirType::List(elem)) = self.ty(current)
                    else {
                        self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                        return;
                    };
                    let Some(index_local) = function.locals.get(local.index()) else {
                        self.push(site, RirVerifyErrorKind::BadId);
                        return;
                    };
                    if self.ty(index_local.ty) != Some(RirType::Int) {
                        self.push(site, RirVerifyErrorKind::UnsupportedRValueType);
                    }
                    if !self
                        .initialized
                        .get(local.index())
                        .copied()
                        .unwrap_or(false)
                    {
                        self.push(site, RirVerifyErrorKind::UninitializedLocal(*local));
                    }
                    current = elem;
                }
            }
        }
        if current != place.ty {
            self.push(
                site,
                RirVerifyErrorKind::TypeMismatch {
                    expected: current,
                    found: place.ty,
                },
            );
        }
        if !self
            .initialized
            .get(place.local.index())
            .copied()
            .unwrap_or(false)
        {
            self.push(site, RirVerifyErrorKind::UninitializedLocal(place.local));
        }
    }

    fn ty(&self, id: RirTypeId) -> Option<RirType> {
        self.program.types.get(id.index()).copied()
    }

    fn type_id(&self, target: RirType) -> Option<RirTypeId> {
        self.program
            .types
            .iter()
            .position(|ty| *ty == target)
            .map(RirTypeId::from_index)
    }

    fn check_type_id(&mut self, site: RirVerifySite, id: RirTypeId) {
        if id.index() >= self.program.types.len() {
            self.push(site, RirVerifyErrorKind::BadId);
        }
    }

    fn check_struct_id(&mut self, site: RirVerifySite, id: RirStructId) {
        if id.index() >= self.program.structs.len() {
            self.push(site, RirVerifyErrorKind::BadId);
        }
    }

    fn check_enum_id(&mut self, site: RirVerifySite, id: RirEnumId) {
        if id.index() >= self.program.enums.len() {
            self.push(site, RirVerifyErrorKind::BadId);
        }
    }

    fn check_stringify_helper_id(&mut self, site: RirVerifySite, id: RirStringifyHelperId) {
        if id.index() >= self.program.stringify_helpers.len() {
            self.push(site, RirVerifyErrorKind::BadId);
        }
    }

    fn stringify_req(&self, ty: RirTypeId) -> Option<&RirStringifyReq> {
        self.program.stringify_reqs.iter().find(|req| req.ty == ty)
    }

    fn copyable_type(&self, ty: RirTypeId) -> bool {
        match self.ty(ty) {
            Some(RirType::Int | RirType::Float | RirType::Bool | RirType::Void) => true,
            Some(RirType::Struct(id)) => {
                self.program
                    .structs
                    .get(id.index())
                    .is_some_and(|strukt| strukt.copyable)
                    && self.inherently_copyable_type(ty)
            }
            Some(RirType::Enum(id)) => {
                self.program
                    .enums
                    .get(id.index())
                    .is_some_and(|enm| enm.copyable)
                    && self.inherently_copyable_type(ty)
            }
            Some(RirType::Array { .. }) => self.inherently_copyable_type(ty),
            Some(RirType::List(_) | RirType::Slice(_) | RirType::String) | None => false,
        }
    }

    fn inherently_copyable_type(&self, ty: RirTypeId) -> bool {
        match self.ty(ty) {
            Some(RirType::Int | RirType::Float | RirType::Bool | RirType::Void) => true,
            Some(RirType::Struct(id)) => {
                self.program.structs.get(id.index()).is_some_and(|strukt| {
                    strukt
                        .fields
                        .iter()
                        .all(|field| self.inherently_copyable_type(field.ty))
                })
            }
            Some(RirType::Enum(id)) => self.program.enums.get(id.index()).is_some_and(|enm| {
                enm.variants.iter().all(|variant| {
                    variant
                        .fields
                        .iter()
                        .all(|field| self.inherently_copyable_type(field.ty))
                })
            }),
            Some(RirType::Array { elem, .. }) => self.inherently_copyable_type(elem),
            Some(RirType::List(_) | RirType::Slice(_) | RirType::String) | None => false,
        }
    }

    fn check_const_id(&mut self, site: RirVerifySite, id: RirConstId) {
        if id.index() >= self.program.consts.len() {
            self.push(site, RirVerifyErrorKind::BadId);
        }
    }

    fn check_extern_id(&mut self, site: RirVerifySite, id: RirExternId) {
        if id.index() >= self.program.externs.len() {
            self.push(site, RirVerifyErrorKind::BadId);
        }
    }

    fn check_function_id(&mut self, site: RirVerifySite, id: RirFunctionId) {
        if id.index() >= self.program.functions.len() {
            self.push(site, RirVerifyErrorKind::BadId);
        }
    }

    fn check_local_id(&mut self, site: RirVerifySite, function: &RirFunction, id: RirLocalId) {
        if id.index() >= function.locals.len() {
            self.push(site, RirVerifyErrorKind::BadId);
        }
    }

    fn push(&mut self, site: RirVerifySite, kind: RirVerifyErrorKind) {
        self.errors.push(RirVerifyError { site, kind });
    }
}

pub fn semantic_from_air(mode: air::ParamMode) -> RirParamSemantic {
    match mode {
        air::ParamMode::Value => RirParamSemantic::Value,
        air::ParamMode::SharedBorrow => RirParamSemantic::SharedBorrow,
        air::ParamMode::MutBorrow => RirParamSemantic::MutBorrow,
    }
}
