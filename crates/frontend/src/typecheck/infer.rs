use std::{
    collections::{HashMap, HashSet},
    hash::{Hash, Hasher},
};

use super::{
    ConstDiagnostic, ConstSubst, GenericArgs, GenericParams, NominalKey, TypeSubst,
    const_term::{ConstInferVarId, ConstTerm},
};
use crate::{
    ast::{
        ArrayLen, ConstArg, ConstExpr, ConstParamId, ContractRef, EscapeMode, ExprId, FuncParam,
        GenericArg, Ident, ModuleOrigin, NominalKind, NominalType, ReturnAccess, ReturnSpec, Type,
        TypeFolder, TypeVarId,
    },
    span::SourceSpan,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
struct InferVarId(u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct TyFuncParam {
    ty: Ty,
    mutable: bool,
    cast_accept: bool,
    escape: EscapeMode,
}

impl TyFuncParam {
    fn try_to_func_param_no_infer(&self) -> Option<FuncParam> {
        Some(FuncParam {
            ty: self.ty.try_to_type_no_infer()?,
            mutable: self.mutable,
            cast_accept: self.cast_accept,
            escape: self.escape,
        })
    }
}

struct TyFuncParts {
    params: Vec<TyFuncParam>,
    ret: TyReturnSpec,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct TyReturnSpec {
    access: ReturnAccess,
    ty: Ty,
    iter: bool,
}

impl TyReturnSpec {
    fn value(ty: Ty) -> Self {
        Self {
            access: ReturnAccess::Value,
            ty,
            iter: false,
        }
    }

    fn with_ty(&self, ty: Ty) -> Self {
        Self {
            access: self.access,
            ty,
            iter: self.iter,
        }
    }

    fn try_to_return_spec_no_infer(&self) -> Option<ReturnSpec> {
        let ty = self.ty.try_to_type_no_infer()?;
        Some(if self.iter {
            ReturnSpec::iter()
        } else {
            match self.access {
                ReturnAccess::Value => ReturnSpec::value(ty),
                ReturnAccess::Place => ReturnSpec::place(ty),
            }
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum TyGenericArg {
    Type(Ty),
    Const(ConstTerm),
}

impl TyGenericArg {
    fn try_to_generic_arg_no_infer(&self) -> Option<GenericArg> {
        match self {
            Self::Type(ty) => Some(GenericArg::Type(ty.try_to_type_no_infer()?)),
            Self::Const(arg) => Some(GenericArg::Const(arg.to_arg_no_infer()?)),
        }
    }
}

#[derive(Debug, Clone)]
struct TyNominal {
    id: crate::semantic_id::NominalId,
    kind: NominalKind,
    name: Ident,
    type_args: Vec<Ty>,
    const_args: Vec<ConstTerm>,
    origin: Option<ModuleOrigin>,
}

impl PartialEq for TyNominal {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
            && self.type_args == other.type_args
            && self.const_args == other.const_args
    }
}

impl Eq for TyNominal {}

impl Hash for TyNominal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.type_args.hash(state);
        self.const_args.hash(state);
    }
}

impl TyNominal {
    fn same_head(&self, other: &Self) -> bool {
        self.id == other.id
            && self.type_args.len() == other.type_args.len()
            && self.const_args.len() == other.const_args.len()
    }

    fn from_nominal(nominal: &NominalType, type_args: Vec<Ty>, const_args: Vec<ConstTerm>) -> Ty {
        Ty::Nominal(Self {
            id: nominal.id.clone(),
            kind: nominal.kind,
            name: nominal.name,
            type_args,
            const_args,
            origin: nominal.origin.clone(),
        })
    }

    fn with_args(self, type_args: Vec<Ty>, const_args: Vec<ConstTerm>) -> Ty {
        Ty::Nominal(Self {
            type_args,
            const_args,
            ..self
        })
    }

    fn with_cloned_args(&self, type_args: Vec<Ty>, const_args: Vec<ConstTerm>) -> Ty {
        self.clone().with_args(type_args, const_args)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Ty {
    Error,
    Infer(InferVarId),
    Any,
    Int,
    Float,
    Bool,
    String,
    Char,
    Void,
    Func {
        params: Vec<TyFuncParam>,
        ret: Box<TyReturnSpec>,
    },
    Dyn(ContractRef),
    Var(TypeVarId),
    UnresolvedName(Ident),
    UnresolvedNominal {
        qualifier: Option<Ident>,
        name: Ident,
        generic_args: Vec<TyGenericArg>,
    },
    Tuple(Vec<Ty>),
    Nominal(TyNominal),
    List {
        elem: Box<Ty>,
    },
    Array {
        elem: Box<Ty>,
        len: ConstTerm,
    },
    Map {
        key: Box<Ty>,
        value: Box<Ty>,
    },
    Slice {
        elem: Box<Ty>,
    },
}

impl Ty {
    fn contains_error(&self) -> bool {
        match self {
            Self::Error => true,
            Self::Func { params, ret } => {
                params.iter().any(|param| param.ty.contains_error()) || ret.ty.contains_error()
            }
            Self::Tuple(elems) => elems.iter().any(Self::contains_error),
            Self::Nominal(nominal) => nominal.type_args.iter().any(Self::contains_error),
            Self::List { elem } | Self::Slice { elem } | Self::Array { elem, .. } => {
                elem.contains_error()
            }
            Self::Map { key, value } => key.contains_error() || value.contains_error(),
            _ => false,
        }
    }

    fn try_types_to_no_infer(types: &[Self]) -> Option<Vec<Type>> {
        types.iter().map(Self::try_to_type_no_infer).collect()
    }

    fn try_nominal_args_to_no_infer(
        type_args: &[Self],
        const_args: &[ConstTerm],
    ) -> Option<(Vec<Type>, Vec<ConstArg>)> {
        Some((
            Self::try_types_to_no_infer(type_args)?,
            ConstTerm::to_args_no_infer(const_args)?,
        ))
    }

    fn try_nominal_to_no_infer(nominal: &TyNominal) -> Option<Type> {
        let (type_args, const_args) =
            Self::try_nominal_args_to_no_infer(&nominal.type_args, &nominal.const_args)?;
        Some(Type::Nominal(NominalType {
            id: nominal.id.clone(),
            kind: nominal.kind,
            name: nominal.name,
            type_args,
            const_args,
            origin: nominal.origin.clone(),
        }))
    }

    fn from_recovery_type(ty: &Type) -> Self {
        match ty {
            Type::Infer | Type::InferReturn => Self::Error,
            Type::Any => Self::Any,
            Type::Int => Self::Int,
            Type::Float => Self::Float,
            Type::Bool => Self::Bool,
            Type::String => Self::String,
            Type::Char => Self::Char,
            Type::Void => Self::Void,
            Type::Func { params, ret } => Self::Func {
                params: params
                    .iter()
                    .map(|param| TyFuncParam {
                        ty: Self::from_recovery_type(&param.ty),
                        mutable: param.mutable,
                        cast_accept: param.cast_accept,
                        escape: param.escape,
                    })
                    .collect(),
                ret: Box::new(TyReturnSpec {
                    access: ret.access(),
                    ty: Self::from_recovery_type(&ret.ty()),
                    iter: ret.is_iter(),
                }),
            },
            Type::Dyn(contract) => Self::Dyn(contract.clone()),
            Type::Var(id) => Self::Var(*id),
            Type::UnresolvedName(name) => Self::UnresolvedName(*name),
            Type::UnresolvedNominal {
                qualifier,
                name,
                generic_args,
            } => Self::UnresolvedNominal {
                qualifier: *qualifier,
                name: *name,
                generic_args: generic_args
                    .iter()
                    .map(|arg| match arg {
                        GenericArg::Type(ty) => TyGenericArg::Type(Self::from_recovery_type(ty)),
                        GenericArg::Const(arg) => TyGenericArg::Const(ConstTerm::from_arg(arg)),
                    })
                    .collect(),
            },
            Type::Tuple(elems) => Self::Tuple(elems.iter().map(Self::from_recovery_type).collect()),
            Type::Nominal(nominal) => {
                if nominal.kind == NominalKind::Extern {
                    debug_assert!(nominal.type_args.is_empty());
                    debug_assert!(nominal.const_args.is_empty());
                }
                let type_args = nominal
                    .type_args
                    .iter()
                    .map(Self::from_recovery_type)
                    .collect();
                let const_args = ConstTerm::from_args(&nominal.const_args);
                TyNominal::from_nominal(nominal, type_args, const_args)
            }
            Type::List { elem } => Self::List {
                elem: Box::new(Self::from_recovery_type(elem)),
            },
            Type::Array { elem, len } => Self::Array {
                elem: Box::new(Self::from_recovery_type(elem)),
                len: ConstTerm::from_array_len(len.clone()),
            },
            Type::Map { key, value } => Self::Map {
                key: Box::new(Self::from_recovery_type(key)),
                value: Box::new(Self::from_recovery_type(value)),
            },
            Type::Slice { elem } => Self::Slice {
                elem: Box::new(Self::from_recovery_type(elem)),
            },
            Type::Optional { .. } => {
                unreachable!("optional syntax must be finalized before inference")
            }
        }
    }

    fn try_to_type_no_infer(&self) -> Option<Type> {
        match self {
            Self::Error => Some(Type::Infer),
            Self::Infer(_) => None,
            Self::Any => Some(Type::Any),
            Self::Int => Some(Type::Int),
            Self::Float => Some(Type::Float),
            Self::Bool => Some(Type::Bool),
            Self::String => Some(Type::String),
            Self::Char => Some(Type::Char),
            Self::Void => Some(Type::Void),
            Self::Func { params, ret } => Some(Type::Func {
                params: params
                    .iter()
                    .map(TyFuncParam::try_to_func_param_no_infer)
                    .collect::<Option<Vec<_>>>()?,
                ret: Box::new(ret.try_to_return_spec_no_infer()?),
            }),
            Self::Dyn(contract) => Some(Type::Dyn(contract.clone())),
            Self::Var(id) => Some(Type::Var(*id)),
            Self::UnresolvedName(name) => Some(Type::UnresolvedName(*name)),
            Self::UnresolvedNominal {
                qualifier,
                name,
                generic_args,
            } => Some(Type::UnresolvedNominal {
                qualifier: *qualifier,
                name: *name,
                generic_args: generic_args
                    .iter()
                    .map(TyGenericArg::try_to_generic_arg_no_infer)
                    .collect::<Option<Vec<_>>>()?,
            }),
            Self::Tuple(elems) => Some(Type::Tuple(
                elems
                    .iter()
                    .map(Ty::try_to_type_no_infer)
                    .collect::<Option<Vec<_>>>()?,
            )),
            Self::Nominal(nominal) => Self::try_nominal_to_no_infer(nominal),
            Self::List { elem } => Some(Type::List {
                elem: Box::new(elem.try_to_type_no_infer()?),
            }),
            Self::Array { elem, len } => Some(Type::Array {
                elem: Box::new(elem.try_to_type_no_infer()?),
                len: len.to_array_len_no_infer()?,
            }),
            Self::Map { key, value } => Some(Type::Map {
                key: Box::new(key.try_to_type_no_infer()?),
                value: Box::new(value.try_to_type_no_infer()?),
            }),
            Self::Slice { elem } => Some(Type::Slice {
                elem: Box::new(elem.try_to_type_no_infer()?),
            }),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub(crate) struct SemanticLocalId(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
struct TempTypeId(u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum TypeRef {
    Concrete(Ty),
    Local(SemanticLocalId),
    Temp(TempTypeId),
    Expr(ExprId),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct TypeHandle(TypeRef);

#[derive(Debug, Clone, Default)]
pub(super) struct GenericSolverSeeds {
    pub(super) type_args: TypeSubst,
    pub(super) const_args: ConstSubst,
}

impl GenericSolverSeeds {
    pub(super) fn from_args(generics: &GenericParams, args: &GenericArgs) -> Self {
        let type_args = generics
            .type_params
            .iter()
            .zip(&args.type_args)
            .map(|(param, ty)| (param.id, ty.clone()))
            .collect();
        let const_args = generics
            .const_params
            .iter()
            .zip(&args.const_args)
            .map(|(param, term)| (param.id, term.clone()))
            .collect();
        Self {
            type_args,
            const_args,
        }
    }
}

#[derive(Debug, Clone)]
pub(super) struct GenericSolverVars {
    types: HashMap<TypeVarId, Ty>,
    consts: HashMap<ConstParamId, ConstTerm>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum SolverRelationError {
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum SolverFinalizeError {
    UnresolvedType { span: Option<SourceSpan> },
    UnresolvedConst { span: Option<SourceSpan> },
}

impl TypeRef {
    fn concrete(ty: Ty) -> Self {
        Self::Concrete(ty)
    }

    fn local(id: SemanticLocalId) -> Self {
        Self::Local(id)
    }

    fn temp(id: TempTypeId) -> Self {
        Self::Temp(id)
    }

    fn expr(id: ExprId) -> Self {
        Self::Expr(id)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TyRelation {
    Equal,
    Assignable,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Constraint {
    span: Option<SourceSpan>,
    kind: ConstraintKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ConstraintKind {
    Equal(TypeRef, TypeRef),
    Assignable { from: TypeRef, to: TypeRef },
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum SolveError {
    TypeMismatch {
        expected: Box<Ty>,
        found: Box<Ty>,
        span: Option<SourceSpan>,
    },
    ConstMismatch {
        expected: ConstTerm,
        found: ConstTerm,
        span: Option<SourceSpan>,
    },
    TypeAlreadyBound {
        var: InferVarId,
        existing: Box<Ty>,
        found: Box<Ty>,
        span: Option<SourceSpan>,
    },
    TypeOccurs {
        var: InferVarId,
        span: Option<SourceSpan>,
    },
    ConstAlreadyBound {
        var: ConstInferVarId,
        existing: ConstTerm,
        found: ConstTerm,
        span: Option<SourceSpan>,
    },
    ConstOccurs {
        var: ConstInferVarId,
        span: Option<SourceSpan>,
    },
}

impl SolveError {
    fn type_mismatch(expected: Ty, found: Ty, span: Option<SourceSpan>) -> Self {
        Self::TypeMismatch {
            expected: Box::new(expected),
            found: Box::new(found),
            span,
        }
    }

    fn const_mismatch(expected: ConstTerm, found: ConstTerm, span: Option<SourceSpan>) -> Self {
        Self::ConstMismatch {
            expected,
            found,
            span,
        }
    }

    fn func_param_mismatch(
        expected: TyFuncParam,
        found: TyFuncParam,
        span: Option<SourceSpan>,
    ) -> Self {
        Self::type_mismatch(
            Ty::Func {
                params: vec![expected],
                ret: Box::new(TyReturnSpec::value(Ty::Void)),
            },
            Ty::Func {
                params: vec![found],
                ret: Box::new(TyReturnSpec::value(Ty::Void)),
            },
            span,
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum InferError {
    UnresolvedType { span: Option<SourceSpan> },
    UnresolvedConst { span: Option<SourceSpan> },
}

struct FinalizeCx<'a> {
    errors: &'a mut Vec<InferError>,
    seen_types: &'a mut HashSet<InferVarId>,
    seen_consts: &'a mut HashSet<ConstInferVarId>,
}

fn rewrite_ty_contract_refs(ty: &mut Ty, f: &mut impl FnMut(&ContractRef) -> ContractRef) {
    match ty {
        Ty::Func { params, ret } => {
            for param in params {
                rewrite_ty_contract_refs(&mut param.ty, f);
            }
            rewrite_ty_contract_refs(&mut ret.ty, f);
        }
        Ty::Dyn(contract) => *contract = rewrite_contract_ref(contract, f),
        Ty::UnresolvedNominal { generic_args, .. } => {
            for arg in generic_args {
                if let TyGenericArg::Type(ty) = arg {
                    rewrite_ty_contract_refs(ty, f);
                }
            }
        }
        Ty::Tuple(elems) => {
            for elem in elems {
                rewrite_ty_contract_refs(elem, f);
            }
        }
        Ty::Nominal(nominal) => {
            for arg in &mut nominal.type_args {
                rewrite_ty_contract_refs(arg, f);
            }
        }
        Ty::List { elem } | Ty::Array { elem, .. } | Ty::Slice { elem } => {
            rewrite_ty_contract_refs(elem, f);
        }
        Ty::Map { key, value } => {
            rewrite_ty_contract_refs(key, f);
            rewrite_ty_contract_refs(value, f);
        }
        Ty::Error
        | Ty::Infer(_)
        | Ty::Any
        | Ty::Int
        | Ty::Float
        | Ty::Bool
        | Ty::String
        | Ty::Char
        | Ty::Void
        | Ty::Var(_)
        | Ty::UnresolvedName(_) => {}
    }
}

fn rewrite_contract_ref(
    contract: &ContractRef,
    f: &mut impl FnMut(&ContractRef) -> ContractRef,
) -> ContractRef {
    ContractRefRewriter { f }.fold_contract_ref(contract)
}

struct ContractRefRewriter<'a, F> {
    f: &'a mut F,
}

impl<F> TypeFolder for ContractRefRewriter<'_, F>
where
    F: FnMut(&ContractRef) -> ContractRef,
{
    fn fold_contract_ref_leaf(&mut self, contract: ContractRef) -> ContractRef {
        (self.f)(&contract)
    }
}

pub(super) type SourceExprTypes = HashMap<ExprId, (Option<SourceSpan>, Type)>;

#[derive(Debug, Default, Clone)]
pub(super) struct Solver {
    core_option: Option<crate::semantic_id::NominalId>,
    next_type_var: u32,
    next_const_var: u32,
    type_spans: HashMap<InferVarId, SourceSpan>,
    const_spans: HashMap<ConstInferVarId, SourceSpan>,
    type_bindings: HashMap<InferVarId, Ty>,
    const_bindings: HashMap<ConstInferVarId, ConstTerm>,
    nil_vars: HashSet<InferVarId>,
    local_types: Vec<Ty>,
    temp_types: Vec<Ty>,
    expr_types: HashMap<ExprId, (Option<SourceSpan>, Ty)>,
    constraints: Vec<Constraint>,
}

impl Solver {
    pub(super) fn new(core_option: Option<NominalKey>) -> Self {
        let core_option = core_option.map(|key| key.id);
        Self {
            core_option,
            ..Self::default()
        }
    }

    fn option_inner<'a>(&self, ty: &'a Ty) -> Option<&'a Ty> {
        let Ty::Nominal(nominal) = ty else {
            return None;
        };
        if !self.is_core_option_nominal(nominal) || !nominal.const_args.is_empty() {
            return None;
        }
        let [inner] = nominal.type_args.as_slice() else {
            return None;
        };
        Some(inner)
    }

    fn is_option(&self, ty: &Ty) -> bool {
        self.option_inner(ty).is_some()
    }

    fn is_core_option_nominal(&self, nominal: &TyNominal) -> bool {
        self.core_option.as_ref() == Some(&nominal.id)
    }

    fn type_for_storage(&self, ty: &Ty) -> Type {
        self.resolve_ty(ty)
            .try_to_type_no_infer()
            .unwrap_or(Type::Infer)
    }

    pub(super) fn concrete_type(ty: &Type) -> TypeHandle {
        TypeHandle(TypeRef::concrete(Ty::from_recovery_type(ty)))
    }

    pub(super) fn snapshot_handle(&mut self, handle: &TypeHandle) -> TypeHandle {
        let ty = self.resolve_ref(&handle.0);
        self.temp_handle(ty)
    }

    pub(super) fn generic_solver_vars(
        &mut self,
        generics: &GenericParams,
        seeds: &GenericSolverSeeds,
        span: Option<SourceSpan>,
    ) -> GenericSolverVars {
        let types = generics
            .type_params
            .iter()
            .map(|param| {
                let ty = seeds
                    .type_args
                    .get(&param.id)
                    .map_or_else(|| self.fresh_type(span), Ty::from_recovery_type);
                (param.id, ty)
            })
            .collect();
        let consts = generics
            .const_params
            .iter()
            .map(|param| {
                let arg = seeds
                    .const_args
                    .get(&param.id)
                    .cloned()
                    .unwrap_or_else(|| self.fresh_const(span));
                (param.id, arg)
            })
            .collect();
        GenericSolverVars { types, consts }
    }

    pub(super) fn instantiate_generic_type(
        &mut self,
        ty: &Type,
        vars: &GenericSolverVars,
    ) -> TypeHandle {
        let ty = self.instantiate_type_template(ty, vars);
        self.temp_handle(ty)
    }

    pub(super) fn finalize_generic_args(
        &self,
        generics: &GenericParams,
        vars: &GenericSolverVars,
    ) -> Result<GenericArgs, Vec<Ident>> {
        let mut type_args = vec![];
        let mut const_args = vec![];
        let mut unbound = vec![];

        for param in &generics.type_params {
            match self.finalize_generic_type_arg(param.id, vars) {
                Some(ty) => type_args.push(ty),
                None => unbound.push(param.name),
            }
        }

        for param in &generics.const_params {
            match self.finalize_generic_const_param(param.id, vars) {
                Some(value) => const_args.push(value),
                None => unbound.push(param.name),
            }
        }

        if unbound.is_empty() {
            Ok(GenericArgs {
                type_args,
                const_args,
            })
        } else {
            Err(unbound)
        }
    }

    fn finalize_generic_type_arg(&self, id: TypeVarId, vars: &GenericSolverVars) -> Option<Type> {
        let ty = vars.types.get(&id)?;
        let resolved = self.resolve_ty(ty);
        let (ty, errors) = self.finalize_ty(&resolved);
        (errors.is_empty() && !matches!(ty, Type::Infer)).then_some(ty)
    }

    fn finalize_generic_const_param(
        &self,
        id: ConstParamId,
        vars: &GenericSolverVars,
    ) -> Option<ConstTerm> {
        let arg = vars.consts.get(&id)?;
        self.finalized_generic_const_arg(arg)
    }

    pub(super) fn local_handle(id: SemanticLocalId) -> TypeHandle {
        TypeHandle(TypeRef::local(id))
    }

    pub(super) fn expr_handle(id: ExprId) -> TypeHandle {
        TypeHandle(TypeRef::expr(id))
    }

    pub(super) fn fresh_temp_handle(&mut self, span: Option<SourceSpan>) -> TypeHandle {
        let ty = self.fresh_type(span);
        self.temp_handle(ty)
    }

    pub(super) fn nominal_handle(
        &mut self,
        nominal: &NominalType,
        type_args: Vec<TypeHandle>,
    ) -> TypeHandle {
        debug_assert_eq!(nominal.type_args.len(), type_args.len());
        let type_args = type_args
            .into_iter()
            .map(|arg| self.resolve_ref(&arg.0))
            .collect();
        let const_args = ConstTerm::from_args(&nominal.const_args);
        self.temp_handle(TyNominal::from_nominal(nominal, type_args, const_args))
    }

    fn temp_handle(&mut self, ty: Ty) -> TypeHandle {
        TypeHandle(TypeRef::temp(self.alloc_temp(ty)))
    }

    pub(super) fn array_handle(&mut self, elem: &TypeHandle, len: &ArrayLen) -> TypeHandle {
        let elem = self.resolve_ref(&elem.0);
        let len = ConstTerm::from_array_len(len.clone());
        self.temp_handle(Ty::Array {
            elem: Box::new(elem),
            len,
        })
    }

    pub(super) fn list_handle(&mut self, elem: &TypeHandle) -> TypeHandle {
        let elem = self.resolve_ref(&elem.0);
        self.temp_handle(Ty::List {
            elem: Box::new(elem),
        })
    }

    pub(super) fn map_handle(&mut self, key: &TypeHandle, value: &TypeHandle) -> TypeHandle {
        let key = self.resolve_ref(&key.0);
        let value = self.resolve_ref(&value.0);
        self.temp_handle(Ty::Map {
            key: Box::new(key),
            value: Box::new(value),
        })
    }

    pub(super) fn tuple_handle(&mut self, elems: Vec<TypeHandle>) -> TypeHandle {
        let elems = elems
            .into_iter()
            .map(|elem| self.resolve_ref(&elem.0))
            .collect();
        self.temp_handle(Ty::Tuple(elems))
    }

    pub(super) fn nil_expr_type(&mut self, id: ExprId, span: Option<SourceSpan>) -> TypeHandle {
        let ty = self.fresh_nil_type(span);
        self.set_expr_handle(id, span, ty)
    }

    pub(super) fn fresh_nil_handle(&mut self, span: Option<SourceSpan>) -> TypeHandle {
        let ty = self.fresh_nil_type(span);
        self.temp_handle(ty)
    }

    pub(super) fn poison_expr_type(&mut self, id: ExprId, span: Option<SourceSpan>) -> TypeHandle {
        self.set_expr_handle(id, span, Ty::Error)
    }

    fn set_expr_handle(&mut self, id: ExprId, span: Option<SourceSpan>, ty: Ty) -> TypeHandle {
        self.set_expr_type(id, span, ty);
        Self::expr_handle(id)
    }

    pub(super) fn set_expr_type_from_handle(
        &mut self,
        id: ExprId,
        span: Option<SourceSpan>,
        handle: &TypeHandle,
    ) -> TypeHandle {
        let ty = self.resolve_ref(&handle.0);
        self.set_expr_type(id, span, ty);
        Self::expr_handle(id)
    }

    pub(super) fn handle_to_type(&self, handle: &TypeHandle) -> Type {
        self.type_for_storage(&self.resolve_ref(&handle.0))
    }

    pub(super) fn finalize_handle_to_type(
        &self,
        handle: &TypeHandle,
    ) -> (Type, Vec<SolverFinalizeError>) {
        let mut errors = Vec::new();
        let mut seen_types = HashSet::new();
        let mut seen_consts = HashSet::new();
        let mut cx = FinalizeCx {
            errors: &mut errors,
            seen_types: &mut seen_types,
            seen_consts: &mut seen_consts,
        };
        let ty = self.finalize_ty_inner(&self.resolve_ref(&handle.0), &mut cx);
        (ty, errors.into_iter().map(Into::into).collect())
    }

    pub(super) fn handle_is_poison(&self, handle: &TypeHandle) -> bool {
        self.resolve_ref(&handle.0).contains_error()
    }

    pub(super) fn handle_to_partial_type(&self, handle: &TypeHandle) -> Type {
        self.finalize_ty(&self.resolve_ref(&handle.0)).0
    }

    pub(super) fn add_handle_equal(
        &mut self,
        span: Option<SourceSpan>,
        left: TypeHandle,
        right: TypeHandle,
    ) {
        self.add_equal(span, left.0, right.0);
    }

    pub(super) fn add_handle_assignable(
        &mut self,
        span: Option<SourceSpan>,
        from: TypeHandle,
        to: TypeHandle,
    ) {
        self.add_assignable(span, from.0, to.0);
    }

    pub(super) fn type_assignable(&self, span: Option<SourceSpan>, from: &Type, to: &Type) -> bool {
        let mut probe = self.clone();
        probe
            .constrain_tys_assignable(
                span,
                &Ty::from_recovery_type(from),
                &Ty::from_recovery_type(to),
            )
            .is_ok()
    }

    pub(super) fn solve_pending(&mut self) -> Vec<SolverRelationError> {
        self.solve_all()
            .into_iter()
            .map(|error| self.relation_error(error))
            .collect()
    }

    pub(super) fn finalize_expr_types(&self) -> (SourceExprTypes, Vec<SolverFinalizeError>) {
        let mut errors = Vec::new();
        let mut seen_types = HashSet::new();
        let mut seen_consts = HashSet::new();
        let mut cx = FinalizeCx {
            errors: &mut errors,
            seen_types: &mut seen_types,
            seen_consts: &mut seen_consts,
        };
        let mut ids = self.expr_types.keys().copied().collect::<Vec<_>>();
        ids.sort_by_key(|id| id.0);
        let types = ids
            .into_iter()
            .map(|id| {
                let (span, ty) = self.expr_types.get(&id).expect("expression type");
                (id, (*span, self.finalize_ty_inner(ty, &mut cx)))
            })
            .collect();
        (types, errors.into_iter().map(Into::into).collect())
    }

    pub(super) fn alloc_local_type(&mut self, ty: &Type) -> SemanticLocalId {
        self.alloc_local(Ty::from_recovery_type(ty))
    }

    pub(super) fn alloc_fresh_local_type(&mut self, span: Option<SourceSpan>) -> SemanticLocalId {
        let ty = self.fresh_type(span);
        self.alloc_local(ty)
    }

    pub(super) fn alloc_local_type_from_handle(&mut self, handle: &TypeHandle) -> SemanticLocalId {
        let ty = self.resolve_ref(&handle.0);
        self.alloc_local(ty)
    }

    pub(super) fn local_type_to_type(&self, id: SemanticLocalId) -> Type {
        self.type_for_storage(self.local_type(id))
    }

    pub(super) fn set_local_type_from_type(&mut self, id: SemanticLocalId, ty: &Type) {
        self.set_local_type(id, Ty::from_recovery_type(ty));
    }

    pub(super) fn set_expr_type_from_type(
        &mut self,
        id: ExprId,
        span: Option<SourceSpan>,
        ty: &Type,
    ) {
        self.set_expr_type(id, span, Ty::from_recovery_type(ty));
    }

    pub(super) fn rewrite_contract_refs(
        &mut self,
        f: &mut impl FnMut(&ContractRef) -> ContractRef,
    ) {
        for ty in &mut self.local_types {
            rewrite_ty_contract_refs(ty, f);
        }
        for ty in &mut self.temp_types {
            rewrite_ty_contract_refs(ty, f);
        }
        for ty in self.type_bindings.values_mut() {
            rewrite_ty_contract_refs(ty, f);
        }
        for (_, ty) in self.expr_types.values_mut() {
            rewrite_ty_contract_refs(ty, f);
        }
    }

    fn instantiate_type_template(&self, ty: &Type, vars: &GenericSolverVars) -> Ty {
        match ty {
            Type::Infer | Type::InferReturn => Ty::Error,
            Type::Any => Ty::Any,
            Type::Int => Ty::Int,
            Type::Float => Ty::Float,
            Type::Bool => Ty::Bool,
            Type::String => Ty::String,
            Type::Char => Ty::Char,
            Type::Void => Ty::Void,
            Type::Func { params, ret } => Ty::Func {
                params: params
                    .iter()
                    .map(|param| TyFuncParam {
                        ty: self.instantiate_type_template(&param.ty, vars),
                        mutable: param.mutable,
                        cast_accept: param.cast_accept,
                        escape: param.escape,
                    })
                    .collect(),
                ret: Box::new(TyReturnSpec {
                    access: ret.access(),
                    ty: self.instantiate_type_template(&ret.ty(), vars),
                    iter: ret.is_iter(),
                }),
            },
            Type::Dyn(contract) => Ty::Dyn(self.instantiate_contract_ref_template(contract, vars)),
            Type::Var(id) => vars.types.get(id).cloned().unwrap_or(Ty::Var(*id)),
            Type::UnresolvedName(name) => Ty::UnresolvedName(*name),
            Type::UnresolvedNominal {
                qualifier,
                name,
                generic_args,
            } => Ty::UnresolvedNominal {
                qualifier: *qualifier,
                name: *name,
                generic_args: generic_args
                    .iter()
                    .map(|arg| self.instantiate_generic_arg_template(arg, vars))
                    .collect(),
            },
            Type::Tuple(elems) => Ty::Tuple(
                elems
                    .iter()
                    .map(|ty| self.instantiate_type_template(ty, vars))
                    .collect(),
            ),
            Type::Nominal(nominal) => {
                if nominal.kind == NominalKind::Extern {
                    debug_assert!(nominal.type_args.is_empty());
                    debug_assert!(nominal.const_args.is_empty());
                }
                self.instantiate_nominal_template(nominal, vars)
            }
            Type::List { elem } => Ty::List {
                elem: Box::new(self.instantiate_type_template(elem, vars)),
            },
            Type::Array { elem, len } => Ty::Array {
                elem: Box::new(self.instantiate_type_template(elem, vars)),
                len: self.instantiate_array_len_template(len, vars),
            },
            Type::Map { key, value } => Ty::Map {
                key: Box::new(self.instantiate_type_template(key, vars)),
                value: Box::new(self.instantiate_type_template(value, vars)),
            },
            Type::Slice { elem } => Ty::Slice {
                elem: Box::new(self.instantiate_type_template(elem, vars)),
            },
            Type::Optional { .. } => {
                unreachable!("optional syntax must be finalized before inference")
            }
        }
    }

    fn instantiate_nominal_template(&self, nominal: &NominalType, vars: &GenericSolverVars) -> Ty {
        TyNominal::from_nominal(
            nominal,
            nominal
                .type_args
                .iter()
                .map(|ty| self.instantiate_type_template(ty, vars))
                .collect(),
            nominal
                .const_args
                .iter()
                .map(|arg| Self::instantiate_const_arg_template(arg, vars))
                .collect(),
        )
    }

    fn instantiate_contract_ref_template(
        &self,
        contract: &ContractRef,
        vars: &GenericSolverVars,
    ) -> ContractRef {
        match contract {
            ContractRef::Anonymous(surface) => {
                ContractRef::Anonymous(crate::ast::AnonymousContract {
                    requirements: surface
                        .requirements
                        .iter()
                        .map(|req| crate::ast::AnonymousContractRequirement {
                            receiver: req.receiver,
                            name: req.name,
                            params: req
                                .params
                                .iter()
                                .map(|param| crate::ast::AnonymousContractParam {
                                    mutable: param.mutable,
                                    escape: param.escape,
                                    name: param.name,
                                    ty: self
                                        .instantiate_type_template(&param.ty, vars)
                                        .try_to_type_no_infer()
                                        .unwrap_or(Type::Infer),
                                })
                                .collect(),
                            ret: req.ret.with_ty(
                                self.instantiate_type_template(&req.ret.ty(), vars)
                                    .try_to_type_no_infer()
                                    .unwrap_or(Type::Infer),
                            ),
                        })
                        .collect(),
                })
            }
            ContractRef::Intersection(contracts) => ContractRef::Intersection(
                contracts
                    .iter()
                    .map(|contract| self.instantiate_contract_ref_template(contract, vars))
                    .collect(),
            ),
            ContractRef::Named { .. } | ContractRef::Infer | ContractRef::Hole(_) => {
                contract.clone()
            }
        }
    }

    fn instantiate_generic_arg_template(
        &self,
        arg: &GenericArg,
        vars: &GenericSolverVars,
    ) -> TyGenericArg {
        match arg {
            GenericArg::Type(ty) => TyGenericArg::Type(self.instantiate_type_template(ty, vars)),
            GenericArg::Const(arg) => {
                TyGenericArg::Const(Self::instantiate_const_arg_template(arg, vars))
            }
        }
    }

    fn instantiate_const_arg_template(arg: &ConstArg, vars: &GenericSolverVars) -> ConstTerm {
        match arg {
            ConstArg::Param(id) => vars
                .consts
                .get(id)
                .cloned()
                .unwrap_or(ConstTerm::Param(*id)),
            ConstArg::Value(_) | ConstArg::Name(_) => ConstTerm::from_arg(arg),
        }
    }

    fn instantiate_array_len_template(
        &self,
        len: &ArrayLen,
        vars: &GenericSolverVars,
    ) -> ConstTerm {
        match len {
            ArrayLen::Param(id) => vars
                .consts
                .get(id)
                .cloned()
                .unwrap_or(ConstTerm::Param(*id)),
            ArrayLen::Expr(expr) => {
                let consts = vars
                    .consts
                    .iter()
                    .map(|(id, term)| (*id, self.resolve_const(term)))
                    .collect();
                match super::generic::substitute_const_expr(expr.clone(), &consts) {
                    ConstExpr::Value(value) => ConstTerm::Value(value),
                    expr => ConstTerm::Expr(expr),
                }
            }
            ArrayLen::Fixed(_) | ArrayLen::Infer | ArrayLen::Named(_) => {
                ConstTerm::from_array_len(len.clone())
            }
        }
    }

    fn finalized_generic_const_arg(&self, arg: &ConstTerm) -> Option<ConstTerm> {
        let resolved = self.resolve_const(arg);
        match resolved {
            ConstTerm::Value(_) | ConstTerm::Name(_) | ConstTerm::Param(_) | ConstTerm::Expr(_) => {
                Some(resolved)
            }
            ConstTerm::ArrayInfer | ConstTerm::Infer(_) => None,
        }
    }

    fn add_equal(&mut self, span: Option<SourceSpan>, left: TypeRef, right: TypeRef) {
        self.constraints.push(Constraint {
            span,
            kind: ConstraintKind::Equal(left, right),
        });
    }

    fn add_assignable(&mut self, span: Option<SourceSpan>, from: TypeRef, to: TypeRef) {
        self.constraints.push(Constraint {
            span,
            kind: ConstraintKind::Assignable { from, to },
        });
    }

    fn solve_all(&mut self) -> Vec<SolveError> {
        let constraints = std::mem::take(&mut self.constraints);
        let equal = constraints
            .iter()
            .filter(|constraint| matches!(constraint.kind, ConstraintKind::Equal(..)));
        let assignable = constraints
            .iter()
            .filter(|constraint| matches!(constraint.kind, ConstraintKind::Assignable { .. }));
        let mut errors = vec![];

        for constraint in equal.chain(assignable) {
            let result = match &constraint.kind {
                ConstraintKind::Equal(left, right) => {
                    self.unify_equal(constraint.span, left.clone(), right.clone())
                }
                ConstraintKind::Assignable { from, to } => {
                    self.constrain_assignable(constraint.span, from.clone(), to.clone())
                }
            };
            if let Err(error) = result
                && !errors.contains(&error)
            {
                errors.push(error);
            }
        }
        errors
    }

    fn unify_equal(
        &mut self,
        span: Option<SourceSpan>,
        left: TypeRef,
        right: TypeRef,
    ) -> Result<Ty, SolveError> {
        let left = self.resolve_owned_ref(left);
        let right = self.resolve_owned_ref(right);
        self.unify_tys_equal(span, &left, &right)
    }

    fn constrain_assignable(
        &mut self,
        span: Option<SourceSpan>,
        from: TypeRef,
        to: TypeRef,
    ) -> Result<Ty, SolveError> {
        let from = self.resolve_owned_ref(from);
        let to = self.resolve_owned_ref(to);
        self.constrain_tys_assignable(span, &from, &to)
    }

    fn fresh_type(&mut self, span: Option<SourceSpan>) -> Ty {
        let id = InferVarId(self.next_type_var);
        self.next_type_var += 1;
        if let Some(span) = span {
            self.type_spans.insert(id, span);
        }
        Ty::Infer(id)
    }

    fn fresh_const(&mut self, span: Option<SourceSpan>) -> ConstTerm {
        let id = ConstInferVarId(self.next_const_var);
        self.next_const_var += 1;
        if let Some(span) = span {
            self.const_spans.insert(id, span);
        }
        ConstTerm::Infer(id)
    }

    fn fresh_nil_type(&mut self, span: Option<SourceSpan>) -> Ty {
        let ty = self.fresh_type(span);
        let Ty::Infer(id) = ty else {
            unreachable!();
        };
        self.nil_vars.insert(id);
        Ty::Infer(id)
    }

    fn is_nil_var(&self, id: InferVarId) -> bool {
        self.nil_vars.contains(&id)
    }

    fn alloc_local(&mut self, ty: Ty) -> SemanticLocalId {
        let id = SemanticLocalId(self.local_types.len() as u32);
        self.local_types.push(ty);
        id
    }

    fn local_type(&self, id: SemanticLocalId) -> &Ty {
        &self.local_types[id.0 as usize]
    }

    fn set_local_type(&mut self, id: SemanticLocalId, ty: Ty) {
        self.local_types[id.0 as usize] = ty;
    }

    fn alloc_temp(&mut self, ty: Ty) -> TempTypeId {
        let id = TempTypeId(self.temp_types.len() as u32);
        self.temp_types.push(ty);
        id
    }

    fn temp_type(&self, id: TempTypeId) -> &Ty {
        &self.temp_types[id.0 as usize]
    }

    fn expr_type(&self, id: ExprId) -> Option<&Ty> {
        self.expr_types.get(&id).map(|(_, ty)| ty)
    }

    fn set_expr_type(&mut self, id: ExprId, span: Option<SourceSpan>, ty: Ty) {
        self.expr_types.insert(id, (span, ty));
    }

    fn relate_tys(
        &mut self,
        span: Option<SourceSpan>,
        expected: &Ty,
        found: &Ty,
        relation: TyRelation,
    ) -> Result<Ty, SolveError> {
        match relation {
            TyRelation::Equal => self.unify_tys_equal(span, expected, found),
            TyRelation::Assignable => self.constrain_tys_assignable(span, found, expected),
        }
    }

    fn relate_boxed(
        &mut self,
        span: Option<SourceSpan>,
        expected: &Ty,
        found: &Ty,
        relation: TyRelation,
    ) -> Result<Box<Ty>, SolveError> {
        Ok(Box::new(self.relate_tys(span, expected, found, relation)?))
    }

    fn relate_boxed_assignable(
        &mut self,
        span: Option<SourceSpan>,
        expected_elem: &Ty,
        found_elem: &Ty,
        expected: &Ty,
        found: &Ty,
    ) -> Result<Box<Ty>, SolveError> {
        match self.relate_boxed(span, expected_elem, found_elem, TyRelation::Assignable) {
            Ok(ty) => Ok(ty),
            Err(_) => Err(SolveError::type_mismatch(
                expected.clone(),
                found.clone(),
                span,
            )),
        }
    }

    fn outer_mismatch<T>(
        result: Result<T, SolveError>,
        mismatch: bool,
        expected: &Ty,
        found: &Ty,
        span: Option<SourceSpan>,
    ) -> Result<T, SolveError> {
        match result {
            Ok(value) => Ok(value),
            Err(_) if mismatch => Err(SolveError::type_mismatch(
                expected.clone(),
                found.clone(),
                span,
            )),
            Err(error) => Err(error),
        }
    }

    fn relate_ty_lists(
        &mut self,
        span: Option<SourceSpan>,
        expected: Vec<Ty>,
        found: Vec<Ty>,
        relation: TyRelation,
    ) -> Result<Vec<Ty>, SolveError> {
        let mut related = Vec::with_capacity(expected.len());
        for (expected, found) in expected.into_iter().zip(found) {
            related.push(self.relate_tys(span, &expected, &found, relation)?);
        }
        Ok(related)
    }

    fn unify_func_invariant(
        &mut self,
        span: Option<SourceSpan>,
        expected_func: TyFuncParts,
        found_func: TyFuncParts,
        expected: &Ty,
        found: &Ty,
    ) -> Result<Ty, SolveError> {
        Self::ensure_arity(
            expected_func.params.len(),
            found_func.params.len(),
            expected,
            found,
            span,
        )?;
        let params = expected_func
            .params
            .into_iter()
            .zip(found_func.params)
            .map(|(expected, found)| {
                if expected.mutable != found.mutable || expected.escape != found.escape {
                    return Err(SolveError::func_param_mismatch(expected, found, span));
                }
                let mutable = expected.mutable;
                let cast_accept = expected.cast_accept || found.cast_accept;
                let escape = expected.escape;
                let ty = self.unify_tys_equal(span, &expected.ty, &found.ty)?;
                Ok(TyFuncParam {
                    ty,
                    mutable,
                    cast_accept,
                    escape,
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Ty::Func {
            params,
            ret: Box::new(self.unify_return_specs_equal(
                span,
                expected_func.ret,
                found_func.ret,
            )?),
        })
    }

    fn unify_return_specs_equal(
        &mut self,
        span: Option<SourceSpan>,
        expected: TyReturnSpec,
        found: TyReturnSpec,
    ) -> Result<TyReturnSpec, SolveError> {
        if expected.access != found.access || expected.iter != found.iter {
            return Err(SolveError::type_mismatch(
                Ty::Func {
                    params: vec![],
                    ret: Box::new(expected),
                },
                Ty::Func {
                    params: vec![],
                    ret: Box::new(found),
                },
                span,
            ));
        }
        Ok(expected.with_ty(self.unify_tys_equal(span, &expected.ty, &found.ty)?))
    }

    fn unify_tys_equal(
        &mut self,
        span: Option<SourceSpan>,
        left: &Ty,
        right: &Ty,
    ) -> Result<Ty, SolveError> {
        let left = self.resolve_ty(left);
        let right = self.resolve_ty(right);
        if left == right {
            return Ok(left);
        }

        let expected = left.clone();
        let found = right.clone();
        if let (Ty::Nominal(left), Ty::Nominal(right)) = (left.clone(), right.clone()) {
            return self.unify_nominal_equal(span, left, right, expected, found);
        }

        match (left, right) {
            (Ty::Error, _) | (_, Ty::Error) => Ok(Ty::Error),
            (Ty::Infer(id), ty) | (ty, Ty::Infer(id)) => {
                self.bind_type(id, &ty, span)?;
                Ok(self.resolve_ty(&Ty::Infer(id)))
            }
            (
                Ty::Func { params, ret },
                Ty::Func {
                    params: found_params,
                    ret: found_ret,
                },
            ) => self.unify_func_invariant(
                span,
                TyFuncParts { params, ret: *ret },
                TyFuncParts {
                    params: found_params,
                    ret: *found_ret,
                },
                &expected,
                &found,
            ),
            (
                Ty::UnresolvedNominal {
                    qualifier,
                    name,
                    generic_args,
                },
                Ty::UnresolvedNominal {
                    qualifier: found_qualifier,
                    name: found_name,
                    generic_args: found_args,
                },
            ) => {
                let same_head = qualifier == found_qualifier && name == found_name;
                if !same_head {
                    return Err(SolveError::type_mismatch(expected, found, span));
                }
                Self::ensure_arity(
                    generic_args.len(),
                    found_args.len(),
                    &expected,
                    &found,
                    span,
                )?;
                let generic_args = self.unify_generic_args_equal(
                    span,
                    generic_args,
                    found_args,
                    &expected,
                    &found,
                )?;
                Ok(Ty::UnresolvedNominal {
                    qualifier,
                    name,
                    generic_args,
                })
            }
            (Ty::Tuple(elems), Ty::Tuple(found_elems)) => {
                Self::ensure_arity(elems.len(), found_elems.len(), &expected, &found, span)?;
                Ok(Ty::Tuple(self.relate_ty_lists(
                    span,
                    elems,
                    found_elems,
                    TyRelation::Equal,
                )?))
            }
            (Ty::List { elem }, Ty::List { elem: found_elem }) => Ok(Ty::List {
                elem: self.relate_boxed(
                    span,
                    elem.as_ref(),
                    found_elem.as_ref(),
                    TyRelation::Equal,
                )?,
            }),
            (
                Ty::Array { elem, len },
                Ty::Array {
                    elem: found_elem,
                    len: found_len,
                },
            ) => Ok(Ty::Array {
                elem: self.relate_boxed(
                    span,
                    elem.as_ref(),
                    found_elem.as_ref(),
                    TyRelation::Equal,
                )?,
                len: self.unify_const_equal(span, &len, &found_len)?,
            }),
            (
                Ty::Map { key, value },
                Ty::Map {
                    key: found_key,
                    value: found_value,
                },
            ) => Ok(Ty::Map {
                key: self.relate_boxed(
                    span,
                    key.as_ref(),
                    found_key.as_ref(),
                    TyRelation::Equal,
                )?,
                value: self.relate_boxed(
                    span,
                    value.as_ref(),
                    found_value.as_ref(),
                    TyRelation::Equal,
                )?,
            }),
            (Ty::Slice { elem }, Ty::Slice { elem: found_elem }) => Ok(Ty::Slice {
                elem: self.relate_boxed(
                    span,
                    elem.as_ref(),
                    found_elem.as_ref(),
                    TyRelation::Equal,
                )?,
            }),
            _ => Err(SolveError::type_mismatch(expected, found, span)),
        }
    }

    fn constrain_tys_assignable(
        &mut self,
        span: Option<SourceSpan>,
        from: &Ty,
        to: &Ty,
    ) -> Result<Ty, SolveError> {
        let from = self.resolve_ty(from);
        let to = self.resolve_ty(to);
        if from == to {
            return self.unify_tys_equal(span, &from, &to);
        }
        if matches!(from, Ty::Error) || matches!(to, Ty::Error) {
            return Ok(Ty::Error);
        }
        if let Some(result) = self.assign_nil_origin(span, &from, &to) {
            return result;
        }
        if matches!(from, Ty::Any) || matches!(to, Ty::Any) {
            return Ok(to);
        }
        if matches!(from, Ty::Infer(_)) || matches!(to, Ty::Infer(_)) {
            return self.unify_tys_equal(span, &from, &to);
        }
        if let Some(inner) = self.option_inner(&to).cloned()
            && !self.is_option(&from)
        {
            self.constrain_tys_assignable(span, &from, &inner)?;
            return Ok(to);
        }

        let expected = to.clone();
        let found = from.clone();
        if let (Ty::Nominal(from), Ty::Nominal(to)) = (from.clone(), to.clone()) {
            return self.constrain_nominal_assignable(span, from, to, expected, found);
        }

        match (from, to) {
            (
                Ty::Func { params, ret },
                Ty::Func {
                    params: to_params,
                    ret: to_ret,
                },
            ) => self.unify_func_invariant(
                span,
                TyFuncParts {
                    params: to_params,
                    ret: *to_ret,
                },
                TyFuncParts { params, ret: *ret },
                &expected,
                &found,
            ),
            (Ty::Tuple(elems), Ty::Tuple(to_elems)) => {
                Self::ensure_arity(to_elems.len(), elems.len(), &expected, &found, span)?;
                Ok(Ty::Tuple(self.relate_ty_lists(
                    span,
                    to_elems,
                    elems,
                    TyRelation::Assignable,
                )?))
            }
            (Ty::List { elem }, Ty::List { elem: to_elem }) => Ok(Ty::List {
                elem: self.relate_boxed_assignable(
                    span,
                    to_elem.as_ref(),
                    elem.as_ref(),
                    &expected,
                    &found,
                )?,
            }),
            (
                Ty::Array { elem, len },
                Ty::Array {
                    elem: to_elem,
                    len: to_len,
                },
            ) => Ok(Ty::Array {
                elem: self.relate_boxed_assignable(
                    span,
                    to_elem.as_ref(),
                    elem.as_ref(),
                    &expected,
                    &found,
                )?,
                len: self.unify_const_equal(span, &len, &to_len)?,
            }),
            (
                Ty::Map { key, value },
                Ty::Map {
                    key: to_key,
                    value: to_value,
                },
            ) => Ok(Ty::Map {
                key: self.relate_boxed_assignable(
                    span,
                    to_key.as_ref(),
                    key.as_ref(),
                    &expected,
                    &found,
                )?,
                value: self.relate_boxed_assignable(
                    span,
                    to_value.as_ref(),
                    value.as_ref(),
                    &expected,
                    &found,
                )?,
            }),
            (Ty::Slice { elem }, Ty::Slice { elem: to_elem }) => Ok(Ty::Slice {
                elem: self.relate_boxed_assignable(
                    span,
                    to_elem.as_ref(),
                    elem.as_ref(),
                    &expected,
                    &found,
                )?,
            }),
            (Ty::Array { elem, .. } | Ty::List { elem }, Ty::Slice { elem: to_elem }) => {
                Ok(Ty::Slice {
                    elem: self.relate_boxed_assignable(
                        span,
                        to_elem.as_ref(),
                        elem.as_ref(),
                        &expected,
                        &found,
                    )?,
                })
            }
            _ => Err(SolveError::type_mismatch(expected, found, span)),
        }
    }

    fn assign_nil_origin(
        &mut self,
        span: Option<SourceSpan>,
        from: &Ty,
        to: &Ty,
    ) -> Option<Result<Ty, SolveError>> {
        let Ty::Infer(id) = from else {
            return None;
        };
        if !self.is_nil_var(*id) {
            return None;
        }
        let target_accepts_nil = matches!(to, Ty::Infer(_)) || self.is_option(to);
        if target_accepts_nil {
            return Some(self.unify_tys_equal(span, from, to));
        }
        Some(Err(SolveError::type_mismatch(
            to.clone(),
            Ty::Infer(*id),
            span,
        )))
    }

    fn unify_nominal_equal(
        &mut self,
        span: Option<SourceSpan>,
        left: TyNominal,
        right: TyNominal,
        expected: Ty,
        found: Ty,
    ) -> Result<Ty, SolveError> {
        if !left.same_head(&right) {
            return Err(SolveError::type_mismatch(expected, found, span));
        }
        let head = left.clone();
        let type_args =
            self.relate_ty_lists(span, left.type_args, right.type_args, TyRelation::Equal)?;
        let const_args = self.relate_const_arg_lists(span, left.const_args, right.const_args)?;
        Ok(head.with_args(type_args, const_args))
    }

    fn constrain_nominal_assignable(
        &mut self,
        span: Option<SourceSpan>,
        from: TyNominal,
        to: TyNominal,
        expected: Ty,
        found: Ty,
    ) -> Result<Ty, SolveError> {
        if !from.same_head(&to) {
            return Err(SolveError::type_mismatch(expected, found, span));
        }
        let head = to.clone();
        let outer_mismatch = self.is_core_option_nominal(&to);
        let type_args = Self::outer_mismatch(
            self.relate_ty_lists(span, to.type_args, from.type_args, TyRelation::Assignable),
            outer_mismatch,
            &expected,
            &found,
            span,
        )?;
        let const_args = Self::outer_mismatch(
            self.relate_const_arg_lists(span, to.const_args, from.const_args),
            outer_mismatch,
            &expected,
            &found,
            span,
        )?;
        Ok(head.with_args(type_args, const_args))
    }

    fn unify_generic_args_equal(
        &mut self,
        span: Option<SourceSpan>,
        left: Vec<TyGenericArg>,
        right: Vec<TyGenericArg>,
        expected: &Ty,
        found: &Ty,
    ) -> Result<Vec<TyGenericArg>, SolveError> {
        left.into_iter()
            .zip(right)
            .map(|(left, right)| match (left, right) {
                (TyGenericArg::Type(left), TyGenericArg::Type(right)) => Ok(TyGenericArg::Type(
                    self.unify_tys_equal(span, &left, &right)?,
                )),
                (TyGenericArg::Const(left), TyGenericArg::Const(right)) => Ok(TyGenericArg::Const(
                    self.unify_const_equal(span, &left, &right)?,
                )),
                _ => Err(SolveError::type_mismatch(
                    expected.clone(),
                    found.clone(),
                    span,
                )),
            })
            .collect()
    }

    fn relate_const_arg_lists(
        &mut self,
        span: Option<SourceSpan>,
        expected: Vec<ConstTerm>,
        found: Vec<ConstTerm>,
    ) -> Result<Vec<ConstTerm>, SolveError> {
        expected
            .into_iter()
            .zip(found)
            .map(|(expected, found)| self.unify_const_equal(span, &expected, &found))
            .collect()
    }

    fn unify_const_equal(
        &mut self,
        span: Option<SourceSpan>,
        left: &ConstTerm,
        right: &ConstTerm,
    ) -> Result<ConstTerm, SolveError> {
        let left = self.resolve_const(left);
        let right = self.resolve_const(right);
        if left == right {
            return Ok(left);
        }
        match (left, right) {
            (ConstTerm::ArrayInfer, term) | (term, ConstTerm::ArrayInfer) => Ok(term),
            (ConstTerm::Infer(id), term) | (term, ConstTerm::Infer(id)) => {
                self.bind_const(id, &term, span)?;
                Ok(self.resolve_const(&ConstTerm::Infer(id)))
            }
            (expected, found) => Err(SolveError::const_mismatch(expected, found, span)),
        }
    }

    fn ensure_arity(
        expected_len: usize,
        found_len: usize,
        expected: &Ty,
        found: &Ty,
        span: Option<SourceSpan>,
    ) -> Result<(), SolveError> {
        if expected_len == found_len {
            Ok(())
        } else {
            Err(SolveError::type_mismatch(
                expected.clone(),
                found.clone(),
                span,
            ))
        }
    }

    fn bind_type(
        &mut self,
        var: InferVarId,
        ty: &Ty,
        span: Option<SourceSpan>,
    ) -> Result<(), SolveError> {
        let found = self.resolve_ty(ty);
        let binds_to_self = matches!(found, Ty::Infer(other) if other == var);
        if binds_to_self {
            return Ok(());
        }
        self.propagate_nil_origin(var, &found, span)?;
        if let Some(bound) = self.type_bindings.get(&var) {
            let existing = self.resolve_ty(bound);
            if existing == found {
                return Ok(());
            }
            return Err(SolveError::TypeAlreadyBound {
                var,
                existing: Box::new(existing),
                found: Box::new(found),
                span,
            });
        }
        if self.type_occurs_in_ty(var, &found) {
            return Err(SolveError::TypeOccurs { var, span });
        }
        self.type_bindings.insert(var, found);
        Ok(())
    }

    fn propagate_nil_origin(
        &mut self,
        var: InferVarId,
        found: &Ty,
        span: Option<SourceSpan>,
    ) -> Result<(), SolveError> {
        if self.is_nil_var(var) {
            match found {
                Ty::Infer(other) => {
                    self.nil_vars.insert(*other);
                    Ok(())
                }
                ty if self.is_option(ty) => Ok(()),
                _ => Err(SolveError::type_mismatch(
                    found.clone(),
                    Ty::Infer(var),
                    span,
                )),
            }
        } else if let Ty::Infer(id) = found
            && self.is_nil_var(*id)
        {
            self.nil_vars.insert(var);
            Ok(())
        } else {
            Ok(())
        }
    }

    fn bind_const(
        &mut self,
        var: ConstInferVarId,
        found: &ConstTerm,
        span: Option<SourceSpan>,
    ) -> Result<(), SolveError> {
        let found = self.resolve_const(found);
        if found.is_self_binding(var) {
            return Ok(());
        }
        if let Some(bound) = self.const_bindings.get(&var) {
            let existing = self.resolve_const(bound);
            if existing == found {
                return Ok(());
            }
            return Err(SolveError::ConstAlreadyBound {
                var,
                existing,
                found,
                span,
            });
        }
        if self.const_occurs_in_term(var, &found) {
            return Err(SolveError::ConstOccurs { var, span });
        }
        self.const_bindings.insert(var, found);
        Ok(())
    }

    fn resolve_ref(&self, r: &TypeRef) -> Ty {
        match r {
            TypeRef::Concrete(ty) => self.resolve_ty(ty),
            TypeRef::Local(id) => self.resolve_ty(self.local_type(*id)),
            TypeRef::Temp(id) => self.resolve_ty(self.temp_type(*id)),
            TypeRef::Expr(id) => {
                let ty = self
                    .expr_type(*id)
                    .expect("expression type must be set before use");
                self.resolve_ty(ty)
            }
        }
    }

    fn resolve_owned_ref(&self, r: TypeRef) -> Ty {
        match r {
            TypeRef::Concrete(ty) => self.resolve_ty(&ty),
            TypeRef::Local(id) => self.resolve_ty(self.local_type(id)),
            TypeRef::Temp(id) => self.resolve_ty(self.temp_type(id)),
            TypeRef::Expr(id) => {
                let ty = self
                    .expr_type(id)
                    .expect("expression type must be set before use");
                self.resolve_ty(ty)
            }
        }
    }

    fn resolve_tys(&self, types: &[Ty]) -> Vec<Ty> {
        types.iter().map(|ty| self.resolve_ty(ty)).collect()
    }

    fn resolve_consts(&self, args: &[ConstTerm]) -> Vec<ConstTerm> {
        args.iter().map(|arg| self.resolve_const(arg)).collect()
    }

    fn resolve_nominal_args(
        &self,
        type_args: &[Ty],
        const_args: &[ConstTerm],
    ) -> (Vec<Ty>, Vec<ConstTerm>) {
        (self.resolve_tys(type_args), self.resolve_consts(const_args))
    }

    fn resolve_nominal(&self, nominal: &TyNominal) -> Ty {
        let (type_args, const_args) =
            self.resolve_nominal_args(&nominal.type_args, &nominal.const_args);
        nominal.with_cloned_args(type_args, const_args)
    }

    fn resolve_ty(&self, ty: &Ty) -> Ty {
        match ty {
            Ty::Infer(id) => match self.type_bindings.get(id) {
                Some(bound) => self.resolve_ty(bound),
                None => Ty::Infer(*id),
            },
            Ty::Func { params, ret } => Ty::Func {
                params: params
                    .iter()
                    .map(|param| TyFuncParam {
                        ty: self.resolve_ty(&param.ty),
                        mutable: param.mutable,
                        cast_accept: param.cast_accept,
                        escape: param.escape,
                    })
                    .collect(),
                ret: Box::new(ret.with_ty(self.resolve_ty(&ret.ty))),
            },
            Ty::UnresolvedNominal {
                qualifier,
                name,
                generic_args,
            } => Ty::UnresolvedNominal {
                qualifier: *qualifier,
                name: *name,
                generic_args: generic_args
                    .iter()
                    .map(|arg| match arg {
                        TyGenericArg::Type(ty) => TyGenericArg::Type(self.resolve_ty(ty)),
                        TyGenericArg::Const(arg) => TyGenericArg::Const(self.resolve_const(arg)),
                    })
                    .collect(),
            },
            Ty::Tuple(elems) => Ty::Tuple(elems.iter().map(|ty| self.resolve_ty(ty)).collect()),
            Ty::Nominal(nominal) => self.resolve_nominal(nominal),
            Ty::List { elem } => Ty::List {
                elem: Box::new(self.resolve_ty(elem)),
            },
            Ty::Array { elem, len } => Ty::Array {
                elem: Box::new(self.resolve_ty(elem)),
                len: self.resolve_const(len),
            },
            Ty::Map { key, value } => Ty::Map {
                key: Box::new(self.resolve_ty(key)),
                value: Box::new(self.resolve_ty(value)),
            },
            Ty::Slice { elem } => Ty::Slice {
                elem: Box::new(self.resolve_ty(elem)),
            },
            Ty::Error
            | Ty::Dyn(_)
            | Ty::Any
            | Ty::Int
            | Ty::Float
            | Ty::Bool
            | Ty::String
            | Ty::Char
            | Ty::Void
            | Ty::Var(_)
            | Ty::UnresolvedName(_) => ty.clone(),
        }
    }

    fn resolve_const(&self, term: &ConstTerm) -> ConstTerm {
        match term {
            ConstTerm::Infer(id) => self
                .const_bindings
                .get(id)
                .map_or(ConstTerm::Infer(*id), |term| self.resolve_const(term)),
            ConstTerm::Value(_)
            | ConstTerm::Name(_)
            | ConstTerm::Param(_)
            | ConstTerm::Expr(_)
            | ConstTerm::ArrayInfer => term.clone(),
        }
    }

    fn type_occurs_in_ty(&self, var: InferVarId, ty: &Ty) -> bool {
        match self.resolve_ty(ty) {
            Ty::Infer(id) => id == var,
            Ty::Func { params, ret } => {
                params
                    .iter()
                    .any(|param| self.type_occurs_in_ty(var, &param.ty))
                    || self.type_occurs_in_ty(var, &ret.ty)
            }
            Ty::UnresolvedNominal { generic_args, .. } => {
                generic_args.iter().any(|arg| match arg {
                    TyGenericArg::Type(ty) => self.type_occurs_in_ty(var, ty),
                    TyGenericArg::Const(_) => false,
                })
            }
            Ty::Tuple(elems) => elems.iter().any(|ty| self.type_occurs_in_ty(var, ty)),
            Ty::Nominal(nominal) => nominal
                .type_args
                .iter()
                .any(|ty| self.type_occurs_in_ty(var, ty)),
            Ty::List { elem } | Ty::Slice { elem } | Ty::Array { elem, .. } => {
                self.type_occurs_in_ty(var, &elem)
            }
            Ty::Map { key, value } => {
                self.type_occurs_in_ty(var, &key) || self.type_occurs_in_ty(var, &value)
            }
            Ty::Error
            | Ty::Dyn(_)
            | Ty::Any
            | Ty::Int
            | Ty::Float
            | Ty::Bool
            | Ty::String
            | Ty::Char
            | Ty::Void
            | Ty::Var(_)
            | Ty::UnresolvedName(_) => false,
        }
    }

    fn const_occurs_in_term(&self, var: ConstInferVarId, term: &ConstTerm) -> bool {
        matches!(self.resolve_const(term), ConstTerm::Infer(id) if id == var)
    }

    fn finalize_ty(&self, ty: &Ty) -> (Type, Vec<InferError>) {
        let mut errors = Vec::new();
        let mut seen_types = HashSet::new();
        let mut seen_consts = HashSet::new();
        let ty = {
            let mut cx = FinalizeCx {
                errors: &mut errors,
                seen_types: &mut seen_types,
                seen_consts: &mut seen_consts,
            };
            self.finalize_ty_inner(ty, &mut cx)
        };
        (ty, errors)
    }

    fn finalize_ty_inner(&self, ty: &Ty, cx: &mut FinalizeCx<'_>) -> Type {
        match self.resolve_ty(ty) {
            Ty::Error => Type::Infer,
            Ty::Infer(id) => {
                self.push_unresolved_type(id, cx);
                Type::Infer
            }
            Ty::Any => Type::Any,
            Ty::Int => Type::Int,
            Ty::Float => Type::Float,
            Ty::Bool => Type::Bool,
            Ty::String => Type::String,
            Ty::Char => Type::Char,
            Ty::Void => Type::Void,
            Ty::Func { params, ret } => Type::Func {
                params: params
                    .into_iter()
                    .map(|param| FuncParam {
                        ty: self.finalize_ty_inner(&param.ty, cx),
                        mutable: param.mutable,
                        cast_accept: param.cast_accept,
                        escape: param.escape,
                    })
                    .collect(),
                ret: Box::new(if ret.iter {
                    ReturnSpec::iter()
                } else {
                    match ret.access {
                        ReturnAccess::Value => {
                            ReturnSpec::value(self.finalize_ty_inner(&ret.ty, cx))
                        }
                        ReturnAccess::Place => {
                            ReturnSpec::place(self.finalize_ty_inner(&ret.ty, cx))
                        }
                    }
                }),
            },
            Ty::Dyn(contract) => Type::Dyn(contract),
            Ty::Var(id) => Type::Var(id),
            Ty::UnresolvedName(name) => Type::UnresolvedName(name),
            Ty::UnresolvedNominal {
                qualifier,
                name,
                generic_args,
            } => {
                let generic_args = generic_args
                    .into_iter()
                    .map(|arg| self.finalize_generic_arg(arg, cx))
                    .collect::<Option<Vec<_>>>();
                match generic_args {
                    Some(generic_args) => Type::UnresolvedNominal {
                        qualifier,
                        name,
                        generic_args,
                    },
                    None => Type::Infer,
                }
            }
            Ty::Tuple(elems) => Type::Tuple(
                elems
                    .into_iter()
                    .map(|ty| self.finalize_ty_inner(&ty, cx))
                    .collect(),
            ),
            Ty::Nominal(nominal) => self.finalize_nominal(nominal, cx),
            Ty::List { elem } => Type::List {
                elem: Box::new(self.finalize_ty_inner(&elem, cx)),
            },
            Ty::Array { elem, len } => {
                let elem = self.finalize_ty_inner(&elem, cx);
                match self.finalize_array_len(&len, cx) {
                    Some(len) => Type::Array {
                        elem: Box::new(elem),
                        len,
                    },
                    None => Type::Infer,
                }
            }
            Ty::Map { key, value } => Type::Map {
                key: Box::new(self.finalize_ty_inner(&key, cx)),
                value: Box::new(self.finalize_ty_inner(&value, cx)),
            },
            Ty::Slice { elem } => Type::Slice {
                elem: Box::new(self.finalize_ty_inner(&elem, cx)),
            },
        }
    }

    fn finalize_nominal(&self, nominal: TyNominal, cx: &mut FinalizeCx<'_>) -> Type {
        let type_args = self.finalize_tys(nominal.type_args, cx);
        match self.finalize_const_args(nominal.const_args, cx) {
            Some(const_args) => Type::Nominal(NominalType {
                id: nominal.id,
                kind: nominal.kind,
                name: nominal.name,
                type_args,
                const_args,
                origin: nominal.origin,
            }),
            None => Type::Infer,
        }
    }

    fn finalize_tys(&self, types: Vec<Ty>, cx: &mut FinalizeCx<'_>) -> Vec<Type> {
        types
            .into_iter()
            .map(|ty| self.finalize_ty_inner(&ty, cx))
            .collect()
    }

    fn finalize_const_args(
        &self,
        args: Vec<ConstTerm>,
        cx: &mut FinalizeCx<'_>,
    ) -> Option<Vec<ConstArg>> {
        args.into_iter()
            .map(|arg| self.finalize_const_arg(&arg, cx))
            .collect()
    }

    fn finalize_generic_arg(
        &self,
        arg: TyGenericArg,
        cx: &mut FinalizeCx<'_>,
    ) -> Option<GenericArg> {
        match arg {
            TyGenericArg::Type(ty) => Some(GenericArg::Type(self.finalize_ty_inner(&ty, cx))),
            TyGenericArg::Const(arg) => Some(GenericArg::Const(self.finalize_const_arg(&arg, cx)?)),
        }
    }

    fn finalize_const_arg(&self, arg: &ConstTerm, cx: &mut FinalizeCx<'_>) -> Option<ConstArg> {
        match self.resolve_const(arg) {
            ConstTerm::Infer(id) => {
                self.push_unresolved_const(id, cx);
                None
            }
            ConstTerm::ArrayInfer => None,
            term => term.to_arg_no_infer(),
        }
    }

    fn finalize_array_len(&self, len: &ConstTerm, cx: &mut FinalizeCx<'_>) -> Option<ArrayLen> {
        match self.resolve_const(len) {
            ConstTerm::Infer(id) => {
                self.push_unresolved_const(id, cx);
                None
            }
            term => {
                let len = term.to_array_len_no_infer();
                debug_assert!(
                    len.is_some(),
                    "invalid const value cannot finalize as array length"
                );
                len
            }
        }
    }

    fn push_unresolved_type(&self, id: InferVarId, cx: &mut FinalizeCx<'_>) {
        if cx.seen_types.insert(id) {
            let span = self.type_spans.get(&id).copied();
            cx.errors.push(InferError::UnresolvedType { span });
        }
    }

    fn push_unresolved_const(&self, id: ConstInferVarId, cx: &mut FinalizeCx<'_>) {
        if cx.seen_consts.insert(id) {
            let span = self.const_spans.get(&id).copied();
            cx.errors.push(InferError::UnresolvedConst { span });
        }
    }

    fn relation_error(&self, error: SolveError) -> SolverRelationError {
        match error {
            SolveError::TypeMismatch {
                expected,
                found,
                span,
            }
            | SolveError::TypeAlreadyBound {
                existing: expected,
                found,
                span,
                ..
            } => SolverRelationError::TypeMismatch {
                expected: self.type_for_storage(&expected),
                found: self.type_for_storage(&found),
                span,
            },
            SolveError::ConstMismatch {
                expected,
                found,
                span,
            }
            | SolveError::ConstAlreadyBound {
                existing: expected,
                found,
                span,
                ..
            } => self.const_mismatch_error(&expected, &found, span),
            SolveError::TypeOccurs { span, .. } | SolveError::ConstOccurs { span, .. } => {
                SolverRelationError::RecursiveInference { span }
            }
        }
    }

    fn const_mismatch_error(
        &self,
        expected: &ConstTerm,
        found: &ConstTerm,
        span: Option<SourceSpan>,
    ) -> SolverRelationError {
        SolverRelationError::ConstMismatch {
            expected: self.const_diagnostic(expected),
            found: self.const_diagnostic(found),
            span,
        }
    }

    fn const_diagnostic(&self, term: &ConstTerm) -> ConstDiagnostic {
        self.resolve_const(term).diagnostic()
    }
}

impl From<InferError> for SolverFinalizeError {
    fn from(error: InferError) -> Self {
        match error {
            InferError::UnresolvedType { span } => Self::UnresolvedType { span },
            InferError::UnresolvedConst { span } => Self::UnresolvedConst { span },
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::OnceLock;

    use super::*;
    use crate::source::{SourceId, SourceKind, SourceTable};
    fn source() -> SourceId {
        static SOURCE: OnceLock<SourceId> = OnceLock::new();
        *SOURCE.get_or_init(|| {
            let mut sources = SourceTable::default();
            sources.add(SourceKind::Virtual, "test", None, "")
        })
    }

    fn span(start: usize, end: usize) -> Option<SourceSpan> {
        (start <= end).then(|| SourceSpan::new(source(), start, end))
    }
    fn ty_ref(ty: Ty) -> TypeRef {
        TypeRef::concrete(ty)
    }

    fn infer_id(ty: &Ty) -> InferVarId {
        match ty {
            Ty::Infer(id) => *id,
            _ => panic!("expected inference var"),
        }
    }

    fn fixed_array(elem: Ty, len: usize) -> Ty {
        Ty::Array {
            elem: Box::new(elem),
            len: ConstTerm::from_usize(len),
        }
    }
    #[test]
    fn equal_occurs_check() {
        let mut solver = Solver::default();
        let var = infer_id(&solver.fresh_type(span(1, 2)));
        assert_eq!(
            solver.unify_equal(
                span(3, 4),
                ty_ref(Ty::Infer(var)),
                ty_ref(fixed_array(Ty::Infer(var), 1)),
            ),
            Err(SolveError::TypeOccurs {
                var,
                span: span(3, 4),
            })
        );
    }
}
