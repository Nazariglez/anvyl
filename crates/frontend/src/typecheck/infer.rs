use std::collections::{HashMap, HashSet};

use super::{
    ConstDiagnostic, ConstSubst, GenericArgs, GenericParams, TypeSubst,
    const_term::{ConstInferVarId, ConstTerm},
};
use crate::{
    ast::{
        ArrayLen, ConstArg, ConstParamId, ExprId, FuncParam, GenericArg, Ident, ModuleOrigin,
        NominalKind, NominalType, Type, TypeVarId,
    },
    span::{SourceSpan, Span},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
struct InferVarId(u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct TyFuncParam {
    ty: Ty,
    mutable: bool,
    cast_accept: bool,
}

impl TyFuncParam {
    fn from_recovery_func_param(param: &FuncParam) -> Self {
        Self {
            ty: Ty::from_recovery_type(&param.ty),
            mutable: param.mutable,
            cast_accept: param.cast_accept,
        }
    }

    fn try_to_func_param_no_infer(&self) -> Option<FuncParam> {
        Some(FuncParam {
            ty: self.ty.try_to_type_no_infer()?,
            mutable: self.mutable,
            cast_accept: self.cast_accept,
        })
    }
}

struct TyFuncParts {
    params: Vec<TyFuncParam>,
    ret: Ty,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum TyGenericArg {
    Type(Ty),
    Const(ConstTerm),
}

impl TyGenericArg {
    fn from_recovery_generic_arg(arg: &GenericArg) -> Self {
        match arg {
            GenericArg::Type(ty) => Self::Type(Ty::from_recovery_type(ty)),
            GenericArg::Const(arg) => Self::Const(ConstTerm::from_arg(arg)),
        }
    }

    fn try_to_generic_arg_no_infer(&self) -> Option<GenericArg> {
        match self {
            Self::Type(ty) => Some(GenericArg::Type(ty.try_to_type_no_infer()?)),
            Self::Const(arg) => Some(GenericArg::Const(arg.to_arg_no_infer()?)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct TyNominal {
    kind: NominalKind,
    name: Ident,
    type_args: Vec<Ty>,
    const_args: Vec<ConstTerm>,
    origin: Option<ModuleOrigin>,
}

impl TyNominal {
    fn same_head(&self, other: &Self) -> bool {
        self.kind == other.kind
            && self.name == other.name
            && self.origin == other.origin
            && self.type_args.len() == other.type_args.len()
            && self.const_args.len() == other.const_args.len()
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
    Void,
    Func {
        params: Vec<TyFuncParam>,
        ret: Box<Ty>,
    },
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
    fn from_recovery_types(types: &[Type]) -> Vec<Self> {
        types.iter().map(Self::from_recovery_type).collect()
    }

    fn from_recovery_nominal_args(
        type_args: &[Type],
        const_args: &[ConstArg],
    ) -> (Vec<Self>, Vec<ConstTerm>) {
        (
            Self::from_recovery_types(type_args),
            ConstTerm::from_args(const_args),
        )
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

    fn nominal(
        kind: NominalKind,
        name: Ident,
        type_args: Vec<Self>,
        const_args: Vec<ConstTerm>,
        origin: Option<ModuleOrigin>,
    ) -> Self {
        Self::Nominal(TyNominal {
            kind,
            name,
            type_args,
            const_args,
            origin,
        })
    }

    fn from_recovery_nominal(
        kind: NominalKind,
        name: Ident,
        type_args: &[Type],
        const_args: &[ConstArg],
        origin: Option<&ModuleOrigin>,
    ) -> Self {
        let (type_args, const_args) = Self::from_recovery_nominal_args(type_args, const_args);
        Self::nominal(kind, name, type_args, const_args, origin.cloned())
    }

    fn try_nominal_to_no_infer(nominal: &TyNominal) -> Option<Type> {
        let (type_args, const_args) =
            Self::try_nominal_args_to_no_infer(&nominal.type_args, &nominal.const_args)?;
        Some(Type::nominal_with_origin(
            nominal.kind,
            nominal.name,
            type_args,
            const_args,
            nominal.origin.clone(),
        ))
    }

    fn option_inner(&self) -> Option<&Ty> {
        match self {
            Self::Nominal(nominal)
                if nominal.kind == NominalKind::Enum
                    && nominal.name.0.as_ref() == Type::OPTION_ENUM_NAME =>
            {
                nominal.type_args.first()
            }
            _ => None,
        }
    }

    fn is_option(&self) -> bool {
        self.option_inner().is_some()
    }

    fn from_recovery_type(ty: &Type) -> Self {
        match ty {
            Type::Infer | Type::InferReturn => Self::Error,
            Type::Any => Self::Any,
            Type::Int => Self::Int,
            Type::Float => Self::Float,
            Type::Bool => Self::Bool,
            Type::String => Self::String,
            Type::Void => Self::Void,
            Type::Func { params, ret } => Self::Func {
                params: params
                    .iter()
                    .map(TyFuncParam::from_recovery_func_param)
                    .collect(),
                ret: Box::new(Self::from_recovery_type(ret)),
            },
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
                    .map(TyGenericArg::from_recovery_generic_arg)
                    .collect(),
            },
            Type::Tuple(elems) => Self::Tuple(elems.iter().map(Self::from_recovery_type).collect()),
            Type::Nominal(nominal) => {
                if nominal.kind == NominalKind::Extern {
                    debug_assert!(nominal.type_args.is_empty());
                    debug_assert!(nominal.const_args.is_empty());
                }
                Self::from_recovery_nominal(
                    nominal.kind,
                    nominal.name,
                    &nominal.type_args,
                    &nominal.const_args,
                    nominal.origin.as_ref(),
                )
            }
            Type::List { elem } => Self::List {
                elem: Box::new(Self::from_recovery_type(elem)),
            },
            Type::Array { elem, len } => Self::Array {
                elem: Box::new(Self::from_recovery_type(elem)),
                len: ConstTerm::from_array_len(*len),
            },
            Type::Map { key, value } => Self::Map {
                key: Box::new(Self::from_recovery_type(key)),
                value: Box::new(Self::from_recovery_type(value)),
            },
            Type::Slice { elem } => Self::Slice {
                elem: Box::new(Self::from_recovery_type(elem)),
            },
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
            Self::Void => Some(Type::Void),
            Self::Func { params, ret } => Some(Type::Func {
                params: params
                    .iter()
                    .map(TyFuncParam::try_to_func_param_no_infer)
                    .collect::<Option<Vec<_>>>()?,
                ret: Box::new(ret.try_to_type_no_infer()?),
            }),
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
pub(super) struct LocalTypeId(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
struct TempTypeId(u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum TypeRef {
    Concrete(Ty),
    Local(LocalTypeId),
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

    fn local(id: LocalTypeId) -> Self {
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

impl ConstraintKind {
    fn is_equal(&self) -> bool {
        matches!(self, Self::Equal(..))
    }

    fn is_assignable(&self) -> bool {
        matches!(self, Self::Assignable { .. })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum SolveError {
    TypeMismatch {
        expected: Ty,
        found: Ty,
        span: Option<SourceSpan>,
    },
    ConstMismatch {
        expected: ConstTerm,
        found: ConstTerm,
        span: Option<SourceSpan>,
    },
    TypeAlreadyBound {
        var: InferVarId,
        existing: Ty,
        found: Ty,
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
            expected,
            found,
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
                ret: Box::new(Ty::Void),
            },
            Ty::Func {
                params: vec![found],
                ret: Box::new(Ty::Void),
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

pub(super) type SourceExprTypes = HashMap<ExprId, (Option<SourceSpan>, Type)>;

#[derive(Debug, Default, Clone)]
pub(super) struct Solver {
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
    fn type_for_storage(&self, ty: &Ty) -> Type {
        self.resolve_ty(ty)
            .try_to_type_no_infer()
            .unwrap_or(Type::Infer)
    }

    pub(super) fn concrete_type(&self, ty: &Type) -> TypeHandle {
        TypeHandle(TypeRef::concrete(Ty::from_recovery_type(ty)))
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

    pub(super) fn local_handle(&self, id: LocalTypeId) -> TypeHandle {
        TypeHandle(TypeRef::local(id))
    }

    pub(super) fn expr_handle(&self, id: ExprId) -> TypeHandle {
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
        self.temp_handle(Ty::nominal(
            nominal.kind,
            nominal.name,
            type_args,
            const_args,
            nominal.origin.clone(),
        ))
    }

    fn temp_handle(&mut self, ty: Ty) -> TypeHandle {
        TypeHandle(TypeRef::temp(self.alloc_temp(ty)))
    }

    pub(super) fn array_handle(&mut self, elem: TypeHandle, len: ArrayLen) -> TypeHandle {
        let elem = self.resolve_ref(&elem.0);
        let len = ConstTerm::from_array_len(len);
        self.temp_handle(Ty::Array {
            elem: Box::new(elem),
            len,
        })
    }

    pub(super) fn list_handle(&mut self, elem: TypeHandle) -> TypeHandle {
        let elem = self.resolve_ref(&elem.0);
        self.temp_handle(Ty::List {
            elem: Box::new(elem),
        })
    }

    pub(super) fn map_handle(&mut self, key: TypeHandle, value: TypeHandle) -> TypeHandle {
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

    pub(super) fn error_expr_type(&mut self, id: ExprId, span: Option<SourceSpan>) -> TypeHandle {
        self.set_expr_handle(id, span, Ty::Error)
    }

    fn set_expr_handle(&mut self, id: ExprId, span: Option<SourceSpan>, ty: Ty) -> TypeHandle {
        self.set_expr_type(id, span, ty);
        self.expr_handle(id)
    }

    pub(super) fn set_expr_type_from_handle(
        &mut self,
        id: ExprId,
        span: Option<SourceSpan>,
        handle: TypeHandle,
    ) -> TypeHandle {
        let ty = self.resolve_ref(&handle.0);
        self.set_expr_type(id, span, ty);
        self.expr_handle(id)
    }

    pub(super) fn handle_to_type(&self, handle: &TypeHandle) -> Type {
        self.type_for_storage(&self.resolve_ref(&handle.0))
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
                Ty::from_recovery_type(from),
                Ty::from_recovery_type(to),
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

    pub(super) fn alloc_local_type(&mut self, ty: &Type) -> LocalTypeId {
        self.alloc_local(Ty::from_recovery_type(ty))
    }

    pub(super) fn alloc_local_type_from_handle(&mut self, handle: &TypeHandle) -> LocalTypeId {
        let ty = self.resolve_ref(&handle.0);
        self.alloc_local(ty)
    }

    pub(super) fn local_type_to_type(&self, id: LocalTypeId) -> Type {
        self.type_for_storage(self.local_type(id))
    }

    pub(super) fn set_local_type_from_type(&mut self, id: LocalTypeId, ty: &Type) {
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

    pub(super) fn expr_types_to_types(&self) -> HashMap<ExprId, (Span, Type)> {
        self.expr_types
            .iter()
            .map(|(id, (span, ty))| {
                let span = span.expect("expression type missing source span").byte();
                (*id, (span, self.type_for_storage(ty)))
            })
            .collect()
    }

    fn instantiate_type_template(&self, ty: &Type, vars: &GenericSolverVars) -> Ty {
        match ty {
            Type::Infer | Type::InferReturn => Ty::Error,
            Type::Any => Ty::Any,
            Type::Int => Ty::Int,
            Type::Float => Ty::Float,
            Type::Bool => Ty::Bool,
            Type::String => Ty::String,
            Type::Void => Ty::Void,
            Type::Func { params, ret } => Ty::Func {
                params: params
                    .iter()
                    .map(|param| TyFuncParam {
                        ty: self.instantiate_type_template(&param.ty, vars),
                        mutable: param.mutable,
                        cast_accept: param.cast_accept,
                    })
                    .collect(),
                ret: Box::new(self.instantiate_type_template(ret, vars)),
            },
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
                self.instantiate_nominal_template(
                    nominal.kind,
                    nominal.name,
                    &nominal.type_args,
                    &nominal.const_args,
                    nominal.origin.as_ref(),
                    vars,
                )
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
        }
    }

    fn instantiate_nominal_template(
        &self,
        kind: NominalKind,
        name: Ident,
        type_args: &[Type],
        const_args: &[ConstArg],
        origin: Option<&ModuleOrigin>,
        vars: &GenericSolverVars,
    ) -> Ty {
        Ty::nominal(
            kind,
            name,
            type_args
                .iter()
                .map(|ty| self.instantiate_type_template(ty, vars))
                .collect(),
            const_args
                .iter()
                .map(|arg| self.instantiate_const_arg_template(arg, vars))
                .collect(),
            origin.cloned(),
        )
    }

    fn instantiate_generic_arg_template(
        &self,
        arg: &GenericArg,
        vars: &GenericSolverVars,
    ) -> TyGenericArg {
        match arg {
            GenericArg::Type(ty) => TyGenericArg::Type(self.instantiate_type_template(ty, vars)),
            GenericArg::Const(arg) => {
                TyGenericArg::Const(self.instantiate_const_arg_template(arg, vars))
            }
        }
    }

    fn instantiate_const_arg_template(
        &self,
        arg: &ConstArg,
        vars: &GenericSolverVars,
    ) -> ConstTerm {
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
            ArrayLen::Fixed(_) | ArrayLen::Infer | ArrayLen::Named(_) => {
                ConstTerm::from_array_len(*len)
            }
        }
    }

    fn finalized_generic_const_arg(&self, arg: &ConstTerm) -> Option<ConstTerm> {
        let resolved = self.resolve_const(arg);
        match resolved {
            ConstTerm::Value(_) | ConstTerm::Name(_) | ConstTerm::Param(_) => Some(resolved),
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
            .filter(|constraint| constraint.kind.is_equal());
        let assignable = constraints
            .iter()
            .filter(|constraint| constraint.kind.is_assignable());
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
        let left = self.resolve_ref(&left);
        let right = self.resolve_ref(&right);
        self.unify_tys_equal(span, left, right)
    }

    fn constrain_assignable(
        &mut self,
        span: Option<SourceSpan>,
        from: TypeRef,
        to: TypeRef,
    ) -> Result<Ty, SolveError> {
        let from = self.resolve_ref(&from);
        let to = self.resolve_ref(&to);
        self.constrain_tys_assignable(span, from, to)
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

    fn alloc_local(&mut self, ty: Ty) -> LocalTypeId {
        let id = LocalTypeId(self.local_types.len() as u32);
        self.local_types.push(ty);
        id
    }

    fn local_type(&self, id: LocalTypeId) -> &Ty {
        &self.local_types[id.0 as usize]
    }

    fn set_local_type(&mut self, id: LocalTypeId, ty: Ty) {
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
        expected: Ty,
        found: Ty,
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
        expected: Ty,
        found: Ty,
        relation: TyRelation,
    ) -> Result<Box<Ty>, SolveError> {
        Ok(Box::new(self.relate_tys(span, expected, found, relation)?))
    }

    fn relate_boxed_assignable(
        &mut self,
        span: Option<SourceSpan>,
        expected_elem: Ty,
        found_elem: Ty,
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
            related.push(self.relate_tys(span, expected, found, relation)?);
        }
        Ok(related)
    }

    fn unify_func_invariant(
        &mut self,
        span: Option<SourceSpan>,
        expected_func: TyFuncParts,
        found_func: TyFuncParts,
        expected: Ty,
        found: Ty,
    ) -> Result<Ty, SolveError> {
        self.ensure_arity(
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
                if expected.mutable != found.mutable {
                    return Err(SolveError::func_param_mismatch(expected, found, span));
                }
                let mutable = expected.mutable;
                let cast_accept = expected.cast_accept || found.cast_accept;
                let ty = self.unify_tys_equal(span, expected.ty, found.ty)?;
                Ok(TyFuncParam {
                    ty,
                    mutable,
                    cast_accept,
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Ty::Func {
            params,
            ret: Box::new(self.unify_tys_equal(span, expected_func.ret, found_func.ret)?),
        })
    }

    fn unify_tys_equal(
        &mut self,
        span: Option<SourceSpan>,
        left: Ty,
        right: Ty,
    ) -> Result<Ty, SolveError> {
        let left = self.resolve_ty(&left);
        let right = self.resolve_ty(&right);
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
            (Ty::Infer(id), ty) => {
                self.bind_type(id, ty, span)?;
                Ok(self.resolve_ty(&Ty::Infer(id)))
            }
            (ty, Ty::Infer(id)) => {
                self.bind_type(id, ty, span)?;
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
                expected,
                found,
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
                self.ensure_arity(
                    generic_args.len(),
                    found_args.len(),
                    expected.clone(),
                    found.clone(),
                    span,
                )?;
                let generic_args =
                    self.unify_generic_args_equal(span, generic_args, found_args, expected, found)?;
                Ok(Ty::UnresolvedNominal {
                    qualifier,
                    name,
                    generic_args,
                })
            }
            (Ty::Tuple(elems), Ty::Tuple(found_elems)) => {
                self.ensure_arity(elems.len(), found_elems.len(), expected, found, span)?;
                Ok(Ty::Tuple(self.relate_ty_lists(
                    span,
                    elems,
                    found_elems,
                    TyRelation::Equal,
                )?))
            }
            (Ty::List { elem }, Ty::List { elem: found_elem }) => Ok(Ty::List {
                elem: self.relate_boxed(span, *elem, *found_elem, TyRelation::Equal)?,
            }),
            (
                Ty::Array { elem, len },
                Ty::Array {
                    elem: found_elem,
                    len: found_len,
                },
            ) => Ok(Ty::Array {
                elem: self.relate_boxed(span, *elem, *found_elem, TyRelation::Equal)?,
                len: self.unify_const_equal(span, len, found_len)?,
            }),
            (
                Ty::Map { key, value },
                Ty::Map {
                    key: found_key,
                    value: found_value,
                },
            ) => Ok(Ty::Map {
                key: self.relate_boxed(span, *key, *found_key, TyRelation::Equal)?,
                value: self.relate_boxed(span, *value, *found_value, TyRelation::Equal)?,
            }),
            (Ty::Slice { elem }, Ty::Slice { elem: found_elem }) => Ok(Ty::Slice {
                elem: self.relate_boxed(span, *elem, *found_elem, TyRelation::Equal)?,
            }),
            _ => Err(SolveError::type_mismatch(expected, found, span)),
        }
    }

    fn constrain_tys_assignable(
        &mut self,
        span: Option<SourceSpan>,
        from: Ty,
        to: Ty,
    ) -> Result<Ty, SolveError> {
        let from = self.resolve_ty(&from);
        let to = self.resolve_ty(&to);
        if from == to {
            return self.unify_tys_equal(span, from, to);
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
            return self.unify_tys_equal(span, from, to);
        }
        if let Some(inner) = to.option_inner().cloned()
            && !from.is_option()
        {
            self.constrain_tys_assignable(span, from, inner)?;
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
                expected,
                found,
            ),
            (Ty::Tuple(elems), Ty::Tuple(to_elems)) => {
                self.ensure_arity(to_elems.len(), elems.len(), expected, found, span)?;
                Ok(Ty::Tuple(self.relate_ty_lists(
                    span,
                    to_elems,
                    elems,
                    TyRelation::Assignable,
                )?))
            }
            (Ty::List { elem }, Ty::List { elem: to_elem }) => Ok(Ty::List {
                elem: self.relate_boxed_assignable(span, *to_elem, *elem, &expected, &found)?,
            }),
            (
                Ty::Array { elem, len },
                Ty::Array {
                    elem: to_elem,
                    len: to_len,
                },
            ) => Ok(Ty::Array {
                elem: self.relate_boxed_assignable(span, *to_elem, *elem, &expected, &found)?,
                len: self.unify_const_equal(span, len, to_len)?,
            }),
            (
                Ty::Map { key, value },
                Ty::Map {
                    key: to_key,
                    value: to_value,
                },
            ) => Ok(Ty::Map {
                key: self.relate_boxed_assignable(span, *to_key, *key, &expected, &found)?,
                value: self.relate_boxed_assignable(span, *to_value, *value, &expected, &found)?,
            }),
            (Ty::Slice { elem }, Ty::Slice { elem: to_elem }) => Ok(Ty::Slice {
                elem: self.relate_boxed_assignable(span, *to_elem, *elem, &expected, &found)?,
            }),
            (Ty::Array { elem, .. } | Ty::List { elem }, Ty::Slice { elem: to_elem }) => {
                Ok(Ty::Slice {
                    elem: self.relate_boxed_assignable(span, *to_elem, *elem, &expected, &found)?,
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
        let target_accepts_nil = matches!(to, Ty::Infer(_)) || to.is_option();
        if target_accepts_nil {
            return Some(self.unify_tys_equal(span, from.clone(), to.clone()));
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
        let type_args =
            self.relate_ty_lists(span, left.type_args, right.type_args, TyRelation::Equal)?;
        let const_args = self.relate_const_arg_lists(span, left.const_args, right.const_args)?;
        Ok(Ty::nominal(
            left.kind,
            left.name,
            type_args,
            const_args,
            left.origin,
        ))
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
        let outer_mismatch =
            to.kind == NominalKind::Enum && to.name.0.as_ref() == Type::OPTION_ENUM_NAME;
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
        Ok(Ty::nominal(
            to.kind, to.name, type_args, const_args, to.origin,
        ))
    }

    fn unify_generic_args_equal(
        &mut self,
        span: Option<SourceSpan>,
        left: Vec<TyGenericArg>,
        right: Vec<TyGenericArg>,
        expected: Ty,
        found: Ty,
    ) -> Result<Vec<TyGenericArg>, SolveError> {
        left.into_iter()
            .zip(right)
            .map(|(left, right)| match (left, right) {
                (TyGenericArg::Type(left), TyGenericArg::Type(right)) => {
                    Ok(TyGenericArg::Type(self.unify_tys_equal(span, left, right)?))
                }
                (TyGenericArg::Const(left), TyGenericArg::Const(right)) => Ok(TyGenericArg::Const(
                    self.unify_const_equal(span, left, right)?,
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
            .map(|(expected, found)| self.unify_const_equal(span, expected, found))
            .collect()
    }

    fn unify_const_equal(
        &mut self,
        span: Option<SourceSpan>,
        left: ConstTerm,
        right: ConstTerm,
    ) -> Result<ConstTerm, SolveError> {
        let left = self.resolve_const(&left);
        let right = self.resolve_const(&right);
        if left == right {
            return Ok(left);
        }
        match (left, right) {
            (ConstTerm::ArrayInfer, term) | (term, ConstTerm::ArrayInfer) => Ok(term),
            (ConstTerm::Infer(id), term) => {
                self.bind_const(id, term, span)?;
                Ok(self.resolve_const(&ConstTerm::Infer(id)))
            }
            (term, ConstTerm::Infer(id)) => {
                self.bind_const(id, term, span)?;
                Ok(self.resolve_const(&ConstTerm::Infer(id)))
            }
            (expected, found) => Err(SolveError::const_mismatch(expected, found, span)),
        }
    }

    fn ensure_arity(
        &self,
        expected_len: usize,
        found_len: usize,
        expected: Ty,
        found: Ty,
        span: Option<SourceSpan>,
    ) -> Result<(), SolveError> {
        if expected_len == found_len {
            Ok(())
        } else {
            Err(SolveError::type_mismatch(expected, found, span))
        }
    }

    fn bind_type(
        &mut self,
        var: InferVarId,
        ty: Ty,
        span: Option<SourceSpan>,
    ) -> Result<(), SolveError> {
        let found = self.resolve_ty(&ty);
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
                existing,
                found,
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
                ty if ty.is_option() => Ok(()),
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
        found: ConstTerm,
        span: Option<SourceSpan>,
    ) -> Result<(), SolveError> {
        let found = self.resolve_const(&found);
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
        Ty::nominal(
            nominal.kind,
            nominal.name,
            type_args,
            const_args,
            nominal.origin.clone(),
        )
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
                    })
                    .collect(),
                ret: Box::new(self.resolve_ty(ret)),
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
            | Ty::Any
            | Ty::Int
            | Ty::Float
            | Ty::Bool
            | Ty::String
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
                    || self.type_occurs_in_ty(var, &ret)
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
            Ty::List { elem } | Ty::Slice { elem } => self.type_occurs_in_ty(var, &elem),
            Ty::Array { elem, .. } => self.type_occurs_in_ty(var, &elem),
            Ty::Map { key, value } => {
                self.type_occurs_in_ty(var, &key) || self.type_occurs_in_ty(var, &value)
            }
            Ty::Error
            | Ty::Any
            | Ty::Int
            | Ty::Float
            | Ty::Bool
            | Ty::String
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
            Ty::Void => Type::Void,
            Ty::Func { params, ret } => Type::Func {
                params: params
                    .into_iter()
                    .map(|param| FuncParam {
                        ty: self.finalize_ty_inner(&param.ty, cx),
                        mutable: param.mutable,
                        cast_accept: param.cast_accept,
                    })
                    .collect(),
                ret: Box::new(self.finalize_ty_inner(&ret, cx)),
            },
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
                match self.finalize_array_len(len, cx) {
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
            Some(const_args) => Type::nominal_with_origin(
                nominal.kind,
                nominal.name,
                type_args,
                const_args,
                nominal.origin,
            ),
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
            .map(|arg| self.finalize_const_arg(arg, cx))
            .collect()
    }

    fn finalize_generic_arg(
        &self,
        arg: TyGenericArg,
        cx: &mut FinalizeCx<'_>,
    ) -> Option<GenericArg> {
        match arg {
            TyGenericArg::Type(ty) => Some(GenericArg::Type(self.finalize_ty_inner(&ty, cx))),
            TyGenericArg::Const(arg) => Some(GenericArg::Const(self.finalize_const_arg(arg, cx)?)),
        }
    }

    fn finalize_const_arg(&self, arg: ConstTerm, cx: &mut FinalizeCx<'_>) -> Option<ConstArg> {
        match self.resolve_const(&arg) {
            ConstTerm::Infer(id) => {
                self.push_unresolved_const(id, cx);
                None
            }
            ConstTerm::ArrayInfer => None,
            term => term.to_arg_no_infer(),
        }
    }

    fn finalize_array_len(&self, len: ConstTerm, cx: &mut FinalizeCx<'_>) -> Option<ArrayLen> {
        match self.resolve_const(&len) {
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
    use crate::{
        ast::ConstValue,
        source::{SourceId, SourceKind, SourceTable},
    };

    fn ident(name: &str) -> Ident {
        Ident::new(name)
    }

    fn origin(parts: &[&str]) -> ModuleOrigin {
        ModuleOrigin::Module(std::rc::Rc::from(
            parts
                .iter()
                .map(|part| (*part).to_string())
                .collect::<Vec<_>>(),
        ))
    }

    fn source() -> SourceId {
        static SOURCE: OnceLock<SourceId> = OnceLock::new();
        *SOURCE.get_or_init(|| {
            let mut sources = SourceTable::default();
            sources.add(SourceKind::Virtual, "test", None, "")
        })
    }

    fn span(start: usize, end: usize) -> Option<SourceSpan> {
        Some(SourceSpan::new(source(), start, end))
    }

    fn type_var(id: u32) -> TypeVarId {
        TypeVarId(id)
    }

    fn const_param(id: u32) -> ConstParamId {
        ConstParamId(id)
    }

    fn option(inner: Type) -> Type {
        Type::option_of(inner)
    }

    fn ty_option(inner: Ty) -> Ty {
        Ty::from_recovery_type(&option(
            inner.try_to_type_no_infer().expect("concrete option arg"),
        ))
    }

    fn ty_ref(ty: Ty) -> TypeRef {
        TypeRef::concrete(ty)
    }

    fn infer_id(ty: Ty) -> InferVarId {
        match ty {
            Ty::Infer(id) => id,
            _ => panic!("expected inference var"),
        }
    }

    trait SolverTestExt {
        fn fresh_expr_type(&mut self, id: ExprId, span: Option<SourceSpan>) -> TypeHandle;
        fn finalize_handle(&self, handle: &TypeHandle) -> (Type, Vec<SolverFinalizeError>);
    }

    impl SolverTestExt for Solver {
        fn fresh_expr_type(&mut self, id: ExprId, span: Option<SourceSpan>) -> TypeHandle {
            let ty = self.fresh_type(span);
            self.set_expr_handle(id, span, ty)
        }

        fn finalize_handle(&self, handle: &TypeHandle) -> (Type, Vec<SolverFinalizeError>) {
            let (ty, errors) = self.finalize_ty(&self.resolve_ref(&handle.0));
            (ty, errors.into_iter().map(Into::into).collect())
        }
    }

    fn fixed_array(elem: Ty, len: usize) -> Ty {
        Ty::Array {
            elem: Box::new(elem),
            len: ConstTerm::from_usize(len),
        }
    }

    fn list(elem: Ty) -> Ty {
        Ty::List {
            elem: Box::new(elem),
        }
    }

    fn slice(elem: Ty) -> Ty {
        Ty::Slice {
            elem: Box::new(elem),
        }
    }

    fn map(key: Ty, value: Ty) -> Ty {
        Ty::Map {
            key: Box::new(key),
            value: Box::new(value),
        }
    }

    fn nominal(name: &str, args: Vec<Ty>) -> Ty {
        ty_nominal(NominalKind::Struct, name, args)
    }

    fn ty_nominal(kind: NominalKind, name: &str, args: Vec<Ty>) -> Ty {
        Ty::nominal(kind, ident(name), args, vec![], Some(origin(&["pkg"])))
    }

    fn type_param(name: &str, id: u32) -> crate::ast::TypeParam {
        crate::ast::TypeParam {
            name: ident(name),
            id: type_var(id),
        }
    }

    fn const_generic_param(name: &str, id: u32) -> crate::ast::ConstParam {
        crate::ast::ConstParam {
            name: ident(name),
            id: const_param(id),
        }
    }

    fn assert_roundtrip(ty: Type) {
        let infer = Ty::from_recovery_type(&ty);
        assert_eq!(infer.try_to_type_no_infer(), Some(ty));
    }

    fn solver_with_vars(generics: &GenericParams) -> (Solver, GenericSolverVars) {
        let mut solver = Solver::default();
        let vars = solver.generic_solver_vars(generics, &GenericSolverSeeds::default(), span(1, 2));
        (solver, vars)
    }

    #[test]
    fn generic_type_from_arg() {
        let generics = GenericParams {
            type_params: vec![type_param("T", 0)],
            const_params: vec![],
        };
        let (mut solver, vars) = solver_with_vars(&generics);
        let template = solver.instantiate_generic_type(&Type::Var(type_var(0)), &vars);
        let found = solver.concrete_type(&Type::Int);
        solver.add_handle_equal(span(1, 2), template, found);
        assert!(solver.solve_pending().is_empty());
        assert_eq!(
            solver.finalize_generic_args(&generics, &vars),
            Ok(GenericArgs {
                type_args: vec![Type::Int],
                const_args: vec![],
            }),
        );
    }

    #[test]
    fn generic_type_from_return() {
        let generics = GenericParams {
            type_params: vec![type_param("T", 0)],
            const_params: vec![],
        };
        let (mut solver, vars) = solver_with_vars(&generics);
        let template = solver.instantiate_generic_type(&option(Type::Var(type_var(0))), &vars);
        let expected = solver.concrete_type(&option(Type::Int));
        solver.add_handle_assignable(span(1, 2), template, expected);
        assert!(solver.solve_pending().is_empty());
        assert_eq!(
            solver.finalize_generic_args(&generics, &vars),
            Ok(GenericArgs {
                type_args: vec![Type::Int],
                const_args: vec![],
            }),
        );
    }

    #[test]
    fn generic_const_from_array_return() {
        let generics = GenericParams {
            type_params: vec![],
            const_params: vec![const_generic_param("N", 0)],
        };
        let (mut solver, vars) = solver_with_vars(&generics);
        let template = solver.instantiate_generic_type(
            &Type::Array {
                elem: Box::new(Type::Int),
                len: ArrayLen::Param(const_param(0)),
            },
            &vars,
        );
        let expected = solver.concrete_type(&Type::Array {
            elem: Box::new(Type::Int),
            len: ArrayLen::Fixed(3),
        });
        solver.add_handle_equal(span(1, 2), template, expected);
        assert!(solver.solve_pending().is_empty());
        assert_eq!(
            solver.finalize_generic_args(&generics, &vars),
            Ok(GenericArgs {
                type_args: vec![],
                const_args: vec![ConstTerm::from_usize(3)],
            }),
        );
    }

    #[test]
    fn generic_unresolved_name() {
        let generics = GenericParams {
            type_params: vec![type_param("T", 0)],
            const_params: vec![],
        };
        let (solver, vars) = solver_with_vars(&generics);
        assert_eq!(
            solver.finalize_generic_args(&generics, &vars),
            Err(vec![ident("T")]),
        );
    }

    #[test]
    fn roundtrip_primitives_and_var() {
        for ty in [
            Type::Any,
            Type::Int,
            Type::Float,
            Type::Bool,
            Type::String,
            Type::Void,
            Type::Var(type_var(7)),
        ] {
            assert_roundtrip(ty);
        }
    }

    #[test]
    fn roundtrip_func_mutability() {
        assert_roundtrip(Type::Func {
            params: vec![
                FuncParam::new(Type::Int, true, false),
                FuncParam::immut(Type::Bool),
            ],
            ret: Box::new(Type::String),
        });
    }

    #[test]
    fn roundtrip_array_lens() {
        assert_roundtrip(Type::Array {
            elem: Box::new(Type::Int),
            len: ArrayLen::Fixed(3),
        });
        assert_roundtrip(Type::Array {
            elem: Box::new(Type::Float),
            len: ArrayLen::Named(ident("N")),
        });
        assert_roundtrip(Type::Array {
            elem: Box::new(Type::Bool),
            len: ArrayLen::Param(const_param(2)),
        });
        assert_roundtrip(Type::Array {
            elem: Box::new(Type::String),
            len: ArrayLen::Infer,
        });
    }

    #[test]
    fn roundtrip_tuples() {
        assert_roundtrip(Type::Tuple(vec![Type::Int, option(Type::String)]));
    }

    #[test]
    fn roundtrip_nominals() {
        let type_args = vec![
            Type::String,
            Type::Array {
                elem: Box::new(Type::Int),
                len: ArrayLen::Param(const_param(4)),
            },
        ];
        let const_args = vec![
            ConstArg::Value(ConstValue::Int(3)),
            ConstArg::Name(ident("CAP")),
            ConstArg::Param(const_param(9)),
        ];
        assert_roundtrip(Type::nominal_with_origin(
            NominalKind::Struct,
            ident("FixedBuf"),
            type_args.clone(),
            const_args.clone(),
            Some(origin(&["gamekit", "mem"])),
        ));
        assert_roundtrip(Type::nominal_with_origin(
            NominalKind::DataRef,
            ident("Handle"),
            type_args.clone(),
            const_args.clone(),
            Some(origin(&["gamekit", "mem"])),
        ));
        assert_roundtrip(Type::nominal_with_origin(
            NominalKind::Enum,
            ident("Option"),
            type_args,
            const_args,
            Some(origin(&["gamekit", "core"])),
        ));
    }

    #[test]
    fn roundtrip_collections() {
        assert_roundtrip(Type::List {
            elem: Box::new(Type::Int),
        });
        assert_roundtrip(Type::Slice {
            elem: Box::new(Type::String),
        });
        assert_roundtrip(Type::Map {
            key: Box::new(Type::String),
            value: Box::new(option(Type::Int)),
        });
    }

    #[test]
    fn roundtrip_externs() {
        assert_roundtrip(Type::nominal_with_origin(
            NominalKind::Extern,
            ident("Texture"),
            vec![],
            vec![],
            Some(origin(&["gamekit", "gfx"])),
        ));
    }

    #[test]
    fn nominals_enter_solver_as_nominal() {
        let origin = Some(origin(&["gamekit", "mem"]));
        let ty = Type::nominal_with_origin(
            NominalKind::Struct,
            ident("FixedBuf"),
            vec![Type::Int],
            vec![ConstArg::Value(ConstValue::Int(4))],
            origin.clone(),
        );
        let solver_ty = Ty::from_recovery_type(&ty);
        assert!(matches!(
            &solver_ty,
            Ty::Nominal(nominal)
                if nominal.kind == NominalKind::Struct
                    && nominal.name == ident("FixedBuf")
                    && nominal.origin == origin
        ));
        assert_eq!(
            solver_ty.try_to_type_no_infer(),
            Some(Type::nominal_with_origin(
                NominalKind::Struct,
                ident("FixedBuf"),
                vec![Type::Int],
                vec![ConstArg::Value(ConstValue::Int(4))],
                origin,
            ))
        );
    }

    #[test]
    fn externs_enter_solver_as_nominal() {
        let origin = Some(origin(&["gamekit", "gfx"]));
        let ty = Type::nominal_with_origin(
            NominalKind::Extern,
            ident("Texture"),
            vec![],
            vec![],
            origin.clone(),
        );
        let solver_ty = Ty::from_recovery_type(&ty);
        assert!(matches!(
            &solver_ty,
            Ty::Nominal(nominal)
                if nominal.kind == NominalKind::Extern
                    && nominal.name == ident("Texture")
                    && nominal.origin == origin
        ));
        assert_eq!(
            solver_ty.try_to_type_no_infer(),
            Some(Type::nominal_with_origin(
                NominalKind::Extern,
                ident("Texture"),
                vec![],
                vec![],
                origin,
            ))
        );
    }

    #[test]
    fn roundtrip_unresolved_forms() {
        assert_roundtrip(Type::UnresolvedName(ident("T")));
        assert_roundtrip(Type::UnresolvedNominal {
            qualifier: Some(ident("pkg")),
            name: ident("Thing"),
            generic_args: vec![
                GenericArg::Type(Type::Var(type_var(3))),
                GenericArg::Const(ConstArg::Value(ConstValue::Int(8))),
            ],
        });
    }

    #[test]
    fn fresh_vars_are_unique() {
        let mut solver = Solver::default();
        let ty0 = solver.fresh_type(span(1, 2));
        let ty1 = solver.fresh_type(span(3, 4));
        let c0 = solver.fresh_const(span(5, 6));
        let c1 = solver.fresh_const(span(7, 8));
        let l0 = solver.fresh_const(span(9, 10));
        let l1 = solver.fresh_const(span(11, 12));
        assert_ne!(ty0, ty1);
        assert_ne!(c0, c1);
        assert_ne!(l0, l1);
    }

    #[test]
    fn cell_refs() {
        let mut solver = Solver::default();
        let local = solver.alloc_local(Ty::Int);
        let temp = solver.alloc_temp(Ty::Void);
        let expr = ExprId(42);
        solver.set_expr_type(expr, span(20, 21), Ty::String);
        solver.set_local_type(local, Ty::Float);
        assert_eq!(
            solver.expr_types.get(&expr).map(|(span, _)| *span),
            Some(span(20, 21))
        );
        assert_eq!(solver.resolve_ref(&TypeRef::local(local)), Ty::Float);
        assert_eq!(solver.resolve_ref(&TypeRef::temp(temp)), Ty::Void);
        assert_eq!(solver.resolve_ref(&TypeRef::expr(expr)), Ty::String);
        assert_eq!(solver.resolve_ref(&TypeRef::concrete(Ty::Int)), Ty::Int);
    }

    #[test]
    fn nested_binding() {
        let mut solver = Solver::default();
        let Ty::Infer(var) = solver.fresh_type(span(10, 20)) else {
            panic!("expected fresh type var");
        };
        solver
            .bind_type(var, Ty::Int, span(30, 40))
            .expect("bind should succeed");
        let nested = Ty::nominal(
            NominalKind::Enum,
            ident(Type::OPTION_ENUM_NAME),
            vec![Ty::Infer(var)],
            vec![],
            None,
        );
        assert_eq!(
            solver.resolve_ty(&nested),
            Ty::from_recovery_type(&option(Type::Int))
        );
    }

    #[test]
    fn type_occurs() {
        let mut solver = Solver::default();
        let Ty::Infer(var) = solver.fresh_type(span(1, 3)) else {
            panic!("expected fresh type var");
        };
        let recursive = Ty::Array {
            elem: Box::new(Ty::Infer(var)),
            len: ConstTerm::from_usize(1),
        };
        assert_eq!(
            solver.bind_type(var, recursive, span(4, 8)),
            Err(SolveError::TypeOccurs {
                var,
                span: span(4, 8),
            })
        );
    }

    #[test]
    fn type_rebind() {
        let mut solver = Solver::default();
        let Ty::Infer(var) = solver.fresh_type(span(1, 3)) else {
            panic!("expected fresh type var");
        };
        solver
            .bind_type(var, Ty::Int, span(4, 8))
            .expect("initial bind should succeed");
        assert_eq!(
            solver.bind_type(var, Ty::String, span(9, 12)),
            Err(SolveError::TypeAlreadyBound {
                var,
                existing: Ty::Int,
                found: Ty::String,
                span: span(9, 12),
            })
        );
    }

    #[test]
    fn const_len_bindings() {
        let mut solver = Solver::default();
        let ConstTerm::Infer(arg_var) = solver.fresh_const(span(1, 2)) else {
            panic!("expected fresh const var");
        };
        let ConstTerm::Infer(len_var) = solver.fresh_const(span(3, 4)) else {
            panic!("expected fresh array len var");
        };
        solver
            .bind_const(arg_var, ConstTerm::Value(ConstValue::Int(8)), span(5, 6))
            .expect("const arg bind should succeed");
        solver
            .bind_const(len_var, ConstTerm::from_usize(4), span(7, 8))
            .expect("array len bind should succeed");
        assert_eq!(
            solver.resolve_const(&ConstTerm::Infer(arg_var)),
            ConstTerm::Value(ConstValue::Int(8))
        );
        assert_eq!(
            solver.resolve_const(&ConstTerm::Infer(arg_var)),
            ConstTerm::from_usize(8)
        );
        assert_eq!(
            solver.resolve_const(&ConstTerm::Infer(len_var)),
            ConstTerm::Value(ConstValue::Int(4))
        );
    }

    #[test]
    fn const_rebind() {
        let mut solver = Solver::default();
        let ConstTerm::Infer(var) = solver.fresh_const(span(1, 2)) else {
            panic!("expected fresh const var");
        };
        solver
            .bind_const(var, ConstTerm::Value(ConstValue::Int(1)), span(3, 4))
            .expect("initial bind should succeed");
        assert_eq!(
            solver.bind_const(var, ConstTerm::from_usize(2), span(5, 6)),
            Err(SolveError::ConstAlreadyBound {
                var,
                existing: ConstTerm::Value(ConstValue::Int(1)),
                found: ConstTerm::from_usize(2),
                span: span(5, 6),
            })
        );
    }

    #[test]
    fn unresolved_finalize() {
        let mut solver = Solver::default();
        let ty = solver.fresh_type(span(2, 6));
        let (finalized, errors) = solver.finalize_ty(&ty);
        assert_eq!(finalized, Type::Infer);
        assert_eq!(
            errors,
            vec![InferError::UnresolvedType { span: span(2, 6) }]
        );
    }

    #[test]
    fn error_finalize() {
        let solver = Solver::default();
        let (finalized, errors) = solver.finalize_ty(&Ty::Error);
        assert_eq!(finalized, Type::Infer);
        assert!(errors.is_empty());
    }

    #[test]
    fn equal_bind_concrete() {
        let mut solver = Solver::default();
        let var = infer_id(solver.fresh_type(span(1, 2)));
        let result = solver
            .unify_equal(span(3, 4), ty_ref(Ty::Infer(var)), ty_ref(Ty::Int))
            .expect("unification should bind");
        assert_eq!(result, Ty::Int);
        assert_eq!(solver.resolve_ty(&Ty::Infer(var)), Ty::Int);
    }

    #[test]
    fn equal_bind_chain() {
        let mut solver = Solver::default();
        let a = infer_id(solver.fresh_type(span(1, 2)));
        let b = infer_id(solver.fresh_type(span(3, 4)));
        solver
            .unify_equal(span(5, 6), ty_ref(Ty::Infer(a)), ty_ref(Ty::Infer(b)))
            .expect("vars should unify");
        solver
            .unify_equal(span(7, 8), ty_ref(Ty::Infer(b)), ty_ref(Ty::String))
            .expect("chain should bind");
        assert_eq!(solver.resolve_ty(&Ty::Infer(a)), Ty::String);
        assert_eq!(solver.resolve_ty(&Ty::Infer(b)), Ty::String);
    }

    #[test]
    fn equal_arrays_and_lengths() {
        let mut solver = Solver::default();
        let var = infer_id(solver.fresh_type(span(1, 2)));
        let result = solver
            .unify_equal(
                span(3, 4),
                ty_ref(fixed_array(Ty::Infer(var), 3)),
                ty_ref(fixed_array(Ty::Int, 3)),
            )
            .expect("arrays should unify");
        assert_eq!(result, fixed_array(Ty::Int, 3));
        assert_eq!(
            solver.unify_equal(
                span(5, 6),
                ty_ref(fixed_array(Ty::Int, 3)),
                ty_ref(fixed_array(Ty::Int, 4)),
            ),
            Err(SolveError::ConstMismatch {
                expected: ConstTerm::from_usize(3),
                found: ConstTerm::from_usize(4),
                span: span(5, 6),
            })
        );
    }

    #[test]
    fn array_infer_unifies_with_fixed_length() {
        let mut solver = Solver::default();
        let wildcard = Ty::Array {
            elem: Box::new(Ty::Int),
            len: ConstTerm::ArrayInfer,
        };
        let fixed = fixed_array(Ty::Int, 5);
        let result = solver
            .unify_equal(span(1, 2), ty_ref(wildcard), ty_ref(fixed.clone()))
            .expect("array wildcard length should unify");
        assert_eq!(result, fixed);
    }

    #[test]
    fn unresolved_const_infer_finalizes_error() {
        let mut solver = Solver::default();
        let term = solver.fresh_const(span(3, 4));
        let ty = Ty::nominal(
            NominalKind::Struct,
            ident("Buf"),
            vec![],
            vec![term],
            Some(origin(&["pkg"])),
        );
        let (finalized, errors) = solver.finalize_ty(&ty);
        assert_eq!(finalized, Type::Infer);
        assert_eq!(
            errors,
            vec![InferError::UnresolvedConst { span: span(3, 4) }]
        );
    }

    #[test]
    fn relation_error_preserves_const_values() {
        let mut solver = Solver::default();
        let left = Type::Array {
            elem: Box::new(Type::Int),
            len: ArrayLen::Fixed(3),
        };
        let right = Type::Array {
            elem: Box::new(Type::Int),
            len: ArrayLen::Fixed(4),
        };
        solver.add_handle_equal(
            span(1, 2),
            solver.concrete_type(&left),
            solver.concrete_type(&right),
        );
        assert!(matches!(
            solver.solve_pending().as_slice(),
            [SolverRelationError::ConstMismatch {
                expected: ConstDiagnostic::Value(ConstValue::Int(3)),
                found: ConstDiagnostic::Value(ConstValue::Int(4)),
                span: err_span,
            }] if *err_span == span(1, 2)
        ));
    }

    #[test]
    fn occurs_error() {
        let solver = Solver::default();
        assert_eq!(
            solver.relation_error(SolveError::TypeOccurs {
                var: InferVarId(0),
                span: span(1, 2),
            }),
            SolverRelationError::RecursiveInference { span: span(1, 2) }
        );
        assert_eq!(
            solver.relation_error(SolveError::ConstOccurs {
                var: ConstInferVarId(0),
                span: span(3, 4),
            }),
            SolverRelationError::RecursiveInference { span: span(3, 4) }
        );
    }

    #[test]
    fn equal_tuples() {
        let mut solver = Solver::default();
        let a = infer_id(solver.fresh_type(span(1, 2)));
        let b = infer_id(solver.fresh_type(span(3, 4)));
        solver
            .unify_equal(
                span(5, 6),
                ty_ref(Ty::Tuple(vec![Ty::Infer(a), Ty::String])),
                ty_ref(Ty::Tuple(vec![Ty::Int, Ty::Infer(b)])),
            )
            .expect("tuple should unify");
        assert_eq!(solver.resolve_ty(&Ty::Infer(a)), Ty::Int);
        assert_eq!(solver.resolve_ty(&Ty::Infer(b)), Ty::String);
    }

    #[test]
    fn equal_nominal_identity_and_args() {
        let mut solver = Solver::default();
        let a = infer_id(solver.fresh_type(span(1, 2)));
        solver
            .unify_equal(
                span(3, 4),
                ty_ref(nominal("Box", vec![Ty::Infer(a)])),
                ty_ref(nominal("Box", vec![Ty::Bool])),
            )
            .expect("same nominal should unify args");
        assert_eq!(solver.resolve_ty(&Ty::Infer(a)), Ty::Bool);
        assert!(matches!(
            solver.unify_equal(
                span(5, 6),
                ty_ref(nominal("Box", vec![Ty::Int])),
                ty_ref(nominal("Bag", vec![Ty::Int])),
            ),
            Err(SolveError::TypeMismatch { .. })
        ));
        assert!(matches!(
            solver.unify_equal(
                span(7, 8),
                ty_ref(Ty::nominal(
                    NominalKind::Struct,
                    ident("Box"),
                    vec![Ty::Int],
                    vec![],
                    Some(origin(&["a"])),
                )),
                ty_ref(Ty::nominal(
                    NominalKind::Struct,
                    ident("Box"),
                    vec![Ty::Int],
                    vec![],
                    Some(origin(&["b"])),
                )),
            ),
            Err(SolveError::TypeMismatch { .. })
        ));
    }

    #[test]
    fn equal_nominal_dedup() {
        let mut solver = Solver::default();
        solver.add_equal(
            span(1, 2),
            ty_ref(nominal("Box", vec![Ty::Int])),
            ty_ref(nominal("Box", vec![Ty::String])),
        );
        solver.add_equal(
            span(1, 2),
            ty_ref(nominal("Box", vec![Ty::Int])),
            ty_ref(nominal("Box", vec![Ty::String])),
        );
        let errors = solver.solve_all();
        assert_eq!(errors.len(), 1);
        assert!(matches!(errors[0], SolveError::TypeMismatch { .. }));
    }

    #[test]
    fn equal_functions() {
        let mut solver = Solver::default();
        let left = Ty::Func {
            params: vec![TyFuncParam {
                ty: Ty::Int,
                mutable: false,
                cast_accept: false,
            }],
            ret: Box::new(Ty::Bool),
        };
        let wrong_count = Ty::Func {
            params: vec![],
            ret: Box::new(Ty::Bool),
        };
        let wrong_mutability = Ty::Func {
            params: vec![TyFuncParam {
                ty: Ty::Int,
                mutable: true,
                cast_accept: false,
            }],
            ret: Box::new(Ty::Bool),
        };
        assert!(matches!(
            solver.unify_equal(span(1, 2), ty_ref(left.clone()), ty_ref(wrong_count)),
            Err(SolveError::TypeMismatch { .. })
        ));
        assert!(matches!(
            solver.unify_equal(span(3, 4), ty_ref(left), ty_ref(wrong_mutability)),
            Err(SolveError::TypeMismatch { .. })
        ));
    }

    #[test]
    fn equal_occurs_check() {
        let mut solver = Solver::default();
        let var = infer_id(solver.fresh_type(span(1, 2)));
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

    #[test]
    fn equal_any_exact() {
        let mut solver = Solver::default();
        assert_eq!(
            solver.unify_equal(span(1, 2), ty_ref(Ty::Any), ty_ref(Ty::Any)),
            Ok(Ty::Any)
        );
        assert!(matches!(
            solver.unify_equal(span(3, 4), ty_ref(Ty::Any), ty_ref(Ty::Int)),
            Err(SolveError::TypeMismatch { .. })
        ));
    }

    #[test]
    fn equal_vars_externs() {
        let mut solver = Solver::default();
        assert_eq!(
            solver.unify_equal(
                span(1, 2),
                ty_ref(Ty::Var(type_var(1))),
                ty_ref(Ty::Var(type_var(1))),
            ),
            Ok(Ty::Var(type_var(1)))
        );
        assert!(matches!(
            solver.unify_equal(
                span(3, 4),
                ty_ref(Ty::Var(type_var(1))),
                ty_ref(Ty::Var(type_var(2))),
            ),
            Err(SolveError::TypeMismatch { .. })
        ));

        let tex = Ty::nominal(
            NominalKind::Extern,
            ident("Texture"),
            vec![],
            vec![],
            Some(origin(&["gfx"])),
        );
        assert_eq!(
            solver.unify_equal(span(5, 6), ty_ref(tex.clone()), ty_ref(tex.clone())),
            Ok(tex)
        );
        assert!(matches!(
            solver.unify_equal(
                span(7, 8),
                ty_ref(Ty::nominal(
                    NominalKind::Extern,
                    ident("Texture"),
                    vec![],
                    vec![],
                    Some(origin(&["gfx"])),
                )),
                ty_ref(Ty::nominal(
                    NominalKind::Extern,
                    ident("Texture"),
                    vec![],
                    vec![],
                    Some(origin(&["ui"])),
                )),
            ),
            Err(SolveError::TypeMismatch { .. })
        ));
    }

    #[test]
    fn equal_nominal_const_args() {
        let mut solver = Solver::default();
        let ConstTerm::Infer(n) = solver.fresh_const(span(1, 2)) else {
            panic!("expected const infer var");
        };
        let left = Ty::nominal(
            NominalKind::Struct,
            ident("Buf"),
            vec![Ty::Int],
            vec![ConstTerm::Infer(n)],
            None,
        );
        let right = Ty::nominal(
            NominalKind::Struct,
            ident("Buf"),
            vec![Ty::Int],
            vec![ConstTerm::Value(ConstValue::Int(8))],
            None,
        );
        solver
            .unify_equal(span(3, 4), ty_ref(left), ty_ref(right))
            .expect("nominal const arg should bind");
        assert_eq!(
            solver.resolve_const(&ConstTerm::Infer(n)),
            ConstTerm::Value(ConstValue::Int(8))
        );
    }

    #[test]
    fn assign_option_promotion() {
        let mut solver = Solver::default();
        let option_int = ty_option(Ty::Int);
        assert_eq!(
            solver.constrain_assignable(span(1, 2), ty_ref(Ty::Int), ty_ref(option_int.clone())),
            Ok(option_int.clone())
        );
        assert!(matches!(
            solver.constrain_assignable(span(3, 4), ty_ref(option_int), ty_ref(Ty::Int)),
            Err(SolveError::TypeMismatch { .. })
        ));
    }

    #[test]
    fn equal_does_not_promote_option() {
        let mut solver = Solver::default();
        assert!(matches!(
            solver.unify_equal(span(1, 2), ty_ref(Ty::Int), ty_ref(ty_option(Ty::Int))),
            Err(SolveError::TypeMismatch { .. })
        ));
    }

    #[test]
    fn nil_assigns_to_option() {
        let mut solver = Solver::default();
        let nil = solver.fresh_nil_handle(span(1, 2));
        let option_handle = solver.concrete_type(&option(Type::Int));
        solver.add_handle_assignable(span(3, 4), nil.clone(), option_handle);
        assert!(solver.solve_all().is_empty());
        let (ty, errors) = solver.finalize_handle(&nil);
        assert_eq!(ty, option(Type::Int));
        assert!(errors.is_empty());
    }

    #[test]
    fn nil_rejects_plain_type() {
        let mut solver = Solver::default();
        let nil = solver.fresh_nil_handle(span(1, 2));
        let int = solver.concrete_type(&Type::Int);
        solver.add_handle_assignable(span(3, 4), nil, int);
        let errors = solver.solve_all();
        assert_eq!(errors.len(), 1);
        assert!(matches!(errors[0], SolveError::TypeMismatch { .. }));
    }

    #[test]
    fn nil_to_var_then_option() {
        let mut solver = Solver::default();
        let nil = solver.fresh_nil_handle(span(1, 2));
        let var = solver.fresh_temp_handle(span(3, 4));
        let option_handle = solver.concrete_type(&option(Type::Int));
        solver.add_handle_assignable(span(5, 6), nil.clone(), var.clone());
        solver.add_handle_assignable(span(7, 8), var.clone(), option_handle);
        assert!(solver.solve_all().is_empty());
        assert_eq!(solver.finalize_handle(&nil).0, option(Type::Int));
        assert_eq!(solver.finalize_handle(&var).0, option(Type::Int));
    }

    #[test]
    fn nil_unresolved_finalize() {
        let mut solver = Solver::default();
        solver.nil_expr_type(ExprId(7), span(1, 2));
        let (_, errors) = solver.finalize_expr_types();
        assert_eq!(
            errors,
            vec![SolverFinalizeError::UnresolvedType { span: span(1, 2) }]
        );
    }

    #[test]
    fn assign_arrays_and_slices() {
        let mut solver = Solver::default();
        assert!(
            solver
                .constrain_assignable(
                    span(1, 2),
                    ty_ref(fixed_array(Ty::Int, 3)),
                    ty_ref(fixed_array(Ty::Int, 3)),
                )
                .is_ok()
        );
        assert!(matches!(
            solver.constrain_assignable(
                span(3, 4),
                ty_ref(fixed_array(Ty::Int, 3)),
                ty_ref(fixed_array(Ty::Int, 4)),
            ),
            Err(SolveError::ConstMismatch { .. })
        ));
        assert!(
            solver
                .constrain_assignable(
                    span(5, 6),
                    ty_ref(fixed_array(Ty::Int, 3)),
                    ty_ref(slice(Ty::Int)),
                )
                .is_ok()
        );
        assert!(
            solver
                .constrain_assignable(span(7, 8), ty_ref(list(Ty::Int)), ty_ref(slice(Ty::Int)))
                .is_ok()
        );
    }

    #[test]
    fn assign_mismatch_dedup() {
        let mut solver = Solver::default();
        solver.add_assignable(
            span(1, 2),
            ty_ref(map(Ty::String, Ty::Int)),
            ty_ref(map(Ty::String, Ty::Bool)),
        );
        solver.add_assignable(
            span(1, 2),
            ty_ref(map(Ty::String, Ty::Int)),
            ty_ref(map(Ty::String, Ty::Bool)),
        );
        let errors = solver.solve_all();
        assert_eq!(errors.len(), 1);
        assert!(matches!(errors[0], SolveError::TypeMismatch { .. }));

        let mut solver = Solver::default();
        solver.add_assignable(
            span(3, 4),
            ty_ref(nominal("Meters", vec![])),
            ty_ref(nominal("Seconds", vec![])),
        );
        solver.add_assignable(
            span(3, 4),
            ty_ref(nominal("Meters", vec![])),
            ty_ref(nominal("Seconds", vec![])),
        );
        let errors = solver.solve_all();
        assert_eq!(errors.len(), 1);
        assert!(matches!(errors[0], SolveError::TypeMismatch { .. }));
    }

    #[test]
    fn assign_any_wildcard() {
        let mut solver = Solver::default();
        assert_eq!(
            solver.constrain_assignable(span(1, 2), ty_ref(Ty::Int), ty_ref(Ty::Any)),
            Ok(Ty::Any)
        );
        assert_eq!(
            solver.constrain_assignable(span(3, 4), ty_ref(Ty::Any), ty_ref(Ty::Int)),
            Ok(Ty::Int)
        );
    }

    #[test]
    fn assign_func_invariant() {
        let mut solver = Solver::default();
        let from = Ty::Func {
            params: vec![TyFuncParam {
                ty: Ty::Int,
                mutable: false,
                cast_accept: false,
            }],
            ret: Box::new(Ty::Int),
        };
        let to = Ty::Func {
            params: vec![TyFuncParam {
                ty: Ty::Int,
                mutable: false,
                cast_accept: false,
            }],
            ret: Box::new(ty_option(Ty::Int)),
        };
        assert!(matches!(
            solver.constrain_assignable(span(1, 2), ty_ref(from), ty_ref(to)),
            Err(SolveError::TypeMismatch { .. })
        ));
    }

    #[test]
    fn constraints_late_binding() {
        let mut solver = Solver::default();
        let a = infer_id(solver.fresh_type(span(1, 2)));
        let b = infer_id(solver.fresh_type(span(3, 4)));
        solver.add_equal(span(5, 6), ty_ref(Ty::Infer(a)), ty_ref(Ty::Infer(b)));
        solver.add_equal(span(7, 8), ty_ref(Ty::Infer(b)), ty_ref(Ty::String));
        assert!(solver.solve_all().is_empty());
        assert_eq!(solver.resolve_ty(&Ty::Infer(a)), Ty::String);
        assert_eq!(solver.resolve_ty(&Ty::Infer(b)), Ty::String);
    }

    #[test]
    fn constraints_equal_first() {
        let mut solver = Solver::default();
        let a = infer_id(solver.fresh_type(span(1, 2)));
        solver.add_assignable(span(3, 4), ty_ref(Ty::Infer(a)), ty_ref(ty_option(Ty::Int)));
        solver.add_equal(span(5, 6), ty_ref(Ty::Infer(a)), ty_ref(Ty::Int));
        assert!(solver.solve_all().is_empty());
        assert_eq!(solver.resolve_ty(&Ty::Infer(a)), Ty::Int);
    }

    #[test]
    fn constraint_conflict_order() {
        let mut solver = Solver::default();
        solver.add_equal(span(1, 2), ty_ref(Ty::Int), ty_ref(Ty::String));
        solver.add_equal(span(3, 4), ty_ref(Ty::Bool), ty_ref(Ty::String));
        let errors = solver.solve_all();
        assert_eq!(
            errors,
            vec![
                SolveError::TypeMismatch {
                    expected: Ty::Int,
                    found: Ty::String,
                    span: span(1, 2),
                },
                SolveError::TypeMismatch {
                    expected: Ty::Bool,
                    found: Ty::String,
                    span: span(3, 4),
                },
            ]
        );
    }

    #[test]
    fn discarded_temp_unresolved() {
        let mut solver = Solver::default();
        let temp_ty = solver.fresh_type(span(1, 2));
        solver.alloc_temp(temp_ty);
        assert!(solver.solve_all().is_empty());
    }

    #[test]
    fn observable_expr_unresolved() {
        let mut solver = Solver::default();
        let expr = ExprId(77);
        let ty = solver.fresh_type(span(4, 9));
        solver.set_expr_type(expr, span(4, 9), ty);
        let (finalized, errors) = solver.finalize_ty(&solver.resolve_ref(&TypeRef::expr(expr)));
        assert_eq!(finalized, Type::Infer);
        assert_eq!(
            errors,
            vec![InferError::UnresolvedType { span: span(4, 9) }]
        );
    }

    #[test]
    fn facade_assign() {
        let mut solver = Solver::default();
        let temp = solver.fresh_temp_handle(span(1, 2));
        solver.add_handle_assignable(span(3, 4), solver.concrete_type(&Type::Int), temp.clone());
        assert!(solver.solve_pending().is_empty());
        assert_eq!(solver.handle_to_type(&temp), Type::Int);
    }

    #[test]
    fn finalize_dedup() {
        let mut solver = Solver::default();
        let first = solver.fresh_expr_type(ExprId(1), span(1, 2));
        solver.set_expr_type_from_handle(ExprId(2), span(3, 4), first);
        let (_, errors) = solver.finalize_expr_types();
        assert_eq!(
            errors,
            vec![SolverFinalizeError::UnresolvedType { span: span(1, 2) }]
        );
    }

    #[test]
    fn nil_non_option() {
        let mut solver = Solver::default();
        let nil = solver.nil_expr_type(ExprId(1), span(1, 2));
        solver.add_handle_assignable(span(3, 4), nil, solver.concrete_type(&Type::Int));
        assert!(matches!(
            solver.solve_pending().as_slice(),
            [SolverRelationError::TypeMismatch {
                expected: Type::Int,
                found: Type::Infer,
                ..
            }]
        ));
    }

    #[test]
    fn finalize_unresolved() {
        let mut solver = Solver::default();
        let handle = solver.fresh_temp_handle(span(5, 6));
        let (ty, errors) = solver.finalize_handle(&handle);
        assert_eq!(ty, Type::Infer);
        assert_eq!(
            errors,
            vec![SolverFinalizeError::UnresolvedType { span: span(5, 6) }]
        );
    }
}
