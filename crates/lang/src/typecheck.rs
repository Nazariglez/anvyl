use crate::{
    ast::{
        AssignNode, AssignOp, BinaryNode, BinaryOp, BindingNode, BlockNode, CallNode, ExprId,
        ExprKind, ExprNode, FieldAccessNode, Func, FuncNode, Ident, IfNode, Lit, Pattern,
        PatternNode, Program, ReturnNode, Stmt, StmtNode, StructField, StructLiteralNode,
        TupleIndexNode, Type, TypeParam, TypeVarId, UnaryNode, UnaryOp,
    },
    span::Span,
};
use internment::Intern;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
struct StructDef {
    type_params: Vec<TypeParam>,
    fields: Vec<StructField>,
}

type InferenceSlots = HashMap<TypeVarId, Ident>;

pub fn check_program(program: &Program) -> Result<TypeChecker, Vec<TypeErr>> {
    let mut type_checker = TypeChecker::default();
    let mut errors = vec![];

    // first pass we collect the types from the ast
    // we don't need the type of the file scope blocks
    let _ = check_block_stmts(&program.stmts, &mut type_checker, &mut errors);

    if !errors.is_empty() {
        return Err(errors);
    }

    // second pass we infer the types from the constraints
    resolve_constraints(&mut type_checker, &mut errors);

    // at this point there should be no remaining unresolved types
    // so if there are any we add an error
    for (_expr_id, (span, ty)) in &type_checker.types {
        if contains_infer(ty) {
            errors.push(TypeErr {
                span: *span,
                kind: TypeErrKind::UnresolvedInfer,
            });
        }
    }

    if !errors.is_empty() {
        return Err(errors);
    }

    Ok(type_checker)
}

#[derive(Debug, Clone, PartialEq)]
struct RetType {
    ty: Type,
    has_explicit: bool,
}

/// Key for caching scpecialized generic functions (instantiated with concrete types)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct SpecializationKey {
    func_name: Ident,
    type_args: Vec<Type>,
}

#[derive(Debug, Clone)]
struct SpecializationResult {
    ret_ty: Type,
    err_kind: Option<TypeErrKind>,
}

#[derive(Debug)]
pub struct TypeChecker {
    /// Resolved type for each expression
    types: HashMap<ExprId, (Span, Type)>,

    /// Stack of scopes for variable lookup
    scopes: Vec<HashMap<Ident, Type>>,

    /// Stack of return types for function calls
    return_types: Vec<RetType>,

    /// Type constraints to be resolved by inference pass
    constraints: Vec<Constraint>,

    /// Generic type params declared for function
    func_type_params: HashMap<Ident, Vec<TypeParam>>,

    /// Identify inference slots uniquely across multiple generic calls
    next_infer_call_id: usize,

    /// Stores the generic function templates for later instantiation at call sites
    /// the bodies are checked when instantiated with concrete type arguments in a later pass
    generic_func_templates: HashMap<Ident, FuncNode>,

    /// Stores specialized functions avoiding re-checking for same type arguments
    specialization_cache: HashMap<SpecializationKey, SpecializationResult>,

    /// Stores struct definitions (name -> fields)
    struct_defs: HashMap<Ident, StructDef>,
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self {
            types: HashMap::new(),
            scopes: vec![],
            return_types: vec![],
            constraints: vec![],
            func_type_params: HashMap::new(),
            next_infer_call_id: 0,
            generic_func_templates: HashMap::new(),
            specialization_cache: HashMap::new(),
            struct_defs: HashMap::new(),
        }
    }
}

impl TypeChecker {
    fn next_call_id(&mut self) -> usize {
        let id = self.next_infer_call_id;
        self.next_infer_call_id += 1;
        id
    }
}

impl TypeChecker {
    fn get_struct(&self, name: Ident) -> Option<&StructDef> {
        self.struct_defs.get(&name)
    }

    fn resolve_type(&self, ty: &Type) -> Type {
        match ty {
            Type::UnresolvedName(name) if self.struct_defs.contains_key(name) => Type::Struct {
                name: *name,
                type_args: vec![],
            },
            Type::Struct { name, type_args } => Type::Struct {
                name: *name,
                type_args: type_args.iter().map(|t| self.resolve_type(t)).collect(),
            },
            Type::Optional(inner) => Type::Optional(Box::new(self.resolve_type(inner))),
            Type::Tuple(elems) => Type::Tuple(elems.iter().map(|t| self.resolve_type(t)).collect()),
            Type::NamedTuple(fields) => Type::NamedTuple(
                fields
                    .iter()
                    .map(|(n, t)| (*n, self.resolve_type(t)))
                    .collect(),
            ),
            Type::Func { params, ret } => Type::Func {
                params: params.iter().map(|t| self.resolve_type(t)).collect(),
                ret: Box::new(self.resolve_type(ret)),
            },
            _ => ty.clone(),
        }
    }
}

impl TypeChecker {
    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn set_type(&mut self, id: ExprId, ty: Type, span: Span) {
        self.types.insert(id, (span, ty));
    }

    pub fn get_type(&self, id: ExprId) -> Option<&(Span, Type)> {
        self.types.get(&id)
    }

    pub fn set_var(&mut self, name: Ident, ty: Type) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, ty);
        }
    }

    pub fn get_var(&self, name: Ident) -> Option<&Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(&name) {
                return Some(ty);
            }
        }
        None
    }

    fn push_return_type(&mut self, ty: Type) {
        self.return_types.push(RetType {
            ty,
            has_explicit: false,
        });
    }

    fn pop_return_type(&mut self) {
        self.return_types.pop();
    }

    fn current_return_type(&self) -> Option<&Type> {
        self.return_types.last().map(|r| &r.ty)
    }

    fn mark_explicit_return(&mut self) {
        if let Some(ret_ty) = self.return_types.last_mut() {
            ret_ty.has_explicit = true;
        }
    }

    fn has_explicit_return(&self) -> bool {
        self.return_types.last().map_or(false, |r| r.has_explicit)
    }

    fn add_constraint(&mut self, span: Span, left: TypeRef, right: TypeRef) {
        self.constraints.push(Constraint { span, left, right });
    }

    fn get_type_ref(&self, r: &TypeRef) -> Option<Type> {
        match r {
            TypeRef::Expr(id) => self.get_type(*id).map(|(_, ty)| ty.clone()),
            TypeRef::Var(ident) => self.get_var(*ident).cloned(),
            TypeRef::Concrete(t) => Some(t.clone()),
        }
    }

    fn set_type_ref(&mut self, r: &TypeRef, ty: Type, span: Span) {
        match r {
            TypeRef::Expr(id) => self.set_type(*id, ty, span),
            TypeRef::Var(ident) => self.set_var(*ident, ty),
            TypeRef::Concrete(_) => {} // Cannot write to concrete types
        }
    }

    /// Constrains two types that must be the same
    /// ie: let x:int = 10; x += 10; (int to int)
    fn constrain_equal(
        &mut self,
        span: Span,
        left: TypeRef,
        right: TypeRef,
        errors: &mut Vec<TypeErr>,
    ) {
        // try to unify the types immediately
        let unified = unify_equal(self, span, &left, &right, errors);

        // otherwise add a constraint to be resolved later
        if !unified {
            self.add_constraint(span, left, right);
        }
    }

    /// Constraints 'value' to be assignable to 'target'
    /// ie: let x:int? = 10; fn() -> int? { return 10; } (T to T?)
    fn constrain_assignable(
        &mut self,
        span: Span,
        from: TypeRef,
        to: TypeRef,
        errors: &mut Vec<TypeErr>,
    ) {
        let from_ty = self.get_type_ref(&from).unwrap_or(Type::Infer);
        let to_ty = self.get_type_ref(&to).unwrap_or(Type::Infer);

        // if both types are resolved, check assignability and set the types
        let both_resolved = !(contains_infer(&from_ty) || contains_infer(&to_ty));
        if both_resolved {
            if !is_assignable(&from_ty, &to_ty) {
                errors.push(TypeErr {
                    span,
                    kind: TypeErrKind::MismatchedTypes {
                        expected: to_ty.clone(),
                        found: from_ty.clone(),
                    },
                });
            }
            self.set_type_ref(&to, to_ty, span);
            self.set_type_ref(&from, from_ty, span);
            return;
        }

        // at this point at least one type is unresolved
        // if both are optionals, constrain the inner types
        if let (Type::Optional(inner_from), Type::Optional(inner_to)) = (&from_ty, &to_ty) {
            let inner_from_ref = TypeRef::Concrete(*inner_from.clone());
            let inner_to_ref = TypeRef::Concrete(*inner_to.clone());
            self.constrain_equal(span, inner_from_ref, inner_to_ref, errors);
            self.set_type_ref(&from, Type::Optional(inner_to.clone()), span);
            return;
        }

        // if to is an optional and from has inference, constrain from to the inner type of to
        if let Type::Optional(inner_to) = &to_ty
            && contains_infer(&from_ty)
        {
            self.constrain_equal(span, from, TypeRef::Concrete(*inner_to.clone()), errors);
            return;
        }

        // otherwise just constrain them to be the same as fallback
        self.constrain_equal(span, from, to, errors);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeErr {
    pub span: Span,
    pub kind: TypeErrKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeErrKind {
    UnknownVariable {
        name: Ident,
    },
    MismatchedTypes {
        expected: Type,
        found: Type,
    },
    InvalidOperand {
        op: String,
        operand_type: Type,
    },
    UnknownFunction {
        name: Ident,
    },
    UnresolvedInfer,
    NotAFunction {
        expr_type: Type,
    },
    GenericArgNumMismatch {
        expected: usize,
        found: usize,
    },
    NotGenericFunction,
    IfConditionNotBool {
        found: Type,
    },
    IfMissingElse,
    TupleIndexOnNonTuple {
        found: Type,
        index: u32,
    },
    TupleIndexOutOfBounds {
        tuple_type: Type,
        index: u32,
        len: usize,
    },
    TuplePatternArityMismatch {
        expected: usize,
        found: usize,
    },
    NonTupleInTuplePattern {
        found: Type,
        pattern_arity: usize,
    },
    TuplePatternLabelMismatch {
        expected: Ident,
        found: Ident,
    },
    NamedPatternOnPositionalTuple,
    DuplicateTupleLabel {
        label: Ident,
    },
    NoSuchFieldOnTuple {
        field: Ident,
        tuple_type: Type,
    },
    FieldAccessOnNonNamedTuple {
        field: Ident,
        found: Type,
    },
    UnknownStruct {
        name: Ident,
    },
    UnknownType {
        name: Ident,
    },
    StructMissingField {
        struct_name: Ident,
        field: Ident,
    },
    StructUnknownField {
        struct_name: Ident,
        field: Ident,
    },
    StructDuplicateField {
        struct_name: Ident,
        field: Ident,
    },
    FieldAccessOnNonStruct {
        field: Ident,
        found: Type,
    },
}

#[derive(Debug, Clone)]
enum TypeRef {
    Expr(ExprId),
    Var(Ident),
    Concrete(Type),
}

#[derive(Debug, Clone)]
struct Constraint {
    span: Span,
    left: TypeRef,
    right: TypeRef,
}

fn contains_infer(ty: &Type) -> bool {
    match ty {
        Type::Infer => true,
        Type::Optional(inner) => contains_infer(inner),
        Type::Func { params, ret } => params.iter().any(contains_infer) || contains_infer(ret),
        _ => false,
    }
}

/// Checks if 'from' is assignable to 'to'
fn is_assignable(from: &Type, to: &Type) -> bool {
    use Type::*;

    // same type is always assignable
    let is_same_type = from == to;
    if is_same_type {
        return true;
    }

    // if either side is Infer, we need to unify them
    let needs_inference = from.is_infer() || to.is_infer();
    if needs_inference {
        return true;
    }

    match (from, to) {
        // optional types needs to check the inner types
        (Optional(inner_from), Optional(inner_to)) => is_assignable(inner_from, inner_to),

        // T to T? is assignable
        (from_ty, Optional(inner_to)) if !matches!(from_ty, Optional(_)) => {
            is_assignable(from_ty, inner_to)
        }

        // function types needs to check the signature (params + return type)
        (
            Func {
                params: params_from,
                ret: ret_from,
            },
            Func {
                params: params_to,
                ret: ret_to,
            },
        ) => {
            params_from.len() == params_to.len()
                && params_from
                    .iter()
                    .zip(params_to.iter())
                    .all(|(pf, pt)| is_assignable(pf, pt))
                && is_assignable(ret_from, ret_to)
        }

        // T? to T is not assignable, the value must be unwrapped first
        (Optional(_), non_opt) if !matches!(non_opt, Optional(_)) => false,

        // anything else is just not assignable
        _ => false,
    }
}

/// Unifies two types returning the unified type if successful
fn unify_types(left: &Type, right: &Type, span: Span, errors: &mut Vec<TypeErr>) -> Option<Type> {
    use Type::*;

    // same type, no need to unify
    if left == right {
        return Some(left.clone());
    }

    match (left, right) {
        // if either side is Infer we use the concrete side
        (Infer, t) | (t, Infer) => Some(t.clone()),

        // optional types needs to unify the inner types
        (Optional(l), Optional(r)) => {
            unify_types(l, r, span, errors).map(|inner| Optional(Box::new(inner)))
        }

        // function types needs to unify the params and return type
        (
            Func {
                params: lp,
                ret: lr,
            },
            Func {
                params: rp,
                ret: rr,
            },
        ) => {
            if lp.len() != rp.len() {
                errors.push(TypeErr {
                    span,
                    kind: TypeErrKind::MismatchedTypes {
                        expected: left.clone(),
                        found: right.clone(),
                    },
                });
                return None;
            }

            let mut new_params = Vec::with_capacity(lp.len());
            for (lpi, rpi) in lp.iter().zip(rp.iter()) {
                unify_types(lpi, rpi, span, errors).map(|p| new_params.push(p))?;
            }

            unify_types(lr, rr, span, errors).map(|new_ret| Func {
                params: new_params,
                ret: Box::new(new_ret),
            })
        }

        // mismatched types report an error
        (l, r) => {
            errors.push(TypeErr {
                span,
                kind: TypeErrKind::MismatchedTypes {
                    expected: l.clone(),
                    found: r.clone(),
                },
            });
            None
        }
    }
}

fn unify_equal(
    tcx: &mut TypeChecker,
    span: Span,
    left: &TypeRef,
    right: &TypeRef,
    errors: &mut Vec<TypeErr>,
) -> bool {
    let (Some(lt), Some(rt)) = (tcx.get_type_ref(left), tcx.get_type_ref(right)) else {
        return false;
    };

    match unify_types(&lt, &rt, span, errors) {
        Some(new_ty) => {
            tcx.set_type_ref(left, new_ty.clone(), span);
            tcx.set_type_ref(right, new_ty, span);
            true
        }
        None => false,
    }
}

fn resolve_constraints(type_checker: &mut TypeChecker, errors: &mut Vec<TypeErr>) {
    // keep going until we make no progress infering types
    loop {
        if !resolve_constraints_pass(type_checker, errors) {
            break;
        }
    }
}

fn resolve_constraints_pass(type_checker: &mut TypeChecker, errors: &mut Vec<TypeErr>) -> bool {
    let mut made_progress = false;

    let constraints = std::mem::take(&mut type_checker.constraints);
    for c in constraints {
        let unified = unify_equal(type_checker, c.span, &c.left, &c.right, errors);
        if !unified {
            type_checker.constraints.push(c);
        }

        // if unified just set made_progress to true otherwise keep it false
        made_progress |= unified;
    }

    made_progress
}

fn check_block_stmts(
    stmts: &[StmtNode],
    type_checker: &mut TypeChecker,
    errors: &mut Vec<TypeErr>,
) -> Option<ExprId> {
    type_checker.push_scope();
    collect_scope_types(stmts, type_checker);

    let last_expr_id = stmts.split_last().and_then(|(last, rest)| {
        // check all the statements except the last one
        rest.iter().for_each(|stmt| {
            check_stmt(&stmt.node, type_checker, errors);
        });

        // process the last statement as expression if needed
        match &last.node {
            Stmt::Expr(expr_node) => {
                let _ = check_expr(expr_node, type_checker, errors);
                Some(expr_node.node.id)
            }
            _ => {
                check_stmt(&last.node, type_checker, errors);
                None
            }
        }
    });

    type_checker.pop_scope();
    last_expr_id
}

fn check_block_expr(
    block: &BlockNode,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<TypeErr>,
) -> (Type, Option<ExprId>) {
    let last_expr_id = check_block_stmts(&block.node.stmts, type_checker, errors);
    let Some(id) = last_expr_id else {
        return (Type::Void, None);
    };

    let ty = type_checker
        .get_type(id)
        .cloned()
        .map(|(_, ty)| ty)
        .unwrap_or(Type::Void);

    (ty, Some(id))
}

fn collect_scope_types(stmts: &[StmtNode], type_checker: &mut TypeChecker) {
    for stmt in stmts {
        match &stmt.node {
            Stmt::Func(node) => {
                let func = &node.node;
                type_checker.set_var(func.name, type_from_fn(func));

                // if the function is generic, store the type parameters and template
                // because they are not fully typechecked at definition time until they are used
                let is_generic = !func.type_params.is_empty();
                if is_generic {
                    type_checker
                        .func_type_params
                        .insert(func.name, func.type_params.clone());

                    // store the function ast for later instantiation
                    type_checker
                        .generic_func_templates
                        .insert(func.name, node.clone());
                }
            }

            Stmt::Struct(node) => {
                let decl = &node.node;
                type_checker.struct_defs.insert(
                    decl.name,
                    StructDef {
                        type_params: decl.type_params.clone(),
                        fields: decl.fields.clone(),
                    },
                );
            }

            _ => {}
        }
    }
}

/// Builds a fucntion type from the AST node
fn type_from_fn(func: &Func) -> Type {
    Type::Func {
        params: func.params.iter().map(|param| param.ty.clone()).collect(),
        ret: Box::new(func.ret.clone()),
    }
}

/// Substitutes type variables in a type with concrete types from the substitution map
/// fn(T) -> T where "T = int" then fn(int) -> int
fn subst_type(ty: &Type, subst: &HashMap<TypeVarId, Type>) -> Type {
    use Type::*;
    match ty {
        Var(id) => subst.get(id).cloned().unwrap_or_else(|| ty.clone()),
        Optional(inner) => Optional(subst_type(inner, subst).boxed()),
        Func { params, ret } => Func {
            params: params.iter().map(|p| subst_type(p, subst)).collect(),
            ret: subst_type(ret, subst).boxed(),
        },
        Tuple(elems) => Tuple(elems.iter().map(|e| subst_type(e, subst)).collect()),
        NamedTuple(fields) => NamedTuple(
            fields
                .iter()
                .map(|(n, t)| (*n, subst_type(t, subst)))
                .collect(),
        ),
        Struct { name, type_args } => Struct {
            name: *name,
            type_args: type_args.iter().map(|a| subst_type(a, subst)).collect(),
        },
        _ => ty.clone(),
    }
}

/// Instantiates a generic function type with explicit type arguments
fn instantiate_func_type(
    type_params: &[TypeParam],
    template: &Type,
    type_args: &[Type],
    span: Span,
    errors: &mut Vec<TypeErr>,
) -> Option<Type> {
    let same_param_count = type_params.len() == type_args.len();
    if !same_param_count {
        errors.push(TypeErr {
            span,
            kind: TypeErrKind::GenericArgNumMismatch {
                expected: type_params.len(),
                found: type_args.len(),
            },
        });
        return None;
    }

    let subst = type_params
        .iter()
        .zip(type_args.iter())
        .map(|(param, arg)| (param.id, arg.clone()))
        .collect();

    Some(subst_type(template, &subst))
}

/// Creates inference slots for a generic function call
/// for each type parameter, this creates a synthetic variable
/// in the type checker initialized to Type::Infer
fn create_inference_slots(
    type_params: &[TypeParam],
    type_checker: &mut TypeChecker,
    call_id: usize,
) -> InferenceSlots {
    type_params
        .iter()
        .map(|param| {
            // lets use as name _infer_<name>_<id>_<call_id>
            let name = format!("__infer_{}_{}_{}", param.name, param.id.0, call_id);
            let infer_var_name = Ident(Intern::new(name));

            // initialize the inference slot
            type_checker.set_var(infer_var_name, Type::Infer);

            // map the type variable id to its synthetic variable name
            (param.id, infer_var_name)
        })
        .collect()
}

/// Converts 'Type::Var' references to 'TypeRef' for constraints
fn type_to_ref_with_inference(ty: &Type, slots: &InferenceSlots) -> TypeRef {
    match ty {
        // use the synthetic variable ref for type variables
        Type::Var(id) => slots
            .get(id)
            .cloned()
            .map(TypeRef::Var)
            .unwrap_or_else(|| TypeRef::Concrete(ty.clone())),

        // anything else just use concrete
        _ => TypeRef::Concrete(substitute_vars_with_infer(ty, slots)),
    }
}

/// Substitutes 'Type::Var' with 'Type::Infer' for nested positions in compound types
/// imagine infer T from fn my_fn(T?) -> T then my_fn(10) and we should infer int from T?
fn substitute_vars_with_infer(ty: &Type, slots: &InferenceSlots) -> Type {
    match ty {
        Type::Var(id) if slots.contains_key(id) => Type::Infer,
        Type::Optional(inner) => Type::Optional(substitute_vars_with_infer(inner, slots).boxed()),
        Type::Func { params, ret } => Type::Func {
            params: params
                .iter()
                .map(|p| substitute_vars_with_infer(p, slots))
                .collect(),
            ret: substitute_vars_with_infer(ret, slots).boxed(),
        },
        Type::Tuple(elems) => Type::Tuple(
            elems
                .iter()
                .map(|e| substitute_vars_with_infer(e, slots))
                .collect(),
        ),
        Type::NamedTuple(fields) => Type::NamedTuple(
            fields
                .iter()
                .map(|(n, t)| (*n, substitute_vars_with_infer(t, slots)))
                .collect(),
        ),
        Type::Struct { name, type_args } => Type::Struct {
            name: *name,
            type_args: type_args
                .iter()
                .map(|a| substitute_vars_with_infer(a, slots))
                .collect(),
        },
        _ => ty.clone(),
    }
}

fn type_from_lit(lit: &Lit) -> Type {
    match lit {
        Lit::Int(_) => Type::Int,
        Lit::Float(_) => Type::Float,
        Lit::Bool(_) => Type::Bool,
        Lit::String(_) => Type::String,
        Lit::Nil => Type::Optional(Box::new(Type::Infer)),
    }
}

fn check_stmt(stmt: &Stmt, type_checker: &mut TypeChecker, errors: &mut Vec<TypeErr>) {
    match stmt {
        Stmt::Func(node) => check_func(node, type_checker, errors),
        Stmt::Struct(_) => {}
        Stmt::Expr(node) => {
            let _ = check_expr(node, type_checker, errors);
        }
        Stmt::Binding(node) => check_binding(node, type_checker, errors),
        Stmt::Return(node) => check_ret(node, type_checker, errors),
    }
}

fn check_fn_body(
    func: &Func,
    param_types: &[Type],
    ret_ty: Type,
    error_span: Span,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<TypeErr>,
) {
    type_checker.push_scope();
    type_checker.push_return_type(ret_ty.clone());

    // bind parameters into scope with the provided types
    for (param, ty) in func.params.iter().zip(param_types.iter()) {
        type_checker.set_var(param.name, ty.clone());
    }

    // treat the block as an expression for implicit returns
    let (body_ty, last_expr_id) = check_block_expr(&func.body, type_checker, errors);

    // void fn cannot have trailing expressions (at least they are void too)
    let is_void_fn = ret_ty.is_void();
    if is_void_fn {
        if !body_ty.is_void() {
            errors.push(TypeErr {
                span: error_span,
                kind: TypeErrKind::MismatchedTypes {
                    expected: Type::Void,
                    found: body_ty,
                },
            });
        }
    } else if let Some(last_id) = last_expr_id {
        // if there is a last expression, it must be assignable to return type
        let expr_ref = TypeRef::Expr(last_id);
        let ret_ref = TypeRef::Concrete(ret_ty.clone());
        type_checker.constrain_assignable(error_span, expr_ref, ret_ref, errors);
    } else if !type_checker.has_explicit_return() {
        // no implicit or explicit return, fn with non-void return is invalid
        errors.push(TypeErr {
            span: error_span,
            kind: TypeErrKind::MismatchedTypes {
                expected: ret_ty,
                found: Type::Void,
            },
        });
    }

    type_checker.pop_return_type();
    type_checker.pop_scope();
}

fn check_func(fn_node: &FuncNode, type_checker: &mut TypeChecker, errors: &mut Vec<TypeErr>) {
    let func = &fn_node.node;

    // if the function is generic we skip checking here
    // it will be done at instantiation time with concrete types
    let is_generic = !func.type_params.is_empty();
    if is_generic {
        return;
    }

    let Some(ty) = type_checker.get_var(func.name) else {
        errors.push(TypeErr {
            span: fn_node.span,
            kind: TypeErrKind::UnknownFunction { name: func.name },
        });

        return;
    };

    if !matches!(ty, Type::Func { .. }) {
        errors.push(TypeErr {
            span: fn_node.span,
            kind: TypeErrKind::MismatchedTypes {
                expected: type_from_fn(func),
                found: ty.clone(),
            },
        });

        return;
    }

    // build param types from the functions declared parameters
    let param_types: Vec<Type> = func.params.iter().map(|p| p.ty.clone()).collect();

    check_fn_body(
        func,
        &param_types,
        func.ret.clone(),
        fn_node.span,
        type_checker,
        errors,
    );
}

fn check_expr(
    expr_node: &ExprNode,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<TypeErr>,
) -> Type {
    let expr = &expr_node.node;
    let ty = match &expr.kind {
        ExprKind::Ident(ident) => match type_checker.get_var(*ident) {
            Some(ty) => ty.clone(),
            None => {
                errors.push(TypeErr {
                    span: expr_node.span,
                    kind: TypeErrKind::UnknownVariable { name: *ident },
                });
                Type::Infer
            }
        },
        ExprKind::Block(spanned) => {
            let (block_ty, _) = check_block_expr(spanned, type_checker, errors);
            block_ty
        }
        ExprKind::Lit(lit) => type_from_lit(lit),
        ExprKind::Call(call) => check_call(call, type_checker, errors),
        ExprKind::Binary(bin) => check_binary(bin, type_checker, errors),
        ExprKind::Unary(unary) => check_unary(unary, type_checker, errors),
        ExprKind::Assign(assign) => check_assign(assign, type_checker, errors),
        ExprKind::If(if_node) => check_if(if_node, type_checker, errors),
        ExprKind::Tuple(elements) => check_tuple(elements, type_checker, errors),
        ExprKind::NamedTuple(elements) => {
            check_named_tuple(elements, expr_node.span, type_checker, errors)
        }
        ExprKind::TupleIndex(index_node) => check_tuple_index(index_node, type_checker, errors),
        ExprKind::Field(field_node) => check_field_access(field_node, type_checker, errors),
        ExprKind::StructLiteral(lit_node) => check_struct_lit(lit_node, type_checker, errors),
    };

    type_checker.set_type(expr_node.node.id, ty.clone(), expr_node.span);
    ty
}

fn check_call(call: &CallNode, type_checker: &mut TypeChecker, errors: &mut Vec<TypeErr>) -> Type {
    let node = &call.node;
    let func_ty = check_expr(&node.func, type_checker, errors);

    // try to get the function name to look up type parameters
    let func_name = match &node.func.node.kind {
        ExprKind::Ident(ident) => Some(*ident),
        _ => None,
    };

    // lookup type params for generic functions
    let type_params = func_name
        .and_then(|name| type_checker.func_type_params.get(&name))
        .cloned()
        .unwrap_or_default();

    let is_generic = !type_params.is_empty();
    let has_type_args = !node.type_args.is_empty();

    // handle generic function calls with template instantiation
    match (is_generic, has_type_args) {
        // non generic functions with type params are invalid
        (false, true) => {
            errors.push(TypeErr {
                span: call.span,
                kind: TypeErrKind::NotGenericFunction,
            });
            return Type::Infer;
        }

        // generic function without type args -> infer type args, then instantiate
        (true, false) => {
            let Some(name) = func_name else {
                errors.push(TypeErr {
                    span: call.span,
                    kind: TypeErrKind::NotAFunction {
                        expr_type: func_ty.clone(),
                    },
                });
                return Type::Infer;
            };

            // create inference slots for each type param
            let call_id = type_checker.next_call_id();
            let slots = create_inference_slots(&type_params, type_checker, call_id);

            // check arguments and generate constraints to infer type args
            let Type::Func { params, ret: _ } = &func_ty else {
                errors.push(TypeErr {
                    span: call.span,
                    kind: TypeErrKind::NotAFunction {
                        expr_type: func_ty.clone(),
                    },
                });
                return Type::Infer;
            };

            // check argument count
            let same_param_count = params.len() == node.args.len();
            if !same_param_count {
                errors.push(TypeErr {
                    span: call.span,
                    kind: TypeErrKind::MismatchedTypes {
                        expected: func_ty.clone(),
                        found: Type::Func {
                            params: vec![Type::Infer; node.args.len()],
                            ret: Box::new(Type::Infer),
                        },
                    },
                });
                return Type::Infer;
            }

            // check each argument and add constraints for type inference
            for (arg_expr, param_ty) in node.args.iter().zip(params.iter()) {
                check_expr(arg_expr, type_checker, errors);
                let arg_ref = TypeRef::Expr(arg_expr.node.id);
                let param_ref = type_to_ref_with_inference(param_ty, &slots);
                type_checker.constrain_assignable(arg_expr.span, arg_ref, param_ref, errors);
            }

            // resolve constraints to get inferred types
            resolve_constraints(type_checker, errors); // FIXME: isn't resolving all the ast here too much? maybe we can do this in a more efficient way?

            // read back the inferred types from the slots
            let mut inferred_type_args = Vec::with_capacity(type_params.len());
            let mut inference_failed = false;

            for param in &type_params {
                let slot_var = slots
                    .get(&param.id)
                    .and_then(|slot_ident| type_checker.get_var(*slot_ident));

                let ty = slot_var
                    .filter(|ty| !contains_infer(ty))
                    .cloned()
                    .unwrap_or_else(|| {
                        inference_failed = true;
                        Type::Infer
                    });

                inferred_type_args.push(ty);
            }

            if inference_failed {
                errors.push(TypeErr {
                    span: call.span,
                    kind: TypeErrKind::UnresolvedInfer,
                });
                return Type::Infer;
            }

            // now instantiate and check the function body with the inferred types
            return instantiate_and_check_fn(
                name,
                &inferred_type_args,
                call.span,
                type_checker,
                errors,
            );
        }

        // generic function with type args -> explicit instantiation
        (true, true) => {
            // error if not a function
            let Some(name) = func_name else {
                errors.push(TypeErr {
                    span: call.span,
                    kind: TypeErrKind::NotAFunction {
                        expr_type: func_ty.clone(),
                    },
                });
                return Type::Infer;
            };

            let same_param_count = type_params.len() == node.type_args.len();
            if !same_param_count {
                errors.push(TypeErr {
                    span: call.span,
                    kind: TypeErrKind::GenericArgNumMismatch {
                        expected: type_params.len(),
                        found: node.type_args.len(),
                    },
                });
                return Type::Infer;
            }

            // check arguments against the instantiated parameter types
            let Type::Func { params, ret: _ } = &func_ty else {
                errors.push(TypeErr {
                    span: call.span,
                    kind: TypeErrKind::NotAFunction {
                        expr_type: func_ty.clone(),
                    },
                });
                return Type::Infer;
            };

            // build map substitution for parameter type checking
            let subst = type_params
                .iter()
                .zip(node.type_args.iter())
                .map(|(param, arg)| (param.id, arg.clone()))
                .collect::<HashMap<TypeVarId, _>>();

            let params_count = params.len() == node.args.len();
            if !params_count {
                let instantiated_ty = instantiate_func_type(
                    &type_params,
                    &func_ty,
                    &node.type_args,
                    call.span,
                    errors,
                );
                errors.push(TypeErr {
                    span: call.span,
                    kind: TypeErrKind::MismatchedTypes {
                        expected: instantiated_ty.unwrap_or(func_ty.clone()),
                        found: Type::Func {
                            params: vec![Type::Infer; node.args.len()],
                            ret: Box::new(Type::Infer),
                        },
                    },
                });
                return Type::Infer;
            }

            // check each argument against the substituted parameter type
            for (arg_expr, param_ty) in node.args.iter().zip(params.iter()) {
                check_expr(arg_expr, type_checker, errors);
                let instantiated_param_ty = subst_type(param_ty, &subst);
                let arg_ref = TypeRef::Expr(arg_expr.node.id);
                let param_ref = TypeRef::Concrete(instantiated_param_ty);
                type_checker.constrain_assignable(arg_expr.span, arg_ref, param_ref, errors);
            }

            // now instantiate and check the function body with the explicit types
            return instantiate_and_check_fn(
                name,
                &node.type_args,
                call.span,
                type_checker,
                errors,
            );
        }

        // non generic function without type args then must be a normal call
        (false, false) => {}
    }

    // fallback to normal call
    check_call_with_type(call, func_ty, type_checker, errors)
}

/// Check a function call given the function type
fn check_call_with_type(
    call: &CallNode,
    func_ty: Type,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<TypeErr>,
) -> Type {
    let node = &call.node;

    match func_ty.clone() {
        Type::Func { params, ret } => {
            let same_params_len = params.len() == node.args.len();
            if !same_params_len {
                errors.push(TypeErr {
                    span: call.span,
                    kind: TypeErrKind::MismatchedTypes {
                        expected: Type::Func {
                            params: params.clone(),
                            ret: ret.clone(),
                        },
                        found: func_ty,
                    },
                });
                return Type::Infer;
            }

            for (arg_expr, param_ty) in node.args.iter().zip(params.iter()) {
                check_expr(arg_expr, type_checker, errors);
                let arg_ref = TypeRef::Expr(arg_expr.node.id);
                let param_ref = TypeRef::Concrete(param_ty.clone());
                type_checker.constrain_assignable(arg_expr.span, arg_ref, param_ref, errors);
            }

            *ret
        }
        _ => {
            errors.push(TypeErr {
                span: call.span,
                kind: TypeErrKind::NotAFunction { expr_type: func_ty },
            });
            Type::Infer
        }
    }
}

/// Instantiates a generic function with concrete type arguments and typechecks the specialized body
fn instantiate_and_check_fn(
    func_name: Ident,
    type_args: &[Type],
    call_span: Span,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<TypeErr>,
) -> Type {
    let cache_key = SpecializationKey {
        func_name,
        type_args: type_args.to_vec(),
    };

    // check cache first
    if let Some(cached) = type_checker.specialization_cache.get(&cache_key) {
        // if there was an error we report it with the current call site span
        if let Some(err_kind) = &cached.err_kind {
            errors.push(TypeErr {
                span: call_span,
                kind: err_kind.clone(),
            });
        }
        return cached.ret_ty.clone();
    }

    // look up the generic function template
    let Some(fn_template) = type_checker.generic_func_templates.get(&func_name).cloned() else {
        errors.push(TypeErr {
            span: call_span,
            kind: TypeErrKind::UnknownFunction { name: func_name },
        });
        return Type::Infer;
    };

    // look up type parameters
    let Some(type_params) = type_checker.func_type_params.get(&func_name).cloned() else {
        errors.push(TypeErr {
            span: call_span,
            kind: TypeErrKind::UnknownFunction { name: func_name },
        });
        return Type::Infer;
    };

    // verify arity
    let same_param_count = type_params.len() == type_args.len();
    if !same_param_count {
        errors.push(TypeErr {
            span: call_span,
            kind: TypeErrKind::GenericArgNumMismatch {
                expected: type_params.len(),
                found: type_args.len(),
            },
        });
        return Type::Infer;
    }

    // build substitution map to convert TypeVarId -> concrete Type
    let subst: HashMap<TypeVarId, Type> = type_params
        .iter()
        .zip(type_args.iter())
        .map(|(param, arg)| (param.id, arg.clone()))
        .collect();

    let func = &fn_template.node;

    // compute the specialized parameter and return types
    let specialized_param_types: Vec<Type> = func
        .params
        .iter()
        .map(|p| subst_type(&p.ty, &subst))
        .collect();
    let specialized_ret = subst_type(&func.ret, &subst);

    // typecheck the body with specialized types
    let mut body_errors = vec![];
    check_fn_body(
        func,
        &specialized_param_types,
        specialized_ret.clone(),
        call_span,
        type_checker,
        &mut body_errors,
    );

    // cache the result
    let err_kind = body_errors.first().map(|err| err.kind.clone());
    type_checker.specialization_cache.insert(
        cache_key,
        SpecializationResult {
            ret_ty: specialized_ret.clone(),
            err_kind,
        },
    );

    // report any errors from the body with the call site span
    for err in body_errors {
        errors.push(TypeErr {
            span: call_span,
            kind: err.kind,
        });
    }

    specialized_ret
}

fn check_binary(
    bin: &BinaryNode,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<TypeErr>,
) -> Type {
    use BinaryOp::*;

    let node = &bin.node;
    let left_ty = check_expr(&node.left, type_checker, errors);
    let right_ty = check_expr(&node.right, type_checker, errors);
    let same_ty = left_ty == right_ty;

    match node.op {
        // numeric ops
        Add | Sub | Mul | Div | Rem => {
            if left_ty.is_num() && same_ty {
                left_ty
            } else {
                errors.push(TypeErr {
                    span: bin.span,
                    kind: TypeErrKind::MismatchedTypes {
                        expected: left_ty.clone(),
                        found: right_ty.clone(),
                    },
                });
                Type::Infer
            }
        }

        // equal ops must be the same type
        Eq | NotEq => {
            if same_ty {
                Type::Bool
            } else {
                errors.push(TypeErr {
                    span: bin.span,
                    kind: TypeErrKind::MismatchedTypes {
                        expected: left_ty.clone(),
                        found: right_ty.clone(),
                    },
                });
                Type::Bool
            }
        }

        // comparison ops must be numeric
        LessThan | GreaterThan | LessThanEq | GreaterThanEq => {
            if left_ty.is_num() && same_ty {
                Type::Bool
            } else {
                errors.push(TypeErr {
                    span: bin.span,
                    kind: TypeErrKind::MismatchedTypes {
                        expected: left_ty.clone(),
                        found: right_ty.clone(),
                    },
                });
                Type::Infer
            }
        }

        // logical ops must be bool
        And | Or | Xor => {
            if left_ty.is_bool() && same_ty {
                Type::Bool
            } else {
                let wrong_ty = if !left_ty.is_bool() {
                    left_ty
                } else {
                    right_ty
                };
                errors.push(TypeErr {
                    span: bin.span,
                    kind: TypeErrKind::InvalidOperand {
                        op: node.op.to_string(),
                        operand_type: wrong_ty,
                    },
                });
                Type::Infer
            }
        }

        Coalesce => check_coalesce(bin, left_ty, right_ty, type_checker, errors),
    }
}

fn check_coalesce(
    bin: &BinaryNode,
    left_ty: Type,
    right_ty: Type,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<TypeErr>,
) -> Type {
    let node = &bin.node;

    // left must be optional
    let Type::Optional(left_inner) = left_ty.clone() else {
        errors.push(TypeErr {
            span: bin.span,
            kind: TypeErrKind::InvalidOperand {
                op: node.op.to_string(),
                operand_type: left_ty,
            },
        });
        return Type::Infer;
    };

    let right_ref = TypeRef::Expr(node.right.node.id);
    let left_inner_ty = *left_inner;

    // if right is optional too then we're chaining optionals
    if let Type::Optional(right_inner) = right_ty.clone() {
        // constrain the inner types if both are optional
        let left_inner_ref = TypeRef::Concrete(left_inner_ty.clone());
        let right_inner_ref = TypeRef::Concrete(*right_inner.clone());
        type_checker.constrain_equal(bin.span, left_inner_ref, right_inner_ref, errors);

        // get the unified inner type
        let unified_inner = type_checker
            .get_type_ref(&right_ref)
            .and_then(|t| {
                if let Type::Optional(inner) = t {
                    Some(*inner)
                } else {
                    None
                }
            })
            .unwrap_or(left_inner_ty.clone());

        // set the left expression's type to the unified inner type
        let ty = Type::Optional(Box::new(unified_inner));
        type_checker.set_type(node.left.node.id, ty.clone(), bin.span);

        return ty;
    }

    // if right side is not optional then we're unwrapping or returning the right side
    let left_inner_ref = TypeRef::Concrete(left_inner_ty.clone());
    type_checker.constrain_equal(bin.span, left_inner_ref, right_ref.clone(), errors);

    // get the unified inner type
    let unified_inner = type_checker
        .get_type_ref(&right_ref)
        .unwrap_or(left_inner_ty);

    // set the left expression's type to the unified inner type
    type_checker.set_type(
        node.left.node.id,
        Type::Optional(Box::new(unified_inner.clone())),
        bin.span,
    );

    unified_inner
}

fn check_unary(
    unary: &UnaryNode,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<TypeErr>,
) -> Type {
    let node = &unary.node;
    let expr_ty = check_expr(&node.expr, type_checker, errors);

    match node.op {
        UnaryOp::Neg if expr_ty.is_num() => expr_ty,
        UnaryOp::Not if expr_ty.is_bool() => Type::Bool,
        _ => {
            errors.push(TypeErr {
                span: unary.span,
                kind: TypeErrKind::InvalidOperand {
                    op: node.op.to_string(),
                    operand_type: expr_ty.clone(),
                },
            });
            Type::Infer
        }
    }
}

fn check_assign(
    assign: &AssignNode,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<TypeErr>,
) -> Type {
    let node = &assign.node;
    check_expr(&node.target, type_checker, errors);
    check_expr(&node.value, type_checker, errors);

    let target_ref = TypeRef::Expr(node.target.node.id);
    let value_ref = TypeRef::Expr(node.value.node.id);

    match node.op {
        AssignOp::Assign => check_assign_op(assign, target_ref, value_ref, type_checker, errors),
        AssignOp::AddAssign | AssignOp::SubAssign | AssignOp::MulAssign | AssignOp::DivAssign => {
            check_compound_assign_op(assign, target_ref, value_ref, type_checker, errors)
        }
    }
}

fn check_assign_op(
    assign: &AssignNode,
    target_ref: TypeRef,
    value_ref: TypeRef,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<TypeErr>,
) -> Type {
    type_checker.constrain_assignable(assign.span, value_ref, target_ref.clone(), errors);
    Type::Void
}

fn check_compound_assign_op(
    assign: &AssignNode,
    target_ref: TypeRef,
    value_ref: TypeRef,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<TypeErr>,
) -> Type {
    type_checker.constrain_equal(assign.span, target_ref.clone(), value_ref, errors);

    let target_ty = type_checker
        .get_type_ref(&target_ref)
        .unwrap_or(Type::Infer);

    let is_numeric = target_ty.is_num() || target_ty.is_infer();
    if !is_numeric {
        errors.push(TypeErr {
            span: assign.span,
            kind: TypeErrKind::InvalidOperand {
                op: assign.node.op.to_string(),
                operand_type: target_ty.clone(),
            },
        });
    }

    Type::Void
}

fn check_if(if_node: &IfNode, type_checker: &mut TypeChecker, errors: &mut Vec<TypeErr>) -> Type {
    let node = &if_node.node;

    let cond_ty = check_expr(&node.cond, type_checker, errors);
    let maybe_bool = cond_ty.is_bool() || cond_ty.is_infer();
    if !maybe_bool {
        errors.push(TypeErr {
            span: node.cond.span,
            kind: TypeErrKind::IfConditionNotBool { found: cond_ty },
        });
    }

    let (then_ty, _) = check_block_expr(&node.then_block, type_checker, errors);

    // if there is no else block then the type is void and this must be a statment
    let Some(else_block) = &node.else_block else {
        return Type::Void;
    };

    let (else_ty, _) = check_block_expr(else_block, type_checker, errors);

    // unify branch types
    let same_branch_ty = then_ty == else_ty;
    if !same_branch_ty {
        let is_unifiable = then_ty.is_infer() || else_ty.is_infer();
        if !is_unifiable {
            errors.push(TypeErr {
                span: if_node.span,
                kind: TypeErrKind::MismatchedTypes {
                    expected: then_ty.clone(),
                    found: else_ty.clone(),
                },
            });
            return Type::Infer;
        }
    }

    // return the type of the branch that is not inferred
    if then_ty.is_infer() { else_ty } else { then_ty }
}

fn check_tuple(
    elements: &[ExprNode],
    type_checker: &mut TypeChecker,
    errors: &mut Vec<TypeErr>,
) -> Type {
    let element_types: Vec<Type> = elements
        .iter()
        .map(|el| check_expr(el, type_checker, errors))
        .collect();
    Type::Tuple(element_types)
}

fn check_named_tuple(
    elements: &[(Ident, ExprNode)],
    span: Span,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<TypeErr>,
) -> Type {
    let mut seen_labels = HashSet::new();
    let mut fields = Vec::with_capacity(elements.len());

    for (label, expr) in elements {
        let ty = check_expr(expr, type_checker, errors);

        let inserted = seen_labels.insert(*label);
        if !inserted {
            errors.push(TypeErr {
                span,
                kind: TypeErrKind::DuplicateTupleLabel { label: *label },
            });
        }

        fields.push((*label, ty));
    }

    Type::NamedTuple(fields)
}

fn check_struct_lit(
    lit_node: &StructLiteralNode,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<TypeErr>,
) -> Type {
    let lit = &lit_node.node;
    let struct_name = lit.name;

    let Some(struct_def) = type_checker.get_struct(struct_name).cloned() else {
        errors.push(TypeErr {
            span: lit_node.span,
            kind: TypeErrKind::UnknownStruct { name: struct_name },
        });
        return Type::Infer;
    };

    let is_generic = !struct_def.type_params.is_empty();
    let slots = is_generic
        .then(|| {
            let call_id = type_checker.next_call_id();
            create_inference_slots(&struct_def.type_params, type_checker, call_id)
        })
        .unwrap_or_default();

    let mut seen_fields = HashSet::new();
    let mut provided_fields = HashMap::new();

    for (field_name, field_expr) in &lit.fields {
        let field_ty = check_expr(field_expr, type_checker, errors);

        let is_new = seen_fields.insert(*field_name);
        if !is_new {
            errors.push(TypeErr {
                span: field_expr.span,
                kind: TypeErrKind::StructDuplicateField {
                    struct_name,
                    field: *field_name,
                },
            });
            continue;
        }

        let expected_field = struct_def.fields.iter().find(|f| f.name == *field_name);
        let Some(expected) = expected_field else {
            errors.push(TypeErr {
                span: field_expr.span,
                kind: TypeErrKind::StructUnknownField {
                    struct_name,
                    field: *field_name,
                },
            });
            continue;
        };

        let field_ref = TypeRef::Expr(field_expr.node.id);
        let expected_ref = if is_generic {
            type_to_ref_with_inference(&expected.ty, &slots)
        } else {
            let resolved = type_checker.resolve_type(&expected.ty);
            TypeRef::Concrete(resolved)
        };
        type_checker.constrain_assignable(field_expr.span, field_ref, expected_ref, errors);

        provided_fields.insert(*field_name, field_ty);
    }

    for struct_field in &struct_def.fields {
        let contains_field = provided_fields.contains_key(&struct_field.name);
        if !contains_field {
            errors.push(TypeErr {
                span: lit_node.span,
                kind: TypeErrKind::StructMissingField {
                    struct_name,
                    field: struct_field.name,
                },
            });
        }
    }

    let type_args = is_generic
        .then(|| {
            struct_def
                .type_params
                .iter()
                .map(|param| {
                    let slot_name = slots.get(&param.id).expect("slot exists");
                    type_checker
                        .get_var(*slot_name)
                        .cloned()
                        .unwrap_or(Type::Infer)
                })
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();

    Type::Struct {
        name: struct_name,
        type_args,
    }
}

fn check_tuple_index(
    index_node: &TupleIndexNode,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<TypeErr>,
) -> Type {
    let node = &index_node.node;
    let target_ty = check_expr(&node.target, type_checker, errors);
    let index = node.index;

    let Some(element_types) = target_ty.tuple_element_types() else {
        if matches!(target_ty, Type::Infer) {
            return Type::Infer;
        }

        errors.push(TypeErr {
            span: index_node.span,
            kind: TypeErrKind::TupleIndexOnNonTuple {
                found: target_ty.clone(),
                index,
            },
        });
        return Type::Infer;
    };

    let len = element_types.len();
    let is_in_bounds = (index as usize) < len;
    if is_in_bounds {
        element_types[index as usize].clone()
    } else {
        errors.push(TypeErr {
            span: index_node.span,
            kind: TypeErrKind::TupleIndexOutOfBounds {
                tuple_type: target_ty.clone(),
                index,
                len,
            },
        });
        Type::Infer
    }
}

fn check_field_access(
    field_node: &FieldAccessNode,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<TypeErr>,
) -> Type {
    let node = &field_node.node;
    let target_ty = check_expr(&node.target, type_checker, errors);
    let field = node.field;

    match &target_ty {
        Type::NamedTuple(fields) => {
            for (label, ty) in fields {
                if *label == field {
                    return ty.clone();
                }
            }
            errors.push(TypeErr {
                span: field_node.span,
                kind: TypeErrKind::NoSuchFieldOnTuple {
                    field,
                    tuple_type: target_ty.clone(),
                },
            });
            Type::Infer
        }
        Type::Struct {
            name: struct_name,
            type_args,
        } => {
            let Some(struct_def) = type_checker.get_struct(*struct_name).cloned() else {
                errors.push(TypeErr {
                    span: field_node.span,
                    kind: TypeErrKind::UnknownStruct { name: *struct_name },
                });
                return Type::Infer;
            };

            let subst = struct_def
                .type_params
                .iter()
                .zip(type_args.iter())
                .map(|(param, arg)| (param.id, arg.clone()))
                .collect::<HashMap<_, _>>();

            for struct_field in &struct_def.fields {
                if struct_field.name == field {
                    let field_ty = subst_type(&struct_field.ty, &subst);
                    return type_checker.resolve_type(&field_ty);
                }
            }

            errors.push(TypeErr {
                span: field_node.span,
                kind: TypeErrKind::StructUnknownField {
                    struct_name: *struct_name,
                    field,
                },
            });
            Type::Infer
        }
        Type::Infer => Type::Infer,
        _ => {
            errors.push(TypeErr {
                span: field_node.span,
                kind: TypeErrKind::FieldAccessOnNonNamedTuple {
                    field,
                    found: target_ty.clone(),
                },
            });
            Type::Infer
        }
    }
}

fn check_binding(binding: &BindingNode, type_checker: &mut TypeChecker, errors: &mut Vec<TypeErr>) {
    let node = &binding.node;
    check_expr(&node.value, type_checker, errors);

    if is_if_without_else(&node.value) {
        errors.push(TypeErr {
            span: node.value.span,
            kind: TypeErrKind::IfMissingElse,
        });
    }

    let val_ref = TypeRef::Expr(node.value.node.id);

    let value_ty = type_checker
        .get_type(node.value.node.id)
        .map(|(_, ty)| ty.clone())
        .unwrap_or(Type::Infer);

    let binding_ty = match &node.ty {
        Some(annot_ty) => {
            let annot_ref = TypeRef::Concrete(annot_ty.clone());
            type_checker.constrain_assignable(binding.span, val_ref, annot_ref, errors);
            annot_ty.clone()
        }
        None => value_ty,
    };

    check_pattern(&node.pattern, &binding_ty, type_checker, errors);
}

fn check_pattern(
    pattern: &PatternNode,
    value_ty: &Type,
    type_checker: &mut TypeChecker,
    errors: &mut Vec<TypeErr>,
) {
    match &pattern.node {
        Pattern::Ident(name) => {
            type_checker.set_var(*name, value_ty.clone());
        }
        Pattern::Wildcard => {}
        Pattern::Tuple(subpatterns) => {
            let Some(elem_types) = value_ty.tuple_element_types() else {
                errors.push(TypeErr {
                    span: pattern.span,
                    kind: TypeErrKind::NonTupleInTuplePattern {
                        found: value_ty.clone(),
                        pattern_arity: subpatterns.len(),
                    },
                });
                return;
            };

            let same_arity = subpatterns.len() == elem_types.len();
            if !same_arity {
                errors.push(TypeErr {
                    span: pattern.span,
                    kind: TypeErrKind::TuplePatternArityMismatch {
                        expected: elem_types.len(),
                        found: subpatterns.len(),
                    },
                });
                return;
            }

            for (subpat, elem_ty) in subpatterns.iter().zip(elem_types.iter()) {
                check_pattern(subpat, elem_ty, type_checker, errors);
            }
        }
        Pattern::NamedTuple(elems) => {
            let Some(elem_types) = value_ty.tuple_element_types() else {
                errors.push(TypeErr {
                    span: pattern.span,
                    kind: TypeErrKind::NonTupleInTuplePattern {
                        found: value_ty.clone(),
                        pattern_arity: elems.len(),
                    },
                });
                return;
            };

            let value_labels: Option<Vec<Ident>> = match value_ty {
                Type::NamedTuple(fields) => Some(fields.iter().map(|(name, _)| *name).collect()),
                _ => None,
            };

            let same_arity = elems.len() == elem_types.len();
            if !same_arity {
                errors.push(TypeErr {
                    span: pattern.span,
                    kind: TypeErrKind::TuplePatternArityMismatch {
                        expected: elem_types.len(),
                        found: elems.len(),
                    },
                });
                return;
            }

            let Some(labels) = value_labels else {
                errors.push(TypeErr {
                    span: pattern.span,
                    kind: TypeErrKind::NamedPatternOnPositionalTuple,
                });
                return;
            };

            for ((pat_label, _), ty_label) in elems.iter().zip(labels.iter()) {
                if *pat_label != *ty_label {
                    errors.push(TypeErr {
                        span: pattern.span,
                        kind: TypeErrKind::TuplePatternLabelMismatch {
                            expected: *ty_label,
                            found: *pat_label,
                        },
                    });
                }
            }

            for ((_, subpat), elem_ty) in elems.iter().zip(elem_types.iter()) {
                check_pattern(subpat, elem_ty, type_checker, errors);
            }
        }
    }
}

fn is_if_without_else(expr: &ExprNode) -> bool {
    match &expr.node.kind {
        ExprKind::If(if_node) => if_node.node.else_block.is_none(),
        _ => false,
    }
}

fn check_ret(ret: &ReturnNode, type_checker: &mut TypeChecker, errors: &mut Vec<TypeErr>) {
    let node = &ret.node;

    // if return is outside a function then we just return (although this shouldn't happen)
    let Some(expected_ret) = type_checker.current_return_type().cloned() else {
        return;
    };

    type_checker.mark_explicit_return();

    match (&node.value, &expected_ret) {
        // returning a value in a non-void fn needs constraining
        (Some(value_expr), expected_ty) => {
            check_expr(value_expr, type_checker, errors);
            let expr_ref = TypeRef::Expr(value_expr.node.id);
            let ret_ref = TypeRef::Concrete(expected_ty.clone());
            type_checker.constrain_assignable(ret.span, expr_ref, ret_ref, errors);
        }

        // returning nothing in a void fn is fine
        (None, Type::Void) => {}

        // returning nothing in a non-void fn is invalid
        (None, expected_ty) => {
            errors.push(TypeErr {
                span: ret.span,
                kind: TypeErrKind::MismatchedTypes {
                    expected: expected_ty.clone(),
                    found: Type::Void,
                },
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ast::{
            Assign, Binary, Binding, Block, BlockNode, Call, Expr, Func, Mutability, Param,
            Pattern, PatternNode, Return, TypeParam, TypeVarId, Unary, Visibility,
        },
        span::Span,
    };
    use internment::Intern;
    use std::cell::Cell;

    thread_local! {
        static EXPR_ID_COUNTER: Cell<u64> = Cell::new(0);
    }

    fn dummy_span() -> Span {
        Span::new(0, 0)
    }

    fn dummy_ident(s: &str) -> Ident {
        Ident(Intern::new(s.to_string()))
    }

    // reset the expression id counter for deterministic test ids
    fn reset_expr_ids() {
        EXPR_ID_COUNTER.with(|counter| counter.set(0));
    }

    fn next_expr_id() -> ExprId {
        EXPR_ID_COUNTER.with(|counter| {
            let id = counter.get();
            counter.set(id + 1);
            ExprId(id)
        })
    }

    // ---- ast builder helpers ----
    fn lit_int(val: i64) -> ExprNode {
        ExprNode {
            node: Expr::new(ExprKind::Lit(Lit::Int(val)), next_expr_id()),
            span: dummy_span(),
        }
    }

    fn lit_float(val: f64) -> ExprNode {
        ExprNode {
            node: Expr::new(ExprKind::Lit(Lit::Float(val)), next_expr_id()),
            span: dummy_span(),
        }
    }

    fn lit_bool(val: bool) -> ExprNode {
        ExprNode {
            node: Expr::new(ExprKind::Lit(Lit::Bool(val)), next_expr_id()),
            span: dummy_span(),
        }
    }

    fn lit_string(val: &str) -> ExprNode {
        ExprNode {
            node: Expr::new(ExprKind::Lit(Lit::String(val.to_string())), next_expr_id()),
            span: dummy_span(),
        }
    }

    fn lit_nil() -> ExprNode {
        ExprNode {
            node: Expr::new(ExprKind::Lit(Lit::Nil), next_expr_id()),
            span: dummy_span(),
        }
    }

    fn ident_expr(name: &str) -> ExprNode {
        ExprNode {
            node: Expr::new(ExprKind::Ident(dummy_ident(name)), next_expr_id()),
            span: dummy_span(),
        }
    }

    fn binary_expr(left: ExprNode, op: BinaryOp, right: ExprNode) -> ExprNode {
        ExprNode {
            node: Expr::new(
                ExprKind::Binary(BinaryNode {
                    node: Binary {
                        left: Box::new(left),
                        op,
                        right: Box::new(right),
                    },
                    span: dummy_span(),
                }),
                next_expr_id(),
            ),
            span: dummy_span(),
        }
    }

    fn unary_expr(op: UnaryOp, expr: ExprNode) -> ExprNode {
        ExprNode {
            node: Expr::new(
                ExprKind::Unary(UnaryNode {
                    node: Unary {
                        op,
                        expr: Box::new(expr),
                    },
                    span: dummy_span(),
                }),
                next_expr_id(),
            ),
            span: dummy_span(),
        }
    }

    fn call_expr(func: ExprNode, args: Vec<ExprNode>) -> ExprNode {
        ExprNode {
            node: Expr::new(
                ExprKind::Call(CallNode {
                    node: Call {
                        func: Box::new(func),
                        args,
                        type_args: vec![],
                    },
                    span: dummy_span(),
                }),
                next_expr_id(),
            ),
            span: dummy_span(),
        }
    }

    fn assign_expr(target: ExprNode, op: AssignOp, value: ExprNode) -> ExprNode {
        ExprNode {
            node: Expr::new(
                ExprKind::Assign(AssignNode {
                    node: Assign {
                        target: Box::new(target),
                        op,
                        value: Box::new(value),
                    },
                    span: dummy_span(),
                }),
                next_expr_id(),
            ),
            span: dummy_span(),
        }
    }

    fn dummy_pattern(name: &str) -> PatternNode {
        PatternNode {
            node: Pattern::Ident(dummy_ident(name)),
            span: dummy_span(),
        }
    }

    fn let_binding(name: &str, ty: Option<Type>, value: ExprNode) -> StmtNode {
        StmtNode {
            node: Stmt::Binding(BindingNode {
                node: Binding {
                    pattern: dummy_pattern(name),
                    ty,
                    mutability: Mutability::Immutable,
                    value,
                },
                span: dummy_span(),
            }),
            span: dummy_span(),
        }
    }

    fn var_binding(name: &str, ty: Option<Type>, value: ExprNode) -> StmtNode {
        StmtNode {
            node: Stmt::Binding(BindingNode {
                node: Binding {
                    pattern: dummy_pattern(name),
                    ty,
                    mutability: Mutability::Mutable,
                    value,
                },
                span: dummy_span(),
            }),
            span: dummy_span(),
        }
    }

    fn fn_decl(name: &str, params: Vec<(&str, Type)>, ret: Type, body: Vec<StmtNode>) -> StmtNode {
        StmtNode {
            node: Stmt::Func(FuncNode {
                node: Func {
                    name: dummy_ident(name),
                    visibility: Visibility::Private,
                    type_params: vec![],
                    params: params
                        .into_iter()
                        .map(|(n, t)| Param {
                            name: dummy_ident(n),
                            ty: t,
                        })
                        .collect(),
                    ret,
                    body: BlockNode {
                        node: Block { stmts: body },
                        span: dummy_span(),
                    },
                },
                span: dummy_span(),
            }),
            span: dummy_span(),
        }
    }

    fn return_stmt(value: Option<ExprNode>) -> StmtNode {
        StmtNode {
            node: Stmt::Return(ReturnNode {
                node: Return { value },
                span: dummy_span(),
            }),
            span: dummy_span(),
        }
    }

    fn expr_stmt(expr: ExprNode) -> StmtNode {
        StmtNode {
            node: Stmt::Expr(expr),
            span: dummy_span(),
        }
    }

    fn program(stmts: Vec<StmtNode>) -> Program {
        Program { stmts }
    }

    // ---- runner helpers ----
    #[track_caller]
    fn run_ok(prog: Program) -> TypeChecker {
        match check_program(&prog) {
            Ok(tcx) => tcx,
            Err(errors) => {
                panic!("Expected Ok, got errors: {:?}", errors);
            }
        }
    }

    #[track_caller]
    fn run_err(prog: Program) -> Vec<TypeErr> {
        match check_program(&prog) {
            Ok(_) => panic!("Expected Err, got Ok"),
            Err(errors) => errors,
        }
    }

    // ---- assertion helpers ----
    #[track_caller]
    fn assert_expr_type(tcx: &TypeChecker, id: ExprId, expected: Type) {
        match tcx.get_type(id) {
            Some((_, ty)) => assert_eq!(
                *ty, expected,
                "Expression {:?} has wrong type. Expected {:?}, got {:?}",
                id, expected, ty
            ),
            None => panic!("Expression {:?} not found in type map", id),
        }
    }

    fn get_expr_id(expr: &ExprNode) -> ExprId {
        expr.node.id
    }

    #[test]
    fn test_unify_primitives() {
        let span = dummy_span();
        let mut errors = vec![];

        // int unifies with int
        let result = unify_types(&Type::Int, &Type::Int, span, &mut errors);
        assert_eq!(result, Some(Type::Int));
        assert_eq!(errors.len(), 0);

        // float unifies with float
        let result = unify_types(&Type::Float, &Type::Float, span, &mut errors);
        assert_eq!(result, Some(Type::Float));
        assert_eq!(errors.len(), 0);

        // bool unifies with bool
        let result = unify_types(&Type::Bool, &Type::Bool, span, &mut errors);
        assert_eq!(result, Some(Type::Bool));
        assert_eq!(errors.len(), 0);

        // string unifies with string
        let result = unify_types(&Type::String, &Type::String, span, &mut errors);
        assert_eq!(result, Some(Type::String));
        assert_eq!(errors.len(), 0);

        // void unifies with void
        let result = unify_types(&Type::Void, &Type::Void, span, &mut errors);
        assert_eq!(result, Some(Type::Void));
        assert_eq!(errors.len(), 0);
    }

    #[test]
    fn test_unify_infer_with_concrete() {
        let span = dummy_span();
        let mut errors = vec![];

        // infer unifies with int (both directions)
        let result = unify_types(&Type::Infer, &Type::Int, span, &mut errors);
        assert_eq!(result, Some(Type::Int));
        assert_eq!(errors.len(), 0);

        let result = unify_types(&Type::Int, &Type::Infer, span, &mut errors);
        assert_eq!(result, Some(Type::Int));
        assert_eq!(errors.len(), 0);

        // infer unifies with optional(int)
        let result = unify_types(
            &Type::Infer,
            &Type::Optional(Box::new(Type::Int)),
            span,
            &mut errors,
        );
        assert_eq!(result, Some(Type::Optional(Box::new(Type::Int))));
        assert_eq!(errors.len(), 0);
    }

    #[test]
    fn test_unify_optional() {
        let span = dummy_span();
        let mut errors = vec![];

        // int? unifies with int?
        let result = unify_types(
            &Type::Optional(Box::new(Type::Int)),
            &Type::Optional(Box::new(Type::Int)),
            span,
            &mut errors,
        );
        assert_eq!(result, Some(Type::Optional(Box::new(Type::Int))));
        assert_eq!(errors.len(), 0);

        // infer? unifies with string?
        let result = unify_types(
            &Type::Optional(Box::new(Type::Infer)),
            &Type::Optional(Box::new(Type::String)),
            span,
            &mut errors,
        );
        assert_eq!(result, Some(Type::Optional(Box::new(Type::String))));
        assert_eq!(errors.len(), 0);
    }

    #[test]
    fn test_unify_function_types() {
        let span = dummy_span();
        let mut errors = vec![];

        // fn(int, bool) -> float unifies with identical signature
        let func_type = Type::Func {
            params: vec![Type::Int, Type::Bool],
            ret: Box::new(Type::Float),
        };
        let result = unify_types(&func_type, &func_type, span, &mut errors);
        assert_eq!(result, Some(func_type.clone()));
        assert_eq!(errors.len(), 0);

        // parameter length mismatch produces error
        let func1 = Type::Func {
            params: vec![Type::Int],
            ret: Box::new(Type::Void),
        };
        let func2 = Type::Func {
            params: vec![Type::Int, Type::Bool],
            ret: Box::new(Type::Void),
        };
        let result = unify_types(&func1, &func2, span, &mut errors);
        assert_eq!(result, None);
        assert_eq!(errors.len(), 1);
        assert!(matches!(
            &errors[0].kind,
            TypeErrKind::MismatchedTypes { .. }
        ));
    }

    #[test]
    fn test_unify_mismatched_types() {
        let span = dummy_span();
        let mut errors = vec![];

        // int vs bool produces error
        let result = unify_types(&Type::Int, &Type::Bool, span, &mut errors);
        assert_eq!(result, None);
        assert_eq!(errors.len(), 1);
        assert!(matches!(
            &errors[0].kind,
            TypeErrKind::MismatchedTypes { expected, found }
            if *expected == Type::Int && *found == Type::Bool
        ));

        // optional vs non-optional produces error
        errors.clear();
        let result = unify_types(
            &Type::Optional(Box::new(Type::Int)),
            &Type::Int,
            span,
            &mut errors,
        );
        assert_eq!(result, None);
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn test_binding_annotated_success() {
        reset_expr_ids();

        // let x: int = 1;
        let value_expr = lit_int(1);
        let value_id = get_expr_id(&value_expr);
        let prog = program(vec![let_binding("x", Some(Type::Int), value_expr)]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, value_id, Type::Int);
    }

    #[test]
    fn test_binding_annotated_mismatch() {
        reset_expr_ids();

        // let x: int = true;
        let prog = program(vec![let_binding("x", Some(Type::Int), lit_bool(true))]);

        let errors = run_err(prog);
        // should have at least one mismatched types error between int and bool
        assert!(!errors.is_empty());
        assert!(
            errors.iter().any(|e| matches!(
                &e.kind,
                TypeErrKind::MismatchedTypes { expected, found }
                if (*expected == Type::Int && *found == Type::Bool) ||
                   (*expected == Type::Bool && *found == Type::Int)
            )),
            "Expected MismatchedTypes error (int/bool mismatch), got: {:?}",
            errors
        );
    }

    #[test]
    fn test_binding_unannotated_simple_inference() {
        reset_expr_ids();

        // let x = 1;
        let value_expr = lit_int(1);
        let value_id = get_expr_id(&value_expr);
        let prog = program(vec![let_binding("x", None, value_expr)]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, value_id, Type::Int);
    }

    #[test]
    fn test_binding_unannotated_unresolved_infer() {
        reset_expr_ids();

        // let x = nil; (no other uses)
        let prog = program(vec![let_binding("x", None, lit_nil())]);

        let errors = run_err(prog);
        assert!(
            errors
                .iter()
                .any(|e| matches!(&e.kind, TypeErrKind::UnresolvedInfer))
        );
    }

    #[test]
    fn test_binding_chained_inference() {
        reset_expr_ids();

        // let x: int = 1; let y = x;
        // first binding needs type annotation so x is in scope for second binding
        let x_val_expr = lit_int(1);
        let x_val_id = get_expr_id(&x_val_expr);
        let y_val_expr = ident_expr("x");
        let y_val_id = get_expr_id(&y_val_expr);
        let prog = program(vec![
            let_binding("x", Some(Type::Int), x_val_expr),
            let_binding("y", None, y_val_expr),
        ]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, x_val_id, Type::Int);
        assert_expr_type(&tcx, y_val_id, Type::Int);
    }

    #[test]
    fn test_call_happy_path() {
        reset_expr_ids();
        // fn f(a: int, b: bool) -> string { return "ok"; }
        // f(1, true);
        let fn_def = fn_decl(
            "f",
            vec![("a", Type::Int), ("b", Type::Bool)],
            Type::String,
            vec![return_stmt(Some(lit_string("ok")))],
        );
        let call_expr_node = call_expr(ident_expr("f"), vec![lit_int(1), lit_bool(true)]);
        let call_id = get_expr_id(&call_expr_node);
        let prog = program(vec![fn_def, expr_stmt(call_expr_node)]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, call_id, Type::String);
    }

    #[test]
    fn test_call_arity_mismatch_too_few() {
        reset_expr_ids();
        // fn f(a: int, b: bool) -> string { return "ok"; }
        // f(1);
        let fn_def = fn_decl(
            "f",
            vec![("a", Type::Int), ("b", Type::Bool)],
            Type::String,
            vec![return_stmt(Some(lit_string("ok")))],
        );
        let prog = program(vec![
            fn_def,
            expr_stmt(call_expr(ident_expr("f"), vec![lit_int(1)])),
        ]);

        let errors = run_err(prog);
        assert!(
            errors
                .iter()
                .any(|e| matches!(&e.kind, TypeErrKind::MismatchedTypes { .. }))
        );
    }

    #[test]
    fn test_call_arity_mismatch_too_many() {
        reset_expr_ids();
        // fn f(a: int, b: bool) -> string { return "ok"; }
        // f(1, true, 3);
        let fn_def = fn_decl(
            "f",
            vec![("a", Type::Int), ("b", Type::Bool)],
            Type::String,
            vec![return_stmt(Some(lit_string("ok")))],
        );
        let prog = program(vec![
            fn_def,
            expr_stmt(call_expr(
                ident_expr("f"),
                vec![lit_int(1), lit_bool(true), lit_int(3)],
            )),
        ]);

        let errors = run_err(prog);
        assert!(
            errors
                .iter()
                .any(|e| matches!(&e.kind, TypeErrKind::MismatchedTypes { .. }))
        );
    }

    #[test]
    fn test_call_argument_type_mismatch() {
        reset_expr_ids();
        // fn f(a: int, b: bool) -> string { return "ok"; }
        // f("nope", true);
        let fn_def = fn_decl(
            "f",
            vec![("a", Type::Int), ("b", Type::Bool)],
            Type::String,
            vec![return_stmt(Some(lit_string("ok")))],
        );
        let prog = program(vec![
            fn_def,
            expr_stmt(call_expr(
                ident_expr("f"),
                vec![lit_string("nope"), lit_bool(true)],
            )),
        ]);

        let errors = run_err(prog);
        assert!(!errors.is_empty());
        assert!(
            errors.iter().any(|e| matches!(
                &e.kind,
                TypeErrKind::MismatchedTypes { expected, found }
                if (*expected == Type::Int && *found == Type::String) ||
                   (*expected == Type::String && *found == Type::Int)
            )),
            "Expected MismatchedTypes error (int/string mismatch), got: {:?}",
            errors
        );
    }

    #[test]
    fn test_return_void_function_ok() {
        reset_expr_ids();
        // fn main() { return; }
        let prog = program(vec![fn_decl(
            "main",
            vec![],
            Type::Void,
            vec![return_stmt(None)],
        )]);

        let _tcx = run_ok(prog);
    }

    #[test]
    fn test_return_void_function_returning_value() {
        reset_expr_ids();
        // fn main() { return 1; }
        let prog = program(vec![fn_decl(
            "main",
            vec![],
            Type::Void,
            vec![return_stmt(Some(lit_int(1)))],
        )]);

        let errors = run_err(prog);
        assert!(!errors.is_empty());
        assert!(
            errors.iter().any(|e| matches!(
                &e.kind,
                TypeErrKind::MismatchedTypes { expected, found }
                if (*expected == Type::Void && *found == Type::Int) ||
                   (*expected == Type::Int && *found == Type::Void)
            )),
            "Expected MismatchedTypes error (void/int mismatch), got: {:?}",
            errors
        );
    }

    #[test]
    fn test_return_non_void_function_correct() {
        reset_expr_ids();
        // fn f() -> int { return 1; }
        let value_expr = lit_int(1);
        let value_id = get_expr_id(&value_expr);
        let prog = program(vec![fn_decl(
            "f",
            vec![],
            Type::Int,
            vec![return_stmt(Some(value_expr))],
        )]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, value_id, Type::Int);
    }

    #[test]
    fn test_return_non_void_wrong_type() {
        reset_expr_ids();
        // fn f() -> int { return true; }
        let prog = program(vec![fn_decl(
            "f",
            vec![],
            Type::Int,
            vec![return_stmt(Some(lit_bool(true)))],
        )]);

        let errors = run_err(prog);
        assert!(!errors.is_empty());
        assert!(
            errors.iter().any(|e| matches!(
                &e.kind,
                TypeErrKind::MismatchedTypes { expected, found }
                if (*expected == Type::Int && *found == Type::Bool) ||
                   (*expected == Type::Bool && *found == Type::Int)
            )),
            "Expected MismatchedTypes error (int/bool mismatch), got: {:?}",
            errors
        );
    }

    #[test]
    fn test_return_non_void_without_value() {
        reset_expr_ids();
        // fn f() -> int { return; }
        let prog = program(vec![fn_decl(
            "f",
            vec![],
            Type::Int,
            vec![return_stmt(None)],
        )]);

        let errors = run_err(prog);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrKind::MismatchedTypes { expected, found }
            if *expected == Type::Int && *found == Type::Void
        )));
    }

    #[test]
    fn test_coalesce_optional_with_concrete_fallback() {
        reset_expr_ids();
        // let a: int? = nil;
        // let x: int = a ?? 10;
        let a_expr = lit_nil();
        let a_binding = let_binding("a", Some(Type::Optional(Box::new(Type::Int))), a_expr);
        let coalesce_expr = binary_expr(ident_expr("a"), BinaryOp::Coalesce, lit_int(10));
        let coalesce_id = get_expr_id(&coalesce_expr);
        let x_binding = let_binding("x", Some(Type::Int), coalesce_expr);
        let prog = program(vec![a_binding, x_binding]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, coalesce_id, Type::Int);
    }

    #[test]
    fn test_coalesce_non_optional_left_error() {
        reset_expr_ids();
        // let x = 10 ?? 20;
        let coalesce_expr = binary_expr(lit_int(10), BinaryOp::Coalesce, lit_int(20));
        let prog = program(vec![let_binding("x", None, coalesce_expr)]);

        let errors = run_err(prog);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrKind::InvalidOperand { op, operand_type }
            if op == "??" && *operand_type == Type::Int
        )));
    }

    #[test]
    fn test_coalesce_mismatched_types() {
        reset_expr_ids();
        // let x: int? = nil;
        // let y = x ?? "s"; // int? ?? string should error
        let x_binding = let_binding("x", Some(Type::Optional(Box::new(Type::Int))), lit_nil());
        let coalesce_expr = binary_expr(ident_expr("x"), BinaryOp::Coalesce, lit_string("s"));
        let y_binding = let_binding("y", None, coalesce_expr);
        let prog = program(vec![x_binding, y_binding]);

        let errors = run_err(prog);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrKind::MismatchedTypes { expected, found }
            if *expected == Type::Int && *found == Type::String
        )));
    }

    #[test]
    fn test_assignment_plain_ok() {
        reset_expr_ids();
        // var x: int = 1; x = 2;
        let assign_expr = assign_expr(ident_expr("x"), AssignOp::Assign, lit_int(2));
        let assign_id = get_expr_id(&assign_expr);
        let prog = program(vec![
            var_binding("x", Some(Type::Int), lit_int(1)),
            expr_stmt(assign_expr),
        ]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, assign_id, Type::Void);
    }

    #[test]
    fn test_assignment_plain_mismatch() {
        reset_expr_ids();
        // var x: int = 1; x = true;
        let prog = program(vec![
            var_binding("x", Some(Type::Int), lit_int(1)),
            expr_stmt(assign_expr(
                ident_expr("x"),
                AssignOp::Assign,
                lit_bool(true),
            )),
        ]);

        let errors = run_err(prog);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrKind::MismatchedTypes { expected, found }
            if *expected == Type::Int && *found == Type::Bool
        )));
    }

    #[test]
    fn test_assignment_compound_ok() {
        reset_expr_ids();
        // var x: int = 1; x += 2;
        let assign_expr = assign_expr(ident_expr("x"), AssignOp::AddAssign, lit_int(2));
        let assign_id = get_expr_id(&assign_expr);
        let prog = program(vec![
            var_binding("x", Some(Type::Int), lit_int(1)),
            expr_stmt(assign_expr),
        ]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, assign_id, Type::Void);
    }

    #[test]
    fn test_assignment_compound_non_numeric() {
        reset_expr_ids();
        // var x: string = "a"; x += 1;
        let prog = program(vec![
            var_binding("x", Some(Type::String), lit_string("a")),
            expr_stmt(assign_expr(
                ident_expr("x"),
                AssignOp::AddAssign,
                lit_int(1),
            )),
        ]);

        let errors = run_err(prog);
        // should get either InvalidOperand or MismatchedTypes
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrKind::InvalidOperand { .. } | TypeErrKind::MismatchedTypes { .. }
        )));
    }

    #[test]
    fn test_binary_arithmetic_int() {
        reset_expr_ids();
        // 1 + 2
        let expr = binary_expr(lit_int(1), BinaryOp::Add, lit_int(2));
        let expr_id = get_expr_id(&expr);
        let prog = program(vec![expr_stmt(expr)]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, expr_id, Type::Int);
    }

    #[test]
    fn test_binary_arithmetic_float() {
        reset_expr_ids();
        // 1.0 + 2.0
        let expr = binary_expr(lit_float(1.0), BinaryOp::Add, lit_float(2.0));
        let expr_id = get_expr_id(&expr);
        let prog = program(vec![expr_stmt(expr)]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, expr_id, Type::Float);
    }

    #[test]
    fn test_binary_arithmetic_mismatch() {
        reset_expr_ids();
        // 1 + true
        let prog = program(vec![expr_stmt(binary_expr(
            lit_int(1),
            BinaryOp::Add,
            lit_bool(true),
        ))]);

        let errors = run_err(prog);
        assert!(
            errors
                .iter()
                .any(|e| matches!(&e.kind, TypeErrKind::MismatchedTypes { .. }))
        );
    }

    #[test]
    fn test_binary_logical_ok() {
        reset_expr_ids();
        // true && false
        let expr = binary_expr(lit_bool(true), BinaryOp::And, lit_bool(false));
        let expr_id = get_expr_id(&expr);
        let prog = program(vec![expr_stmt(expr)]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, expr_id, Type::Bool);
    }

    #[test]
    fn test_binary_logical_invalid_operand() {
        reset_expr_ids();
        // 1 && 2
        let prog = program(vec![expr_stmt(binary_expr(
            lit_int(1),
            BinaryOp::And,
            lit_int(2),
        ))]);

        let errors = run_err(prog);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrKind::InvalidOperand { operand_type, .. }
            if *operand_type == Type::Int
        )));
    }

    #[test]
    fn test_binary_comparison_ok() {
        reset_expr_ids();
        // 1 < 2
        let expr = binary_expr(lit_int(1), BinaryOp::LessThan, lit_int(2));
        let expr_id = get_expr_id(&expr);
        let prog = program(vec![expr_stmt(expr)]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, expr_id, Type::Bool);
    }

    #[test]
    fn test_binary_comparison_mismatch() {
        reset_expr_ids();
        // 1 < true
        let prog = program(vec![expr_stmt(binary_expr(
            lit_int(1),
            BinaryOp::LessThan,
            lit_bool(true),
        ))]);

        let errors = run_err(prog);
        assert!(
            errors
                .iter()
                .any(|e| matches!(&e.kind, TypeErrKind::MismatchedTypes { .. }))
        );
    }

    #[test]
    fn test_unary_neg_int() {
        reset_expr_ids();
        // -1
        let expr = unary_expr(UnaryOp::Neg, lit_int(1));
        let expr_id = get_expr_id(&expr);
        let prog = program(vec![expr_stmt(expr)]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, expr_id, Type::Int);
    }

    #[test]
    fn test_unary_neg_float() {
        reset_expr_ids();
        // -1.0
        let expr = unary_expr(UnaryOp::Neg, lit_float(1.0));
        let expr_id = get_expr_id(&expr);
        let prog = program(vec![expr_stmt(expr)]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, expr_id, Type::Float);
    }

    #[test]
    fn test_unary_not_bool() {
        reset_expr_ids();
        // !true
        let expr = unary_expr(UnaryOp::Not, lit_bool(true));
        let expr_id = get_expr_id(&expr);
        let prog = program(vec![expr_stmt(expr)]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, expr_id, Type::Bool);
    }

    #[test]
    fn test_unary_neg_invalid() {
        reset_expr_ids();
        // -true
        let prog = program(vec![expr_stmt(unary_expr(UnaryOp::Neg, lit_bool(true)))]);

        let errors = run_err(prog);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrKind::InvalidOperand { operand_type, .. }
            if *operand_type == Type::Bool
        )));
    }

    #[test]
    fn test_unary_not_invalid() {
        reset_expr_ids();
        // !1
        let prog = program(vec![expr_stmt(unary_expr(UnaryOp::Not, lit_int(1)))]);

        let errors = run_err(prog);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrKind::InvalidOperand { operand_type, .. }
            if *operand_type == Type::Int
        )));
    }

    #[test]
    fn test_constraint_chain_resolves() {
        reset_expr_ids();
        // let a: int? = nil; let b: int? = a;
        let a_expr = lit_nil();
        let a_id = get_expr_id(&a_expr);
        let b_expr = ident_expr("a");
        let b_id = get_expr_id(&b_expr);
        let prog = program(vec![
            let_binding("a", Some(Type::Optional(Box::new(Type::Int))), a_expr),
            let_binding("b", Some(Type::Optional(Box::new(Type::Int))), b_expr),
        ]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, a_id, Type::Optional(Box::new(Type::Int)));
        assert_expr_type(&tcx, b_id, Type::Optional(Box::new(Type::Int)));
    }

    #[test]
    fn test_leftover_infer() {
        reset_expr_ids();
        // let a = nil;
        let prog = program(vec![let_binding("a", None, lit_nil())]);

        let errors = run_err(prog);
        assert!(
            errors
                .iter()
                .any(|e| matches!(&e.kind, TypeErrKind::UnresolvedInfer))
        );
    }

    #[test]
    fn test_constraint_through_function_call() {
        reset_expr_ids();
        // fn id(x: int) -> int { return x; }
        // let a: int = id(1);
        let fn_def = fn_decl(
            "id",
            vec![("x", Type::Int)],
            Type::Int,
            vec![return_stmt(Some(ident_expr("x")))],
        );
        let a_val = call_expr(ident_expr("id"), vec![lit_int(1)]);
        let a_val_id = get_expr_id(&a_val);
        let prog = program(vec![fn_def, let_binding("a", Some(Type::Int), a_val)]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, a_val_id, Type::Int);
    }

    #[test]
    fn test_function_as_value() {
        reset_expr_ids();
        // fn f(a: int) -> int { return a; }
        // let g: fn(int) -> int = f;
        let fn_def = fn_decl(
            "f",
            vec![("a", Type::Int)],
            Type::Int,
            vec![return_stmt(Some(ident_expr("a")))],
        );
        let g_val = ident_expr("f");
        let g_val_id = get_expr_id(&g_val);
        let expected_fn_type = Type::Func {
            params: vec![Type::Int],
            ret: Box::new(Type::Int),
        };
        let prog = program(vec![
            fn_def,
            let_binding("g", Some(expected_fn_type.clone()), g_val),
        ]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, g_val_id, expected_fn_type);
    }

    #[test]
    fn test_function_call_through_variable() {
        reset_expr_ids();
        // fn f(a: int) -> int { return a; }
        // let g: fn(int) -> int = f;
        // g(42);
        let fn_def = fn_decl(
            "f",
            vec![("a", Type::Int)],
            Type::Int,
            vec![return_stmt(Some(ident_expr("a")))],
        );
        let fn_type = Type::Func {
            params: vec![Type::Int],
            ret: Box::new(Type::Int),
        };
        let g_binding = let_binding("g", Some(fn_type), ident_expr("f"));
        let call_expr_node = call_expr(ident_expr("g"), vec![lit_int(42)]);
        let call_id = get_expr_id(&call_expr_node);
        let prog = program(vec![fn_def, g_binding, expr_stmt(call_expr_node)]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, call_id, Type::Int);
    }

    #[test]
    fn test_nested_function_scope() {
        reset_expr_ids();
        // fn outer() -> int {
        //   fn inner() -> int { return 10; }
        //   return inner();
        // }
        let inner_fn = fn_decl(
            "inner",
            vec![],
            Type::Int,
            vec![return_stmt(Some(lit_int(10)))],
        );
        let call_inner = call_expr(ident_expr("inner"), vec![]);
        let outer_fn = fn_decl(
            "outer",
            vec![],
            Type::Int,
            vec![inner_fn, return_stmt(Some(call_inner))],
        );
        let prog = program(vec![outer_fn]);

        let _tcx = run_ok(prog);
    }

    #[test]
    fn test_function_forward_reference() {
        reset_expr_ids();
        // fn a() -> int { return b(); }
        // fn b() -> int { return 1; }
        let a_fn = fn_decl(
            "a",
            vec![],
            Type::Int,
            vec![return_stmt(Some(call_expr(ident_expr("b"), vec![])))],
        );
        let b_fn = fn_decl("b", vec![], Type::Int, vec![return_stmt(Some(lit_int(1)))]);
        let prog = program(vec![a_fn, b_fn]);

        let _tcx = run_ok(prog);
    }

    #[test]
    fn test_function_mutual_recursion() {
        reset_expr_ids();
        // fn even(n: int) -> bool {
        //   return odd(n);
        // }
        // fn odd(n: int) -> bool {
        //   return even(n);
        // }
        let even_fn = fn_decl(
            "even",
            vec![("n", Type::Int)],
            Type::Bool,
            vec![return_stmt(Some(call_expr(
                ident_expr("odd"),
                vec![ident_expr("n")],
            )))],
        );
        let odd_fn = fn_decl(
            "odd",
            vec![("n", Type::Int)],
            Type::Bool,
            vec![return_stmt(Some(call_expr(
                ident_expr("even"),
                vec![ident_expr("n")],
            )))],
        );
        let prog = program(vec![even_fn, odd_fn]);

        let _tcx = run_ok(prog);
    }

    #[test]
    fn test_assignability_int_to_optional_int() {
        reset_expr_ids();
        // let x: int? = 10;
        let value_expr = lit_int(10);
        let value_id = get_expr_id(&value_expr);
        let prog = program(vec![let_binding(
            "x",
            Some(Type::Optional(Box::new(Type::Int))),
            value_expr,
        )]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, value_id, Type::Int);
    }

    #[test]
    fn test_assignability_nil_to_optional_int() {
        reset_expr_ids();
        // let x: int? = nil;
        let value_expr = lit_nil();
        let value_id = get_expr_id(&value_expr);
        let prog = program(vec![let_binding(
            "x",
            Some(Type::Optional(Box::new(Type::Int))),
            value_expr,
        )]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, value_id, Type::Optional(Box::new(Type::Int)));
    }

    #[test]
    fn test_assignability_optional_to_non_optional_fails() {
        reset_expr_ids();
        // let a: int? = nil; let b: int = a;
        let a_expr = lit_nil();
        let b_expr = ident_expr("a");
        let prog = program(vec![
            let_binding("a", Some(Type::Optional(Box::new(Type::Int))), a_expr),
            let_binding("b", Some(Type::Int), b_expr),
        ]);

        let errors = run_err(prog);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrKind::MismatchedTypes { expected, found }
            if *expected == Type::Int && *found == Type::Optional(Box::new(Type::Int))
        )));
    }

    #[test]
    fn test_assignment_int_to_optional_var() {
        reset_expr_ids();
        // var x: int? = nil; x = 10;
        let nil_expr = lit_nil();
        let ten_expr = lit_int(10);
        let ten_id = get_expr_id(&ten_expr);
        let assign_expr = assign_expr(ident_expr("x"), AssignOp::Assign, ten_expr);
        let prog = program(vec![
            var_binding("x", Some(Type::Optional(Box::new(Type::Int))), nil_expr),
            expr_stmt(assign_expr),
        ]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, ten_id, Type::Int);
    }

    #[test]
    fn test_assignment_string_to_optional_string() {
        reset_expr_ids();
        // var c: string? = nil; c = "whatever";
        let nil_expr = lit_nil();
        let str_expr = lit_string("whatever");
        let str_id = get_expr_id(&str_expr);
        let assign_expr = assign_expr(ident_expr("c"), AssignOp::Assign, str_expr);
        let prog = program(vec![
            var_binding("c", Some(Type::Optional(Box::new(Type::String))), nil_expr),
            expr_stmt(assign_expr),
        ]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, str_id, Type::String);
    }

    #[test]
    fn test_assignment_float_to_optional_float() {
        reset_expr_ids();
        // var d: float? = 10.0; d = nil;
        let float_expr = lit_float(10.0);
        let nil_expr = lit_nil();
        let assign_expr = assign_expr(ident_expr("d"), AssignOp::Assign, nil_expr);
        let prog = program(vec![
            var_binding("d", Some(Type::Optional(Box::new(Type::Float))), float_expr),
            expr_stmt(assign_expr),
        ]);

        let _tcx = run_ok(prog);
    }

    #[test]
    fn test_coalesce_nil_with_int() {
        reset_expr_ids();
        // let a: int = nil ?? 10;
        let nil_expr = lit_nil();
        let nil_id = get_expr_id(&nil_expr);
        let ten_expr = lit_int(10);
        let coalesce_expr = binary_expr(nil_expr, BinaryOp::Coalesce, ten_expr);
        let coalesce_id = get_expr_id(&coalesce_expr);
        let prog = program(vec![let_binding("a", Some(Type::Int), coalesce_expr)]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, nil_id, Type::Optional(Box::new(Type::Int)));
        assert_expr_type(&tcx, coalesce_id, Type::Int);
    }

    #[test]
    fn test_coalesce_string_with_string_error() {
        reset_expr_ids();
        // let b = "nice" ?? "other";
        let nice_expr = lit_string("nice");
        let other_expr = lit_string("other");
        let coalesce_expr = binary_expr(nice_expr, BinaryOp::Coalesce, other_expr);
        let prog = program(vec![let_binding("b", None, coalesce_expr)]);

        let errors = run_err(prog);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrKind::InvalidOperand { op, operand_type }
            if op == "??" && *operand_type == Type::String
        )));
    }

    #[test]
    fn test_coalesce_mismatched_inner_types() {
        reset_expr_ids();
        // let a: int? = nil ?? true;
        let nil_expr = lit_nil();
        let bool_expr = lit_bool(true);
        let coalesce_expr = binary_expr(nil_expr, BinaryOp::Coalesce, bool_expr);
        let prog = program(vec![let_binding(
            "a",
            Some(Type::Optional(Box::new(Type::Int))),
            coalesce_expr,
        )]);

        let errors = run_err(prog);
        assert!(
            errors
                .iter()
                .any(|e| matches!(&e.kind, TypeErrKind::MismatchedTypes { .. }))
        );
    }

    #[test]
    fn test_coalesce_optional_string_with_string() {
        reset_expr_ids();
        // let a: string? = nil;
        // let b: string = a ?? "fallback";
        let a_expr = lit_nil();
        let a_binding = let_binding("a", Some(Type::Optional(Box::new(Type::String))), a_expr);
        let coalesce_expr =
            binary_expr(ident_expr("a"), BinaryOp::Coalesce, lit_string("fallback"));
        let coalesce_id = get_expr_id(&coalesce_expr);
        let b_binding = let_binding("b", Some(Type::String), coalesce_expr);
        let prog = program(vec![a_binding, b_binding]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, coalesce_id, Type::String);
    }

    #[test]
    fn test_coalesce_optional_int_with_float_error() {
        reset_expr_ids();
        // let a: int? = nil;
        // let b = a ?? 1.5;  // error: int? ?? float mismatch
        let a_binding = let_binding("a", Some(Type::Optional(Box::new(Type::Int))), lit_nil());
        let coalesce_expr = binary_expr(ident_expr("a"), BinaryOp::Coalesce, lit_float(1.5));
        let b_binding = let_binding("b", None, coalesce_expr);
        let prog = program(vec![a_binding, b_binding]);

        let errors = run_err(prog);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrKind::MismatchedTypes { expected, found }
            if *expected == Type::Int && *found == Type::Float
        )));
    }

    #[test]
    fn test_multiple_optional_assignments() {
        reset_expr_ids();
        // var e: int? = nil; e = 10;
        let nil_expr = lit_nil();
        let ten_expr = lit_int(10);
        let ten_id = get_expr_id(&ten_expr);
        let assign_expr = assign_expr(ident_expr("e"), AssignOp::Assign, ten_expr);
        let prog = program(vec![
            var_binding("e", Some(Type::Optional(Box::new(Type::Int))), nil_expr),
            expr_stmt(assign_expr),
        ]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, ten_id, Type::Int);
    }

    // ---- block expression helper ----
    fn block_expr(stmts: Vec<StmtNode>) -> ExprNode {
        ExprNode {
            node: Expr::new(
                ExprKind::Block(BlockNode {
                    node: Block { stmts },
                    span: dummy_span(),
                }),
                next_expr_id(),
            ),
            span: dummy_span(),
        }
    }

    // ---- block expression type tests ----
    #[test]
    fn test_block_expr_empty_is_void() {
        reset_expr_ids();
        // { }
        let block = block_expr(vec![]);
        let block_id = get_expr_id(&block);
        let prog = program(vec![fn_decl(
            "main",
            vec![],
            Type::Void,
            vec![expr_stmt(block)],
        )]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, block_id, Type::Void);
    }

    #[test]
    fn test_block_expr_trailing_int() {
        reset_expr_ids();
        // { 1 }
        let one = lit_int(1);
        let block = block_expr(vec![expr_stmt(one)]);
        let block_id = get_expr_id(&block);
        let prog = program(vec![fn_decl(
            "main",
            vec![],
            Type::Int,
            vec![expr_stmt(block)],
        )]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, block_id, Type::Int);
    }

    #[test]
    fn test_block_expr_let_then_ident() {
        reset_expr_ids();
        // { let x: int = 1; x }
        let x_val = lit_int(1);
        let x_binding = let_binding("x", Some(Type::Int), x_val);
        let x_ref = ident_expr("x");
        let x_ref_id = get_expr_id(&x_ref);
        let block = block_expr(vec![x_binding, expr_stmt(x_ref)]);
        let block_id = get_expr_id(&block);
        let prog = program(vec![fn_decl(
            "main",
            vec![],
            Type::Int,
            vec![expr_stmt(block)],
        )]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, block_id, Type::Int);
        assert_expr_type(&tcx, x_ref_id, Type::Int);
    }

    // ---- let-binding with block tests ----
    #[test]
    fn test_let_binding_block_infers_int() {
        reset_expr_ids();
        // let x = { 1 };
        let one = lit_int(1);
        let one_id = get_expr_id(&one);
        let block = block_expr(vec![expr_stmt(one)]);
        let block_id = get_expr_id(&block);
        let prog = program(vec![fn_decl(
            "main",
            vec![],
            Type::Void,
            vec![let_binding("x", None, block)],
        )]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, one_id, Type::Int);
        assert_expr_type(&tcx, block_id, Type::Int);
    }

    #[test]
    fn test_let_binding_block_annotated_int_ok() {
        reset_expr_ids();
        // let x: int = { 1 };
        let one = lit_int(1);
        let block = block_expr(vec![expr_stmt(one)]);
        let block_id = get_expr_id(&block);
        let prog = program(vec![fn_decl(
            "main",
            vec![],
            Type::Void,
            vec![let_binding("x", Some(Type::Int), block)],
        )]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, block_id, Type::Int);
    }

    #[test]
    fn test_let_binding_block_type_mismatch() {
        reset_expr_ids();
        // let x: string = { 1 };
        let one = lit_int(1);
        let block = block_expr(vec![expr_stmt(one)]);
        let prog = program(vec![fn_decl(
            "main",
            vec![],
            Type::Void,
            vec![let_binding("x", Some(Type::String), block)],
        )]);

        let errors = run_err(prog);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrKind::MismatchedTypes { expected, found }
            if *expected == Type::String && *found == Type::Int
        )));
    }

    // ---- implicit return function tests ----
    #[test]
    fn test_implicit_return_simple_int() {
        reset_expr_ids();
        // fn f() -> int { 1 }
        let one = lit_int(1);
        let one_id = get_expr_id(&one);
        let prog = program(vec![fn_decl("f", vec![], Type::Int, vec![expr_stmt(one)])]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, one_id, Type::Int);
    }

    #[test]
    fn test_implicit_return_let_then_ident() {
        reset_expr_ids();
        // fn f() -> int { let x: int = 1; x }
        let x_val = lit_int(1);
        let x_binding = let_binding("x", Some(Type::Int), x_val);
        let x_ref = ident_expr("x");
        let x_ref_id = get_expr_id(&x_ref);
        let prog = program(vec![fn_decl(
            "f",
            vec![],
            Type::Int,
            vec![x_binding, expr_stmt(x_ref)],
        )]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, x_ref_id, Type::Int);
    }

    #[test]
    fn test_explicit_return_still_works() {
        reset_expr_ids();
        // fn f() -> int { return 1; }
        let one = lit_int(1);
        let one_id = get_expr_id(&one);
        let prog = program(vec![fn_decl(
            "f",
            vec![],
            Type::Int,
            vec![return_stmt(Some(one))],
        )]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, one_id, Type::Int);
    }

    #[test]
    fn test_implicit_return_empty_body_non_void_error() {
        reset_expr_ids();
        // fn f() -> int { }
        let prog = program(vec![fn_decl("f", vec![], Type::Int, vec![])]);

        let errors = run_err(prog);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrKind::MismatchedTypes { expected, found }
            if *expected == Type::Int && *found == Type::Void
        )));
    }

    #[test]
    fn test_implicit_return_wrong_type_error() {
        reset_expr_ids();
        // fn f() -> int { true }
        let prog = program(vec![fn_decl(
            "f",
            vec![],
            Type::Int,
            vec![expr_stmt(lit_bool(true))],
        )]);

        let errors = run_err(prog);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrKind::MismatchedTypes { expected, found }
            if *expected == Type::Int && *found == Type::Bool
        )));
    }

    #[test]
    fn test_void_fn_trailing_value_error() {
        reset_expr_ids();
        // fn f() { 1 }
        // void functions should error on trailing non-void expressions
        let prog = program(vec![fn_decl(
            "f",
            vec![],
            Type::Void,
            vec![expr_stmt(lit_int(1))],
        )]);

        let errors = run_err(prog);
        assert!(errors.iter().any(|e| matches!(
            &e.kind,
            TypeErrKind::MismatchedTypes { expected, found }
            if *expected == Type::Void && *found == Type::Int
        )));
    }

    #[test]
    fn test_void_fn_empty_body_ok() {
        reset_expr_ids();
        // fn f() { }
        let prog = program(vec![fn_decl("f", vec![], Type::Void, vec![])]);

        let _tcx = run_ok(prog);
    }

    // ---- nested function / scope tests ----
    #[test]
    fn test_nested_fn_implicit_return() {
        reset_expr_ids();
        // fn outer() -> int {
        //   fn inner() -> int { 1 }
        //   inner()
        // }
        let inner_fn = fn_decl("inner", vec![], Type::Int, vec![expr_stmt(lit_int(1))]);
        let call_inner = call_expr(ident_expr("inner"), vec![]);
        let call_id = get_expr_id(&call_inner);
        let outer_fn = fn_decl(
            "outer",
            vec![],
            Type::Int,
            vec![inner_fn, expr_stmt(call_inner)],
        );
        let prog = program(vec![outer_fn]);

        let tcx = run_ok(prog);
        assert_expr_type(&tcx, call_id, Type::Int);
    }

    fn type_var(id: u32) -> Type {
        Type::Var(TypeVarId(id))
    }

    // ---- unification tests for type variables ----

    #[test]
    fn test_unify_same_type_var() {
        let span = dummy_span();
        let mut errors = vec![];

        // T unifies with T (same variable)
        let t = type_var(0);
        let result = unify_types(&t, &t, span, &mut errors);
        assert_eq!(result, Some(t.clone()));
        assert!(errors.is_empty());
    }

    #[test]
    fn test_unify_different_type_vars_error() {
        let span = dummy_span();
        let mut errors = vec![];

        // T and U are different type variables
        let t = type_var(0);
        let u = type_var(1);
        let result = unify_types(&t, &u, span, &mut errors);
        assert_eq!(result, None);
        assert_eq!(errors.len(), 1);
        assert!(matches!(
            &errors[0].kind,
            TypeErrKind::MismatchedTypes { expected, found }
            if *expected == t && *found == u
        ));
    }

    #[test]
    fn test_unify_type_var_with_concrete_error() {
        let span = dummy_span();
        let mut errors = vec![];

        // T and int are different types
        let t = type_var(0);
        let result = unify_types(&t, &Type::Int, span, &mut errors);
        assert_eq!(result, None);
        assert_eq!(errors.len(), 1);
        assert!(matches!(
            &errors[0].kind,
            TypeErrKind::MismatchedTypes { expected, found }
            if *expected == t && *found == Type::Int
        ));

        // int and T are different types
        errors.clear();
        let result = unify_types(&Type::Int, &t, span, &mut errors);
        assert_eq!(result, None);
        assert_eq!(errors.len(), 1);
        assert!(matches!(
            &errors[0].kind,
            TypeErrKind::MismatchedTypes { expected, found }
            if *expected == Type::Int && *found == t
        ));
    }

    #[test]
    fn test_unify_func_with_same_type_vars() {
        let span = dummy_span();
        let mut errors = vec![];

        // fn(T) -> T unifies with itself
        let t = type_var(0);
        let func_type = Type::Func {
            params: vec![t.clone()],
            ret: Box::new(t.clone()),
        };
        let result = unify_types(&func_type, &func_type, span, &mut errors);
        assert_eq!(result, Some(func_type.clone()));
        assert!(errors.is_empty());
    }

    #[test]
    fn test_unify_func_with_different_type_vars_error() {
        let span = dummy_span();
        let mut errors = vec![];

        // fn(T) -> T and fn(U) -> U are different functions
        let t = type_var(0);
        let u = type_var(1);
        let func_t = Type::Func {
            params: vec![t.clone()],
            ret: Box::new(t.clone()),
        };
        let func_u = Type::Func {
            params: vec![u.clone()],
            ret: Box::new(u.clone()),
        };
        let result = unify_types(&func_t, &func_u, span, &mut errors);
        assert_eq!(result, None);
        assert!(!errors.is_empty());
    }

    #[test]
    fn test_unify_optional_with_same_type_var() {
        let span = dummy_span();
        let mut errors = vec![];

        // T? unifies with T?
        let t = type_var(0);
        let opt_t = Type::Optional(Box::new(t.clone()));
        let result = unify_types(&opt_t, &opt_t, span, &mut errors);
        assert_eq!(result, Some(opt_t.clone()));
        assert!(errors.is_empty());
    }

    #[test]
    fn test_unify_optional_with_different_type_vars_error() {
        let span = dummy_span();
        let mut errors = vec![];

        // T? and U? are different optional types
        let t = type_var(0);
        let u = type_var(1);
        let opt_t = Type::Optional(Box::new(t.clone()));
        let opt_u = Type::Optional(Box::new(u.clone()));
        let result = unify_types(&opt_t, &opt_u, span, &mut errors);
        assert_eq!(result, None);
        assert!(!errors.is_empty());
    }

    // ---- assignability tests for type variables ----

    #[test]
    fn test_assignable_same_type_var() {
        // T is assignable to T
        let t = type_var(0);
        assert!(is_assignable(&t, &t));
    }

    #[test]
    fn test_assignable_different_type_vars() {
        // T is NOT assignable to U
        let t = type_var(0);
        let u = type_var(1);
        assert!(!is_assignable(&t, &u));
    }

    #[test]
    fn test_assignable_type_var_to_concrete() {
        // T is NOT assignable to int
        let t = type_var(0);
        assert!(!is_assignable(&t, &Type::Int));
    }

    #[test]
    fn test_assignable_concrete_to_type_var() {
        // int is NOT assignable to T
        let t = type_var(0);
        assert!(!is_assignable(&Type::Int, &t));
    }

    #[test]
    fn test_assignable_func_with_same_type_vars() {
        // fn(T) -> U is assignable to fn(T) -> U
        let t = type_var(0);
        let u = type_var(1);
        let func_type = Type::Func {
            params: vec![t.clone()],
            ret: Box::new(u.clone()),
        };
        assert!(is_assignable(&func_type, &func_type));
    }

    #[test]
    fn test_assignable_func_with_different_type_vars() {
        // fn(T) -> T is NOT assignable to fn(U) -> U
        let t = type_var(0);
        let u = type_var(1);
        let func_t = Type::Func {
            params: vec![t.clone()],
            ret: Box::new(t.clone()),
        };
        let func_u = Type::Func {
            params: vec![u.clone()],
            ret: Box::new(u.clone()),
        };
        assert!(!is_assignable(&func_t, &func_u));
    }

    #[test]
    fn test_assignable_optional_same_type_var() {
        // T? is assignable to T?
        let t = type_var(0);
        let opt_t = Type::Optional(Box::new(t.clone()));
        assert!(is_assignable(&opt_t, &opt_t));
    }

    #[test]
    fn test_assignable_optional_different_type_vars() {
        // T? is NOT assignable to U?
        let t = type_var(0);
        let u = type_var(1);
        let opt_t = Type::Optional(Box::new(t.clone()));
        let opt_u = Type::Optional(Box::new(u.clone()));
        assert!(!is_assignable(&opt_t, &opt_u));
    }

    // ---- contains_infer tests for type variables ----

    #[test]
    fn test_contains_infer_type_var_is_false() {
        // type variables are not considered as containing inference
        let t = type_var(0);
        assert!(!contains_infer(&t));
    }

    #[test]
    fn test_contains_infer_optional_type_var_is_false() {
        // T? does not contain infer
        let t = type_var(0);
        let opt_t = Type::Optional(Box::new(t));
        assert!(!contains_infer(&opt_t));
    }

    #[test]
    fn test_contains_infer_func_with_type_var_is_false() {
        // fn(T) -> U does not contain infer
        let t = type_var(0);
        let u = type_var(1);
        let func_type = Type::Func {
            params: vec![t],
            ret: Box::new(u),
        };
        assert!(!contains_infer(&func_type));
    }

    #[test]
    fn test_contains_infer_optional_infer() {
        // infer? returns true
        let opt_infer = Type::Optional(Box::new(Type::Infer));
        assert!(contains_infer(&opt_infer));
    }

    #[test]
    fn test_contains_infer_func_with_infer() {
        // fn(Infer) -> int returns true
        let func_type = Type::Func {
            params: vec![Type::Infer],
            ret: Box::new(Type::Int),
        };
        assert!(contains_infer(&func_type));
    }

    // ---- type variable display tests ----

    #[test]
    fn test_type_var_display() {
        let t = type_var(0);
        assert_eq!(format!("{}", t), "$0");

        let u = type_var(42);
        assert_eq!(format!("{}", u), "$42");
    }

    #[test]
    fn test_optional_type_var_display() {
        let t = type_var(0);
        let opt_t = Type::Optional(t.boxed());
        assert_eq!(format!("{}", opt_t), "$0?");
    }

    #[test]
    fn test_func_type_var_display() {
        let t = type_var(0);
        let u = type_var(1);
        let func_type = Type::Func {
            params: vec![t],
            ret: u.boxed(),
        };
        assert_eq!(format!("{}", func_type), "fn($0) -> $1");
    }

    // ---- type variable predicates tests ----

    #[test]
    fn test_is_type_var() {
        let t = type_var(0);
        assert!(t.is_type_var());
        assert!(!Type::Int.is_type_var());
        assert!(!Type::Infer.is_type_var());
    }

    #[test]
    fn test_type_var_is_not_inferred() {
        // type variables are not considered infer
        let t = type_var(0);
        assert!(!t.is_infer());
        assert!(Type::Infer.is_infer());
    }

    #[test]
    fn test_type_var_is_not_num_bool_etc() {
        let t = type_var(0);
        assert!(!t.is_num());
        assert!(!t.is_bool());
        assert!(!t.is_str());
        assert!(!t.is_void());
        assert!(!t.is_optional());
        assert!(!t.is_func());
    }

    fn type_param(name: &str, id: u32) -> TypeParam {
        TypeParam {
            name: dummy_ident(name),
            id: TypeVarId(id),
        }
    }

    // ---- subst_type tests ----

    #[test]
    fn test_subst_type_simple() {
        // substitute T -> int in T
        let t_var = type_var(0);
        let mut subst = std::collections::HashMap::new();
        subst.insert(TypeVarId(0), Type::Int);

        let result = subst_type(&t_var, &subst);
        assert_eq!(result, Type::Int);
    }

    #[test]
    fn test_subst_type_optional() {
        // substitute T -> int in T?
        let t_var = type_var(0);
        let opt_t = Type::Optional(Box::new(t_var));
        let mut subst = std::collections::HashMap::new();
        subst.insert(TypeVarId(0), Type::Int);

        let result = subst_type(&opt_t, &subst);
        assert_eq!(result, Type::Optional(Box::new(Type::Int)));
    }

    #[test]
    fn test_subst_type_func() {
        // substitute T -> int, U -> bool in fn(T) -> U
        let t_var = type_var(0);
        let u_var = type_var(1);
        let func_ty = Type::Func {
            params: vec![t_var],
            ret: Box::new(u_var),
        };
        let mut subst = std::collections::HashMap::new();
        subst.insert(TypeVarId(0), Type::Int);
        subst.insert(TypeVarId(1), Type::Bool);

        let result = subst_type(&func_ty, &subst);
        assert_eq!(
            result,
            Type::Func {
                params: vec![Type::Int],
                ret: Box::new(Type::Bool),
            }
        );
    }

    #[test]
    fn test_subst_type_repeated_var() {
        // substitute T -> int in fn(T, T) -> T
        let t_var = type_var(0);
        let func_ty = Type::Func {
            params: vec![t_var.clone(), t_var.clone()],
            ret: Box::new(t_var),
        };
        let mut subst = std::collections::HashMap::new();
        subst.insert(TypeVarId(0), Type::Int);

        let result = subst_type(&func_ty, &subst);
        assert_eq!(
            result,
            Type::Func {
                params: vec![Type::Int, Type::Int],
                ret: Box::new(Type::Int),
            }
        );
    }

    #[test]
    fn test_subst_type_no_change_for_concrete() {
        // substitute T -> int in int (no change)
        let mut subst = std::collections::HashMap::new();
        subst.insert(TypeVarId(0), Type::Int);

        let result = subst_type(&Type::Bool, &subst);
        assert_eq!(result, Type::Bool);
    }

    // ---- instantiate_func_type tests ----

    #[test]
    fn test_instantiate_identity() {
        let span = dummy_span();
        let mut errors = vec![];

        // fn identity<T>(x: T) -> T instantiated with <int> yields fn(int) -> int
        let type_params = vec![type_param("T", 0)];
        let t_var = type_var(0);
        let template = Type::Func {
            params: vec![t_var.clone()],
            ret: Box::new(t_var),
        };
        let type_args = vec![Type::Int];

        let result = instantiate_func_type(&type_params, &template, &type_args, span, &mut errors);
        assert!(errors.is_empty());
        assert_eq!(
            result,
            Some(Type::Func {
                params: vec![Type::Int],
                ret: Box::new(Type::Int),
            })
        );
    }

    #[test]
    fn test_instantiate_two_params() {
        let span = dummy_span();
        let mut errors = vec![];

        // fn pair<T, U>(a: T, b: U) -> T? instantiated with <int, bool> yields fn(int, bool) -> int?
        let type_params = vec![type_param("T", 0), type_param("U", 1)];
        let t_var = type_var(0);
        let u_var = type_var(1);
        let template = Type::Func {
            params: vec![t_var.clone(), u_var],
            ret: Box::new(Type::Optional(Box::new(t_var))),
        };
        let type_args = vec![Type::Int, Type::Bool];

        let result = instantiate_func_type(&type_params, &template, &type_args, span, &mut errors);
        assert!(errors.is_empty());
        assert_eq!(
            result,
            Some(Type::Func {
                params: vec![Type::Int, Type::Bool],
                ret: Box::new(Type::Optional(Box::new(Type::Int))),
            })
        );
    }

    #[test]
    fn test_instantiate_arity_mismatch_too_few() {
        let span = dummy_span();
        let mut errors = vec![];

        // fn<T, U> instantiated with <int> (too few)
        let type_params = vec![type_param("T", 0), type_param("U", 1)];
        let template = Type::Func {
            params: vec![type_var(0), type_var(1)],
            ret: Box::new(Type::Void),
        };
        let type_args = vec![Type::Int];

        let result = instantiate_func_type(&type_params, &template, &type_args, span, &mut errors);
        assert_eq!(result, None);
        assert_eq!(errors.len(), 1);
        assert!(matches!(
            &errors[0].kind,
            TypeErrKind::GenericArgNumMismatch {
                expected: 2,
                found: 1
            }
        ));
    }

    #[test]
    fn test_instantiate_arity_mismatch_too_many() {
        let span = dummy_span();
        let mut errors = vec![];

        // fn<T> instantiated with <int, bool, string> (too many)
        let type_params = vec![type_param("T", 0)];
        let template = Type::Func {
            params: vec![type_var(0)],
            ret: Box::new(Type::Void),
        };
        let type_args = vec![Type::Int, Type::Bool, Type::String];

        let result = instantiate_func_type(&type_params, &template, &type_args, span, &mut errors);
        assert_eq!(result, None);
        assert_eq!(errors.len(), 1);
        assert!(matches!(
            &errors[0].kind,
            TypeErrKind::GenericArgNumMismatch {
                expected: 1,
                found: 3
            }
        ));
    }

    /// helper to create a generic function declaration
    fn generic_fn_decl(
        name: &str,
        type_params: Vec<TypeParam>,
        params: Vec<(&str, Type)>,
        ret: Type,
        body: Vec<StmtNode>,
    ) -> StmtNode {
        StmtNode {
            node: Stmt::Func(FuncNode {
                node: Func {
                    name: dummy_ident(name),
                    visibility: Visibility::Private,
                    type_params,
                    params: params
                        .into_iter()
                        .map(|(n, t)| Param {
                            name: dummy_ident(n),
                            ty: t,
                        })
                        .collect(),
                    ret,
                    body: BlockNode {
                        node: Block { stmts: body },
                        span: dummy_span(),
                    },
                },
                span: dummy_span(),
            }),
            span: dummy_span(),
        }
    }

    /// helper to create a call expression with explicit type arguments
    fn call_expr_with_type_args(
        func: ExprNode,
        args: Vec<ExprNode>,
        type_args: Vec<Type>,
    ) -> ExprNode {
        ExprNode {
            node: Expr::new(
                ExprKind::Call(CallNode {
                    node: Call {
                        func: Box::new(func),
                        args,
                        type_args,
                    },
                    span: dummy_span(),
                }),
                next_expr_id(),
            ),
            span: dummy_span(),
        }
    }

    #[test]
    fn test_template_generic_add_with_int_ok() {
        reset_expr_ids();

        // fn add<T>(a: T, b: T) -> T { a + b }
        // let x = add(1, 2);
        let t_id = TypeVarId(0);
        let t_type = Type::Var(t_id);
        let type_params = vec![TypeParam {
            name: dummy_ident("T"),
            id: t_id,
        }];

        let add_fn = generic_fn_decl(
            "add",
            type_params,
            vec![("a", t_type.clone()), ("b", t_type.clone())],
            t_type.clone(),
            vec![expr_stmt(binary_expr(
                ident_expr("a"),
                BinaryOp::Add,
                ident_expr("b"),
            ))],
        );

        let call = call_expr(ident_expr("add"), vec![lit_int(1), lit_int(2)]);
        let call_id = get_expr_id(&call);
        let binding = let_binding("x", None, call);

        let prog = program(vec![add_fn, binding]);
        let tcx = run_ok(prog);
        assert_expr_type(&tcx, call_id, Type::Int);
    }

    #[test]
    fn test_template_generic_add_with_float_ok() {
        reset_expr_ids();

        // fn add<T>(a: T, b: T) -> T { a + b }
        // let x = add(1.0, 2.0);
        let t_id = TypeVarId(0);
        let t_type = Type::Var(t_id);
        let type_params = vec![TypeParam {
            name: dummy_ident("T"),
            id: t_id,
        }];

        let add_fn = generic_fn_decl(
            "add",
            type_params,
            vec![("a", t_type.clone()), ("b", t_type.clone())],
            t_type.clone(),
            vec![expr_stmt(binary_expr(
                ident_expr("a"),
                BinaryOp::Add,
                ident_expr("b"),
            ))],
        );

        let call = call_expr(ident_expr("add"), vec![lit_float(1.0), lit_float(2.0)]);
        let call_id = get_expr_id(&call);
        let binding = let_binding("x", None, call);

        let prog = program(vec![add_fn, binding]);
        let tcx = run_ok(prog);
        assert_expr_type(&tcx, call_id, Type::Float);
    }

    #[test]
    fn test_template_generic_add_with_bool_err() {
        reset_expr_ids();

        // fn add<T>(a: T, b: T) -> T { a + b }
        // let x = add(true, false); // Error: bool + bool is invalid
        let t_id = TypeVarId(0);
        let t_type = Type::Var(t_id);
        let type_params = vec![TypeParam {
            name: dummy_ident("T"),
            id: t_id,
        }];

        let add_fn = generic_fn_decl(
            "add",
            type_params,
            vec![("a", t_type.clone()), ("b", t_type.clone())],
            t_type.clone(),
            vec![expr_stmt(binary_expr(
                ident_expr("a"),
                BinaryOp::Add,
                ident_expr("b"),
            ))],
        );

        let call = call_expr(ident_expr("add"), vec![lit_bool(true), lit_bool(false)]);
        let binding = let_binding("x", None, call);

        let prog = program(vec![add_fn, binding]);
        let errors = run_err(prog);

        assert!(!errors.is_empty());
        assert!(
            errors
                .iter()
                .any(|e| matches!(&e.kind, TypeErrKind::MismatchedTypes { .. })),
            "Expected MismatchedTypes error, got: {:?}",
            errors
        );
    }

    #[test]
    fn test_template_generic_explicit_type_args_ok() {
        reset_expr_ids();

        // fn add<T>(a: T, b: T) -> T { a + b }
        // let x = add<int>(1, 2);
        let t_id = TypeVarId(0);
        let t_type = Type::Var(t_id);
        let type_params = vec![TypeParam {
            name: dummy_ident("T"),
            id: t_id,
        }];

        let add_fn = generic_fn_decl(
            "add",
            type_params,
            vec![("a", t_type.clone()), ("b", t_type.clone())],
            t_type.clone(),
            vec![expr_stmt(binary_expr(
                ident_expr("a"),
                BinaryOp::Add,
                ident_expr("b"),
            ))],
        );

        let call = call_expr_with_type_args(
            ident_expr("add"),
            vec![lit_int(1), lit_int(2)],
            vec![Type::Int],
        );
        let call_id = get_expr_id(&call);
        let binding = let_binding("x", None, call);

        let prog = program(vec![add_fn, binding]);
        let tcx = run_ok(prog);
        assert_expr_type(&tcx, call_id, Type::Int);
    }

    #[test]
    fn test_template_generic_explicit_type_args_bool_err() {
        reset_expr_ids();

        // fn add<T>(a: T, b: T) -> T { a + b }
        // let x = add<bool>(true, false); // Error: bool + bool is invalid
        let t_id = TypeVarId(0);
        let t_type = Type::Var(t_id);
        let type_params = vec![TypeParam {
            name: dummy_ident("T"),
            id: t_id,
        }];

        let add_fn = generic_fn_decl(
            "add",
            type_params,
            vec![("a", t_type.clone()), ("b", t_type.clone())],
            t_type.clone(),
            vec![expr_stmt(binary_expr(
                ident_expr("a"),
                BinaryOp::Add,
                ident_expr("b"),
            ))],
        );

        let call = call_expr_with_type_args(
            ident_expr("add"),
            vec![lit_bool(true), lit_bool(false)],
            vec![Type::Bool],
        );
        let binding = let_binding("x", None, call);

        let prog = program(vec![add_fn, binding]);
        let errors = run_err(prog);
        assert!(!errors.is_empty());
        assert!(
            errors
                .iter()
                .any(|e| matches!(&e.kind, TypeErrKind::MismatchedTypes { .. })),
            "Expected MismatchedTypes error, got: {:?}",
            errors
        );
    }

    #[test]
    fn test_template_generic_specialization_cache() {
        reset_expr_ids();

        // fn add<T>(a: T, b: T) -> T { a + b }
        // let x = add(1, 2);
        // let y = add(10, 20); // same instantiation shoudl use cache
        let t_id = TypeVarId(0);
        let t_type = Type::Var(t_id);
        let type_params = vec![TypeParam {
            name: dummy_ident("T"),
            id: t_id,
        }];

        let add_fn = generic_fn_decl(
            "add",
            type_params,
            vec![("a", t_type.clone()), ("b", t_type.clone())],
            t_type.clone(),
            vec![expr_stmt(binary_expr(
                ident_expr("a"),
                BinaryOp::Add,
                ident_expr("b"),
            ))],
        );

        let call1 = call_expr(ident_expr("add"), vec![lit_int(1), lit_int(2)]);
        let call1_id = get_expr_id(&call1);
        let binding1 = let_binding("x", None, call1);

        let call2 = call_expr(ident_expr("add"), vec![lit_int(10), lit_int(20)]);
        let call2_id = get_expr_id(&call2);
        let binding2 = let_binding("y", None, call2);

        let prog = program(vec![add_fn, binding1, binding2]);
        let tcx = run_ok(prog);

        assert_expr_type(&tcx, call1_id, Type::Int);
        assert_expr_type(&tcx, call2_id, Type::Int);
    }

    #[test]
    fn test_template_generic_multiple_instantiations() {
        reset_expr_ids();

        // fn add<T>(a: T, b: T) -> T { a + b }
        // let x = add(1, 2);       // T = int
        // let y = add(1.0, 2.0);   // T = float
        let t_id = TypeVarId(0);
        let t_type = Type::Var(t_id);
        let type_params = vec![TypeParam {
            name: dummy_ident("T"),
            id: t_id,
        }];

        let add_fn = generic_fn_decl(
            "add",
            type_params,
            vec![("a", t_type.clone()), ("b", t_type.clone())],
            t_type.clone(),
            vec![expr_stmt(binary_expr(
                ident_expr("a"),
                BinaryOp::Add,
                ident_expr("b"),
            ))],
        );

        let call1 = call_expr(ident_expr("add"), vec![lit_int(1), lit_int(2)]);
        let call1_id = get_expr_id(&call1);
        let binding1 = let_binding("x", None, call1);

        let call2 = call_expr(ident_expr("add"), vec![lit_float(1.0), lit_float(2.0)]);
        let call2_id = get_expr_id(&call2);
        let binding2 = let_binding("y", None, call2);

        let prog = program(vec![add_fn, binding1, binding2]);
        let tcx = run_ok(prog);

        // first call should have type int
        assert_expr_type(&tcx, call1_id, Type::Int);
        // second call should have type float
        assert_expr_type(&tcx, call2_id, Type::Float);
    }

    #[test]
    fn test_template_generic_identity_ok() {
        reset_expr_ids();

        // fn identity<T>(x: T) -> T { x }
        // let a = identity(42);
        // let b = identity(true);
        let t_id = TypeVarId(0);
        let t_type = Type::Var(t_id);
        let type_params = vec![TypeParam {
            name: dummy_ident("T"),
            id: t_id,
        }];

        let identity_fn = generic_fn_decl(
            "identity",
            type_params,
            vec![("x", t_type.clone())],
            t_type.clone(),
            vec![expr_stmt(ident_expr("x"))],
        );

        let call1 = call_expr(ident_expr("identity"), vec![lit_int(42)]);
        let call1_id = get_expr_id(&call1);
        let binding1 = let_binding("a", None, call1);

        let call2 = call_expr(ident_expr("identity"), vec![lit_bool(true)]);
        let call2_id = get_expr_id(&call2);
        let binding2 = let_binding("b", None, call2);

        let prog = program(vec![identity_fn, binding1, binding2]);
        let tcx = run_ok(prog);

        // first call should have type int
        assert_expr_type(&tcx, call1_id, Type::Int);
        // second call should have type bool
        assert_expr_type(&tcx, call2_id, Type::Bool);
    }
}
