use std::collections::HashMap;

use super::{
    BasicBlock, BlockId, ConstData, ConstValue, Function, FunctionId, FunctionKind, Local, LocalId,
    LocalKind, Module, ModuleId, Mutability as AirMutability, Operand, Param, ParamRole, Place,
    Program, RValue, Signature, Statement, Terminator, TypeData, TypeId, VerifyError,
    typing::{self, PrimitiveTypes, ScalarType},
    verify,
};
use crate::{
    ast::{
        self, AssignOp, BinaryOp, BlockNode, ExprId, ExprKind, ExprNode, Ident, Lit,
        Mutability as AstMutability, Pattern, Stmt, StmtNode, Type,
    },
    resolve::{PackageModulePath, ResolveResult},
    source::SourceId,
    span::SourceSpan,
    typecheck::{
        BodyInstanceKey, CallableId, CallableInstanceKey, DeclarationIndex, GenericArgs,
        LocalDefFact, LocalDefKind, LocalUseFact, LocalUseMode, ModuleScope, SemanticBodyFacts,
        SemanticFunctionFact, SemanticLocalId, SemanticProgram,
    },
};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum LowerError {
    MissingBodyFacts {
        body: BodyInstanceKey,
    },
    MissingExprType {
        body: BodyInstanceKey,
        expr_id: ExprId,
    },
    MissingLocalDef {
        body: BodyInstanceKey,
        local: SemanticLocalId,
    },
    MissingBindingDef {
        body: BodyInstanceKey,
        span: SourceSpan,
    },
    MissingParamDef {
        body: BodyInstanceKey,
        index: usize,
    },
    MissingLocalUse {
        body: BodyInstanceKey,
        expr_id: ExprId,
    },
    UnsupportedType {
        ty: Type,
    },
    UnsupportedDecl {
        kind: &'static str,
        span: Option<SourceSpan>,
    },
    UnsupportedStmt {
        kind: &'static str,
        span: Option<SourceSpan>,
    },
    UnsupportedExpr {
        expr_id: ExprId,
        kind: &'static str,
    },
    UnterminatedBlock,
    Verify(Vec<VerifyError>),
}

#[derive(Debug, Default)]
struct TypeLowerer {
    int: Option<TypeId>,
    float: Option<TypeId>,
    boolean: Option<TypeId>,
    string: Option<TypeId>,
    void: Option<TypeId>,
}

impl TypeLowerer {
    fn lower(&mut self, program: &mut Program, ty: &Type) -> Result<TypeId, LowerError> {
        let (slot, data) = match ty {
            Type::Int => (&mut self.int, TypeData::Int),
            Type::Float => (&mut self.float, TypeData::Float),
            Type::Bool => (&mut self.boolean, TypeData::Bool),
            Type::String => (&mut self.string, TypeData::String),
            Type::Void => (&mut self.void, TypeData::Void),
            _ => return Err(LowerError::UnsupportedType { ty: ty.clone() }),
        };
        Ok(*slot.get_or_insert_with(|| program.alloc_type(data)))
    }
}

#[derive(Debug, Default)]
struct LoweringMaps {
    modules: HashMap<ModuleScope, ModuleId>,
    bodies: HashMap<BodyInstanceKey, FunctionId>,
    locals: HashMap<BodyInstanceKey, HashMap<SemanticLocalId, LocalId>>,
}

#[derive(Debug, Default)]
struct LowerCx {
    program: Program,
    types: TypeLowerer,
    maps: LoweringMaps,
}

impl LowerCx {
    fn lower_ty(&mut self, ty: &Type) -> Result<TypeId, LowerError> {
        self.types.lower(&mut self.program, ty)
    }

    fn lower_modules(&mut self, modules: &SourceModules<'_>) {
        for module in &modules.items {
            let id = self.program.alloc_module(Module {
                path: module.path.clone(),
                functions: vec![],
                aggregates: vec![],
                enums: vec![],
                extern_types: vec![],
                externs: vec![],
            });
            let old = self.maps.modules.insert(module.scope.clone(), id);
            debug_assert!(old.is_none(), "duplicate source module in AIR lowering");
        }
    }

    fn lower_function_shells(
        &mut self,
        modules: &SourceModules<'_>,
        functions: &SourceFunctions<'_>,
        semantic: &SemanticProgram,
    ) -> Result<(), LowerError> {
        for source in &functions.items {
            let module_scope = &modules.items[source.module].scope;
            let module = self.maps.modules[module_scope];
            let empty_facts;
            let body_facts = match semantic.facts.body(&source.body) {
                Some(facts) => facts,
                None if source.func.node.params.is_empty() => {
                    empty_facts = SemanticBodyFacts::default();
                    &empty_facts
                }
                None => {
                    return Err(LowerError::MissingBodyFacts {
                        body: source.body.clone(),
                    });
                }
            };
            let return_type = self.lower_ty(&source.fact.return_ty)?;
            let mut params = vec![];
            let mut locals = vec![];
            let mut local_map = HashMap::new();
            for index in 0..source.func.node.params.len() {
                let semantic_local = body_facts
                    .locals
                    .param_defs
                    .get(&index)
                    .copied()
                    .ok_or_else(|| LowerError::MissingParamDef {
                        body: source.body.clone(),
                        index,
                    })?;
                let def = body_facts.locals.defs.get(&semantic_local).ok_or_else(|| {
                    LowerError::MissingLocalDef {
                        body: source.body.clone(),
                        local: semantic_local,
                    }
                })?;
                debug_assert_eq!(def.kind, LocalDefKind::Parameter);
                let ty = self.lower_ty(&def.ty)?;
                let local_id = LocalId::from_index(locals.len());
                locals.push(Local {
                    name: Some(def.name),
                    ty,
                    mutability: if def.mutable {
                        AirMutability::Mutable
                    } else {
                        AirMutability::Immutable
                    },
                    kind: LocalKind::Arg,
                });
                let old = local_map.insert(semantic_local, local_id);
                debug_assert!(old.is_none(), "duplicate semantic param local");
                params.push(Param {
                    name: Some(def.name),
                    ty,
                    role: ParamRole::Normal,
                    local_id,
                });
            }
            let function = Function {
                name: source.func.node.name,
                module,
                kind: FunctionKind::Normal,
                signature: Signature::new(params, return_type),
                locals,
                body: vec![BasicBlock {
                    statements: vec![],
                    terminator: Terminator::Unreachable,
                }],
            };
            let id = self.program.alloc_function(function);
            self.program.module_mut(module).functions.push(id);
            let old = self.maps.bodies.insert(source.body.clone(), id);
            debug_assert!(old.is_none(), "duplicate lowered function body");
            let old = self.maps.locals.insert(source.body.clone(), local_map);
            debug_assert!(old.is_none(), "duplicate lowered function local map");
        }
        Ok(())
    }

    fn lower_function_bodies(
        &mut self,
        functions: &SourceFunctions<'_>,
        semantic: &SemanticProgram,
    ) -> Result<(), LowerError> {
        for source in &functions.items {
            let empty_facts;
            let facts = match semantic.facts.body(&source.body) {
                Some(facts) => facts,
                None if source.func.node.params.is_empty() => {
                    empty_facts = SemanticBodyFacts::default();
                    &empty_facts
                }
                None => {
                    return Err(LowerError::MissingBodyFacts {
                        body: source.body.clone(),
                    });
                }
            };
            let function = self.maps.bodies[&source.body];
            let locals = self
                .maps
                .locals
                .remove(&source.body)
                .expect("lowered function missing local map");
            let mut lowerer = FunctionLowerer::new(self, source, facts, function, locals);
            lowerer.lower_body(&source.func.node.body)?;
        }
        Ok(())
    }
}

struct FunctionLowerer<'cx, 'facts> {
    cx: &'cx mut LowerCx,
    body: BodyInstanceKey,
    facts: &'facts SemanticBodyFacts,
    function_id: FunctionId,
    source: SourceId,
    function: Function,
    locals: HashMap<SemanticLocalId, LocalId>,
    current: BlockId,
    terminated: bool,
}

impl<'cx, 'facts> FunctionLowerer<'cx, 'facts> {
    fn new(
        cx: &'cx mut LowerCx,
        source: &SourceFunction<'_>,
        facts: &'facts SemanticBodyFacts,
        function_id: FunctionId,
        locals: HashMap<SemanticLocalId, LocalId>,
    ) -> Self {
        let mut function = cx.program.function(function_id).clone();
        function.body = vec![BasicBlock {
            statements: vec![],
            terminator: Terminator::Unreachable,
        }];
        Self {
            cx,
            body: source.body.clone(),
            facts,
            function_id,
            source: source.source,
            function,
            locals,
            current: BlockId::from_index(0),
            terminated: false,
        }
    }

    fn lower_body(&mut self, block: &BlockNode) -> Result<(), LowerError> {
        self.lower_stmts(&block.node.stmts)?;
        if let Some(tail) = &block.node.tail
            && !self.terminated
        {
            if self.returns_void() {
                self.lower_effect(tail)?;
            } else {
                let value = self.lower_value(tail)?;
                self.terminate(Terminator::Return(Some(value)))?;
            }
        }
        if !self.terminated && self.returns_void() {
            self.terminate(Terminator::Return(None))?;
        }
        if !self.terminated {
            return Err(LowerError::UnterminatedBlock);
        }
        self.cx.program.functions[self.function_id.index()] = self.function.clone();
        Ok(())
    }

    fn lower_stmts(&mut self, stmts: &[StmtNode]) -> Result<(), LowerError> {
        for stmt in stmts {
            self.lower_stmt(stmt)?;
        }
        Ok(())
    }

    fn lower_block_value(
        &mut self,
        expr: &ExprNode,
        block: &BlockNode,
    ) -> Result<Operand, LowerError> {
        self.lower_stmts(&block.node.stmts)?;
        let Some(tail) = &block.node.tail else {
            return Err(self.unsupported_expr(expr));
        };
        self.lower_value(tail)
    }

    fn lower_block_effect(&mut self, block: &BlockNode) -> Result<(), LowerError> {
        self.lower_stmts(&block.node.stmts)?;
        if let Some(tail) = &block.node.tail {
            self.lower_effect(tail)?;
        }
        Ok(())
    }

    fn lower_stmt(&mut self, stmt: &StmtNode) -> Result<(), LowerError> {
        if self.terminated {
            return Err(LowerError::UnsupportedStmt {
                kind: "statement after terminator",
                span: Some(self.source_span(stmt.span)),
            });
        }
        match &stmt.node {
            Stmt::Expr(expr) => self.lower_effect(expr),
            Stmt::Binding(binding) => self.lower_binding(binding),
            Stmt::Return(ret) => match &ret.node.value {
                Some(value) => {
                    let value = self.lower_value(value)?;
                    self.terminate(Terminator::Return(Some(value)))
                }
                None => self.terminate(Terminator::Return(None)),
            },
            _ => Err(LowerError::UnsupportedStmt {
                kind: stmt_kind(&stmt.node),
                span: Some(self.source_span(stmt.span)),
            }),
        }
    }

    fn lower_binding(&mut self, binding: &ast::BindingNode) -> Result<(), LowerError> {
        match &binding.node.pattern.node {
            Pattern::Ident(_) => {
                let value = self.lower_value(&binding.node.value)?;
                let site = self.source_span(binding.node.pattern.span);
                let semantic = self
                    .facts
                    .locals
                    .binding_defs
                    .get(&site)
                    .copied()
                    .ok_or_else(|| LowerError::MissingBindingDef {
                        body: self.body.clone(),
                        span: site,
                    })?;
                let def = self.local_def(semantic)?;
                let name = def.name;
                let mutable = def.mutable;
                let ty = def.ty.clone();
                let ty = self.cx.lower_ty(&ty)?;
                let local = self.push_local(
                    Some(name),
                    ty,
                    if mutable {
                        AirMutability::Mutable
                    } else {
                        AirMutability::Immutable
                    },
                    LocalKind::User,
                );
                self.locals.insert(semantic, local);
                self.emit_init(local, RValue::Use(value))
            }
            Pattern::Wildcard if binding.node.mutability == AstMutability::Immutable => {
                self.lower_effect(&binding.node.value)
            }
            _ => Err(LowerError::UnsupportedStmt {
                kind: binding.node.pattern.node.variant_name(),
                span: Some(self.source_span(binding.span)),
            }),
        }
    }

    fn lower_value(&mut self, expr: &ExprNode) -> Result<Operand, LowerError> {
        match &expr.node.kind {
            ExprKind::Lit(lit) => self.lower_lit(expr, lit),
            ExprKind::Ident(_) => {
                let fact = self.local_use(expr, LocalUseMode::Read)?;
                let local = self.local(fact.local)?;
                Ok(self.operand_place(local))
            }
            ExprKind::Block(block) => self.lower_block_value(expr, block),
            ExprKind::Unary(unary) => {
                self.require_builtin_scalar(expr)?;
                let value = self.lower_value(&unary.node.expr)?;
                let ty = self.lower_expr_ty(expr.node.id)?;
                let value_scalar = source_scalar(&self.operand_type(&value))
                    .ok_or_else(|| self.unsupported_expr(expr))?;
                let result_scalar =
                    source_scalar(&ty).ok_or_else(|| self.unsupported_expr(expr))?;
                if !typing::supports_scalar_unary(unary.node.op, value_scalar, result_scalar) {
                    return Err(self.unsupported_expr(expr));
                }
                let ty = self.cx.lower_ty(&ty)?;
                self.emit_temp(RValue::Unary {
                    op: unary.node.op,
                    value,
                    ty,
                })
            }
            ExprKind::Binary(binary) => {
                self.require_builtin_scalar(expr)?;
                let lhs = self.lower_value(&binary.node.left)?;
                let rhs = self.lower_value(&binary.node.right)?;
                let lhs_ty = self.operand_type(&lhs);
                let rhs_ty = self.operand_type(&rhs);
                let result_ty = self.lower_expr_ty(expr.node.id)?;
                let Some((lhs_scalar, rhs_scalar, result_scalar)) =
                    scalar_types(&lhs_ty, &rhs_ty, &result_ty)
                else {
                    return Err(self.unsupported_expr(expr));
                };
                if !typing::supports_scalar_binary(
                    binary.node.op,
                    lhs_scalar,
                    rhs_scalar,
                    result_scalar,
                ) {
                    return Err(self.unsupported_expr(expr));
                }
                let ty = self.cx.lower_ty(&result_ty)?;
                self.emit_temp(RValue::Binary {
                    op: binary.node.op,
                    lhs,
                    rhs,
                    ty,
                })
            }
            ExprKind::Cast(cast) => {
                self.require_builtin_scalar(expr)?;
                let source_ty = self.lower_expr_ty(cast.node.expr.node.id)?;
                let target_ty = self.lower_expr_ty(expr.node.id)?;
                let value = self.lower_value(&cast.node.expr)?;
                if source_ty == target_ty {
                    return Ok(value);
                }
                if !matches!(
                    (&source_ty, &target_ty),
                    (Type::Int, Type::Float) | (Type::Float, Type::Int)
                ) {
                    return Err(self.unsupported_expr(expr));
                }
                let target = self.cx.lower_ty(&target_ty)?;
                self.emit_temp(RValue::Cast { value, target })
            }
            _ => Err(self.unsupported_expr(expr)),
        }
    }

    fn lower_lit(&mut self, expr: &ExprNode, lit: &Lit) -> Result<Operand, LowerError> {
        let ty = self.lower_expr_ty(expr.node.id)?;
        let ty_id = self.cx.lower_ty(&ty)?;
        let value = match (lit, &ty) {
            (Lit::Int(value), Type::Int) => ConstValue::Int(*value),
            (Lit::Float(value), Type::Float) => ConstValue::Float(*value),
            (Lit::Bool(value), Type::Bool) => ConstValue::Bool(*value),
            (Lit::String(value), Type::String) => {
                ConstValue::String(value.clone().into_boxed_str())
            }
            _ => return Err(self.unsupported_expr(expr)),
        };
        Ok(Operand::Const(
            self.cx.program.alloc_const(ConstData { ty: ty_id, value }),
        ))
    }

    fn lower_effect(&mut self, expr: &ExprNode) -> Result<(), LowerError> {
        match &expr.node.kind {
            ExprKind::Assign(assign) => self.lower_assign(expr, assign),
            ExprKind::Block(block) => self.lower_block_effect(block),
            _ => {
                let value = self.lower_value(expr)?;
                self.emit_eval(RValue::Use(value))
            }
        }
    }

    fn lower_assign(
        &mut self,
        expr: &ExprNode,
        assign: &ast::AssignNode,
    ) -> Result<(), LowerError> {
        if !matches!(assign.node.target.node.kind, ExprKind::Ident(_)) {
            return Err(self.unsupported_expr(&assign.node.target));
        }
        match assign.node.op {
            AssignOp::Assign => {
                let fact = self.local_use(&assign.node.target, LocalUseMode::Assign)?;
                let dst = self.lower_place(&assign.node.target, &fact)?;
                let value = self.lower_value(&assign.node.value)?;
                self.emit_assign(dst, RValue::Use(value))
            }
            op => {
                self.require_builtin_scalar(expr)?;
                let binary = assign_op_to_binary(op);
                let fact = self.local_use(&assign.node.target, LocalUseMode::CompoundAssign)?;
                let dst = self.lower_place(&assign.node.target, &fact)?;
                let lhs = Operand::Place(dst.clone());
                let rhs = self.lower_value(&assign.node.value)?;
                let lhs_ty = self.operand_type(&lhs);
                let rhs_ty = self.operand_type(&rhs);
                let result_ty = self.air_type(dst.ty);
                let Some((lhs_scalar, rhs_scalar, result_scalar)) =
                    scalar_types(&lhs_ty, &rhs_ty, &result_ty)
                else {
                    return Err(self.unsupported_expr(&assign.node.target));
                };
                if !typing::supports_scalar_binary(binary, lhs_scalar, rhs_scalar, result_scalar) {
                    return Err(self.unsupported_expr(&assign.node.target));
                }
                let tmp = self.emit_temp(RValue::Binary {
                    op: binary,
                    lhs,
                    rhs,
                    ty: dst.ty,
                })?;
                self.emit_assign(dst, RValue::Use(tmp))
            }
        }
    }

    fn lower_place(&mut self, expr: &ExprNode, fact: &LocalUseFact) -> Result<Place, LowerError> {
        match &expr.node.kind {
            ExprKind::Ident(_) => {
                let local = self.local(fact.local)?;
                Ok(self.local_place(local))
            }
            _ => Err(self.unsupported_expr(expr)),
        }
    }

    fn require_builtin_scalar(&self, expr: &ExprNode) -> Result<(), LowerError> {
        if self.has_deferred_expr_fact(expr.node.id) {
            return Err(self.unsupported_expr(expr));
        }
        Ok(())
    }

    fn has_deferred_expr_fact(&self, id: ExprId) -> bool {
        self.facts.calls.contains_key(&id)
            || self.facts.extern_uses.contains_key(&id)
            || self.facts.member_paths.contains_key(&id)
            || self.facts.expected_projections.contains_key(&id)
            || self.facts.dyn_conversions.contains_key(&id)
            || self.facts.dyn_weakenings.contains_key(&id)
            || self.facts.dyn_calls.contains_key(&id)
            || self.facts.dyn_downcasts.contains_key(&id)
            || self.facts.global_accesses.contains_key(&id)
    }

    fn returns_void(&self) -> bool {
        self.cx
            .program
            .type_data(self.function.signature.return_type)
            == &TypeData::Void
    }

    fn lower_expr_ty(&self, expr_id: ExprId) -> Result<Type, LowerError> {
        self.facts
            .expr_types
            .get(&expr_id)
            .and_then(|fact| fact.ty.clone())
            .ok_or_else(|| LowerError::MissingExprType {
                body: self.body.clone(),
                expr_id,
            })
    }

    fn local_use(&self, expr: &ExprNode, mode: LocalUseMode) -> Result<LocalUseFact, LowerError> {
        let expr_id = expr.node.id;
        let Some(fact) = self.facts.locals.uses.get(&expr_id) else {
            if self.has_deferred_expr_fact(expr_id)
                || self.expr_type_has_no_local_identity(expr_id)?
            {
                return Err(self.unsupported_expr(expr));
            }
            return Err(LowerError::MissingLocalUse {
                body: self.body.clone(),
                expr_id,
            });
        };
        if fact.mode != mode {
            return Err(LowerError::MissingLocalUse {
                body: self.body.clone(),
                expr_id,
            });
        }
        Ok(fact.clone())
    }

    fn expr_type_has_no_local_identity(&self, expr_id: ExprId) -> Result<bool, LowerError> {
        Ok(matches!(self.lower_expr_ty(expr_id)?, Type::Func { .. }))
    }

    fn local_def(&self, local: SemanticLocalId) -> Result<&LocalDefFact, LowerError> {
        self.facts
            .locals
            .defs
            .get(&local)
            .ok_or_else(|| LowerError::MissingLocalDef {
                body: self.body.clone(),
                local,
            })
    }

    fn local(&self, local: SemanticLocalId) -> Result<LocalId, LowerError> {
        self.locals
            .get(&local)
            .copied()
            .ok_or_else(|| LowerError::MissingLocalDef {
                body: self.body.clone(),
                local,
            })
    }

    fn push_local(
        &mut self,
        name: Option<Ident>,
        ty: TypeId,
        mutability: AirMutability,
        kind: LocalKind,
    ) -> LocalId {
        let id = LocalId::from_index(self.function.locals.len());
        self.function.locals.push(Local {
            name,
            ty,
            mutability,
            kind,
        });
        id
    }

    fn temp(&mut self, ty: TypeId) -> LocalId {
        self.push_local(None, ty, AirMutability::Immutable, LocalKind::Temp)
    }

    fn local_place(&self, local: LocalId) -> Place {
        Place {
            root: local,
            projection: vec![],
            ty: self.function.locals[local.index()].ty,
        }
    }

    fn operand_place(&self, local: LocalId) -> Operand {
        Operand::Place(self.local_place(local))
    }

    fn operand_type(&self, operand: &Operand) -> Type {
        match operand {
            Operand::Place(place) => self.air_type(place.ty),
            Operand::Const(id) => self.air_type(self.cx.program.const_data(*id).ty),
        }
    }

    fn air_type(&self, ty: TypeId) -> Type {
        match self.cx.program.type_data(ty) {
            TypeData::Int => Type::Int,
            TypeData::Float => Type::Float,
            TypeData::Bool => Type::Bool,
            TypeData::String => Type::String,
            TypeData::Void => Type::Void,
            _ => Type::Infer,
        }
    }

    fn emit_init(&mut self, local: LocalId, value: RValue) -> Result<(), LowerError> {
        self.ensure_open()?;
        self.current_block()
            .statements
            .push(Statement::Init { local, value });
        Ok(())
    }

    fn emit_assign(&mut self, dst: Place, value: RValue) -> Result<(), LowerError> {
        self.ensure_open()?;
        self.current_block()
            .statements
            .push(Statement::Assign { dst, value });
        Ok(())
    }

    fn emit_eval(&mut self, value: RValue) -> Result<(), LowerError> {
        self.ensure_open()?;
        self.current_block().statements.push(Statement::Eval(value));
        Ok(())
    }

    fn emit_temp(&mut self, value: RValue) -> Result<Operand, LowerError> {
        let primitives = PrimitiveTypes::scan(&self.cx.program);
        let ty = typing::rvalue_ty(&self.cx.program, &primitives, &value)
            .ok_or(LowerError::UnsupportedType { ty: Type::Infer })?;
        let local = self.temp(ty);
        self.emit_init(local, value)?;
        Ok(self.operand_place(local))
    }

    fn terminate(&mut self, term: Terminator) -> Result<(), LowerError> {
        self.ensure_open()?;
        self.current_block().terminator = term;
        self.terminated = true;
        Ok(())
    }

    fn ensure_open(&self) -> Result<(), LowerError> {
        if self.terminated {
            Err(LowerError::UnsupportedStmt {
                kind: "terminated block",
                span: None,
            })
        } else {
            Ok(())
        }
    }

    fn current_block(&mut self) -> &mut BasicBlock {
        &mut self.function.body[self.current.index()]
    }

    fn source_span(&self, span: crate::span::Span) -> SourceSpan {
        SourceSpan::from_byte_span(self.source, span)
    }

    fn unsupported_expr(&self, expr: &ExprNode) -> LowerError {
        LowerError::UnsupportedExpr {
            expr_id: expr.node.id,
            kind: expr.node.kind.variant_name(),
        }
    }
}

fn stmt_kind(stmt: &Stmt) -> &'static str {
    match stmt {
        Stmt::Import(_) => "Import",
        Stmt::Func(_) => "Func",
        Stmt::ExternFunc(_) => "ExternFunc",
        Stmt::ExternType(_) => "ExternType",
        Stmt::Aggregate(_) => "Aggregate",
        Stmt::Enum(_) => "Enum",
        Stmt::Extend(_) => "Extend",
        Stmt::Const(_) => "Const",
        Stmt::Global(_) => "Global",
        Stmt::TypeAlias(_) => "TypeAlias",
        Stmt::Contract(_) => "Contract",
        Stmt::Expr(_) => "Expr",
        Stmt::Binding(_) => "Binding",
        Stmt::LetElse(_) => "LetElse",
        Stmt::Return(_) => "Return",
        Stmt::While(_) => "While",
        Stmt::WhileLet(_) => "WhileLet",
        Stmt::For(_) => "For",
        Stmt::Break => "Break",
        Stmt::Continue => "Continue",
        Stmt::Defer(_) => "Defer",
    }
}

fn assign_op_to_binary(op: AssignOp) -> BinaryOp {
    match op {
        AssignOp::Assign => unreachable!("plain assignment has no binary op"),
        AssignOp::AddAssign => BinaryOp::Add,
        AssignOp::SubAssign => BinaryOp::Sub,
        AssignOp::MulAssign => BinaryOp::Mul,
        AssignOp::DivAssign => BinaryOp::Div,
        AssignOp::XorAssign => BinaryOp::Xor,
        AssignOp::BitAndAssign => BinaryOp::BitAnd,
        AssignOp::BitOrAssign => BinaryOp::BitOr,
        AssignOp::ShlAssign => BinaryOp::Shl,
        AssignOp::ShrAssign => BinaryOp::Shr,
    }
}

fn source_scalar(ty: &Type) -> Option<ScalarType> {
    match ty {
        Type::Int => Some(ScalarType::Int),
        Type::Float => Some(ScalarType::Float),
        Type::Bool => Some(ScalarType::Bool),
        Type::String => Some(ScalarType::String),
        _ => None,
    }
}

fn scalar_types(
    lhs: &Type,
    rhs: &Type,
    result: &Type,
) -> Option<(ScalarType, ScalarType, ScalarType)> {
    Some((
        source_scalar(lhs)?,
        source_scalar(rhs)?,
        source_scalar(result)?,
    ))
}

pub(crate) fn lower_with_modules(
    root: &ast::Program,
    resolved: &ResolveResult,
    semantic: &SemanticProgram,
) -> Result<Program, LowerError> {
    let modules = SourceModules::new(root, resolved);
    let functions = SourceFunctions::new(&modules, semantic)?;
    let mut cx = LowerCx::default();
    cx.lower_modules(&modules);
    cx.lower_function_shells(&modules, &functions, semantic)?;
    cx.lower_function_bodies(&functions, semantic)?;
    verify(&cx.program).map_err(LowerError::Verify)?;
    Ok(cx.program)
}

struct SourceModules<'a> {
    items: Vec<SourceModule<'a>>,
}

struct SourceModule<'a> {
    scope: ModuleScope,
    source: SourceId,
    program: &'a ast::Program,
    path: Vec<Ident>,
    system: bool,
}

impl<'a> SourceModules<'a> {
    fn new(root: &'a ast::Program, resolved: &'a ResolveResult) -> Self {
        let items = DeclarationIndex::source_modules(root, resolved)
            .into_iter()
            .map(|module| SourceModule {
                path: module_path(&module.scope),
                system: module_is_system(&module.scope, resolved),
                scope: module.scope,
                source: module.source,
                program: module.program,
            })
            .collect();
        Self { items }
    }
}

#[derive(Debug)]
struct SourceFunctions<'a> {
    items: Vec<SourceFunction<'a>>,
}

#[derive(Debug)]
struct SourceFunction<'a> {
    module: usize,
    func: &'a ast::FuncNode,
    body: BodyInstanceKey,
    fact: &'a SemanticFunctionFact,
    source: SourceId,
}

impl<'a> SourceFunctions<'a> {
    fn new(
        modules: &'a SourceModules<'a>,
        semantic: &'a SemanticProgram,
    ) -> Result<Self, LowerError> {
        let mut items = vec![];
        for (module_index, module) in modules.items.iter().enumerate() {
            if module.system {
                continue;
            }
            for stmt in &module.program.stmts {
                let span = Some(SourceSpan::from_byte_span(module.source, stmt.span));
                let Stmt::Func(func_node) = &stmt.node else {
                    if !matches!(&stmt.node, Stmt::Import(_)) {
                        return Err(LowerError::UnsupportedDecl {
                            kind: stmt_kind(&stmt.node),
                            span,
                        });
                    }
                    continue;
                };
                let func = &func_node.node;
                if !func.type_params.is_empty() || !func.const_params.is_empty() {
                    return Err(LowerError::UnsupportedDecl {
                        kind: "generic function",
                        span,
                    });
                }
                if func.params.iter().any(|param| param.default.is_some()) {
                    return Err(LowerError::UnsupportedDecl {
                        kind: "function with default params",
                        span,
                    });
                }
                let id = CallableId::function(module.scope.clone(), func.name);
                let body = BodyInstanceKey::Callable(CallableInstanceKey {
                    target: id.clone(),
                    args: GenericArgs::default(),
                });
                let Some(fact) = semantic
                    .declaration_facts
                    .functions
                    .iter()
                    .find(|fact| fact.id == id && fact.body == body)
                else {
                    return Err(LowerError::MissingBodyFacts { body });
                };
                items.push(SourceFunction {
                    module: module_index,
                    func: func_node,
                    body,
                    fact,
                    source: module.source,
                });
            }
        }
        Ok(Self { items })
    }
}

fn module_is_system(scope: &ModuleScope, resolved: &ResolveResult) -> bool {
    resolved.system.core.as_ref().is_some_and(|package| {
        matches!(scope, ModuleScope::Package(module) if module.package_context() == Some(package))
    })
}

fn module_path(scope: &ModuleScope) -> Vec<Ident> {
    match scope {
        ModuleScope::Root => vec![],
        ModuleScope::Named(path) => path.segments().iter().map(Ident::new).collect(),
        ModuleScope::Package(module) => match module.path() {
            PackageModulePath::Root => module
                .package_context()
                .map(|package| vec![Ident::new(package.as_str())])
                .unwrap_or_default(),
            PackageModulePath::Named(path) | PackageModulePath::Provider(path) => {
                path.segments().iter().map(Ident::new).collect()
            }
            PackageModulePath::Source(source) => vec![Ident::new(source.to_string())],
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        externs,
        test_support::{
            parse_program, resolved_modules_with_core_option, resolved_with_core_option,
        },
        typecheck::{self, TypecheckConfig},
    };

    #[test]
    fn empty_program_lowers_to_verified_air() {
        let (root, resolved, semantic) = checked("");
        lower_with_modules(&root, &resolved, &semantic.program).expect("lower failed");
    }

    #[test]
    fn source_modules_follow_resolve_order() {
        let root = parse_program("import gamekit; import util;");
        let resolved = resolved_modules_with_core_option(
            &root,
            &[("gamekit", "fn g() {}"), ("util", "fn u() {}")],
        );
        let modules = SourceModules::new(&root, &resolved);
        assert_eq!(modules.items.len(), 5);
        assert_eq!(
            modules.items[0].scope,
            ModuleScope::from_module_id(&resolved.root)
        );
        assert_eq!(modules.items[0].source, resolved.root_source);
        assert!(
            modules
                .items
                .iter()
                .all(|module| !module.path.is_empty() || matches!(module.scope, ModuleScope::Root))
        );
    }

    #[test]
    fn source_functions_pair_with_semantic_facts() {
        let (root, resolved, semantic) = checked("fn f(a: int) -> int { a }");
        let modules = SourceModules::new(&root, &resolved);
        let functions =
            SourceFunctions::new(&modules, &semantic.program).expect("inventory failed");
        assert_eq!(functions.items.len(), 1);
        let function = &functions.items[0];
        assert_eq!(function.module, 0);
        assert_eq!(function.func.node.name, Ident::new("f"));
        assert_eq!(function.body, function.fact.body);
    }

    #[test]
    fn generic_function_is_unsupported() {
        let (root, resolved, semantic) = checked("fn f<T>(x: T) -> T { x }");
        let modules = SourceModules::new(&root, &resolved);
        let err = SourceFunctions::new(&modules, &semantic.program).expect_err("expected error");
        assert!(matches!(
            err,
            LowerError::UnsupportedDecl {
                kind: "generic function",
                ..
            }
        ));
    }

    #[test]
    fn default_param_function_is_unsupported() {
        let (root, resolved, semantic) = checked("fn f(a: int = 1) -> int { a }");
        let modules = SourceModules::new(&root, &resolved);
        let err = SourceFunctions::new(&modules, &semantic.program).expect_err("expected error");
        assert!(matches!(
            err,
            LowerError::UnsupportedDecl {
                kind: "function with default params",
                ..
            }
        ));
    }

    #[test]
    fn lower_modules_allocates_air_modules() {
        let root = parse_program("");
        let resolved = resolved_with_core_option(&root);
        let modules = SourceModules::new(&root, &resolved);
        let mut cx = LowerCx::default();
        cx.lower_modules(&modules);
        assert_eq!(cx.program.modules.len(), modules.items.len());
        assert_eq!(
            cx.maps.modules[&modules.items[0].scope],
            ModuleId::from_index(0)
        );
        verify(&cx.program).expect("module-only AIR should verify");
    }

    #[test]
    fn concrete_function_lowers_to_verified_body() {
        let (root, resolved, semantic) = checked("fn f(var a: int) -> int { a }");
        let air = lower_with_modules(&root, &resolved, &semantic.program).expect("lower failed");
        assert_eq!(air.functions.len(), 1);
        let (module_id, module) = air
            .modules
            .iter()
            .enumerate()
            .find(|(_, module)| !module.functions.is_empty())
            .expect("missing function module");
        let function_id = module.functions[0];
        let function = air.function(function_id);
        assert_eq!(function.module, ModuleId::from_index(module_id));
        assert_eq!(function.signature.params.len(), 1);
        let param = &function.signature.params[0];
        let local = &function.locals[param.local_id.index()];
        assert_eq!(local.kind, LocalKind::Arg);
        assert_eq!(local.mutability, AirMutability::Mutable);
        assert!(matches!(
            function.body[0].terminator,
            Terminator::Return(Some(_))
        ));
    }

    #[test]
    fn empty_void_function_lowers_to_fallthrough_return() {
        let (root, resolved, semantic) = checked("fn f() {}");
        let air = lower_with_modules(&root, &resolved, &semantic.program).expect("lower failed");
        assert!(matches!(
            air.functions[0].body[0].terminator,
            Terminator::Return(None)
        ));
    }

    #[test]
    fn lowers_slice1_smoke() {
        let source = r"
fn f(a: int) -> int {
    let one = 1;
    var b = a + one;
    b += 3;
    b = (b as int) + (2.0 as int);
    let _ = true == false;
    b
}
";
        let (root, resolved, semantic) = checked(source);
        let air = lower_with_modules(&root, &resolved, &semantic.program).expect("lower failed");
        assert_eq!(air.functions.len(), 1);
        let function = &air.functions[0];
        assert!(
            function
                .body
                .iter()
                .flat_map(|block| &block.statements)
                .any(|stmt| matches!(stmt, Statement::Assign { .. }))
        );
        assert!(matches!(
            function.body[0].terminator,
            Terminator::Return(Some(_))
        ));
    }

    #[test]
    fn nested_value_block_does_not_terminate_function() {
        let source = r"
fn f() -> int {
    let x = { let y = 1; y };
    x + 1
}
";
        let (root, resolved, semantic) = checked(source);
        let air = lower_with_modules(&root, &resolved, &semantic.program).expect("lower failed");
        let function = &air.functions[0];
        assert!(matches!(
            function.body[0].terminator,
            Terminator::Return(Some(_))
        ));
    }

    #[test]
    fn rejects_deferred_string_concat() {
        let source = r#"fn f() -> string { "a" + "b" }"#;
        let (root, resolved, semantic) = checked(source);
        let err =
            lower_with_modules(&root, &resolved, &semantic.program).expect_err("expected error");
        assert!(matches!(
            err,
            LowerError::UnsupportedExpr { kind: "Binary", .. }
        ));
    }

    #[test]
    fn rejects_unsupported_top_level_declarations() {
        let (root, resolved, semantic) = checked("struct S { x: int }");
        let err =
            lower_with_modules(&root, &resolved, &semantic.program).expect_err("expected error");
        assert!(matches!(
            err,
            LowerError::UnsupportedDecl {
                kind: "Aggregate",
                ..
            }
        ));
    }

    #[test]
    fn rejects_function_value_read_as_unsupported() {
        let source = "fn g() -> int { 1 } fn f() -> void { g; }";
        let (root, resolved, semantic) = checked(source);
        let err =
            lower_with_modules(&root, &resolved, &semantic.program).expect_err("expected error");
        assert!(matches!(
            err,
            LowerError::UnsupportedExpr { kind: "Ident", .. }
        ));
    }

    #[test]
    fn type_lowerer_caches_primitives() {
        let mut cx = LowerCx::default();
        let first = cx.lower_ty(&Type::Int).expect("lower int");
        let second = cx.lower_ty(&Type::Int).expect("lower int again");
        let float = cx.lower_ty(&Type::Float).expect("lower float");
        assert_eq!(first, second);
        assert_ne!(first, float);
        verify(&cx.program).expect("type arena should verify");
    }

    #[test]
    fn type_lowerer_rejects_unsupported_types() {
        let mut cx = LowerCx::default();
        let err = cx
            .lower_ty(&Type::Any)
            .expect_err("expected unsupported type");
        assert!(matches!(err, LowerError::UnsupportedType { ty: Type::Any }));
    }

    fn checked(source: &str) -> (ast::Program, ResolveResult, typecheck::SemanticCheckOutput) {
        let root = parse_program(source);
        let resolved = resolved_with_core_option(&root);
        let externs = externs::collect_source_externs(&root, &resolved).unwrap();
        let semantic = typecheck::check_semantic_with_modules(
            &root,
            &resolved,
            externs,
            TypecheckConfig::default(),
        )
        .expect("typecheck failed");
        (root, resolved, semantic)
    }
}
