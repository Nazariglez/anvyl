use std::collections::HashMap;

use anvyx_externs::ParamFlow;

use super::{
    BasicBlock, BlockId, Callee, ConstData, ConstValue, ExternDecl, ExternId, ExternMember,
    Function, FunctionId, FunctionKind, Local, LocalId, LocalKind, Module, ModuleId,
    Mutability as AirMutability, Operand, Param, ParamRole, Place, Program, RValue, Signature,
    Statement, Terminator, TypeData, TypeId, VerifyError,
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
        BodyInstanceKey, CallForm, CallableId, CallableInstanceKey, DeclarationIndex,
        DefaultArgFact, ExternUseTarget, GenericArgs, LocalDefFact, LocalDefKind, LocalUseFact,
        LocalUseMode, ModuleScope, SemanticBodyFacts, SemanticFunctionInstanceFact,
        SemanticLocalId, SemanticProgram,
    },
};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum LowerError {
    MissingBodyFacts {
        body: Box<BodyInstanceKey>,
    },
    MissingExprType {
        body: Box<BodyInstanceKey>,
        expr_id: ExprId,
    },
    MissingLocalDef {
        body: Box<BodyInstanceKey>,
        local: SemanticLocalId,
    },
    MissingBindingDef {
        body: Box<BodyInstanceKey>,
        span: SourceSpan,
    },
    MissingParamDef {
        body: Box<BodyInstanceKey>,
        index: usize,
    },
    MissingLocalUse {
        body: Box<BodyInstanceKey>,
        expr_id: ExprId,
    },
    UnsupportedType {
        ty: Box<Type>,
    },
    UnsupportedExternUse {
        expr_id: ExprId,
    },
    UnsupportedExternSignature,
    UnsupportedDefaultArg {
        call: ExprId,
        param_index: usize,
        expr_id: ExprId,
    },
    UnsupportedDecl {
        kind: &'static str,
        span: Option<SourceSpan>,
    },
    MissingFunctionFact {
        id: Box<CallableId>,
        args: Box<GenericArgs>,
    },
    MissingSpecializedBodyFacts {
        body: Box<BodyInstanceKey>,
    },
    MissingGenericInstanceArgs {
        id: Box<CallableId>,
    },
    UnsupportedCallableInstance {
        id: Box<CallableId>,
        args: Box<GenericArgs>,
    },
    UnsupportedCallForm {
        expr_id: ExprId,
    },
    MissingLoweredCallee {
        body: Box<BodyInstanceKey>,
    },
    UnsupportedStmt {
        kind: &'static str,
        span: Option<SourceSpan>,
    },
    UnsupportedExpr {
        expr_id: ExprId,
        kind: &'static str,
    },
    UnsupportedStringifyType {
        expr_id: ExprId,
        ty: Box<Type>,
    },
    UnterminatedBlock,
    Verify(Box<[VerifyError]>),
    AnyTypeEmitted(TypeId),
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
            _ => {
                return Err(LowerError::UnsupportedType {
                    ty: Box::new(ty.clone()),
                });
            }
        };
        Ok(*slot.get_or_insert_with(|| program.alloc_type(data)))
    }
}

#[derive(Debug, Default)]
struct LoweringMaps {
    modules: HashMap<ModuleScope, ModuleId>,
    bodies: HashMap<BodyInstanceKey, FunctionId>,
    locals: HashMap<BodyInstanceKey, HashMap<SemanticLocalId, LocalId>>,
    externs: HashMap<crate::externs::catalog::ExternFunctionId, ExternId>,
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

    fn lower_extern_declarations(
        &mut self,
        functions: &SourceFunctions<'_>,
        semantic: &SemanticProgram,
    ) -> Result<(), LowerError> {
        for source in &functions.items {
            let Some(facts) = semantic.facts.body(&source.body) else {
                continue;
            };
            let mut externs = facts
                .extern_uses
                .values()
                .flatten()
                .filter_map(|target| match target {
                    ExternUseTarget::Function(id) => Some(*id),
                    _ => None,
                })
                .collect::<Vec<_>>();
            externs.sort_by_key(|id| {
                let function = semantic.externs.function(*id);
                format!("{:?}::{}", function.key.module, function.key.name.as_str())
            });
            externs.dedup();
            for id in externs {
                if self.maps.externs.contains_key(&id) {
                    continue;
                }
                let function = semantic.externs.function(id);
                let module_scope = &function.key.module;
                let module = match self.maps.modules.get(module_scope).copied() {
                    Some(module) => module,
                    None => {
                        let path = module_path(module_scope);
                        let module = self.program.alloc_module(Module {
                            path,
                            functions: vec![],
                            aggregates: vec![],
                            enums: vec![],
                            extern_types: vec![],
                            externs: vec![],
                        });
                        self.maps.modules.insert(module_scope.clone(), module);
                        module
                    }
                };
                let params = function
                    .signature
                    .params
                    .iter()
                    .map(|param| {
                        if param.flow != ParamFlow::Value
                            || param.escape != ast::EscapeMode::NonEscaping
                        {
                            return Err(LowerError::UnsupportedExternSignature);
                        }
                        self.lower_ty(&param.ty.ty)
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let return_type = self.lower_ty(&function.signature.ret.ty)?;
                let air_id = self.program.alloc_extern(ExternDecl {
                    name: function.key.name,
                    module,
                    member: ExternMember::FreeFunction,
                    params,
                    return_type,
                });
                self.program.module_mut(module).externs.push(air_id);
                self.maps.externs.insert(id, air_id);
            }
        }
        Ok(())
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
                None if source.can_omit_body_facts() => {
                    empty_facts = SemanticBodyFacts::default();
                    &empty_facts
                }
                None => {
                    return Err(LowerError::MissingBodyFacts {
                        body: Box::new(source.body.clone()),
                    });
                }
            };
            reject_unsupported_stringifies(body_facts)?;
            let return_type = self.lower_ty(&source.fact.return_ty)?;
            let mut params = vec![];
            let mut locals = vec![];
            let mut local_map = HashMap::new();
            for (index, param_fact) in source.fact.params.iter().enumerate() {
                let semantic_local = body_facts
                    .locals
                    .param_defs
                    .get(&index)
                    .copied()
                    .ok_or_else(|| LowerError::MissingParamDef {
                        body: Box::new(source.body.clone()),
                        index,
                    })?;
                let def = body_facts.locals.defs.get(&semantic_local).ok_or_else(|| {
                    LowerError::MissingLocalDef {
                        body: Box::new(source.body.clone()),
                        local: semantic_local,
                    }
                })?;
                debug_assert_eq!(def.kind, LocalDefKind::Parameter);
                debug_assert_eq!(def.name, param_fact.name);
                debug_assert_eq!(def.ty, param_fact.ty);
                debug_assert_eq!(def.mutable, param_fact.mutable);
                let ty = self.lower_ty(&param_fact.ty)?;
                let local_id = LocalId::from_index(locals.len());
                locals.push(Local {
                    name: Some(param_fact.name),
                    ty,
                    mutability: if param_fact.mutable {
                        AirMutability::Mutable
                    } else {
                        AirMutability::Immutable
                    },
                    kind: LocalKind::Arg,
                });
                let old = local_map.insert(semantic_local, local_id);
                debug_assert!(old.is_none(), "duplicate semantic param local");
                params.push(Param {
                    name: Some(param_fact.name),
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
                None if source.can_omit_body_facts() => {
                    empty_facts = SemanticBodyFacts::default();
                    &empty_facts
                }
                None => {
                    return Err(LowerError::MissingBodyFacts {
                        body: Box::new(source.body.clone()),
                    });
                }
            };
            let function = self.maps.bodies[&source.body];
            let locals = self
                .maps
                .locals
                .remove(&source.body)
                .expect("lowered function missing local map");
            let mut lowerer =
                FunctionLowerer::new(self, functions, source, facts, function, locals);
            lowerer.lower_body(&source.func.node.body)?;
        }
        Ok(())
    }
}

struct FunctionLowerer<'cx, 'facts> {
    cx: &'cx mut LowerCx,
    body: BodyInstanceKey,
    facts: &'facts SemanticBodyFacts,
    default_exprs: &'facts HashMap<(CallableId, SourceId, ExprId), &'facts ExprNode>,
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
        functions: &'facts SourceFunctions<'facts>,
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
            default_exprs: &functions.default_exprs,
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
            return Err(unsupported_expr(expr));
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
                        body: Box::new(self.body.clone()),
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
                    .ok_or_else(|| unsupported_expr(expr))?;
                let result_scalar = source_scalar(&ty).ok_or_else(|| unsupported_expr(expr))?;
                if !typing::supports_scalar_unary(unary.node.op, value_scalar, result_scalar) {
                    return Err(unsupported_expr(expr));
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
                    return Err(unsupported_expr(expr));
                };
                if !typing::supports_scalar_binary(
                    binary.node.op,
                    lhs_scalar,
                    rhs_scalar,
                    result_scalar,
                ) {
                    return Err(unsupported_expr(expr));
                }
                let ty = self.cx.lower_ty(&result_ty)?;
                self.emit_temp(RValue::Binary {
                    op: binary.node.op,
                    lhs,
                    rhs,
                    ty,
                })
            }
            ExprKind::Call(call) => self.lower_call_value(expr, &call.node.args),
            ExprKind::IntrinsicCall(call) => self.lower_intrinsic_value(expr, call),
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
                    return Err(unsupported_expr(expr));
                }
                let target = self.cx.lower_ty(&target_ty)?;
                self.emit_temp(RValue::Cast { value, target })
            }
            _ => Err(unsupported_expr(expr)),
        }
    }

    fn lower_intrinsic_value(
        &mut self,
        expr: &ExprNode,
        call: &ast::IntrinsicCallNode,
    ) -> Result<Operand, LowerError> {
        let Some(fact) = self.facts.stringifies.get(&expr.node.id) else {
            return Err(unsupported_expr(expr));
        };
        let [arg] = call.node.args.as_slice() else {
            return Err(unsupported_expr(expr));
        };
        if fact.arg != arg.node.id {
            return Err(unsupported_expr(expr));
        }
        let result_ty = self.cx.lower_ty(&Type::String)?;
        let source_ty = self.cx.lower_ty(&fact.source_ty)?;
        let value = self.lower_value(arg)?;
        self.emit_typed_temp(result_ty, RValue::Stringify { value, source_ty })
    }

    fn lower_call_value(
        &mut self,
        expr: &ExprNode,
        args: &[ExprNode],
    ) -> Result<Operand, LowerError> {
        let value = self.lower_call_rvalue(expr, args)?;
        if self.lower_expr_ty(expr.node.id)? == Type::Void {
            return Err(unsupported_expr(expr));
        }
        self.emit_temp(value)
    }

    fn lower_call_rvalue(
        &mut self,
        expr: &ExprNode,
        args: &[ExprNode],
    ) -> Result<RValue, LowerError> {
        if let Some(targets) = self.facts.extern_uses.get(&expr.node.id) {
            let [ExternUseTarget::Function(id)] = targets.as_slice() else {
                return Err(LowerError::UnsupportedExternUse {
                    expr_id: expr.node.id,
                });
            };
            let Some(callee) = self.cx.maps.externs.get(id).copied() else {
                return Err(LowerError::UnsupportedExternUse {
                    expr_id: expr.node.id,
                });
            };
            let args = args
                .iter()
                .map(|arg| self.lower_value(arg))
                .collect::<Result<Vec<_>, _>>()?;
            self.require_call_arity(expr.node.id, &Callee::Extern(callee), args.len())?;
            return Ok(RValue::Call {
                callee: Callee::Extern(callee),
                args,
            });
        }

        let target = self
            .facts
            .calls
            .get(&expr.node.id)
            .ok_or_else(|| unsupported_expr(expr))?;
        if target.form != CallForm::Normal {
            return Err(LowerError::UnsupportedCallForm {
                expr_id: expr.node.id,
            });
        }
        let body = BodyInstanceKey::Callable(CallableInstanceKey {
            target: target.id.clone(),
            args: target.args.clone(),
        });
        let Some(callee) = self.cx.maps.bodies.get(&body).copied() else {
            return Err(LowerError::MissingLoweredCallee {
                body: Box::new(body),
            });
        };
        let mut operands = args
            .iter()
            .map(|arg| self.lower_value(arg))
            .collect::<Result<Vec<_>, _>>()?;
        let expected = self.cx.program.function(callee).signature.params.len();
        operands.extend(self.lower_default_args(expr.node.id, args.len(), expected)?);
        self.require_call_arity(expr.node.id, &Callee::Function(callee), operands.len())?;
        Ok(RValue::Call {
            callee: Callee::Function(callee),
            args: operands,
        })
    }

    fn require_call_arity(
        &self,
        call: ExprId,
        callee: &Callee,
        found: usize,
    ) -> Result<(), LowerError> {
        let expected = match *callee {
            Callee::Function(id) => self.cx.program.function(id).signature.params.len(),
            Callee::Extern(id) => self.cx.program.extern_decl(id).params.len(),
            Callee::Closure(_) => return Ok(()),
        };
        if expected == found {
            Ok(())
        } else {
            Err(LowerError::UnsupportedExpr {
                expr_id: call,
                kind: "Call",
            })
        }
    }

    fn lower_default_args(
        &mut self,
        call: ExprId,
        provided: usize,
        expected: usize,
    ) -> Result<Vec<Operand>, LowerError> {
        if provided >= expected {
            return Ok(vec![]);
        }
        let mut defaults = self.facts.default_args.get(&call).cloned().ok_or(
            LowerError::UnsupportedDefaultArg {
                call,
                param_index: provided,
                expr_id: call,
            },
        )?;
        if defaults.len() != expected - provided {
            return Err(LowerError::UnsupportedDefaultArg {
                call,
                param_index: provided,
                expr_id: call,
            });
        }
        defaults.sort_by_key(|fact| fact.param_index);
        defaults
            .iter()
            .map(|fact| self.lower_default_arg(fact))
            .collect()
    }

    fn lower_default_arg(&mut self, fact: &DefaultArgFact) -> Result<Operand, LowerError> {
        let error = || LowerError::UnsupportedDefaultArg {
            call: fact.call,
            param_index: fact.param_index,
            expr_id: fact.default.expr,
        };
        let Some(expr) = self.default_exprs.get(&(
            fact.callee.target.clone(),
            fact.default.source,
            fact.default.expr,
        )) else {
            return Err(error());
        };
        let ExprKind::Lit(lit) = &expr.node.kind else {
            return Err(error());
        };
        let Some(value) = Self::literal_const_value(lit, &fact.ty) else {
            return Err(error());
        };
        let ty = self.cx.lower_ty(&fact.ty)?;
        Ok(Operand::Const(
            self.cx.program.alloc_const(ConstData { ty, value }),
        ))
    }

    fn lower_lit(&mut self, expr: &ExprNode, lit: &Lit) -> Result<Operand, LowerError> {
        let ty = self.lower_expr_ty(expr.node.id)?;
        let ty_id = self.cx.lower_ty(&ty)?;
        let Some(value) = Self::literal_const_value(lit, &ty) else {
            return Err(unsupported_expr(expr));
        };
        Ok(Operand::Const(
            self.cx.program.alloc_const(ConstData { ty: ty_id, value }),
        ))
    }

    fn literal_const_value(lit: &Lit, ty: &Type) -> Option<ConstValue> {
        match (lit, ty) {
            (Lit::Int(value), Type::Int) => Some(ConstValue::Int(*value)),
            (Lit::Float(value), Type::Float) => Some(ConstValue::Float(*value)),
            (Lit::Bool(value), Type::Bool) => Some(ConstValue::Bool(*value)),
            (Lit::String(value), Type::String) => {
                Some(ConstValue::String(value.clone().into_boxed_str()))
            }
            _ => None,
        }
    }

    fn lower_effect(&mut self, expr: &ExprNode) -> Result<(), LowerError> {
        match &expr.node.kind {
            ExprKind::Assign(assign) => self.lower_assign(expr, assign),
            ExprKind::Block(block) => self.lower_block_effect(block),
            ExprKind::Call(call) => {
                let value = self.lower_call_rvalue(expr, &call.node.args)?;
                self.emit_eval(value)
            }
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
            return Err(unsupported_expr(&assign.node.target));
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
                    return Err(unsupported_expr(&assign.node.target));
                };
                if !typing::supports_scalar_binary(binary, lhs_scalar, rhs_scalar, result_scalar) {
                    return Err(unsupported_expr(&assign.node.target));
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
            _ => Err(unsupported_expr(expr)),
        }
    }

    fn require_builtin_scalar(&self, expr: &ExprNode) -> Result<(), LowerError> {
        if self.has_deferred_expr_fact(expr.node.id) {
            return Err(unsupported_expr(expr));
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
                body: Box::new(self.body.clone()),
                expr_id,
            })
    }

    fn local_use(&self, expr: &ExprNode, mode: LocalUseMode) -> Result<LocalUseFact, LowerError> {
        let expr_id = expr.node.id;
        let Some(fact) = self.facts.locals.uses.get(&expr_id) else {
            if self.has_deferred_expr_fact(expr_id)
                || self.expr_type_has_no_local_identity(expr_id)?
            {
                return Err(unsupported_expr(expr));
            }
            return Err(LowerError::MissingLocalUse {
                body: Box::new(self.body.clone()),
                expr_id,
            });
        };
        if fact.mode != mode {
            return Err(LowerError::MissingLocalUse {
                body: Box::new(self.body.clone()),
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
                body: Box::new(self.body.clone()),
                local,
            })
    }

    fn local(&self, local: SemanticLocalId) -> Result<LocalId, LowerError> {
        self.locals
            .get(&local)
            .copied()
            .ok_or_else(|| LowerError::MissingLocalDef {
                body: Box::new(self.body.clone()),
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
        let ty = typing::rvalue_ty(&self.cx.program, &primitives, &value).ok_or_else(|| {
            LowerError::UnsupportedType {
                ty: Box::new(Type::Infer),
            }
        })?;
        self.emit_typed_temp(ty, value)
    }

    fn emit_typed_temp(&mut self, ty: TypeId, value: RValue) -> Result<Operand, LowerError> {
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
}

fn reject_unsupported_stringifies(facts: &SemanticBodyFacts) -> Result<(), LowerError> {
    facts
        .stringifies
        .iter()
        .filter(|(_, fact)| !stringify_source_ty_supported(&fact.source_ty))
        .min_by_key(|(call, _)| call.0)
        .map_or(Ok(()), |(call, fact)| {
            Err(LowerError::UnsupportedStringifyType {
                expr_id: *call,
                ty: Box::new(fact.source_ty.clone()),
            })
        })
}

fn stringify_source_ty_supported(ty: &Type) -> bool {
    matches!(ty, Type::Int | Type::Float | Type::Bool | Type::String)
}

fn unsupported_expr(expr: &ExprNode) -> LowerError {
    LowerError::UnsupportedExpr {
        expr_id: expr.node.id,
        kind: expr.node.kind.variant_name(),
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
    cx.lower_extern_declarations(&functions, semantic)?;
    cx.lower_function_shells(&modules, &functions, semantic)?;
    cx.lower_function_bodies(&functions, semantic)?;
    verify(&cx.program).map_err(|errors| LowerError::Verify(errors.into_boxed_slice()))?;
    reject_any_types(&cx.program)?;
    Ok(cx.program)
}

fn reject_any_types(program: &Program) -> Result<(), LowerError> {
    for (index, ty) in program.type_arena.iter().enumerate() {
        if matches!(ty, TypeData::Any) {
            return Err(LowerError::AnyTypeEmitted(TypeId::from_index(index)));
        }
    }
    Ok(())
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
    default_exprs: HashMap<(CallableId, SourceId, ExprId), &'a ExprNode>,
}

#[derive(Debug)]
struct SourceFunction<'a> {
    module: usize,
    func: &'a ast::FuncNode,
    body: BodyInstanceKey,
    fact: &'a SemanticFunctionInstanceFact,
    source: SourceId,
}

impl SourceFunction<'_> {
    fn can_omit_body_facts(&self) -> bool {
        can_omit_body_facts(self.fact, self.func)
    }
}

fn can_omit_body_facts(fact: &SemanticFunctionInstanceFact, func: &ast::FuncNode) -> bool {
    fact.params.is_empty()
        && fact.return_ty == Type::Void
        && func.node.body.node.stmts.is_empty()
        && func.node.body.node.tail.is_none()
}

impl<'a> SourceFunctions<'a> {
    fn new(
        modules: &'a SourceModules<'a>,
        semantic: &'a SemanticProgram,
    ) -> Result<Self, LowerError> {
        let mut function_index = HashMap::new();
        let mut default_exprs = HashMap::new();
        let mut worklist = vec![];
        let mut queued = std::collections::HashSet::new();

        for (module_index, module) in modules.items.iter().enumerate() {
            for stmt in &module.program.stmts {
                let Stmt::Func(func_node) = &stmt.node else {
                    if !module.system
                        && !matches!(&stmt.node, Stmt::Import(_) | Stmt::ExternFunc(_))
                    {
                        return Err(LowerError::UnsupportedDecl {
                            kind: stmt_kind(&stmt.node),
                            span: Some(SourceSpan::from_byte_span(module.source, stmt.span)),
                        });
                    }
                    continue;
                };
                let id = CallableId::function(module.scope.clone(), func_node.node.name);
                for param in &func_node.node.params {
                    if let Some(default) = &param.default {
                        default_exprs.insert((id.clone(), module.source, default.node.id), default);
                    }
                }
                function_index.insert(id.clone(), (module_index, func_node));
                if !module.system
                    && func_node.node.type_params.is_empty()
                    && func_node.node.const_params.is_empty()
                {
                    let key = CallableInstanceKey {
                        target: id,
                        args: GenericArgs::default(),
                    };
                    queued.insert(key.clone());
                    worklist.push(key);
                }
            }
        }

        let mut items = vec![];
        let mut index = 0;
        while let Some(key) = worklist.get(index).cloned() {
            index += 1;
            let Some((module_index, func_node)) = function_index.get(&key.target).copied() else {
                return Err(LowerError::UnsupportedCallableInstance {
                    id: Box::new(key.target.clone()),
                    args: Box::new(key.args.clone()),
                });
            };
            let module = &modules.items[module_index];
            let func = &func_node.node;
            if (!func.type_params.is_empty() || !func.const_params.is_empty())
                && key.args.is_empty()
            {
                return Err(LowerError::MissingGenericInstanceArgs {
                    id: Box::new(key.target.clone()),
                });
            }
            let body = BodyInstanceKey::Callable(key.clone());
            let Some(fact) = semantic
                .declaration_facts
                .functions
                .iter()
                .find(|fact| fact.id == key.target && fact.args == key.args)
            else {
                return Err(LowerError::MissingFunctionFact {
                    id: Box::new(key.target.clone()),
                    args: Box::new(key.args.clone()),
                });
            };
            let facts = match semantic.facts.body(&body) {
                Some(facts) => Some(facts),
                None if can_omit_body_facts(fact, func_node) => None,
                None => {
                    return Err(LowerError::MissingSpecializedBodyFacts {
                        body: Box::new(body.clone()),
                    });
                }
            };
            if let Some(facts) = facts {
                let mut calls = facts.calls.iter().collect::<Vec<_>>();
                calls.sort_by_key(|(expr, _)| expr.0);
                for (expr, target) in calls {
                    if target.form != CallForm::Normal {
                        return Err(LowerError::UnsupportedCallForm { expr_id: *expr });
                    }
                    let called = CallableInstanceKey {
                        target: target.id.clone(),
                        args: target.args.clone(),
                    };
                    if queued.insert(called.clone()) {
                        worklist.push(called);
                    }
                }
            }
            items.push(SourceFunction {
                module: module_index,
                func: func_node,
                body,
                fact,
                source: module.source,
            });
        }

        Ok(Self {
            items,
            default_exprs,
        })
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
    use anvyx_externs::{
        CallbackEscape, ExternEffects, ExternFunctionDescriptor, ExternModuleDescriptor,
        ExternParam, ExternSignature, ExternTypeExpr, ProviderDescriptor, ProviderId,
    };

    use super::*;
    use crate::{
        externs,
        externs::{ExternInputs, PackageExternInputs},
        test_support::{
            parse_program, resolved_modules_with_core_option,
            resolved_modules_with_core_option_external, resolved_with_core_option,
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
    fn source_functions_include_called_generic_instance() {
        let (root, resolved, semantic) =
            checked("fn f<T>(x: T) -> T { x } fn main() -> int { f(1) }");
        let modules = SourceModules::new(&root, &resolved);
        let functions =
            SourceFunctions::new(&modules, &semantic.program).expect("inventory failed");

        assert!(functions.items.iter().any(|function| {
            function.func.node.name == Ident::new("f")
                && function.fact.args.type_args == vec![Type::Int]
        }));
    }

    #[test]
    fn generic_function_call_lowers_once() {
        let source = "fn id<T>(x: T) -> T { x } fn f() -> int { id(1) + id(2) }";
        let (root, resolved, semantic) = checked(source);
        let air = lower_with_modules(&root, &resolved, &semantic.program).expect("lower failed");

        let id_functions = air
            .functions
            .iter()
            .filter(|function| function.name == Ident::new("id"))
            .collect::<Vec<_>>();
        assert_eq!(id_functions.len(), 1);
        assert_eq!(id_functions[0].signature.params.len(), 1);
        assert!(matches!(
            air.type_arena.get(id_functions[0].signature.return_type),
            Some(TypeData::Int)
        ));
    }

    #[test]
    fn generic_identity_lowers_independent_instances() {
        let source =
            r#"fn id<T>(x: T) -> T { x } fn f() -> int { id(1) } fn g() -> string { id("x") }"#;
        let (root, resolved, semantic) = checked(source);
        let air = lower_with_modules(&root, &resolved, &semantic.program).expect("lower failed");

        let id_functions = air
            .functions
            .iter()
            .filter(|function| function.name == Ident::new("id"))
            .collect::<Vec<_>>();
        assert_eq!(id_functions.len(), 2);
        assert!(id_functions.iter().any(|function| matches!(
            air.type_arena.get(function.signature.return_type),
            Some(TypeData::Int)
        )));
        assert!(id_functions.iter().any(|function| matches!(
            air.type_arena.get(function.signature.return_type),
            Some(TypeData::String)
        )));
    }

    #[test]
    fn generic_wrapper_calls_generic_helper() {
        let source =
            "fn id<T>(x: T) -> T { x } fn wrap<T>(x: T) -> T { id(x) } fn f() -> int { wrap(1) }";
        let (root, resolved, semantic) = checked(source);
        let air = lower_with_modules(&root, &resolved, &semantic.program).expect("lower failed");

        let wrap = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("wrap"))
            .expect("missing wrap");
        assert!(wrap.body.iter().any(|block| block.statements.iter().any(|statement| {
            matches!(statement, Statement::Init { value: RValue::Call { callee: Callee::Function(_), args }, .. } if args.len() == 1)
        })));
    }

    #[test]
    fn uncalled_generic_function_is_not_lowered() {
        let (root, resolved, semantic) = checked("fn id<T>(x: T) -> T { x } fn main() {}");
        let air = lower_with_modules(&root, &resolved, &semantic.program).expect("lower failed");

        assert!(
            air.functions
                .iter()
                .all(|function| function.name != Ident::new("id"))
        );
    }

    #[test]
    fn generic_call_order_drives_function_order() {
        let source = r#"fn id<T>(x: T) -> T { x } fn f() { let a = id("x"); let b = id(1); }"#;
        let (root, resolved, semantic) = checked(source);
        let air = lower_with_modules(&root, &resolved, &semantic.program).expect("lower failed");
        let id_returns = air
            .functions
            .iter()
            .filter(|function| function.name == Ident::new("id"))
            .map(|function| air.type_arena.get(function.signature.return_type))
            .collect::<Vec<_>>();

        assert_eq!(id_returns.len(), 2);
        assert!(matches!(id_returns[0], Some(TypeData::String)));
        assert!(matches!(id_returns[1], Some(TypeData::Int)));
    }

    #[test]
    fn local_function_call_is_unsupported_callable_instance() {
        let source = "fn f() -> int { fn inner() -> int { 1 } inner() }";
        let (root, resolved, semantic) = checked(source);
        let err = lower_with_modules(&root, &resolved, &semantic.program)
            .expect_err("expected unsupported local function");

        assert!(matches!(
            err,
            LowerError::UnsupportedCallableInstance { .. }
        ));
    }

    #[test]
    fn missing_generic_body_facts_are_reported() {
        let (root, resolved, mut semantic) =
            checked("fn id<T>(x: T) -> T { x } fn f() -> int { id(1) }");
        let body = BodyInstanceKey::Callable(CallableInstanceKey {
            target: CallableId::function(ModuleScope::Root, Ident::new("id")),
            args: GenericArgs {
                type_args: vec![Type::Int],
                const_args: vec![],
            },
        });
        semantic.program.facts.bodies.remove(&body);
        let err = lower_with_modules(&root, &resolved, &semantic.program)
            .expect_err("expected missing body facts");

        assert!(matches!(
            err,
            LowerError::MissingSpecializedBodyFacts { .. }
        ));
    }

    #[test]
    fn missing_non_empty_zero_param_body_facts_are_reported() {
        let (root, resolved, mut semantic) = checked("fn f() -> int { 1 }");
        let body = BodyInstanceKey::Callable(CallableInstanceKey {
            target: CallableId::function(ModuleScope::Root, Ident::new("f")),
            args: GenericArgs::default(),
        });
        semantic.program.facts.bodies.remove(&body);
        let err = lower_with_modules(&root, &resolved, &semantic.program)
            .expect_err("expected missing body facts");

        assert!(matches!(
            err,
            LowerError::MissingSpecializedBodyFacts { .. }
        ));
    }

    #[test]
    fn empty_no_param_generic_void_specialization_can_omit_body_facts() {
        let source = "fn noop<T>() {} fn main() { noop<int>(); }";
        let (root, resolved, semantic) = checked(source);

        lower_with_modules(&root, &resolved, &semantic.program).expect("lower failed");
    }

    #[test]
    fn queued_generic_template_without_instance_args_is_explicit_error() {
        let (root, resolved, mut semantic) =
            checked("fn id<T>(x: T) -> T { x } fn main() { let x = 1; }");
        let id = CallableId::function(ModuleScope::Root, Ident::new("id"));
        let args = GenericArgs::default();
        let main_body = BodyInstanceKey::Callable(CallableInstanceKey {
            target: CallableId::function(ModuleScope::Root, Ident::new("main")),
            args: GenericArgs::default(),
        });
        semantic
            .program
            .facts
            .bodies
            .get_mut(&main_body)
            .expect("main body facts")
            .calls
            .insert(
                ExprId(u64::MAX),
                typecheck::CallTarget::new(id.clone(), args.clone()),
            );
        semantic
            .program
            .declaration_facts
            .functions
            .push(SemanticFunctionInstanceFact {
                id: id.clone(),
                args: args.clone(),
                body: BodyInstanceKey::Callable(CallableInstanceKey { target: id, args }),
                module: ModuleScope::Root,
                name: Ident::new("id"),
                span: semantic.program.declaration_facts.functions[0].span,
                body_span: semantic.program.declaration_facts.functions[0].body_span,
                params: vec![],
                return_ty: Type::Infer,
            });
        let err = lower_with_modules(&root, &resolved, &semantic.program)
            .expect_err("expected missing generic args");

        assert!(matches!(err, LowerError::MissingGenericInstanceArgs { .. }));
    }

    #[test]
    fn stringify_int_lowers_to_string_typed_air() {
        let source = "fn f() -> string { #stringify(1) }";
        let (root, resolved, semantic) = checked(source);
        let air = lower_with_modules(&root, &resolved, &semantic.program).expect("lower failed");
        let string_ty = PrimitiveTypes::scan(&air).string().expect("string type");

        assert!(air.functions.iter().any(|function| {
            function.body.iter().any(|block| {
                block.statements.iter().any(|statement| {
                    matches!(
                        statement,
                        Statement::Init {
                            value: RValue::Stringify { value: _, source_ty },
                            ..
                        } if *source_ty != string_ty
                    )
                })
            })
        }));
    }

    #[test]
    fn non_stringify_intrinsic_is_unsupported() {
        let source = "fn f() -> string { #file() }";
        let (root, resolved, semantic) = checked(source);
        let err = lower_with_modules(&root, &resolved, &semantic.program)
            .expect_err("expected unsupported intrinsic");

        assert!(matches!(
            err,
            LowerError::UnsupportedExpr {
                kind: "IntrinsicCall",
                ..
            }
        ));
    }

    #[test]
    fn generic_stringify_lowers_specialized_source_type() {
        let source = "fn f<T>(x: T) -> string { #stringify(x) } fn main() -> string { f(1) }";
        let (root, resolved, semantic) = checked(source);
        let air = lower_with_modules(&root, &resolved, &semantic.program).expect("lower failed");
        let int_ty = PrimitiveTypes::scan(&air).int().expect("int type");

        assert!(air.functions.iter().any(|function| {
            function.name == Ident::new("f")
                && function.body.iter().any(|block| {
                    block.statements.iter().any(|statement| {
                        matches!(
                            statement,
                            Statement::Init {
                                value: RValue::Stringify { value: _, source_ty },
                                ..
                            } if *source_ty == int_ty
                        )
                    })
                })
        }));
    }

    #[test]
    fn unsupported_stringify_type_is_explicit_error() {
        let source = "fn g() {} fn f() -> string { #stringify(g) }";
        let (root, resolved, semantic) = checked(source);
        let err = lower_with_modules(&root, &resolved, &semantic.program)
            .expect_err("expected unsupported stringify type");

        assert!(matches!(err, LowerError::UnsupportedStringifyType { .. }));
    }

    #[test]
    fn generic_unsupported_stringify_type_is_reported_before_shell_type_lowering() {
        let source =
            "fn g() {} fn f<T>(x: T) -> string { #stringify(x) } fn main() -> string { f(g) }";
        let (root, resolved, semantic) = checked(source);
        let err = lower_with_modules(&root, &resolved, &semantic.program)
            .expect_err("expected unsupported stringify type");

        assert!(matches!(err, LowerError::UnsupportedStringifyType { .. }));
    }

    #[test]
    fn source_functions_keep_default_param_instances() {
        let (root, resolved, semantic) = checked("fn f(a: int = 1) -> int { a }");
        let modules = SourceModules::new(&root, &resolved);
        let functions =
            SourceFunctions::new(&modules, &semantic.program).expect("inventory failed");
        assert_eq!(functions.items.len(), 1);
        assert_eq!(functions.items[0].func.node.params.len(), 1);
        assert_eq!(functions.items[0].fact.params.len(), 1);
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
    fn extern_function_use_lowers_declaration() {
        let (root, resolved, semantic) =
            checked("extern fn host_log(message: string); fn f() { host_log(\"ok\"); }");
        let modules = SourceModules::new(&root, &resolved);
        let functions =
            SourceFunctions::new(&modules, &semantic.program).expect("inventory failed");
        let mut cx = LowerCx::default();
        cx.lower_modules(&modules);
        cx.lower_extern_declarations(&functions, &semantic.program)
            .expect("extern lowering failed");

        assert_eq!(cx.program.externs.len(), 1);
        let ext = &cx.program.externs[0];
        assert_eq!(ext.name, Ident::new("host_log"));
        assert_eq!(ext.params.len(), 1);
        assert!(matches!(
            cx.program.type_arena.get(ext.params[0]),
            Some(TypeData::String)
        ));
        assert!(matches!(
            cx.program.type_arena.get(ext.return_type),
            Some(TypeData::Void)
        ));
    }

    #[test]
    fn source_function_call_lowers() {
        let source = "fn add(a: int, b: int) -> int { a + b } fn f() -> int { add(1, 2) }";
        let (root, resolved, semantic) = checked(source);
        let air = lower_with_modules(&root, &resolved, &semantic.program).expect("lower failed");

        assert!(air.functions.iter().any(|function| {
            function.body.iter().any(|block| {
                block.statements.iter().any(|statement| {
                    matches!(
                        statement,
                        Statement::Init {
                            value: RValue::Call {
                                callee: Callee::Function(_),
                                args,
                            },
                            ..
                        } if args.len() == 2
                    )
                })
            })
        }));
    }

    #[test]
    fn method_call_is_unsupported() {
        let source = "struct S { fn value(self) -> int { 1 } } fn f(s: S) -> int { s.value() }";
        let (root, resolved, semantic) = checked(source);
        let err =
            lower_with_modules(&root, &resolved, &semantic.program).expect_err("expected error");
        assert!(matches!(
            err,
            LowerError::UnsupportedDecl {
                kind: "Aggregate",
                ..
            } | LowerError::UnsupportedExpr { kind: "Call", .. }
        ));
    }

    #[test]
    fn runtime_default_arg_is_unsupported() {
        let source = r#"fn fallback() -> string { "ok" } fn ok(message: string = fallback()) -> string { message } fn f() -> string { ok() }"#;
        let (root, resolved, semantic) = checked(source);
        let err =
            lower_with_modules(&root, &resolved, &semantic.program).expect_err("expected error");
        assert!(matches!(err, LowerError::UnsupportedDefaultArg { .. }));
    }

    #[test]
    fn default_arg_lowers_to_literal_operand() {
        let source =
            r#"fn ok(message: string = "ok") -> string { message } fn f() -> string { ok() }"#;
        let (root, resolved, semantic) = checked(source);
        let air = lower_with_modules(&root, &resolved, &semantic.program).expect("lower failed");

        assert!(air.functions.iter().any(|function| {
            function.body.iter().any(|block| {
                block.statements.iter().any(|statement| {
                    matches!(
                        statement,
                        Statement::Init {
                            value: RValue::Call { args, .. },
                            ..
                        } if args.len() == 1 && matches!(args[0], Operand::Const(_))
                    )
                })
            })
        }));
    }

    #[test]
    fn core_runtime_wrapper_lowers_extern_call() {
        let (root, resolved, semantic) = checked_with_modules(
            "import runtime { println }; fn main() { println(1); }",
            &[(
                "runtime",
                "
                extern fn _println(message: string);
                pub fn println<T>(value: T) { _println(#stringify(value)); }
                ",
            )],
        );
        let air = lower_with_modules(&root, &resolved, &semantic.program).expect("lower failed");

        assert_eq!(air.externs.len(), 1);
        assert!(air.functions.iter().any(|function| {
            function.body.iter().any(|block| {
                block.statements.iter().any(|statement| {
                    matches!(
                        statement,
                        Statement::Eval(RValue::Call {
                            callee: Callee::Extern(_),
                            ..
                        })
                    )
                })
            })
        }));
    }

    #[test]
    fn extern_mut_borrow_param_is_unsupported() {
        let provider = ProviderDescriptor {
            provider: ProviderId {
                name: "host".to_string(),
            },
            modules: vec![ExternModuleDescriptor {
                path: anvyx_externs::ModulePath {
                    segments: vec!["host".to_string()],
                },
                types: vec![],
                functions: vec![ExternFunctionDescriptor {
                    name: "touch".to_string(),
                    doc: None,
                    signature: ExternSignature {
                        params: vec![ExternParam {
                            name: Some("value".to_string()),
                            ty: ExternTypeExpr::Int,
                            flow: ParamFlow::MutBorrow,
                            escape: CallbackEscape::NonEscaping,
                        }],
                        ret: ExternTypeExpr::Void,
                    },
                    effects: ExternEffects::default(),
                }],
            }],
        };
        let (root, resolved, semantic) = checked_with_provider(
            "import ext:host { touch }; fn f(var x: int) { touch(x); }",
            provider,
        );
        let err =
            lower_with_modules(&root, &resolved, &semantic.program).expect_err("expected error");
        assert!(matches!(err, LowerError::UnsupportedExternSignature));
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
    fn rejects_extern_type_declarations() {
        let (root, resolved, semantic) = checked("extern type Handle; fn f() {}");
        let err =
            lower_with_modules(&root, &resolved, &semantic.program).expect_err("expected error");

        assert!(matches!(
            err,
            LowerError::UnsupportedDecl {
                kind: "ExternType",
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
        assert!(matches!(err, LowerError::UnsupportedType { ty } if *ty == Type::Any));
    }

    #[test]
    fn source_lowering_rejects_emitted_any_types() {
        let mut program = Program::default();
        let any = program.alloc_type(TypeData::Any);
        let err = reject_any_types(&program).expect_err("expected any rejection");
        assert!(matches!(err, LowerError::AnyTypeEmitted(id) if id == any));
    }

    fn checked(source: &str) -> (ast::Program, ResolveResult, typecheck::SemanticCheckOutput) {
        checked_with_modules(source, &[])
    }

    fn checked_with_modules(
        source: &str,
        modules: &[(&str, &str)],
    ) -> (ast::Program, ResolveResult, typecheck::SemanticCheckOutput) {
        let root = parse_program(source);
        let resolved = resolved_modules_with_core_option(&root, modules);
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

    fn checked_with_provider(
        source: &str,
        provider: ProviderDescriptor,
    ) -> (ast::Program, ResolveResult, typecheck::SemanticCheckOutput) {
        let root = parse_program(source);
        let provider_raw = externs::ingest_providers(ExternInputs {
            packages: vec![PackageExternInputs {
                package: crate::resolve::PackageId::synthetic_root(),
                providers: vec![provider],
            }],
        })
        .expect("valid provider");
        let external_modules = externs::raw_extern_module_ids(&provider_raw);
        let resolved = resolved_modules_with_core_option_external(&root, &[], &external_modules);
        let mut raw = externs::collect_source_externs(&root, &resolved).unwrap();
        raw.append(provider_raw);
        let semantic = typecheck::check_semantic_with_modules(
            &root,
            &resolved,
            raw,
            TypecheckConfig::default(),
        )
        .expect("typecheck failed");
        (root, resolved, semantic)
    }
}
