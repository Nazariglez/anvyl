use std::collections::HashMap;

use anvyx_externs::ParamFlow;

use super::{
    AggregateDecl, AggregateKind, BasicBlock, BlockId, Callee, ConstData, ConstValue,
    DynContractData, EnumDecl, ExternDecl, ExternFieldDecl, ExternId, ExternMember,
    ExternMethodDecl, ExternOp, ExternOpDecl, ExternRep, ExternStaticDecl, ExternTypeDecl,
    FieldDecl, Function, FunctionId, FunctionKind, Local, LocalId, LocalKind, Module, ModuleId,
    Mutability as AirMutability, Operand, Param, ParamRole, Place, Program, RValue, Signature,
    SignatureType, Statement, Terminator, TypeData, TypeId, VariantDecl, VariantShape, VerifyError,
    typing::{self, PrimitiveTypes, ScalarType},
    verify,
};
use crate::{
    ast::{
        self, ArrayLen, AssignOp, BinaryOp, BlockNode, ExprId, ExprKind, ExprNode, Ident, Lit,
        Mutability as AstMutability, Pattern, Stmt, StmtNode, Type,
    },
    externs::catalog::ExternCatalog,
    resolve::{PackageModulePath, ResolveResult},
    source::SourceId,
    span::SourceSpan,
    typecheck::{
        BodyInstanceKey, CallForm, CallableId, CallableInstanceKey, CallableKind, ConstTerm,
        DeclarationIndex, DefaultArgFact, ExternUseTarget, GenericArgs, LocalDefFact, LocalDefKind,
        LocalUseFact, LocalUseMode, MemberPathKind, MethodMode, MethodSurface, ModuleScope,
        NominalKey, SemanticBodyFacts, SemanticFunctionInstanceFact, SemanticLocalId,
        SemanticProgram, VariantPayload, nominal_generic_args, substitute_aggregate_member,
        type_has_unfinished_facts,
    },
};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum LowerError {
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
    NonConcreteRoot {
        id: Box<CallableId>,
        args: Box<GenericArgs>,
    },
    UnsupportedRootCallable {
        id: Box<CallableId>,
        args: Box<GenericArgs>,
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
    MissingEntryRoot {
        root: Box<CallableInstanceKey>,
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
    Verify(Box<[VerifyError]>),
    AnyTypeEmitted(TypeId),
}

#[derive(Debug, Clone, Default)]
pub(crate) struct AirLowerConfig {
    pub(crate) roots: AirRoots,
}

#[derive(Debug, Clone, Default)]
pub(crate) struct AirRoots {
    pub(crate) entry: Option<CallableInstanceKey>,
    pub(crate) callables: Vec<CallableInstanceKey>,
}

impl AirRoots {
    fn normalized(&self) -> Vec<CallableInstanceKey> {
        let mut roots = vec![];
        if let Some(entry) = &self.entry {
            roots.push(entry.clone());
        }
        roots.extend(self.callables.iter().cloned());

        let mut unique = vec![];
        for root in roots {
            if !unique.contains(&root) {
                unique.push(root);
            }
        }
        unique
    }
}

struct SemanticCallableFacts<'a> {
    functions: HashMap<CallableInstanceKey, &'a SemanticFunctionInstanceFact>,
}

impl<'a> SemanticCallableFacts<'a> {
    fn new(semantic: &'a SemanticProgram) -> Self {
        let functions = semantic
            .declaration_facts
            .functions
            .iter()
            .map(|fact| {
                (
                    CallableInstanceKey {
                        target: fact.id.clone(),
                        args: fact.args.clone(),
                    },
                    fact,
                )
            })
            .collect();
        Self { functions }
    }

    fn get(&self, key: &CallableInstanceKey) -> Option<&'a SemanticFunctionInstanceFact> {
        self.functions.get(key).copied()
    }
}

#[derive(Debug, Default)]
struct TypeLowerer {
    cache: HashMap<Type, TypeId>,
}

struct TypeLowerEnv<'a, 'b> {
    modules: &'a mut HashMap<ModuleScope, ModuleId>,
    decls: Option<&'b DeclarationIndex>,
    externs: Option<&'b ExternCatalog>,
}

impl TypeLowerer {
    fn lower(&mut self, program: &mut Program, ty: &Type) -> Result<TypeId, LowerError> {
        self.lower_with_env(
            program,
            ty,
            TypeLowerEnv {
                modules: &mut HashMap::new(),
                decls: None,
                externs: None,
            },
        )
    }

    fn lower_source(
        &mut self,
        program: &mut Program,
        modules: &mut HashMap<ModuleScope, ModuleId>,
        decls: &DeclarationIndex,
        externs: &ExternCatalog,
        ty: &Type,
    ) -> Result<TypeId, LowerError> {
        self.lower_with_env(
            program,
            ty,
            TypeLowerEnv {
                modules,
                decls: Some(decls),
                externs: Some(externs),
            },
        )
    }

    fn lower_with_env(
        &mut self,
        program: &mut Program,
        ty: &Type,
        mut env: TypeLowerEnv<'_, '_>,
    ) -> Result<TypeId, LowerError> {
        if let Some(id) = self.cache.get(ty).copied() {
            return Ok(id);
        }

        let data = match ty {
            Type::Int => TypeData::Int,
            Type::Float => TypeData::Float,
            Type::Bool => TypeData::Bool,
            Type::String => TypeData::String,
            Type::Void => TypeData::Void,
            Type::Optional { inner } => {
                TypeData::Optional(self.lower_with_env(program, inner, env)?)
            }
            Type::Tuple(elems) => TypeData::Tuple(
                elems
                    .iter()
                    .map(|elem| self.lower_with_env(program, elem, env.reborrow()))
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            Type::List { elem } => TypeData::List(self.lower_with_env(program, elem, env)?),
            Type::Array { elem, len } => {
                let ArrayLen::Fixed(len) = len else {
                    return Err(LowerError::UnsupportedType {
                        ty: Box::new(ty.clone()),
                    });
                };
                TypeData::Array {
                    elem: self.lower_with_env(program, elem, env)?,
                    len: *len,
                }
            }
            Type::Map { key, value } => TypeData::Map {
                key: self.lower_with_env(program, key, env.reborrow())?,
                value: self.lower_with_env(program, value, env)?,
                order: super::MapOrder::Insertion,
            },
            Type::Slice { elem } => TypeData::Slice(self.lower_with_env(program, elem, env)?),
            Type::Func { params, ret } => TypeData::Function(SignatureType::new(
                params
                    .iter()
                    .map(|param| self.lower_with_env(program, &param.ty, env.reborrow()))
                    .collect::<Result<Vec<_>, _>>()?,
                self.lower_with_env(program, &ret.ty, env)?,
            )),
            Type::Dyn(contract) => TypeData::Dyn(dyn_contract_data(contract)?),
            Type::Nominal(_) => return self.lower_nominal(program, ty, env),
            _ => {
                return Err(LowerError::UnsupportedType {
                    ty: Box::new(ty.clone()),
                });
            }
        };

        let id = program.alloc_type(data);
        self.cache.insert(ty.clone(), id);
        Ok(id)
    }

    fn lower_nominal(
        &mut self,
        program: &mut Program,
        ty: &Type,
        env: TypeLowerEnv<'_, '_>,
    ) -> Result<TypeId, LowerError> {
        let (Some(decls), Some(externs)) = (env.decls, env.externs) else {
            return Err(LowerError::UnsupportedType {
                ty: Box::new(ty.clone()),
            });
        };
        let Some(key) = decls.key_for_type(ty) else {
            return Err(LowerError::UnsupportedType {
                ty: Box::new(ty.clone()),
            });
        };
        if key.kind == ast::NominalKind::Extern {
            return self.lower_extern_nominal(program, ty, env, externs, key);
        }
        if decls.aggregate(&key).is_some() {
            return self.lower_aggregate_nominal(program, ty, env, decls, key);
        }
        if decls.enum_schema(&key).is_some() {
            return self.lower_enum_nominal(program, ty, env, decls, key);
        }
        Err(LowerError::UnsupportedType {
            ty: Box::new(ty.clone()),
        })
    }

    fn lower_aggregate_nominal(
        &mut self,
        program: &mut Program,
        ty: &Type,
        mut env: TypeLowerEnv<'_, '_>,
        decls: &DeclarationIndex,
        key: NominalKey,
    ) -> Result<TypeId, LowerError> {
        let schema = decls.aggregate(&key).expect("aggregate schema exists");
        let module = ensure_module(program, env.modules, &key.module);
        let kind = if key.kind == ast::NominalKind::DataRef {
            AggregateKind::DataRef
        } else {
            AggregateKind::Struct
        };
        let type_args = self.nominal_type_args(program, ty, env.reborrow())?;
        let const_args = nominal_const_args(ty);
        let agg = program.alloc_aggregate(AggregateDecl {
            name: key.name,
            module,
            kind,
            type_args,
            const_args,
            fields: vec![],
            cycle_capable: kind == AggregateKind::DataRef,
            stringify_override: None,
        });
        program.module_mut(module).aggregates.push(agg);
        let id = program.alloc_type(match kind {
            AggregateKind::Struct => TypeData::Aggregate(agg),
            AggregateKind::DataRef => TypeData::DataRef(agg),
        });
        self.cache.insert(ty.clone(), id);
        let fields = schema
            .fields
            .iter()
            .map(|(name, field)| {
                let field_ty = substitute_aggregate_member(ty, &schema.generics, &field.ty);
                Ok(FieldDecl {
                    name,
                    ty: self.lower_with_env(program, &field_ty, env.reborrow())?,
                })
            })
            .collect::<Result<Vec<_>, LowerError>>()?;
        program.aggregate_mut(agg).fields = fields;
        Ok(id)
    }

    fn lower_enum_nominal(
        &mut self,
        program: &mut Program,
        ty: &Type,
        mut env: TypeLowerEnv<'_, '_>,
        decls: &DeclarationIndex,
        key: NominalKey,
    ) -> Result<TypeId, LowerError> {
        let schema = decls.enum_schema(&key).expect("enum schema exists");
        let module = ensure_module(program, env.modules, &key.module);
        let type_args = self.nominal_type_args(program, ty, env.reborrow())?;
        let const_args = nominal_const_args(ty);
        let enum_id = program.alloc_enum(EnumDecl {
            name: key.name,
            module,
            type_args,
            const_args,
            variants: vec![],
        });
        program.module_mut(module).enums.push(enum_id);
        let id = program.alloc_type(TypeData::Enum(enum_id));
        self.cache.insert(ty.clone(), id);
        let variants = schema
            .variants
            .iter()
            .map(|(name, variant)| {
                let shape = match &variant.payload {
                    VariantPayload::Unit => VariantShape::Unit,
                    VariantPayload::Tuple(items) => VariantShape::Tuple(
                        items
                            .iter()
                            .map(|item| {
                                let item = substitute_aggregate_member(ty, &schema.generics, item);
                                self.lower_with_env(program, &item, env.reborrow())
                            })
                            .collect::<Result<Vec<_>, _>>()?,
                    ),
                    VariantPayload::Struct(fields) => VariantShape::Struct(
                        fields
                            .iter()
                            .map(|(field_name, field)| {
                                let field_ty =
                                    substitute_aggregate_member(ty, &schema.generics, &field.ty);
                                Ok(FieldDecl {
                                    name: field_name,
                                    ty: self.lower_with_env(program, &field_ty, env.reborrow())?,
                                })
                            })
                            .collect::<Result<Vec<_>, LowerError>>()?,
                    ),
                };
                Ok(VariantDecl { name, shape })
            })
            .collect::<Result<Vec<_>, LowerError>>()?;
        program.enum_decl_mut(enum_id).variants = variants;
        Ok(id)
    }

    fn lower_extern_nominal(
        &mut self,
        program: &mut Program,
        ty: &Type,
        mut env: TypeLowerEnv<'_, '_>,
        externs: &ExternCatalog,
        key: NominalKey,
    ) -> Result<TypeId, LowerError> {
        let Some(source_id) = externs.type_by_nominal(&key) else {
            return Err(LowerError::UnsupportedType {
                ty: Box::new(ty.clone()),
            });
        };
        let source = externs.ty(source_id);
        let module = ensure_module(program, env.modules, &key.module);
        let type_args = self.nominal_type_args(program, ty, env.reborrow())?;
        let const_args = nominal_const_args(ty);
        let extern_id = program.alloc_extern_type(ExternTypeDecl {
            name: key.name,
            module,
            type_args,
            const_args,
            rep: match source.rep {
                anvyx_externs::ExternRep::Shared => ExternRep::Shared,
                anvyx_externs::ExternRep::Inline => ExternRep::Inline,
            },
            has_init: source.init.is_some(),
            fields: vec![],
            methods: vec![],
            statics: vec![],
            operators: vec![],
        });
        program.module_mut(module).extern_types.push(extern_id);
        let id = program.alloc_type(TypeData::Extern(extern_id));
        self.cache.insert(ty.clone(), id);

        let fields = source
            .fields
            .iter()
            .map(|field| {
                Ok(ExternFieldDecl {
                    name: field.name,
                    ty: self.lower_with_env(program, &field.ty.ty, env.reborrow())?,
                    computed: field.computed,
                })
            })
            .collect::<Result<Vec<_>, LowerError>>()?;
        let methods = source
            .methods
            .iter()
            .map(|method| {
                let params = method
                    .signature
                    .params
                    .iter()
                    .map(|param| self.lower_with_env(program, &param.ty.ty, env.reborrow()))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(ExternMethodDecl {
                    name: method.name,
                    receiver: match method.receiver {
                        anvyx_externs::ReceiverMode::Value => super::MethodReceiver::Value,
                        anvyx_externs::ReceiverMode::Shared => super::MethodReceiver::Shared,
                        anvyx_externs::ReceiverMode::Mutable => super::MethodReceiver::Mut,
                    },
                    params,
                    return_type: self.lower_with_env(
                        program,
                        &method.signature.ret.ty,
                        env.reborrow(),
                    )?,
                })
            })
            .collect::<Result<Vec<_>, LowerError>>()?;
        let statics = source
            .statics
            .iter()
            .map(|static_method| {
                let params = static_method
                    .signature
                    .params
                    .iter()
                    .map(|param| self.lower_with_env(program, &param.ty.ty, env.reborrow()))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(ExternStaticDecl {
                    name: static_method.name,
                    params,
                    return_type: self.lower_with_env(
                        program,
                        &static_method.signature.ret.ty,
                        env.reborrow(),
                    )?,
                })
            })
            .collect::<Result<Vec<_>, LowerError>>()?;
        let operators = source
            .operators
            .iter()
            .map(|operator| {
                Ok(ExternOpDecl {
                    kind: lower_extern_op(operator.op),
                    operand: operator
                        .signature
                        .params
                        .first()
                        .map(|param| self.lower_with_env(program, &param.ty.ty, env.reborrow()))
                        .transpose()?,
                    return_type: self.lower_with_env(
                        program,
                        &operator.signature.ret.ty,
                        env.reborrow(),
                    )?,
                })
            })
            .collect::<Result<Vec<_>, LowerError>>()?;
        let decl = program.extern_type_mut(extern_id);
        decl.fields = fields;
        decl.methods = methods;
        decl.statics = statics;
        decl.operators = operators;
        Ok(id)
    }

    fn nominal_type_args(
        &mut self,
        program: &mut Program,
        ty: &Type,
        mut env: TypeLowerEnv<'_, '_>,
    ) -> Result<Vec<TypeId>, LowerError> {
        Ok(ty
            .as_nominal()
            .map(|nominal| {
                nominal
                    .type_args
                    .iter()
                    .map(|arg| self.lower_with_env(program, arg, env.reborrow()))
                    .collect::<Result<Vec<_>, _>>()
            })
            .transpose()?
            .unwrap_or_default())
    }
}

fn lower_extern_op(op: anvyx_externs::ExternOperator) -> ExternOp {
    match op {
        anvyx_externs::ExternOperator::Unary(op) => ExternOp::Unary(match op {
            anvyx_externs::UnaryOp::Neg => ast::UnaryOp::Neg,
        }),
        anvyx_externs::ExternOperator::Binary { op, self_on_right } => ExternOp::Binary {
            op: match op {
                anvyx_externs::BinaryOp::Add => BinaryOp::Add,
                anvyx_externs::BinaryOp::Sub => BinaryOp::Sub,
                anvyx_externs::BinaryOp::Mul => BinaryOp::Mul,
                anvyx_externs::BinaryOp::Div => BinaryOp::Div,
                anvyx_externs::BinaryOp::Rem => BinaryOp::Rem,
                anvyx_externs::BinaryOp::Eq => BinaryOp::Eq,
                anvyx_externs::BinaryOp::NotEq => BinaryOp::NotEq,
                anvyx_externs::BinaryOp::LessThan => BinaryOp::LessThan,
                anvyx_externs::BinaryOp::GreaterThan => BinaryOp::GreaterThan,
                anvyx_externs::BinaryOp::LessThanEq => BinaryOp::LessThanEq,
                anvyx_externs::BinaryOp::GreaterThanEq => BinaryOp::GreaterThanEq,
            },
            self_on_right,
        },
    }
}

fn nominal_const_args(ty: &Type) -> Vec<String> {
    ty.as_nominal()
        .map(|nominal| nominal.const_args.iter().map(ToString::to_string).collect())
        .unwrap_or_default()
}

fn ensure_module(
    program: &mut Program,
    modules: &mut HashMap<ModuleScope, ModuleId>,
    scope: &ModuleScope,
) -> ModuleId {
    if let Some(id) = modules.get(scope).copied() {
        return id;
    }
    let id = program.alloc_module(Module {
        path: module_path(scope),
        functions: vec![],
        aggregates: vec![],
        enums: vec![],
        extern_types: vec![],
        externs: vec![],
    });
    modules.insert(scope.clone(), id);
    id
}

impl TypeLowerEnv<'_, '_> {
    fn reborrow(&mut self) -> TypeLowerEnv<'_, '_> {
        TypeLowerEnv {
            modules: self.modules,
            decls: self.decls,
            externs: self.externs,
        }
    }
}

#[derive(Debug, Default)]
struct LoweringMaps {
    modules: HashMap<ModuleScope, ModuleId>,
    bodies: HashMap<BodyInstanceKey, FunctionId>,
    locals: HashMap<BodyInstanceKey, HashMap<SemanticLocalId, LocalId>>,
    externs: HashMap<crate::externs::catalog::ExternFunctionId, ExternId>,
}

#[derive(Default)]
struct LowerCx {
    program: Program,
    types: TypeLowerer,
    maps: LoweringMaps,
    decls: Option<DeclarationIndex>,
    externs: Option<ExternCatalog>,
}

impl LowerCx {
    fn lower_ty(&mut self, ty: &Type) -> Result<TypeId, LowerError> {
        match (&self.decls, &self.externs) {
            (Some(decls), Some(externs)) => self.types.lower_source(
                &mut self.program,
                &mut self.maps.modules,
                decls,
                externs,
                ty,
            ),
            _ => self.types.lower(&mut self.program, ty),
        }
    }

    fn set_entry(&mut self, root: &CallableInstanceKey) -> Result<(), LowerError> {
        let body = BodyInstanceKey::Callable(root.clone());
        let Some(function) = self.maps.bodies.get(&body).copied() else {
            return Err(LowerError::MissingEntryRoot {
                root: Box::new(root.clone()),
            });
        };
        self.program.set_entry(function);
        Ok(())
    }

    fn ensure_module(&mut self, scope: &ModuleScope) -> ModuleId {
        ensure_module(&mut self.program, &mut self.maps.modules, scope)
    }

    fn alloc_function_in_module(
        &mut self,
        scope: &ModuleScope,
        body: BodyInstanceKey,
        locals: HashMap<SemanticLocalId, LocalId>,
        build: impl FnOnce(ModuleId) -> Function,
    ) -> FunctionId {
        let module = self.ensure_module(scope);
        let id = self.program.alloc_function(build(module));
        self.program.module_mut(module).functions.push(id);
        let old = self.maps.bodies.insert(body.clone(), id);
        debug_assert!(old.is_none(), "duplicate lowered function body");
        let old = self.maps.locals.insert(body, locals);
        debug_assert!(old.is_none(), "duplicate lowered function local map");
        id
    }

    fn alloc_extern_in_module(
        &mut self,
        scope: &ModuleScope,
        source_id: crate::externs::catalog::ExternFunctionId,
        name: Ident,
        member: ExternMember,
        params: Vec<TypeId>,
        return_type: TypeId,
    ) -> ExternId {
        let module = self.ensure_module(scope);
        let id = self.program.alloc_extern(ExternDecl {
            name,
            module,
            member,
            params,
            return_type,
        });
        self.program.module_mut(module).externs.push(id);
        let old = self.maps.externs.insert(source_id, id);
        debug_assert!(old.is_none(), "duplicate lowered extern");
        id
    }

    fn lower_extern_declarations(
        &mut self,
        functions: &ReachableCallables<'_>,
        semantic: &SemanticProgram,
    ) -> Result<(), LowerError> {
        for source in &functions.items {
            let mut externs = vec![];
            let mut uses = source
                .body_facts
                .as_facts()
                .extern_uses
                .iter()
                .collect::<Vec<_>>();
            uses.sort_by_key(|(expr_id, _)| expr_id.0);
            for (expr_id, targets) in uses {
                for target in targets {
                    match target {
                        ExternUseTarget::Function(id) => externs.push(*id),
                        _ => return Err(LowerError::UnsupportedExternUse { expr_id: *expr_id }),
                    }
                }
            }
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
                self.alloc_extern_in_module(
                    module_scope,
                    id,
                    function.key.name,
                    ExternMember::FreeFunction,
                    params,
                    return_type,
                );
            }
        }
        Ok(())
    }

    fn lower_function_shells(
        &mut self,
        modules: &SourceModules<'_>,
        functions: &ReachableCallables<'_>,
    ) -> Result<(), LowerError> {
        for source in &functions.items {
            let module_scope = &modules.items[source.callable.module()].scope;
            let fact = source.fact;
            let return_type = self.lower_ty(&fact.return_ty)?;
            let mut params = vec![];
            let mut locals = vec![];
            let mut local_map = HashMap::new();
            for (index, param_fact) in fact.params.iter().enumerate() {
                let body_facts = source.body_facts.as_facts();
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
                    role: if source.callable.is_instance_method() && index == 0 {
                        ParamRole::Receiver
                    } else {
                        ParamRole::Normal
                    },
                    local_id,
                });
            }
            self.alloc_function_in_module(module_scope, source.body.clone(), local_map, |module| {
                Function {
                    name: source.callable.name(),
                    module,
                    kind: source.callable.function_kind(),
                    signature: Signature::new(params, return_type),
                    locals,
                    body: vec![BasicBlock {
                        statements: vec![],
                        terminator: Terminator::Unreachable,
                    }],
                }
            });
        }
        Ok(())
    }

    fn attach_stringify_overrides(&mut self) {
        for (body, function_id) in &self.maps.bodies {
            let BodyInstanceKey::Callable(key) = body else {
                continue;
            };
            if !is_stringify_override(&key.target) {
                continue;
            }
            let function_id = *function_id;
            let Some(receiver) = self.program.function(function_id).signature.params.first() else {
                continue;
            };
            let aggregate = match self.program.type_data(receiver.ty) {
                TypeData::Aggregate(aggregate) | TypeData::DataRef(aggregate) => *aggregate,
                _ => continue,
            };
            self.program.aggregate_mut(aggregate).stringify_override = Some(function_id);
        }
    }

    fn lower_function_bodies(
        &mut self,
        functions: &ReachableCallables<'_>,
    ) -> Result<(), LowerError> {
        for source in &functions.items {
            let facts = source.body_facts.as_facts();
            let function = self.maps.bodies[&source.body];
            let locals = self
                .maps
                .locals
                .remove(&source.body)
                .expect("lowered function missing local map");
            let mut lowerer =
                FunctionLowerer::new(self, functions, source, facts, function, locals);
            lowerer.lower_body(source.callable.body())?;
        }
        Ok(())
    }
}

struct FunctionLowerer<'cx, 'facts> {
    cx: &'cx mut LowerCx,
    body: BodyInstanceKey,
    facts: &'facts SemanticBodyFacts,
    index: &'facts SourceProgramIndex<'facts>,
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
        functions: &'facts ReachableCallables<'facts>,
        source: &ReachableCallable<'_>,
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
            index: functions.index,
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
                let result_ty = self.lower_expr_ty(expr.node.id)?;
                if binary.node.op == BinaryOp::Add && result_ty == Type::String {
                    return self.lower_string_concat(&[
                        binary.node.left.as_ref(),
                        binary.node.right.as_ref(),
                    ]);
                }
                self.require_builtin_scalar(expr)?;
                let lhs = self.lower_value(&binary.node.left)?;
                let rhs = self.lower_value(&binary.node.right)?;
                let lhs_ty = self.operand_type(&lhs);
                let rhs_ty = self.operand_type(&rhs);
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
            ExprKind::Call(call) => self.lower_call_value(expr, call),
            ExprKind::IntrinsicCall(call) => self.lower_intrinsic_value(expr, call),
            ExprKind::StringInterp(parts) => self.lower_string_interp(parts),
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

    fn lower_string_interp(&mut self, parts: &[ast::StringPart]) -> Result<Operand, LowerError> {
        let mut operands = vec![];
        for part in parts {
            match part {
                ast::StringPart::Text(text) if text.is_empty() => {}
                ast::StringPart::Text(text) => operands.push(self.string_const(text)?),
                ast::StringPart::Expr(expr, Some(spec)) => {
                    let value = self.lower_value(expr)?;
                    let string_ty = self.string_ty()?;
                    operands.push(self.emit_typed_temp(
                        string_ty,
                        RValue::Format {
                            value,
                            spec: spec.node.clone(),
                        },
                    )?);
                }
                ast::StringPart::Expr(expr, None) => operands.push(self.lower_string_part(expr)?),
            }
        }
        self.emit_string_concat(operands)
    }

    fn lower_string_concat(&mut self, parts: &[&ExprNode]) -> Result<Operand, LowerError> {
        let operands = parts
            .iter()
            .map(|part| self.lower_string_part(part))
            .collect::<Result<Vec<_>, _>>()?;
        self.emit_string_concat(operands)
    }

    fn string_ty(&mut self) -> Result<TypeId, LowerError> {
        self.cx.lower_ty(&Type::String)
    }

    fn string_const(&mut self, text: impl AsRef<str>) -> Result<Operand, LowerError> {
        let ty = self.string_ty()?;
        let value = self.cx.program.alloc_const(ConstData {
            ty,
            value: ConstValue::String(text.as_ref().into()),
        });
        Ok(Operand::Const(value))
    }

    fn emit_string_concat(&mut self, parts: Vec<Operand>) -> Result<Operand, LowerError> {
        let ty = self.string_ty()?;
        self.emit_typed_temp(ty, RValue::StringConcat { parts })
    }

    fn lower_string_part(&mut self, expr: &ExprNode) -> Result<Operand, LowerError> {
        let ty = self.lower_expr_ty(expr.node.id)?;
        if ty == Type::String {
            return self.lower_value(expr);
        }
        let fact = self
            .facts
            .stringifies
            .get(&expr.node.id)
            .ok_or_else(|| unsupported_expr(expr))?;
        if fact.arg != expr.node.id {
            return Err(unsupported_expr(expr));
        }
        let source_ty = fact.source_ty.clone();
        self.lower_stringify_value(expr, &source_ty)
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
        self.lower_stringify_value(arg, &fact.source_ty)
    }

    fn lower_stringify_value(
        &mut self,
        arg: &ExprNode,
        source: &Type,
    ) -> Result<Operand, LowerError> {
        if *source == Type::Void {
            self.lower_effect(arg)?;
            return self.string_const("<void>");
        }
        let source_ty = self.cx.lower_ty(source)?;
        let value = self.lower_value(arg)?;
        let result_ty = self.string_ty()?;
        self.emit_typed_temp(result_ty, RValue::Stringify { value, source_ty })
    }

    fn lower_call_value(
        &mut self,
        expr: &ExprNode,
        call: &ast::CallNode,
    ) -> Result<Operand, LowerError> {
        let value = self.lower_call_rvalue(expr, call)?;
        if self.lower_expr_ty(expr.node.id)? == Type::Void {
            return Err(unsupported_expr(expr));
        }
        self.emit_temp(value)
    }

    fn lower_call_rvalue(
        &mut self,
        expr: &ExprNode,
        call: &ast::CallNode,
    ) -> Result<RValue, LowerError> {
        let args = &call.node.args;
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
        let mut operands = vec![];
        if target.id.kind == CallableKind::InstanceMethod {
            let ExprKind::Field(field) = &call.node.func.node.kind else {
                return Err(unsupported_expr(&call.node.func));
            };
            if self
                .facts
                .member_paths
                .get(&call.node.func.node.id)
                .is_some_and(|fact| fact.kind == MemberPathKind::MethodReceiver)
            {
                return Err(unsupported_expr(&call.node.func));
            }
            operands.push(self.lower_value(&field.node.target)?);
        }
        operands.extend(
            args.iter()
                .map(|arg| self.lower_value(arg))
                .collect::<Result<Vec<_>, _>>()?,
        );
        let expected = self.cx.program.function(callee).signature.params.len();
        let provided = operands.len();
        operands.extend(self.lower_default_args(expr.node.id, provided, expected)?);
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
        let Some(expr) = self.index.default_exprs.get(&(
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
                let value = self.lower_call_rvalue(expr, call)?;
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
                let binary = assign_op_to_binary(op);
                let fact = self.local_use(&assign.node.target, LocalUseMode::CompoundAssign)?;
                let dst = self.lower_place(&assign.node.target, &fact)?;
                let result_ty = self.air_type(dst.ty);
                if binary == BinaryOp::Add && result_ty == Type::String {
                    let lhs = Operand::Place(dst.clone());
                    let rhs = self.lower_string_part(&assign.node.value)?;
                    let tmp = self.emit_string_concat(vec![lhs, rhs])?;
                    return self.emit_assign(dst, RValue::Use(tmp));
                }
                self.require_builtin_scalar(expr)?;
                let lhs = Operand::Place(dst.clone());
                let rhs = self.lower_value(&assign.node.value)?;
                let lhs_ty = self.operand_type(&lhs);
                let rhs_ty = self.operand_type(&rhs);
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

fn dyn_contract_data(contract: &ast::ContractRef) -> Result<DynContractData, LowerError> {
    let key = dyn_contract_key(contract)?;
    Ok(DynContractData {
        display_name: dyn_contract_name(contract)?,
        method_table_key: key,
        concrete_printer: None,
    })
}

fn dyn_contract_name(contract: &ast::ContractRef) -> Result<String, LowerError> {
    match contract {
        ast::ContractRef::Named {
            qualifier, name, ..
        } => Ok(qualifier
            .map(|qualifier| format!("{qualifier}::{name}"))
            .unwrap_or_else(|| name.to_string())),
        ast::ContractRef::Anonymous(contract) => Ok(format!(
            "contract({})",
            contract
                .requirements
                .iter()
                .map(|requirement| requirement.name.to_string())
                .collect::<Vec<_>>()
                .join(" + ")
        )),
        ast::ContractRef::Intersection(parts) => Ok(parts
            .iter()
            .map(dyn_contract_name)
            .collect::<Result<Vec<_>, _>>()?
            .join(" + ")),
        ast::ContractRef::Infer | ast::ContractRef::Hole(_) => Err(LowerError::UnsupportedType {
            ty: Box::new(Type::Dyn(contract.clone())),
        }),
    }
}

fn dyn_contract_key(contract: &ast::ContractRef) -> Result<String, LowerError> {
    match contract {
        ast::ContractRef::Named {
            qualifier,
            name,
            origin,
        } => Ok(format!(
            "named:{}:{}:{}",
            origin_key(origin.as_ref()),
            qualifier
                .map(|qualifier| qualifier.to_string())
                .unwrap_or_default(),
            name
        )),
        ast::ContractRef::Anonymous(contract) => Ok(format!(
            "anon:{}:{}",
            contract.requirements.len(),
            contract
                .requirements
                .iter()
                .map(|requirement| format!(
                    "{:?}:{}:{:?}:{:?}",
                    requirement.receiver, requirement.name, requirement.params, requirement.ret
                ))
                .collect::<Vec<_>>()
                .join("|")
        )),
        ast::ContractRef::Intersection(parts) => Ok(format!(
            "intersection:{}:{}",
            parts.len(),
            parts
                .iter()
                .map(dyn_contract_key)
                .collect::<Result<Vec<_>, _>>()?
                .join("|")
        )),
        ast::ContractRef::Infer | ast::ContractRef::Hole(_) => Err(LowerError::UnsupportedType {
            ty: Box::new(Type::Dyn(contract.clone())),
        }),
    }
}

fn origin_key(origin: Option<&ast::ModuleOrigin>) -> String {
    match origin {
        Some(ast::ModuleOrigin::Module(path)) => format!("module:{}", path.join("::")),
        Some(ast::ModuleOrigin::SourceFile { package, path }) => {
            format!("source:{}:{path}", package.as_deref().unwrap_or_default())
        }
        Some(ast::ModuleOrigin::Package { package, path }) => format!(
            "package:{package}:{}",
            path.as_ref()
                .map(|path| path.join("::"))
                .unwrap_or_default()
        ),
        Some(ast::ModuleOrigin::Provider { package, path }) => {
            format!("provider:{package}:{}", path.join("::"))
        }
        None => String::new(),
    }
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
    config: AirLowerConfig,
) -> Result<Program, LowerError> {
    let index = SourceProgramIndex::new(root, resolved);
    let facts = SemanticCallableFacts::new(semantic);
    let AirLowerConfig { roots } = config;
    let entry = roots.entry.clone();
    let roots = roots.normalized();
    validate_roots(&roots, &facts)?;
    let functions = ReachableCallables::new(&index, semantic, &facts, roots)?;
    let mut cx = LowerCx {
        decls: Some(semantic.declarations.clone()),
        externs: Some(semantic.externs.clone()),
        ..LowerCx::default()
    };
    cx.lower_function_shells(&index.modules, &functions)?;
    cx.attach_stringify_overrides();
    if let Some(entry) = &entry {
        cx.set_entry(entry)?;
    }
    cx.lower_extern_declarations(&functions, semantic)?;
    cx.lower_function_bodies(&functions)?;
    verify(&cx.program).map_err(|errors| LowerError::Verify(errors.into_boxed_slice()))?;
    reject_any_types(&cx.program)?;
    Ok(cx.program)
}

fn validate_roots(
    roots: &[CallableInstanceKey],
    facts: &SemanticCallableFacts<'_>,
) -> Result<(), LowerError> {
    for root in roots {
        if !generic_args_are_concrete(&root.args) {
            return Err(LowerError::NonConcreteRoot {
                id: Box::new(root.target.clone()),
                args: Box::new(root.args.clone()),
            });
        }
        if !callable_is_top_level_function(&root.target) {
            return Err(LowerError::UnsupportedRootCallable {
                id: Box::new(root.target.clone()),
                args: Box::new(root.args.clone()),
            });
        }
        if facts.get(root).is_none() {
            return Err(LowerError::MissingFunctionFact {
                id: Box::new(root.target.clone()),
                args: Box::new(root.args.clone()),
            });
        }
    }
    Ok(())
}

fn callable_is_top_level_function(id: &CallableId) -> bool {
    id.parent.is_none() && id.kind == CallableKind::Function
}

fn generic_args_are_concrete(args: &GenericArgs) -> bool {
    args.type_args.iter().all(type_is_concrete)
        && args.const_args.iter().all(const_term_is_concrete)
}

fn type_is_concrete(ty: &Type) -> bool {
    !type_has_unfinished_facts(ty)
}

fn const_term_is_concrete(term: &ConstTerm) -> bool {
    matches!(term, ConstTerm::Value(_))
}

fn reject_any_types(program: &Program) -> Result<(), LowerError> {
    for (index, ty) in program.type_arena.iter().enumerate() {
        if matches!(ty, TypeData::Any) {
            return Err(LowerError::AnyTypeEmitted(TypeId::from_index(index)));
        }
    }
    Ok(())
}

#[derive(Debug)]
struct SourceModules<'a> {
    items: Vec<SourceModule<'a>>,
}

#[derive(Debug)]
struct SourceModule<'a> {
    scope: ModuleScope,
    source: SourceId,
    program: &'a ast::Program,
}

impl<'a> SourceModules<'a> {
    fn new(root: &'a ast::Program, resolved: &'a ResolveResult) -> Self {
        let items = DeclarationIndex::source_modules(root, resolved)
            .into_iter()
            .map(|module| SourceModule {
                scope: module.scope,
                source: module.source,
                program: module.program,
            })
            .collect();
        Self { items }
    }
}

#[derive(Debug)]
struct SourceProgramIndex<'a> {
    modules: SourceModules<'a>,
    callables: HashMap<CallableId, SourceCallable<'a>>,
    default_exprs: HashMap<(CallableId, SourceId, ExprId), &'a ExprNode>,
}

#[derive(Debug, Clone, Copy)]
enum SourceCallable<'a> {
    Function {
        module: usize,
        func: &'a ast::FuncNode,
        source: SourceId,
    },
    AggregateMethod {
        module: usize,
        method: &'a ast::Method,
        mode: MethodMode,
        source: SourceId,
    },
}

impl<'a> SourceCallable<'a> {
    fn module(self) -> usize {
        match self {
            Self::Function { module, .. } | Self::AggregateMethod { module, .. } => module,
        }
    }

    fn source(self) -> SourceId {
        match self {
            Self::Function { source, .. } | Self::AggregateMethod { source, .. } => source,
        }
    }

    fn name(self) -> Ident {
        match self {
            Self::Function { func, .. } => func.node.name,
            Self::AggregateMethod { method, .. } => method.sig.name,
        }
    }

    fn body(self) -> &'a BlockNode {
        match self {
            Self::Function { func, .. } => &func.node.body,
            Self::AggregateMethod { method, .. } => &method.body,
        }
    }

    fn has_generics(self) -> bool {
        match self {
            Self::Function { func, .. } => {
                !func.node.type_params.is_empty() || !func.node.const_params.is_empty()
            }
            Self::AggregateMethod { method, .. } => {
                !method.sig.type_params.is_empty() || !method.sig.const_params.is_empty()
            }
        }
    }

    fn is_instance_method(self) -> bool {
        matches!(
            self,
            Self::AggregateMethod {
                mode: MethodMode::Instance { .. },
                ..
            }
        )
    }

    fn function_kind(self) -> FunctionKind {
        match self {
            Self::Function { .. } => FunctionKind::Normal,
            Self::AggregateMethod { .. } => FunctionKind::Method,
        }
    }
}

#[derive(Debug)]
struct ReachableCallables<'a> {
    index: &'a SourceProgramIndex<'a>,
    items: Vec<ReachableCallable<'a>>,
}

#[derive(Debug)]
struct ReachableCallable<'a> {
    callable: SourceCallable<'a>,
    body: BodyInstanceKey,
    fact: &'a SemanticFunctionInstanceFact,
    body_facts: ReachableBodyFacts<'a>,
    source: SourceId,
}

#[derive(Debug)]
enum ReachableBodyFacts<'a> {
    Facts(&'a SemanticBodyFacts),
    Empty(SemanticBodyFacts),
}

impl ReachableBodyFacts<'_> {
    fn as_facts(&self) -> &SemanticBodyFacts {
        match self {
            Self::Facts(facts) => facts,
            Self::Empty(facts) => facts,
        }
    }
}

fn can_omit_body_facts(fact: &SemanticFunctionInstanceFact, callable: SourceCallable<'_>) -> bool {
    let body = callable.body();
    fact.params.is_empty()
        && fact.return_ty == Type::Void
        && body.node.stmts.is_empty()
        && body.node.tail.is_none()
}

impl<'a> SourceProgramIndex<'a> {
    fn new(root: &'a ast::Program, resolved: &'a ResolveResult) -> Self {
        let modules = SourceModules::new(root, resolved);
        let mut callables = HashMap::new();
        let mut default_exprs = HashMap::new();

        for (module_index, module) in modules.items.iter().enumerate() {
            for stmt in &module.program.stmts {
                match &stmt.node {
                    Stmt::Func(func_node) => {
                        let id = CallableId::function(module.scope.clone(), func_node.node.name);
                        for param in &func_node.node.params {
                            if let Some(default) = &param.default {
                                default_exprs
                                    .insert((id.clone(), module.source, default.node.id), default);
                            }
                        }
                        callables.insert(
                            id,
                            SourceCallable::Function {
                                module: module_index,
                                func: func_node,
                                source: module.source,
                            },
                        );
                    }
                    Stmt::Aggregate(agg_node) => {
                        let agg = &agg_node.node;
                        let owner = NominalKey {
                            module: module.scope.clone(),
                            kind: agg.kind.into(),
                            name: agg.name,
                        };
                        for method in &agg.methods {
                            let mode = MethodMode::from_receiver(method.sig.receiver);
                            let id = CallableId::aggregate_method(
                                owner.clone(),
                                method.sig.name,
                                mode.surface(),
                            );
                            for param in &method.sig.params {
                                if let Some(default) = &param.default {
                                    default_exprs.insert(
                                        (id.clone(), module.source, default.node.id),
                                        default,
                                    );
                                }
                            }
                            callables.insert(
                                id,
                                SourceCallable::AggregateMethod {
                                    module: module_index,
                                    method,
                                    mode,
                                    source: module.source,
                                },
                            );
                        }
                    }
                    _ => {}
                }
            }
        }

        Self {
            modules,
            callables,
            default_exprs,
        }
    }
}

impl<'a> ReachableCallables<'a> {
    fn new(
        index: &'a SourceProgramIndex<'a>,
        semantic: &'a SemanticProgram,
        semantic_functions: &SemanticCallableFacts<'a>,
        roots: Vec<CallableInstanceKey>,
    ) -> Result<Self, LowerError> {
        let mut queued = std::collections::HashSet::new();
        let mut worklist = vec![];
        for root in roots {
            queue_callable(&mut queued, &mut worklist, root);
        }

        let mut items = vec![];
        let mut worklist_index = 0;
        while let Some(key) = worklist.get(worklist_index).cloned() {
            worklist_index += 1;
            let Some(source) = index.callables.get(&key.target).copied() else {
                return Err(LowerError::UnsupportedCallableInstance {
                    id: Box::new(key.target.clone()),
                    args: Box::new(key.args.clone()),
                });
            };
            if source.has_generics() && key.args.is_empty() {
                return Err(LowerError::MissingGenericInstanceArgs {
                    id: Box::new(key.target.clone()),
                });
            }
            let body = BodyInstanceKey::Callable(key.clone());
            let Some(fact) = semantic_functions.get(&key) else {
                return Err(LowerError::MissingFunctionFact {
                    id: Box::new(key.target.clone()),
                    args: Box::new(key.args.clone()),
                });
            };
            let body_facts = match semantic.facts.body(&body) {
                Some(facts) => ReachableBodyFacts::Facts(facts),
                None if can_omit_body_facts(fact, source) => {
                    ReachableBodyFacts::Empty(SemanticBodyFacts::default())
                }
                None => {
                    return Err(LowerError::MissingSpecializedBodyFacts {
                        body: Box::new(body.clone()),
                    });
                }
            };
            let mut calls = body_facts.as_facts().calls.iter().collect::<Vec<_>>();
            calls.sort_by_key(|(expr, _)| expr.0);
            for (expr, target) in calls {
                if target.form != CallForm::Normal {
                    return Err(LowerError::UnsupportedCallForm { expr_id: *expr });
                }
                if !index.callables.contains_key(&target.id) {
                    return Err(LowerError::UnsupportedCallableInstance {
                        id: Box::new(target.id.clone()),
                        args: Box::new(target.args.clone()),
                    });
                }
                let called = CallableInstanceKey {
                    target: target.id.clone(),
                    args: target.args.clone(),
                };
                queue_callable(&mut queued, &mut worklist, called);
            }
            enqueue_stringify_overrides(
                index,
                semantic,
                body_facts.as_facts(),
                &mut queued,
                &mut worklist,
            );
            items.push(ReachableCallable {
                callable: source,
                body,
                fact,
                body_facts,
                source: source.source(),
            });
        }

        Ok(Self { index, items })
    }
}

fn is_stringify_override(id: &CallableId) -> bool {
    id.kind == CallableKind::InstanceMethod && id.name == Ident::new("to_string")
}

fn queue_callable(
    queued: &mut std::collections::HashSet<CallableInstanceKey>,
    worklist: &mut Vec<CallableInstanceKey>,
    key: CallableInstanceKey,
) {
    if queued.insert(key.clone()) {
        worklist.push(key);
    }
}

fn enqueue_stringify_overrides(
    index: &SourceProgramIndex<'_>,
    semantic: &SemanticProgram,
    body_facts: &SemanticBodyFacts,
    queued: &mut std::collections::HashSet<CallableInstanceKey>,
    worklist: &mut Vec<CallableInstanceKey>,
) {
    let mut visited = std::collections::HashSet::new();
    for stringify in body_facts.stringifies.values() {
        enqueue_type_stringify_overrides(
            index,
            semantic,
            &stringify.source_ty,
            queued,
            worklist,
            &mut visited,
        );
    }
}

fn enqueue_type_stringify_overrides(
    index: &SourceProgramIndex<'_>,
    semantic: &SemanticProgram,
    ty: &Type,
    queued: &mut std::collections::HashSet<CallableInstanceKey>,
    worklist: &mut Vec<CallableInstanceKey>,
    visited: &mut std::collections::HashSet<Type>,
) {
    if !visited.insert(ty.clone()) {
        return;
    }
    match ty {
        Type::Optional { inner } | Type::List { elem: inner } | Type::Slice { elem: inner } => {
            enqueue_type_stringify_overrides(index, semantic, inner, queued, worklist, visited);
        }
        Type::Array { elem, .. } => {
            enqueue_type_stringify_overrides(index, semantic, elem, queued, worklist, visited);
        }
        Type::Map { key, value } => {
            enqueue_type_stringify_overrides(index, semantic, key, queued, worklist, visited);
            enqueue_type_stringify_overrides(index, semantic, value, queued, worklist, visited);
        }
        Type::Tuple(items) => {
            for item in items {
                enqueue_type_stringify_overrides(index, semantic, item, queued, worklist, visited);
            }
        }
        Type::Nominal(_) => {
            enqueue_nominal_stringify_override(index, semantic, ty, queued, worklist, visited);
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
}

fn enqueue_nominal_stringify_override(
    index: &SourceProgramIndex<'_>,
    semantic: &SemanticProgram,
    ty: &Type,
    queued: &mut std::collections::HashSet<CallableInstanceKey>,
    worklist: &mut Vec<CallableInstanceKey>,
    visited: &mut std::collections::HashSet<Type>,
) {
    if !type_is_concrete(ty) {
        return;
    }
    let Some(owner) = semantic.declarations.key_for_type(ty) else {
        return;
    };
    if let Some(aggregate) = semantic.declarations.aggregate(&owner) {
        if aggregate.stringify_override().is_some() {
            let Some(args) = nominal_generic_args(ty) else {
                return;
            };
            let key = CallableInstanceKey {
                target: CallableId::aggregate_method(
                    owner,
                    Ident::new("to_string"),
                    MethodSurface::Instance,
                ),
                args,
            };
            if index.callables.contains_key(&key.target) {
                queue_callable(queued, worklist, key);
            }
            return;
        }
        for field in aggregate.fields.values() {
            let field_ty = substitute_aggregate_member(ty, &aggregate.generics, &field.ty);
            enqueue_type_stringify_overrides(index, semantic, &field_ty, queued, worklist, visited);
        }
        return;
    }

    let Some(schema) = semantic.declarations.enum_schema(&owner) else {
        return;
    };
    for variant in schema.variants.values() {
        variant.payload.for_each_type(|payload_ty| {
            let payload_ty = substitute_aggregate_member(ty, &schema.generics, payload_ty);
            enqueue_type_stringify_overrides(
                index,
                semantic,
                &payload_ty,
                queued,
                worklist,
                visited,
            );
        });
    }
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
        ast, externs,
        externs::{ExternInputs, PackageExternInputs},
        test_support::{
            checked_with_full_core_shape, parse_program, resolved_modules_with_core_option,
            resolved_modules_with_core_option_external,
        },
        typecheck::{self, TypecheckConfig},
    };

    #[test]
    fn empty_program_lowers_to_verified_air() {
        lower_empty("").expect("lower failed");
    }

    #[test]
    fn type_lowerer_reuses_recursive_composite_types() {
        let mut program = Program::default();
        let mut lowerer = TypeLowerer::default();
        let ty = Type::Map {
            key: Box::new(Type::String),
            value: Box::new(Type::Optional {
                inner: Box::new(Type::List {
                    elem: Box::new(Type::Int),
                }),
            }),
        };

        let first = lowerer.lower(&mut program, &ty).expect("lower failed");
        let second = lowerer.lower(&mut program, &ty).expect("lower failed");

        assert_eq!(first, second);
        assert!(matches!(program.type_data(first), TypeData::Map { .. }));
    }

    #[test]
    fn type_lowerer_lowers_slice_dyn_and_function_types() {
        let mut program = Program::default();
        let mut lowerer = TypeLowerer::default();
        let func = Type::func(
            vec![ast::FuncParam::new(
                Type::Slice {
                    elem: Box::new(Type::Float),
                },
                false,
                false,
                ast::EscapeMode::NonEscaping,
            )],
            ast::ReturnSpec::value(Type::Dyn(ast::ContractRef::Named {
                qualifier: None,
                name: Ident::new("Drawable"),
                origin: None,
            })),
        );

        let id = lowerer.lower(&mut program, &func).expect("lower failed");

        assert!(matches!(program.type_data(id), TypeData::Function(_)));
        assert!(
            program
                .type_arena
                .iter()
                .any(|data| matches!(data, TypeData::Slice(_)))
        );
        assert!(
            program
                .type_arena
                .iter()
                .any(|data| matches!(data, TypeData::Dyn(_)))
        );
    }

    #[test]
    fn type_lowerer_lowers_nominal_aggregate_declarations() {
        let (_, _, semantic) = checked_with_full_core_shape("struct Point { x: int, y: string }");
        let mut program = Program::default();
        let mut modules = HashMap::new();
        let mut lowerer = TypeLowerer::default();
        let point = Type::nominal(
            ast::NominalKind::Struct,
            Ident::new("Point"),
            vec![],
            vec![],
            None,
        );

        let id = lowerer
            .lower_source(
                &mut program,
                &mut modules,
                &semantic.program.declarations,
                &semantic.program.externs,
                &point,
            )
            .expect("lower failed");

        let TypeData::Aggregate(agg) = program.type_data(id) else {
            panic!("expected aggregate type");
        };
        let decl = program.aggregate(*agg);
        assert_eq!(decl.fields.len(), 2);
        assert_eq!(decl.fields[0].name, Ident::new("x"));
        assert_eq!(program.type_data(decl.fields[0].ty), &TypeData::Int);
    }

    #[test]
    fn type_lowerer_marks_datarefs_cycle_capable() {
        let (_, _, semantic) = checked_with_full_core_shape("dataref Node { next: Node? }");
        let mut program = Program::default();
        let mut modules = HashMap::new();
        let mut lowerer = TypeLowerer::default();
        let node = Type::nominal(
            ast::NominalKind::DataRef,
            Ident::new("Node"),
            vec![],
            vec![],
            None,
        );

        let id = lowerer
            .lower_source(
                &mut program,
                &mut modules,
                &semantic.program.declarations,
                &semantic.program.externs,
                &node,
            )
            .expect("lower failed");

        let TypeData::DataRef(agg) = program.type_data(id) else {
            panic!("expected dataref type");
        };
        assert!(program.aggregate(*agg).cycle_capable);
    }

    #[test]
    fn type_lowerer_lowers_nominal_enum_declarations() {
        let (_, _, semantic) =
            checked_with_full_core_shape("enum Choice { A, B(int), C { text: string } }");
        let mut program = Program::default();
        let mut modules = HashMap::new();
        let mut lowerer = TypeLowerer::default();
        let choice = Type::nominal(
            ast::NominalKind::Enum,
            Ident::new("Choice"),
            vec![],
            vec![],
            None,
        );

        let id = lowerer
            .lower_source(
                &mut program,
                &mut modules,
                &semantic.program.declarations,
                &semantic.program.externs,
                &choice,
            )
            .expect("lower failed");

        let TypeData::Enum(enum_id) = program.type_data(id) else {
            panic!("expected enum type");
        };
        assert_eq!(program.enum_decl(*enum_id).variants.len(), 3);
    }

    #[test]
    fn full_core_shape_replaces_option_only_core_for_containment() {
        let (root, resolved, semantic) = checked_with_full_core_shape("");
        let air = lower_with_modules(
            &root,
            &resolved,
            &semantic.program,
            AirLowerConfig::default(),
        )
        .expect("lower failed");

        assert!(air.modules.is_empty());
        assert_eq!(
            resolved.system.core,
            Some(crate::resolve::PackageId::core())
        );
        assert_eq!(
            core_fixture_module_paths(&resolved),
            vec![
                vec![],
                vec!["option"],
                vec!["result"],
                vec!["range"],
                vec!["collections"],
                vec!["runtime"],
                vec!["core_int"],
                vec!["core_float"],
                vec!["core_string"],
            ]
        );
        let core_root = crate::resolve::ModuleId::root(crate::resolve::PackageId::core());
        let root_imports = resolved
            .import_edges
            .get(&core_root)
            .expect("core root imports");
        assert_eq!(
            root_imports
                .iter()
                .map(|target| target.default_name.as_str())
                .collect::<Vec<_>>(),
            vec![
                "core_int",
                "core_float",
                "core_string",
                "runtime",
                "option",
                "result",
                "range",
                "collections",
            ]
        );
    }

    #[test]
    fn empty_full_core_shape_lowers_to_empty_air() {
        let air = lower_full_core("").expect("lower failed");

        assert!(air.modules.is_empty());
        assert!(air.functions.is_empty());
        assert!(air.externs.is_empty());
        assert!(air.aggregates.is_empty());
        assert!(air.enums.is_empty());
        assert!(air.extern_types.is_empty());
        assert!(air.type_arena.is_empty());
        assert!(air.entry.is_none());
    }

    #[test]
    fn root_full_core_shape_lowers_only_root_function() {
        let air = lower_full_core_root("fn main() {}", "main").expect("lower failed");

        assert_eq!(air.modules.len(), 1);
        assert!(air.modules[0].path.is_empty());
        assert_eq!(air.functions.len(), 1);
        assert_eq!(air.functions[0].name.as_str(), "main");
        assert!(air.externs.is_empty());
        assert!(air.aggregates.is_empty());
        assert!(air.enums.is_empty());
        assert!(air.extern_types.is_empty());
        assert!(air.entry.is_none());
    }

    #[test]
    fn primitive_root_with_full_core_shape_emits_no_core() {
        let air = lower_full_core_root(
            "
            fn main() -> int {
                let a = 1;
                var b = a + 2;
                b += 3;
                b
            }
            ",
            "main",
        )
        .expect("lower failed");

        assert_eq!(air.modules.len(), 1);
        assert!(air.modules[0].path.is_empty());
        assert_eq!(air.functions.len(), 1);
        assert_eq!(air.functions[0].name.as_str(), "main");
        assert!(air.externs.is_empty());
        assert!(air.aggregates.is_empty());
        assert!(air.enums.is_empty());
        assert!(air.extern_types.is_empty());
        assert!(air.type_arena.iter().all(|ty| matches!(ty, TypeData::Int)));
    }

    #[test]
    fn helper_call_full_core_shape_emits_only_reachable_user_functions() {
        let air = lower_full_core_entry(
            "fn unused() {} fn helper() {} fn main() { helper(); }",
            "main",
            &[],
        )
        .expect("lower failed");

        assert_eq!(function_names(&air), vec!["main", "helper"]);
        assert!(air.externs.is_empty());
    }

    #[test]
    fn println_int_full_core_shape_emits_runtime_slice_only() {
        let air =
            lower_full_core_entry("fn main() { println(1); }", "main", &[]).expect("lower failed");

        assert_eq!(air.entry, Some(FunctionId::from_index(0)));
        assert_eq!(function_names(&air), vec!["main", "println"]);
        assert_eq!(extern_names(&air), vec!["_println"]);
        assert_eq!(
            module_paths(&air),
            vec![vec![], vec!["runtime"], vec!["core_runtime"]]
        );
        assert_no_deferred_core_decls(&air);

        let println = &air.functions[1];
        assert_eq!(println.signature.params.len(), 1);
        assert!(matches!(
            air.type_arena.data(println.signature.params[0].ty),
            TypeData::Int
        ));
        assert_eq!(stringify_source_types(&air), vec![TypeData::Int]);
        assert_extern_signature(&air, "_println", &[TypeData::String], &TypeData::Void);
    }

    #[test]
    fn println_string_full_core_shape_emits_one_println_instance() {
        let air = lower_full_core_entry("fn main() { println(\"ready\"); }", "main", &[])
            .expect("lower failed");

        assert_eq!(function_names(&air), vec!["main", "println"]);
        assert_eq!(extern_names(&air), vec!["_println"]);
        assert_eq!(stringify_source_types(&air), vec![TypeData::String]);
        assert_no_deferred_core_decls(&air);
    }

    #[test]
    fn assert_full_core_shape_emits_assert_runtime_slice_only() {
        let air = lower_full_core_entry("fn main() { assert(true); }", "main", &[])
            .expect("lower failed");

        assert_eq!(function_names(&air), vec!["main", "assert"]);
        assert_eq!(extern_names(&air), vec!["_assert"]);
        assert_eq!(
            module_paths(&air),
            vec![vec![], vec!["runtime"], vec!["core_runtime"]]
        );
        assert!(air.const_arena.iter().any(|data| {
            matches!(&data.value, ConstValue::String(value) if value.as_ref() == "assertion failed")
        }));
        assert_extern_signature(
            &air,
            "_assert",
            &[TypeData::Bool, TypeData::String],
            &TypeData::Void,
        );
        assert_no_deferred_core_decls(&air);
    }

    #[test]
    fn assert_explicit_message_full_core_shape_uses_runtime_assert_only() {
        let air = lower_full_core_entry("fn main() { assert(false, \"msg\"); }", "main", &[])
            .expect("lower failed");

        assert_eq!(function_names(&air), vec!["main", "assert"]);
        assert_eq!(extern_names(&air), vec!["_assert"]);
        assert!(air.const_arena.iter().any(|data| {
            matches!(&data.value, ConstValue::String(value) if value.as_ref() == "msg")
        }));
    }

    #[test]
    fn unused_extension_block_does_not_affect_air() {
        let air = lower_full_core_root(
            "
            extend int {
                fn plus_one(self) -> int { self + 1 }
                fn print_it(self) { println(self); }
            }
            fn main() {}
            ",
            "main",
        )
        .expect("lower failed");

        assert_eq!(function_names(&air), vec!["main"]);
        assert!(air.externs.is_empty());
        assert_no_deferred_core_decls(&air);
    }

    #[test]
    fn reachable_extension_method_is_explicitly_unsupported() {
        let err = lower_root(
            "
            extend int { fn plus_one(self) -> int { self + 1 } }
            fn main() -> int { 1.plus_one() }
            ",
            "main",
        )
        .expect_err("expected unsupported extension method");

        assert!(matches!(
            err,
            LowerError::UnsupportedCallableInstance { .. }
        ));
    }

    #[test]
    fn reachable_core_extension_method_is_explicitly_unsupported() {
        let err = lower_full_core_entry("fn main() { let x = (-1).abs(); }", "main", &[])
            .expect_err("expected unsupported extension method");

        assert!(matches!(
            err,
            LowerError::UnsupportedCallableInstance { id, .. }
                if id.name.as_str() == "abs" && id.parent.is_some()
        ));
    }

    #[test]
    fn reachable_qualified_core_extension_call_is_explicitly_unsupported() {
        let (root, resolved, semantic) = checked_with_modules(
            "import facade; fn main() -> int { facade.a.pick(1) }",
            &[
                ("facade", "pub import a;"),
                ("a", "pub extend int { fn pick(self) -> int { 1 } }"),
            ],
        );
        let err = lower_checked_entry(&root, &resolved, &semantic.program, "main", &[])
            .expect_err("expected unsupported qualified extension call");

        assert!(matches!(err, LowerError::UnsupportedCallForm { .. }));
    }

    #[test]
    fn reachable_string_extension_optional_return_is_explicitly_unsupported() {
        let err = lower_full_core_entry(
            "fn main() { let x = \"abc\".substring(0, 1); }",
            "main",
            &[],
        )
        .expect_err("expected unsupported string extension");

        assert!(matches!(
            err,
            LowerError::UnsupportedCallableInstance { .. } | LowerError::UnsupportedType { .. }
        ));
    }

    #[test]
    fn reachable_core_option_constructor_is_explicitly_unsupported() {
        let err = lower_full_core_entry(
            "fn main() { let x: Option<int> = Option.Some(1); }",
            "main",
            &[],
        )
        .expect_err("expected unsupported option constructor");

        assert!(matches!(
            err,
            LowerError::UnsupportedCallableInstance { .. } | LowerError::UnsupportedType { .. }
        ));
    }

    #[test]
    fn unused_const_and_lazy_globals_do_not_affect_air() {
        let air = lower_root(
            "
            const ANSWER: int = 42;
            lazy let Value: int = 1;
            lazy var Counter: int = 0;
            fn main() {}
            ",
            "main",
        )
        .expect("lower failed");

        assert_eq!(function_names(&air), vec!["main"]);
        assert!(air.externs.is_empty());
        assert!(air.aggregates.is_empty());
        assert!(air.enums.is_empty());
    }

    #[test]
    fn reached_lazy_global_access_is_explicitly_unsupported() {
        let err = lower_root(
            "lazy let Value: int = 1; fn main() -> int { Value }",
            "main",
        )
        .expect_err("expected unsupported global access");

        assert!(matches!(err, LowerError::UnsupportedExpr { .. }));
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
        assert!(modules.items.iter().all(|module| {
            !module_path(&module.scope).is_empty() || matches!(module.scope, ModuleScope::Root)
        }));
    }

    #[test]
    fn source_functions_pair_with_semantic_facts() {
        with_source_functions("fn f(a: int) -> int { a }", &["f"], |_, functions, _| {
            assert_eq!(functions.items.len(), 1);
            let function = &functions.items[0];
            assert_eq!(function.callable.module(), 0);
            assert_eq!(function.callable.name(), Ident::new("f"));
            assert_eq!(function.body, function.fact.body);
        });
    }

    #[test]
    fn air_roots_normalize_entry_first_and_deduplicate() {
        let entry = root_function("main");
        let helper = root_function("helper");
        let roots = AirRoots {
            entry: Some(entry.clone()),
            callables: vec![helper.clone(), entry.clone(), helper.clone()],
        };

        assert_eq!(roots.normalized(), vec![entry, helper]);
    }

    #[test]
    fn missing_root_fact_is_rejected_before_traversal() {
        let (root, resolved, semantic) = checked("fn main() {}");
        let missing = root_function("missing");
        let err = lower_with_modules(
            &root,
            &resolved,
            &semantic.program,
            AirLowerConfig {
                roots: AirRoots {
                    entry: None,
                    callables: vec![missing],
                },
            },
        )
        .expect_err("expected missing root fact");

        assert!(matches!(err, LowerError::MissingFunctionFact { .. }));
    }

    #[test]
    fn unsupported_root_callable_kind_is_rejected() {
        let (root, resolved, semantic) = checked("extern fn host();");
        let root_key = CallableInstanceKey {
            target: CallableId::extern_function(ModuleScope::Root, Ident::new("host")),
            args: GenericArgs::default(),
        };
        let err = lower_with_modules(
            &root,
            &resolved,
            &semantic.program,
            AirLowerConfig {
                roots: AirRoots {
                    entry: None,
                    callables: vec![root_key],
                },
            },
        )
        .expect_err("expected unsupported root");

        assert!(matches!(err, LowerError::UnsupportedRootCallable { .. }));
    }

    #[test]
    fn non_concrete_root_args_are_rejected() {
        let (root, resolved, semantic) = checked("fn main() {}");
        let root_key = CallableInstanceKey {
            target: CallableId::function(ModuleScope::Root, Ident::new("main")),
            args: GenericArgs {
                type_args: vec![Type::Infer],
                const_args: vec![],
            },
        };
        let err = lower_with_modules(
            &root,
            &resolved,
            &semantic.program,
            AirLowerConfig {
                roots: AirRoots {
                    entry: None,
                    callables: vec![root_key],
                },
            },
        )
        .expect_err("expected non-concrete root");

        assert!(matches!(err, LowerError::NonConcreteRoot { .. }));
    }

    #[test]
    fn source_functions_include_called_generic_instance() {
        with_source_functions(
            "fn f<T>(x: T) -> T { x } fn main() -> int { f(1) }",
            &["main"],
            |_, functions, _| {
                assert!(functions.items.iter().any(|function| {
                    function.callable.name() == Ident::new("f")
                        && function.fact.args.type_args == vec![Type::Int]
                }));
            },
        );
    }

    #[test]
    fn generic_function_call_lowers_once() {
        let source = "fn id<T>(x: T) -> T { x } fn f() -> int { id(1) + id(2) }";
        let air = lower_root(source, "f").expect("lower failed");

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
        let air = lower_roots(source, &["f", "g"]).expect("lower failed");

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
        let air = lower_root(source, "f").expect("lower failed");

        let wrap = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("wrap"))
            .expect("missing wrap");
        assert!(function_statements(wrap).any(|statement| {
            matches!(statement, Statement::Init { value: RValue::Call { callee: Callee::Function(_), args }, .. } if args.len() == 1)
        }));
    }

    #[test]
    fn uncalled_generic_function_is_not_lowered() {
        let air =
            lower_root("fn id<T>(x: T) -> T { x } fn main() {}", "main").expect("lower failed");

        assert!(
            air.functions
                .iter()
                .all(|function| function.name != Ident::new("id"))
        );
    }

    #[test]
    fn generic_call_order_drives_function_order() {
        let source = r#"fn id<T>(x: T) -> T { x } fn f() { let a = id("x"); let b = id(1); }"#;
        let air = lower_root(source, "f").expect("lower failed");
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
        let err = lower_checked_roots(&root, &resolved, &semantic.program, &["f"])
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
        let err = lower_checked_roots(&root, &resolved, &semantic.program, &["f"])
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
        let err = lower_checked_roots(&root, &resolved, &semantic.program, &["f"])
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

        lower_checked_roots(&root, &resolved, &semantic.program, &["main"]).expect("lower failed");
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
        let err = lower_checked_roots(&root, &resolved, &semantic.program, &["main"])
            .expect_err("expected missing generic args");

        assert!(matches!(err, LowerError::MissingGenericInstanceArgs { .. }));
    }

    #[test]
    fn stringify_int_lowers_to_string_typed_air() {
        let source = "fn f() -> string { #stringify(1) }";
        let air = lower_root(source, "f").expect("lower failed");
        let string_ty = PrimitiveTypes::scan(&air).string().expect("string type");

        assert!(program_statements(&air).any(|statement| {
            matches!(
                statement,
                Statement::Init {
                    value: RValue::Stringify { value: _, source_ty },
                    ..
                } if *source_ty != string_ty
            )
        }));
    }

    #[test]
    fn non_stringify_intrinsic_is_unsupported() {
        let source = "fn f() -> string { #file() }";
        let err = lower_root(source, "f").expect_err("expected unsupported intrinsic");

        assert!(matches!(
            err,
            LowerError::UnsupportedExpr {
                kind: "IntrinsicCall",
                ..
            }
        ));
    }

    #[test]
    fn stringify_void_call_lowers_effect_then_void_constant() {
        let source = r#"fn side() {} fn f() -> string { #stringify(side()) }"#;
        let air = lower_full_core_root(source, "f").expect("lower failed");
        let function = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("f"))
            .expect("function missing");

        assert!(matches!(
            function.body[0].statements.first(),
            Some(Statement::Eval(RValue::Call { .. }))
        ));
        assert!(function.body.iter().any(|block| {
            matches!(
                block.terminator,
                Terminator::Return(Some(Operand::Const(id)))
                    if matches!(air.const_data(id).value, ConstValue::String(ref s) if s.as_ref() == "<void>")
            )
        }));
    }

    #[test]
    fn stringify_list_param_lowers_composite_source_type() {
        let source = "fn f(xs: [int]) -> string { #stringify(xs) }";
        let air = lower_full_core_root(source, "f").expect("lower failed");

        assert!(program_statements(&air).any(|statement| {
            matches!(
                statement,
                Statement::Init {
                    value: RValue::Stringify { source_ty, .. },
                    ..
                } if matches!(air.type_data(*source_ty), TypeData::List(_))
            )
        }));
    }

    #[test]
    fn generic_stringify_lowers_specialized_source_type() {
        let source = "fn f<T>(x: T) -> string { #stringify(x) } fn main() -> string { f(1) }";
        let air = lower_root(source, "main").expect("lower failed");
        let int_ty = PrimitiveTypes::scan(&air).int().expect("int type");

        let function = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("f"))
            .expect("function missing");
        assert!(function_statements(function).any(|statement| {
            matches!(
                statement,
                Statement::Init {
                    value: RValue::Stringify { value: _, source_ty },
                    ..
                } if *source_ty == int_ty
            )
        }));
    }

    #[test]
    fn function_stringify_reports_missing_value_lowering() {
        let source = "fn g() {} fn f() -> string { #stringify(g) }";
        let err = lower_root(source, "f").expect_err("expected unsupported expression");

        assert!(matches!(err, LowerError::UnsupportedExpr { .. }));
    }

    #[test]
    fn generic_function_stringify_reports_missing_value_lowering() {
        let source =
            "fn g() {} fn f<T>(x: T) -> string { #stringify(x) } fn main() -> string { f(g) }";
        let err = lower_root(source, "main").expect_err("expected unsupported expression");

        assert!(matches!(err, LowerError::UnsupportedExpr { .. }));
    }

    #[test]
    fn source_functions_keep_default_param_instances() {
        with_source_functions(
            "fn f(a: int = 1) -> int { a }",
            &["f"],
            |_, functions, _| {
                assert_eq!(functions.items.len(), 1);
                assert_eq!(functions.items[0].callable.name(), Ident::new("f"));
                assert_eq!(functions.items[0].fact.params.len(), 1);
            },
        );
    }

    #[test]
    fn empty_roots_allocate_no_modules() {
        let air = lower_empty("").expect("lower failed");

        assert!(air.modules.is_empty());
    }

    #[test]
    fn single_root_allocates_only_reached_module() {
        let (root, resolved, semantic) =
            checked_with_modules("import util; fn main() {}", &[("util", "fn helper() {}")]);
        let air = lower_checked_roots(&root, &resolved, &semantic.program, &["main"])
            .expect("lower failed");

        assert_eq!(air.modules.len(), 1);
        assert_eq!(air.functions.len(), 1);
        assert_eq!(air.functions[0].name, Ident::new("main"));
    }

    #[test]
    fn executable_root_sets_entry() {
        let air = lower_entry("fn main() {}", "main", &[]).expect("lower failed");

        assert_eq!(air.functions.len(), 1);
        assert_eq!(air.entry, Some(FunctionId::from_index(0)));
        assert_eq!(air.function(air.entry.unwrap()).name, Ident::new("main"));
    }

    #[test]
    fn entry_root_preserved_when_already_in_callables() {
        let air = lower_entry("fn helper() {} fn main() {}", "main", &["helper", "main"])
            .expect("lower failed");

        assert_eq!(air.functions.len(), 2);
        assert_eq!(air.entry, Some(FunctionId::from_index(0)));
        assert_eq!(air.functions[0].name, Ident::new("main"));
        assert_eq!(air.functions[1].name, Ident::new("helper"));
    }

    #[test]
    fn multi_root_without_entry_leaves_entry_unset() {
        let air = lower_roots("fn a() {} fn b() {}", &["a", "b"]).expect("lower failed");

        assert_eq!(air.entry, None);
        assert_eq!(air.functions.len(), 2);
    }

    #[test]
    fn invalid_entry_root_is_rejected() {
        let err = lower_entry("fn main() {}", "missing", &[]).expect_err("expected error");

        assert!(matches!(err, LowerError::MissingFunctionFact { .. }));
    }

    #[test]
    fn reachable_extern_allocates_function_module_first() {
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
                    name: "host_log".to_string(),
                    doc: None,
                    signature: ExternSignature {
                        params: vec![ExternParam {
                            name: Some("message".to_string()),
                            ty: ExternTypeExpr::String,
                            flow: ParamFlow::Value,
                            escape: CallbackEscape::NonEscaping,
                        }],
                        ret: ExternTypeExpr::Void,
                    },
                    effects: ExternEffects::default(),
                }],
            }],
        };
        let (root, resolved, semantic) = checked_with_provider(
            "import ext:host { host_log }; fn f() { host_log(\"ok\"); }",
            provider,
        );
        let air =
            lower_checked_roots(&root, &resolved, &semantic.program, &["f"]).expect("lower failed");

        assert!(
            air.modules[0]
                .functions
                .iter()
                .any(|id| air.function(*id).name == Ident::new("f"))
        );
        assert!(
            air.modules[1]
                .externs
                .iter()
                .any(|id| air.extern_decl(*id).name == Ident::new("host_log"))
        );
    }

    #[test]
    fn extern_function_use_lowers_declaration() {
        with_source_functions(
            "extern fn host_log(message: string); fn f() { host_log(\"ok\"); }",
            &["f"],
            |_modules, functions, semantic| {
                let mut cx = LowerCx::default();
                cx.lower_extern_declarations(functions, semantic)
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
            },
        );
    }

    #[test]
    fn source_function_call_lowers() {
        let source = "fn add(a: int, b: int) -> int { a + b } fn f() -> int { add(1, 2) }";
        let air = lower_root(source, "f").expect("lower failed");

        assert!(program_statements(&air).any(|statement| {
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
        }));
    }

    #[test]
    fn reachable_struct_literal_return_value_is_unsupported() {
        let source =
            "struct S { x: int } fn make() -> S { S { x: 1 } } fn main() { let s = make(); }";
        let err = lower_root(source, "main").expect_err("expected error");

        assert!(matches!(err, LowerError::UnsupportedExpr { .. }));
    }

    #[test]
    fn method_call_lowers_inherent_method_with_receiver() {
        let source =
            "struct S { x: int fn value(self) -> int { 1 } } fn f(s: S) -> int { s.value() }";
        let air = lower_root(source, "f").expect("lower failed");
        let method = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("value"))
            .expect("method missing");

        assert_eq!(method.kind, FunctionKind::Method);
        assert!(matches!(
            method.signature.params[0].role,
            ParamRole::Receiver
        ));
        assert!(program_statements(&air).any(|statement| {
            matches!(
                statement,
                Statement::Init {
                    value: RValue::Call {
                        callee: Callee::Function(_),
                        args,
                    },
                    ..
                } if args.len() == 1
            )
        }));
    }

    #[test]
    fn method_call_lowers_default_args_after_receiver() {
        let source =
            "struct S { fn value(self, x: int = 1) -> int { x } } fn f(s: S) -> int { s.value() }";
        let air = lower_root(source, "f").expect("lower failed");
        let method = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("value"))
            .expect("method missing");

        assert_eq!(method.signature.params.len(), 2);
        assert!(matches!(
            method.signature.params[0].role,
            ParamRole::Receiver
        ));
        assert!(program_statements(&air).any(|statement| {
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
        }));
    }

    #[test]
    fn generic_owner_method_call_uses_concrete_function_fact() {
        let source =
            "struct Box<T> { fn id(self, x: T) -> T { x } } fn f(b: Box<int>) -> int { b.id(1) }";
        let air = lower_root(source, "f").expect("lower failed");
        let method = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("id"))
            .expect("method missing");

        assert!(matches!(
            air.type_data(method.signature.return_type),
            TypeData::Int
        ));
        assert!(matches!(
            air.type_data(method.signature.params[1].ty),
            TypeData::Int
        ));
    }

    #[test]
    fn nested_stringified_owner_emits_field_to_string_override() {
        let source = r#"
            struct Inner { fn to_string(self) -> string { "inner" } }
            struct Outer { inner: Inner }
            fn f(outer: Outer) -> string { #stringify(outer) }
        "#;
        let air = lower_full_core_root(source, "f").expect("lower failed");

        assert_stringify_override(&air, "Inner");
    }

    #[test]
    fn stringified_enum_emits_payload_to_string_override() {
        let source = r#"
            struct Inner { fn to_string(self) -> string { "inner" } }
            enum Wrapped { Some(Inner), Named { inner: Inner }, None }
            fn f(value: Wrapped) -> string { #stringify(value) }
        "#;
        let air = lower_full_core_root(source, "f").expect("lower failed");

        assert_stringify_override(&air, "Inner");
    }

    #[test]
    fn owner_stringify_override_stops_structural_override_walk() {
        let source = r#"
            struct Inner { fn to_string(self) -> string { "inner" } }
            struct Outer { inner: Inner fn to_string(self) -> string { "outer" } }
            fn f(outer: Outer) -> string { #stringify(outer) }
        "#;
        let air = lower_full_core_root(source, "f").expect("lower failed");
        let overrides = air
            .functions
            .iter()
            .filter(|function| function.name == Ident::new("to_string"))
            .collect::<Vec<_>>();

        assert_eq!(overrides.len(), 1);
        assert_eq!(overrides[0].signature.params.len(), 1);
        let owner_ty = overrides[0].signature.params[0].ty;
        assert!(
            matches!(air.type_data(owner_ty), TypeData::Aggregate(agg) if air.aggregate(*agg).name == Ident::new("Outer"))
        );
    }

    #[test]
    fn override_body_lowers_calls_defaults_and_stringify() {
        let source = r#"
            fn helper(x: int = 1) -> int { x }
            struct Box { value: int fn to_string(self) -> string { #stringify(helper()) } }
            fn f(value: Box) -> string { #stringify(value) }
        "#;
        let air = lower_full_core_root(source, "f").expect("lower failed");
        let override_fn = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("to_string"))
            .expect("override missing");

        assert!(function_statements(override_fn).any(|statement| {
            matches!(
                statement,
                Statement::Init {
                    value: RValue::Call { args, .. },
                    ..
                } if args.len() == 1
            )
        }));
        assert!(function_statements(override_fn).any(|statement| {
            matches!(
                statement,
                Statement::Init {
                    value: RValue::Stringify { .. },
                    ..
                }
            )
        }));
    }

    #[test]
    fn generic_override_body_fact_replay_is_stable() {
        let source = r#"
            struct Box<T> { value: T fn to_string(self) -> string { #stringify(1) } }
            fn first(value: Box<int>) -> string { value.to_string() }
            fn second(value: Box<int>) -> string { value.to_string() }
        "#;
        let air = lower_roots(source, &["first", "second"]).expect("lower failed");
        let overrides = air
            .functions
            .iter()
            .filter(|function| function.name == Ident::new("to_string"))
            .collect::<Vec<_>>();

        assert_eq!(overrides.len(), 1);
        assert!(!matches!(
            overrides[0].body[0].terminator,
            Terminator::Unreachable
        ));
    }

    #[test]
    fn generic_to_string_overrides_are_emitted_per_owner_instance() {
        let source = r#"
            struct Box<T> { value: T fn to_string(self) -> string { "box" } }
            fn ints(value: Box<int>) -> string { #stringify(value) }
            fn strings(value: Box<string>) -> string { #stringify(value) }
        "#;
        let air = lower_roots(source, &["ints", "strings"]).expect("lower failed");
        let overrides = air
            .functions
            .iter()
            .filter(|function| function.name == Ident::new("to_string"))
            .collect::<Vec<_>>();

        assert_eq!(overrides.len(), 2);
        assert_ne!(
            overrides[0].signature.params[0].ty,
            overrides[1].signature.params[0].ty
        );
        assert!(
            overrides.iter().all(|function| {
                !matches!(function.body[0].terminator, Terminator::Unreachable)
            })
        );
    }

    #[test]
    fn promoted_method_call_is_unsupported() {
        let source = "struct Health { fn damage(self) -> int { 1 } } struct Enemy { embed health: Health } fn f(enemy: Enemy) -> int { enemy.damage() }";
        let err = lower_root(source, "f").expect_err("expected promoted method rejection");

        assert!(matches!(err, LowerError::UnsupportedExpr { .. }));
    }

    #[test]
    fn qualified_extend_call_is_unsupported_call_form() {
        let (root, resolved, semantic) = checked_with_modules(
            "import facade; fn use_it() -> int { facade.pick(1) }",
            &[
                ("facade", "pub import a { * } ;"),
                ("a", "pub extend int { fn pick(self) -> int { 1 } }"),
            ],
        );
        let err = lower_checked_roots(&root, &resolved, &semantic.program, &["use_it"])
            .expect_err("expected error");

        assert!(matches!(err, LowerError::UnsupportedCallForm { .. }));
    }

    #[test]
    fn runtime_default_arg_is_unsupported() {
        let source = r#"fn fallback() -> string { "ok" } fn ok(message: string = fallback()) -> string { message } fn f() -> string { ok() }"#;
        let (root, resolved, semantic) = checked(source);
        let err = lower_checked_roots(&root, &resolved, &semantic.program, &["f"])
            .expect_err("expected error");
        assert!(matches!(err, LowerError::UnsupportedDefaultArg { .. }));
    }

    #[test]
    fn default_arg_lowers_to_literal_operand() {
        let source =
            r#"fn ok(message: string = "ok") -> string { message } fn f() -> string { ok() }"#;
        let air = lower_root(source, "f").expect("lower failed");

        assert!(program_statements(&air).any(|statement| {
            matches!(
                statement,
                Statement::Init {
                    value: RValue::Call { args, .. },
                    ..
                } if args.len() == 1 && matches!(args[0], Operand::Const(_))
            )
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
        let air = lower_checked_roots(&root, &resolved, &semantic.program, &["main"])
            .expect("lower failed");

        assert_eq!(air.externs.len(), 1);
        assert!(program_statements(&air).any(|statement| {
            matches!(
                statement,
                Statement::Eval(RValue::Call {
                    callee: Callee::Extern(_),
                    ..
                })
            )
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
        let err = lower_checked_roots(&root, &resolved, &semantic.program, &["f"])
            .expect_err("expected error");
        assert!(matches!(err, LowerError::UnsupportedExternSignature));
    }

    #[test]
    fn concrete_function_lowers_to_verified_body() {
        let air = lower_root("fn f(var a: int) -> int { a }", "f").expect("lower failed");
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
        let air = lower_root("fn f() {}", "f").expect("lower failed");
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
        let air = lower_root(source, "f").expect("lower failed");
        assert_eq!(air.functions.len(), 1);
        let function = &air.functions[0];
        assert!(function_statements(function).any(|stmt| matches!(stmt, Statement::Assign { .. })));
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
        let air = lower_root(source, "f").expect("lower failed");
        let function = &air.functions[0];
        assert!(matches!(
            function.body[0].terminator,
            Terminator::Return(Some(_))
        ));
    }

    #[test]
    fn string_concat_with_struct_lowers_side_through_stringify() {
        let source = r#"struct S {} fn f(s: S) -> string { "s: " + s }"#;
        let air = lower_full_core_root(source, "f").expect("lower failed");

        assert!(program_statements(&air).any(|statement| matches!(
            statement,
            Statement::Init {
                value: RValue::Stringify { .. },
                ..
            }
        )));
        assert!(program_statements(&air).any(|statement| {
            matches!(statement, Statement::Init { value: RValue::StringConcat { parts }, .. } if parts.len() == 2)
        }));
    }

    #[test]
    fn string_add_assign_lowers_to_concat_rvalue() {
        let source = r#"fn f() { var s = "count: "; s += 1; }"#;
        let air = lower_root(source, "f").expect("lower failed");

        assert!(program_statements(&air).any(|statement| matches!(
            statement,
            Statement::Assign {
                value: RValue::Use(Operand::Place(_)),
                ..
            }
        )));
        assert!(program_statements(&air).any(|statement| {
            matches!(statement, Statement::Init { value: RValue::StringConcat { parts }, .. } if parts.len() == 2)
        }));
    }

    #[test]
    fn string_interpolation_lowers_default_and_explicit_parts() {
        let source = r#"struct S {} fn f(s: S, x: int) -> string { f"value {s} {x:04}" }"#;
        let air = lower_full_core_root(source, "f").expect("lower failed");

        assert!(program_statements(&air).any(|statement| matches!(
            statement,
            Statement::Init {
                value: RValue::Stringify { .. },
                ..
            }
        )));
        assert!(program_statements(&air).any(|statement| matches!(
            statement,
            Statement::Init {
                value: RValue::Format { .. },
                ..
            }
        )));
        assert!(program_statements(&air).any(|statement| {
            matches!(
                statement,
                Statement::Init {
                    value: RValue::StringConcat { .. },
                    ..
                }
            )
        }));
    }

    #[test]
    fn string_concat_lowers_to_concat_rvalue() {
        let source = r#"fn f() -> string { "a" + "b" }"#;
        let air = lower_root(source, "f").expect("lower failed");

        assert!(program_statements(&air).any(|statement| {
            matches!(statement, Statement::Init { value: RValue::StringConcat { parts }, .. } if parts.len() == 2)
        }));
    }

    #[test]
    fn unused_unsupported_top_level_declarations_are_ignored() {
        lower_empty("struct S { x: int }").expect("lower failed");
    }

    #[test]
    fn unused_extern_type_declarations_are_ignored() {
        lower_root("extern type Handle; fn f() {}", "f").expect("lower failed");
    }

    #[test]
    fn unused_function_body_is_not_traversed() {
        let air = lower_root(
            "extern fn host(); fn unused() { host(); } fn main() {}",
            "main",
        )
        .expect("lower failed");

        assert_eq!(air.functions.len(), 1);
        assert_eq!(air.functions[0].name, Ident::new("main"));
        assert!(air.externs.is_empty());
    }

    #[test]
    fn unused_declarations_do_not_block_root_lowering() {
        let source = "
struct S { x: int }
dataref D { x: int }
enum E { A, B }
type Alias = int;
contract Drawable { fn draw(self); }
extend int { fn unused(self) -> int { self + 1 } }
extern fn host();
const ANSWER: int = 42;
lazy let Value: int = 1;
lazy var Count: int = 1;
fn unused() { host(); }
fn main() {}
";
        let air = lower_root(source, "main").expect("lower failed");

        assert_eq!(air.functions.len(), 1);
        assert_eq!(air.functions[0].name, Ident::new("main"));
        assert!(air.externs.is_empty());
        assert!(air.aggregates.is_empty());
        assert!(air.enums.is_empty());
        assert!(air.extern_types.is_empty());
    }

    #[test]
    fn rejects_function_value_read_as_unsupported() {
        let source = "fn g() -> int { 1 } fn f() -> void { g; }";
        let (root, resolved, semantic) = checked(source);
        let err = lower_checked_roots(&root, &resolved, &semantic.program, &["f"])
            .expect_err("expected error");
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

    fn root_function(name: &str) -> CallableInstanceKey {
        CallableInstanceKey {
            target: CallableId::function(ModuleScope::Root, Ident::new(name)),
            args: GenericArgs::default(),
        }
    }

    fn lower_empty(source: &str) -> Result<Program, LowerError> {
        let (root, resolved, semantic) = checked(source);
        lower_with_modules(
            &root,
            &resolved,
            &semantic.program,
            AirLowerConfig::default(),
        )
    }

    fn lower_full_core(source: &str) -> Result<Program, LowerError> {
        let (root, resolved, semantic) = checked_with_full_core_shape(source);
        lower_with_modules(
            &root,
            &resolved,
            &semantic.program,
            AirLowerConfig::default(),
        )
    }

    fn lower_full_core_root(source: &str, name: &str) -> Result<Program, LowerError> {
        let (root, resolved, semantic) = checked_with_full_core_shape(source);
        lower_checked_roots(&root, &resolved, &semantic.program, &[name])
    }

    fn lower_full_core_entry(
        source: &str,
        entry: &str,
        callables: &[&str],
    ) -> Result<Program, LowerError> {
        let (root, resolved, semantic) = checked_with_full_core_shape(source);
        lower_checked_entry(&root, &resolved, &semantic.program, entry, callables)
    }

    fn lower_checked_entry(
        root: &ast::Program,
        resolved: &ResolveResult,
        semantic: &SemanticProgram,
        entry: &str,
        callables: &[&str],
    ) -> Result<Program, LowerError> {
        lower_with_modules(
            root,
            resolved,
            semantic,
            AirLowerConfig {
                roots: AirRoots {
                    entry: Some(root_function(entry)),
                    callables: callables.iter().map(|name| root_function(name)).collect(),
                },
            },
        )
    }

    fn lower_root(source: &str, name: &str) -> Result<Program, LowerError> {
        lower_roots(source, &[name])
    }

    fn lower_roots(source: &str, names: &[&str]) -> Result<Program, LowerError> {
        let (root, resolved, semantic) = checked(source);
        lower_checked_roots(&root, &resolved, &semantic.program, names)
    }

    fn lower_checked_roots(
        root: &ast::Program,
        resolved: &ResolveResult,
        semantic: &SemanticProgram,
        names: &[&str],
    ) -> Result<Program, LowerError> {
        let callables = names.iter().map(|name| root_function(name)).collect();
        lower_with_modules(
            root,
            resolved,
            semantic,
            AirLowerConfig {
                roots: AirRoots {
                    entry: None,
                    callables,
                },
            },
        )
    }

    fn lower_entry(source: &str, name: &str, callables: &[&str]) -> Result<Program, LowerError> {
        let (root, resolved, semantic) = checked(source);
        lower_checked_entry(&root, &resolved, &semantic.program, name, callables)
    }

    fn function_names(program: &Program) -> Vec<&str> {
        program
            .functions
            .iter()
            .map(|function| function.name.as_str())
            .collect()
    }

    fn extern_names(program: &Program) -> Vec<&str> {
        program
            .externs
            .iter()
            .map(|decl| decl.name.as_str())
            .collect()
    }

    fn module_paths(program: &Program) -> Vec<Vec<&str>> {
        program
            .modules
            .iter()
            .map(|module| module.path.iter().map(Ident::as_str).collect())
            .collect()
    }

    fn core_fixture_module_paths(resolved: &ResolveResult) -> Vec<Vec<&str>> {
        resolved
            .module_groups
            .iter()
            .flatten()
            .map(|module| match module.key.path() {
                PackageModulePath::Root => vec![],
                PackageModulePath::Named(path) | PackageModulePath::Provider(path) => {
                    path.segments().iter().map(String::as_str).collect()
                }
                PackageModulePath::Source(_) => panic!("unexpected source module in core fixture"),
            })
            .collect()
    }

    fn program_statements(program: &Program) -> impl Iterator<Item = &Statement> {
        program
            .functions
            .iter()
            .flat_map(|function| function_statements(function))
    }

    fn function_statements(function: &Function) -> impl Iterator<Item = &Statement> {
        function
            .body
            .iter()
            .flat_map(|block| block.statements.iter())
    }

    fn stringify_source_types(program: &Program) -> Vec<TypeData> {
        program_statements(program)
            .filter_map(|statement| match statement {
                Statement::Init {
                    value: RValue::Stringify { source_ty, .. },
                    ..
                }
                | Statement::Assign {
                    value: RValue::Stringify { source_ty, .. },
                    ..
                }
                | Statement::Eval(RValue::Stringify { source_ty, .. }) => {
                    Some(program.type_arena.data(*source_ty).clone())
                }
                _ => None,
            })
            .collect()
    }

    fn assert_stringify_override(program: &Program, owner: &str) {
        let (override_id, _) = program
            .functions
            .iter()
            .enumerate()
            .find(|(_, function)| {
                function.name == Ident::new("to_string") && function.kind == FunctionKind::Method
            })
            .expect("override missing");
        assert!(program.aggregates.iter().any(|decl| {
            decl.name.as_str() == owner
                && decl.stringify_override == Some(FunctionId::from_index(override_id))
        }));
    }

    fn assert_extern_signature(program: &Program, name: &str, params: &[TypeData], ret: &TypeData) {
        let decl = program
            .externs
            .iter()
            .find(|decl| decl.name.as_str() == name)
            .expect("extern not found");
        let actual_params = decl
            .params
            .iter()
            .map(|ty| program.type_arena.data(*ty).clone())
            .collect::<Vec<_>>();
        assert_eq!(actual_params, params);
        assert_eq!(program.type_arena.data(decl.return_type), ret);
    }

    fn assert_no_deferred_core_decls(program: &Program) {
        assert!(program.aggregates.is_empty());
        assert!(program.enums.is_empty());
        assert!(program.extern_types.is_empty());
        assert!(program.modules.iter().all(|module| {
            !matches!(
                module.path.first().map(Ident::as_str),
                Some(
                    "option"
                        | "result"
                        | "range"
                        | "collections"
                        | "core_int"
                        | "core_float"
                        | "core_string"
                )
            )
        }));
    }

    fn with_source_functions<R>(
        source: &str,
        names: &[&str],
        f: impl FnOnce(&SourceModules<'_>, &ReachableCallables<'_>, &SemanticProgram) -> R,
    ) -> R {
        let (root, resolved, semantic) = checked(source);
        let index = SourceProgramIndex::new(&root, &resolved);
        let facts = SemanticCallableFacts::new(&semantic.program);
        let roots = names.iter().map(|name| root_function(name)).collect();
        let functions = ReachableCallables::new(&index, &semantic.program, &facts, roots)
            .expect("source functions failed");
        f(&index.modules, &functions, &semantic.program)
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
        checked_with_providers(source, vec![provider])
    }

    fn checked_with_providers(
        source: &str,
        providers: Vec<ProviderDescriptor>,
    ) -> (ast::Program, ResolveResult, typecheck::SemanticCheckOutput) {
        let root = parse_program(source);
        let provider_raw = externs::ingest_providers(ExternInputs {
            packages: vec![PackageExternInputs {
                package: crate::resolve::PackageId::synthetic_root(),
                providers,
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
