use std::collections::{HashMap, HashSet};

use anvyx_externs::ParamFlow;

use super::{
    AggregateCtor, AggregateDecl, AggregateKind, AirBlock, AirBody, AirCollectionFor,
    AirCollectionLoan, AirCollectionLoanMode, AirCollectionRootKind, AirCollectionSlot,
    AirCollectionSlotKind, AirCollectionSlotScope, AirDynMatch, AirDynMatchArm,
    AirDynMatchFallback, AirDynMatchFallbackBinding, AirDynMatchSource, AirDynMatchTargetBinding,
    AirIf, AirLoop, AirLoopId, AirMapEntryMatch, AirOptionalMatch, AirOrdinalAdapter,
    AirOrdinalPlan, AirPatternAlternative, AirPatternArm, AirPatternBinding, AirPatternBindingMode,
    AirPatternMatch, AirPatternPath, AirPatternPathStep, AirPatternTest, AirRangeFor, AirStmt,
    AirTail, BindingId as AirBindingId, CallArg, Callee, CaptureCellDecl, CaptureCellId,
    CaptureCellLifetime, CaptureLocalSource, ConstData, ConstId, ConstValue, ContractParamDecl,
    ContractReceiver, ContractReturnDecl, ContractSlotDecl, ContractSlotId, ContractSurfaceDecl,
    ContractSurfaceId, ContractWeakeningDecl, ContractWeakeningId, ContractWitnessDecl,
    ContractWitnessId, ContractWitnessKey as AirWitnessKey, ContractWitnessSlotDecl,
    ContractWitnessTarget, CoreEnumKind, DynBorrow, DynBorrowParamDecl, DynBorrowSource,
    DynOwnedUse, DynReceiver, EnumDecl, EnumRepr, ExternAbi, ExternBindingDecl, ExternDecl,
    ExternFieldDecl, ExternId, ExternInitArgDecl, ExternMember, ExternMethodDecl, ExternOp,
    ExternOpDecl, ExternParamDecl, ExternReceiverDecl, ExternRep, ExternStaticDecl,
    ExternTypeBindingDecl, ExternTypeDecl, ExternVariantAbiDecl, FieldDecl, FieldId, FlagDecl,
    FlagMemberDecl, FlagMemberId, FlagStaticOp, Function, FunctionId, FunctionKind, FunctionOwner,
    FunctionSpecialization, FunctionValueCapability, GlobalDecl, GlobalId, GlobalInitEffect,
    IterCountCheck, LambdaCaptureArg, LambdaCaptureDecl, LambdaCaptureSlotId, LambdaDecl,
    LambdaEscape, LambdaId, Local, LocalId, LocalKind, MapWriteKind, Module, ModuleId,
    Mutability as AirMutability, Operand, Param, ParamEscape, ParamMode, ParamRole, ParamType,
    Place, PlaceRoot, Program, RValue, RawEnumValue, ReturnMode, ScopedBorrowDecl, ScopedBorrowId,
    ScopedBorrowSource, Signature, SignatureType, TypeData, TypeId, VariantDecl, VariantShape,
    VerifyError, ownership, place_model,
    typing::{self, PrimitiveTypes},
    verify,
};
use crate::{
    ast::{
        self, ArrayLen, AssignOp, BinaryOp, BlockNode, ExprId, ExprKind, ExprNode, Ident, Lit,
        Mutability as AstMutability, Pattern, ReturnAccess, Stmt, StmtNode, Type,
    },
    collection_effect,
    externs::catalog::{ExternCatalog, ExternLoweringInfo},
    resolve::PackageModulePath,
    semantic_id::NominalId,
    source::SourceId,
    span::SourceSpan,
    typecheck::{
        BindingId, BodyInstanceKey, CallForm, CallTarget, CallableId, CallableInstanceKey,
        CallableKind, CallableParent, CaptureStorageOrigin, CastFromInstanceKey, CastFromSignature,
        CheckedConditionalPattern, CheckedDynMatchBinding, CheckedDynMatchPlan, CheckedEnumPayload,
        CheckedLiteralPattern, CheckedMatchAccess, CheckedMatchArm, CheckedMatchPlan,
        CheckedPattern, CheckedPatternBinding, CheckedPatternBindingKind, CheckedPatternOwner,
        ConstTerm, ContractParamSchema, ContractReturnSchema,
        ContractSurfaceId as SemanticContractSurfaceId, ContractSurfaceSchemas, ContractTypeSchema,
        CoreRangeKind, DeclarationIndex, DefaultArgFact, DefaultExprSite, DynCallFact,
        DynDowncastSource, EnumRepr as TcEnumRepr, ExternUseTarget, FunctionValueEscapeCapability,
        FunctionValueKind, FunctionValueOrigin, GenericArgs, GlobalAccessFact, GlobalAccessMode,
        GlobalInitEffect as TcGlobalInitEffect, GlobalKey, GlobalSig, IterRuntimeCheckKind,
        LambdaBodyKey, LambdaCaptureFact, LambdaCaptureRuntimePlan, LambdaEscapeFact,
        LambdaEscapeKind, LocalDefFact, LocalDefKind, LocalUseFact, LocalUseMode, MemberPathKind,
        MethodMode, MethodSurface, ModuleScope, NominalKey, RawEnumValue as TcRawEnumValue,
        SemanticBodyFacts, SemanticFunctionInstanceFact, SemanticLocalId, SemanticProgram,
        TypecheckFacts, UserCastSite, VariantPayload, WitnessId as SemanticWitnessId,
        WitnessSlotTarget, generic_args_are_concrete, nominal_generic_args, nominal_id_for_type,
        nominal_type_with_args, substitute_aggregate_member, type_has_unfinished_facts,
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
        kind: UnsupportedExternUseKind,
    },
    UnsupportedExternSignature,
    UnsupportedDefaultArg {
        call: ExprId,
        param_index: usize,
        expr_id: ExprId,
    },
    MissingDefaultExprFacts {
        site: DefaultExprSite,
    },
    NonConcreteRoot {
        id: Box<CallableId>,
        args: Box<GenericArgs>,
    },
    NonConcreteCallableInstance {
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
    MissingGlobalSig {
        key: Box<GlobalKey>,
    },
    MissingSourceGlobal {
        key: Box<GlobalKey>,
    },
    MissingLoweredGlobal {
        key: Box<GlobalKey>,
    },
    MissingSpecializedBodyFacts {
        body: Box<BodyInstanceKey>,
    },
    MissingTypecheckFacts,
    MissingLambdaEscape {
        expr_id: ExprId,
    },
    DuplicateBindingBridge {
        body: Box<BodyInstanceKey>,
        binding: BindingId,
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
    Ownership(Box<[ownership::OwnershipError]>),
    Verify(Box<[VerifyError]>),
    AnyTypeEmitted(TypeId),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum UnsupportedExternUseKind {
    FieldRead,
    FieldWrite,
    Method,
    Static,
    Init,
    UnaryOperator,
    BinaryOperator,
    MissingFunction,
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

#[derive(Clone, Copy)]
enum FilterCollection {
    List { elem: TypeId },
    Map { key: TypeId, value: TypeId },
}

#[derive(Debug)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum TypeCacheKey {
    Source(Type),
    Nominal {
        id: NominalId,
        type_args: Vec<TypeId>,
        const_args: Vec<ast::ConstArg>,
    },
}

#[derive(Debug, Default)]
struct TypeLowerer {
    cache: HashMap<TypeCacheKey, TypeId>,
}

struct TypeLowerEnv<'a, 'b> {
    modules: &'a mut HashMap<ModuleScope, ModuleId>,
    decls: Option<&'b DeclarationIndex>,
    externs: Option<&'b ExternCatalog>,
    contract_surfaces: Option<&'b ContractSurfaceSchemas>,
    contract_surface_ids: Option<&'b HashMap<SemanticContractSurfaceId, ContractSurfaceId>>,
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
                contract_surfaces: None,
                contract_surface_ids: None,
            },
        )
    }

    fn lower_source(
        &mut self,
        program: &mut Program,
        modules: &mut HashMap<ModuleScope, ModuleId>,
        decls: &DeclarationIndex,
        externs: &ExternCatalog,
        contract_surfaces: &ContractSurfaceSchemas,
        contract_surface_ids: &HashMap<SemanticContractSurfaceId, ContractSurfaceId>,
        ty: &Type,
    ) -> Result<TypeId, LowerError> {
        self.lower_with_env(
            program,
            ty,
            TypeLowerEnv {
                modules,
                decls: Some(decls),
                externs: Some(externs),
                contract_surfaces: Some(contract_surfaces),
                contract_surface_ids: Some(contract_surface_ids),
            },
        )
    }

    fn lower_with_env(
        &mut self,
        program: &mut Program,
        ty: &Type,
        mut env: TypeLowerEnv<'_, '_>,
    ) -> Result<TypeId, LowerError> {
        if let Some(id) = self.cache.get(&TypeCacheKey::Source(ty.clone())).copied() {
            return Ok(id);
        }

        let data = match ty {
            Type::Int => TypeData::Int,
            Type::Float => TypeData::Float,
            Type::Bool => TypeData::Bool,
            Type::String => TypeData::String,
            Type::Char => TypeData::Char,
            Type::Void => TypeData::Void,
            Type::Optional { inner } => {
                let inner = self.lower_with_env(program, inner, env)?;
                let id = optional_ty(program, inner);
                self.cache.insert(TypeCacheKey::Source(ty.clone()), id);
                return Ok(id);
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
                    len: *len.value(),
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
                    .map(|param| {
                        let ty = self.lower_with_env(program, &param.ty, env.reborrow())?;
                        Ok(ParamType {
                            ty,
                            mode: source_param_mode(param.mutable),
                            escape: param.escape.into(),
                        })
                    })
                    .collect::<Result<Vec<_>, LowerError>>()?,
                match ret.access() {
                    ReturnAccess::Value => {
                        ReturnMode::Value(self.lower_with_env(program, &ret.ty(), env)?)
                    }
                    ReturnAccess::Place => {
                        ReturnMode::Place(self.lower_with_env(program, &ret.ty(), env)?)
                    }
                },
            )),
            Type::Dyn(contract) => {
                let surface = env
                    .contract_surfaces
                    .zip(env.decls)
                    .and_then(|(surfaces, decls)| {
                        surfaces.id_for_ref(decls, &ModuleScope::Root, contract)
                    })
                    .and_then(|id| env.contract_surface_ids?.get(&id).copied())
                    .ok_or_else(|| LowerError::UnsupportedType {
                        ty: Box::new(ty.clone()),
                    })?;
                TypeData::Dyn(surface)
            }
            Type::Nominal(_) => return self.lower_nominal(program, ty, env),
            _ => {
                return Err(LowerError::UnsupportedType {
                    ty: Box::new(ty.clone()),
                });
            }
        };

        let id = intern_structural_type(program, data);
        self.cache.insert(TypeCacheKey::Source(ty.clone()), id);
        Ok(id)
    }

    fn lower_nominal(
        &mut self,
        program: &mut Program,
        ty: &Type,
        mut env: TypeLowerEnv<'_, '_>,
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
            return self.lower_extern_nominal(program, ty, &mut env, externs, &key);
        }
        if decls.aggregate(&key).is_some() {
            return self.lower_aggregate_nominal(program, ty, &mut env, decls, &key);
        }
        if enum_core_kind(decls, &key) == Some(CoreEnumKind::Option) {
            let args = self.nominal_type_args(program, ty, env.reborrow())?;
            let [inner] = args.as_slice() else {
                return Err(LowerError::UnsupportedType {
                    ty: Box::new(ty.clone()),
                });
            };
            let id = optional_ty(program, *inner);
            self.cache.insert(TypeCacheKey::Source(ty.clone()), id);
            return Ok(id);
        }
        if decls.enum_schema(&key).is_some() {
            return self.lower_enum_nominal(program, ty, &mut env, decls, &key);
        }
        Err(LowerError::UnsupportedType {
            ty: Box::new(ty.clone()),
        })
    }

    fn lower_aggregate_nominal(
        &mut self,
        program: &mut Program,
        ty: &Type,
        env: &mut TypeLowerEnv<'_, '_>,
        decls: &DeclarationIndex,
        key: &NominalKey,
    ) -> Result<TypeId, LowerError> {
        let schema = decls
            .aggregate_for_type(ty)
            .expect("aggregate schema exists");
        let module = ensure_module(program, env.modules, &key.module);
        let kind = if key.kind == ast::NominalKind::DataRef {
            AggregateKind::DataRef
        } else {
            AggregateKind::Struct
        };
        let type_args = self.nominal_type_args(program, ty, env.reborrow())?;
        let (const_key, const_args) = nominal_const_args(ty);
        let cache_key = TypeCacheKey::Nominal {
            id: key.id.clone(),
            type_args: type_args.clone(),
            const_args: const_key,
        };
        if let Some(id) = self.cache.get(&cache_key).copied() {
            return Ok(id);
        }
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
        self.cache.insert(cache_key, id);
        let generics = schema.all_generics();
        let fields = schema
            .fields
            .iter()
            .map(|(name, field)| {
                let field_ty = substitute_aggregate_member(ty, &generics, &field.ty);
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
        env: &mut TypeLowerEnv<'_, '_>,
        decls: &DeclarationIndex,
        key: &NominalKey,
    ) -> Result<TypeId, LowerError> {
        let schema = decls.enum_schema_for_type(ty).expect("enum schema exists");
        let module = ensure_module(program, env.modules, &key.module);
        let type_args = self.nominal_type_args(program, ty, env.reborrow())?;
        let (const_key, const_args) = nominal_const_args(ty);
        let cache_key = TypeCacheKey::Nominal {
            id: key.id.clone(),
            type_args: type_args.clone(),
            const_args: const_key,
        };
        if let Some(id) = self.cache.get(&cache_key).copied() {
            return Ok(id);
        }
        if let Some(flag) = schema.body.kind.flag() {
            let members = flag
                .members
                .iter()
                .enumerate()
                .map(|(index, (name, member))| FlagMemberDecl {
                    id: FlagMemberId::from_index(index),
                    name,
                    value: member.value,
                    atomic: member.atomic,
                })
                .collect();
            let flag_id = program.alloc_flag(FlagDecl {
                name: key.name,
                module,
                known_bits: flag.known_bits,
                members,
            });
            program.module_mut(module).flags.push(flag_id);
            let id = program.alloc_type(TypeData::Flag(flag_id));
            self.cache.insert(cache_key, id);
            return Ok(id);
        }
        let raw = schema.body.kind.raw();
        let raw_type = raw
            .map(|raw| self.lower_with_env(program, &raw.backing.ty(), env.reborrow()))
            .transpose()?;
        let enum_id = program.alloc_enum(EnumDecl {
            name: key.name,
            module,
            type_args,
            const_args,
            core: enum_core_kind(decls, key),
            repr: lower_enum_repr(schema.body.kind.repr()),
            raw_type,
            variants: vec![],
        });
        program.module_mut(module).enums.push(enum_id);
        let id = program.alloc_type(TypeData::Enum(enum_id));
        self.cache.insert(cache_key, id);
        let generics = schema.all_generics();
        let variants = schema
            .body
            .variants
            .iter()
            .map(|(name, variant)| {
                let shape = match &variant.payload {
                    VariantPayload::Unit => VariantShape::Unit,
                    VariantPayload::Tuple(items) => VariantShape::Tuple(
                        items
                            .iter()
                            .map(|item| {
                                let item = substitute_aggregate_member(ty, &generics, item);
                                self.lower_with_env(program, &item, env.reborrow())
                            })
                            .collect::<Result<Vec<_>, _>>()?,
                    ),
                    VariantPayload::Struct(fields) => VariantShape::Struct(
                        fields
                            .iter()
                            .map(|(field_name, field)| {
                                let field_ty =
                                    substitute_aggregate_member(ty, &generics, &field.ty);
                                Ok(FieldDecl {
                                    name: field_name,
                                    ty: self.lower_with_env(program, &field_ty, env.reborrow())?,
                                })
                            })
                            .collect::<Result<Vec<_>, LowerError>>()?,
                    ),
                };
                Ok(VariantDecl {
                    name,
                    shape,
                    raw_value: raw
                        .and_then(|raw| raw.value(name))
                        .map(lower_raw_enum_value),
                })
            })
            .collect::<Result<Vec<_>, LowerError>>()?;
        program.enum_decl_mut(enum_id).variants = variants;
        Ok(id)
    }

    fn lower_extern_nominal(
        &mut self,
        program: &mut Program,
        ty: &Type,
        env: &mut TypeLowerEnv<'_, '_>,
        externs: &ExternCatalog,
        key: &NominalKey,
    ) -> Result<TypeId, LowerError> {
        let Some(source_id) = externs.type_by_nominal(key) else {
            return Err(LowerError::UnsupportedType {
                ty: Box::new(ty.clone()),
            });
        };
        let source = externs.ty(source_id);
        let module = ensure_module(program, env.modules, &key.module);
        let type_args = self.nominal_type_args(program, ty, env.reborrow())?;
        let (const_key, const_args) = nominal_const_args(ty);
        let cache_key = TypeCacheKey::Nominal {
            id: key.id.clone(),
            type_args: type_args.clone(),
            const_args: const_key,
        };
        if let Some(id) = self.cache.get(&cache_key).copied() {
            return Ok(id);
        }
        let binding = externs
            .type_lowering_info(source_id)
            .map(|info| ExternTypeBindingDecl {
                package: info.package,
                provider: info.provider,
                key: info.key,
            });
        let extern_id = program.alloc_extern_type(ExternTypeDecl {
            name: key.name,
            module,
            binding,
            type_args,
            const_args,
            rep: match source.rep {
                anvyx_externs::ExternRep::Shared => ExternRep::Shared,
                anvyx_externs::ExternRep::Inline => ExternRep::Inline,
            },
            layout: source.layout,
            materialization: source.materialization,
            owns_heap_edges: source.owns_heap_edges,
            has_init: source.constructor_fields().is_some(),
            init_args: vec![],
            fields: vec![],
            variants: vec![],
            variant_abis: vec![],
            methods: vec![],
            statics: vec![],
            operators: vec![],
        });
        program.module_mut(module).extern_types.push(extern_id);
        let id = program.alloc_type(TypeData::Extern(extern_id));
        self.cache.insert(cache_key, id);

        let fields = source
            .fields
            .iter()
            .map(|field| {
                Ok(ExternFieldDecl {
                    name: field.name,
                    ty: self.lower_with_env(program, &field.ty.ty, env.reborrow())?,
                    abi: field.ty.abi.clone(),
                    get_receiver: ExternReceiverDecl {
                        ty: id,
                        mode: receiver_mode(field.get_receiver),
                    },
                    set_receiver: ExternReceiverDecl {
                        ty: id,
                        mode: receiver_mode(field.set_receiver),
                    },
                    computed: field.computed,
                    readable: field.readable,
                    writable: field.writable,
                })
            })
            .collect::<Result<Vec<_>, LowerError>>()?;
        let variant_data = source
            .variants
            .iter()
            .map(|variant| {
                let fields = variant
                    .fields
                    .iter()
                    .map(|field| {
                        Ok((
                            field.name,
                            self.lower_with_env(program, &field.ty.ty, env.reborrow())?,
                            field.ty.abi.clone(),
                        ))
                    })
                    .collect::<Result<Vec<_>, LowerError>>()?;
                let shape = match fields.as_slice() {
                    [] => VariantShape::Unit,
                    fields if fields.iter().all(|(name, _, _)| name.is_none()) => {
                        VariantShape::Tuple(fields.iter().map(|(_, ty, _)| *ty).collect())
                    }
                    fields => VariantShape::Struct(
                        fields
                            .iter()
                            .map(|(name, ty, _)| FieldDecl {
                                name: name.expect("validated named extern variant field"),
                                ty: *ty,
                            })
                            .collect(),
                    ),
                };
                Ok((
                    VariantDecl {
                        name: variant.name,
                        shape,
                        raw_value: None,
                    },
                    ExternVariantAbiDecl {
                        fields: fields.into_iter().map(|(_, _, abi)| abi).collect(),
                    },
                ))
            })
            .collect::<Result<Vec<_>, LowerError>>()?;
        let (variants, variant_abis) = variant_data.into_iter().unzip();
        let methods = source
            .methods
            .iter()
            .map(|method| {
                let params = method
                    .signature
                    .params
                    .iter()
                    .map(|param| {
                        let ty = self.lower_with_env(program, &param.ty.ty, env.reborrow())?;
                        checked_extern_param(
                            program,
                            ty,
                            param_flow_mode(param.flow),
                            param.escape.into(),
                        )
                    })
                    .collect::<Result<Vec<_>, LowerError>>()?;
                Ok(ExternMethodDecl {
                    name: method.name,
                    receiver: ExternReceiverDecl {
                        ty: id,
                        mode: match method.receiver {
                            anvyx_externs::ReceiverMode::Value => ParamMode::Value,
                            anvyx_externs::ReceiverMode::Shared => ParamMode::SharedBorrow,
                            anvyx_externs::ReceiverMode::Mutable => ParamMode::MutBorrow,
                        },
                    },
                    params,
                    return_type: self.lower_with_env(
                        program,
                        &method.signature.ret.ty,
                        env.reborrow(),
                    )?,
                    abi: extern_receiver_signature_abi(
                        &source.key,
                        extern_signature_abi(&method.signature),
                    ),
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
                    .map(|param| {
                        let ty = self.lower_with_env(program, &param.ty.ty, env.reborrow())?;
                        checked_extern_param(
                            program,
                            ty,
                            param_flow_mode(param.flow),
                            param.escape.into(),
                        )
                    })
                    .collect::<Result<Vec<_>, LowerError>>()?;
                Ok(ExternStaticDecl {
                    name: static_method.name,
                    params,
                    return_type: self.lower_with_env(
                        program,
                        &static_method.signature.ret.ty,
                        env.reborrow(),
                    )?,
                    abi: extern_signature_abi(&static_method.signature),
                })
            })
            .collect::<Result<Vec<_>, LowerError>>()?;
        let operators = source
            .operators
            .iter()
            .map(|operator| {
                Ok(ExternOpDecl {
                    kind: lower_extern_op(operator.op),
                    receiver: ExternReceiverDecl {
                        ty: id,
                        mode: ParamMode::SharedBorrow,
                    },
                    operand: operator
                        .signature
                        .params
                        .first()
                        .map(|param| {
                            let ty = self.lower_with_env(program, &param.ty.ty, env.reborrow())?;
                            checked_extern_param(
                                program,
                                ty,
                                param_flow_mode(param.flow),
                                param.escape.into(),
                            )
                        })
                        .transpose()?,
                    return_type: self.lower_with_env(
                        program,
                        &operator.signature.ret.ty,
                        env.reborrow(),
                    )?,
                    abi: extern_receiver_signature_abi(
                        &source.key,
                        extern_signature_abi(&operator.signature),
                    ),
                })
            })
            .collect::<Result<Vec<_>, LowerError>>()?;
        let init_args = source
            .required_init_fields()
            .into_iter()
            .flatten()
            .map(|(init, _)| ExternInitArgDecl {
                field: FieldId::from_index(init.field.index()),
                param: init.param,
                presence: false,
            })
            .chain(
                source
                    .presence_init_fields()
                    .into_iter()
                    .flatten()
                    .map(|(init, _)| ExternInitArgDecl {
                        field: FieldId::from_index(init.field.index()),
                        param: init.param,
                        presence: true,
                    }),
            )
            .collect();
        let decl = program.extern_type_mut(extern_id);
        decl.fields = fields;
        decl.variants = variants;
        decl.variant_abis = variant_abis;
        decl.init_args = init_args;
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

fn extern_signature_abi(signature: &crate::externs::catalog::ResolvedExternSignature) -> ExternAbi {
    ExternAbi {
        params: signature
            .params
            .iter()
            .map(|param| param.ty.abi.clone())
            .collect(),
        ret: signature.ret.abi.clone(),
    }
}

fn extern_receiver_abi(
    owner: &crate::externs::catalog::TypeKey,
    ret: anvyx_externs::ExternTypeExpr,
) -> ExternAbi {
    extern_receiver_signature_abi(
        owner,
        ExternAbi {
            params: vec![],
            ret,
        },
    )
}

fn extern_receiver_signature_abi(
    owner: &crate::externs::catalog::TypeKey,
    mut abi: ExternAbi,
) -> ExternAbi {
    abi.params.insert(
        0,
        anvyx_externs::ExternTypeExpr::Named {
            module: extern_receiver_module(&owner.module),
            name: owner.name.as_str().to_string(),
            args: vec![],
        },
    );
    abi
}

fn extern_receiver_module(module: &ModuleScope) -> Option<anvyx_externs::ModulePath> {
    match module {
        ModuleScope::Named(path) => Some(path.to_extern_path()),
        ModuleScope::Package(module) => match module.path() {
            PackageModulePath::Named(path) | PackageModulePath::Provider(path) => {
                Some(path.to_extern_path())
            }
            PackageModulePath::Root | PackageModulePath::Source(_) => None,
        },
        ModuleScope::Root => None,
    }
}

fn witness_receiver_mode(target: &WitnessSlotTarget) -> ParamMode {
    match target {
        WitnessSlotTarget::Direct { receiver_mode, .. }
        | WitnessSlotTarget::Extend { receiver_mode, .. } => match receiver_mode {
            MethodMode::Static => {
                unreachable!("contract witness target must be an instance method")
            }
            MethodMode::Instance { mutable: true } => ParamMode::MutBorrow,
            MethodMode::Instance { mutable: false } => ParamMode::SharedBorrow,
        },
        WitnessSlotTarget::Extern { receiver, .. } => receiver_mode(*receiver),
        WitnessSlotTarget::Promoted { target, .. } => witness_receiver_mode(target),
    }
}

fn receiver_mode(mode: anvyx_externs::ReceiverMode) -> ParamMode {
    match mode {
        anvyx_externs::ReceiverMode::Value => ParamMode::Value,
        anvyx_externs::ReceiverMode::Shared => ParamMode::SharedBorrow,
        anvyx_externs::ReceiverMode::Mutable => ParamMode::MutBorrow,
    }
}

fn lower_unary_op(op: anvyx_externs::UnaryOp) -> ast::UnaryOp {
    match op {
        anvyx_externs::UnaryOp::Neg => ast::UnaryOp::Neg,
    }
}

fn lower_binary_op(op: anvyx_externs::BinaryOp) -> BinaryOp {
    match op {
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
    }
}

fn operator_name(op: anvyx_externs::ExternOperator) -> Ident {
    let name = match op {
        anvyx_externs::ExternOperator::Unary(_) => "__unary_op",
        anvyx_externs::ExternOperator::Binary { .. } => "__binary_op",
    };
    Ident::new(name)
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

fn nominal_const_args(ty: &Type) -> (Vec<ast::ConstArg>, Vec<String>) {
    let args = ty
        .as_nominal()
        .map(|nominal| nominal.const_args.clone())
        .unwrap_or_default();
    let display = args.iter().map(ToString::to_string).collect();
    (args, display)
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
        ..Module::default()
    });
    modules.insert(scope.clone(), id);
    id
}

fn intern_structural_type(program: &mut Program, data: TypeData) -> TypeId {
    let existing = program
        .type_arena
        .iter()
        .position(|existing| existing == &data)
        .map(TypeId::from_index);
    existing.unwrap_or_else(|| program.alloc_type(data))
}

fn optional_ty(program: &mut Program, inner: TypeId) -> TypeId {
    intern_structural_type(program, TypeData::Optional(inner))
}

impl TypeLowerEnv<'_, '_> {
    fn reborrow(&mut self) -> TypeLowerEnv<'_, '_> {
        TypeLowerEnv {
            modules: self.modules,
            decls: self.decls,
            externs: self.externs,
            contract_surfaces: self.contract_surfaces,
            contract_surface_ids: self.contract_surface_ids,
        }
    }
}

#[derive(Debug, Default)]
struct LoweringMaps {
    modules: HashMap<ModuleScope, ModuleId>,
    bodies: HashMap<BodyInstanceKey, FunctionId>,
    locals: HashMap<BodyInstanceKey, HashMap<SemanticLocalId, LocalId>>,
    lambdas: HashMap<LambdaBodyKey, LambdaId>,
    globals: HashMap<GlobalKey, GlobalId>,
    externs: HashMap<ExternUseTarget, ExternId>,
    contract_surfaces: HashMap<SemanticContractSurfaceId, ContractSurfaceId>,
    contract_witnesses: HashMap<SemanticWitnessId, ContractWitnessId>,
    contract_weakenings: HashMap<(BodyInstanceKey, ExprId), ContractWeakeningId>,
}

#[derive(Default)]
struct LowerCx<'facts> {
    program: Program,
    types: TypeLowerer,
    maps: LoweringMaps,
    decls: Option<DeclarationIndex>,
    externs: Option<ExternCatalog>,
    contract_surfaces: Option<ContractSurfaceSchemas>,
    typecheck_facts: Option<&'facts TypecheckFacts>,
}

impl LowerCx<'_> {
    fn lower_ty(&mut self, ty: &Type) -> Result<TypeId, LowerError> {
        match (&self.decls, &self.externs, &self.contract_surfaces) {
            (Some(decls), Some(externs), Some(contract_surfaces)) => self.types.lower_source(
                &mut self.program,
                &mut self.maps.modules,
                decls,
                externs,
                contract_surfaces,
                &self.maps.contract_surfaces,
                ty,
            ),
            _ => self.types.lower(&mut self.program, ty),
        }
    }

    fn lower_contract_surfaces(&mut self) -> Result<(), LowerError> {
        let surfaces = self
            .contract_surfaces
            .clone()
            .expect("semantic contract surfaces are available");
        for surface in surfaces.iter() {
            let id = self.program.alloc_contract_surface(ContractSurfaceDecl {
                display_name: surface.display_name.clone(),
                slots: vec![],
            });
            let old = self.maps.contract_surfaces.insert(surface.id, id);
            debug_assert!(old.is_none());
        }
        for surface in surfaces.iter() {
            let id = self.maps.contract_surfaces[&surface.id];
            let slots = surface
                .slots
                .iter()
                .map(|slot| {
                    let params = slot
                        .params
                        .iter()
                        .map(|param| self.lower_contract_param(param))
                        .collect::<Result<Vec<_>, _>>()?;
                    let ret = match &slot.ret {
                        ContractReturnSchema::Value(ty) => {
                            ContractReturnDecl::Value(self.lower_contract_schema_type(ty)?)
                        }
                        ContractReturnSchema::Place(ty) => {
                            ContractReturnDecl::Place(self.lower_contract_schema_type(ty)?)
                        }
                        ContractReturnSchema::Iter => ContractReturnDecl::Iter,
                    };
                    Ok(ContractSlotDecl {
                        id: ContractSlotId::from_index(slot.id.0 as usize),
                        name: slot.name,
                        receiver: match slot.receiver {
                            ast::MethodReceiver::Value => ContractReceiver::Value,
                            ast::MethodReceiver::Ref => ContractReceiver::Ref,
                        },
                        params,
                        ret,
                    })
                })
                .collect::<Result<Vec<_>, LowerError>>()?;
            self.program.contract_surface_mut(id).slots = slots;
        }
        Ok(())
    }

    fn lower_contract_param(
        &mut self,
        param: &ContractParamSchema,
    ) -> Result<ContractParamDecl, LowerError> {
        Ok(ContractParamDecl {
            ty: self.lower_contract_schema_type(&param.ty)?,
            mode: source_param_mode(param.mutable),
            cast_accept: param.cast_accept,
            escape: param.escape.into(),
        })
    }

    fn lower_contract_schema_type(
        &mut self,
        schema: &ContractTypeSchema,
    ) -> Result<TypeId, LowerError> {
        let surfaces = self
            .contract_surfaces
            .as_ref()
            .expect("contract surfaces are available");
        let decls = self.decls.as_ref().expect("declarations are available");
        let ty = contract_schema_source_type(schema, surfaces, decls);
        self.lower_ty(&ty)
    }

    fn lower_contract_declarations(
        &mut self,
        functions: &ReachableItems<'_>,
        semantic: &SemanticProgram,
    ) -> Result<(), LowerError> {
        let fact_slices = functions.contract_fact_slices()?;
        let mut witnesses = fact_slices
            .iter()
            .flat_map(|slice| {
                slice
                    .facts
                    .dyn_conversions
                    .values()
                    .filter(|fact| slice.includes(fact.expr_id))
            })
            .map(|conversion| conversion.witness)
            .collect::<Vec<_>>();
        witnesses.sort_by_key(|id| id.0);
        witnesses.dedup();
        for witness in witnesses {
            self.lower_contract_witness(witness, semantic)?;
        }

        let mut weakening_sites = vec![];
        for slice in &fact_slices {
            let mut facts = slice
                .facts
                .dyn_weakenings
                .values()
                .filter(|fact| slice.includes(fact.expr_id))
                .cloned()
                .collect::<Vec<_>>();
            facts.sort_by_key(|fact| fact.expr_id.0);
            weakening_sites.extend(facts.into_iter().map(|fact| (slice.body.clone(), fact)));
        }
        for (body, fact) in weakening_sites {
            let decl = ContractWeakeningDecl {
                source: self.maps.contract_surfaces[&fact.source],
                target: self.maps.contract_surfaces[&fact.target],
                target_to_source: fact
                    .target_to_source
                    .iter()
                    .map(|slot| ContractSlotId::from_index(slot.0 as usize))
                    .collect(),
            };
            let id = match self
                .program
                .contract_weakenings
                .iter()
                .position(|existing| existing == &decl)
            {
                Some(index) => ContractWeakeningId::from_index(index),
                None => self.program.alloc_contract_weakening(decl),
            };
            self.maps
                .contract_weakenings
                .insert((body, fact.expr_id), id);
        }
        self.close_projected_contract_witnesses();
        Ok(())
    }

    fn lower_contract_witness(
        &mut self,
        witness: SemanticWitnessId,
        semantic: &SemanticProgram,
    ) -> Result<ContractWitnessId, LowerError> {
        if let Some(id) = self.maps.contract_witnesses.get(&witness) {
            return Ok(*id);
        }
        let key = semantic
            .facts
            .witness_structural_keys
            .get(&witness)
            .expect("reachable witness has a finalized structural key")
            .clone();
        let concrete_ty = self.lower_ty(&key.concrete_ty)?;
        let surface = self.maps.contract_surfaces[&key.surface];
        let slots = key
            .slots
            .iter()
            .enumerate()
            .map(|(index, target)| {
                let iter = matches!(
                    self.program.contract_surface(surface).slots[index].ret,
                    ContractReturnDecl::Iter
                );
                Ok(ContractWitnessSlotDecl {
                    slot: ContractSlotId::from_index(index),
                    receiver: witness_receiver_mode(target),
                    target: self.lower_contract_witness_target(concrete_ty, target, iter)?,
                })
            })
            .collect::<Result<Vec<_>, LowerError>>()?;
        let structural_key = AirWitnessKey {
            concrete_ty,
            surface,
            slots: slots.clone(),
        };
        let id = match self
            .program
            .contract_witnesses
            .iter()
            .position(|existing| existing.key == structural_key)
        {
            Some(index) => ContractWitnessId::from_index(index),
            None => self.program.alloc_contract_witness(ContractWitnessDecl {
                key: structural_key,
            }),
        };
        self.maps.contract_witnesses.insert(witness, id);
        Ok(id)
    }

    fn lower_contract_witness_target(
        &mut self,
        receiver_ty: TypeId,
        target: &WitnessSlotTarget,
        iter: bool,
    ) -> Result<ContractWitnessTarget, LowerError> {
        match target {
            WitnessSlotTarget::Direct {
                callable,
                owner_args,
                ..
            }
            | WitnessSlotTarget::Extend {
                callable,
                owner_args,
                ..
            } => {
                let body = BodyInstanceKey::Callable(CallableInstanceKey {
                    target: callable.clone(),
                    args: owner_args.clone(),
                });
                let function = self.maps.bodies.get(&body).copied().ok_or_else(|| {
                    LowerError::MissingSpecializedBodyFacts {
                        body: Box::new(body),
                    }
                })?;
                Ok(if iter {
                    ContractWitnessTarget::IteratorFunction { function }
                } else {
                    ContractWitnessTarget::Function { function }
                })
            }
            WitnessSlotTarget::Extern { method, .. } => {
                let target = ExternUseTarget::Method(*method);
                let function = self.maps.externs.get(&target).copied().ok_or(
                    LowerError::UnsupportedExternUse {
                        expr_id: ExprId::default(),
                        kind: UnsupportedExternUseKind::Method,
                    },
                )?;
                Ok(ContractWitnessTarget::Extern { function })
            }
            WitnessSlotTarget::Promoted {
                path,
                origin_owner,
                target,
                ..
            } => {
                let mut ty = receiver_ty;
                let mut fields = vec![];
                for name in path {
                    let (field, field_ty) =
                        self.contract_witness_field(ty, *name).ok_or_else(|| {
                            LowerError::UnsupportedType {
                                ty: Box::new(origin_owner.clone()),
                            }
                        })?;
                    fields.push(field);
                    ty = field_ty;
                }
                Ok(ContractWitnessTarget::Promoted {
                    fields,
                    target: Box::new(self.lower_contract_witness_target(ty, target, iter)?),
                })
            }
        }
    }

    fn contract_witness_field(&self, ty: TypeId, name: Ident) -> Option<(FieldId, TypeId)> {
        let aggregate = match self.program.type_data(ty) {
            TypeData::Aggregate(id) | TypeData::DataRef(id) => self.program.aggregate(*id),
            _ => return None,
        };
        aggregate
            .fields
            .iter()
            .enumerate()
            .find(|(_, field)| field.name == name)
            .map(|(index, field)| (FieldId::from_index(index), field.ty))
    }

    fn close_projected_contract_witnesses(&mut self) {
        loop {
            let mut projected = vec![];
            for weakening in &self.program.contract_weakenings {
                for witness in &self.program.contract_witnesses {
                    if witness.key.surface != weakening.source {
                        continue;
                    }
                    let slots = weakening
                        .target_to_source
                        .iter()
                        .enumerate()
                        .map(|(target_slot, source_slot)| {
                            let source = &witness.key.slots[source_slot.index()];
                            ContractWitnessSlotDecl {
                                slot: ContractSlotId::from_index(target_slot),
                                receiver: source.receiver,
                                target: source.target.clone(),
                            }
                        })
                        .collect::<Vec<_>>();
                    let structural_key = AirWitnessKey {
                        concrete_ty: witness.key.concrete_ty,
                        surface: weakening.target,
                        slots: slots.clone(),
                    };
                    if !self
                        .program
                        .contract_witnesses
                        .iter()
                        .any(|existing| existing.key == structural_key)
                        && !projected
                            .iter()
                            .any(|candidate: &ContractWitnessDecl| candidate.key == structural_key)
                    {
                        projected.push(ContractWitnessDecl {
                            key: structural_key,
                        });
                    }
                }
            }
            if projected.is_empty() {
                break;
            }
            for witness in projected {
                self.program.alloc_contract_witness(witness);
            }
        }
    }

    fn optional_ty(&mut self, inner: TypeId) -> TypeId {
        optional_ty(&mut self.program, inner)
    }

    fn lambda_escape_fact(&self, expr_id: ExprId) -> Result<&LambdaEscapeFact, LowerError> {
        self.typecheck_facts
            .ok_or(LowerError::MissingTypecheckFacts)?
            .lambda_escapes()
            .get(&expr_id)
            .ok_or(LowerError::MissingLambdaEscape { expr_id })
    }

    fn binding_requires_capture_cell(&self, binding: BindingId) -> bool {
        self.typecheck_facts
            .is_some_and(|facts| facts.capture_cell_requirements().contains_key(&binding))
    }

    fn iter_runtime_check(&self, expr_id: ExprId) -> Option<IterRuntimeCheckKind> {
        self.typecheck_facts
            .and_then(|facts| facts.iter_runtime_check(expr_id))
    }

    fn ordered_lambda_capture_facts(
        &self,
        expr_id: ExprId,
    ) -> Result<Vec<LambdaCaptureFact>, LowerError> {
        let mut facts = self
            .typecheck_facts
            .ok_or(LowerError::MissingTypecheckFacts)?
            .lambda_captures()
            .values()
            .filter(|capture| capture.lambda_id == expr_id)
            .cloned()
            .collect::<Vec<_>>();
        facts.sort_by_key(|fact| fact.binding_id);
        Ok(facts)
    }

    fn lower_lambda_capture_decls(
        &mut self,
        expr_id: ExprId,
        owner: FunctionId,
        owner_function: &Function,
        sources: &HashMap<BindingId, LambdaCaptureSource>,
    ) -> Result<Vec<LambdaCaptureDecl>, LowerError> {
        self.ordered_lambda_capture_facts(expr_id)?
            .into_iter()
            .map(|capture| {
                self.lower_lambda_capture_decl(expr_id, owner, owner_function, sources, &capture)
            })
            .collect()
    }

    fn lower_lambda_capture_decl(
        &mut self,
        expr_id: ExprId,
        owner: FunctionId,
        owner_function: &Function,
        sources: &HashMap<BindingId, LambdaCaptureSource>,
        capture: &LambdaCaptureFact,
    ) -> Result<LambdaCaptureDecl, LowerError> {
        let binding = air_binding_id(capture.binding_id);
        let ty = self.lower_ty(&capture.ty)?;
        match lowered_capture_kind(expr_id, capture.runtime_plan)? {
            LoweredCaptureKind::NoRuntime => Ok(LambdaCaptureDecl::NoRuntime { binding, ty }),
            LoweredCaptureKind::ReadonlyLocal => {
                let source = exact_local_capture_source(
                    expr_id,
                    owner,
                    owner_function,
                    sources,
                    capture,
                    ty,
                )?;
                Ok(LambdaCaptureDecl::ReadonlyLocal {
                    binding,
                    source,
                    ty,
                })
            }
            LoweredCaptureKind::CaptureCell => {
                let cell = exact_capture_cell(expr_id, sources, capture, ty)?;
                Ok(LambdaCaptureDecl::CaptureCell { binding, cell, ty })
            }
            LoweredCaptureKind::ScopedBorrow => {
                let borrow = exact_scoped_borrow(expr_id, sources, capture, ty)?;
                Ok(LambdaCaptureDecl::ScopedBorrow {
                    binding,
                    borrow,
                    ty,
                    mutability: AirMutability::Mutable,
                })
            }
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
        self.register_body_function(body, locals, id);
        id
    }

    fn alloc_lambda_function_in_module(
        &mut self,
        module: ModuleId,
        body: BodyInstanceKey,
        locals: HashMap<SemanticLocalId, LocalId>,
        decl: impl FnOnce(FunctionId) -> LambdaDecl,
        build: impl FnOnce(LambdaId) -> Function,
    ) -> FunctionId {
        let expected = FunctionId::from_index(self.program.functions.len());
        let lambda = self.program.alloc_lambda(decl(expected));
        let function = self.program.alloc_function(build(lambda));
        debug_assert_eq!(function, expected);
        self.program.module_mut(module).functions.push(function);
        if let BodyInstanceKey::Lambda(key) = &body {
            self.maps.lambdas.insert(key.clone(), lambda);
        }
        self.register_body_function(body, locals, function);
        function
    }

    fn alloc_global_in_module(
        &mut self,
        scope: &ModuleScope,
        key: GlobalKey,
        body: BodyInstanceKey,
        build: impl FnOnce(ModuleId, GlobalId, FunctionId) -> (GlobalDecl, Function),
    ) -> FunctionId {
        let module = self.ensure_module(scope);
        let (global, init) = self
            .program
            .alloc_global_with_init(|global, init| build(module, global, init));
        self.maps.globals.insert(key, global);
        self.register_body_function(body, HashMap::new(), init);
        init
    }

    fn register_body_function(
        &mut self,
        body: BodyInstanceKey,
        locals: HashMap<SemanticLocalId, LocalId>,
        function: FunctionId,
    ) {
        let old = self.maps.bodies.insert(body.clone(), function);
        debug_assert!(old.is_none(), "duplicate lowered function body");
        let old = self.maps.locals.insert(body, locals);
        debug_assert!(old.is_none(), "duplicate lowered function local map");
    }

    fn alloc_extern_in_module(
        &mut self,
        scope: &ModuleScope,
        target: ExternUseTarget,
        name: Ident,
        member: ExternMember,
        params: Vec<ExternParamDecl>,
        return_type: TypeId,
        abi: ExternAbi,
        info: ExternLoweringInfo,
    ) -> ExternId {
        let module = self.ensure_module(scope);
        let (binding, effects) = match info {
            ExternLoweringInfo::Provider(info) => (
                Some(ExternBindingDecl {
                    package: info.package,
                    provider: info.provider,
                    key: info.key,
                }),
                info.effects,
            ),
            ExternLoweringInfo::Source { effects } => (None, effects),
        };
        let id = self.program.alloc_extern(ExternDecl {
            name,
            module,
            member,
            params,
            return_type,
            abi,
            binding,
            effects,
        });
        self.program.module_mut(module).externs.push(id);
        let old = self.maps.externs.insert(target, id);
        debug_assert!(old.is_none(), "duplicate lowered extern");
        id
    }

    fn lower_extern_declarations(
        &mut self,
        functions: &ReachableItems<'_>,
        semantic: &SemanticProgram,
    ) -> Result<(), LowerError> {
        let mut externs = vec![];
        for slice in functions.contract_fact_slices()? {
            externs.extend(
                slice
                    .facts
                    .extern_uses
                    .iter()
                    .filter(|(expr, _)| slice.includes(**expr))
                    .flat_map(|(_, targets)| targets.iter().copied())
                    .filter(|target| extern_use_requires_decl(&semantic.externs, *target)),
            );
            for conversion in slice
                .facts
                .dyn_conversions
                .values()
                .filter(|fact| slice.includes(fact.expr_id))
            {
                let key = semantic
                    .facts
                    .witness_structural_keys
                    .get(&conversion.witness)
                    .expect("finalized witness structural key exists");
                for target in &key.slots {
                    collect_witness_extern_targets(target, &mut externs);
                }
            }
        }
        externs.sort_by_key(|target| extern_sort_key(&semantic.externs, *target));
        externs.dedup();
        for target in externs {
            if !self.maps.externs.contains_key(&target) {
                self.lower_extern_declaration(&semantic.externs, target)?;
            }
        }
        Ok(())
    }

    fn lower_extern_declaration(
        &mut self,
        externs: &ExternCatalog,
        target: ExternUseTarget,
    ) -> Result<ExternId, LowerError> {
        match target {
            ExternUseTarget::Function(id) => self.lower_extern_function(externs, id),
            ExternUseTarget::FieldRead(field) => {
                let (ty, field_decl) = externs.field_ref(field);
                let owner = self.lower_extern_owner(&ty.nominal)?;
                let receiver_ty = self.extern_owner_type(owner);
                let params = vec![];
                let return_type = self.lower_ty(&field_decl.ty.ty)?;
                Ok(self.alloc_extern_in_module(
                    &ty.key.module,
                    target,
                    field_decl.name,
                    ExternMember::FieldGetter {
                        owner,
                        receiver: ExternReceiverDecl {
                            ty: receiver_ty,
                            mode: receiver_mode(field_decl.get_receiver),
                        },
                        computed: field_decl.computed,
                    },
                    params,
                    return_type,
                    extern_receiver_abi(&ty.key, field_decl.ty.abi.clone()),
                    externs.field_lowering_info(field, anvyx_externs::ExternBindingOp::Get),
                ))
            }
            ExternUseTarget::FieldWrite(field) => {
                let (ty, field_decl) = externs.field_ref(field);
                let owner = self.lower_extern_owner(&ty.nominal)?;
                let receiver_ty = self.extern_owner_type(owner);
                let params = vec![ExternParamDecl {
                    ty: self.lower_ty(&field_decl.ty.ty)?,
                    mode: ParamMode::Value,
                    escape: ParamEscape::NonEscaping,
                }];
                let return_type = self.lower_ty(&Type::Void)?;
                Ok(self.alloc_extern_in_module(
                    &ty.key.module,
                    target,
                    field_decl.name,
                    ExternMember::FieldSetter {
                        owner,
                        receiver: ExternReceiverDecl {
                            ty: receiver_ty,
                            mode: receiver_mode(field_decl.set_receiver),
                        },
                    },
                    params,
                    return_type,
                    extern_receiver_signature_abi(
                        &ty.key,
                        ExternAbi {
                            params: vec![field_decl.ty.abi.clone()],
                            ret: anvyx_externs::ExternTypeExpr::Void,
                        },
                    ),
                    externs.field_lowering_info(field, anvyx_externs::ExternBindingOp::Set),
                ))
            }
            ExternUseTarget::Method(method_ref) => {
                let (ty, method) = externs.method_ref(method_ref);
                let owner = self.lower_extern_owner(&ty.nominal)?;
                let receiver_ty = self.extern_owner_type(owner);
                let params = self.lower_extern_signature_params(&method.signature)?;
                let return_type = self.lower_ty(&method.signature.ret.ty)?;
                Ok(self.alloc_extern_in_module(
                    &ty.key.module,
                    target,
                    method.name,
                    ExternMember::Method {
                        owner,
                        receiver: ExternReceiverDecl {
                            ty: receiver_ty,
                            mode: receiver_mode(method.receiver),
                        },
                    },
                    params,
                    return_type,
                    extern_receiver_signature_abi(&ty.key, extern_signature_abi(&method.signature)),
                    externs.method_lowering_info(method_ref),
                ))
            }
            ExternUseTarget::Static(static_ref) => {
                let (ty, static_method) = externs.static_ref(static_ref);
                let owner = self.lower_extern_owner(&ty.nominal)?;
                let params = self.lower_extern_signature_params(&static_method.signature)?;
                let return_type = self.lower_ty(&static_method.signature.ret.ty)?;
                Ok(self.alloc_extern_in_module(
                    &ty.key.module,
                    target,
                    static_method.name,
                    ExternMember::StaticMethod { owner },
                    params,
                    return_type,
                    extern_signature_abi(&static_method.signature),
                    externs.static_lowering_info(static_ref),
                ))
            }
            ExternUseTarget::Init(owner_id) => {
                let ty = externs.ty(owner_id);
                let owner = self.lower_extern_owner(&ty.nominal)?;
                let signature = ty
                    .init
                    .as_ref()
                    .map(|init| &init.signature)
                    .expect("extern init target has descriptor");
                let params = self.lower_extern_signature_params(signature)?;
                let return_type = self.lower_ty(&signature.ret.ty)?;
                Ok(self.alloc_extern_in_module(
                    &ty.key.module,
                    target,
                    ty.key.name,
                    ExternMember::Init { owner },
                    params,
                    return_type,
                    extern_signature_abi(signature),
                    externs.init_lowering_info(owner_id),
                ))
            }
            ExternUseTarget::UnaryOperator(operator_ref) => {
                let (ty, operator) = externs.operator_ref(operator_ref);
                let owner = self.lower_extern_owner(&ty.nominal)?;
                let receiver_ty = self.extern_owner_type(owner);
                let op = match operator.op {
                    anvyx_externs::ExternOperator::Unary(op) => op,
                    anvyx_externs::ExternOperator::Binary { .. } => unreachable!("unary target"),
                };
                let return_type = self.lower_ty(&operator.signature.ret.ty)?;
                Ok(self.alloc_extern_in_module(
                    &ty.key.module,
                    target,
                    operator_name(operator.op),
                    ExternMember::UnaryOperator {
                        owner,
                        receiver: ExternReceiverDecl {
                            ty: receiver_ty,
                            mode: receiver_mode(operator.receiver),
                        },
                        op: lower_unary_op(op),
                    },
                    vec![],
                    return_type,
                    extern_receiver_signature_abi(
                        &ty.key,
                        extern_signature_abi(&operator.signature),
                    ),
                    externs.operator_lowering_info(operator_ref),
                ))
            }
            ExternUseTarget::BinaryOperator(operator_ref) => {
                let (ty, operator) = externs.operator_ref(operator_ref);
                let owner = self.lower_extern_owner(&ty.nominal)?;
                let receiver_ty = self.extern_owner_type(owner);
                let anvyx_externs::ExternOperator::Binary { op, self_on_right } = operator.op
                else {
                    unreachable!("binary target")
                };
                let params = self.lower_extern_signature_params(&operator.signature)?;
                let return_type = self.lower_ty(&operator.signature.ret.ty)?;
                Ok(self.alloc_extern_in_module(
                    &ty.key.module,
                    target,
                    operator_name(operator.op),
                    ExternMember::BinaryOperator {
                        owner,
                        receiver: ExternReceiverDecl {
                            ty: receiver_ty,
                            mode: receiver_mode(operator.receiver),
                        },
                        op: lower_binary_op(op),
                        self_on_right,
                    },
                    params,
                    return_type,
                    extern_receiver_signature_abi(
                        &ty.key,
                        extern_signature_abi(&operator.signature),
                    ),
                    externs.operator_lowering_info(operator_ref),
                ))
            }
        }
    }

    fn lower_extern_function(
        &mut self,
        externs: &ExternCatalog,
        id: crate::externs::catalog::ExternFunctionId,
    ) -> Result<ExternId, LowerError> {
        let function = externs.function(id);
        let params = self.lower_extern_signature_params(&function.signature)?;
        let return_type = self.lower_ty(&function.signature.ret.ty)?;
        Ok(self.alloc_extern_in_module(
            &function.key.module,
            ExternUseTarget::Function(id),
            function.key.name,
            ExternMember::FreeFunction,
            params,
            return_type,
            extern_signature_abi(&function.signature),
            externs.function_lowering_info(id),
        ))
    }

    fn lower_extern_signature_params(
        &mut self,
        signature: &crate::externs::catalog::ResolvedExternSignature,
    ) -> Result<Vec<ExternParamDecl>, LowerError> {
        signature
            .params
            .iter()
            .map(|param| {
                let ty = self.lower_ty(&param.ty.ty)?;
                checked_extern_param(
                    &self.program,
                    ty,
                    param_flow_mode(param.flow),
                    param.escape.into(),
                )
            })
            .collect()
    }

    fn lower_extern_owner(
        &mut self,
        key: &NominalKey,
    ) -> Result<crate::air::ExternTypeId, LowerError> {
        let ty = crate::typecheck::nominal_type(key);
        let ty = self.lower_ty(&ty)?;
        let TypeData::Extern(owner) = self.program.type_data(ty) else {
            return Err(LowerError::UnsupportedType {
                ty: Box::new(crate::typecheck::nominal_type(key)),
            });
        };
        Ok(*owner)
    }

    fn extern_owner_type(&self, owner: crate::air::ExternTypeId) -> TypeId {
        self.program
            .type_arena
            .iter()
            .enumerate()
            .find_map(|(index, ty)| match ty {
                TypeData::Extern(id) if *id == owner => Some(TypeId::from_index(index)),
                _ => None,
            })
            .expect("extern owner type was just lowered")
    }

    fn lower_function_shells(
        &mut self,
        modules: &[ModuleScope],
        functions: &ReachableItems<'_>,
    ) -> Result<(), LowerError> {
        for source in &functions.items {
            match &source.source {
                ReachableSource::Callable { callable, fact } => {
                    let module_scope = &modules[callable.module()];
                    let return_mode = if fact.ret.is_iter() {
                        ReturnMode::Value(self.lower_ty(&Type::Void)?)
                    } else {
                        let return_type = self.lower_ty(&fact.ret.ty())?;
                        match fact.ret.access() {
                            ReturnAccess::Value => ReturnMode::Value(return_type),
                            ReturnAccess::Place => ReturnMode::Place(return_type),
                        }
                    };
                    let (params, locals, local_map) =
                        self.lower_callable_params(source, *callable, fact)?;
                    let specialization = self.function_specialization(&source.body)?;
                    self.alloc_function_in_module(
                        module_scope,
                        source.body.clone(),
                        local_map,
                        |module| Function {
                            name: callable.name(),
                            module,
                            kind: callable.function_kind(),
                            owner: callable.owner(),
                            specialization,
                            signature: Signature::with_return_mode(params, return_mode),
                            locals,
                            body: AirBody {
                                block: AirBlock::default(),
                            },
                        },
                    );
                }
                ReachableSource::Lambda { owner, lambda, ty } => {
                    let owner = self.maps.bodies[owner];
                    let module = self.program.function(owner).module;
                    let ty = *ty;
                    let Type::Func {
                        params: source_params,
                        ret,
                    } = ty
                    else {
                        return Err(LowerError::UnsupportedType {
                            ty: Box::new(ty.clone()),
                        });
                    };
                    let return_ty = self.lower_ty(&ret.ty())?;
                    let return_mode = match ret.access() {
                        ReturnAccess::Value => ReturnMode::Value(return_ty),
                        ReturnAccess::Place => ReturnMode::Place(return_ty),
                    };
                    let (params, locals, local_map) =
                        self.lower_lambda_params(source, lambda, source_params)?;
                    let signature = Signature::with_return_mode(params, return_mode);
                    let lambda_signature = SignatureType::new(
                        signature.params.iter().map(Param::param_type).collect(),
                        signature.return_mode,
                    );
                    let lambda_key = match &source.body {
                        BodyInstanceKey::Lambda(key) => key.clone(),
                        _ => unreachable!("lambda source must use lambda body key"),
                    };
                    let escape = self.lambda_escape(lambda_key.expr)?;
                    self.alloc_lambda_function_in_module(
                        module,
                        source.body.clone(),
                        local_map,
                        |body| LambdaDecl {
                            source: lambda_key.expr,
                            module,
                            owner,
                            body,
                            signature: lambda_signature,
                            escape,
                            captures: vec![],
                        },
                        |lambda| Function {
                            name: Ident::new("lambda"),
                            module,
                            kind: FunctionKind::Lambda(lambda),
                            owner: None,
                            specialization: None,
                            signature,
                            locals,
                            body: AirBody {
                                block: AirBlock::default(),
                            },
                        },
                    );
                }
                ReachableSource::Global { global, sig } => {
                    let module_scope = &modules[global.module];
                    let ty = self.lower_ty(&sig.ty)?;
                    let mutability = lower_global_mutability(sig.mutability);
                    self.alloc_global_in_module(
                        module_scope,
                        sig.key.clone(),
                        source.body.clone(),
                        |module, global_id, init| {
                            (
                                GlobalDecl {
                                    name: sig.key.name,
                                    module,
                                    ty,
                                    mutability,
                                    init,
                                },
                                Function {
                                    name: sig.key.name,
                                    module,
                                    kind: FunctionKind::GlobalInit(global_id),
                                    owner: None,
                                    specialization: None,
                                    signature: Signature::new(vec![], ty),
                                    locals: vec![],
                                    body: AirBody {
                                        block: AirBlock::default(),
                                    },
                                },
                            )
                        },
                    );
                }
                ReachableSource::CastFrom { cast, signature } => {
                    let module_scope = &modules[cast.module];
                    let return_type = self.lower_ty(&signature.ret)?;
                    let (params, locals, local_map) = self.lower_params(
                        source,
                        [ParamLowerSpec {
                            name: cast.node.node.param.name,
                            ty: &signature.source,
                            mutable: cast.node.node.param.mutability == AstMutability::Mutable,
                            escape: cast.node.node.param.escape,
                            role: ParamRole::Normal,
                        }],
                    )?;
                    self.alloc_function_in_module(
                        module_scope,
                        source.body.clone(),
                        local_map,
                        |module| Function {
                            name: Ident::new("cast_from"),
                            module,
                            kind: FunctionKind::Normal,
                            owner: None,
                            specialization: None,
                            signature: Signature::new(params, return_type),
                            locals,
                            body: AirBody {
                                block: AirBlock::default(),
                            },
                        },
                    );
                }
            }
        }
        Ok(())
    }

    fn lower_callable_params(
        &mut self,
        source: &ReachableItem<'_>,
        callable: SourceCallable<'_>,
        fact: &SemanticFunctionInstanceFact,
    ) -> Result<ParamLowerResult, LowerError> {
        let receiver = callable.is_instance_method();
        self.lower_params(
            source,
            fact.params
                .iter()
                .enumerate()
                .map(|(index, param)| ParamLowerSpec {
                    name: param.name,
                    ty: &param.ty,
                    mutable: param.mutable,
                    escape: param.escape,
                    role: if receiver && index == 0 {
                        ParamRole::Receiver
                    } else {
                        ParamRole::Normal
                    },
                }),
        )
    }

    fn lower_lambda_params(
        &mut self,
        source: &ReachableItem<'_>,
        lambda: &ast::LambdaNode,
        source_params: &[ast::FuncParam],
    ) -> Result<ParamLowerResult, LowerError> {
        let specs =
            source_params
                .iter()
                .enumerate()
                .map(|(index, param)| {
                    let lambda_param = lambda.node.params.get(index).ok_or_else(|| {
                        LowerError::MissingParamDef {
                            body: Box::new(source.body.clone()),
                            index,
                        }
                    })?;
                    Ok(ParamLowerSpec {
                        name: lambda_param.name,
                        ty: &param.ty,
                        mutable: param.mutable,
                        escape: param.escape,
                        role: ParamRole::Normal,
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;
        self.lower_params(source, specs)
    }

    fn lower_params<'a>(
        &mut self,
        source: &ReachableItem<'_>,
        specs: impl IntoIterator<Item = ParamLowerSpec<'a>>,
    ) -> Result<ParamLowerResult, LowerError> {
        let body_facts = source.body_facts.as_facts();
        let mut params = vec![];
        let mut locals = vec![];
        let mut local_map = HashMap::new();
        for (index, spec) in specs.into_iter().enumerate() {
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
            debug_assert_eq!(def.name, spec.name);
            debug_assert_eq!(def.mutable, spec.mutable);
            let ty = self.lower_ty(spec.ty)?;
            let local_id = LocalId::from_index(locals.len());
            locals.push(Local {
                name: Some(spec.name),
                binding: def.binding_id.map(air_binding_id),
                ty,
                mutability: if spec.mutable {
                    AirMutability::Mutable
                } else {
                    AirMutability::Immutable
                },
                kind: LocalKind::Arg,
            });
            let old = local_map.insert(semantic_local, local_id);
            debug_assert!(old.is_none(), "duplicate semantic param local");
            params.push(Param {
                name: Some(spec.name),
                ty,
                mode: source_param_mode(spec.mutable),
                escape: spec.escape.into(),
                role: spec.role,
                local_id,
            });
        }
        Ok((params, locals, local_map))
    }

    fn lambda_escape(&self, expr: ExprId) -> Result<LambdaEscape, LowerError> {
        match self.lambda_escape_fact(expr)?.escape {
            LambdaEscapeKind::NonEscaping => Ok(LambdaEscape::NonEscaping),
            LambdaEscapeKind::Escaping => Ok(LambdaEscape::Escaping),
        }
    }

    fn function_specialization(
        &mut self,
        body: &BodyInstanceKey,
    ) -> Result<Option<FunctionSpecialization>, LowerError> {
        let BodyInstanceKey::Callable(key) = body else {
            return Ok(None);
        };
        if key.args.is_empty() {
            return Ok(None);
        }

        let type_args = key
            .args
            .type_args
            .iter()
            .map(|ty| self.lower_ty(ty))
            .collect::<Result<Vec<_>, _>>()?;
        let const_args = key
            .args
            .const_args
            .iter()
            .map(|arg| match arg {
                ConstTerm::Value(value) => Ok(lower_const_specialization_value(value)),
                ConstTerm::Name(_)
                | ConstTerm::Param(_)
                | ConstTerm::Expr(_)
                | ConstTerm::ArrayInfer
                | ConstTerm::Infer(_) => Err(LowerError::UnsupportedCallableInstance {
                    id: Box::new(key.target.clone()),
                    args: Box::new(key.args.clone()),
                }),
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Some(FunctionSpecialization {
            type_args,
            const_args,
        }))
    }

    fn attach_stringify_overrides(&mut self, functions: &ReachableItems<'_>) {
        for source in &functions.items {
            let ReachableSource::Callable { fact, .. } = &source.source else {
                continue;
            };
            if !fact.is_stringify_override {
                continue;
            }
            let function_id = self.maps.bodies[&source.body];
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

    fn lower_function_bodies(&mut self, functions: &ReachableItems<'_>) -> Result<(), LowerError> {
        let mut lowered = HashSet::new();
        for source in &functions.items {
            if let ReachableSource::Lambda { owner, .. } = &source.source
                && !lowered.contains(owner)
            {
                return Err(LowerError::MissingSpecializedBodyFacts {
                    body: Box::new(owner.clone()),
                });
            }
            let facts = source.body_facts.as_facts();
            let function = self.maps.bodies[&source.body];
            let locals = self
                .maps
                .locals
                .remove(&source.body)
                .expect("lowered function missing local map");
            let mut lowerer = FunctionLowerer::new(
                self,
                functions,
                &source.body,
                facts,
                function,
                locals,
                source.source_id,
            )?;
            match &source.source {
                ReachableSource::Callable { callable, .. } => {
                    lowerer.lower_body(callable.body())?;
                }
                ReachableSource::Lambda { lambda, .. } => {
                    lowerer.lower_expr_body(&lambda.node.body)?;
                }
                ReachableSource::Global { global, .. } => {
                    lowerer.lower_expr_body(&global.node.node.value)?;
                }
                ReachableSource::CastFrom { cast, .. } => {
                    lowerer.lower_body(&cast.node.node.body)?;
                }
            }
            lowered.insert(source.body.clone());
        }
        Ok(())
    }
}

type ParamLowerResult = (Vec<Param>, Vec<Local>, HashMap<SemanticLocalId, LocalId>);

struct ParamLowerSpec<'a> {
    name: Ident,
    ty: &'a Type,
    mutable: bool,
    escape: ast::EscapeMode,
    role: ParamRole,
}

#[derive(Clone)]
enum LambdaCaptureSource {
    Local(Place),
    Capture {
        slot: LambdaCaptureSlotId,
        decl: LambdaCaptureDecl,
    },
}

#[derive(Clone, Copy)]
enum LoweredCaptureKind {
    NoRuntime,
    ReadonlyLocal,
    CaptureCell,
    ScopedBorrow,
}

struct FunctionLowerer<'cx, 'facts, 'tc> {
    cx: &'cx mut LowerCx<'tc>,
    body: BodyInstanceKey,
    facts: &'facts SemanticBodyFacts,
    index: &'facts SourceProgramIndex<'facts>,
    body_facts: &'facts HashMap<BodyInstanceKey, SemanticBodyFacts>,
    callable_facts: &'facts SemanticCallableFacts<'facts>,
    function_id: FunctionId,
    source: SourceId,
    function: Function,
    locals: HashMap<SemanticLocalId, Place>,
    capture_sources: HashMap<BindingId, LambdaCaptureSource>,
    binding_scoped_borrows: HashMap<BindingId, ScopedBorrowId>,
    binding_cells: HashMap<BindingId, CaptureCellId>,
    owned_lambdas: Vec<(ExprId, LambdaId)>,
    block: AirBlock,
    terminated: bool,
    next_loop: u32,
    active_loops: Vec<AirLoopId>,
    type_overrides: HashMap<ExprId, Type>,
}

impl<'cx, 'facts, 'tc> FunctionLowerer<'cx, 'facts, 'tc> {
    fn new(
        cx: &'cx mut LowerCx<'tc>,
        functions: &'facts ReachableItems<'facts>,
        body: &BodyInstanceKey,
        facts: &'facts SemanticBodyFacts,
        function_id: FunctionId,
        locals: HashMap<SemanticLocalId, LocalId>,
        source: SourceId,
    ) -> Result<Self, LowerError> {
        let function = cx.program.function(function_id).clone();
        let mut capture_sources = initial_capture_sources(body, facts, &locals, &function)?;
        capture_sources.extend(lambda_capture_sources(&cx.program, &function));
        let owned_lambdas = owned_reachable_lambdas(cx, functions, body);
        let binding_cells = binding_capture_cells(&cx.program, function_id);
        let mut binding_scoped_borrows = binding_scoped_borrows(&cx.program, function_id);
        alloc_scoped_borrows(
            cx,
            function_id,
            &function,
            &owned_lambdas,
            &mut capture_sources,
            &mut binding_scoped_borrows,
        )?;
        let locals = locals
            .into_iter()
            .map(|(semantic, local)| {
                let ty = function.locals[local.index()].ty;
                let source_ref_dyn = facts
                    .locals
                    .defs
                    .get(&semantic)
                    .is_some_and(|def| def.mutable);
                if source_ref_dyn && let TypeData::Dyn(surface) = cx.program.type_data(ty) {
                    let borrow = cx.program.alloc_dyn_borrow_param(DynBorrowParamDecl {
                        owner: function_id,
                        source: local,
                        ty,
                        surface: *surface,
                    });
                    return (
                        semantic,
                        cx.program
                            .dyn_borrow_param_place(borrow)
                            .expect("allocated dynamic borrow parameter"),
                    );
                }
                let place = facts
                    .locals
                    .defs
                    .get(&semantic)
                    .and_then(|def| def.binding_id)
                    .and_then(|binding| binding_scoped_borrows.get(&binding).copied())
                    .and_then(|borrow| cx.program.scoped_borrow_place(borrow))
                    .unwrap_or_else(|| function_local_place(&function, local));
                (semantic, place)
            })
            .collect();
        Ok(Self {
            cx,
            body: body.clone(),
            facts,
            index: functions.index,
            body_facts: functions.body_facts,
            callable_facts: functions.callable_facts,
            function_id,
            source,
            function,
            locals,
            capture_sources,
            binding_scoped_borrows,
            binding_cells,
            owned_lambdas,
            block: AirBlock::default(),
            terminated: false,
            next_loop: 0,
            active_loops: vec![],
            type_overrides: HashMap::new(),
        })
    }

    fn insert_capture_source(
        &mut self,
        semantic: SemanticLocalId,
        place: Place,
    ) -> Result<(), LowerError> {
        let Some(binding) = self.local_def(semantic)?.binding_id else {
            return Ok(());
        };
        if self
            .capture_sources
            .insert(binding, LambdaCaptureSource::Local(place))
            .is_some()
        {
            return Err(LowerError::DuplicateBindingBridge {
                body: Box::new(self.body.clone()),
                binding,
            });
        }
        Ok(())
    }

    fn alloc_binding_cell(
        &mut self,
        binding: BindingId,
        source_local: LocalId,
        ty: TypeId,
    ) -> Result<CaptureCellId, LowerError> {
        if let Some(cell) = self.binding_cells.get(&binding).copied() {
            return Ok(cell);
        }
        let lifetime = match self.active_loops.last().copied() {
            Some(loop_id) => CaptureCellLifetime::Loop { loop_id },
            None => CaptureCellLifetime::Function,
        };
        let cell = self.cx.program.alloc_capture_cell(CaptureCellDecl {
            binding: air_binding_id(binding),
            owner: self.function_id,
            source_local,
            ty,
            lifetime,
        });
        if self.binding_cells.insert(binding, cell).is_some() {
            return Err(LowerError::DuplicateBindingBridge {
                body: Box::new(self.body.clone()),
                binding,
            });
        }
        Ok(cell)
    }

    fn current_specialization(&self) -> GenericArgs {
        match &self.body {
            BodyInstanceKey::Callable(key) => key.args.clone(),
            BodyInstanceKey::Lambda(key) => key.specialization.clone(),
            BodyInstanceKey::CastFrom(key) => key.args.clone(),
            BodyInstanceKey::Module(_) | BodyInstanceKey::Global(_) => GenericArgs::default(),
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
                let value = self.lower_return_operand(tail)?;
                if !self.terminated {
                    self.terminate(AirTail::Return(Some(value)))?;
                }
            }
        }
        if !self.terminated && self.returns_void() {
            self.terminate(AirTail::Return(None))?;
        }
        self.finish_body()
    }

    fn lower_expr_body(&mut self, body: &ExprNode) -> Result<(), LowerError> {
        if self.returns_void() {
            self.lower_effect(body)?;
            if !self.terminated {
                self.terminate(AirTail::Return(None))?;
            }
        } else {
            let value = self.lower_return_operand(body)?;
            if !self.terminated {
                self.terminate(AirTail::Return(Some(value)))?;
            }
        }
        self.finish_body()
    }

    fn finish_body(&mut self) -> Result<(), LowerError> {
        if !self.terminated {
            return Err(LowerError::UnterminatedBlock);
        }
        self.populate_owned_lambda_capture_decls()?;
        self.function.body = AirBody {
            block: std::mem::take(&mut self.block),
        };
        self.cx.program.functions[self.function_id.index()] = self.function.clone();
        Ok(())
    }

    fn lower_stmts(&mut self, stmts: &[StmtNode]) -> Result<(), LowerError> {
        for stmt in stmts {
            self.lower_stmt(stmt)?;
        }
        Ok(())
    }

    fn lower_return_operand(&mut self, expr: &ExprNode) -> Result<Operand, LowerError> {
        match self.function.signature.return_mode {
            ReturnMode::Value(expected) => self.lower_value_to(expr, expected, expr),
            ReturnMode::Place(_) => self.lower_place_arg(expr, true).map(Operand::Place),
        }
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
        if let Some(tail) = &block.node.tail
            && !self.terminated
        {
            self.lower_effect(tail)?;
        }
        Ok(())
    }

    fn lower_if_effect(&mut self, if_expr: &ast::IfNode) -> Result<(), LowerError> {
        let cond = self.lower_if_cond(&if_expr.node.cond)?;
        let then_block = self.lower_nested_effect(&if_expr.node.then_block)?;
        let else_block = if let Some(else_block) = &if_expr.node.else_block {
            Some(self.lower_nested_effect(else_block)?)
        } else {
            None
        };
        let then_falls = air_block_falls_through(&then_block);
        let else_falls = else_block.as_ref().is_none_or(air_block_falls_through);
        self.ensure_open()?;
        self.block.stmts.push(AirStmt::If(AirIf {
            cond,
            then_block,
            else_block,
        }));
        if !then_falls && !else_falls {
            self.terminate(AirTail::Unreachable)?;
        }
        Ok(())
    }

    fn lower_if_let_effect(&mut self, if_let: &ast::IfLetNode) -> Result<(), LowerError> {
        if self
            .flag_conditional_pattern(if_let.node.value.node.id)
            .is_some()
        {
            return self.lower_flag_if_let_effect(if_let);
        }
        if let Some(pattern) = direct_dyn_failable_alias_pattern(if_let)
            && self
                .facts
                .dyn_downcasts
                .get(&if_let.node.value.node.id)
                .is_some_and(|fact| fact.mutable)
        {
            return self.lower_dyn_if_let_alias(if_let, pattern, MatchOutput::Effect);
        }
        if let Some(pattern) = direct_failable_payload_pattern(if_let)? {
            let subject = self.lower_optional_subject(&if_let.node.value, &if_let.node.value)?;
            let payload = self.temp(subject.inner_ty());
            return self.emit_optional_match_with_payload_ref(
                subject,
                Some(payload),
                false,
                |this, payload| {
                    this.lower_optional_payload_binding(pattern, payload, false)?;
                    this.lower_block_effect(&if_let.node.then_block)
                },
                |this| this.lower_optional_else_effect(if_let.node.else_block.as_ref()),
            );
        }
        let alias = if_let.node.head.is_ref();
        let subject =
            self.lower_optional_pattern_subject(&if_let.node.value, &if_let.node.value, alias)?;
        match classify_optional_pattern(&if_let.node.pattern)? {
            OptionalPattern::Some(pattern) => {
                let mode = optional_payload_mode(pattern, alias);
                let payload = mode.needs_payload().then(|| self.temp(subject.inner_ty()));
                self.emit_optional_match_with_payload_ref(
                    subject,
                    payload,
                    mode.payload_ref(),
                    |this, payload| {
                        this.lower_optional_payload_binding(pattern, payload, alias)?;
                        this.lower_block_effect(&if_let.node.then_block)
                    },
                    |this| this.lower_optional_else_effect(if_let.node.else_block.as_ref()),
                )
            }
            OptionalPattern::None => self.emit_optional_match(
                subject,
                None,
                |this, _| this.lower_optional_else_effect(if_let.node.else_block.as_ref()),
                |this| this.lower_block_effect(&if_let.node.then_block),
            ),
        }
    }

    fn flag_conditional_pattern(&self, value: ExprId) -> Option<&CheckedConditionalPattern> {
        self.facts
            .conditional_patterns
            .get(&value)
            .filter(|pattern| checked_pattern_has_flag_value(&pattern.pattern))
    }

    fn lower_flag_if_let_effect(&mut self, if_let: &ast::IfLetNode) -> Result<(), LowerError> {
        let pattern = self
            .flag_conditional_pattern(if_let.node.value.node.id)
            .cloned()
            .ok_or(LowerError::MissingTypecheckFacts)?;
        let subject = self.lower_pattern_subject(
            &if_let.node.value,
            &if_let.node.value,
            CheckedMatchAccess::Owned,
        )?;
        let matched =
            self.with_nested_block(|this| this.lower_block_effect(&if_let.node.then_block))?;
        let fallback = self.with_nested_block(|this| {
            this.lower_optional_else_effect(if_let.node.else_block.as_ref())
        })?;
        let arms =
            self.lower_flag_conditional_arms(&if_let.node.value, &pattern, matched, fallback)?;
        self.push_pattern_match(subject, arms)
    }

    fn lower_flag_conditional_arms(
        &mut self,
        owner: &ExprNode,
        pattern: &CheckedConditionalPattern,
        matched: AirBlock,
        fallback: AirBlock,
    ) -> Result<Vec<AirPatternArm>, LowerError> {
        let checked = CheckedMatchArm {
            bindings: pattern.bindings.clone(),
            pattern: pattern.pattern.clone(),
        };
        Ok(vec![
            AirPatternArm {
                alternatives: self.lower_pattern_alternatives(owner, &checked)?,
                block: matched,
            },
            AirPatternArm {
                alternatives: vec![AirPatternAlternative::default()],
                block: fallback,
            },
        ])
    }

    fn lower_if_let_value(
        &mut self,
        expr: &ExprNode,
        if_let: &ast::IfLetNode,
    ) -> Result<Operand, LowerError> {
        let Some(else_block) = &if_let.node.else_block else {
            return Err(unsupported_expr(expr));
        };
        let result_ty = self.cx.lower_ty(&self.lower_expr_ty(expr.node.id)?)?;
        let result = self.temp(result_ty);
        if let Some(pattern) = self
            .flag_conditional_pattern(if_let.node.value.node.id)
            .cloned()
        {
            let subject = self.lower_pattern_subject(
                &if_let.node.value,
                &if_let.node.value,
                CheckedMatchAccess::Owned,
            )?;
            let matched = self.with_nested_block(|this| {
                this.lower_if_let_result(&if_let.node.then_block, result, result_ty, expr)
            })?;
            let fallback = self.with_nested_block(|this| {
                this.lower_if_let_result(else_block, result, result_ty, expr)
            })?;
            let arms =
                self.lower_flag_conditional_arms(&if_let.node.value, &pattern, matched, fallback)?;
            self.push_pattern_match(subject, arms)?;
            if self.terminated {
                return self.dummy_operand(self.function.signature.return_type());
            }
            return Ok(self.operand_place(result));
        }
        if let Some(pattern) = direct_dyn_failable_alias_pattern(if_let)
            && self
                .facts
                .dyn_downcasts
                .get(&if_let.node.value.node.id)
                .is_some_and(|fact| fact.mutable)
        {
            self.lower_dyn_if_let_alias(if_let, pattern, MatchOutput::Value { result, result_ty })?;
            return Ok(Operand::Place(self.local_place(result)));
        }
        if let Some(pattern) = direct_failable_payload_pattern(if_let)? {
            let subject = self.lower_optional_subject(&if_let.node.value, expr)?;
            let payload = self.temp(subject.inner_ty());
            self.emit_optional_match_with_payload_ref(
                subject,
                Some(payload),
                false,
                |this, payload| {
                    this.lower_optional_payload_binding(pattern, payload, false)?;
                    this.lower_if_let_result(&if_let.node.then_block, result, result_ty, expr)
                },
                |this| this.lower_if_let_result(else_block, result, result_ty, expr),
            )?;
            return Ok(Operand::Place(self.local_place(result)));
        }
        let alias = if_let.node.head.is_ref();
        let subject = self.lower_optional_pattern_subject(&if_let.node.value, expr, alias)?;
        match classify_optional_pattern(&if_let.node.pattern)? {
            OptionalPattern::Some(pattern) => {
                let mode = optional_payload_mode(pattern, alias);
                let payload = mode.needs_payload().then(|| self.temp(subject.inner_ty()));
                self.emit_optional_match_with_payload_ref(
                    subject,
                    payload,
                    mode.payload_ref(),
                    |this, payload| {
                        this.lower_optional_payload_binding(pattern, payload, alias)?;
                        this.lower_if_let_result(&if_let.node.then_block, result, result_ty, expr)
                    },
                    |this| this.lower_if_let_result(else_block, result, result_ty, expr),
                )?;
            }
            OptionalPattern::None => {
                self.emit_optional_match(
                    subject,
                    None,
                    |this, _| this.lower_if_let_result(else_block, result, result_ty, expr),
                    |this| {
                        this.lower_if_let_result(&if_let.node.then_block, result, result_ty, expr)
                    },
                )?;
            }
        }
        if self.terminated {
            return self.dummy_operand(self.function.signature.return_type());
        }
        Ok(self.operand_place(result))
    }

    fn lower_let_else(&mut self, let_else: &ast::LetElse) -> Result<(), LowerError> {
        let alias = false;
        let subject =
            self.lower_optional_pattern_subject(&let_else.value, &let_else.value, alias)?;
        let pattern = classify_optional_pattern(&let_else.pattern)?;
        let mode = match pattern {
            OptionalPattern::Some(pattern) => optional_payload_mode(pattern, alias),
            OptionalPattern::None => PayloadMode::None,
        };
        let payload = mode.needs_payload().then(|| self.temp(subject.inner_ty()));
        let some_block = match pattern {
            OptionalPattern::Some(pattern) => self.with_nested_block(|this| {
                this.lower_optional_payload_binding(pattern, payload, alias)
            })?,
            OptionalPattern::None => self.lower_let_else_fallback(&let_else.fallback)?,
        };
        let none_block = match pattern {
            OptionalPattern::Some(_) => self.lower_let_else_fallback(&let_else.fallback)?,
            OptionalPattern::None => self.with_nested_block(|_| Ok(()))?,
        };
        let fallback_block = match pattern {
            OptionalPattern::Some(_) => &none_block,
            OptionalPattern::None => &some_block,
        };
        if air_block_falls_through(fallback_block) {
            return Err(unsupported_pattern_stmt(&let_else.pattern));
        }
        self.push_optional_match(
            subject,
            payload,
            mode.payload_ref(),
            mode.payload_ref(),
            some_block,
            none_block,
        )?;
        Ok(())
    }

    fn lower_dyn_if_let_alias(
        &mut self,
        if_let: &ast::IfLetNode,
        pattern: &ast::PatternNode,
        output: MatchOutput,
    ) -> Result<(), LowerError> {
        let fact = self
            .facts
            .dyn_downcasts
            .get(&if_let.node.value.node.id)
            .cloned()
            .ok_or(LowerError::MissingTypecheckFacts)?;
        let ExprKind::FailableCast(cast) = &if_let.node.value.node.kind else {
            return Err(unsupported_expr(&if_let.node.value));
        };
        let DynDowncastSource::Resolved(surface) = fact.source else {
            return Err(LowerError::MissingTypecheckFacts);
        };
        if fact.source_id != cast.node.expr.node.id || !fact.mutable {
            return Err(LowerError::MissingTypecheckFacts);
        }
        let surface = self
            .cx
            .maps
            .contract_surfaces
            .get(&surface)
            .copied()
            .ok_or(LowerError::MissingTypecheckFacts)?;
        let target = self.cx.lower_ty(&fact.target)?;
        let binding = self.lower_dyn_alias_pattern_binding(pattern, target)?;
        let some_block = match output {
            MatchOutput::Effect => self.lower_nested_effect(&if_let.node.then_block)?,
            MatchOutput::Value { result, result_ty } => self.with_nested_block(|this| {
                this.lower_if_let_result(
                    &if_let.node.then_block,
                    result,
                    result_ty,
                    &if_let.node.value,
                )
            })?,
        };
        let none_block = match (output, if_let.node.else_block.as_ref()) {
            (MatchOutput::Effect, Some(block)) => self.lower_nested_effect(block)?,
            (MatchOutput::Effect, None) => AirBlock::default(),
            (MatchOutput::Value { result, result_ty }, Some(block)) => {
                self.with_nested_block(|this| {
                    this.lower_if_let_result(block, result, result_ty, &if_let.node.value)
                })?
            }
            (MatchOutput::Value { .. }, None) => return Err(unsupported_expr(&if_let.node.value)),
        };
        let source = self.lower_mut_call_arg(&cast.node.expr)?;
        self.push_dyn_match(AirDynMatch {
            source: AirDynMatchSource::Mutable(source),
            surface,
            arms: vec![AirDynMatchArm {
                target,
                binding: AirDynMatchTargetBinding::Alias(binding),
                block: some_block,
            }],
            fallback: AirDynMatchFallback {
                binding: AirDynMatchFallbackBinding::Discard,
                block: none_block,
            },
        })
    }

    fn lower_dyn_alias_pattern_binding(
        &mut self,
        pattern: &ast::PatternNode,
        ty: TypeId,
    ) -> Result<LocalId, LowerError> {
        let name = pattern_ident(pattern)?;
        let semantic = self.pattern_binding_semantic(pattern)?;
        if let Some(local) = self.existing_semantic_local(semantic) {
            return Ok(local);
        }
        let binding = self.local_def(semantic)?.binding_id;
        let local = self.push_local(
            Some(name),
            binding.map(air_binding_id),
            ty,
            AirMutability::Mutable,
            LocalKind::PatternBinding,
        );
        let place = self.local_place(local);
        self.locals.insert(semantic, place.clone());
        self.insert_capture_source(semantic, place.clone())?;
        self.promote_pattern_alias_scoped_borrow(semantic, binding, &place)?;
        Ok(local)
    }

    fn lower_optional_else_effect(
        &mut self,
        else_block: Option<&BlockNode>,
    ) -> Result<(), LowerError> {
        if let Some(block) = else_block {
            self.lower_block_effect(block)
        } else {
            Ok(())
        }
    }

    fn lower_if_let_result(
        &mut self,
        block: &BlockNode,
        result: LocalId,
        result_ty: TypeId,
        owner: &ExprNode,
    ) -> Result<(), LowerError> {
        if let Some(value) = self.lower_block_branch_value(block, result_ty, owner)? {
            self.emit_init(result, RValue::Use(value))?;
        }
        Ok(())
    }

    fn lower_let_else_fallback(
        &mut self,
        fallback: &ast::LetElseFallbackNode,
    ) -> Result<AirBlock, LowerError> {
        self.with_nested_block(|this| match &fallback.node {
            ast::LetElseFallback::Block(block) => this.lower_block_effect(block),
            ast::LetElseFallback::Return(ret) => match &ret.node.value {
                Some(value) => {
                    let value = this.lower_return_operand(value)?;
                    this.terminate(AirTail::Return(Some(value)))
                }
                None => this.terminate(AirTail::Return(None)),
            },
            ast::LetElseFallback::Break => {
                this.lower_active_loop_tail(fallback.span, AirTail::Break)
            }
            ast::LetElseFallback::Continue => {
                this.lower_active_loop_tail(fallback.span, AirTail::Continue)
            }
        })
    }

    fn lower_active_loop_tail(
        &mut self,
        span: crate::span::Span,
        tail: fn(AirLoopId) -> AirTail,
    ) -> Result<(), LowerError> {
        let Some(id) = self.active_loops.last().copied() else {
            return Err(LowerError::UnsupportedStmt {
                kind: "loop tail outside loop",
                span: Some(self.source_span(span)),
            });
        };
        self.terminate(tail(id))
    }

    fn lower_optional_payload_binding(
        &mut self,
        pattern: &ast::PatternNode,
        payload: Option<LocalId>,
        alias: bool,
    ) -> Result<(), LowerError> {
        if let Some(payload) = payload {
            let payload = Operand::Place(self.local_place(payload));
            self.lower_optional_payload_pattern(pattern, payload, alias)?;
        }
        Ok(())
    }

    fn lower_optional_payload_pattern(
        &mut self,
        pattern: &ast::PatternNode,
        payload: Operand,
        alias: bool,
    ) -> Result<(), LowerError> {
        match &pattern.node {
            Pattern::Optional(inner) => self.lower_optional_payload_pattern(inner, payload, alias),
            Pattern::Ident(_) if alias => self.lower_pattern_alias_binding(pattern, payload),
            Pattern::Ident(_) => self.lower_pattern_binding(pattern, payload),
            Pattern::Wildcard => Ok(()),
            _ => Err(unsupported_pattern_stmt(pattern)),
        }
    }

    fn pattern_binding_semantic(
        &self,
        pattern: &ast::PatternNode,
    ) -> Result<SemanticLocalId, LowerError> {
        let site = self.source_span(pattern.span);
        if let Some(semantic) = self.facts.locals.binding_defs.get(&site).copied() {
            return Ok(semantic);
        }
        Err(LowerError::MissingBindingDef {
            body: Box::new(self.body.clone()),
            span: site,
        })
    }

    fn lower_pattern_binding(
        &mut self,
        pattern: &ast::PatternNode,
        value: Operand,
    ) -> Result<(), LowerError> {
        let semantic = self.pattern_binding_semantic(pattern)?;
        if let Some(place) = self.locals.get(&semantic).cloned() {
            return self.emit_init_place(place, RValue::Use(value));
        }
        let def = self.local_def(semantic)?;
        let name = def.name;
        let binding = def.binding_id.map(air_binding_id);
        let mutable = def.mutable;
        let source_ty = def.ty.clone();
        let ty = self.cx.lower_ty(&source_ty)?;
        let local = self.push_local(
            Some(name),
            binding,
            ty,
            if mutable {
                AirMutability::Mutable
            } else {
                AirMutability::Immutable
            },
            LocalKind::User,
        );
        let place = self.local_place(local);
        self.locals.insert(semantic, place.clone());
        self.insert_capture_source(semantic, place)?;
        self.emit_init(local, RValue::Use(value))
    }

    fn lower_pattern_alias_binding(
        &mut self,
        pattern: &ast::PatternNode,
        value: Operand,
    ) -> Result<(), LowerError> {
        let name = pattern_ident(pattern)?;
        let semantic = self.pattern_binding_semantic(pattern)?;
        let source_binding = self.local_def(semantic)?.binding_id;
        let binding = source_binding.map(air_binding_id);
        let Operand::Place(place) = value else {
            return Err(unsupported_pattern_stmt(pattern));
        };
        if let Some(root) = place.root.local()
            && place.projection.is_empty()
            && self.function.locals[root.index()].kind == LocalKind::Temp
        {
            let local = &mut self.function.locals[root.index()];
            local.name = Some(name);
            local.binding = binding;
            local.mutability = AirMutability::Mutable;
            local.kind = LocalKind::PatternBinding;
        }
        self.locals.insert(semantic, place.clone());
        self.insert_capture_source(semantic, place.clone())?;
        self.promote_pattern_alias_scoped_borrow(semantic, source_binding, &place)
    }

    fn promote_pattern_alias_scoped_borrow(
        &mut self,
        semantic: SemanticLocalId,
        binding: Option<BindingId>,
        source: &Place,
    ) -> Result<(), LowerError> {
        let Some(binding) = binding else {
            return Ok(());
        };
        let ty = source.ty;
        for (expr_id, _) in self.owned_lambdas.clone() {
            for capture in self.cx.ordered_lambda_capture_facts(expr_id)? {
                if capture.binding_id != binding
                    || !matches!(
                        capture.runtime_plan,
                        LambdaCaptureRuntimePlan::ScopedBorrow(CaptureStorageOrigin::PatternAlias)
                    )
                {
                    continue;
                }
                if !self.pattern_alias_scoped_source_supported(source) {
                    return Err(lambda_capture_gap(expr_id));
                }
                let borrow = match self.binding_scoped_borrows.get(&binding).copied() {
                    Some(borrow) => borrow,
                    None => {
                        let borrow = self.cx.program.alloc_scoped_borrow(ScopedBorrowDecl {
                            owner: self.function_id,
                            binding: air_binding_id(binding),
                            source: ScopedBorrowSource::PatternAlias {
                                source: source.clone(),
                            },
                            ty,
                            mutability: AirMutability::Mutable,
                        });
                        self.binding_scoped_borrows.insert(binding, borrow);
                        borrow
                    }
                };
                let place = self
                    .cx
                    .program
                    .scoped_borrow_place(borrow)
                    .ok_or_else(|| lambda_capture_gap(expr_id))?;
                self.locals.insert(semantic, place.clone());
                self.capture_sources
                    .insert(binding, LambdaCaptureSource::Local(place));
                return Ok(());
            }
        }
        Ok(())
    }

    fn promote_for_ref_alias_scoped_borrow(
        &mut self,
        binding: Option<BindingId>,
        source: &Place,
    ) -> Result<(), LowerError> {
        let Some(binding) = binding else {
            return Ok(());
        };
        for (expr_id, lambda) in self.owned_lambdas.clone() {
            for capture in self.cx.ordered_lambda_capture_facts(expr_id)? {
                if capture.binding_id != binding
                    || !matches!(
                        capture.runtime_plan,
                        LambdaCaptureRuntimePlan::ScopedBorrow(CaptureStorageOrigin::ForRefAlias)
                    )
                {
                    continue;
                }
                let borrow = match self.binding_scoped_borrows.get(&binding).copied() {
                    Some(borrow) => borrow,
                    None => {
                        let borrow = self.cx.program.alloc_scoped_borrow(ScopedBorrowDecl {
                            owner: self.function_id,
                            binding: air_binding_id(binding),
                            source: ScopedBorrowSource::ForRefAlias {
                                source: source.clone(),
                            },
                            ty: source.ty,
                            mutability: AirMutability::Mutable,
                        });
                        self.binding_scoped_borrows.insert(binding, borrow);
                        borrow
                    }
                };
                let place = self
                    .cx
                    .program
                    .scoped_borrow_place(borrow)
                    .ok_or_else(|| lambda_capture_gap(expr_id))?;
                self.capture_sources
                    .insert(binding, LambdaCaptureSource::Local(place));
                self.update_for_ref_alias_capture(lambda, binding, borrow, source.ty);
                return Ok(());
            }
        }
        Ok(())
    }

    fn update_for_ref_alias_capture(
        &mut self,
        lambda: LambdaId,
        binding: BindingId,
        borrow: ScopedBorrowId,
        ty: TypeId,
    ) {
        let binding = air_binding_id(binding);
        if let Some(decl) = self.cx.program.lambdas[lambda.index()]
            .captures
            .iter_mut()
            .find(|decl| decl.binding() == binding)
        {
            *decl = LambdaCaptureDecl::ScopedBorrow {
                binding,
                borrow,
                ty,
                mutability: AirMutability::Mutable,
            };
        }
    }

    fn pattern_alias_scoped_source_supported(&self, source: &Place) -> bool {
        let PlaceRoot::Local(local) = source.root else {
            return false;
        };
        self.function.locals[local.index()].kind == LocalKind::PatternBinding
            || self
                .function
                .signature
                .params
                .iter()
                .any(|param| param.local_id == local && param.mode == ParamMode::MutBorrow)
    }

    fn lower_match_effect(
        &mut self,
        expr: &ExprNode,
        match_expr: &ast::MatchNode,
    ) -> Result<(), LowerError> {
        if matches!(match_expr.node.mode, ast::MatchMode::Dynamic) {
            let plan = self.checked_dyn_match_plan(expr)?.clone();
            return self.lower_dyn_match_effect(expr, match_expr, &plan);
        }
        let plan = self.checked_match_plan(expr)?;
        if match_expr.node.access.is_ref()
            && self
                .facts
                .dyn_downcasts
                .get(&match_expr.node.scrutinee.node.id)
                .is_some_and(|fact| fact.mutable)
        {
            return self.lower_dyn_downcast_alias_match(
                expr,
                match_expr,
                &plan,
                MatchOutput::Effect,
            );
        }
        if match_expr.node.access.is_ref() && self.is_optional_expr(&match_expr.node.scrutinee)? {
            return self.lower_optional_match_effect(expr, match_expr, &plan);
        }
        self.lower_pattern_match_effect(expr, match_expr, &plan)
    }

    fn lower_stmt(&mut self, stmt: &StmtNode) -> Result<(), LowerError> {
        if matches!(
            stmt.node,
            Stmt::Const(_)
                | Stmt::TypeAlias(_)
                | Stmt::Aggregate(_)
                | Stmt::Enum(_)
                | Stmt::Func(_)
        ) {
            return Ok(());
        }
        if self.terminated {
            return Err(LowerError::UnsupportedStmt {
                kind: "statement after terminator",
                span: Some(self.source_span(stmt.span)),
            });
        }
        match &stmt.node {
            Stmt::Expr(expr) => self.lower_effect(expr),
            Stmt::Binding(binding) => self.lower_binding(binding),
            Stmt::LetElse(let_else) => self.lower_let_else(&let_else.node),
            Stmt::Return(ret) => match &ret.node.value {
                Some(value) => {
                    let value = self.lower_return_operand(value)?;
                    self.terminate(AirTail::Return(Some(value)))
                }
                None => self.terminate(AirTail::Return(None)),
            },
            Stmt::While(while_) => self.lower_while(&while_.node),
            Stmt::WhileLet(while_let) => self.lower_while_let(&while_let.node),
            Stmt::For(for_) => self.lower_for(&for_.node),
            Stmt::Break => self.lower_loop_tail(stmt, AirTail::Break),
            Stmt::Continue => self.lower_loop_tail(stmt, AirTail::Continue),
            _ => Err(LowerError::UnsupportedStmt {
                kind: stmt_kind(&stmt.node),
                span: Some(self.source_span(stmt.span)),
            }),
        }
    }

    fn lower_for(&mut self, for_: &ast::For) -> Result<(), LowerError> {
        if let Some(branch) = self.iter_for_branch_source(&for_.iterable)? {
            return self.lower_for_branch(for_, &branch);
        }
        let source = self.iter_for_source(&for_.iterable)?;
        self.lower_for_source(for_, &source)
    }

    fn lower_for_source(
        &mut self,
        for_: &ast::For,
        source: &IterForSource<'_>,
    ) -> Result<(), LowerError> {
        if let Some(parts) = self.lower_range_for_parts(source)? {
            return self.lower_range_for(for_, source, parts);
        }

        let plan = self.for_plan(for_, source)?;
        let id = self.alloc_loop();
        self.active_loops.push(id);
        let body = self.with_nested_block(|this| this.lower_for_loan_body(id, for_, &plan));
        self.active_loops.pop();
        let body = body?;
        self.ensure_open()?;
        self.block
            .stmts
            .push(AirStmt::CollectionLoan(AirCollectionLoan {
                root: plan.root.clone(),
                root_kind: plan.root_kind,
                mode: plan.mode,
                body,
            }));
        Ok(())
    }

    fn lower_for_branch(
        &mut self,
        for_: &ast::For,
        branch: &IterForBranch<'_>,
    ) -> Result<(), LowerError> {
        let cond = self.lower_if_cond(branch.cond)?;
        let then_block =
            self.with_nested_block(|this| this.lower_for_source(for_, &branch.then_source))?;
        let else_block =
            self.with_nested_block(|this| this.lower_for_source(for_, &branch.else_source))?;
        self.ensure_open()?;
        self.block.stmts.push(AirStmt::If(AirIf {
            cond,
            then_block,
            else_block: Some(else_block),
        }));
        Ok(())
    }

    fn lower_range_for_parts(
        &mut self,
        source: &IterForSource<'_>,
    ) -> Result<Option<RangeForParts>, LowerError> {
        let iterable = source.source;
        if let Some(range) = &source.range {
            let int_ty = self.cx.lower_ty(&Type::Int)?;
            return Ok(Some(RangeForParts {
                start: self.lower_int_expr(range.start, int_ty)?,
                end: self.lower_int_expr(range.end, int_ty)?,
                inclusive: range.inclusive,
            }));
        }
        let ty = self.lower_expr_ty(iterable.node.id)?;
        let Some(kind) = self
            .cx
            .decls
            .as_ref()
            .and_then(|decls| decls.core_range_kind(&ty))
        else {
            return Ok(None);
        };
        let inclusive = match kind {
            CoreRangeKind::Exclusive => false,
            CoreRangeKind::Inclusive => true,
            CoreRangeKind::From | CoreRangeKind::To | CoreRangeKind::ToInclusive => {
                return Err(LowerError::UnsupportedStmt {
                    kind: "RangeFor",
                    span: Some(self.source_span(iterable.span)),
                });
            }
        };
        let value = self.lower_value(iterable)?;
        let place = self.place_from_operand(value, iterable)?;
        let start = self.project_field(iterable, place.clone(), Ident::new(RANGE_START_FIELD))?;
        let end = self.project_field(iterable, place, Ident::new(RANGE_END_FIELD))?;
        Ok(Some(RangeForParts {
            start: Operand::Place(start),
            end: Operand::Place(end),
            inclusive,
        }))
    }

    fn lower_int_expr(&mut self, expr: &ExprNode, int_ty: TypeId) -> Result<Operand, LowerError> {
        if let ExprKind::Lit(Lit::Int(value)) = &expr.node.kind {
            return self.int_const(*value.value());
        }
        self.lower_value_to(expr, int_ty, expr)
    }

    fn lower_range_for(
        &mut self,
        for_: &ast::For,
        source: &IterForSource<'_>,
        parts: RangeForParts,
    ) -> Result<(), LowerError> {
        let int_ty = self.cx.lower_ty(&Type::Int)?;
        let ordinal_plan = self.lower_for_ordinal_plan(source)?;
        let item = self.push_local(None, None, int_ty, AirMutability::Mutable, LocalKind::Temp);
        let ordinal = (for_.bindings.len() == 2)
            .then(|| self.push_local(None, None, int_ty, AirMutability::Mutable, LocalKind::Temp));
        let id = self.alloc_loop();
        self.active_loops.push(id);
        let body = self.with_nested_block(|this| {
            this.lower_range_for_iteration_bindings(for_, ordinal, item)?;
            this.lower_block_effect(&for_.body)?;
            if !this.terminated {
                this.terminate(AirTail::Continue(id))?;
            }
            Ok(())
        });
        self.active_loops.pop();
        let body = body?;
        self.ensure_open()?;
        self.block.stmts.push(AirStmt::RangeFor(AirRangeFor {
            id,
            start: parts.start,
            end: parts.end,
            ordinal_plan,
            inclusive: parts.inclusive,
            ordinal,
            item,
            body,
        }));
        Ok(())
    }

    fn lower_range_for_iteration_bindings(
        &mut self,
        for_: &ast::For,
        ordinal: Option<LocalId>,
        item: LocalId,
    ) -> Result<(), LowerError> {
        match for_.bindings.as_slice() {
            [binding] => self.lower_for_pattern_binding(
                &binding.pattern,
                Operand::Place(self.local_place(item)),
                false,
            ),
            [index, binding] => {
                let ordinal = ordinal.ok_or_else(|| unsupported_pattern_stmt(&index.pattern))?;
                self.lower_for_pattern_binding(
                    &index.pattern,
                    Operand::Place(self.local_place(ordinal)),
                    false,
                )?;
                self.lower_for_pattern_binding(
                    &binding.pattern,
                    Operand::Place(self.local_place(item)),
                    false,
                )
            }
            _ => Err(unsupported_pattern_stmt(&for_.bindings[0].pattern)),
        }
    }

    fn for_plan(
        &mut self,
        for_: &ast::For,
        source: &IterForSource<'_>,
    ) -> Result<ForPlan, LowerError> {
        let root = self.lower_place_arg(source.source, false)?;
        let len = self.for_len_local()?;
        let int_ty = self.cx.lower_ty(&Type::Int)?;
        let index = self.push_local(None, None, int_ty, AirMutability::Mutable, LocalKind::Temp);
        let ordinal_plan = self.lower_for_ordinal_plan(source)?;
        let type_data = self.cx.program.type_data(root.ty).clone();
        match type_data {
            TypeData::List(elem) => self.sequence_for_plan(
                for_,
                root,
                AirCollectionRootKind::List,
                elem,
                len,
                index,
                ordinal_plan.clone(),
            ),
            TypeData::Array { elem, .. } => self.sequence_for_plan(
                for_,
                root,
                AirCollectionRootKind::FixedArray,
                elem,
                len,
                index,
                ordinal_plan.clone(),
            ),
            TypeData::Slice(elem) => self.sequence_for_plan(
                for_,
                root,
                AirCollectionRootKind::Slice,
                elem,
                len,
                index,
                ordinal_plan.clone(),
            ),
            TypeData::Map { key, value, .. } => self.map_for_plan(
                for_,
                source.projection,
                root,
                key,
                value,
                len,
                index,
                ordinal_plan,
            ),
            _ => Err(LowerError::UnsupportedStmt {
                kind: "For",
                span: Some(self.source_span(for_.iterable.span)),
            }),
        }
    }

    fn sequence_for_plan(
        &mut self,
        for_: &ast::For,
        root: Place,
        root_kind: AirCollectionRootKind,
        elem: TypeId,
        len: LocalId,
        index: LocalId,
        ordinal_plan: AirOrdinalPlan,
    ) -> Result<ForPlan, LowerError> {
        let mut mode = AirCollectionLoanMode::ReadonlySequence;
        let mut bindings = vec![];
        let item = match for_.bindings.as_slice() {
            [item] => item,
            [index_binding, item] => {
                if !matches!(&index_binding.pattern.node, Pattern::Wildcard) {
                    let int_ty = self.cx.lower_ty(&Type::Int)?;
                    let local = self.push_local(
                        None,
                        None,
                        int_ty,
                        AirMutability::Mutable,
                        LocalKind::Temp,
                    );
                    bindings.push(ForBindingPlan::OwnedIndex {
                        pattern: index_binding.pattern.clone(),
                        local,
                    });
                }
                item
            }
            _ => return Err(unsupported_pattern_stmt(&for_.bindings[0].pattern)),
        };
        if item.access.is_ref() {
            let local = self.push_for_slot_local(&item.pattern, elem)?;
            bindings.push(ForBindingPlan::ElementSlot {
                pattern: item.pattern.clone(),
                local,
                ty: elem,
            });
            mode = AirCollectionLoanMode::MutableSequenceElement;
        } else {
            bindings.push(ForBindingPlan::OwnedElement {
                pattern: item.pattern.clone(),
                ty: elem,
            });
        }
        Ok(ForPlan {
            root_kind,
            mode,
            root,
            len,
            index,
            ordinal_plan,
            bindings,
        })
    }

    fn map_for_plan(
        &mut self,
        for_: &ast::For,
        projection: IterForProjection,
        root: Place,
        key: TypeId,
        value: TypeId,
        len: LocalId,
        index: LocalId,
        ordinal_plan: AirOrdinalPlan,
    ) -> Result<ForPlan, LowerError> {
        let mut mode = AirCollectionLoanMode::ReadonlyMap;
        let mut bindings = vec![];
        match (projection, for_.bindings.as_slice()) {
            (IterForProjection::MapKeys, [key_binding]) if !key_binding.access.is_ref() => {
                bindings.push(ForBindingPlan::OwnedMapKey {
                    pattern: key_binding.pattern.clone(),
                    ty: key,
                });
            }
            (IterForProjection::MapValues, [value_binding]) => {
                if value_binding.access.is_ref() {
                    let local = self.push_for_slot_local(&value_binding.pattern, value)?;
                    bindings.push(ForBindingPlan::MapValueSlot {
                        pattern: value_binding.pattern.clone(),
                        local,
                        ty: value,
                    });
                    mode = AirCollectionLoanMode::MutableMapValue;
                } else {
                    bindings.push(ForBindingPlan::OwnedMapValue {
                        pattern: value_binding.pattern.clone(),
                        ty: value,
                    });
                }
            }
            (IterForProjection::Default, [entry]) if !entry.access.is_ref() => {
                bindings.push(ForBindingPlan::OwnedMapEntry {
                    pattern: entry.pattern.clone(),
                    ty: self.for_pattern_ty(&entry.pattern)?.unwrap_or_else(|| {
                        self.cx
                            .program
                            .alloc_type(TypeData::Tuple(vec![key, value]))
                    }),
                });
            }
            (IterForProjection::Default, [key_binding, value_binding]) => {
                bindings.push(ForBindingPlan::OwnedMapKey {
                    pattern: key_binding.pattern.clone(),
                    ty: key,
                });
                if value_binding.access.is_ref() {
                    let local = self.push_for_slot_local(&value_binding.pattern, value)?;
                    bindings.push(ForBindingPlan::MapValueSlot {
                        pattern: value_binding.pattern.clone(),
                        local,
                        ty: value,
                    });
                    mode = AirCollectionLoanMode::MutableMapValue;
                } else {
                    bindings.push(ForBindingPlan::OwnedMapValue {
                        pattern: value_binding.pattern.clone(),
                        ty: value,
                    });
                }
            }
            _ => return Err(unsupported_pattern_stmt(&for_.bindings[0].pattern)),
        }
        Ok(ForPlan {
            root_kind: AirCollectionRootKind::Map,
            mode,
            root,
            len,
            index,
            ordinal_plan,
            bindings,
        })
    }

    fn iter_for_source<'a>(&self, iterable: &'a ExprNode) -> Result<IterForSource<'a>, LowerError>
    where
        'facts: 'a,
    {
        self.iter_for_source_with_subst(iterable, &IterParamSubst::default(), self.facts)
    }

    fn iter_for_source_with_subst<'a>(
        &self,
        iterable: &'a ExprNode,
        subst: &IterParamSubst<'a>,
        facts: &'facts SemanticBodyFacts,
    ) -> Result<IterForSource<'a>, LowerError>
    where
        'facts: 'a,
    {
        let (current, adapters, projection) = Self::peel_iter_for_adapters(iterable, subst);
        if let ExprKind::IterSource(iter) = &current.node.kind {
            return Ok(IterForSource {
                source: subst.expr(&iter.node.source),
                range: iter_for_range_source(&iter.node.source, subst),
                adapters,
                projection,
            });
        }
        if let Some(mut helper) = self.iter_helper_source(current, subst, facts)? {
            helper.adapters.extend(adapters);
            if projection != IterForProjection::Default {
                helper.projection = projection;
            }
            return Ok(helper);
        }
        if adapters.is_empty() {
            return Ok(IterForSource {
                source: subst.expr(iterable),
                range: iter_for_range_source(iterable, subst),
                adapters,
                projection,
            });
        }
        Err(LowerError::UnsupportedStmt {
            kind: "For",
            span: None,
        })
    }

    fn peel_iter_for_adapters<'a>(
        iterable: &'a ExprNode,
        subst: &IterParamSubst<'a>,
    ) -> (&'a ExprNode, Vec<IterForAdapter<'a>>, IterForProjection) {
        let mut current = iterable;
        let mut adapters = vec![];
        let mut projection = IterForProjection::Default;
        while let ExprKind::Call(call) = &current.node.kind {
            if call.node.safe || !call.node.generic_args.is_empty() {
                break;
            }
            let ExprKind::Field(field) = &call.node.func.node.kind else {
                break;
            };
            if field.node.safe {
                break;
            }
            match (field.node.field.as_str(), call.node.args.as_slice()) {
                ("rev", []) => adapters.push(IterForAdapter::Rev),
                ("skip", [count]) => adapters.push(IterForAdapter::Skip(subst.expr(count))),
                ("take", [count]) => adapters.push(IterForAdapter::Take(subst.expr(count))),
                ("step_by", [step]) => adapters.push(IterForAdapter::StepBy(subst.expr(step))),
                ("keys", []) => projection = IterForProjection::MapKeys,
                ("values", []) => projection = IterForProjection::MapValues,
                _ => break,
            }
            current = &field.node.target;
        }
        adapters.reverse();
        (current, adapters, projection)
    }

    fn iter_for_branch_source<'a>(
        &self,
        iterable: &'a ExprNode,
    ) -> Result<Option<IterForBranch<'a>>, LowerError>
    where
        'facts: 'a,
    {
        let (current, adapters, projection) =
            Self::peel_iter_for_adapters(iterable, &IterParamSubst::default());
        let ExprKind::Call(call) = &current.node.kind else {
            return Ok(None);
        };
        let Some(helper) = self.iter_helper_callable(current.node.id, self.facts) else {
            return Ok(None);
        };
        let body = helper.callable.body();
        let Some(expr) = iter_helper_terminal_expr(body) else {
            return Ok(None);
        };
        let ExprKind::If(if_) = &expr.node.kind else {
            return Ok(None);
        };
        let subst = IterParamSubst {
            params: helper.callable.params(),
            args: call.node.args.iter().collect(),
            locals: iter_helper_locals(body),
        };
        let then_subst = subst.with_block_locals(&if_.node.then_block);
        let Some(else_block) = if_.node.else_block.as_ref() else {
            return Ok(None);
        };
        let else_subst = subst.with_block_locals(else_block);
        let Some(then_expr) = iter_helper_expr(&if_.node.then_block, &then_subst) else {
            return Ok(None);
        };
        let Some(else_expr) = iter_helper_expr(else_block, &else_subst) else {
            return Ok(None);
        };
        let mut then_source =
            self.iter_for_source_with_subst(then_expr, &then_subst, helper.facts)?;
        let mut else_source =
            self.iter_for_source_with_subst(else_expr, &else_subst, helper.facts)?;
        then_source.adapters.extend(adapters.iter().copied());
        else_source.adapters.extend(adapters);
        if projection != IterForProjection::Default {
            then_source.projection = projection;
            else_source.projection = projection;
        }
        Ok(Some(IterForBranch {
            cond: subst.expr(&if_.node.cond),
            then_source,
            else_source,
        }))
    }

    fn iter_helper_source<'a>(
        &self,
        call_expr: &'a ExprNode,
        outer: &IterParamSubst<'a>,
        facts: &'facts SemanticBodyFacts,
    ) -> Result<Option<IterForSource<'a>>, LowerError>
    where
        'facts: 'a,
    {
        let ExprKind::Call(call) = &call_expr.node.kind else {
            return Ok(None);
        };
        let Some(helper) = self.iter_helper_callable(call_expr.node.id, facts) else {
            return Ok(None);
        };
        let body = helper.callable.body();
        let subst = IterParamSubst {
            params: helper.callable.params(),
            args: call.node.args.iter().map(|arg| outer.expr(arg)).collect(),
            locals: iter_helper_locals(body),
        };
        let Some(expr) = iter_helper_expr(body, &subst) else {
            return Ok(None);
        };
        self.iter_for_source_with_subst(expr, &subst, helper.facts)
            .map(Some)
    }

    fn iter_helper_callable(
        &self,
        call_id: ExprId,
        facts: &'facts SemanticBodyFacts,
    ) -> Option<IterHelper<'facts>> {
        let target = facts.calls.get(&call_id)?;
        let key = CallableInstanceKey {
            target: target.id.clone(),
            args: target.args.clone(),
        };
        let fact = self.callable_facts.get(&key)?;
        if !fact.ret.is_iter() {
            return None;
        }
        let callable = self.index.callables.get(&target.id).copied()?;
        let facts = self.body_facts.get(&fact.body)?;
        Some(IterHelper { callable, facts })
    }

    fn lower_for_ordinal_plan(
        &mut self,
        source: &IterForSource<'_>,
    ) -> Result<AirOrdinalPlan, LowerError> {
        let mut adapters = vec![];
        for adapter in &source.adapters {
            match adapter {
                IterForAdapter::Rev => adapters.push(AirOrdinalAdapter::Rev),
                IterForAdapter::Skip(count) => adapters.push(AirOrdinalAdapter::Skip {
                    count: self.lower_iter_count(count, IterCountCheck::SkipNonNegative)?,
                }),
                IterForAdapter::Take(count) => adapters.push(AirOrdinalAdapter::Take {
                    count: self.lower_iter_count(count, IterCountCheck::TakeNonNegative)?,
                }),
                IterForAdapter::StepBy(step) => adapters.push(AirOrdinalAdapter::StepBy {
                    step: self.lower_iter_count(step, IterCountCheck::StepByPositive)?,
                }),
            }
        }
        Ok(AirOrdinalPlan { adapters })
    }

    fn lower_iter_count(
        &mut self,
        expr: &ExprNode,
        default_check: IterCountCheck,
    ) -> Result<Operand, LowerError> {
        if let ExprKind::Lit(Lit::Int(value)) = &expr.node.kind
            && iter_count_check_accepts(default_check, *value.value())
        {
            return self.int_const(*value.value());
        }
        let int_ty = self.cx.lower_ty(&Type::Int)?;
        let value = self.lower_value_to(expr, int_ty, expr)?;
        let check = match self.cx.iter_runtime_check(expr.node.id) {
            Some(IterRuntimeCheckKind::SkipNonNegative) => IterCountCheck::SkipNonNegative,
            Some(IterRuntimeCheckKind::TakeNonNegative) => IterCountCheck::TakeNonNegative,
            Some(IterRuntimeCheckKind::StepByPositive) => IterCountCheck::StepByPositive,
            None => default_check,
        };
        self.emit_typed_temp(
            int_ty,
            RValue::CheckedIterCount {
                count: value,
                check,
            },
        )
    }

    fn for_len_local(&mut self) -> Result<LocalId, LowerError> {
        let int_ty = self.cx.lower_ty(&Type::Int)?;
        Ok(self.push_local(
            None,
            None,
            int_ty,
            AirMutability::Immutable,
            LocalKind::Temp,
        ))
    }

    fn lower_for_loan_body(
        &mut self,
        id: AirLoopId,
        for_: &ast::For,
        plan: &ForPlan,
    ) -> Result<(), LowerError> {
        self.emit_init(
            plan.len,
            RValue::Len {
                source: plan.root.clone(),
            },
        )?;
        let body = self.with_nested_block(|this| this.lower_for_iteration_scope(id, for_, plan))?;
        self.ensure_open()?;
        self.block
            .stmts
            .push(AirStmt::CollectionFor(AirCollectionFor {
                id,
                len: plan.len,
                ordinal_plan: plan.ordinal_plan.clone(),
                index: plan.index,
                ordinal: plan.ordinal(),
                body,
            }));
        Ok(())
    }

    fn lower_for_iteration_scope(
        &mut self,
        id: AirLoopId,
        for_: &ast::For,
        plan: &ForPlan,
    ) -> Result<(), LowerError> {
        let slots = Self::active_for_slots(plan);
        if slots.is_empty() {
            return self.lower_for_iteration_body(id, for_, plan);
        }
        let body = self.with_nested_block(|this| this.lower_for_iteration_body(id, for_, plan))?;
        self.ensure_open()?;
        self.block
            .stmts
            .push(AirStmt::CollectionSlotScope(AirCollectionSlotScope {
                root: plan.root.clone(),
                index: plan.index,
                slots,
                body,
            }));
        Ok(())
    }

    fn lower_for_iteration_body(
        &mut self,
        id: AirLoopId,
        for_: &ast::For,
        plan: &ForPlan,
    ) -> Result<(), LowerError> {
        self.lower_for_iteration_bindings(plan)?;
        self.lower_block_effect(&for_.body)?;
        if !self.terminated {
            self.terminate(AirTail::Continue(id))?;
        }
        Ok(())
    }

    fn active_for_slots(plan: &ForPlan) -> Vec<AirCollectionSlot> {
        plan.bindings
            .iter()
            .filter_map(|binding| match binding {
                ForBindingPlan::ElementSlot { local, ty, .. } => Some(AirCollectionSlot {
                    kind: AirCollectionSlotKind::SequenceElement,
                    local: *local,
                    ty: *ty,
                    mutable: true,
                }),
                ForBindingPlan::MapValueSlot { local, ty, .. } => Some(AirCollectionSlot {
                    kind: AirCollectionSlotKind::MapValue,
                    local: *local,
                    ty: *ty,
                    mutable: true,
                }),
                _ => None,
            })
            .collect()
    }

    fn lower_for_iteration_bindings(&mut self, plan: &ForPlan) -> Result<(), LowerError> {
        let mut map_entry = None;
        for binding in &plan.bindings {
            match binding {
                ForBindingPlan::OwnedIndex { pattern, local } => self.lower_for_pattern_binding(
                    pattern,
                    Operand::Place(self.local_place(*local)),
                    false,
                )?,
                ForBindingPlan::OwnedElement { pattern, ty } => {
                    let place = Self::sequence_element_place(&plan.root, plan.index, *ty);
                    self.lower_for_pattern_binding(pattern, Operand::Place(place), false)?;
                }
                ForBindingPlan::ElementSlot { pattern, .. }
                | ForBindingPlan::MapValueSlot { pattern, .. } => {
                    if !matches!(pattern.node, Pattern::Ident(_) | Pattern::Wildcard) {
                        return Err(unsupported_pattern_stmt(pattern));
                    }
                }
                ForBindingPlan::OwnedMapEntry { pattern, ty } => {
                    if !matches!(pattern.node, Pattern::Wildcard) {
                        let entry = self.map_entry_operand(plan, &mut map_entry, |_, _| Ok(*ty))?;
                        self.lower_for_pattern_binding(pattern, entry, false)?;
                    }
                }
                ForBindingPlan::OwnedMapKey { pattern, ty } => {
                    if !matches!(pattern.node, Pattern::Wildcard) {
                        let key = self.map_key_operand(plan, *ty)?;
                        self.lower_for_pattern_binding(pattern, key, false)?;
                    }
                }
                ForBindingPlan::OwnedMapValue { pattern, ty } => {
                    if !matches!(pattern.node, Pattern::Wildcard) {
                        let value = self.map_value_operand(plan, *ty)?;
                        self.lower_for_pattern_binding(pattern, value, false)?;
                    }
                }
            }
        }
        Ok(())
    }

    fn sequence_element_place(root: &Place, index: LocalId, ty: TypeId) -> Place {
        let mut place = root.clone();
        place.projection.push(crate::air::Projection::Index(index));
        place.ty = ty;
        place
    }

    fn for_pattern_ty(&mut self, pattern: &ast::PatternNode) -> Result<Option<TypeId>, LowerError> {
        if !matches!(pattern.node, Pattern::Ident(_)) {
            return Ok(None);
        }
        let semantic = self.pattern_binding_semantic(pattern)?;
        let source_ty = self.local_def(semantic)?.ty.clone();
        self.cx.lower_ty(&source_ty).map(Some)
    }

    fn map_entry_operand(
        &mut self,
        plan: &ForPlan,
        entry: &mut Option<Operand>,
        ty: impl FnOnce(&mut Self, &ForPlan) -> Result<TypeId, LowerError>,
    ) -> Result<Operand, LowerError> {
        if let Some(entry) = entry {
            return Ok(entry.clone());
        }
        let ty = ty(self, plan)?;
        let value = self.emit_typed_temp(
            ty,
            RValue::MapEntryAt {
                map: plan.root.clone(),
                index: plan.index,
                ty,
            },
        )?;
        *entry = Some(value.clone());
        Ok(value)
    }

    fn map_key_operand(&mut self, plan: &ForPlan, ty: TypeId) -> Result<Operand, LowerError> {
        self.emit_typed_temp(
            ty,
            RValue::MapKeyAt {
                map: plan.root.clone(),
                index: plan.index,
                ty,
            },
        )
    }

    fn map_value_operand(&mut self, plan: &ForPlan, ty: TypeId) -> Result<Operand, LowerError> {
        self.emit_typed_temp(
            ty,
            RValue::MapValueAt {
                map: plan.root.clone(),
                index: plan.index,
                ty,
            },
        )
    }

    fn tuple_field_operand(tuple: Operand, index: u32, ty: TypeId) -> Operand {
        let mut place = match tuple {
            Operand::Place(place) => place,
            Operand::Const(_) => unreachable!("map entries are emitted as places"),
        };
        place
            .projection
            .push(crate::air::Projection::TupleField(index));
        place.ty = ty;
        Operand::Place(place)
    }

    fn lower_for_pattern_binding(
        &mut self,
        pattern: &ast::PatternNode,
        value: Operand,
        alias: bool,
    ) -> Result<(), LowerError> {
        self.lower_for_pattern_binding_at(pattern, value, alias, None)
    }

    fn lower_for_pattern_binding_at(
        &mut self,
        pattern: &ast::PatternNode,
        value: Operand,
        alias: bool,
        extern_site: Option<ExprId>,
    ) -> Result<(), LowerError> {
        match &pattern.node {
            Pattern::Wildcard => Ok(()),
            Pattern::Ident(_) if alias => self.lower_pattern_alias_binding(pattern, value),
            Pattern::Ident(_) => {
                let local = self.push_for_owned_local(pattern)?;
                self.emit_init(local, RValue::Use(value))
            }
            Pattern::Tuple(items) => {
                let place = self.pattern_operand_place(value)?;
                let TypeData::Tuple(types) = self.cx.program.type_data(place.ty) else {
                    return Err(unsupported_pattern_stmt(pattern));
                };
                if items.len() != types.len() {
                    return Err(unsupported_pattern_stmt(pattern));
                }
                let types = types.clone();
                for (index, item) in items.iter().enumerate() {
                    let mut field = place.clone();
                    field
                        .projection
                        .push(crate::air::Projection::TupleField(index as u32));
                    field.ty = types[index];
                    self.lower_for_pattern_binding_at(
                        item,
                        Operand::Place(field),
                        alias,
                        extern_site,
                    )?;
                }
                Ok(())
            }
            Pattern::Struct { fields, .. } => {
                let place = self.pattern_operand_place(value)?;
                for (name, item) in fields {
                    if alias && matches!(item.node, Pattern::Wildcard) {
                        continue;
                    }
                    let value =
                        self.lower_pattern_field(pattern, extern_site, place.clone(), *name)?;
                    self.lower_for_pattern_binding_at(item, value, alias, extern_site)?;
                }
                Ok(())
            }
            _ => Err(unsupported_pattern_stmt(pattern)),
        }
    }

    fn lower_pattern_field(
        &mut self,
        pattern: &ast::PatternNode,
        extern_site: Option<ExprId>,
        place: Place,
        name: Ident,
    ) -> Result<Operand, LowerError> {
        if let TypeData::Extern(owner) = self.cx.program.type_data(place.ty) {
            return self.lower_extern_pattern_field(pattern, extern_site, place, *owner, name);
        }
        let Some((field, ty)) = typing::field_by_name(&self.cx.program, place.ty, name) else {
            return Err(unsupported_pattern_stmt(pattern));
        };
        let mut field_place = place;
        field_place
            .projection
            .push(crate::air::Projection::Field(field));
        field_place.ty = ty;
        Ok(Operand::Place(field_place))
    }

    fn pattern_operand_place(&mut self, value: Operand) -> Result<Place, LowerError> {
        match value {
            Operand::Place(place) => Ok(place),
            Operand::Const(_) => {
                let ty = self.operand_ty(&value);
                match self.emit_typed_temp(ty, RValue::Use(value))? {
                    Operand::Place(place) => Ok(place),
                    Operand::Const(_) => Err(LowerError::UnsupportedStmt {
                        kind: "pattern",
                        span: None,
                    }),
                }
            }
        }
    }

    fn push_for_owned_local(&mut self, pattern: &ast::PatternNode) -> Result<LocalId, LowerError> {
        let semantic = self.pattern_binding_semantic(pattern)?;
        let def = self.local_def(semantic)?;
        if let Some(local) = self.existing_semantic_local(semantic) {
            return Ok(local);
        }
        let name = def.name;
        let binding = def.binding_id.map(air_binding_id);
        let source_ty = def.ty.clone();
        let mutable = def.mutable;
        let ty = self.cx.lower_ty(&source_ty)?;
        let local = self.push_local(
            Some(name),
            binding,
            ty,
            if mutable {
                AirMutability::Mutable
            } else {
                AirMutability::Immutable
            },
            LocalKind::PatternBinding,
        );
        let place = self.local_place(local);
        self.locals.insert(semantic, place.clone());
        self.insert_capture_source(semantic, place)?;
        Ok(local)
    }

    fn push_for_slot_local(
        &mut self,
        pattern: &ast::PatternNode,
        ty: TypeId,
    ) -> Result<LocalId, LowerError> {
        if let Pattern::Ident(_) = &pattern.node {
            let semantic = self.pattern_binding_semantic(pattern)?;
            let def = self.local_def(semantic)?;
            if let Some(local) = self.existing_semantic_local(semantic) {
                return Ok(local);
            }
            let name = def.name;
            let binding = def.binding_id;
            let local = self.push_local(
                Some(name),
                binding.map(air_binding_id),
                ty,
                AirMutability::Mutable,
                LocalKind::PatternBinding,
            );
            let place = self.local_place(local);
            self.locals.insert(semantic, place.clone());
            self.insert_capture_source(semantic, place.clone())?;
            self.promote_for_ref_alias_scoped_borrow(binding, &place)?;
            return Ok(local);
        }
        Ok(self.push_local(
            None,
            None,
            ty,
            AirMutability::Mutable,
            LocalKind::PatternBinding,
        ))
    }

    fn int_const(&mut self, value: i64) -> Result<Operand, LowerError> {
        let ty = self.cx.lower_ty(&Type::Int)?;
        Ok(Operand::Const(self.cx.program.alloc_const(ConstData {
            ty,
            value: ConstValue::Int(value),
        })))
    }

    fn lower_while(&mut self, while_: &ast::While) -> Result<(), LowerError> {
        let id = self.alloc_loop();
        self.active_loops.push(id);
        let body = self.with_nested_block(|this| this.lower_while_body(id, while_));
        self.active_loops.pop();
        let body = body?;
        self.ensure_open()?;
        self.block.stmts.push(AirStmt::Loop(AirLoop { id, body }));
        Ok(())
    }

    fn lower_while_body(&mut self, id: AirLoopId, while_: &ast::While) -> Result<(), LowerError> {
        let cond = self.lower_while_cond(&while_.cond)?;
        let mut then_block = self.lower_nested_effect(&while_.body)?;
        if air_block_falls_through(&then_block) {
            then_block.tail = AirTail::Continue(id);
        }
        let else_block = AirBlock {
            stmts: vec![],
            tail: AirTail::Break(id),
        };
        self.ensure_open()?;
        self.block.stmts.push(AirStmt::If(AirIf {
            cond,
            then_block,
            else_block: Some(else_block),
        }));
        Ok(())
    }

    fn lower_while_let(&mut self, while_let: &ast::WhileLet) -> Result<(), LowerError> {
        let id = self.alloc_loop();
        self.active_loops.push(id);
        let body = self.with_nested_block(|this| this.lower_while_let_body(id, while_let));
        self.active_loops.pop();
        let body = body?;
        self.ensure_open()?;
        self.block.stmts.push(AirStmt::Loop(AirLoop { id, body }));
        Ok(())
    }

    fn lower_while_let_body(
        &mut self,
        id: AirLoopId,
        while_let: &ast::WhileLet,
    ) -> Result<(), LowerError> {
        if let Some(pattern) = self
            .flag_conditional_pattern(while_let.value.node.id)
            .cloned()
        {
            let subject = self.lower_pattern_subject(
                &while_let.value,
                &while_let.value,
                CheckedMatchAccess::Owned,
            )?;
            let matched =
                self.with_nested_block(|this| this.lower_loop_body_continue(id, &while_let.body))?;
            let fallback = AirBlock {
                stmts: vec![],
                tail: AirTail::Break(id),
            };
            let arms =
                self.lower_flag_conditional_arms(&while_let.value, &pattern, matched, fallback)?;
            return self.push_pattern_match(subject, arms);
        }
        let alias = while_let.head.is_ref();
        let subject =
            self.lower_optional_pattern_subject(&while_let.value, &while_let.value, alias)?;
        match classify_optional_pattern(&while_let.pattern)? {
            OptionalPattern::Some(pattern) => {
                let mode = optional_payload_mode(pattern, alias);
                let payload = mode.needs_payload().then(|| self.temp(subject.inner_ty()));
                self.emit_optional_match_with_payload_ref(
                    subject,
                    payload,
                    mode.payload_ref(),
                    |this, payload| {
                        this.lower_optional_payload_binding(pattern, payload, alias)?;
                        this.lower_loop_body_continue(id, &while_let.body)
                    },
                    |this| this.terminate(AirTail::Break(id)),
                )
            }
            OptionalPattern::None => self.emit_optional_match(
                subject,
                None,
                |this, _| this.terminate(AirTail::Break(id)),
                |this| this.lower_loop_body_continue(id, &while_let.body),
            ),
        }
    }

    fn lower_loop_body_continue(
        &mut self,
        id: AirLoopId,
        body: &BlockNode,
    ) -> Result<(), LowerError> {
        self.lower_block_effect(body)?;
        if !self.terminated {
            self.terminate(AirTail::Continue(id))?;
        }
        Ok(())
    }

    fn lower_while_cond(&mut self, expr: &ExprNode) -> Result<Operand, LowerError> {
        let cond = self.lower_value(expr)?;
        if self.terminated || self.operand_type(&cond) != Type::Bool {
            return Err(unsupported_expr(expr));
        }
        let bool_ty = self.cx.lower_ty(&Type::Bool)?;
        self.emit_typed_temp(bool_ty, RValue::Use(cond))
    }

    fn lower_loop_tail(
        &mut self,
        stmt: &StmtNode,
        tail: fn(AirLoopId) -> AirTail,
    ) -> Result<(), LowerError> {
        let Some(id) = self.active_loops.last().copied() else {
            return Err(LowerError::UnsupportedStmt {
                kind: stmt_kind(&stmt.node),
                span: Some(self.source_span(stmt.span)),
            });
        };
        self.terminate(tail(id))
    }

    fn alloc_loop(&mut self) -> AirLoopId {
        let id = AirLoopId(self.next_loop);
        self.next_loop += 1;
        id
    }

    fn lower_binding(&mut self, binding: &ast::BindingNode) -> Result<(), LowerError> {
        match &binding.node.pattern.node {
            Pattern::Ident(_) => {
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
                let existing = self.locals.get(&semantic).cloned();
                let def = self.local_def(semantic)?;
                let name = def.name;
                let semantic_binding = def.binding_id;
                let binding_id = semantic_binding.map(air_binding_id);
                let mutable = def.mutable;
                let ty = def.ty.clone();
                let ty = self.cx.lower_ty(&ty)?;
                let init = match self.lower_binding_string_init(&binding.node.value)? {
                    Some(value) => value,
                    None => RValue::Use(self.lower_value_to(
                        &binding.node.value,
                        ty,
                        &binding.node.value,
                    )?),
                };
                if let Some(place) = existing {
                    return self.emit_init_place(place, init);
                }
                let local = self.push_local(
                    Some(name),
                    binding_id,
                    ty,
                    if mutable {
                        AirMutability::Mutable
                    } else {
                        AirMutability::Immutable
                    },
                    LocalKind::User,
                );
                let place = match semantic_binding {
                    Some(binding) if self.cx.binding_requires_capture_cell(binding) => {
                        let cell = self.alloc_binding_cell(binding, local, ty)?;
                        let place = capture_cell_place(cell, ty);
                        self.emit_assign(place.clone(), init)?;
                        place
                    }
                    _ => {
                        self.emit_init(local, init)?;
                        self.local_place(local)
                    }
                };
                self.locals.insert(semantic, place.clone());
                self.insert_capture_source(semantic, place)
            }
            Pattern::Tuple(_) | Pattern::Struct { .. } => {
                let value = Operand::Place(self.lower_place_or_temp(&binding.node.value, false)?);
                self.lower_for_pattern_binding_at(
                    &binding.node.pattern,
                    value,
                    false,
                    Some(binding.node.value.node.id),
                )
            }
            Pattern::Wildcard => self.lower_effect(&binding.node.value),
            _ => Err(LowerError::UnsupportedStmt {
                kind: binding.node.pattern.node.variant_name(),
                span: Some(self.source_span(binding.span)),
            }),
        }
    }

    fn lower_place_arg(
        &mut self,
        expr: &ExprNode,
        requires_mut: bool,
    ) -> Result<Place, LowerError> {
        self.lower_place_arg_impl(expr, requires_mut, false)
    }

    fn lower_mut_call_arg(&mut self, expr: &ExprNode) -> Result<Place, LowerError> {
        self.lower_place_or_temp(expr, true)
    }

    fn lower_mut_slice_call_arg(
        &mut self,
        expr: &ExprNode,
        slice_ty: TypeId,
    ) -> Result<Place, LowerError> {
        let source = self.lower_place_or_temp(expr, true)?;
        self.lower_slice_view_call_arg(source, slice_ty, true, expr)
    }

    fn lower_place_or_temp(&mut self, expr: &ExprNode, mutable: bool) -> Result<Place, LowerError> {
        match self.lower_place_arg_impl(expr, mutable, mutable) {
            Ok(place) => Ok(place),
            Err(err) if !mutable => {
                let value = self.lower_value(expr).map_err(|_| err)?;
                self.place_from_operand(value, expr)
            }
            Err(err) => Err(err),
        }
    }

    fn lower_slice_view_call_arg(
        &mut self,
        source: Place,
        slice_ty: TypeId,
        mutable: bool,
        site: &ExprNode,
    ) -> Result<Place, LowerError> {
        let needs_view = source.ty != slice_ty;
        if needs_view {
            self.check_slice_view_source(site, &source, slice_ty)?;
        }
        if mutable {
            self.require_mutable_place(site, &source)?;
        }
        if !needs_view {
            return Ok(source);
        }
        let start = self.int_local(0)?;
        let end = self.len_local(&source)?;
        self.emit_slice_view_temp(slice_ty, source, start, end, false, mutable, site)
    }

    fn check_slice_view_source(
        &self,
        site: &ExprNode,
        source: &Place,
        slice_ty: TypeId,
    ) -> Result<(), LowerError> {
        let Some(source_elem) = typing::sequence_elem(&self.cx.program, source.ty) else {
            return Err(unsupported_expr(site));
        };
        let TypeData::Slice(slice_elem) = self.cx.program.type_data(slice_ty) else {
            return Err(unsupported_expr(site));
        };
        if source_elem != *slice_elem {
            return Err(unsupported_expr(site));
        }
        Ok(())
    }

    fn emit_slice_view_temp(
        &mut self,
        ty: TypeId,
        source: Place,
        start: LocalId,
        end: LocalId,
        inclusive: bool,
        mutable: bool,
        site: &ExprNode,
    ) -> Result<Place, LowerError> {
        let value = RValue::SliceView {
            source,
            start,
            end,
            inclusive,
            ty,
        };
        let temp = if mutable {
            self.emit_mut_typed_temp(ty, value)?
        } else {
            self.emit_typed_temp(ty, value)?
        };
        self.place_from_operand(temp, site)
    }

    fn lower_place_arg_impl(
        &mut self,
        expr: &ExprNode,
        requires_mut: bool,
        allow_promoted_root: bool,
    ) -> Result<Place, LowerError> {
        self.lower_place_arg_impl_with_fallback(expr, requires_mut, allow_promoted_root, true)
    }

    fn lower_place_arg_impl_with_fallback(
        &mut self,
        expr: &ExprNode,
        requires_mut: bool,
        allow_promoted_root: bool,
        allow_named_fallback: bool,
    ) -> Result<Place, LowerError> {
        if let Some(fact) = self.global_access(expr.node.id).cloned() {
            let valid = if requires_mut {
                matches!(
                    fact.mode,
                    GlobalAccessMode::RefArgument
                        | GlobalAccessMode::MutableBorrow
                        | GlobalAccessMode::MutReceiver
                )
            } else {
                matches!(
                    fact.mode,
                    GlobalAccessMode::ImmutableBorrow | GlobalAccessMode::Read
                )
            };
            if !valid {
                return Err(unsupported_expr(expr));
            }
            return self.lower_global_projected_place(expr, &fact);
        }
        let fact = if requires_mut {
            match self
                .local_use(expr, LocalUseMode::RefArgument)
                .or_else(|_| self.local_use(expr, LocalUseMode::MutBorrow))
            {
                Ok(fact) => fact,
                Err(LowerError::MissingLocalUse { .. }) => {
                    return self.lower_mut_place_from_read_fact(
                        expr,
                        allow_promoted_root,
                        allow_named_fallback,
                    );
                }
                Err(err) => return Err(err),
            }
        } else {
            match self.local_use(expr, LocalUseMode::Borrow) {
                Ok(fact) => fact,
                Err(_) => match self.local_use(expr, LocalUseMode::Read) {
                    Ok(fact) => fact,
                    Err(_) => return self.lower_shared_projected_place_arg(expr),
                },
            }
        };
        let root = self.binding_place(&fact)?;
        if requires_mut {
            return self.lower_mut_source_place(expr, root, allow_promoted_root);
        }
        self.lower_projected_place(expr, root)
    }

    fn lower_mut_place_from_read_fact(
        &mut self,
        expr: &ExprNode,
        allow_promoted_root: bool,
        allow_named_fallback: bool,
    ) -> Result<Place, LowerError> {
        let root = projection_root(expr).unwrap_or(expr);
        if let Ok(fact) = self.local_use(root, LocalUseMode::Read) {
            let root = self.binding_place(&fact)?;
            return self.lower_mut_source_place(expr, root, allow_promoted_root);
        }
        self.lower_self_mut_place_arg(expr).or_else(|err| {
            if allow_named_fallback {
                self.lower_unique_named_mut_place_arg(expr)
            } else {
                Err(err)
            }
        })
    }

    fn lower_mut_source_place(
        &mut self,
        expr: &ExprNode,
        root: Place,
        allow_promoted_root: bool,
    ) -> Result<Place, LowerError> {
        if self.place_is_capture_cell(&root) || self.place_is_scoped_borrow(&root) {
            return self.lower_promoted_mut_arg(expr, root, allow_promoted_root);
        }
        let root_local = match root.root {
            PlaceRoot::Local(local) => local,
            PlaceRoot::DynBorrowParam(id) => self
                .cx
                .program
                .dyn_borrow_params
                .get(id.index())
                .filter(|decl| decl.owner == self.function_id)
                .map(|decl| decl.source)
                .ok_or_else(|| unsupported_expr(expr))?,
            _ => return Err(unsupported_expr(expr)),
        };
        if self.function.locals[root_local.index()].mutability != AirMutability::Mutable {
            return Err(unsupported_expr(expr));
        }
        self.lower_projected_place(expr, root)
    }

    fn lower_promoted_mut_arg(
        &mut self,
        expr: &ExprNode,
        root: Place,
        allowed: bool,
    ) -> Result<Place, LowerError> {
        if !allowed {
            return Err(lambda_capture_gap(expr.node.id));
        }
        if matches!(expr.node.kind, ExprKind::Ident(_)) {
            return Ok(root);
        }
        if self.promoted_dataref_projection_root(&root)
            || self.place_is_capture_cell(&root)
            || self.place_is_scoped_borrow(&root)
        {
            return self.lower_projected_place(expr, root);
        }
        Err(mutable_place_projection_gap(expr.node.id))
    }

    fn lower_unique_named_mut_place_arg(&mut self, expr: &ExprNode) -> Result<Place, LowerError> {
        let root = projection_root(expr).unwrap_or(expr);
        let ExprKind::Ident(name) = &root.node.kind else {
            return Err(unsupported_expr(expr));
        };
        let mut matches = self
            .function
            .locals
            .iter()
            .enumerate()
            .filter(|(_, local)| local.name == Some(*name));
        let Some((index, local)) = matches.next() else {
            return Err(unsupported_expr(expr));
        };
        if matches.next().is_some() || local.mutability != AirMutability::Mutable {
            return Err(unsupported_expr(expr));
        }
        if self.cx.lower_ty(&self.lower_expr_ty(root.node.id)?)? != local.ty {
            return Err(unsupported_expr(expr));
        }
        self.lower_projected_place(
            expr,
            Place {
                root: PlaceRoot::Local(LocalId::from_index(index)),
                projection: vec![],
                ty: local.ty,
            },
        )
    }

    fn lower_self_mut_place_arg(&mut self, expr: &ExprNode) -> Result<Place, LowerError> {
        let root = projection_root(expr).unwrap_or(expr);
        if !matches!(&root.node.kind, ExprKind::Ident(name) if name.as_str() == "self") {
            return Err(LowerError::MissingLocalUse {
                body: Box::new(self.body.clone()),
                expr_id: expr.node.id,
            });
        }
        let place = self.self_place(expr)?;
        let Some(root_local) = place.root.local() else {
            return Err(unsupported_expr(expr));
        };
        if self.function.locals[root_local.index()].mutability != AirMutability::Mutable {
            return Err(unsupported_expr(expr));
        }
        self.lower_projected_place(expr, place)
    }

    fn lower_shared_projected_place_arg(&mut self, expr: &ExprNode) -> Result<Place, LowerError> {
        if projection_root(expr).is_none() {
            return self.materialize_shared_borrow_arg(expr);
        }
        self.lower_projected_read_place(expr)
    }

    fn lower_projected_read_place(&mut self, expr: &ExprNode) -> Result<Place, LowerError> {
        let Some(root) = projection_root(expr) else {
            return Err(unsupported_expr(expr));
        };
        if let Ok(fact) = self.local_use(root, LocalUseMode::Read) {
            return self.lower_projected_place(expr, self.binding_place(&fact)?);
        }
        if let Some(fact) = self.global_access(expr.node.id).cloned() {
            return self.lower_global_projected_place(expr, &fact);
        }
        if matches!(&root.node.kind, ExprKind::Ident(name) if name.as_str() == "self") {
            return self.lower_projected_place(expr, self.self_place(expr)?);
        }
        Err(LowerError::MissingLocalUse {
            body: Box::new(self.body.clone()),
            expr_id: root.node.id,
        })
    }

    fn global_access(&self, expr_id: ExprId) -> Option<&GlobalAccessFact> {
        self.facts.global_accesses.get(&expr_id)
    }

    fn lowered_global(&self, key: &GlobalKey) -> Result<GlobalId, LowerError> {
        self.cx
            .maps
            .globals
            .get(key)
            .copied()
            .ok_or_else(|| LowerError::MissingLoweredGlobal {
                key: Box::new(key.clone()),
            })
    }

    fn global_root_place(&self, fact: &GlobalAccessFact) -> Result<Place, LowerError> {
        let global = self.lowered_global(&fact.key)?;
        let ty = self.cx.program.globals[global.index()].ty;
        Ok(Place {
            root: PlaceRoot::Global(global),
            projection: vec![],
            ty,
        })
    }

    fn lower_global_projected_place(
        &mut self,
        expr: &ExprNode,
        fact: &GlobalAccessFact,
    ) -> Result<Place, LowerError> {
        self.emit_global_ensure_for_place(fact)?;
        let root = self.global_root_place(fact)?;
        self.lower_projected_place_from(expr, Some(fact.root_expr_id), root)
    }

    fn emit_global_ensure_for_place(&mut self, fact: &GlobalAccessFact) -> Result<(), LowerError> {
        if fact.init_effect == TcGlobalInitEffect::InitializeFirst {
            self.emit_global_ensure(self.lowered_global(&fact.key)?)?;
        }
        Ok(())
    }

    fn self_place(&self, expr: &ExprNode) -> Result<Place, LowerError> {
        let Some(param) = self.function.signature.params.iter().find(|param| {
            param.role == ParamRole::Receiver || param.name == Some(Ident::new("self"))
        }) else {
            return Err(unsupported_expr(expr));
        };
        Ok(self.local_place(param.local_id))
    }

    fn lower_shared_slice_call_arg(
        &mut self,
        expr: &ExprNode,
        slice_ty: TypeId,
    ) -> Result<Place, LowerError> {
        let source = self.lower_place_or_temp(expr, false)?;
        self.lower_slice_view_call_arg(source, slice_ty, false, expr)
    }

    fn lower_shared_call_arg(
        &mut self,
        expr: &ExprNode,
        ty: TypeId,
    ) -> Result<CallArg, LowerError> {
        if typing::optional_inner(&self.cx.program, ty).is_some() {
            let value = self.lower_value_to(expr, ty, expr)?;
            return self
                .materialize_shared_operand(expr, value, ty)
                .map(CallArg::SharedBorrow);
        }
        if self.facts.const_values.contains_key(&expr.node.id)
            || matches!(expr.node.kind, ExprKind::Lit(Lit::String(_)))
        {
            let value = self.lower_value(expr)?;
            return self.lower_operand_call_arg(
                value,
                ParamType {
                    ty,
                    mode: ParamMode::SharedBorrow,
                    escape: ParamEscape::NonEscaping,
                },
                expr,
            );
        }
        if matches!(self.cx.program.type_data(ty), TypeData::Slice(_))
            && let Ok(place) = self.lower_shared_slice_call_arg(expr, ty)
        {
            return Ok(CallArg::SharedBorrow(place));
        }
        if self.facts.raw_projections.contains_key(&expr.node.id) {
            let value = self.lower_value_to(expr, ty, expr)?;
            return self
                .materialize_shared_operand(expr, value, ty)
                .map(CallArg::SharedBorrow);
        }
        match self.lower_place_arg(expr, false) {
            Ok(place) if place.ty == ty => Ok(CallArg::SharedBorrow(place)),
            Ok(_) => Err(unsupported_expr(expr)),
            Err(err) => {
                let value = self.lower_value_to(expr, ty, expr).map_err(|_| err)?;
                self.materialize_shared_operand(expr, value, ty)
                    .map(CallArg::SharedBorrow)
            }
        }
    }

    fn materialize_shared_operand(
        &mut self,
        expr: &ExprNode,
        value: Operand,
        ty: TypeId,
    ) -> Result<Place, LowerError> {
        match self.emit_typed_temp(ty, RValue::Use(value))? {
            Operand::Place(place) => Ok(place),
            Operand::Const(_) => Err(unsupported_expr(expr)),
        }
    }

    fn const_is_string(&self, id: ConstId) -> bool {
        let primitives = PrimitiveTypes::scan(&self.cx.program);
        typing::const_is_string(&self.cx.program, &primitives, id)
    }

    fn materialize_shared_borrow_arg(&mut self, expr: &ExprNode) -> Result<Place, LowerError> {
        let value = self.lower_value(expr)?;
        match value {
            Operand::Place(place) => Ok(place),
            Operand::Const(value) => {
                let value = Operand::Const(value);
                let ty = self.cx.lower_ty(&self.operand_type(&value))?;
                match self.emit_typed_temp(ty, RValue::Use(value))? {
                    Operand::Place(place) => Ok(place),
                    Operand::Const(_) => Err(unsupported_expr(expr)),
                }
            }
        }
    }

    fn lower_projected_place(
        &mut self,
        expr: &ExprNode,
        mut root: Place,
    ) -> Result<Place, LowerError> {
        if let Some(fact) = self.facts.member_paths.get(&expr.node.id).cloned()
            && fact.kind == MemberPathKind::MethodReceiver
        {
            for field in fact.path {
                root = self.project_field(expr, root, field)?;
            }
            return Ok(root);
        }
        self.lower_projected_place_from(expr, None, root)
    }

    fn lower_projected_place_from(
        &mut self,
        expr: &ExprNode,
        root_expr: Option<ExprId>,
        root: Place,
    ) -> Result<Place, LowerError> {
        if root_expr == Some(expr.node.id) {
            return Ok(root);
        }
        match &expr.node.kind {
            ExprKind::Ident(_) => Ok(root),
            ExprKind::Field(field) => {
                if field.node.safe {
                    return Err(unsupported_expr(expr));
                }
                let place = self.lower_projected_place_from(&field.node.target, root_expr, root)?;
                self.project_field(expr, place, field.node.field)
            }
            ExprKind::TupleIndex(tuple) => {
                let place = self.lower_projected_place_from(&tuple.node.target, root_expr, root)?;
                self.project_tuple_index(expr, place, tuple.node.index)
            }
            ExprKind::Index(index) => {
                if index.node.safe {
                    return Err(unsupported_expr(expr));
                }
                let mut place =
                    self.lower_projected_place_from(&index.node.target, root_expr, root)?;
                if self.place_is_capture_cell(&place) || self.place_is_scoped_borrow(&place) {
                    match self.cx.program.type_data(place.ty) {
                        TypeData::List(_) | TypeData::Slice(_) | TypeData::Map { .. } => {
                            return Err(mutable_place_projection_gap(expr.node.id));
                        }
                        _ => {}
                    }
                }
                let index_local = self.lower_index_local(&index.node.index)?;
                let Some(ty) = typing::index_elem(&self.cx.program, place.ty) else {
                    return Err(unsupported_expr(expr));
                };
                place
                    .projection
                    .push(crate::air::Projection::Index(index_local));
                place.ty = ty;
                Ok(place)
            }
            _ => Err(unsupported_expr(expr)),
        }
    }

    fn place_is_capture_cell(&self, place: &Place) -> bool {
        self.cx
            .program
            .capture_cell_root(self.function_id, place.root)
            .is_some()
    }

    fn place_is_scoped_borrow(&self, place: &Place) -> bool {
        self.cx
            .program
            .scoped_borrow_root(self.function_id, place.root)
            .is_some()
    }

    fn promoted_dataref_projection_root(&self, place: &Place) -> bool {
        matches!(self.cx.program.type_data(place.ty), TypeData::DataRef(_))
    }

    fn project_tuple_index(
        &self,
        expr: &ExprNode,
        mut place: Place,
        index: u32,
    ) -> Result<Place, LowerError> {
        let Some(ty) = typing::tuple_field(&self.cx.program, place.ty, index) else {
            return Err(unsupported_expr(expr));
        };
        place
            .projection
            .push(crate::air::Projection::TupleField(index));
        place.ty = ty;
        Ok(place)
    }

    fn project_tuple_index_operand(
        &mut self,
        value: Operand,
        expr: &ExprNode,
        index: u32,
    ) -> Result<Operand, LowerError> {
        let place = self.place_from_operand(value, expr)?;
        self.project_tuple_index(expr, place, index)
            .map(Operand::Place)
    }

    fn project_field(
        &self,
        expr: &ExprNode,
        mut place: Place,
        field_name: Ident,
    ) -> Result<Place, LowerError> {
        let Some((field, ty)) = typing::field_by_name(&self.cx.program, place.ty, field_name)
        else {
            return Err(unsupported_expr(expr));
        };
        place.projection.push(crate::air::Projection::Field(field));
        place.ty = ty;
        Ok(place)
    }

    fn lower_extern_value(&mut self, expr: &ExprNode) -> Result<Option<Operand>, LowerError> {
        let Some(target) = self.select_extern_target(expr.node.id, |target| {
            matches!(
                target,
                ExternUseTarget::FieldRead(_)
                    | ExternUseTarget::UnaryOperator(_)
                    | ExternUseTarget::BinaryOperator(_)
            ) && self.cx.maps.externs.contains_key(&target)
        }) else {
            return Ok(None);
        };
        let rvalue = match target {
            ExternUseTarget::FieldRead(_) => self.lower_extern_field_read(expr, target)?,
            ExternUseTarget::UnaryOperator(_) => self.lower_extern_unary(expr, target)?,
            ExternUseTarget::BinaryOperator(_) => self.lower_extern_binary(expr, target)?,
            ExternUseTarget::Function(_)
            | ExternUseTarget::FieldWrite(_)
            | ExternUseTarget::Method(_)
            | ExternUseTarget::Static(_)
            | ExternUseTarget::Init(_) => return Ok(None),
        };
        Ok(Some(self.emit_temp(rvalue)?))
    }

    fn select_extern_target(
        &self,
        expr_id: ExprId,
        accepts: impl Fn(ExternUseTarget) -> bool,
    ) -> Option<ExternUseTarget> {
        self.facts
            .extern_uses
            .get(&expr_id)
            .and_then(|targets| targets.iter().copied().find(|target| accepts(*target)))
    }

    fn lower_extern_field_read(
        &mut self,
        expr: &ExprNode,
        target: ExternUseTarget,
    ) -> Result<RValue, LowerError> {
        let ExprKind::Field(field) = &expr.node.kind else {
            return Err(unsupported_expr(expr));
        };
        let callee = self.extern_callee(expr.node.id, target)?;
        if !matches!(
            self.cx.program.extern_decl(callee).member,
            ExternMember::FieldGetter { .. }
        ) {
            return Err(unsupported_expr(expr));
        }
        let callee = Callee::Extern(callee);
        let args = self.lower_exact_call_args(
            expr.node.id,
            &callee,
            std::iter::once(field.node.target.as_ref()),
        )?;
        Ok(RValue::Call { callee, args })
    }

    fn lower_extern_field_read_operand(
        &mut self,
        expr_id: ExprId,
        pattern: &ast::PatternNode,
        receiver: Place,
        target: ExternUseTarget,
    ) -> Result<RValue, LowerError> {
        self.lower_extern_field_read_from_place(expr_id, None, receiver, target)
            .map_err(|err| match err {
                LowerError::UnsupportedExternUse { .. } => unsupported_pattern_stmt(pattern),
                err => err,
            })
    }

    fn lower_extern_field_read_from_place(
        &mut self,
        expr_id: ExprId,
        site: Option<&ExprNode>,
        receiver: Place,
        target: ExternUseTarget,
    ) -> Result<RValue, LowerError> {
        let callee = self.extern_callee(expr_id, target)?;
        if !matches!(
            self.cx.program.extern_decl(callee).member,
            ExternMember::FieldGetter { .. }
        ) {
            return Err(LowerError::UnsupportedExternUse {
                expr_id,
                kind: unsupported_extern_kind(target),
            });
        }
        let params = self.callee_params(&Callee::Extern(callee))?;
        let Some(receiver_param) = params.get(&self.cx.program, 0) else {
            return Err(LowerError::UnsupportedExternUse {
                expr_id,
                kind: unsupported_extern_kind(target),
            });
        };
        let receiver = self.lower_place_receiver_call_arg(receiver, receiver_param, site)?;
        Ok(RValue::Call {
            callee: Callee::Extern(callee),
            args: vec![receiver],
        })
    }

    fn lower_extern_field_write(
        &mut self,
        target_expr: &ExprNode,
        value_expr: &ExprNode,
        target: ExternUseTarget,
    ) -> Result<RValue, LowerError> {
        let ExprKind::Field(field) = &target_expr.node.kind else {
            return Err(unsupported_expr(target_expr));
        };
        let callee = self.extern_callee(target_expr.node.id, target)?;
        if !matches!(
            &self.cx.program.extern_decl(callee).member,
            ExternMember::FieldSetter { .. }
        ) {
            return Err(unsupported_expr(target_expr));
        }
        let args = self.lower_exact_call_args(
            target_expr.node.id,
            &Callee::Extern(callee),
            [field.node.target.as_ref(), value_expr].into_iter(),
        )?;
        Ok(RValue::Call {
            callee: Callee::Extern(callee),
            args,
        })
    }

    fn lower_extern_literal_init(
        &mut self,
        expr: &ExprNode,
        init_args: &[(Ident, usize, bool)],
        lowered: &[(Ident, Operand)],
    ) -> Result<RValue, LowerError> {
        let target = self
            .select_extern_target(expr.node.id, |target| {
                matches!(target, ExternUseTarget::Init(_))
                    && self.cx.maps.externs.contains_key(&target)
            })
            .ok_or_else(|| unsupported_expr(expr))?;
        let callee = self.extern_callee(expr.node.id, target)?;
        let params = self.callee_params(&Callee::Extern(callee))?;
        if params.len(&self.cx.program) != Some(init_args.len()) {
            return Err(unsupported_expr(expr));
        }
        let mut args = vec![];
        let mut init_args = init_args.iter().collect::<Vec<_>>();
        init_args.sort_by_key(|(_, param, _)| *param);
        for (name, param_index, presence) in init_args {
            let Some(param) = params.get(&self.cx.program, *param_index) else {
                return Err(unsupported_expr(expr));
            };
            if param.mode != ParamMode::Value {
                return Err(unsupported_expr(expr));
            }
            let value = lowered
                .iter()
                .find_map(|(field, value)| (field == name).then(|| value.clone()));
            let arg = match (*presence, value) {
                (false, Some(value)) => CallArg::Value(value),
                (true, Some(value)) => CallArg::InitFieldProvided(value),
                (true, None) => CallArg::InitFieldOmitted,
                (false, None) => return Err(unsupported_expr(expr)),
            };
            args.push(arg);
        }
        Ok(RValue::Call {
            callee: Callee::Extern(callee),
            args,
        })
    }

    fn extern_field_write_target(&self, expr_id: ExprId, name: Ident) -> Option<ExternUseTarget> {
        let externs = self.cx.externs.as_ref()?;
        self.facts
            .extern_uses
            .get(&expr_id)
            .into_iter()
            .flatten()
            .copied()
            .find(|target| match target {
                ExternUseTarget::FieldWrite(field) => externs.field_ref(*field).1.name == name,
                _ => false,
            })
            .filter(|target| self.cx.maps.externs.contains_key(target))
    }

    fn extern_field_read_target(
        &self,
        expr_id: ExprId,
        owner: crate::air::ExternTypeId,
        name: Ident,
    ) -> Option<ExternUseTarget> {
        let externs = self.cx.externs.as_ref()?;
        self.facts
            .extern_uses
            .get(&expr_id)
            .into_iter()
            .flatten()
            .copied()
            .find(|target| match target {
                ExternUseTarget::FieldRead(field) => {
                    let Some(callee) = self.cx.maps.externs.get(target) else {
                        return false;
                    };
                    matches!(
                        self.cx.program.extern_decl(*callee).member,
                        ExternMember::FieldGetter { owner: field_owner, .. } if field_owner == owner
                    ) && externs.field_ref(*field).1.name == name
                }
                _ => false,
            })
    }

    fn lower_extern_pattern_field(
        &mut self,
        pattern: &ast::PatternNode,
        extern_site: Option<ExprId>,
        place: Place,
        owner: crate::air::ExternTypeId,
        name: Ident,
    ) -> Result<Operand, LowerError> {
        if let Some((site, target)) = extern_site.and_then(|site| {
            self.extern_field_read_target(site, owner, name)
                .map(|target| (site, target))
        }) {
            let value = self.lower_extern_field_read_operand(site, pattern, place, target)?;
            return self.emit_temp(value);
        }
        let field = self.extern_direct_read_field_place(pattern, owner, place, name)?;
        Ok(Operand::Place(field))
    }

    fn extern_direct_read_field_place(
        &self,
        pattern: &ast::PatternNode,
        extern_id: crate::air::ExternTypeId,
        mut place: Place,
        name: Ident,
    ) -> Result<Place, LowerError> {
        let decl = self.cx.program.extern_type(extern_id);
        if decl.rep != ExternRep::Inline {
            return Err(unsupported_pattern_stmt(pattern));
        }
        let Some((index, field)) = decl
            .fields
            .iter()
            .enumerate()
            .find(|(_, field)| field.name == name)
        else {
            return Err(unsupported_pattern_stmt(pattern));
        };
        if field.computed || !field.readable {
            return Err(unsupported_pattern_stmt(pattern));
        }
        place
            .projection
            .push(crate::air::Projection::Field(FieldId::from_index(index)));
        place.ty = field.ty;
        Ok(place)
    }

    fn extern_direct_field_place(
        &self,
        expr: &ExprNode,
        extern_id: crate::air::ExternTypeId,
        mut place: Place,
        name: Ident,
    ) -> Result<Place, LowerError> {
        let decl = self.cx.program.extern_type(extern_id);
        if decl.rep != ExternRep::Inline {
            return Err(unsupported_expr(expr));
        }
        let Some((index, field)) = decl
            .fields
            .iter()
            .enumerate()
            .find(|(_, field)| field.name == name)
        else {
            return Err(unsupported_expr(expr));
        };
        if field.computed || !field.writable {
            return Err(unsupported_expr(expr));
        }
        place
            .projection
            .push(crate::air::Projection::Field(FieldId::from_index(index)));
        place.ty = field.ty;
        Ok(place)
    }

    fn lower_extern_field_write_operand(
        &mut self,
        expr: &ExprNode,
        receiver: Place,
        value: Operand,
        target: ExternUseTarget,
    ) -> Result<RValue, LowerError> {
        let callee = self.extern_callee(expr.node.id, target)?;
        if !matches!(
            &self.cx.program.extern_decl(callee).member,
            ExternMember::FieldSetter { .. }
        ) {
            return Err(unsupported_expr(expr));
        }
        let params = self.callee_params(&Callee::Extern(callee))?;
        let Some(receiver_param) = params.get(&self.cx.program, 0) else {
            return Err(unsupported_expr(expr));
        };
        let Some(value_param) = params.get(&self.cx.program, 1) else {
            return Err(unsupported_expr(expr));
        };
        let receiver = self.lower_place_receiver_call_arg(receiver, receiver_param, Some(expr))?;
        let value = self.lower_operand_call_arg(value, value_param, expr)?;
        Ok(RValue::Call {
            callee: Callee::Extern(callee),
            args: vec![receiver, value],
        })
    }

    fn lower_extern_unary(
        &mut self,
        expr: &ExprNode,
        target: ExternUseTarget,
    ) -> Result<RValue, LowerError> {
        let ExprKind::Unary(unary) = &expr.node.kind else {
            return Err(unsupported_expr(expr));
        };
        let callee = self.extern_callee(expr.node.id, target)?;
        if !matches!(
            self.cx.program.extern_decl(callee).member,
            ExternMember::UnaryOperator { .. }
        ) {
            return Err(unsupported_expr(expr));
        }
        let args = self.lower_exact_call_args(
            expr.node.id,
            &Callee::Extern(callee),
            [unary.node.expr.as_ref()].into_iter(),
        )?;
        Ok(RValue::Call {
            callee: Callee::Extern(callee),
            args,
        })
    }

    fn lower_extern_binary(
        &mut self,
        expr: &ExprNode,
        target: ExternUseTarget,
    ) -> Result<RValue, LowerError> {
        let ExprKind::Binary(binary) = &expr.node.kind else {
            return Err(unsupported_expr(expr));
        };
        let callee = self.extern_callee(expr.node.id, target)?;
        let self_on_right = match &self.cx.program.extern_decl(callee).member {
            ExternMember::BinaryOperator { self_on_right, .. } => *self_on_right,
            _ => return Err(unsupported_expr(expr)),
        };
        let (receiver, operand) = if self_on_right {
            (&binary.node.right, &binary.node.left)
        } else {
            (&binary.node.left, &binary.node.right)
        };
        let args = self.lower_exact_call_args(
            expr.node.id,
            &Callee::Extern(callee),
            [receiver.as_ref(), operand.as_ref()].into_iter(),
        )?;
        Ok(RValue::Call {
            callee: Callee::Extern(callee),
            args,
        })
    }

    fn extern_callee(
        &self,
        expr_id: ExprId,
        target: ExternUseTarget,
    ) -> Result<ExternId, LowerError> {
        self.cx
            .maps
            .externs
            .get(&target)
            .copied()
            .ok_or(LowerError::UnsupportedExternUse {
                expr_id,
                kind: unsupported_extern_kind(target),
            })
    }

    fn lower_index_local(&mut self, expr: &ExprNode) -> Result<LocalId, LowerError> {
        let value = self.lower_value(expr)?;
        let ty = self.cx.lower_ty(&self.operand_type(&value))?;
        match value {
            Operand::Place(place) if place.projection.is_empty() => {
                place.root.local().ok_or_else(|| unsupported_expr(expr))
            }
            value => match self.emit_typed_temp(ty, RValue::Use(value))? {
                Operand::Place(place) if place.projection.is_empty() => {
                    place.root.local().ok_or_else(|| unsupported_expr(expr))
                }
                _ => Err(unsupported_expr(expr)),
            },
        }
    }

    fn lower_function_value(&mut self, expr: &ExprNode) -> Result<Option<Operand>, LowerError> {
        let Some(fact) = self.facts.function_values.get(&expr.node.id) else {
            return Ok(None);
        };
        if !matches!(self.lower_expr_ty(expr.node.id)?, Type::Func { .. }) {
            return Ok(None);
        }
        let ty = self.cx.lower_ty(&fact.ty)?;
        let value = match &fact.kind {
            FunctionValueKind::Named(target) => {
                let body = BodyInstanceKey::Callable(target.clone());
                let Some(function) = self.cx.maps.bodies.get(&body).copied() else {
                    return Err(LowerError::MissingLoweredCallee {
                        body: Box::new(body),
                    });
                };
                RValue::FunctionRef { function, ty }
            }
            FunctionValueKind::Lambda { lambda_expr } => {
                let key = LambdaBodyKey {
                    expr: *lambda_expr,
                    specialization: self.current_specialization(),
                };
                let Some(lambda) = self.cx.maps.lambdas.get(&key).copied() else {
                    return Err(LowerError::MissingSpecializedBodyFacts {
                        body: Box::new(BodyInstanceKey::Lambda(key)),
                    });
                };
                RValue::MakeLambda {
                    lambda,
                    captures: self.lower_lambda_capture_args(*lambda_expr)?,
                    ty,
                }
            }
            FunctionValueKind::Storage(FunctionValueOrigin::KnownLocal) => return Ok(None),
            FunctionValueKind::Storage(origin) => {
                let Some(value) = self.lower_storage_function_value(expr)? else {
                    return Ok(None);
                };
                RValue::FunctionValue {
                    value,
                    capability: Self::storage_function_value_capability(*origin),
                }
            }
        };
        Ok(Some(self.emit_typed_temp(ty, value)?))
    }

    fn lower_storage_function_value(
        &mut self,
        expr: &ExprNode,
    ) -> Result<Option<Operand>, LowerError> {
        if let ExprKind::Call(call) = &expr.node.kind {
            return self.lower_call_value(expr, call).map(Some);
        }
        if let Some(fact) = self.global_access(expr.node.id).cloned()
            && fact.mode == GlobalAccessMode::Read
        {
            return self
                .lower_global_projected_place(expr, &fact)
                .map(Operand::Place)
                .map(Some);
        }
        match self.local_use(expr, LocalUseMode::Read) {
            Ok(fact) => self.lower_place(expr, &fact).map(Operand::Place).map(Some),
            Err(LowerError::MissingLocalUse { .. }) => self
                .lower_projected_read_place(expr)
                .map(Operand::Place)
                .map(Some),
            Err(err) => Err(err),
        }
    }

    fn storage_function_value_capability(origin: FunctionValueOrigin) -> FunctionValueCapability {
        match origin.escape_capability() {
            FunctionValueEscapeCapability::EscapingSafe => FunctionValueCapability::Escaping,
            FunctionValueEscapeCapability::Unknown => FunctionValueCapability::Unknown,
        }
    }

    fn lower_lambda_capture_args(
        &mut self,
        expr_id: ExprId,
    ) -> Result<Vec<LambdaCaptureArg>, LowerError> {
        self.cx
            .ordered_lambda_capture_facts(expr_id)?
            .into_iter()
            .map(|capture| self.lower_lambda_capture_arg(expr_id, &capture))
            .collect()
    }

    fn populate_owned_lambda_capture_decls(&mut self) -> Result<(), LowerError> {
        for (expr_id, lambda) in &self.owned_lambdas {
            let captures = self.cx.lower_lambda_capture_decls(
                *expr_id,
                self.function_id,
                &self.function,
                &self.capture_sources,
            )?;
            let decl = &mut self.cx.program.lambdas[lambda.index()];
            if decl.captures.is_empty() {
                decl.captures = captures;
            } else if decl.captures != captures {
                return Err(lambda_capture_gap(*expr_id));
            }
        }
        Ok(())
    }

    fn lower_lambda_capture_arg(
        &mut self,
        expr_id: ExprId,
        capture: &LambdaCaptureFact,
    ) -> Result<LambdaCaptureArg, LowerError> {
        let ty = self.cx.lower_ty(&capture.ty)?;
        match lowered_capture_kind(expr_id, capture.runtime_plan)? {
            LoweredCaptureKind::NoRuntime => Ok(LambdaCaptureArg::NoRuntime),
            LoweredCaptureKind::ReadonlyLocal => {
                let place = exact_local_capture_place(expr_id, &self.capture_sources, capture, ty)?;
                Ok(LambdaCaptureArg::ReadonlyLocal {
                    value: Operand::Place(place),
                })
            }
            LoweredCaptureKind::CaptureCell => {
                let cell = exact_capture_cell(expr_id, &self.capture_sources, capture, ty)?;
                Ok(LambdaCaptureArg::CaptureCell { cell })
            }
            LoweredCaptureKind::ScopedBorrow => {
                let place = exact_scoped_borrow_place(expr_id, &self.capture_sources, capture, ty)?;
                Ok(LambdaCaptureArg::ScopedBorrow { place })
            }
        }
    }

    fn lower_range_value(
        &mut self,
        expr: &ExprNode,
        range: &ast::Range,
    ) -> Result<Operand, LowerError> {
        let source_ty = self.lower_expr_ty(expr.node.id)?;
        if self
            .cx
            .decls
            .as_ref()
            .and_then(|decls| decls.core_range_kind(&source_ty))
            .is_none()
        {
            return Err(unsupported_expr(expr));
        }
        let ty = self.cx.lower_ty(&source_ty)?;
        let aggregate = match self.cx.program.type_data(ty) {
            TypeData::Aggregate(aggregate) => *aggregate,
            _ => return Err(unsupported_expr(expr)),
        };
        let aggregate_fields = self.cx.program.aggregate(aggregate).fields.clone();
        let field_ty = |name: &str| {
            aggregate_fields
                .iter()
                .find(|field| field.name.as_str() == name)
                .map(|field| field.ty)
                .ok_or_else(|| unsupported_expr(expr))
        };
        let (start, end) = match range {
            ast::Range::Bounded { start, end, .. } => (
                Some(self.lower_value_to(start, field_ty(RANGE_START_FIELD)?, start)?),
                Some(self.lower_value_to(end, field_ty(RANGE_END_FIELD)?, end)?),
            ),
            ast::Range::From { start } => (
                Some(self.lower_value_to(start, field_ty(RANGE_START_FIELD)?, start)?),
                None,
            ),
            ast::Range::To { end, .. } => (
                None,
                Some(self.lower_value_to(end, field_ty(RANGE_END_FIELD)?, end)?),
            ),
        };
        let fields = aggregate_fields
            .iter()
            .map(|field| match field.name.as_str() {
                RANGE_START_FIELD => start.clone().ok_or_else(|| unsupported_expr(expr)),
                RANGE_END_FIELD => end.clone().ok_or_else(|| unsupported_expr(expr)),
                _ => Err(unsupported_expr(expr)),
            })
            .collect::<Result<Vec<_>, _>>()?;
        self.emit_typed_temp(
            ty,
            RValue::Aggregate {
                kind: AggregateCtor::Struct(aggregate),
                fields,
                ty,
            },
        )
    }

    fn lower_value(&mut self, expr: &ExprNode) -> Result<Operand, LowerError> {
        if let Some(fact) = self.facts.raw_try_constructs.get(&expr.node.id).cloned() {
            if fact.expr_id != expr.node.id {
                return Err(LowerError::MissingTypecheckFacts);
            }
            let ExprKind::FailableCast(cast) = &expr.node.kind else {
                return Err(LowerError::MissingTypecheckFacts);
            };
            if cast.node.expr.node.id != fact.source_expr {
                return Err(LowerError::MissingTypecheckFacts);
            }
            let source = self.cx.lower_ty(&fact.source_ty)?;
            let target = self.cx.lower_ty(&fact.target_ty)?;
            let ty = self.cx.lower_ty(&fact.result_ty)?;
            let value = self.lower_value(&cast.node.expr)?;
            if typing::operand_ty(&self.cx.program, &value) != Some(source) {
                return Err(LowerError::MissingTypecheckFacts);
            }
            return self.emit_temp(RValue::RawTryConstruct { value, target, ty });
        }
        let Some(fact) = self.facts.raw_projections.get(&expr.node.id).cloned() else {
            return self.lower_unprojected_value(expr);
        };
        if fact.expr_id != expr.node.id {
            return Err(LowerError::MissingTypecheckFacts);
        }
        let source = self.cx.lower_ty(&fact.source_ty)?;
        let target = self.cx.lower_ty(&fact.target_ty)?;
        let value = if fact.source_expr == expr.node.id {
            self.lower_unprojected_value_as(expr, fact.source_ty.clone())?
        } else {
            let ExprKind::Cast(cast) = &expr.node.kind else {
                return Err(LowerError::MissingTypecheckFacts);
            };
            if cast.node.expr.node.id != fact.source_expr {
                return Err(LowerError::MissingTypecheckFacts);
            }
            self.lower_value(&cast.node.expr)?
        };
        if typing::operand_ty(&self.cx.program, &value) != Some(source) {
            return Err(LowerError::MissingTypecheckFacts);
        }
        self.emit_temp(RValue::RawProject { value, target })
    }

    fn lower_unprojected_value_as(
        &mut self,
        expr: &ExprNode,
        ty: Type,
    ) -> Result<Operand, LowerError> {
        let previous = self.type_overrides.insert(expr.node.id, ty);
        let result = self.lower_unprojected_value(expr);
        match previous {
            Some(previous) => {
                self.type_overrides.insert(expr.node.id, previous);
            }
            None => {
                self.type_overrides.remove(&expr.node.id);
            }
        }
        result
    }

    fn lower_unprojected_value(&mut self, expr: &ExprNode) -> Result<Operand, LowerError> {
        if let Some(fact) = self.facts.flag_statics.get(&expr.node.id).cloned() {
            if fact.expr_id != expr.node.id {
                return Err(LowerError::MissingTypecheckFacts);
            }
            let ty = self.cx.lower_ty(&fact.owner_ty)?;
            if !matches!(self.cx.program.type_data(ty), TypeData::Flag(_)) {
                return Err(LowerError::MissingTypecheckFacts);
            }
            let op = match fact.op {
                crate::typecheck::FlagStaticOp::Empty => FlagStaticOp::Empty,
                crate::typecheck::FlagStaticOp::All => FlagStaticOp::All,
            };
            return self.emit_temp(RValue::FlagStatic { op, ty });
        }
        if let Some(fact) = self.facts.flag_members.get(&expr.node.id).cloned() {
            if fact.expr_id != expr.node.id {
                return Err(LowerError::MissingTypecheckFacts);
            }
            let ty = self.cx.lower_ty(&fact.owner_ty)?;
            let TypeData::Flag(flag) = self.cx.program.type_data(ty) else {
                return Err(LowerError::MissingTypecheckFacts);
            };
            return Ok(Operand::Const(self.cx.program.alloc_const(ConstData {
                ty,
                value: ConstValue::Flag {
                    flag: *flag,
                    bits: fact.value,
                },
            })));
        }
        if let Some(fact) = self.facts.dyn_downcasts.get(&expr.node.id).cloned() {
            if fact.mutable {
                return Err(unsupported_expr(expr));
            }
            let ExprKind::FailableCast(cast) = &expr.node.kind else {
                return Err(unsupported_expr(expr));
            };
            let DynDowncastSource::Resolved(surface) = fact.source else {
                return Err(unsupported_expr(expr));
            };
            if fact.source_id != cast.node.expr.node.id {
                return Err(LowerError::MissingTypecheckFacts);
            }
            let surface = self
                .cx
                .maps
                .contract_surfaces
                .get(&surface)
                .copied()
                .ok_or_else(|| unsupported_expr(expr))?;
            let target = self.cx.lower_ty(&fact.target)?;
            let raw_ty = self.cx.lower_ty(&Type::Optional {
                inner: Box::new(fact.target),
            })?;
            let value = self.lower_value(&cast.node.expr)?;
            let downcast = match value {
                Operand::Place(place) if matches!(place.root, PlaceRoot::DynBorrowParam(_)) => self
                    .lower_borrowed_dyn_downcast(&cast.node.expr, place, surface, target, raw_ty)?,
                value => {
                    let use_ = self.dyn_owned_use(&value);
                    self.emit_typed_temp(
                        raw_ty,
                        RValue::DynDowncast {
                            value,
                            use_,
                            surface,
                            target,
                            ty: raw_ty,
                        },
                    )?
                }
            };
            let expected = self.cx.lower_ty(&self.lower_expr_ty(expr.node.id)?)?;
            if expected == raw_ty {
                return Ok(downcast);
            }
            return self.lower_contextual_dyn_downcast(expr, downcast, target, expected);
        }
        if let Some(fact) = self.facts.dyn_conversions.get(&expr.node.id).cloned() {
            let witness = self
                .cx
                .maps
                .contract_witnesses
                .get(&fact.witness)
                .copied()
                .ok_or_else(|| unsupported_expr(expr))?;
            let concrete_ty = self.cx.program.contract_witness(witness).key.concrete_ty;
            let value = match &expr.node.kind {
                ExprKind::StructLiteral(literal) => {
                    self.lower_struct_literal_as(expr, literal, concrete_ty)?
                }
                _ => self.lower_dynamic_source(expr)?,
            };
            let surface = self.cx.program.contract_witness(witness).key.surface;
            let ty = self
                .cx
                .program
                .type_arena
                .iter()
                .position(|ty| ty == &TypeData::Dyn(surface))
                .map(TypeId::from_index)
                .ok_or_else(|| unsupported_expr(expr))?;
            let use_ = self.dyn_owned_use(&value);
            return self.emit_typed_temp(
                ty,
                RValue::DynPack {
                    value,
                    use_,
                    witness,
                    ty,
                },
            );
        }
        if let Some(_fact) = self.facts.dyn_weakenings.get(&expr.node.id) {
            let value = self.lower_dynamic_source(expr)?;
            let use_ = self.dyn_owned_use(&value);
            let weakening = self
                .cx
                .maps
                .contract_weakenings
                .get(&(self.body.clone(), expr.node.id))
                .copied()
                .ok_or_else(|| unsupported_expr(expr))?;
            let surface = self.cx.program.contract_weakening(weakening).target;
            let ty = self
                .cx
                .program
                .type_arena
                .iter()
                .position(|ty| ty == &TypeData::Dyn(surface))
                .map(TypeId::from_index)
                .ok_or_else(|| unsupported_expr(expr))?;
            return self.emit_typed_temp(
                ty,
                RValue::DynWeaken {
                    value,
                    use_,
                    weakening,
                    ty,
                },
            );
        }
        self.lower_plain_value(expr)
    }

    fn lower_borrowed_dyn_downcast(
        &mut self,
        source: &ExprNode,
        place: Place,
        surface: ContractSurfaceId,
        target: TypeId,
        ty: TypeId,
    ) -> Result<Operand, LowerError> {
        let borrow = self.dyn_reborrow(source, place, surface)?;
        let payload = self.push_local(
            None,
            None,
            target,
            AirMutability::Immutable,
            LocalKind::PatternBinding,
        );
        let result = self.temp(ty);
        let some_block = self.with_nested_block(|this| {
            let value = this.optional_some(this.operand_place(payload), ty, source)?;
            this.emit_init(result, RValue::Use(value))
        })?;
        let none_block = self.with_nested_block(|this| {
            let value = this.optional_none(ty, source)?;
            this.emit_init(result, RValue::Use(value))
        })?;
        self.push_dyn_match(AirDynMatch {
            source: AirDynMatchSource::Borrowed(borrow),
            surface,
            arms: vec![AirDynMatchArm {
                target,
                binding: AirDynMatchTargetBinding::Materialize(payload),
                block: some_block,
            }],
            fallback: AirDynMatchFallback {
                binding: AirDynMatchFallbackBinding::Discard,
                block: none_block,
            },
        })?;
        Ok(self.operand_place(result))
    }

    fn lower_contextual_dyn_downcast(
        &mut self,
        expr: &ExprNode,
        downcast: Operand,
        target: TypeId,
        expected: TypeId,
    ) -> Result<Operand, LowerError> {
        let TypeData::Optional(dyn_ty) = self.cx.program.type_data(expected) else {
            return Err(unsupported_expr(expr));
        };
        let dyn_ty = *dyn_ty;
        let TypeData::Dyn(surface) = self.cx.program.type_data(dyn_ty) else {
            return Err(unsupported_expr(expr));
        };
        let surface = *surface;
        let conversion = self
            .facts
            .dyn_conversions
            .get(&expr.node.id)
            .ok_or_else(|| unsupported_expr(expr))?;
        let witness = self
            .cx
            .maps
            .contract_witnesses
            .get(&conversion.witness)
            .copied()
            .ok_or_else(|| unsupported_expr(expr))?;
        let witness_decl = self.cx.program.contract_witness(witness);
        if witness_decl.key.concrete_ty != target || witness_decl.key.surface != surface {
            return Err(unsupported_expr(expr));
        }
        let Operand::Place(discr) = downcast else {
            return Err(unsupported_expr(expr));
        };
        let payload = self.temp(target);
        let result = self.temp(expected);
        let some_block = self.with_nested_block(|this| {
            let packed = this.emit_typed_temp(
                dyn_ty,
                RValue::DynPack {
                    value: Operand::Place(this.local_place(payload)),
                    use_: DynOwnedUse::ConsumeTemporary,
                    witness,
                    ty: dyn_ty,
                },
            )?;
            let wrapped = this.optional_some(packed, expected, expr)?;
            this.emit_init(result, RValue::Use(wrapped))
        })?;
        let none_block = self.with_nested_block(|this| {
            let none = this.optional_none(expected, expr)?;
            this.emit_init(result, RValue::Use(none))
        })?;
        self.ensure_open()?;
        self.block
            .stmts
            .push(AirStmt::OptionalMatch(AirOptionalMatch {
                discr,
                payload: Some(payload),
                payload_ref: false,
                payload_escapes: false,
                some_block,
                none_block,
            }));
        Ok(self.operand_place(result))
    }

    fn lower_dynamic_source(&mut self, expr: &ExprNode) -> Result<Operand, LowerError> {
        if let Some(fact) = self.facts.locals.uses.get(&expr.node.id).cloned() {
            return self.lower_place(expr, &fact).map(Operand::Place);
        }
        self.lower_plain_value(expr)
    }

    fn dyn_owned_use(&self, value: &Operand) -> DynOwnedUse {
        match value {
            Operand::Place(Place {
                root: PlaceRoot::Local(local),
                projection,
                ..
            }) if projection.is_empty()
                && self.function.locals[local.index()].kind == LocalKind::Temp =>
            {
                DynOwnedUse::ConsumeTemporary
            }
            Operand::Const(_) | Operand::Place(_) => DynOwnedUse::ReusableRead,
        }
    }

    fn lower_plain_value(&mut self, expr: &ExprNode) -> Result<Operand, LowerError> {
        if let Some(value) = self.facts.const_values.get(&expr.node.id).cloned() {
            return self.lower_const_value(expr, &value);
        }
        if let Some(value) = self.lower_extern_value(expr)? {
            return Ok(value);
        }
        if let Some(value) = self.lower_function_value(expr)? {
            return Ok(value);
        }
        if let Some(value) = self.lower_safe_field_chain(expr)? {
            return Ok(value);
        }
        match &expr.node.kind {
            ExprKind::Lit(lit) => self.lower_lit(expr, lit),
            ExprKind::Ident(_) | ExprKind::Field(_) => {
                if matches!(self.lower_expr_ty(expr.node.id)?, Type::Dyn(_))
                    && let Some(fact) = self.facts.locals.uses.get(&expr.node.id).cloned()
                {
                    return self.lower_place(expr, &fact).map(Operand::Place);
                }
                if !self.facts.locals.uses.contains_key(&expr.node.id)
                    && let Some(value) = self.lower_qualified_unit_enum(expr)?
                {
                    return Ok(value);
                }
                if let Some(fact) = self.global_access(expr.node.id).cloned()
                    && fact.mode == GlobalAccessMode::Read
                {
                    return self
                        .lower_global_projected_place(expr, &fact)
                        .map(Operand::Place);
                }
                match self.local_use(expr, LocalUseMode::Read) {
                    Ok(fact) => {
                        let place = self.lower_place(expr, &fact)?;
                        Ok(Operand::Place(place))
                    }
                    Err(LowerError::MissingLocalUse { .. }) => {
                        self.lower_projected_read_place(expr).map(Operand::Place)
                    }
                    Err(err) => Err(err),
                }
            }
            ExprKind::TupleIndex(tuple) => {
                let value = self.lower_value(&tuple.node.target)?;
                self.project_tuple_index_operand(value, expr, tuple.node.index)
            }
            ExprKind::Index(index) => self.lower_index_value(expr, index),
            ExprKind::Range(range) => self.lower_range_value(expr, &range.node),
            ExprKind::Block(block) => self.lower_block_value(expr, block),
            ExprKind::If(if_expr) => self.lower_if_value(expr, if_expr),
            ExprKind::IfLet(if_let) => self.lower_if_let_value(expr, if_let),
            ExprKind::Match(match_expr) => self.lower_match_value(expr, match_expr),
            ExprKind::Unary(unary) => {
                let value = self.lower_value(&unary.node.expr)?;
                let result_ty = self.lower_expr_ty(expr.node.id)?;
                let value_ty = self.operand_ty(&value);
                let ty = self.cx.lower_ty(&result_ty)?;
                let flag_not = unary.node.op == ast::UnaryOp::BitNot
                    && value_ty == ty
                    && matches!(self.cx.program.type_data(ty), TypeData::Flag(_));
                if !flag_not {
                    self.require_builtin_scalar(expr)?;
                    let value_scalar = self
                        .operand_type(&value)
                        .scalar_kind()
                        .ok_or_else(|| unsupported_expr(expr))?;
                    let result_scalar = result_ty
                        .scalar_kind()
                        .ok_or_else(|| unsupported_expr(expr))?;
                    if unary.node.op.scalar_result(value_scalar) != Some(result_scalar) {
                        return Err(unsupported_expr(expr));
                    }
                }
                self.emit_temp(RValue::Unary {
                    op: unary.node.op,
                    value,
                    ty,
                })
            }
            ExprKind::Binary(binary) => {
                let result_ty = self.lower_expr_ty(expr.node.id)?;
                if binary.node.op == BinaryOp::Add && result_ty == Type::String {
                    return self.lower_string_concat(expr);
                }
                if binary.node.op == BinaryOp::Coalesce {
                    return self.lower_coalesce(expr, binary, &result_ty);
                }
                if matches!(binary.node.op, BinaryOp::And | BinaryOp::Or) {
                    return Err(unsupported_expr(expr));
                }
                if let Some(value) = self.lower_nil_equality(expr, binary)? {
                    return Ok(value);
                }
                if matches!(binary.node.op, BinaryOp::Eq | BinaryOp::NotEq)
                    && let Some(value) = self.lower_dataref_eq(expr, binary, &result_ty)?
                {
                    return Ok(value);
                }
                if matches!(binary.node.op, BinaryOp::Eq | BinaryOp::NotEq)
                    && let Some(value) = self.lower_unit_enum_eq(binary, &result_ty)?
                {
                    return Ok(value);
                }
                let lhs = self.lower_value(&binary.node.left)?;
                let rhs = self.lower_value(&binary.node.right)?;
                let lhs_air_ty = self.operand_ty(&lhs);
                let rhs_air_ty = self.operand_ty(&rhs);
                let ty = self.cx.lower_ty(&result_ty)?;
                let same_flag = lhs_air_ty == rhs_air_ty
                    && matches!(self.cx.program.type_data(lhs_air_ty), TypeData::Flag(_));
                let flag_op = same_flag
                    && match binary.node.op {
                        BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::Xor => ty == lhs_air_ty,
                        BinaryOp::Eq | BinaryOp::NotEq => {
                            matches!(self.cx.program.type_data(ty), TypeData::Bool)
                        }
                        _ => false,
                    };
                if !flag_op {
                    self.require_builtin_scalar(expr)?;
                    let lhs_ty = self.operand_type(&lhs);
                    let rhs_ty = self.operand_type(&rhs);
                    let (Some(lhs_scalar), Some(rhs_scalar), Some(result_scalar)) = (
                        lhs_ty.scalar_kind(),
                        rhs_ty.scalar_kind(),
                        result_ty.scalar_kind(),
                    ) else {
                        return Err(unsupported_expr(expr));
                    };
                    if binary.node.op.scalar_result(lhs_scalar, rhs_scalar) != Some(result_scalar) {
                        return Err(unsupported_expr(expr));
                    }
                }
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
            ExprKind::StructLiteral(literal) => self.lower_struct_literal(expr, literal),
            ExprKind::Tuple(elems) => self.lower_tuple_literal(expr, elems),
            ExprKind::ArrayLiteral(literal) => self.lower_array_literal(expr, literal),
            ExprKind::ArrayFill(fill) => self.lower_array_fill(expr, fill),
            ExprKind::MapLiteral(literal) => self.lower_map_literal(expr, literal),
            ExprKind::InferredEnum(inferred) => self.lower_inferred_enum(expr, inferred),
            ExprKind::Cast(cast) | ExprKind::FailableCast(cast) => {
                let site = UserCastSite {
                    expr: expr.node.id,
                    source: cast.node.expr.node.id,
                };
                if let Some(instance) = self.facts.user_cast_conversions.get(&site).cloned() {
                    self.lower_user_cast_conversion(expr, &cast.node.expr, &instance)
                } else {
                    self.lower_cast_expr(expr, cast)
                }
            }
            _ => Err(unsupported_expr(expr)),
        }
    }

    fn lower_safe_field_chain(&mut self, expr: &ExprNode) -> Result<Option<Operand>, LowerError> {
        let Some((base, steps)) = collect_safe_field_chain(expr) else {
            return Ok(None);
        };
        let mut result_ty = self.cx.lower_ty(&self.lower_expr_ty(expr.node.id)?)?;
        if typing::optional_inner(&self.cx.program, result_ty).is_none() {
            result_ty = self.cx.optional_ty(result_ty);
        }
        let result = self.temp(result_ty);
        self.lower_collected_field_chain(
            base,
            &steps,
            expr,
            ChainMode::Value { result, result_ty },
        )?;
        Ok(Some(Operand::Place(self.local_place(result))))
    }

    fn lower_safe_field_chain_effect(&mut self, expr: &ExprNode) -> Result<bool, LowerError> {
        let Some((base, steps)) = collect_safe_field_chain(expr) else {
            return Ok(false);
        };
        self.lower_collected_field_chain(base, &steps, expr, ChainMode::Effect)?;
        Ok(true)
    }

    fn lower_collected_field_chain<'a>(
        &mut self,
        base: &'a ExprNode,
        steps: &'a [ChainStep<'a>],
        expr: &ExprNode,
        mode: ChainMode,
    ) -> Result<(), LowerError> {
        let (base, steps) = self.lower_field_chain_base(base, steps)?;
        self.lower_field_chain_steps(base, steps, expr, mode)
    }

    fn lower_field_chain_base<'a>(
        &mut self,
        base: &ExprNode,
        steps: &'a [ChainStep<'a>],
    ) -> Result<(Operand, &'a [ChainStep<'a>]), LowerError> {
        if let Some((ChainStep::Call { expr, node }, rest)) = steps.split_first()
            && !node.node.safe
        {
            let base = self.lower_call_value(expr, node)?;
            return Ok((base, rest));
        }
        if let Some((step, rest)) = steps.split_first()
            && field_call_step(step, rest).is_some()
            && let Ok(place) = self.lower_place_arg(base, false)
        {
            return Ok((Operand::Place(place), steps));
        }
        match self.lower_value(base) {
            Ok(base) => Ok((base, steps)),
            Err(LowerError::UnsupportedExpr { kind: "Ident", .. })
                if let Some((step, rest)) = steps.split_first()
                    && field_call_step(step, rest).is_some() =>
            {
                Err(lambda_capture_gap(base.node.id))
            }
            Err(err) => Err(err),
        }
    }

    fn lower_field_chain_steps(
        &mut self,
        current: Operand,
        steps: &[ChainStep<'_>],
        site: &ExprNode,
        mode: ChainMode,
    ) -> Result<(), LowerError> {
        let Some((step, rest)) = steps.split_first() else {
            return self.finish_field_chain(current, site, mode);
        };

        if let Some((call_expr, call, call_rest)) = field_call_step(step, rest)
            && !chain_step_is_safe(step)
        {
            return self
                .lower_field_chain_method_call(current, call_expr, call, call_rest, site, mode);
        }

        match step {
            ChainStep::Field { expr, node } if node.node.safe => {
                let subject = self.optional_subject_from_operand(current, site)?;
                let payload = self.temp(subject.inner_ty());
                self.emit_optional_match(
                    subject,
                    Some(payload),
                    |this, payload| {
                        let payload =
                            Operand::Place(this.local_place(payload.expect("payload local")));
                        if let Some((call_expr, call, call_rest)) = field_call_step(step, rest) {
                            return this.lower_field_chain_method_call(
                                payload, call_expr, call, call_rest, site, mode,
                            );
                        }
                        let value = this.lower_field_read_step(payload, expr, node.node.field)?;
                        this.lower_field_chain_steps(value, rest, site, mode)
                    },
                    |this| this.skip_field_chain(site, mode),
                )
            }
            ChainStep::Index { expr, node } if node.node.safe => {
                let subject = self.optional_subject_from_operand(current, site)?;
                let payload = self.temp(subject.inner_ty());
                self.emit_optional_match(
                    subject,
                    Some(payload),
                    |this, payload| {
                        let payload =
                            Operand::Place(this.local_place(payload.expect("payload local")));
                        let value = this.lower_index_step(payload, expr, node)?;
                        this.lower_field_chain_steps(value, rest, site, mode)
                    },
                    |this| this.skip_field_chain(site, mode),
                )
            }
            ChainStep::Call { expr, node } if node.node.safe => {
                let subject = self.optional_subject_from_operand(current, site)?;
                self.emit_optional_match(
                    subject,
                    None,
                    |this, _| this.lower_field_chain_call(expr, node, rest, site, mode),
                    |this| this.skip_field_chain(site, mode),
                )
            }
            ChainStep::Field { expr, node } => {
                let value = self.lower_field_read_step(current, expr, node.node.field)?;
                self.lower_field_chain_steps(value, rest, site, mode)
            }
            ChainStep::Index { expr, node } => {
                let value = self.lower_index_step(current, expr, node)?;
                self.lower_field_chain_steps(value, rest, site, mode)
            }
            ChainStep::TupleIndex { expr, node } => {
                let value = self.project_tuple_index_operand(current, expr, node.node.index)?;
                self.lower_field_chain_steps(value, rest, site, mode)
            }
            ChainStep::Call { expr, node } => {
                self.lower_field_chain_call(expr, node, rest, site, mode)
            }
        }
    }

    fn lower_field_read_step(
        &mut self,
        current: Operand,
        expr: &ExprNode,
        field: Ident,
    ) -> Result<Operand, LowerError> {
        let place = self.place_from_operand(current, expr)?;
        if let TypeData::Extern(owner) = self.cx.program.type_data(place.ty)
            && let Some(target) = self.extern_field_read_target(expr.node.id, *owner, field)
        {
            let value =
                self.lower_extern_field_read_from_place(expr.node.id, Some(expr), place, target)?;
            return self.emit_temp(value);
        }
        self.project_field(expr, place, field).map(Operand::Place)
    }

    fn lower_field_chain_call(
        &mut self,
        expr: &ExprNode,
        call: &ast::CallNode,
        rest: &[ChainStep<'_>],
        site: &ExprNode,
        mode: ChainMode,
    ) -> Result<(), LowerError> {
        if matches!(mode, ChainMode::Effect) && rest.is_empty() {
            let value = self.lower_call_rvalue(expr, call)?;
            return self.emit_eval(value);
        }
        let value = self.lower_call_value(expr, call)?;
        self.lower_field_chain_steps(value, rest, site, mode)
    }

    fn lower_field_chain_method_call(
        &mut self,
        receiver: Operand,
        expr: &ExprNode,
        call: &ast::CallNode,
        rest: &[ChainStep<'_>],
        site: &ExprNode,
        mode: ChainMode,
    ) -> Result<(), LowerError> {
        if matches!(mode, ChainMode::Effect) && rest.is_empty() {
            return self.lower_method_call_effect_with_receiver(receiver, expr, call);
        }
        let value = self.lower_method_call_with_receiver(receiver, expr, call)?;
        self.lower_field_chain_steps(value, rest, site, mode)
    }

    fn finish_field_chain(
        &mut self,
        value: Operand,
        site: &ExprNode,
        mode: ChainMode,
    ) -> Result<(), LowerError> {
        match mode {
            ChainMode::Value { result, result_ty } => {
                self.emit_chain_result(value, result, result_ty, site)
            }
            ChainMode::Effect => self.emit_eval(RValue::Use(value)),
        }
    }

    fn skip_field_chain(&mut self, site: &ExprNode, mode: ChainMode) -> Result<(), LowerError> {
        match mode {
            ChainMode::Value { result, result_ty } => {
                let none = self.optional_none(result_ty, site)?;
                self.emit_init(result, RValue::Use(none))
            }
            ChainMode::Effect => Ok(()),
        }
    }

    fn emit_chain_result(
        &mut self,
        value: Operand,
        result: LocalId,
        result_ty: TypeId,
        site: &ExprNode,
    ) -> Result<(), LowerError> {
        let value = if self.operand_ty(&value) == result_ty {
            value
        } else {
            self.optional_some(value, result_ty, site)?
        };
        self.emit_init(result, RValue::Use(value))
    }

    fn place_from_operand(&mut self, value: Operand, site: &ExprNode) -> Result<Place, LowerError> {
        match value {
            Operand::Place(place) => Ok(place),
            Operand::Const(_) => {
                let ty = self.operand_ty(&value);
                match self.emit_typed_temp(ty, RValue::Use(value))? {
                    Operand::Place(place) => Ok(place),
                    Operand::Const(_) => Err(unsupported_expr(site)),
                }
            }
        }
    }

    fn lower_index_step(
        &mut self,
        current: Operand,
        expr: &ExprNode,
        index: &ast::IndexNode,
    ) -> Result<Operand, LowerError> {
        let mut target = self.place_from_operand(current, expr)?;
        if let ExprKind::Range(range) = &index.node.index.node.kind {
            let Some(_) = typing::sequence_elem(&self.cx.program, target.ty) else {
                return Err(unsupported_expr(expr));
            };
            return self.emit_range_list_copy(expr, target, range);
        }
        if let Some((key_ty, value_ty)) = typing::map_kv(&self.cx.program, target.ty) {
            let key = self.lower_value_to(&index.node.index, key_ty, expr)?;
            let ty = self.cx.optional_ty(value_ty);
            return self.emit_typed_temp(
                ty,
                RValue::MapGet {
                    map: target,
                    key,
                    ty,
                },
            );
        }
        if let Some(elem) = typing::index_elem(&self.cx.program, target.ty) {
            let index = self.lower_index_local(&index.node.index)?;
            target.projection.push(crate::air::Projection::Index(index));
            target.ty = elem;
            return Ok(Operand::Place(target));
        }
        Err(unsupported_expr(expr))
    }

    fn lower_dataref_eq(
        &mut self,
        expr: &ExprNode,
        binary: &ast::BinaryNode,
        result_ty: &Type,
    ) -> Result<Option<Operand>, LowerError> {
        if *result_ty != Type::Bool {
            return Ok(None);
        }
        let lhs_ty = self
            .cx
            .lower_ty(&self.lower_expr_ty(binary.node.left.node.id)?)?;
        let rhs_ty = self
            .cx
            .lower_ty(&self.lower_expr_ty(binary.node.right.node.id)?)?;
        if lhs_ty != rhs_ty || !matches!(self.cx.program.type_data(lhs_ty), TypeData::DataRef(_)) {
            return Ok(None);
        }
        let lhs = self.lower_value(&binary.node.left)?;
        let rhs = self.lower_value(&binary.node.right)?;
        let bool_ty = self.cx.lower_ty(&Type::Bool)?;
        self.emit_typed_temp(
            bool_ty,
            RValue::SharedRefEq {
                lhs,
                rhs,
                negated: binary.node.op == BinaryOp::NotEq,
            },
        )
        .map(Some)
        .map_err(|_| unsupported_expr(expr))
    }

    fn lower_unit_enum_eq(
        &mut self,
        binary: &ast::BinaryNode,
        result_ty: &Type,
    ) -> Result<Option<Operand>, LowerError> {
        if *result_ty != Type::Bool {
            return Ok(None);
        }
        let lhs_ty = self
            .cx
            .lower_ty(&self.lower_expr_ty(binary.node.left.node.id)?)?;
        let rhs_ty = self
            .cx
            .lower_ty(&self.lower_expr_ty(binary.node.right.node.id)?)?;
        if lhs_ty != rhs_ty || !self.cx.program.unit_only_enum(lhs_ty) {
            return Ok(None);
        }
        let lhs = self.lower_value(&binary.node.left)?;
        let rhs = self.lower_value(&binary.node.right)?;
        let ty = self.cx.lower_ty(&Type::Bool)?;
        self.emit_temp(RValue::Binary {
            op: binary.node.op,
            lhs,
            rhs,
            ty,
        })
        .map(Some)
    }

    fn lower_coalesce(
        &mut self,
        expr: &ExprNode,
        binary: &ast::BinaryNode,
        result_ty: &Type,
    ) -> Result<Operand, LowerError> {
        let result_ty = self.cx.lower_ty(result_ty)?;
        let subject = self.lower_optional_subject(&binary.node.left, expr)?;
        let inner_ty = subject.inner_ty();
        let optional_ty = subject.optional_ty();
        self.lower_optional_value(
            subject,
            result_ty,
            expr,
            |this, payload| {
                let payload = Operand::Place(this.local_place(payload));
                if result_ty == optional_ty {
                    this.optional_some(payload, result_ty, expr).map(Some)
                } else if result_ty == inner_ty {
                    Ok(Some(payload))
                } else {
                    Err(unsupported_expr(expr))
                }
            },
            |this| {
                this.lower_value_to(&binary.node.right, result_ty, expr)
                    .map(Some)
            },
        )
    }

    fn lower_nil_equality(
        &mut self,
        expr: &ExprNode,
        binary: &ast::BinaryNode,
    ) -> Result<Option<Operand>, LowerError> {
        if !matches!(binary.node.op, BinaryOp::Eq | BinaryOp::NotEq) {
            return Ok(None);
        }
        let left_nil = is_nil_lit(&binary.node.left);
        let right_nil = is_nil_lit(&binary.node.right);
        if left_nil == right_nil {
            return Ok(None);
        }

        let subject_expr = if left_nil {
            &binary.node.right
        } else {
            &binary.node.left
        };
        let subject = self.lower_optional_subject(subject_expr, expr)?;
        let bool_ty = self.cx.lower_ty(&Type::Bool)?;
        let result = self.temp(bool_ty);
        let some_value = binary.node.op == BinaryOp::NotEq;
        let none_value = binary.node.op == BinaryOp::Eq;
        let some_const = self.bool_const(bool_ty, some_value);
        let none_const = self.bool_const(bool_ty, none_value);
        self.emit_optional_match(
            subject,
            None,
            |this, _| this.emit_init(result, RValue::Use(some_const)),
            |this| this.emit_init(result, RValue::Use(none_const)),
        )?;
        Ok(Some(Operand::Place(self.local_place(result))))
    }

    fn bool_const(&mut self, ty: TypeId, value: bool) -> Operand {
        Operand::Const(self.cx.program.alloc_const(ConstData {
            ty,
            value: ConstValue::Bool(value),
        }))
    }

    fn lower_user_cast_conversion(
        &mut self,
        expr: &ExprNode,
        source_expr: &ExprNode,
        instance: &CastFromInstanceKey,
    ) -> Result<Operand, LowerError> {
        let body = BodyInstanceKey::CastFrom(instance.clone());
        let Some(callee) = self.cx.maps.bodies.get(&body).copied() else {
            return Err(LowerError::MissingLoweredCallee {
                body: Box::new(body),
            });
        };
        let params = self.callee_params(&Callee::Function(callee))?;
        let Some(param) = params.get(&self.cx.program, 0) else {
            return Err(unsupported_expr(expr));
        };
        let source = self.lower_expected_value_raw(source_expr, param.ty, expr)?;
        self.emit_temp(RValue::Call {
            callee: Callee::Function(callee),
            args: vec![CallArg::Value(source)],
        })
    }

    fn lower_cast_expr(
        &mut self,
        expr: &ExprNode,
        cast: &crate::span::Spanned<ast::Cast>,
    ) -> Result<Operand, LowerError> {
        self.require_builtin_scalar(expr)?;
        let source_ty = self.lower_expr_ty(cast.node.expr.node.id)?;
        let target_ty = self.lower_expr_ty(expr.node.id)?;
        let source = self.cx.lower_ty(&source_ty)?;
        let target = self.cx.lower_ty(&target_ty)?;
        let value = self.lower_value(&cast.node.expr)?;
        if source == target {
            return Ok(value);
        }
        let primitives = PrimitiveTypes::scan(&self.cx.program);
        if !typing::valid_cast(&primitives, source, target) {
            return Err(unsupported_expr(expr));
        }
        self.emit_temp(RValue::Cast { value, target })
    }

    fn lower_struct_literal(
        &mut self,
        expr: &ExprNode,
        literal: &ast::StructLiteralNode,
    ) -> Result<Operand, LowerError> {
        let ty = self.lower_expr_ty(expr.node.id)?;
        let ty = self.cx.lower_ty(&ty)?;
        self.lower_struct_literal_as(expr, literal, ty)
    }

    fn lower_struct_literal_as(
        &mut self,
        expr: &ExprNode,
        literal: &ast::StructLiteralNode,
        ty: TypeId,
    ) -> Result<Operand, LowerError> {
        match self.cx.program.type_data(ty) {
            TypeData::Aggregate(aggregate) | TypeData::DataRef(aggregate) => {
                self.lower_struct_aggregate_literal(expr, literal, *aggregate, ty)
            }
            TypeData::Enum(enum_id) => self.lower_struct_enum_literal(expr, literal, *enum_id, ty),
            TypeData::Extern(extern_id) => {
                self.lower_struct_extern_literal(expr, literal, *extern_id, ty)
            }
            _ => Err(unsupported_expr(expr)),
        }
    }

    fn lower_map_literal(
        &mut self,
        expr: &ExprNode,
        _literal: &ast::MapLiteralNode,
    ) -> Result<Operand, LowerError> {
        let ty = self.cx.lower_ty(&self.lower_expr_ty(expr.node.id)?)?;
        if !matches!(self.cx.program.type_data(ty), TypeData::Map { .. }) {
            return Err(unsupported_expr(expr));
        }
        self.lower_expected_value(expr, ty, expr)
    }

    fn lower_expected_value(
        &mut self,
        expr: &ExprNode,
        expected: TypeId,
        site: &ExprNode,
    ) -> Result<Operand, LowerError> {
        let cast_site = UserCastSite {
            expr: expr.node.id,
            source: expr.node.id,
        };
        if let Some(instance) = self.facts.user_cast_conversions.get(&cast_site).cloned() {
            let value = self.lower_user_cast_conversion(expr, expr, &instance)?;
            if self.operand_ty(&value) == expected {
                return Ok(value);
            }
            return self.optional_some(value, expected, site);
        }
        self.lower_expected_value_raw(expr, expected, site)
    }

    fn lower_expected_value_raw(
        &mut self,
        expr: &ExprNode,
        expected: TypeId,
        site: &ExprNode,
    ) -> Result<Operand, LowerError> {
        if let TypeData::Optional(inner) = self.cx.program.type_data(expected).clone()
            && matches!(
                expr.node.kind,
                ExprKind::ArrayLiteral(_)
                    | ExprKind::ArrayFill(_)
                    | ExprKind::MapLiteral(_)
                    | ExprKind::Tuple(_)
                    | ExprKind::StructLiteral(_)
            )
        {
            let value = self.lower_expected_value_raw(expr, inner, site)?;
            return self.optional_some(value, expected, site);
        }

        match (&expr.node.kind, self.cx.program.type_data(expected).clone()) {
            (ExprKind::ArrayLiteral(literal), TypeData::List(elem)) => {
                self.lower_array_literal_to(expr, literal, AggregateCtor::List, elem, expected)
            }
            (ExprKind::ArrayLiteral(literal), TypeData::Array { elem, len }) => {
                if len != literal.node.elements.len() {
                    return Err(unsupported_expr(site));
                }
                self.lower_array_literal_to(expr, literal, AggregateCtor::Array, elem, expected)
            }
            (ExprKind::ArrayFill(fill), TypeData::Array { elem, len }) => {
                self.lower_array_fill_to(expr, fill, elem, len, expected)
            }
            (ExprKind::MapLiteral(literal), TypeData::Map { key, value, .. }) => {
                self.lower_map_literal_to(expr, literal, key, value, expected)
            }
            (ExprKind::Tuple(items), TypeData::Tuple(expected_items)) => {
                self.lower_tuple_literal_to(expr, items, expected_items, expected)
            }
            (
                ExprKind::StructLiteral(literal),
                TypeData::Aggregate(aggregate) | TypeData::DataRef(aggregate),
            ) => self.lower_struct_aggregate_literal(expr, literal, aggregate, expected),
            _ => self.lower_value_to(expr, expected, site),
        }
    }

    fn lower_array_literal_to(
        &mut self,
        expr: &ExprNode,
        literal: &ast::ArrayLiteralNode,
        kind: AggregateCtor,
        elem: TypeId,
        ty: TypeId,
    ) -> Result<Operand, LowerError> {
        let fields = literal
            .node
            .elements
            .iter()
            .map(|element| self.lower_expected_value(element, elem, expr))
            .collect::<Result<Vec<_>, _>>()?;
        self.emit_typed_temp(ty, RValue::Aggregate { kind, fields, ty })
    }

    fn lower_array_fill_to(
        &mut self,
        expr: &ExprNode,
        fill: &ast::ArrayFillNode,
        elem: TypeId,
        len: usize,
        ty: TypeId,
    ) -> Result<Operand, LowerError> {
        let value = self.lower_expected_value(&fill.node.value, elem, expr)?;
        self.emit_typed_temp(
            ty,
            RValue::Aggregate {
                kind: AggregateCtor::Array,
                fields: vec![value; len],
                ty,
            },
        )
    }

    fn lower_map_literal_to(
        &mut self,
        expr: &ExprNode,
        literal: &ast::MapLiteralNode,
        key: TypeId,
        value: TypeId,
        ty: TypeId,
    ) -> Result<Operand, LowerError> {
        let mut fields = vec![];
        for (key_expr, value_expr) in &literal.node.entries {
            fields.push(self.lower_expected_value(key_expr, key, expr)?);
            fields.push(self.lower_expected_value(value_expr, value, expr)?);
        }
        self.emit_typed_temp(
            ty,
            RValue::Aggregate {
                kind: AggregateCtor::Map,
                fields,
                ty,
            },
        )
    }

    fn lower_tuple_literal_to(
        &mut self,
        expr: &ExprNode,
        items: &[ExprNode],
        expected: Vec<TypeId>,
        ty: TypeId,
    ) -> Result<Operand, LowerError> {
        if expected.len() != items.len() {
            return Err(unsupported_expr(expr));
        }
        let fields = items
            .iter()
            .zip(expected)
            .map(|(item, ty)| self.lower_expected_value(item, ty, expr))
            .collect::<Result<Vec<_>, _>>()?;
        self.emit_typed_temp(
            ty,
            RValue::Aggregate {
                kind: AggregateCtor::Tuple,
                fields,
                ty,
            },
        )
    }

    fn lower_value_to(
        &mut self,
        expr: &ExprNode,
        expected: TypeId,
        site: &ExprNode,
    ) -> Result<Operand, LowerError> {
        if let ExprKind::Lit(lit) = &expr.node.kind {
            if matches!(lit, Lit::Nil) {
                return self.optional_none(expected, site);
            }
            if let Some(value) =
                Self::literal_air_const_value(lit, self.cx.program.type_data(expected))
            {
                return Ok(Operand::Const(self.cx.program.alloc_const(ConstData {
                    ty: expected,
                    value,
                })));
            }
        }

        let value = self.lower_value(expr)?;
        if self.operand_ty(&value) == expected {
            return Ok(value);
        }
        self.optional_some(value, expected, site)
    }

    fn optional_none(&mut self, ty: TypeId, site: &ExprNode) -> Result<Operand, LowerError> {
        if typing::optional_inner(&self.cx.program, ty).is_none() {
            return Err(unsupported_expr(site));
        }
        Ok(Operand::Const(self.cx.program.alloc_const(ConstData {
            ty,
            value: ConstValue::Nil,
        })))
    }

    fn optional_some(
        &mut self,
        value: Operand,
        ty: TypeId,
        site: &ExprNode,
    ) -> Result<Operand, LowerError> {
        let Some(inner) = typing::optional_inner(&self.cx.program, ty) else {
            return Err(unsupported_expr(site));
        };
        if self.operand_ty(&value) != inner {
            return Err(unsupported_expr(site));
        }
        self.emit_typed_temp(ty, RValue::OptionalSome { value, ty })
    }

    fn optional_subject_from_operand(
        &mut self,
        operand: Operand,
        site: &ExprNode,
    ) -> Result<OptionalSubject, LowerError> {
        let optional_ty = self.operand_ty(&operand);
        let Some(inner_ty) = typing::optional_inner(&self.cx.program, optional_ty) else {
            return Err(unsupported_expr(site));
        };
        let place = self.place_from_operand(operand, site)?;
        Ok(OptionalSubject::Place {
            place,
            optional_ty,
            inner_ty,
        })
    }

    fn lower_optional_subject(
        &mut self,
        expr: &ExprNode,
        site: &ExprNode,
    ) -> Result<OptionalSubject, LowerError> {
        let operand = self.lower_value(expr)?;
        self.optional_subject_from_operand(operand, site)
    }

    fn lower_optional_pattern_subject(
        &mut self,
        expr: &ExprNode,
        site: &ExprNode,
        alias: bool,
    ) -> Result<OptionalSubject, LowerError> {
        if !alias {
            return self.lower_optional_subject(expr, site);
        }
        if let Some(subject) = self.lower_map_entry_pattern_subject(expr)? {
            return Ok(subject);
        }
        let fact = self.local_use(expr, LocalUseMode::MutBorrow)?;
        let place = self.lower_place(expr, &fact)?;
        let optional_ty = place.ty;
        let Some(inner_ty) = typing::optional_inner(&self.cx.program, optional_ty) else {
            return Err(unsupported_expr(site));
        };
        Ok(OptionalSubject::Place {
            place: place.clone(),
            optional_ty,
            inner_ty,
        })
    }

    fn lower_map_entry_pattern_subject(
        &mut self,
        expr: &ExprNode,
    ) -> Result<Option<OptionalSubject>, LowerError> {
        let ExprKind::Index(index) = &expr.node.kind else {
            return Ok(None);
        };
        if matches!(index.node.index.node.kind, ExprKind::Range(_)) {
            return Ok(None);
        }
        let map = self.lower_place_arg(&index.node.target, true)?;
        let Some((key_ty, inner_ty)) = typing::map_kv(&self.cx.program, map.ty) else {
            return Ok(None);
        };
        let key = self.lower_value_to(&index.node.index, key_ty, expr)?;
        Ok(Some(OptionalSubject::MapEntry {
            map,
            key,
            optional_ty: self.cx.optional_ty(inner_ty),
            inner_ty,
        }))
    }

    fn emit_optional_match(
        &mut self,
        subject: OptionalSubject,
        payload: Option<LocalId>,
        some: impl FnOnce(&mut Self, Option<LocalId>) -> Result<(), LowerError>,
        none: impl FnOnce(&mut Self) -> Result<(), LowerError>,
    ) -> Result<(), LowerError> {
        self.emit_optional_match_with_payload_ref(subject, payload, false, some, none)
    }

    fn emit_optional_match_with_payload_ref(
        &mut self,
        subject: OptionalSubject,
        payload: Option<LocalId>,
        payload_ref: bool,
        some: impl FnOnce(&mut Self, Option<LocalId>) -> Result<(), LowerError>,
        none: impl FnOnce(&mut Self) -> Result<(), LowerError>,
    ) -> Result<(), LowerError> {
        let some_block = self.with_nested_block(|this| some(this, payload))?;
        let none_block = self.with_nested_block(none)?;
        let (some_falls, none_falls) =
            self.push_optional_match(subject, payload, payload_ref, false, some_block, none_block)?;
        if !some_falls && !none_falls {
            self.terminated = true;
            self.block.tail = AirTail::Unreachable;
        }
        Ok(())
    }

    fn push_optional_match(
        &mut self,
        subject: OptionalSubject,
        payload: Option<LocalId>,
        payload_ref: bool,
        payload_escapes: bool,
        some_block: AirBlock,
        none_block: AirBlock,
    ) -> Result<(bool, bool), LowerError> {
        let some_falls = air_block_falls_through(&some_block);
        let none_falls = air_block_falls_through(&none_block);
        self.ensure_open()?;
        match subject {
            OptionalSubject::Place { place, .. } => {
                self.block
                    .stmts
                    .push(AirStmt::OptionalMatch(AirOptionalMatch {
                        discr: place,
                        payload,
                        payload_ref,
                        payload_escapes,
                        some_block,
                        none_block,
                    }));
            }
            OptionalSubject::MapEntry { map, key, .. } => {
                self.block
                    .stmts
                    .push(AirStmt::MapEntryMatch(AirMapEntryMatch {
                        map,
                        key,
                        payload,
                        payload_escapes,
                        some_block,
                        none_block,
                    }));
            }
        }
        Ok((some_falls, none_falls))
    }

    fn lower_optional_value(
        &mut self,
        subject: OptionalSubject,
        result_ty: TypeId,
        site: &ExprNode,
        some: impl FnOnce(&mut Self, LocalId) -> Result<Option<Operand>, LowerError>,
        none: impl FnOnce(&mut Self) -> Result<Option<Operand>, LowerError>,
    ) -> Result<Operand, LowerError> {
        let result = self.temp(result_ty);
        let payload = self.temp(subject.inner_ty());
        self.emit_optional_match(
            subject,
            Some(payload),
            |this, payload| {
                let payload = payload.expect("payload local");
                if let Some(value) = some(this, payload)? {
                    this.emit_init(result, RValue::Use(value))?;
                }
                Ok(())
            },
            |this| {
                if let Some(value) = none(this)? {
                    this.emit_init(result, RValue::Use(value))?;
                }
                Ok(())
            },
        )?;
        if self.terminated {
            return Err(unsupported_expr(site));
        }
        Ok(Operand::Place(self.local_place(result)))
    }

    fn lower_index_value(
        &mut self,
        expr: &ExprNode,
        index: &ast::IndexNode,
    ) -> Result<Operand, LowerError> {
        if let Some(value) = self.lower_range_index_value(expr, index)? {
            return Ok(value);
        }

        let target = self.lower_place_or_temp(&index.node.target, false)?;
        if let Some((key_ty, value_ty)) = typing::map_kv(&self.cx.program, target.ty) {
            let key = self.lower_value_to(&index.node.index, key_ty, expr)?;
            let ty = self.cx.optional_ty(value_ty);
            return self.emit_typed_temp(
                ty,
                RValue::MapGet {
                    map: target,
                    key,
                    ty,
                },
            );
        }
        if let Some(elem) = typing::index_elem(&self.cx.program, target.ty) {
            let index = self.lower_index_local(&index.node.index)?;
            let mut place = target;
            place.projection.push(crate::air::Projection::Index(index));
            place.ty = elem;
            return Ok(Operand::Place(place));
        }
        self.lower_place_arg(expr, false).map(Operand::Place)
    }

    fn lower_range_index_value(
        &mut self,
        expr: &ExprNode,
        index: &ast::IndexNode,
    ) -> Result<Option<Operand>, LowerError> {
        let ExprKind::Range(range) = &index.node.index.node.kind else {
            return Ok(None);
        };
        let source = self.lower_place_or_temp(&index.node.target, false)?;
        let Some(_) = typing::sequence_elem(&self.cx.program, source.ty) else {
            return Ok(None);
        };
        self.emit_range_list_copy(expr, source, range).map(Some)
    }

    fn emit_range_list_copy(
        &mut self,
        expr: &ExprNode,
        source: Place,
        range: &ast::RangeNode,
    ) -> Result<Operand, LowerError> {
        let (start, end, inclusive) = self.lower_range_bounds(range, &source)?;
        let ty = self.cx.lower_ty(&self.lower_expr_ty(expr.node.id)?)?;
        self.emit_typed_temp(
            ty,
            RValue::RangeListCopy {
                source,
                start,
                end,
                inclusive,
                ty,
            },
        )
    }

    fn lower_range_bounds(
        &mut self,
        range: &ast::RangeNode,
        source: &Place,
    ) -> Result<(LocalId, LocalId, bool), LowerError> {
        match &range.node {
            ast::Range::Bounded {
                start,
                end,
                inclusive,
            } => Ok((
                self.lower_index_local(start)?,
                self.lower_index_local(end)?,
                *inclusive,
            )),
            ast::Range::From { start } => {
                let start = self.lower_index_local(start)?;
                let end = self.len_local(source)?;
                Ok((start, end, false))
            }
            ast::Range::To { end, inclusive } => {
                let start = self.int_local(0)?;
                let end = self.lower_index_local(end)?;
                Ok((start, end, *inclusive))
            }
        }
    }

    fn len_local(&mut self, source: &Place) -> Result<LocalId, LowerError> {
        let operand = self.emit_temp(RValue::Len {
            source: source.clone(),
        })?;
        Self::local_from_operand(operand)
    }

    fn int_local(&mut self, value: i64) -> Result<LocalId, LowerError> {
        let int_ty = self.cx.lower_ty(&Type::Int)?;
        let value = self.int_const(value)?;
        let operand = self.emit_typed_temp(int_ty, RValue::Use(value))?;
        Self::local_from_operand(operand)
    }

    fn local_from_operand(operand: Operand) -> Result<LocalId, LowerError> {
        match operand {
            Operand::Place(place) if place.projection.is_empty() => {
                place.root.local().ok_or(LowerError::UnsupportedStmt {
                    kind: "local",
                    span: None,
                })
            }
            Operand::Place(_) | Operand::Const(_) => Err(LowerError::UnsupportedStmt {
                kind: "local",
                span: None,
            }),
        }
    }

    fn lower_array_fill(
        &mut self,
        expr: &ExprNode,
        _fill: &ast::ArrayFillNode,
    ) -> Result<Operand, LowerError> {
        let ty = self.cx.lower_ty(&self.lower_expr_ty(expr.node.id)?)?;
        if !matches!(self.cx.program.type_data(ty), TypeData::Array { .. }) {
            return Err(unsupported_expr(expr));
        }
        self.lower_expected_value(expr, ty, expr)
    }

    fn lower_tuple_literal(
        &mut self,
        expr: &ExprNode,
        _elems: &[ExprNode],
    ) -> Result<Operand, LowerError> {
        let ty = self.cx.lower_ty(&self.lower_expr_ty(expr.node.id)?)?;
        if !matches!(self.cx.program.type_data(ty), TypeData::Tuple(_)) {
            return Err(unsupported_expr(expr));
        }
        self.lower_expected_value(expr, ty, expr)
    }

    fn lower_array_literal(
        &mut self,
        expr: &ExprNode,
        _literal: &ast::ArrayLiteralNode,
    ) -> Result<Operand, LowerError> {
        let ty = self.cx.lower_ty(&self.lower_expr_ty(expr.node.id)?)?;
        if !matches!(
            self.cx.program.type_data(ty),
            TypeData::Array { .. } | TypeData::List(_)
        ) {
            return Err(unsupported_expr(expr));
        }
        self.lower_expected_value(expr, ty, expr)
    }

    fn lower_struct_aggregate_literal(
        &mut self,
        expr: &ExprNode,
        literal: &ast::StructLiteralNode,
        aggregate: crate::air::AggregateId,
        ty_id: TypeId,
    ) -> Result<Operand, LowerError> {
        let decl = self.cx.program.aggregate(aggregate);
        if !matches!(decl.kind, AggregateKind::Struct | AggregateKind::DataRef) {
            return Err(unsupported_expr(expr));
        }
        let kind = match decl.kind {
            AggregateKind::Struct => AggregateCtor::Struct(aggregate),
            AggregateKind::DataRef => AggregateCtor::DataRef(aggregate),
        };
        let fields = self.lower_aggregate_fields_with_defaults(expr, literal, aggregate, ty_id)?;
        self.emit_typed_temp(
            ty_id,
            RValue::Aggregate {
                kind,
                fields,
                ty: ty_id,
            },
        )
    }

    fn lower_aggregate_fields_with_defaults(
        &mut self,
        expr: &ExprNode,
        literal: &ast::StructLiteralNode,
        aggregate: crate::air::AggregateId,
        ty_id: TypeId,
    ) -> Result<Vec<Operand>, LowerError> {
        let decl = self.cx.program.aggregate(aggregate);
        let expected = decl
            .fields
            .iter()
            .enumerate()
            .map(|(slot, field)| (slot, field.name, field.ty))
            .collect::<Vec<_>>();
        let mut values = HashMap::new();
        for (name, field_expr) in &literal.node.fields {
            let known = expected.iter().any(|(_, field, _)| field == name);
            if !known || values.insert(*name, field_expr).is_some() {
                return Err(unsupported_expr(expr));
            }
        }

        let defaults = self
            .facts
            .default_fields
            .get(&expr.node.id)
            .cloned()
            .unwrap_or_default();
        let mut default_keys = HashSet::new();
        for default in &defaults {
            let Some((_, _, field_ty)) = expected
                .iter()
                .find(|(slot, name, _)| *slot == default.slot && *name == default.field)
            else {
                return Err(unsupported_expr(expr));
            };
            if values.contains_key(&default.field)
                || !default_keys.insert((default.slot, default.field))
                || nominal_id_for_type(&default.owner) != Some(&default.owner_key.id)
                || self.cx.lower_ty(&default.owner)? != ty_id
                || self.cx.lower_ty(&default.ty)? != *field_ty
            {
                return Err(unsupported_expr(expr));
            }
        }

        let fields = expected
            .into_iter()
            .map(|(slot, name, ty)| {
                if let Some(field_expr) = values.remove(&name) {
                    return self.lower_expected_value(field_expr, ty, expr);
                }
                let Some(default) = defaults
                    .iter()
                    .find(|default| default.field == name && default.slot == slot)
                else {
                    return Err(unsupported_expr(expr));
                };
                self.lower_default_field(expr, default, ty)
            })
            .collect::<Result<Vec<_>, _>>()?;
        debug_assert!(values.is_empty());
        Ok(fields)
    }

    fn lower_default_field(
        &mut self,
        expr: &ExprNode,
        default: &crate::typecheck::DefaultFieldFact,
        ty: TypeId,
    ) -> Result<Operand, LowerError> {
        let Some(default_expr) = self.index.get_default_expr(default.default) else {
            return Err(unsupported_expr(expr));
        };
        let default_expr = (*default_expr).clone();
        self.with_default_facts(default.default, &default.facts_body, |this| {
            this.lower_expected_value(&default_expr, ty, expr)
        })
    }

    fn lower_ordered_fields(
        &mut self,
        expr: &ExprNode,
        fields: &[(Ident, ExprNode)],
        expected: Vec<(Ident, TypeId)>,
    ) -> Result<Vec<Operand>, LowerError> {
        if expected.len() != fields.len() {
            return Err(unsupported_expr(expr));
        }
        let mut values = HashMap::new();
        for (name, field_expr) in fields {
            if values.contains_key(name) {
                return Err(unsupported_expr(expr));
            }
            values.insert(*name, field_expr);
        }
        expected
            .into_iter()
            .map(|(name, ty)| {
                let Some(field_expr) = values.remove(&name) else {
                    return Err(unsupported_expr(expr));
                };
                self.lower_expected_value(field_expr, ty, expr)
            })
            .collect()
    }

    fn lower_struct_extern_literal(
        &mut self,
        expr: &ExprNode,
        literal: &ast::StructLiteralNode,
        extern_id: crate::air::ExternTypeId,
        ty_id: TypeId,
    ) -> Result<Operand, LowerError> {
        let decl = self.cx.program.extern_type(extern_id);
        if decl.required_init_fields().is_none() {
            return Err(unsupported_expr(expr));
        }
        let init_args = decl
            .init_args
            .iter()
            .filter_map(|arg| {
                decl.fields
                    .get(arg.field.index())
                    .map(|field| (field.name, arg.param, arg.presence))
            })
            .collect::<Vec<_>>();
        let init_field_names = init_args
            .iter()
            .map(|(name, _, _)| *name)
            .collect::<Vec<_>>();
        let field_types = literal
            .node
            .fields
            .iter()
            .map(|(name, field_expr)| {
                let field = decl
                    .fields
                    .iter()
                    .find(|field| field.name == *name)
                    .ok_or_else(|| unsupported_expr(field_expr))?;
                Ok((*name, field.ty, field_expr))
            })
            .collect::<Result<Vec<_>, LowerError>>()?;

        let mut lowered = vec![];
        for (name, field_ty, field_expr) in field_types {
            let value = self.lower_value_to(field_expr, field_ty, field_expr)?;
            let value = self.emit_typed_temp(field_ty, RValue::Use(value))?;
            lowered.push((name, value));
        }

        let init = self.lower_extern_literal_init(expr, &init_args, &lowered)?;
        let has_overrides = lowered
            .iter()
            .any(|(name, _)| !init_field_names.iter().any(|init| init == name));
        if !has_overrides {
            return self.emit_typed_temp(ty_id, init);
        }

        let local = self.push_local(None, None, ty_id, AirMutability::Mutable, LocalKind::Temp);
        self.emit_init(local, init)?;
        let place = self.local_place(local);
        for (name, value) in lowered {
            if init_field_names.contains(&name) {
                continue;
            }
            if let Some(target) = self.extern_field_write_target(expr.node.id, name) {
                let write =
                    self.lower_extern_field_write_operand(expr, place.clone(), value, target)?;
                self.emit_eval(write)?;
            } else {
                let dst = self.extern_direct_field_place(expr, extern_id, place.clone(), name)?;
                self.emit_assign(dst, RValue::Use(value))?;
            }
        }
        Ok(Operand::Place(place))
    }

    fn lower_struct_enum_literal(
        &mut self,
        expr: &ExprNode,
        literal: &ast::StructLiteralNode,
        enum_id: crate::air::EnumId,
        ty_id: TypeId,
    ) -> Result<Operand, LowerError> {
        let Some((variant, expected)) = self.enum_struct_variant(enum_id, literal.node.name) else {
            return Err(unsupported_expr(expr));
        };
        let fields = self.lower_ordered_fields(expr, &literal.node.fields, expected)?;
        self.emit_enum_variant(ty_id, enum_id, variant, fields)
    }

    fn lower_inferred_enum(
        &mut self,
        expr: &ExprNode,
        inferred: &ast::InferredEnumNode,
    ) -> Result<Operand, LowerError> {
        let ty = self.lower_expr_ty(expr.node.id)?;
        let ty_id = self.cx.lower_ty(&ty)?;
        let enum_id = match self.cx.program.type_data(ty_id) {
            TypeData::Enum(enum_id) => *enum_id,
            _ => return Err(unsupported_expr(expr)),
        };
        let Some(variant) = self.enum_variant_id(enum_id, inferred.node.variant) else {
            return Err(unsupported_expr(expr));
        };
        match &inferred.node.args {
            ast::InferredEnumArgs::Unit => self.emit_enum_variant(ty_id, enum_id, variant, vec![]),
            ast::InferredEnumArgs::Tuple(args) => {
                let VariantShape::Tuple(expected) =
                    &self.cx.program.enum_decl(enum_id).variants[variant.index()].shape
                else {
                    return Err(unsupported_expr(expr));
                };
                if expected.len() != args.len() {
                    return Err(unsupported_expr(expr));
                }
                let expected = expected.clone();
                let fields = args
                    .iter()
                    .zip(expected)
                    .map(|(arg, ty)| self.lower_value_to(arg, ty, expr))
                    .collect::<Result<Vec<_>, _>>()?;
                self.emit_enum_variant(ty_id, enum_id, variant, fields)
            }
            ast::InferredEnumArgs::Struct(args) => {
                let Some((_, expected)) = self.enum_struct_variant(enum_id, inferred.node.variant)
                else {
                    return Err(unsupported_expr(expr));
                };
                let fields = self.lower_ordered_fields(expr, args, expected)?;
                self.emit_enum_variant(ty_id, enum_id, variant, fields)
            }
        }
    }

    fn lower_qualified_unit_enum(
        &mut self,
        expr: &ExprNode,
    ) -> Result<Option<Operand>, LowerError> {
        let ExprKind::Field(field) = &expr.node.kind else {
            return Ok(None);
        };
        let ty = self.lower_expr_ty(expr.node.id)?;
        let ty_id = self.cx.lower_ty(&ty)?;
        let enum_id = match self.cx.program.type_data(ty_id) {
            TypeData::Enum(enum_id) => *enum_id,
            _ => return Ok(None),
        };
        let Some(variant) = self.enum_variant_id(enum_id, field.node.field) else {
            return Err(unsupported_expr(expr));
        };
        Ok(Some(self.emit_enum_variant(
            ty_id,
            enum_id,
            variant,
            vec![],
        )?))
    }

    fn emit_enum_variant(
        &mut self,
        ty: TypeId,
        enum_id: crate::air::EnumId,
        variant: crate::air::VariantId,
        fields: Vec<Operand>,
    ) -> Result<Operand, LowerError> {
        self.emit_typed_temp(
            ty,
            RValue::Aggregate {
                kind: AggregateCtor::EnumVariant { enum_id, variant },
                fields,
                ty,
            },
        )
    }

    fn enum_variant_id(
        &self,
        enum_id: crate::air::EnumId,
        name: Ident,
    ) -> Option<crate::air::VariantId> {
        self.cx
            .program
            .enum_decl(enum_id)
            .variants
            .iter()
            .position(|variant| variant.name == name)
            .map(crate::air::VariantId::from_index)
    }

    fn enum_struct_variant(
        &self,
        enum_id: crate::air::EnumId,
        name: Ident,
    ) -> Option<(crate::air::VariantId, Vec<(Ident, TypeId)>)> {
        let decl = self.cx.program.enum_decl(enum_id);
        let (index, variant) = decl
            .variants
            .iter()
            .enumerate()
            .find(|(_, variant)| variant.name == name)?;
        let VariantShape::Struct(fields) = &variant.shape else {
            return None;
        };
        Some((
            crate::air::VariantId::from_index(index),
            fields.iter().map(|field| (field.name, field.ty)).collect(),
        ))
    }

    fn lower_if_value(
        &mut self,
        expr: &ExprNode,
        if_expr: &ast::IfNode,
    ) -> Result<Operand, LowerError> {
        let Some(else_block) = &if_expr.node.else_block else {
            return Err(unsupported_expr(expr));
        };
        let result_ty = match self.lower_expr_ty(expr.node.id)? {
            Type::Void => None,
            ty => Some(self.cx.lower_ty(&ty)?),
        };
        let result = result_ty.map(|ty| self.temp(ty));
        let cond = self.lower_if_cond(&if_expr.node.cond)?;
        let then_block = self.lower_nested_branch_value(&if_expr.node.then_block, expr, result)?;
        let else_block = self.lower_nested_branch_value(else_block, expr, result)?;
        let then_falls = air_block_falls_through(&then_block);
        let else_falls = air_block_falls_through(&else_block);
        self.ensure_open()?;
        self.block.stmts.push(AirStmt::If(AirIf {
            cond,
            then_block,
            else_block: Some(else_block),
        }));
        if !then_falls && !else_falls {
            self.terminate(AirTail::Unreachable)?;
            return self.dummy_operand(self.function.signature.return_type());
        }
        let Some(result) = result else {
            return Err(unsupported_expr(expr));
        };
        Ok(self.operand_place(result))
    }

    fn lower_match_value(
        &mut self,
        expr: &ExprNode,
        match_expr: &ast::MatchNode,
    ) -> Result<Operand, LowerError> {
        if matches!(match_expr.node.mode, ast::MatchMode::Dynamic) {
            let plan = self.checked_dyn_match_plan(expr)?.clone();
            let result_ty = match self.lower_expr_ty(expr.node.id)? {
                Type::Void => return Err(unsupported_expr(expr)),
                ty => self.cx.lower_ty(&ty)?,
            };
            let result = self.temp(result_ty);
            return self.lower_dyn_match_value(expr, match_expr, &plan, result, result_ty);
        }
        let plan = self.checked_match_plan(expr)?;
        if match_expr.node.access.is_ref()
            && self
                .facts
                .dyn_downcasts
                .get(&match_expr.node.scrutinee.node.id)
                .is_some_and(|fact| fact.mutable)
        {
            let result_ty = match self.lower_expr_ty(expr.node.id)? {
                Type::Void => return Err(unsupported_expr(expr)),
                ty => self.cx.lower_ty(&ty)?,
            };
            let result = self.temp(result_ty);
            self.lower_dyn_downcast_alias_match(
                expr,
                match_expr,
                &plan,
                MatchOutput::Value { result, result_ty },
            )?;
            if self.terminated {
                return self.dummy_operand(result_ty);
            }
            return Ok(self.operand_place(result));
        }
        if match_expr.node.access.is_ref() && self.is_optional_expr(&match_expr.node.scrutinee)? {
            let result_ty = self.cx.lower_ty(&self.lower_expr_ty(expr.node.id)?)?;
            let result = self.temp(result_ty);
            return self.lower_optional_match_value(expr, match_expr, &plan, result, result_ty);
        }
        let result_ty = match self.lower_expr_ty(expr.node.id)? {
            Type::Void => return Err(unsupported_expr(expr)),
            ty => self.cx.lower_ty(&ty)?,
        };
        let result = self.temp(result_ty);
        self.lower_pattern_match_value(expr, match_expr, &plan, result, result_ty)
    }

    fn lower_optional_match_effect(
        &mut self,
        expr: &ExprNode,
        match_expr: &ast::MatchNode,
        checked: &CheckedMatchPlan,
    ) -> Result<(), LowerError> {
        let alias = match_expr.node.access.is_ref();
        let subject =
            self.lower_optional_pattern_subject(&match_expr.node.scrutinee, expr, alias)?;
        let plan = optional_match_plan(expr, &match_expr.node.arms, checked)?;
        let mode = optional_match_payload_mode(&plan, alias);
        let payload = mode.needs_payload().then(|| self.temp(subject.inner_ty()));
        let (some_block, none_block) = self.lower_optional_match_blocks(
            &plan,
            subject.place(),
            expr,
            alias,
            payload,
            MatchOutput::Effect,
        )?;
        let (some_falls, none_falls) = self.push_optional_match(
            subject,
            payload,
            mode.payload_ref(),
            false,
            some_block,
            none_block,
        )?;
        if !some_falls && !none_falls {
            self.terminate(AirTail::Unreachable)?;
        }
        Ok(())
    }

    fn lower_optional_match_value(
        &mut self,
        expr: &ExprNode,
        match_expr: &ast::MatchNode,
        checked: &CheckedMatchPlan,
        result: LocalId,
        result_ty: TypeId,
    ) -> Result<Operand, LowerError> {
        let alias = match_expr.node.access.is_ref();
        let subject =
            self.lower_optional_pattern_subject(&match_expr.node.scrutinee, expr, alias)?;
        let plan = optional_match_plan(expr, &match_expr.node.arms, checked)?;
        let mode = optional_match_payload_mode(&plan, alias);
        let payload = mode.needs_payload().then(|| self.temp(subject.inner_ty()));
        let (some_block, none_block) = self.lower_optional_match_blocks(
            &plan,
            subject.place(),
            expr,
            alias,
            payload,
            MatchOutput::Value { result, result_ty },
        )?;
        let (some_falls, none_falls) = self.push_optional_match(
            subject,
            payload,
            mode.payload_ref(),
            false,
            some_block,
            none_block,
        )?;
        if !some_falls && !none_falls {
            self.terminate(AirTail::Unreachable)?;
            return self.dummy_operand(self.function.signature.return_type());
        }
        Ok(self.operand_place(result))
    }

    fn lower_optional_match_blocks(
        &mut self,
        plan: &OptionalMatchPlan<'_>,
        subject: Option<&Place>,
        site: &ExprNode,
        alias: bool,
        payload: Option<LocalId>,
        output: MatchOutput,
    ) -> Result<(AirBlock, AirBlock), LowerError> {
        let some_block = self.with_nested_block(|this| {
            if let Some((pattern, body)) = plan.some {
                if optional_plan_arm_is_default(plan, pattern, body) {
                    this.lower_optional_default_binding(
                        pattern,
                        Operand::Place(subject.ok_or_else(|| unsupported_expr(site))?.clone()),
                        alias,
                    )?;
                } else {
                    this.lower_optional_payload_binding(pattern, payload, alias)?;
                }
                this.lower_match_body(body, output)
            } else if let Some((pattern, body)) = plan.default {
                this.lower_optional_default_binding(
                    pattern,
                    Operand::Place(subject.ok_or_else(|| unsupported_expr(site))?.clone()),
                    alias,
                )?;
                this.lower_match_body(body, output)
            } else {
                this.terminate(AirTail::Unreachable)
            }
        })?;
        let none_block = self.with_nested_block(|this| {
            if let Some(body) = plan.none {
                this.lower_match_body(body, output)
            } else if let Some((pattern, body)) = plan.default {
                this.lower_optional_default_binding(
                    pattern,
                    Operand::Place(subject.ok_or_else(|| unsupported_expr(site))?.clone()),
                    alias,
                )?;
                this.lower_match_body(body, output)
            } else {
                this.terminate(AirTail::Unreachable)
            }
        })?;
        Ok((some_block, none_block))
    }

    fn lower_match_body(&mut self, body: &ExprNode, output: MatchOutput) -> Result<(), LowerError> {
        match output {
            MatchOutput::Effect => self.lower_effect(body),
            MatchOutput::Value { result, result_ty } => {
                self.lower_match_result_body(body, result, result_ty)
            }
        }
    }

    fn lower_match_result_body(
        &mut self,
        body: &ExprNode,
        result: LocalId,
        result_ty: TypeId,
    ) -> Result<(), LowerError> {
        let value = self.lower_value_to(body, result_ty, body)?;
        if !self.terminated {
            self.emit_init(result, RValue::Use(value))?;
        }
        Ok(())
    }

    fn lower_optional_default_binding(
        &mut self,
        pattern: &ast::PatternNode,
        subject: Operand,
        alias: bool,
    ) -> Result<(), LowerError> {
        match &pattern.node {
            Pattern::Wildcard => Ok(()),
            Pattern::Ident(_) if alias => self.lower_pattern_alias_binding(pattern, subject),
            Pattern::Ident(_) => self.lower_pattern_binding(pattern, subject),
            _ => Err(unsupported_pattern_stmt(pattern)),
        }
    }

    fn checked_match_plan(&self, expr: &ExprNode) -> Result<CheckedMatchPlan, LowerError> {
        self.facts
            .match_patterns
            .get(&expr.node.id)
            .cloned()
            .ok_or(LowerError::MissingTypecheckFacts)
    }

    fn checked_dyn_match_plan(&self, expr: &ExprNode) -> Result<&CheckedDynMatchPlan, LowerError> {
        self.facts
            .dyn_matches
            .get(&expr.node.id)
            .ok_or(LowerError::MissingTypecheckFacts)
    }

    fn is_optional_expr(&mut self, expr: &ExprNode) -> Result<bool, LowerError> {
        let ty = self.cx.lower_ty(&self.lower_expr_ty(expr.node.id)?)?;
        Ok(typing::optional_inner(&self.cx.program, ty).is_some())
    }

    fn lower_pattern_subject(
        &mut self,
        owner: &ExprNode,
        scrutinee: &ExprNode,
        access: CheckedMatchAccess,
    ) -> Result<Place, LowerError> {
        let subject = match access {
            CheckedMatchAccess::Owned => self.lower_value(scrutinee)?,
            CheckedMatchAccess::RefAlias => {
                let fact = self.local_use(scrutinee, LocalUseMode::MutBorrow)?;
                Operand::Place(self.lower_place(scrutinee, &fact)?)
            }
        };
        let ty = self.operand_ty(&subject);
        if !matches!(
            self.cx.program.type_data(ty),
            TypeData::Bool
                | TypeData::Int
                | TypeData::Float
                | TypeData::String
                | TypeData::Char
                | TypeData::Optional(_)
                | TypeData::Tuple(_)
                | TypeData::Enum(_)
                | TypeData::Flag(_)
        ) {
            return Err(unsupported_expr(owner));
        }
        match subject {
            Operand::Place(place) => Ok(place),
            Operand::Const(value) => {
                let local = self.temp(ty);
                self.emit_init(local, RValue::Use(Operand::Const(value)))?;
                Ok(self.local_place(local))
            }
        }
    }

    fn lower_pattern_match_effect(
        &mut self,
        expr: &ExprNode,
        match_expr: &ast::MatchNode,
        plan: &CheckedMatchPlan,
    ) -> Result<(), LowerError> {
        let subject = self.lower_pattern_subject(expr, &match_expr.node.scrutinee, plan.access)?;
        let arms = self.lower_pattern_match_arms(expr, match_expr, plan, MatchOutput::Effect)?;
        self.push_pattern_match(subject, arms)
    }

    fn lower_pattern_match_value(
        &mut self,
        expr: &ExprNode,
        match_expr: &ast::MatchNode,
        plan: &CheckedMatchPlan,
        result: LocalId,
        result_ty: TypeId,
    ) -> Result<Operand, LowerError> {
        let subject = self.lower_pattern_subject(expr, &match_expr.node.scrutinee, plan.access)?;
        let arms = self.lower_pattern_match_arms(
            expr,
            match_expr,
            plan,
            MatchOutput::Value { result, result_ty },
        )?;
        self.push_pattern_match(subject, arms)?;
        if self.terminated {
            return self.dummy_operand(result_ty);
        }
        Ok(self.operand_place(result))
    }

    fn lower_dyn_match_effect(
        &mut self,
        owner: &ExprNode,
        match_expr: &ast::MatchNode,
        plan: &CheckedDynMatchPlan,
    ) -> Result<(), LowerError> {
        let match_ = self.lower_dyn_match(owner, match_expr, plan, MatchOutput::Effect)?;
        self.push_dyn_match(match_)
    }

    fn lower_dyn_match_value(
        &mut self,
        owner: &ExprNode,
        match_expr: &ast::MatchNode,
        plan: &CheckedDynMatchPlan,
        result: LocalId,
        result_ty: TypeId,
    ) -> Result<Operand, LowerError> {
        let match_ = self.lower_dyn_match(
            owner,
            match_expr,
            plan,
            MatchOutput::Value { result, result_ty },
        )?;
        self.push_dyn_match(match_)?;
        if self.terminated {
            return self.dummy_operand(result_ty);
        }
        Ok(self.operand_place(result))
    }

    fn lower_dyn_match(
        &mut self,
        owner: &ExprNode,
        match_expr: &ast::MatchNode,
        plan: &CheckedDynMatchPlan,
        output: MatchOutput,
    ) -> Result<AirDynMatch, LowerError> {
        if match_expr.node.arms.len() != plan.arms.len() + 1
            || plan.source != match_expr.node.scrutinee.node.id
        {
            return Err(LowerError::MissingTypecheckFacts);
        }
        let source_ty = self
            .cx
            .lower_ty(&self.lower_expr_ty(match_expr.node.scrutinee.node.id)?)?;
        let TypeData::Dyn(surface) = self.cx.program.type_data(source_ty) else {
            return Err(unsupported_expr(owner));
        };
        let surface = *surface;
        let source = match plan.access {
            CheckedMatchAccess::RefAlias => {
                AirDynMatchSource::Mutable(self.lower_mut_call_arg(&match_expr.node.scrutinee)?)
            }
            CheckedMatchAccess::Owned => {
                let source_fact = self
                    .facts
                    .locals
                    .uses
                    .get(&match_expr.node.scrutinee.node.id)
                    .cloned();
                let source_place = match source_fact {
                    Some(fact) => self.lower_place(&match_expr.node.scrutinee, &fact).ok(),
                    None => None,
                };
                let value = if let Some(place) = source_place {
                    Operand::Place(place)
                } else {
                    self.lower_dynamic_source(&match_expr.node.scrutinee)?
                };
                match value {
                    Operand::Place(place) if matches!(place.root, PlaceRoot::DynBorrowParam(_)) => {
                        AirDynMatchSource::Borrowed(self.dyn_reborrow(
                            &match_expr.node.scrutinee,
                            place,
                            surface,
                        )?)
                    }
                    value => AirDynMatchSource::Owned {
                        use_: self.dyn_owned_use(&value),
                        value,
                    },
                }
            }
        };
        let aliases = matches!(source, AirDynMatchSource::Mutable(_));
        let takes = matches!(
            source,
            AirDynMatchSource::Owned {
                use_: DynOwnedUse::ConsumeTemporary,
                ..
            }
        );
        let mut arms = Vec::with_capacity(plan.arms.len());
        for (ast_arm, checked) in match_expr.node.arms.iter().zip(&plan.arms) {
            let ast::MatchArmHead::DynDowncast(ast_downcast) = &ast_arm.node.head else {
                return Err(unsupported_expr(owner));
            };
            if ast_downcast.node.id != checked.downcast {
                return Err(LowerError::MissingTypecheckFacts);
            }
            let fact = self
                .facts
                .dyn_downcasts
                .get(&checked.downcast)
                .ok_or(LowerError::MissingTypecheckFacts)?;
            let DynDowncastSource::Resolved(fact_surface) = fact.source else {
                return Err(LowerError::MissingTypecheckFacts);
            };
            let fact_surface = self
                .cx
                .maps
                .contract_surfaces
                .get(&fact_surface)
                .copied()
                .ok_or(LowerError::MissingTypecheckFacts)?;
            if fact.source_id != plan.source || fact_surface != surface || fact.mutable != aliases {
                return Err(LowerError::MissingTypecheckFacts);
            }
            let target = self.cx.lower_ty(&fact.target)?;
            let binding = self.lower_dyn_match_binding(
                checked.binding,
                ast_downcast.node.binding,
                target,
                plan.access == CheckedMatchAccess::RefAlias,
            )?;
            let block = match output {
                MatchOutput::Effect => self.lower_nested_expr_effect(&ast_arm.node.body)?,
                MatchOutput::Value { result, .. } => {
                    self.lower_nested_expr_branch_value(&ast_arm.node.body, result)?
                }
            };
            let binding = match (binding, aliases, takes) {
                (Some(local), true, _) => AirDynMatchTargetBinding::Alias(local),
                (Some(local), false, true) => AirDynMatchTargetBinding::Take(local),
                (Some(local), false, false) => AirDynMatchTargetBinding::Materialize(local),
                (None, _, _) => AirDynMatchTargetBinding::Discard,
            };
            arms.push(AirDynMatchArm {
                target,
                binding,
                block,
            });
        }
        let ast_fallback = match_expr
            .node
            .arms
            .last()
            .ok_or(LowerError::MissingTypecheckFacts)?;
        let ast::MatchArmHead::DynFallback(name) = ast_fallback.node.head else {
            return Err(unsupported_expr(owner));
        };
        let binding = self.lower_dyn_match_binding(
            plan.fallback.binding,
            name,
            source_ty,
            plan.access == CheckedMatchAccess::RefAlias,
        )?;
        let block = match output {
            MatchOutput::Effect => self.lower_nested_expr_effect(&ast_fallback.node.body)?,
            MatchOutput::Value { result, .. } => {
                self.lower_nested_expr_branch_value(&ast_fallback.node.body, result)?
            }
        };
        let binding = match (binding, aliases) {
            (Some(local), true) => AirDynMatchFallbackBinding::Alias(local),
            (Some(local), false) => AirDynMatchFallbackBinding::Preserve(local),
            (None, _) => AirDynMatchFallbackBinding::Discard,
        };
        Ok(AirDynMatch {
            source,
            surface,
            arms,
            fallback: AirDynMatchFallback { binding, block },
        })
    }

    fn lower_dyn_match_binding(
        &mut self,
        semantic: Option<CheckedDynMatchBinding>,
        name: Option<Ident>,
        ty: TypeId,
        mutable: bool,
    ) -> Result<Option<LocalId>, LowerError> {
        let Some(binding) = semantic else {
            if name.is_some() {
                return Err(LowerError::MissingTypecheckFacts);
            }
            return Ok(None);
        };
        let name = name.ok_or(LowerError::MissingTypecheckFacts)?;
        if let Some(local) = self.existing_semantic_local(binding.local) {
            return Ok(Some(local));
        }
        let local = self.push_local(
            Some(name),
            Some(air_binding_id(binding.binding_id)),
            ty,
            if mutable {
                AirMutability::Mutable
            } else {
                AirMutability::Immutable
            },
            LocalKind::PatternBinding,
        );
        let place = self.local_place(local);
        self.locals.insert(binding.local, place.clone());
        if self
            .capture_sources
            .insert(
                binding.binding_id,
                LambdaCaptureSource::Local(place.clone()),
            )
            .is_some()
        {
            return Err(LowerError::DuplicateBindingBridge {
                body: Box::new(self.body.clone()),
                binding: binding.binding_id,
            });
        }
        if mutable {
            self.promote_pattern_alias_scoped_borrow(
                binding.local,
                Some(binding.binding_id),
                &place,
            )?;
        }
        Ok(Some(local))
    }

    fn push_dyn_match(&mut self, match_: AirDynMatch) -> Result<(), LowerError> {
        let any_falls = match_
            .arms
            .iter()
            .map(|arm| &arm.block)
            .chain(std::iter::once(&match_.fallback.block))
            .any(air_block_falls_through);
        self.ensure_open()?;
        self.block.stmts.push(AirStmt::DynMatch(match_));
        if !any_falls {
            self.terminate(AirTail::Unreachable)?;
        }
        Ok(())
    }

    fn lower_dyn_downcast_alias_match(
        &mut self,
        owner: &ExprNode,
        match_expr: &ast::MatchNode,
        checked: &CheckedMatchPlan,
        output: MatchOutput,
    ) -> Result<(), LowerError> {
        let fact = self
            .facts
            .dyn_downcasts
            .get(&match_expr.node.scrutinee.node.id)
            .cloned()
            .ok_or(LowerError::MissingTypecheckFacts)?;
        let ExprKind::FailableCast(cast) = &match_expr.node.scrutinee.node.kind else {
            return Err(unsupported_expr(owner));
        };
        let DynDowncastSource::Resolved(surface) = fact.source else {
            return Err(LowerError::MissingTypecheckFacts);
        };
        if fact.source_id != cast.node.expr.node.id || !fact.mutable {
            return Err(LowerError::MissingTypecheckFacts);
        }
        let surface = self
            .cx
            .maps
            .contract_surfaces
            .get(&surface)
            .copied()
            .ok_or(LowerError::MissingTypecheckFacts)?;
        let target = self.cx.lower_ty(&fact.target)?;
        let plan = optional_match_plan(owner, &match_expr.node.arms, checked)?;
        let (_, some_body) = plan.some.ok_or_else(|| unsupported_expr(owner))?;
        let checked_binding = checked
            .arms
            .iter()
            .flat_map(|arm| &arm.bindings.bindings)
            .find(|binding| binding.ty == fact.target)
            .ok_or(LowerError::MissingTypecheckFacts)?;
        let binding = self
            .lower_checked_pattern_binding(checked_binding, AirPatternPath::default())?
            .local;
        let some_block = match output {
            MatchOutput::Effect => self.lower_nested_expr_effect(some_body)?,
            MatchOutput::Value { result, .. } => {
                self.lower_nested_expr_branch_value(some_body, result)?
            }
        };
        let none_body = plan.none.ok_or_else(|| unsupported_expr(owner))?;
        let none_block = match output {
            MatchOutput::Effect => self.lower_nested_expr_effect(none_body)?,
            MatchOutput::Value { result, .. } => {
                self.lower_nested_expr_branch_value(none_body, result)?
            }
        };
        let source = self.lower_mut_call_arg(&cast.node.expr)?;
        self.push_dyn_match(AirDynMatch {
            source: AirDynMatchSource::Mutable(source),
            surface,
            arms: vec![AirDynMatchArm {
                target,
                binding: AirDynMatchTargetBinding::Alias(binding),
                block: some_block,
            }],
            fallback: AirDynMatchFallback {
                binding: AirDynMatchFallbackBinding::Discard,
                block: none_block,
            },
        })
    }

    fn lower_pattern_match_arms(
        &mut self,
        owner: &ExprNode,
        match_expr: &ast::MatchNode,
        plan: &CheckedMatchPlan,
        output: MatchOutput,
    ) -> Result<Vec<AirPatternArm>, LowerError> {
        if match_expr.node.arms.len() != plan.arms.len() {
            return Err(LowerError::MissingTypecheckFacts);
        }
        let mut arms = vec![];
        for (arm, checked) in match_expr.node.arms.iter().zip(&plan.arms) {
            let ast::MatchArmHead::Pattern(_) = &arm.node.head else {
                return Err(unsupported_expr(owner));
            };
            let alternatives = self.lower_pattern_alternatives(owner, checked)?;
            let block = match output {
                MatchOutput::Effect => self.lower_nested_expr_effect(&arm.node.body)?,
                MatchOutput::Value { result, .. } => {
                    self.lower_nested_expr_branch_value(&arm.node.body, result)?
                }
            };
            arms.push(AirPatternArm {
                alternatives,
                block,
            });
        }
        Ok(arms)
    }

    fn lower_pattern_alternatives(
        &mut self,
        owner: &ExprNode,
        arm: &CheckedMatchArm,
    ) -> Result<Vec<AirPatternAlternative>, LowerError> {
        self.lower_checked_pattern_branches(
            owner,
            &arm.pattern,
            &arm.bindings.bindings,
            &AirPatternPath::default(),
            vec![AirPatternAlternative::default()],
        )
    }

    fn lower_checked_pattern_branches(
        &mut self,
        owner: &ExprNode,
        pattern: &CheckedPattern,
        bindings: &[CheckedPatternBinding],
        path: &AirPatternPath,
        alternatives: Vec<AirPatternAlternative>,
    ) -> Result<Vec<AirPatternAlternative>, LowerError> {
        match pattern {
            CheckedPattern::Or(branches) => {
                let mut branch_plans = Vec::with_capacity(branches.len());
                let mut structural = true;
                let mut common_bindings = None;
                for branch in branches {
                    let mut plans = self.lower_checked_pattern_branches(
                        owner,
                        &branch.pattern,
                        &branch.bindings.bindings,
                        path,
                        vec![AirPatternAlternative::default()],
                    )?;
                    if plans.len() != 1
                        || !plans[0]
                            .tests
                            .iter()
                            .all(Self::pattern_disjunction_test_supported)
                    {
                        structural = false;
                    } else if let Some(expected) = &common_bindings {
                        structural &= expected == &plans[0].bindings;
                    } else {
                        common_bindings = Some(plans[0].bindings.clone());
                    }
                    branch_plans.append(&mut plans);
                }
                if structural {
                    let branches: Vec<Vec<AirPatternTest>> =
                        branch_plans.into_iter().map(|plan| plan.tests).collect();
                    let bindings = common_bindings.unwrap_or_default();
                    return Ok(alternatives
                        .into_iter()
                        .map(|mut alternative| {
                            alternative.tests.push(AirPatternTest::Any {
                                branches: branches.clone(),
                            });
                            alternative.bindings.extend(bindings.clone());
                            alternative
                        })
                        .collect());
                }
                let mut lowered = vec![];
                for alternative in alternatives {
                    for branch in branches {
                        lowered.extend(self.lower_checked_pattern_branches(
                            owner,
                            &branch.pattern,
                            &branch.bindings.bindings,
                            path,
                            vec![alternative.clone()],
                        )?);
                    }
                }
                Ok(lowered)
            }
            CheckedPattern::Tuple(fields) => {
                let mut alternatives = alternatives;
                for (index, field) in fields.iter().enumerate() {
                    let mut field_path = path.clone();
                    field_path
                        .steps
                        .push(AirPatternPathStep::TupleField(index as u32));
                    alternatives = self.lower_checked_pattern_branches(
                        owner,
                        field,
                        bindings,
                        &field_path,
                        alternatives,
                    )?;
                }
                Ok(alternatives)
            }
            CheckedPattern::Struct { fields, .. } => {
                let mut alternatives = alternatives;
                for field in fields {
                    let mut field_path = path.clone();
                    field_path
                        .steps
                        .push(AirPatternPathStep::Field(FieldId::from_index(field.slot)));
                    alternatives = self.lower_checked_pattern_branches(
                        owner,
                        &field.pattern,
                        bindings,
                        &field_path,
                        alternatives,
                    )?;
                }
                Ok(alternatives)
            }
            CheckedPattern::OptionalSome(inner) => {
                let mut some_path = path.clone();
                some_path.steps.push(AirPatternPathStep::OptionalSome);
                self.lower_checked_pattern_branches(
                    owner,
                    inner,
                    bindings,
                    &some_path,
                    Self::add_optional_some_test(alternatives, path),
                )
            }
            CheckedPattern::FlagValue {
                owner: flag_owner,
                value,
            } => {
                let ty = self.cx.lower_ty(&flag_owner.ty)?;
                let TypeData::Flag(flag) = self.cx.program.type_data(ty) else {
                    return Err(unsupported_expr(owner));
                };
                Ok(alternatives
                    .into_iter()
                    .map(|mut alternative| {
                        alternative.tests.push(AirPatternTest::FlagValue {
                            path: path.clone(),
                            flag: *flag,
                            bits: *value,
                        });
                        alternative
                    })
                    .collect())
            }
            CheckedPattern::Enum {
                owner: enum_owner,
                variant,
                payload,
            } if self.checked_pattern_owner_is_optional(enum_owner)? => self
                .lower_optional_enum_pattern_branches(
                    owner,
                    *variant,
                    payload,
                    bindings,
                    path,
                    alternatives,
                ),
            CheckedPattern::Enum {
                owner: enum_owner,
                variant,
                payload,
            } => {
                let enum_id = self.checked_pattern_enum_id(owner, enum_owner)?;
                let Some(variant_id) = self.enum_variant_id(enum_id, *variant) else {
                    return Err(unsupported_expr(owner));
                };
                let mut alternatives = alternatives
                    .into_iter()
                    .map(|mut alternative| {
                        alternative.tests.push(AirPatternTest::EnumVariant {
                            path: path.clone(),
                            enum_id,
                            variant: variant_id,
                        });
                        alternative
                    })
                    .collect::<Vec<_>>();
                match payload {
                    CheckedEnumPayload::Unit => Ok(alternatives),
                    CheckedEnumPayload::Tuple(fields) => {
                        for (index, field) in fields.iter().enumerate() {
                            let mut field_path = path.clone();
                            field_path.steps.push(AirPatternPathStep::EnumTupleField {
                                enum_id,
                                variant: variant_id,
                                field: index as u16,
                            });
                            alternatives = self.lower_checked_pattern_branches(
                                owner,
                                field,
                                bindings,
                                &field_path,
                                alternatives,
                            )?;
                        }
                        Ok(alternatives)
                    }
                    CheckedEnumPayload::Struct(fields) => {
                        for field in fields {
                            let mut field_path = path.clone();
                            field_path.steps.push(AirPatternPathStep::EnumStructField {
                                enum_id,
                                variant: variant_id,
                                field: field.slot as u16,
                            });
                            alternatives = self.lower_checked_pattern_branches(
                                owner,
                                &field.pattern,
                                bindings,
                                &field_path,
                                alternatives,
                            )?;
                        }
                        Ok(alternatives)
                    }
                }
            }
            CheckedPattern::Wildcard => Ok(alternatives),
            CheckedPattern::Binding(binding) => {
                let binding = bindings
                    .iter()
                    .find(|candidate| candidate.local == binding.local)
                    .ok_or(LowerError::MissingTypecheckFacts)?;
                alternatives
                    .into_iter()
                    .map(|mut alternative| {
                        alternative
                            .bindings
                            .push(self.lower_checked_pattern_binding(binding, path.clone())?);
                        Ok(alternative)
                    })
                    .collect()
            }
            CheckedPattern::Literal(literal) => alternatives
                .into_iter()
                .map(|mut alternative| {
                    alternative
                        .tests
                        .push(self.lower_literal_pattern_test(literal, path.clone())?);
                    Ok(alternative)
                })
                .collect(),
            CheckedPattern::Nil => Ok(alternatives
                .into_iter()
                .map(|mut alternative| {
                    alternative
                        .tests
                        .push(AirPatternTest::Nil { path: path.clone() });
                    alternative
                })
                .collect()),
            CheckedPattern::Unsupported => Err(LowerError::UnsupportedExpr {
                expr_id: owner.node.id,
                kind: "Match",
            }),
        }
    }

    fn pattern_disjunction_test_supported(test: &AirPatternTest) -> bool {
        match test {
            AirPatternTest::Any { branches } => branches
                .iter()
                .flatten()
                .all(Self::pattern_disjunction_test_supported),
            AirPatternTest::Literal { path, .. } | AirPatternTest::FlagValue { path, .. } => {
                path.steps.iter().all(|step| {
                    matches!(
                        step,
                        AirPatternPathStep::Field(_) | AirPatternPathStep::TupleField(_)
                    )
                })
            }
            AirPatternTest::Nil { .. }
            | AirPatternTest::OptionalSome { .. }
            | AirPatternTest::EnumVariant { .. } => false,
        }
    }

    fn add_optional_some_test(
        alternatives: Vec<AirPatternAlternative>,
        path: &AirPatternPath,
    ) -> Vec<AirPatternAlternative> {
        alternatives
            .into_iter()
            .map(|mut alternative| {
                alternative
                    .tests
                    .push(AirPatternTest::OptionalSome { path: path.clone() });
                alternative
            })
            .collect()
    }

    fn checked_pattern_owner_is_optional(
        &mut self,
        owner: &CheckedPatternOwner,
    ) -> Result<bool, LowerError> {
        let ty = self.cx.lower_ty(&owner.ty)?;
        Ok(matches!(
            self.cx.program.type_data(ty),
            TypeData::Optional(_)
        ))
    }

    fn lower_optional_enum_pattern_branches(
        &mut self,
        owner: &ExprNode,
        variant: Ident,
        payload: &CheckedEnumPayload,
        bindings: &[CheckedPatternBinding],
        path: &AirPatternPath,
        alternatives: Vec<AirPatternAlternative>,
    ) -> Result<Vec<AirPatternAlternative>, LowerError> {
        if variant == Ident::new("None") {
            return Ok(alternatives
                .into_iter()
                .map(|mut alternative| {
                    alternative
                        .tests
                        .push(AirPatternTest::Nil { path: path.clone() });
                    alternative
                })
                .collect());
        }
        if variant != Ident::new("Some") {
            return Err(unsupported_expr(owner));
        }
        let CheckedEnumPayload::Tuple(fields) = payload else {
            return Err(unsupported_expr(owner));
        };
        let [field] = fields.as_slice() else {
            return Err(unsupported_expr(owner));
        };
        let mut some_path = path.clone();
        some_path.steps.push(AirPatternPathStep::OptionalSome);
        self.lower_checked_pattern_branches(
            owner,
            field,
            bindings,
            &some_path,
            Self::add_optional_some_test(alternatives, path),
        )
    }

    fn checked_pattern_enum_id(
        &mut self,
        expr: &ExprNode,
        owner: &CheckedPatternOwner,
    ) -> Result<crate::air::EnumId, LowerError> {
        let ty = self.cx.lower_ty(&owner.ty)?;
        let TypeData::Enum(enum_id) = self.cx.program.type_data(ty) else {
            return Err(unsupported_expr(expr));
        };
        Ok(*enum_id)
    }

    fn lower_literal_pattern_test(
        &mut self,
        literal: &CheckedLiteralPattern,
        path: AirPatternPath,
    ) -> Result<AirPatternTest, LowerError> {
        let ty = self.cx.lower_ty(&literal.ty)?;
        let value = self.cx.program.alloc_const(ConstData {
            ty,
            value: lower_const_specialization_value(&literal.value),
        });
        Ok(AirPatternTest::Literal { path, value })
    }

    fn lower_checked_pattern_binding(
        &mut self,
        binding: &CheckedPatternBinding,
        path: AirPatternPath,
    ) -> Result<AirPatternBinding, LowerError> {
        let ty = self.cx.lower_ty(&binding.ty)?;
        let mode = match &binding.kind {
            CheckedPatternBindingKind::Owned => AirPatternBindingMode::Owned,
            CheckedPatternBindingKind::Alias(_) => AirPatternBindingMode::Alias,
        };
        let local = match self.existing_semantic_local(binding.local) {
            Some(local) => local,
            None => {
                let def = self.local_def(binding.local)?;
                let local = self.push_local(
                    Some(def.name),
                    def.binding_id.map(air_binding_id),
                    ty,
                    match mode {
                        AirPatternBindingMode::Owned if !def.mutable => AirMutability::Immutable,
                        AirPatternBindingMode::Owned | AirPatternBindingMode::Alias => {
                            AirMutability::Mutable
                        }
                    },
                    match mode {
                        AirPatternBindingMode::Owned => LocalKind::User,
                        AirPatternBindingMode::Alias => LocalKind::PatternBinding,
                    },
                );
                let place = self.local_place(local);
                self.locals.insert(binding.local, place.clone());
                self.insert_capture_source(binding.local, place)?;
                local
            }
        };
        Ok(AirPatternBinding {
            local,
            path,
            ty,
            mode,
        })
    }

    fn push_pattern_match(
        &mut self,
        subject: Place,
        arms: Vec<AirPatternArm>,
    ) -> Result<(), LowerError> {
        let any_falls = arms.iter().any(|arm| air_block_falls_through(&arm.block));
        self.ensure_open()?;
        self.block
            .stmts
            .push(AirStmt::PatternMatch(AirPatternMatch { subject, arms }));
        if !any_falls {
            self.terminate(AirTail::Unreachable)?;
        }
        Ok(())
    }

    fn lower_block_branch_value(
        &mut self,
        block: &BlockNode,
        expected: TypeId,
        owner: &ExprNode,
    ) -> Result<Option<Operand>, LowerError> {
        self.lower_stmts(&block.node.stmts)?;
        if self.terminated {
            return Ok(None);
        }
        let Some(tail) = &block.node.tail else {
            return Err(unsupported_expr(owner));
        };
        self.lower_value_to(tail, expected, owner).map(Some)
    }

    fn lower_nested_effect(&mut self, block: &BlockNode) -> Result<AirBlock, LowerError> {
        self.with_nested_block(|this| this.lower_block_effect(block))
    }

    fn lower_nested_expr_effect(&mut self, expr: &ExprNode) -> Result<AirBlock, LowerError> {
        self.with_nested_block(|this| this.lower_effect(expr))
    }

    fn lower_nested_branch_value(
        &mut self,
        block: &BlockNode,
        owner: &ExprNode,
        result: Option<LocalId>,
    ) -> Result<AirBlock, LowerError> {
        self.with_nested_block(|this| {
            let Some(result) = result else {
                return this.lower_block_effect(block);
            };
            let expected = this.function.locals[result.index()].ty;
            if let Some(value) = this.lower_block_branch_value(block, expected, owner)? {
                this.emit_init(result, RValue::Use(value))?;
            }
            Ok(())
        })
    }

    fn lower_nested_expr_branch_value(
        &mut self,
        expr: &ExprNode,
        result: LocalId,
    ) -> Result<AirBlock, LowerError> {
        self.with_nested_block(|this| {
            let value = this.lower_value_to(expr, this.function.locals[result.index()].ty, expr)?;
            if !this.terminated {
                this.emit_init(result, RValue::Use(value))?;
            }
            Ok(())
        })
    }

    fn with_nested_block(
        &mut self,
        lower: impl FnOnce(&mut Self) -> Result<(), LowerError>,
    ) -> Result<AirBlock, LowerError> {
        let outer_block = std::mem::take(&mut self.block);
        let outer_terminated = self.terminated;
        self.terminated = false;
        lower(self)?;
        let nested = std::mem::take(&mut self.block);
        self.block = outer_block;
        self.terminated = outer_terminated;
        Ok(nested)
    }

    fn lower_if_cond(&mut self, expr: &ExprNode) -> Result<Operand, LowerError> {
        let cond = self.lower_value(expr)?;
        if self.terminated || self.operand_type(&cond) != Type::Bool {
            return Err(unsupported_expr(expr));
        }
        Ok(cond)
    }

    fn lower_string_interp(&mut self, parts: &[ast::StringPart]) -> Result<Operand, LowerError> {
        let operands = self.lower_string_interp_parts(parts)?;
        self.emit_string_concat(operands)
    }

    fn lower_string_interp_parts(
        &mut self,
        parts: &[ast::StringPart],
    ) -> Result<Vec<Operand>, LowerError> {
        let mut operands = vec![];
        for part in parts {
            match part {
                ast::StringPart::Text(text) if text.is_empty() => {}
                ast::StringPart::Text(text) => operands.push(self.string_const(text)?),
                ast::StringPart::Expr(expr, Some(spec)) => {
                    let value = if let Some(fact) = self.facts.stringifies.get(&expr.node.id) {
                        let source = fact.source_ty.clone();
                        self.lower_stringify_value(expr, &source)?
                    } else {
                        self.lower_value(expr)?
                    };
                    let string_ty = self.string_ty()?;
                    operands.push(self.emit_typed_temp(
                        string_ty,
                        RValue::Format {
                            value,
                            spec: spec.node,
                        },
                    )?);
                }
                ast::StringPart::Expr(expr, None) => operands.push(self.lower_string_part(expr)?),
            }
        }
        Ok(operands)
    }

    fn lower_string_concat(&mut self, expr: &ExprNode) -> Result<Operand, LowerError> {
        let operands = self.lower_string_concat_parts(expr)?;
        self.emit_string_concat(operands)
    }

    fn lower_binding_string_init(&mut self, expr: &ExprNode) -> Result<Option<RValue>, LowerError> {
        match &expr.node.kind {
            ExprKind::Binary(binary)
                if binary.node.op == BinaryOp::Add
                    && self.lower_expr_ty(expr.node.id)? == Type::String =>
            {
                self.lower_string_concat_parts(expr)
                    .map(|parts| Some(RValue::StringConcat { parts }))
            }
            ExprKind::StringInterp(parts) => self
                .lower_string_interp_parts(parts)
                .map(|parts| Some(RValue::StringConcat { parts })),
            _ => Ok(None),
        }
    }

    fn lower_string_concat_parts(&mut self, expr: &ExprNode) -> Result<Vec<Operand>, LowerError> {
        let mut parts = vec![];
        self.collect_string_concat_parts(expr, &mut parts)?;
        Ok(parts)
    }

    fn collect_string_concat_parts(
        &mut self,
        expr: &ExprNode,
        parts: &mut Vec<Operand>,
    ) -> Result<(), LowerError> {
        if let ExprKind::Binary(binary) = &expr.node.kind
            && binary.node.op == BinaryOp::Add
            && self.lower_expr_ty(expr.node.id)? == Type::String
        {
            self.collect_string_concat_parts(&binary.node.left, parts)?;
            self.collect_string_concat_parts(&binary.node.right, parts)?;
            return Ok(());
        }
        parts.push(self.lower_string_part(expr)?);
        Ok(())
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
        if matches!(source, Type::Func { .. }) {
            return Err(unsupported_expr(arg));
        }
        let source_ty = self.cx.lower_ty(source)?;
        let value = self.lower_value(arg)?;
        let result_ty = self.string_ty()?;
        self.emit_typed_temp(result_ty, RValue::Stringify { value, source_ty })
    }

    fn callee_params(&self, callee: &Callee) -> Result<typing::CalleeParams, LowerError> {
        typing::callee_params(&self.cx.program, callee).ok_or_else(|| LowerError::UnsupportedType {
            ty: Box::new(Type::Infer),
        })
    }

    fn lower_exact_call_args<'a>(
        &mut self,
        expr_id: ExprId,
        callee: &Callee,
        exprs: impl Iterator<Item = &'a ExprNode>,
    ) -> Result<Vec<CallArg>, LowerError> {
        let params = self.callee_params(callee)?;
        let (arg_count, max_count) = exprs.size_hint();
        if max_count != Some(arg_count) || params.len(&self.cx.program) != Some(arg_count) {
            return Err(LowerError::UnsupportedExpr {
                expr_id,
                kind: "Call",
            });
        }
        self.lower_call_args(
            expr_id,
            exprs,
            params,
            0,
            !matches!(callee, Callee::Extern(_)),
        )
    }

    fn lower_call_args<'a>(
        &mut self,
        expr_id: ExprId,
        exprs: impl Iterator<Item = &'a ExprNode>,
        params: typing::CalleeParams,
        offset: usize,
        capture_dataref_roots: bool,
    ) -> Result<Vec<CallArg>, LowerError> {
        exprs
            .enumerate()
            .map(|(index, expr)| {
                let Some(param) = params.get(&self.cx.program, offset + index) else {
                    return Err(unsupported_expr(expr));
                };
                self.lower_expr_call_arg(expr, param, capture_dataref_roots)
            })
            .collect::<Result<Vec<_>, _>>()
            .map_err(|err| match err {
                LowerError::MissingLocalUse { .. } => LowerError::UnsupportedExpr {
                    expr_id,
                    kind: "borrow argument lowering",
                },
                err => err,
            })
    }

    fn capture_dataref_mut_place_root(&mut self, place: Place) -> Result<Place, LowerError> {
        if place.projection.is_empty() {
            return Ok(place);
        }
        let root_ty = match place.root {
            PlaceRoot::Local(local) => self
                .function
                .locals
                .get(local.index())
                .map(|local| local.ty),
            root => {
                place_model::root_info(&self.cx.program, self.function_id, root).map(|root| root.ty)
            }
        };
        let Some(root_ty) = root_ty else {
            return Ok(place);
        };
        if !matches!(self.cx.program.type_data(root_ty), TypeData::DataRef(_)) {
            return Ok(place);
        }
        let root = Place {
            root: place.root,
            projection: vec![],
            ty: root_ty,
        };
        let Operand::Place(captured) =
            self.emit_typed_temp(root_ty, RValue::Use(Operand::Place(root)))?
        else {
            unreachable!("typed temp lowers to a place")
        };
        Ok(Place {
            root: captured.root,
            projection: place.projection,
            ty: place.ty,
        })
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

    fn lower_method_call_with_receiver(
        &mut self,
        receiver: Operand,
        expr: &ExprNode,
        call: &ast::CallNode,
    ) -> Result<Operand, LowerError> {
        let value = self.lower_method_call_rvalue_with_receiver(receiver, expr, call)?;
        if self.lower_expr_ty(expr.node.id)? == Type::Void {
            return Err(unsupported_expr(expr));
        }
        self.emit_temp(value)
    }

    fn lower_method_call_effect_with_receiver(
        &mut self,
        receiver: Operand,
        expr: &ExprNode,
        call: &ast::CallNode,
    ) -> Result<(), LowerError> {
        let value = self.lower_method_call_rvalue_with_receiver(receiver, expr, call)?;
        self.emit_eval(value)
    }

    fn lower_method_call_rvalue_with_receiver(
        &mut self,
        receiver: Operand,
        expr: &ExprNode,
        call: &ast::CallNode,
    ) -> Result<RValue, LowerError> {
        let receiver = self.project_promoted_method_receiver(receiver, &call.node.func)?;
        let target = self
            .facts
            .calls
            .get(&expr.node.id)
            .ok_or_else(|| unsupported_expr(expr))?;
        if target.form != CallForm::Normal || !target.id.kind.has_receiver_param() {
            return Err(unsupported_expr(expr));
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
        let params = self.callee_params(&Callee::Function(callee))?;
        let Some(expected) = params.len(&self.cx.program) else {
            return Err(unsupported_expr(expr));
        };
        let provided = 1 + call.node.args.len();
        if provided > expected {
            return Err(unsupported_expr(expr));
        }
        let Some(receiver_param) = params.get(&self.cx.program, 0) else {
            return Err(unsupported_expr(expr));
        };
        let receiver = self.lower_operand_call_arg(receiver, receiver_param, expr)?;
        let receiver = match receiver {
            CallArg::MutBorrow(place) => {
                CallArg::MutBorrow(self.capture_dataref_mut_place_root(place)?)
            }
            arg => arg,
        };
        let mut args = vec![receiver];
        args.extend(self.lower_call_args(expr.node.id, call.node.args.iter(), params, 1, true)?);
        let defaults = self.lower_default_args(expr.node.id, args.len(), expected, params)?;
        args.extend(defaults);
        if args.len() != expected {
            return Err(unsupported_expr(expr));
        }
        Ok(RValue::Call {
            callee: Callee::Function(callee),
            args,
        })
    }

    fn project_promoted_method_receiver(
        &mut self,
        receiver: Operand,
        func: &ExprNode,
    ) -> Result<Operand, LowerError> {
        let Some(fact) = self.facts.member_paths.get(&func.node.id).cloned() else {
            return Ok(receiver);
        };
        if fact.kind != MemberPathKind::MethodReceiver {
            return Ok(receiver);
        }
        let origin_ty = self.cx.lower_ty(&fact.origin_owner)?;
        if self.operand_ty(&receiver) == origin_ty {
            return Ok(receiver);
        }
        let mut place = self.place_from_operand(receiver, func)?;
        for field in fact.path {
            place = self.project_field(func, place, field)?;
        }
        Ok(Operand::Place(place))
    }

    fn lower_dyn_borrow_arg(&mut self, expr: &ExprNode, ty: TypeId) -> Result<CallArg, LowerError> {
        let TypeData::Dyn(surface) = self.cx.program.type_data(ty) else {
            return Err(unsupported_expr(expr));
        };
        let surface = *surface;
        let place = self.lower_mut_call_arg(expr)?;
        let (source, weakening) = if let Some(fact) = self.facts.dyn_conversions.get(&expr.node.id)
        {
            let witness = self
                .cx
                .maps
                .contract_witnesses
                .get(&fact.witness)
                .copied()
                .ok_or_else(|| unsupported_expr(expr))?;
            (DynBorrowSource::Concrete { place, witness }, None)
        } else {
            let weakening = self
                .facts
                .dyn_weakenings
                .get(&expr.node.id)
                .map(|_| {
                    self.cx
                        .maps
                        .contract_weakenings
                        .get(&(self.body.clone(), expr.node.id))
                        .copied()
                        .ok_or_else(|| unsupported_expr(expr))
                })
                .transpose()?;
            let source = if matches!(place.root, PlaceRoot::DynBorrowParam(_)) {
                DynBorrowSource::Borrowed(place)
            } else {
                DynBorrowSource::Owned(place)
            };
            (source, weakening)
        };
        Ok(CallArg::DynBorrow(DynBorrow {
            source,
            ty,
            surface,
            weakening,
        }))
    }

    fn lower_expr_call_arg(
        &mut self,
        expr: &ExprNode,
        param: ParamType,
        capture_dataref_roots: bool,
    ) -> Result<CallArg, LowerError> {
        if param.mode == ParamMode::MutBorrow
            && matches!(self.cx.program.type_data(param.ty), TypeData::Dyn(_))
        {
            return self.lower_dyn_borrow_arg(expr, param.ty);
        }
        match param.mode {
            ParamMode::Value
                if matches!(self.cx.program.type_data(param.ty), TypeData::Slice(_)) =>
            {
                Ok(CallArg::Value(Operand::Place(
                    self.lower_shared_slice_call_arg(expr, param.ty)?,
                )))
            }
            ParamMode::Value => Ok(CallArg::Value(
                self.lower_expected_value(expr, param.ty, expr)?,
            )),
            ParamMode::SharedBorrow => self.lower_shared_call_arg(expr, param.ty),
            ParamMode::MutBorrow => {
                let place = if matches!(self.cx.program.type_data(param.ty), TypeData::Slice(_)) {
                    self.lower_mut_slice_call_arg(expr, param.ty)?
                } else {
                    self.lower_mut_call_arg(expr)?
                };
                if place.ty != param.ty {
                    return Err(unsupported_expr(expr));
                }
                let place = if capture_dataref_roots {
                    self.capture_dataref_mut_place_root(place)?
                } else {
                    place
                };
                Ok(CallArg::MutBorrow(place))
            }
        }
    }

    fn lower_place_receiver_call_arg(
        &mut self,
        receiver: Place,
        param: ParamType,
        site: Option<&ExprNode>,
    ) -> Result<CallArg, LowerError> {
        if let Some(site) = site {
            return self.lower_operand_call_arg(Operand::Place(receiver), param, site);
        }
        Ok(match param.mode {
            ParamMode::Value => CallArg::Value(Operand::Place(receiver)),
            ParamMode::SharedBorrow => CallArg::SharedBorrow(receiver),
            ParamMode::MutBorrow => CallArg::MutBorrow(receiver),
        })
    }

    fn lower_operand_call_arg(
        &mut self,
        value: Operand,
        param: ParamType,
        site: &ExprNode,
    ) -> Result<CallArg, LowerError> {
        if matches!(self.cx.program.type_data(param.ty), TypeData::Slice(_)) {
            let place = self.place_from_operand(value, site)?;
            let place = self.lower_slice_view_call_arg(
                place,
                param.ty,
                matches!(param.mode, ParamMode::MutBorrow),
                site,
            )?;
            return Ok(match param.mode {
                ParamMode::Value => CallArg::Value(Operand::Place(place)),
                ParamMode::SharedBorrow => CallArg::SharedBorrow(place),
                ParamMode::MutBorrow => CallArg::MutBorrow(place),
            });
        }

        match param.mode {
            ParamMode::Value => {
                let value = if self.operand_ty(&value) == param.ty {
                    value
                } else {
                    self.optional_some(value, param.ty, site)?
                };
                Ok(CallArg::Value(value))
            }
            ParamMode::SharedBorrow => {
                if self.operand_ty(&value) != param.ty {
                    return Err(unsupported_expr(site));
                }
                let place = match value {
                    Operand::Place(place) => place,
                    Operand::Const(id) if self.const_is_string(id) => {
                        return Ok(CallArg::SharedStringConst(id));
                    }
                    Operand::Const(id) => {
                        self.materialize_shared_operand(site, Operand::Const(id), param.ty)?
                    }
                };
                Ok(CallArg::SharedBorrow(place))
            }
            ParamMode::MutBorrow => {
                let place = self.place_from_operand(value, site)?;
                if place.ty != param.ty {
                    return Err(unsupported_expr(site));
                }
                self.require_mutable_place(site, &place)?;
                Ok(CallArg::MutBorrow(place))
            }
        }
    }

    fn lower_dyn_call_rvalue(
        &mut self,
        expr: &ExprNode,
        call: &ast::CallNode,
        fact: &DynCallFact,
    ) -> Result<RValue, LowerError> {
        let ExprKind::Field(field) = &call.node.func.node.kind else {
            return Err(unsupported_expr(expr));
        };
        if field.node.target.node.id != fact.receiver_id || call.node.args.len() != fact.arg_count {
            return Err(unsupported_expr(expr));
        }
        let surface = self
            .cx
            .maps
            .contract_surfaces
            .get(&fact.surface)
            .copied()
            .ok_or_else(|| unsupported_expr(expr))?;
        let surface_decl = self
            .cx
            .program
            .contract_surfaces
            .get(surface.index())
            .ok_or_else(|| unsupported_expr(expr))?;
        let slot_id = ContractSlotId::from_index(fact.slot.0 as usize);
        let slot = surface_decl
            .slots
            .get(slot_id.index())
            .cloned()
            .ok_or_else(|| unsupported_expr(expr))?;
        let receiver = if fact.requires_mutable {
            let place = self.lower_mut_call_arg(&field.node.target)?;
            if matches!(place.root, PlaceRoot::DynBorrowParam(_)) {
                DynReceiver::Borrowed(self.dyn_reborrow(&field.node.target, place, surface)?)
            } else {
                DynReceiver::MutableOwned(place)
            }
        } else {
            let value = self.lower_dynamic_source(&field.node.target)?;
            match &value {
                Operand::Place(place) if matches!(place.root, PlaceRoot::DynBorrowParam(_)) => {
                    DynReceiver::Borrowed(self.dyn_reborrow(
                        &field.node.target,
                        place.clone(),
                        surface,
                    )?)
                }
                _ => DynReceiver::Owned(value),
            }
        };
        let args = call
            .node
            .args
            .iter()
            .zip(&slot.params)
            .map(|(arg, param)| {
                self.lower_expr_call_arg(
                    arg,
                    ParamType {
                        ty: param.ty,
                        mode: param.mode,
                        escape: param.escape,
                    },
                    true,
                )
            })
            .collect::<Result<Vec<_>, _>>()?;
        if args.len() != slot.params.len() {
            return Err(unsupported_expr(expr));
        }
        Ok(RValue::DynCall {
            receiver,
            surface,
            slot: slot_id,
            args,
        })
    }

    fn dyn_reborrow(
        &mut self,
        expr: &ExprNode,
        place: Place,
        surface: ContractSurfaceId,
    ) -> Result<DynBorrow, LowerError> {
        let weakening = self
            .facts
            .dyn_weakenings
            .get(&expr.node.id)
            .map(|_| {
                self.cx
                    .maps
                    .contract_weakenings
                    .get(&(self.body.clone(), expr.node.id))
                    .copied()
                    .ok_or_else(|| unsupported_expr(expr))
            })
            .transpose()?;
        let ty = self
            .cx
            .program
            .type_arena
            .iter()
            .position(|ty| ty == &TypeData::Dyn(surface))
            .map(TypeId::from_index)
            .ok_or_else(|| unsupported_expr(expr))?;
        Ok(DynBorrow {
            source: DynBorrowSource::Borrowed(place),
            ty,
            surface,
            weakening,
        })
    }

    fn lower_call_rvalue(
        &mut self,
        expr: &ExprNode,
        call: &ast::CallNode,
    ) -> Result<RValue, LowerError> {
        if let Some(fact) = self.facts.dyn_calls.get(&expr.node.id).cloned() {
            return self.lower_dyn_call_rvalue(expr, call, &fact);
        }
        let args = &call.node.args;
        if let Some(target) = self.facts.calls.get(&expr.node.id)
            && is_lowered_collection_stub(&target.id)
        {
            if let Some(value) = self.lower_list_push_call(expr, call)? {
                return Ok(value);
            }
            if let Some(value) = self.lower_map_insert_call(expr, call)? {
                return Ok(value);
            }
            if let Some(value) = self.lower_map_remove_call(expr, call)? {
                return Ok(value);
            }
            return Err(unsupported_expr(expr));
        }
        if let Some(value) = self.lower_list_push_call(expr, call)? {
            return Ok(value);
        }
        if let Some(value) = self.lower_map_insert_call(expr, call)? {
            return Ok(value);
        }
        if let Some(value) = self.lower_map_remove_call(expr, call)? {
            return Ok(value);
        }
        if let Some(targets) = self.facts.extern_uses.get(&expr.node.id) {
            let [target] = targets.as_slice() else {
                return Err(LowerError::UnsupportedExternUse {
                    expr_id: expr.node.id,
                    kind: targets
                        .first()
                        .map_or(UnsupportedExternUseKind::MissingFunction, |target| {
                            unsupported_extern_kind(*target)
                        }),
                });
            };
            return self.lower_extern_call(expr, call, *target);
        }
        if self.facts.function_value_calls.contains_key(&expr.node.id) {
            return self.lower_function_value_call(expr, call);
        }

        let target = self
            .facts
            .calls
            .get(&expr.node.id)
            .ok_or_else(|| unsupported_expr(expr))?;
        if target.id.kind == CallableKind::EnumVariant {
            let CallableParent::Nominal(key) =
                target.id.parent.as_ref().expect("enum variant owner")
            else {
                return Err(unsupported_expr(expr));
            };
            let ty = self.lower_expr_ty(expr.node.id)?;
            let ty_id = self.cx.lower_ty(&ty)?;
            if let Some(inner) = typing::optional_inner(&self.cx.program, ty_id) {
                return match (target.id.name.as_str(), call.node.args.as_slice()) {
                    ("Some", [value]) => {
                        let value = self.lower_value_to(value, inner, expr)?;
                        Ok(RValue::Use(self.optional_some(value, ty_id, expr)?))
                    }
                    ("None", []) => Ok(RValue::Use(self.optional_none(ty_id, expr)?)),
                    _ => Err(unsupported_expr(expr)),
                };
            }
            let enum_id = match self.cx.program.type_data(ty_id) {
                TypeData::Enum(enum_id) => *enum_id,
                _ => return Err(unsupported_expr(expr)),
            };
            let Some(variant) = self.enum_variant_id(enum_id, target.id.name) else {
                return Err(unsupported_expr(expr));
            };
            let Some(key) = self.cx.decls.as_ref().and_then(|decls| decls.nominal(key)) else {
                return Err(unsupported_expr(expr));
            };
            if self.cx.program.enum_decl(enum_id).name != key.name {
                return Err(unsupported_expr(expr));
            }
            let VariantShape::Tuple(expected) =
                &self.cx.program.enum_decl(enum_id).variants[variant.index()].shape
            else {
                return Err(unsupported_expr(expr));
            };
            if expected.len() != call.node.args.len() {
                return Err(unsupported_expr(expr));
            }
            let expected = expected.clone();
            let fields = call
                .node
                .args
                .iter()
                .zip(expected)
                .map(|(arg, ty)| self.lower_value_to(arg, ty, expr))
                .collect::<Result<Vec<_>, _>>()?;
            return Ok(RValue::Aggregate {
                kind: AggregateCtor::EnumVariant { enum_id, variant },
                fields,
                ty: ty_id,
            });
        }
        if target.id.kind.has_receiver_param()
            && self
                .facts
                .member_paths
                .get(&call.node.func.node.id)
                .is_some_and(|fact| fact.kind == MemberPathKind::MethodReceiver)
        {
            let ExprKind::Field(_) = &call.node.func.node.kind else {
                return Err(unsupported_expr(&call.node.func));
            };
            let receiver = self.lower_place_arg(&call.node.func, false)?;
            return self.lower_method_call_rvalue_with_receiver(
                Operand::Place(receiver),
                expr,
                call,
            );
        }
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
        let mut arg_exprs = vec![];
        if target.id.kind.has_receiver_param() {
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
            arg_exprs.push(field.node.target.as_ref());
        }
        arg_exprs.extend(args.iter());
        let params = self.callee_params(&Callee::Function(callee))?;
        let Some(expected) = params.len(&self.cx.program) else {
            return Err(unsupported_expr(expr));
        };
        let provided = arg_exprs.len();
        if provided > expected {
            return Err(unsupported_expr(expr));
        }
        let mut args =
            self.lower_call_args(expr.node.id, arg_exprs.into_iter(), params, 0, true)?;
        let defaults = self.lower_default_args(expr.node.id, provided, expected, params)?;
        args.extend(defaults);
        if args.len() != expected {
            return Err(unsupported_expr(expr));
        }
        Ok(RValue::Call {
            callee: Callee::Function(callee),
            args,
        })
    }

    fn lower_function_value_call(
        &mut self,
        expr: &ExprNode,
        call: &ast::CallNode,
    ) -> Result<RValue, LowerError> {
        let fact = self
            .facts
            .function_value_calls
            .get(&expr.node.id)
            .expect("function-value call fact checked before lowering");
        if fact.callee != call.node.func.node.id {
            return Err(unsupported_expr(expr));
        }
        let sig = fact.sig.clone();
        let callee = self.lower_value(&call.node.func)?;
        if self.operand_ty(&callee) != self.cx.lower_ty(&sig)? {
            return Err(unsupported_expr(&call.node.func));
        }
        let callee = Callee::Lambda(callee);
        let args = self.lower_exact_call_args(expr.node.id, &callee, call.node.args.iter())?;
        Ok(RValue::Call { callee, args })
    }

    fn lower_collection_filter_effect(
        &mut self,
        expr: &ExprNode,
        call: &ast::CallNode,
    ) -> Result<bool, LowerError> {
        let ExprKind::Field(field) = &call.node.func.node.kind else {
            return Ok(false);
        };
        if call.node.args.len() != 1 {
            return Ok(false);
        }
        let Some(target) = self.facts.calls.get(&expr.node.id) else {
            return Ok(false);
        };
        if !is_lowered_collection_stub(&target.id) {
            return Ok(false);
        }
        let sequence_filter = collection_effect::filter_remove_matches(
            collection_effect::CollectionKind::Sequence,
            field.node.field,
        );
        let map_filter = collection_effect::filter_remove_matches(
            collection_effect::CollectionKind::Map,
            field.node.field,
        );
        if sequence_filter.is_none() && map_filter.is_none() {
            return Ok(false);
        }
        let root = self.lower_collection_method_target(&field.node.target)?;
        self.require_mutable_place(expr, &root)?;
        if let Some(elem) = typing::list_elem(&self.cx.program, root.ty) {
            let Some(remove_matches) = sequence_filter else {
                return Ok(false);
            };
            self.lower_filter_effect(
                &root,
                &call.node.args[0],
                remove_matches,
                FilterCollection::List { elem },
            )?;
            return Ok(true);
        }
        let Some((key, value)) = typing::map_kv(&self.cx.program, root.ty) else {
            return Ok(false);
        };
        let Some(remove_matches) = map_filter else {
            return Ok(false);
        };
        self.lower_filter_effect(
            &root,
            &call.node.args[0],
            remove_matches,
            FilterCollection::Map { key, value },
        )?;
        Ok(true)
    }

    fn lower_filter_effect(
        &mut self,
        root: &Place,
        predicate: &ExprNode,
        remove_matches: bool,
        collection: FilterCollection,
    ) -> Result<(), LowerError> {
        let bool_ty = self.cx.lower_ty(&Type::Bool)?;
        let int_ty = self.cx.lower_ty(&Type::Int)?;
        let flags_ty = self.cx.program.alloc_type(TypeData::List(bool_ty));
        let len = self.for_len_local()?;
        let index = self.push_local(None, None, int_ty, AirMutability::Mutable, LocalKind::Temp);
        let flags = self.push_local(
            None,
            None,
            flags_ty,
            AirMutability::Mutable,
            LocalKind::Temp,
        );
        let kept = self.push_local(None, None, root.ty, AirMutability::Mutable, LocalKind::Temp);
        let entry_ty = match collection {
            FilterCollection::List { .. } => None,
            FilterCollection::Map { key, value } => Some(
                self.cx
                    .program
                    .alloc_type(TypeData::Tuple(vec![key, value])),
            ),
        };
        let one = self.int_const(1)?;
        let step = self.emit_typed_temp(int_ty, RValue::Use(one))?;
        let id = self.alloc_loop();
        let callback = self.lower_filter_callback(predicate)?;
        let body = self.with_nested_block(|this| {
            this.emit_init(
                flags,
                RValue::Aggregate {
                    kind: AggregateCtor::List,
                    fields: vec![],
                    ty: flags_ty,
                },
            )?;
            this.emit_init(
                len,
                RValue::Len {
                    source: root.clone(),
                },
            )?;
            this.reset_filter_index(index, int_ty, &step, true)?;
            this.emit_filter_loop(id, len, index, int_ty, bool_ty, &step, |this| {
                let args = this.filter_callback_args(root, index, collection, entry_ty)?;
                let keep = this.lower_filter_keep(predicate, &callback, args, remove_matches)?;
                this.emit_eval(RValue::ListPush {
                    list: this.local_place(flags),
                    value: keep,
                })?;
                this.terminate(AirTail::Continue(id))
            })
        })?;
        self.ensure_open()?;
        self.block
            .stmts
            .push(AirStmt::CollectionLoan(AirCollectionLoan {
                root: root.clone(),
                root_kind: match collection {
                    FilterCollection::List { .. } => AirCollectionRootKind::List,
                    FilterCollection::Map { .. } => AirCollectionRootKind::Map,
                },
                mode: match collection {
                    FilterCollection::List { .. } => AirCollectionLoanMode::ReadonlySequence,
                    FilterCollection::Map { .. } => AirCollectionLoanMode::ReadonlyMap,
                },
                body,
            }));
        let rebuild_body = self.with_nested_block(|this| {
            this.rebuild_filtered_collection(
                root, len, index, kept, flags, &step, collection, entry_ty,
            )
        })?;
        self.ensure_open()?;
        self.block
            .stmts
            .push(AirStmt::CollectionLoan(AirCollectionLoan {
                root: root.clone(),
                root_kind: match collection {
                    FilterCollection::List { .. } => AirCollectionRootKind::List,
                    FilterCollection::Map { .. } => AirCollectionRootKind::Map,
                },
                mode: match collection {
                    FilterCollection::List { .. } => AirCollectionLoanMode::ReadonlySequence,
                    FilterCollection::Map { .. } => AirCollectionLoanMode::ReadonlyMap,
                },
                body: rebuild_body,
            }));
        self.emit_assign(
            root.clone(),
            RValue::Use(Operand::Place(self.local_place(kept))),
        )
    }

    fn rebuild_filtered_collection(
        &mut self,
        root: &Place,
        len: LocalId,
        index: LocalId,
        kept: LocalId,
        flags: LocalId,
        step: &Operand,
        collection: FilterCollection,
        entry_ty: Option<TypeId>,
    ) -> Result<(), LowerError> {
        let bool_ty = self.cx.lower_ty(&Type::Bool)?;
        let int_ty = self.cx.lower_ty(&Type::Int)?;
        self.emit_init(
            kept,
            RValue::Aggregate {
                kind: match collection {
                    FilterCollection::List { .. } => AggregateCtor::List,
                    FilterCollection::Map { .. } => AggregateCtor::Map,
                },
                fields: vec![],
                ty: root.ty,
            },
        )?;
        self.reset_filter_index(index, int_ty, step, false)?;
        let id = self.alloc_loop();
        self.emit_filter_loop(id, len, index, int_ty, bool_ty, step, |this| {
            let keep = Operand::Place(Self::sequence_element_place(
                &this.local_place(flags),
                index,
                bool_ty,
            ));
            let keep_block = this.with_nested_block(|this| {
                this.emit_kept_filter_entry(root, index, kept, collection, entry_ty)?;
                this.terminate(AirTail::Continue(id))
            })?;
            this.ensure_open()?;
            this.block.stmts.push(AirStmt::If(AirIf {
                cond: keep,
                then_block: keep_block,
                else_block: Some(AirBlock {
                    stmts: vec![],
                    tail: AirTail::Continue(id),
                }),
            }));
            Ok(())
        })
    }

    fn reset_filter_index(
        &mut self,
        index: LocalId,
        int_ty: TypeId,
        step: &Operand,
        init: bool,
    ) -> Result<(), LowerError> {
        let zero = self.int_const(0)?;
        let value = RValue::Binary {
            op: BinaryOp::Sub,
            lhs: zero,
            rhs: step.clone(),
            ty: int_ty,
        };
        if init {
            self.emit_init(index, value)
        } else {
            self.emit_assign(self.local_place(index), value)
        }
    }

    fn emit_filter_loop(
        &mut self,
        id: AirLoopId,
        len: LocalId,
        index: LocalId,
        int_ty: TypeId,
        bool_ty: TypeId,
        step: &Operand,
        mut emit_then: impl FnMut(&mut Self) -> Result<(), LowerError>,
    ) -> Result<(), LowerError> {
        let body = self.with_nested_block(|this| {
            this.emit_assign(
                this.local_place(index),
                RValue::Binary {
                    op: BinaryOp::Add,
                    lhs: Operand::Place(this.local_place(index)),
                    rhs: step.clone(),
                    ty: int_ty,
                },
            )?;
            let cond = this.emit_typed_temp(
                bool_ty,
                RValue::Binary {
                    op: BinaryOp::LessThan,
                    lhs: Operand::Place(this.local_place(index)),
                    rhs: Operand::Place(this.local_place(len)),
                    ty: bool_ty,
                },
            )?;
            let then_block = this.with_nested_block(|this| emit_then(this))?;
            this.ensure_open()?;
            this.block.stmts.push(AirStmt::If(AirIf {
                cond,
                then_block,
                else_block: Some(AirBlock {
                    stmts: vec![],
                    tail: AirTail::Break(id),
                }),
            }));
            Ok(())
        })?;
        self.ensure_open()?;
        self.block.stmts.push(AirStmt::Loop(AirLoop { id, body }));
        Ok(())
    }

    fn filter_callback_args(
        &mut self,
        root: &Place,
        index: LocalId,
        collection: FilterCollection,
        entry_ty: Option<TypeId>,
    ) -> Result<Vec<Operand>, LowerError> {
        match collection {
            FilterCollection::List { elem } => Ok(vec![self.emit_typed_temp(
                elem,
                RValue::Use(Operand::Place(Self::sequence_element_place(
                    root, index, elem,
                ))),
            )?]),
            FilterCollection::Map { key, value } => {
                let entry =
                    self.map_filter_entry(root, index, entry_ty.expect("map entry type"))?;
                Ok(vec![
                    Self::tuple_field_operand(entry.clone(), 0, key),
                    Self::tuple_field_operand(entry, 1, value),
                ])
            }
        }
    }

    fn emit_kept_filter_entry(
        &mut self,
        root: &Place,
        index: LocalId,
        kept: LocalId,
        collection: FilterCollection,
        entry_ty: Option<TypeId>,
    ) -> Result<(), LowerError> {
        match collection {
            FilterCollection::List { elem } => {
                let elem_value = self.emit_typed_temp(
                    elem,
                    RValue::Use(Operand::Place(Self::sequence_element_place(
                        root, index, elem,
                    ))),
                )?;
                self.emit_eval(RValue::ListPush {
                    list: self.local_place(kept),
                    value: elem_value,
                })
            }
            FilterCollection::Map { key, value } => {
                let entry =
                    self.map_filter_entry(root, index, entry_ty.expect("map entry type"))?;
                self.emit_eval(RValue::MapInsert {
                    map: self.local_place(kept),
                    key: Self::tuple_field_operand(entry.clone(), 0, key),
                    value: Self::tuple_field_operand(entry, 1, value),
                    kind: MapWriteKind::StructuralInsert,
                })
            }
        }
    }

    fn map_filter_entry(
        &mut self,
        root: &Place,
        index: LocalId,
        entry_ty: TypeId,
    ) -> Result<Operand, LowerError> {
        self.emit_typed_temp(
            entry_ty,
            RValue::MapEntryAt {
                map: root.clone(),
                index,
                ty: entry_ty,
            },
        )
    }

    fn lower_filter_callback(&mut self, predicate: &ExprNode) -> Result<Operand, LowerError> {
        self.lower_value(predicate)
    }

    fn lower_filter_call_args(
        &self,
        site: &ExprNode,
        callback: &Operand,
        args: Vec<Operand>,
    ) -> Result<Vec<CallArg>, LowerError> {
        let TypeData::Function(sig) = self.cx.program.type_data(self.operand_ty(callback)) else {
            return Err(unsupported_expr(site));
        };
        if sig.params.len() != args.len() {
            return Err(unsupported_expr(site));
        }
        args.into_iter()
            .zip(&sig.params)
            .map(|(arg, param)| match param.mode {
                ParamMode::Value => Ok(CallArg::Value(arg)),
                ParamMode::SharedBorrow => match arg {
                    Operand::Place(place) => Ok(CallArg::SharedBorrow(place)),
                    Operand::Const(_) => Err(unsupported_expr(site)),
                },
                ParamMode::MutBorrow => Err(unsupported_expr(site)),
            })
            .collect()
    }

    fn lower_filter_keep(
        &mut self,
        predicate: &ExprNode,
        callback: &Operand,
        args: Vec<Operand>,
        remove_matches: bool,
    ) -> Result<Operand, LowerError> {
        let bool_ty = self.cx.lower_ty(&Type::Bool)?;
        let args = self.lower_filter_call_args(predicate, callback, args)?;
        let matched = self.emit_typed_temp(
            bool_ty,
            RValue::Call {
                callee: Callee::Lambda(callback.clone()),
                args,
            },
        )?;
        if !remove_matches {
            return Ok(matched);
        }
        self.emit_typed_temp(
            bool_ty,
            RValue::Unary {
                op: ast::UnaryOp::Not,
                value: matched,
                ty: bool_ty,
            },
        )
    }

    fn lower_list_for_each_effect(
        &mut self,
        expr: &ExprNode,
        call: &ast::CallNode,
    ) -> Result<bool, LowerError> {
        let ExprKind::Field(field) = &call.node.func.node.kind else {
            return Ok(false);
        };
        if !matches!(
            collection_effect::classify_sequence_method(field.node.field),
            Some(collection_effect::SequenceMethod::ForEach)
        ) || call.node.args.len() != 1
        {
            return Ok(false);
        }
        let Some(target) = self.facts.calls.get(&expr.node.id) else {
            return Ok(false);
        };
        if !is_lowered_collection_stub(&target.id) {
            return Ok(false);
        }
        let root = self.lower_collection_method_target(&field.node.target)?;
        let Some(elem) = typing::list_elem(&self.cx.program, root.ty) else {
            return Ok(false);
        };
        self.require_mutable_place(expr, &root)?;
        let callback = self.lower_value(&call.node.args[0])?;
        let int_ty = self.cx.lower_ty(&Type::Int)?;
        let bool_ty = self.cx.lower_ty(&Type::Bool)?;
        let len = self.for_len_local()?;
        let index = self.push_local(None, None, int_ty, AirMutability::Mutable, LocalKind::Temp);
        let slot = self.push_local(None, None, elem, AirMutability::Mutable, LocalKind::Temp);
        let one = self.int_const(1)?;
        let step = self.emit_typed_temp(int_ty, RValue::Use(one))?;
        let id = self.alloc_loop();
        let body = self.with_nested_block(|this| {
            this.emit_init(
                len,
                RValue::Len {
                    source: root.clone(),
                },
            )?;
            let zero = this.int_const(0)?;
            this.emit_init(
                index,
                RValue::Binary {
                    op: BinaryOp::Sub,
                    lhs: zero,
                    rhs: step.clone(),
                    ty: int_ty,
                },
            )?;
            let loop_body = this.with_nested_block(|this| {
                this.emit_assign(
                    this.local_place(index),
                    RValue::Binary {
                        op: BinaryOp::Add,
                        lhs: Operand::Place(this.local_place(index)),
                        rhs: step.clone(),
                        ty: int_ty,
                    },
                )?;
                let cond = this.emit_typed_temp(
                    bool_ty,
                    RValue::Binary {
                        op: BinaryOp::LessThan,
                        lhs: Operand::Place(this.local_place(index)),
                        rhs: Operand::Place(this.local_place(len)),
                        ty: bool_ty,
                    },
                )?;
                let then_block = this.with_nested_block(|this| {
                    let slot_body = this.with_nested_block(|this| {
                        this.emit_eval(RValue::Call {
                            callee: Callee::Lambda(callback.clone()),
                            args: vec![CallArg::MutBorrow(this.local_place(slot))],
                        })?;
                        if !this.terminated {
                            this.terminate(AirTail::Continue(id))?;
                        }
                        Ok(())
                    })?;
                    this.ensure_open()?;
                    this.block
                        .stmts
                        .push(AirStmt::CollectionSlotScope(AirCollectionSlotScope {
                            root: root.clone(),
                            index,
                            slots: vec![AirCollectionSlot {
                                kind: AirCollectionSlotKind::SequenceElement,
                                local: slot,
                                ty: elem,
                                mutable: true,
                            }],
                            body: slot_body,
                        }));
                    Ok(())
                })?;
                let else_block = AirBlock {
                    stmts: vec![],
                    tail: AirTail::Break(id),
                };
                this.ensure_open()?;
                this.block.stmts.push(AirStmt::If(AirIf {
                    cond,
                    then_block,
                    else_block: Some(else_block),
                }));
                Ok(())
            })?;
            this.ensure_open()?;
            this.block.stmts.push(AirStmt::Loop(AirLoop {
                id,
                body: loop_body,
            }));
            Ok(())
        })?;
        self.ensure_open()?;
        self.block
            .stmts
            .push(AirStmt::CollectionLoan(AirCollectionLoan {
                root,
                root_kind: AirCollectionRootKind::List,
                mode: AirCollectionLoanMode::MutableSequenceElement,
                body,
            }));
        Ok(true)
    }

    fn lower_list_push_call(
        &mut self,
        expr: &ExprNode,
        call: &ast::CallNode,
    ) -> Result<Option<RValue>, LowerError> {
        let ExprKind::Field(field) = &call.node.func.node.kind else {
            return Ok(None);
        };
        if !matches!(
            collection_effect::classify_sequence_method(field.node.field),
            Some(collection_effect::SequenceMethod::Push)
        ) || call.node.args.len() != 1
        {
            return Ok(None);
        }
        let list = self.lower_collection_method_target(&field.node.target)?;
        let Some(elem) = typing::list_elem(&self.cx.program, list.ty) else {
            return Ok(None);
        };
        self.require_mutable_place(expr, &list)?;
        let value = self.lower_value_to(&call.node.args[0], elem, expr)?;
        Ok(Some(RValue::ListPush { list, value }))
    }

    fn lower_map_insert_call(
        &mut self,
        expr: &ExprNode,
        call: &ast::CallNode,
    ) -> Result<Option<RValue>, LowerError> {
        let ExprKind::Field(field) = &call.node.func.node.kind else {
            return Ok(None);
        };
        if !matches!(
            collection_effect::classify_map_method(field.node.field),
            Some(collection_effect::MapMethod::Insert)
        ) || call.node.args.len() != 2
        {
            return Ok(None);
        }
        let map = self.lower_collection_method_target(&field.node.target)?;
        let Some((key_ty, value_ty)) = typing::map_kv(&self.cx.program, map.ty) else {
            return Ok(None);
        };
        self.require_mutable_place(expr, &map)?;
        let key = self.lower_value_to(&call.node.args[0], key_ty, expr)?;
        let value = self.lower_value_to(&call.node.args[1], value_ty, expr)?;
        Ok(Some(RValue::MapInsert {
            map,
            key,
            value,
            kind: MapWriteKind::StructuralInsert,
        }))
    }

    fn lower_map_remove_call(
        &mut self,
        expr: &ExprNode,
        call: &ast::CallNode,
    ) -> Result<Option<RValue>, LowerError> {
        let ExprKind::Field(field) = &call.node.func.node.kind else {
            return Ok(None);
        };
        if !matches!(
            collection_effect::classify_map_method(field.node.field),
            Some(collection_effect::MapMethod::Remove)
        ) || call.node.args.len() != 1
        {
            return Ok(None);
        }
        let map = self.lower_collection_method_target(&field.node.target)?;
        let Some((key_ty, value_ty)) = typing::map_kv(&self.cx.program, map.ty) else {
            return Ok(None);
        };
        self.require_mutable_place(expr, &map)?;
        let key = self.lower_value_to(&call.node.args[0], key_ty, expr)?;
        let ty = self.cx.optional_ty(value_ty);
        Ok(Some(RValue::MapRemove { map, key, ty }))
    }

    fn lower_collection_method_target(&mut self, target: &ExprNode) -> Result<Place, LowerError> {
        self.lower_place_arg_impl_with_fallback(target, true, true, false)
    }

    fn require_mutable_place(&self, expr: &ExprNode, place: &Place) -> Result<(), LowerError> {
        if self.place_is_capture_cell(place) || self.place_is_scoped_borrow(place) {
            return if place.projection.is_empty() {
                Ok(())
            } else {
                Err(mutable_place_projection_gap(expr.node.id))
            };
        }
        if let Some(root) = place.root.local()
            && self.function.locals[root.index()].mutability == AirMutability::Mutable
        {
            return Ok(());
        }
        if let PlaceRoot::LambdaCapture(slot) = place.root
            && self.lambda_capture_mutability(slot) == Some(AirMutability::Mutable)
        {
            return Ok(());
        }
        if let PlaceRoot::Global(global) = place.root
            && self.cx.program.globals[global.index()].mutability == AirMutability::Mutable
        {
            return Ok(());
        }
        Err(unsupported_expr(expr))
    }

    fn lower_extern_call(
        &mut self,
        expr: &ExprNode,
        call: &ast::CallNode,
        target: ExternUseTarget,
    ) -> Result<RValue, LowerError> {
        let Some(callee) = self.cx.maps.externs.get(&target).copied() else {
            return Err(LowerError::UnsupportedExternUse {
                expr_id: expr.node.id,
                kind: unsupported_extern_kind(target),
            });
        };
        let decl = self.cx.program.extern_decl(callee);
        let args = match &decl.member {
            ExternMember::FreeFunction
            | ExternMember::StaticMethod { .. }
            | ExternMember::Init { .. } => self.lower_exact_call_args(
                expr.node.id,
                &Callee::Extern(callee),
                call.node.args.iter(),
            )?,
            ExternMember::Method { .. } => {
                let ExprKind::Field(field) = &call.node.func.node.kind else {
                    return Err(unsupported_expr(&call.node.func));
                };
                self.lower_exact_call_args(
                    expr.node.id,
                    &Callee::Extern(callee),
                    std::iter::once(field.node.target.as_ref()).chain(call.node.args.iter()),
                )?
            }
            ExternMember::FieldGetter { .. }
            | ExternMember::FieldSetter { .. }
            | ExternMember::UnaryOperator { .. }
            | ExternMember::BinaryOperator { .. } => {
                return Err(LowerError::UnsupportedExternUse {
                    expr_id: expr.node.id,
                    kind: unsupported_extern_kind(target),
                });
            }
        };
        Ok(RValue::Call {
            callee: Callee::Extern(callee),
            args,
        })
    }

    fn lower_default_args(
        &mut self,
        call: ExprId,
        provided: usize,
        expected: usize,
        params: typing::CalleeParams,
    ) -> Result<Vec<CallArg>, LowerError> {
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
        let has_receiver = matches!(
            params,
            typing::CalleeParams::Function(id)
                if self.cx.program.functions[id.index()]
                    .signature
                    .params
                    .first()
                    .is_some_and(|param| matches!(param.role, ParamRole::Receiver))
        );
        let fact_offset = usize::from(has_receiver);
        defaults.sort_by_key(|fact| fact.param_index);
        defaults
            .iter()
            .enumerate()
            .map(|(index, fact)| {
                let param_index = provided + index;
                let Some(fact_param_index) = param_index.checked_sub(fact_offset) else {
                    return Err(LowerError::UnsupportedDefaultArg {
                        call,
                        param_index,
                        expr_id: fact.default.expr,
                    });
                };
                if fact.call != call || fact.param_index != fact_param_index {
                    return Err(LowerError::UnsupportedDefaultArg {
                        call,
                        param_index,
                        expr_id: fact.default.expr,
                    });
                }
                let Some(param) = params.get(&self.cx.program, param_index) else {
                    return Err(LowerError::UnsupportedDefaultArg {
                        call,
                        param_index,
                        expr_id: call,
                    });
                };
                self.lower_default_arg(fact, param)
            })
            .collect()
    }

    fn lower_default_arg(
        &mut self,
        fact: &DefaultArgFact,
        param: ParamType,
    ) -> Result<CallArg, LowerError> {
        let error = || LowerError::UnsupportedDefaultArg {
            call: fact.call,
            param_index: fact.param_index,
            expr_id: fact.default.expr,
        };
        if matches!(param.mode, ParamMode::MutBorrow) {
            return Err(error());
        }
        let Some(expr) = self.index.get_default_expr(fact.default) else {
            return Err(error());
        };
        let expr = (*expr).clone();
        self.with_default_facts(fact.default, &fact.facts_body, |this| {
            let value = this.lower_expected_value(&expr, param.ty, &expr)?;
            this.lower_operand_call_arg(value, param, &expr)
        })
        .map_err(|err| match err {
            LowerError::UnsupportedExpr { .. } => error(),
            err => err,
        })
    }

    fn with_default_facts<R>(
        &mut self,
        site: DefaultExprSite,
        body: &BodyInstanceKey,
        lower: impl FnOnce(&mut Self) -> Result<R, LowerError>,
    ) -> Result<R, LowerError> {
        let facts = self
            .body_facts
            .get(body)
            .ok_or(LowerError::MissingDefaultExprFacts { site })?;
        let old_facts = std::mem::replace(&mut self.facts, facts);
        let result = lower(self);
        self.facts = old_facts;
        result
    }

    fn lower_const_value(
        &mut self,
        expr: &ExprNode,
        value: &ast::ConstValue,
    ) -> Result<Operand, LowerError> {
        let ty = self.cx.lower_ty(&self.lower_expr_ty(expr.node.id)?)?;
        Ok(Operand::Const(self.cx.program.alloc_const(ConstData {
            ty,
            value: lower_const_specialization_value(value),
        })))
    }

    fn lower_lit(&mut self, expr: &ExprNode, lit: &Lit) -> Result<Operand, LowerError> {
        let ty = self.lower_expr_ty(expr.node.id)?;
        let ty_id = self.cx.lower_ty(&ty)?;
        if matches!(lit, Lit::Nil) {
            return self.optional_none(ty_id, expr);
        }
        let value = Self::literal_const_value(lit, &ty).ok_or_else(|| unsupported_expr(expr))?;
        Ok(Operand::Const(
            self.cx.program.alloc_const(ConstData { ty: ty_id, value }),
        ))
    }

    fn literal_const_value(lit: &Lit, ty: &Type) -> Option<ConstValue> {
        let value = lit.const_value()?;
        source_const_matches_type(&value, ty).then(|| lower_const_specialization_value(&value))
    }

    fn literal_air_const_value(lit: &Lit, ty: &TypeData) -> Option<ConstValue> {
        let value = lit.const_value()?;
        air_const_matches_type(&value, ty).then(|| lower_const_specialization_value(&value))
    }

    fn lower_effect(&mut self, expr: &ExprNode) -> Result<(), LowerError> {
        if self.lower_safe_field_chain_effect(expr)? {
            return Ok(());
        }
        match &expr.node.kind {
            ExprKind::Assign(assign) => self.lower_assign(expr, assign),
            ExprKind::Block(block) => self.lower_block_effect(block),
            ExprKind::If(if_expr) => self.lower_if_effect(if_expr),
            ExprKind::IfLet(if_let) => self.lower_if_let_effect(if_let),
            ExprKind::Match(match_expr) => self.lower_match_effect(expr, match_expr),
            ExprKind::Call(call) => {
                if self.lower_collection_filter_effect(expr, call)?
                    || self.lower_list_for_each_effect(expr, call)?
                {
                    return Ok(());
                }
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
        match assign.node.op {
            AssignOp::Assign => {
                if self.lower_map_index_assignment(&assign.node.target, &assign.node.value)? {
                    return Ok(());
                }
                if let Some(target) =
                    self.select_extern_target(assign.node.target.node.id, |target| {
                        matches!(target, ExternUseTarget::FieldWrite(_))
                            && self.cx.maps.externs.contains_key(&target)
                    })
                {
                    let value = self.lower_extern_field_write(
                        &assign.node.target,
                        &assign.node.value,
                        target,
                    )?;
                    return self.emit_eval(value);
                }
                if let Some(fact) = self.global_access(assign.node.target.node.id).cloned() {
                    let dst = self.lower_global_projected_place(&assign.node.target, &fact)?;
                    let value =
                        self.lower_value_to(&assign.node.value, dst.ty, &assign.node.value)?;
                    return match fact.mode {
                        GlobalAccessMode::RootAssign
                            if fact.init_effect == TcGlobalInitEffect::StoreWithoutInit =>
                        {
                            let global = self.lowered_global(&fact.key)?;
                            self.emit_global_set_root(global, RValue::Use(value))
                        }
                        GlobalAccessMode::ProjectedAssign => {
                            self.emit_assign(dst, RValue::Use(value))
                        }
                        _ => Err(unsupported_expr(&assign.node.target)),
                    };
                }
                let fact = self.local_use(&assign.node.target, LocalUseMode::Assign)?;
                let dst = self.lower_place(&assign.node.target, &fact)?;
                let value = self.lower_value_to(&assign.node.value, dst.ty, &assign.node.value)?;
                self.emit_assign(dst, RValue::Use(value))
            }
            op => {
                let binary = assign_op_to_binary(op);
                let dst = if let Some(fact) =
                    self.global_access(assign.node.target.node.id).cloned()
                {
                    if fact.mode != GlobalAccessMode::CompoundAssign {
                        return Err(unsupported_expr(&assign.node.target));
                    }
                    self.lower_global_projected_place(&assign.node.target, &fact)?
                } else {
                    let fact = self.local_use(&assign.node.target, LocalUseMode::CompoundAssign)?;
                    self.lower_place(&assign.node.target, &fact)?
                };
                let is_global_root =
                    matches!(dst.root, PlaceRoot::Global(_)) && dst.projection.is_empty();
                let result_ty = self.air_type(dst.ty);
                if binary == BinaryOp::Add && result_ty == Type::String {
                    let lhs = Operand::Place(dst.clone());
                    let rhs = self.lower_string_part(&assign.node.value)?;
                    let value = RValue::StringConcat {
                        parts: vec![lhs, rhs],
                    };
                    if is_global_root {
                        let PlaceRoot::Global(global) = dst.root else {
                            unreachable!("checked global root")
                        };
                        self.emit_global_update_root(global, value)?;
                    } else {
                        self.emit_assign(dst, value)?;
                    }
                    return Ok(());
                }
                let lhs = Operand::Place(dst.clone());
                let rhs = self.lower_value(&assign.node.value)?;
                let rhs_air_ty = self.operand_ty(&rhs);
                let flag_op = dst.ty == rhs_air_ty
                    && matches!(self.cx.program.type_data(dst.ty), TypeData::Flag(_))
                    && matches!(binary, BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::Xor);
                if !flag_op {
                    self.require_builtin_scalar(expr)?;
                    let lhs_ty = self.operand_type(&lhs);
                    let rhs_ty = self.operand_type(&rhs);
                    let (Some(lhs_scalar), Some(rhs_scalar), Some(result_scalar)) = (
                        lhs_ty.scalar_kind(),
                        rhs_ty.scalar_kind(),
                        result_ty.scalar_kind(),
                    ) else {
                        return Err(unsupported_expr(&assign.node.target));
                    };
                    if binary.scalar_result(lhs_scalar, rhs_scalar) != Some(result_scalar) {
                        return Err(unsupported_expr(&assign.node.target));
                    }
                }
                let tmp = self.emit_temp(RValue::Binary {
                    op: binary,
                    lhs,
                    rhs,
                    ty: dst.ty,
                })?;
                if is_global_root {
                    let PlaceRoot::Global(global) = dst.root else {
                        unreachable!("checked global root")
                    };
                    self.emit_global_update_root(global, RValue::Use(tmp))
                } else {
                    self.emit_assign(dst, RValue::Use(tmp))
                }
            }
        }
    }

    fn lower_map_index_assignment(
        &mut self,
        target: &ExprNode,
        value_expr: &ExprNode,
    ) -> Result<bool, LowerError> {
        let ExprKind::Index(index) = &target.node.kind else {
            return Ok(false);
        };
        if index.node.safe || matches!(index.node.index.node.kind, ExprKind::Range(_)) {
            return Ok(false);
        }
        if !matches!(
            self.lower_expr_ty(index.node.target.node.id)?,
            Type::Map { .. }
        ) {
            return Ok(false);
        }

        let map = if let Some(fact) = self.global_access(target.node.id).cloned() {
            if fact.mode != GlobalAccessMode::ProjectedAssign {
                return Err(unsupported_expr(target));
            }
            self.lower_global_projected_place(&index.node.target, &fact)?
        } else {
            let fact = self.local_use(target, LocalUseMode::Assign)?;
            self.lower_projected_place(&index.node.target, self.binding_place(&fact)?)?
        };
        let Some((key_ty, value_ty)) = typing::map_kv(&self.cx.program, map.ty) else {
            return Err(unsupported_expr(target));
        };
        self.require_mutable_place(target, &map)?;
        let key = self.lower_value_to(&index.node.index, key_ty, target)?;
        let value = self.lower_value_to(value_expr, value_ty, value_expr)?;
        self.emit_eval(RValue::MapInsert {
            map,
            key,
            value,
            kind: MapWriteKind::IndexedAssignment,
        })?;
        Ok(true)
    }

    fn lower_place(&mut self, expr: &ExprNode, fact: &LocalUseFact) -> Result<Place, LowerError> {
        self.lower_projected_place(expr, self.binding_place(fact)?)
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
            || self.facts.raw_projections.contains_key(&id)
            || self.facts.raw_try_constructs.contains_key(&id)
            || self.facts.flag_members.contains_key(&id)
            || self.facts.flag_statics.contains_key(&id)
    }

    fn returns_void(&self) -> bool {
        self.cx
            .program
            .type_data(self.function.signature.return_type())
            == &TypeData::Void
    }

    fn lower_expr_ty(&self, expr_id: ExprId) -> Result<Type, LowerError> {
        if let Some(ty) = self.type_overrides.get(&expr_id) {
            return Ok(ty.clone());
        }
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

    fn binding_place(&self, fact: &LocalUseFact) -> Result<Place, LowerError> {
        if let Some(place) = self.locals.get(&fact.local) {
            return Ok(place.clone());
        }
        let Some(binding) = fact.binding_id else {
            return Err(LowerError::MissingLocalDef {
                body: Box::new(self.body.clone()),
                local: fact.local,
            });
        };
        let Some(LambdaCaptureSource::Capture { slot, decl }) = self.capture_sources.get(&binding)
        else {
            return Err(LowerError::MissingLocalDef {
                body: Box::new(self.body.clone()),
                local: fact.local,
            });
        };
        Ok(Place {
            root: PlaceRoot::LambdaCapture(*slot),
            projection: vec![],
            ty: decl.ty(),
        })
    }

    fn lambda_capture_mutability(&self, slot: LambdaCaptureSlotId) -> Option<AirMutability> {
        let FunctionKind::Lambda(lambda) = self.function.kind else {
            return None;
        };
        self.cx.program.lambdas[lambda.index()]
            .captures
            .get(slot.index())
            .map(LambdaCaptureDecl::mutability)
    }

    fn existing_semantic_local(&self, semantic: SemanticLocalId) -> Option<LocalId> {
        let place = self.locals.get(&semantic)?;
        place.projection.is_empty().then_some(())?;
        place.root.local()
    }

    fn push_local(
        &mut self,
        name: Option<Ident>,
        binding: Option<AirBindingId>,
        ty: TypeId,
        mutability: AirMutability,
        kind: LocalKind,
    ) -> LocalId {
        let id = LocalId::from_index(self.function.locals.len());
        self.function.locals.push(Local {
            name,
            binding,
            ty,
            mutability,
            kind,
        });
        id
    }

    fn temp(&mut self, ty: TypeId) -> LocalId {
        self.push_local(None, None, ty, AirMutability::Immutable, LocalKind::Temp)
    }

    fn local_place(&self, local: LocalId) -> Place {
        function_local_place(&self.function, local)
    }

    fn operand_place(&self, local: LocalId) -> Operand {
        Operand::Place(self.local_place(local))
    }

    fn operand_type(&self, operand: &Operand) -> Type {
        self.air_type(self.operand_ty(operand))
    }

    fn operand_ty(&self, operand: &Operand) -> TypeId {
        typing::operand_ty(&self.cx.program, operand).expect("lowered operand const should exist")
    }

    fn dummy_operand(&mut self, ty: TypeId) -> Result<Operand, LowerError> {
        let value = match self.cx.program.type_data(ty) {
            TypeData::Int => ConstValue::Int(0),
            TypeData::Float => ConstValue::Float(0.0),
            TypeData::Bool => ConstValue::Bool(false),
            TypeData::String => ConstValue::String("".into()),
            TypeData::Char => ConstValue::Char('\0'),
            _ => {
                return Err(LowerError::UnsupportedType {
                    ty: Box::new(Type::Infer),
                });
            }
        };
        Ok(Operand::Const(
            self.cx.program.alloc_const(ConstData { ty, value }),
        ))
    }

    fn air_type(&self, ty: TypeId) -> Type {
        match self.cx.program.type_data(ty) {
            TypeData::Int => Type::Int,
            TypeData::Float => Type::Float,
            TypeData::Bool => Type::Bool,
            TypeData::String => Type::String,
            TypeData::Char => Type::Char,
            TypeData::Void => Type::Void,
            _ => Type::Infer,
        }
    }

    fn emit_init(&mut self, local: LocalId, value: RValue) -> Result<(), LowerError> {
        self.ensure_open()?;
        self.block.stmts.push(AirStmt::Init { local, value });
        Ok(())
    }

    fn emit_init_place(&mut self, place: Place, value: RValue) -> Result<(), LowerError> {
        match place.root.local().filter(|_| place.projection.is_empty()) {
            Some(local) => self.emit_init(local, value),
            None => self.emit_assign(place, value),
        }
    }

    fn emit_assign(&mut self, dst: Place, value: RValue) -> Result<(), LowerError> {
        if let PlaceRoot::Global(global) = dst.root
            && dst.projection.is_empty()
        {
            return self.emit_global_update_root(global, value);
        }
        self.ensure_open()?;
        self.block.stmts.push(AirStmt::Assign { dst, value });
        Ok(())
    }

    fn emit_eval(&mut self, value: RValue) -> Result<(), LowerError> {
        self.ensure_open()?;
        self.block.stmts.push(AirStmt::Eval(value));
        Ok(())
    }

    fn emit_global_ensure(&mut self, global: GlobalId) -> Result<(), LowerError> {
        self.ensure_open()?;
        self.block.stmts.push(AirStmt::GlobalEnsure { global });
        Ok(())
    }

    fn emit_global_set_root(&mut self, global: GlobalId, value: RValue) -> Result<(), LowerError> {
        self.ensure_open()?;
        self.block.stmts.push(AirStmt::GlobalSetRoot {
            global,
            value,
            init: GlobalInitEffect::StoreWithoutInit,
        });
        Ok(())
    }

    fn emit_global_update_root(
        &mut self,
        global: GlobalId,
        value: RValue,
    ) -> Result<(), LowerError> {
        self.ensure_open()?;
        self.block
            .stmts
            .push(AirStmt::GlobalUpdateRoot { global, value });
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

    fn emit_mut_typed_temp(&mut self, ty: TypeId, value: RValue) -> Result<Operand, LowerError> {
        let local = self.push_local(None, None, ty, AirMutability::Mutable, LocalKind::Temp);
        self.emit_init(local, value)?;
        Ok(self.operand_place(local))
    }

    fn terminate(&mut self, tail: AirTail) -> Result<(), LowerError> {
        self.ensure_open()?;
        self.block.tail = tail;
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

    fn source_span(&self, span: crate::span::Span) -> SourceSpan {
        SourceSpan::from_byte_span(self.source, span)
    }
}

fn air_block_falls_through(block: &AirBlock) -> bool {
    matches!(block.tail, AirTail::None)
}

fn optional_match_payload_mode(plan: &OptionalMatchPlan<'_>, alias: bool) -> PayloadMode {
    match plan.some {
        Some((pattern, body)) if !optional_plan_arm_is_default(plan, pattern, body) => {
            optional_payload_mode(pattern, alias)
        }
        _ => PayloadMode::None,
    }
}

fn optional_payload_mode(pattern: &ast::PatternNode, alias: bool) -> PayloadMode {
    match &pattern.node {
        Pattern::Wildcard => PayloadMode::None,
        Pattern::Optional(inner) => optional_payload_mode(inner, alias),
        Pattern::Ident(_) if alias => PayloadMode::Alias,
        _ => PayloadMode::Copy,
    }
}

fn direct_dyn_failable_alias_pattern(if_let: &ast::IfLetNode) -> Option<&ast::PatternNode> {
    (matches!(if_let.node.value.node.kind, ExprKind::FailableCast(_))
        && if_let.node.head.is_ref()
        && matches!(if_let.node.pattern.node, Pattern::Ident(_)))
    .then_some(&if_let.node.pattern)
}

fn direct_failable_payload_pattern(
    if_let: &ast::IfLetNode,
) -> Result<Option<&ast::PatternNode>, LowerError> {
    if !matches!(if_let.node.value.node.kind, ExprKind::FailableCast(_)) {
        return Ok(None);
    }
    if if_let.node.head.is_ref() {
        return Err(unsupported_expr(&if_let.node.value));
    }
    match if_let.node.pattern.node {
        Pattern::Ident(_) => Ok(Some(&if_let.node.pattern)),
        _ => Ok(None),
    }
}

fn checked_pattern_has_flag_value(pattern: &CheckedPattern) -> bool {
    match pattern {
        CheckedPattern::FlagValue { .. } => true,
        CheckedPattern::Or(alternatives) => alternatives
            .iter()
            .any(|alternative| checked_pattern_has_flag_value(&alternative.pattern)),
        _ => false,
    }
}

fn classify_optional_pattern(
    pattern: &ast::PatternNode,
) -> Result<OptionalPattern<'_>, LowerError> {
    match pattern.node.optional_payload() {
        ast::OptionalPayloadPattern::Some(payload) => Ok(OptionalPattern::Some(payload)),
        ast::OptionalPayloadPattern::None => Ok(OptionalPattern::None),
        ast::OptionalPayloadPattern::NotOptional => Err(unsupported_pattern_stmt(pattern)),
    }
}

fn optional_match_plan<'a>(
    owner: &ExprNode,
    arms: &'a [ast::MatchArmNode],
    checked: &CheckedMatchPlan,
) -> Result<OptionalMatchPlan<'a>, LowerError> {
    let mut plan = OptionalMatchPlan {
        some: None,
        none: None,
        default: None,
    };
    if arms.len() != checked.arms.len() {
        return Err(LowerError::MissingTypecheckFacts);
    }
    for (arm, checked_arm) in arms.iter().zip(&checked.arms) {
        if plan.default.is_some() {
            continue;
        }
        let ast::MatchArmHead::Pattern(pattern) = &arm.node.head else {
            return Err(unsupported_expr(owner));
        };
        match optional_arm(pattern, &checked_arm.pattern, &arm.node.body)? {
            OptionalArm::Some(pattern, body) => {
                if plan.some.replace((pattern, body)).is_some() {
                    return Err(unsupported_expr(owner));
                }
            }
            OptionalArm::None(body) => {
                if plan.none.replace(body).is_some() {
                    return Err(unsupported_expr(owner));
                }
            }
            OptionalArm::Default(pattern, body) => {
                if plan.some.is_none() {
                    plan.some = Some((pattern, body));
                }
                if plan.none.is_none() {
                    plan.none = Some(body);
                }
                plan.default = Some((pattern, body));
            }
        }
    }
    if plan.default.is_none() && (plan.some.is_none() || plan.none.is_none()) {
        return Err(unsupported_expr(owner));
    }
    Ok(plan)
}

fn optional_arm<'a>(
    pattern: &'a ast::PatternNode,
    checked: &CheckedPattern,
    body: &'a ExprNode,
) -> Result<OptionalArm<'a>, LowerError> {
    match checked {
        CheckedPattern::Wildcard | CheckedPattern::Binding(_) => {
            Ok(OptionalArm::Default(pattern, body))
        }
        CheckedPattern::Nil => Ok(OptionalArm::None(body)),
        CheckedPattern::OptionalSome(_) => match classify_optional_pattern(pattern)? {
            OptionalPattern::Some(pattern) => Ok(OptionalArm::Some(pattern, body)),
            OptionalPattern::None => Err(unsupported_pattern_stmt(pattern)),
        },
        CheckedPattern::Enum { variant, .. } if *variant == Ident::new("None") => {
            Ok(OptionalArm::None(body))
        }
        CheckedPattern::Enum { variant, .. } if *variant == Ident::new("Some") => {
            match classify_optional_pattern(pattern)? {
                OptionalPattern::Some(pattern) => Ok(OptionalArm::Some(pattern, body)),
                OptionalPattern::None => Err(unsupported_pattern_stmt(pattern)),
            }
        }
        _ => Err(unsupported_pattern_stmt(pattern)),
    }
}

fn optional_plan_arm_is_default(
    plan: &OptionalMatchPlan<'_>,
    pattern: &ast::PatternNode,
    body: &ExprNode,
) -> bool {
    plan.default.is_some_and(|(default_pattern, default_body)| {
        std::ptr::eq(default_pattern, pattern) && std::ptr::eq(default_body, body)
    })
}

fn pattern_ident(pattern: &ast::PatternNode) -> Result<Ident, LowerError> {
    match &pattern.node {
        Pattern::Ident(name) => Ok(*name),
        Pattern::Optional(inner) => pattern_ident(inner),
        _ => Err(unsupported_pattern_stmt(pattern)),
    }
}

fn unsupported_expr(expr: &ExprNode) -> LowerError {
    LowerError::UnsupportedExpr {
        expr_id: expr.node.id,
        kind: expr.node.kind.variant_name(),
    }
}

fn unsupported_pattern_stmt(pattern: &ast::PatternNode) -> LowerError {
    LowerError::UnsupportedStmt {
        kind: pattern.node.variant_name(),
        span: None,
    }
}

fn is_nil_lit(expr: &ExprNode) -> bool {
    matches!(expr.node.kind, ExprKind::Lit(Lit::Nil))
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

fn iter_count_check_accepts(check: IterCountCheck, value: i64) -> bool {
    match check {
        IterCountCheck::SkipNonNegative | IterCountCheck::TakeNonNegative => value >= 0,
        IterCountCheck::StepByPositive => value > 0,
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

fn lower_enum_repr(repr: TcEnumRepr) -> EnumRepr {
    match repr {
        TcEnumRepr::Adt => EnumRepr::Adt,
        TcEnumRepr::RawInt => EnumRepr::RawInt,
        TcEnumRepr::RawString => EnumRepr::RawString,
    }
}

fn lower_raw_enum_value(value: &TcRawEnumValue) -> RawEnumValue {
    match value {
        TcRawEnumValue::Int(value) => RawEnumValue::Int(*value),
        TcRawEnumValue::String(value) => RawEnumValue::String(value.clone()),
    }
}

fn enum_core_kind(decls: &DeclarationIndex, key: &NominalKey) -> Option<CoreEnumKind> {
    if decls.core_option_key().as_ref() == Some(key) {
        Some(CoreEnumKind::Option)
    } else if decls.core_result_key().as_ref() == Some(key) {
        Some(CoreEnumKind::Result)
    } else {
        None
    }
}

fn contract_schema_source_type(
    schema: &ContractTypeSchema,
    surfaces: &ContractSurfaceSchemas,
    decls: &DeclarationIndex,
) -> Type {
    match schema {
        ContractTypeSchema::Any => Type::Any,
        ContractTypeSchema::Int => Type::Int,
        ContractTypeSchema::Float => Type::Float,
        ContractTypeSchema::Bool => Type::Bool,
        ContractTypeSchema::String => Type::String,
        ContractTypeSchema::Char => Type::Char,
        ContractTypeSchema::Void => Type::Void,
        ContractTypeSchema::Func { params, ret } => Type::Func {
            params: params
                .iter()
                .map(|param| {
                    ast::FuncParam::new(
                        contract_schema_source_type(&param.ty, surfaces, decls),
                        param.mutable,
                        param.cast_accept,
                        param.escape,
                    )
                })
                .collect(),
            ret: Box::new(match ret.as_ref() {
                ContractReturnSchema::Value(ty) => {
                    ast::ReturnSpec::value(contract_schema_source_type(ty, surfaces, decls))
                }
                ContractReturnSchema::Place(ty) => {
                    ast::ReturnSpec::place(contract_schema_source_type(ty, surfaces, decls))
                }
                ContractReturnSchema::Iter => ast::ReturnSpec::iter(),
            }),
        },
        ContractTypeSchema::Dyn(surface) => Type::Dyn(
            surfaces
                .representative_ref(*surface)
                .expect("canonical dynamic surface has a representative"),
        ),
        ContractTypeSchema::Tuple(items) => Type::Tuple(
            items
                .iter()
                .map(|item| contract_schema_source_type(item, surfaces, decls))
                .collect(),
        ),
        ContractTypeSchema::Nominal {
            id,
            type_args,
            const_args,
        } => {
            let key = decls
                .nominal(id)
                .expect("contract nominal missing registry metadata");
            let type_args = type_args
                .iter()
                .map(|arg| contract_schema_source_type(arg, surfaces, decls))
                .collect::<Vec<_>>();
            nominal_type_with_args(key, &type_args, const_args)
        }
        ContractTypeSchema::List(elem) => Type::List {
            elem: Box::new(contract_schema_source_type(elem, surfaces, decls)),
        },
        ContractTypeSchema::Array { elem, len } => Type::Array {
            elem: Box::new(contract_schema_source_type(elem, surfaces, decls)),
            len: len.clone(),
        },
        ContractTypeSchema::Map { key, value } => Type::Map {
            key: Box::new(contract_schema_source_type(key, surfaces, decls)),
            value: Box::new(contract_schema_source_type(value, surfaces, decls)),
        },
        ContractTypeSchema::Slice(elem) => Type::Slice {
            elem: Box::new(contract_schema_source_type(elem, surfaces, decls)),
        },
        ContractTypeSchema::Optional(inner) => Type::Optional {
            inner: Box::new(contract_schema_source_type(inner, surfaces, decls)),
        },
    }
}

fn source_param_mode(mutable: bool) -> ParamMode {
    if mutable {
        ParamMode::MutBorrow
    } else {
        ParamMode::SharedBorrow
    }
}

fn air_binding_id(binding: BindingId) -> AirBindingId {
    AirBindingId::from_index(binding.0 as usize)
}

fn collect_witness_extern_targets(target: &WitnessSlotTarget, externs: &mut Vec<ExternUseTarget>) {
    match target {
        WitnessSlotTarget::Extern { method, .. } => {
            externs.push(ExternUseTarget::Method(*method));
        }
        WitnessSlotTarget::Promoted { target, .. } => {
            collect_witness_extern_targets(target, externs);
        }
        WitnessSlotTarget::Direct { .. } | WitnessSlotTarget::Extend { .. } => {}
    }
}

fn extern_use_requires_decl(externs: &ExternCatalog, target: ExternUseTarget) -> bool {
    match target {
        ExternUseTarget::FieldRead(field) | ExternUseTarget::FieldWrite(field) => {
            externs.field_ref(field).1.computed
        }
        ExternUseTarget::Function(_)
        | ExternUseTarget::Method(_)
        | ExternUseTarget::Static(_)
        | ExternUseTarget::Init(_)
        | ExternUseTarget::UnaryOperator(_)
        | ExternUseTarget::BinaryOperator(_) => true,
    }
}

fn extern_sort_key(externs: &ExternCatalog, target: ExternUseTarget) -> String {
    match target {
        ExternUseTarget::Function(id) => {
            let function = externs.function(id);
            format!("{:?}::{}", function.key.module, function.key.name.as_str())
        }
        ExternUseTarget::FieldRead(field) | ExternUseTarget::FieldWrite(field) => {
            let (ty, field) = externs.field_ref(field);
            format!(
                "{:?}::{}::{}",
                ty.key.module,
                ty.key.name.as_str(),
                field.name.as_str()
            )
        }
        ExternUseTarget::Method(method) => {
            let (ty, method) = externs.method_ref(method);
            format!(
                "{:?}::{}::{}",
                ty.key.module,
                ty.key.name.as_str(),
                method.name.as_str()
            )
        }
        ExternUseTarget::Static(static_method) => {
            let (ty, static_method) = externs.static_ref(static_method);
            format!(
                "{:?}::{}::{}",
                ty.key.module,
                ty.key.name.as_str(),
                static_method.name.as_str()
            )
        }
        ExternUseTarget::Init(owner) => {
            let ty = externs.ty(owner);
            format!("{:?}::{}::__init", ty.key.module, ty.key.name.as_str())
        }
        ExternUseTarget::UnaryOperator(operator) | ExternUseTarget::BinaryOperator(operator) => {
            let (ty, operator) = externs.operator_ref(operator);
            format!(
                "{:?}::{}::{:?}",
                ty.key.module,
                ty.key.name.as_str(),
                operator.op
            )
        }
    }
}

fn unsupported_extern_kind(target: ExternUseTarget) -> UnsupportedExternUseKind {
    match target {
        ExternUseTarget::Function(_) => UnsupportedExternUseKind::MissingFunction,
        ExternUseTarget::FieldRead(_) => UnsupportedExternUseKind::FieldRead,
        ExternUseTarget::FieldWrite(_) => UnsupportedExternUseKind::FieldWrite,
        ExternUseTarget::Method(_) => UnsupportedExternUseKind::Method,
        ExternUseTarget::Static(_) => UnsupportedExternUseKind::Static,
        ExternUseTarget::Init(_) => UnsupportedExternUseKind::Init,
        ExternUseTarget::UnaryOperator(_) => UnsupportedExternUseKind::UnaryOperator,
        ExternUseTarget::BinaryOperator(_) => UnsupportedExternUseKind::BinaryOperator,
    }
}

fn param_flow_mode(flow: ParamFlow) -> ParamMode {
    match flow {
        ParamFlow::Value => ParamMode::Value,
        ParamFlow::Borrow => ParamMode::SharedBorrow,
        ParamFlow::MutBorrow => ParamMode::MutBorrow,
    }
}

fn checked_extern_param(
    program: &Program,
    ty: TypeId,
    mode: ParamMode,
    escape: ParamEscape,
) -> Result<ExternParamDecl, LowerError> {
    if escape == ParamEscape::Escaping
        && (mode != ParamMode::Value || !matches!(program.type_data(ty), TypeData::Function(_)))
    {
        return Err(LowerError::UnsupportedExternSignature);
    }
    Ok(ExternParamDecl { ty, mode, escape })
}

#[cfg(test)]
pub(crate) fn lower_with_modules(
    root: &ast::Program,
    resolved: &crate::resolve::ResolveResult,
    semantic: &SemanticProgram,
    typecheck_facts: &TypecheckFacts,
    config: AirLowerConfig,
) -> Result<Program, LowerError> {
    let source_index = crate::source_ast::SourceAstIndex::new(root, resolved);
    lower_with_source_index(&source_index, semantic, typecheck_facts, config)
}

pub(crate) fn lower_with_source_index(
    source_index: &crate::source_ast::SourceAstIndex,
    semantic: &SemanticProgram,
    typecheck_facts: &TypecheckFacts,
    config: AirLowerConfig,
) -> Result<Program, LowerError> {
    validate_lambda_fact_carrier(typecheck_facts);
    let index = SourceProgramIndex::new(source_index, semantic);
    let callable_facts = SemanticCallableFacts::new(semantic);
    let AirLowerConfig { roots } = config;
    let entry = roots.entry.clone();
    let roots = roots.normalized();
    validate_roots(&roots, &callable_facts)?;
    let functions = ReachableItems::new(&index, semantic, &callable_facts, roots)?;
    let mut cx = LowerCx {
        decls: Some(semantic.declarations.clone()),
        externs: Some(semantic.externs.clone()),
        contract_surfaces: Some(semantic.contract_surfaces.clone()),
        typecheck_facts: Some(typecheck_facts),
        ..LowerCx::default()
    };
    cx.lower_contract_surfaces()?;
    cx.lower_function_shells(&source_index.modules, &functions)?;
    ownership::finalize(&mut cx.program)
        .map_err(|errors| LowerError::Ownership(errors.into_boxed_slice()))?;
    cx.attach_stringify_overrides(&functions);
    if let Some(entry) = &entry {
        cx.set_entry(entry)?;
    }
    cx.lower_extern_declarations(&functions, semantic)?;
    cx.lower_contract_declarations(&functions, semantic)?;
    cx.lower_function_bodies(&functions)?;
    ownership::finalize(&mut cx.program)
        .map_err(|errors| LowerError::Ownership(errors.into_boxed_slice()))?;
    verify(&cx.program).map_err(|errors| LowerError::Verify(errors.into_boxed_slice()))?;
    reject_any_types(&cx.program)?;
    Ok(cx.program)
}

fn validate_lambda_fact_carrier(facts: &TypecheckFacts) {
    for ((lambda, binding), capture) in facts.lambda_captures() {
        debug_assert_eq!(*lambda, capture.lambda_id);
        debug_assert_eq!(*binding, capture.binding_id);
    }
    for (binding, promotion) in facts.capture_cell_requirements() {
        debug_assert_eq!(*binding, promotion.binding_id);
    }
}

fn collect_safe_field_chain(expr: &ExprNode) -> Option<(&ExprNode, Vec<ChainStep<'_>>)> {
    let (base, steps) = collect_field_chain(expr)?;
    steps
        .iter()
        .any(chain_step_is_safe)
        .then_some((base, steps))
}

fn collect_field_chain(expr: &ExprNode) -> Option<(&ExprNode, Vec<ChainStep<'_>>)> {
    match &expr.node.kind {
        ExprKind::Field(field) => {
            let (base, mut steps) = collect_field_chain(&field.node.target)
                .unwrap_or_else(|| (field.node.target.as_ref(), vec![]));
            steps.push(ChainStep::Field { expr, node: field });
            Some((base, steps))
        }
        ExprKind::Index(index) => {
            let (base, mut steps) = collect_field_chain(&index.node.target)
                .unwrap_or_else(|| (index.node.target.as_ref(), vec![]));
            steps.push(ChainStep::Index { expr, node: index });
            Some((base, steps))
        }
        ExprKind::TupleIndex(tuple) => {
            let (base, mut steps) = collect_field_chain(&tuple.node.target)
                .unwrap_or_else(|| (tuple.node.target.as_ref(), vec![]));
            steps.push(ChainStep::TupleIndex { expr, node: tuple });
            Some((base, steps))
        }
        ExprKind::Call(call) => {
            let (base, mut steps) = collect_field_chain(&call.node.func)
                .unwrap_or_else(|| (call.node.func.as_ref(), vec![]));
            steps.push(ChainStep::Call { expr, node: call });
            Some((base, steps))
        }
        _ => None,
    }
}

fn chain_step_is_safe(step: &ChainStep<'_>) -> bool {
    match step {
        ChainStep::Field { node, .. } => node.node.safe,
        ChainStep::Index { node, .. } => node.node.safe,
        ChainStep::TupleIndex { .. } => false,
        ChainStep::Call { node, .. } => node.node.safe,
    }
}

fn field_call_step<'a, 's>(
    step: &'s ChainStep<'a>,
    rest: &'s [ChainStep<'a>],
) -> Option<(&'a ExprNode, &'a ast::CallNode, &'s [ChainStep<'a>])> {
    let ChainStep::Field { expr, .. } = step else {
        return None;
    };
    let Some((
        ChainStep::Call {
            expr: call_expr,
            node: call,
        },
        call_rest,
    )) = rest.split_first()
    else {
        return None;
    };
    if call.node.safe || call.node.func.node.id != expr.node.id {
        return None;
    }
    Some((call_expr, call, call_rest))
}

fn projection_root(expr: &ExprNode) -> Option<&ExprNode> {
    match &expr.node.kind {
        ExprKind::Ident(_) => Some(expr),
        ExprKind::Field(field) => projection_root(&field.node.target),
        ExprKind::TupleIndex(tuple) => projection_root(&tuple.node.target),
        ExprKind::Index(index) => projection_root(&index.node.target),
        _ => None,
    }
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

fn type_is_concrete(ty: &Type) -> bool {
    !type_has_unfinished_facts(ty)
}

fn lower_global_mutability(mutability: AstMutability) -> AirMutability {
    match mutability {
        AstMutability::Immutable => AirMutability::Immutable,
        AstMutability::Mutable => AirMutability::Mutable,
    }
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
struct SourceProgramIndex<'a> {
    source: &'a crate::source_ast::SourceAstIndex,
    callables: HashMap<CallableId, SourceCallable<'a>>,
}

fn source_const_matches_type(value: &ast::ConstValue, ty: &Type) -> bool {
    value.ty() == *ty
}

fn air_const_matches_type(value: &ast::ConstValue, ty: &TypeData) -> bool {
    matches!(
        (value, ty),
        (ast::ConstValue::Int(_), TypeData::Int)
            | (ast::ConstValue::Float(_), TypeData::Float)
            | (ast::ConstValue::Bool(_), TypeData::Bool)
            | (ast::ConstValue::String(_), TypeData::String)
            | (ast::ConstValue::Char(_), TypeData::Char)
    )
}

fn lower_const_specialization_value(value: &ast::ConstValue) -> ConstValue {
    match value {
        ast::ConstValue::Int(value) => ConstValue::Int(*value),
        ast::ConstValue::Float(value) => ConstValue::Float(*value),
        ast::ConstValue::Bool(value) => ConstValue::Bool(*value),
        ast::ConstValue::String(value) => ConstValue::String(value.clone().into_boxed_str()),
        ast::ConstValue::Char(value) => ConstValue::Char(*value),
    }
}

#[derive(Debug, Clone, Copy)]
struct SourceCallable<'a> {
    module: usize,
    source: SourceId,
    node: crate::source_ast::SourceCallable<'a>,
}

impl<'a> SourceCallable<'a> {
    fn module(self) -> usize {
        self.module
    }

    fn source(self) -> SourceId {
        self.source
    }

    fn name(self) -> Ident {
        self.node.name()
    }

    fn body(self) -> &'a BlockNode {
        self.node.body()
    }

    fn params(self) -> Vec<Ident> {
        self.node.params()
    }

    fn has_generics(self) -> bool {
        self.node.has_generics()
    }

    fn is_instance_method(self) -> bool {
        self.node.receiver().is_some()
    }

    fn function_kind(self) -> FunctionKind {
        if self.node.is_method() {
            FunctionKind::Method
        } else {
            FunctionKind::Normal
        }
    }

    fn owner(self) -> Option<FunctionOwner> {
        self.node.owner().map(|name| FunctionOwner { name })
    }
}

#[derive(Debug)]
struct ReachableItems<'a> {
    index: &'a SourceProgramIndex<'a>,
    body_facts: &'a HashMap<BodyInstanceKey, SemanticBodyFacts>,
    callable_facts: &'a SemanticCallableFacts<'a>,
    items: Vec<ReachableItem<'a>>,
}

#[derive(Debug)]
struct ReachableFactSlice<'a> {
    body: BodyInstanceKey,
    facts: &'a SemanticBodyFacts,
    exprs: Option<&'a HashSet<ExprId>>,
}

impl ReachableFactSlice<'_> {
    fn includes(&self, expr: ExprId) -> bool {
        self.exprs
            .as_ref()
            .is_none_or(|exprs| exprs.contains(&expr))
    }
}

#[derive(Debug)]
struct ReachableItem<'a> {
    source: ReachableSource<'a>,
    body: BodyInstanceKey,
    body_facts: ReachableBodyFacts<'a>,
    source_id: SourceId,
}

#[derive(Debug)]
enum ReachableSource<'a> {
    Callable {
        callable: SourceCallable<'a>,
        fact: &'a SemanticFunctionInstanceFact,
    },
    Lambda {
        owner: BodyInstanceKey,
        lambda: &'a ast::LambdaNode,
        ty: &'a Type,
    },
    Global {
        global: &'a crate::source_ast::SourceGlobal,
        sig: GlobalSig,
    },
    CastFrom {
        cast: crate::source_ast::SourceCastFrom<'a>,
        signature: CastFromSignature,
    },
}

#[derive(Debug)]
enum ReachableBodyFacts<'a> {
    Facts(&'a SemanticBodyFacts),
    Empty(Box<SemanticBodyFacts>),
}

enum OptionalSubject {
    Place {
        place: Place,
        optional_ty: TypeId,
        inner_ty: TypeId,
    },
    MapEntry {
        map: Place,
        key: Operand,
        optional_ty: TypeId,
        inner_ty: TypeId,
    },
}

impl OptionalSubject {
    fn optional_ty(&self) -> TypeId {
        match self {
            Self::Place { optional_ty, .. } | Self::MapEntry { optional_ty, .. } => *optional_ty,
        }
    }

    fn inner_ty(&self) -> TypeId {
        match self {
            Self::Place { inner_ty, .. } | Self::MapEntry { inner_ty, .. } => *inner_ty,
        }
    }

    fn place(&self) -> Option<&Place> {
        match self {
            Self::Place { place, .. } => Some(place),
            Self::MapEntry { .. } => None,
        }
    }
}

const RANGE_START_FIELD: &str = "start";
const RANGE_END_FIELD: &str = "end";

struct IterForSource<'a> {
    source: &'a ExprNode,
    range: Option<IterForRange<'a>>,
    adapters: Vec<IterForAdapter<'a>>,
    projection: IterForProjection,
}

struct IterForRange<'a> {
    start: &'a ExprNode,
    end: &'a ExprNode,
    inclusive: bool,
}

struct IterForBranch<'a> {
    cond: &'a ExprNode,
    then_source: IterForSource<'a>,
    else_source: IterForSource<'a>,
}

struct IterHelper<'a> {
    callable: SourceCallable<'a>,
    facts: &'a SemanticBodyFacts,
}

#[derive(Default)]
struct IterParamSubst<'a> {
    params: Vec<Ident>,
    args: Vec<&'a ExprNode>,
    locals: Vec<(Ident, &'a ExprNode)>,
}

impl<'a> IterParamSubst<'a> {
    fn with_block_locals(&self, block: &'a BlockNode) -> Self {
        let mut locals = self.locals.clone();
        locals.extend(iter_helper_locals(block));
        Self {
            params: self.params.clone(),
            args: self.args.clone(),
            locals,
        }
    }

    fn expr(&self, expr: &'a ExprNode) -> &'a ExprNode {
        let mut current = expr;
        for _ in 0..=self.locals.len() + self.params.len() {
            let ExprKind::Ident(name) = current.node.kind else {
                return current;
            };
            let Some(next) = self
                .locals
                .iter()
                .rev()
                .find_map(|(local, value)| (*local == name).then_some(*value))
                .or_else(|| {
                    self.params
                        .iter()
                        .position(|param| *param == name)
                        .and_then(|index| self.args.get(index).copied())
                })
            else {
                return current;
            };
            if next.node.id == current.node.id {
                return current;
            }
            current = next;
        }
        current
    }
}

fn iter_for_range_source<'a>(
    expr: &'a ExprNode,
    subst: &IterParamSubst<'a>,
) -> Option<IterForRange<'a>> {
    let ExprKind::Range(range) = &expr.node.kind else {
        return None;
    };
    let ast::Range::Bounded {
        start,
        end,
        inclusive,
    } = &range.node
    else {
        return None;
    };
    Some(IterForRange {
        start: subst.expr(start),
        end: subst.expr(end),
        inclusive: *inclusive,
    })
}

fn iter_helper_locals(body: &BlockNode) -> Vec<(Ident, &ExprNode)> {
    body.node
        .stmts
        .iter()
        .filter_map(|stmt| {
            let Stmt::Binding(binding) = &stmt.node else {
                return None;
            };
            let Pattern::Ident(name) = binding.node.pattern.node else {
                return None;
            };
            Some((name, &binding.node.value))
        })
        .collect()
}

fn iter_helper_terminal_expr(body: &BlockNode) -> Option<&ExprNode> {
    body.node.tail.as_deref().or_else(|| {
        body.node.stmts.last().and_then(|stmt| match &stmt.node {
            Stmt::Expr(expr) => Some(expr),
            Stmt::Return(ret) => ret.node.value.as_ref(),
            _ => None,
        })
    })
}

fn iter_helper_expr<'a>(body: &'a BlockNode, subst: &IterParamSubst<'a>) -> Option<&'a ExprNode> {
    let expr = iter_helper_terminal_expr(body)?;
    if let ExprKind::If(if_) = &expr.node.kind {
        let ExprKind::Lit(Lit::Bool(cond)) = subst.expr(&if_.node.cond).node.kind else {
            return None;
        };
        let branch = if cond {
            &if_.node.then_block
        } else {
            if_.node.else_block.as_ref()?
        };
        let subst = subst.with_block_locals(branch);
        return iter_helper_expr(branch, &subst);
    }
    Some(expr)
}

#[derive(Clone, Copy)]
enum IterForAdapter<'a> {
    Rev,
    Skip(&'a ExprNode),
    Take(&'a ExprNode),
    StepBy(&'a ExprNode),
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum IterForProjection {
    Default,
    MapKeys,
    MapValues,
}

struct RangeForParts {
    start: Operand,
    end: Operand,
    inclusive: bool,
}

struct ForPlan {
    root_kind: AirCollectionRootKind,
    mode: AirCollectionLoanMode,
    root: Place,
    len: LocalId,
    index: LocalId,
    ordinal_plan: AirOrdinalPlan,
    bindings: Vec<ForBindingPlan>,
}

impl ForPlan {
    fn ordinal(&self) -> Option<LocalId> {
        self.bindings.iter().find_map(|binding| match binding {
            ForBindingPlan::OwnedIndex { local, .. } => Some(*local),
            _ => None,
        })
    }
}

enum ForBindingPlan {
    OwnedIndex {
        pattern: ast::PatternNode,
        local: LocalId,
    },
    OwnedElement {
        pattern: ast::PatternNode,
        ty: TypeId,
    },
    ElementSlot {
        pattern: ast::PatternNode,
        local: LocalId,
        ty: TypeId,
    },
    OwnedMapEntry {
        pattern: ast::PatternNode,
        ty: TypeId,
    },
    OwnedMapKey {
        pattern: ast::PatternNode,
        ty: TypeId,
    },
    OwnedMapValue {
        pattern: ast::PatternNode,
        ty: TypeId,
    },
    MapValueSlot {
        pattern: ast::PatternNode,
        local: LocalId,
        ty: TypeId,
    },
}

enum ChainStep<'a> {
    Field {
        expr: &'a ExprNode,
        node: &'a ast::FieldAccessNode,
    },
    Index {
        expr: &'a ExprNode,
        node: &'a ast::IndexNode,
    },
    TupleIndex {
        expr: &'a ExprNode,
        node: &'a ast::TupleIndexNode,
    },
    Call {
        expr: &'a ExprNode,
        node: &'a ast::CallNode,
    },
}

#[derive(Clone, Copy)]
enum ChainMode {
    Value { result: LocalId, result_ty: TypeId },
    Effect,
}

#[derive(Clone, Copy)]
enum OptionalPattern<'a> {
    Some(&'a ast::PatternNode),
    None,
}

enum OptionalArm<'a> {
    Some(&'a ast::PatternNode, &'a ExprNode),
    None(&'a ExprNode),
    Default(&'a ast::PatternNode, &'a ExprNode),
}

struct OptionalMatchPlan<'a> {
    some: Option<(&'a ast::PatternNode, &'a ExprNode)>,
    none: Option<&'a ExprNode>,
    default: Option<(&'a ast::PatternNode, &'a ExprNode)>,
}

#[derive(Clone, Copy)]
enum MatchOutput {
    Effect,
    Value { result: LocalId, result_ty: TypeId },
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum PayloadMode {
    None,
    Copy,
    Alias,
}

impl PayloadMode {
    fn needs_payload(self) -> bool {
        !matches!(self, Self::None)
    }

    fn payload_ref(self) -> bool {
        matches!(self, Self::Alias)
    }
}

fn lowered_capture_kind(
    expr_id: ExprId,
    plan: LambdaCaptureRuntimePlan,
) -> Result<LoweredCaptureKind, LowerError> {
    match plan {
        LambdaCaptureRuntimePlan::NoRuntime => Ok(LoweredCaptureKind::NoRuntime),
        LambdaCaptureRuntimePlan::ReadonlyOwned => Ok(LoweredCaptureKind::ReadonlyLocal),
        LambdaCaptureRuntimePlan::MutableCaptureCell => Ok(LoweredCaptureKind::CaptureCell),
        LambdaCaptureRuntimePlan::ScopedBorrow(_) => Ok(LoweredCaptureKind::ScopedBorrow),
        LambdaCaptureRuntimePlan::Illegal(_) => Err(lambda_capture_gap(expr_id)),
    }
}

fn lambda_capture_gap(expr_id: ExprId) -> LowerError {
    LowerError::UnsupportedExpr {
        expr_id,
        kind: "UnsupportedLambdaCapture",
    }
}

fn mutable_place_projection_gap(expr_id: ExprId) -> LowerError {
    LowerError::UnsupportedExpr {
        expr_id,
        kind: "UnsupportedMutablePlaceProjection",
    }
}

fn exact_local_capture_source(
    expr_id: ExprId,
    owner: FunctionId,
    owner_function: &Function,
    sources: &HashMap<BindingId, LambdaCaptureSource>,
    capture: &LambdaCaptureFact,
    ty: TypeId,
) -> Result<CaptureLocalSource, LowerError> {
    match exact_capture_source(expr_id, sources, capture, ty)? {
        LambdaCaptureSource::Local(place) => {
            exact_owner_local_capture_source(expr_id, owner, owner_function, capture, &place)
                .map(|(source, _)| source)
        }
        LambdaCaptureSource::Capture { decl, .. } => match decl {
            LambdaCaptureDecl::ReadonlyLocal { source, .. } => Ok(source),
            _ => Err(lambda_capture_gap(expr_id)),
        },
    }
}

fn exact_owner_local_capture_source<'a>(
    expr_id: ExprId,
    owner: FunctionId,
    owner_function: &'a Function,
    capture: &LambdaCaptureFact,
    place: &Place,
) -> Result<(CaptureLocalSource, &'a Local), LowerError> {
    let PlaceRoot::Local(local) = place.root else {
        return Err(lambda_capture_gap(expr_id));
    };
    let source = CaptureLocalSource { owner, local };
    let local_decl = &owner_function.locals[local.index()];
    if local_decl.binding != Some(air_binding_id(capture.binding_id)) {
        return Err(lambda_capture_gap(expr_id));
    }
    Ok((source, local_decl))
}

fn exact_local_capture_place(
    expr_id: ExprId,
    sources: &HashMap<BindingId, LambdaCaptureSource>,
    capture: &LambdaCaptureFact,
    ty: TypeId,
) -> Result<Place, LowerError> {
    match exact_capture_source(expr_id, sources, capture, ty)? {
        LambdaCaptureSource::Local(place) => Ok(place),
        LambdaCaptureSource::Capture { slot, .. } => Ok(Place {
            root: PlaceRoot::LambdaCapture(slot),
            projection: vec![],
            ty,
        }),
    }
}

fn exact_capture_cell(
    expr_id: ExprId,
    sources: &HashMap<BindingId, LambdaCaptureSource>,
    capture: &LambdaCaptureFact,
    ty: TypeId,
) -> Result<CaptureCellId, LowerError> {
    match exact_capture_source(expr_id, sources, capture, ty)? {
        LambdaCaptureSource::Local(place) => match place.root {
            PlaceRoot::CaptureCell(cell) if place.projection.is_empty() => Ok(cell),
            _ => Err(lambda_capture_gap(expr_id)),
        },
        LambdaCaptureSource::Capture { decl, .. } => match decl {
            LambdaCaptureDecl::CaptureCell { cell, .. } => Ok(cell),
            _ => Err(lambda_capture_gap(expr_id)),
        },
    }
}

fn exact_scoped_borrow(
    expr_id: ExprId,
    sources: &HashMap<BindingId, LambdaCaptureSource>,
    capture: &LambdaCaptureFact,
    ty: TypeId,
) -> Result<ScopedBorrowId, LowerError> {
    match exact_capture_source(expr_id, sources, capture, ty)? {
        LambdaCaptureSource::Local(place) => match place.root {
            PlaceRoot::ScopedBorrow(borrow) if place.projection.is_empty() => Ok(borrow),
            _ => Err(lambda_capture_gap(expr_id)),
        },
        LambdaCaptureSource::Capture { decl, .. } => match decl {
            LambdaCaptureDecl::ScopedBorrow { borrow, .. } => Ok(borrow),
            _ => Err(lambda_capture_gap(expr_id)),
        },
    }
}

fn exact_scoped_borrow_place(
    expr_id: ExprId,
    sources: &HashMap<BindingId, LambdaCaptureSource>,
    capture: &LambdaCaptureFact,
    ty: TypeId,
) -> Result<Place, LowerError> {
    match exact_capture_source(expr_id, sources, capture, ty)? {
        LambdaCaptureSource::Local(place) => match place.root {
            PlaceRoot::ScopedBorrow(_) if place.projection.is_empty() => Ok(place),
            _ => Err(lambda_capture_gap(expr_id)),
        },
        LambdaCaptureSource::Capture { slot, decl } => match decl {
            LambdaCaptureDecl::ScopedBorrow { .. } => Ok(Place {
                root: PlaceRoot::LambdaCapture(slot),
                projection: vec![],
                ty,
            }),
            _ => Err(lambda_capture_gap(expr_id)),
        },
    }
}

fn exact_capture_source(
    expr_id: ExprId,
    sources: &HashMap<BindingId, LambdaCaptureSource>,
    capture: &LambdaCaptureFact,
    ty: TypeId,
) -> Result<LambdaCaptureSource, LowerError> {
    let Some(source) = sources.get(&capture.binding_id) else {
        return Err(lambda_capture_gap(expr_id));
    };
    match source {
        LambdaCaptureSource::Local(place) if place.projection.is_empty() && place.ty == ty => {
            Ok(source.clone())
        }
        LambdaCaptureSource::Capture { decl, .. } if decl.ty() == ty => Ok(source.clone()),
        _ => Err(lambda_capture_gap(expr_id)),
    }
}

fn owned_reachable_lambdas(
    cx: &LowerCx<'_>,
    functions: &ReachableItems<'_>,
    body: &BodyInstanceKey,
) -> Vec<(ExprId, LambdaId)> {
    functions
        .items
        .iter()
        .filter_map(|source| match (&source.source, &source.body) {
            (ReachableSource::Lambda { owner, .. }, BodyInstanceKey::Lambda(key))
                if owner == body =>
            {
                cx.maps
                    .lambdas
                    .get(key)
                    .copied()
                    .map(|lambda| (key.expr, lambda))
            }
            _ => None,
        })
        .collect()
}

fn alloc_scoped_borrows(
    cx: &mut LowerCx<'_>,
    owner: FunctionId,
    owner_function: &Function,
    owned_lambdas: &[(ExprId, LambdaId)],
    capture_sources: &mut HashMap<BindingId, LambdaCaptureSource>,
    binding_scoped_borrows: &mut HashMap<BindingId, ScopedBorrowId>,
) -> Result<(), LowerError> {
    for (expr_id, _) in owned_lambdas {
        for capture in cx.ordered_lambda_capture_facts(*expr_id)? {
            if !matches!(
                capture.runtime_plan,
                LambdaCaptureRuntimePlan::ScopedBorrow(
                    CaptureStorageOrigin::BorrowedParam | CaptureStorageOrigin::RefSelf
                )
            ) {
                continue;
            }
            let binding = capture.binding_id;
            let ty = cx.lower_ty(&capture.ty)?;
            if exact_capture_source(*expr_id, capture_sources, &capture, ty).is_ok_and(|source| {
                matches!(
                    source,
                    LambdaCaptureSource::Local(Place {
                        root: PlaceRoot::ScopedBorrow(_),
                        ..
                    }) | LambdaCaptureSource::Capture {
                        decl: LambdaCaptureDecl::ScopedBorrow { .. },
                        ..
                    }
                )
            }) {
                continue;
            }
            let borrow = match binding_scoped_borrows.get(&binding).copied() {
                Some(borrow) => borrow,
                None => {
                    let source = scoped_borrow_source(
                        *expr_id,
                        owner,
                        owner_function,
                        capture_sources,
                        &capture,
                        ty,
                    )?;
                    let borrow = cx.program.alloc_scoped_borrow(ScopedBorrowDecl {
                        owner,
                        binding: air_binding_id(binding),
                        source,
                        ty,
                        mutability: AirMutability::Mutable,
                    });
                    binding_scoped_borrows.insert(binding, borrow);
                    borrow
                }
            };
            let place = cx
                .program
                .scoped_borrow_place(borrow)
                .ok_or_else(|| lambda_capture_gap(*expr_id))?;
            capture_sources.insert(binding, LambdaCaptureSource::Local(place));
        }
    }
    Ok(())
}

fn scoped_borrow_source(
    expr_id: ExprId,
    owner: FunctionId,
    owner_function: &Function,
    sources: &HashMap<BindingId, LambdaCaptureSource>,
    capture: &LambdaCaptureFact,
    ty: TypeId,
) -> Result<ScopedBorrowSource, LowerError> {
    let source = exact_local_capture_source(expr_id, owner, owner_function, sources, capture, ty)?;
    let LambdaCaptureRuntimePlan::ScopedBorrow(origin) = capture.runtime_plan else {
        return Err(lambda_capture_gap(expr_id));
    };
    let (role, borrow_source) = match origin {
        CaptureStorageOrigin::BorrowedParam => (
            ParamRole::Normal,
            ScopedBorrowSource::SourceMutParam {
                local: source.local,
            },
        ),
        CaptureStorageOrigin::RefSelf => (
            ParamRole::Receiver,
            ScopedBorrowSource::RefSelf {
                local: source.local,
            },
        ),
        _ => return Err(lambda_capture_gap(expr_id)),
    };
    owner_function
        .signature
        .params
        .iter()
        .any(|param| {
            param.local_id == source.local
                && param.mode == ParamMode::MutBorrow
                && param.role == role
        })
        .then_some(borrow_source)
        .ok_or_else(|| lambda_capture_gap(expr_id))
}

fn capture_cell_place(cell: CaptureCellId, ty: TypeId) -> Place {
    Place {
        root: PlaceRoot::CaptureCell(cell),
        projection: vec![],
        ty,
    }
}

fn binding_capture_cells(
    program: &Program,
    owner: FunctionId,
) -> HashMap<BindingId, CaptureCellId> {
    program
        .capture_cells
        .iter()
        .enumerate()
        .filter(|(_, decl)| decl.owner == owner)
        .map(|(index, decl)| {
            (
                typecheck_binding_id(decl.binding),
                CaptureCellId::from_index(index),
            )
        })
        .collect()
}

fn binding_scoped_borrows(
    program: &Program,
    owner: FunctionId,
) -> HashMap<BindingId, ScopedBorrowId> {
    program
        .scoped_borrows
        .iter()
        .enumerate()
        .filter(|(_, decl)| decl.owner == owner)
        .map(|(index, decl)| {
            (
                typecheck_binding_id(decl.binding),
                ScopedBorrowId::from_index(index),
            )
        })
        .collect()
}

fn lambda_capture_sources(
    program: &Program,
    function: &Function,
) -> HashMap<BindingId, LambdaCaptureSource> {
    let FunctionKind::Lambda(lambda) = function.kind else {
        return HashMap::new();
    };
    program.lambdas[lambda.index()]
        .captures
        .iter()
        .enumerate()
        .map(|(index, decl)| {
            (
                typecheck_binding_id(decl.binding()),
                LambdaCaptureSource::Capture {
                    slot: LambdaCaptureSlotId::from_index(index),
                    decl: decl.clone(),
                },
            )
        })
        .collect()
}

fn typecheck_binding_id(binding: AirBindingId) -> BindingId {
    BindingId(binding.0)
}

fn function_local_place(function: &Function, local: LocalId) -> Place {
    Place {
        root: PlaceRoot::Local(local),
        projection: vec![],
        ty: function.locals[local.index()].ty,
    }
}

fn initial_capture_sources(
    body: &BodyInstanceKey,
    facts: &SemanticBodyFacts,
    locals: &HashMap<SemanticLocalId, LocalId>,
    function: &Function,
) -> Result<HashMap<BindingId, LambdaCaptureSource>, LowerError> {
    let mut bindings = HashMap::new();
    for fact in facts.locals.defs.values() {
        let Some(binding) = fact.binding_id else {
            continue;
        };
        let Some(local) = locals.get(&fact.id).copied() else {
            continue;
        };
        let place = function_local_place(function, local);
        if bindings
            .insert(binding, LambdaCaptureSource::Local(place))
            .is_some()
        {
            return Err(LowerError::DuplicateBindingBridge {
                body: Box::new(body.clone()),
                binding,
            });
        }
    }
    Ok(bindings)
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
        && fact.ret.ty() == Type::Void
        && body.node.stmts.is_empty()
        && body.node.tail.is_none()
}

impl<'a> SourceProgramIndex<'a> {
    fn get_default_expr(&self, site: DefaultExprSite) -> Option<&'a ExprNode> {
        self.source.default_expr(site)
    }

    fn new(source: &'a crate::source_ast::SourceAstIndex, semantic: &SemanticProgram) -> Self {
        let mut callables = HashMap::new();
        for fact in &semantic.declaration_facts.functions {
            let declaration = fact.site.owner();
            let Some(module) = source.declaration_module(&declaration) else {
                continue;
            };
            let Some(entry) = source.callable(&fact.site) else {
                continue;
            };
            let callable = SourceCallable {
                module,
                source: declaration.source(),
                node: entry,
            };
            callables.entry(fact.id.clone()).or_insert(callable);
        }
        Self { source, callables }
    }
}

impl<'a> ReachableItems<'a> {
    fn new(
        index: &'a SourceProgramIndex<'a>,
        semantic: &'a SemanticProgram,
        semantic_functions: &'a SemanticCallableFacts<'a>,
        roots: Vec<CallableInstanceKey>,
    ) -> Result<Self, LowerError> {
        let mut queued = HashSet::new();
        let mut worklist = vec![];
        for root in roots {
            queue_reachable(&mut queued, &mut worklist, ReachableKey::Callable(root));
        }

        let mut items = vec![];
        let mut worklist_index = 0;
        while let Some(key) = worklist.get(worklist_index).cloned() {
            worklist_index += 1;
            let item = match key {
                ReachableKey::Callable(key) => {
                    reachable_callable(index, semantic, semantic_functions, &key)?
                }
                ReachableKey::Lambda { owner, key, source } => {
                    reachable_lambda(index, semantic, *owner, &key, source)?
                }
                ReachableKey::Global(key) => reachable_global(index, semantic, &key)?,
                ReachableKey::CastFrom(key) => reachable_cast_from(index, semantic, &key)?,
            };
            enqueue_body_references(
                index,
                semantic_functions,
                semantic,
                item.body_facts.as_facts(),
                &item.body,
                item.source_id,
                &mut queued,
                &mut worklist,
            )?;
            items.push(item);
        }

        Ok(Self {
            index,
            body_facts: &semantic.facts.bodies,
            callable_facts: semantic_functions,
            items,
        })
    }

    fn contract_fact_slices(&self) -> Result<Vec<ReachableFactSlice<'_>>, LowerError> {
        let mut slices = self
            .items
            .iter()
            .map(|item| ReachableFactSlice {
                body: item.body.clone(),
                facts: item.body_facts.as_facts(),
                exprs: None,
            })
            .collect::<Vec<_>>();
        let mut worklist = self
            .items
            .iter()
            .flat_map(|item| default_uses(item.body_facts.as_facts(), None))
            .collect::<Vec<_>>();
        let mut visited = HashSet::new();
        let mut index = 0;
        while let Some(default_use) = worklist.get(index).cloned() {
            index += 1;
            if !visited.insert(default_use.clone()) {
                continue;
            }
            let site = default_use.site;
            self.index
                .get_default_expr(site)
                .ok_or(LowerError::MissingDefaultExprFacts { site })?;
            let exprs = self.index.source.expression_ids(site);
            let facts = self
                .body_facts
                .get(&default_use.facts_body)
                .ok_or(LowerError::MissingDefaultExprFacts { site })?;
            worklist.extend(default_uses(facts, exprs));
            slices.push(ReachableFactSlice {
                body: default_use.facts_body,
                facts,
                exprs,
            });
        }
        Ok(slices)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ReachableKey {
    Callable(CallableInstanceKey),
    Global(GlobalKey),
    CastFrom(CastFromInstanceKey),
    Lambda {
        owner: Box<BodyInstanceKey>,
        key: LambdaBodyKey,
        source: SourceId,
    },
}

fn reachable_callable<'a>(
    index: &'a SourceProgramIndex<'a>,
    semantic: &'a SemanticProgram,
    semantic_functions: &SemanticCallableFacts<'a>,
    key: &CallableInstanceKey,
) -> Result<ReachableItem<'a>, LowerError> {
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
    let Some(fact) = semantic_functions.get(key) else {
        return Err(LowerError::MissingFunctionFact {
            id: Box::new(key.target.clone()),
            args: Box::new(key.args.clone()),
        });
    };
    let body_facts = match semantic.facts.body(&body) {
        Some(facts) => ReachableBodyFacts::Facts(facts),
        None if can_omit_body_facts(fact, source) => ReachableBodyFacts::Empty(Box::default()),
        None => {
            return Err(LowerError::MissingSpecializedBodyFacts {
                body: Box::new(body.clone()),
            });
        }
    };
    Ok(ReachableItem {
        source: ReachableSource::Callable {
            callable: source,
            fact,
        },
        body,
        body_facts,
        source_id: source.source(),
    })
}

fn reachable_global<'a>(
    index: &'a SourceProgramIndex<'a>,
    semantic: &'a SemanticProgram,
    key: &GlobalKey,
) -> Result<ReachableItem<'a>, LowerError> {
    let global = index
        .source
        .globals
        .get(key)
        .ok_or_else(|| LowerError::MissingSourceGlobal {
            key: Box::new(key.clone()),
        })?;
    let sig = semantic
        .declarations
        .global(key)
        .ok_or_else(|| LowerError::MissingGlobalSig {
            key: Box::new(key.clone()),
        })?;
    let body = BodyInstanceKey::Global(key.clone());
    let facts =
        semantic
            .facts
            .body(&body)
            .ok_or_else(|| LowerError::MissingSpecializedBodyFacts {
                body: Box::new(body.clone()),
            })?;
    Ok(ReachableItem {
        source: ReachableSource::Global { global, sig },
        body,
        body_facts: ReachableBodyFacts::Facts(facts),
        source_id: global.source,
    })
}

fn reachable_cast_from<'a>(
    index: &'a SourceProgramIndex<'a>,
    semantic: &'a SemanticProgram,
    key: &CastFromInstanceKey,
) -> Result<ReachableItem<'a>, LowerError> {
    let cast = index
        .source
        .cast_from(&key.extend, key.index)
        .ok_or_else(|| LowerError::MissingSpecializedBodyFacts {
            body: Box::new(BodyInstanceKey::CastFrom(key.clone())),
        })?;
    let body = BodyInstanceKey::CastFrom(key.clone());
    let facts =
        semantic
            .facts
            .body(&body)
            .ok_or_else(|| LowerError::MissingSpecializedBodyFacts {
                body: Box::new(body.clone()),
            })?;
    let signature = semantic
        .declarations
        .cast_from_signature(key)
        .ok_or_else(|| LowerError::MissingSpecializedBodyFacts {
            body: Box::new(body.clone()),
        })?;
    Ok(ReachableItem {
        source: ReachableSource::CastFrom { cast, signature },
        body,
        body_facts: ReachableBodyFacts::Facts(facts),
        source_id: cast.source,
    })
}

fn reachable_lambda<'a>(
    index: &'a SourceProgramIndex<'a>,
    semantic: &'a SemanticProgram,
    owner: BodyInstanceKey,
    key: &LambdaBodyKey,
    source_id: SourceId,
) -> Result<ReachableItem<'a>, LowerError> {
    let body = BodyInstanceKey::Lambda(key.clone());
    let facts =
        semantic
            .facts
            .body(&body)
            .ok_or_else(|| LowerError::MissingSpecializedBodyFacts {
                body: Box::new(body.clone()),
            })?;
    let lambda = index
        .source
        .lambda(source_id, key.expr)
        .ok_or(LowerError::UnsupportedExpr {
            expr_id: key.expr,
            kind: "Lambda",
        })?;
    let fact = semantic
        .facts
        .body(&owner)
        .and_then(|facts| facts.function_values.get(&key.expr))
        .ok_or(LowerError::UnsupportedExpr {
            expr_id: key.expr,
            kind: "Lambda",
        })?;
    Ok(ReachableItem {
        source: ReachableSource::Lambda {
            owner,
            lambda,
            ty: &fact.ty,
        },
        body,
        body_facts: ReachableBodyFacts::Facts(facts),
        source_id,
    })
}

fn enqueue_body_references(
    index: &SourceProgramIndex<'_>,
    semantic_functions: &SemanticCallableFacts<'_>,
    semantic: &SemanticProgram,
    body_facts: &SemanticBodyFacts,
    body: &BodyInstanceKey,
    source_id: SourceId,
    queued: &mut HashSet<ReachableKey>,
    worklist: &mut Vec<ReachableKey>,
) -> Result<(), LowerError> {
    enqueue_calls(
        index,
        semantic_functions,
        body_facts,
        None,
        queued,
        worklist,
    )?;
    enqueue_global_accesses(body_facts, None, queued, worklist);
    enqueue_user_cast_conversions(body_facts, None, queued, worklist);
    enqueue_contract_witness_targets(index, semantic, body_facts, None, queued, worklist)?;
    enqueue_function_values(index, body_facts, body, source_id, None, queued, worklist)?;
    enqueue_stringify_overrides(index, semantic, body_facts, None, queued, worklist);
    let mut default_env = DefaultDependencyEnv {
        index,
        semantic_functions,
        semantic,
        queued,
        worklist,
        visited: HashSet::new(),
    };
    enqueue_used_default_references(&mut default_env, body_facts)
}

struct DefaultDependencyEnv<'a, 'b> {
    index: &'a SourceProgramIndex<'a>,
    semantic_functions: &'a SemanticCallableFacts<'a>,
    semantic: &'a SemanticProgram,
    queued: &'b mut HashSet<ReachableKey>,
    worklist: &'b mut Vec<ReachableKey>,
    visited: HashSet<DefaultUse>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct DefaultUse {
    site: DefaultExprSite,
    facts_body: BodyInstanceKey,
}

fn enqueue_used_default_references(
    env: &mut DefaultDependencyEnv<'_, '_>,
    body_facts: &SemanticBodyFacts,
) -> Result<(), LowerError> {
    for default_use in default_uses(body_facts, None) {
        enqueue_default_references(env, &default_use)?;
    }
    Ok(())
}

fn default_uses(
    body_facts: &SemanticBodyFacts,
    exprs: Option<&HashSet<ExprId>>,
) -> Vec<DefaultUse> {
    let mut uses = vec![];
    for defaults in body_facts.default_args.values() {
        for default in defaults {
            if exprs.is_some_and(|exprs| !exprs.contains(&default.call)) {
                continue;
            }
            uses.push(DefaultUse {
                site: default.default,
                facts_body: default.facts_body.clone(),
            });
        }
    }
    for defaults in body_facts.default_fields.values() {
        for default in defaults {
            if exprs.is_some_and(|exprs| !exprs.contains(&default.aggregate)) {
                continue;
            }
            uses.push(DefaultUse {
                site: default.default,
                facts_body: default.facts_body.clone(),
            });
        }
    }
    uses.sort_by_key(|default_use| default_use.site.expr.0);
    uses.dedup();
    uses
}

fn enqueue_default_references(
    env: &mut DefaultDependencyEnv<'_, '_>,
    default_use: &DefaultUse,
) -> Result<(), LowerError> {
    if !env.visited.insert(default_use.clone()) {
        return Ok(());
    }
    let site = default_use.site;
    env.index
        .get_default_expr(site)
        .ok_or(LowerError::MissingDefaultExprFacts { site })?;
    let exprs = env.index.source.expression_ids(site);
    let facts = env
        .semantic
        .facts
        .body(&default_use.facts_body)
        .ok_or(LowerError::MissingDefaultExprFacts { site })?;

    enqueue_calls(
        env.index,
        env.semantic_functions,
        facts,
        exprs,
        env.queued,
        env.worklist,
    )?;
    enqueue_global_accesses(facts, exprs, env.queued, env.worklist);
    enqueue_user_cast_conversions(facts, exprs, env.queued, env.worklist);
    enqueue_contract_witness_targets(
        env.index,
        env.semantic,
        facts,
        exprs,
        env.queued,
        env.worklist,
    )?;
    enqueue_function_values(
        env.index,
        facts,
        &default_use.facts_body,
        site.source,
        exprs,
        env.queued,
        env.worklist,
    )?;
    enqueue_stringify_overrides(
        env.index,
        env.semantic,
        facts,
        exprs,
        env.queued,
        env.worklist,
    );
    for nested in default_uses(facts, exprs) {
        enqueue_default_references(env, &nested)?;
    }
    Ok(())
}

fn call_target_returns_iter(
    semantic_functions: &SemanticCallableFacts<'_>,
    target: &CallTarget,
) -> bool {
    let key = CallableInstanceKey {
        target: target.id.clone(),
        args: target.args.clone(),
    };
    semantic_functions
        .get(&key)
        .is_some_and(|fact| fact.ret.is_iter())
}

fn enqueue_calls(
    index: &SourceProgramIndex<'_>,
    semantic_functions: &SemanticCallableFacts<'_>,
    body_facts: &SemanticBodyFacts,
    exprs: Option<&HashSet<ExprId>>,
    queued: &mut HashSet<ReachableKey>,
    worklist: &mut Vec<ReachableKey>,
) -> Result<(), LowerError> {
    let mut calls = body_facts.calls.iter().collect::<Vec<_>>();
    calls.sort_by_key(|(expr, _)| expr.0);
    for (expr, target) in calls {
        if exprs.is_some_and(|exprs| !exprs.contains(expr)) {
            continue;
        }
        if target.form != CallForm::Normal {
            return Err(LowerError::UnsupportedCallForm { expr_id: *expr });
        }
        if target.id.kind == CallableKind::EnumVariant
            || is_lowered_collection_stub(&target.id)
            || call_target_returns_iter(semantic_functions, target)
        {
            continue;
        }
        if !generic_args_are_concrete(&target.args) {
            return Err(LowerError::NonConcreteCallableInstance {
                id: Box::new(target.id.clone()),
                args: Box::new(target.args.clone()),
            });
        }
        if !index.callables.contains_key(&target.id) {
            return Err(LowerError::UnsupportedCallableInstance {
                id: Box::new(target.id.clone()),
                args: Box::new(target.args.clone()),
            });
        }
        queue_reachable(
            queued,
            worklist,
            ReachableKey::Callable(CallableInstanceKey {
                target: target.id.clone(),
                args: target.args.clone(),
            }),
        );
    }
    Ok(())
}

fn enqueue_global_accesses(
    body_facts: &SemanticBodyFacts,
    exprs: Option<&HashSet<ExprId>>,
    queued: &mut HashSet<ReachableKey>,
    worklist: &mut Vec<ReachableKey>,
) {
    let mut accesses = body_facts.global_accesses.values().collect::<Vec<_>>();
    accesses.sort_by_key(|fact| fact.expr_id.0);
    for fact in accesses {
        if exprs.is_some_and(|exprs| {
            !exprs.contains(&fact.root_expr_id) && !exprs.contains(&fact.expr_id)
        }) {
            continue;
        }
        queue_reachable(queued, worklist, ReachableKey::Global(fact.key.clone()));
    }
}

fn enqueue_user_cast_conversions(
    body_facts: &SemanticBodyFacts,
    exprs: Option<&HashSet<ExprId>>,
    queued: &mut HashSet<ReachableKey>,
    worklist: &mut Vec<ReachableKey>,
) {
    let mut conversions = body_facts.user_cast_conversions.iter().collect::<Vec<_>>();
    conversions.sort_by_key(|(site, _)| (site.expr.0, site.source.0));
    for (site, instance) in conversions {
        if exprs.is_some_and(|exprs| !exprs.contains(&site.expr)) {
            continue;
        }
        queue_reachable(queued, worklist, ReachableKey::CastFrom(instance.clone()));
    }
}

fn enqueue_contract_witness_targets(
    index: &SourceProgramIndex<'_>,
    semantic: &SemanticProgram,
    body_facts: &SemanticBodyFacts,
    exprs: Option<&HashSet<ExprId>>,
    queued: &mut HashSet<ReachableKey>,
    worklist: &mut Vec<ReachableKey>,
) -> Result<(), LowerError> {
    let mut conversions = body_facts.dyn_conversions.values().collect::<Vec<_>>();
    conversions.sort_by_key(|fact| fact.expr_id.0);
    for conversion in conversions {
        if exprs.is_some_and(|exprs| !exprs.contains(&conversion.expr_id)) {
            continue;
        }
        let key = semantic
            .facts
            .witness_structural_keys
            .get(&conversion.witness)
            .expect("finalized witness structural key exists");
        for target in &key.slots {
            enqueue_witness_target(index, target, queued, worklist)?;
        }
    }
    Ok(())
}

fn enqueue_witness_target(
    index: &SourceProgramIndex<'_>,
    target: &WitnessSlotTarget,
    queued: &mut HashSet<ReachableKey>,
    worklist: &mut Vec<ReachableKey>,
) -> Result<(), LowerError> {
    let instance = match target {
        WitnessSlotTarget::Direct {
            callable,
            owner_args,
            ..
        }
        | WitnessSlotTarget::Extend {
            callable,
            owner_args,
            ..
        } => Some(CallableInstanceKey {
            target: callable.clone(),
            args: owner_args.clone(),
        }),
        WitnessSlotTarget::Promoted { target, .. } => {
            enqueue_witness_target(index, target, queued, worklist)?;
            None
        }
        WitnessSlotTarget::Extern { .. } => None,
    };
    if let Some(instance) = instance {
        if !generic_args_are_concrete(&instance.args) {
            return Err(LowerError::NonConcreteCallableInstance {
                id: Box::new(instance.target),
                args: Box::new(instance.args),
            });
        }
        if !index.callables.contains_key(&instance.target) {
            return Err(LowerError::UnsupportedCallableInstance {
                id: Box::new(instance.target),
                args: Box::new(instance.args),
            });
        }
        queue_reachable(queued, worklist, ReachableKey::Callable(instance));
    }
    Ok(())
}

fn is_lowered_collection_stub(id: &CallableId) -> bool {
    matches!(id.kind, CallableKind::ExtendMethod(MethodSurface::Instance))
        && id.module.is_core_module("collections")
        && collection_effect::has_lowered_stub(id.name)
}

fn queue_reachable(
    queued: &mut HashSet<ReachableKey>,
    worklist: &mut Vec<ReachableKey>,
    key: ReachableKey,
) {
    if queued.insert(key.clone()) {
        worklist.push(key);
    }
}

fn enqueue_function_values(
    index: &SourceProgramIndex<'_>,
    body_facts: &SemanticBodyFacts,
    owner: &BodyInstanceKey,
    source: SourceId,
    exprs: Option<&HashSet<ExprId>>,
    queued: &mut HashSet<ReachableKey>,
    worklist: &mut Vec<ReachableKey>,
) -> Result<(), LowerError> {
    let mut function_values = body_facts.function_values.values().collect::<Vec<_>>();
    function_values.sort_by_key(|fact| fact.expr.0);
    for fact in function_values {
        if exprs.is_some_and(|exprs| !exprs.contains(&fact.expr)) {
            continue;
        }
        match &fact.kind {
            FunctionValueKind::Named(target) => {
                if !index.callables.contains_key(&target.target) {
                    return Err(LowerError::UnsupportedCallableInstance {
                        id: Box::new(target.target.clone()),
                        args: Box::new(target.args.clone()),
                    });
                }
                queue_reachable(queued, worklist, ReachableKey::Callable(target.clone()));
            }
            FunctionValueKind::Lambda { lambda_expr } => {
                queue_reachable(
                    queued,
                    worklist,
                    ReachableKey::Lambda {
                        owner: Box::new(owner.clone()),
                        key: LambdaBodyKey {
                            expr: *lambda_expr,
                            specialization: match owner {
                                BodyInstanceKey::Callable(key) => key.args.clone(),
                                BodyInstanceKey::Lambda(key) => key.specialization.clone(),
                                BodyInstanceKey::CastFrom(key) => key.args.clone(),
                                BodyInstanceKey::Module(_) | BodyInstanceKey::Global(_) => {
                                    GenericArgs::default()
                                }
                            },
                        },
                        source,
                    },
                );
            }
            FunctionValueKind::Storage(_) => {}
        }
    }
    Ok(())
}

fn enqueue_stringify_overrides(
    index: &SourceProgramIndex<'_>,
    semantic: &SemanticProgram,
    body_facts: &SemanticBodyFacts,
    exprs: Option<&HashSet<ExprId>>,
    queued: &mut HashSet<ReachableKey>,
    worklist: &mut Vec<ReachableKey>,
) {
    let mut visited = HashSet::new();
    for (expr, stringify) in &body_facts.stringifies {
        if exprs.is_some_and(|exprs| !exprs.contains(expr) && !exprs.contains(&stringify.arg)) {
            continue;
        }
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
    queued: &mut HashSet<ReachableKey>,
    worklist: &mut Vec<ReachableKey>,
    visited: &mut HashSet<Type>,
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
        | Type::Char
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
    queued: &mut HashSet<ReachableKey>,
    worklist: &mut Vec<ReachableKey>,
    visited: &mut HashSet<Type>,
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
                queue_reachable(queued, worklist, ReachableKey::Callable(key));
            }
            return;
        }
        let generics = aggregate.all_generics();
        for field in aggregate.fields.values() {
            let field_ty = substitute_aggregate_member(ty, &generics, &field.ty);
            enqueue_type_stringify_overrides(index, semantic, &field_ty, queued, worklist, visited);
        }
        return;
    }

    let Some(schema) = semantic.declarations.enum_schema(&owner) else {
        return;
    };
    let generics = schema.all_generics();
    for variant in schema.body.variants.values() {
        variant.payload.for_each_type(|payload_ty| {
            let payload_ty = substitute_aggregate_member(ty, &generics, payload_ty);
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
        CallbackEscape, ExternBindingOp, ExternBindingTarget, ExternEffects,
        ExternFunctionDescriptor, ExternModuleDescriptor, ExternParam, ExternSignature,
        ExternTypeExpr, ProviderDescriptor, ProviderId,
    };

    use super::*;
    use crate::{
        air::{
            BadContract, BadPlace, BadStatement, VerifyErrorKind,
            verify::{verify as verify_air, verify_contract_declarations as verify_contracts},
        },
        ast, externs,
        externs::{ExternInputs, PackageExternInputs, RawExterns},
        resolve::{PackageId, ResolveResult},
        test_support::{
            checked_with_full_core_shape, parse_program, resolved_modules_with_core_option,
            resolved_modules_with_core_option_external,
        },
        typecheck::{self, TypecheckConfig, nominal_type, nominal_type_with_args},
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
    fn type_lowerer_lowers_slice_function_types() {
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
            ast::ReturnSpec::value(Type::Bool),
        );

        let id = lowerer.lower(&mut program, &func).expect("lower failed");

        assert!(matches!(program.type_data(id), TypeData::Function(_)));
        assert!(
            program
                .type_arena
                .iter()
                .any(|data| matches!(data, TypeData::Slice(_)))
        );
    }

    #[test]
    fn canonical_contract_surfaces_transfer_to_air() {
        let air = lower_root(
            "contract Node { fn next(self) -> dyn Node; }
             fn use(node: dyn Node) {}",
            "use",
        )
        .expect("lower failed");

        let (surface_id, surface) = air
            .contract_surfaces
            .iter()
            .enumerate()
            .find(|(_, surface)| surface.display_name == "Node")
            .expect("missing Node surface");
        let surface_id = ContractSurfaceId::from_index(surface_id);
        assert_eq!(surface.slots.len(), 1);
        assert_eq!(surface.slots[0].name, Ident::new("next"));
        assert_eq!(
            surface.slots[0].ret,
            ContractReturnDecl::Value(
                air.type_arena
                    .iter()
                    .enumerate()
                    .find_map(|(index, ty)| {
                        (*ty == TypeData::Dyn(surface_id)).then_some(TypeId::from_index(index))
                    })
                    .expect("missing recursive dynamic type")
            )
        );
        let function = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("use"))
            .expect("missing use function");
        assert_eq!(
            air.type_data(function.signature.params[0].ty),
            &TypeData::Dyn(surface_id)
        );
        assert_eq!(
            air.type_helper_key(function.signature.params[0].ty),
            format!("dyn_surface_{}", surface_id.index())
        );
    }

    #[test]
    fn nested_dynamic_slot_types_share_source_type_identity() {
        let air = lower_root(
            "contract Inner { fn call(self); }
             contract Outer { fn take(self, values: [dyn Inner]); }
             fn direct(values: [dyn Inner]) {}",
            "direct",
        )
        .expect("lower failed");

        let slot_ty = air
            .contract_surfaces
            .iter()
            .find(|surface| surface.display_name == "Outer")
            .expect("missing Outer surface")
            .slots[0]
            .params[0]
            .ty;
        let param_ty = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("direct"))
            .expect("missing direct function")
            .signature
            .params[0]
            .ty;
        assert_eq!(slot_ty, param_ty);
    }

    #[test]
    fn generic_nominal_dynamic_slot_types_share_source_type_identity() {
        let air = lower_root(
            "contract Inner { fn call(self); }
             struct Box<T> { value: T }
             contract Outer { fn take(self, value: Box<dyn Inner>); }
             fn direct(value: Box<dyn Inner>) {}",
            "direct",
        )
        .expect("lower failed");

        let slot_ty = air
            .contract_surfaces
            .iter()
            .find(|surface| surface.display_name == "Outer")
            .expect("missing Outer surface")
            .slots[0]
            .params[0]
            .ty;
        let param_ty = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("direct"))
            .expect("missing direct function")
            .signature
            .params[0]
            .ty;
        assert_eq!(slot_ty, param_ty);
    }

    #[test]
    fn equivalent_compositions_share_air_type_identity() {
        let air = lower_roots(
            "contract A { fn a(self); }
             contract B { fn b(self); }
             fn left(value: dyn A + B) {}
             fn right(value: dyn B + A) {}",
            &["left", "right"],
        )
        .expect("lower failed");

        let param_ty = |name: &str| {
            air.functions
                .iter()
                .find(|function| function.name == Ident::new(name))
                .expect("missing function")
                .signature
                .params[0]
                .ty
        };
        assert_eq!(param_ty("left"), param_ty("right"));
    }

    #[test]
    fn reachable_contract_witness_lowers_exact_target() {
        let air = lower_contract_boundary(
            "contract Action { fn act(self); }
             struct Item { fn act(self) {} }
             fn main() { let action: dyn Action = Item {}; }",
            &["main"],
        );

        assert_eq!(air.contract_witnesses.len(), 1);
        let witness = &air.contract_witnesses[0];
        assert_eq!(air.type_display_name(witness.key.concrete_ty), "Item");
        assert_eq!(witness.key.slots.len(), 1);
        let ContractWitnessTarget::Function { function } = witness.key.slots[0].target else {
            panic!("expected function witness target");
        };
        assert_eq!(air.function(function).name, Ident::new("act"));
    }

    #[test]
    fn default_expression_contract_facts_enter_air_closure() {
        let air = lower_contract_boundary(
            "contract A { fn a(self); }
             contract B { fn b(self); }
             struct Both { fn a(self) {} fn b(self) {} }
             fn make() -> dyn A + B { Both {} }
             fn use(value: dyn B = make()) {}
             fn main() { use(); }",
            &["main"],
        );

        assert_eq!(air.contract_weakenings.len(), 1);
        assert_eq!(air.contract_witnesses.len(), 2);
    }

    #[test]
    fn dynamic_pack_weaken_and_call_enter_air() {
        let air = lower_roots(
            "contract A { fn a(ref self); }
             contract B { fn b(ref self); }
             struct Both { fn a(ref self) {} fn b(ref self) {} }
             fn main() {
                 var concrete = Both {};
                 var both: dyn A + B = concrete;
                 var one: dyn A = both;
                 one.a();
             }",
            &["main"],
        )
        .expect("dynamic AIR lowering failed");

        let mut pack = false;
        let mut weaken = false;
        let mut call = false;
        air.functions
            .iter()
            .find(|function| function.name == Ident::new("main"))
            .unwrap()
            .body
            .for_each_rvalue(&mut |value| match value {
                RValue::DynPack { .. } => pack = true,
                RValue::DynWeaken { .. } => weaken = true,
                RValue::DynCall { .. } => call = true,
                _ => {}
            });
        assert!(
            pack && weaken && call,
            "pack={pack} weaken={weaken} call={call}"
        );
    }

    #[test]
    fn owned_dynamic_params_keep_owned_roots() {
        let air = lower_roots(
            "contract A { fn a(self); }
             struct Item { fn a(self) {} }
             fn inspect(value: dyn A) { value.a(); }
             fn keep(value: dyn A) -> dyn A { value }
             fn main() {
                 let kept = keep(Item {});
                 inspect(Item {});
             }",
            &["main"],
        )
        .expect("owned dynamic parameter lowering failed");

        for name in ["inspect", "keep"] {
            let function = air
                .functions
                .iter()
                .find(|function| function.name == Ident::new(name))
                .unwrap();
            let local = function.signature.params[0].local_id;
            let mut borrowed_root = false;
            function.body.walk_children(&mut |child| {
                let place = match child {
                    crate::air::AirChild::Operand {
                        operand: Operand::Place(place),
                        ..
                    }
                    | crate::air::AirChild::Place { place, .. } => Some(place),
                    _ => None,
                };
                borrowed_root |=
                    place.is_some_and(|place| matches!(place.root, PlaceRoot::DynBorrowParam(_)));
            });
            assert!(!borrowed_root);
            assert!(matches!(
                function.locals[local.index()].kind,
                LocalKind::Arg
            ));
        }
        let inspect = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("inspect"))
            .unwrap();
        assert!(matches!(
            inspect.body.block.stmts[0],
            AirStmt::Eval(RValue::DynCall {
                receiver: DynReceiver::Owned(_),
                ..
            })
        ));
    }

    #[test]
    fn owned_downcast_and_dynamic_match_enter_air() {
        let downcast = lower_roots(
            "contract Drawable { fn draw(self); }
             struct Enemy { fn draw(self) {} }
             fn main() -> Enemy? {
                 let actor: dyn Drawable = Enemy {};
                 actor as? Enemy
             }",
            &["main"],
        )
        .expect("dynamic downcast AIR lowering failed");
        let main = downcast
            .functions
            .iter()
            .find(|function| function.name == Ident::new("main"))
            .unwrap();
        assert!(
            function_statements(main).any(|stmt| matches!(
                stmt,
                AirStmt::Init {
                    value: RValue::DynDowncast { .. },
                    ..
                } | AirStmt::Eval(RValue::DynDowncast { .. })
            )),
            "{:#?}",
            main.body.block.stmts
        );

        let matched = lower_roots(
            "contract Drawable { fn draw(self); }
             struct Enemy { fn draw(self) {} }
             fn main() {
                 let actor: dyn Drawable = Enemy {};
                 match actor as? {
                     Enemy(enemy) => enemy.draw(),
                     other => other.draw(),
                 };
             }",
            &["main"],
        )
        .expect("dynamic match AIR lowering failed");
        let main = matched
            .functions
            .iter()
            .find(|function| function.name == Ident::new("main"))
            .unwrap();
        let Some(AirStmt::DynMatch(match_)) = main.body.block.stmts.last() else {
            panic!("expected structured dynamic match");
        };
        assert!(matches!(match_.source, AirDynMatchSource::Owned { .. }));
        assert_eq!(match_.arms.len(), 1);
        assert!(match_.arms[0].binding.local().is_some());
        assert!(match_.fallback.binding.local().is_some());
    }

    #[test]
    fn borrowed_and_mutable_dynamic_match_sources_are_explicit() {
        let borrowed = lower_roots(
            "contract Updatable { fn update(ref self); }
             struct Enemy { fn update(ref self) {} }
             fn inspect(ref actor: dyn Updatable) {
                 match actor as? {
                     Enemy(enemy) => {},
                     other => {},
                 };
             }",
            &["inspect"],
        )
        .expect("borrowed dynamic match AIR lowering failed");
        let inspect = borrowed
            .functions
            .iter()
            .find(|function| function.name == Ident::new("inspect"))
            .unwrap();
        assert!(
            function_statements(inspect).any(|stmt| matches!(
                stmt,
                AirStmt::DynMatch(AirDynMatch {
                    source: AirDynMatchSource::Borrowed(_),
                    ..
                })
            )),
            "{:#?}",
            inspect.body.block.stmts
        );

        let mutable = lower_roots(
            "contract Updatable { fn update(ref self); }
             struct Enemy { hp: int, fn update(ref self) {} }
             fn main() {
                 var actor: dyn Updatable = Enemy { hp: 1 };
                 match ref actor as? {
                     Enemy(enemy) => enemy.hp = 2,
                     other => other.update(),
                 };
             }",
            &["main"],
        )
        .expect("mutable dynamic match AIR lowering failed");
        let main = mutable
            .functions
            .iter()
            .find(|function| function.name == Ident::new("main"))
            .unwrap();
        assert!(function_statements(main).any(|stmt| matches!(
            stmt,
            AirStmt::DynMatch(AirDynMatch {
                source: AirDynMatchSource::Mutable(_),
                ..
            })
        )));
    }

    #[test]
    fn mutable_dynamic_match_discard_arms_can_use_root() {
        lower_roots(
            "contract Updatable { fn update(ref self); }
             struct Enemy { fn update(ref self) {} }
             fn main() {
                 var actor: dyn Updatable = Enemy {};
                 match ref actor as? {
                     Enemy(_) => actor.update(),
                     _ => actor.update(),
                 };
             }",
            &["main"],
        )
        .expect("discard-only mutable dynamic match should not loan the root");
    }

    #[test]
    fn malformed_mutable_dynamic_match_rejects_root_reads() {
        let mut air = lower_roots(
            "contract Updatable { fn update(ref self); }
             struct Enemy { hp: int, fn update(ref self) {} }
             fn main() {
                 var actor: dyn Updatable = Enemy { hp: 1 };
                 match ref actor as? {
                     Enemy(enemy) => {},
                     other => {},
                 };
             }",
            &["main"],
        )
        .expect("mutable dynamic match AIR lowering failed");
        let function_id = air
            .functions
            .iter()
            .position(|function| function.name == Ident::new("main"))
            .map(FunctionId::from_index)
            .unwrap();
        let mut immutable = air.clone();
        let AirStmt::DynMatch(AirDynMatch {
            source:
                AirDynMatchSource::Mutable(Place {
                    root: PlaceRoot::Local(root),
                    ..
                }),
            ..
        }) = immutable
            .function(function_id)
            .body
            .block
            .stmts
            .last()
            .unwrap()
        else {
            panic!("expected local mutable dynamic match source");
        };
        let root = *root;
        immutable.function_mut(function_id).locals[root.index()].mutability =
            AirMutability::Immutable;
        let errors = verify_air(&immutable).unwrap_err();
        assert!(errors.iter().any(|error| matches!(
            error.kind,
            VerifyErrorKind::BadPlace(BadPlace::ImmutableRoot(_))
        )));

        let mut bad_transition = air.clone();
        let AirStmt::DynMatch(match_) = bad_transition
            .function_mut(function_id)
            .body
            .block
            .stmts
            .last_mut()
            .unwrap()
        else {
            panic!("expected dynamic match");
        };
        let binding = match_.arms[0].binding.local().unwrap();
        match_.arms[0].binding = AirDynMatchTargetBinding::Materialize(binding);
        let errors = verify_air(&bad_transition).unwrap_err();
        assert!(errors.iter().any(|error| matches!(
            error.kind,
            VerifyErrorKind::BadStatement(BadStatement::InvalidDynMatch)
        )));

        let main = air.function_mut(function_id);
        let AirStmt::DynMatch(match_) = main.body.block.stmts.last_mut().unwrap() else {
            panic!("expected dynamic match");
        };
        let AirDynMatchSource::Mutable(root) = &match_.source else {
            panic!("expected mutable source");
        };
        match_.arms[0]
            .block
            .stmts
            .push(AirStmt::Eval(RValue::Use(Operand::Place(root.clone()))));

        let errors = verify_air(&air).unwrap_err();
        assert!(errors.iter().any(|error| matches!(
            error.kind,
            VerifyErrorKind::BadStatement(BadStatement::DynMatchRootUsed)
        )));
    }

    #[test]
    fn readonly_dynamic_match_preserves_projected_source() {
        let air = lower_roots(
            "contract Drawable { fn draw(self); }
             struct Enemy { fn draw(self) {} }
             struct Holder { actor: dyn Drawable }
             fn main() {
                 let holder = Holder { actor: Enemy {} };
                 match holder.actor as? {
                     Enemy(enemy) => {},
                     other => {},
                 };
             }",
            &["main"],
        )
        .expect("projected dynamic match AIR lowering failed");
        let main = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("main"))
            .unwrap();
        assert!(function_statements(main).any(|stmt| matches!(
            stmt,
            AirStmt::DynMatch(AirDynMatch {
                source: AirDynMatchSource::Owned {
                    value: Operand::Place(Place { projection, .. }),
                    ..
                },
                ..
            }) if !projection.is_empty()
        )));
    }

    #[test]
    fn mutable_downcast_alias_supports_non_escaping_capture() {
        let air = lower_roots(
            "contract Updatable { fn update(ref self); }
             struct Enemy { hp: int, fn update(ref self) {} }
             fn touch(ref actor: dyn Updatable) {
                 if ref enemy = actor as? Enemy {
                     let f = || { enemy.hp = 1; };
                     f();
                 }
             }
             fn main() {
                 var actor: dyn Updatable = Enemy { hp: 0 };
                 touch(actor);
             }",
            &["main"],
        )
        .expect("captured mutable downcast alias AIR lowering failed");
        assert!(
            air.scoped_borrows
                .iter()
                .any(|borrow| matches!(borrow.source, ScopedBorrowSource::PatternAlias { .. }))
        );

        let mut forged = air.clone();
        let borrow = forged
            .scoped_borrows
            .iter()
            .position(|borrow| matches!(borrow.source, ScopedBorrowSource::PatternAlias { .. }))
            .map(ScopedBorrowId::from_index)
            .unwrap();
        let touch = forged
            .functions
            .iter()
            .position(|function| function.name == Ident::new("touch"))
            .map(FunctionId::from_index)
            .unwrap();
        let target = forged.scoped_borrows[borrow.index()].ty;
        let alias = forged.scoped_borrow_place(borrow).unwrap();
        let function = forged.function_mut(touch);
        function.signature.return_mode = ReturnMode::Place(target);
        let AirStmt::DynMatch(match_) = function.body.block.stmts.first_mut().unwrap() else {
            panic!("expected captured dynamic match");
        };
        match_.arms[0].block.tail = AirTail::Return(Some(Operand::Place(alias)));
        let errors = verify_air(&forged).unwrap_err();
        assert!(errors.iter().any(|error| matches!(
            error.kind,
            VerifyErrorKind::BadStatement(BadStatement::DynMatchAliasEscapes(_))
        )));
    }

    #[test]
    fn downcast_preserves_optional_dynamic_expected_context() {
        let air = lower_roots(
            "contract Drawable { fn draw(self); }
             struct Enemy { fn draw(self) {} }
             fn main() -> dyn Drawable? {
                 let actor: dyn Drawable = Enemy {};
                 actor as? Enemy
             }",
            &["main"],
        )
        .expect("contextual dynamic downcast AIR lowering failed");
        let main = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("main"))
            .unwrap();
        let values = function_statements(main).collect::<Vec<_>>();
        assert!(values.iter().any(|stmt| matches!(
            stmt,
            AirStmt::Init {
                value: RValue::DynDowncast { .. },
                ..
            }
        )));
        assert!(values.iter().any(|stmt| matches!(
            stmt,
            AirStmt::Init {
                value: RValue::DynPack { .. },
                ..
            }
        )));
    }

    #[test]
    fn mutable_downcast_alias_uses_structured_match() {
        let air = lower_roots(
            "contract Updatable { fn update(ref self); }
             struct Enemy { hp: int, fn update(ref self) {} }
             fn main() {
                 var actor: dyn Updatable = Enemy { hp: 1 };
                 if ref enemy = actor as? Enemy {
                     enemy.hp = 2;
                 }
             }",
            &["main"],
        )
        .expect("mutable downcast AIR lowering failed");
        let main = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("main"))
            .unwrap();
        let match_ = function_statements(main)
            .find_map(|stmt| match stmt {
                AirStmt::DynMatch(match_) => Some(match_),
                _ => None,
            })
            .expect("expected structured mutable downcast match");
        assert!(matches!(match_.source, AirDynMatchSource::Mutable(_)));
        assert_eq!(match_.arms.len(), 1);
        assert!(match_.arms[0].binding.local().is_some());
    }

    #[test]
    fn dynamic_pack_preserves_container_and_optional_contexts() {
        let air = lower_roots(
            "contract A { fn a(ref self); }
             struct Item { fn a(ref self) {} }
             fn main() {
                 let items: [dyn A] = [Item {}];
                 let maybe: (dyn A)? = Item {};
             }",
            &["main"],
        )
        .expect("contextual dynamic AIR lowering failed");

        let main = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("main"))
            .unwrap();
        let mut packs = 0;
        let mut optional = false;
        main.body.for_each_rvalue(&mut |value| match value {
            RValue::DynPack { .. } => packs += 1,
            RValue::OptionalSome { .. } => optional = true,
            _ => {}
        });
        assert_eq!(packs, 2);
        assert!(optional);
    }

    #[test]
    fn borrowed_dynamic_forms_are_explicit() {
        let air = lower_roots(
            "contract A { fn a(ref self); }
             contract B { fn b(ref self); }
             struct Both { fn a(ref self) {} fn b(ref self) {} }
             fn take_a(ref value: dyn A) { value.a(); }
             fn take_both(ref value: dyn A + B) { value.a(); }
             fn forward(ref value: dyn A + B) {
                 take_a(value);
                 take_both(value);
             }
             fn main() {
                 var both = Both {};
                 var actor: dyn A + B = both;
                 forward(actor);
                 forward(both);
             }",
            &["main"],
        )
        .expect("borrowed dynamic AIR lowering failed");

        let take_a = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("take_a"))
            .unwrap();
        assert!(matches!(
            take_a.body.block.stmts[0],
            AirStmt::Eval(RValue::DynCall {
                receiver: DynReceiver::Borrowed(_),
                ..
            })
        ));
        let forward = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("forward"))
            .unwrap();
        let AirStmt::Eval(RValue::Call { args, .. }) = &forward.body.block.stmts[0] else {
            panic!("expected forwarding call");
        };
        let CallArg::DynBorrow(borrow) = &args[0] else {
            panic!("expected borrowed dynamic argument");
        };
        assert!(matches!(borrow.source, DynBorrowSource::Borrowed(_)));
        assert!(borrow.weakening.is_some());
        let AirStmt::Eval(RValue::Call { args, .. }) = &forward.body.block.stmts[1] else {
            panic!("expected same-surface forwarding call");
        };
        assert!(matches!(
            args.first(),
            Some(CallArg::DynBorrow(DynBorrow {
                source: DynBorrowSource::Borrowed(_),
                weakening: None,
                ..
            }))
        ));
        let main = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("main"))
            .unwrap();
        let mut concrete = false;
        let mut owned = false;
        main.body.for_each_rvalue(&mut |value| {
            if let RValue::Call { args, .. } = value
                && matches!(
                    args.first(),
                    Some(CallArg::DynBorrow(DynBorrow {
                        source: DynBorrowSource::Concrete { .. },
                        ..
                    }))
                )
            {
                concrete = true;
            }
            if let RValue::Call { args, .. } = value
                && matches!(
                    args.first(),
                    Some(CallArg::DynBorrow(DynBorrow {
                        source: DynBorrowSource::Owned(_),
                        ..
                    }))
                )
            {
                owned = true;
            }
        });
        assert!(concrete && owned);
    }

    #[test]
    fn default_expression_extern_witness_enters_air_closure() {
        let air = lower_contract_boundary(
            "contract Movable { fn move_by(ref self, dx: float); }
             extern type Point {
                 fn new() -> Self;
                 fn move_by(ref self, dx: float);
             }
             fn use(movable: dyn Movable = Point.new()) {}
             fn main() { use(); }",
            &["main"],
        );

        let ContractWitnessTarget::Extern { function } =
            air.contract_witnesses[0].key.slots[0].target
        else {
            panic!("expected default-expression extern witness");
        };
        assert_eq!(air.extern_decl(function).name, Ident::new("move_by"));
    }

    #[test]
    fn cached_generic_witness_is_in_reachable_air_closure() {
        let air = lower_contract_boundary(
            "contract Drawable { fn draw(self); }
             struct Enemy { fn draw(self) {} }
             fn use_actor(actor: dyn Drawable) {}
             fn wrap<T>(actor: T) { use_actor(actor); }
             fn main() { wrap(Enemy {}); }",
            &["main"],
        );

        assert_eq!(air.contract_witnesses.len(), 1);
        assert_eq!(
            air.type_display_name(air.contract_witnesses[0].key.concrete_ty),
            "Enemy"
        );
    }

    #[test]
    fn lexical_extension_witnesses_remain_distinct_in_air() {
        let air = lower_contract_boundary_with_modules(
            "import aggressive_use;
             import passive_use;
             import item { Item };
             fn main(item: Item) {
                 aggressive_use.make(item);
                 passive_use.make(item);
             }",
            &[
                ("api", "pub contract A { fn a(self); }"),
                ("item", "pub struct Item {}"),
                (
                    "aggressive_ext",
                    "pub import item { Item }; pub extend Item { fn a(self) {} }",
                ),
                (
                    "passive_ext",
                    "pub import item { Item }; pub extend Item { fn a(self) {} }",
                ),
                (
                    "aggressive_use",
                    "pub import api { A }; pub import aggressive_ext { Item }; pub fn make(item: Item) -> dyn A { item }",
                ),
                (
                    "passive_use",
                    "pub import api { A }; pub import passive_ext { Item }; pub fn make(item: Item) -> dyn A { item }",
                ),
            ],
            &["main"],
        );

        assert_eq!(air.contract_witnesses.len(), 2);
        assert_eq!(
            air.contract_witnesses[0].key.concrete_ty,
            air.contract_witnesses[1].key.concrete_ty
        );
        assert_eq!(
            air.contract_witnesses[0].key.surface,
            air.contract_witnesses[1].key.surface
        );
        assert_ne!(
            air.contract_witnesses[0].key.slots[0].target,
            air.contract_witnesses[1].key.slots[0].target
        );
    }

    #[test]
    fn ref_requirement_accepts_value_receiver_witness() {
        let air = lower_contract_boundary(
            "contract Action { fn act(ref self); }
             struct Item { fn act(self) {} }
             fn main() { let action: dyn Action = Item {}; }",
            &["main"],
        );

        assert_eq!(air.contract_witnesses.len(), 1);
        assert_eq!(
            air.contract_witnesses[0].key.slots[0].receiver,
            ParamMode::SharedBorrow
        );
    }

    #[test]
    fn consumed_dynamic_temporary_cannot_be_reused() {
        let mut air = lower_roots(
            "contract Action { fn act(self); }
             struct Item { fn act(self) {} }
             fn main() {
                 let action: dyn Action = Item {};
                 action.act();
             }",
            &["main"],
        )
        .expect("dynamic pack AIR lowering failed");
        let main = air
            .functions
            .iter()
            .position(|function| function.name == Ident::new("main"))
            .map(FunctionId::from_index)
            .unwrap();
        let source = air
            .function(main)
            .body
            .block
            .stmts
            .iter()
            .find_map(|stmt| match stmt {
                AirStmt::Init {
                    value:
                        RValue::DynPack {
                            value:
                                Operand::Place(Place {
                                    root: PlaceRoot::Local(local),
                                    projection,
                                    ty,
                                }),
                            use_: DynOwnedUse::ConsumeTemporary,
                            ..
                        },
                    ..
                } if projection.is_empty() => Some((*local, *ty)),
                _ => None,
            })
            .expect("expected consumed pack temporary");
        air.function_mut(main)
            .body
            .block
            .stmts
            .push(AirStmt::Eval(RValue::Use(Operand::Place(Place {
                root: PlaceRoot::Local(source.0),
                projection: vec![],
                ty: source.1,
            }))));

        let errors = verify_air(&air).unwrap_err();
        assert!(errors.iter().any(|error| matches!(
            error.kind,
            VerifyErrorKind::BadStatement(BadStatement::ReadUninitializedLocal(local))
                if local == source.0
        )));
    }

    #[test]
    fn witness_value_params_accept_finalized_value_mode() {
        lower_roots(
            "contract Updatable { fn update(ref self, dt: float); }
             struct Enemy { fn update(ref self, dt: float) {} }
             fn main() {
                 var actor: dyn Updatable = Enemy {};
                 actor.update(1.0);
             }",
            &["main"],
        )
        .expect("finalized value parameter should satisfy contract witness");
    }

    #[test]
    fn forged_witness_receiver_adjustment_rejects() {
        let mut air = lower_contract_boundary(
            "contract Action { fn act(ref self); }
             struct Item { fn act(self) {} }
             fn main() { let action: dyn Action = Item {}; }",
            &["main"],
        );
        air.contract_witnesses[0].key.slots[0].receiver = ParamMode::MutBorrow;

        let errors = verify_contracts(&air).unwrap_err();
        assert!(errors.iter().any(|error| matches!(
            error.kind,
            VerifyErrorKind::BadContract(BadContract::WitnessTargetSignatureMismatch)
        )));

        let surface = air.contract_witnesses[0].key.surface;
        let dyn_ty = air.alloc_type(TypeData::Dyn(surface));
        air.contract_witnesses[0].key.concrete_ty = dyn_ty;
        let errors = verify_contracts(&air).unwrap_err();
        assert!(errors.iter().any(|error| matches!(
            error.kind,
            VerifyErrorKind::BadContract(BadContract::InvalidWitnessConcreteType)
        )));
    }

    #[test]
    fn extension_and_promoted_witness_targets_are_resolved() {
        let extension = lower_contract_boundary(
            "contract Action { fn act(self); }
             struct Item {}
             extend Item { fn act(self) {} }
             fn main() { let action: dyn Action = Item {}; }",
            &["main"],
        );
        let ContractWitnessTarget::Function { function } =
            extension.contract_witnesses[0].key.slots[0].target
        else {
            panic!("expected extension function target");
        };
        assert_eq!(extension.function(function).name, Ident::new("act"));

        let promoted = lower_contract_boundary(
            "contract Action { fn act(ref self); }
             struct Entity { fn act(ref self) {} }
             struct Item { embed entity: Entity }
             fn main() {
                 let action: dyn Action = Item { entity: Entity {} };
             }",
            &["main"],
        );
        let ContractWitnessTarget::Promoted { fields, target } =
            &promoted.contract_witnesses[0].key.slots[0].target
        else {
            panic!("expected promoted target");
        };
        assert_eq!(fields, &[FieldId::from_index(0)]);
        let ContractWitnessTarget::Function { function } = target.as_ref() else {
            panic!("expected promoted function target");
        };
        assert_eq!(promoted.function(*function).name, Ident::new("act"));
    }

    #[test]
    fn promoted_witness_keeps_renamed_exposed_slot() {
        let air = lower_contract_boundary(
            "contract Hittable { fn hit(self, amount: int) -> int; }
             struct Health { fn damage(self, amount: int) -> int { amount } }
             struct Enemy { embed health: Health { fn damage as hit }, }
             fn main() {
                 let target: dyn Hittable = Enemy { health: Health {} };
             }",
            &["main"],
        );

        let surface = air.contract_surface(air.contract_witnesses[0].key.surface);
        assert_eq!(surface.slots[0].name, Ident::new("hit"));
        let ContractWitnessTarget::Promoted { target, .. } =
            &air.contract_witnesses[0].key.slots[0].target
        else {
            panic!("expected promoted target");
        };
        let ContractWitnessTarget::Function { function } = target.as_ref() else {
            panic!("expected promoted function target");
        };
        assert_eq!(air.function(*function).name, Ident::new("damage"));
    }

    #[test]
    fn extern_witness_target_reuses_air_extern_declaration() {
        let air = lower_contract_boundary(
            "contract Movable { fn move_by(ref self, dx: float); }
             extern type Point { fn move_by(ref self, dx: float); }
             fn main(point: Point) { let movable: dyn Movable = point; }",
            &["main"],
        );

        let ContractWitnessTarget::Extern { function } =
            air.contract_witnesses[0].key.slots[0].target
        else {
            panic!("expected extern witness target");
        };
        assert_eq!(air.extern_decl(function).name, Ident::new("move_by"));
    }

    #[test]
    fn iterator_witness_target_is_explicit() {
        let air = lower_contract_boundary_full_core(
            "contract Sequence { fn items(self) -> iter; }
             struct Items { fn items(self) -> iter { iter(0..2) } }
             fn main() { let sequence: dyn Sequence = Items {}; }",
            &["main"],
        );

        let ContractWitnessTarget::IteratorFunction { function } =
            air.contract_witnesses[0].key.slots[0].target
        else {
            panic!("expected iterator function target");
        };
        assert_eq!(air.function(function).name, Ident::new("items"));
    }

    #[test]
    fn weakening_closes_projected_contract_witnesses() {
        let air = lower_contract_boundary(
            "contract A { fn a(self); }
             contract B { fn b(self); }
             struct Both { fn a(self) {} fn b(self) {} }
             fn main() {
                 let both: dyn A + B = Both {};
                 let b: dyn B = both;
             }",
            &["main"],
        );

        assert_eq!(air.contract_weakenings.len(), 1);
        assert_eq!(air.contract_witnesses.len(), 2);
        let weakening = &air.contract_weakenings[0];
        let source = air
            .contract_witnesses
            .iter()
            .find(|witness| witness.key.surface == weakening.source)
            .expect("missing source witness");
        let target = air
            .contract_witnesses
            .iter()
            .find(|witness| witness.key.surface == weakening.target)
            .expect("missing projected witness");
        assert_eq!(source.key.concrete_ty, target.key.concrete_ty);
        assert_eq!(target.key.slots.len(), 1);
        assert_eq!(target.key.slots[0].target, source.key.slots[1].target);
    }

    fn checked_with_same_named_local_nominals() -> (typecheck::SemanticCheckOutput, [Type; 4]) {
        let (_, _, semantic) = checked(
            "fn main() {
                struct Item {}
                enum Choice { Unit }
                { struct Item {} enum Choice { Unit } };
            }",
        );
        let decls = &semantic.program.declarations;
        let mut aggregates = decls
            .aggregates()
            .filter(|(key, _)| key.name == Ident::new("Item"))
            .map(|(key, _)| nominal_type(key))
            .collect::<Vec<_>>();
        let mut enums = decls
            .enums()
            .filter(|(key, _)| key.name == Ident::new("Choice"))
            .map(|(key, _)| nominal_type(key))
            .collect::<Vec<_>>();
        aggregates.sort_by_key(|ty| format!("{ty:?}"));
        enums.sort_by_key(|ty| format!("{ty:?}"));
        assert_eq!(aggregates.len(), 2);
        assert_eq!(enums.len(), 2);
        (
            semantic,
            [
                aggregates.remove(0),
                aggregates.remove(0),
                enums.remove(0),
                enums.remove(0),
            ],
        )
    }

    #[test]
    fn type_lowerer_cache_distinguishes_same_named_local_nominals() {
        let (semantic, types) = checked_with_same_named_local_nominals();
        let decls = &semantic.program.declarations;
        let mut program = Program::default();
        let mut modules = HashMap::new();
        let mut lowerer = TypeLowerer::default();
        let mut lower = |ty: &Type| {
            lowerer
                .lower_source(
                    &mut program,
                    &mut modules,
                    decls,
                    &semantic.program.externs,
                    &semantic.program.contract_surfaces,
                    &HashMap::new(),
                    ty,
                )
                .expect("lower failed")
        };

        let outer_aggregate = lower(&types[0]);
        let inner_aggregate = lower(&types[1]);
        let outer_enum = lower(&types[2]);
        let inner_enum = lower(&types[3]);

        assert_ne!(outer_aggregate, inner_aggregate);
        assert_ne!(outer_enum, inner_enum);
        assert_eq!(program.aggregates.len(), 2);
        assert_eq!(program.enums.len(), 2);
        assert_eq!(program.aggregates[0].name, program.aggregates[1].name);
        assert_eq!(program.enums[0].name, program.enums[1].name);
        assert_eq!(program.modules.len(), 1);
        assert_eq!(program.modules[0].aggregates.len(), 2);
        assert_eq!(program.modules[0].enums.len(), 2);
    }

    #[test]
    fn reachable_local_nominal_closure_uses_shared_air_paths() {
        let (root, resolved, semantic) = checked(
            "contract Named { fn name(self) -> string; }
             fn consume(value: dyn Named) -> string { value.name() }
             fn main() -> string {
                 struct Local {
                     value: int = field_value(),
                     fn total(self, add: int = add_value()) -> int { self.value + add }
                     fn apply(self) -> int { self.value }
                     fn name(self) -> string { \"local\" }
                     fn to_string(self) -> string { \"shown\" }
                 }
                 enum Choice { None, Some(Local) }
                 dataref Cell { value: int = 2 }
                 struct Unused {
                     value: int,
                     fn never(self) -> int { self.value }
                 }
                 fn field_value() -> int { 4 }
                 fn add_value() -> int {
                     let next = |x: int| x + 1;
                     next(2)
                 }
                 Local {}.total();
                 Local {}.apply();
                 let shown = Local {};
                 f\"{shown}\";
                 Choice.Some(Local {});
                 Cell {};
                 consume(Local {})
             }",
        );

        let air = lower_checked_roots(&root, &resolved, &semantic, &["main"])
            .expect("local nominal closure failed to lower");
        let local = air
            .aggregates
            .iter()
            .find(|decl| decl.name == Ident::new("Local"))
            .expect("reachable local aggregate was not emitted");

        assert!(
            air.aggregates
                .iter()
                .any(|decl| decl.name == Ident::new("Cell"))
        );
        assert!(
            air.enums
                .iter()
                .any(|decl| decl.name == Ident::new("Choice"))
        );
        assert!(
            air.aggregates
                .iter()
                .all(|decl| decl.name != Ident::new("Unused"))
        );
        assert!(
            air.functions
                .iter()
                .all(|function| function.name != Ident::new("never"))
        );
        for name in [
            "field_value",
            "add_value",
            "total",
            "apply",
            "name",
            "to_string",
        ] {
            assert!(
                air.functions
                    .iter()
                    .any(|function| function.name == Ident::new(name)),
                "missing reachable local method {name}"
            );
        }
        assert!(local.stringify_override.is_some());
        assert_eq!(air.lambdas.len(), 1);
        assert!(!air.contract_witnesses.is_empty());
    }

    #[test]
    fn reachable_local_nominals_preserve_owner_specializations() {
        let (root, resolved, semantic) = checked(
            "fn use<T>(value: T) -> T {
                 struct Local {
                     value: T,
                     fn get(self) -> T { self.value }
                 }
                 Local { value: value }.get()
             }
             fn main() { use(1); use(\"text\"); }",
        );

        let air = lower_checked_roots(&root, &resolved, &semantic, &["main"])
            .expect("owner-specialized local nominal failed to lower");
        let locals = air
            .aggregates
            .iter()
            .filter(|decl| decl.name == Ident::new("Local"))
            .collect::<Vec<_>>();

        assert_eq!(locals.len(), 2);
        assert_ne!(locals[0].type_args, locals[1].type_args);
        assert_eq!(
            air.functions
                .iter()
                .filter(|function| function.name == Ident::new("get"))
                .count(),
            2
        );
    }

    #[test]
    fn unused_owner_generic_keeps_reachable_instances_distinct() {
        let (root, resolved, semantic) = checked(
            "fn use<T>() -> int {
                struct Marker { tag: int }
                struct Unreachable { tag: int }
                let marker = Marker { tag: 1 };
                marker.tag
            }
            fn main() { use<int>(); use<string>(); }",
        );

        let air = lower_checked_roots(&root, &resolved, &semantic, &["main"])
            .expect("unused-owner local nominal failed to lower");
        let markers = air
            .aggregates
            .iter()
            .filter(|decl| decl.name == Ident::new("Marker"))
            .collect::<Vec<_>>();

        assert_eq!(markers.len(), 2);
        assert_ne!(markers[0].type_args, markers[1].type_args);
        assert!(markers.iter().all(|decl| {
            decl.fields.len() == 1 && matches!(air.type_data(decl.fields[0].ty), TypeData::Int)
        }));
        assert!(
            air.aggregates
                .iter()
                .all(|decl| decl.name != Ident::new("Unreachable"))
        );
        assert_eq!(
            air.modules
                .iter()
                .map(|module| module.aggregates.len())
                .sum::<usize>(),
            2
        );
    }

    #[test]
    fn local_placement_adds_no_box_or_closure_environment() {
        let (root, resolved, semantic) = checked(
            "fn main() -> int {
                struct Local {
                    value: int,
                    fn make(value: int) -> Self { Local { value } }
                    fn get(self) -> int { self.value }
                }
                fn helper() -> int { 2 }
                Local.make(5).get() + helper()
            }",
        );

        let air = lower_checked_roots(&root, &resolved, &semantic, &["main"])
            .expect("local declarations should lower without runtime wrappers");
        assert_eq!(air.aggregates.len(), 1);
        assert_eq!(air.aggregates[0].kind, AggregateKind::Struct);
        assert!(air.lambdas.is_empty());
        assert!(air.capture_cells.is_empty());
        assert!(
            air.type_arena
                .iter()
                .all(|ty| !matches!(ty, TypeData::DataRef(_)))
        );
        for name in ["make", "get", "helper"] {
            assert!(
                air.functions
                    .iter()
                    .any(|function| function.name == Ident::new(name)),
                "missing flat local callable {name}"
            );
        }
    }

    #[test]
    fn nominal_cache_uses_typed_const_identity() {
        let (_, _, semantic) = checked_with_full_core_shape("struct Marker {}");
        let id = semantic
            .program
            .declarations
            .local_nominal_type(&ModuleScope::Root, Ident::new("Marker"))
            .expect("missing Marker declaration")
            .id
            .clone();
        let nan_a = ast::ConstArg::value(ast::ConstValue::Float(f64::from_bits(
            0x7ff8_0000_0000_0001,
        )));
        let nan_b = ast::ConstArg::value(ast::ConstValue::Float(f64::from_bits(
            0x7ff8_0000_0000_0002,
        )));
        assert_eq!(nan_a.to_string(), nan_b.to_string());
        assert_ne!(
            TypeCacheKey::Nominal {
                id: id.clone(),
                type_args: vec![],
                const_args: vec![nan_a],
            },
            TypeCacheKey::Nominal {
                id,
                type_args: vec![],
                const_args: vec![nan_b],
            }
        );
    }

    #[test]
    fn type_lowerer_preserves_local_owner_and_declaration_args() {
        let (_, _, semantic) = checked(
            "fn outer<T>() { struct Box<U> { owner: T, item: U } }
             fn main() { outer<int>(); }",
        );
        let decls = &semantic.program.declarations;
        let key = decls
            .aggregates()
            .find(|(key, _)| key.name == Ident::new("Box"))
            .map(|(key, _)| key)
            .expect("missing local Box declaration");
        let ty = nominal_type_with_args(key, &[Type::Int, Type::String], &[]);
        let mut program = Program::default();
        let mut modules = HashMap::new();
        let id = TypeLowerer::default()
            .lower_source(
                &mut program,
                &mut modules,
                decls,
                &semantic.program.externs,
                &semantic.program.contract_surfaces,
                &HashMap::new(),
                &ty,
            )
            .expect("lower failed");

        let TypeData::Aggregate(aggregate) = program.type_data(id) else {
            panic!("expected aggregate type");
        };
        let decl = program.aggregate(*aggregate);
        assert_eq!(decl.type_args.len(), 2);
        assert_eq!(program.type_data(decl.type_args[0]), &TypeData::Int);
        assert_eq!(program.type_data(decl.type_args[1]), &TypeData::String);
        assert_eq!(program.type_data(decl.fields[0].ty), &TypeData::Int);
        assert_eq!(program.type_data(decl.fields[1].ty), &TypeData::String);
    }

    #[test]
    fn type_lowerer_lowers_nominal_aggregate_declarations() {
        let (_, _, semantic) = checked_with_full_core_shape("struct Point { x: int, y: string }");
        let mut program = Program::default();
        let mut modules = HashMap::new();
        let mut lowerer = TypeLowerer::default();
        let point = nominal_type(
            &semantic
                .program
                .declarations
                .local_nominal_type(&ModuleScope::Root, Ident::new("Point"))
                .expect("missing Point declaration"),
        );

        let id = lowerer
            .lower_source(
                &mut program,
                &mut modules,
                &semantic.program.declarations,
                &semantic.program.externs,
                &semantic.program.contract_surfaces,
                &HashMap::new(),
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
        let node = nominal_type(
            &semantic
                .program
                .declarations
                .local_nominal_type(&ModuleScope::Root, Ident::new("Node"))
                .expect("missing Node declaration"),
        );

        let id = lowerer
            .lower_source(
                &mut program,
                &mut modules,
                &semantic.program.declarations,
                &semantic.program.externs,
                &semantic.program.contract_surfaces,
                &HashMap::new(),
                &node,
            )
            .expect("lower failed");

        let TypeData::DataRef(agg) = program.type_data(id) else {
            panic!("expected dataref type");
        };
        assert!(program.aggregate(*agg).cycle_capable);
    }

    #[test]
    fn type_lowerer_reserves_mutually_recursive_enum_and_dataref() {
        let (_, _, semantic) = checked_with_full_core_shape(
            "dataref Node { choice: Choice } enum Choice { Next(Node) }",
        );
        let mut program = Program::default();
        let mut modules = HashMap::new();
        let mut lowerer = TypeLowerer::default();
        let choice = nominal_type(
            &semantic
                .program
                .declarations
                .local_nominal_type(&ModuleScope::Root, Ident::new("Choice"))
                .expect("missing Choice declaration"),
        );

        let id = lowerer
            .lower_source(
                &mut program,
                &mut modules,
                &semantic.program.declarations,
                &semantic.program.externs,
                &semantic.program.contract_surfaces,
                &HashMap::new(),
                &choice,
            )
            .expect("lower failed");

        assert!(matches!(program.type_data(id), TypeData::Enum(_)));
        assert_eq!(program.enums.len(), 1);
        assert_eq!(program.aggregates.len(), 1);
        let VariantShape::Tuple(payload) = &program.enums[0].variants[0].shape else {
            panic!("expected tuple enum payload");
        };
        let TypeData::DataRef(node) = program.type_data(payload[0]) else {
            panic!("expected dataref enum payload");
        };
        assert!(matches!(
            program.type_data(program.aggregate(*node).fields[0].ty),
            TypeData::Enum(_)
        ));
    }

    #[test]
    fn type_lowerer_lowers_nominal_enum_declarations() {
        let (_, _, semantic) =
            checked_with_full_core_shape("enum Choice { A, B(int), C { text: string } }");
        let mut program = Program::default();
        let mut modules = HashMap::new();
        let mut lowerer = TypeLowerer::default();
        let choice = nominal_type(
            &semantic
                .program
                .declarations
                .local_nominal_type(&ModuleScope::Root, Ident::new("Choice"))
                .expect("missing Choice declaration"),
        );

        let id = lowerer
            .lower_source(
                &mut program,
                &mut modules,
                &semantic.program.declarations,
                &semantic.program.externs,
                &semantic.program.contract_surfaces,
                &HashMap::new(),
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
            &semantic.public_facts,
            AirLowerConfig::default(),
        )
        .expect("lower failed");

        assert!(air.modules.is_empty());
        assert_eq!(resolved.system.core, Some(PackageId::core()));
        assert_eq!(
            core_fixture_module_paths(&resolved),
            vec![
                vec![],
                vec!["option"],
                vec!["result"],
                vec!["range"],
                vec!["collections"],
                vec!["flags"],
                vec!["runtime"],
                vec!["core_int"],
                vec!["core_float"],
                vec!["core_string"],
                vec!["core_char"],
            ]
        );
        let core_root = crate::resolve::ModuleId::root(PackageId::core());
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
                "core_char",
                "runtime",
                "option",
                "result",
                "range",
                "collections",
                "flags",
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
        let specialization = println
            .specialization
            .as_ref()
            .expect("println specialization");
        assert!(specialization.const_args.is_empty());
        assert!(matches!(
            specialization.type_args.as_slice(),
            [ty] if matches!(air.type_arena.data(*ty), TypeData::Int)
        ));
        assert_eq!(stringify_source_types(&air), vec![TypeData::Int]);
        assert_extern_signature(
            &air,
            "_println",
            &[(TypeData::String, ParamMode::SharedBorrow)],
            &TypeData::Void,
        );
    }

    #[test]
    fn repeated_generic_source_name_preserves_each_specialization() {
        let air = lower_full_core_entry("fn main() { println(1); println(2.0); }", "main", &[])
            .expect("lower failed");

        let specs = air
            .functions
            .iter()
            .filter(|function| function.name.as_str() == "println")
            .map(|function| {
                function
                    .specialization
                    .as_ref()
                    .expect("println specialization")
            })
            .collect::<Vec<_>>();

        assert_eq!(specs.len(), 2);
        assert!(specs.iter().any(|spec| matches!(
            spec.type_args.as_slice(),
            [ty] if matches!(air.type_arena.data(*ty), TypeData::Int)
        )));
        assert!(specs.iter().any(|spec| matches!(
            spec.type_args.as_slice(),
            [ty] if matches!(air.type_arena.data(*ty), TypeData::Float)
        )));
    }

    #[test]
    fn println_string_full_core_shape_emits_one_println_instance() {
        let air = lower_full_core_entry("fn main() { println(\"ready\"); }", "main", &[])
            .expect("lower failed");

        assert_eq!(function_names(&air), vec!["main", "println"]);
        assert_eq!(extern_names(&air), vec!["_println"]);
        assert_eq!(stringify_source_types(&air), vec![TypeData::String]);
        assert!(program_statements(&air).any(|statement| {
            matches!(
                statement,
                AirStmt::Eval(RValue::Call { args, .. })
                    if matches!(args.as_slice(), [CallArg::SharedStringConst(_)])
            )
        }));
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
            &[
                (TypeData::Bool, ParamMode::Value),
                (TypeData::String, ParamMode::SharedBorrow),
            ],
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
    fn reachable_extension_method_lowers_as_function() {
        let air = lower_root(
            "
            extend int { fn plus_one(self) -> int { self + 1 } }
            fn main() -> int { 1.plus_one() }
            ",
            "main",
        )
        .expect("lower failed");

        assert_eq!(function_names(&air), vec!["main", "plus_one"]);
    }

    #[test]
    fn reachable_core_extension_method_lowers_with_provider_extern() {
        let air = lower_full_core_entry("fn main() { let x = (-1).abs(); }", "main", &[])
            .expect("lower failed");

        assert!(function_names(&air).contains(&"abs"));
        assert!(extern_names(&air).contains(&"int_abs"));
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
        let err = lower_checked_entry(&root, &resolved, &semantic, "main", &[])
            .expect_err("expected unsupported qualified extension call");

        assert!(matches!(err, LowerError::UnsupportedCallForm { .. }));
    }

    #[test]
    fn function_value_call_lowers_to_lambda_callee() {
        let air =
            lower_root("fn main(f: fn() -> int) -> int { f() }", "main").expect("lower failed");
        let main = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("main"))
            .expect("missing main");

        assert!(function_statements(main).any(|statement| {
            matches!(statement, AirStmt::Init { value: RValue::Call { callee: Callee::Lambda(_), args }, .. } if args.is_empty())
        }));
    }

    #[test]
    fn named_function_value_can_be_passed_and_called() {
        let source = "fn tick() {} fn each(f: fn()) { f(); } fn main() { each(tick); }";
        let air = lower_root(source, "main").expect("lower failed");
        let each = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("each"))
            .expect("missing each");
        let main = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("main"))
            .expect("missing main");

        assert_eq!(function_names(&air), vec!["main", "each", "tick"]);
        assert!(function_statements(each).any(|statement| {
            matches!(statement, AirStmt::Eval(RValue::Call { callee: Callee::Lambda(_), args }) if args.is_empty())
        }));
        assert!(function_statements(main).any(|statement| {
            matches!(
                statement,
                AirStmt::Init {
                    value: RValue::FunctionRef { .. },
                    ..
                }
            )
        }));
    }

    #[test]
    fn named_function_alias_can_flow_to_escaping_param() {
        let source =
            "fn tick() {} fn keep(f: escaping fn()) {} fn main() { let f = tick; keep(f); }";
        let air = lower_root(source, "main").expect("lower failed");
        let main = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("main"))
            .expect("missing main");

        assert!(function_statements(main).any(|statement| {
            matches!(statement, AirStmt::Eval(RValue::Call { callee: Callee::Function(_), args }) if matches!(args.as_slice(), [CallArg::Value(Operand::Place(_))]))
        }));
    }

    #[test]
    fn stored_function_value_lowers_with_escape_capability() {
        let source = "fn tick() {} fn keep(f: escaping fn()) {} fn main() { let pair = (tick, tick); keep(pair.0); }";
        let air = lower_root(source, "main").expect("lower failed");
        let main = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("main"))
            .expect("missing main");

        assert!(function_statements(main).any(|statement| {
            matches!(
                statement,
                AirStmt::Init {
                    value: RValue::FunctionValue {
                        capability: FunctionValueCapability::Escaping,
                        ..
                    },
                    ..
                }
            )
        }));
    }

    #[test]
    fn lowers_function_param_escape_contracts() {
        let air = lower_root("fn main(cb: escaping fn()) {}", "main").expect("lower failed");
        let main = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("main"))
            .expect("missing main");

        assert_eq!(main.signature.params[0].escape, ParamEscape::Escaping);
    }

    #[test]
    fn extern_callback_param_lowers_as_non_escaping_function() {
        let air = lower_root(
            "extern fn native_apply(value: int, cb: fn(int) -> int) -> int; fn mul2(x: int) -> int { x * 2 } fn main() { let result = native_apply(5, mul2); }",
            "main",
        )
        .expect("lower failed");
        let native = air
            .externs
            .iter()
            .find(|decl| decl.name == Ident::new("native_apply"))
            .expect("missing extern");

        assert_eq!(native.params[1].mode, ParamMode::Value);
        assert_eq!(native.params[1].escape, ParamEscape::NonEscaping);
        let TypeData::Function(sig) = air.type_data(native.params[1].ty) else {
            panic!("expected function param type");
        };
        assert_eq!(sig.params[0].escape, ParamEscape::NonEscaping);
    }

    #[test]
    fn extern_callback_non_capturing_lambda_arg_lowers_as_value_arg() {
        let air = lower_root(
            "extern fn native_each(cb: fn(int)); fn main() { native_each(|x: int| { x; }); }",
            "main",
        )
        .expect("lower failed");

        assert_eq!(air.lambdas.len(), 1);
        assert!(air.lambdas[0].captures.is_empty());
        assert!(program_statements(&air).any(|statement| {
            matches!(statement, AirStmt::Init { value: RValue::MakeLambda { captures, .. }, .. }
                if captures.is_empty())
        }));
        assert!(program_statements(&air).any(|statement| {
            matches!(statement, AirStmt::Eval(RValue::Call { callee: Callee::Extern(_), args })
                if matches!(args.as_slice(), [CallArg::Value(Operand::Place(place))]
                    if matches!(air.type_data(place.ty), TypeData::Function(_))))
        }));
    }

    #[test]
    fn extern_callback_captured_lambda_arg_preserves_capture_metadata() {
        let air = lower_root(
            "extern fn native_each(cb: fn()); fn main(seed: int) { let x = seed + 1; native_each(|| { x; }); }",
            "main",
        )
        .expect("lower failed");
        let lambda = air.lambdas.first().expect("missing lambda");

        assert!(matches!(
            &lambda.captures[..],
            [LambdaCaptureDecl::ReadonlyLocal { .. }]
        ));
        assert!(program_statements(&air).any(|statement| {
            matches!(statement, AirStmt::Init { value: RValue::MakeLambda { captures, .. }, .. }
                if matches!(&captures[..], [LambdaCaptureArg::ReadonlyLocal { .. }]))
        }));
        assert!(program_statements(&air).any(|statement| {
            matches!(statement, AirStmt::Eval(RValue::Call { callee: Callee::Extern(_), args })
                if matches!(args.as_slice(), [CallArg::Value(Operand::Place(place))]
                    if matches!(air.type_data(place.ty), TypeData::Function(_))))
        }));
    }

    #[test]
    fn escaping_extern_callback_param_lowers() {
        let air = lower_root(
            "extern fn retain(cb: escaping fn(int)); fn callback(x: int) {} fn main() { retain(callback); }",
            "main",
        )
        .expect("lower failed");
        let retain = air
            .externs
            .iter()
            .find(|decl| decl.name == Ident::new("retain"))
            .expect("missing extern");

        assert_eq!(retain.params[0].mode, ParamMode::Value);
        assert_eq!(retain.params[0].escape, ParamEscape::Escaping);
        assert!(matches!(
            air.type_data(retain.params[0].ty),
            TypeData::Function(_)
        ));
    }

    #[test]
    fn source_callback_return_lowers_as_escaping_function_value() {
        let air = lower_root(
            "fn add(x: int) -> int { x + 1 } fn make() -> fn(int) -> int { add } fn main() { let f = make(); }",
            "main",
        )
        .expect("lower failed");

        assert!(program_statements(&air).any(|statement| {
            matches!(
                statement,
                AirStmt::Init {
                    value: RValue::FunctionValue {
                        capability: FunctionValueCapability::Escaping,
                        ..
                    },
                    ..
                }
            )
        }));
    }

    #[test]
    fn function_type_escape_affects_type_identity_and_rendering() {
        let air = lower_root("fn main(non: fn(fn()), esc: fn(escaping fn())) {}", "main")
            .expect("lower failed");
        let main = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("main"))
            .expect("missing main");
        let non_ty = main.signature.params[0].ty;
        let esc_ty = main.signature.params[1].ty;

        assert_ne!(non_ty, esc_ty);
        assert_ne!(air.type_helper_key(non_ty), air.type_helper_key(esc_ty));
        assert!(air.type_display_name(esc_ty).contains("escaping fn"));
        let TypeData::Function(esc_sig) = air.type_data(esc_ty) else {
            panic!("expected function type");
        };
        assert_eq!(esc_sig.params[0].escape, ParamEscape::Escaping);
    }

    #[test]
    fn reachable_string_extension_optional_return_lowers_to_optional_type() {
        let air = lower_full_core_entry(
            "fn main() { let x = \"abc\".substring(0, 1); }",
            "main",
            &[],
        )
        .expect("lower failed");

        assert!(function_names(&air).contains(&"substring"));
        assert!(extern_names(&air).contains(&"str_substring"));
        assert!(
            air.type_arena
                .iter()
                .any(|ty| matches!(ty, TypeData::Optional(_)))
        );
    }

    #[test]
    fn reachable_core_option_constructor_lowers_to_optional_some() {
        let air = lower_full_core_entry(
            "fn main() { let x: Option<int> = Option.Some(1); }",
            "main",
            &[],
        )
        .expect("lower failed");

        assert!(program_statements(&air).any(|statement| {
            matches!(
                statement,
                AirStmt::Init {
                    value: RValue::OptionalSome { .. },
                    ..
                }
            )
        }));
    }

    #[test]
    fn optional_air_type_allocation_is_canonical() {
        let mut cx = LowerCx::default();
        let int_ty = cx.program.alloc_type(TypeData::Int);

        let first = cx.optional_ty(int_ty);
        let second = cx.optional_ty(int_ty);

        assert_eq!(first, second);
        assert_eq!(
            cx.program
                .type_arena
                .iter()
                .filter(|ty| matches!(ty, TypeData::Optional(inner) if *inner == int_ty))
                .count(),
            1
        );
    }

    #[test]
    fn nil_constants_are_typed_as_optional() {
        let air = lower_full_core_entry("fn main() { let x: int? = nil; }", "main", &[])
            .expect("lower failed");

        assert!(air.const_arena.iter().any(|data| {
            matches!(data.value, ConstValue::Nil)
                && matches!(air.type_data(data.ty), TypeData::Optional(_))
        }));
    }

    #[test]
    fn option_some_wraps_exact_inner_type() {
        let air = lower_full_core_entry(
            "fn main() { let x: Option<int> = Option.Some(1); }",
            "main",
            &[],
        )
        .expect("lower failed");

        let mut found = false;
        for statement in program_statements(&air) {
            let AirStmt::Init {
                value: RValue::OptionalSome { value, ty },
                ..
            } = statement
            else {
                continue;
            };
            let TypeData::Optional(inner) = air.type_data(ty) else {
                panic!("OptionalSome result must be optional");
            };
            assert_eq!(test_operand_ty(&air, &value), *inner);
            found = true;
        }
        assert!(found);
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
        assert!(air.globals.is_empty());
        assert!(air.externs.is_empty());
        assert!(air.aggregates.is_empty());
        assert!(air.enums.is_empty());
    }

    #[test]
    fn reached_lazy_global_access_lowers_to_global_root() {
        let air = lower_root(
            "lazy let Value: int = 1; fn main() -> int { Value }",
            "main",
        )
        .expect("lower failed");

        assert_eq!(air.globals.len(), 1);
        let global = GlobalId::from_index(0);
        assert!(matches!(
            air.functions[air.globals[0].init.index()].kind,
            FunctionKind::GlobalInit(id) if id == global
        ));
        let main = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("main"))
            .expect("missing main");
        assert!(matches!(
            main.body.block.stmts.as_slice(),
            [AirStmt::GlobalEnsure { global: id }] if *id == global
        ));
        assert!(matches!(
            main.body.block.tail,
            AirTail::Return(Some(Operand::Place(Place {
                root: PlaceRoot::Global(id),
                projection: ref fields,
                ..
            }))) if id == global && fields.is_empty()
        ));
    }

    #[test]
    fn repeated_global_reads_reuse_one_global() {
        let air = lower_root(
            "lazy let Value: int = 1; fn main() -> int { Value + Value }",
            "main",
        )
        .expect("lower failed");

        assert_eq!(air.globals.len(), 1);
        let statements = program_statements(&air).collect::<Vec<_>>();
        let ensures = statements
            .iter()
            .filter(|statement| {
                matches!(
                    statement,
                    AirStmt::GlobalEnsure {
                        global: GlobalId(0)
                    }
                )
            })
            .count();
        assert_eq!(ensures, 2);
        let reads = statements
            .iter()
            .filter(|statement| {
                matches!(
                    statement,
                    AirStmt::Init {
                        value: RValue::Binary {
                            lhs: Operand::Place(Place {
                                root: PlaceRoot::Global(GlobalId(0)),
                                ..
                            }),
                            rhs: Operand::Place(Place {
                                root: PlaceRoot::Global(GlobalId(0)),
                                ..
                            }),
                            ..
                        },
                        ..
                    }
                )
            })
            .count();
        assert_eq!(reads, 1);
    }

    #[test]
    fn global_root_assignment_lowers_to_global_set_root() {
        let air = lower_root("lazy var Value: int = 1; fn main() { Value = 2; }", "main")
            .expect("lower failed");

        assert_eq!(air.globals.len(), 1);
        let statements = program_statements(&air).collect::<Vec<_>>();
        assert!(!statements.iter().any(|statement| matches!(
            statement,
            AirStmt::GlobalEnsure {
                global: GlobalId(0)
            }
        )));
        assert!(statements.iter().any(|statement| matches!(
            statement,
            AirStmt::GlobalSetRoot {
                global: GlobalId(0),
                init: GlobalInitEffect::StoreWithoutInit,
                ..
            }
        )));
    }

    #[test]
    fn global_root_assignment_evaluates_rhs_before_store() {
        let air = lower_root(
            "lazy let Source: int = 1; lazy var Target: int = 0; fn main() { Target = Source + 1; }",
            "main",
        )
        .expect("lower failed");

        let source = global_id(&air, "Source");
        let target = global_id(&air, "Target");
        let main = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("main"))
            .expect("missing main");
        let statements = function_statements(main).collect::<Vec<_>>();
        let ensure_source = statements
            .iter()
            .position(|statement| {
                matches!(statement, AirStmt::GlobalEnsure { global } if *global == source)
            })
            .expect("missing source ensure");
        let rhs = statements
            .iter()
            .position(|statement| {
                matches!(
                    statement,
                    AirStmt::Init {
                        value: RValue::Binary {
                            lhs: Operand::Place(Place {
                                root: PlaceRoot::Global(global),
                                ..
                            }),
                            ..
                        },
                        ..
                    } if *global == source
                )
            })
            .expect("missing rhs temp");
        let set_target = statements
            .iter()
            .position(|statement| {
                matches!(
                    statement,
                    AirStmt::GlobalSetRoot {
                        global,
                        init: GlobalInitEffect::StoreWithoutInit,
                        ..
                    } if *global == target
                )
            })
            .expect("missing target set");

        assert!(ensure_source < rhs && rhs < set_target);
        assert!(!statements.iter().any(|statement| matches!(
            statement,
            AirStmt::GlobalEnsure { global } if *global == target
        )));
    }

    #[test]
    fn global_root_compound_assignment_lowers_to_update_after_ensure() {
        let air = lower_root("lazy var Value: int = 1; fn main() { Value += 2; }", "main")
            .expect("lower failed");
        let statements = program_statements(&air).collect::<Vec<_>>();
        let ensure = statements
            .iter()
            .position(|statement| {
                matches!(statement, AirStmt::GlobalEnsure { global } if *global == GlobalId(0))
            })
            .expect("missing ensure");
        let update = statements
            .iter()
            .position(|statement| {
                matches!(statement, AirStmt::GlobalUpdateRoot { global, .. } if *global == GlobalId(0))
            })
            .expect("missing root update");

        assert_eq!(
            statements
                .iter()
                .filter(|statement| {
                    matches!(statement, AirStmt::GlobalEnsure { global } if *global == GlobalId(0))
                })
                .count(),
            1
        );
        assert!(ensure < update);
        assert!(
            !statements
                .iter()
                .any(|statement| matches!(statement, AirStmt::GlobalSetRoot { .. }))
        );
    }

    #[test]
    fn global_root_update_requires_prior_ensure() {
        let mut program = lower_root("lazy var Value: int = 1; fn main() { Value += 2; }", "main")
            .expect("lower failed");
        let main = program
            .functions
            .iter_mut()
            .find(|function| function.name == Ident::new("main"))
            .expect("missing main");
        main.body.block.stmts.retain(|statement| {
            !matches!(statement, AirStmt::GlobalEnsure { global } if *global == GlobalId(0))
        });

        let errors = verify(&program).expect_err("root update without ensure should fail");
        assert!(errors.iter().any(|error| matches!(
            error.kind,
            VerifyErrorKind::BadStatement(BadStatement::GlobalUpdateRootWithoutEnsure(GlobalId(0)))
        )));
    }

    #[test]
    fn projected_global_assignment_emits_ensure_then_assign() {
        let air = lower_root(
            "struct Box { value: int } lazy var State: Box = Box { value: 0 }; fn main() { State.value = 3; }",
            "main",
        )
        .expect("lower failed");

        let statements = program_statements(&air).collect::<Vec<_>>();
        let ensure = statements
            .iter()
            .position(|statement| {
                matches!(
                    statement,
                    AirStmt::GlobalEnsure {
                        global: GlobalId(0)
                    }
                )
            })
            .expect("missing ensure");
        let assign = statements
            .iter()
            .position(|statement| {
                matches!(
                    statement,
                    AirStmt::Assign {
                        dst: Place {
                            root: PlaceRoot::Global(GlobalId(0)),
                            projection,
                            ..
                        },
                        ..
                    } if !projection.is_empty()
                )
            })
            .expect("missing projected assign");
        assert!(ensure < assign);
    }

    #[test]
    fn global_safe_chain_ensures_global_base() {
        let air = lower_root(
            "struct Config { title: int } lazy let maybe_config: Config? = Config { title: 1 }; fn main() -> int? { maybe_config?.title }",
            "main",
        )
        .expect("lower failed");
        let main = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("main"))
            .expect("missing main");

        assert!(main.body.block.stmts.iter().any(|statement| matches!(
            statement,
            AirStmt::GlobalEnsure {
                global: GlobalId(0)
            }
        )));
        assert!(main.body.block.stmts.iter().any(|statement| matches!(
            statement,
            AirStmt::OptionalMatch(AirOptionalMatch {
                discr: Place {
                    root: PlaceRoot::Global(GlobalId(0)),
                    ..
                },
                ..
            })
        )));
    }

    #[test]
    fn global_ref_argument_emits_ensure_and_mut_borrow_arg() {
        let air = lower_root(
            "lazy var Value: int = 1; fn set(ref x: int) { x = 2; } fn main() { set(Value); }",
            "main",
        )
        .expect("lower failed");

        let statements = program_statements(&air).collect::<Vec<_>>();
        assert!(statements.iter().any(|statement| matches!(
            statement,
            AirStmt::GlobalEnsure {
                global: GlobalId(0)
            }
        )));
        assert!(statements.iter().any(|statement| matches!(
            statement,
            AirStmt::Eval(RValue::Call { args, .. })
                if matches!(args.as_slice(), [CallArg::MutBorrow(Place { root: PlaceRoot::Global(GlobalId(0)), .. })])
        )));
    }

    #[test]
    fn global_extern_shared_borrow_emits_ensure_and_borrow_arg() {
        let air = lower_global_extern_arg(ParamFlow::Borrow, "lazy let Value: int = 1;");
        let statements = program_statements(&air).collect::<Vec<_>>();

        assert!(statements.iter().any(|statement| matches!(
            statement,
            AirStmt::GlobalEnsure {
                global: GlobalId(0)
            }
        )));
        assert!(statements.iter().any(|statement| matches!(
            statement,
            AirStmt::Eval(RValue::Call { args, .. })
                if matches!(args.as_slice(), [CallArg::SharedBorrow(Place { root: PlaceRoot::Global(GlobalId(0)), .. })])
        )));
    }

    #[test]
    fn raw_enum_extern_borrow_materializes_projection() {
        let air = lower_raw_enum_extern_borrow();
        let statements = program_statements(&air).collect::<Vec<_>>();

        assert!(statements.iter().any(|statement| matches!(
            statement,
            AirStmt::Init {
                value: RValue::RawProject { .. },
                ..
            }
        )));
        assert!(statements.iter().any(|statement| matches!(
            statement,
            AirStmt::Eval(RValue::Call { args, .. })
                if matches!(args.as_slice(), [CallArg::SharedBorrow(_)])
        )));
    }

    #[test]
    fn global_extern_mut_borrow_emits_ensure_and_borrow_arg() {
        let air = lower_global_extern_arg(ParamFlow::MutBorrow, "lazy var Value: int = 1;");
        let statements = program_statements(&air).collect::<Vec<_>>();

        assert!(statements.iter().any(|statement| matches!(
            statement,
            AirStmt::GlobalEnsure {
                global: GlobalId(0)
            }
        )));
        assert!(statements.iter().any(|statement| matches!(
            statement,
            AirStmt::Eval(RValue::Call { args, .. })
                if matches!(args.as_slice(), [CallArg::MutBorrow(Place { root: PlaceRoot::Global(GlobalId(0)), .. })])
        )));
    }

    #[test]
    fn global_mut_receiver_emits_ensure_and_mut_borrow_receiver() {
        let air = lower_root(
            "struct Box { value: int } extend Box { fn reset(ref self) { self.value = 0; } } lazy var State: Box = Box { value: 1 }; fn main() { State.reset(); }",
            "main",
        )
        .expect("lower failed");

        let statements = program_statements(&air).collect::<Vec<_>>();
        assert!(statements.iter().any(|statement| matches!(
            statement,
            AirStmt::GlobalEnsure {
                global: GlobalId(0)
            }
        )));
        assert!(statements.iter().any(|statement| matches!(
            statement,
            AirStmt::Eval(RValue::Call { args, .. })
                if matches!(args.as_slice(), [CallArg::MutBorrow(Place { root: PlaceRoot::Global(GlobalId(0)), .. })])
        )));
    }

    #[test]
    fn global_initializer_read_extends_reachability() {
        let air = lower_root(
            "lazy let Dep: int = 1; lazy let Value: int = Dep + 1; fn main() -> int { Value }",
            "main",
        )
        .expect("lower failed");

        assert_eq!(air.globals.len(), 2);
        let statements = program_statements(&air).collect::<Vec<_>>();
        assert!(
            statements
                .iter()
                .any(|statement| matches!(statement, AirStmt::GlobalEnsure { .. }))
        );
        assert!(statements.iter().any(|statement| matches!(
            statement,
            AirStmt::Init {
                value: RValue::Binary {
                    lhs: Operand::Place(Place {
                        root: PlaceRoot::Global(_),
                        ..
                    }),
                    ..
                },
                ..
            }
        )));
    }

    #[test]
    fn global_initializer_block_lowers_locals_and_reads() {
        let air = lower_root(
            "lazy let base: int = 1; lazy let value: int = { let local = base + 1; local }; fn main() -> int { value }",
            "main",
        )
        .expect("lower failed");
        let base = global_id(&air, "base");
        let value = global_id(&air, "value");
        let init = air
            .functions
            .iter()
            .find(|function| function.kind == FunctionKind::GlobalInit(value))
            .expect("missing value init");
        let statements = function_statements(init).collect::<Vec<_>>();

        assert!(statements.iter().any(|statement| matches!(
            statement,
            AirStmt::GlobalEnsure { global } if *global == base
        )));
        assert!(statements.iter().any(|statement| matches!(
            statement,
            AirStmt::Init {
                value: RValue::Binary {
                    lhs: Operand::Place(Place {
                        root: PlaceRoot::Global(global),
                        ..
                    }),
                    ..
                },
                ..
            } if *global == base
        )));
        assert!(matches!(
            init.body.block.tail,
            AirTail::Return(Some(Operand::Place(Place {
                root: PlaceRoot::Local(_),
                ..
            })))
        ));
    }

    #[test]
    fn global_initializer_lambda_reads_global_as_global_root() {
        let air = lower_root(
            "lazy let base: int = 1; lazy let make: fn() -> int = || base; fn main() { let f: fn() -> int = make; }",
            "main",
        )
        .expect("lower failed");
        let base = global_id(&air, "base");
        let make = global_id(&air, "make");
        let init = air
            .functions
            .iter()
            .find(|function| function.kind == FunctionKind::GlobalInit(make))
            .expect("missing make init");

        assert!(function_statements(init).any(|statement| matches!(
            statement,
            AirStmt::Init {
                value: RValue::MakeLambda { .. },
                ..
            }
        )));
        assert!(air.lambdas.iter().all(|lambda| lambda.captures.is_empty()));
        assert!(air.functions.iter().any(|function| {
            matches!(function.kind, FunctionKind::Lambda(_))
                && matches!(
                    function.body.block.stmts.as_slice(),
                    [AirStmt::GlobalEnsure { global }] if *global == base
                )
                && matches!(
                    function.body.block.tail,
                    AirTail::Return(Some(Operand::Place(Place {
                        root: PlaceRoot::Global(global),
                        ..
                    }))) if global == base
                )
        }));
    }

    #[test]
    fn global_initializer_cycle_lowers_without_static_rejection() {
        let air = lower_root(
            "lazy let A: int = B; lazy let B: int = A; fn main() -> int { A }",
            "main",
        )
        .expect("lower failed");

        assert_eq!(air.globals.len(), 2);
    }

    #[test]
    fn lambda_reads_global_as_global_root() {
        let air = lower_root(
            "lazy let Value: int = 1; fn make() -> fn() -> int { || Value } fn main() -> int { make()() }",
            "main",
        )
        .expect("lower failed");

        assert_eq!(air.globals.len(), 1);
        assert!(air.lambdas.iter().all(|lambda| lambda.captures.is_empty()));
        assert!(air.functions.iter().any(|function| {
            matches!(function.kind, FunctionKind::Lambda(_))
                && matches!(
                    function.body.block.stmts.as_slice(),
                    [AirStmt::GlobalEnsure {
                        global: GlobalId(0)
                    }]
                )
                && matches!(
                    function.body.block.tail,
                    AirTail::Return(Some(Operand::Place(Place {
                        root: PlaceRoot::Global(GlobalId(0)),
                        ..
                    })))
                )
        }));
    }

    #[test]
    fn qualified_global_root_read_lowers_without_value_projection() {
        let (root, resolved, semantic) = checked_with_modules(
            "import dep; fn main() -> int { dep.count }",
            &[("dep", "pub lazy let count: int = 1;")],
        );
        let air =
            lower_checked_roots(&root, &resolved, &semantic, &["main"]).expect("lower failed");
        let main = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("main"))
            .expect("missing main");

        assert!(matches!(
            main.body.block.tail,
            AirTail::Return(Some(Operand::Place(Place {
                root: PlaceRoot::Global(GlobalId(0)),
                projection: ref fields,
                ..
            }))) if fields.is_empty()
        ));
    }

    #[test]
    fn qualified_global_root_assignment_lowers_to_root_set() {
        let (root, resolved, semantic) = checked_with_modules(
            "import dep; fn main() { dep.count = 2; }",
            &[("dep", "pub lazy var count: int = 1;")],
        );
        let air =
            lower_checked_roots(&root, &resolved, &semantic, &["main"]).expect("lower failed");
        let statements = program_statements(&air).collect::<Vec<_>>();

        assert!(statements.iter().any(|statement| matches!(
            statement,
            AirStmt::GlobalSetRoot {
                global: GlobalId(0),
                ..
            }
        )));
        assert!(!statements.iter().any(|statement| matches!(
            statement,
            AirStmt::Assign {
                dst: Place {
                    root: PlaceRoot::Global(GlobalId(0)),
                    projection,
                    ..
                },
                ..
            } if projection.is_empty()
        )));
    }

    #[test]
    fn tuple_global_read_lowers_from_final_fact() {
        let air = lower_root(
            "lazy let pair: (int, int) = (1, 2); fn main() -> int { pair.0 }",
            "main",
        )
        .expect("lower failed");
        let main = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("main"))
            .expect("missing main");

        assert!(matches!(
            main.body.block.tail,
            AirTail::Return(Some(Operand::Place(Place {
                root: PlaceRoot::Global(GlobalId(0)),
                projection: ref fields,
                ..
            }))) if matches!(fields.as_slice(), [crate::air::Projection::TupleField(0)])
        ));
    }

    #[test]
    fn indexed_global_read_lowers_from_final_fact() {
        let air = lower_root(
            "lazy let xs: [int; 2] = [1, 2]; fn main() -> int { xs[0] }",
            "main",
        )
        .expect("lower failed");
        let main = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("main"))
            .expect("missing main");

        assert!(matches!(
            main.body.block.tail,
            AirTail::Return(Some(Operand::Place(Place {
                root: PlaceRoot::Global(GlobalId(0)),
                projection: ref fields,
                ..
            }))) if matches!(fields.as_slice(), [crate::air::Projection::Index(_)])
        ));
    }

    #[test]
    fn map_global_read_lowers_to_map_get() {
        let air = lower_root(
            "lazy let counts: [string: int] = [\"a\": 1]; fn main() -> int? { counts[\"a\"] }",
            "main",
        )
        .expect("lower failed");
        let statements = program_statements(&air).collect::<Vec<_>>();

        assert!(statements.iter().any(|statement| matches!(
            statement,
            AirStmt::Init {
                value: RValue::MapGet {
                    map: Place {
                        root: PlaceRoot::Global(GlobalId(0)),
                        ..
                    },
                    ..
                },
                ..
            }
        )));
    }

    #[test]
    fn qualified_global_projected_read_lowers_only_value_suffix() {
        let (root, resolved, semantic) = checked_with_modules(
            "import dep; fn main() -> int { dep.state.value }",
            &[(
                "dep",
                "pub struct Box { value: int } pub lazy var state: Box = Box { value: 1 };",
            )],
        );
        let air =
            lower_checked_roots(&root, &resolved, &semantic, &["main"]).expect("lower failed");
        let main = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("main"))
            .expect("missing main");

        assert!(matches!(
            main.body.block.tail,
            AirTail::Return(Some(Operand::Place(Place {
                root: PlaceRoot::Global(GlobalId(0)),
                projection: ref fields,
                ..
            }))) if fields.len() == 1
        ));
    }

    #[test]
    fn dependency_function_reaches_own_global() {
        let (root, resolved, semantic) = checked_with_modules(
            "import dep { value }; fn main() -> int { value() }",
            &[(
                "dep",
                "lazy let Count: int = 1; pub fn value() -> int { Count }",
            )],
        );
        let air =
            lower_checked_roots(&root, &resolved, &semantic, &["main"]).expect("lower failed");

        assert_eq!(air.globals.len(), 1);
        let global = &air.globals[0];
        assert_eq!(
            air.modules[global.module.index()].path,
            vec![Ident::new("dep")]
        );
    }

    #[test]
    fn reexported_global_uses_one_declaring_identity() {
        let (root, resolved, semantic) = checked_with_modules(
            "import prelude { atlas }; fn main() -> int { atlas + atlas }",
            &[
                ("assets", "pub lazy let atlas: int = 1;"),
                ("prelude", "pub import assets { atlas } ;"),
            ],
        );
        let air =
            lower_checked_roots(&root, &resolved, &semantic, &["main"]).expect("lower failed");

        assert_eq!(air.globals.len(), 1);
        let global = &air.globals[0];
        assert_eq!(
            air.modules[global.module.index()].path,
            vec![Ident::new("assets")]
        );
    }

    #[test]
    fn source_modules_follow_resolve_order() {
        let root = parse_program("import gamekit; import util;");
        let resolved = resolved_modules_with_core_option(
            &root,
            &[("gamekit", "fn g() {}"), ("util", "fn u() {}")],
        );
        let source_index = crate::source_ast::SourceAstIndex::new(&root, &resolved);
        let modules = &source_index.modules;
        assert_eq!(modules.len(), 5);
        assert_eq!(modules[0], ModuleScope::from_module_id(&resolved.root));
        assert!(modules.iter().all(|module| {
            !module_path(module).is_empty() || matches!(module, ModuleScope::Root)
        }));
    }

    #[test]
    fn source_functions_pair_with_semantic_facts() {
        with_source_functions("fn f(a: int) -> int { a }", &["f"], |_, functions, _| {
            assert_eq!(functions.items.len(), 1);
            let function = &functions.items[0];
            assert_eq!(test_reachable_callable(function).module(), 0);
            assert_eq!(test_reachable_callable(function).name(), Ident::new("f"));
            assert_eq!(function.body, test_reachable_fact(function).body);
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
            &semantic.public_facts,
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
            &semantic.public_facts,
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
            &semantic.public_facts,
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
                    test_reachable_callable(function).name() == Ident::new("f")
                        && test_reachable_fact(function).args.type_args == vec![Type::Int]
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
            air.type_arena.get(id_functions[0].signature.return_type()),
            Some(TypeData::Int)
        ));
    }

    #[test]
    fn generic_identity_lowers_independent_instances() {
        let source =
            "fn id<T>(x: T) -> T { x } fn f() -> int { id(1) } fn g() -> bool { id(true) }";
        let air = lower_roots(source, &["f", "g"]).expect("lower failed");

        let id_functions = air
            .functions
            .iter()
            .filter(|function| function.name == Ident::new("id"))
            .collect::<Vec<_>>();
        assert_eq!(id_functions.len(), 2);
        assert!(id_functions.iter().any(|function| matches!(
            air.type_arena.get(function.signature.return_type()),
            Some(TypeData::Int)
        )));
        assert!(id_functions.iter().any(|function| matches!(
            air.type_arena.get(function.signature.return_type()),
            Some(TypeData::Bool)
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
            matches!(statement, AirStmt::Init { value: RValue::Call { callee: Callee::Function(_), args }, .. } if args.len() == 1)
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
        let source = "fn id<T>(x: T) -> T { x } fn f() { let a = id(true); let b = id(1); }";
        let air = lower_root(source, "f").expect("lower failed");
        let id_returns = air
            .functions
            .iter()
            .filter(|function| function.name == Ident::new("id"))
            .map(|function| air.type_arena.get(function.signature.return_type()))
            .collect::<Vec<_>>();

        assert_eq!(id_returns.len(), 2);
        assert!(matches!(id_returns[0], Some(TypeData::Bool)));
        assert!(matches!(id_returns[1], Some(TypeData::Int)));
    }

    #[test]
    fn map_for_tuple_entry_pattern_lowers() {
        lower_root("fn f(m: [string: int]) { for (k, v) in m { k; v; } }", "f")
            .expect("lower failed");
    }

    #[test]
    fn map_for_ref_value_lowers_map_value_slot() {
        let air = lower_root(
            "fn f(ref m: [string: int]) { for k, ref v in m { v += 1; } }",
            "f",
        )
        .expect("lower failed");

        assert!(program_statements(&air).any(|statement| {
            matches!(
                statement,
                AirStmt::CollectionSlotScope(AirCollectionSlotScope { slots, .. })
                    if matches!(slots.as_slice(), [AirCollectionSlot { kind: AirCollectionSlotKind::MapValue, mutable: true, .. }])
            )
        }));
    }

    #[test]
    fn for_body_break_and_continue_lower_inside_loan_loop() {
        lower_root(
            "fn f(xs: [int]) { for x in xs { if x == 0 { continue; } break; } }",
            "f",
        )
        .expect("lower failed");
    }

    #[test]
    fn range_index_slice_arg_lowers_to_copy_then_view() {
        let air = lower_full_core_root(
            "fn take(s: slice[int]) {} fn f(xs: [int]) { take(xs[1..3]); }",
            "f",
        )
        .expect("lower failed");

        let statements = program_statements(&air).collect::<Vec<_>>();
        let range_copy = statements.iter().find_map(|statement| match statement {
            AirStmt::Init {
                local,
                value: RValue::RangeListCopy { .. },
            } => Some(*local),
            _ => None,
        });
        let range_copy = range_copy.expect("missing range list copy");

        assert!(statements.iter().any(|statement| matches!(
            statement,
            AirStmt::Init {
                value: RValue::SliceView { source, .. },
                ..
            } if source.root == PlaceRoot::Local(range_copy) && source.projection.is_empty()
        )));
    }

    #[test]
    fn direct_range_index_lowers_to_copy_without_slice_view() {
        let air = lower_full_core_root("fn f(xs: [int]) { let ys = xs[1..3]; }", "f")
            .expect("lower failed");

        let statements = program_statements(&air).collect::<Vec<_>>();
        assert!(statements.iter().any(|statement| matches!(
            statement,
            AirStmt::Init {
                value: RValue::RangeListCopy { .. },
                ..
            }
        )));
        assert!(!statements.iter().any(|statement| matches!(
            statement,
            AirStmt::Init {
                value: RValue::SliceView { .. },
                ..
            }
        )));
    }

    #[test]
    fn slice_for_lowers_to_collection_loan_loop() {
        let air =
            lower_root("fn f(xs: slice[int]) { for x in xs { x; } }", "f").expect("lower failed");
        let function = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("f"))
            .expect("missing f");

        assert!(function.body.block.stmts.iter().any(|stmt| matches!(
            stmt,
            AirStmt::CollectionLoan(AirCollectionLoan {
                root_kind: AirCollectionRootKind::Slice,
                mode: AirCollectionLoanMode::ReadonlySequence,
                ..
            })
        )));
    }

    #[test]
    fn while_lowers_to_loop_with_condition_branch_and_fallthrough_continue() {
        let air = lower_root("fn f() { var x = 0; while x < 2 { x = x + 1; } }", "f")
            .expect("lower failed");
        let function = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("f"))
            .expect("missing f");
        let loop_ = function
            .body
            .block
            .stmts
            .iter()
            .find_map(|stmt| match stmt {
                AirStmt::Loop(loop_) => Some(loop_),
                _ => None,
            })
            .expect("missing loop");

        assert_eq!(loop_.id, AirLoopId::from_index(0));
        let branch_index = loop_
            .body
            .stmts
            .iter()
            .position(|stmt| matches!(stmt, AirStmt::If(_)))
            .expect("missing loop condition branch");
        assert!(
            loop_.body.stmts[..branch_index]
                .iter()
                .all(|stmt| matches!(stmt, AirStmt::Init { .. }))
        );
        let AirStmt::If(branch) = &loop_.body.stmts[branch_index] else {
            unreachable!()
        };
        assert_eq!(branch.then_block.tail, AirTail::Continue(loop_.id));
        assert!(matches!(
            branch.else_block.as_ref().map(|block| &block.tail),
            Some(AirTail::Break(id)) if *id == loop_.id
        ));
    }

    #[test]
    fn while_preserves_explicit_break_and_continue_tails() {
        let air = lower_root(
            "fn f() { while true { break; } while true { continue; } }",
            "f",
        )
        .expect("lower failed");
        let function = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("f"))
            .expect("missing f");
        let loops = function
            .body
            .block
            .stmts
            .iter()
            .filter_map(|stmt| match stmt {
                AirStmt::Loop(loop_) => Some(loop_),
                _ => None,
            })
            .collect::<Vec<_>>();

        assert_eq!(loops.len(), 2);
        let first_branch = loops[0]
            .body
            .stmts
            .iter()
            .find_map(|stmt| match stmt {
                AirStmt::If(branch) => Some(branch),
                _ => None,
            })
            .expect("missing first branch");
        assert_eq!(first_branch.then_block.tail, AirTail::Break(loops[0].id));
        let second_branch = loops[1]
            .body
            .stmts
            .iter()
            .find_map(|stmt| match stmt {
                AirStmt::If(branch) => Some(branch),
                _ => None,
            })
            .expect("missing second branch");
        assert_eq!(
            second_branch.then_block.tail,
            AirTail::Continue(loops[1].id)
        );
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
        let err = lower_checked_roots(&root, &resolved, &semantic, &["f"])
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
        let err = lower_checked_roots(&root, &resolved, &semantic, &["f"])
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

        lower_checked_roots(&root, &resolved, &semantic, &["main"]).expect("lower failed");
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
            .insert(ExprId(u64::MAX), CallTarget::new(id.clone(), args.clone()));
        let source_index = crate::source_ast::SourceAstIndex::new(&root, &resolved);
        let site = source_index
            .declarations()
            .find_map(|(site, _, decl)| match decl {
                crate::source_ast::SourceDecl::Function(func)
                    if func.node.name == Ident::new("id") =>
                {
                    Some(site)
                }
                _ => None,
            })
            .expect("missing id declaration");
        semantic
            .program
            .declaration_facts
            .functions
            .push(SemanticFunctionInstanceFact {
                site: crate::source_ast::SourceCallableSite::Function(site),
                id: id.clone(),
                args: args.clone(),
                body: BodyInstanceKey::Callable(CallableInstanceKey { target: id, args }),
                module: ModuleScope::Root,
                name: Ident::new("id"),
                span: semantic.program.declaration_facts.functions[0].span,
                body_span: semantic.program.declaration_facts.functions[0].body_span,
                params: vec![],
                ret: ast::ReturnSpec::value(Type::Infer),
                is_stringify_override: false,
            });
        let err = lower_checked_roots(&root, &resolved, &semantic, &["main"])
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
                AirStmt::Init {
                    value: RValue::Stringify { value: _, source_ty },
                    ..
                } if source_ty != string_ty
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
        let source = r"fn side() {} fn f() -> string { #stringify(side()) }";
        let air = lower_full_core_root(source, "f").expect("lower failed");
        let function = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("f"))
            .expect("function missing");

        let body = &function.body.block;
        assert!(matches!(
            body.stmts.first(),
            Some(AirStmt::Eval(RValue::Call { .. }))
        ));
        assert!(matches!(
            body.tail,
            AirTail::Return(Some(Operand::Const(id)))
                if matches!(air.const_data(id).value, ConstValue::String(ref s) if s.as_ref() == "<void>")
        ));
    }

    #[test]
    fn stringify_list_param_lowers_composite_source_type() {
        let source = "fn f(xs: [int]) -> string { #stringify(xs) }";
        let air = lower_full_core_root(source, "f").expect("lower failed");

        assert!(program_statements(&air).any(|statement| {
            matches!(
                statement,
                AirStmt::Init {
                    value: RValue::Stringify { source_ty, .. },
                    ..
                } if matches!(air.type_data(source_ty), TypeData::List(_))
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
                AirStmt::Init {
                    value: RValue::Stringify { value: _, source_ty },
                    ..
                } if source_ty == int_ty
            )
        }));
    }

    #[test]
    fn enum_variant_constructors_lower_to_air_aggregates() {
        let source = r"
            enum Message { Quit, Ping(int), Move { x: int, y: int } }
            fn f() {
                let a = Message.Quit;
                let b = Message.Ping(42);
                let c = Message.Move { x: 1, y: 2 };
            }
        ";
        let air = lower_full_core_root(source, "f").expect("lower failed");
        let enum_ty = air
            .type_arena
            .iter()
            .position(|ty| matches!(ty, TypeData::Enum(_)))
            .map(TypeId::from_index)
            .expect("enum type missing");
        let variants = program_statements(&air)
            .filter_map(|statement| match statement {
                AirStmt::Init {
                    value:
                        RValue::Aggregate {
                            kind: AggregateCtor::EnumVariant { variant, .. },
                            ty,
                            fields,
                        },
                    ..
                } if ty == enum_ty => Some((variant.index(), fields.len())),
                _ => None,
            })
            .collect::<Vec<_>>();

        assert_eq!(variants, vec![(0, 0), (1, 1), (2, 2)]);
    }

    #[test]
    fn inferred_enum_variants_lower_to_air_aggregates() {
        let source = r"
            enum Message { Quit, Ping(int), Move { x: int, y: int } }
            fn f() {
                let a: Message = .Quit;
                let b: Message = .Ping(42);
                let c: Message = .Move { x: 1, y: 2 };
            }
        ";
        let air = lower_full_core_root(source, "f").expect("lower failed");
        let variants = program_statements(&air)
            .filter_map(|statement| match statement {
                AirStmt::Init {
                    value:
                        RValue::Aggregate {
                            kind: AggregateCtor::EnumVariant { variant, .. },
                            fields,
                            ..
                        },
                    ..
                } => Some((variant.index(), fields.len())),
                _ => None,
            })
            .collect::<Vec<_>>();

        assert_eq!(variants, vec![(0, 0), (1, 1), (2, 2)]);
    }

    #[test]
    fn enum_typed_field_read_does_not_lower_as_unit_variant() {
        let source = r"
            enum Color { Red, Blue }
            struct Box { color: Color }
            fn f(box: Box) -> Color { box.color }
        ";
        let air = lower_full_core_root(source, "f").expect("lower failed");
        let function = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("f"))
            .expect("function missing");

        let body = &function.body.block;
        assert!(matches!(
            body.tail,
            AirTail::Return(Some(Operand::Place(Place {
                projection: ref projections,
                ..
            }))) if matches!(projections.as_slice(), [crate::air::Projection::Field(_)])
        ));
    }

    #[test]
    fn unit_enum_match_lowers_to_pattern_match() {
        let source = r"
            enum Color { Red, Blue }
            fn f(c: Color) -> int {
                match c {
                    Color.Red => 1,
                    Color.Blue => 2,
                }
            }
        ";
        let air = lower_full_core_root(source, "f").expect("lower failed");
        let function = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("f"))
            .expect("function missing");

        let body = &function.body.block;
        assert!(matches!(
            body.stmts.as_slice(),
            [.., AirStmt::PatternMatch(match_)] if match_.arms.len() == 2
        ));
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
                assert_eq!(
                    test_reachable_callable(&functions.items[0]).name(),
                    Ident::new("f")
                );
                assert_eq!(test_reachable_fact(&functions.items[0]).params.len(), 1);
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
        let air =
            lower_checked_roots(&root, &resolved, &semantic, &["main"]).expect("lower failed");

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
                            name: Some("value".to_string()),
                            ty: ExternTypeExpr::Int,
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
            "import ext:host { host_log }; fn f() { host_log(1); }",
            provider,
        );
        let air = lower_checked_roots(&root, &resolved, &semantic, &["f"]).expect("lower failed");

        assert!(
            air.modules[0]
                .functions
                .iter()
                .any(|id| air.function(*id).name == Ident::new("f"))
        );
        let ext = air.modules[1]
            .externs
            .iter()
            .map(|id| air.extern_decl(*id))
            .find(|decl| decl.name == Ident::new("host_log"))
            .expect("host extern lowered");
        let binding = ext.binding.as_ref().expect("provider binding is preserved");
        assert_eq!(binding.provider.name, "host");
        assert_eq!(binding.key.operation, ExternBindingOp::Call);
        let ExternBindingTarget::Function(function) = &binding.key.target else {
            panic!("expected function binding");
        };
        assert_eq!(function.module.segments, ["host"]);
        assert_eq!(function.name, "host_log");
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
                    cx.program.type_arena.get(ext.params[0].ty),
                    Some(TypeData::String)
                ));
                assert!(matches!(
                    cx.program.type_arena.get(ext.return_type),
                    Some(TypeData::Void)
                ));
                assert!(ext.binding.is_none());
                assert_eq!(ext.effects, ExternEffects::default());
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
                AirStmt::Init {
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
    fn reachable_struct_literal_return_value_lowers_to_aggregate() {
        let source =
            "struct S { x: int } fn make() -> S { S { x: 1 } } fn main() { let s = make(); }";
        let air = lower_root(source, "main").expect("lower failed");

        assert!(program_statements(&air).any(|statement| {
            matches!(
                statement,
                AirStmt::Init {
                    value: RValue::Aggregate {
                        kind: AggregateCtor::Struct(_),
                        fields,
                        ..
                    },
                    ..
                } if fields.len() == 1
            )
        }));
    }

    #[test]
    fn aggregate_default_fields_lower_in_declaration_order() {
        let source = "fn one() -> int { 1 } struct Pair { a: int = one(), b: int } fn f() -> Pair { Pair { b: 2 } }";
        let air = lower_root(source, "f").expect("lower failed");
        let function = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("f"))
            .expect("function missing");
        let mut calls_before_aggregate = 0;
        let mut found = false;
        for stmt in &function.body.block.stmts {
            match stmt {
                AirStmt::Init {
                    value: RValue::Call { .. },
                    ..
                } => calls_before_aggregate += 1,
                AirStmt::Init {
                    value:
                        RValue::Aggregate {
                            kind: AggregateCtor::Struct(_),
                            fields,
                            ..
                        },
                    ..
                } => {
                    assert_eq!(calls_before_aggregate, 1);
                    assert_eq!(fields.len(), 2);
                    assert!(matches!(fields[0], Operand::Place(_)));
                    assert!(matches!(fields[1], Operand::Const(_)));
                    found = true;
                }
                _ => {}
            }
        }
        assert!(found);
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
                AirStmt::Init {
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
                AirStmt::Init {
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
            air.type_data(method.signature.return_type()),
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
    fn non_override_to_string_is_not_attached() {
        let source = r#"
            struct Box { value: int }
            extend Box { fn to_string(self) -> string { "box" } }
            fn f(box: Box) -> string { box.to_string() + #stringify(box) }
        "#;
        let air = lower_full_core_root(source, "f").expect("lower failed");

        assert!(
            air.aggregates
                .iter()
                .all(|decl| decl.stringify_override.is_none())
        );
    }

    #[test]
    fn non_to_string_method_is_not_attached() {
        let source = r#"
            struct Box { fn name(self) -> string { "box" } }
            fn f(box: Box) -> string { box.name() + #stringify(box) }
        "#;
        let air = lower_full_core_root(source, "f").expect("lower failed");

        assert!(
            air.aggregates
                .iter()
                .all(|decl| decl.stringify_override.is_none())
        );
    }

    #[test]
    fn string_relational_binary_lowers() {
        let source = r#"fn f() -> bool { "a" < "b" }"#;
        let air = lower_full_core_root(source, "f").expect("lower failed");

        assert!(program_statements(&air).any(|statement| {
            matches!(
                statement,
                AirStmt::Init {
                    value: RValue::Binary {
                        op: BinaryOp::LessThan,
                        ..
                    },
                    ..
                }
            )
        }));
    }

    #[test]
    fn override_body_lowers_calls_defaults_and_stringify() {
        let source = r"
            fn helper(x: int = 1) -> int { x }
            struct Box { value: int fn to_string(self) -> string { #stringify(helper()) } }
            fn f(value: Box) -> string { #stringify(value) }
        ";
        let air = lower_full_core_root(source, "f").expect("lower failed");
        let override_fn = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("to_string"))
            .expect("override missing");

        assert!(function_statements(override_fn).any(|statement| {
            matches!(
                statement,
                AirStmt::Init {
                    value: RValue::Call { args, .. },
                    ..
                } if args.len() == 1
            )
        }));
        assert!(function_statements(override_fn).any(|statement| {
            matches!(
                statement,
                AirStmt::Init {
                    value: RValue::Stringify { .. },
                    ..
                }
            )
        }));
    }

    #[test]
    fn generic_override_body_fact_replay_is_stable() {
        let source = r"
            struct Box<T> { value: T fn to_string(self) -> string { #stringify(1) } }
            fn first(value: Box<int>) -> string { value.to_string() }
            fn second(value: Box<int>) -> string { value.to_string() }
        ";
        let air = lower_roots(source, &["first", "second"]).expect("lower failed");
        let overrides = air
            .functions
            .iter()
            .filter(|function| function.name == Ident::new("to_string"))
            .collect::<Vec<_>>();

        assert_eq!(overrides.len(), 1);
        assert!(!matches!(
            overrides[0].body.block.tail,
            AirTail::Unreachable
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
            overrides
                .iter()
                .all(|function| !matches!(function.body.block.tail, AirTail::Unreachable))
        );
    }

    #[test]
    fn promoted_method_call_projects_receiver() {
        let source = "struct Health { fn damage(self) -> int { 1 } } struct Enemy { embed health: Health } fn f(enemy: Enemy) -> int { enemy.damage() }";
        let air = lower_root(source, "f").expect("promoted method should lower");
        let function = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("f"))
            .expect("missing f");
        let AirStmt::Init {
            value: RValue::Call { args, .. },
            ..
        } = &function.body.block.stmts[0]
        else {
            panic!("expected promoted method call")
        };
        assert!(matches!(
            args.as_slice(),
            [CallArg::Value(Operand::Place(Place { projection, .. }))]
                if projection == &[crate::air::Projection::Field(FieldId::from_index(0))]
        ));
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
        let err = lower_checked_roots(&root, &resolved, &semantic, &["use_it"])
            .expect_err("expected error");

        assert!(matches!(err, LowerError::UnsupportedCallForm { .. }));
    }

    #[test]
    fn runtime_default_arg_lowers_before_call() {
        let source = r#"fn fallback() -> string { "ok" } fn ok(message: string = fallback()) -> string { message } fn f() -> string { ok() }"#;
        let air = lower_root(source, "f").expect("lower failed");
        let function = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("f"))
            .expect("function missing");
        let calls = function
            .body
            .block
            .stmts
            .iter()
            .filter_map(|stmt| match stmt {
                AirStmt::Init {
                    value: RValue::Call { callee, args },
                    ..
                } => Some((callee, args)),
                _ => None,
            })
            .collect::<Vec<_>>();

        assert_eq!(calls.len(), 2);
        assert!(matches!(calls[0], (Callee::Function(_), args) if args.is_empty()));
        assert!(matches!(calls[1], (Callee::Function(_), args) if args.len() == 1));
    }

    #[test]
    fn default_arg_lowers_to_borrowed_literal_const() {
        let source = r#"fn ok(message: string = "ok") {} fn f() { ok(); }"#;
        let air = lower_root(source, "f").expect("lower failed");

        assert!(program_statements(&air).any(|statement| {
            matches!(
                statement,
                AirStmt::Init { value: RValue::Call { args, .. }, .. }
                    | AirStmt::Eval(RValue::Call { args, .. })
                    if args.len() == 1 && matches!(args[0], CallArg::SharedStringConst(_))
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
                extern fn _print_int(value: int);
                pub fn println<T>(value: T) { _print_int(1); }
                ",
            )],
        );
        let air =
            lower_checked_roots(&root, &resolved, &semantic, &["main"]).expect("lower failed");

        assert_eq!(air.externs.len(), 1);
        assert_eq!(air.externs[0].name, Ident::new("_print_int"));
        assert!(program_statements(&air).any(|statement| {
            matches!(
                statement,
                AirStmt::Eval(RValue::Call {
                    callee: Callee::Extern(_),
                    ..
                })
            )
        }));
    }

    #[test]
    fn extern_type_provider_flow_metadata_is_preserved() {
        let provider = ProviderDescriptor {
            provider: ProviderId {
                name: "gfx".to_string(),
            },
            modules: vec![ExternModuleDescriptor {
                path: anvyx_externs::ModulePath {
                    segments: vec!["gfx".to_string()],
                },
                functions: vec![],
                types: vec![anvyx_externs::ExternTypeDescriptor {
                    name: "Sprite".to_string(),
                    doc: None,
                    rep: anvyx_externs::ExternRep::Shared,
                    layout: None,
                    materialization: None,
                    owns_heap_edges: Some(false),
                    fields: vec![anvyx_externs::ExternFieldDescriptor {
                        name: "x".to_string(),
                        ty: ExternTypeExpr::Float,
                        computed: false,
                        readable: true,
                        writable: true,
                        get_receiver: anvyx_externs::ReceiverMode::Shared,
                        set_receiver: anvyx_externs::ReceiverMode::Mutable,
                        doc: None,
                    }],
                    variants: vec![],
                    init: None,
                    methods: vec![anvyx_externs::ExternMethodDescriptor {
                        name: "move".to_string(),
                        doc: None,
                        receiver: anvyx_externs::ReceiverMode::Mutable,
                        signature: ExternSignature {
                            params: vec![ExternParam {
                                name: Some("dx".to_string()),
                                ty: ExternTypeExpr::Float,
                                flow: ParamFlow::Borrow,
                                escape: CallbackEscape::NonEscaping,
                            }],
                            ret: ExternTypeExpr::Void,
                        },
                        effects: ExternEffects::default(),
                    }],
                    statics: vec![anvyx_externs::ExternStaticDescriptor {
                        name: "find".to_string(),
                        doc: None,
                        signature: ExternSignature {
                            params: vec![ExternParam {
                                name: Some("id".to_string()),
                                ty: ExternTypeExpr::Int,
                                flow: ParamFlow::MutBorrow,
                                escape: CallbackEscape::NonEscaping,
                            }],
                            ret: ExternTypeExpr::Void,
                        },
                        effects: ExternEffects::default(),
                    }],
                    operators: vec![anvyx_externs::ExternOperatorDescriptor {
                        op: anvyx_externs::ExternOperator::Binary {
                            op: anvyx_externs::BinaryOp::Add,
                            self_on_right: false,
                        },
                        receiver: anvyx_externs::ReceiverMode::Shared,
                        signature: ExternSignature {
                            params: vec![ExternParam {
                                name: None,
                                ty: ExternTypeExpr::Named {
                                    module: None,
                                    name: "Sprite".to_string(),
                                    args: vec![],
                                },
                                flow: ParamFlow::Borrow,
                                escape: CallbackEscape::NonEscaping,
                            }],
                            ret: ExternTypeExpr::Named {
                                module: None,
                                name: "Sprite".to_string(),
                                args: vec![],
                            },
                        },
                        effects: ExternEffects::default(),
                    }],
                }],
            }],
        };
        let (root, resolved, semantic) = checked_with_provider(
            "import ext:gfx { Sprite }; fn f(sprite: Sprite) { }",
            provider,
        );
        let air = lower_checked_roots(&root, &resolved, &semantic, &["f"]).expect("lower failed");
        let sprite = air.extern_type(crate::air::ExternTypeId::from_index(0));
        assert_eq!(sprite.fields[0].get_receiver.mode, ParamMode::SharedBorrow);
        assert_eq!(sprite.fields[0].set_receiver.mode, ParamMode::MutBorrow);
        assert_eq!(sprite.methods[0].receiver.mode, ParamMode::MutBorrow);
        assert_eq!(sprite.methods[0].params[0].mode, ParamMode::SharedBorrow);
        assert_eq!(sprite.statics[0].params[0].mode, ParamMode::MutBorrow);
        assert_eq!(
            sprite.operators[0].operand.as_ref().unwrap().mode,
            ParamMode::SharedBorrow
        );
    }

    #[test]
    fn extern_mut_borrow_param_flow_is_preserved() {
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
            "import ext:host { touch }; fn f(ref x: int) { touch(x); }",
            provider,
        );
        let air = lower_checked_roots(&root, &resolved, &semantic, &["f"]).expect("lower failed");
        let ext = air.extern_decl(ExternId::from_index(0));
        assert_eq!(ext.params[0].mode, ParamMode::MutBorrow);
        assert!(program_statements(&air).any(|statement| {
            matches!(
                statement,
                AirStmt::Eval(RValue::Call {
                    callee: Callee::Extern(_),
                    args,
                }) if matches!(args.as_slice(), [CallArg::MutBorrow(_)])
            )
        }));
    }

    #[test]
    fn source_ref_call_lowers_to_mut_borrow_arg() {
        let air = lower_root("fn inc(ref x: int) {} fn f() { var x = 1; inc(x); }", "f")
            .expect("lower failed");
        assert!(program_statements(&air).any(|statement| {
            matches!(
                statement,
                AirStmt::Eval(RValue::Call {
                    callee: Callee::Function(_),
                    args,
                }) if matches!(args.as_slice(), [CallArg::MutBorrow(_)])
            )
        }));
    }

    #[test]
    fn mutable_capture_ref_arg_lowers_owner_cell() {
        let source = "fn bump(ref x: int) {} fn f() { var count = 0; let g: fn() = || { count = 1; }; bump(count); g; }";
        let air = lower_root(source, "f").expect("lower failed");
        let cell = CaptureCellId::from_index(0);

        let function = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("f"))
            .expect("missing f");

        assert!(function_statements(function).any(|statement| {
            matches!(
                statement,
                AirStmt::Eval(RValue::Call {
                    args,
                    ..
                }) if matches!(args.as_slice(), [CallArg::MutBorrow(place)] if place.root == PlaceRoot::CaptureCell(cell) && place.projection.is_empty())
            )
        }));
    }

    #[test]
    fn projected_capture_ref_arg_lowers_owner_cell_projection() {
        let source = "struct Point { x: int } fn bump(ref x: int) {} fn f() { var p = Point { x: 0 }; let g: fn() = || { bump(p.x); }; g; }";
        let air = lower_root(source, "f").expect("lower failed");
        let body = air.lambdas[0].body;

        assert!(function_statements(air.function(body)).any(|statement| {
            matches!(
                statement,
                AirStmt::Eval(RValue::Call { args, .. })
                    if matches!(args.as_slice(), [CallArg::MutBorrow(place)] if matches!(place.root, PlaceRoot::LambdaCapture(_)) && !place.projection.is_empty())
            )
        }));
    }

    #[test]
    fn source_valid_read_ref_arg_lowers_read_to_value_snapshot() {
        let air = lower_root(
            r#"
fn both(read: string, ref write: string) {
    #stringify(read);
    write = "changed";
}
fn f() {
    var text = "hello";
    both(text, text);
}
"#,
            "f",
        )
        .expect("lower failed");
        assert!(program_statements(&air).any(|statement| {
            matches!(
                statement,
                AirStmt::Eval(RValue::Call {
                    callee: Callee::Function(_),
                    args,
                }) if matches!(args.as_slice(), [CallArg::Value(Operand::Place(_)), CallArg::MutBorrow(_)])
            )
        }));
    }

    #[test]
    fn extern_computed_getter_uses_method_receiver_call_arg() {
        let provider = ProviderDescriptor {
            provider: ProviderId {
                name: "gfx".to_string(),
            },
            modules: vec![ExternModuleDescriptor {
                path: anvyx_externs::ModulePath {
                    segments: vec!["gfx".to_string()],
                },
                functions: vec![ExternFunctionDescriptor {
                    name: "make".to_string(),
                    doc: None,
                    signature: ExternSignature {
                        params: vec![],
                        ret: ExternTypeExpr::Named {
                            module: None,
                            name: "Sprite".to_string(),
                            args: vec![],
                        },
                    },
                    effects: ExternEffects::default(),
                }],
                types: vec![anvyx_externs::ExternTypeDescriptor {
                    name: "Sprite".to_string(),
                    doc: None,
                    rep: anvyx_externs::ExternRep::Shared,
                    layout: None,
                    materialization: None,
                    owns_heap_edges: Some(false),
                    fields: vec![anvyx_externs::ExternFieldDescriptor {
                        name: "score".to_string(),
                        ty: ExternTypeExpr::Int,
                        computed: true,
                        readable: true,
                        writable: false,
                        get_receiver: anvyx_externs::ReceiverMode::Shared,
                        set_receiver: anvyx_externs::ReceiverMode::Mutable,
                        doc: None,
                    }],
                    variants: vec![],
                    init: None,
                    methods: vec![anvyx_externs::ExternMethodDescriptor {
                        name: "score_value".to_string(),
                        doc: None,
                        receiver: anvyx_externs::ReceiverMode::Shared,
                        signature: ExternSignature {
                            params: vec![],
                            ret: ExternTypeExpr::Int,
                        },
                        effects: ExternEffects::default(),
                    }],
                    statics: vec![],
                    operators: vec![],
                }],
            }],
        };
        let (root, resolved, semantic) = checked_with_provider(
            "import ext:gfx { Sprite, make }; fn f() { let sprite = make(); let a = sprite.score; let b = sprite.score_value(); }",
            provider,
        );
        let air = lower_checked_roots(&root, &resolved, &semantic, &["f"]).expect("lower failed");
        let calls = program_statements(&air)
            .filter_map(|statement| match statement {
                AirStmt::Init {
                    value:
                        RValue::Call {
                            callee: Callee::Extern(_),
                            args,
                        },
                    ..
                } if !args.is_empty() => Some(matches!(
                    args.as_slice(),
                    [CallArg::SharedBorrow(Place {
                        root: PlaceRoot::Local(_),
                        ..
                    })]
                )),
                _ => None,
            })
            .collect::<Vec<_>>();

        assert_eq!(calls, [true, true]);
    }

    #[test]
    fn source_shared_field_call_lowers_to_shared_borrow_projection() {
        let air = lower_root(
            "struct Named { name: string } fn take(name: string) {} fn f(item: Named) { take(item.name); }",
            "f",
        )
        .expect("lower failed");
        assert!(program_statements(&air).any(|statement| {
            matches!(
                statement,
                AirStmt::Eval(RValue::Call {
                    callee: Callee::Function(_),
                    args,
                }) if matches!(args.as_slice(), [CallArg::SharedBorrow(place)] if matches!(place.projection.as_slice(), [crate::air::Projection::Field(_)]))
            )
        }));
    }

    #[test]
    fn source_shared_index_call_lowers_to_shared_borrow_projection() {
        let air = lower_root(
            "fn take(name: string) {} fn f(names: [string]) { take(names[0]); }",
            "f",
        )
        .expect("lower failed");
        assert!(program_statements(&air).any(|statement| {
            matches!(
                statement,
                AirStmt::Eval(RValue::Call {
                    callee: Callee::Function(_),
                    args,
                }) if matches!(args.as_slice(), [CallArg::SharedBorrow(place)] if matches!(place.projection.as_slice(), [crate::air::Projection::Index(_)]))
            )
        }));
    }

    #[test]
    fn source_ref_field_call_lowers_to_mut_borrow_projection() {
        let air = lower_root(
            "struct Pair { a: int, b: int } fn set(ref x: int) {} fn f(ref p: Pair) { set(p.a); }",
            "f",
        )
        .expect("lower failed");
        assert!(program_statements(&air).any(|statement| {
            matches!(
                statement,
                AirStmt::Eval(RValue::Call {
                    callee: Callee::Function(_),
                    args,
                }) if matches!(args.as_slice(), [CallArg::MutBorrow(place)] if matches!(place.projection.as_slice(), [crate::air::Projection::Field(_)]))
            )
        }));
    }

    #[test]
    fn source_ref_tuple_field_lowers_to_mut_borrow_projection() {
        let air = lower_root(
            "fn set(ref x: int) {} fn f(ref pair: (int, int)) { set(pair.0); }",
            "f",
        )
        .expect("lower failed");
        assert!(program_statements(&air).any(|statement| {
            matches!(
                statement,
                AirStmt::Eval(RValue::Call {
                    callee: Callee::Function(_),
                    args,
                }) if matches!(args.as_slice(), [CallArg::MutBorrow(place)] if matches!(place.projection.as_slice(), [crate::air::Projection::TupleField(0)]))
            )
        }));
    }

    #[test]
    fn source_ref_index_lowers_to_mut_borrow_projection() {
        let air = lower_root(
            "fn set(ref x: int) {} fn f(ref xs: [int]) { set(xs[0]); }",
            "f",
        )
        .expect("lower failed");
        assert!(program_statements(&air).any(|statement| {
            matches!(
                statement,
                AirStmt::Eval(RValue::Call {
                    callee: Callee::Function(_),
                    args,
                }) if matches!(args.as_slice(), [CallArg::MutBorrow(place)] if matches!(place.projection.as_slice(), [crate::air::Projection::Index(_)]))
            )
        }));
    }

    #[test]
    fn map_index_assignment_lowers_to_indexed_insert() {
        let air = lower_root(
            "fn f(ref counts: [string: int]) { counts[\"a\"] = 1; }",
            "f",
        )
        .expect("lower failed");
        assert!(program_statements(&air).any(|statement| {
            matches!(
                statement,
                AirStmt::Eval(RValue::MapInsert {
                    kind: MapWriteKind::IndexedAssignment,
                    ..
                })
            )
        }));
        assert!(
            !program_statements(&air)
                .any(|statement| { matches!(statement, AirStmt::Assign { .. }) })
        );
    }

    #[test]
    fn concrete_function_lowers_to_verified_body() {
        let air = lower_root("fn f(ref a: int) -> int { a }", "f").expect("lower failed");
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
        let body = &function.body.block;
        assert!(matches!(body.tail, AirTail::Return(Some(_))));
    }

    #[test]
    fn empty_void_function_lowers_to_fallthrough_return() {
        let air = lower_root("fn f() {}", "f").expect("lower failed");
        let body = &air.functions[0].body.block;
        assert!(matches!(body.tail, AirTail::Return(None)));
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
        assert!(function_statements(function).any(|stmt| matches!(stmt, AirStmt::Assign { .. })));
        let body = &function.body.block;
        assert!(matches!(body.tail, AirTail::Return(Some(_))));
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
        let body = &function.body.block;
        assert!(matches!(body.tail, AirTail::Return(Some(_))));
    }

    #[test]
    fn string_concat_with_struct_lowers_side_through_stringify() {
        let source = r#"struct S {} fn f(s: S) -> string { "s: " + s }"#;
        let air = lower_full_core_root(source, "f").expect("lower failed");

        assert!(program_statements(&air).any(|statement| matches!(
            statement,
            AirStmt::Init {
                value: RValue::Stringify { .. },
                ..
            }
        )));
        assert!(program_statements(&air).any(|statement| {
            matches!(statement, AirStmt::Init { value: RValue::StringConcat { parts }, .. } if parts.len() == 2)
        }));
    }

    #[test]
    fn string_add_assign_lowers_to_concat_rvalue() {
        let source = r#"fn f() { var s = "count: "; s += 1; }"#;
        let air = lower_root(source, "f").expect("lower failed");

        assert!(program_statements(&air).any(|statement| {
            matches!(statement, AirStmt::Assign { value: RValue::StringConcat { parts }, .. } if parts.len() == 2)
        }));
    }

    #[test]
    fn if_statement_lowers_to_structured_branch() {
        let source = r"fn f(x: bool) -> int { var y = 0; if x { y = 1; } else { y = 2; } y }";
        let air = lower_root(source, "f").expect("lower failed");
        let function = &air.functions[0];
        let body = &function.body.block;

        assert!(matches!(body.stmts[1], AirStmt::If(_)));
        let AirStmt::If(branch) = &body.stmts[1] else {
            unreachable!()
        };
        assert!(branch.else_block.is_some());
        assert!(matches!(body.tail, AirTail::Return(Some(_))));
    }

    #[test]
    fn if_expression_lowers_to_branch_result_temp() {
        let source = r"fn f(x: bool) -> int { if x { 1 } else { 2 } }";
        let air = lower_root(source, "f").expect("lower failed");
        let function = &air.functions[0];

        let body = &function.body.block;
        let AirStmt::If(branch) = &body.stmts[0] else {
            panic!("missing branch")
        };
        assert!(matches!(branch.then_block.stmts[0], AirStmt::Init { .. }));
        assert!(matches!(
            branch.else_block.as_ref().unwrap().stmts[0],
            AirStmt::Init { .. }
        ));
    }

    #[test]
    fn nested_if_lowers_structurally() {
        let source = r"fn f(a: bool, b: bool) -> int { if a { if b { 1 } else { 2 } } else { 3 } }";
        let air = lower_root(source, "f").expect("lower failed");
        let function = &air.functions[0];
        let body = &function.body.block;
        let AirStmt::If(outer) = &body.stmts[0] else {
            panic!("missing outer branch")
        };

        assert!(
            outer
                .then_block
                .stmts
                .iter()
                .any(|stmt| matches!(stmt, AirStmt::If(_)))
        );
    }

    #[test]
    fn if_branch_early_return_lowers_with_reachable_join() {
        let source = r"fn f(x: bool) -> int { if x { return 1; } 2 }";
        let air = lower_root(source, "f").expect("lower failed");
        let function = &air.functions[0];

        let body = &function.body.block;
        let AirStmt::If(branch) = &body.stmts[0] else {
            panic!("missing branch")
        };
        assert!(matches!(branch.then_block.tail, AirTail::Return(Some(_))));
        assert!(branch.else_block.is_none());
        assert!(matches!(body.tail, AirTail::Return(Some(_))));
    }

    #[test]
    fn if_both_branches_return_lowers_without_join() {
        let source = r"fn f(x: bool) -> int { if x { return 1; } else { return 2; } }";
        let air = lower_root(source, "f").expect("lower failed");
        let function = &air.functions[0];

        let body = &function.body.block;
        let AirStmt::If(branch) = &body.stmts[0] else {
            panic!("missing branch")
        };
        assert!(matches!(branch.then_block.tail, AirTail::Return(Some(_))));
        assert!(matches!(
            branch.else_block.as_ref().unwrap().tail,
            AirTail::Return(Some(_))
        ));
        assert!(matches!(body.tail, AirTail::Unreachable));
    }

    #[test]
    fn string_interpolation_lowers_default_and_explicit_parts() {
        let source = r#"struct S {} fn f(s: S, x: int) -> string { f"value {s} {x:04}" }"#;
        let air = lower_full_core_root(source, "f").expect("lower failed");

        assert!(program_statements(&air).any(|statement| matches!(
            statement,
            AirStmt::Init {
                value: RValue::Stringify { .. },
                ..
            }
        )));
        assert!(program_statements(&air).any(|statement| matches!(
            statement,
            AirStmt::Init {
                value: RValue::Format { .. },
                ..
            }
        )));
        assert!(program_statements(&air).any(|statement| {
            matches!(
                statement,
                AirStmt::Init {
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
            matches!(statement, AirStmt::Init { value: RValue::StringConcat { parts }, .. } if parts.len() == 2)
        }));
    }

    #[test]
    fn string_concat_chain_lowers_to_one_ordered_concat() {
        let source = r#"fn f(x: int) -> string { "a" + x + "b" }"#;
        let air = lower_root(source, "f").expect("lower failed");

        assert!(program_statements(&air).any(|statement| {
            matches!(statement, AirStmt::Init { value: RValue::StringConcat { parts }, .. } if parts.len() == 3)
        }));
    }

    #[test]
    fn unused_top_level_declarations_emit_nothing() {
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
    fn cross_source_default_function_value_uses_declaration_source() {
        let (root, resolved, semantic) = checked_with_modules(
            "import util { invoke }; fn main() -> int { invoke() }",
            &[(
                "util",
                "fn fallback() -> int { 7 }
                 pub fn invoke(callback: fn() -> int = fallback) -> int { callback() }",
            )],
        );
        let air = lower_checked_roots(&root, &resolved, &semantic, &["main"])
            .expect("cross-source default function value failed to lower");

        for name in ["invoke", "fallback"] {
            assert!(
                air.functions
                    .iter()
                    .any(|function| { function.name == Ident::new(name) })
            );
        }
    }

    #[test]
    fn named_function_value_lowers_to_ref_and_reaches_body() {
        let source = "fn g() -> int { 1 } fn f() -> void { g; }";
        let (root, resolved, semantic) = checked(source);
        let air = lower_checked_roots(&root, &resolved, &semantic, &["f"]).expect("lower failed");

        assert_eq!(function_names(&air), vec!["f", "g"]);
        let f = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("f"))
            .expect("missing f");
        assert!(function_statements(f).any(|statement| {
            matches!(
                statement,
                AirStmt::Init {
                    value: RValue::FunctionRef { .. },
                    ..
                }
            )
        }));
    }

    #[test]
    fn named_function_alias_uses_local_storage() {
        let source = "fn g() -> int { 1 } fn f() -> int { let h = g; h; 0 }";
        let air = lower_root(source, "f").expect("lower failed");
        let f = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("f"))
            .expect("missing f");

        assert!(function_statements(f).any(|statement| {
            matches!(
                statement,
                AirStmt::Init {
                    value: RValue::FunctionRef { .. },
                    ..
                }
            )
        }));
        assert!(function_statements(f).any(|statement| {
            matches!(statement, AirStmt::Eval(RValue::Use(Operand::Place(place))) if matches!(air.type_data(place.ty), TypeData::Function(_)))
        }));
    }

    #[test]
    fn local_facts_preserve_shadowed_binding_ids() {
        let source = "fn f() { let x = 1; if true { let x = 2; x; } x; }";
        let (_, _, semantic) = checked(source);
        let body = BodyInstanceKey::Callable(root_function("f"));
        let facts = semantic
            .program
            .facts
            .body(&body)
            .expect("body facts missing");
        let x_bindings = facts
            .locals
            .defs
            .values()
            .filter(|fact| fact.name.as_str() == "x")
            .map(|fact| {
                fact.binding_id
                    .expect("binding local should carry BindingId")
            })
            .collect::<HashSet<_>>();

        assert_eq!(x_bindings.len(), 2);
    }

    #[test]
    fn lambda_capture_binding_has_lowered_local_bridge() {
        let source = "fn f() { let x = 1; || { x; }; }";
        let (_, _, semantic) = checked(source);
        let capture = semantic
            .public_facts
            .lambda_captures()
            .values()
            .next()
            .expect("lambda capture fact missing");
        let body = BodyInstanceKey::Callable(root_function("f"));
        let facts = semantic
            .program
            .facts
            .body(&body)
            .expect("body facts missing");

        assert!(
            facts
                .locals
                .defs
                .values()
                .any(|fact| fact.binding_id == Some(capture.binding_id))
        );
    }

    #[test]
    fn readonly_lambda_capture_lowers_to_slot() {
        let source = "fn f(seed: int) { let x = seed + 1; let g: fn() = || { x; }; g; }";
        let air = lower_root(source, "f").expect("lower failed");
        let lambda = air.lambdas.first().expect("missing lambda");

        assert!(matches!(
            &lambda.captures[..],
            [LambdaCaptureDecl::ReadonlyLocal { .. }]
        ));
        assert!(program_statements(&air).any(|statement| {
            matches!(statement, AirStmt::Init { value: RValue::MakeLambda { captures, .. }, .. }
                if matches!(&captures[..], [LambdaCaptureArg::ReadonlyLocal { .. }]))
        }));
        assert!(
            function_statements(air.function(lambda.body)).any(|statement| {
                matches!(statement, AirStmt::Eval(RValue::Use(Operand::Place(place)))
                if matches!(place.root, PlaceRoot::LambdaCapture(_)))
            })
        );
    }

    #[test]
    fn escaping_readonly_lambda_capture_lowers_to_slot() {
        for (source, root) in [
            (
                "fn make(seed: int) -> fn() { let x = seed + 1; || { x; } }",
                "make",
            ),
            ("fn make(seed: int) -> fn() { || { seed; } }", "make"),
            (
                "fn later(f: escaping fn()) {} fn main(seed: int) { let x = seed + 1; later(|| { x; }); }",
                "main",
            ),
        ] {
            let air = lower_root(source, root).expect("lower failed");
            let lambda = air.lambdas.first().expect("missing lambda");

            assert_eq!(lambda.escape, LambdaEscape::Escaping);
            assert!(matches!(
                &lambda.captures[..],
                [LambdaCaptureDecl::ReadonlyLocal { .. }]
            ));
            assert!(program_statements(&air).any(|statement| {
                matches!(statement, AirStmt::Init { value: RValue::MakeLambda { captures, .. }, .. }
                    if matches!(&captures[..], [LambdaCaptureArg::ReadonlyLocal { .. }]))
            }));
        }
    }

    #[test]
    fn escaping_no_runtime_capture_kind_stays_no_runtime() {
        let kind = lowered_capture_kind(ExprId(0), LambdaCaptureRuntimePlan::NoRuntime)
            .expect("capture kind failed");

        assert!(matches!(kind, LoweredCaptureKind::NoRuntime));
    }

    #[test]
    fn nested_escaping_readonly_capture_forwards_parent_slot() {
        let source = "fn later(f: escaping fn()) {} fn main(seed: int) { let x = seed + 1; let outer: fn() = || { later(|| { x; }); }; outer(); }";
        let air = lower_root(source, "main").expect("lower failed");
        let outer = air
            .lambdas
            .iter()
            .find(|decl| air.function(decl.owner).name == Ident::new("main"))
            .expect("missing outer lambda");
        let inner = air
            .lambdas
            .iter()
            .find(|decl| decl.owner == outer.body)
            .expect("missing inner lambda");

        assert_eq!(inner.escape, LambdaEscape::Escaping);
        assert!(matches!(
            &inner.captures[..],
            [LambdaCaptureDecl::ReadonlyLocal { .. }]
        ));
        assert!(
            function_statements(air.function(outer.body)).any(|statement| {
                matches!(statement, AirStmt::Init { value: RValue::MakeLambda { captures, .. }, .. }
                if matches!(&captures[..], [LambdaCaptureArg::ReadonlyLocal {
                    value: Operand::Place(place)
                }] if matches!(place.root, PlaceRoot::LambdaCapture(_))))
            })
        );
    }

    #[test]
    fn returned_zero_env_lambda_lowers_without_captures() {
        let source = "fn make() -> fn() { || {} }";
        let air = lower_root(source, "make").expect("lower failed");
        let lambda = air.lambdas.first().expect("missing lambda");

        assert_eq!(lambda.escape, LambdaEscape::Escaping);
        assert!(lambda.captures.is_empty());
        assert!(program_statements(&air).any(|statement| {
            matches!(statement, AirStmt::Init { value: RValue::MakeLambda { captures, .. }, .. }
                if captures.is_empty())
        }));
    }

    #[test]
    fn mutable_lambda_capture_lowers_to_capture_cell() {
        let source = "fn f(seed: int) { var x = seed; let g: fn() = || { x = x + 1; }; g; }";
        let air = lower_root(source, "f").expect("lower failed");
        let lambda = air.lambdas.first().expect("missing lambda");
        let cell = CaptureCellId::from_index(0);
        let source_local = air.capture_cells[cell.index()].source_local;

        assert_eq!(
            air.capture_cells[cell.index()].lifetime,
            CaptureCellLifetime::Function
        );
        assert!(matches!(
            &lambda.captures[..],
            [LambdaCaptureDecl::CaptureCell { cell: captured, .. }] if *captured == cell
        ));
        assert!(program_statements(&air).any(|statement| {
            matches!(statement, AirStmt::Init { value: RValue::MakeLambda { captures, .. }, .. }
                if matches!(&captures[..], [LambdaCaptureArg::CaptureCell { cell: captured }] if *captured == cell))
        }));
        assert!(program_statements(&air).any(|statement| {
            matches!(statement, AirStmt::Assign { dst, .. }
                if dst.root == PlaceRoot::CaptureCell(cell))
        }));
        assert!(!program_statements(&air).any(|statement| {
            matches!(statement, AirStmt::Init { local, .. } if local == source_local)
        }));
        assert!(
            function_statements(air.function(lambda.body)).any(|statement| {
                matches!(statement, AirStmt::Assign { dst, .. }
                if matches!(dst.root, PlaceRoot::LambdaCapture(_)))
            })
        );
    }

    #[test]
    fn loop_local_mutable_capture_cell_records_loop_lifetime() {
        let source = "fn f(seed: int) { while seed < 1 { var x = seed; let g = || { x = x + 1; }; g(); break; } }";
        let air = lower_root(source, "f").expect("lower failed");

        assert!(matches!(
            air.capture_cells[0].lifetime,
            CaptureCellLifetime::Loop {
                loop_id: AirLoopId(0)
            }
        ));
    }

    #[test]
    fn two_mutable_lambdas_share_one_capture_cell() {
        let source = "fn f(seed: int) { var x = seed; let a: fn() = || { x = 1; }; let b: fn() = || { x = 2; }; a; b; }";
        let air = lower_root(source, "f").expect("lower failed");
        let cell = CaptureCellId::from_index(0);

        assert_eq!(air.capture_cells.len(), 1);
        assert_eq!(
            air.lambdas
                .iter()
                .filter(|lambda| matches!(
                    &lambda.captures[..],
                    [LambdaCaptureDecl::CaptureCell { cell: captured, .. }] if *captured == cell
                ))
                .count(),
            2
        );
        assert_eq!(
            program_statements(&air)
                .filter(|statement| matches!(
                    statement,
                    AirStmt::Init { value: RValue::MakeLambda { captures, .. }, .. }
                        if matches!(&captures[..], [LambdaCaptureArg::CaptureCell { cell: captured }] if *captured == cell)
                ))
                .count(),
            2
        );
    }

    #[test]
    fn shadowed_mutable_captures_use_distinct_capture_cells() {
        let source = "fn f() { var x = 1; let a: fn() = || { x = 2; }; { var x = 3; let b: fn() = || { x = 4; }; } }";
        let air = lower_root(source, "f").expect("lower failed");

        assert_eq!(air.capture_cells.len(), 2);
        assert_ne!(
            air.capture_cells[0].source_local,
            air.capture_cells[1].source_local
        );
        assert!(air.lambdas.iter().any(|lambda| matches!(
            &lambda.captures[..],
            [LambdaCaptureDecl::CaptureCell { cell, .. }] if *cell == CaptureCellId::from_index(0)
        )));
        assert!(air.lambdas.iter().any(|lambda| matches!(
            &lambda.captures[..],
            [LambdaCaptureDecl::CaptureCell { cell, .. }] if *cell == CaptureCellId::from_index(1)
        )));
    }

    #[test]
    fn mutable_reader_and_writer_share_one_capture_cell() {
        let source = "fn f(seed: int) { var x = seed; let w: fn() = || { x = 1; }; let r: fn() = || { x; }; w; r; }";
        let air = lower_root(source, "f").expect("lower failed");
        let cell = CaptureCellId::from_index(0);

        assert_eq!(air.capture_cells.len(), 1);
        assert!(air.lambdas.iter().all(|lambda| matches!(
            &lambda.captures[..],
            [LambdaCaptureDecl::CaptureCell { cell: captured, .. }] if *captured == cell
        )));
    }

    #[test]
    fn nested_mutable_capture_forwards_parent_capture_cell() {
        let source = "fn f(seed: int) { var x = seed; let outer: fn() = || { let inner: fn() = || { x = 1; }; inner; }; outer; }";
        let air = lower_root(source, "f").expect("lower failed");
        let cell = CaptureCellId::from_index(0);
        let outer = air
            .lambdas
            .iter()
            .find(|decl| air.function(decl.owner).name == Ident::new("f"))
            .expect("missing outer lambda");
        let inner = air
            .lambdas
            .iter()
            .find(|decl| decl.owner == outer.body)
            .expect("missing inner lambda");

        assert_eq!(air.capture_cells.len(), 1);
        assert!(matches!(
            &outer.captures[..],
            [LambdaCaptureDecl::CaptureCell { cell: captured, .. }] if *captured == cell
        ));
        assert!(matches!(
            &inner.captures[..],
            [LambdaCaptureDecl::CaptureCell { cell: captured, .. }] if *captured == cell
        ));
    }

    #[test]
    fn owner_write_after_lambda_creation_uses_capture_cell() {
        let source = "fn f(seed: int) { var x = seed; let g: fn() = || { x = 1; }; x = 2; g; }";
        let air = lower_root(source, "f").expect("lower failed");
        let cell = CaptureCellId::from_index(0);
        let statements = program_statements(&air).collect::<Vec<_>>();
        let lambda_index = statements
            .iter()
            .position(|statement| {
                matches!(
                    statement,
                    AirStmt::Init {
                        value: RValue::MakeLambda { .. },
                        ..
                    }
                )
            })
            .expect("missing lambda init");

        assert!(statements[lambda_index + 1..].iter().any(|statement| {
            matches!(statement, AirStmt::Assign { dst, .. } if dst.root == PlaceRoot::CaptureCell(cell))
        }));
    }

    #[test]
    fn projected_mutable_capture_assignment_lowers_to_capture_cell_projection() {
        let source = "struct Point { x: int } fn f() { var p = Point { x: 0 }; let g: fn() = || { p.x = 1; }; g; }";
        let air = lower_root(source, "f").expect("lower failed");
        let body = air.lambdas[0].body;

        assert!(function_statements(air.function(body)).any(|statement| {
            matches!(statement, AirStmt::Assign { dst, .. } if matches!(dst.root, PlaceRoot::LambdaCapture(_)) && !dst.projection.is_empty())
        }));
    }

    #[test]
    fn mutable_source_lambdas_do_not_lower_to_scoped_local() {
        let source = "fn f(seed: int) { var x = seed; let g: fn() = || { x = 1; }; g; }";
        let air = lower_root(source, "f").expect("lower failed");

        assert!(!air.lambdas.iter().any(|lambda| {
            lambda
                .captures
                .iter()
                .any(|capture| matches!(capture, LambdaCaptureDecl::ScopedLocal { .. }))
        }));
        assert!(!program_statements(&air).any(|statement| {
            matches!(statement, AirStmt::Init { value: RValue::MakeLambda { captures, .. }, .. }
                if captures.iter().any(|capture| matches!(capture, LambdaCaptureArg::ScopedLocal { .. })))
        }));
    }

    #[test]
    fn escaping_mutable_counter_lowers_to_capture_cell() {
        let source =
            "fn make_counter() -> fn() -> int { var count = 0; || { count = count + 1; count } }";
        let air = lower_root(source, "make_counter").expect("lower failed");
        let cell = CaptureCellId::from_index(0);
        let lambda = air.lambdas.first().expect("missing lambda");

        assert_eq!(air.capture_cells.len(), 1);
        assert_eq!(lambda.escape, LambdaEscape::Escaping);
        assert!(matches!(
            &lambda.captures[..],
            [LambdaCaptureDecl::CaptureCell { cell: captured, .. }] if *captured == cell
        ));
        assert!(program_statements(&air).any(|statement| {
            matches!(statement, AirStmt::Init { value: RValue::MakeLambda { captures, .. }, .. }
                if matches!(&captures[..], [LambdaCaptureArg::CaptureCell { cell: captured }] if *captured == cell))
        }));
        assert!(
            function_statements(air.function(lambda.body)).any(|statement| {
                matches!(statement, AirStmt::Assign { dst, .. }
                    if dst.root == PlaceRoot::LambdaCapture(LambdaCaptureSlotId::from_index(0)))
            })
        );
    }

    #[test]
    fn escaping_mutable_branch_lambdas_share_one_capture_cell() {
        let source = "fn make(cond: bool) -> fn() -> int { var count = 0; let inc = || { count = count + 1; count }; let get = || count; if cond { inc } else { get } }";
        let air = lower_root(source, "make").expect("lower failed");
        let cell = CaptureCellId::from_index(0);

        assert_eq!(air.capture_cells.len(), 1);
        assert_eq!(
            air.lambdas
                .iter()
                .filter(|lambda| lambda.escape == LambdaEscape::Escaping)
                .filter(|lambda| matches!(
                    &lambda.captures[..],
                    [LambdaCaptureDecl::CaptureCell { cell: captured, .. }] if *captured == cell
                ))
                .count(),
            2
        );
    }

    #[test]
    fn escaping_owner_write_after_lambda_creation_uses_capture_cell() {
        let source = "fn make(seed: int) -> fn() -> int { var x = seed; let g = || x; x = 2; g }";
        let air = lower_root(source, "make").expect("lower failed");
        let cell = CaptureCellId::from_index(0);
        let owner = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("make"))
            .expect("missing owner");
        let statements = function_statements(owner).collect::<Vec<_>>();
        let lambda_index = statements
            .iter()
            .position(|statement| {
                matches!(
                    statement,
                    AirStmt::Init {
                        value: RValue::MakeLambda { .. },
                        ..
                    }
                )
            })
            .expect("missing lambda init");

        assert!(statements[lambda_index + 1..].iter().any(|statement| {
            matches!(statement, AirStmt::Assign { dst, .. } if dst.root == PlaceRoot::CaptureCell(cell))
        }));
    }

    #[test]
    fn escaping_lambda_write_then_owner_read_uses_capture_cell() {
        let source = "fn keep(f: escaping fn()) {} fn f(seed: int) -> int { var x = seed; let g = || { x = 2; }; g(); keep(g); x }";
        let air = lower_root(source, "f").expect("lower failed");
        let cell = CaptureCellId::from_index(0);
        let owner = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("f"))
            .expect("missing owner");
        let lambda = air.lambdas.first().expect("missing lambda");

        assert_eq!(lambda.escape, LambdaEscape::Escaping);
        assert!(matches!(
            &owner.body.block.tail,
            AirTail::Return(Some(Operand::Place(place))) if place.root == PlaceRoot::CaptureCell(cell)
        ));
    }

    #[test]
    fn nested_escaping_mutable_capture_forwards_capture_cell() {
        let source = "fn make() -> fn() -> fn() -> int { var count = 0; || { || { count = count + 1; count } } }";
        let air = lower_root(source, "make").expect("lower failed");
        let cell = CaptureCellId::from_index(0);
        let outer = air
            .lambdas
            .iter()
            .find(|decl| air.function(decl.owner).name == Ident::new("make"))
            .expect("missing outer lambda");
        let inner = air
            .lambdas
            .iter()
            .find(|decl| decl.owner == outer.body)
            .expect("missing inner lambda");

        assert_eq!(air.capture_cells.len(), 1);
        assert!(matches!(
            &outer.captures[..],
            [LambdaCaptureDecl::CaptureCell { cell: captured, .. }] if *captured == cell
        ));
        assert!(matches!(
            &inner.captures[..],
            [LambdaCaptureDecl::CaptureCell { cell: captured, .. }] if *captured == cell
        ));
        assert!(
            function_statements(air.function(outer.body)).any(|statement| {
                matches!(statement, AirStmt::Init { value: RValue::MakeLambda { captures, .. }, .. }
                    if matches!(&captures[..], [LambdaCaptureArg::CaptureCell { cell: captured }] if *captured == cell))
            })
        );
    }

    #[test]
    fn escaping_shadowed_mutable_captures_use_distinct_cells() {
        let source = "fn make(cond: bool) -> fn() -> int { if cond { var x = 1; || { x = 2; x } } else { var x = 3; || { x = 4; x } } }";
        let air = lower_root(source, "make").expect("lower failed");

        assert_eq!(air.capture_cells.len(), 2);
        assert_ne!(
            air.capture_cells[0].source_local,
            air.capture_cells[1].source_local
        );
        assert!(air.lambdas.iter().any(|lambda| matches!(
            &lambda.captures[..],
            [LambdaCaptureDecl::CaptureCell { cell, .. }] if *cell == CaptureCellId::from_index(0)
        )));
        assert!(air.lambdas.iter().any(|lambda| matches!(
            &lambda.captures[..],
            [LambdaCaptureDecl::CaptureCell { cell, .. }] if *cell == CaptureCellId::from_index(1)
        )));
    }

    #[test]
    fn escaping_readonly_capture_does_not_create_cell() {
        let source = "fn make(seed: int) -> fn() -> int { var count = 0; let bonus = seed; || { count = count + 1; count + bonus } }";
        let air = lower_root(source, "make").expect("lower failed");
        let lambda = air.lambdas.first().expect("missing lambda");

        assert_eq!(air.capture_cells.len(), 1);
        assert_eq!(lambda.captures.len(), 2);
        assert!(
            lambda
                .captures
                .iter()
                .any(|capture| matches!(capture, LambdaCaptureDecl::CaptureCell { .. }))
        );
        assert!(
            lambda
                .captures
                .iter()
                .any(|capture| matches!(capture, LambdaCaptureDecl::ReadonlyLocal { .. }))
        );
    }

    #[test]
    fn escaping_projected_mutable_capture_assignment_lowers_to_capture_cell_projection() {
        let source =
            "struct Point { x: int } fn make() -> fn() { var p = Point { x: 0 }; || { p.x = 1; } }";
        let air = lower_root(source, "make").expect("lower failed");
        let body = air.lambdas[0].body;

        assert!(function_statements(air.function(body)).any(|statement| {
            matches!(statement, AirStmt::Assign { dst, .. } if matches!(dst.root, PlaceRoot::LambdaCapture(_)) && !dst.projection.is_empty())
        }));
    }

    #[test]
    fn nested_lambda_capture_lowers_through_parent_slot() {
        let source = "fn f(seed: int) { let x = seed + 1; let outer: fn() = || { let inner: fn() = || { x; }; inner; }; outer; }";
        let air = lower_root(source, "f").expect("lower failed");
        let outer = air
            .lambdas
            .iter()
            .find(|decl| air.function(decl.owner).name == Ident::new("f"))
            .expect("missing outer lambda");
        let inner = air
            .lambdas
            .iter()
            .find(|decl| decl.owner == outer.body)
            .expect("missing inner lambda");

        assert!(matches!(
            &outer.captures[..],
            [LambdaCaptureDecl::ReadonlyLocal { .. }]
        ));
        assert!(matches!(
            &inner.captures[..],
            [LambdaCaptureDecl::ReadonlyLocal { .. }]
        ));
        assert!(
            function_statements(air.function(outer.body)).any(|statement| {
                matches!(statement, AirStmt::Init { value: RValue::MakeLambda { captures, .. }, .. }
                if matches!(&captures[..], [LambdaCaptureArg::ReadonlyLocal {
                    value: Operand::Place(place)
                }] if matches!(place.root, PlaceRoot::LambdaCapture(_))))
            })
        );
    }

    #[test]
    fn captured_iife_uses_same_capture_slots() {
        let source = "fn f(seed: int) { let x = seed + 1; || { x; }(); }";
        let air = lower_root(source, "f").expect("lower failed");
        let lambda = air.lambdas.first().expect("missing lambda");

        assert!(matches!(
            &lambda.captures[..],
            [LambdaCaptureDecl::ReadonlyLocal { .. }]
        ));
        assert!(program_statements(&air).any(|statement| {
            matches!(statement, AirStmt::Init { value: RValue::MakeLambda { captures, .. }, .. }
                if matches!(&captures[..], [LambdaCaptureArg::ReadonlyLocal { .. }]))
        }));
        assert!(program_statements(&air).any(|statement| {
            matches!(
                statement,
                AirStmt::Eval(RValue::Call {
                    callee: Callee::Lambda(_),
                    ..
                })
            )
        }));
    }

    #[test]
    fn captured_callback_argument_uses_positional_capture_args() {
        let source =
            "fn each(f: fn()) { f(); } fn main(seed: int) { let x = seed + 1; each(|| { x; }); }";
        let air = lower_root(source, "main").expect("lower failed");
        let lambda = air.lambdas.first().expect("missing lambda");

        assert!(matches!(
            &lambda.captures[..],
            [LambdaCaptureDecl::ReadonlyLocal { .. }]
        ));
        assert!(program_statements(&air).any(|statement| {
            matches!(statement, AirStmt::Init { value: RValue::MakeLambda { captures, .. }, .. }
                if matches!(&captures[..], [LambdaCaptureArg::ReadonlyLocal { .. }]))
        }));
    }

    #[test]
    fn captured_alias_call_materializes_lambda_value() {
        let source = "fn f(seed: int) { let x = seed + 1; let g: fn() = || { x; }; g(); }";
        let air = lower_root(source, "f").expect("lower failed");

        assert!(program_statements(&air).any(|statement| {
            matches!(statement, AirStmt::Init { value: RValue::MakeLambda { captures, .. }, .. }
                if matches!(&captures[..], [LambdaCaptureArg::ReadonlyLocal { .. }]))
        }));
        assert!(program_statements(&air).any(|statement| {
            matches!(
                statement,
                AirStmt::Eval(RValue::Call {
                    callee: Callee::Lambda(_),
                    ..
                })
            )
        }));
    }

    #[test]
    fn borrowed_param_capture_lowers_to_scoped_borrow() {
        let source = "fn f(ref x: int) { || { x = 1; }; }";
        let air = lower_root(source, "f").expect("lower failed");
        let scoped = ScopedBorrowId::from_index(0);
        let lambda = air.lambdas.first().expect("missing lambda");

        assert_eq!(air.scoped_borrows.len(), 1);
        assert_eq!(air.capture_cells.len(), 0);
        assert!(matches!(
            &lambda.captures[..],
            [LambdaCaptureDecl::ScopedBorrow { borrow, .. }] if *borrow == scoped
        ));
        assert!(program_statements(&air).any(|statement| {
            matches!(statement, AirStmt::Init { value: RValue::MakeLambda { captures, .. }, .. }
                if matches!(&captures[..], [LambdaCaptureArg::ScopedBorrow { place }]
                    if place.root == PlaceRoot::ScopedBorrow(scoped)))
        }));
        assert!(
            function_statements(air.function(lambda.body)).any(|statement| {
                matches!(statement, AirStmt::Assign { dst, .. }
                if dst.root == PlaceRoot::LambdaCapture(LambdaCaptureSlotId::from_index(0)))
            })
        );
    }

    #[test]
    fn ref_self_capture_lowers_to_receiver_scoped_borrow() {
        let source = "struct Counter { value: int } extend Counter { fn touch(ref self) { let f = || { self.value = 1; }; f(); } } fn main() { var c = Counter { value: 0 }; c.touch(); }";
        let air = lower_root(source, "main").expect("lower failed");
        let scoped = ScopedBorrowId::from_index(0);
        let lambda = air.lambdas.first().expect("missing lambda");

        assert_eq!(air.scoped_borrows.len(), 1);
        assert!(matches!(
            air.scoped_borrows[scoped.index()].source,
            ScopedBorrowSource::RefSelf { .. }
        ));
        assert!(matches!(
            &lambda.captures[..],
            [LambdaCaptureDecl::ScopedBorrow { borrow, .. }] if *borrow == scoped
        ));
        assert!(
            function_statements(air.function(lambda.body)).any(|statement| {
                matches!(statement, AirStmt::Assign { dst, .. }
                if dst.root == PlaceRoot::LambdaCapture(LambdaCaptureSlotId::from_index(0)))
            })
        );
    }

    #[test]
    fn owner_access_after_borrowed_param_capture_uses_scoped_borrow() {
        let source = "fn f(ref x: int) { let g: fn() = || { x = 1; }; x = 2; g; }";
        let air = lower_root(source, "f").expect("lower failed");
        let scoped = ScopedBorrowId::from_index(0);
        let owner = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("f"))
            .expect("missing owner");
        let statements = function_statements(owner).collect::<Vec<_>>();
        let lambda_index = statements
            .iter()
            .position(|statement| {
                matches!(
                    statement,
                    AirStmt::Init {
                        value: RValue::MakeLambda { .. },
                        ..
                    }
                )
            })
            .expect("missing lambda init");

        assert!(statements[lambda_index + 1..].iter().any(|statement| {
            matches!(statement, AirStmt::Assign { dst, .. }
                if dst.root == PlaceRoot::ScopedBorrow(scoped))
        }));
    }

    #[test]
    fn nested_borrowed_param_recapture_uses_parent_slot() {
        let source = "fn f(ref x: int) { let outer: fn() = || { let inner: fn() = || { x = 1; }; inner; }; outer; }";
        let air = lower_root(source, "f").expect("lower failed");
        let scoped = ScopedBorrowId::from_index(0);
        let outer = air
            .lambdas
            .iter()
            .find(|decl| air.function(decl.owner).name == Ident::new("f"))
            .expect("missing outer lambda");
        let inner = air
            .lambdas
            .iter()
            .find(|decl| decl.owner == outer.body)
            .expect("missing inner lambda");

        assert_eq!(air.scoped_borrows.len(), 1);
        assert!(matches!(
            &outer.captures[..],
            [LambdaCaptureDecl::ScopedBorrow { borrow, .. }] if *borrow == scoped
        ));
        assert!(matches!(
            &inner.captures[..],
            [LambdaCaptureDecl::ScopedBorrow { borrow, .. }] if *borrow == scoped
        ));
        assert!(
            function_statements(air.function(outer.body)).any(|statement| {
                matches!(statement, AirStmt::Init { value: RValue::MakeLambda { captures, .. }, .. }
                if matches!(&captures[..], [LambdaCaptureArg::ScopedBorrow { place }]
                    if place.root == PlaceRoot::LambdaCapture(LambdaCaptureSlotId::from_index(0))))
            })
        );
    }

    #[test]
    fn uncaptured_ref_param_remains_local() {
        let source = "fn f(ref x: int) { x = 1; }";
        let air = lower_root(source, "f").expect("lower failed");

        assert!(air.scoped_borrows.is_empty());
        assert!(program_statements(&air).any(|statement| {
            matches!(statement, AirStmt::Assign { dst, .. }
                if dst.root == PlaceRoot::Local(LocalId::from_index(0)))
        }));
    }

    #[test]
    fn non_capturing_lambdas_get_distinct_body_functions() {
        let source = "fn f() { let a: fn() = || {}; let b: fn() = || {}; a; b; }";
        let air = lower_root(source, "f").expect("lower failed");
        let f = air
            .functions
            .iter()
            .position(|function| function.name == Ident::new("f"))
            .map(FunctionId::from_index)
            .expect("missing f");

        assert_eq!(air.lambdas.len(), 2);
        assert!(
            air.lambdas
                .iter()
                .all(|decl| decl.owner == f && decl.captures.is_empty())
        );
        assert_eq!(
            air.functions
                .iter()
                .filter(|function| matches!(function.kind, FunctionKind::Lambda(_)))
                .count(),
            2
        );
    }

    #[test]
    fn nested_non_capturing_lambda_uses_lambda_body_facts() {
        let source = "fn f() { let outer: fn() = || { let inner: fn() = || {}; inner; }; outer; }";
        let air = lower_root(source, "f").expect("lower failed");
        let outer = air
            .lambdas
            .iter()
            .find(|decl| air.function(decl.owner).name == Ident::new("f"))
            .expect("missing outer lambda");
        let inner = air
            .lambdas
            .iter()
            .find(|decl| decl.owner == outer.body)
            .expect("missing inner lambda");

        assert!(matches!(
            air.function(outer.body).kind,
            FunctionKind::Lambda(_)
        ));
        assert!(matches!(
            air.function(inner.body).kind,
            FunctionKind::Lambda(_)
        ));
        assert!(outer.captures.is_empty());
        assert!(inner.captures.is_empty());
    }

    #[test]
    fn non_capturing_lambda_values_and_calls_use_make_lambda_and_lambda_callee() {
        let source =
            "fn each(f: fn()) { f(); } fn f() { let g: fn() = || {}; g(); each(|| {}); || {}(); }";
        let air = lower_root(source, "f").expect("lower failed");
        let f = air
            .functions
            .iter()
            .find(|function| function.name == Ident::new("f"))
            .expect("missing f");

        assert!(function_statements(f).any(|statement| {
            matches!(statement, AirStmt::Init { value: RValue::MakeLambda { captures, .. }, .. } if captures.is_empty())
        }));
        assert!(function_statements(f).any(|statement| {
            matches!(
                statement,
                AirStmt::Eval(RValue::Call {
                    callee: Callee::Lambda(_),
                    ..
                })
            )
        }));
    }

    #[test]
    fn lambda_value_lowering_uses_passed_typecheck_facts() {
        let source = "fn f() { || {}; }";
        let (root, resolved, semantic) = checked(source);
        assert!(!semantic.public_facts.lambda_escapes().is_empty());

        let missing = TypecheckFacts::default();
        let err = lower_with_modules(
            &root,
            &resolved,
            &semantic.program,
            &missing,
            AirLowerConfig {
                roots: AirRoots {
                    entry: None,
                    callables: vec![root_function("f")],
                },
            },
        )
        .expect_err("expected missing lambda fact");
        assert!(matches!(err, LowerError::MissingLambdaEscape { .. }));

        let air = lower_checked_roots(&root, &resolved, &semantic, &["f"]).expect("lower failed");
        assert_eq!(air.lambdas.len(), 1);
        assert!(air.functions.iter().any(|function| {
            matches!(function.kind, FunctionKind::Lambda(lambda) if lambda == LambdaId::from_index(0))
        }));
        assert!(program_statements(&air).any(|statement| {
            matches!(statement, AirStmt::Init { value: RValue::MakeLambda { captures, .. }, .. } if captures.is_empty())
        }));
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

    fn lower_contract_boundary(source: &str, roots: &[&str]) -> Program {
        let (root, resolved, semantic) = checked(source);
        lower_checked_contract_boundary(&root, &resolved, &semantic, roots)
    }

    fn lower_contract_boundary_full_core(source: &str, roots: &[&str]) -> Program {
        let (root, resolved, semantic) = checked_with_full_core_shape(source);
        lower_checked_contract_boundary(&root, &resolved, &semantic, roots)
    }

    fn lower_contract_boundary_with_modules(
        source: &str,
        modules: &[(&str, &str)],
        roots: &[&str],
    ) -> Program {
        let (root, resolved, semantic) = checked_with_modules(source, modules);
        lower_checked_contract_boundary(&root, &resolved, &semantic, roots)
    }

    fn lower_checked_contract_boundary(
        root: &ast::Program,
        resolved: &ResolveResult,
        semantic: &typecheck::SemanticCheckOutput,
        roots: &[&str],
    ) -> Program {
        let source_index = crate::source_ast::SourceAstIndex::new(root, resolved);
        let index = SourceProgramIndex::new(&source_index, &semantic.program);
        let callable_facts = SemanticCallableFacts::new(&semantic.program);
        let root_keys = roots.iter().map(|name| root_function(name)).collect();
        let functions = ReachableItems::new(&index, &semantic.program, &callable_facts, root_keys)
            .expect("reachable closure failed");
        let mut cx = LowerCx {
            decls: Some(semantic.program.declarations.clone()),
            externs: Some(semantic.program.externs.clone()),
            contract_surfaces: Some(semantic.program.contract_surfaces.clone()),
            typecheck_facts: Some(&semantic.public_facts),
            ..LowerCx::default()
        };
        cx.lower_contract_surfaces()
            .expect("surface lowering failed");
        cx.lower_function_shells(&source_index.modules, &functions)
            .expect("function shell lowering failed");
        cx.lower_extern_declarations(&functions, &semantic.program)
            .expect("extern lowering failed");
        cx.lower_contract_declarations(&functions, &semantic.program)
            .expect("witness lowering failed");
        verify_contracts(&cx.program).expect("contract declarations should verify");
        cx.program
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
            &semantic.public_facts,
            AirLowerConfig::default(),
        )
    }

    fn lower_full_core(source: &str) -> Result<Program, LowerError> {
        let (root, resolved, semantic) = checked_with_full_core_shape(source);
        lower_with_modules(
            &root,
            &resolved,
            &semantic.program,
            &semantic.public_facts,
            AirLowerConfig::default(),
        )
    }

    fn lower_full_core_root(source: &str, name: &str) -> Result<Program, LowerError> {
        let (root, resolved, semantic) = checked_with_full_core_shape(source);
        lower_checked_roots(&root, &resolved, &semantic, &[name])
    }

    fn lower_full_core_entry(
        source: &str,
        entry: &str,
        callables: &[&str],
    ) -> Result<Program, LowerError> {
        let (root, resolved, semantic) = checked_with_full_core_shape(source);
        lower_checked_entry(&root, &resolved, &semantic, entry, callables)
    }

    fn lower_checked_entry(
        root: &ast::Program,
        resolved: &ResolveResult,
        semantic: &typecheck::SemanticCheckOutput,
        entry: &str,
        callables: &[&str],
    ) -> Result<Program, LowerError> {
        lower_with_modules(
            root,
            resolved,
            &semantic.program,
            &semantic.public_facts,
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
        lower_checked_roots(&root, &resolved, &semantic, names)
    }

    fn lower_checked_roots(
        root: &ast::Program,
        resolved: &ResolveResult,
        semantic: &typecheck::SemanticCheckOutput,
        names: &[&str],
    ) -> Result<Program, LowerError> {
        let callables = names.iter().map(|name| root_function(name)).collect();
        lower_with_modules(
            root,
            resolved,
            &semantic.program,
            &semantic.public_facts,
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
        lower_checked_entry(&root, &resolved, &semantic, name, callables)
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

    fn program_statements(program: &Program) -> impl Iterator<Item = AirStmt> + '_ {
        program.functions.iter().flat_map(function_statements)
    }

    fn test_operand_ty(program: &Program, operand: &Operand) -> TypeId {
        typing::operand_ty(program, operand).expect("test operand const should exist")
    }

    fn function_statements(function: &Function) -> impl Iterator<Item = AirStmt> + '_ {
        let mut statements = vec![];
        collect_block_statements(&function.body.block, &mut statements);
        statements.into_iter()
    }

    fn global_id(program: &Program, name: &str) -> GlobalId {
        let index = program
            .globals
            .iter()
            .position(|global| global.name == Ident::new(name))
            .unwrap_or_else(|| panic!("missing global {name}"));
        GlobalId::from_index(index)
    }

    fn collect_block_statements(block: &AirBlock, statements: &mut Vec<AirStmt>) {
        for stmt in &block.stmts {
            match stmt {
                AirStmt::Init { local, value } => statements.push(AirStmt::Init {
                    local: *local,
                    value: value.clone(),
                }),
                AirStmt::Assign { dst, value } => statements.push(AirStmt::Assign {
                    dst: dst.clone(),
                    value: value.clone(),
                }),
                AirStmt::Eval(value) => statements.push(AirStmt::Eval(value.clone())),
                AirStmt::GlobalEnsure { global } => {
                    statements.push(AirStmt::GlobalEnsure { global: *global });
                }
                AirStmt::GlobalSetRoot {
                    global,
                    value,
                    init,
                } => statements.push(AirStmt::GlobalSetRoot {
                    global: *global,
                    value: value.clone(),
                    init: *init,
                }),
                AirStmt::GlobalUpdateRoot { global, value } => {
                    statements.push(AirStmt::GlobalUpdateRoot {
                        global: *global,
                        value: value.clone(),
                    });
                }
                AirStmt::If(branch) => {
                    collect_block_statements(&branch.then_block, statements);
                    if let Some(block) = &branch.else_block {
                        collect_block_statements(block, statements);
                    }
                }
                AirStmt::PatternMatch(match_) => {
                    for arm in &match_.arms {
                        collect_block_statements(&arm.block, statements);
                    }
                }
                AirStmt::DynMatch(match_) => {
                    statements.push(AirStmt::DynMatch(match_.clone()));
                    for arm in &match_.arms {
                        collect_block_statements(&arm.block, statements);
                    }
                    collect_block_statements(&match_.fallback.block, statements);
                }
                AirStmt::Loop(loop_) => collect_block_statements(&loop_.body, statements),
                AirStmt::RangeFor(range) => {
                    statements.push(AirStmt::RangeFor(range.clone()));
                    collect_block_statements(&range.body, statements);
                }
                AirStmt::CollectionFor(for_) => {
                    statements.push(AirStmt::CollectionFor(for_.clone()));
                    collect_block_statements(&for_.body, statements);
                }
                AirStmt::CollectionLoan(loan) => {
                    collect_block_statements(&loan.body, statements);
                }
                AirStmt::CollectionSlotScope(scope) => {
                    statements.push(AirStmt::CollectionSlotScope(scope.clone()));
                    collect_block_statements(&scope.body, statements);
                }
                AirStmt::OptionalMatch(match_) => {
                    collect_block_statements(&match_.some_block, statements);
                    collect_block_statements(&match_.none_block, statements);
                }
                AirStmt::MapEntryMatch(match_) => {
                    collect_block_statements(&match_.some_block, statements);
                    collect_block_statements(&match_.none_block, statements);
                }
            }
        }
    }

    fn stringify_source_types(program: &Program) -> Vec<TypeData> {
        program_statements(program)
            .filter_map(|statement| match statement {
                AirStmt::Init {
                    value: RValue::Stringify { source_ty, .. },
                    ..
                }
                | AirStmt::Assign {
                    value: RValue::Stringify { source_ty, .. },
                    ..
                }
                | AirStmt::Eval(RValue::Stringify { source_ty, .. }) => {
                    Some(program.type_arena.data(source_ty).clone())
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

    fn assert_extern_signature(
        program: &Program,
        name: &str,
        params: &[(TypeData, ParamMode)],
        ret: &TypeData,
    ) {
        let decl = program
            .externs
            .iter()
            .find(|decl| decl.name.as_str() == name)
            .expect("extern not found");
        let actual_params = decl
            .params
            .iter()
            .map(|ty| (program.type_arena.data(ty.ty).clone(), ty.mode))
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

    fn test_reachable_callable<'a>(item: &ReachableItem<'a>) -> SourceCallable<'a> {
        match item.source {
            ReachableSource::Callable { callable, .. } => callable,
            ReachableSource::Lambda { .. }
            | ReachableSource::Global { .. }
            | ReachableSource::CastFrom { .. } => {
                panic!("expected callable item")
            }
        }
    }

    fn test_reachable_fact<'a>(item: &ReachableItem<'a>) -> &'a SemanticFunctionInstanceFact {
        match item.source {
            ReachableSource::Callable { fact, .. } => fact,
            ReachableSource::Lambda { .. }
            | ReachableSource::Global { .. }
            | ReachableSource::CastFrom { .. } => {
                panic!("expected callable item")
            }
        }
    }

    fn with_source_functions<R>(
        source: &str,
        names: &[&str],
        f: impl FnOnce(&[ModuleScope], &ReachableItems<'_>, &SemanticProgram) -> R,
    ) -> R {
        let (root, resolved, semantic) = checked(source);
        let source_index = crate::source_ast::SourceAstIndex::new(&root, &resolved);
        let index = SourceProgramIndex::new(&source_index, &semantic.program);
        let facts = SemanticCallableFacts::new(&semantic.program);
        let roots = names.iter().map(|name| root_function(name)).collect();
        let functions = ReachableItems::new(&index, &semantic.program, &facts, roots)
            .expect("source functions failed");
        f(&source_index.modules, &functions, &semantic.program)
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
        let externs =
            externs::prepare_raw_externs(RawExterns::default(), &root, &resolved).unwrap();
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

    fn lower_raw_enum_extern_borrow() -> Program {
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
                            flow: ParamFlow::Borrow,
                            escape: CallbackEscape::NonEscaping,
                        }],
                        ret: ExternTypeExpr::Void,
                    },
                    effects: ExternEffects::default(),
                }],
            }],
        };
        let source = "import ext:host { touch }; enum State: int { Idle } fn main() { let state = State.Idle; touch(state); }";
        let (root, resolved, semantic) = checked_with_provider(source, provider);
        lower_checked_roots(&root, &resolved, &semantic, &["main"]).expect("lower failed")
    }

    fn lower_global_extern_arg(flow: ParamFlow, global: &str) -> Program {
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
                            flow,
                            escape: CallbackEscape::NonEscaping,
                        }],
                        ret: ExternTypeExpr::Void,
                    },
                    effects: ExternEffects::default(),
                }],
            }],
        };
        let source = format!("import ext:host {{ touch }}; {global} fn main() {{ touch(Value); }}");
        let (root, resolved, semantic) = checked_with_provider(&source, provider);
        lower_checked_roots(&root, &resolved, &semantic, &["main"]).expect("lower failed")
    }

    fn checked_with_providers(
        source: &str,
        providers: Vec<ProviderDescriptor>,
    ) -> (ast::Program, ResolveResult, typecheck::SemanticCheckOutput) {
        let root = parse_program(source);
        let provider_raw = externs::ingest_providers(ExternInputs {
            packages: vec![PackageExternInputs {
                package: PackageId::synthetic_root(),
                providers,
            }],
        })
        .expect("valid provider");
        let external_modules = externs::raw_extern_module_ids(&provider_raw);
        let resolved = resolved_modules_with_core_option_external(&root, &[], &external_modules);
        let raw = externs::prepare_raw_externs(provider_raw, &root, &resolved).unwrap();
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
