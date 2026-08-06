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
    DynReceiver, EnumDecl, EnumRepr, ExternAbi, ExternBindingDecl, ExternDecl, ExternFieldDecl,
    ExternId, ExternInitArgDecl, ExternMember, ExternMethodDecl, ExternOp, ExternOpDecl,
    ExternParamDecl, ExternReceiverDecl, ExternRep, ExternStaticDecl, ExternTypeBindingDecl,
    ExternTypeDecl, ExternVariantAbiDecl, FieldDecl, FieldId, FlagDecl, FlagMemberDecl,
    FlagMemberId, FlagStaticOp, Function, FunctionId, FunctionKind, FunctionOwner,
    FunctionSpecialization, FunctionValueCapability, GlobalDecl, GlobalId, GlobalInitEffect,
    IterCountCheck, LambdaCaptureArg, LambdaCaptureDecl, LambdaCaptureSlotId, LambdaDecl,
    LambdaEscape, LambdaId, Local, LocalId, LocalKind, MapWriteKind, Module, ModuleId,
    Mutability as AirMutability, Operand, OwnedValue, Param, ParamEscape, ParamMode, ParamRole,
    ParamType, Place, PlaceRoot, Program, RValue, RawEnumValue, ReturnMode, ScopedBorrowDecl,
    ScopedBorrowId, ScopedBorrowSource, Signature, SignatureType, TypeData, TypeId, VariantDecl,
    VariantShape, VerifyError, ownership, place_model,
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

fn owned(value: Operand) -> OwnedValue<Operand> {
    OwnedValue::reusable(value)
}

fn owned_fields(fields: Vec<Operand>) -> Vec<OwnedValue<Operand>> {
    fields.into_iter().map(owned).collect()
}

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
        span: Option<SourceSpan>,
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
            span,
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
                    field_decl.site.span,
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
                    field_decl.site.span,
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
                    method.site.span,
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
                    static_method.site.span,
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
                    ty.site.span,
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
                    operator.site.span,
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
                    operator.site.span,
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
            function.site.span,
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
            ReturnMode::Value(expected) => self.lower_expected_value(expr, expected, expr),
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
                    None => RValue::Use(self.lower_expected_value(
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
                (false, Some(value)) => CallArg::Value(owned(value)),
                (true, Some(value)) => CallArg::InitFieldProvided(owned(value)),
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
                    value: owned(value),
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
                    value: owned(Operand::Place(place)),
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
                Some(self.lower_expected_value(start, field_ty(RANGE_START_FIELD)?, start)?),
                Some(self.lower_expected_value(end, field_ty(RANGE_END_FIELD)?, end)?),
            ),
            ast::Range::From { start } => (
                Some(self.lower_expected_value(start, field_ty(RANGE_START_FIELD)?, start)?),
                None,
            ),
            ast::Range::To { end, .. } => (
                None,
                Some(self.lower_expected_value(end, field_ty(RANGE_END_FIELD)?, end)?),
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
                fields: owned_fields(fields),
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
                value => self.emit_typed_temp(
                    raw_ty,
                    RValue::DynDowncast {
                        value: owned(value),
                        surface,
                        target,
                        ty: raw_ty,
                    },
                )?,
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
            return self.emit_typed_temp(
                ty,
                RValue::DynPack {
                    value: owned(value),
                    witness,
                    ty,
                },
            );
        }
        if let Some(_fact) = self.facts.dyn_weakenings.get(&expr.node.id) {
            let value = self.lower_dynamic_source(expr)?;
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
                    value: owned(value),
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
                    value: owned(Operand::Place(this.local_place(payload))),
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
            ExprKind::Binary(binary) => self.lower_binary_value(expr, binary),
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

    fn lower_binary_value(
        &mut self,
        expr: &ExprNode,
        binary: &ast::BinaryNode,
    ) -> Result<Operand, LowerError> {
        let result_ty = self.lower_expr_ty(expr.node.id)?;
        if binary.node.op == BinaryOp::Add && result_ty == Type::String {
            return self.lower_string_concat(expr);
        }
        if binary.node.op == BinaryOp::Coalesce {
            return self.lower_coalesce(expr, binary, &result_ty);
        }
        if let Some(decisive) = binary.node.op.short_circuit_value() {
            return self.lower_short_circuit_value(expr, binary, decisive, &result_ty);
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

    fn lower_short_circuit_value(
        &mut self,
        expr: &ExprNode,
        binary: &ast::BinaryNode,
        decisive: bool,
        result_ty: &Type,
    ) -> Result<Operand, LowerError> {
        if result_ty != &Type::Bool {
            return Err(unsupported_expr(expr));
        }
        let ty = self.cx.lower_ty(result_ty)?;
        let result = self.temp(ty);
        let cond = self.lower_if_cond(&binary.node.left)?;
        let rhs = self.lower_nested_expr_branch_value(&binary.node.right, result)?;
        let decisive_branch = self.with_nested_block(|this| {
            let value = this.bool_const(ty, decisive);
            this.emit_init(result, RValue::Use(value))
        })?;
        let (then_block, else_block) = if decisive {
            (decisive_branch, rhs)
        } else {
            (rhs, decisive_branch)
        };
        self.finish_conditional_value(expr, cond, Some(result), then_block, else_block)
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
                this.lower_expected_value(&binary.node.right, result_ty, expr)
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
            args: vec![CallArg::Value(owned(source))],
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

        if let ExprKind::InferredEnum(inferred) = &expr.node.kind
            && matches!(
                self.cx.program.type_data(expected),
                TypeData::Optional(_) | TypeData::Enum(_)
            )
        {
            return self.lower_inferred_enum_to(expr, inferred, expected);
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
        self.emit_typed_temp(
            ty,
            RValue::Aggregate {
                kind,
                fields: owned_fields(fields),
                ty,
            },
        )
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
                kind: AggregateCtor::ArrayFill,
                fields: vec![owned(value); len],
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
                fields: owned_fields(fields),
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
                fields: owned_fields(fields),
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
        self.emit_typed_temp(
            ty,
            RValue::OptionalSome {
                value: owned(value),
                ty,
            },
        )
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
                fields: owned_fields(fields),
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
        let field_types = expected
            .iter()
            .map(|(_, name, ty)| (*name, *ty))
            .collect::<Vec<_>>();
        let mut values = self.lower_named_fields(expr, &literal.node.fields, &field_types)?;

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
                if let Some(value) = values.remove(&name) {
                    return Ok(value);
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
        let value = self.with_default_facts(default.default, &default.facts_body, |this| {
            this.lower_expected_value(&default_expr, ty, expr)
        })?;
        self.snapshot_field_value(value, ty)
    }

    fn lower_named_fields(
        &mut self,
        expr: &ExprNode,
        fields: &[(Ident, ExprNode)],
        expected: &[(Ident, TypeId)],
    ) -> Result<HashMap<Ident, Operand>, LowerError> {
        let mut values = HashMap::new();
        for (name, field_expr) in fields {
            let Some((_, ty)) = expected.iter().find(|(expected, _)| expected == name) else {
                return Err(unsupported_expr(expr));
            };
            if values.contains_key(name) {
                return Err(unsupported_expr(expr));
            }
            let value = self.lower_expected_value(field_expr, *ty, expr)?;
            let value = self.snapshot_field_value(value, *ty)?;
            values.insert(*name, value);
        }
        Ok(values)
    }

    fn snapshot_field_value(&mut self, value: Operand, ty: TypeId) -> Result<Operand, LowerError> {
        let whole_temp = matches!(
            &value,
            Operand::Place(place)
                if place.projection.is_empty()
                    && place.root.local().is_some_and(|local| {
                        self.function.locals[local.index()].kind == LocalKind::Temp
                    })
        );
        if matches!(value, Operand::Const(_)) || whole_temp {
            return Ok(value);
        }
        self.emit_typed_temp(ty, RValue::Use(value))
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
        let mut values = self.lower_named_fields(expr, fields, &expected)?;
        expected
            .into_iter()
            .map(|(name, _)| values.remove(&name).ok_or_else(|| unsupported_expr(expr)))
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
            let value = self.lower_expected_value(field_expr, field_ty, field_expr)?;
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
        let ty = self.cx.lower_ty(&self.lower_expr_ty(expr.node.id)?)?;
        self.lower_inferred_enum_to(expr, inferred, ty)
    }

    fn lower_inferred_enum_to(
        &mut self,
        expr: &ExprNode,
        inferred: &ast::InferredEnumNode,
        ty: TypeId,
    ) -> Result<Operand, LowerError> {
        if let TypeData::Optional(inner) = self.cx.program.type_data(ty).clone() {
            return match (inferred.node.variant.as_str(), &inferred.node.args) {
                ("Some", ast::InferredEnumArgs::Tuple(args)) if args.len() == 1 => {
                    let value = self.lower_expected_value(&args[0], inner, expr)?;
                    self.optional_some(value, ty, expr)
                }
                ("None", ast::InferredEnumArgs::Unit) => self.optional_none(ty, expr),
                _ => Err(unsupported_expr(expr)),
            };
        }

        let TypeData::Enum(enum_id) = self.cx.program.type_data(ty) else {
            return Err(unsupported_expr(expr));
        };
        let enum_id = *enum_id;
        let Some(variant) = self.enum_variant_id(enum_id, inferred.node.variant) else {
            return Err(unsupported_expr(expr));
        };
        match &inferred.node.args {
            ast::InferredEnumArgs::Unit => self.emit_enum_variant(ty, enum_id, variant, vec![]),
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
                    .map(|(arg, ty)| self.lower_expected_value(arg, ty, expr))
                    .collect::<Result<Vec<_>, _>>()?;
                self.emit_enum_variant(ty, enum_id, variant, fields)
            }
            ast::InferredEnumArgs::Struct(args) => {
                let Some((_, expected)) = self.enum_struct_variant(enum_id, inferred.node.variant)
                else {
                    return Err(unsupported_expr(expr));
                };
                let fields = self.lower_ordered_fields(expr, args, expected)?;
                self.emit_enum_variant(ty, enum_id, variant, fields)
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
                fields: owned_fields(fields),
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
        self.finish_conditional_value(expr, cond, result, then_block, else_block)
    }

    fn finish_conditional_value(
        &mut self,
        owner: &ExprNode,
        cond: Operand,
        result: Option<LocalId>,
        then_block: AirBlock,
        else_block: AirBlock,
    ) -> Result<Operand, LowerError> {
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
            return Err(unsupported_expr(owner));
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
        let value = self.lower_expected_value(body, result_ty, body)?;
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
                    value => AirDynMatchSource::Owned(owned(value)),
                }
            }
        };
        let aliases = matches!(source, AirDynMatchSource::Mutable(_));
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
            let binding = match (binding, aliases) {
                (Some(local), true) => AirDynMatchTargetBinding::Alias(local),
                (Some(local), false) => AirDynMatchTargetBinding::Materialize(local),
                (None, _) => AirDynMatchTargetBinding::Discard,
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
        self.lower_expected_value(tail, expected, owner).map(Some)
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
            let value =
                this.lower_expected_value(expr, this.function.locals[result.index()].ty, expr)?;
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
                Ok(CallArg::Value(owned(Operand::Place(
                    self.lower_shared_slice_call_arg(expr, param.ty)?,
                ))))
            }
            ParamMode::Value => Ok(CallArg::Value(owned(
                self.lower_expected_value(expr, param.ty, expr)?,
            ))),
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
            ParamMode::Value => CallArg::Value(owned(Operand::Place(receiver))),
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
                ParamMode::Value => CallArg::Value(owned(Operand::Place(place))),
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
                Ok(CallArg::Value(owned(value)))
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
                _ => DynReceiver::Owned(owned(value)),
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
                        let value = self.lower_expected_value(value, inner, expr)?;
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
                .map(|(arg, ty)| self.lower_expected_value(arg, ty, expr))
                .collect::<Result<Vec<_>, _>>()?;
            return Ok(RValue::Aggregate {
                kind: AggregateCtor::EnumVariant { enum_id, variant },
                fields: owned_fields(fields),
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
                    value: owned(keep),
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
                    value: owned(elem_value),
                })
            }
            FilterCollection::Map { key, value } => {
                let entry =
                    self.map_filter_entry(root, index, entry_ty.expect("map entry type"))?;
                self.emit_eval(RValue::MapInsert {
                    map: self.local_place(kept),
                    key: owned(Self::tuple_field_operand(entry.clone(), 0, key)),
                    value: owned(Self::tuple_field_operand(entry, 1, value)),
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
                ParamMode::Value => Ok(CallArg::Value(owned(arg))),
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
        let value = self.lower_expected_value(&call.node.args[0], elem, expr)?;
        Ok(Some(RValue::ListPush {
            list,
            value: owned(value),
        }))
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
        let key = self.lower_expected_value(&call.node.args[0], key_ty, expr)?;
        let value = self.lower_expected_value(&call.node.args[1], value_ty, expr)?;
        Ok(Some(RValue::MapInsert {
            map,
            key: owned(key),
            value: owned(value),
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
                        self.lower_expected_value(&assign.node.value, dst.ty, &assign.node.value)?;
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
                let value =
                    self.lower_expected_value(&assign.node.value, dst.ty, &assign.node.value)?;
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
        let key = self.lower_expected_value(&index.node.index, key_ty, target)?;
        let value = self.lower_expected_value(value_expr, value_ty, value_expr)?;
        self.emit_eval(RValue::MapInsert {
            map,
            key: owned(key),
            value: owned(value),
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
    super::materialization::finalize(&mut cx.program);
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

    use super::*;
    use crate::{
        ast, externs,
        externs::RawExterns,
        resolve::ResolveResult,
        test_support::{parse_program, resolved_modules_with_core_option},
        typecheck::{self, TypecheckConfig},
    };
    #[test]
    fn function_stringify_reports_missing_value_lowering() {
        let source = "fn g() {} fn f() -> string { #stringify(g) }";
        let err = lower_root(source, "f").expect_err("expected unsupported expression");

        assert!(matches!(err, LowerError::UnsupportedExpr { .. }));
    }
    fn root_function(name: &str) -> CallableInstanceKey {
        CallableInstanceKey {
            target: CallableId::function(ModuleScope::Root, Ident::new(name)),
            args: GenericArgs::default(),
        }
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
}
