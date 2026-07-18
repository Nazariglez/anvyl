use super::super::{
    AggregateDecl, AirBlock, AirBody, AirStmt, AirTail, CaptureCellDecl, ConstData,
    ContractSurfaceDecl, ContractWeakeningDecl, ContractWitnessDecl, DynBorrowParamDecl, EnumDecl,
    EnumRepr, ExternDecl, ExternTypeDecl, FlagDecl, Function, FunctionKind, GlobalDecl, LambdaDecl,
    Local, LocalKind, Module, Mutability, Operand, OwnedValue, Param, ParamEscape, ParamMode,
    ParamRole, Place, PlaceRoot, Program, RValue, RawEnumValue, ScopedBorrowDecl,
    ScopedBorrowSource, Signature, TypeData, ValueSource, VariantDecl, VariantShape,
    ids::{
        AggregateId, BindingId, BlockId, CaptureCellId, ConstId, ContractSurfaceId,
        ContractWeakeningId, ContractWitnessId, DynBorrowParamId, EnumId, ExternId, ExternTypeId,
        FlagId, FunctionId, GlobalId, LambdaId, LocalId, ModuleId, ScopedBorrowId, TypeId,
    },
};
use crate::{
    air::verify::{self, VerifiedProgram},
    ast::Ident,
};

#[derive(Default)]
pub struct ProgramBuilder {
    program: Program,
    primitive_tys: [Option<TypeId>; 6],
}

impl ProgramBuilder {
    pub fn alloc_type(&mut self, data: TypeData) -> TypeId {
        self.program.alloc_type(data)
    }

    pub fn alloc_contract_surface(&mut self, decl: ContractSurfaceDecl) -> ContractSurfaceId {
        self.program.alloc_contract_surface(decl)
    }

    pub fn alloc_contract_weakening(&mut self, decl: ContractWeakeningDecl) -> ContractWeakeningId {
        self.program.alloc_contract_weakening(decl)
    }

    pub fn alloc_contract_witness(&mut self, decl: ContractWitnessDecl) -> ContractWitnessId {
        self.program.alloc_contract_witness(decl)
    }

    pub fn alloc_dyn_borrow_param(&mut self, decl: DynBorrowParamDecl) -> DynBorrowParamId {
        self.program.alloc_dyn_borrow_param(decl)
    }

    pub fn int_ty(&mut self) -> TypeId {
        self.primitive_ty(0, TypeData::Int)
    }

    pub fn float_ty(&mut self) -> TypeId {
        self.primitive_ty(1, TypeData::Float)
    }

    pub fn bool_ty(&mut self) -> TypeId {
        self.primitive_ty(2, TypeData::Bool)
    }

    pub fn string_ty(&mut self) -> TypeId {
        self.primitive_ty(3, TypeData::String)
    }

    pub fn void_ty(&mut self) -> TypeId {
        self.primitive_ty(4, TypeData::Void)
    }

    pub fn any_ty(&mut self) -> TypeId {
        self.primitive_ty(5, TypeData::Any)
    }

    fn primitive_ty(&mut self, index: usize, data: TypeData) -> TypeId {
        if let Some(id) = self.primitive_tys[index] {
            return id;
        }
        let id = self.program.alloc_type(data);
        self.primitive_tys[index] = Some(id);
        id
    }

    pub fn alloc_const(&mut self, data: ConstData) -> ConstId {
        self.program.alloc_const(data)
    }

    pub fn alloc_aggregate_raw(&mut self, decl: AggregateDecl) -> AggregateId {
        self.program.alloc_aggregate(decl)
    }

    pub fn alloc_aggregate(&mut self, decl: AggregateDecl) -> AggregateId {
        let module = decl.module;
        let id = self.program.alloc_aggregate(decl);
        if let Some(module) = self.program.modules.get_mut(module.index()) {
            module.aggregates.push(id);
        }
        id
    }

    pub fn alloc_enum_raw(&mut self, decl: EnumDecl) -> EnumId {
        self.program.alloc_enum(decl)
    }

    pub fn alloc_flag(&mut self, decl: FlagDecl) -> FlagId {
        let module = decl.module;
        let id = self.program.alloc_flag(decl);
        self.program.module_mut(module).flags.push(id);
        id
    }

    pub fn alloc_enum(&mut self, decl: EnumDecl) -> EnumId {
        let module = decl.module;
        let id = self.program.alloc_enum(decl);
        if let Some(module) = self.program.modules.get_mut(module.index()) {
            module.enums.push(id);
        }
        id
    }

    pub fn unit_enum(&mut self, module: ModuleId, name: &str) -> (EnumId, TypeId) {
        let id = self.alloc_enum(EnumDecl {
            name: Ident::new(name),
            module,
            core: None,
            repr: EnumRepr::Adt,
            raw_type: None,
            type_args: vec![],
            const_args: vec![],
            variants: vec![VariantDecl {
                name: Ident::new("Idle"),
                shape: VariantShape::Unit,
                raw_value: None,
            }],
        });
        (id, self.alloc_type(TypeData::Enum(id)))
    }

    pub fn raw_int_enum(
        &mut self,
        module: ModuleId,
        name: &str,
        variants: Vec<(&str, i64)>,
    ) -> (EnumId, TypeId) {
        let int = self.int_ty();
        let id = self.alloc_enum(EnumDecl {
            name: Ident::new(name),
            module,
            core: None,
            repr: EnumRepr::RawInt,
            raw_type: Some(int),
            type_args: vec![],
            const_args: vec![],
            variants: variants
                .into_iter()
                .map(|(name, value)| VariantDecl {
                    name: Ident::new(name),
                    shape: VariantShape::Unit,
                    raw_value: Some(RawEnumValue::Int(value)),
                })
                .collect(),
        });
        (id, self.alloc_type(TypeData::Enum(id)))
    }

    pub fn alloc_extern_type_raw(&mut self, decl: ExternTypeDecl) -> ExternTypeId {
        self.program.alloc_extern_type(decl)
    }

    pub fn alloc_extern_type(&mut self, decl: ExternTypeDecl) -> ExternTypeId {
        let module = decl.module;
        let id = self.program.alloc_extern_type(decl);
        if let Some(module) = self.program.modules.get_mut(module.index()) {
            module.extern_types.push(id);
        }
        id
    }

    pub fn alloc_extern_raw(&mut self, decl: ExternDecl) -> ExternId {
        self.program.alloc_extern(decl)
    }

    pub fn alloc_extern(&mut self, decl: ExternDecl) -> ExternId {
        let module = decl.module;
        let id = self.program.alloc_extern(decl);
        if let Some(module) = self.program.modules.get_mut(module.index()) {
            module.externs.push(id);
        }
        id
    }

    pub fn alloc_function_raw(&mut self, func: Function) -> FunctionId {
        self.program.alloc_function(func)
    }

    pub fn alloc_lambda(&mut self, decl: LambdaDecl) -> LambdaId {
        self.program.alloc_lambda(decl)
    }

    pub fn alloc_scoped_borrow(&mut self, decl: ScopedBorrowDecl) -> ScopedBorrowId {
        self.program.alloc_scoped_borrow(decl)
    }

    pub fn alloc_capture_cell(&mut self, decl: CaptureCellDecl) -> CaptureCellId {
        self.program.alloc_capture_cell(decl)
    }

    pub fn alloc_global_raw(&mut self, decl: GlobalDecl) -> GlobalId {
        self.program.alloc_global(decl)
    }

    pub fn alloc_global_with_init(
        &mut self,
        module: ModuleId,
        name: &str,
        ty: TypeId,
        mutability: Mutability,
    ) -> (GlobalId, FunctionId) {
        self.program.alloc_global_with_init(|global, init| {
            let mut fb = FunctionBuilder::new(name, module, FunctionKind::GlobalInit(global), ty);
            fb.push_block(AirTail::Unreachable);
            (
                GlobalDecl {
                    name: Ident::new(name),
                    module,
                    ty,
                    mutability,
                    init,
                },
                fb.finish(),
            )
        })
    }

    pub fn alloc_function(&mut self, func: Function) -> FunctionId {
        let module = func.module;
        let id = self.program.alloc_function(func);
        if let Some(module) = self.program.modules.get_mut(module.index()) {
            module.functions.push(id);
        }
        id
    }

    pub fn alloc_module(&mut self, module: Module) -> ModuleId {
        self.program.alloc_module(module)
    }

    pub fn set_entry(&mut self, entry: FunctionId) {
        self.program.set_entry(entry);
    }

    pub fn module_mut(&mut self, id: ModuleId) -> &mut Module {
        self.program.module_mut(id)
    }

    pub fn function_count(&self) -> usize {
        self.program.functions.len()
    }

    pub fn lambda_count(&self) -> usize {
        self.program.lambdas.len()
    }

    pub fn finish(self) -> Program {
        self.program
    }
}

pub struct FunctionBuilder {
    name: Ident,
    module: ModuleId,
    kind: FunctionKind,
    return_type: TypeId,
    params: Vec<Param>,
    locals: Vec<Local>,
    block: AirBlock,
}

impl FunctionBuilder {
    pub fn new(name: &str, module: ModuleId, kind: FunctionKind, return_type: TypeId) -> Self {
        Self {
            name: Ident::new(name),
            module,
            kind,
            return_type,
            params: Vec::new(),
            locals: Vec::new(),
            block: AirBlock::default(),
        }
    }

    pub fn push_param(&mut self, name: &str, ty: TypeId, role: ParamRole) -> LocalId {
        self.push_param_with_mode(name, ty, ParamMode::Value, role)
    }

    pub fn push_param_with_mode(
        &mut self,
        name: &str,
        ty: TypeId,
        mode: ParamMode,
        role: ParamRole,
    ) -> LocalId {
        let mutability = if mode == ParamMode::MutBorrow {
            Mutability::Mutable
        } else {
            Mutability::Immutable
        };
        let local_id = self.push_local(Some(name), ty, mutability, LocalKind::Arg);
        self.params.push(Param {
            name: Some(Ident::new(name)),
            ty,
            mode,
            escape: ParamEscape::NonEscaping,
            role,
            local_id,
        });
        local_id
    }

    pub fn set_param_escape(&mut self, index: usize, escape: ParamEscape) {
        self.params[index].escape = escape;
    }

    pub fn push_local(
        &mut self,
        name: Option<&str>,
        ty: TypeId,
        mutability: Mutability,
        kind: LocalKind,
    ) -> LocalId {
        let id = LocalId::from_index(self.locals.len());
        self.locals.push(Local {
            name: name.map(Ident::new),
            binding: None,
            ty,
            mutability,
            kind,
        });
        id
    }

    pub fn bind_local(&mut self, local: LocalId, binding: BindingId) {
        self.locals[local.index()].binding = Some(binding);
    }

    pub fn push_block(&mut self, tail: AirTail) -> BlockId {
        self.block.tail = tail;
        BlockId::from_index(0)
    }

    pub fn add_statement(&mut self, block: BlockId, stmt: AirStmt) {
        assert_eq!(block, BlockId::from_index(0));
        self.block.stmts.push(stmt);
    }

    pub fn finish(self) -> Function {
        Function {
            name: self.name,
            module: self.module,
            kind: self.kind,
            owner: None,
            specialization: None,
            signature: Signature::new(self.params, self.return_type),
            locals: self.locals,
            body: AirBody { block: self.block },
        }
    }
}

pub fn body_from_block(block: AirBlock) -> AirBody {
    AirBody { block }
}

pub fn empty_module(path: &str) -> Module {
    Module {
        path: vec![Ident::new(path)],
        ..Module::default()
    }
}

pub fn test_module(builder: &mut ProgramBuilder) -> ModuleId {
    builder.alloc_module(empty_module("test"))
}

pub fn stmt_init(local: LocalId, value: RValue) -> AirStmt {
    AirStmt::Init {
        local,
        value: finalized_store(value),
    }
}

pub fn stmt_assign(dst: Place, value: RValue) -> AirStmt {
    AirStmt::Assign {
        dst,
        value: finalized_store(value),
    }
}

fn finalized_store(value: RValue) -> RValue {
    match value {
        RValue::Use(value) => RValue::Materialize(OwnedValue {
            value,
            source: ValueSource::Reusable,
        }),
        value => value,
    }
}

pub fn owned(value: Operand) -> OwnedValue<Operand> {
    OwnedValue::reusable(value)
}

pub fn stmt_eval(value: RValue) -> AirStmt {
    AirStmt::Eval(value)
}

pub fn term_return(value: Operand) -> AirTail {
    AirTail::ReturnOwned(OwnedValue {
        value,
        source: ValueSource::Reusable,
    })
}

pub fn term_return_void() -> AirTail {
    AirTail::Return(None)
}

pub fn term_unreachable() -> AirTail {
    AirTail::Unreachable
}

pub fn scoped_mut_param_borrow(
    owner: FunctionId,
    binding: BindingId,
    local: LocalId,
    ty: TypeId,
    mutability: Mutability,
) -> ScopedBorrowDecl {
    ScopedBorrowDecl {
        owner,
        binding,
        source: ScopedBorrowSource::SourceMutParam { local },
        ty,
        mutability,
    }
}

pub fn place(local: LocalId, ty: TypeId) -> Place {
    Place {
        root: PlaceRoot::Local(local),
        projection: Vec::new(),
        ty,
    }
}

pub fn op_place(local: LocalId, ty: TypeId) -> Operand {
    Operand::Place(place(local, ty))
}

pub fn op_const(id: ConstId) -> Operand {
    Operand::Const(id)
}

#[track_caller]
pub fn expect_verified(program: &Program) -> VerifiedProgram<'_> {
    match verify::verify(program) {
        Ok(v) => v,
        Err(errors) => panic!("expected verified AIR, got: {errors:#?}"),
    }
}
