use super::super::{
    AggregateDecl, AirBlock, AirBody, AirStmt, AirTail, ConstData, EnumDecl, ExternDecl,
    ExternTypeDecl, Function, FunctionKind, Local, LocalKind, Module, Mutability, Operand, Param,
    ParamMode, ParamRole, Place, Program, RValue, Signature, TypeData,
    ids::{
        AggregateId, BlockId, ConstId, EnumId, ExternId, ExternTypeId, FunctionId, LocalId,
        ModuleId, TypeId,
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

    pub fn alloc_enum(&mut self, decl: EnumDecl) -> EnumId {
        let module = decl.module;
        let id = self.program.alloc_enum(decl);
        if let Some(module) = self.program.modules.get_mut(module.index()) {
            module.enums.push(id);
        }
        id
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
            role,
            local_id,
        });
        local_id
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
            ty,
            mutability,
            kind,
        });
        id
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
        functions: vec![],
        aggregates: vec![],
        enums: vec![],
        extern_types: vec![],
        externs: vec![],
    }
}

pub fn test_module(builder: &mut ProgramBuilder) -> ModuleId {
    builder.alloc_module(empty_module("test"))
}

pub fn stmt_init(local: LocalId, value: RValue) -> AirStmt {
    AirStmt::Init { local, value }
}

pub fn stmt_assign(dst: Place, value: RValue) -> AirStmt {
    AirStmt::Assign { dst, value }
}

pub fn stmt_eval(value: RValue) -> AirStmt {
    AirStmt::Eval(value)
}

pub fn term_return(value: Operand) -> AirTail {
    AirTail::Return(Some(value))
}

pub fn term_return_void() -> AirTail {
    AirTail::Return(None)
}

pub fn term_unreachable() -> AirTail {
    AirTail::Unreachable
}

pub fn place(local: LocalId, ty: TypeId) -> Place {
    Place {
        root: local,
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
        Err(errors) => panic!("expected verifier-clean AIR, got: {errors:#?}"),
    }
}
