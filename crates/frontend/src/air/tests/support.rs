use super::super::{
    AggregateDecl, BasicBlock, ConstData, EnumDecl, ExternDecl, ExternTypeDecl, Function,
    FunctionKind, Local, LocalKind, Module, Mutability, Operand, Param, ParamRole, Place, Program,
    RValue, Signature, Statement, Terminator, TypeData,
    ids::{
        AggregateId, BlockId, ConstId, EnumId, ExternId, ExternTypeId, FunctionId, LocalId,
        ModuleId, TypeId, VariantId,
    },
};
use crate::{
    air::verify::{self, VerifiedProgram},
    ast::Ident,
};

pub struct ProgramBuilder {
    program: Program,
}

impl ProgramBuilder {
    pub fn new() -> Self {
        Self {
            program: Program::default(),
        }
    }

    pub fn alloc_type(&mut self, data: TypeData) -> TypeId {
        self.program.alloc_type(data)
    }

    pub fn alloc_const(&mut self, data: ConstData) -> ConstId {
        self.program.alloc_const(data)
    }

    pub fn alloc_aggregate(&mut self, decl: AggregateDecl) -> AggregateId {
        self.program.alloc_aggregate(decl)
    }

    pub fn alloc_enum(&mut self, decl: EnumDecl) -> EnumId {
        self.program.alloc_enum(decl)
    }

    pub fn alloc_extern_type(&mut self, decl: ExternTypeDecl) -> ExternTypeId {
        self.program.alloc_extern_type(decl)
    }

    pub fn alloc_extern(&mut self, decl: ExternDecl) -> ExternId {
        self.program.alloc_extern(decl)
    }

    pub fn alloc_function(&mut self, func: Function) -> FunctionId {
        self.program.alloc_function(func)
    }

    pub fn alloc_module(&mut self, module: Module) -> ModuleId {
        self.program.alloc_module(module)
    }

    pub fn set_entry(&mut self, entry: FunctionId) {
        self.program.set_entry(entry);
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
    blocks: Vec<BasicBlock>,
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
            blocks: Vec::new(),
        }
    }

    pub fn push_param(&mut self, name: &str, ty: TypeId, role: ParamRole) -> LocalId {
        let local_id = self.push_local(Some(name), ty, Mutability::Immutable, LocalKind::Arg);
        self.params.push(Param {
            name: Some(Ident::new(name)),
            ty,
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

    pub fn push_block(&mut self, terminator: Terminator) -> BlockId {
        let id = BlockId::from_index(self.blocks.len());
        self.blocks.push(BasicBlock {
            statements: Vec::new(),
            terminator,
        });
        id
    }

    pub fn add_statement(&mut self, block: BlockId, stmt: Statement) {
        self.blocks[block.index()].statements.push(stmt);
    }

    pub fn finish(self) -> Function {
        Function {
            name: self.name,
            module: self.module,
            kind: self.kind,
            signature: Signature::new(self.params, self.return_type),
            locals: self.locals,
            body: self.blocks,
        }
    }
}

// ── Module helper ────────────────────────────────────────────────────

pub fn test_module(builder: &mut ProgramBuilder) -> ModuleId {
    builder.alloc_module(Module {
        path: vec![Ident::new("test")],
        functions: vec![],
        aggregates: vec![],
        enums: vec![],
        extern_types: vec![],
        externs: vec![],
    })
}

pub fn stmt_assign(dst: Place, value: RValue) -> Statement {
    Statement::Assign { dst, value }
}

pub fn stmt_eval(value: RValue) -> Statement {
    Statement::Eval(value)
}

pub fn term_goto(target: BlockId) -> Terminator {
    Terminator::Goto(target)
}

pub fn term_if(cond: Operand, then_bb: BlockId, else_bb: BlockId) -> Terminator {
    Terminator::If {
        cond,
        then_bb,
        else_bb,
    }
}

pub fn term_switch_enum(
    discr: Place,
    arms: Vec<(VariantId, BlockId)>,
    else_bb: Option<BlockId>,
) -> Terminator {
    Terminator::SwitchEnum {
        discr,
        arms,
        else_bb,
    }
}

pub fn term_return(value: Operand) -> Terminator {
    Terminator::Return(Some(value))
}

pub fn term_return_void() -> Terminator {
    Terminator::Return(None)
}

pub fn term_unreachable() -> Terminator {
    Terminator::Unreachable
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
