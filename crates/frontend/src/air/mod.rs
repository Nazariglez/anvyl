pub mod body;
pub mod decl;
pub mod ids;
pub mod types;

#[cfg(test)]
mod tests;

mod typing;
mod verify;

pub use body::*;
pub use decl::*;
pub use ids::{
    AggregateId, BlockId, ConstId, EnumId, ExternId, ExternTypeId, FieldId, FunctionId, LocalId,
    ModuleId, TypeId, VariantId,
};
pub use types::*;
pub use verify::*;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Program {
    pub modules: Vec<Module>,
    pub entry: Option<FunctionId>,
    pub functions: Vec<Function>,
    pub externs: Vec<ExternDecl>,
    pub extern_types: Vec<ExternTypeDecl>,
    pub aggregates: Vec<AggregateDecl>,
    pub enums: Vec<EnumDecl>,
    pub type_arena: TypeArena,
    pub const_arena: ConstArena,
}

impl Program {
    pub fn alloc_module(&mut self, module: Module) -> ModuleId {
        let id = ModuleId::from_index(self.modules.len());
        self.modules.push(module);
        id
    }

    pub fn module(&self, id: ModuleId) -> &Module {
        &self.modules[id.index()]
    }

    pub fn module_mut(&mut self, id: ModuleId) -> &mut Module {
        &mut self.modules[id.index()]
    }

    pub fn set_entry(&mut self, entry: FunctionId) {
        self.entry = Some(entry);
    }

    pub fn entry(&self) -> Option<FunctionId> {
        self.entry
    }

    pub fn alloc_function(&mut self, func: Function) -> FunctionId {
        let id = FunctionId::from_index(self.functions.len());
        self.functions.push(func);
        id
    }

    pub fn function(&self, id: FunctionId) -> &Function {
        &self.functions[id.index()]
    }

    pub fn function_mut(&mut self, id: FunctionId) -> &mut Function {
        &mut self.functions[id.index()]
    }

    pub fn alloc_extern(&mut self, decl: ExternDecl) -> ExternId {
        let id = ExternId::from_index(self.externs.len());
        self.externs.push(decl);
        id
    }

    pub fn extern_decl(&self, id: ExternId) -> &ExternDecl {
        &self.externs[id.index()]
    }

    pub fn extern_decl_mut(&mut self, id: ExternId) -> &mut ExternDecl {
        &mut self.externs[id.index()]
    }

    pub fn alloc_aggregate(&mut self, agg: AggregateDecl) -> AggregateId {
        let id = AggregateId::from_index(self.aggregates.len());
        self.aggregates.push(agg);
        id
    }

    pub fn aggregate(&self, id: AggregateId) -> &AggregateDecl {
        &self.aggregates[id.index()]
    }

    pub fn aggregate_mut(&mut self, id: AggregateId) -> &mut AggregateDecl {
        &mut self.aggregates[id.index()]
    }

    pub fn alloc_enum(&mut self, enm: EnumDecl) -> EnumId {
        let id = EnumId::from_index(self.enums.len());
        self.enums.push(enm);
        id
    }

    pub fn enum_decl(&self, id: EnumId) -> &EnumDecl {
        &self.enums[id.index()]
    }

    pub fn enum_decl_mut(&mut self, id: EnumId) -> &mut EnumDecl {
        &mut self.enums[id.index()]
    }

    pub fn alloc_extern_type(&mut self, ty: ExternTypeDecl) -> ExternTypeId {
        let id = ExternTypeId::from_index(self.extern_types.len());
        self.extern_types.push(ty);
        id
    }

    pub fn extern_type(&self, id: ExternTypeId) -> &ExternTypeDecl {
        &self.extern_types[id.index()]
    }

    pub fn extern_type_mut(&mut self, id: ExternTypeId) -> &mut ExternTypeDecl {
        &mut self.extern_types[id.index()]
    }

    pub fn alloc_type(&mut self, data: TypeData) -> TypeId {
        self.type_arena.alloc(data)
    }

    pub fn type_data(&self, id: TypeId) -> &TypeData {
        self.type_arena.data(id)
    }

    pub fn alloc_const(&mut self, data: ConstData) -> ConstId {
        self.const_arena.alloc(data)
    }

    pub fn const_data(&self, id: ConstId) -> &ConstData {
        self.const_arena.get(id)
    }
}
