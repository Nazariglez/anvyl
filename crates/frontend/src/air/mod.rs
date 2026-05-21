pub mod body;
pub mod decl;
pub mod ids;
pub mod types;

#[cfg(test)]
mod tests;

#[cfg(test)]
pub(crate) mod lower;
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

use crate::ast::Ident;

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

    pub fn type_display_name(&self, ty: TypeId) -> String {
        self.render_type(ty, TypeRender::Display)
    }

    pub fn type_helper_key(&self, ty: TypeId) -> String {
        self.render_type(ty, TypeRender::HelperKey)
    }

    fn render_type(&self, ty: TypeId, mode: TypeRender) -> String {
        match (mode, self.type_data(ty)) {
            (_, TypeData::Int) => "int".to_string(),
            (_, TypeData::Float) => "float".to_string(),
            (_, TypeData::Bool) => "bool".to_string(),
            (_, TypeData::String) => "string".to_string(),
            (_, TypeData::Void) => "void".to_string(),
            (_, TypeData::Any) => "any".to_string(),
            (TypeRender::Display, TypeData::Optional(inner)) => {
                format!("{}?", self.render_type(*inner, mode))
            }
            (TypeRender::HelperKey, TypeData::Optional(inner)) => {
                format!("opt_{}", helper_part(self.render_type(*inner, mode)))
            }
            (TypeRender::Display, TypeData::Tuple(items)) => format!(
                "({})",
                items
                    .iter()
                    .map(|item| self.render_type(*item, mode))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            (TypeRender::HelperKey, TypeData::Tuple(items)) => format!(
                "tuple{}_{}",
                items.len(),
                helper_parts(items.iter().map(|item| self.render_type(*item, mode)))
            ),
            (TypeRender::Display, TypeData::List(elem)) => {
                format!("[{}]", self.render_type(*elem, mode))
            }
            (TypeRender::HelperKey, TypeData::List(elem)) => {
                format!("list_{}", helper_part(self.render_type(*elem, mode)))
            }
            (TypeRender::Display, TypeData::Array { elem, len }) => {
                format!("[{}; {}]", self.render_type(*elem, mode), len)
            }
            (TypeRender::HelperKey, TypeData::Array { elem, len }) => {
                format!(
                    "array{}_{}",
                    len,
                    helper_part(self.render_type(*elem, mode))
                )
            }
            (TypeRender::Display, TypeData::Map { key, value, .. }) => format!(
                "[{}: {}]",
                self.render_type(*key, mode),
                self.render_type(*value, mode)
            ),
            (TypeRender::HelperKey, TypeData::Map { key, value, .. }) => format!(
                "map_{}{}",
                helper_part(self.render_type(*key, mode)),
                helper_part(self.render_type(*value, mode))
            ),
            (TypeRender::Display, TypeData::Slice(elem)) => {
                format!("&[{}]", self.render_type(*elem, mode))
            }
            (TypeRender::HelperKey, TypeData::Slice(elem)) => {
                format!("slice_{}", helper_part(self.render_type(*elem, mode)))
            }
            (TypeRender::Display, TypeData::Function(sig)) => format!(
                "fn({}) -> {}",
                sig.params
                    .iter()
                    .map(|param| self.render_type(*param, mode))
                    .collect::<Vec<_>>()
                    .join(", "),
                self.render_type(sig.ret, mode)
            ),
            (TypeRender::HelperKey, TypeData::Function(sig)) => format!(
                "fn{}_{}ret_{}",
                sig.params.len(),
                helper_parts(
                    sig.params
                        .iter()
                        .map(|param| self.render_type(*param, mode))
                ),
                helper_part(self.render_type(sig.ret, mode))
            ),
            (TypeRender::Display, TypeData::Dyn(contract)) => {
                format!("dyn {}", contract.display_name)
            }
            (TypeRender::HelperKey, TypeData::Dyn(contract)) => {
                format!(
                    "dyn_{}",
                    helper_part(mangle_segment(&contract.method_table_key))
                )
            }
            (_, TypeData::Aggregate(id)) | (_, TypeData::DataRef(id)) => {
                let decl = self.aggregate(*id);
                self.render_named_type(
                    decl.module,
                    decl.name,
                    &decl.type_args,
                    &decl.const_args,
                    mode,
                )
            }
            (_, TypeData::Enum(id)) => {
                let decl = self.enum_decl(*id);
                self.render_named_type(
                    decl.module,
                    decl.name,
                    &decl.type_args,
                    &decl.const_args,
                    mode,
                )
            }
            (_, TypeData::Extern(id)) => {
                let decl = self.extern_type(*id);
                self.render_named_type(
                    decl.module,
                    decl.name,
                    &decl.type_args,
                    &decl.const_args,
                    mode,
                )
            }
        }
    }

    fn render_named_type(
        &self,
        module: ModuleId,
        name: Ident,
        type_args: &[TypeId],
        const_args: &[String],
        mode: TypeRender,
    ) -> String {
        let path = self
            .module(module)
            .path
            .iter()
            .chain(std::iter::once(&name))
            .map(ToString::to_string)
            .collect::<Vec<_>>();
        let base = match mode {
            TypeRender::Display => path.join("::"),
            TypeRender::HelperKey => path
                .iter()
                .map(|segment| mangle_segment(segment))
                .collect::<Vec<_>>()
                .join("_"),
        };
        if type_args.is_empty() && const_args.is_empty() {
            return base;
        }
        match mode {
            TypeRender::Display => format!(
                "{}<{}>",
                base,
                type_args
                    .iter()
                    .map(|arg| self.render_type(*arg, mode))
                    .chain(const_args.iter().cloned())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            TypeRender::HelperKey => format!(
                "{}__args_{}{}",
                base,
                helper_parts(type_args.iter().map(|arg| self.render_type(*arg, mode))),
                helper_parts(const_args.iter().map(|arg| mangle_segment(arg)))
            ),
        }
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

#[derive(Clone, Copy)]
enum TypeRender {
    Display,
    HelperKey,
}

fn helper_parts(parts: impl Iterator<Item = String>) -> String {
    let mut key = String::new();
    for part in parts {
        key.push_str(&helper_part(part));
    }
    key
}

fn helper_part(part: String) -> String {
    format!("{}_{part}", part.len())
}

fn mangle_segment(segment: &str) -> String {
    segment
        .as_bytes()
        .iter()
        .map(|byte| format!("{byte:02x}"))
        .collect()
}
