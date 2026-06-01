use anvyx_frontend::air::{
    self, ParamMode, Program as AirProgram, TypeData, TypeId, TypePassClass, TypePassClasses,
    VariantShape,
};

use super::rir::{RirEnumId, RirParamAbi, RirParamSemantic, RirProgram, RirType, RirTypeId};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustValueRep {
    InlineCopy,
    InlineArray,
    InlineStruct,
    InlineEnum,
    RawIntEnum,
    RawStringEnum,
    CowString,
    CowList,
    CowMap,
    Opaque,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RustBorrowView {
    Ref,
    Str,
    Slice,
    TargetGap,
}

impl RustBorrowView {
    pub fn render(self, value: String, borrowed_root: bool) -> String {
        match self {
            Self::Ref if borrowed_root => value,
            Self::Ref => format!("&{value}"),
            Self::Str | Self::Slice if borrowed_root => value,
            Self::Str => format!("{value}.as_str()"),
            Self::Slice => format!("&{value}"),
            Self::TargetGap => value,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AirRustRepPolicy<'a> {
    program: &'a AirProgram,
    classes: &'a TypePassClasses,
}

impl<'a> AirRustRepPolicy<'a> {
    pub fn new(program: &'a AirProgram, classes: &'a TypePassClasses) -> Self {
        Self { program, classes }
    }

    pub fn copyable(self, ty: TypeId) -> bool {
        if let TypeData::Optional(inner) = self.program.type_arena.data(ty) {
            return self.copyable(*inner);
        }
        self.classes.get(ty).is_some_and(|class| {
            matches!(
                class,
                TypePassClass::Immediate | TypePassClass::SmallCopyInline(_)
            )
        })
    }

    pub fn value_place_shareable(self, ty: TypeId) -> bool {
        self.copyable(ty) || self.shareable_value(ty)
    }

    pub fn list_supported(self, ty: TypeId) -> bool {
        let TypeData::List(elem) = self.program.type_arena.data(ty) else {
            return false;
        };
        self.value_place_shareable(*elem)
    }

    fn shareable_value(self, ty: TypeId) -> bool {
        match self.program.type_arena.data(ty) {
            TypeData::String | TypeData::List(_) | TypeData::Map { .. } => true,
            TypeData::Optional(_) => false,
            TypeData::Enum(id) => self.enum_shareable(*id),
            _ => false,
        }
    }

    fn enum_shareable(self, id: air::EnumId) -> bool {
        self.program.enum_decl(id).variants.iter().all(|variant| {
            let fields: &[TypeId] = match &variant.shape {
                VariantShape::Unit => &[],
                VariantShape::Tuple(fields) => fields,
                VariantShape::Struct(fields) => {
                    return fields
                        .iter()
                        .all(|field| self.value_place_shareable(field.ty));
                }
            };
            fields
                .iter()
                .all(|field| self.value_place_shareable(*field))
        })
    }

    pub fn map_supported(self, ty: TypeId) -> bool {
        let TypeData::Map { key, value, .. } = self.program.type_arena.data(ty) else {
            return false;
        };
        self.map_slot_supported(*key) && self.map_slot_supported(*value)
    }

    fn map_slot_supported(self, ty: TypeId) -> bool {
        matches!(
            self.program.type_arena.data(ty),
            TypeData::Int | TypeData::Float | TypeData::Bool | TypeData::String
        )
    }

    pub fn supports_param_mode(self, ty: TypeId, mode: ParamMode) -> bool {
        match mode {
            ParamMode::Value => matches!(
                self.program.type_arena.data(ty),
                TypeData::Int
                    | TypeData::Float
                    | TypeData::Bool
                    | TypeData::Void
                    | TypeData::String
                    | TypeData::Aggregate(_)
                    | TypeData::Enum(_)
                    | TypeData::Extern(_)
                    | TypeData::Array { .. }
                    | TypeData::List(_)
                    | TypeData::Map { .. }
            ),
            ParamMode::SharedBorrow => matches!(
                self.program.type_arena.data(ty),
                TypeData::String
                    | TypeData::Aggregate(_)
                    | TypeData::Enum(_)
                    | TypeData::Extern(_)
                    | TypeData::Array { .. }
                    | TypeData::List(_)
                    | TypeData::Map { .. }
            ),
            ParamMode::MutBorrow => matches!(
                self.program.type_arena.data(ty),
                TypeData::Extern(_) | TypeData::List(_) | TypeData::Map { .. }
            ),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct RustRepPolicy<'a> {
    program: &'a RirProgram,
}

impl<'a> RustRepPolicy<'a> {
    pub fn new(program: &'a RirProgram) -> Self {
        Self { program }
    }

    pub fn value_rep(self, ty: RirTypeId) -> RustValueRep {
        match self.ty(ty) {
            RirType::Int | RirType::Float | RirType::Bool | RirType::Void => {
                RustValueRep::InlineCopy
            }
            RirType::String => RustValueRep::CowString,
            RirType::Array { .. } => RustValueRep::InlineArray,
            RirType::List(_) => RustValueRep::CowList,
            RirType::Map { .. } => RustValueRep::CowMap,
            RirType::Option(_) => RustValueRep::InlineEnum,
            RirType::Slice(_) => RustValueRep::Opaque,
            RirType::Struct(_) => RustValueRep::InlineStruct,
            RirType::Enum(id) => self.enum_rep(id),
        }
    }

    pub fn cow_value(self, ty: RirTypeId) -> bool {
        matches!(
            self.value_rep(ty),
            RustValueRep::CowString | RustValueRep::CowList | RustValueRep::CowMap
        )
    }

    pub fn param_abi(self, semantic: RirParamSemantic) -> RirParamAbi {
        match semantic {
            RirParamSemantic::Value => RirParamAbi::Value,
            RirParamSemantic::SharedBorrow => RirParamAbi::SharedBorrow,
            RirParamSemantic::MutBorrow => RirParamAbi::MutBorrow,
        }
    }

    pub fn supports_param(self, ty: RirTypeId, semantic: RirParamSemantic) -> bool {
        self.ty_opt(ty)
            .is_some_and(|ty_data| self.supports_type_semantic(ty_data, semantic))
    }

    pub fn call_arg_abi(self, ty: RirTypeId, semantic: RirParamSemantic) -> Option<RirParamAbi> {
        self.supports_param(ty, semantic)
            .then(|| self.param_abi(semantic))
    }

    pub fn shareable_value(self, ty: RirTypeId) -> bool {
        self.copyable(ty)
            || match self.ty(ty) {
                RirType::String | RirType::List(_) | RirType::Map { .. } => true,
                RirType::Option(_) => false,
                RirType::Enum(id) => {
                    self.program.enums[id.index()]
                        .variants
                        .iter()
                        .all(|variant| {
                            variant
                                .fields
                                .iter()
                                .all(|field| self.shareable_value(field.ty))
                        })
                }
                _ => false,
            }
    }

    pub fn borrow_view(self, ty: RirTypeId) -> RustBorrowView {
        match self.ty(ty) {
            RirType::String => RustBorrowView::Str,
            RirType::Slice(_) => RustBorrowView::Slice,
            RirType::Void => RustBorrowView::TargetGap,
            _ => RustBorrowView::Ref,
        }
    }

    pub fn rust_ty(self, ty: RirTypeId) -> String {
        match self.ty(ty) {
            RirType::Int => "i64".into(),
            RirType::Float => "f64".into(),
            RirType::Bool => "bool".into(),
            RirType::String => "anvyx_runtime::AnvString".into(),
            RirType::Void => "()".into(),
            RirType::Struct(id) => self.program.structs[id.index()].symbol.as_str().into(),
            RirType::Enum(id) => self.program.enums[id.index()].symbol.as_str().into(),
            RirType::Array { elem, len } => format!("[{}; {len}]", self.rust_ty(elem)),
            RirType::List(elem) => format!("anvyx_runtime::AnvList<{}>", self.rust_ty(elem)),
            RirType::Map { key, value } => format!(
                "anvyx_runtime::AnvMap<{}, {}>",
                self.rust_ty(key),
                self.rust_ty(value)
            ),
            RirType::Option(inner) => format!("Option<{}>", self.rust_ty(inner)),
            RirType::Slice(elem) => format!("&[{}]", self.rust_ty(elem)),
        }
    }

    pub fn copyable(self, ty: RirTypeId) -> bool {
        match self.ty(ty) {
            RirType::Int | RirType::Float | RirType::Bool | RirType::Void => true,
            RirType::String | RirType::List(_) | RirType::Map { .. } | RirType::Slice(_) => false,
            RirType::Option(inner) => self.copyable(inner),
            RirType::Array { elem, .. } => self.copyable(elem),
            RirType::Struct(id) => self.program.structs[id.index()].copyable,
            RirType::Enum(id) => self.program.enums[id.index()].copyable,
        }
    }

    fn supports_type_semantic(self, ty: RirType, semantic: RirParamSemantic) -> bool {
        match semantic {
            RirParamSemantic::Value => matches!(
                ty,
                RirType::Int
                    | RirType::Float
                    | RirType::Bool
                    | RirType::Void
                    | RirType::String
                    | RirType::Struct(_)
                    | RirType::Enum(_)
                    | RirType::Array { .. }
                    | RirType::List(_)
                    | RirType::Map { .. }
            ),
            RirParamSemantic::SharedBorrow => matches!(
                ty,
                RirType::String
                    | RirType::Struct(_)
                    | RirType::Enum(_)
                    | RirType::Array { .. }
                    | RirType::List(_)
                    | RirType::Map { .. }
            ),
            RirParamSemantic::MutBorrow => match ty {
                RirType::List(_) | RirType::Map { .. } => true,
                RirType::Struct(id) => self.program.structs[id.index()].native_path.is_some(),
                _ => false,
            },
        }
    }

    fn enum_rep(self, id: RirEnumId) -> RustValueRep {
        let enum_ = &self.program.enums[id.index()];
        match enum_.raw_type {
            Some(ty) => match self.ty(ty) {
                RirType::String => RustValueRep::RawStringEnum,
                _ => RustValueRep::RawIntEnum,
            },
            None => RustValueRep::InlineEnum,
        }
    }

    fn ty(self, ty: RirTypeId) -> RirType {
        self.program.types[ty.index()]
    }

    fn ty_opt(self, ty: RirTypeId) -> Option<RirType> {
        self.program.types.get(ty.index()).copied()
    }
}

#[cfg(test)]
mod tests {
    use anvyx_frontend::air::{ParamMode, Program, TypeData, TypePassClasses};

    use super::{AirRustRepPolicy, RustBorrowView, RustRepPolicy, RustValueRep};
    use crate::rust::rir::{RirProgram, RirType, RirTypeId};

    #[test]
    fn policy_classifies_current_storage_reps() {
        let mut program = RirProgram::default();
        let int = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::Int);
        let string = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::String);
        let list = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::List(int));
        let array = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::Array { elem: int, len: 2 });
        let policy = RustRepPolicy::new(&program);

        assert_eq!(policy.value_rep(int), RustValueRep::InlineCopy);
        assert_eq!(policy.value_rep(string), RustValueRep::CowString);
        assert_eq!(policy.value_rep(list), RustValueRep::CowList);
        assert_eq!(policy.value_rep(array), RustValueRep::InlineArray);
    }

    #[test]
    fn air_policy_preserves_current_profile_param_modes() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let string = program.alloc_type(TypeData::String);
        let list = program.alloc_type(TypeData::List(int));
        let classes = TypePassClasses::analyze(&program);
        let policy = AirRustRepPolicy::new(&program, &classes);

        assert!(policy.supports_param_mode(int, ParamMode::Value));
        assert!(policy.supports_param_mode(string, ParamMode::SharedBorrow));
        assert!(policy.supports_param_mode(string, ParamMode::Value));
        assert!(policy.supports_param_mode(list, ParamMode::Value));
        assert!(policy.supports_param_mode(list, ParamMode::MutBorrow));
    }

    #[test]
    fn policy_keeps_string_borrow_as_str_view() {
        let mut program = RirProgram::default();
        let string = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::String);
        let policy = RustRepPolicy::new(&program);

        assert_eq!(policy.borrow_view(string), RustBorrowView::Str);
        assert!(policy.cow_value(string));
    }
}
