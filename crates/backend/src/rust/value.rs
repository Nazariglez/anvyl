use super::{
    place::RustPlaces,
    rep_policy::{RustBorrowView, RustRepPolicy},
    rir::{
        RirCallArg, RirCellRef, RirConst, RirConstValue, RirEnum, RirField, RirFunction,
        RirMutPlaceArg, RirOperand, RirParamSemantic, RirPlace, RirProgram, RirScopedPlaceCellRef,
        RirType, RirTypeId, RirVariant, RirVariantKind,
    },
    syntax::{
        comma, field_init, match_expr, rust_string, struct_lit, struct_variant, tuple_variant,
        variant_path,
    },
    target,
};

pub(super) struct RustValues<'a> {
    program: &'a RirProgram,
    function: &'a RirFunction,
    policy: RustRepPolicy<'a>,
    places: RustPlaces<'a>,
}

impl<'a> RustValues<'a> {
    pub(super) fn new(program: &'a RirProgram, function: &'a RirFunction) -> Self {
        Self {
            program,
            function,
            policy: RustRepPolicy::new(program),
            places: RustPlaces::new(program, function),
        }
    }

    pub(super) fn call_arg(&self, arg: &RirCallArg) -> String {
        match arg {
            RirCallArg::Value(operand) => self.value_operand(operand),
            RirCallArg::SharedBorrow(place) => self.borrow_arg(place),
            RirCallArg::SharedStringConst(id) => match &self.program.consts[id.index()].value {
                RirConstValue::String(value) => rust_string(value),
                _ => unreachable!("verified shared string const"),
            },
            RirCallArg::MutBorrow(place) => self.mut_borrow_arg(place),
            RirCallArg::MutPlace(arg) => self.mut_place_arg(arg),
        }
    }

    pub(super) fn borrow_arg(&self, place: &RirPlace) -> String {
        let place_expr = self.places.local_place(place);
        let borrowed_root = self.places.shared_borrow_root_param(place);
        match self.policy.borrow_view(place.ty) {
            RustBorrowView::Ref if borrowed_root => place_expr,
            RustBorrowView::Ref => format!("&{place_expr}"),
            RustBorrowView::Str | RustBorrowView::Slice if borrowed_root => place_expr,
            RustBorrowView::Str => format!("{place_expr}.as_str()"),
            RustBorrowView::Slice => format!("&{place_expr}"),
            RustBorrowView::TargetGap => place_expr,
        }
    }

    pub(super) fn mut_borrow_arg(&self, place: &RirPlace) -> String {
        format!("&mut {}", self.place(place))
    }

    pub(super) fn mut_place_arg(&self, arg: &RirMutPlaceArg) -> String {
        match arg {
            RirMutPlaceArg::Local(place) => {
                format!(
                    "{}::local(&mut {})",
                    target::mut_place_ty(),
                    self.place(place)
                )
            }
            RirMutPlaceArg::Param { local, .. } => {
                format!(
                    "{}.reborrow()",
                    self.function.locals[local.index()].symbol.as_str()
                )
            }
            RirMutPlaceArg::StackCell { cell, .. } => {
                format!(
                    "{}::stack_cell(&{})",
                    target::mut_place_ty(),
                    self.cell_ref(*cell)
                )
            }
            RirMutPlaceArg::ScopedPlaceCell { cell, .. } => {
                let cell = match cell {
                    RirScopedPlaceCellRef::Owner(_) => {
                        format!("&{}", self.scoped_place_cell_ref(*cell))
                    }
                    RirScopedPlaceCellRef::Capture { .. } => self.scoped_place_cell_ref(*cell),
                };
                format!("{}::scoped_cell({cell})", target::mut_place_ty())
            }
        }
    }

    fn cell_ref(&self, cell: RirCellRef) -> String {
        match cell {
            RirCellRef::Owner(cell) => self.program.cells[cell.index()].symbol.as_str().to_string(),
            RirCellRef::Capture { local, .. } => self.function.locals[local.index()]
                .symbol
                .as_str()
                .to_string(),
        }
    }

    fn scoped_place_cell_ref(&self, cell: RirScopedPlaceCellRef) -> String {
        match cell {
            RirScopedPlaceCellRef::Owner(cell) => self.program.scoped_place_cells[cell.index()]
                .symbol
                .as_str()
                .to_string(),
            RirScopedPlaceCellRef::Capture { local, .. } => self.function.locals[local.index()]
                .symbol
                .as_str()
                .to_string(),
        }
    }

    pub(super) fn operand_ref(&self, operand: &RirOperand) -> String {
        match operand {
            RirOperand::Place(place) => {
                let place_expr = self.place(place);
                if self.places.shared_borrow_root_param(place) {
                    place_expr
                } else {
                    format!("&{place_expr}")
                }
            }
            RirOperand::Const(_) => format!("&{}", self.operand(operand)),
        }
    }

    pub(super) fn value_operand(&self, operand: &RirOperand) -> String {
        let RirOperand::Place(place) = operand else {
            return self.operand(operand);
        };
        if self.places.mut_place_root_param(place) {
            return self.mut_place_value_operand(place);
        }
        let place_expr = self.place(place);
        if self.policy.cow_value(place.ty) {
            if self.places.shared_borrow_root_param(place)
                && matches!(self.program.types[place.ty.index()], RirType::String)
            {
                return target::anv_string_from(&place_expr);
            }
            return format!("{place_expr}.share()");
        }
        if !self.policy.copyable(place.ty) && self.policy.shareable_value(place.ty) {
            return self.value_from_place(place.ty, &place_expr);
        }
        self.operand(operand)
    }

    pub(super) fn operand(&self, operand: &RirOperand) -> String {
        match operand {
            RirOperand::Place(place) if self.places.mut_place_root_param(place) => {
                self.mut_place_value_operand(place)
            }
            RirOperand::Place(place) => match self.program.types[place.ty.index()] {
                RirType::Struct(id) if self.program.structs[id.index()].copyable => {
                    self.copy_struct_place(place)
                }
                RirType::Enum(id) if self.program.enums[id.index()].copyable => {
                    self.copy_enum_place(place)
                }
                RirType::Tuple(id) if self.program.tuples[id.index()].copyable => {
                    self.copy_tuple_place(place)
                }
                RirType::Array { .. } => self.copy_array_place(place),
                _ => self.place(place),
            },
            RirOperand::Const(id) => self.const_value(&self.program.consts[id.index()]),
        }
    }

    fn place(&self, place: &RirPlace) -> String {
        self.places.local_place(place)
    }

    fn mut_place_value_operand(&self, place: &RirPlace) -> String {
        self.place_value_from_access(place.ty, &self.place(place))
    }

    pub(super) fn scoped_place_cell_value(
        &self,
        cell: RirScopedPlaceCellRef,
        ty: RirTypeId,
    ) -> String {
        self.place_value_from_access(ty, &self.scoped_place_cell_ref(cell))
    }

    fn place_value_from_access(&self, ty: RirTypeId, expr: &str) -> String {
        if self.policy.cow_value(ty) {
            return format!("{expr}.access(|value| Ok(value.share()))?");
        }
        if !self.policy.copyable(ty) && self.policy.shareable_value(ty) {
            let value = self.value_from_ref(ty, "value");
            return format!("{expr}.access(|value| Ok({value}))?");
        }
        format!("{expr}.get_copy()?")
    }

    pub(super) fn value_from_ref(&self, ty: RirTypeId, expr: &str) -> String {
        self.copy_from_ref(ty, expr)
    }

    pub(super) fn value_from_place(&self, ty: RirTypeId, expr: &str) -> String {
        match self.program.types[ty.index()] {
            RirType::Int | RirType::Float | RirType::Bool => expr.to_string(),
            RirType::String | RirType::List(_) | RirType::Map { .. } => format!("{expr}.share()"),
            RirType::DataRef(_) => format!("{expr}.clone()"),
            RirType::Lambda(sig) if self.policy.lambda_sig_has_heap_env(sig) => {
                format!("{expr}.clone()")
            }
            RirType::Struct(_) | RirType::Tuple(_) | RirType::Array { .. } | RirType::Enum(_) => {
                self.copy_from_ref(ty, &format!("&{expr}"))
            }
            RirType::Option(_) => self.copy_from_ref(ty, &format!("&{expr}")),
            RirType::Slice(_) | RirType::Lambda(_) | RirType::Void => {
                unreachable!("verified dataref field value")
            }
        }
    }

    pub(super) fn native_return_call(
        &self,
        ret: RirTypeId,
        abi: &anvyx_runtime::RustReturnAbi,
        call: String,
    ) -> String {
        match abi {
            anvyx_runtime::RustReturnAbi::Value(ty) => self.native_value_return(ty, &call),
            anvyx_runtime::RustReturnAbi::Option(inner) => {
                self.option_return_call(ret, inner, call)
            }
            _ => call,
        }
    }

    pub(super) fn stringify_arg(&self, mode: RirParamSemantic, value: &RirOperand) -> String {
        match mode {
            RirParamSemantic::Value => self.operand(value),
            RirParamSemantic::SharedBorrow => {
                let RirOperand::Place(place) = value else {
                    unreachable!("verified stringify override place")
                };
                self.borrow_arg(place)
            }
            RirParamSemantic::MutBorrow
            | RirParamSemantic::MutPlace
            | RirParamSemantic::StackCell
            | RirParamSemantic::ScopedPlaceCell => unreachable!("verified stringify override mode"),
        }
    }

    pub(super) fn string_arg(&self, operand: &RirOperand) -> String {
        match operand {
            RirOperand::Const(id) => {
                let konst = &self.program.consts[id.index()];
                match &konst.value {
                    RirConstValue::String(value) => rust_string(value),
                    _ => format!("{}.as_str()", self.const_value(konst)),
                }
            }
            RirOperand::Place(place) => self.borrow_arg(place),
        }
    }

    pub(super) fn format_arg(&self, operand: &RirOperand, source_ty: RirTypeId) -> String {
        if matches!(self.program.types[source_ty.index()], RirType::String) {
            self.string_arg(operand)
        } else {
            self.operand(operand)
        }
    }

    pub(super) fn const_value(&self, konst: &RirConst) -> String {
        match &konst.value {
            RirConstValue::Int(value) => value.to_string(),
            RirConstValue::Float(value) => {
                let text = value.to_string();
                if text.contains('.') {
                    text
                } else {
                    format!("{text}.0")
                }
            }
            RirConstValue::Bool(value) => value.to_string(),
            RirConstValue::String(value) => target::anv_string_from(&rust_string(value)),
            RirConstValue::Nil => "None".into(),
        }
    }

    fn option_return_call(
        &self,
        ret: RirTypeId,
        inner: &anvyx_runtime::RustReturnAbi,
        call: String,
    ) -> String {
        let RirType::Option(_) = self.program.types[ret.index()] else {
            unreachable!("verified native option return type")
        };
        let value = match inner {
            anvyx_runtime::RustReturnAbi::Value(ty) => self.native_value_return(ty, "value"),
            _ => unreachable!("verified native option return inner"),
        };
        match_expr(
            &call,
            [
                format!("Some(value) => Some({value})"),
                "None => None".to_string(),
            ],
        )
    }

    fn native_value_return(&self, ty: &anvyx_runtime::ExternTypeExpr, expr: &str) -> String {
        match ty {
            anvyx_runtime::ExternTypeExpr::String => target::anv_string_from(expr),
            _ => expr.to_string(),
        }
    }

    fn copy_struct_place(&self, place: &RirPlace) -> String {
        let RirType::Struct(struct_id) = self.program.types[place.ty.index()] else {
            unreachable!("verified struct copy place")
        };
        let strukt = &self.program.structs[struct_id.index()];
        self.copy_record_place(place, strukt.symbol.as_str(), &strukt.fields)
    }

    fn copy_tuple_place(&self, place: &RirPlace) -> String {
        let RirType::Tuple(tuple_id) = self.program.types[place.ty.index()] else {
            unreachable!("verified tuple copy place")
        };
        let tuple = &self.program.tuples[tuple_id.index()];
        self.copy_record_place(place, tuple.symbol.as_str(), &tuple.fields)
    }

    fn copy_record_place(&self, place: &RirPlace, symbol: &str, fields: &[RirField]) -> String {
        let fields = fields.iter().map(|field| {
            let field_place = self.places.record_field_place(place, field);
            field_init(
                field.symbol.as_str(),
                self.operand(&RirOperand::Place(field_place)),
            )
        });
        struct_lit(symbol, fields)
    }

    fn copy_array_place(&self, place: &RirPlace) -> String {
        let RirType::Array { elem, len } = self.program.types[place.ty.index()] else {
            unreachable!("verified array copy place")
        };
        let source = self.place(place);
        let elems =
            comma((0..len).map(|index| self.copy_from_ref(elem, &format!("&{source}[{index}]"))));
        format!("[{elems}]")
    }

    fn copy_enum_place(&self, place: &RirPlace) -> String {
        let source = self.place(place);
        self.copy_enum_expr(&source, place.ty)
    }

    fn copy_enum_expr(&self, source: &str, ty: RirTypeId) -> String {
        self.copy_enum_ref_expr(&format!("&{source}"), ty)
    }

    fn copy_enum_ref_expr(&self, source: &str, ty: RirTypeId) -> String {
        let RirType::Enum(enum_id) = self.program.types[ty.index()] else {
            unreachable!("verified enum copy expression")
        };
        let enm = &self.program.enums[enum_id.index()];
        let arms = enm
            .variants
            .iter()
            .map(|variant| self.copy_enum_variant_arm(enm, variant));
        match_expr(source, arms)
    }

    fn copy_enum_variant_arm(&self, enm: &RirEnum, variant: &RirVariant) -> String {
        let path = variant_path(enm.symbol.as_str(), variant.symbol.as_str());
        match variant.kind {
            RirVariantKind::Unit => format!("{path} => {path}"),
            RirVariantKind::Tuple => {
                let vars = (0..variant.fields.len())
                    .map(|index| format!("f{index}"))
                    .collect::<Vec<_>>();
                let values = variant
                    .fields
                    .iter()
                    .zip(&vars)
                    .map(|(field, var)| self.value_from_ref(field.ty, var));
                format!(
                    "{} => {}",
                    tuple_variant(&path, vars.iter().cloned()),
                    tuple_variant(&path, values)
                )
            }
            RirVariantKind::Struct => {
                let vars = variant
                    .fields
                    .iter()
                    .map(|field| field.symbol.as_str().to_string())
                    .collect::<Vec<_>>();
                let values = variant.fields.iter().zip(&vars).map(|(field, var)| {
                    field_init(field.symbol.as_str(), self.value_from_ref(field.ty, var))
                });
                format!(
                    "{} => {}",
                    struct_variant(&path, vars.iter().cloned()),
                    struct_variant(&path, values)
                )
            }
        }
    }

    fn copy_from_ref(&self, ty: RirTypeId, expr: &str) -> String {
        match self.program.types[ty.index()] {
            RirType::Int | RirType::Float | RirType::Bool => format!("*({expr})"),
            RirType::Lambda(sig) if self.policy.lambda_sig_has_heap_env(sig) => {
                format!("({expr}).clone()")
            }
            RirType::Lambda(_) => format!("*({expr})"),
            RirType::Struct(id) => {
                let strukt = &self.program.structs[id.index()];
                self.copy_record_from_ref(strukt.symbol.as_str(), &strukt.fields, expr)
            }
            RirType::Array { elem, len } => {
                let elems = comma(
                    (0..len).map(|index| self.value_from_ref(elem, &format!("&({expr})[{index}]"))),
                );
                format!("[{elems}]")
            }
            RirType::Tuple(id) => {
                let tuple = &self.program.tuples[id.index()];
                self.copy_record_from_ref(tuple.symbol.as_str(), &tuple.fields, expr)
            }
            RirType::Enum(id) if self.program.enums[id.index()].variants.is_empty() => {
                format!("match *({expr}) {{}}")
            }
            RirType::Enum(_) => self.copy_enum_ref_expr(expr, ty),
            RirType::Option(inner) => format!(
                "({expr}).as_ref().map(|value| {})",
                self.value_from_ref(inner, "value")
            ),
            RirType::DataRef(_) => format!("(*({expr})).clone()"),
            RirType::String | RirType::List(_) | RirType::Map { .. } => {
                format!("(*({expr})).share()")
            }
            RirType::Slice(_) | RirType::Void => unreachable!("verified copy enum payload"),
        }
    }

    fn copy_record_from_ref(&self, symbol: &str, fields: &[RirField], expr: &str) -> String {
        let fields = fields.iter().map(|field| {
            let field_expr = format!("&({expr}).{}", field.symbol.as_str());
            field_init(
                field.symbol.as_str(),
                self.value_from_ref(field.ty, &field_expr),
            )
        });
        struct_lit(symbol, fields)
    }
}
