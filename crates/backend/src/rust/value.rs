use super::{
    place::{MutPlaceProjection, MutPlaceProjectionStep, RustPlaces, projected_ops_ctor},
    rep_policy::{RustBorrowView, RustRepPolicy},
    rir::{
        RirCallArg, RirCellRef, RirConst, RirConstValue, RirEnum, RirField, RirFunction,
        RirMutPlaceArg, RirMutPlaceRoot, RirOperand, RirParamSemantic, RirPlace, RirProgram,
        RirScopedPlaceCellRef, RirType, RirTypeId, RirVariant, RirVariantKind,
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

pub(super) struct ProjectedPlaceDescriptor {
    pub(super) struct_decl: String,
    pub(super) ctor: String,
    pub(super) impl_decl: String,
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
        debug_assert!(arg.projections.is_empty());
        self.mut_place_root_arg(&arg.root)
            .expect("dataref mut-place args must be prepared before rendering")
            .1
    }

    pub(super) fn mut_place_root_arg(&self, root: &RirMutPlaceRoot) -> Option<(RirTypeId, String)> {
        match root {
            RirMutPlaceRoot::Local { local, ty } => Some((
                *ty,
                target::mut_place_local(&self.place(&RirPlace {
                    local: *local,
                    projections: vec![],
                    ty: *ty,
                })),
            )),
            RirMutPlaceRoot::Param { local, ty } => Some((
                *ty,
                target::mut_place_reborrow(self.function.locals[local.index()].symbol.as_str()),
            )),
            RirMutPlaceRoot::StackCell { cell, ty } => {
                Some((*ty, target::mut_place_stack_cell(&self.cell_ref(*cell))))
            }
            RirMutPlaceRoot::HeapCell { cell, ty } => {
                Some((*ty, target::mut_place_heap_cell(&self.cell_ref(*cell))))
            }
            RirMutPlaceRoot::ScopedPlaceCell { cell, ty } => {
                let cell = match cell {
                    RirScopedPlaceCellRef::Owner(_) => {
                        format!("&{}", self.scoped_place_cell_ref(*cell))
                    }
                    RirScopedPlaceCellRef::Capture { .. } => self.scoped_place_cell_ref(*cell),
                };
                Some((*ty, target::mut_place_scoped_cell(&cell)))
            }
            RirMutPlaceRoot::DataRef { .. } => None,
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
        if self.places.mut_place_projection(place).is_some() {
            return self.mut_place_projected_value_operand(place);
        }
        if let Some(value) = self.map_slot_value_operand(place) {
            return value;
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

    pub(super) fn operand_ty(&self, operand: &RirOperand) -> RirTypeId {
        match operand {
            RirOperand::Place(place) => place.ty,
            RirOperand::Const(id) => self.program.consts[id.index()].ty,
        }
    }

    pub(super) fn operand(&self, operand: &RirOperand) -> String {
        match operand {
            RirOperand::Place(place) if self.places.mut_place_root_param(place) => {
                self.mut_place_value_operand(place)
            }
            RirOperand::Place(place) if self.places.mut_place_projection(place).is_some() => {
                self.mut_place_projected_value_operand(place)
            }
            RirOperand::Place(place) => {
                if let Some(value) = self.map_slot_value_operand(place) {
                    return value;
                }
                match self.program.types[place.ty.index()] {
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
                }
            }
            RirOperand::Const(id) => self.const_value(&self.program.consts[id.index()]),
        }
    }

    fn place(&self, place: &RirPlace) -> String {
        self.places.local_place(place)
    }

    fn mut_place_value_operand(&self, place: &RirPlace) -> String {
        self.place_value_from_access(place.ty, &self.place(place))
    }

    fn map_slot_value_operand(&self, place: &RirPlace) -> Option<String> {
        let access = self.places.map_slot_access(place)?;
        let value = self.value_from_ref(access.value_ty, "value");
        Some(format!(
            "{}.get(&{}).map(|value| {value})",
            access.map, access.key
        ))
    }

    pub(super) fn mut_place_projected_set(&self, place: &RirPlace, value: &str) -> Option<String> {
        let projection = self.places.mut_place_projection(place)?;
        let set = if self.collection_replace_ty(place.ty) {
            target::mut_place_replace_collection(
                "__anv_place",
                &target::ctx_runtime("ctx"),
                "__anv_value",
            )
        } else {
            target::mut_place_set("__anv_place", &target::ctx_runtime("ctx"), "__anv_value")
        };
        Some(self.mut_place_projected_region(
            &projection,
            &format!("let __anv_value = {value};"),
            &set,
        ))
    }

    fn mut_place_projected_value_operand(&self, place: &RirPlace) -> String {
        let projection = self
            .places
            .mut_place_projection(place)
            .expect("checked mut-place projection");
        self.mut_place_projected_region(
            &projection,
            "",
            &self.place_value_from_access(place.ty, "__anv_place"),
        )
    }

    fn collection_replace_ty(&self, ty: RirTypeId) -> bool {
        matches!(
            self.program.types[ty.index()],
            RirType::List(_) | RirType::Map { .. }
        )
    }

    fn mut_place_projected_region(
        &self,
        projection: &MutPlaceProjection,
        before_place: &str,
        body: &str,
    ) -> String {
        let descriptor = self.mut_place_projection_descriptor("__AnvProjectedPlaceOps", projection);
        format!(
            "{{ {before_place} {} {} let __anv_ops = {}; let mut __anv_place = {}; {body} }}",
            descriptor.struct_decl,
            descriptor.impl_decl,
            descriptor.ctor,
            target::mut_place_projected(
                &target::mut_place_reborrow(&projection.root),
                "&__anv_ops"
            ),
        )
    }

    pub(super) fn mut_place_projection_descriptor_for(
        &self,
        ops: &str,
        root_ty: RirTypeId,
        root: &str,
        slot_ty: RirTypeId,
        projections: &[super::rir::RirProjection],
    ) -> ProjectedPlaceDescriptor {
        let projection = self
            .places
            .projected_place(root_ty, root, slot_ty, projections)
            .expect("verified projected place descriptor");
        self.mut_place_projection_descriptor(ops, &projection)
    }

    pub(super) fn mut_place_projection_descriptor(
        &self,
        ops: &str,
        projection: &MutPlaceProjection,
    ) -> ProjectedPlaceDescriptor {
        let struct_decl = if projection.fields.is_empty() {
            format!("struct {ops};")
        } else {
            format!(
                "struct {ops} {{ {} }}",
                comma(projection.fields.iter().cloned())
            )
        };
        let root_ty = self.policy.rust_ty(projection.root_ty);
        let slot_ty = self.policy.rust_ty(projection.slot_ty);
        ProjectedPlaceDescriptor {
            struct_decl,
            ctor: projected_ops_ctor(ops, &projection.inits),
            impl_decl: self.mut_place_projection_ops_impl(
                ops,
                &root_ty,
                &slot_ty,
                &projection.steps,
            ),
        }
    }

    fn mut_place_projection_ops_impl(
        &self,
        ops: &str,
        root_ty: &str,
        slot_ty: &str,
        steps: &[MutPlaceProjectionStep],
    ) -> String {
        let access = self.projected_access("root", true, steps);
        let mutate = self.projected_mutate("root", true, steps);
        format!(
            "impl<'cx> {} for {ops} {{ fn access(&self, root: &{root_ty}, f: &mut dyn FnMut(&{slot_ty}) -> {}) -> {} {{ {access} }} fn mutate(&self, root: &mut {root_ty}, f: &mut dyn FnMut(&mut {slot_ty}) -> {}) -> {} {{ {mutate} }} }}",
            target::projection_ops_ty(root_ty, slot_ty),
            target::result_ty("()"),
            target::result_ty("()"),
            target::result_ty("()"),
            target::result_ty("()"),
        )
    }

    fn projected_access(
        &self,
        expr: &str,
        by_ref: bool,
        steps: &[MutPlaceProjectionStep],
    ) -> String {
        let Some((step, rest)) = steps.split_first() else {
            return if by_ref {
                format!("f({expr})")
            } else {
                format!("f(&{expr})")
            };
        };
        match step {
            MutPlaceProjectionStep::Field(field) => {
                self.projected_access(&format!("{expr}.{field}"), false, rest)
            }
            MutPlaceProjectionStep::ArrayIndex { index, len } => {
                let checked = target::checked_index_result(
                    &format!("self.{index}"),
                    &len.to_string(),
                    "array",
                );
                let body = self.projected_access(&format!("{expr}[index]"), false, rest);
                format!("{{ let index = {checked}; {body} }}")
            }
            MutPlaceProjectionStep::ListIndex { index, version } => {
                let checked = target::checked_index_result(
                    &format!("self.{index}"),
                    &format!("{expr}.len()"),
                    "list",
                );
                let body = self.projected_access("value", true, rest);
                let access = target::list_with_elem_shared_short(
                    expr,
                    "index",
                    &format!("self.{version}"),
                    &body,
                );
                format!("{{ let index = {checked}; {access} }}")
            }
            MutPlaceProjectionStep::SliceIndex { index } => {
                let body = self.projected_access("value", false, rest);
                format!("{{ let value = {expr}.elem_at_shared(self.{index})?; {body} }}")
            }
            MutPlaceProjectionStep::MapIndex { key, value_ty } => {
                debug_assert!(rest.is_empty());
                let value = self.value_from_ref(*value_ty, "value");
                format!("{{ let value = {expr}.get(&self.{key}).map(|value| {value}); f(&value) }}")
            }
        }
    }

    fn projected_mutate(
        &self,
        expr: &str,
        by_ref: bool,
        steps: &[MutPlaceProjectionStep],
    ) -> String {
        let Some((step, rest)) = steps.split_first() else {
            return if by_ref {
                format!("f({expr})")
            } else {
                format!("f(&mut {expr})")
            };
        };
        match step {
            MutPlaceProjectionStep::Field(field) => {
                self.projected_mutate(&format!("{expr}.{field}"), false, rest)
            }
            MutPlaceProjectionStep::ArrayIndex { index, len } => {
                let checked = target::checked_index_result(
                    &format!("self.{index}"),
                    &len.to_string(),
                    "array",
                );
                let body = self.projected_mutate(&format!("{expr}[index]"), false, rest);
                format!("{{ let index = {checked}; {body} }}")
            }
            MutPlaceProjectionStep::ListIndex { index, version } => {
                let checked = target::checked_index_result(
                    &format!("self.{index}"),
                    &format!("{expr}.len()"),
                    "list",
                );
                let body = self.projected_mutate("value", true, rest);
                format!(
                    "{{ let index = {checked}; {expr}.with_elem_mut_short(index, self.{version}, |value| {{ {body} }}) }}"
                )
            }
            MutPlaceProjectionStep::SliceIndex { index } => {
                let body = self.projected_mutate("value", true, rest);
                format!("{expr}.with_elem_mut_short(self.{index}, |value| {{ {body} }})")
            }
            MutPlaceProjectionStep::MapIndex { key, value_ty } => {
                debug_assert!(rest.is_empty());
                let value = self.value_from_ref(*value_ty, "value");
                let set = target::map_optional_slot_set(expr, &format!("self.{key}"), "slot");
                format!(
                    "{{ let mut slot = {expr}.get(&self.{key}).map(|value| {value}); f(&mut slot)?; {set} }}"
                )
            }
        }
    }

    pub(super) fn scoped_place_cell_value(
        &self,
        cell: RirScopedPlaceCellRef,
        ty: RirTypeId,
    ) -> String {
        self.place_value_from_access(ty, &self.scoped_place_cell_ref(cell))
    }

    fn place_value_from_access(&self, ty: RirTypeId, expr: &str) -> String {
        let runtime = target::ctx_runtime("ctx");
        if self.policy.cow_value(ty) {
            return target::mut_place_access(expr, &runtime, "Ok(value.share())");
        }
        if !self.policy.copyable(ty) && self.policy.shareable_value(ty) {
            let value = self.value_from_ref(ty, "value");
            return target::mut_place_access(expr, &runtime, &format!("Ok({value})"));
        }
        target::mut_place_get_copy(expr, &runtime)
    }

    pub(super) fn value_from_ref(&self, ty: RirTypeId, expr: &str) -> String {
        self.copy_from_ref(ty, expr)
    }

    pub(super) fn value_from_place(&self, ty: RirTypeId, expr: &str) -> String {
        match self.program.types[ty.index()] {
            RirType::Int | RirType::Float | RirType::Bool => expr.to_string(),
            RirType::String | RirType::List(_) | RirType::Map { .. } => format!("{expr}.share()"),
            RirType::DataRef(_) => format!("{expr}.clone()"),
            RirType::Slice(_) => format!("{expr}.readonly()"),
            RirType::Lambda(sig) if !self.policy.lambda_sig_copyable(sig) => {
                format!("{expr}.clone()")
            }
            RirType::Struct(_) | RirType::Tuple(_) | RirType::Array { .. } | RirType::Enum(_) => {
                self.copy_from_ref(ty, &format!("&{expr}"))
            }
            RirType::Option(_) => self.copy_from_ref(ty, &format!("&{expr}")),
            RirType::Lambda(_) | RirType::Void => unreachable!("verified dataref field value"),
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
            | RirParamSemantic::HeapCell
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
            RirType::Lambda(sig) if self.policy.lambda_sig_copyable(sig) => format!("*({expr})"),
            RirType::Lambda(sig) if self.policy.lambda_sig_cloneable(sig) => {
                format!("({expr}).clone()")
            }
            RirType::Lambda(_) => unreachable!("verified cloneable lambda value"),
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
            RirType::Slice(_) => format!("(*({expr})).readonly()"),
            RirType::Void => unreachable!("verified copy enum payload"),
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
