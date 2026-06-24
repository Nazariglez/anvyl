use super::{
    place::{MutPlaceProjection, MutPlaceProjectionStep, RustPlaces, projected_ops_ctor},
    rep_policy::{
        RustBorrowView, RustMaterialIntent, RustMaterialSource, RustMaterialization, RustRepPolicy,
    },
    rir::{
        RirCallArg, RirCellRef, RirConst, RirConstValue, RirEnum, RirField, RirFunction,
        RirGlobalId, RirMutPlaceAccess, RirMutPlaceArg, RirMutPlaceHandle, RirOperand,
        RirParamSemantic, RirPlace, RirPlaceRoot, RirProgram, RirProjection, RirScopedPlaceCellRef,
        RirType, RirTypeId, RirVariant, RirVariantKind,
    },
    syntax::{
        block_expr, comma, field_init, match_expr, rust_string, struct_lit, struct_variant,
        tuple_variant, variant_path,
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
            RirCallArg::ScopedLambda { .. } => {
                unreachable!("scoped lambda arguments must be prepared before rendering")
            }
            RirCallArg::EscapingLambda { .. } => {
                unreachable!("escaping lambda arguments must be prepared before rendering")
            }
        }
    }

    pub(super) fn borrow_arg(&self, place: &RirPlace) -> String {
        let place_expr = self.places.local_place(place);
        self.borrow_expr(
            place.ty,
            &place_expr,
            self.places.shared_borrow_root_param(place),
        )
    }

    pub(super) fn global_slot_expr(program: &RirProgram, global: RirGlobalId) -> String {
        let global = &program.globals[global.index()];
        target::global_slot_field(target::globals_param_name(), global.slot_symbol.as_str())
    }

    pub(super) fn global_init_call(program: &RirProgram, global: RirGlobalId) -> String {
        let global = &program.globals[global.index()];
        let symbol = program.functions[global.init.index()].symbol.as_str();
        if program.has_retained_callbacks() {
            target::retained_generated_call(symbol, [])
        } else {
            target::generated_call(symbol, [])
        }
    }

    pub(super) fn global_value_binding(&self, global: RirGlobalId, tmp: &str) -> String {
        format!("let {tmp} = {};", self.global_read(global))
    }

    pub(super) fn borrow_temp_arg(&self, ty: RirTypeId, tmp: &str) -> String {
        self.borrow_expr(ty, tmp, false)
    }

    fn borrow_expr(&self, ty: RirTypeId, expr: &str, borrowed_root: bool) -> String {
        match self.policy.borrow_view(ty) {
            RustBorrowView::Ref | RustBorrowView::Slice | RustBorrowView::Str if borrowed_root => {
                expr.to_string()
            }
            RustBorrowView::Ref | RustBorrowView::Slice => format!("&{expr}"),
            RustBorrowView::Str => format!("{expr}.as_str()"),
            RustBorrowView::TargetGap => expr.to_string(),
        }
    }

    pub(super) fn mut_borrow_arg(&self, place: &RirPlace) -> String {
        format!("&mut {}", self.place(place))
    }

    pub(super) fn mut_place_arg(&self, arg: &RirMutPlaceArg) -> String {
        debug_assert!(arg.projections.is_empty());
        self.mut_place_access_arg(&arg.access)
            .expect("dataref mut-place args must be prepared before rendering")
            .1
    }

    pub(super) fn mut_place_access_arg(
        &self,
        access: &RirMutPlaceAccess,
    ) -> Option<(RirTypeId, String)> {
        match access {
            RirMutPlaceAccess::Handle(handle) => Some(self.mut_place_handle_arg(handle)),
            RirMutPlaceAccess::DataRef { .. } => None,
        }
    }

    fn mut_place_handle_arg(&self, root: &RirMutPlaceHandle) -> (RirTypeId, String) {
        match root {
            RirMutPlaceHandle::Local { local, ty }
                if self.places.payload_ref_cell_local(*local) =>
            {
                (
                    *ty,
                    target::mut_place_scoped_cell(&format!(
                        "&{}",
                        self.function.locals[local.index()].symbol.as_str()
                    )),
                )
            }
            RirMutPlaceHandle::Local { local, ty } => (
                *ty,
                target::mut_place_local(&self.place(&RirPlace::local(*local, vec![], *ty))),
            ),
            RirMutPlaceHandle::Param { local, ty } => (
                *ty,
                target::mut_place_reborrow(self.function.locals[local.index()].symbol.as_str()),
            ),
            RirMutPlaceHandle::StackCell { cell, ty } => {
                (*ty, target::mut_place_stack_cell(&self.cell_ref(*cell)))
            }
            RirMutPlaceHandle::HeapCell { cell, ty } => {
                (*ty, target::mut_place_heap_cell(&self.cell_ref(*cell)))
            }
            RirMutPlaceHandle::ScopedPlaceCell { cell, ty } => {
                let cell = match cell {
                    RirScopedPlaceCellRef::Owner(_) => {
                        format!("&{}", self.scoped_place_cell_ref(*cell))
                    }
                    RirScopedPlaceCellRef::Capture { .. } => self.scoped_place_cell_ref(*cell),
                };
                (*ty, target::mut_place_scoped_cell(&cell))
            }
            RirMutPlaceHandle::Global { global, ty } => (
                *ty,
                target::mut_place_global(
                    &Self::global_slot_expr(self.program, *global),
                    &Self::global_init_call(self.program, *global),
                ),
            ),
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
        if let Some(value) = self.global_value_operand(place) {
            return value;
        }
        if self.places.mut_place_root_param(place) {
            return self.mut_place_value_operand(place);
        }
        if self.places.mut_place_projection(place).is_some() {
            return self.mut_place_projected_value_operand(place);
        }
        let place_expr = self.place(place);
        match self.policy.materialization_for(
            place.ty,
            RustMaterialSource::Value,
            RustMaterialIntent::Read,
        ) {
            RustMaterialization::Copy => self.operand(operand),
            RustMaterialization::Share
                if self.places.shared_borrow_root_param(place)
                    && matches!(self.program.types[place.ty.index()], RirType::String) =>
            {
                target::anv_string_from(&place_expr)
            }
            RustMaterialization::Share
            | RustMaterialization::CloneHandle
            | RustMaterialization::CloneLambda
            | RustMaterialization::BorrowGuard => self.value_from_place(place.ty, &place_expr),
            RustMaterialization::Gap => unreachable!("verified materializable value operand"),
        }
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
                if let Some(value) = self.global_value_operand(place) {
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
            RirOperand::Const(id) => Self::const_value(&self.program.consts[id.index()]),
        }
    }

    fn place(&self, place: &RirPlace) -> String {
        self.places.local_place(place)
    }

    fn mut_place_value_operand(&self, place: &RirPlace) -> String {
        self.place_value_from_access(place.ty, &self.place(place))
    }

    fn global_value_operand(&self, place: &RirPlace) -> Option<String> {
        let RirPlaceRoot::Global(global) = place.root else {
            return None;
        };
        if place.projections.is_empty() {
            return Some(self.global_read(global));
        }
        let root_ty = self.program.globals[global.index()].ty;
        let guard = "__anv_global";
        let root = format!("(&*{guard})");
        let expr = self
            .places
            .projected_expr(root_ty, &root, place.ty, &place.projections)?;
        let value = self.value_from_place(place.ty, &expr);
        let read = target::global_read(
            &Self::global_slot_expr(self.program, global),
            &Self::global_init_call(self.program, global),
        );
        Some(block_expr([format!("let {guard} = {read};")], Some(value)))
    }

    fn global_read(&self, global: RirGlobalId) -> String {
        let read = target::global_read(
            &Self::global_slot_expr(self.program, global),
            &Self::global_init_call(self.program, global),
        );
        block_expr(
            [format!("let __global = {read};")],
            Some(self.value_from_ref_with_source(
                self.program.globals[global.index()].ty,
                "&*__global",
                RustMaterialSource::ExactGlobalRoot,
            )),
        )
    }

    pub(super) fn assign(&self, dst: &RirPlace, value: &str) -> String {
        if let Some(set) = self.projected_mut_place_assign(dst, value) {
            return set;
        }
        if let Some(set) = self.dynamic_local_assign(dst, value) {
            return set;
        }
        if let Some(access) = self.places.dynamic_place_access(dst) {
            return Self::assign_slice_index(&access, value);
        }
        let dst_expr = self.places.local_place(dst);
        if self.places.mut_place_root_param(dst) {
            self.mut_place_set(dst.ty, &dst_expr, value)
        } else if self.program.collection_replace_ty(dst.ty) {
            target::replace_collection(&dst_expr, value)
        } else {
            format!("{dst_expr} = {value}")
        }
    }

    fn mut_place_set(&self, ty: RirTypeId, place: &str, value: &str) -> String {
        if self.program.collection_replace_ty(ty) {
            target::mut_place_replace_collection(place, target::runtime_param_name(), value)
        } else {
            target::mut_place_set(place, target::runtime_param_name(), value)
        }
    }

    fn assign_slice_index(access: &super::place::SliceIndexAccess, value: &str) -> String {
        if access.list_root {
            let checked = target::checked_index_result(
                &access.index,
                &format!("{}.len()", access.slice),
                "list",
            );
            let version = target::collection_structural_version(&access.slice);
            let update = target::list_with_elem_owned_mut_short(
                &access.slice,
                target::runtime_param_name(),
                "index",
                "version",
                "*value = __anv_slice_value; Ok(())",
            );
            format!(
                "{{ let index = {checked}; let version = {version}; let __anv_slice_value = {value}; {update}?; }}"
            )
        } else {
            format!(
                "{{ let __anv_slice_value = {value}; {}?; }}",
                target::slice_with_elem_owned_mut_short(
                    &access.slice,
                    target::runtime_param_name(),
                    &access.index,
                    "*value = __anv_slice_value; Ok(())",
                )
            )
        }
    }

    fn needs_dynamic_set_region(projection: &MutPlaceProjection) -> bool {
        projection
            .steps
            .split_last()
            .is_some_and(|(_, prefix)| prefix.iter().any(Self::dynamic_projection_step))
    }

    fn dynamic_projection_step(step: &MutPlaceProjectionStep) -> bool {
        matches!(
            step,
            MutPlaceProjectionStep::ListIndex { .. } | MutPlaceProjectionStep::SliceIndex { .. }
        )
    }

    fn projected_mut_place_assign(&self, place: &RirPlace, value: &str) -> Option<String> {
        let projection = self.places.mut_place_projection(place)?;
        Some(self.projected_set_region(
            &projection,
            &Self::projection_root_place(&projection),
            value,
        ))
    }

    fn dynamic_local_assign(&self, place: &RirPlace, value: &str) -> Option<String> {
        let RirPlaceRoot::Local(root) = place.root else {
            return None;
        };
        let local = &self.function.locals[root.index()];
        let projection = self.places.projected_place(
            local.ty,
            local.symbol.as_str(),
            place.ty,
            &place.projections,
        )?;
        if !Self::needs_dynamic_set_region(&projection) {
            return None;
        }
        Some(self.projected_set_region(
            &projection,
            &target::mut_place_local(&projection.root),
            value,
        ))
    }

    fn mut_place_projected_value_operand(&self, place: &RirPlace) -> String {
        let projection = self
            .places
            .mut_place_projection(place)
            .expect("checked mut-place projection");
        self.mut_place_projected_region(
            &projection,
            &Self::projection_root_place(&projection),
            "",
            &self.place_value_from_access(place.ty, "__anv_place"),
        )
    }

    fn projection_root_place(projection: &MutPlaceProjection) -> String {
        if projection.root_owned {
            projection.root.clone()
        } else {
            target::mut_place_reborrow(&projection.root)
        }
    }

    fn projected_set_region(
        &self,
        projection: &MutPlaceProjection,
        root_place: &str,
        value: &str,
    ) -> String {
        let set = if self.program.collection_replace_ty(projection.slot_ty) {
            target::mut_place_replace_collection(
                "__anv_place",
                target::runtime_param_name(),
                "__anv_value",
            )
        } else {
            target::mut_place_set("__anv_place", target::runtime_param_name(), "__anv_value")
        };
        self.mut_place_projected_region(
            projection,
            root_place,
            &format!("let __anv_value = {value};"),
            &set,
        )
    }

    fn mut_place_projected_region(
        &self,
        projection: &MutPlaceProjection,
        root_place: &str,
        before_place: &str,
        body: &str,
    ) -> String {
        let descriptor = self.mut_place_projection_descriptor("__AnvProjectedPlaceOps", projection);
        format!(
            "{{ {} {} let __anv_ops = {}; {before_place} let __anv_root = {root_place}; let mut __anv_place = {}; {body} }}",
            descriptor.struct_decl,
            descriptor.impl_decl,
            descriptor.ctor,
            target::mut_place_projected("__anv_root", "&__anv_ops"),
        )
    }

    pub(super) fn mut_place_projection_descriptor_for(
        &self,
        ops: &str,
        root_ty: RirTypeId,
        root: &str,
        slot_ty: RirTypeId,
        projections: &[RirProjection],
    ) -> ProjectedPlaceDescriptor {
        let projection = self
            .places
            .projected_mut_place(root_ty, root, slot_ty, projections)
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
            impl_decl: Self::mut_place_projection_ops_impl(
                ops,
                &root_ty,
                &slot_ty,
                &projection.steps,
            ),
        }
    }

    fn mut_place_projection_ops_impl(
        ops: &str,
        root_ty: &str,
        slot_ty: &str,
        steps: &[MutPlaceProjectionStep],
    ) -> String {
        let access = Self::projected_access("root", true, steps);
        let mutate = Self::projected_mutate("root", true, steps);
        format!(
            "impl<'cx> {} for {ops} {{ fn access(&self, rt: &mut anvyx_runtime::Ctx<'cx, '_>, root: &{root_ty}, f: &mut dyn FnMut(&{slot_ty}) -> {}) -> {} {{ {access} }} fn mutate(&self, rt: &mut anvyx_runtime::Ctx<'cx, '_>, root: &mut {root_ty}, f: &mut dyn FnMut(&mut {slot_ty}) -> {}) -> {} {{ {mutate} }} }}",
            target::projection_ops_ty(root_ty, slot_ty),
            target::result_ty("()"),
            target::result_ty("()"),
            target::result_ty("()"),
            target::result_ty("()"),
        )
    }

    fn projected_access(expr: &str, by_ref: bool, steps: &[MutPlaceProjectionStep]) -> String {
        let Some((step, rest)) = steps.split_first() else {
            return if by_ref {
                format!("f({expr})")
            } else {
                format!("f(&{expr})")
            };
        };
        match step {
            MutPlaceProjectionStep::Field(field) => {
                Self::projected_access(&format!("{expr}.{field}"), false, rest)
            }
            MutPlaceProjectionStep::ArrayIndex { index, len } => {
                let checked = target::checked_index_result(
                    &format!("self.{index}"),
                    &len.to_string(),
                    "array",
                );
                let body = Self::projected_access(&format!("{expr}[index]"), false, rest);
                format!("{{ let index = {checked}; {body} }}")
            }
            MutPlaceProjectionStep::ListIndex { index, version } => {
                let checked = target::checked_index_result(
                    &format!("self.{index}"),
                    &format!("{expr}.len()"),
                    "list",
                );
                let body = Self::projected_access("value", true, rest);
                let access = target::list_with_elem_shared_short(
                    expr,
                    "rt",
                    "index",
                    &format!("self.{version}"),
                    &body,
                );
                format!("{{ let index = {checked}; {access} }}")
            }
            MutPlaceProjectionStep::SliceIndex { index } => {
                let body = Self::projected_access("value", false, rest);
                format!("{{ let value = {expr}.elem_at_shared(rt, self.{index})?; {body} }}")
            }
        }
    }

    fn projected_mutate(expr: &str, by_ref: bool, steps: &[MutPlaceProjectionStep]) -> String {
        let Some((step, rest)) = steps.split_first() else {
            return if by_ref {
                format!("f({expr})")
            } else {
                format!("f(&mut {expr})")
            };
        };
        match step {
            MutPlaceProjectionStep::Field(field) => {
                Self::projected_mutate(&format!("{expr}.{field}"), false, rest)
            }
            MutPlaceProjectionStep::ArrayIndex { index, len } => {
                let checked = target::checked_index_result(
                    &format!("self.{index}"),
                    &len.to_string(),
                    "array",
                );
                let body = Self::projected_mutate(&format!("{expr}[index]"), false, rest);
                format!("{{ let index = {checked}; {body} }}")
            }
            MutPlaceProjectionStep::ListIndex { index, version } => {
                let checked = target::checked_index_result(
                    &format!("self.{index}"),
                    &format!("{expr}.len()"),
                    "list",
                );
                let body = Self::projected_mutate("value", true, rest);
                let access = target::list_with_elem_owned_mut_short(
                    expr,
                    "rt",
                    "index",
                    &format!("self.{version}"),
                    &body,
                );
                format!("{{ let index = {checked}; {access} }}")
            }
            MutPlaceProjectionStep::SliceIndex { index } => {
                let body = Self::projected_mutate("value", true, rest);
                target::slice_with_elem_owned_mut_short(expr, "rt", &format!("self.{index}"), &body)
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
        let runtime = target::runtime_param_name();
        match self.policy.materialization_for(
            ty,
            RustMaterialSource::Value,
            RustMaterialIntent::Read,
        ) {
            RustMaterialization::Copy => target::mut_place_get_copy(expr, runtime),
            RustMaterialization::Share
            | RustMaterialization::CloneHandle
            | RustMaterialization::CloneLambda
            | RustMaterialization::BorrowGuard => {
                let value = self.value_from_ref(ty, "value");
                target::mut_place_access(expr, runtime, &format!("Ok({value})"))
            }
            RustMaterialization::Gap => {
                unreachable!("verified materializable mutable-place access")
            }
        }
    }

    pub(super) fn value_from_ref(&self, ty: RirTypeId, expr: &str) -> String {
        self.value_from_ref_with_source(ty, expr, RustMaterialSource::Value)
    }

    fn value_from_ref_with_source(
        &self,
        ty: RirTypeId,
        expr: &str,
        source: RustMaterialSource,
    ) -> String {
        match self
            .policy
            .materialization_for(ty, source, RustMaterialIntent::Read)
        {
            RustMaterialization::Copy => self.copy_from_ref(ty, expr),
            RustMaterialization::Share => self.share_from_ref(ty, expr),
            RustMaterialization::CloneHandle => format!("(*({expr})).clone()"),
            RustMaterialization::CloneLambda => format!("({expr}).clone()"),
            RustMaterialization::BorrowGuard => self.borrow_guard_from_ref(ty, expr),
            RustMaterialization::Gap => unreachable!("verified materializable value"),
        }
    }

    pub(super) fn value_from_place(&self, ty: RirTypeId, expr: &str) -> String {
        match self.policy.materialization_for(
            ty,
            RustMaterialSource::Value,
            RustMaterialIntent::Read,
        ) {
            RustMaterialization::Copy => match self.program.types[ty.index()] {
                RirType::Int | RirType::Float | RirType::Bool => expr.to_string(),
                _ => self.copy_from_ref(ty, &format!("&{expr}")),
            },
            RustMaterialization::Share => self.share_from_place(ty, expr),
            RustMaterialization::CloneHandle | RustMaterialization::CloneLambda => {
                format!("{expr}.clone()")
            }
            RustMaterialization::BorrowGuard => self.borrow_guard_from_place(ty, expr),
            RustMaterialization::Gap => unreachable!("verified materializable value"),
        }
    }

    fn share_from_ref(&self, ty: RirTypeId, expr: &str) -> String {
        match self.program.types[ty.index()] {
            RirType::String | RirType::List(_) | RirType::Map { .. } => {
                format!("(*({expr})).share()")
            }
            RirType::Option(inner) => format!(
                "({expr}).as_ref().map(|value| {})",
                self.value_from_ref(inner, "value")
            ),
            RirType::Array { elem, len } => {
                let elems = comma(
                    (0..len).map(|index| self.value_from_ref(elem, &format!("&({expr})[{index}]"))),
                );
                format!("[{elems}]")
            }
            RirType::Struct(id) => {
                let strukt = &self.program.structs[id.index()];
                self.copy_record_from_ref(strukt.symbol.as_str(), &strukt.fields, expr)
            }
            RirType::Tuple(id) => {
                let tuple = &self.program.tuples[id.index()];
                self.copy_record_from_ref(tuple.symbol.as_str(), &tuple.fields, expr)
            }
            RirType::Enum(id) if self.program.enums[id.index()].variants.is_empty() => {
                format!("match *({expr}) {{}}")
            }
            RirType::Enum(_) => self.copy_enum_ref_expr(expr, ty),
            _ => unreachable!("verified shareable value"),
        }
    }

    fn share_from_place(&self, ty: RirTypeId, expr: &str) -> String {
        match self.program.types[ty.index()] {
            RirType::String | RirType::List(_) | RirType::Map { .. } => format!("{expr}.share()"),
            RirType::Option(_)
            | RirType::Array { .. }
            | RirType::Struct(_)
            | RirType::Tuple(_)
            | RirType::Enum(_) => self.share_from_ref(ty, &format!("&{expr}")),
            _ => unreachable!("verified shareable value"),
        }
    }

    fn borrow_guard_from_ref(&self, ty: RirTypeId, expr: &str) -> String {
        match self.program.types[ty.index()] {
            RirType::Slice(_) => format!("(*({expr})).readonly()"),
            _ => unreachable!("verified borrow-guard materialization"),
        }
    }

    fn borrow_guard_from_place(&self, ty: RirTypeId, expr: &str) -> String {
        match self.program.types[ty.index()] {
            RirType::Slice(_) => format!("{expr}.readonly()"),
            _ => unreachable!("verified borrow-guard materialization"),
        }
    }

    pub(super) fn native_return_call(
        &self,
        ret: RirTypeId,
        abi: &anvyx_runtime::RustReturnAbi,
        call: String,
    ) -> String {
        match abi {
            anvyx_runtime::RustReturnAbi::Value(ty) => Self::native_value_return(ty, &call),
            anvyx_runtime::RustReturnAbi::Option(inner) => {
                self.option_return_call(ret, inner, &call)
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
            | RirParamSemantic::ScopedLambda
            | RirParamSemantic::EscapingLambda
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
                    _ => format!("{}.as_str()", Self::const_value(konst)),
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

    pub(super) fn const_value(konst: &RirConst) -> String {
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
        call: &str,
    ) -> String {
        let RirType::Option(_) = self.program.types[ret.index()] else {
            unreachable!("verified native option return type")
        };
        let value = match inner {
            anvyx_runtime::RustReturnAbi::Value(ty) => Self::native_value_return(ty, "value"),
            _ => unreachable!("verified native option return inner"),
        };
        match_expr(
            call,
            [
                format!("Some(value) => Some({value})"),
                "None => None".to_string(),
            ],
        )
    }

    fn native_value_return(ty: &anvyx_runtime::ExternTypeExpr, expr: &str) -> String {
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
            RirType::Lambda(_) => unreachable!("verified copyable lambda value"),
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
            RirType::DataRef(_)
            | RirType::String
            | RirType::List(_)
            | RirType::Map { .. }
            | RirType::Slice(_)
            | RirType::Void => unreachable!("verified copyable value"),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rust::rir::{
        RirFieldId, RirFunctionId, RirLambdaEnvId, RirLambdaEscape, RirLambdaId, RirLambdaSig,
        RirLambdaSigId, RirLambdaSource, RirLambdaStorage, RirReturn, RirStruct, RirStructId,
        RirSymbol, RirTuple, RirTupleId,
    };

    fn field(id: usize, symbol: &str, ty: RirTypeId) -> RirField {
        RirField {
            id: RirFieldId::from_index(id),
            symbol: RirSymbol::new(symbol),
            ty,
        }
    }

    #[test]
    fn materializes_values_through_policy_classes() {
        let mut program = RirProgram::default();
        let int = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::Int);
        let string = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::String);
        let list = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::List(int));
        let map = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::Map {
            key: int,
            value: string,
        });
        let node = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::DataRef(
            crate::rust::rir::RirDataRefId::from_index(0),
        ));
        program.datarefs.push(crate::rust::rir::RirDataRef {
            id: crate::rust::rir::RirDataRefId::from_index(0),
            air_id: anvyx_frontend::air::AggregateId::from_index(0),
            symbol: RirSymbol::new("Node"),
            display: RirSymbol::new("Node"),
            cycle_capable: true,
            fields: vec![field(0, "value", int)],
        });
        let label = RirTypeId::from_index(program.types.len());
        program
            .types
            .push(RirType::Struct(RirStructId::from_index(0)));
        program.structs.push(RirStruct {
            id: RirStructId::from_index(0),
            air_id: None,
            symbol: RirSymbol::new("Label"),
            display: RirSymbol::new("Label"),
            native_path: None,
            native_key: None,
            copyable: false,
            fields: vec![field(0, "text", string)],
        });
        let pair = RirTypeId::from_index(program.types.len());
        program
            .types
            .push(RirType::Tuple(RirTupleId::from_index(0)));
        program.tuples.push(RirTuple {
            id: RirTupleId::from_index(0),
            symbol: RirSymbol::new("Pair"),
            display: RirSymbol::new("Pair"),
            fields: vec![field(0, "a", int), field(1, "b", int)],
            copyable: true,
        });
        let maybe_node = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::Option(node));
        let slice = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::Slice(int));
        let lambda_sig = RirLambdaSigId::from_index(0);
        let lambda = RirTypeId::from_index(program.types.len());
        program.types.push(RirType::Lambda(lambda_sig));
        program.lambda_sigs.push(RirLambdaSig {
            id: lambda_sig,
            params: vec![],
            ret: int,
        });
        program.lambdas.push(crate::rust::rir::RirLambda {
            id: RirLambdaId::from_index(0),
            source: RirLambdaSource::Function(anvyx_frontend::air::FunctionId::from_index(0)),
            function: RirFunctionId::from_index(0),
            sig: lambda_sig,
            escape: RirLambdaEscape::Escaping,
            storage: RirLambdaStorage::HeapEnv {
                env: RirLambdaEnvId::from_index(0),
            },
            captures: vec![],
        });
        let function = RirFunction {
            id: RirFunctionId::from_index(0),
            air_id: None,
            symbol: RirSymbol::new("f"),
            params: vec![],
            ret: RirReturn { ty: int },
            locals: vec![],
            body: crate::rust::rir::RirStructuredBlock::default(),
        };
        let values = RustValues::new(&program, &function);

        assert_eq!(values.value_from_ref(int, "x"), "*(x)");
        assert_eq!(values.value_from_place(int, "x"), "x");
        assert_eq!(values.value_from_ref(string, "x"), "(*(x)).share()");
        assert_eq!(values.value_from_place(string, "x"), "x.share()");
        assert_eq!(values.value_from_ref(list, "x"), "(*(x)).share()");
        assert_eq!(values.value_from_place(map, "x"), "x.share()");
        assert_eq!(values.value_from_ref(node, "x"), "(*(x)).clone()");
        assert_eq!(values.value_from_place(node, "x"), "x.clone()");
        assert!(
            values
                .value_from_ref(label, "x")
                .contains("text: (*(&(x).text)).share()")
        );
        assert_eq!(
            values.value_from_ref(maybe_node, "x"),
            "(x).as_ref().map(|value| (*(value)).clone())"
        );
        assert!(values.value_from_ref(pair, "x").contains("a: *(&(x).a)"));
        assert_eq!(values.value_from_ref(slice, "x"), "(*(x)).readonly()");
        assert_eq!(values.value_from_place(slice, "x"), "x.readonly()");
        assert_eq!(values.value_from_ref(lambda, "x"), "(x).clone()");
    }
}
