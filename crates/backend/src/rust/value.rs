use super::{
    place::{MutPlaceProjection, MutPlaceProjectionStep, RustPlaces, projected_ops_ctor},
    rep_policy::{RirRustRepPolicy, RustBorrowView},
    rir::{
        RirCallArg, RirCellRef, RirConst, RirConstValue, RirDynCarrierId, RirDynStorage,
        RirDynVariantId, RirEnum, RirField, RirFunction, RirGlobalId, RirMaterializerId,
        RirMutPlaceAccess, RirMutPlaceArg, RirMutPlaceHandle, RirOperand, RirParamSemantic,
        RirPlace, RirPlaceRoot, RirProgram, RirProjection, RirScopedPlaceCellRef, RirType,
        RirTypeId, RirVariant, RirVariantKind,
    },
    syntax::{
        block_expr, comma, field_init, match_expr, rust_char, rust_string, struct_lit,
        struct_variant, tuple_variant, variant_path,
    },
    target,
};

pub(super) struct RustValues<'a> {
    program: &'a RirProgram,
    function: &'a RirFunction,
    policy: RirRustRepPolicy<'a>,
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
            policy: RirRustRepPolicy::new(program),
            places: RustPlaces::new(program, function),
        }
    }

    pub(super) fn call_arg(&self, arg: &RirCallArg) -> String {
        match arg {
            RirCallArg::Value(owned) | RirCallArg::InitFieldProvided(owned) => {
                self.owned_value(owned)
            }
            RirCallArg::InitFieldOmitted => target::init_field_omitted(),
            RirCallArg::SharedBorrow(place) => self.borrow_arg(place),
            RirCallArg::SharedStringConst(id) => {
                rust_string(&self.program.string_literal(*id).text)
            }
            RirCallArg::MutBorrow(place) => self.mut_borrow_arg(place),
            RirCallArg::MutPlace(arg) => self.mut_place_arg(arg),
            RirCallArg::DynBorrow(_) => {
                unreachable!("dynamic borrow arguments require structured preparation")
            }
            RirCallArg::ScopedLambda { .. } => {
                unreachable!("scoped lambda arguments must be prepared before rendering")
            }
            RirCallArg::EscapingLambda { .. } => {
                unreachable!("escaping lambda arguments must be prepared before rendering")
            }
            RirCallArg::AnvCallback { .. } => {
                unreachable!("callback carrier arguments must be prepared before rendering")
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

    pub(super) fn dyn_payload_from_ref(
        &self,
        materializer: RirMaterializerId,
        expr: &str,
    ) -> String {
        self.render_materializer_ref(materializer, expr)
    }

    pub(super) fn owned_value(&self, owned: &super::rir::RirOwnedValue) -> String {
        if let super::rir::RirOwnedOperand::DynBorrow(borrow) = &owned.value {
            let super::rir::RirOwnedSource::Reuse(materializer) = owned.source else {
                unreachable!("verified borrowed dynamic transfer")
            };
            let (
                super::rir::RirDynBorrowSource::Borrowed { local, .. }
                | super::rir::RirDynBorrowSource::Reborrowed { local, .. },
                None,
            ) = (&borrow.source, borrow.air_weakening)
            else {
                unreachable!("verified owned dynamic reborrow")
            };
            let local = self.function.locals[local.index()].symbol.as_str();
            return target::dyn_borrow_materialize(local, materializer);
        }
        let super::rir::RirOwnedOperand::Value(operand) = &owned.value else {
            unreachable!("access-aware owned values require statement rendering")
        };
        let value = match operand {
            RirOperand::Place(place) => self.place(place),
            RirOperand::Const(id) => self.const_value(&self.program.consts[id.index()]),
        };
        let physical_ref = matches!(
            operand,
            RirOperand::Place(place) if self.places.physical_ref_root(place)
        );
        match owned.source {
            super::rir::RirOwnedSource::Reuse(_)
                if physical_ref
                    && matches!(
                        self.program.types[self.operand_ty(operand).index()],
                        RirType::String
                    ) =>
            {
                target::anv_string_from(&value)
            }
            super::rir::RirOwnedSource::Reuse(materializer) => {
                let source = if physical_ref {
                    value
                } else {
                    format!("&{value}")
                };
                self.render_materializer_ref(materializer, &source)
            }
            super::rir::RirOwnedSource::Transfer { .. } => value,
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
        if self.places.shared_borrow_root_param(place)
            && matches!(self.program.types[place.ty.index()], RirType::String)
        {
            return target::anv_string_from(&place_expr);
        }
        let source = if self.places.shared_borrow_root_param(place) {
            place_expr
        } else {
            format!("&{place_expr}")
        };
        self.render_value_ref(place.ty, &source)
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
                    RirType::Struct(id)
                        if self.program.structs[id.index()].copyable
                            && self.program.structs[id.index()].native_path.is_some() =>
                    {
                        self.place(place)
                    }
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
            Some(self.render_value_ref(self.program.globals[global.index()].ty, "&*__global")),
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
            return self.assign_slice_index(&access, value);
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

    fn assign_slice_index(&self, access: &super::place::SliceIndexAccess, value: &str) -> String {
        if access.list_root {
            let materialize = self.sequence_materializer_closure(access.ty);
            let checked = target::checked_index_result(
                &access.index,
                &format!("{}.len()", access.slice),
                "list",
            );
            let version = target::collection_structural_version(&access.slice);
            let update = target::list_with_elem_mut_leaf(
                &access.slice,
                target::runtime_param_name(),
                "index",
                "version",
                &materialize,
                "*value = __anv_slice_value; Ok(())",
            );
            format!(
                "{{ let index = {checked}; let version = {version}; let __anv_slice_value = {value}; {update}?; }}"
            )
        } else {
            format!(
                "{{ let __anv_slice_value = {value}; {}?; }}",
                target::slice_with_elem_mut_leaf(
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

    pub(super) fn dyn_payload_projection_descriptor(
        &self,
        ops: &str,
        carrier: RirDynCarrierId,
        variants: &[RirDynVariantId],
        target: RirTypeId,
    ) -> ProjectedPlaceDescriptor {
        let carrier = &self.program.dyn_carriers[carrier.index()];
        let RirType::Enum(id) = self.program.types[carrier.storage_ty.index()] else {
            unreachable!("verified dynamic carrier storage")
        };
        let enm = &self.program.enums[id.index()];
        let paths = variants
            .iter()
            .map(|id| {
                let variant = &carrier.variants[id.index()];
                let path = variant_path(
                    enm.symbol.as_str(),
                    enm.variants[id.index()].symbol.as_str(),
                );
                (path, variant.storage)
            })
            .collect::<Vec<_>>();
        let fallback = (paths.len() != carrier.variants.len()).then(|| {
            format!(
                "_ => Err({})",
                target::runtime_error("dynamic carrier variant changed during projected access")
            )
        });
        let access = match_expr(
            "root",
            paths
                .iter()
                .map(|(path, storage)| {
                    let payload = if *storage == RirDynStorage::Boxed {
                        "payload.as_ref()"
                    } else {
                        "payload"
                    };
                    format!("{path}(payload) => f({payload})")
                })
                .chain(fallback.iter().cloned()),
        );
        let mutate = match_expr(
            "root",
            paths
                .iter()
                .map(|(path, storage)| {
                    let payload = if *storage == RirDynStorage::Boxed {
                        "payload.as_mut()"
                    } else {
                        "payload"
                    };
                    format!("{path}(payload) => f({payload})")
                })
                .chain(fallback),
        );
        let root_ty = self.policy.rust_ty(carrier.storage_ty);
        let target_ty = self.policy.rust_ty(target);
        let rt_ty = target::runtime_ctx_ty_with_lifetimes("'cx", "'_");
        let result = target::result_ty("()");
        ProjectedPlaceDescriptor {
            struct_decl: format!("struct {ops};"),
            ctor: ops.to_string(),
            impl_decl: format!(
                "unsafe impl<'cx> {} for {ops} {{ fn access(&self, _rt: &mut {rt_ty}, root: &{root_ty}, f: &mut dyn FnMut(&{target_ty}) -> {result}) -> {result} {{ {access} }} fn mutate(&self, _rt: &mut {rt_ty}, root: &mut {root_ty}, f: &mut dyn FnMut(&mut {target_ty}) -> {result}) -> {result} {{ {mutate} }} }}",
                target::projection_ops_ty(&root_ty, &target_ty),
            ),
        }
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
        let access = Self::projected_access("root", true, steps);
        let mutate = self.projected_mutate("root", true, steps);
        let rt_ty = target::runtime_ctx_ty_with_lifetimes("'cx", "'_");
        let rt = if steps.iter().any(|step| {
            matches!(
                step,
                MutPlaceProjectionStep::ListIndex { .. }
                    | MutPlaceProjectionStep::SliceIndex { .. }
            )
        }) {
            "rt"
        } else {
            "_rt"
        };
        format!(
            "unsafe impl<'cx> {} for {ops} {{ fn access(&self, {rt}: &mut {rt_ty}, root: &{root_ty}, f: &mut dyn FnMut(&{slot_ty}) -> {}) -> {} {{ {access} }} fn mutate(&self, {rt}: &mut {rt_ty}, root: &mut {root_ty}, f: &mut dyn FnMut(&mut {slot_ty}) -> {}) -> {} {{ {mutate} }} }}",
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
            MutPlaceProjectionStep::ListIndex { index, version, .. } => {
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
            MutPlaceProjectionStep::SliceIndex { index, .. } => {
                let body = Self::projected_access("value", true, rest);
                target::slice_with_elem_shared_leaf(expr, "rt", &format!("self.{index}"), &body)
            }
        }
    }

    fn projected_static_path(
        expr: &str,
        by_ref: bool,
        steps: &[MutPlaceProjectionStep],
        leaf: &dyn Fn(&str, bool) -> String,
    ) -> String {
        let Some((step, rest)) = steps.split_first() else {
            return leaf(expr, by_ref);
        };
        match step {
            MutPlaceProjectionStep::Field(field) => {
                Self::projected_static_path(&format!("{expr}.{field}"), false, rest, leaf)
            }
            MutPlaceProjectionStep::ArrayIndex { index, len } => {
                let checked = target::checked_index_result(
                    &format!("self.{index}"),
                    &len.to_string(),
                    "array",
                );
                let body =
                    Self::projected_static_path(&format!("{expr}[index]"), false, rest, leaf);
                format!("{{ let index = {checked}; {body} }}")
            }
            MutPlaceProjectionStep::ListIndex { .. }
            | MutPlaceProjectionStep::SliceIndex { .. } => {
                unreachable!("static projection prefix")
            }
        }
    }

    fn nested_projection_mutation(
        &self,
        steps: &[MutPlaceProjectionStep],
    ) -> (String, String, String) {
        let split = steps
            .iter()
            .position(Self::dynamic_projection_step)
            .expect("nested collection projection");
        let prefix = &steps[..split];
        let read = Self::projected_static_path("value", true, prefix, &|expr, by_ref| {
            let reference = if by_ref {
                expr.to_string()
            } else {
                format!("&{expr}")
            };
            format!("Ok({})", target::collection_projection_owner(&reference))
        });
        let mutate = self.projected_mutate("__anv_owner", false, &steps[split..]);
        let write = Self::projected_static_path("value", true, prefix, &|expr, by_ref| {
            if by_ref {
                format!("*{expr} = __anv_owner; Ok(())")
            } else {
                format!("{expr} = __anv_owner; Ok(())")
            }
        });
        (read, mutate, write)
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
            MutPlaceProjectionStep::ListIndex { index, version, ty } => {
                let materialize = self.sequence_materializer_closure(*ty);
                let checked = target::checked_index_result(
                    &format!("self.{index}"),
                    &format!("{expr}.len()"),
                    "list",
                );
                if !rest.iter().any(Self::dynamic_projection_step) {
                    let body = self.projected_mutate("value", true, rest);
                    let access = target::list_with_elem_mut_leaf(
                        expr,
                        "rt",
                        "index",
                        &format!("self.{version}"),
                        &materialize,
                        &body,
                    );
                    return format!("{{ let index = {checked}; {access} }}");
                }
                let (read_body, body, write_body) = self.nested_projection_mutation(rest);
                let read = target::list_with_elem_shared_short(
                    expr,
                    "rt",
                    "index",
                    &format!("self.{version}"),
                    &read_body,
                );
                let write = target::list_with_elem_mut_leaf(
                    expr,
                    "rt",
                    "index",
                    &format!("self.{version}"),
                    &materialize,
                    &write_body,
                );
                format!(
                    "{{ let index = {checked}; let mut __anv_owner = {read}?; let __anv_result = {body}; {write}?; __anv_result }}"
                )
            }
            MutPlaceProjectionStep::SliceIndex { index, .. } => {
                if !rest.iter().any(Self::dynamic_projection_step) {
                    let body = self.projected_mutate("value", true, rest);
                    return target::slice_with_elem_mut_leaf(
                        expr,
                        "rt",
                        &format!("self.{index}"),
                        &body,
                    );
                }
                let (read_body, body, write_body) = self.nested_projection_mutation(rest);
                let read = target::slice_with_elem_shared_leaf(
                    expr,
                    "rt",
                    &format!("self.{index}"),
                    &read_body,
                );
                let write = target::slice_with_elem_mut_leaf(
                    expr,
                    "rt",
                    &format!("self.{index}"),
                    &write_body,
                );
                format!(
                    "{{ let mut __anv_owner = {read}?; let __anv_result = {body}; {write}?; __anv_result }}"
                )
            }
        }
    }

    fn place_value_from_access(&self, ty: RirTypeId, expr: &str) -> String {
        let runtime = target::runtime_param_name();
        let copy = self.value_materializer(ty).is_some_and(|materializer| {
            matches!(
                self.program.materializers[materializer.index()].action,
                super::rir::RirMaterializerAction::Copy
            )
        });
        if copy {
            target::mut_place_get_copy(expr, runtime)
        } else {
            let value = self.render_value_ref(ty, "value");
            target::mut_place_access(expr, runtime, &format!("Ok({value})"))
        }
    }

    pub(super) fn value_from_ref(&self, ty: RirTypeId, expr: &str) -> String {
        self.render_value_ref(ty, expr)
    }

    pub(super) fn materialize_ref(&self, materializer: RirMaterializerId, expr: &str) -> String {
        self.render_materializer_ref(materializer, expr)
    }

    fn sequence_materializer_closure(&self, ty: RirTypeId) -> String {
        let materializer = self
            .program
            .sequence_elem_materializer(ty)
            .expect("verified sequence storage declaration");
        let body = self.render_materializer_ref(materializer, "value");
        target::materializer_closure("value", &body)
    }

    pub(super) fn value_from_place(&self, ty: RirTypeId, expr: &str) -> String {
        self.render_value_ref(ty, &format!("&{expr}"))
    }

    fn value_materializer(&self, ty: RirTypeId) -> Option<RirMaterializerId> {
        self.program
            .value_materializers
            .get(ty.index())
            .copied()
            .flatten()
    }

    fn render_value_ref(&self, ty: RirTypeId, expr: &str) -> String {
        let materializer = self
            .value_materializer(ty)
            .expect("verified value materializer");
        self.render_materializer_ref(materializer, expr)
    }

    pub(super) fn materializer_body(program: &'a RirProgram, id: RirMaterializerId) -> String {
        let function = program
            .functions
            .first()
            .expect("materializer helper requires an executable program");
        Self::new(program, function).render_materializer_body(id, "value")
    }

    pub(super) fn render_materializer_ref(&self, id: RirMaterializerId, expr: &str) -> String {
        if self.program.materializers[id.index()]
            .action
            .requires_helper()
        {
            target::materializer_call(id, expr)
        } else {
            self.render_materializer_body(id, expr)
        }
    }

    fn render_materializer_body(&self, id: RirMaterializerId, expr: &str) -> String {
        let materializer = &self.program.materializers[id.index()];
        match &materializer.action {
            super::rir::RirMaterializerAction::Copy => self.copy_from_ref(materializer.ty, expr),
            super::rir::RirMaterializerAction::ManagedShare
                if materializer.stages_collection(self.program) =>
            {
                target::collection_staged_owner(&format!("*({expr})"))
            }
            super::rir::RirMaterializerAction::ManagedShare => target::managed_share_from_ref(expr),
            super::rir::RirMaterializerAction::IdentityShare => {
                target::identity_share_from_ref(expr)
            }
            super::rir::RirMaterializerAction::CallableShare => {
                target::callable_share_from_ref(expr)
            }
            super::rir::RirMaterializerAction::ProviderMaterialize { binding } => {
                target::provider_materialize(&binding.path, expr)
            }
            super::rir::RirMaterializerAction::Struct { fields } => {
                let RirType::Struct(strukt) = self.program.types[materializer.ty.index()] else {
                    unreachable!("verified struct materializer")
                };
                let strukt = &self.program.structs[strukt.index()];
                self.render_record_materializer(
                    strukt.symbol.as_str(),
                    &strukt.fields,
                    fields,
                    expr,
                )
            }
            super::rir::RirMaterializerAction::Tuple { fields } => {
                let RirType::Tuple(tuple) = self.program.types[materializer.ty.index()] else {
                    unreachable!("verified tuple materializer")
                };
                let tuple = &self.program.tuples[tuple.index()];
                self.render_record_materializer(tuple.symbol.as_str(), &tuple.fields, fields, expr)
            }
            super::rir::RirMaterializerAction::Array { elem } => {
                let RirType::Array { len, .. } = self.program.types[materializer.ty.index()] else {
                    unreachable!("verified array materializer")
                };
                let elems = (0..len).map(|index| {
                    self.render_materializer_ref(*elem, &format!("&({expr})[{index}]"))
                });
                format!("[{}]", comma(elems))
            }
            super::rir::RirMaterializerAction::Enum { variants } => {
                self.render_enum_materializer(materializer.ty, variants, expr)
            }
            super::rir::RirMaterializerAction::Optional { payload } => format!(
                "({expr}).as_ref().map(|value| {})",
                self.render_materializer_ref(*payload, "value")
            ),
            super::rir::RirMaterializerAction::DynamicMaterialize { surface, variants } => {
                self.render_dynamic_materializer(*surface, variants, expr)
            }
        }
    }

    fn render_record_materializer(
        &self,
        symbol: &str,
        fields: &[RirField],
        actions: &[RirMaterializerId],
        expr: &str,
    ) -> String {
        struct_lit(
            symbol,
            fields.iter().zip(actions).map(|(field, action)| {
                field_init(
                    field.symbol.as_str(),
                    self.render_materializer_ref(
                        *action,
                        &format!("&({expr}).{}", field.symbol.as_str()),
                    ),
                )
            }),
        )
    }

    fn render_enum_materializer(
        &self,
        ty: RirTypeId,
        actions: &[Vec<RirMaterializerId>],
        expr: &str,
    ) -> String {
        let RirType::Enum(id) = self.program.types[ty.index()] else {
            unreachable!("verified enum materializer")
        };
        let decl = &self.program.enums[id.index()];
        let arms = decl.variants.iter().zip(actions).map(|(variant, actions)| {
            let path = variant_path(decl.symbol.as_str(), variant.symbol.as_str());
            match variant.kind {
                RirVariantKind::Unit => format!("{path} => {path}"),
                RirVariantKind::Tuple => {
                    let names = (0..variant.fields.len())
                        .map(|index| format!("field_{index}"))
                        .collect::<Vec<_>>();
                    let pattern = tuple_variant(&path, names.iter().cloned());
                    let fields = names
                        .iter()
                        .zip(actions)
                        .map(|(name, action)| self.render_materializer_ref(*action, name));
                    format!("{pattern} => {}", tuple_variant(&path, fields))
                }
                RirVariantKind::Struct => {
                    let names = variant
                        .fields
                        .iter()
                        .map(|field| field.symbol.as_str().to_string())
                        .collect::<Vec<_>>();
                    let pattern = struct_variant(&path, names.iter().cloned());
                    let fields = names.iter().zip(actions).map(|(name, action)| {
                        field_init(name, self.render_materializer_ref(*action, name))
                    });
                    format!("{pattern} => {}", struct_variant(&path, fields))
                }
            }
        });
        match_expr(expr, arms)
    }

    fn render_dynamic_materializer(
        &self,
        surface: anvyx_frontend::air::ContractSurfaceId,
        actions: &[super::rir::RirDynamicMaterializerVariant],
        expr: &str,
    ) -> String {
        let carrier = self
            .program
            .dyn_carriers
            .iter()
            .find(|carrier| carrier.air_surface == surface)
            .expect("verified dynamic carrier materializer");
        let RirType::Enum(enm) = self.program.types[carrier.storage_ty.index()] else {
            unreachable!("verified dynamic carrier materializer")
        };
        let decl = &self.program.enums[enm.index()];
        let arms = carrier.variants.iter().map(|variant| {
            let declaration = &decl.variants[variant.id.index()];
            let path = variant_path(decl.symbol.as_str(), declaration.symbol.as_str());
            let action = actions
                .iter()
                .find(|action| action.witness == variant.air_witness)
                .expect("verified dynamic materializer variant");
            let payload_ref = if variant.storage == RirDynStorage::Boxed {
                "payload.as_ref()"
            } else {
                "payload"
            };
            let payload = self.render_materializer_ref(action.payload, payload_ref);
            let payload = if variant.storage == RirDynStorage::Boxed {
                format!("Box::new({payload})")
            } else {
                payload
            };
            format!("{path}(payload) => {}", tuple_variant(&path, [payload]))
        });
        match_expr(expr, arms)
    }

    pub(super) fn staged_commit_body(program: &'a RirProgram, id: RirMaterializerId) -> String {
        let function = program
            .functions
            .first()
            .expect("staged commit helper requires an executable program");
        Self::new(program, function).render_staged_commit(id, "value")
    }

    pub(super) fn staged_commit(&self, id: RirMaterializerId, value: &str) -> String {
        if self.program.materializers[id.index()]
            .action
            .requires_helper()
        {
            target::staged_commit_call(id, value)
        } else {
            self.render_staged_commit(id, value)
        }
    }

    fn render_staged_commit(&self, id: RirMaterializerId, value: &str) -> String {
        let materializer = &self.program.materializers[id.index()];
        match &materializer.action {
            super::rir::RirMaterializerAction::ManagedShare
                if materializer.stages_collection(self.program) =>
            {
                target::collection_commit_staged_owner(value)
            }
            super::rir::RirMaterializerAction::Copy
            | super::rir::RirMaterializerAction::ManagedShare
            | super::rir::RirMaterializerAction::IdentityShare
            | super::rir::RirMaterializerAction::CallableShare
            | super::rir::RirMaterializerAction::ProviderMaterialize { .. } => value.to_string(),
            super::rir::RirMaterializerAction::Struct { fields } => {
                let RirType::Struct(strukt) = self.program.types[materializer.ty.index()] else {
                    unreachable!("verified struct staged commit")
                };
                let strukt = &self.program.structs[strukt.index()];
                struct_lit(
                    strukt.symbol.as_str(),
                    strukt.fields.iter().zip(fields).map(|(field, action)| {
                        field_init(
                            field.symbol.as_str(),
                            self.staged_commit(
                                *action,
                                &format!("({value}).{}", field.symbol.as_str()),
                            ),
                        )
                    }),
                )
            }
            super::rir::RirMaterializerAction::Tuple { fields } => {
                let RirType::Tuple(tuple) = self.program.types[materializer.ty.index()] else {
                    unreachable!("verified tuple staged commit")
                };
                let tuple = &self.program.tuples[tuple.index()];
                struct_lit(
                    tuple.symbol.as_str(),
                    tuple.fields.iter().zip(fields).map(|(field, action)| {
                        field_init(
                            field.symbol.as_str(),
                            self.staged_commit(
                                *action,
                                &format!("({value}).{}", field.symbol.as_str()),
                            ),
                        )
                    }),
                )
            }
            super::rir::RirMaterializerAction::Array { elem } => {
                let RirType::Array { len, .. } = self.program.types[materializer.ty.index()] else {
                    unreachable!("verified array staged commit")
                };
                let names = (0..len)
                    .map(|index| format!("field_{index}"))
                    .collect::<Vec<_>>();
                let values = names.iter().map(|name| self.staged_commit(*elem, name));
                format!(
                    "{{ let [{}] = {value}; [{}] }}",
                    comma(names.iter().cloned()),
                    comma(values)
                )
            }
            super::rir::RirMaterializerAction::Enum { variants } => {
                self.render_staged_enum_commit(materializer.ty, variants, value)
            }
            super::rir::RirMaterializerAction::Optional { payload } => format!(
                "({value}).map(|value| {})",
                self.staged_commit(*payload, "value")
            ),
            super::rir::RirMaterializerAction::DynamicMaterialize { surface, variants } => {
                self.render_staged_dynamic_commit(*surface, variants, value)
            }
        }
    }

    fn render_staged_enum_commit(
        &self,
        ty: RirTypeId,
        actions: &[Vec<RirMaterializerId>],
        value: &str,
    ) -> String {
        let RirType::Enum(id) = self.program.types[ty.index()] else {
            unreachable!("verified enum staged commit")
        };
        let decl = &self.program.enums[id.index()];
        let arms = decl.variants.iter().zip(actions).map(|(variant, actions)| {
            let path = variant_path(decl.symbol.as_str(), variant.symbol.as_str());
            match variant.kind {
                RirVariantKind::Unit => format!("{path} => {path}"),
                RirVariantKind::Tuple => {
                    let names = (0..variant.fields.len())
                        .map(|index| format!("field_{index}"))
                        .collect::<Vec<_>>();
                    let fields = names
                        .iter()
                        .zip(actions)
                        .map(|(name, action)| self.staged_commit(*action, name));
                    format!(
                        "{} => {}",
                        tuple_variant(&path, names.iter().cloned()),
                        tuple_variant(&path, fields)
                    )
                }
                RirVariantKind::Struct => {
                    let names = variant
                        .fields
                        .iter()
                        .map(|field| field.symbol.as_str().to_string())
                        .collect::<Vec<_>>();
                    let fields = names
                        .iter()
                        .zip(actions)
                        .map(|(name, action)| field_init(name, self.staged_commit(*action, name)));
                    format!(
                        "{} => {}",
                        struct_variant(&path, names.iter().cloned()),
                        struct_variant(&path, fields)
                    )
                }
            }
        });
        match_expr(value, arms)
    }

    fn render_staged_dynamic_commit(
        &self,
        surface: anvyx_frontend::air::ContractSurfaceId,
        actions: &[super::rir::RirDynamicMaterializerVariant],
        value: &str,
    ) -> String {
        let carrier = self
            .program
            .dyn_carriers
            .iter()
            .find(|carrier| carrier.air_surface == surface)
            .expect("verified dynamic staged commit");
        let RirType::Enum(enm) = self.program.types[carrier.storage_ty.index()] else {
            unreachable!("verified dynamic staged commit")
        };
        let decl = &self.program.enums[enm.index()];
        let arms = carrier.variants.iter().map(|variant| {
            let declaration = &decl.variants[variant.id.index()];
            let path = variant_path(decl.symbol.as_str(), declaration.symbol.as_str());
            let action = actions
                .iter()
                .find(|action| action.witness == variant.air_witness)
                .expect("verified dynamic staged commit variant");
            let payload = if variant.storage == RirDynStorage::Boxed {
                self.staged_commit(action.payload, "*payload")
            } else {
                self.staged_commit(action.payload, "payload")
            };
            let payload = if variant.storage == RirDynStorage::Boxed {
                format!("Box::new({payload})")
            } else {
                payload
            };
            format!("{path}(payload) => {}", tuple_variant(&path, [payload]))
        });
        match_expr(value, arms)
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
            | RirParamSemantic::DynBorrow
            | RirParamSemantic::ScopedLambda
            | RirParamSemantic::EscapingLambda
            | RirParamSemantic::AnvCallback
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
                    RirConstValue::String(id) => {
                        rust_string(&self.program.string_literal(*id).text)
                    }
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
            RirConstValue::Flag { flag, bits } => {
                target::flag_value(self.program.flags[flag.index()].symbol.as_str(), *bits)
            }
            RirConstValue::Float(value) => target::float_const(*value),
            RirConstValue::Bool(value) => value.to_string(),
            RirConstValue::String(id) => {
                target::string_literal_share(target::statics_param_name(), *id)
            }
            RirConstValue::Char(value) => rust_char(*value),
            RirConstValue::Nil => {
                let RirType::Option(inner) = self.program.types[konst.ty.index()] else {
                    unreachable!("verified nil constant type")
                };
                format!("None::<{}>", self.policy.rust_ty(inner))
            }
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
        if enm.variants.is_empty() {
            return "unreachable!()".to_string();
        }
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
            RirType::Int | RirType::Float | RirType::Bool | RirType::Char | RirType::Flag(_) => {
                format!("(*({expr}))")
            }
            RirType::Lambda(sig) if self.policy.lambda_sig_copyable(sig) => format!("(*({expr}))"),
            RirType::Lambda(_) => unreachable!("verified copyable lambda value"),
            RirType::Struct(id) => {
                let strukt = &self.program.structs[id.index()];
                if strukt.native_path.is_some() {
                    format!("(*({expr}))")
                } else {
                    self.copy_record_from_ref(strukt.symbol.as_str(), &strukt.fields, expr)
                }
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
                "unreachable!()".to_string()
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
