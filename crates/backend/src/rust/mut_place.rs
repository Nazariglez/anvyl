use anvyx_frontend::air::{self, FunctionId, Place};

use super::{
    PlanCx, PlannedCallArg, RustPlanError, RustTargetGapKind, RustTargetGapSite,
    dataref_mut_place::{DataRefMutPlace, DataRefMutPlaceSupport, projected_ty},
    rir::{RirCallArg, RirLocal, RirLocalId, RirMutPlaceArg, RirMutPlaceRoot, RirOperand},
};

impl PlanCx<'_> {
    pub(super) fn plan_source_mut_place_arg(
        &self,
        function: FunctionId,
        place: &Place,
        locals: &mut Vec<RirLocal>,
    ) -> Result<PlannedCallArg, RustPlanError> {
        match self.dataref_mut_place_support(function, place) {
            DataRefMutPlaceSupport::Supported(supported) => {
                return Ok(self.plan_dataref_mut_place_arg(function, place, locals, supported));
            }
            DataRefMutPlaceSupport::UnsupportedDataRef => {
                return Self::unsupported_mut_place(
                    function,
                    RustTargetGapKind::UnsupportedMutablePlaceDataRef,
                );
            }
            DataRefMutPlaceSupport::Ordinary => {}
        }

        if !place.projection.is_empty() {
            return self.plan_projected_local_mut_place_arg(function, place);
        }

        self.plan_root_mut_place_arg(function, place)
    }

    fn plan_dataref_mut_place_arg(
        &self,
        function: FunctionId,
        place: &Place,
        locals: &mut Vec<RirLocal>,
        supported: DataRefMutPlace,
    ) -> PlannedCallArg {
        let mut stmts = vec![];
        let (_, object) = self.dataref_root_place(function, place, locals, &mut stmts);
        let arg = RirMutPlaceArg::dataref(
            RirOperand::Place(object),
            self.dataref_map[&supported.dataref],
            supported
                .projections
                .iter()
                .map(Self::rir_projection)
                .collect(),
            self.type_map[&supported.ty],
        );
        PlannedCallArg {
            stmts,
            arg: RirCallArg::MutPlace(arg),
        }
    }

    fn plan_projected_local_mut_place_arg(
        &self,
        function: FunctionId,
        place: &Place,
    ) -> Result<PlannedCallArg, RustPlanError> {
        let (root_ty, root, allow_collections) =
            if let Some(cell) = self.place_capture_cell(function, place) {
                let ty = self.air.capture_cells[cell.index()].ty;
                let root = match self.classify_capture_cell_storage(cell) {
                    super::rir::RirCellStorage::StackScoped => RirMutPlaceRoot::StackCell {
                        cell: self.capture_cell_ref(function, cell),
                        ty: self.type_map[&ty],
                    },
                    super::rir::RirCellStorage::Heap => RirMutPlaceRoot::HeapCell {
                        cell: self.capture_cell_ref(function, cell),
                        ty: self.type_map[&ty],
                    },
                };
                (ty, root, false)
            } else if let Some(borrow) = self.place_scoped_borrow(function, place) {
                let ty = self.air.scoped_borrows[borrow.index()].ty;
                (
                    ty,
                    RirMutPlaceRoot::ScopedPlaceCell {
                        cell: self.scoped_place_cell_ref(function, borrow),
                        ty: self.type_map[&ty],
                    },
                    false,
                )
            } else {
                let Some(root) = place.root.local() else {
                    return Self::unsupported_mut_place(
                        function,
                        RustTargetGapKind::UnsupportedMutablePlaceProjection,
                    );
                };
                let (root_ty, _) = self.current_place_root(function, place);
                let local = RirLocalId::from_index(root.index());
                let root = if self.place_is_source_mut_place_param(function, place) {
                    RirMutPlaceRoot::Param {
                        local,
                        ty: self.type_map[&root_ty],
                    }
                } else {
                    RirMutPlaceRoot::Local {
                        local,
                        ty: self.type_map[&root_ty],
                    }
                };
                (root_ty, root, true)
            };
        if !self.projected_mut_place_supported(root_ty, place, allow_collections) {
            return Self::unsupported_mut_place(
                function,
                RustTargetGapKind::UnsupportedMutablePlaceProjection,
            );
        }
        Ok(PlannedCallArg::from_arg(RirCallArg::MutPlace(
            RirMutPlaceArg::projected(
                root,
                place.projection.iter().map(Self::rir_projection).collect(),
                self.type_map[&place.ty],
            ),
        )))
    }

    fn projected_mut_place_supported(
        &self,
        root_ty: air::TypeId,
        place: &Place,
        allow_collections: bool,
    ) -> bool {
        let mut ty = root_ty;
        let mut slice_dynamic = false;
        for projection in &place.projection {
            match (self.air.type_arena.data(ty), projection) {
                (
                    air::TypeData::Aggregate(_) | air::TypeData::Tuple(_),
                    air::Projection::Field(_) | air::Projection::TupleField(_),
                )
                | (air::TypeData::Array { .. }, air::Projection::Index(_)) => {
                    let Some(next) = projected_ty(self.air, ty, projection) else {
                        return false;
                    };
                    ty = next;
                }
                (air::TypeData::List(_), air::Projection::Index(_))
                | (air::TypeData::Map { .. }, air::Projection::MapIndex(_))
                    if allow_collections && !slice_dynamic =>
                {
                    let Some(next) = projected_ty(self.air, ty, projection) else {
                        return false;
                    };
                    ty = next;
                }
                (air::TypeData::Slice(_), air::Projection::Index(_)) if allow_collections => {
                    let Some(next) = projected_ty(self.air, ty, projection) else {
                        return false;
                    };
                    ty = next;
                    slice_dynamic = true;
                }
                _ => return false,
            }
        }
        true
    }

    fn plan_root_mut_place_arg(
        &self,
        function: FunctionId,
        place: &Place,
    ) -> Result<PlannedCallArg, RustPlanError> {
        let ty = self.type_map[&place.ty];
        if let Some(cell) = self.place_capture_cell(function, place) {
            let cell_ref = self.capture_cell_ref(function, cell);
            let arg = match self.classify_capture_cell_storage(cell) {
                super::rir::RirCellStorage::StackScoped => RirMutPlaceArg::stack_cell(cell_ref, ty),
                super::rir::RirCellStorage::Heap => RirMutPlaceArg::heap_cell(cell_ref, ty),
            };
            return Ok(PlannedCallArg::from_arg(RirCallArg::MutPlace(arg)));
        }

        if let Some(borrow) = self.place_scoped_borrow(function, place) {
            let arg =
                RirMutPlaceArg::scoped_place_cell(self.scoped_place_cell_ref(function, borrow), ty);
            return Ok(PlannedCallArg::from_arg(RirCallArg::MutPlace(arg)));
        }

        let Some(root) = place.root.local() else {
            return Self::unsupported_mut_place(
                function,
                RustTargetGapKind::UnsupportedMutablePlace,
            );
        };
        let local = RirLocalId::from_index(root.index());
        let arg = if self.place_is_source_mut_place_param(function, place) {
            RirMutPlaceArg::param(local, ty)
        } else {
            RirMutPlaceArg::local(self.rir_root_place(local, place.ty))
        };
        Ok(PlannedCallArg::from_arg(RirCallArg::MutPlace(arg)))
    }

    pub(super) fn plan_native_mut_borrow_arg(
        &self,
        function: FunctionId,
        place: &Place,
    ) -> Result<PlannedCallArg, RustPlanError> {
        if !place.projection.is_empty()
            || self.place_is_source_mut_place_param(function, place)
            || self.place_capture_cell(function, place).is_some()
            || self.place_scoped_borrow(function, place).is_some()
        {
            return Self::unsupported_mut_place(
                function,
                RustTargetGapKind::UnsupportedMutablePlaceNativeBoundary,
            );
        }
        if self.place_crosses_dataref(function, place) {
            return Self::unsupported_mut_place(
                function,
                RustTargetGapKind::UnsupportedMutablePlaceDataRef,
            );
        }
        Ok(PlannedCallArg::from_arg(RirCallArg::MutBorrow(
            self.plan_place_in_function(function, place),
        )))
    }

    fn unsupported_mut_place<T>(
        function: FunctionId,
        kind: RustTargetGapKind,
    ) -> Result<T, RustPlanError> {
        Err(Self::gap(RustTargetGapSite::Function(function), kind))
    }

    fn dataref_mut_place_support(
        &self,
        function: FunctionId,
        place: &Place,
    ) -> DataRefMutPlaceSupport {
        let root_ty = match place.root {
            air::PlaceRoot::CaptureCell(cell) => self.air.capture_cells[cell.index()].ty,
            air::PlaceRoot::ScopedBorrow(borrow) => self.air.scoped_borrows[borrow.index()].ty,
            air::PlaceRoot::Global(_) => return DataRefMutPlaceSupport::Ordinary,
            air::PlaceRoot::Local(_) | air::PlaceRoot::LambdaCapture(_) => {
                self.current_place_root(function, place).0
            }
        };
        super::dataref_mut_place::classify(self.air, root_ty, place)
    }
}
