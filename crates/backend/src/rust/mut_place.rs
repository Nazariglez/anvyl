use anvyx_frontend::air::{
    self, FunctionId, LocalKind, Mutability, ParamMode, Place, PlaceRoot, Program,
};

use super::{
    PlanCx, PlannedCallArg, RustPlanError, RustTargetGapKind, RustTargetGapSite,
    place_access::{PlaceAccessGapKind, PlaceAccessIntent, PlaceAccessPlan, PlaceAccessRoot},
    rir::{RirCallArg, RirLocal, RirLocalId, RirMutPlaceArg, RirMutPlaceHandle},
};

pub(super) fn direct_native_mut_borrow_supported(
    program: &Program,
    function: FunctionId,
    place: &Place,
) -> bool {
    let PlaceRoot::Local(local_id) = place.root else {
        return false;
    };
    if !place.projection.is_empty() {
        return false;
    }
    let function = program.function(function);
    let Some(local) = function.locals.get(local_id.index()) else {
        return false;
    };
    local.kind == LocalKind::User
        && local.mutability == Mutability::Mutable
        && !function
            .signature
            .params
            .iter()
            .any(|param| param.local_id == local_id && param.mode == ParamMode::MutBorrow)
}

impl PlanCx<'_> {
    pub(super) fn plan_source_mut_place_arg(
        &self,
        function: FunctionId,
        place: &Place,
        locals: &mut Vec<RirLocal>,
    ) -> Result<PlannedCallArg, RustPlanError> {
        let plan = self
            .access()
            .plan(function, PlaceAccessIntent::MutPlaceArg, place)
            .map_err(|gap| Self::access_gap(function, gap))?;
        if plan.crosses_dataref {
            return Ok(self.plan_dataref_mut_place_arg(function, &plan, locals));
        }
        if plan.projection.is_empty() {
            return self.plan_root_mut_place_arg(function, &plan);
        }
        self.plan_projected_mut_place_arg(function, &plan)
    }

    fn plan_dataref_mut_place_arg(
        &self,
        function: FunctionId,
        plan: &PlaceAccessPlan,
        locals: &mut Vec<RirLocal>,
    ) -> PlannedCallArg {
        let mut stmts = vec![];
        let segment = self.dataref_mut_place_segment(function, plan, locals, &mut stmts);
        let arg = RirMutPlaceArg::dataref(
            segment.object,
            segment.dataref,
            segment.projections,
            self.type_map[&plan.ty],
        );
        PlannedCallArg {
            stmts,
            arg: RirCallArg::MutPlace(arg),
        }
    }

    fn plan_projected_mut_place_arg(
        &self,
        function: FunctionId,
        plan: &PlaceAccessPlan,
    ) -> Result<PlannedCallArg, RustPlanError> {
        let root_ty = plan.projection[0].source_ty;
        let handle = self.mut_place_handle(function, plan.root, root_ty)?;
        Ok(PlannedCallArg::from_arg(RirCallArg::MutPlace(
            RirMutPlaceArg::projected(
                handle,
                plan.projection
                    .iter()
                    .map(Self::rir_plan_projection)
                    .collect(),
                self.type_map[&plan.ty],
            ),
        )))
    }

    fn plan_root_mut_place_arg(
        &self,
        function: FunctionId,
        plan: &PlaceAccessPlan,
    ) -> Result<PlannedCallArg, RustPlanError> {
        let ty = self.type_map[&plan.ty];
        let arg = match plan.root {
            PlaceAccessRoot::Local {
                local,
                source_mut_param: true,
                ..
            } => RirMutPlaceArg::param(RirLocalId::from_index(local.index()), ty),
            PlaceAccessRoot::Local { local, .. } => RirMutPlaceArg::local(
                self.rir_root_place(RirLocalId::from_index(local.index()), plan.ty),
            ),
            PlaceAccessRoot::CaptureCell(cell) => {
                let cell_ref = self.capture_cell_ref(function, cell);
                match self.classify_capture_cell_storage(cell) {
                    super::rir::RirCellStorage::StackScoped => {
                        RirMutPlaceArg::stack_cell(cell_ref, ty)
                    }
                    super::rir::RirCellStorage::Heap => RirMutPlaceArg::heap_cell(cell_ref, ty),
                }
            }
            PlaceAccessRoot::ScopedPlaceCell(borrow) => {
                RirMutPlaceArg::scoped_place_cell(self.scoped_place_cell_ref(function, borrow), ty)
            }
            PlaceAccessRoot::Global(global) => RirMutPlaceArg::global(self.global_map[&global], ty),
            PlaceAccessRoot::LambdaCapture(_) => {
                return Self::unsupported_mut_place(
                    function,
                    RustTargetGapKind::UnsupportedMutablePlace,
                );
            }
        };
        Ok(PlannedCallArg::from_arg(RirCallArg::MutPlace(arg)))
    }

    fn mut_place_handle(
        &self,
        function: FunctionId,
        root: PlaceAccessRoot,
        ty: air::TypeId,
    ) -> Result<RirMutPlaceHandle, RustPlanError> {
        let rir_ty = self.type_map[&ty];
        match root {
            PlaceAccessRoot::Local {
                local,
                source_mut_param: true,
                ..
            } => Ok(RirMutPlaceHandle::Param {
                local: RirLocalId::from_index(local.index()),
                ty: rir_ty,
            }),
            PlaceAccessRoot::Local { local, .. } => Ok(RirMutPlaceHandle::Local {
                local: RirLocalId::from_index(local.index()),
                ty: rir_ty,
            }),
            PlaceAccessRoot::CaptureCell(cell) => {
                let cell_ref = self.capture_cell_ref(function, cell);
                Ok(match self.classify_capture_cell_storage(cell) {
                    super::rir::RirCellStorage::StackScoped => RirMutPlaceHandle::StackCell {
                        cell: cell_ref,
                        ty: rir_ty,
                    },
                    super::rir::RirCellStorage::Heap => RirMutPlaceHandle::HeapCell {
                        cell: cell_ref,
                        ty: rir_ty,
                    },
                })
            }
            PlaceAccessRoot::ScopedPlaceCell(borrow) => Ok(RirMutPlaceHandle::ScopedPlaceCell {
                cell: self.scoped_place_cell_ref(function, borrow),
                ty: rir_ty,
            }),
            PlaceAccessRoot::Global(global) => Ok(RirMutPlaceHandle::Global {
                global: self.global_map[&global],
                ty: rir_ty,
            }),
            PlaceAccessRoot::LambdaCapture(_) => Self::unsupported_mut_place(
                function,
                RustTargetGapKind::UnsupportedMutablePlaceProjection,
            ),
        }
    }

    pub(super) fn plan_native_mut_borrow_arg(
        &self,
        function: FunctionId,
        place: &Place,
    ) -> Result<PlannedCallArg, RustPlanError> {
        self.access()
            .plan(function, PlaceAccessIntent::NativeMutBorrow, place)
            .map_err(|gap| Self::access_gap(function, gap))?;
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

    fn access_gap(function: FunctionId, kind: PlaceAccessGapKind) -> RustPlanError {
        let kind = match kind {
            PlaceAccessGapKind::PlaceProjection => RustTargetGapKind::UnsupportedPlaceProjection,
            PlaceAccessGapKind::GlobalAccess => RustTargetGapKind::UnsupportedGlobalAccess,
            PlaceAccessGapKind::GlobalBorrow => RustTargetGapKind::UnsupportedGlobalBorrow,
            PlaceAccessGapKind::GlobalProjection => RustTargetGapKind::UnsupportedGlobalProjection,
            PlaceAccessGapKind::GlobalValueRead => RustTargetGapKind::UnsupportedGlobalValueRead,
            PlaceAccessGapKind::GlobalRooting => RustTargetGapKind::UnsupportedGlobalRooting,
            PlaceAccessGapKind::GlobalType => RustTargetGapKind::UnsupportedGlobalType,
            PlaceAccessGapKind::MutablePlace => RustTargetGapKind::UnsupportedMutablePlace,
            PlaceAccessGapKind::MutablePlaceProjection => {
                RustTargetGapKind::UnsupportedMutablePlaceProjection
            }
            PlaceAccessGapKind::MutablePlaceDataRef => {
                RustTargetGapKind::UnsupportedMutablePlaceDataRef
            }
            PlaceAccessGapKind::MutablePlaceNativeBoundary => {
                RustTargetGapKind::UnsupportedMutablePlaceNativeBoundary
            }
            PlaceAccessGapKind::SliceView => RustTargetGapKind::UnsupportedSliceView,
            PlaceAccessGapKind::ReturnPlace => RustTargetGapKind::UnsupportedReturnMode,
        };
        Self::gap(RustTargetGapSite::Function(function), kind)
    }
}
