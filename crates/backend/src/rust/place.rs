use super::{
    dataref_place::{self as dataref_paths},
    rir::{
        RirDataRefId, RirField, RirFunction, RirLocalId, RirMutPlaceArg, RirOptionSubject,
        RirParamAbi, RirPlace, RirPlaceModel, RirPlaceRoot, RirProgram, RirProjection, RirStmt,
        RirStructuredBlock, RirType, RirTypeId, stmt_child_blocks_any,
    },
    syntax::comma,
    target,
};

struct RenderedPlace {
    expr: String,
    ty: RirTypeId,
}

pub(super) struct SliceIndexAccess {
    pub slice: String,
    pub index: String,
    pub list_root: bool,
}

pub(super) struct MutPlaceProjection {
    pub root: String,
    pub root_owned: bool,
    pub root_ty: RirTypeId,
    pub slot_ty: RirTypeId,
    pub fields: Vec<String>,
    pub inits: Vec<(String, String)>,
    pub steps: Vec<MutPlaceProjectionStep>,
}

pub(super) enum MutPlaceProjectionStep {
    Field(String),
    ArrayIndex { index: String, len: u64 },
    ListIndex { index: String, version: String },
    SliceIndex { index: String },
}

pub(super) struct ProjectionFacts {
    pub(super) fallible_projection: bool,
}

pub(super) fn place_dynamic_facts(
    program: &RirProgram,
    function: &RirFunction,
    place: &RirPlace,
) -> Option<ProjectionFacts> {
    let RirPlaceRoot::Local(local) = place.root else {
        return None;
    };
    dynamic_facts_from(
        program,
        function.locals.get(local.index())?.ty,
        &place.projections,
    )
}

pub(super) fn mut_place_dynamic_facts(
    program: &RirProgram,
    arg: &RirMutPlaceArg,
) -> Option<ProjectionFacts> {
    dynamic_facts_from(program, arg.access.ty()?, &arg.projections)
}

fn dynamic_facts_from(
    program: &RirProgram,
    ty: RirTypeId,
    projections: &[RirProjection],
) -> Option<ProjectionFacts> {
    let fallible_projection =
        RirPlaceModel::new(program).projection_dynamic_facts(ty, projections)?;
    Some(ProjectionFacts {
        fallible_projection,
    })
}

pub(super) fn projected_ops_ctor(ops: &str, inits: &[(String, String)]) -> String {
    if inits.is_empty() {
        return ops.to_string();
    }
    let mut lets = vec![];
    let mut fields = vec![];
    for (field, value) in inits {
        lets.push(format!("let {field} = {value};"));
        fields.push(field.clone());
    }
    format!("{{ {} {ops} {{ {} }} }}", lets.join(" "), comma(fields))
}

#[derive(Clone, Copy)]
pub(super) struct RustPlaces<'a> {
    program: &'a RirProgram,
    function: &'a RirFunction,
}

impl<'a> RustPlaces<'a> {
    pub(super) fn new(program: &'a RirProgram, function: &'a RirFunction) -> Self {
        Self { program, function }
    }

    pub(super) fn local_place(&self, place: &RirPlace) -> String {
        self.local_place_with_ty(place).expr
    }

    fn local_place_with_ty(&self, place: &RirPlace) -> RenderedPlace {
        let RirPlaceRoot::Local(root) = place.root else {
            unreachable!("expected a local RIR place")
        };
        let local = &self.function.locals[root.index()];
        let mut rendered = RenderedPlace {
            expr: if self.root_needs_deref(place) {
                format!("(*{})", local.symbol.as_str())
            } else {
                local.symbol.as_str().to_string()
            },
            ty: local.ty,
        };
        self.apply_projections(&mut rendered, &place.projections, true);
        rendered
    }

    pub(super) fn storage_path(
        &self,
        dataref: RirDataRefId,
        projections: &[RirProjection],
    ) -> String {
        dataref_paths::storage_path(self.program, dataref, projections)
    }

    pub(super) fn projected_expr(
        &self,
        root_ty: RirTypeId,
        root: &str,
        slot_ty: RirTypeId,
        projections: &[RirProjection],
    ) -> Option<String> {
        let mut rendered = RenderedPlace {
            expr: root.to_string(),
            ty: root_ty,
        };
        self.apply_projections(&mut rendered, projections, true);
        (rendered.ty == slot_ty).then_some(rendered.expr)
    }

    pub(super) fn record_field_place(&self, place: &RirPlace, field: &RirField) -> RirPlace {
        let projection = match self.program.types[place.ty.index()] {
            RirType::Struct(_) => RirProjection::Field(field.id),
            RirType::Tuple(_) => RirProjection::TupleField(field.id),
            _ => unreachable!("verified record field place"),
        };
        let mut child = place.clone();
        child.projections.push(projection);
        child.ty = field.ty;
        child
    }

    pub(super) fn shared_borrow_root_param(&self, place: &RirPlace) -> bool {
        let RirPlaceRoot::Local(local) = place.root else {
            unreachable!("expected a local RIR place")
        };
        place.projections.is_empty()
            && self.param_abi_for_local(local) == Some(RirParamAbi::SharedBorrow)
    }

    pub(super) fn mut_place_root_param(&self, place: &RirPlace) -> bool {
        let RirPlaceRoot::Local(local) = place.root else {
            unreachable!("expected a local RIR place")
        };
        place.projections.is_empty()
            && (self.local_is_mut_place_param(local) || self.payload_ref_cell_local(local))
    }

    pub(super) fn mut_place_projection(&self, place: &RirPlace) -> Option<MutPlaceProjection> {
        let RirPlaceRoot::Local(root) = place.root else {
            unreachable!("expected a local RIR place")
        };
        if place.projections.is_empty()
            || !(self.local_is_mut_place_param(root) || self.payload_ref_cell_local(root))
        {
            return None;
        }
        let local = &self.function.locals[root.index()];
        let payload_ref_root = self.payload_ref_cell_local(root);
        let root_expr = if payload_ref_root {
            target::mut_place_scoped_cell(&format!("&{}", local.symbol.as_str()))
        } else {
            local.symbol.as_str().to_string()
        };
        let mut projection =
            self.projected_mut_place(local.ty, &root_expr, place.ty, &place.projections)?;
        projection.root_owned = payload_ref_root;
        Some(projection)
    }

    pub(super) fn projected_place(
        &self,
        root_ty: RirTypeId,
        root: &str,
        slot_ty: RirTypeId,
        projections: &[RirProjection],
    ) -> Option<MutPlaceProjection> {
        self.projected_place_impl(root_ty, root, slot_ty, projections, false)
    }

    pub(super) fn projected_mut_place(
        &self,
        root_ty: RirTypeId,
        root: &str,
        slot_ty: RirTypeId,
        projections: &[RirProjection],
    ) -> Option<MutPlaceProjection> {
        self.projected_place_impl(root_ty, root, slot_ty, projections, true)
    }

    fn projected_place_impl(
        &self,
        root_ty: RirTypeId,
        root: &str,
        slot_ty: RirTypeId,
        projections: &[RirProjection],
        root_is_mut_place: bool,
    ) -> Option<MutPlaceProjection> {
        let path = RirPlaceModel::new(self.program).projection_path(root_ty, projections)?;
        if path.ty() != slot_ty {
            return None;
        }
        let mut ty = root_ty;
        let mut fields = vec![];
        let mut inits = vec![];
        let mut steps = vec![];
        for projection in projections {
            match projection {
                RirProjection::Field(field_id) => {
                    let RirType::Struct(struct_id) = self.program.types[ty.index()] else {
                        return None;
                    };
                    let field = &self.program.structs[struct_id.index()].fields[field_id.index()];
                    steps.push(MutPlaceProjectionStep::Field(
                        field.symbol.as_str().to_string(),
                    ));
                    ty = field.ty;
                }
                RirProjection::TupleField(field_id) => {
                    let RirType::Tuple(tuple_id) = self.program.types[ty.index()] else {
                        return None;
                    };
                    let field = &self.program.tuples[tuple_id.index()].fields[field_id.index()];
                    steps.push(MutPlaceProjectionStep::Field(
                        field.symbol.as_str().to_string(),
                    ));
                    ty = field.ty;
                }
                RirProjection::Index(index) => {
                    let local = self.function.locals[index.index()].symbol.as_str();
                    let field = format!("__i{}", fields.len());
                    fields.push(format!("{field}: i64"));
                    inits.push((field.clone(), local.to_string()));
                    match self.program.types[ty.index()] {
                        RirType::Array { elem, len } => {
                            steps.push(MutPlaceProjectionStep::ArrayIndex { index: field, len });
                            ty = elem;
                        }
                        RirType::List(elem) => {
                            let version = format!("__v{}", fields.len());
                            let body = if root_is_mut_place {
                                target::mut_place_access(
                                    root,
                                    target::runtime_param_name(),
                                    &Self::projection_version_body(&steps)?,
                                )
                            } else {
                                format!("{}?", Self::projection_version_body_from(root, &steps)?)
                            };
                            fields.push(format!("{version}: u64"));
                            inits.push((version.clone(), body));
                            steps.push(MutPlaceProjectionStep::ListIndex {
                                index: field,
                                version,
                            });
                            ty = elem;
                        }
                        RirType::Slice(elem) => {
                            steps.push(MutPlaceProjectionStep::SliceIndex { index: field });
                            ty = elem;
                        }
                        _ => return None,
                    }
                }
            }
        }
        Some(MutPlaceProjection {
            root: root.to_string(),
            root_owned: false,
            root_ty,
            slot_ty,
            fields,
            inits,
            steps,
        })
    }

    fn projection_version_body(steps: &[MutPlaceProjectionStep]) -> Option<String> {
        Self::projection_version_body_from("value", steps)
    }

    fn projection_version_body_from(
        expr: &str,
        steps: &[MutPlaceProjectionStep],
    ) -> Option<String> {
        let Some((step, rest)) = steps.split_first() else {
            return Some(format!("Ok({expr}.structural_version())"));
        };
        match step {
            MutPlaceProjectionStep::Field(field) => {
                Self::projection_version_body_from(&format!("{expr}.{field}"), rest)
            }
            MutPlaceProjectionStep::ArrayIndex { index, len } => {
                let checked = target::checked_index_result(index, &len.to_string(), "array");
                let body = Self::projection_version_body_from(&format!("{expr}[index]"), rest)?;
                Some(format!("{{ let index = {checked}; {body} }}"))
            }
            MutPlaceProjectionStep::ListIndex { index, .. } => {
                let checked = target::checked_index_result(index, &format!("{expr}.len()"), "list");
                let body = Self::projection_version_body_from("value", rest)?;
                Some(format!(
                    "{{ let index = {checked}; {expr}.with_elem_shared_short(rt, index, {expr}.structural_version(), |value| {{ {body} }}) }}"
                ))
            }
            MutPlaceProjectionStep::SliceIndex { .. } => None,
        }
    }

    pub(super) fn dynamic_place_access(&self, place: &RirPlace) -> Option<SliceIndexAccess> {
        self.slice_index_dynamic_access(place)
    }

    fn slice_index_dynamic_access(&self, place: &RirPlace) -> Option<SliceIndexAccess> {
        let (last, prefix) = place.projections.split_last()?;
        let RirProjection::Index(index) = last else {
            return None;
        };
        let RirPlaceRoot::Local(root) = place.root else {
            unreachable!("expected a local RIR place")
        };
        let mut base =
            RirPlace::local(root, prefix.to_vec(), self.function.locals[root.index()].ty);
        for projection in prefix {
            base.ty = match (self.program.types[base.ty.index()], projection) {
                (RirType::Struct(id), RirProjection::Field(field)) => {
                    self.program.structs[id.index()].fields[field.index()].ty
                }
                (RirType::Tuple(id), RirProjection::TupleField(field)) => {
                    self.program.tuples[id.index()].fields[field.index()].ty
                }
                (RirType::Array { elem, .. } | RirType::List(elem), RirProjection::Index(_)) => {
                    elem
                }
                _ => return None,
            };
        }
        let rendered = self.local_place_with_ty(&base);
        let list_root = match self.program.types[rendered.ty.index()] {
            RirType::Slice(_) => false,
            RirType::List(_) => true,
            _ => return None,
        };
        Some(SliceIndexAccess {
            slice: rendered.expr,
            index: self.function.locals[index.index()]
                .symbol
                .as_str()
                .to_string(),
            list_root,
        })
    }

    fn root_needs_deref(&self, place: &RirPlace) -> bool {
        let RirPlaceRoot::Local(local) = place.root else {
            unreachable!("expected a local RIR place")
        };
        self.function.locals[local.index()].payload_ref && !self.payload_ref_cell_local(local)
            || self.param_abi_for_local(local) == Some(RirParamAbi::MutBorrow)
    }

    fn local_is_mut_place_param(&self, local: RirLocalId) -> bool {
        self.param_abi_for_local(local) == Some(RirParamAbi::MutPlace)
    }

    pub(super) fn payload_ref_cell_local(&self, local: RirLocalId) -> bool {
        self.function.locals[local.index()].payload_ref
            && block_has_mut_place_payload(&self.function.body, local)
    }

    fn param_abi_for_local(&self, local: RirLocalId) -> Option<RirParamAbi> {
        self.function
            .params
            .iter()
            .find(|param| param.local == local)
            .map(|param| param.abi)
    }

    fn apply_projections(
        &self,
        rendered: &mut RenderedPlace,
        projections: &[RirProjection],
        allow_list_index: bool,
    ) {
        for projection in projections {
            match projection {
                RirProjection::Field(field_id) => {
                    let RirType::Struct(struct_id) = self.program.types[rendered.ty.index()] else {
                        unreachable!("verified field projection")
                    };
                    let field = &self.program.structs[struct_id.index()].fields[field_id.index()];
                    rendered.expr.push('.');
                    rendered.expr.push_str(field.symbol.as_str());
                    rendered.ty = field.ty;
                }
                RirProjection::TupleField(field_id) => {
                    let RirType::Tuple(tuple_id) = self.program.types[rendered.ty.index()] else {
                        unreachable!("verified tuple projection")
                    };
                    let field = &self.program.tuples[tuple_id.index()].fields[field_id.index()];
                    rendered.expr.push('.');
                    rendered.expr.push_str(field.symbol.as_str());
                    rendered.ty = field.ty;
                }
                RirProjection::Index(index) => {
                    let index = self.function.locals[index.index()].symbol.as_str();
                    match self.program.types[rendered.ty.index()] {
                        RirType::Array { elem, len } => {
                            let array = rendered.expr.clone();
                            let checked =
                                target::checked_index_result(index, &len.to_string(), "array");
                            rendered.expr = format!("({array})[{checked}]");
                            rendered.ty = elem;
                        }
                        RirType::List(elem) if allow_list_index => {
                            let list = rendered.expr.clone();
                            let checked =
                                target::checked_index_result(index, "__anv_list.len()", "list");
                            rendered.expr = format!(
                                "{{ let __anv_list = &({list}); let index = {checked}; __anv_list.elem_at_shared({}, index, __anv_list.structural_version())? }}",
                                target::runtime_param_name()
                            );
                            rendered.ty = elem;
                        }
                        RirType::Slice(elem) => {
                            rendered.expr = format!(
                                "{}?",
                                target::slice_elem_at_shared(
                                    &rendered.expr,
                                    target::runtime_param_name(),
                                    index,
                                )
                            );
                            rendered.ty = elem;
                        }
                        _ => unreachable!("verified index projection"),
                    }
                }
            }
        }
    }
}

fn block_has_mut_place_payload(block: &RirStructuredBlock, local: RirLocalId) -> bool {
    block.stmts.iter().any(|stmt| {
        stmt_has_mut_place_payload(stmt, local)
            || stmt_child_blocks_any(stmt, |block| block_has_mut_place_payload(block, local))
    })
}

fn stmt_has_mut_place_payload(stmt: &RirStmt, local: RirLocalId) -> bool {
    match stmt {
        RirStmt::OptionMatch(match_) => {
            match_.payload == Some(local)
                && match_.payload_ref
                && matches!(match_.subject, RirOptionSubject::MutPlace(_))
        }
        RirStmt::MapEntryMatch(match_) => match_.payload == Some(local),
        _ => false,
    }
}
