use super::{
    rir::{
        RirFunction, RirLocalBinding, RirPassMode, RirPlace, RirPlaceRoot, RirPlaceStep,
        RirPlaceStepKind, RirProgram, RirType, RirTypeId,
    },
    syntax::comma,
    target,
    value::RustValues,
};

struct RenderedPlace {
    expr: String,
    ty: RirTypeId,
}

pub(super) fn dataref_storage_path(program: &RirProgram, storage: &[RirPlaceStep]) -> String {
    let fields = storage
        .iter()
        .map(|step| field_step_symbol(program, step))
        .collect::<Vec<_>>()
        .join(".");
    format!("storage.{fields}")
}

pub(super) fn field_step_symbol<'a>(program: &'a RirProgram, step: &RirPlaceStep) -> &'a str {
    match step.kind {
        RirPlaceStepKind::DataRefField(field) => {
            let RirType::DataRef(dataref) = program.types[step.source_ty.index()] else {
                unreachable!("verified dataref field projection")
            };
            program.datarefs[dataref.index()].fields[field.index()]
                .symbol
                .as_str()
        }
        RirPlaceStepKind::StructField(field) | RirPlaceStepKind::ExternField(field) => {
            let RirType::Struct(strukt) = program.types[step.source_ty.index()] else {
                unreachable!("verified struct field projection")
            };
            program.structs[strukt.index()].fields[field.index()]
                .symbol
                .as_str()
        }
        RirPlaceStepKind::TupleField(field) => {
            let RirType::Tuple(tuple) = program.types[step.source_ty.index()] else {
                unreachable!("verified tuple field projection")
            };
            program.tuples[tuple.index()].fields[field.index()]
                .symbol
                .as_str()
        }
        _ => unreachable!("verified field projection"),
    }
}

pub(super) struct SliceIndexAccess {
    pub slice: String,
    pub index: String,
    pub ty: RirTypeId,
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
    ArrayIndex {
        index: String,
        len: u64,
    },
    ListIndex {
        index: String,
        version: String,
        ty: RirTypeId,
    },
    SliceIndex {
        index: String,
    },
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

    pub(super) fn projected_expr(
        &self,
        root_ty: RirTypeId,
        root: &str,
        slot_ty: RirTypeId,
        projections: &[RirPlaceStep],
    ) -> Option<String> {
        let mut rendered = RenderedPlace {
            expr: root.to_string(),
            ty: root_ty,
        };
        self.apply_projections(&mut rendered, projections, true);
        (rendered.ty == slot_ty).then_some(rendered.expr)
    }

    pub(super) fn shared_borrow_root_param(&self, place: &RirPlace) -> bool {
        let RirPlaceRoot::Local(local) = place.root else {
            unreachable!("expected a local RIR place")
        };
        place.projections.is_empty()
            && matches!(
                self.function.locals[local.index()].binding,
                RirLocalBinding::Parameter {
                    mode: RirPassMode::SharedBorrow,
                    ..
                }
            )
    }

    pub(super) fn physical_ref_root(&self, place: &RirPlace) -> bool {
        let RirPlaceRoot::Local(local) = place.root else {
            return false;
        };
        place.projections.is_empty()
            && matches!(
                self.function.locals[local.index()].binding,
                RirLocalBinding::Parameter {
                    mode: RirPassMode::SharedBorrow,
                    ..
                } | RirLocalBinding::ScopedPlacePayload
            )
    }

    pub(super) fn mut_place_root_param(&self, place: &RirPlace) -> bool {
        let RirPlaceRoot::Local(local) = place.root else {
            unreachable!("expected a local RIR place")
        };
        place.projections.is_empty()
            && matches!(
                self.function.locals[local.index()].binding,
                RirLocalBinding::Parameter {
                    mode: RirPassMode::MutPlace,
                    ..
                } | RirLocalBinding::ScopedPlacePayload
            )
    }

    pub(super) fn mut_place_projection(&self, place: &RirPlace) -> Option<MutPlaceProjection> {
        let RirPlaceRoot::Local(root) = place.root else {
            unreachable!("expected a local RIR place")
        };
        if place.projections.is_empty()
            || !matches!(
                self.function.locals[root.index()].binding,
                RirLocalBinding::Parameter {
                    mode: RirPassMode::MutPlace,
                    ..
                } | RirLocalBinding::ScopedPlacePayload
            )
        {
            return None;
        }
        let local = &self.function.locals[root.index()];
        let payload_ref_root = matches!(local.binding, RirLocalBinding::ScopedPlacePayload);
        let root_expr = if payload_ref_root {
            target::mut_place_scoped_cell(&format!("&{}", local.symbol.as_str()))
        } else {
            local.symbol.as_str().to_string()
        };
        let mut projection = self.projected_mut_place(
            local.ty,
            &root_expr,
            self.program.verified_place_ty(self.function, place),
            &place.projections,
        )?;
        projection.root_owned = payload_ref_root;
        Some(projection)
    }

    pub(super) fn projected_place(
        &self,
        root_ty: RirTypeId,
        root: &str,
        slot_ty: RirTypeId,
        projections: &[RirPlaceStep],
    ) -> Option<MutPlaceProjection> {
        self.projected_place_impl(root_ty, root, slot_ty, projections, false)
    }

    pub(super) fn projected_mut_place(
        &self,
        root_ty: RirTypeId,
        root: &str,
        slot_ty: RirTypeId,
        projections: &[RirPlaceStep],
    ) -> Option<MutPlaceProjection> {
        self.projected_place_impl(root_ty, root, slot_ty, projections, true)
    }

    fn projected_place_impl(
        &self,
        root_ty: RirTypeId,
        root: &str,
        slot_ty: RirTypeId,
        projections: &[RirPlaceStep],
        root_is_mut_place: bool,
    ) -> Option<MutPlaceProjection> {
        if projections.last().map_or(root_ty, |step| step.target_ty) != slot_ty {
            return None;
        }
        let mut ty = root_ty;
        let mut fields = vec![];
        let mut inits = vec![];
        let mut steps = vec![];
        for projection in projections {
            if projection.source_ty != ty {
                return None;
            }
            match projection.kind {
                RirPlaceStepKind::StructField(field_id)
                | RirPlaceStepKind::ExternField(field_id) => {
                    let RirType::Struct(struct_id) = self.program.types[ty.index()] else {
                        return None;
                    };
                    let field = &self.program.structs[struct_id.index()].fields[field_id.index()];
                    steps.push(MutPlaceProjectionStep::Field(
                        field.symbol.as_str().to_string(),
                    ));
                }
                RirPlaceStepKind::TupleField(field_id) => {
                    let RirType::Tuple(tuple_id) = self.program.types[ty.index()] else {
                        return None;
                    };
                    let field = &self.program.tuples[tuple_id.index()].fields[field_id.index()];
                    steps.push(MutPlaceProjectionStep::Field(
                        field.symbol.as_str().to_string(),
                    ));
                }
                RirPlaceStepKind::ArrayIndex { index, len, .. } => {
                    let local = self.function.locals[index.index()].symbol.as_str();
                    let field = format!("__i{}", fields.len());
                    fields.push(format!("{field}: i64"));
                    inits.push((field.clone(), local.to_string()));
                    steps.push(MutPlaceProjectionStep::ArrayIndex { index: field, len });
                }
                RirPlaceStepKind::ListIndex { index, .. } => {
                    let local = self.function.locals[index.index()].symbol.as_str();
                    let field = format!("__i{}", fields.len());
                    fields.push(format!("{field}: i64"));
                    inits.push((field.clone(), local.to_string()));
                    let version = format!("__v{}", fields.len());
                    let body = if root_is_mut_place {
                        target::mut_place_access_ctx(
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
                        ty,
                    });
                }
                RirPlaceStepKind::SliceIndex { index, .. } => {
                    let local = self.function.locals[index.index()].symbol.as_str();
                    let field = format!("__i{}", fields.len());
                    fields.push(format!("{field}: i64"));
                    inits.push((field.clone(), local.to_string()));
                    steps.push(MutPlaceProjectionStep::SliceIndex { index: field });
                }
                RirPlaceStepKind::DataRefField(_) => return None,
            }
            ty = projection.target_ty;
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
            return Some(format!(
                "Ok({})",
                target::collection_structural_version(expr)
            ));
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
                    "{{ let index = {checked}; {} }}",
                    target::list_with_elem_shared_short(
                        expr,
                        target::runtime_param_name(),
                        "index",
                        &target::collection_structural_version(expr),
                        &body,
                    )
                ))
            }
            MutPlaceProjectionStep::SliceIndex { .. } => None,
        }
    }

    pub(super) fn slice_index_access(&self, place: &RirPlace) -> Option<SliceIndexAccess> {
        let (last, prefix) = place.projections.split_last()?;
        let (index, list_root) = match last.kind {
            RirPlaceStepKind::ListIndex { index, .. } => (index, true),
            RirPlaceStepKind::SliceIndex { index, .. } => (index, false),
            _ => return None,
        };
        let RirPlaceRoot::Local(root) = place.root else {
            unreachable!("expected a local RIR place")
        };
        let base = RirPlace::local(root, prefix.to_vec());
        let rendered = self.local_place_with_ty(&base);
        Some(SliceIndexAccess {
            slice: rendered.expr,
            index: self.function.locals[index.index()]
                .symbol
                .as_str()
                .to_string(),
            ty: rendered.ty,
            list_root,
        })
    }

    fn root_needs_deref(&self, place: &RirPlace) -> bool {
        let RirPlaceRoot::Local(local) = place.root else {
            unreachable!("expected a local RIR place")
        };
        matches!(
            self.function.locals[local.index()].binding,
            RirLocalBinding::DirectPayload
                | RirLocalBinding::Parameter {
                    mode: RirPassMode::MutBorrow,
                    ..
                }
        )
    }

    fn apply_projections(
        &self,
        rendered: &mut RenderedPlace,
        projections: &[RirPlaceStep],
        allow_list_index: bool,
    ) {
        for projection in projections {
            debug_assert_eq!(rendered.ty, projection.source_ty);
            match projection.kind {
                RirPlaceStepKind::StructField(_)
                | RirPlaceStepKind::ExternField(_)
                | RirPlaceStepKind::TupleField(_) => {
                    rendered.expr.push('.');
                    rendered
                        .expr
                        .push_str(field_step_symbol(self.program, projection));
                }
                RirPlaceStepKind::ArrayIndex { index, len, .. } => {
                    let index = self.function.locals[index.index()].symbol.as_str();
                    let array = rendered.expr.clone();
                    let checked = target::checked_index_result(index, &len.to_string(), "array");
                    rendered.expr = format!("({array})[{checked}]");
                }
                RirPlaceStepKind::ListIndex {
                    index,
                    elem_materializer,
                } if allow_list_index => {
                    let index = self.function.locals[index.index()].symbol.as_str();
                    let materialize = RustValues::new(self.program, self.function)
                        .materialize_ref(elem_materializer, "value");
                    let materialize = target::materializer_closure("value", &materialize);
                    let list = rendered.expr.clone();
                    let checked = target::checked_index_result(index, "__anv_list.len()", "list");
                    rendered.expr = format!(
                        "{{ let __anv_list = &({list}); let index = {checked}; {}? }}",
                        target::list_elem_at_shared(
                            "__anv_list",
                            target::runtime_param_name(),
                            "index",
                            &target::collection_structural_version("__anv_list"),
                            &materialize,
                        )
                    );
                }
                RirPlaceStepKind::SliceIndex {
                    index,
                    elem_materializer,
                } => {
                    let index = self.function.locals[index.index()].symbol.as_str();
                    let materialize = RustValues::new(self.program, self.function)
                        .materialize_ref(elem_materializer, "value");
                    let materialize = target::materializer_closure("value", &materialize);
                    rendered.expr = format!(
                        "{}?",
                        target::slice_elem_at_shared(
                            &rendered.expr,
                            target::runtime_param_name(),
                            index,
                            &materialize,
                        )
                    );
                }
                RirPlaceStepKind::ListIndex { .. } | RirPlaceStepKind::DataRefField(_) => {
                    unreachable!("verified projection")
                }
            }
            rendered.ty = projection.target_ty;
        }
    }
}
