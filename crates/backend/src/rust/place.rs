use super::{
    dataref_place::storage_path as dataref_storage_path,
    rep_policy::RustRepPolicy,
    rir::{
        RirDataRefId, RirField, RirFunction, RirLocalId, RirParamAbi, RirPlace, RirPlaceRoot,
        RirProgram, RirProjection, RirType, RirTypeId,
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
    MapIndex { key: String, value_ty: RirTypeId },
}

pub(super) struct MapSlotAccess {
    pub map: String,
    pub key: String,
    pub key_value: String,
    pub value_ty: RirTypeId,
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
            unreachable!("global RIR places are not supported here")
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
        dataref_storage_path(self.program, dataref, projections)
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
            unreachable!("global RIR places are not supported here")
        };
        place.projections.is_empty()
            && self.param_abi_for_local(local) == Some(RirParamAbi::SharedBorrow)
    }

    pub(super) fn mut_place_root_param(&self, place: &RirPlace) -> bool {
        let RirPlaceRoot::Local(local) = place.root else {
            unreachable!("global RIR places are not supported here")
        };
        place.projections.is_empty() && self.local_is_mut_place_param(local)
    }

    pub(super) fn mut_place_projection(&self, place: &RirPlace) -> Option<MutPlaceProjection> {
        let RirPlaceRoot::Local(root) = place.root else {
            unreachable!("global RIR places are not supported here")
        };
        if place.projections.is_empty() || !self.local_is_mut_place_param(root) {
            return None;
        }
        let local = &self.function.locals[root.index()];
        self.projected_place(
            local.ty,
            local.symbol.as_str(),
            place.ty,
            &place.projections,
        )
    }

    pub(super) fn projected_place(
        &self,
        root_ty: RirTypeId,
        root: &str,
        slot_ty: RirTypeId,
        projections: &[RirProjection],
    ) -> Option<MutPlaceProjection> {
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
                RirProjection::MapIndex(index) => {
                    let RirType::Map { key, value } = self.program.types[ty.index()] else {
                        return None;
                    };
                    let field = format!("__k{}", fields.len());
                    fields.push(format!(
                        "{field}: {}",
                        RustRepPolicy::new(self.program).rust_ty(key)
                    ));
                    inits.push((field.clone(), self.captured_local_value(*index, key)));
                    steps.push(MutPlaceProjectionStep::MapIndex {
                        key: field,
                        value_ty: value,
                    });
                    ty = self.option_ty(value)?;
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
                            let body = self.projection_version_body(&steps)?;
                            fields.push(format!("{version}: u64"));
                            inits.push((
                                version.clone(),
                                target::mut_place_access(root, target::runtime_param_name(), &body),
                            ));
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
        (ty == slot_ty).then(|| MutPlaceProjection {
            root: root.to_string(),
            root_ty,
            slot_ty,
            fields,
            inits,
            steps,
        })
    }

    fn projection_version_body(&self, steps: &[MutPlaceProjectionStep]) -> Option<String> {
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
                    "{{ let index = {checked}; let value = &{expr}.as_slice()[index]; {body} }}"
                ))
            }
            MutPlaceProjectionStep::SliceIndex { .. } | MutPlaceProjectionStep::MapIndex { .. } => {
                None
            }
        }
    }

    pub(super) fn map_slot_access(&self, place: &RirPlace) -> Option<MapSlotAccess> {
        let (last, prefix) = place.projections.split_last()?;
        let RirProjection::MapIndex(index) = last else {
            return None;
        };
        let RirPlaceRoot::Local(root_local) = place.root else {
            unreachable!("global RIR places are not supported here")
        };
        let local = &self.function.locals[root_local.index()];
        let root = local.symbol.as_str().to_string();
        let mut rendered = RenderedPlace {
            expr: if self.root_needs_deref(place) {
                format!("(*{})", local.symbol.as_str())
            } else {
                root.clone()
            },
            ty: local.ty,
        };
        self.apply_projections(&mut rendered, prefix, true);
        let RirType::Map { key, value } = self.program.types[rendered.ty.index()] else {
            return None;
        };
        Some(MapSlotAccess {
            map: rendered.expr,
            key: self.function.locals[index.index()]
                .symbol
                .as_str()
                .to_string(),
            key_value: self.captured_local_value(*index, key),
            value_ty: value,
        })
    }

    pub(super) fn slice_index_access(&self, place: &RirPlace) -> Option<SliceIndexAccess> {
        let (last, prefix) = place.projections.split_last()?;
        let RirProjection::Index(index) = last else {
            return None;
        };
        let RirPlaceRoot::Local(root) = place.root else {
            unreachable!("global RIR places are not supported here")
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
            unreachable!("global RIR places are not supported here")
        };
        self.function.locals[local.index()].payload_ref
            || self.param_abi_for_local(local) == Some(RirParamAbi::MutBorrow)
    }

    fn captured_local_value(&self, local: RirLocalId, ty: RirTypeId) -> String {
        let symbol = self.function.locals[local.index()].symbol.as_str();
        let policy = RustRepPolicy::new(self.program);
        if policy.cow_value(ty) {
            format!("{symbol}.share()")
        } else if policy.copyable(ty) {
            symbol.to_string()
        } else {
            format!("{symbol}.clone()")
        }
    }

    fn option_ty(&self, inner: RirTypeId) -> Option<RirTypeId> {
        self.program
            .types
            .iter()
            .position(|ty| matches!(ty, RirType::Option(found) if *found == inner))
            .map(RirTypeId::from_index)
    }

    fn local_is_mut_place_param(&self, local: RirLocalId) -> bool {
        self.param_abi_for_local(local) == Some(RirParamAbi::MutPlace)
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
                RirProjection::MapIndex(_) => {
                    unreachable!("map slot projection has no Rust lvalue path")
                }
                RirProjection::Index(index) => {
                    let index = self.function.locals[index.index()].symbol.as_str();
                    match self.program.types[rendered.ty.index()] {
                        RirType::Array { elem, len } => {
                            rendered.expr.push('[');
                            rendered
                                .expr
                                .push_str(&target::checked_index(index, &len.to_string()));
                            rendered.expr.push(']');
                            rendered.ty = elem;
                        }
                        RirType::List(elem) if allow_list_index => {
                            let len = format!("{}.len()", rendered.expr);
                            rendered.expr.push('[');
                            rendered.expr.push_str(&target::checked_index(index, &len));
                            rendered.expr.push(']');
                            rendered.ty = elem;
                        }
                        RirType::Slice(elem) => {
                            rendered.expr =
                                format!("{}?", target::slice_elem_at_shared(&rendered.expr, index));
                            rendered.ty = elem;
                        }
                        _ => unreachable!("verified index projection"),
                    }
                }
            }
        }
    }
}
