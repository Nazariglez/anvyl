use super::{
    dataref_place::storage_path as dataref_storage_path,
    rir::{
        RirDataRefId, RirField, RirFunction, RirParamAbi, RirPlace, RirProgram, RirProjection,
        RirType, RirTypeId,
    },
    target,
};

struct RenderedPlace {
    expr: String,
    ty: RirTypeId,
}

pub(super) struct SliceIndexAccess {
    pub slice: String,
    pub root: String,
    pub index: String,
    pub root_is_mut_place: bool,
    pub list_root: bool,
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
        let local = &self.function.locals[place.local.index()];
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
        place.projections.is_empty()
            && self.param_abi_for_local(place.local) == Some(RirParamAbi::SharedBorrow)
    }

    pub(super) fn mut_place_root_param(&self, place: &RirPlace) -> bool {
        place.projections.is_empty() && self.local_is_mut_place_param(place.local)
    }

    pub(super) fn slice_index_access(&self, place: &RirPlace) -> Option<SliceIndexAccess> {
        let (last, prefix) = place.projections.split_last()?;
        let RirProjection::Index(index) = last else {
            return None;
        };
        let mut base = RirPlace {
            local: place.local,
            projections: prefix.to_vec(),
            ty: self.function.locals[place.local.index()].ty,
        };
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
        let root_is_mut_place = self.local_is_mut_place_param(place.local);
        let list_root = match self.program.types[rendered.ty.index()] {
            RirType::Slice(_) => false,
            RirType::List(_) => true,
            _ => return None,
        };
        let root = self.function.locals[place.local.index()]
            .symbol
            .as_str()
            .to_string();
        Some(SliceIndexAccess {
            slice: rendered.expr,
            root,
            index: self.function.locals[index.index()]
                .symbol
                .as_str()
                .to_string(),
            root_is_mut_place,
            list_root,
        })
    }

    fn root_needs_deref(&self, place: &RirPlace) -> bool {
        self.function.locals[place.local.index()].payload_ref
            || self.param_abi_for_local(place.local) == Some(RirParamAbi::MutBorrow)
    }

    fn local_is_mut_place_param(&self, local: super::rir::RirLocalId) -> bool {
        self.param_abi_for_local(local) == Some(RirParamAbi::MutPlace)
    }

    fn param_abi_for_local(&self, local: super::rir::RirLocalId) -> Option<RirParamAbi> {
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
