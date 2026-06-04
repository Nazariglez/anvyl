use super::{
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
        let dataref = &self.program.datarefs[dataref.index()];
        let Some((first, rest)) = projections.split_first() else {
            unreachable!("verified dataref projection")
        };
        let RirProjection::Field(field_id) = first else {
            unreachable!("verified dataref field projection")
        };
        let field = &dataref.fields[field_id.index()];
        let mut rendered = RenderedPlace {
            expr: format!("storage.{}", field.symbol.as_str()),
            ty: field.ty,
        };
        self.apply_projections(&mut rendered, rest, false);
        rendered.expr
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
        place.projections.is_empty() && self.param_abi(place) == Some(RirParamAbi::SharedBorrow)
    }

    fn root_needs_deref(&self, place: &RirPlace) -> bool {
        self.function.locals[place.local.index()].payload_ref
            || self.param_abi(place) == Some(RirParamAbi::MutBorrow)
    }

    fn param_abi(&self, place: &RirPlace) -> Option<RirParamAbi> {
        self.function
            .params
            .iter()
            .find(|param| param.local == place.local)
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
                    let (elem, len) = match self.program.types[rendered.ty.index()] {
                        RirType::Array { elem, len } => (elem, len.to_string()),
                        RirType::List(elem) if allow_list_index => {
                            (elem, format!("{}.len()", rendered.expr))
                        }
                        _ => unreachable!("verified index projection"),
                    };
                    let index = self.function.locals[index.index()].symbol.as_str();
                    rendered.expr.push('[');
                    rendered.expr.push_str(&target::checked_index(index, &len));
                    rendered.expr.push(']');
                    rendered.ty = elem;
                }
            }
        }
    }
}
