use anvyx_frontend::air::{self, Place, Projection, TypeData, TypeId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum DataRefMutPlaceSupport {
    Ordinary,
    Supported(DataRefMutPlace),
    UnsupportedDataRef,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct DataRefMutPlace {
    pub(super) dataref: air::AggregateId,
    pub(super) projections: Vec<Projection>,
    pub(super) ty: TypeId,
}

pub(super) fn classify(
    program: &air::Program,
    root_ty: TypeId,
    place: &Place,
) -> DataRefMutPlaceSupport {
    if place.projection.is_empty() {
        return DataRefMutPlaceSupport::Ordinary;
    }

    let mut current_ty = root_ty;
    for (prefix_len, projection) in place.projection.iter().enumerate() {
        if let TypeData::DataRef(dataref) = program.type_arena.data(current_ty) {
            if prefix_len != 0 {
                return DataRefMutPlaceSupport::UnsupportedDataRef;
            }
            return classify_storage_projection(program, *dataref, &place.projection[prefix_len..]);
        }
        let Some(next) = projected_ty(program, current_ty, projection) else {
            return DataRefMutPlaceSupport::Ordinary;
        };
        current_ty = next;
    }
    DataRefMutPlaceSupport::Ordinary
}

fn classify_storage_projection(
    program: &air::Program,
    dataref: air::AggregateId,
    projections: &[Projection],
) -> DataRefMutPlaceSupport {
    let Some((Projection::Field(field), rest)) = projections.split_first() else {
        return DataRefMutPlaceSupport::UnsupportedDataRef;
    };
    let Some(field) = program.aggregate(dataref).fields.get(field.index()) else {
        return DataRefMutPlaceSupport::UnsupportedDataRef;
    };
    let mut ty = field.ty;
    for projection in rest {
        if matches!(program.type_arena.data(ty), TypeData::DataRef(_)) {
            return DataRefMutPlaceSupport::UnsupportedDataRef;
        }
        let Some(next) = inline_projected_ty(program, ty, projection) else {
            return DataRefMutPlaceSupport::UnsupportedDataRef;
        };
        ty = next;
    }
    if !supported_payload(program, ty, rest.is_empty()) {
        return DataRefMutPlaceSupport::UnsupportedDataRef;
    }
    DataRefMutPlaceSupport::Supported(DataRefMutPlace {
        dataref,
        projections: projections.to_vec(),
        ty,
    })
}

fn supported_payload(program: &air::Program, ty: TypeId, direct: bool) -> bool {
    match program.type_arena.data(ty) {
        TypeData::Int | TypeData::Float | TypeData::Bool => true,
        TypeData::DataRef(_) => direct,
        _ => false,
    }
}

fn inline_projected_ty(
    program: &air::Program,
    ty: TypeId,
    projection: &Projection,
) -> Option<TypeId> {
    match (program.type_arena.data(ty), projection) {
        (TypeData::Aggregate(aggregate), Projection::Field(field)) => program
            .aggregate(*aggregate)
            .fields
            .get(field.index())
            .map(|field| field.ty),
        (TypeData::Tuple(fields), Projection::TupleField(field)) => {
            fields.get(*field as usize).copied()
        }
        _ => None,
    }
}

pub(super) fn projected_ty(
    program: &air::Program,
    ty: TypeId,
    projection: &Projection,
) -> Option<TypeId> {
    match (program.type_arena.data(ty), projection) {
        (
            TypeData::Aggregate(aggregate) | TypeData::DataRef(aggregate),
            Projection::Field(field),
        ) => program
            .aggregate(*aggregate)
            .fields
            .get(field.index())
            .map(|field| field.ty),
        (
            TypeData::Array { elem, .. } | TypeData::List(elem) | TypeData::Slice(elem),
            Projection::Index(_),
        ) => Some(*elem),
        (TypeData::Map { value, .. }, Projection::MapIndex(_)) => program
            .type_arena
            .iter()
            .enumerate()
            .find_map(|(index, ty)| {
                matches!(ty, TypeData::Optional(inner) if *inner == *value)
                    .then(|| TypeId::from_index(index))
            }),
        (TypeData::Tuple(fields), Projection::TupleField(field)) => {
            fields.get(*field as usize).copied()
        }
        _ => None,
    }
}
