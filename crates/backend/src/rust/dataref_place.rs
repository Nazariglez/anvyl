use super::rir::{
    RirCallArg, RirChild, RirCollectionAccess, RirDataRefId, RirDynBorrow, RirDynBorrowSource,
    RirFieldId, RirMutPlaceAccess, RirMutPlaceArg, RirPlaceModel, RirProgram, RirProjection,
    RirStructuredBlock, RirTypeId,
};

impl DataRefPlaceDescriptor {
    pub(super) fn heap_type_field(&self, program: &RirProgram) -> String {
        program.datarefs[self.dataref.index()].heap_type_symbol()
    }

    pub(super) fn storage_path(&self, program: &RirProgram) -> String {
        storage_path(program, self.dataref, &self.projections)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct DataRefPlaceDescriptor {
    pub(super) symbol: String,
    pub(super) dataref: RirDataRefId,
    pub(super) projections: Vec<RirProjection>,
    pub(super) ty: RirTypeId,
}

#[derive(Debug, Default, Clone)]
pub(super) struct DataRefPlaceDescriptors {
    descriptors: Vec<DataRefPlaceDescriptor>,
}

impl DataRefPlaceDescriptors {
    pub(super) fn build(program: &RirProgram) -> Self {
        let mut descriptors = Self::default();
        for function in &program.functions {
            descriptors.collect_block(program, &function.body);
        }
        descriptors
    }

    pub(super) fn all(&self) -> &[DataRefPlaceDescriptor] {
        &self.descriptors
    }

    fn collect_block(&mut self, program: &RirProgram, block: &RirStructuredBlock) {
        block.for_each_child(&mut |child| match child {
            RirChild::MutPlace { place, .. } => self.collect_mut_place_arg(program, place),
            RirChild::Collection { collection, .. } => {
                self.collect_collection_access(program, collection);
            }
            RirChild::CallArg(arg) => {
                arg.for_each_owned_value(&mut |owned| {
                    if let super::rir::RirOwnedOperand::Access(place) = &owned.value {
                        self.collect_mut_place_arg(program, place);
                    }
                });
                match arg {
                    RirCallArg::MutPlace(place) => self.collect_mut_place_arg(program, place),
                    RirCallArg::DynBorrow(borrow) => self.collect_dyn_borrow(program, borrow),
                    _ => {}
                }
            }
            RirChild::Block(block) => self.collect_block(program, block),
            RirChild::CaptureArg(super::rir::RirLambdaCaptureArg::Owned { value }) => {
                if let super::rir::RirOwnedOperand::Access(place) = &value.value {
                    self.collect_mut_place_arg(program, place);
                }
            }
            RirChild::Operand { .. }
            | RirChild::Place { .. }
            | RirChild::CaptureArg(_)
            | RirChild::LocalRead(_)
            | RirChild::Tail(_) => {}
        });
    }

    fn collect_collection_access(&mut self, program: &RirProgram, access: &RirCollectionAccess) {
        if let RirCollectionAccess::MutPlace(arg) = access {
            self.collect_mut_place_arg(program, arg);
        }
    }

    fn collect_dyn_borrow(&mut self, program: &RirProgram, borrow: &RirDynBorrow) {
        match &borrow.source {
            RirDynBorrowSource::Concrete { place, .. }
            | RirDynBorrowSource::Owned { place, .. } => {
                self.collect_mut_place_arg(program, place);
            }
            RirDynBorrowSource::Borrowed { .. } | RirDynBorrowSource::Reborrowed { .. } => {}
        }
    }

    fn collect_mut_place_arg(&mut self, program: &RirProgram, arg: &RirMutPlaceArg) {
        if let RirMutPlaceAccess::DataRef { dataref, .. } = &arg.access {
            let Ok((path, consumed)) =
                RirPlaceModel::new(program).dataref_storage_prefix(*dataref, &arg.projections)
            else {
                return;
            };
            self.intern(program, *dataref, &arg.projections[..consumed], path.ty());
        }
    }

    fn intern(
        &mut self,
        program: &RirProgram,
        dataref: RirDataRefId,
        projections: &[RirProjection],
        ty: RirTypeId,
    ) {
        if self.find(dataref, projections, ty).is_some() {
            return;
        }
        let symbol = descriptor_symbol(program, self.descriptors.len(), dataref, projections);
        self.descriptors.push(DataRefPlaceDescriptor {
            symbol,
            dataref,
            projections: projections.to_vec(),
            ty,
        });
    }

    pub(super) fn find(
        &self,
        dataref: RirDataRefId,
        projections: &[RirProjection],
        ty: RirTypeId,
    ) -> Option<&DataRefPlaceDescriptor> {
        self.descriptors.iter().find(|descriptor| {
            descriptor.dataref == dataref
                && descriptor.projections == projections
                && descriptor.ty == ty
        })
    }
}

pub(super) fn storage_path(
    program: &RirProgram,
    dataref: RirDataRefId,
    projections: &[RirProjection],
) -> String {
    let path = RirPlaceModel::new(program)
        .dataref_storage_path(dataref, projections)
        .expect("verified dataref place descriptor projection");
    let fields = path
        .steps()
        .iter()
        .map(|step| step.symbol.as_str())
        .collect::<Vec<_>>()
        .join(".");
    format!("storage.{fields}")
}

fn descriptor_symbol(
    program: &RirProgram,
    index: usize,
    dataref: RirDataRefId,
    projections: &[RirProjection],
) -> String {
    let dataref_name = program.datarefs[dataref.index()].symbol.as_str();
    let path = RirPlaceModel::new(program)
        .dataref_storage_path(dataref, projections)
        .expect("verified dataref place descriptor projection");
    let mut parts = vec![format!("anvP{index}"), dataref_name.to_string()];
    for step in path.steps() {
        match step.projection {
            RirProjection::Field(_) => parts.push(step.symbol.as_str().to_string()),
            RirProjection::TupleField(field) => parts.push(tuple_field_part(field)),
            RirProjection::Index(_) => {
                unreachable!("verified dataref place descriptor projection")
            }
        }
    }
    parts.push("place".to_string());
    parts.join("_")
}

fn tuple_field_part(field: RirFieldId) -> String {
    format!("field{}", field.index())
}
