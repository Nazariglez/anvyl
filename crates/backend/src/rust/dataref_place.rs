use super::rir::{
    RirCallArg, RirDataRefId, RirFieldId, RirMutPlaceArg, RirOptionMatch, RirProgram,
    RirProjection, RirRValue, RirStmt, RirStructuredBlock, RirTypeId,
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
        for stmt in &block.stmts {
            self.collect_stmt(program, stmt);
        }
    }

    fn collect_stmt(&mut self, program: &RirProgram, stmt: &RirStmt) {
        match stmt {
            RirStmt::Init { value, .. }
            | RirStmt::Assign { value, .. }
            | RirStmt::Eval(value)
            | RirStmt::CellInit { value, .. }
            | RirStmt::CellSet { value, .. }
            | RirStmt::ScopedPlaceCellSet { value, .. } => {
                self.collect_rvalue(program, value);
            }
            RirStmt::DataRefSet { .. } | RirStmt::MapValueSet { .. } => {}
            RirStmt::If(branch) => {
                self.collect_block(program, &branch.then_block);
                if let Some(block) = &branch.else_block {
                    self.collect_block(program, block);
                }
            }
            RirStmt::Loop(loop_) => self.collect_block(program, &loop_.body),
            RirStmt::CollectionLoanScope(scope) => self.collect_block(program, &scope.body),
            RirStmt::CollectionSlotScope(block) => self.collect_block(program, block),
            RirStmt::EnumMatch(match_) => {
                for arm in &match_.arms {
                    self.collect_block(program, &arm.block);
                }
                if let Some(block) = &match_.else_block {
                    self.collect_block(program, block);
                }
            }
            RirStmt::OptionMatch(match_) => self.collect_option_match(program, match_),
        }
    }

    fn collect_option_match(&mut self, program: &RirProgram, match_: &RirOptionMatch) {
        self.collect_block(program, &match_.some_block);
        self.collect_block(program, &match_.none_block);
    }

    fn collect_rvalue(&mut self, program: &RirProgram, value: &RirRValue) {
        let RirRValue::Call { args, .. } = value else {
            return;
        };
        for arg in args {
            if let RirCallArg::MutPlace(RirMutPlaceArg::DataRefProjection {
                dataref,
                projections,
                ty,
                ..
            }) = arg
            {
                self.intern(program, *dataref, projections, *ty);
            }
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
    let Some((first, rest)) = projections.split_first() else {
        unreachable!("verified dataref place descriptor projection")
    };
    let RirProjection::Field(field_id) = first else {
        unreachable!("verified dataref field projection")
    };
    let field = &program.datarefs[dataref.index()].fields[field_id.index()];
    let mut path = format!("storage.{}", field.symbol.as_str());
    let mut ty = container_for(program, field.ty);
    for projection in rest {
        match projection {
            RirProjection::Field(field_id) => {
                let Some(RirPlaceContainer::Struct(struct_id)) = ty else {
                    unreachable!("verified field projection")
                };
                let field = &program.structs[struct_id.index()].fields[field_id.index()];
                path.push('.');
                path.push_str(field.symbol.as_str());
                ty = container_for(program, field.ty);
            }
            RirProjection::TupleField(field_id) => {
                let Some(RirPlaceContainer::Tuple(tuple_id)) = ty else {
                    unreachable!("verified tuple projection")
                };
                let field = &program.tuples[tuple_id.index()].fields[field_id.index()];
                path.push('.');
                path.push_str(field.symbol.as_str());
                ty = container_for(program, field.ty);
            }
            RirProjection::Index(_) => unreachable!("verified dataref place descriptor projection"),
        }
    }
    path
}

fn descriptor_symbol(
    program: &RirProgram,
    index: usize,
    dataref: RirDataRefId,
    projections: &[RirProjection],
) -> String {
    let dataref_name = program.datarefs[dataref.index()].symbol.as_str();
    let mut parts = vec![format!("anvP{index}"), dataref_name.to_string()];
    let mut ty = None;
    for projection in projections {
        match projection {
            RirProjection::Field(field_id) => {
                let field = match ty {
                    None => &program.datarefs[dataref.index()].fields[field_id.index()],
                    Some(RirPlaceContainer::Struct(struct_id)) => {
                        &program.structs[struct_id.index()].fields[field_id.index()]
                    }
                    Some(RirPlaceContainer::Tuple(_)) => unreachable!("verified field projection"),
                };
                parts.push(field.symbol.as_str().to_string());
                ty = container_for(program, field.ty);
            }
            RirProjection::TupleField(field_id) => {
                let Some(RirPlaceContainer::Tuple(tuple_id)) = ty else {
                    unreachable!("verified tuple projection")
                };
                let field = &program.tuples[tuple_id.index()].fields[field_id.index()];
                parts.push(tuple_field_part(*field_id));
                ty = container_for(program, field.ty);
            }
            RirProjection::Index(_) => unreachable!("verified dataref place descriptor projection"),
        }
    }
    parts.push("place".to_string());
    parts.join("_")
}

#[derive(Clone, Copy)]
enum RirPlaceContainer {
    Struct(super::rir::RirStructId),
    Tuple(super::rir::RirTupleId),
}

fn container_for(program: &RirProgram, ty: RirTypeId) -> Option<RirPlaceContainer> {
    match program.types[ty.index()] {
        super::rir::RirType::Struct(id) => Some(RirPlaceContainer::Struct(id)),
        super::rir::RirType::Tuple(id) => Some(RirPlaceContainer::Tuple(id)),
        _ => None,
    }
}

fn tuple_field_part(field: RirFieldId) -> String {
    format!("field{}", field.index())
}
