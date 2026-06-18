use super::rir::{
    RirCallArg, RirDataRefId, RirFieldId, RirMutPlaceAccess, RirOptionMatch, RirOptionSubject,
    RirPlaceModel, RirProgram, RirProjection, RirRValue, RirStmt, RirStructuredBlock, RirTypeId,
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
            | RirStmt::GlobalSetRoot { value, .. }
            | RirStmt::GlobalUpdateRoot { value, .. }
            | RirStmt::MutPlaceSet { value, .. }
            | RirStmt::Eval(value)
            | RirStmt::CellInit { value, .. }
            | RirStmt::CellSet { value, .. }
            | RirStmt::ScopedPlaceCellSet { value, .. } => {
                self.collect_rvalue(program, value);
            }
            RirStmt::GlobalEnsure { .. }
            | RirStmt::DataRefSet { .. }
            | RirStmt::MapValueSet { .. } => {}
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
        if let RirOptionSubject::MutPlace(arg) = &match_.subject
            && let RirMutPlaceAccess::DataRef { dataref, .. } = arg.access
        {
            self.intern(program, dataref, &arg.projections, arg.ty);
        }
        self.collect_block(program, &match_.some_block);
        self.collect_block(program, &match_.none_block);
    }

    fn collect_rvalue(&mut self, program: &RirProgram, value: &RirRValue) {
        let RirRValue::Call { args, .. } = value else {
            return;
        };
        for arg in args {
            if let RirCallArg::MutPlace(arg) = arg
                && let RirMutPlaceAccess::DataRef { dataref, .. } = arg.access
            {
                self.intern(program, dataref, &arg.projections, arg.ty);
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
            RirProjection::Index(_) | RirProjection::MapIndex(_) => {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rust::rir::{RirDataRef, RirField, RirStruct, RirStructId, RirSymbol, RirType};

    fn ty(index: usize) -> RirTypeId {
        RirTypeId::from_index(index)
    }

    fn field(index: usize, name: &str, ty: RirTypeId) -> RirField {
        RirField {
            id: RirFieldId::from_index(index),
            symbol: RirSymbol::new(name),
            ty,
        }
    }

    #[test]
    fn descriptor_paths_use_rir_storage_resolver() {
        let dataref = RirDataRefId::from_index(0);
        let program = RirProgram {
            types: vec![RirType::Int, RirType::Struct(RirStructId::from_index(0))],
            structs: vec![RirStruct {
                id: RirStructId::from_index(0),
                air_id: None,
                symbol: RirSymbol::new("Payload"),
                display: RirSymbol::new("Payload"),
                native_path: None,
                native_key: None,
                copyable: true,
                fields: vec![field(0, "inner", ty(0))],
            }],
            datarefs: vec![RirDataRef {
                id: dataref,
                air_id: anvyx_frontend::air::AggregateId::from_index(0),
                symbol: RirSymbol::new("Box"),
                display: RirSymbol::new("Box"),
                cycle_capable: false,
                fields: vec![field(0, "payload", ty(1))],
            }],
            ..RirProgram::default()
        };
        let projections = [
            RirProjection::Field(RirFieldId::from_index(0)),
            RirProjection::Field(RirFieldId::from_index(0)),
        ];

        assert_eq!(
            storage_path(&program, dataref, &projections),
            "storage.payload.inner"
        );
        assert_eq!(
            descriptor_symbol(&program, 0, dataref, &projections),
            "anvP0_Box_payload_inner_place"
        );
    }
}
