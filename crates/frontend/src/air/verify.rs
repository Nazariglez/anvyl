use super::{
    BasicBlock, Function, Program, TypeData, VariantShape,
    body::{
        AggregateCtor, Builtin, Callee, Operand, Place, Projection, RValue, Statement, Terminator,
    },
    ids::*,
};

#[derive(Clone, PartialEq, Eq)]
pub struct VerifyError {
    pub site: VerifySite,
    pub kind: VerifyErrorKind,
}

impl std::fmt::Display for VerifyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}: {:?}", self.site, self.kind)
    }
}

impl std::fmt::Debug for VerifyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "VerifyError {{ site: {:?}, kind: {:?} }}",
            self.site, self.kind
        )
    }
}

impl std::error::Error for VerifyError {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VerifySite {
    Program,
    Module(ModuleId),
    Type(TypeId),
    Const(ConstId),
    Aggregate(AggregateId),
    Enum(EnumId),
    ExternType(ExternTypeId),
    Extern(ExternId),
    Function(FunctionId),
    Block {
        function: FunctionId,
        block: BlockId,
    },
    Statement {
        function: FunctionId,
        block: BlockId,
        index: usize,
    },
    Terminator {
        function: FunctionId,
        block: BlockId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VerifyErrorKind {
    BadReference(BadReference),
    BadFunction(BadFunction),
    BadPlace(BadPlace),
    BadCall(BadCall),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadReference {
    InvalidEntry(FunctionId),
    InvalidFunction(FunctionId),
    InvalidExtern(ExternId),
    InvalidExternType(ExternTypeId),
    InvalidAggregate(AggregateId),
    InvalidEnum(EnumId),
    InvalidType(TypeId),
    InvalidConst(ConstId),
    InvalidLocal(LocalId),
    InvalidBlock(BlockId),
    InvalidField {
        aggregate: AggregateId,
        field: FieldId,
    },
    InvalidVariant {
        enum_id: EnumId,
        variant: VariantId,
    },
    InvalidModule(ModuleId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadFunction {
    FunctionHasNoBlocks,
    ParamLocalOutOfRange {
        param: usize,
        total_locals: usize,
    },
    DuplicateParamLocal {
        first: LocalId,
        second: LocalId,
    },
    IfCondMustBeBool(TypeId),
    SwitchDiscriminantMustBeEnum(TypeId),
    DuplicateSwitchArm(VariantId),
    SwitchArmVariantMismatch {
        expected_enum: EnumId,
        variant: VariantId,
    },
    NonVoidFunctionMustReturnValue(TypeId),
    VoidFunctionMustReturnNone,
    ReturnedTypeMismatch {
        expected: TypeId,
        found: TypeId,
    },
    LenSourceMustBeCountable(TypeId),
    ListElementTypeMismatch {
        expected: TypeId,
        found: TypeId,
    },
    MapKeyTypeMismatch {
        expected: TypeId,
        found: TypeId,
    },
    MapValueTypeMismatch {
        expected: TypeId,
        found: TypeId,
    },
    MapGetResultMustBeOptionalValue {
        expected_value: TypeId,
        found: TypeId,
    },
    ListPopResultMustBeOptionalElement {
        expected_elem: TypeId,
        found: TypeId,
    },
    ListSliceResultMustBeList {
        found: TypeId,
    },
    SliceIndexMustBeInt {
        which: &'static str,
        found: TypeId,
    },
    CastMustUseNumericScalars {
        value: TypeId,
        target: TypeId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadPlace {
    FieldProjectionOnNonAggregate(TypeId),
    TupleFieldOutOfRange { ty: TypeId, index: u16, len: usize },
    TupleProjectionOnNonTuple(TypeId),
    VariantFieldOutOfRange { ty: TypeId, index: u16, len: usize },
    VariantProjectionOnNonEnum(TypeId),
    IndexProjectionOnNonIndexable(TypeId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BadCall {
    ClosureCalleeMustBeFunction,
    ArityMismatch {
        expected: usize,
        found: usize,
    },
    ArgTypeMismatch {
        index: usize,
        expected: TypeId,
        found: TypeId,
    },
}

pub fn verify(program: &Program) -> Result<VerifiedProgram<'_>, Vec<VerifyError>> {
    let mut cx = VerifyCx::new(program);
    collect_errors(&mut cx);
    if cx.errors.is_empty() {
        Ok(VerifiedProgram { program })
    } else {
        Err(cx.errors)
    }
}

pub struct VerifiedProgram<'a> {
    program: &'a Program,
}

impl std::fmt::Debug for VerifiedProgram<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("VerifiedProgram").finish()
    }
}

impl VerifiedProgram<'_> {
    pub fn program(&self) -> &Program {
        self.program
    }
}

struct VerifyCx<'a> {
    program: &'a Program,
    errors: Vec<VerifyError>,
}

impl<'a> VerifyCx<'a> {
    fn new(program: &'a Program) -> Self {
        Self {
            program,
            errors: Vec::new(),
        }
    }

    fn push(&mut self, site: VerifySite, kind: VerifyErrorKind) {
        self.errors.push(VerifyError { site, kind });
    }

    fn fn_site(&self, function_id: FunctionId) -> VerifySite {
        VerifySite::Function(function_id)
    }

    fn term_site(&self, function_id: FunctionId, block_id: BlockId) -> VerifySite {
        VerifySite::Terminator {
            function: function_id,
            block: block_id,
        }
    }

    fn stmt_site(&self, function_id: FunctionId, block_id: BlockId, index: usize) -> VerifySite {
        VerifySite::Statement {
            function: function_id,
            block: block_id,
            index,
        }
    }

    fn has_type(&self, id: TypeId) -> bool {
        id.index() < self.program.type_arena.len()
    }

    fn has_aggregate(&self, id: AggregateId) -> bool {
        id.index() < self.program.aggregates.len()
    }

    fn has_enum(&self, id: EnumId) -> bool {
        id.index() < self.program.enums.len()
    }

    fn has_extern_type(&self, id: ExternTypeId) -> bool {
        id.index() < self.program.extern_types.len()
    }

    fn has_function(&self, id: FunctionId) -> bool {
        id.index() < self.program.functions.len()
    }

    fn has_extern(&self, id: ExternId) -> bool {
        id.index() < self.program.externs.len()
    }

    fn has_const(&self, id: ConstId) -> bool {
        id.index() < self.program.const_arena.len()
    }

    fn verify_type_ref(&mut self, site: VerifySite, ty: TypeId) {
        if !self.has_type(ty) {
            self.push(
                site,
                VerifyErrorKind::BadReference(BadReference::InvalidType(ty)),
            );
            return;
        }
        verify_type(self, ty);
    }

    fn verify_block_ref(&mut self, site: VerifySite, function: &Function, block: BlockId) {
        if block.index() >= function.body.len() {
            self.push(
                site,
                VerifyErrorKind::BadReference(BadReference::InvalidBlock(block)),
            );
        }
    }

    fn variant_belongs_to_enum(&self, enum_id: EnumId, variant: VariantId) -> bool {
        let enm = self.program.enum_decl(enum_id);
        variant.index() < enm.variants.len()
    }

    fn is_numeric_scalar_type(&self, ty: TypeId) -> bool {
        matches!(
            self.program.type_data(ty),
            TypeData::Int | TypeData::Float | TypeData::Double
        )
    }
}

fn collect_errors(cx: &mut VerifyCx<'_>) {
    if let Some(entry) = cx.program.entry {
        if !cx.has_function(entry) {
            cx.push(
                VerifySite::Program,
                VerifyErrorKind::BadReference(BadReference::InvalidEntry(entry)),
            );
        }
    }

    for (id, _) in cx.program.modules.iter().enumerate() {
        verify_module(cx, ModuleId::from_index(id));
    }
    for (id, _) in cx.program.aggregates.iter().enumerate() {
        verify_aggregate(cx, AggregateId::from_index(id));
    }
    for (id, _) in cx.program.enums.iter().enumerate() {
        verify_enum(cx, EnumId::from_index(id));
    }
    for (id, _) in cx.program.extern_types.iter().enumerate() {
        verify_extern_type(cx, ExternTypeId::from_index(id));
    }
    for (id, _) in cx.program.externs.iter().enumerate() {
        verify_extern(cx, ExternId::from_index(id));
    }
    for (id, _) in cx.program.functions.iter().enumerate() {
        verify_function(cx, FunctionId::from_index(id));
    }
}

fn verify_module(_cx: &mut VerifyCx<'_>, _module_id: ModuleId) {
    // TODO: will verify module-internal references here
}

fn verify_aggregate(cx: &mut VerifyCx<'_>, id: AggregateId) {
    let agg = cx.program.aggregate(id);
    let site = VerifySite::Aggregate(id);
    for field in &agg.fields {
        cx.verify_type_ref(site.clone(), field.ty);
    }
}

fn verify_enum(cx: &mut VerifyCx<'_>, id: EnumId) {
    let enm = cx.program.enum_decl(id);
    for variant in &enm.variants {
        match &variant.shape {
            VariantShape::Unit => {}
            VariantShape::Tuple(types) => {
                for ty in types {
                    cx.verify_type_ref(VerifySite::Enum(id), *ty);
                }
            }
            VariantShape::Struct(fields) => {
                for field in fields {
                    cx.verify_type_ref(VerifySite::Enum(id), field.ty);
                }
            }
        }
    }
}

fn verify_extern_type(cx: &mut VerifyCx<'_>, id: ExternTypeId) {
    let ty = cx.program.extern_type(id);
    let site = VerifySite::ExternType(id);
    for field in &ty.fields {
        cx.verify_type_ref(site.clone(), field.ty);
    }
    for method in &ty.methods {
        for param in &method.params {
            cx.verify_type_ref(site.clone(), *param);
        }
        cx.verify_type_ref(site.clone(), method.return_type);
    }
    for static_ in &ty.statics {
        for param in &static_.params {
            cx.verify_type_ref(site.clone(), *param);
        }
        cx.verify_type_ref(site.clone(), static_.return_type);
    }
    for op in &ty.operators {
        if let Some(operand) = op.operand {
            cx.verify_type_ref(site.clone(), operand);
        }
        cx.verify_type_ref(site.clone(), op.return_type);
    }
}

fn verify_extern(cx: &mut VerifyCx<'_>, id: ExternId) {
    let ext = cx.program.extern_decl(id);
    let site = VerifySite::Extern(id);
    for param in &ext.params {
        cx.verify_type_ref(site.clone(), *param);
    }
    cx.verify_type_ref(site, ext.return_type);
}

fn verify_function(cx: &mut VerifyCx<'_>, id: FunctionId) {
    let func = cx.program.function(id);
    let site = cx.fn_site(id);

    // param-local integrity
    let mut seen_locals = std::collections::HashSet::new();
    for (i, param) in func.signature.params.iter().enumerate() {
        let is_out_of_range = param.local_id.index() >= func.locals.len();
        if is_out_of_range {
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::ParamLocalOutOfRange {
                    param: i,
                    total_locals: func.locals.len(),
                }),
            );
        } else if !seen_locals.insert(param.local_id) {
            let first_idx = func.signature.params[..i]
                .iter()
                .position(|p| p.local_id == param.local_id)
                .unwrap();
            cx.push(
                site.clone(),
                VerifyErrorKind::BadFunction(BadFunction::DuplicateParamLocal {
                    first: func.signature.params[first_idx].local_id,
                    second: param.local_id,
                }),
            );
        }
    }

    // local type refs
    for local in &func.locals {
        cx.verify_type_ref(site.clone(), local.ty);
    }

    // return type ref
    cx.verify_type_ref(site, func.signature.return_type);

    // body
    if func.body.is_empty() {
        cx.push(
            VerifySite::Function(id),
            VerifyErrorKind::BadFunction(BadFunction::FunctionHasNoBlocks),
        );
    } else {
        for (block_idx, block) in func.body.iter().enumerate() {
            verify_block(cx, id, BlockId::from_index(block_idx), block);
        }
    }
}

fn verify_block(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    block: &BasicBlock,
) {
    for (stmt_idx, stmt) in block.statements.iter().enumerate() {
        verify_statement(cx, function_id, block_id, stmt_idx, stmt);
    }
    verify_terminator(cx, function_id, block_id, &block.terminator);
}

fn verify_statement(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    index: usize,
    stmt: &Statement,
) {
    match stmt {
        Statement::Assign { dst, value } => {
            verify_place(cx, function_id, block_id, Some(index), dst);
            verify_rvalue(cx, function_id, block_id, Some(index), value);
        }
        Statement::Eval(value) => {
            verify_rvalue(cx, function_id, block_id, Some(index), value);
        }
    }
}

fn verify_terminator(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    term: &Terminator,
) {
    let function = cx.program.function(function_id);
    let site = cx.term_site(function_id, block_id);

    match term {
        Terminator::Goto(target) => {
            cx.verify_block_ref(site, function, *target);
        }
        Terminator::If {
            cond,
            then_bb,
            else_bb,
        } => {
            cx.verify_block_ref(site.clone(), function, *then_bb);
            cx.verify_block_ref(site.clone(), function, *else_bb);
            verify_operand(cx, function_id, block_id, None, cond);
            if let Some(cond_ty) = operand_ty(cx, cond) {
                let is_bool = matches!(cx.program.type_data(cond_ty), TypeData::Bool);
                if !is_bool {
                    cx.push(
                        site,
                        VerifyErrorKind::BadFunction(BadFunction::IfCondMustBeBool(cond_ty)),
                    );
                }
            }
        }
        Terminator::SwitchEnum {
            discr,
            arms,
            else_bb,
        } => {
            verify_place(cx, function_id, block_id, None, discr);
            let discr_ty = place_ty(discr);
            let expected_enum = match discr_ty {
                Some(ty) => match cx.program.type_data(ty) {
                    TypeData::Enum(id) => Some(id),
                    TypeData::Optional(_) => None,
                    _ => {
                        cx.push(
                            site.clone(),
                            VerifyErrorKind::BadFunction(
                                BadFunction::SwitchDiscriminantMustBeEnum(ty),
                            ),
                        );
                        None
                    }
                },
                None => None,
            };

            let mut seen = std::collections::HashSet::new();
            for (variant, target) in arms {
                if !seen.insert(*variant) {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadFunction(BadFunction::DuplicateSwitchArm(*variant)),
                    );
                }
                cx.verify_block_ref(site.clone(), function, *target);
                if let Some(enum_id) = expected_enum {
                    if !cx.variant_belongs_to_enum(*enum_id, *variant) {
                        cx.push(
                            site.clone(),
                            VerifyErrorKind::BadFunction(BadFunction::SwitchArmVariantMismatch {
                                expected_enum: *enum_id,
                                variant: *variant,
                            }),
                        );
                    }
                }
            }
            if let Some(target) = else_bb {
                cx.verify_block_ref(site, function, *target);
            }
        }
        Terminator::Return(value) => {
            verify_return(
                cx,
                function_id,
                block_id,
                site,
                function.signature.return_type,
                value,
            );
        }
        Terminator::Unreachable => {}
    }
}

fn verify_return(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    site: VerifySite,
    ret_ty: TypeId,
    value: &Option<Operand>,
) {
    let ret_is_void = cx.has_type(ret_ty) && matches!(cx.program.type_data(ret_ty), TypeData::Void);
    match value {
        None => {
            if !ret_is_void {
                cx.push(
                    site,
                    VerifyErrorKind::BadFunction(BadFunction::NonVoidFunctionMustReturnValue(
                        ret_ty,
                    )),
                );
            }
        }
        Some(op) => {
            if ret_is_void {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadFunction(BadFunction::VoidFunctionMustReturnNone),
                );
            }
            verify_operand(cx, function_id, block_id, None, op);
            if let Some(op_ty) = operand_ty(cx, op) {
                if op_ty != ret_ty {
                    cx.push(
                        site,
                        VerifyErrorKind::BadFunction(BadFunction::ReturnedTypeMismatch {
                            expected: ret_ty,
                            found: op_ty,
                        }),
                    );
                }
            }
        }
    }
}

fn verify_rvalue(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    stmt_index: Option<usize>,
    value: &RValue,
) {
    let function = cx.program.function(function_id);
    let stmt_idx = stmt_index.unwrap_or(0);
    let site = cx.stmt_site(function_id, block_id, stmt_idx);

    match value {
        RValue::Use(op) | RValue::ToString { value: op } => {
            verify_operand(cx, function_id, block_id, stmt_index, op);
        }
        RValue::Unary { value: op, ty, .. } => {
            verify_operand(cx, function_id, block_id, stmt_index, op);
            cx.verify_type_ref(site, *ty);
        }
        RValue::Binary { lhs, rhs, ty, .. } => {
            verify_operand(cx, function_id, block_id, stmt_index, lhs);
            verify_operand(cx, function_id, block_id, stmt_index, rhs);
            cx.verify_type_ref(site, *ty);
        }
        RValue::SharedRefEq { lhs, rhs, .. } => {
            verify_operand(cx, function_id, block_id, stmt_index, lhs);
            verify_operand(cx, function_id, block_id, stmt_index, rhs);
        }
        RValue::Cast { value: op, target } => {
            verify_operand(cx, function_id, block_id, stmt_index, op);
            cx.verify_type_ref(site.clone(), *target);
            if let Some(value_ty) = operand_ty(cx, op) {
                let both_numeric =
                    cx.is_numeric_scalar_type(value_ty) && cx.is_numeric_scalar_type(*target);
                if !both_numeric {
                    cx.push(
                        site,
                        VerifyErrorKind::BadFunction(BadFunction::CastMustUseNumericScalars {
                            value: value_ty,
                            target: *target,
                        }),
                    );
                }
            }
        }
        RValue::Aggregate { kind, fields, ty } => {
            cx.verify_type_ref(site.clone(), *ty);
            match kind {
                AggregateCtor::Struct(id) | AggregateCtor::DataRef(id) => {
                    if !cx.has_aggregate(*id) {
                        cx.push(
                            site.clone(),
                            VerifyErrorKind::BadReference(BadReference::InvalidAggregate(*id)),
                        );
                    }
                }
                AggregateCtor::EnumVariant { enum_id, variant } => {
                    if !cx.has_enum(*enum_id) {
                        cx.push(
                            site.clone(),
                            VerifyErrorKind::BadReference(BadReference::InvalidEnum(*enum_id)),
                        );
                    } else if !cx.variant_belongs_to_enum(*enum_id, *variant) {
                        cx.push(
                            site.clone(),
                            VerifyErrorKind::BadReference(BadReference::InvalidVariant {
                                enum_id: *enum_id,
                                variant: *variant,
                            }),
                        );
                    }
                }
                AggregateCtor::Tuple
                | AggregateCtor::List
                | AggregateCtor::Array
                | AggregateCtor::Map => {}
            }
            for field in fields {
                verify_operand(cx, function_id, block_id, stmt_index, field);
            }
        }
        RValue::Call { callee, args } => {
            verify_call(cx, function_id, block_id, stmt_index, callee, args);
        }
        RValue::Format { value, .. } => {
            verify_operand(cx, function_id, block_id, stmt_index, value);
        }
        RValue::Len { source } => {
            verify_place(cx, function_id, block_id, stmt_index, source);
            if let Some(source_ty) = place_ty(source) {
                if !matches!(
                    cx.program.type_data(source_ty),
                    TypeData::String
                        | TypeData::List(_)
                        | TypeData::Array { .. }
                        | TypeData::Map { .. }
                ) {
                    cx.push(
                        site,
                        VerifyErrorKind::BadFunction(BadFunction::LenSourceMustBeCountable(
                            source_ty,
                        )),
                    );
                }
            }
        }
        RValue::ListPush { list, value } => {
            verify_place(cx, function_id, block_id, stmt_index, list);
            verify_operand(cx, function_id, block_id, stmt_index, value);
            if let Some(expected_elem) = list_elem_ty(cx, list) {
                if let Some(value_ty) = operand_ty(cx, value) {
                    if value_ty != expected_elem {
                        cx.push(
                            site,
                            VerifyErrorKind::BadFunction(BadFunction::ListElementTypeMismatch {
                                expected: expected_elem,
                                found: value_ty,
                            }),
                        );
                    }
                }
            }
        }
        RValue::ListPop { list, ty } => {
            verify_place(cx, function_id, block_id, stmt_index, list);
            cx.verify_type_ref(site.clone(), *ty);
            if let Some(expected_elem) = list_elem_ty(cx, list) {
                let is_optional_of_elem = matches!(cx.program.type_data(*ty), TypeData::Optional(inner) if *inner == expected_elem);
                if !is_optional_of_elem {
                    cx.push(
                        site,
                        VerifyErrorKind::BadFunction(
                            BadFunction::ListPopResultMustBeOptionalElement {
                                expected_elem,
                                found: *ty,
                            },
                        ),
                    );
                }
            }
        }
        RValue::ListSlice {
            source,
            start,
            end,
            ty,
            ..
        } => {
            verify_place(cx, function_id, block_id, stmt_index, source);
            cx.verify_type_ref(site.clone(), *ty);
            if let Some(expected_elem) = list_elem_ty(cx, source) {
                let is_list_of_elem = matches!(cx.program.type_data(*ty), TypeData::List(inner) if *inner == expected_elem);
                if !is_list_of_elem {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadFunction(BadFunction::ListSliceResultMustBeList {
                            found: *ty,
                        }),
                    );
                }
            }
            verify_slice_index(
                cx,
                function_id,
                block_id,
                stmt_idx,
                function,
                "start",
                *start,
            );
            verify_slice_index(cx, function_id, block_id, stmt_idx, function, "end", *end);
        }
        RValue::MapGet { map, key, ty } => {
            verify_place(cx, function_id, block_id, stmt_index, map);
            verify_operand(cx, function_id, block_id, stmt_index, key);
            cx.verify_type_ref(site.clone(), *ty);
            if let Some((expected_key, expected_value)) = map_kv(cx, map) {
                if let Some(key_ty) = operand_ty(cx, key) {
                    if key_ty != expected_key {
                        cx.push(
                            site.clone(),
                            VerifyErrorKind::BadFunction(BadFunction::MapKeyTypeMismatch {
                                expected: expected_key,
                                found: key_ty,
                            }),
                        );
                    }
                }
                let is_optional_of_value = matches!(cx.program.type_data(*ty), TypeData::Optional(inner) if *inner == expected_value);
                if !is_optional_of_value {
                    cx.push(
                        site,
                        VerifyErrorKind::BadFunction(
                            BadFunction::MapGetResultMustBeOptionalValue {
                                expected_value,
                                found: *ty,
                            },
                        ),
                    );
                }
            }
        }
        RValue::MapInsert { map, key, value } => {
            verify_place(cx, function_id, block_id, stmt_index, map);
            verify_operand(cx, function_id, block_id, stmt_index, key);
            verify_operand(cx, function_id, block_id, stmt_index, value);
            if let Some((expected_key, expected_value)) = map_kv(cx, map) {
                if let Some(key_ty) = operand_ty(cx, key) {
                    if key_ty != expected_key {
                        cx.push(
                            site.clone(),
                            VerifyErrorKind::BadFunction(BadFunction::MapKeyTypeMismatch {
                                expected: expected_key,
                                found: key_ty,
                            }),
                        );
                    }
                }
                if let Some(value_ty) = operand_ty(cx, value) {
                    if value_ty != expected_value {
                        cx.push(
                            site,
                            VerifyErrorKind::BadFunction(BadFunction::MapValueTypeMismatch {
                                expected: expected_value,
                                found: value_ty,
                            }),
                        );
                    }
                }
            }
        }
        RValue::MapRemove { map, key, ty } => {
            verify_place(cx, function_id, block_id, stmt_index, map);
            verify_operand(cx, function_id, block_id, stmt_index, key);
            cx.verify_type_ref(site.clone(), *ty);
            if let Some((expected_key, expected_value)) = map_kv(cx, map) {
                if let Some(key_ty) = operand_ty(cx, key) {
                    if key_ty != expected_key {
                        cx.push(
                            site.clone(),
                            VerifyErrorKind::BadFunction(BadFunction::MapKeyTypeMismatch {
                                expected: expected_key,
                                found: key_ty,
                            }),
                        );
                    }
                }
                let is_optional_of_value = matches!(cx.program.type_data(*ty), TypeData::Optional(inner) if *inner == expected_value);
                if !is_optional_of_value {
                    cx.push(
                        site,
                        VerifyErrorKind::BadFunction(
                            BadFunction::MapGetResultMustBeOptionalValue {
                                expected_value,
                                found: *ty,
                            },
                        ),
                    );
                }
            }
        }
        RValue::MapEntryAt { map, index: _, ty } => {
            verify_place(cx, function_id, block_id, stmt_index, map);
            cx.verify_type_ref(site, *ty);
        }
        RValue::SliceView {
            source,
            start: _,
            end: _,
            ty,
            inclusive: _,
        } => {
            verify_place(cx, function_id, block_id, stmt_index, source);
            cx.verify_type_ref(site, *ty);
        }
        RValue::MakeClosure { func, captures, ty } => {
            if !cx.has_function(*func) {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadReference(BadReference::InvalidFunction(*func)),
                );
            }
            for cap in captures {
                verify_operand(cx, function_id, block_id, stmt_index, cap);
            }
            cx.verify_type_ref(site, *ty);
        }
    }
}

fn verify_operand(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    stmt_index: Option<usize>,
    op: &Operand,
) {
    let site = cx.stmt_site(function_id, block_id, stmt_index.unwrap_or(0));
    match op {
        Operand::Place(place) => verify_place(cx, function_id, block_id, stmt_index, place),
        Operand::Const(id) => {
            if !cx.has_const(*id) {
                cx.push(
                    site,
                    VerifyErrorKind::BadReference(BadReference::InvalidConst(*id)),
                );
            }
        }
    }
}

fn verify_place(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    stmt_index: Option<usize>,
    place: &Place,
) {
    let function = cx.program.function(function_id);
    let stmt_idx = stmt_index.unwrap_or(0);
    let site = cx.stmt_site(function_id, block_id, stmt_idx);

    if place.root.index() >= function.locals.len() {
        cx.push(
            site,
            VerifyErrorKind::BadReference(BadReference::InvalidLocal(place.root)),
        );
        return;
    }

    let mut current_ty = function.locals[place.root.index()].ty;

    for proj in &place.projection {
        match proj {
            Projection::Field(field_id) => match cx.program.type_data(current_ty) {
                TypeData::Aggregate(agg_id) => {
                    let agg = cx.program.aggregate(*agg_id);
                    if field_id.index() >= agg.fields.len() {
                        cx.push(
                            site.clone(),
                            VerifyErrorKind::BadReference(BadReference::InvalidField {
                                aggregate: *agg_id,
                                field: *field_id,
                            }),
                        );
                        break;
                    }
                    current_ty = agg.fields[field_id.index()].ty;
                }
                _ => {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadPlace(BadPlace::FieldProjectionOnNonAggregate(
                            current_ty,
                        )),
                    );
                    break;
                }
            },
            Projection::TupleField(index) => match cx.program.type_data(current_ty) {
                TypeData::Tuple(elems) => {
                    if *index as usize >= elems.len() {
                        cx.push(
                            site.clone(),
                            VerifyErrorKind::BadPlace(BadPlace::TupleFieldOutOfRange {
                                ty: current_ty,
                                index: *index,
                                len: elems.len(),
                            }),
                        );
                        break;
                    }
                    current_ty = elems[*index as usize];
                }
                _ => {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadPlace(BadPlace::TupleProjectionOnNonTuple(current_ty)),
                    );
                    break;
                }
            },
            Projection::VariantField {
                enum_id,
                variant,
                field,
            } => match cx.program.type_data(current_ty) {
                TypeData::Enum(eid) if *eid == *enum_id => {
                    let enm = cx.program.enum_decl(*enum_id);
                    if variant.index() >= enm.variants.len() {
                        cx.push(
                            site.clone(),
                            VerifyErrorKind::BadReference(BadReference::InvalidVariant {
                                enum_id: *enum_id,
                                variant: *variant,
                            }),
                        );
                        break;
                    }
                    let variant_shape = &enm.variants[variant.index()].shape;
                    let field_count = match variant_shape {
                        VariantShape::Unit => 0,
                        VariantShape::Tuple(ts) => ts.len(),
                        VariantShape::Struct(fs) => fs.len(),
                    };
                    if *field as usize >= field_count {
                        cx.push(
                            site.clone(),
                            VerifyErrorKind::BadPlace(BadPlace::VariantFieldOutOfRange {
                                ty: current_ty,
                                index: *field,
                                len: field_count,
                            }),
                        );
                        break;
                    }
                    current_ty = match variant_shape {
                        VariantShape::Unit => current_ty,
                        VariantShape::Tuple(ts) => ts[*field as usize],
                        VariantShape::Struct(fs) => fs[*field as usize].ty,
                    };
                }
                _ => {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadPlace(BadPlace::VariantProjectionOnNonEnum(current_ty)),
                    );
                    break;
                }
            },
            Projection::Index(local) => {
                if local.index() >= function.locals.len() {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadReference(BadReference::InvalidLocal(*local)),
                    );
                    break;
                }
                let is_indexable = matches!(
                    cx.program.type_data(current_ty),
                    TypeData::List(_) | TypeData::Array { .. } | TypeData::Map { .. }
                );
                if !is_indexable {
                    cx.push(
                        site.clone(),
                        VerifyErrorKind::BadPlace(BadPlace::IndexProjectionOnNonIndexable(
                            current_ty,
                        )),
                    );
                    break;
                }
            }
        }
    }
}

fn verify_call(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    stmt_index: Option<usize>,
    callee: &Callee,
    args: &[Operand],
) {
    let site = cx.stmt_site(function_id, block_id, stmt_index.unwrap_or(0));

    // always verify operand structural validity upfront
    if let Callee::Closure(op) = callee {
        verify_operand(cx, function_id, block_id, stmt_index, op);
    }
    for arg in args {
        verify_operand(cx, function_id, block_id, stmt_index, arg);
    }

    // callee-specific type checks
    match callee {
        Callee::Function(id) => {
            if !cx.has_function(*id) {
                cx.push(
                    site,
                    VerifyErrorKind::BadReference(BadReference::InvalidFunction(*id)),
                );
                return;
            }
            let param_tys: Vec<TypeId> = cx
                .program
                .function(*id)
                .signature
                .params
                .iter()
                .map(|p| p.ty)
                .collect();
            verify_call_args(cx, site, args, &param_tys);
        }
        Callee::Extern(id) => {
            if !cx.has_extern(*id) {
                cx.push(
                    site,
                    VerifyErrorKind::BadReference(BadReference::InvalidExtern(*id)),
                );
                return;
            }
            let param_tys = cx.program.extern_decl(*id).params.clone();
            verify_call_args(cx, site, args, &param_tys);
        }
        Callee::Builtin(builtin) => {
            let arity = match builtin {
                Builtin::Println => 1,
                Builtin::Assert => 1,
                Builtin::AssertMsg => 2,
            };
            if args.len() != arity {
                cx.push(
                    site,
                    VerifyErrorKind::BadCall(BadCall::ArityMismatch {
                        expected: arity,
                        found: args.len(),
                    }),
                );
            }
        }
        Callee::Closure(op) => {
            if let Some(ty) = operand_ty(cx, op) {
                if !matches!(cx.program.type_data(ty), TypeData::Function(_)) {
                    cx.push(
                        site,
                        VerifyErrorKind::BadCall(BadCall::ClosureCalleeMustBeFunction),
                    );
                } else if let TypeData::Function(sig) = cx.program.type_data(ty) {
                    verify_call_args(cx, site, args, &sig.params);
                }
            }
        }
    }
}

fn verify_call_args(
    cx: &mut VerifyCx<'_>,
    site: VerifySite,
    args: &[Operand],
    expected: &[TypeId],
) {
    if args.len() != expected.len() {
        cx.push(
            site.clone(),
            VerifyErrorKind::BadCall(BadCall::ArityMismatch {
                expected: expected.len(),
                found: args.len(),
            }),
        );
    }
    for (i, (arg, &expected_ty)) in args.iter().zip(expected.iter()).enumerate() {
        if let Some(found_ty) = operand_ty(cx, arg) {
            if found_ty != expected_ty {
                cx.push(
                    site.clone(),
                    VerifyErrorKind::BadCall(BadCall::ArgTypeMismatch {
                        index: i,
                        expected: expected_ty,
                        found: found_ty,
                    }),
                );
            }
        }
    }
}

fn verify_slice_index(
    cx: &mut VerifyCx<'_>,
    function_id: FunctionId,
    block_id: BlockId,
    stmt_idx: usize,
    function: &Function,
    which: &'static str,
    local: LocalId,
) {
    let site = cx.stmt_site(function_id, block_id, stmt_idx);
    if local.index() >= function.locals.len() {
        cx.push(
            site,
            VerifyErrorKind::BadReference(BadReference::InvalidLocal(local)),
        );
        return;
    }
    let local_ty = function.locals[local.index()].ty;
    let is_int = cx.has_type(local_ty) && matches!(cx.program.type_data(local_ty), TypeData::Int);
    if !is_int {
        cx.push(
            cx.stmt_site(function_id, block_id, stmt_idx),
            VerifyErrorKind::BadFunction(BadFunction::SliceIndexMustBeInt {
                which,
                found: local_ty,
            }),
        );
    }
}

fn verify_type(cx: &mut VerifyCx<'_>, id: TypeId) {
    let data = cx.program.type_data(id);
    let site = VerifySite::Type(id);
    match data {
        TypeData::Int
        | TypeData::Float
        | TypeData::Double
        | TypeData::Bool
        | TypeData::String
        | TypeData::Void
        | TypeData::Any => {}
        TypeData::Optional(inner) | TypeData::List(inner) => {
            cx.verify_type_ref(site, *inner);
        }
        TypeData::Tuple(items) => {
            for item in items {
                cx.verify_type_ref(site.clone(), *item);
            }
        }
        TypeData::Array { elem, .. } => {
            cx.verify_type_ref(site, *elem);
        }
        TypeData::Map { key, value } => {
            cx.verify_type_ref(site.clone(), *key);
            cx.verify_type_ref(site, *value);
        }
        TypeData::Function(sig) => {
            for param in &sig.params {
                cx.verify_type_ref(site.clone(), *param);
            }
            cx.verify_type_ref(site, sig.ret);
        }
        TypeData::Aggregate(agg_id) | TypeData::DataRef(agg_id) => {
            if !cx.has_aggregate(*agg_id) {
                cx.push(
                    VerifySite::Type(id),
                    VerifyErrorKind::BadReference(BadReference::InvalidAggregate(*agg_id)),
                );
            }
        }
        TypeData::Enum(enum_id) => {
            if !cx.has_enum(*enum_id) {
                cx.push(
                    VerifySite::Type(id),
                    VerifyErrorKind::BadReference(BadReference::InvalidEnum(*enum_id)),
                );
            }
        }
        TypeData::Extern(ext_id) => {
            if !cx.has_extern_type(*ext_id) {
                cx.push(
                    VerifySite::Type(id),
                    VerifyErrorKind::BadReference(BadReference::InvalidExternType(*ext_id)),
                );
            }
        }
    }
}

fn place_ty(place: &Place) -> Option<TypeId> {
    Some(place.ty)
}

fn operand_ty(cx: &VerifyCx<'_>, op: &Operand) -> Option<TypeId> {
    match op {
        Operand::Place(place) => Some(place.ty),
        Operand::Const(id) => {
            if cx.has_const(*id) {
                Some(cx.program.const_data(*id).ty)
            } else {
                None
            }
        }
    }
}

fn list_elem_ty(cx: &VerifyCx<'_>, place: &Place) -> Option<TypeId> {
    match cx.program.type_data(place.ty) {
        TypeData::List(elem) => Some(*elem),
        _ => None,
    }
}

fn map_kv(cx: &VerifyCx<'_>, place: &Place) -> Option<(TypeId, TypeId)> {
    match cx.program.type_data(place.ty) {
        TypeData::Map { key, value } => Some((*key, *value)),
        _ => None,
    }
}
