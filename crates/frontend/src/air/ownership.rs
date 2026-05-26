use std::collections::{HashMap, HashSet};

use super::{
    AggregateKind, AirBlock, AirStmt, AirTail, CallArg, Callee, Function, FunctionId, LocalId,
    Operand, ParamMode, Place, Program, Projection, RValue, TypeData, TypeId, VariantShape,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypePassClass {
    Immediate,
    CheapHandle,
    SmallCopyInline(AirCopyLayout),
    LargeInline(AirCopyLayout),
    ManagedBuffer,
    Opaque,
}

impl TypePassClass {
    pub fn is_cheap_value(self) -> bool {
        matches!(
            self,
            Self::Immediate | Self::CheapHandle | Self::SmallCopyInline(_)
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AirCopyLayout {
    pub size: u32,
    pub align: u32,
}

const AIR_SMALL_COPY_INLINE_BYTES: u32 = 32;
const HANDLE_LAYOUT: AirCopyLayout = AirCopyLayout { size: 8, align: 8 };

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LayoutResult {
    Copy(AirCopyLayout),
    NonCopy(AirCopyLayout),
    Opaque,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OwnershipError {
    ReborrowMutImmutableParam { function: FunctionId, param: usize },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypePassClasses {
    classes: Vec<TypePassClass>,
}

impl TypePassClasses {
    pub fn analyze(program: &Program) -> Self {
        let mut analyzer = TypePassClassAnalyzer {
            program,
            classes: HashMap::new(),
            visiting: HashSet::new(),
        };
        let classes = (0..program.type_arena.len())
            .map(|index| analyzer.classify(TypeId::from_index(index)))
            .collect();
        Self { classes }
    }

    pub fn get(&self, ty: TypeId) -> Option<TypePassClass> {
        self.classes.get(ty.index()).copied()
    }

    #[cfg(test)]
    fn class(&self, ty: TypeId) -> TypePassClass {
        self.get(ty).expect("missing AIR type pass class")
    }
}

struct TypePassClassAnalyzer<'a> {
    program: &'a Program,
    classes: HashMap<TypeId, TypePassClass>,
    visiting: HashSet<TypeId>,
}

impl TypePassClassAnalyzer<'_> {
    fn classify(&mut self, ty: TypeId) -> TypePassClass {
        if let Some(class) = self.classes.get(&ty) {
            return *class;
        }
        if !self.visiting.insert(ty) {
            return TypePassClass::Opaque;
        }
        let class = self.compute(ty);
        self.visiting.remove(&ty);
        self.classes.insert(ty, class);
        class
    }

    fn compute(&mut self, ty: TypeId) -> TypePassClass {
        let Some(data) = self.program.type_arena.get(ty) else {
            return TypePassClass::Opaque;
        };
        match data {
            TypeData::Int | TypeData::Float | TypeData::Bool | TypeData::Void => {
                TypePassClass::Immediate
            }
            TypeData::String | TypeData::List(_) | TypeData::Map { .. } => {
                TypePassClass::ManagedBuffer
            }
            TypeData::DataRef(_) | TypeData::Function(_) | TypeData::Dyn(_) => {
                TypePassClass::CheapHandle
            }
            TypeData::Slice(_) | TypeData::Any => TypePassClass::Opaque,
            TypeData::Extern(id) => {
                match self.program.extern_types.get(id.index()).map(|ty| ty.rep) {
                    Some(super::ExternRep::Shared) => TypePassClass::CheapHandle,
                    Some(super::ExternRep::Inline) => inline_class(self.extern_layout(*id)),
                    None => TypePassClass::Opaque,
                }
            }
            TypeData::Optional(inner) => inline_class(self.optional_layout(*inner)),
            TypeData::Tuple(elems) => inline_class(self.sequence_layout(elems.iter().copied())),
            TypeData::Array { elem, len } => inline_class(self.array_layout(*elem, *len)),
            TypeData::Aggregate(id) => inline_class(self.aggregate_layout(*id)),
            TypeData::Enum(id) => inline_class(self.enum_layout(*id)),
        }
    }

    fn optional_layout(&mut self, inner: TypeId) -> LayoutResult {
        let inner = match self.copy_layout(inner) {
            LayoutResult::Opaque => return LayoutResult::Opaque,
            layout => layout,
        };
        let layout = finish_layout(inner.layout().size.saturating_add(1), inner.layout().align);
        inner.with_layout(layout)
    }

    fn sequence_layout(&mut self, elems: impl Iterator<Item = TypeId>) -> LayoutResult {
        let mut size = 0;
        let mut align = 1;
        let mut copy = true;
        for elem in elems {
            let elem = match self.copy_layout(elem) {
                LayoutResult::Opaque => return LayoutResult::Opaque,
                layout => layout,
            };
            let layout = elem.layout();
            size = align_to(size, layout.align).saturating_add(layout.size);
            align = align.max(layout.align).min(8);
            copy &= matches!(elem, LayoutResult::Copy(_));
        }
        let layout = finish_layout(size, align);
        if copy {
            LayoutResult::Copy(layout)
        } else {
            LayoutResult::NonCopy(layout)
        }
    }

    fn array_layout(&mut self, elem: TypeId, len: usize) -> LayoutResult {
        let elem = match self.copy_layout(elem) {
            LayoutResult::Opaque => return LayoutResult::Opaque,
            layout => layout,
        };
        let layout = elem.layout();
        let stride = align_to(layout.size, layout.align);
        let len = u32::try_from(len).unwrap_or(u32::MAX);
        let layout = finish_layout(stride.saturating_mul(len), layout.align);
        elem.with_layout(layout)
    }

    fn aggregate_layout(&mut self, id: super::AggregateId) -> LayoutResult {
        let Some(aggregate) = self.program.aggregates.get(id.index()) else {
            return LayoutResult::Opaque;
        };
        match aggregate.kind {
            AggregateKind::Struct => self.sequence_layout(aggregate.fields.iter().map(|f| f.ty)),
            AggregateKind::DataRef => LayoutResult::Copy(HANDLE_LAYOUT),
        }
    }

    fn extern_layout(&mut self, id: super::ExternTypeId) -> LayoutResult {
        let Some(decl) = self.program.extern_types.get(id.index()) else {
            return LayoutResult::Opaque;
        };
        if decl.rep != super::ExternRep::Inline {
            return LayoutResult::Opaque;
        }
        self.sequence_layout(decl.fields.iter().map(|f| f.ty))
    }

    fn enum_layout(&mut self, id: super::EnumId) -> LayoutResult {
        let Some(enm) = self.program.enums.get(id.index()) else {
            return LayoutResult::Opaque;
        };
        let discr = AirCopyLayout { size: 1, align: 1 };
        let mut payload_size = 0;
        let mut align = discr.align;
        let mut copy = true;
        for variant in &enm.variants {
            let payload = match &variant.shape {
                VariantShape::Unit => LayoutResult::Copy(AirCopyLayout { size: 0, align: 1 }),
                VariantShape::Tuple(elems) => self.sequence_layout(elems.iter().copied()),
                VariantShape::Struct(fields) => self.sequence_layout(fields.iter().map(|f| f.ty)),
            };
            let payload = match payload {
                LayoutResult::Opaque => return LayoutResult::Opaque,
                layout => layout,
            };
            let layout = payload.layout();
            payload_size = payload_size.max(layout.size);
            align = align.max(layout.align).min(8);
            copy &= matches!(payload, LayoutResult::Copy(_));
        }
        let size = align_to(discr.size, align).saturating_add(payload_size);
        let layout = finish_layout(size, align);
        if copy {
            LayoutResult::Copy(layout)
        } else {
            LayoutResult::NonCopy(layout)
        }
    }

    fn copy_layout(&mut self, ty: TypeId) -> LayoutResult {
        match self.classify(ty) {
            TypePassClass::Immediate => LayoutResult::Copy(match self.program.type_arena.get(ty) {
                Some(TypeData::Int | TypeData::Float) => AirCopyLayout { size: 8, align: 8 },
                Some(TypeData::Bool) => AirCopyLayout { size: 1, align: 1 },
                Some(TypeData::Void) => AirCopyLayout { size: 0, align: 1 },
                _ => unreachable!("non-primitive immediate"),
            }),
            TypePassClass::CheapHandle => LayoutResult::Copy(HANDLE_LAYOUT),
            TypePassClass::SmallCopyInline(layout) => LayoutResult::Copy(layout),
            TypePassClass::LargeInline(layout) => LayoutResult::NonCopy(layout),
            TypePassClass::ManagedBuffer => LayoutResult::NonCopy(HANDLE_LAYOUT),
            TypePassClass::Opaque => LayoutResult::Opaque,
        }
    }
}

impl LayoutResult {
    fn layout(self) -> AirCopyLayout {
        match self {
            Self::Copy(layout) | Self::NonCopy(layout) => layout,
            Self::Opaque => unreachable!("opaque layout has no size"),
        }
    }

    fn with_layout(self, layout: AirCopyLayout) -> Self {
        match self {
            Self::Copy(_) => Self::Copy(layout),
            Self::NonCopy(_) => Self::NonCopy(layout),
            Self::Opaque => Self::Opaque,
        }
    }
}

fn finish_layout(size: u32, align: u32) -> AirCopyLayout {
    let align = align.clamp(1, 8);
    AirCopyLayout {
        size: align_to(size, align),
        align,
    }
}

fn inline_class(layout: LayoutResult) -> TypePassClass {
    match layout {
        LayoutResult::Copy(layout) if layout.size <= AIR_SMALL_COPY_INLINE_BYTES => {
            TypePassClass::SmallCopyInline(layout)
        }
        LayoutResult::Copy(layout) | LayoutResult::NonCopy(layout) => {
            TypePassClass::LargeInline(layout)
        }
        LayoutResult::Opaque => TypePassClass::Opaque,
    }
}

fn align_to(size: u32, align: u32) -> u32 {
    let rem = size % align;
    if rem == 0 {
        size
    } else {
        size.saturating_add(align - rem)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ParamUse {
    ReadOnly,
    ReborrowMut,
    ValueRequired,
    Mixed,
}

#[cfg(test)]
fn analyze_param_uses(program: &Program, function: FunctionId) -> Vec<ParamUse> {
    let classes = TypePassClasses::analyze(program);
    analyze_param_uses_with_modes(program, function, None, &classes)
}

fn analyze_param_uses_with_modes(
    program: &Program,
    function: FunctionId,
    param_modes: Option<&[Vec<ParamMode>]>,
    classes: &TypePassClasses,
) -> Vec<ParamUse> {
    let function = program.function(function);
    let params_by_local = function
        .signature
        .params
        .iter()
        .enumerate()
        .map(|(index, param)| (param.local_id, index))
        .collect();
    ParamUseAnalyzer {
        classes,
        function,
        param_modes,
        params_by_local,
        uses: vec![None; function.signature.params.len()],
    }
    .analyze()
}

struct ParamUseAnalyzer<'a> {
    classes: &'a TypePassClasses,
    function: &'a Function,
    param_modes: Option<&'a [Vec<ParamMode>]>,
    params_by_local: HashMap<LocalId, usize>,
    uses: Vec<Option<ParamUse>>,
}

impl ParamUseAnalyzer<'_> {
    fn analyze(mut self) -> Vec<ParamUse> {
        self.observe_air_block(&self.function.body.block);
        self.uses
            .into_iter()
            .map(|use_| use_.unwrap_or(ParamUse::ReadOnly))
            .collect()
    }

    fn observe_air_block(&mut self, block: &AirBlock) {
        for stmt in &block.stmts {
            self.observe_air_stmt(stmt);
        }
        self.observe_air_tail(&block.tail);
    }

    fn observe_air_stmt(&mut self, stmt: &AirStmt) {
        match stmt {
            AirStmt::Init { value, .. } => self.observe_rvalue(value, ValueContext::Store),
            AirStmt::Assign { dst, value } => {
                self.observe_place(dst, ParamUse::ReborrowMut);
                self.observe_rvalue(value, ValueContext::Store);
            }
            AirStmt::Eval(value) => self.observe_rvalue(value, ValueContext::Read),
            AirStmt::If(branch) => {
                self.observe_operand(&branch.cond, ValueContext::Read);
                self.observe_air_block(&branch.then_block);
                if let Some(block) = &branch.else_block {
                    self.observe_air_block(block);
                }
            }
            AirStmt::Loop(loop_) => self.observe_air_block(&loop_.body),
            AirStmt::EnumMatch(match_) => {
                self.observe_place(&match_.discr, ParamUse::ReadOnly);
                for arm in &match_.arms {
                    self.observe_air_block(&arm.block);
                }
                if let Some(block) = &match_.else_block {
                    self.observe_air_block(block);
                }
            }
        }
    }

    fn observe_air_tail(&mut self, tail: &AirTail) {
        if let AirTail::Return(Some(value)) = tail {
            self.observe_operand(value, ValueContext::Store);
        }
    }

    fn observe_rvalue(&mut self, value: &RValue, context: ValueContext) {
        match value {
            RValue::Use(operand) => self.observe_operand(operand, context),
            RValue::Unary { value, .. }
            | RValue::Cast { value, .. }
            | RValue::Stringify { value, .. }
            | RValue::Format { value, .. } => self.observe_operand(value, ValueContext::Read),
            RValue::Binary { lhs, rhs, .. } | RValue::SharedRefEq { lhs, rhs, .. } => {
                self.observe_operand(lhs, ValueContext::Read);
                self.observe_operand(rhs, ValueContext::Read);
            }
            RValue::Aggregate { fields, .. }
            | RValue::MakeClosure {
                captures: fields, ..
            } => {
                for field in fields {
                    self.observe_operand(field, ValueContext::Store);
                }
            }
            RValue::StringConcat { parts } => {
                for part in parts {
                    self.observe_operand(part, ValueContext::Read);
                }
            }
            RValue::Call { callee, args } => {
                if let Callee::Closure(operand) = callee {
                    self.observe_operand(operand, ValueContext::Read);
                }
                for (index, arg) in args.iter().enumerate() {
                    self.observe_call_arg(callee, index, arg);
                }
            }
            RValue::Len { source } => self.observe_place(source, ParamUse::ReadOnly),
            RValue::ListPush { list, value } => {
                self.observe_place(list, ParamUse::ReborrowMut);
                self.observe_operand(value, ValueContext::Store);
            }
            RValue::ListPop { list, .. } => self.observe_place(list, ParamUse::ReborrowMut),
            RValue::ListSlice {
                source, start, end, ..
            }
            | RValue::SliceView {
                source, start, end, ..
            } => {
                self.observe_place(source, ParamUse::ReadOnly);
                self.observe_local(*start, ParamUse::ReadOnly);
                self.observe_local(*end, ParamUse::ReadOnly);
            }
            RValue::MapGet { map, key, .. } => {
                self.observe_place(map, ParamUse::ReadOnly);
                self.observe_operand(key, ValueContext::Read);
            }
            RValue::MapEntryAt { map, index, .. } => {
                self.observe_place(map, ParamUse::ReadOnly);
                self.observe_local(*index, ParamUse::ReadOnly);
            }
            RValue::MapInsert { map, key, value } => {
                self.observe_place(map, ParamUse::ReborrowMut);
                self.observe_operand(key, ValueContext::Store);
                self.observe_operand(value, ValueContext::Store);
            }
            RValue::MapRemove { map, key, .. } => {
                self.observe_place(map, ParamUse::ReborrowMut);
                self.observe_operand(key, ValueContext::Read);
            }
        }
    }

    fn observe_call_arg(&mut self, callee: &Callee, index: usize, arg: &CallArg) {
        match self
            .callee_param_mode(callee, index)
            .unwrap_or_else(|| arg.mode())
        {
            ParamMode::Value => match arg {
                CallArg::Value(operand) => self.observe_operand(operand, ValueContext::CallValue),
                CallArg::SharedBorrow(place) | CallArg::MutBorrow(place) => {
                    self.observe_place(place, ParamUse::ValueRequired);
                }
                CallArg::SharedStringConst(_) => {}
            },
            ParamMode::SharedBorrow => match arg {
                CallArg::Value(operand) => self.observe_operand(operand, ValueContext::Read),
                CallArg::SharedBorrow(place) | CallArg::MutBorrow(place) => {
                    self.observe_place(place, ParamUse::ReadOnly);
                }
                CallArg::SharedStringConst(_) => {}
            },
            ParamMode::MutBorrow => match arg {
                CallArg::Value(Operand::Place(place))
                | CallArg::SharedBorrow(place)
                | CallArg::MutBorrow(place) => self.observe_place(place, ParamUse::ReborrowMut),
                CallArg::Value(Operand::Const(_)) | CallArg::SharedStringConst(_) => {}
            },
        }
    }

    fn callee_param_mode(&self, callee: &Callee, index: usize) -> Option<ParamMode> {
        let Callee::Function(function) = callee else {
            return None;
        };
        self.param_modes
            .and_then(|modes| modes.get(function.index()))
            .and_then(|modes| modes.get(index))
            .copied()
    }

    fn observe_operand(&mut self, operand: &Operand, context: ValueContext) {
        let Operand::Place(place) = operand else {
            return;
        };
        let use_ = match context {
            ValueContext::Read => ParamUse::ReadOnly,
            ValueContext::CallValue if self.is_cheap(place.ty) => ParamUse::ReadOnly,
            ValueContext::Store | ValueContext::CallValue => ParamUse::ValueRequired,
        };
        self.observe_place(place, use_);
    }

    fn observe_place(&mut self, place: &Place, use_: ParamUse) {
        for projection in &place.projection {
            if let Projection::Index(local) = projection {
                self.observe_local(*local, ParamUse::ReadOnly);
            }
        }
        self.observe_local(place.root, use_);
    }

    fn observe_local(&mut self, local: LocalId, use_: ParamUse) {
        let Some(index) = self.param_index(local) else {
            return;
        };
        self.uses[index] = Some(merge_param_use(self.uses[index], use_));
    }

    fn param_index(&self, local: LocalId) -> Option<usize> {
        self.params_by_local.get(&local).copied()
    }

    fn is_cheap(&self, ty: TypeId) -> bool {
        self.classes
            .get(ty)
            .is_some_and(TypePassClass::is_cheap_value)
    }
}

#[derive(Debug, Clone, Copy)]
enum ValueContext {
    Read,
    Store,
    CallValue,
}

fn merge_param_use(current: Option<ParamUse>, next: ParamUse) -> ParamUse {
    let Some(current) = current else {
        return next;
    };
    if current == next {
        current
    } else if current == ParamUse::ReadOnly {
        next
    } else if next == ParamUse::ReadOnly {
        current
    } else {
        ParamUse::Mixed
    }
}

fn infer_param_modes(program: &Program) -> Result<Vec<Vec<ParamMode>>, Vec<OwnershipError>> {
    let classes = TypePassClasses::analyze(program);
    let mut modes = program
        .functions
        .iter()
        .map(|function| {
            function
                .signature
                .params
                .iter()
                .map(|param| initial_param_mode(param.mode, classes.get(param.ty)))
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let mut errors;

    loop {
        let mut changed = false;
        errors = vec![];
        for (index, function) in program.functions.iter().enumerate() {
            let id = FunctionId::from_index(index);
            let uses = analyze_param_uses_with_modes(program, id, Some(&modes), &classes);
            for (param, detail) in uses.into_iter().enumerate() {
                let current = modes[index][param];
                let param_ty = function.signature.params[param].ty;
                let class = classes.get(param_ty).unwrap_or(TypePassClass::Opaque);
                let next = match final_param_mode(id, param, current, class, detail) {
                    Ok(mode) => mode,
                    Err(error) => {
                        errors.push(error);
                        continue;
                    }
                };
                if modes[index][param] != next {
                    modes[index][param] = next;
                    changed = true;
                }
            }
        }
        changed |= force_alias_snapshots(program, &mut modes);
        if !changed {
            break;
        }
    }

    if errors.is_empty() {
        Ok(modes)
    } else {
        Err(errors)
    }
}

fn initial_param_mode(current: ParamMode, class: Option<TypePassClass>) -> ParamMode {
    if current == ParamMode::MutBorrow {
        ParamMode::MutBorrow
    } else if class.is_some_and(TypePassClass::is_cheap_value) {
        ParamMode::Value
    } else {
        ParamMode::SharedBorrow
    }
}

fn final_param_mode(
    function: FunctionId,
    param: usize,
    current: ParamMode,
    class: TypePassClass,
    use_: ParamUse,
) -> Result<ParamMode, OwnershipError> {
    if current == ParamMode::MutBorrow {
        return Ok(ParamMode::MutBorrow);
    }
    if matches!(use_, ParamUse::ReborrowMut | ParamUse::Mixed) {
        return Err(OwnershipError::ReborrowMutImmutableParam { function, param });
    }
    if current == ParamMode::Value {
        return Ok(ParamMode::Value);
    }
    if class.is_cheap_value() || use_ == ParamUse::ValueRequired {
        return Ok(ParamMode::Value);
    }
    Ok(ParamMode::SharedBorrow)
}

fn force_alias_snapshots(program: &Program, modes: &mut [Vec<ParamMode>]) -> bool {
    let mut changed = false;
    for function in &program.functions {
        function.body.for_each_rvalue(&mut |value| {
            let RValue::Call {
                callee: Callee::Function(callee),
                args,
            } = value
            else {
                return;
            };
            let Some(callee_modes) = modes.get_mut(callee.index()) else {
                return;
            };
            for first in 0..args.len() {
                for second in first + 1..args.len() {
                    changed |= force_alias_snapshot(callee_modes, args, first, second);
                }
            }
        });
    }
    changed
}

fn force_alias_snapshot(
    modes: &mut [ParamMode],
    args: &[CallArg],
    first: usize,
    second: usize,
) -> bool {
    let Some(first_mode) = modes.get(first).copied() else {
        return false;
    };
    let Some(second_mode) = modes.get(second).copied() else {
        return false;
    };
    let snapshot = match (first_mode, second_mode) {
        (ParamMode::SharedBorrow, ParamMode::MutBorrow) => first,
        (ParamMode::MutBorrow, ParamMode::SharedBorrow) => second,
        _ => return false,
    };
    if !call_args_overlap(&args[first], &args[second]) {
        return false;
    }
    modes[snapshot] = ParamMode::Value;
    true
}

fn call_args_overlap(left: &CallArg, right: &CallArg) -> bool {
    left.place()
        .zip(right.place())
        .is_some_and(|(left, right)| left.may_overlap(right))
}

fn apply_param_modes(program: &mut Program, modes: &[Vec<ParamMode>]) {
    for (function, modes) in program.functions.iter_mut().zip(modes) {
        for (param, mode) in function.signature.params.iter_mut().zip(modes) {
            param.mode = *mode;
        }
    }
    let function_type_modes = collect_function_type_modes(program, modes);
    for (ty, modes) in function_type_modes {
        let TypeData::Function(sig) = program.type_arena.data_mut(ty) else {
            continue;
        };
        for (param, mode) in sig.params.iter_mut().zip(modes) {
            param.mode = mode;
        }
    }
    rewrite_direct_call_args(program);
}

fn rewrite_direct_call_args(program: &mut Program) {
    let modes = program
        .functions
        .iter()
        .map(|function| {
            function
                .signature
                .params
                .iter()
                .map(|param| param.mode)
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let function_type_modes = program
        .type_arena
        .iter()
        .enumerate()
        .filter_map(|(index, ty)| {
            let TypeData::Function(sig) = ty else {
                return None;
            };
            Some((
                TypeId::from_index(index),
                sig.params
                    .iter()
                    .map(|param| param.mode)
                    .collect::<Vec<_>>(),
            ))
        })
        .collect::<HashMap<_, _>>();
    let const_types = program
        .const_arena
        .iter()
        .map(|data| data.ty)
        .collect::<Vec<_>>();

    for function in &mut program.functions {
        rewrite_air_block_call_args(
            &mut function.body.block,
            &modes,
            &function_type_modes,
            &const_types,
            &mut function.locals,
        );
    }
}

fn rewrite_air_block_call_args(
    block: &mut AirBlock,
    modes: &[Vec<ParamMode>],
    function_type_modes: &HashMap<TypeId, Vec<ParamMode>>,
    const_types: &[TypeId],
    locals: &mut Vec<super::Local>,
) {
    for stmt in std::mem::take(&mut block.stmts) {
        match stmt {
            AirStmt::Init { local, value } => {
                rewrite_air_value_stmt(
                    &mut block.stmts,
                    AirStmtValue::Init(local),
                    value,
                    modes,
                    function_type_modes,
                    const_types,
                    locals,
                );
            }
            AirStmt::Assign { dst, value } => {
                rewrite_air_value_stmt(
                    &mut block.stmts,
                    AirStmtValue::Assign(dst),
                    value,
                    modes,
                    function_type_modes,
                    const_types,
                    locals,
                );
            }
            AirStmt::Eval(value) => {
                rewrite_air_value_stmt(
                    &mut block.stmts,
                    AirStmtValue::Eval,
                    value,
                    modes,
                    function_type_modes,
                    const_types,
                    locals,
                );
            }
            AirStmt::If(mut branch) => {
                rewrite_air_block_call_args(
                    &mut branch.then_block,
                    modes,
                    function_type_modes,
                    const_types,
                    locals,
                );
                if let Some(else_block) = &mut branch.else_block {
                    rewrite_air_block_call_args(
                        else_block,
                        modes,
                        function_type_modes,
                        const_types,
                        locals,
                    );
                }
                block.stmts.push(AirStmt::If(branch));
            }
            AirStmt::Loop(mut loop_) => {
                rewrite_air_block_call_args(
                    &mut loop_.body,
                    modes,
                    function_type_modes,
                    const_types,
                    locals,
                );
                block.stmts.push(AirStmt::Loop(loop_));
            }
            AirStmt::EnumMatch(mut match_) => {
                for arm in &mut match_.arms {
                    rewrite_air_block_call_args(
                        &mut arm.block,
                        modes,
                        function_type_modes,
                        const_types,
                        locals,
                    );
                }
                if let Some(else_block) = &mut match_.else_block {
                    rewrite_air_block_call_args(
                        else_block,
                        modes,
                        function_type_modes,
                        const_types,
                        locals,
                    );
                }
                block.stmts.push(AirStmt::EnumMatch(match_));
            }
        }
    }
}

enum AirStmtValue {
    Init(LocalId),
    Assign(Place),
    Eval,
}

fn rewrite_air_value_stmt(
    out: &mut Vec<AirStmt>,
    kind: AirStmtValue,
    mut value: RValue,
    modes: &[Vec<ParamMode>],
    function_type_modes: &HashMap<TypeId, Vec<ParamMode>>,
    const_types: &[TypeId],
    locals: &mut Vec<super::Local>,
) {
    let prepended =
        rewrite_value_call_args(&mut value, modes, function_type_modes, const_types, locals);
    out.extend(prepended);
    out.push(match kind {
        AirStmtValue::Init(local) => AirStmt::Init { local, value },
        AirStmtValue::Assign(dst) => AirStmt::Assign { dst, value },
        AirStmtValue::Eval => AirStmt::Eval(value),
    });
}

fn rewrite_value_call_args(
    value: &mut RValue,
    modes: &[Vec<ParamMode>],
    function_type_modes: &HashMap<TypeId, Vec<ParamMode>>,
    const_types: &[TypeId],
    locals: &mut Vec<super::Local>,
) -> Vec<AirStmt> {
    let RValue::Call { callee, args } = value else {
        return vec![];
    };
    let expected = match callee {
        Callee::Function(callee) => modes.get(callee.index()),
        Callee::Closure(operand) => {
            operand_type(operand, const_types).and_then(|ty| function_type_modes.get(&ty))
        }
        Callee::Extern(_) => None,
    };
    let Some(expected) = expected else {
        return vec![];
    };
    rewrite_call_args(args, expected, const_types, locals)
}

fn rewrite_call_args(
    args: &mut [CallArg],
    expected: &[ParamMode],
    const_types: &[TypeId],
    locals: &mut Vec<super::Local>,
) -> Vec<AirStmt> {
    let mut prepended = vec![];
    for (arg, mode) in args.iter_mut().zip(expected) {
        let replacement = match (mode, &*arg) {
            (ParamMode::Value, CallArg::SharedBorrow(place) | CallArg::MutBorrow(place)) => {
                Some(CallArg::Value(Operand::Place(place.clone())))
            }
            (ParamMode::Value, CallArg::SharedStringConst(id)) => {
                Some(CallArg::Value(Operand::Const(*id)))
            }
            (ParamMode::SharedBorrow, CallArg::Value(Operand::Place(place))) => {
                Some(CallArg::SharedBorrow(place.clone()))
            }
            (ParamMode::SharedBorrow, CallArg::Value(Operand::Const(value))) => {
                let ty = const_types[value.index()];
                let local = LocalId::from_index(locals.len());
                locals.push(super::Local {
                    name: None,
                    ty,
                    mutability: super::Mutability::Immutable,
                    kind: super::LocalKind::Temp,
                });
                prepended.push(AirStmt::Init {
                    local,
                    value: RValue::Use(Operand::Const(*value)),
                });
                Some(CallArg::SharedBorrow(Place {
                    root: local,
                    projection: vec![],
                    ty,
                }))
            }
            (
                ParamMode::MutBorrow,
                CallArg::Value(Operand::Place(place)) | CallArg::SharedBorrow(place),
            ) => Some(CallArg::MutBorrow(place.clone())),
            _ => None,
        };
        if let Some(replacement) = replacement {
            *arg = replacement;
        }
    }
    prepended
}

fn operand_type(operand: &Operand, const_types: &[TypeId]) -> Option<TypeId> {
    match operand {
        Operand::Place(place) => Some(place.ty),
        Operand::Const(value) => const_types.get(value.index()).copied(),
    }
}

fn collect_function_type_modes(
    program: &Program,
    modes: &[Vec<ParamMode>],
) -> Vec<(TypeId, Vec<ParamMode>)> {
    let classes = TypePassClasses::analyze(program);
    let mut updates = program
        .type_arena
        .iter()
        .enumerate()
        .filter_map(|(index, ty)| {
            let TypeData::Function(sig) = ty else {
                return None;
            };
            Some((
                TypeId::from_index(index),
                sig.params
                    .iter()
                    .map(|param| initial_param_mode(param.mode, classes.get(param.ty)))
                    .collect(),
            ))
        })
        .collect::<Vec<_>>();
    for function in &program.functions {
        function.body.for_each_rvalue(&mut |value| {
            if let RValue::MakeClosure { func, ty, .. } = value
                && let Some(modes) = modes.get(func.index())
            {
                updates.push((*ty, modes.clone()));
            }
        });
    }
    updates
}

pub fn finalize(program: &mut Program) -> Result<(), Vec<OwnershipError>> {
    let modes = infer_param_modes(program)?;
    apply_param_modes(program, &modes);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        air::{
            AggregateDecl, AirBlock, AirBody, AirStmt, AirTail, CallArg, Callee, ConstData,
            ConstValue, DynContractData, EnumDecl, ExternRep, ExternTypeDecl, FieldDecl, Function,
            FunctionKind, Local, LocalKind, MapOrder, Module, ModuleId, Mutability, Operand, Param,
            ParamRole, Place, RValue, ReturnMode, Signature, VariantDecl,
        },
        ast::Ident,
    };

    fn module(program: &mut Program) -> ModuleId {
        program.alloc_module(Module {
            path: vec![Ident::new("test")],
            functions: vec![],
            aggregates: vec![],
            enums: vec![],
            extern_types: vec![],
            externs: vec![],
        })
    }

    fn classes(program: &Program) -> TypePassClasses {
        TypePassClasses::analyze(program)
    }

    fn param_function(
        program: &mut Program,
        ty: TypeId,
        statements: Vec<AirStmt>,
        tail: AirTail,
    ) -> FunctionId {
        let module = module(program);
        let local = LocalId::from_index(0);
        let void = program.alloc_type(TypeData::Void);
        program.alloc_function(Function {
            name: Ident::new("f"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature {
                params: vec![Param {
                    name: Some(Ident::new("arg")),
                    ty,
                    mode: ParamMode::Value,
                    role: ParamRole::Normal,
                    local_id: local,
                }],
                return_mode: ReturnMode::Value(void),
            },
            locals: vec![Local {
                name: Some(Ident::new("arg")),
                ty,
                mutability: Mutability::Immutable,
                kind: LocalKind::Arg,
            }],
            body: test_body(statements, tail),
        })
    }

    fn test_body(stmts: Vec<AirStmt>, tail: AirTail) -> AirBody {
        AirBody {
            block: AirBlock { stmts, tail },
        }
    }

    fn param_place(ty: TypeId) -> Place {
        Place {
            root: LocalId::from_index(0),
            projection: vec![],
            ty,
        }
    }

    fn param_operand(ty: TypeId) -> Operand {
        Operand::Place(param_place(ty))
    }

    #[test]
    fn param_use_read_only_large_param() {
        let mut program = Program::default();
        let string = program.alloc_type(TypeData::String);
        let function = param_function(
            &mut program,
            string,
            vec![AirStmt::Eval(RValue::Stringify {
                value: param_operand(string),
                source_ty: string,
            })],
            AirTail::Return(None),
        );

        assert_eq!(
            analyze_param_uses(&program, function),
            vec![ParamUse::ReadOnly]
        );
    }

    #[test]
    fn param_use_return_by_value() {
        let mut program = Program::default();
        let string = program.alloc_type(TypeData::String);
        let function = param_function(
            &mut program,
            string,
            vec![],
            AirTail::Return(Some(param_operand(string))),
        );

        assert_eq!(
            analyze_param_uses(&program, function),
            vec![ParamUse::ValueRequired]
        );
    }

    #[test]
    fn param_use_store_by_value() {
        let mut program = Program::default();
        let string = program.alloc_type(TypeData::String);
        let dst = LocalId::from_index(1);
        let function = param_function(
            &mut program,
            string,
            vec![AirStmt::Init {
                local: dst,
                value: RValue::Use(param_operand(string)),
            }],
            AirTail::Return(None),
        );
        program.function_mut(function).locals.push(Local {
            name: Some(Ident::new("tmp")),
            ty: string,
            mutability: Mutability::Immutable,
            kind: LocalKind::Temp,
        });

        assert_eq!(
            analyze_param_uses(&program, function),
            vec![ParamUse::ValueRequired]
        );
    }

    #[test]
    fn param_use_reborrow_mut() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let value = program.alloc_const(ConstData {
            ty: int,
            value: ConstValue::Int(1),
        });
        let function = param_function(
            &mut program,
            int,
            vec![AirStmt::Assign {
                dst: param_place(int),
                value: RValue::Use(Operand::Const(value)),
            }],
            AirTail::Return(None),
        );

        assert_eq!(
            analyze_param_uses(&program, function),
            vec![ParamUse::ReborrowMut]
        );
    }

    #[test]
    fn param_use_call_arg_modes() {
        let mut program = Program::default();
        let string = program.alloc_type(TypeData::String);
        let function = param_function(
            &mut program,
            string,
            vec![AirStmt::Eval(RValue::Call {
                callee: Callee::Function(FunctionId::from_index(999)),
                args: vec![CallArg::SharedBorrow(param_place(string))],
            })],
            AirTail::Return(None),
        );
        assert_eq!(
            analyze_param_uses(&program, function),
            vec![ParamUse::ReadOnly]
        );

        let function = param_function(
            &mut program,
            string,
            vec![AirStmt::Eval(RValue::Call {
                callee: Callee::Function(FunctionId::from_index(999)),
                args: vec![CallArg::Value(param_operand(string))],
            })],
            AirTail::Return(None),
        );
        assert_eq!(
            analyze_param_uses(&program, function),
            vec![ParamUse::ValueRequired]
        );
    }

    #[test]
    fn param_use_mutating_helper_operand() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let list = program.alloc_type(TypeData::List(int));
        let value = program.alloc_const(ConstData {
            ty: int,
            value: ConstValue::Int(1),
        });
        let function = param_function(
            &mut program,
            list,
            vec![AirStmt::Eval(RValue::ListPush {
                list: param_place(list),
                value: Operand::Const(value),
            })],
            AirTail::Return(None),
        );

        assert_eq!(
            analyze_param_uses(&program, function),
            vec![ParamUse::ReborrowMut]
        );
    }

    #[test]
    fn param_use_string_concat_reads_parts() {
        let mut program = Program::default();
        let string = program.alloc_type(TypeData::String);
        let function = param_function(
            &mut program,
            string,
            vec![AirStmt::Eval(RValue::StringConcat {
                parts: vec![param_operand(string)],
            })],
            AirTail::Return(None),
        );

        assert_eq!(
            analyze_param_uses(&program, function),
            vec![ParamUse::ReadOnly]
        );
    }

    #[test]
    fn param_use_helper_index_locals_are_read() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let list = program.alloc_type(TypeData::List(int));
        let module = module(&mut program);
        let void = program.alloc_type(TypeData::Void);
        let list_local = LocalId::from_index(0);
        let index_local = LocalId::from_index(1);
        let function = program.alloc_function(Function {
            name: Ident::new("f"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature {
                params: vec![
                    Param {
                        name: Some(Ident::new("xs")),
                        ty: list,
                        mode: ParamMode::SharedBorrow,
                        role: ParamRole::Normal,
                        local_id: list_local,
                    },
                    Param {
                        name: Some(Ident::new("i")),
                        ty: int,
                        mode: ParamMode::Value,
                        role: ParamRole::Normal,
                        local_id: index_local,
                    },
                ],
                return_mode: ReturnMode::Value(void),
            },
            locals: vec![
                Local {
                    name: Some(Ident::new("xs")),
                    ty: list,
                    mutability: Mutability::Immutable,
                    kind: LocalKind::Arg,
                },
                Local {
                    name: Some(Ident::new("i")),
                    ty: int,
                    mutability: Mutability::Immutable,
                    kind: LocalKind::Arg,
                },
            ],
            body: test_body(
                vec![AirStmt::Eval(RValue::MapEntryAt {
                    map: Place {
                        root: list_local,
                        projection: vec![],
                        ty: list,
                    },
                    index: index_local,
                    ty: int,
                })],
                AirTail::Return(None),
            ),
        });

        assert_eq!(
            analyze_param_uses(&program, function),
            vec![ParamUse::ReadOnly, ParamUse::ReadOnly]
        );
    }

    #[test]
    fn param_use_closure_callee_operand_is_read() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let void = program.alloc_type(TypeData::Void);
        let func_ty = program.alloc_type(TypeData::Function(super::super::SignatureType {
            params: vec![],
            ret: ReturnMode::Value(void),
        }));
        let arg = program.alloc_const(ConstData {
            ty: int,
            value: ConstValue::Int(1),
        });
        let function = param_function(
            &mut program,
            func_ty,
            vec![AirStmt::Eval(RValue::Call {
                callee: Callee::Closure(param_operand(func_ty)),
                args: vec![CallArg::Value(Operand::Const(arg))],
            })],
            AirTail::Return(None),
        );

        assert_eq!(
            analyze_param_uses(&program, function),
            vec![ParamUse::ReadOnly]
        );
    }

    #[test]
    fn finalize_rejects_mut_reborrow_from_non_mut_param() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let value = program.alloc_const(ConstData {
            ty: int,
            value: ConstValue::Int(1),
        });
        let function = param_function(
            &mut program,
            int,
            vec![AirStmt::Assign {
                dst: param_place(int),
                value: RValue::Use(Operand::Const(value)),
            }],
            AirTail::Return(None),
        );

        let errors = finalize(&mut program).expect_err("expected ownership error");
        assert!(errors.iter().any(|error| matches!(
            error,
            OwnershipError::ReborrowMutImmutableParam { function: found, param: 0 }
                if *found == function
        )));
    }

    #[test]
    fn infer_param_modes_borrows_large_read_only_param() {
        let mut program = Program::default();
        let string = program.alloc_type(TypeData::String);
        let function = param_function(
            &mut program,
            string,
            vec![AirStmt::Eval(RValue::Stringify {
                value: param_operand(string),
                source_ty: string,
            })],
            AirTail::Return(None),
        );

        let modes = infer_param_modes(&program).expect("mode inference failed");
        assert_eq!(modes[function.index()], vec![ParamMode::SharedBorrow]);

        finalize(&mut program).expect("ownership finalization failed");
        assert_eq!(
            program.function(function).signature.params[0].mode,
            ParamMode::SharedBorrow
        );
    }

    #[test]
    fn infer_param_modes_keeps_cheap_read_only_param_by_value() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let function = param_function(
            &mut program,
            int,
            vec![AirStmt::Eval(RValue::Stringify {
                value: param_operand(int),
                source_ty: int,
            })],
            AirTail::Return(None),
        );

        let modes = infer_param_modes(&program).expect("mode inference failed");
        assert_eq!(modes[function.index()], vec![ParamMode::Value]);
    }

    #[test]
    fn infer_param_modes_uses_value_for_noncheap_value_required_param() {
        let mut program = Program::default();
        let string = program.alloc_type(TypeData::String);
        let function = param_function(
            &mut program,
            string,
            vec![],
            AirTail::Return(Some(param_operand(string))),
        );

        let modes = infer_param_modes(&program).expect("mode inference failed");
        assert_eq!(modes[function.index()], vec![ParamMode::Value]);
    }

    #[test]
    fn infer_param_modes_preserves_noncheap_value_required_var_param() {
        let mut program = Program::default();
        let string = program.alloc_type(TypeData::String);
        let function = param_function(
            &mut program,
            string,
            vec![],
            AirTail::Return(Some(param_operand(string))),
        );
        program.function_mut(function).signature.params[0].mode = ParamMode::MutBorrow;

        let modes = infer_param_modes(&program).expect("mode inference failed");
        assert_eq!(modes[function.index()], vec![ParamMode::MutBorrow]);
    }

    #[test]
    fn infer_param_modes_preserves_mixed_noncheap_var_param() {
        let mut program = Program::default();
        let string = program.alloc_type(TypeData::String);
        let tmp = LocalId::from_index(1);
        let replacement = program.alloc_const(ConstData {
            ty: string,
            value: ConstValue::String("x".into()),
        });
        let function = param_function(
            &mut program,
            string,
            vec![
                AirStmt::Init {
                    local: tmp,
                    value: RValue::Use(param_operand(string)),
                },
                AirStmt::Assign {
                    dst: param_place(string),
                    value: RValue::Use(Operand::Const(replacement)),
                },
            ],
            AirTail::Return(None),
        );
        program.function_mut(function).signature.params[0].mode = ParamMode::MutBorrow;
        program.function_mut(function).locals.push(Local {
            name: Some(Ident::new("tmp")),
            ty: string,
            mutability: Mutability::Immutable,
            kind: LocalKind::Temp,
        });

        let modes = infer_param_modes(&program).expect("mode inference failed");
        assert_eq!(modes[function.index()], vec![ParamMode::MutBorrow]);
    }

    #[test]
    fn infer_param_modes_propagates_borrow_through_forwarding_call() {
        let mut program = Program::default();
        let string = program.alloc_type(TypeData::String);
        let forward = param_function(
            &mut program,
            string,
            vec![AirStmt::Eval(RValue::Call {
                callee: Callee::Function(FunctionId::from_index(1)),
                args: vec![CallArg::Value(param_operand(string))],
            })],
            AirTail::Return(None),
        );
        let read = param_function(
            &mut program,
            string,
            vec![AirStmt::Eval(RValue::Stringify {
                value: param_operand(string),
                source_ty: string,
            })],
            AirTail::Return(None),
        );

        let modes = infer_param_modes(&program).expect("mode inference failed");
        assert_eq!(modes[read.index()], vec![ParamMode::SharedBorrow]);
        assert_eq!(modes[forward.index()], vec![ParamMode::SharedBorrow]);
    }

    #[test]
    fn finalize_rewrites_direct_value_place_arg_to_shared_borrow() {
        let mut program = Program::default();
        let string = program.alloc_type(TypeData::String);
        let read = param_function(
            &mut program,
            string,
            vec![AirStmt::Eval(RValue::Stringify {
                value: param_operand(string),
                source_ty: string,
            })],
            AirTail::Return(None),
        );
        let caller = param_function(
            &mut program,
            string,
            vec![AirStmt::Eval(RValue::Call {
                callee: Callee::Function(read),
                args: vec![CallArg::Value(param_operand(string))],
            })],
            AirTail::Return(None),
        );

        finalize(&mut program).expect("ownership finalization failed");
        let AirStmt::Eval(RValue::Call { args, .. }) =
            &program.function(caller).body.block.stmts[0]
        else {
            panic!("expected call");
        };
        assert!(matches!(args[0], CallArg::SharedBorrow(_)));
    }

    #[test]
    fn finalize_materializes_const_arg_after_shared_borrow_inference() {
        let mut program = Program::default();
        let string = program.alloc_type(TypeData::String);
        let read = param_function(
            &mut program,
            string,
            vec![AirStmt::Eval(RValue::Stringify {
                value: param_operand(string),
                source_ty: string,
            })],
            AirTail::Return(None),
        );
        let arg = program.alloc_const(ConstData {
            ty: string,
            value: ConstValue::String("x".into()),
        });
        let module = module(&mut program);
        let void = program.alloc_type(TypeData::Void);
        let caller = program.alloc_function(Function {
            name: Ident::new("caller"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], void),
            locals: vec![],
            body: test_body(
                vec![AirStmt::Eval(RValue::Call {
                    callee: Callee::Function(read),
                    args: vec![CallArg::Value(Operand::Const(arg))],
                })],
                AirTail::Return(None),
            ),
        });

        finalize(&mut program).expect("ownership finalization failed");
        let stmts = &program.function(caller).body.block.stmts;
        assert!(matches!(stmts[0], AirStmt::Init { .. }));
        assert!(matches!(
            stmts[1],
            AirStmt::Eval(RValue::Call { ref args, .. })
                if matches!(args[0], CallArg::SharedBorrow(_))
        ));
    }

    #[test]
    fn finalize_rewrites_shared_string_const_to_value_arg() {
        let mut program = Program::default();
        let string = program.alloc_type(TypeData::String);
        let id = param_function(
            &mut program,
            string,
            vec![],
            AirTail::Return(Some(param_operand(string))),
        );
        let arg = program.alloc_const(ConstData {
            ty: string,
            value: ConstValue::String("x".into()),
        });
        let module = module(&mut program);
        let void = program.alloc_type(TypeData::Void);
        let caller = program.alloc_function(Function {
            name: Ident::new("caller"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], void),
            locals: vec![],
            body: test_body(
                vec![AirStmt::Eval(RValue::Call {
                    callee: Callee::Function(id),
                    args: vec![CallArg::SharedStringConst(arg)],
                })],
                AirTail::Return(None),
            ),
        });

        finalize(&mut program).expect("ownership finalization failed");
        let AirStmt::Eval(RValue::Call { args, .. }) =
            &program.function(caller).body.block.stmts[0]
        else {
            panic!("expected call");
        };
        assert!(matches!(args[0], CallArg::Value(Operand::Const(found)) if found == arg));
    }

    #[test]
    fn finalize_rewrites_closure_call_args() {
        let mut program = Program::default();
        let string = program.alloc_type(TypeData::String);
        let void = program.alloc_type(TypeData::Void);
        let closure_ty = program.alloc_type(TypeData::Function(super::super::SignatureType {
            params: vec![super::super::ParamType {
                ty: string,
                mode: ParamMode::SharedBorrow,
            }],
            ret: ReturnMode::Value(void),
        }));
        let closure_local = LocalId::from_index(0);
        let arg = program.alloc_const(ConstData {
            ty: string,
            value: ConstValue::String("x".into()),
        });
        let module = module(&mut program);
        let caller = program.alloc_function(Function {
            name: Ident::new("caller"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(
                vec![Param {
                    name: Some(Ident::new("f")),
                    ty: closure_ty,
                    mode: ParamMode::Value,
                    role: ParamRole::Normal,
                    local_id: closure_local,
                }],
                void,
            ),
            locals: vec![Local {
                name: Some(Ident::new("f")),
                ty: closure_ty,
                mutability: Mutability::Immutable,
                kind: LocalKind::Arg,
            }],
            body: test_body(
                vec![AirStmt::Eval(RValue::Call {
                    callee: Callee::Closure(Operand::Place(Place {
                        root: closure_local,
                        projection: vec![],
                        ty: closure_ty,
                    })),
                    args: vec![CallArg::Value(Operand::Const(arg))],
                })],
                AirTail::Return(None),
            ),
        });

        finalize(&mut program).expect("ownership finalization failed");
        let stmts = &program.function(caller).body.block.stmts;
        assert!(matches!(stmts[0], AirStmt::Init { .. }));
        assert!(matches!(
            stmts[1],
            AirStmt::Eval(RValue::Call { ref args, .. })
                if matches!(args[0], CallArg::SharedBorrow(_))
        ));
    }

    #[test]
    fn finalize_updates_closure_function_type_modes() {
        let mut program = Program::default();
        let string = program.alloc_type(TypeData::String);
        let void = program.alloc_type(TypeData::Void);
        let read = param_function(
            &mut program,
            string,
            vec![AirStmt::Eval(RValue::Stringify {
                value: param_operand(string),
                source_ty: string,
            })],
            AirTail::Return(None),
        );
        let closure_ty = program.alloc_type(TypeData::Function(super::super::SignatureType {
            params: vec![super::super::ParamType {
                ty: string,
                mode: ParamMode::Value,
            }],
            ret: ReturnMode::Value(void),
        }));
        let module = module(&mut program);
        program.alloc_function(Function {
            name: Ident::new("make"),
            module,
            kind: FunctionKind::Normal,
            owner: None,
            specialization: None,
            signature: Signature::new(vec![], void),
            locals: vec![],
            body: test_body(
                vec![AirStmt::Eval(RValue::MakeClosure {
                    func: read,
                    captures: vec![],
                    ty: closure_ty,
                })],
                AirTail::Return(None),
            ),
        });

        finalize(&mut program).expect("ownership finalization failed");
        let TypeData::Function(sig) = program.type_data(closure_ty) else {
            panic!("expected function type");
        };
        assert_eq!(sig.params[0].mode, ParamMode::SharedBorrow);
    }

    #[test]
    fn infer_param_modes_handles_self_recursion_without_value_copy() {
        let mut program = Program::default();
        let string = program.alloc_type(TypeData::String);
        let function = param_function(
            &mut program,
            string,
            vec![AirStmt::Eval(RValue::Call {
                callee: Callee::Function(FunctionId::from_index(0)),
                args: vec![CallArg::Value(param_operand(string))],
            })],
            AirTail::Return(None),
        );

        let modes = infer_param_modes(&program).expect("mode inference failed");
        assert_eq!(modes[function.index()], vec![ParamMode::SharedBorrow]);
    }

    #[test]
    fn primitives() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let float = program.alloc_type(TypeData::Float);
        let bool_ = program.alloc_type(TypeData::Bool);
        let void = program.alloc_type(TypeData::Void);
        let classes = classes(&program);
        assert_eq!(classes.class(int), TypePassClass::Immediate);
        assert_eq!(classes.class(float), TypePassClass::Immediate);
        assert_eq!(classes.class(bool_), TypePassClass::Immediate);
        assert_eq!(classes.class(void), TypePassClass::Immediate);
    }

    #[test]
    fn managed_buffers() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let string = program.alloc_type(TypeData::String);
        let list = program.alloc_type(TypeData::List(int));
        let map = program.alloc_type(TypeData::Map {
            key: int,
            value: int,
            order: MapOrder::Insertion,
        });
        let classes = classes(&program);
        assert_eq!(classes.class(string), TypePassClass::ManagedBuffer);
        assert_eq!(classes.class(list), TypePassClass::ManagedBuffer);
        assert_eq!(classes.class(map), TypePassClass::ManagedBuffer);
    }

    #[test]
    fn handles_and_opaque_types() {
        let mut program = Program::default();
        let module = module(&mut program);
        let dataref_agg = program.alloc_aggregate(AggregateDecl {
            name: Ident::new("Node"),
            module,
            kind: AggregateKind::DataRef,
            type_args: vec![],
            const_args: vec![],
            fields: vec![],
            cycle_capable: true,
            stringify_override: None,
        });
        let shared_ext = program.alloc_extern_type(ExternTypeDecl {
            name: Ident::new("SharedExt"),
            module,
            binding: None,
            type_args: vec![],
            const_args: vec![],
            rep: ExternRep::Shared,
            has_init: false,
            fields: vec![],
            methods: vec![],
            statics: vec![],
            operators: vec![],
        });
        let inline_ext = program.alloc_extern_type(ExternTypeDecl {
            name: Ident::new("InlineExt"),
            module,
            binding: None,
            type_args: vec![],
            const_args: vec![],
            rep: ExternRep::Inline,
            has_init: false,
            fields: vec![],
            methods: vec![],
            statics: vec![],
            operators: vec![],
        });
        let void = program.alloc_type(TypeData::Void);
        let dataref = program.alloc_type(TypeData::DataRef(dataref_agg));
        let function = program.alloc_type(TypeData::Function(super::super::SignatureType {
            params: vec![],
            ret: ReturnMode::Value(void),
        }));
        let dyn_ = program.alloc_type(TypeData::Dyn(DynContractData {
            display_name: "AnyAction".into(),
            method_table_key: "AnyAction".into(),
            concrete_printer: None,
        }));
        let shared_ext = program.alloc_type(TypeData::Extern(shared_ext));
        let inline_ext = program.alloc_type(TypeData::Extern(inline_ext));
        let any = program.alloc_type(TypeData::Any);
        let slice = program.alloc_type(TypeData::Slice(void));
        let classes = classes(&program);
        assert_eq!(classes.class(dataref), TypePassClass::CheapHandle);
        assert_eq!(classes.class(function), TypePassClass::CheapHandle);
        assert_eq!(classes.class(dyn_), TypePassClass::CheapHandle);
        assert_eq!(classes.class(shared_ext), TypePassClass::CheapHandle);
        assert_eq!(
            classes.class(inline_ext),
            TypePassClass::SmallCopyInline(AirCopyLayout { size: 0, align: 1 })
        );
        assert_eq!(classes.class(any), TypePassClass::Opaque);
        assert_eq!(classes.class(slice), TypePassClass::Opaque);
    }

    #[test]
    fn zero_sized_tuple_and_struct_are_small_inline() {
        let mut program = Program::default();
        let module = module(&mut program);
        let tuple = program.alloc_type(TypeData::Tuple(vec![]));
        let agg = program.alloc_aggregate(AggregateDecl {
            name: Ident::new("Empty"),
            module,
            kind: AggregateKind::Struct,
            type_args: vec![],
            const_args: vec![],
            fields: vec![],
            cycle_capable: false,
            stringify_override: None,
        });
        let aggregate = program.alloc_type(TypeData::Aggregate(agg));
        let classes = classes(&program);
        let zst = TypePassClass::SmallCopyInline(AirCopyLayout { size: 0, align: 1 });
        assert_eq!(classes.class(tuple), zst);
        assert_eq!(classes.class(aggregate), zst);
    }

    #[test]
    fn optional_uses_recursive_layout() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let optional = program.alloc_type(TypeData::Optional(int));
        assert_eq!(
            classes(&program).class(optional),
            TypePassClass::SmallCopyInline(AirCopyLayout { size: 16, align: 8 })
        );
    }

    #[test]
    fn tuple_threshold() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let four = program.alloc_type(TypeData::Tuple(vec![int; 4]));
        let five = program.alloc_type(TypeData::Tuple(vec![int; 5]));
        let classes = classes(&program);
        assert_eq!(
            classes.class(four),
            TypePassClass::SmallCopyInline(AirCopyLayout { size: 32, align: 8 })
        );
        assert_eq!(
            classes.class(five),
            TypePassClass::LargeInline(AirCopyLayout { size: 40, align: 8 })
        );
    }

    #[test]
    fn aggregate_threshold() {
        let mut program = Program::default();
        let module = module(&mut program);
        let int = program.alloc_type(TypeData::Int);
        let small = program.alloc_aggregate(AggregateDecl {
            name: Ident::new("Small"),
            module,
            kind: AggregateKind::Struct,
            type_args: vec![],
            const_args: vec![],
            fields: vec![
                FieldDecl {
                    name: Ident::new("a"),
                    ty: int,
                },
                FieldDecl {
                    name: Ident::new("b"),
                    ty: int,
                },
                FieldDecl {
                    name: Ident::new("c"),
                    ty: int,
                },
                FieldDecl {
                    name: Ident::new("d"),
                    ty: int,
                },
            ],
            cycle_capable: false,
            stringify_override: None,
        });
        let large = program.alloc_aggregate(AggregateDecl {
            name: Ident::new("Large"),
            module,
            kind: AggregateKind::Struct,
            type_args: vec![],
            const_args: vec![],
            fields: vec![
                FieldDecl {
                    name: Ident::new("a"),
                    ty: int,
                },
                FieldDecl {
                    name: Ident::new("b"),
                    ty: int,
                },
                FieldDecl {
                    name: Ident::new("c"),
                    ty: int,
                },
                FieldDecl {
                    name: Ident::new("d"),
                    ty: int,
                },
                FieldDecl {
                    name: Ident::new("e"),
                    ty: int,
                },
            ],
            cycle_capable: false,
            stringify_override: None,
        });
        let small = program.alloc_type(TypeData::Aggregate(small));
        let large = program.alloc_type(TypeData::Aggregate(large));
        let classes = classes(&program);
        assert_eq!(
            classes.class(small),
            TypePassClass::SmallCopyInline(AirCopyLayout { size: 32, align: 8 })
        );
        assert_eq!(
            classes.class(large),
            TypePassClass::LargeInline(AirCopyLayout { size: 40, align: 8 })
        );
    }

    #[test]
    fn aggregate_with_managed_field_is_known_noncopy() {
        let mut program = Program::default();
        let module = module(&mut program);
        let string = program.alloc_type(TypeData::String);
        let agg = program.alloc_aggregate(AggregateDecl {
            name: Ident::new("Named"),
            module,
            kind: AggregateKind::Struct,
            type_args: vec![],
            const_args: vec![],
            fields: vec![FieldDecl {
                name: Ident::new("name"),
                ty: string,
            }],
            cycle_capable: false,
            stringify_override: None,
        });
        let ty = program.alloc_type(TypeData::Aggregate(agg));
        assert_eq!(
            classes(&program).class(ty),
            TypePassClass::LargeInline(AirCopyLayout { size: 8, align: 8 })
        );
    }

    #[test]
    fn recursive_inline_aggregate_is_opaque() {
        let mut program = Program::default();
        let module = module(&mut program);
        let agg = program.alloc_aggregate(AggregateDecl {
            name: Ident::new("Node"),
            module,
            kind: AggregateKind::Struct,
            type_args: vec![],
            const_args: vec![],
            fields: vec![],
            cycle_capable: false,
            stringify_override: None,
        });
        let ty = program.alloc_type(TypeData::Aggregate(agg));
        program.aggregate_mut(agg).fields.push(FieldDecl {
            name: Ident::new("next"),
            ty,
        });
        assert_eq!(classes(&program).class(ty), TypePassClass::Opaque);
    }

    #[test]
    fn enum_unit_variant_keeps_zero_payload() {
        let mut program = Program::default();
        let module = module(&mut program);
        let int = program.alloc_type(TypeData::Int);
        let enm = program.alloc_enum(EnumDecl {
            name: Ident::new("MaybeInt"),
            module,
            type_args: vec![],
            const_args: vec![],
            core: None,
            variants: vec![
                VariantDecl {
                    name: Ident::new("None"),
                    shape: VariantShape::Unit,
                },
                VariantDecl {
                    name: Ident::new("Some"),
                    shape: VariantShape::Tuple(vec![int]),
                },
            ],
        });
        let ty = program.alloc_type(TypeData::Enum(enm));
        assert_eq!(
            classes(&program).class(ty),
            TypePassClass::SmallCopyInline(AirCopyLayout { size: 16, align: 8 })
        );
    }

    #[test]
    fn enum_uses_largest_payload() {
        let mut program = Program::default();
        let module = module(&mut program);
        let int = program.alloc_type(TypeData::Int);
        let enm = program.alloc_enum(EnumDecl {
            name: Ident::new("E"),
            module,
            type_args: vec![],
            const_args: vec![],
            core: None,
            variants: vec![
                VariantDecl {
                    name: Ident::new("A"),
                    shape: VariantShape::Tuple(vec![int]),
                },
                VariantDecl {
                    name: Ident::new("B"),
                    shape: VariantShape::Tuple(vec![int, int]),
                },
            ],
        });
        let ty = program.alloc_type(TypeData::Enum(enm));
        assert_eq!(
            classes(&program).class(ty),
            TypePassClass::SmallCopyInline(AirCopyLayout { size: 24, align: 8 })
        );
    }

    #[test]
    fn array_threshold() {
        let mut program = Program::default();
        let int = program.alloc_type(TypeData::Int);
        let small = program.alloc_type(TypeData::Array { elem: int, len: 4 });
        let large = program.alloc_type(TypeData::Array { elem: int, len: 5 });
        let classes = classes(&program);
        assert_eq!(
            classes.class(small),
            TypePassClass::SmallCopyInline(AirCopyLayout { size: 32, align: 8 })
        );
        assert_eq!(
            classes.class(large),
            TypePassClass::LargeInline(AirCopyLayout { size: 40, align: 8 })
        );
    }
}
