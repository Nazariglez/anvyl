use std::{collections::HashMap, fmt};

use super::{
    bytecode::{CastKind, Chunk, Op},
    cycle_collector::make_dataref_vtable,
    managed_rc::ManagedRc,
    value::Value,
};
use crate::{
    Profile,
    ast::{BinaryOp, Type, UnaryOp},
    builtin::Builtin,
    hir,
    ir_meta::{AggregateKind, AggregateMeta, EnumMeta},
};

#[derive(Debug)]
pub enum CompileError {
    NoMainFunction,
    TooManyConstants { func_name: String },
    TooManyLocals { func_name: String },
    TooManyParams { func_name: String },
    JumpTooFar { func_name: String },
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NoMainFunction => write!(f, "no main function found"),
            Self::TooManyConstants { func_name } => {
                write!(f, "too many constants in function '{func_name}'")
            }
            Self::TooManyLocals { func_name } => {
                write!(f, "too many locals in function '{func_name}'")
            }
            Self::TooManyParams { func_name } => {
                write!(f, "too many parameters in function '{func_name}' (max 255)")
            }
            Self::JumpTooFar { func_name } => {
                write!(
                    f,
                    "function '{func_name}' is too large: jump offset exceeds i16 range"
                )
            }
        }
    }
}

pub struct CompiledProgram {
    pub chunks: Vec<Chunk>,
    pub main_idx: usize,
    pub extern_names: Vec<String>,
    pub aggregate_meta: Vec<AggregateMeta>,
    pub enum_meta: Vec<EnumMeta>,
    pub aggregate_vtables: Vec<Option<&'static anvyx_runtime::CycleVtable>>,
    pub profile: Profile,
}

struct LoopState {
    start: usize,
    break_patches: Vec<usize>,
}

enum WriteThroughKind {
    Field(u16),
    Whole,
}

struct WriteThroughInfo {
    ref_local: hir::LocalId,
    kind: WriteThroughKind,
}

struct FuncCompiler<'a> {
    chunk: Chunk,
    loop_stack: Vec<LoopState>,
    locals: &'a [hir::Local],
    write_through_map: HashMap<hir::LocalId, WriteThroughInfo>,
}

impl<'a> FuncCompiler<'a> {
    fn new(
        name: impl Into<String>,
        local_count: u16,
        params_count: u8,
        locals: &'a [hir::Local],
    ) -> Self {
        Self {
            chunk: Chunk::new(name, local_count, params_count),
            loop_stack: vec![],
            locals,
            write_through_map: HashMap::new(),
        }
    }

    fn emit(&mut self, op: Op) {
        self.chunk.emit(op);
    }

    fn emit_jump(&mut self, op: Op) -> usize {
        self.chunk.emit_jump(op)
    }

    fn check_jump_offset(&self, offset: isize) -> Result<i16, CompileError> {
        if offset < i16::MIN as isize || offset > i16::MAX as isize {
            return Err(CompileError::JumpTooFar {
                func_name: self.chunk.name.clone(),
            });
        }
        Ok(offset as i16)
    }

    fn patch_jump(&mut self, pos: usize) -> Result<(), CompileError> {
        let target = self.chunk.code.len();
        let offset = self.check_jump_offset(target as isize - pos as isize - 1)?;
        match &mut self.chunk.code[pos] {
            Op::Jump(o) | Op::JumpIfFalse(o) => *o = offset,
            _ => panic!("patch_jump called on non-jump op at position {pos}"),
        }
        Ok(())
    }

    fn add_constant(&mut self, value: Value) -> Result<u16, CompileError> {
        let idx = self.chunk.constants.len();
        if idx > u16::MAX as usize {
            return Err(CompileError::TooManyConstants {
                func_name: self.chunk.name.clone(),
            });
        }
        Ok(self.chunk.add_constant(value))
    }

    fn emit_load_local_or_deref(&mut self, local_id: hir::LocalId) {
        if self.locals[local_id.0 as usize].is_ref {
            self.emit(Op::DerefRead(local_id.0 as u16));
        } else {
            self.emit(Op::GetLocal(local_id.0 as u16));
        }
    }

    fn emit_store_local_or_deref(&mut self, local_id: hir::LocalId) {
        if self.locals[local_id.0 as usize].is_ref {
            self.emit(Op::DerefWrite(local_id.0 as u16));
        } else {
            self.emit(Op::SetLocal(local_id.0 as u16));
        }
    }

    fn emit_write_through(&mut self, local: hir::LocalId) {
        let wt_data = self.write_through_map.get(&local).map(|wt| {
            let ref_local = wt.ref_local.0 as u16;
            let field_idx = match &wt.kind {
                WriteThroughKind::Field(f) => Some(*f),
                WriteThroughKind::Whole => None,
            };
            (ref_local, field_idx)
        });
        if let Some((ref_local, field_idx)) = wt_data {
            self.emit(Op::GetLocal(local.0 as u16));
            match field_idx {
                Some(f) => self.emit(Op::SetFieldRef(ref_local, f)),
                None => self.emit(Op::DerefWrite(ref_local)),
            }
        }
    }
}

fn build_aggregate_vtables(
    aggregate_meta: &[AggregateMeta],
) -> Vec<Option<&'static anvyx_runtime::CycleVtable>> {
    aggregate_meta
        .iter()
        .map(|meta| match meta.kind {
            AggregateKind::DataRef => Some(make_dataref_vtable(
                &meta.qualified_name,
                meta.cycle_capable,
            )),
            AggregateKind::Struct => None,
        })
        .collect()
}

pub fn compile(hir: &hir::Program, profile: Profile) -> Result<CompiledProgram, CompileError> {
    let mut chunks = vec![];

    for func in &hir.funcs {
        chunks.push(compile_func(func)?);
    }

    let main_idx = hir
        .funcs
        .iter()
        .position(|f| f.name.to_string() == "main")
        .ok_or(CompileError::NoMainFunction)?;

    let extern_names = hir.externs.iter().map(|e| e.name.to_string()).collect();

    Ok(CompiledProgram {
        chunks,
        main_idx,
        extern_names,
        aggregate_meta: hir.aggregate_meta.clone(),
        enum_meta: hir.enum_meta.clone(),
        aggregate_vtables: build_aggregate_vtables(&hir.aggregate_meta),
        profile,
    })
}

fn compile_func(func: &hir::Func) -> Result<Chunk, CompileError> {
    let local_count = func.locals.len();
    if local_count > u16::MAX as usize {
        return Err(CompileError::TooManyLocals {
            func_name: func.name.to_string(),
        });
    }
    if func.params_len > u32::from(u8::MAX) {
        return Err(CompileError::TooManyParams {
            func_name: func.name.to_string(),
        });
    }

    let mut fc = FuncCompiler::new(
        func.name.to_string(),
        local_count as u16,
        func.params_len as u8,
        &func.locals,
    );

    compile_block(&mut fc, &func.body)?;

    // implicit fallthrough, void return, unreachable if all paths already returned
    fc.emit(Op::Nil);
    fc.emit(Op::Return);

    Ok(fc.chunk)
}

fn compile_block(fc: &mut FuncCompiler<'_>, block: &hir::Block) -> Result<(), CompileError> {
    for stmt in &block.stmts {
        compile_stmt(fc, stmt)?;
    }
    Ok(())
}

fn compile_stmt(fc: &mut FuncCompiler<'_>, stmt: &hir::Stmt) -> Result<(), CompileError> {
    match &stmt.kind {
        hir::StmtKind::Let { local, init } => {
            compile_expr(fc, init)?;
            fc.emit(Op::SetLocal(local.0 as u16));
        }

        hir::StmtKind::Assign { local, value } => {
            compile_expr(fc, value)?;
            if fc.locals[local.0 as usize].is_ref {
                fc.emit(Op::DerefWrite(local.0 as u16));
            } else {
                fc.emit(Op::SetLocal(local.0 as u16));
            }
            fc.emit_write_through(*local);
        }

        hir::StmtKind::Expr(expr) => {
            compile_expr(fc, expr)?;
            fc.emit(Op::Pop);
        }

        hir::StmtKind::Return(None) => {
            fc.emit(Op::Nil);
            fc.emit(Op::Return);
        }

        hir::StmtKind::Return(Some(expr)) => {
            compile_expr(fc, expr)?;
            fc.emit(Op::Return);
        }

        hir::StmtKind::If {
            cond,
            then_block,
            else_block,
        } => {
            compile_expr(fc, cond)?;
            let jump_pos = fc.emit_jump(Op::JumpIfFalse(0));

            compile_block(fc, then_block)?;

            match else_block {
                Some(else_b) => {
                    let skip_else_pos = fc.emit_jump(Op::Jump(0));
                    fc.patch_jump(jump_pos)?;
                    compile_block(fc, else_b)?;
                    fc.patch_jump(skip_else_pos)?;
                }
                None => {
                    fc.patch_jump(jump_pos)?;
                }
            }
        }

        hir::StmtKind::While { cond, body } => {
            let loop_start = fc.chunk.code.len();
            fc.loop_stack.push(LoopState {
                start: loop_start,
                break_patches: vec![],
            });

            compile_expr(fc, cond)?;
            let exit_jump_pos = fc.emit_jump(Op::JumpIfFalse(0));

            compile_block(fc, body)?;

            // back-jump to loop condition, offset is negative
            let back_offset =
                fc.check_jump_offset(loop_start as isize - fc.chunk.code.len() as isize - 1)?;
            fc.emit(Op::Jump(back_offset));

            // patch exit jump to land after the back-jump
            fc.patch_jump(exit_jump_pos)?;

            // patch all break jumps to the same landing spot
            let loop_state = fc.loop_stack.pop().expect("loop stack underflow");
            let after_loop = fc.chunk.code.len();
            for patch_pos in loop_state.break_patches {
                let offset = fc.check_jump_offset(after_loop as isize - patch_pos as isize - 1)?;
                match &mut fc.chunk.code[patch_pos] {
                    Op::Jump(o) => *o = offset,
                    _ => panic!("expected Jump at break patch position {patch_pos}"),
                }
            }
        }

        hir::StmtKind::Break => {
            let pos = fc.chunk.code.len();
            fc.emit(Op::Jump(0));
            fc.loop_stack
                .last_mut()
                .expect("break outside loop (should be caught earlier)")
                .break_patches
                .push(pos);
        }

        hir::StmtKind::Continue => {
            let loop_start = fc
                .loop_stack
                .last()
                .expect("continue outside loop (should be caught earlier)")
                .start;
            let back_offset =
                fc.check_jump_offset(loop_start as isize - fc.chunk.code.len() as isize - 1)?;
            fc.emit(Op::Jump(back_offset));
        }

        hir::StmtKind::SetField {
            object,
            field_index,
            value,
        } => {
            if fc.locals[object.0 as usize].is_ref {
                compile_expr(fc, value)?;
                fc.emit(Op::SetFieldRef(object.0 as u16, *field_index));
            } else {
                fc.emit(Op::GetLocal(object.0 as u16));
                compile_expr(fc, value)?;
                fc.emit(Op::SetField(*field_index));
                fc.emit(Op::SetLocal(object.0 as u16));
            }
            fc.emit_write_through(*object);
        }

        hir::StmtKind::Match {
            scrutinee_init,
            scrutinee,
            write_through,
            arms,
            else_body,
        } => {
            // evaluate scrutinee and store to local
            compile_expr(fc, scrutinee_init)?;
            fc.emit(Op::SetLocal(scrutinee.0 as u16));

            // initialize ref local for writethrough if any
            if let Some(wt) = write_through {
                if fc.locals[wt.original.0 as usize].is_ref {
                    fc.emit(Op::GetLocal(wt.original.0 as u16));
                } else {
                    fc.emit(Op::PushRef(wt.original.0 as u16));
                }
                fc.emit(Op::SetLocal(wt.ref_local.0 as u16));
            }

            // we collect patch positions for the "jump to end" after each matched arm
            let mut end_jump_positions: Vec<usize> = vec![];

            for arm in arms {
                // check if variant matches, GetLocal scrutinee, GetEnumVariant, push expected, Eq, JumpIfFalse(skip)
                fc.emit(Op::GetLocal(scrutinee.0 as u16));
                fc.emit(Op::GetEnumVariant);
                let variant_idx = fc.add_constant(Value::Int(i64::from(arm.variant)))?;
                fc.emit(Op::Constant(variant_idx));
                fc.emit(Op::Eq);
                let skip_pos = fc.emit_jump(Op::JumpIfFalse(0));

                // bind fields to locals
                for binding in &arm.bindings {
                    fc.emit(Op::GetLocal(scrutinee.0 as u16));
                    fc.emit(Op::GetField(binding.field_index));
                    fc.emit(Op::SetLocal(binding.local.0 as u16));
                    if binding.mutable
                        && let Some(wt) = write_through
                    {
                        fc.write_through_map.insert(
                            binding.local,
                            WriteThroughInfo {
                                ref_local: wt.ref_local,
                                kind: WriteThroughKind::Field(binding.field_index),
                            },
                        );
                    }
                }

                // evaluate if false, skip to next arm
                let guard_skip_pos = if let Some(guard) = &arm.guard {
                    compile_expr(fc, guard)?;
                    Some(fc.emit_jump(Op::JumpIfFalse(0)))
                } else {
                    None
                };

                compile_block(fc, &arm.body)?;

                // remove writethrough entries for this arm's bindings
                for binding in &arm.bindings {
                    if binding.mutable {
                        fc.write_through_map.remove(&binding.local);
                    }
                }

                // jump past all remaining arms
                let end_pos = fc.emit_jump(Op::Jump(0));
                end_jump_positions.push(end_pos);

                // patch both the variant mismatch skip and the guard skip to the next arm
                fc.patch_jump(skip_pos)?;
                if let Some(guard_pos) = guard_skip_pos {
                    fc.patch_jump(guard_pos)?;
                }
            }

            // else_body (wildcard / Ident catch-all)
            if let Some(else_b) = else_body {
                if let Some((binding_local, mutable)) = else_b.binding {
                    fc.emit(Op::GetLocal(scrutinee.0 as u16));
                    fc.emit(Op::SetLocal(binding_local.0 as u16));
                    if mutable && let Some(wt) = write_through {
                        fc.write_through_map.insert(
                            binding_local,
                            WriteThroughInfo {
                                ref_local: wt.ref_local,
                                kind: WriteThroughKind::Whole,
                            },
                        );
                    }
                }
                compile_block(fc, &else_b.body)?;
                if let Some((binding_local, mutable)) = else_b.binding
                    && mutable
                {
                    fc.write_through_map.remove(&binding_local);
                }
            }

            // patch all end-jumps to land after the whole match
            for pos in end_jump_positions {
                fc.patch_jump(pos)?;
            }
        }

        hir::StmtKind::SetIndex {
            object,
            index,
            value,
        } => {
            if fc.locals[object.0 as usize].is_ref {
                compile_expr(fc, index)?;
                compile_expr(fc, value)?;
                fc.emit(Op::SetIndexRef(object.0 as u16));
            } else {
                fc.emit(Op::GetLocal(object.0 as u16));
                compile_expr(fc, index)?;
                compile_expr(fc, value)?;
                fc.emit(Op::IndexSet);
                fc.emit(Op::SetLocal(object.0 as u16));
            }
            fc.emit_write_through(*object);
        }
    }

    Ok(())
}

fn extract_ref_path(expr: &hir::Expr) -> (hir::LocalId, Vec<u16>) {
    match &expr.kind {
        hir::ExprKind::Local(id) => (*id, vec![]),
        hir::ExprKind::FieldGet { object, index } => {
            let (root, mut path) = extract_ref_path(object);
            path.push(*index);
            (root, path)
        }
        _ => panic!("ref arg must be a local or field access chain"),
    }
}

fn compile_ref_args(
    fc: &mut FuncCompiler<'_>,
    args: &[hir::Expr],
    ref_mask: &[bool],
) -> Result<(), CompileError> {
    for (arg, is_ref) in args.iter().zip(ref_mask.iter()) {
        if *is_ref {
            let (root_id, path) = extract_ref_path(arg);
            if path.is_empty() {
                if fc.locals[root_id.0 as usize].is_ref {
                    fc.emit(Op::GetLocal(root_id.0 as u16));
                } else {
                    fc.emit(Op::PushRef(root_id.0 as u16));
                }
            } else {
                let depth = path.len() as u8;
                assert!(depth <= 4, "field path depth exceeds maximum of 4");
                let mut segments = [0u16; 4];
                for (i, &seg) in path.iter().enumerate() {
                    segments[i] = seg;
                }
                fc.emit(Op::PushPathRef(root_id.0 as u16, depth, segments));
            }
        } else {
            compile_expr(fc, arg)?;
        }
    }
    Ok(())
}

fn compile_expr(fc: &mut FuncCompiler<'_>, expr: &hir::Expr) -> Result<(), CompileError> {
    match &expr.kind {
        hir::ExprKind::Local(id) => {
            let idx = id.0 as u16;
            if fc.locals[id.0 as usize].is_ref {
                fc.emit(Op::DerefRead(idx));
            } else {
                match expr.ownership {
                    hir::Ownership::Move => fc.emit(Op::MoveLocal(idx)),
                    hir::Ownership::Borrow => fc.emit(Op::GetLocal(idx)),
                    hir::Ownership::Own => fc.emit(Op::CloneLocal(idx)),
                }
            }
        }

        hir::ExprKind::Int(v) => {
            let idx = fc.add_constant(Value::Int(*v))?;
            fc.emit(Op::Constant(idx));
        }

        hir::ExprKind::Float(v) => {
            let idx = fc.add_constant(Value::Float(*v))?;
            fc.emit(Op::Constant(idx));
        }

        hir::ExprKind::Double(v) => {
            let idx = fc.add_constant(Value::Double(*v))?;
            fc.emit(Op::Constant(idx));
        }

        hir::ExprKind::Bool(v) => {
            fc.emit(if *v { Op::True } else { Op::False });
        }

        hir::ExprKind::String(s) => {
            let idx = fc.add_constant(Value::String(ManagedRc::new(s.clone())))?;
            fc.emit(Op::Constant(idx));
        }

        hir::ExprKind::Nil => {
            fc.emit(Op::Nil);
        }

        hir::ExprKind::Unary { op, expr } => {
            compile_expr(fc, expr)?;
            match op {
                UnaryOp::Neg => fc.emit(Op::Negate),
                UnaryOp::Not => fc.emit(Op::Not),
                UnaryOp::BitNot => fc.emit(Op::BitNot),
            }
        }

        hir::ExprKind::Binary { op, lhs, rhs } => {
            compile_expr(fc, lhs)?;
            compile_expr(fc, rhs)?;
            let opcode = match op {
                BinaryOp::Add => Op::Add,
                BinaryOp::Sub => Op::Sub,
                BinaryOp::Mul => Op::Mul,
                BinaryOp::Div => Op::Div,
                BinaryOp::Rem => Op::Rem,
                BinaryOp::Eq => Op::Eq,
                BinaryOp::NotEq => Op::NotEq,
                BinaryOp::LessThan => Op::LessThan,
                BinaryOp::GreaterThan => Op::GreaterThan,
                BinaryOp::LessThanEq => Op::LessThanEq,
                BinaryOp::GreaterThanEq => Op::GreaterThanEq,
                BinaryOp::And => Op::And,
                BinaryOp::Or => Op::Or,
                BinaryOp::Xor => Op::Xor,
                BinaryOp::BitAnd => Op::BitAnd,
                BinaryOp::BitOr => Op::BitOr,
                BinaryOp::Shl => Op::Shl,
                BinaryOp::Shr => Op::Shr,
                BinaryOp::Coalesce => unreachable!("coalesce rejected during lowering"),
            };
            fc.emit(opcode);
        }

        hir::ExprKind::Call {
            func,
            args,
            ref_mask,
        } => {
            compile_ref_args(fc, args, ref_mask)?;
            fc.emit(Op::Call(func.0 as u16, args.len() as u8));
        }

        hir::ExprKind::CallBuiltin { builtin, args } => {
            for arg in args {
                compile_expr(fc, arg)?;
                if *builtin == Builtin::Println && arg.ty.is_optional() {
                    let type_name = arg
                        .ty
                        .option_inner()
                        .map(ToString::to_string)
                        .unwrap_or_default();
                    let idx = fc.add_constant(Value::String(ManagedRc::new(type_name)))?;
                    fc.emit(Op::OptionalToString(idx));
                }
            }
            let builtin_idx = Builtin::all()
                .iter()
                .position(|b| b == builtin)
                .expect("unknown builtin") as u8;
            fc.emit(Op::CallBuiltin(builtin_idx, args.len() as u8));
        }

        hir::ExprKind::CallExtern { extern_id, args } => {
            for arg in args {
                compile_expr(fc, arg)?;
            }
            fc.emit(Op::CallExtern(extern_id.0 as u16, args.len() as u8));
        }

        hir::ExprKind::StructLiteral { type_id, fields } => {
            for field in fields {
                compile_expr(fc, field)?;
            }
            fc.emit(Op::ConstructStruct(*type_id, fields.len() as u16));
        }

        hir::ExprKind::DataRefLiteral { type_id, fields } => {
            for field in fields {
                compile_expr(fc, field)?;
            }
            fc.emit(Op::ConstructDataRef(*type_id, fields.len() as u16));
        }

        hir::ExprKind::TupleLiteral { elements } => {
            for elem in elements {
                compile_expr(fc, elem)?;
            }
            fc.emit(Op::ConstructTuple(elements.len() as u16));
        }

        hir::ExprKind::FieldGet { object, index } => {
            compile_expr(fc, object)?;
            fc.emit(Op::GetField(*index));
        }

        hir::ExprKind::TupleIndex { tuple, index } => {
            compile_expr(fc, tuple)?;
            fc.emit(Op::GetField(*index));
        }

        hir::ExprKind::EnumLiteral {
            type_id,
            variant,
            fields,
        } => {
            for field in fields {
                compile_expr(fc, field)?;
            }
            fc.emit(Op::ConstructEnum(*type_id, *variant, fields.len() as u16));
        }

        hir::ExprKind::ArrayLiteral { elements } => {
            for elem in elements {
                compile_expr(fc, elem)?;
            }
            fc.emit(Op::ConstructArray(elements.len() as u16));
        }

        hir::ExprKind::ListLiteral { elements } => {
            for elem in elements {
                compile_expr(fc, elem)?;
            }
            fc.emit(Op::ConstructList(elements.len() as u16));
        }

        hir::ExprKind::ArrayFill { value, len } => {
            for _ in 0..*len {
                compile_expr(fc, value)?;
            }
            fc.emit(Op::ConstructArray(*len as u16));
        }

        hir::ExprKind::ListFill { value, len } => {
            for _ in 0..*len {
                compile_expr(fc, value)?;
            }
            fc.emit(Op::ConstructList(*len as u16));
        }

        hir::ExprKind::MapLiteral { entries } => {
            for (key, value) in entries {
                compile_expr(fc, key)?;
                compile_expr(fc, value)?;
            }
            fc.emit(Op::ConstructMap(entries.len() as u16));
        }

        hir::ExprKind::IndexGet { target, index } => {
            compile_expr(fc, target)?;
            compile_expr(fc, index)?;
            fc.emit(Op::IndexGet);
        }

        hir::ExprKind::Slice {
            target,
            start,
            end,
            inclusive,
        } => {
            compile_expr(fc, target)?;
            compile_expr(fc, start)?;
            compile_expr(fc, end)?;
            fc.emit(Op::Slice(*inclusive));
        }

        hir::ExprKind::CollectionLen { collection } => {
            compile_expr(fc, collection)?;
            fc.emit(Op::CollectionLen);
        }

        hir::ExprKind::MapLen { map } => {
            compile_expr(fc, map)?;
            fc.emit(Op::MapLen);
        }

        hir::ExprKind::MapEntryAt { map, index } => {
            compile_expr(fc, map)?;
            compile_expr(fc, index)?;
            fc.emit(Op::MapEntryAt);
        }

        hir::ExprKind::Cast(inner) => {
            compile_expr(fc, inner)?;
            let cast_kind = match (&inner.ty, &expr.ty) {
                (Type::Int, Type::Float) => CastKind::IntToFloat,
                (Type::Float, Type::Int) => CastKind::FloatToInt,
                (Type::Int, Type::Double) => CastKind::IntToDouble,
                (Type::Double, Type::Int) => CastKind::DoubleToInt,
                (Type::Float, Type::Double) => CastKind::FloatToDouble,
                (Type::Double, Type::Float) => CastKind::DoubleToFloat,
                _ => unreachable!("invalid cast pair — typechecker should have rejected this"),
            };
            fc.emit(Op::Cast(cast_kind));
        }

        hir::ExprKind::ToString(inner) => {
            compile_expr(fc, inner)?;
            if inner.ty.is_optional() {
                let type_name = inner
                    .ty
                    .option_inner()
                    .map(ToString::to_string)
                    .unwrap_or_default();
                let idx = fc.add_constant(Value::String(ManagedRc::new(type_name)))?;
                fc.emit(Op::OptionalToString(idx));
            } else {
                fc.emit(Op::ToString);
            }
        }

        hir::ExprKind::Format(inner, spec) => {
            compile_expr(fc, inner)?;
            fc.emit(Op::Format(*spec));
        }

        hir::ExprKind::CollectionMut {
            object,
            method,
            args,
        } => {
            fc.emit_load_local_or_deref(*object);
            for arg in args {
                compile_expr(fc, arg)?;
            }
            fc.emit(match method {
                hir::CollectionMethod::ListPush => Op::ListPush,
                hir::CollectionMethod::ListPop => Op::ListPop,
                hir::CollectionMethod::MapInsert => Op::MapInsert,
                hir::CollectionMethod::MapRemove => Op::MapRemove,
            });
            fc.emit_store_local_or_deref(*object);
        }

        hir::ExprKind::CreateClosure { func, captures } => {
            for capture in captures {
                compile_expr(fc, capture)?;
            }
            fc.emit(Op::CreateClosure(func.0 as u16, captures.len() as u8));
        }

        hir::ExprKind::CallClosure {
            callee,
            args,
            ref_mask,
        } => {
            compile_expr(fc, callee)?;
            compile_ref_args(fc, args, ref_mask)?;
            fc.emit(Op::CallClosure(args.len() as u8));
        }

        hir::ExprKind::SortBy {
            collection,
            comparator,
        } => {
            fc.emit_load_local_or_deref(*collection);
            fc.emit(Op::ListSortBy(comparator.0 as u16));
            fc.emit_store_local_or_deref(*collection);
        }

        hir::ExprKind::UnwrapOptional(inner) => {
            compile_expr(fc, inner)?;
            fc.emit(Op::UnwrapOptional);
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        Profile,
        ast::Type,
        hir::{Block, Expr, ExprKind, Func, FuncId, Local, LocalId, Program, StmtKind},
        test_helpers::{
            dummy_ident, dummy_span, hir_binary_expr as binary_expr, hir_bool_expr as bool_expr,
            hir_int_expr as int_expr, hir_local_expr as local_expr, hir_main_func as main_func,
            hir_program as prog, hir_simple_func as simple_func, hir_stmt as stmt,
        },
    };

    #[test]
    fn empty_main_emits_nil_return() {
        let compiled = compile(&prog(main_func(vec![])), Profile::default()).unwrap();
        assert_eq!(compiled.main_idx, 0);
        let chunk = &compiled.chunks[0];
        assert_eq!(chunk.code, vec![Op::Nil, Op::Return]);
        assert_eq!(chunk.constants.len(), 0);
        assert_eq!(chunk.local_count, 0);
    }

    #[test]
    fn let_local_emits_set_local() {
        let func = Func {
            id: FuncId(0),
            name: dummy_ident("main"),
            locals: vec![Local {
                name: Some(dummy_ident("x")),
                ty: Type::Int,
                is_ref: false,
            }],
            params_len: 0,
            ret: Type::Void,
            body: Block {
                stmts: vec![stmt(StmtKind::Let {
                    local: LocalId(0),
                    init: int_expr(42),
                })],
            },
            span: dummy_span(),
        };
        let compiled = compile(&prog(func), Profile::default()).unwrap();
        let chunk = &compiled.chunks[0];
        assert_eq!(
            chunk.code,
            vec![Op::Constant(0), Op::SetLocal(0), Op::Nil, Op::Return]
        );
        assert_eq!(chunk.constants, vec![Value::Int(42)]);
        assert_eq!(chunk.local_count, 1);
    }

    #[test]
    fn return_binary_add() {
        let func = Func {
            id: FuncId(0),
            name: dummy_ident("main"),
            locals: vec![],
            params_len: 0,
            ret: Type::Int,
            body: Block {
                stmts: vec![stmt(StmtKind::Return(Some(binary_expr(
                    BinaryOp::Add,
                    int_expr(1),
                    int_expr(2),
                ))))],
            },
            span: dummy_span(),
        };
        let compiled = compile(&prog(func), Profile::default()).unwrap();
        let chunk = &compiled.chunks[0];
        // return comes first from the explicit stmt, nil+return follows (unreachable)
        assert!(matches!(
            &chunk.code[..4],
            [Op::Constant(0), Op::Constant(1), Op::Add, Op::Return]
        ));
        assert_eq!(chunk.constants, vec![Value::Int(1), Value::Int(2)]);
    }

    #[test]
    fn bool_literals_use_true_false_ops() {
        let func = main_func(vec![
            stmt(StmtKind::Expr(bool_expr(true))),
            stmt(StmtKind::Expr(bool_expr(false))),
        ]);
        let compiled = compile(&prog(func), Profile::default()).unwrap();
        let chunk = &compiled.chunks[0];
        assert_eq!(chunk.code[0], Op::True);
        assert_eq!(chunk.code[2], Op::False);
    }

    #[test]
    fn if_without_else_emits_correct_jumps() {
        // if true { } (then_block is empty)
        let func = main_func(vec![stmt(StmtKind::If {
            cond: bool_expr(true),
            then_block: Block { stmts: vec![] },
            else_block: None,
        })]);
        let compiled = compile(&prog(func), Profile::default()).unwrap();
        let chunk = &compiled.chunks[0];
        // 0: True, 1: JumpIfFalse(?), 2: Nil, 3: Return
        assert_eq!(chunk.code[0], Op::True);
        assert!(matches!(chunk.code[1], Op::JumpIfFalse(_)));
        // JumpIfFalse should skip 0 instructions and land right after (at Nil)
        // offset = 2 - 1 - 1 = 0
        assert_eq!(chunk.code[1], Op::JumpIfFalse(0));
    }

    #[test]
    fn if_else_emits_skip_jump() {
        // if true { } else { }
        let func = main_func(vec![stmt(StmtKind::If {
            cond: bool_expr(true),
            then_block: Block { stmts: vec![] },
            else_block: Some(Block { stmts: vec![] }),
        })]);
        let compiled = compile(&prog(func), Profile::default()).unwrap();
        let chunk = &compiled.chunks[0];
        // 0: True, 1: JumpIfFalse(1), 2: Jump(0), 3: Nil, 4: Return
        assert_eq!(chunk.code[0], Op::True);
        assert_eq!(chunk.code[1], Op::JumpIfFalse(1));
        assert_eq!(chunk.code[2], Op::Jump(0));
    }

    #[test]
    fn while_false_emits_back_jump() {
        // while false {}
        let func = main_func(vec![stmt(StmtKind::While {
            cond: bool_expr(false),
            body: Block { stmts: vec![] },
        })]);
        let compiled = compile(&prog(func), Profile::default()).unwrap();
        let chunk = &compiled.chunks[0];
        // 0: False, 1: JumpIfFalse(1), 2: Jump(-3), 3: Nil, 4: Return
        assert_eq!(chunk.code[0], Op::False);
        assert!(matches!(chunk.code[1], Op::JumpIfFalse(_)));
        assert!(matches!(chunk.code[2], Op::Jump(n) if n < 0));
        // verify exit jump lands at position 3 (Nil)
        // offset for JumpIfFalse at pos 1: target = 3, offset = 3 - 1 - 1 = 1
        assert_eq!(chunk.code[1], Op::JumpIfFalse(1));
    }

    #[test]
    fn while_with_break_patches_correctly() {
        // while true { break; }
        let func = main_func(vec![stmt(StmtKind::While {
            cond: bool_expr(true),
            body: Block {
                stmts: vec![stmt(StmtKind::Break)],
            },
        })]);
        let compiled = compile(&prog(func), Profile::default()).unwrap();
        let chunk = &compiled.chunks[0];
        // 0: True, 1: JumpIfFalse(?), 2: Jump(?) [break], 3: Jump(-4) [back], 4: Nil, 5: Return
        assert_eq!(chunk.code[0], Op::True);
        assert!(matches!(chunk.code[1], Op::JumpIfFalse(_)));
        assert!(matches!(chunk.code[2], Op::Jump(_)));
        assert!(matches!(chunk.code[3], Op::Jump(n) if n < 0));
        // break at pos 2 jumps to after_loop = 4, offset = 4 - 2 - 1 = 1
        assert_eq!(chunk.code[2], Op::Jump(1));
        // exit jump at pos 1 also lands at 4, offset = 4 - 1 - 1 = 2
        assert_eq!(chunk.code[1], Op::JumpIfFalse(2));
    }

    #[test]
    fn get_local_roundtrip() {
        let func = Func {
            id: FuncId(0),
            name: dummy_ident("main"),
            locals: vec![Local {
                name: Some(dummy_ident("x")),
                ty: Type::Int,
                is_ref: false,
            }],
            params_len: 0,
            ret: Type::Void,
            body: Block {
                stmts: vec![
                    stmt(StmtKind::Let {
                        local: LocalId(0),
                        init: int_expr(7),
                    }),
                    stmt(StmtKind::Expr(local_expr(0))),
                ],
            },
            span: dummy_span(),
        };
        let compiled = compile(&prog(func), Profile::default()).unwrap();
        let chunk = &compiled.chunks[0];
        assert_eq!(chunk.code[0], Op::Constant(0));
        assert_eq!(chunk.code[1], Op::SetLocal(0));
        assert_eq!(chunk.code[2], Op::CloneLocal(0));
        assert_eq!(chunk.code[3], Op::Pop);
    }

    #[test]
    fn two_function_call_emits_call_op() {
        // fn helper() {} fn main() { helper(); }
        let helper = Func {
            id: FuncId(0),
            name: dummy_ident("helper"),
            locals: vec![],
            params_len: 0,
            ret: Type::Void,
            body: Block { stmts: vec![] },
            span: dummy_span(),
        };
        let main = Func {
            id: FuncId(1),
            name: dummy_ident("main"),
            locals: vec![],
            params_len: 0,
            ret: Type::Void,
            body: Block {
                stmts: vec![stmt(StmtKind::Expr(Expr::new(
                    Type::Void,
                    dummy_span(),
                    ExprKind::Call {
                        func: FuncId(0),
                        args: vec![],
                        ref_mask: vec![],
                    },
                )))],
            },
            span: dummy_span(),
        };
        let program = Program {
            funcs: vec![helper, main],
            externs: vec![],
            aggregate_meta: vec![],
            enum_meta: vec![],
        };
        let compiled = compile(&program, Profile::default()).unwrap();
        assert_eq!(compiled.main_idx, 1);
        let main_chunk = &compiled.chunks[1];
        assert_eq!(main_chunk.code[0], Op::Call(0, 0));
        assert_eq!(main_chunk.code[1], Op::Pop);
    }

    #[test]
    fn no_main_returns_error() {
        let program = Program {
            funcs: vec![simple_func("notmain", vec![], vec![], 0, Type::Void)],
            externs: vec![],
            aggregate_meta: vec![],
            enum_meta: vec![],
        };
        assert!(matches!(
            compile(&program, Profile::default()),
            Err(CompileError::NoMainFunction)
        ));
    }

    #[test]
    fn call_builtin_println_emits_call_builtin_op() {
        use crate::hir::ExprKind as EK;
        let func = main_func(vec![stmt(StmtKind::Expr(Expr::new(
            Type::Void,
            dummy_span(),
            EK::CallBuiltin {
                builtin: Builtin::Println,
                args: vec![Expr::new(
                    Type::String,
                    dummy_span(),
                    EK::String("hi".into()),
                )],
            },
        )))]);
        let compiled = compile(&prog(func), Profile::default()).unwrap();
        let chunk = &compiled.chunks[0];
        // Constant(0) [String "hi"], CallBuiltin(0, 1), Pop, Nil, Return
        assert_eq!(chunk.code[0], Op::Constant(0));
        assert_eq!(chunk.code[1], Op::CallBuiltin(0, 1));
        assert_eq!(chunk.code[2], Op::Pop);
    }

    #[test]
    fn struct_literal_emits_construct_struct() {
        use crate::hir::ExprKind as EK;
        let func = main_func(vec![stmt(StmtKind::Let {
            local: LocalId(0),
            init: Expr::new(
                Type::Int,
                dummy_span(),
                EK::StructLiteral {
                    type_id: 5,
                    fields: vec![int_expr(10), int_expr(20)],
                },
            ),
        })]);
        let compiled = compile(
            &prog(Func {
                locals: vec![Local {
                    name: None,
                    ty: Type::Int,
                    is_ref: false,
                }],
                ..func
            }),
            Profile::default(),
        )
        .unwrap();
        let chunk = &compiled.chunks[0];
        // Constant(0)[10], Constant(1)[20], ConstructStruct(5, 2), SetLocal(0), Nil, Return
        assert_eq!(chunk.code[0], Op::Constant(0));
        assert_eq!(chunk.code[1], Op::Constant(1));
        assert_eq!(chunk.code[2], Op::ConstructStruct(5, 2));
        assert_eq!(chunk.code[3], Op::SetLocal(0));
    }

    #[test]
    fn tuple_literal_emits_construct_tuple() {
        use crate::hir::ExprKind as EK;
        let func = main_func(vec![stmt(StmtKind::Let {
            local: LocalId(0),
            init: Expr::new(
                Type::Int,
                dummy_span(),
                EK::TupleLiteral {
                    elements: vec![int_expr(1), int_expr(2), int_expr(3)],
                },
            ),
        })]);
        let compiled = compile(
            &prog(Func {
                locals: vec![Local {
                    name: None,
                    ty: Type::Int,
                    is_ref: false,
                }],
                ..func
            }),
            Profile::default(),
        )
        .unwrap();
        let chunk = &compiled.chunks[0];
        assert_eq!(chunk.code[3], Op::ConstructTuple(3));
    }

    #[test]
    fn field_get_emits_get_field() {
        use crate::hir::ExprKind as EK;
        let func = main_func(vec![stmt(StmtKind::Expr(Expr::new(
            Type::Int,
            dummy_span(),
            EK::FieldGet {
                object: Box::new(int_expr(0)),
                index: 2,
            },
        )))]);
        let compiled = compile(&prog(func), Profile::default()).unwrap();
        let chunk = &compiled.chunks[0];
        // Constant(0), GetField(2), Pop, Nil, Return
        assert_eq!(chunk.code[1], Op::GetField(2));
    }

    #[test]
    fn unwrap_optional_emits_opcode() {
        use crate::hir::ExprKind as EK;
        let func = main_func(vec![stmt(StmtKind::Expr(Expr::new(
            Type::Int,
            dummy_span(),
            EK::UnwrapOptional(Box::new(int_expr(42))),
        )))]);
        let compiled = compile(&prog(func), Profile::default()).unwrap();
        let chunk = &compiled.chunks[0];
        assert!(
            chunk.code.contains(&Op::UnwrapOptional),
            "expected UnwrapOptional opcode"
        );
    }

    #[test]
    fn set_field_emits_correct_sequence() {
        use crate::hir::StmtKind as SK;
        let func = Func {
            id: FuncId(0),
            name: dummy_ident("main"),
            locals: vec![Local {
                name: None,
                ty: Type::Int,
                is_ref: false,
            }],
            params_len: 0,
            ret: Type::Void,
            body: Block {
                stmts: vec![stmt(SK::SetField {
                    object: LocalId(0),
                    field_index: 1,
                    value: int_expr(42),
                })],
            },
            span: dummy_span(),
        };
        let compiled = compile(&prog(func), Profile::default()).unwrap();
        let chunk = &compiled.chunks[0];
        // GetLocal(0), Constant(0)[42], SetField(1), SetLocal(0), Nil, Return
        assert_eq!(chunk.code[0], Op::GetLocal(0));
        assert_eq!(chunk.code[2], Op::SetField(1));
        assert_eq!(chunk.code[3], Op::SetLocal(0));
    }

    #[test]
    fn enum_literal_unit_emits_construct_enum() {
        use crate::hir::ExprKind as EK;
        let func = main_func(vec![stmt(StmtKind::Let {
            local: LocalId(0),
            init: Expr::new(
                Type::Int,
                dummy_span(),
                EK::EnumLiteral {
                    type_id: 3,
                    variant: 1,
                    fields: vec![],
                },
            ),
        })]);
        let compiled = compile(
            &prog(Func {
                locals: vec![Local {
                    name: None,
                    ty: Type::Int,
                    is_ref: false,
                }],
                ..func
            }),
            Profile::default(),
        )
        .unwrap();
        let chunk = &compiled.chunks[0];
        // ConstructEnum(3, 1, 0), SetLocal(0), Nil, Return
        assert_eq!(chunk.code[0], Op::ConstructEnum(3, 1, 0));
        assert_eq!(chunk.code[1], Op::SetLocal(0));
    }

    #[test]
    fn enum_literal_with_fields_emits_construct_enum() {
        use crate::hir::ExprKind as EK;
        let func = main_func(vec![stmt(StmtKind::Let {
            local: LocalId(0),
            init: Expr::new(
                Type::Int,
                dummy_span(),
                EK::EnumLiteral {
                    type_id: 2,
                    variant: 0,
                    fields: vec![int_expr(42)],
                },
            ),
        })]);
        let compiled = compile(
            &prog(Func {
                locals: vec![Local {
                    name: None,
                    ty: Type::Int,
                    is_ref: false,
                }],
                ..func
            }),
            Profile::default(),
        )
        .unwrap();
        let chunk = &compiled.chunks[0];
        // Constant(0)[42], ConstructEnum(2, 0, 1), SetLocal(0), Nil, Return
        assert_eq!(chunk.code[0], Op::Constant(0));
        assert_eq!(chunk.code[1], Op::ConstructEnum(2, 0, 1));
        assert_eq!(chunk.code[2], Op::SetLocal(0));
    }

    #[test]
    fn match_stmt_emits_get_enum_variant_and_eq() {
        use crate::hir::{ExprKind as EK, MatchArm, MatchElse};
        let func = main_func(vec![
            stmt(StmtKind::Let {
                local: LocalId(0),
                init: Expr::new(
                    Type::Int,
                    dummy_span(),
                    EK::EnumLiteral {
                        type_id: 0,
                        variant: 0,
                        fields: vec![],
                    },
                ),
            }),
            stmt(StmtKind::Match {
                scrutinee_init: Box::new(Expr::new(Type::Int, dummy_span(), EK::Local(LocalId(0)))),
                scrutinee: LocalId(1),
                write_through: None,
                arms: vec![MatchArm {
                    variant: 0,
                    bindings: vec![],
                    guard: None,
                    body: Block { stmts: vec![] },
                }],
                else_body: Some(MatchElse {
                    binding: None,
                    body: Block { stmts: vec![] },
                }),
            }),
        ]);
        let compiled = compile(
            &prog(Func {
                locals: vec![
                    Local {
                        name: None,
                        ty: Type::Int,
                        is_ref: false,
                    },
                    Local {
                        name: None,
                        ty: Type::Int,
                        is_ref: false,
                    },
                ],
                ..func
            }),
            Profile::default(),
        )
        .unwrap();
        let chunk = &compiled.chunks[0];
        // After SetLocal(0) for scrutinee_init:
        // GetLocal(1), GetEnumVariant, Constant(idx), Eq, JumpIfFalse, Jump(end), [else], Return
        let has_get_enum_variant = chunk.code.contains(&Op::GetEnumVariant);
        let has_eq = chunk.code.contains(&Op::Eq);
        assert!(has_get_enum_variant, "expected GetEnumVariant opcode");
        assert!(has_eq, "expected Eq opcode");
    }

    #[test]
    fn array_literal_emits_construct_array() {
        let func = Func {
            id: FuncId(0),
            name: dummy_ident("main"),
            locals: vec![Local {
                name: None,
                ty: Type::Int,
                is_ref: false,
            }],
            params_len: 0,
            ret: Type::Void,
            body: Block {
                stmts: vec![stmt(StmtKind::Let {
                    local: LocalId(0),
                    init: Expr::new(
                        Type::Int,
                        dummy_span(),
                        ExprKind::ArrayLiteral {
                            elements: vec![int_expr(1), int_expr(2)],
                        },
                    ),
                })],
            },
            span: dummy_span(),
        };
        let compiled = compile(&prog(func), Profile::default()).unwrap();
        let chunk = &compiled.chunks[0];
        assert_eq!(
            &chunk.code[..4],
            &[
                Op::Constant(0),
                Op::Constant(1),
                Op::ConstructArray(2),
                Op::SetLocal(0)
            ]
        );
    }

    #[test]
    fn list_literal_emits_construct_list() {
        let func = Func {
            id: FuncId(0),
            name: dummy_ident("main"),
            locals: vec![Local {
                name: None,
                ty: Type::Int,
                is_ref: false,
            }],
            params_len: 0,
            ret: Type::Void,
            body: Block {
                stmts: vec![stmt(StmtKind::Let {
                    local: LocalId(0),
                    init: Expr::new(
                        Type::Int,
                        dummy_span(),
                        ExprKind::ListLiteral {
                            elements: vec![int_expr(1), int_expr(2)],
                        },
                    ),
                })],
            },
            span: dummy_span(),
        };
        let compiled = compile(&prog(func), Profile::default()).unwrap();
        let chunk = &compiled.chunks[0];
        assert_eq!(
            &chunk.code[..4],
            &[
                Op::Constant(0),
                Op::Constant(1),
                Op::ConstructList(2),
                Op::SetLocal(0)
            ]
        );
    }

    #[test]
    fn array_fill_emits_repeated_values() {
        let func = Func {
            id: FuncId(0),
            name: dummy_ident("main"),
            locals: vec![Local {
                name: None,
                ty: Type::Int,
                is_ref: false,
            }],
            params_len: 0,
            ret: Type::Void,
            body: Block {
                stmts: vec![stmt(StmtKind::Let {
                    local: LocalId(0),
                    init: Expr::new(
                        Type::Int,
                        dummy_span(),
                        ExprKind::ArrayFill {
                            value: Box::new(int_expr(7)),
                            len: 3,
                        },
                    ),
                })],
            },
            span: dummy_span(),
        };
        let compiled = compile(&prog(func), Profile::default()).unwrap();
        let chunk = &compiled.chunks[0];
        // each compile_expr call adds a new constant slot, so indices are 0, 1, 2
        assert_eq!(
            &chunk.code[..5],
            &[
                Op::Constant(0),
                Op::Constant(1),
                Op::Constant(2),
                Op::ConstructArray(3),
                Op::SetLocal(0),
            ]
        );
        assert_eq!(
            chunk.constants,
            vec![Value::Int(7), Value::Int(7), Value::Int(7)]
        );
    }

    #[test]
    fn index_get_emits_ops() {
        let func = Func {
            id: FuncId(0),
            name: dummy_ident("main"),
            locals: vec![Local {
                name: None,
                ty: Type::Int,
                is_ref: false,
            }],
            params_len: 0,
            ret: Type::Void,
            body: Block {
                stmts: vec![stmt(StmtKind::Expr(Expr::new(
                    Type::Int,
                    dummy_span(),
                    ExprKind::IndexGet {
                        target: Box::new(local_expr(0)),
                        index: Box::new(int_expr(1)),
                    },
                )))],
            },
            span: dummy_span(),
        };
        let compiled = compile(&prog(func), Profile::default()).unwrap();
        let chunk = &compiled.chunks[0];
        let has_get_local = chunk.code.contains(&Op::CloneLocal(0));
        let has_index_get = chunk.code.contains(&Op::IndexGet);
        assert!(has_get_local, "expected CloneLocal(0)");
        assert!(has_index_get, "expected IndexGet");
    }

    #[test]
    fn set_index_emits_ops() {
        let func = Func {
            id: FuncId(0),
            name: dummy_ident("main"),
            locals: vec![Local {
                name: None,
                ty: Type::Int,
                is_ref: false,
            }],
            params_len: 0,
            ret: Type::Void,
            body: Block {
                stmts: vec![stmt(StmtKind::SetIndex {
                    object: LocalId(0),
                    index: Box::new(int_expr(1)),
                    value: int_expr(99),
                })],
            },
            span: dummy_span(),
        };
        let compiled = compile(&prog(func), Profile::default()).unwrap();
        let chunk = &compiled.chunks[0];
        let has_get_local = chunk.code.contains(&Op::GetLocal(0));
        let has_index_set = chunk.code.contains(&Op::IndexSet);
        let has_set_local = chunk.code.contains(&Op::SetLocal(0));
        assert!(has_get_local, "expected GetLocal(0)");
        assert!(has_index_set, "expected IndexSet");
        assert!(has_set_local, "expected SetLocal(0)");
    }

    #[test]
    fn map_literal_emits_construct_map() {
        let func = Func {
            id: FuncId(0),
            name: dummy_ident("main"),
            locals: vec![Local {
                name: None,
                ty: Type::Int,
                is_ref: false,
            }],
            params_len: 0,
            ret: Type::Void,
            body: Block {
                stmts: vec![stmt(StmtKind::Let {
                    local: LocalId(0),
                    init: Expr::new(
                        Type::Int,
                        dummy_span(),
                        ExprKind::MapLiteral {
                            entries: vec![(int_expr(1), bool_expr(true))],
                        },
                    ),
                })],
            },
            span: dummy_span(),
        };
        let compiled = compile(&prog(func), Profile::default()).unwrap();
        let chunk = &compiled.chunks[0];
        assert_eq!(
            &chunk.code[..4],
            &[
                Op::Constant(0),
                Op::True,
                Op::ConstructMap(1),
                Op::SetLocal(0)
            ]
        );
    }

    #[test]
    fn empty_map_literal_emits_construct_map_zero() {
        let func = Func {
            id: FuncId(0),
            name: dummy_ident("main"),
            locals: vec![Local {
                name: None,
                ty: Type::Int,
                is_ref: false,
            }],
            params_len: 0,
            ret: Type::Void,
            body: Block {
                stmts: vec![stmt(StmtKind::Let {
                    local: LocalId(0),
                    init: Expr::new(
                        Type::Int,
                        dummy_span(),
                        ExprKind::MapLiteral { entries: vec![] },
                    ),
                })],
            },
            span: dummy_span(),
        };
        let compiled = compile(&prog(func), Profile::default()).unwrap();
        let chunk = &compiled.chunks[0];
        assert_eq!(&chunk.code[..2], &[Op::ConstructMap(0), Op::SetLocal(0)]);
    }

    fn local_expr_with_ownership(id: u32, ownership: hir::Ownership) -> Expr {
        let mut e = local_expr(id);
        e.ownership = ownership;
        e
    }

    #[test]
    fn local_move_emits_move_local() {
        let func = Func {
            id: FuncId(0),
            name: dummy_ident("main"),
            locals: vec![Local {
                name: None,
                ty: Type::Int,
                is_ref: false,
            }],
            params_len: 0,
            ret: Type::Void,
            body: Block {
                stmts: vec![stmt(StmtKind::Expr(local_expr_with_ownership(
                    0,
                    hir::Ownership::Move,
                )))],
            },
            span: dummy_span(),
        };
        let compiled = compile(&prog(func), Profile::default()).unwrap();
        let chunk = &compiled.chunks[0];
        assert_eq!(chunk.code[0], Op::MoveLocal(0));
    }

    #[test]
    fn local_borrow_emits_get_local() {
        let func = Func {
            id: FuncId(0),
            name: dummy_ident("main"),
            locals: vec![Local {
                name: None,
                ty: Type::Int,
                is_ref: false,
            }],
            params_len: 0,
            ret: Type::Void,
            body: Block {
                stmts: vec![stmt(StmtKind::Expr(local_expr_with_ownership(
                    0,
                    hir::Ownership::Borrow,
                )))],
            },
            span: dummy_span(),
        };
        let compiled = compile(&prog(func), Profile::default()).unwrap();
        let chunk = &compiled.chunks[0];
        assert_eq!(chunk.code[0], Op::GetLocal(0));
    }

    #[test]
    fn local_own_emits_clone_local() {
        let func = Func {
            id: FuncId(0),
            name: dummy_ident("main"),
            locals: vec![Local {
                name: None,
                ty: Type::Int,
                is_ref: false,
            }],
            params_len: 0,
            ret: Type::Void,
            body: Block {
                stmts: vec![stmt(StmtKind::Expr(local_expr_with_ownership(
                    0,
                    hir::Ownership::Own,
                )))],
            },
            span: dummy_span(),
        };
        let compiled = compile(&prog(func), Profile::default()).unwrap();
        let chunk = &compiled.chunks[0];
        assert_eq!(chunk.code[0], Op::CloneLocal(0));
    }

    #[test]
    fn return_move_local_emits_move_then_return() {
        let func = Func {
            id: FuncId(0),
            name: dummy_ident("main"),
            locals: vec![Local {
                name: None,
                ty: Type::Int,
                is_ref: false,
            }],
            params_len: 0,
            ret: Type::Int,
            body: Block {
                stmts: vec![stmt(StmtKind::Return(Some(local_expr_with_ownership(
                    0,
                    hir::Ownership::Move,
                ))))],
            },
            span: dummy_span(),
        };
        let compiled = compile(&prog(func), Profile::default()).unwrap();
        let chunk = &compiled.chunks[0];
        assert_eq!(chunk.code[0], Op::MoveLocal(0));
        assert_eq!(chunk.code[1], Op::Return);
    }
}
