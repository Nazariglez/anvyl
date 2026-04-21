use std::fmt::Write;

use super::{
    builtins,
    bytecode::{CastKind, Op},
    callback::{VmWithRegistry, callback_scope},
    compiler::CompiledProgram,
    managed_rc::ManagedRc,
    value::{
        ClosureData, EnumData, MapStorage, RefPath, RuntimeError, StructData, Value, type_name,
        value_add, value_and, value_bit_and, value_bit_not, value_bit_or, value_div, value_eq,
        value_gt, value_gte, value_lt, value_lte, value_mul, value_negate, value_neq, value_not,
        value_or, value_rem, value_shl, value_shr, value_sub, value_xor,
    },
};
use crate::{
    Profile,
    ast::{FormatKind, FormatSpec},
    builtin::Builtin,
    ir_meta::VariantShape,
    prelude_enums::OPTION_TYPE_ID,
};

pub type ExternHandler = Box<dyn Fn(Vec<Value>) -> Result<Value, RuntimeError>>;

pub struct ExternRegistry {
    handlers: Vec<Option<ExternHandler>>,
}

impl ExternRegistry {
    pub fn new(count: usize) -> Self {
        Self {
            handlers: (0..count).map(|_| None).collect(),
        }
    }

    pub fn register(&mut self, index: usize, handler: ExternHandler) {
        if index < self.handlers.len() {
            self.handlers[index] = Some(handler);
        }
    }

    pub fn get(&self, index: usize) -> Option<&ExternHandler> {
        self.handlers.get(index).and_then(|slot| slot.as_ref())
    }
}

fn as_usize_index(val: &Value) -> Result<usize, RuntimeError> {
    match val {
        Value::Int(i) if *i < 0 => Err(RuntimeError::new(format!("negative index {i}"))),
        Value::Int(i) => Ok(*i as usize),
        other => Err(RuntimeError::new(format!("index must be int, got {other}"))),
    }
}

fn set_index_on_slot(
    slot: &mut Value,
    index_val: Value,
    new_value: Value,
) -> Result<(), RuntimeError> {
    match slot {
        Value::Array(a) => {
            let idx = as_usize_index(&index_val)?;
            let len = a.len();
            if idx >= len {
                return Err(RuntimeError::new(format!(
                    "index {idx} out of bounds for array of length {len}"
                )));
            }
            ManagedRc::make_mut(a)[idx] = new_value;
        }
        Value::List(l) => {
            let idx = as_usize_index(&index_val)?;
            let len = l.len();
            if idx >= len {
                return Err(RuntimeError::new(format!(
                    "index {idx} out of bounds for list of length {len}"
                )));
            }
            ManagedRc::make_mut(l)[idx] = new_value;
        }
        Value::Map(m) => {
            ManagedRc::make_mut(m).insert(index_val, new_value);
        }
        other => {
            return Err(RuntimeError::new(format!(
                "SetIndexRef on non-indexable value: {other}"
            )));
        }
    }
    Ok(())
}

fn resolve_path_ref<'a>(slot: &'a Value, segments: &[u16]) -> &'a Value {
    let mut current = slot;
    for &seg in segments {
        current = match current {
            Value::Struct(s) | Value::DataRef(s) => &s.fields[seg as usize],
            Value::Tuple(t) => &t[seg as usize],
            Value::Enum(e) => &e.fields[seg as usize],
            _ => panic!("resolve_path_ref: cannot index into {}", type_name(current)),
        };
    }
    current
}

fn resolve_path_mut<'a>(slot: &'a mut Value, segments: &[u16]) -> &'a mut Value {
    if segments.is_empty() {
        return slot;
    }
    let seg = segments[0] as usize;
    let rest = &segments[1..];
    match slot {
        Value::Struct(s) => resolve_path_mut(&mut ManagedRc::make_mut(s).fields[seg], rest),
        Value::DataRef(s) => resolve_path_mut(&mut s.force_mut().fields[seg], rest),
        Value::Tuple(t) => resolve_path_mut(&mut ManagedRc::make_mut(t)[seg], rest),
        Value::Enum(e) => resolve_path_mut(&mut ManagedRc::make_mut(e).fields[seg], rest),
        _ => panic!("resolve_path_mut: cannot index into {}", type_name(slot)),
    }
}

struct CallFrame {
    chunk_idx: usize,
    ip: usize,
    stack_base: usize,
}

pub struct VM<'a> {
    program: &'a CompiledProgram,
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    pub stdout: String,
    pub profile: Profile,
}

impl<'a> VM<'a> {
    pub fn new(program: &'a CompiledProgram) -> Self {
        Self {
            program,
            stack: vec![],
            frames: vec![],
            stdout: String::new(),
            profile: program.profile,
        }
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("stack underflow")
    }

    pub fn run(&mut self, registry: &ExternRegistry) -> Result<(), RuntimeError> {
        // bootstrap the main frame
        let main_idx = self.program.main_idx;
        let main_local_count = self.program.chunks[main_idx].local_count as usize;

        self.frames.push(CallFrame {
            chunk_idx: main_idx,
            ip: 0,
            stack_base: 0,
        });
        self.stack.resize(main_local_count, Value::Nil);

        self.run_until_depth(registry, 0).map(|_| ())
    }

    fn run_until_depth(
        &mut self,
        registry: &ExternRegistry,
        stop_depth: usize,
    ) -> Result<Value, RuntimeError> {
        loop {
            // fetch current instruction and advance IP atomically
            let (chunk_idx, ip, stack_base) = {
                let frame = self.frames.last_mut().expect("no active frame");
                let result = (frame.chunk_idx, frame.ip, frame.stack_base);
                frame.ip += 1;
                result
            };

            let op = self.program.chunks[chunk_idx].code[ip].clone();

            match op {
                Op::Constant(idx) => {
                    let val = self.program.chunks[chunk_idx].constants[idx as usize].clone();
                    self.push(val);
                }

                Op::True => self.push(Value::Bool(true)),
                Op::False => self.push(Value::Bool(false)),
                Op::Nil => self.push(Value::Nil),

                Op::Pop => {
                    self.pop();
                }

                Op::GetLocal(idx) | Op::CloneLocal(idx) => {
                    let val = self.stack[stack_base + idx as usize].clone();
                    self.push(val);
                }

                Op::MoveLocal(idx) => {
                    let slot = &mut self.stack[stack_base + idx as usize];
                    let val = std::mem::replace(slot, Value::Nil);
                    self.push(val);
                }

                Op::PushRef(idx) => {
                    let abs_idx = (stack_base + idx as usize) as u32;
                    self.push(Value::StackRef(abs_idx));
                }

                Op::PushPathRef(local_idx, depth, segments) => {
                    let slot = &self.stack[stack_base + local_idx as usize];
                    match slot {
                        Value::StackRef(abs) => {
                            let abs = *abs;
                            self.push(Value::PathRef(RefPath::new(abs, depth, segments)));
                        }
                        Value::PathRef(rp) => {
                            let rp = *rp;
                            let new_depth = rp.depth + depth;
                            assert!(new_depth <= 4, "PathRef depth overflow in PushPathRef");
                            let mut new_segments = rp.segments;
                            for i in 0..depth as usize {
                                new_segments[rp.depth as usize + i] = segments[i];
                            }
                            self.push(Value::PathRef(RefPath::new(
                                rp.root,
                                new_depth,
                                new_segments,
                            )));
                        }
                        _ => {
                            let abs_root = (stack_base + local_idx as usize) as u32;
                            self.push(Value::PathRef(RefPath::new(abs_root, depth, segments)));
                        }
                    }
                }

                Op::DerefRead(idx) => {
                    let ref_val = &self.stack[stack_base + idx as usize];
                    match ref_val {
                        Value::StackRef(abs_idx) => {
                            let val = self.stack[*abs_idx as usize].clone();
                            self.push(val);
                        }
                        Value::PathRef(rp) => {
                            let rp = *rp;
                            let val =
                                resolve_path_ref(&self.stack[rp.root as usize], rp.path()).clone();
                            self.push(val);
                        }
                        _ => panic!("DerefRead on non-ref local"),
                    }
                }

                Op::DerefWrite(idx) => {
                    let val = self.pop();
                    let ref_val = &self.stack[stack_base + idx as usize];
                    match ref_val {
                        Value::StackRef(abs_idx) => {
                            let abs = *abs_idx as usize;
                            self.stack[abs] = val;
                        }
                        Value::PathRef(rp) => {
                            let rp = *rp;
                            let target =
                                resolve_path_mut(&mut self.stack[rp.root as usize], rp.path());
                            *target = val;
                        }
                        _ => panic!("DerefWrite on non-ref local"),
                    }
                }

                Op::SetFieldRef(local_idx, field_idx) => {
                    let new_value = self.pop();
                    let ref_val = &self.stack[stack_base + local_idx as usize];
                    match ref_val {
                        Value::StackRef(abs_idx) => {
                            let abs = *abs_idx as usize;
                            let slot = &mut self.stack[abs];
                            match slot {
                                Value::Struct(s) => {
                                    ManagedRc::make_mut(s).fields[field_idx as usize] = new_value;
                                }
                                Value::DataRef(s) => {
                                    s.force_mut().fields[field_idx as usize] = new_value;
                                }
                                Value::Tuple(t) => {
                                    ManagedRc::make_mut(t)[field_idx as usize] = new_value;
                                }
                                Value::Enum(e) => {
                                    ManagedRc::make_mut(e).fields[field_idx as usize] = new_value;
                                }
                                // flat nullable representation where field 0 is the value itself,
                                // mirroring GetField's "other -> other" fallback
                                other => {
                                    if field_idx == 0 {
                                        *other = new_value;
                                    } else {
                                        return Err(RuntimeError::new(format!(
                                            "SetFieldRef on unsupported type: {other}"
                                        )));
                                    }
                                }
                            }
                        }
                        Value::PathRef(rp) => {
                            let mut rp = *rp;
                            // extend the path with field_idx and navigate to the target
                            assert!((rp.depth as usize) < 4, "PathRef depth overflow");
                            rp.segments[rp.depth as usize] = field_idx;
                            rp.depth += 1;
                            let target =
                                resolve_path_mut(&mut self.stack[rp.root as usize], rp.path());
                            *target = new_value;
                        }
                        _ => panic!("SetFieldRef on non-ref local"),
                    }
                }

                Op::SetIndexRef(local_idx) => {
                    let new_value = self.pop();
                    let index_val = self.pop();
                    let ref_val = &self.stack[stack_base + local_idx as usize];
                    match ref_val {
                        Value::StackRef(abs_idx) => {
                            let abs = *abs_idx as usize;
                            set_index_on_slot(&mut self.stack[abs], index_val, new_value)?;
                        }
                        Value::PathRef(rp) => {
                            let rp = *rp;
                            let slot =
                                resolve_path_mut(&mut self.stack[rp.root as usize], rp.path());
                            set_index_on_slot(slot, index_val, new_value)?;
                        }
                        _ => panic!("SetIndexRef on non-ref local"),
                    }
                }

                Op::SetLocal(idx) => {
                    let val = self.pop();
                    let old = std::mem::replace(&mut self.stack[stack_base + idx as usize], val);
                    drop(old);
                }

                Op::Negate => {
                    let v = self.pop();
                    self.push(value_negate(v)?);
                }

                Op::Not => {
                    let v = self.pop();
                    self.push(value_not(v)?);
                }

                Op::BitNot => {
                    let v = self.pop();
                    self.push(value_bit_not(v)?);
                }

                Op::Add => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(value_add(lhs, rhs)?);
                }

                Op::Sub => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(value_sub(lhs, rhs)?);
                }

                Op::Mul => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(value_mul(lhs, rhs)?);
                }

                Op::Div => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(value_div(lhs, rhs)?);
                }

                Op::Rem => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(value_rem(lhs, rhs)?);
                }

                Op::Eq => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(value_eq(&lhs, &rhs));
                }

                Op::NotEq => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(value_neq(&lhs, &rhs));
                }

                Op::LessThan => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(value_lt(lhs, rhs)?);
                }

                Op::GreaterThan => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(value_gt(lhs, rhs)?);
                }

                Op::LessThanEq => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(value_lte(lhs, rhs)?);
                }

                Op::GreaterThanEq => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(value_gte(lhs, rhs)?);
                }

                Op::And => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(value_and(lhs, rhs)?);
                }

                Op::Or => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(value_or(lhs, rhs)?);
                }

                Op::Xor => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(value_xor(lhs, rhs)?);
                }

                Op::BitAnd => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(value_bit_and(lhs, rhs)?);
                }

                Op::BitOr => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(value_bit_or(lhs, rhs)?);
                }

                Op::Shl => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(value_shl(lhs, rhs)?);
                }

                Op::Shr => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(value_shr(lhs, rhs)?);
                }

                Op::Jump(offset) => {
                    let frame = self.frames.last_mut().expect("no active frame");
                    frame.ip = (frame.ip as isize + offset as isize) as usize;
                }

                Op::JumpIfFalse(offset) => {
                    let cond = self.pop();
                    let is_falsy = matches!(cond, Value::Bool(false) | Value::Nil);
                    if is_falsy {
                        let frame = self.frames.last_mut().expect("no active frame");
                        frame.ip = (frame.ip as isize + offset as isize) as usize;
                    }
                }

                Op::Call(callee_chunk_idx, arg_count) => {
                    let callee_idx = callee_chunk_idx as usize;
                    let (local_count, params_count) = {
                        let callee = &self.program.chunks[callee_idx];
                        (callee.local_count as usize, callee.params_count as usize)
                    };

                    let new_stack_base = self.stack.len() - arg_count as usize;
                    self.frames.push(CallFrame {
                        chunk_idx: callee_idx,
                        ip: 0,
                        stack_base: new_stack_base,
                    });

                    let extra_locals = local_count - params_count;
                    self.stack
                        .resize(self.stack.len() + extra_locals, Value::Nil);
                }

                Op::CallBuiltin(builtin_idx, arg_count) => {
                    let n = arg_count as usize;
                    let args: Vec<Value> = self.stack.drain(self.stack.len() - n..).collect();
                    let builtin = Builtin::all()[builtin_idx as usize];
                    let result = if builtin == Builtin::Println {
                        let s = self.format_value(registry, &args[0])?;
                        writeln!(self.stdout, "{s}").unwrap();
                        Value::Nil
                    } else {
                        builtins::call_builtin(builtin, &args, &mut self.stdout)?
                    };
                    self.push(result);
                }

                Op::CallExtern(extern_idx, arg_count) => {
                    let n = arg_count as usize;
                    let args: Vec<Value> = self.stack.drain(self.stack.len() - n..).collect();
                    let idx = extern_idx as usize;
                    let handler = registry.get(idx).ok_or_else(|| {
                        let name = self
                            .program
                            .extern_names
                            .get(idx)
                            .map_or("<unknown>", String::as_str);
                        RuntimeError::new(format!("missing extern implementation for '{name}'"))
                    })?;
                    let result = {
                        let mut ctx = VmWithRegistry { vm: self, registry };
                        let _guard = callback_scope(&mut ctx);
                        handler(args)?
                    };
                    self.push(result);
                }

                Op::CreateClosure(fn_id, capture_count) => {
                    let count = capture_count as usize;
                    let start = self.stack.len() - count;
                    let captures: Vec<Value> = self.stack.drain(start..).collect();
                    let data = ClosureData { fn_id, captures };
                    self.push(Value::Closure(ManagedRc::new(data)));
                }

                Op::CallClosure(arg_count) => {
                    let n_args = arg_count as usize;
                    let args_start = self.stack.len() - n_args;
                    let args: Vec<Value> = self.stack.drain(args_start..).collect();
                    let closure_val = self.pop();
                    let closure = match closure_val {
                        Value::Closure(c) => c,
                        other => {
                            return Err(RuntimeError::new(format!(
                                "type error: cannot call {}",
                                type_name(&other)
                            )));
                        }
                    };
                    self.setup_closure_frame(&closure, args);
                }

                Op::Return => {
                    let return_val = self.pop();
                    let frame = self.frames.pop().expect("no frame to return from");

                    // discard all callee locals and temporaries
                    self.stack.truncate(frame.stack_base);

                    if self.frames.len() == stop_depth {
                        return Ok(return_val);
                    }

                    // push return value for the caller
                    self.push(return_val);
                }

                Op::ConstructStruct(type_id, field_count) => {
                    let count = field_count as usize;
                    let start = self.stack.len() - count;
                    let fields: Vec<Value> = self.stack.drain(start..).collect();
                    let data = StructData { type_id, fields };
                    self.push(Value::Struct(ManagedRc::new(data)));
                }

                Op::ConstructDataRef(type_id, field_count) => {
                    let count = field_count as usize;
                    let start = self.stack.len() - count;
                    let fields: Vec<Value> = self.stack.drain(start..).collect();
                    let data = StructData { type_id, fields };
                    let vtable = self.program.aggregate_vtables[type_id as usize].unwrap();
                    self.push(Value::DataRef(ManagedRc::new_with_vtable(data, vtable)));
                }

                Op::ConstructTuple(count) => {
                    let count = count as usize;
                    let start = self.stack.len() - count;
                    let elements: Vec<Value> = self.stack.drain(start..).collect();
                    self.push(Value::Tuple(ManagedRc::new(elements)));
                }

                Op::ConstructEnum(type_id, variant, field_count) => {
                    let count = field_count as usize;
                    let start = self.stack.len() - count;
                    let fields: Vec<Value> = self.stack.drain(start..).collect();
                    let data = EnumData {
                        type_id,
                        variant,
                        fields,
                    };
                    self.push(Value::Enum(ManagedRc::new(data)));
                }

                Op::GetEnumVariant => {
                    let val = self.pop();
                    match val {
                        Value::Enum(e) => self.push(Value::Int(i64::from(e.variant))),
                        Value::Nil => self.push(Value::Int(0)),
                        _ => self.push(Value::Int(1)),
                    }
                }

                Op::UnwrapOptional => {
                    let val = self.pop();
                    match val {
                        Value::Enum(e) if e.fields.len() == 1 => {
                            self.push(e.fields[0].clone());
                        }
                        other => self.push(other),
                    }
                }

                Op::GetField(index) => {
                    let obj = self.pop();
                    let idx = index as usize;
                    let field_val = match obj {
                        Value::Struct(s) => s.fields.get(idx).cloned().ok_or_else(|| {
                            RuntimeError::new(format!("field index {idx} out of bounds on struct"))
                        })?,
                        Value::DataRef(s) => s.fields.get(idx).cloned().ok_or_else(|| {
                            RuntimeError::new(format!("field index {idx} out of bounds on dataref"))
                        })?,
                        Value::Tuple(t) => t.get(idx).cloned().ok_or_else(|| {
                            RuntimeError::new(format!("index {idx} out of bounds on tuple"))
                        })?,
                        Value::Enum(e) => e.fields.get(idx).cloned().ok_or_else(|| {
                            RuntimeError::new(format!(
                                "field index {idx} out of bounds on enum variant"
                            ))
                        })?,
                        other => other,
                    };
                    self.push(field_val);
                }

                Op::SetField(index) => {
                    let new_value = self.pop();
                    let obj = self.pop();
                    let idx = index as usize;
                    match obj {
                        Value::Struct(mut s) => {
                            ManagedRc::make_mut(&mut s).fields[idx] = new_value;
                            self.push(Value::Struct(s));
                        }
                        Value::DataRef(mut s) => {
                            // shared state is intentional
                            s.force_mut().fields[idx] = new_value;
                            self.push(Value::DataRef(s));
                        }
                        other => {
                            return Err(RuntimeError::new(format!(
                                "SetField on non-struct value: {other}"
                            )));
                        }
                    }
                }

                Op::ConstructArray(count) => {
                    let count = count as usize;
                    let start = self.stack.len() - count;
                    let elements: Vec<Value> = self.stack.drain(start..).collect();
                    self.push(Value::Array(ManagedRc::new(elements)));
                }

                Op::ConstructList(count) => {
                    let count = count as usize;
                    let start = self.stack.len() - count;
                    let elements: Vec<Value> = self.stack.drain(start..).collect();
                    self.push(Value::List(ManagedRc::new(elements)));
                }

                Op::ConstructMap(count) => {
                    let count = count as usize;
                    let total = count * 2;
                    let start = self.stack.len() - total;
                    let flat: Vec<Value> = self.stack.drain(start..).collect();
                    let mut storage = MapStorage::with_capacity_unordered(count);
                    for pair in flat.chunks_exact(2) {
                        let key = pair[0].clone();
                        let value = pair[1].clone();
                        storage.insert(key, value);
                    }
                    self.push(Value::Map(ManagedRc::new(storage)));
                }

                Op::IndexGet => {
                    let index_val = self.pop();
                    let collection = self.pop();

                    match collection {
                        Value::Map(m) => {
                            let element = m.get(&index_val).cloned().unwrap_or(Value::Nil);
                            self.push(element);
                        }
                        Value::Array(a) => {
                            let idx = as_usize_index(&index_val)?;
                            let element = a.get(idx).cloned().ok_or_else(|| {
                                RuntimeError::new(format!(
                                    "index {idx} out of bounds for array of length {}",
                                    a.len()
                                ))
                            })?;
                            self.push(element);
                        }
                        Value::List(l) => {
                            let idx = as_usize_index(&index_val)?;
                            let element = l.get(idx).cloned().ok_or_else(|| {
                                RuntimeError::new(format!(
                                    "index {idx} out of bounds for list of length {}",
                                    l.len()
                                ))
                            })?;
                            self.push(element);
                        }
                        Value::Slice { data, offset, len } => {
                            let idx = as_usize_index(&index_val)?;
                            if idx >= len {
                                return Err(RuntimeError::new(format!(
                                    "index {idx} out of bounds for slice of length {len}"
                                )));
                            }
                            let element = data[offset + idx].clone();
                            self.push(element);
                        }
                        other => {
                            return Err(RuntimeError::new(format!(
                                "IndexGet on non-indexable value: {other}"
                            )));
                        }
                    }
                }

                Op::IndexSet => {
                    let new_value = self.pop();
                    let index_val = self.pop();
                    let collection = self.pop();

                    match collection {
                        Value::Map(mut m) => {
                            ManagedRc::make_mut(&mut m).insert(index_val, new_value);
                            self.push(Value::Map(m));
                        }
                        Value::Array(mut a) => {
                            let idx = as_usize_index(&index_val)?;
                            let len = a.len();
                            if idx >= len {
                                return Err(RuntimeError::new(format!(
                                    "index {idx} out of bounds for array of length {len}"
                                )));
                            }
                            ManagedRc::make_mut(&mut a)[idx] = new_value;
                            self.push(Value::Array(a));
                        }
                        Value::List(mut l) => {
                            let idx = as_usize_index(&index_val)?;
                            let len = l.len();
                            if idx >= len {
                                return Err(RuntimeError::new(format!(
                                    "index {idx} out of bounds for list of length {len}"
                                )));
                            }
                            ManagedRc::make_mut(&mut l)[idx] = new_value;
                            self.push(Value::List(l));
                        }
                        Value::Slice { .. } => {
                            return Err(RuntimeError::new(
                                "cannot mutate through a slice".to_string(),
                            ));
                        }
                        other => {
                            return Err(RuntimeError::new(format!(
                                "IndexSet on non-indexable value: {other}"
                            )));
                        }
                    }
                }

                Op::CollectionLen => {
                    let collection = self.pop();
                    match collection {
                        Value::Array(a) => self.push(Value::Int(a.len() as i64)),
                        Value::List(l) => self.push(Value::Int(l.len() as i64)),
                        Value::Slice { len, .. } => self.push(Value::Int(len as i64)),
                        other => {
                            return Err(RuntimeError::new(format!(
                                "CollectionLen: expected array or list, got {other}"
                            )));
                        }
                    }
                }

                Op::Slice(inclusive) => {
                    let end_val = self.pop();
                    let start_val = self.pop();
                    let collection = self.pop();

                    let start = as_usize_index(&start_val)?;
                    let raw_end = as_usize_index(&end_val)?;
                    let end = if inclusive { raw_end + 1 } else { raw_end };

                    match collection {
                        Value::Array(a) => {
                            let len = a.len();
                            if start > end || end > len {
                                return Err(RuntimeError::new(format!(
                                    "slice {start}..{end} out of bounds for array of length {len}"
                                )));
                            }
                            self.push(Value::Slice {
                                data: a,
                                offset: start,
                                len: end - start,
                            });
                        }
                        Value::Slice { data, offset, len } => {
                            if start > end || end > len {
                                return Err(RuntimeError::new(format!(
                                    "slice {start}..{end} out of bounds for slice of length {len}"
                                )));
                            }
                            self.push(Value::Slice {
                                data,
                                offset: offset + start,
                                len: end - start,
                            });
                        }
                        Value::List(l) => {
                            let len = l.len();
                            if start > end || end > len {
                                return Err(RuntimeError::new(format!(
                                    "slice {start}..{end} out of bounds for list of length {len}"
                                )));
                            }
                            let sliced = l[start..end].to_vec();
                            self.push(Value::List(ManagedRc::new(sliced)));
                        }
                        other => {
                            return Err(RuntimeError::new(format!(
                                "Slice on non-sliceable value: {other}"
                            )));
                        }
                    }
                }

                Op::MapLen => {
                    let map = self.pop();
                    match map {
                        Value::Map(m) => self.push(Value::Int(m.len() as i64)),
                        other => {
                            return Err(RuntimeError::new(format!(
                                "MapLen: expected map, got {other}"
                            )));
                        }
                    }
                }

                Op::MapEntryAt => {
                    let index_val = self.pop();
                    let map = self.pop();
                    match (&map, &index_val) {
                        (Value::Map(m), Value::Int(idx)) => {
                            let idx = *idx as usize;
                            match m.get_index(idx) {
                                Some((k, v)) => {
                                    let entry =
                                        Value::Tuple(ManagedRc::new(vec![k.clone(), v.clone()]));
                                    self.push(entry);
                                }
                                None => {
                                    return Err(RuntimeError::new(format!(
                                        "MapEntryAt: index {idx} out of bounds for map of length {}",
                                        m.len()
                                    )));
                                }
                            }
                        }
                        _ => {
                            return Err(RuntimeError::new(format!(
                                "MapEntryAt: expected (map, int), got ({map}, {index_val})"
                            )));
                        }
                    }
                }

                Op::ListPush => {
                    let value = self.pop();
                    let collection = self.pop();
                    match collection {
                        Value::List(mut l) => {
                            ManagedRc::make_mut(&mut l).push(value);
                            self.push(Value::Nil);
                            self.push(Value::List(l));
                        }
                        other => {
                            return Err(RuntimeError::new(format!(
                                "ListPush on non-list value: {other}"
                            )));
                        }
                    }
                }

                Op::ListPop => {
                    let collection = self.pop();
                    match collection {
                        Value::List(mut l) => {
                            let popped = ManagedRc::make_mut(&mut l).pop().unwrap_or(Value::Nil);
                            self.push(popped);
                            self.push(Value::List(l));
                        }
                        other => {
                            return Err(RuntimeError::new(format!(
                                "ListPop on non-list value: {other}"
                            )));
                        }
                    }
                }

                Op::MapInsert => {
                    let value = self.pop();
                    let key = self.pop();
                    let collection = self.pop();
                    match collection {
                        Value::Map(mut m) => {
                            ManagedRc::make_mut(&mut m).insert(key, value);
                            self.push(Value::Nil);
                            self.push(Value::Map(m));
                        }
                        other => {
                            return Err(RuntimeError::new(format!(
                                "MapInsert on non-map value: {other}"
                            )));
                        }
                    }
                }

                Op::MapRemove => {
                    let key = self.pop();
                    let collection = self.pop();
                    match collection {
                        Value::Map(mut m) => {
                            let removed = ManagedRc::make_mut(&mut m)
                                .remove(&key)
                                .unwrap_or(Value::Nil);
                            self.push(removed);
                            self.push(Value::Map(m));
                        }
                        other => {
                            return Err(RuntimeError::new(format!(
                                "MapRemove on non-map value: {other}"
                            )));
                        }
                    }
                }

                Op::ListSortBy(comparator_idx) => {
                    let comparator_idx = comparator_idx as usize;
                    let collection = self.pop();
                    match collection {
                        Value::List(mut l) => {
                            let items = ManagedRc::make_mut(&mut l);
                            let len = items.len();
                            // insertion sort, stable, sequential, allows re-entrant
                            // call_function between comparisons without borrow conflicts
                            for i in 1..len {
                                let mut j = i;
                                while j > 0 {
                                    let a = items[j - 1].clone();
                                    let b = items[j].clone();
                                    let result =
                                        self.call_function(registry, comparator_idx, vec![a, b])?;
                                    let Value::Bool(is_ordered) = result else {
                                        return Err(RuntimeError::new(
                                            "sort_by comparator must return bool",
                                        ));
                                    };
                                    if is_ordered {
                                        break;
                                    }
                                    items.swap(j - 1, j);
                                    j -= 1;
                                }
                            }
                            // push nil as void result, then the sorted list, matches the
                            // collectionmut stack protocol, setlocal consumes the list and
                            // stmt::expr's pop consumes the nil
                            self.push(Value::Nil);
                            self.push(Value::List(l));
                        }
                        other => {
                            return Err(RuntimeError::new(format!(
                                "ListSortBy on non-list value: {other}"
                            )));
                        }
                    }
                }

                Op::Cast(kind) => {
                    let val = self.pop();
                    let result = match kind {
                        CastKind::IntToFloat => {
                            let Value::Int(n) = val else { unreachable!() };
                            Value::Float(n as f32)
                        }
                        CastKind::FloatToInt => {
                            let Value::Float(f) = val else { unreachable!() };
                            Value::Int(f as i64)
                        }
                        CastKind::IntToDouble => {
                            let Value::Int(n) = val else { unreachable!() };
                            Value::Double(n as f64)
                        }
                        CastKind::DoubleToInt => {
                            let Value::Double(d) = val else {
                                unreachable!()
                            };
                            Value::Int(d as i64)
                        }
                        CastKind::FloatToDouble => {
                            let Value::Float(f) = val else { unreachable!() };
                            Value::Double(f64::from(f))
                        }
                        CastKind::DoubleToFloat => {
                            let Value::Double(d) = val else {
                                unreachable!()
                            };
                            Value::Float(d as f32)
                        }
                    };
                    self.push(result);
                }

                Op::ToString => {
                    let val = self.pop();
                    let s = self.format_value(registry, &val)?;
                    self.push(Value::String(ManagedRc::new(s)));
                }

                Op::OptionalToString(type_name_idx) => {
                    let val = self.pop();
                    let type_name =
                        match &self.program.chunks[chunk_idx].constants[type_name_idx as usize] {
                            Value::String(s) => s.as_str().to_string(),
                            _ => unreachable!("OptionalToString constant must be a string"),
                        };
                    let s = match val {
                        Value::Enum(ref e) if e.type_id == OPTION_TYPE_ID && e.variant == 1 => {
                            let inner = self.format_value(registry, &e.fields[0])?;
                            format!(".Some({inner})")
                        }
                        Value::Enum(ref e) if e.type_id == OPTION_TYPE_ID => {
                            format!(".None<{type_name}>")
                        }
                        Value::Nil => format!(".None<{type_name}>"),
                        other => {
                            let inner = self.format_value(registry, &other)?;
                            format!(".Some({inner})")
                        }
                    };
                    self.push(Value::String(ManagedRc::new(s)));
                }

                Op::Format(spec) => {
                    let val = self.pop();
                    let s = self.format_value_with_spec(registry, &val, &spec)?;
                    self.push(Value::String(ManagedRc::new(s)));
                }
            }
        }
    }

    fn setup_closure_frame(&mut self, closure: &ClosureData, args: Vec<Value>) {
        let callee_idx = closure.fn_id as usize;
        let (local_count, params_count) = {
            let callee = &self.program.chunks[callee_idx];
            (callee.local_count as usize, callee.params_count as usize)
        };
        let new_stack_base = self.stack.len();
        self.stack.extend(closure.captures.clone());
        self.stack.extend(args);

        let extra_locals = local_count - params_count;
        self.stack
            .resize(self.stack.len() + extra_locals, Value::Nil);
        self.frames.push(CallFrame {
            chunk_idx: callee_idx,
            ip: 0,
            stack_base: new_stack_base,
        });
    }

    pub fn call_closure(
        &mut self,
        registry: &ExternRegistry,
        closure: &Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let closure = match closure {
            Value::Closure(c) => c.clone(),
            other => {
                return Err(RuntimeError::new(format!(
                    "type error: expected callable, got {}",
                    type_name(other)
                )));
            }
        };
        let return_depth = self.frames.len();
        self.setup_closure_frame(&closure, args);
        self.run_until_depth(registry, return_depth)
    }

    fn call_function(
        &mut self,
        registry: &ExternRegistry,
        chunk_idx: usize,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let return_depth = self.frames.len();

        let stack_base = self.stack.len();
        self.stack.extend(args);

        let extra_locals = {
            let chunk = &self.program.chunks[chunk_idx];
            chunk.local_count as usize - chunk.params_count as usize
        };
        self.frames.push(CallFrame {
            chunk_idx,
            ip: 0,
            stack_base,
        });
        self.stack
            .resize(self.stack.len() + extra_locals, Value::Nil);

        self.run_until_depth(registry, return_depth)
    }

    fn format_struct_data(
        &mut self,
        registry: &ExternRegistry,
        s: &StructData,
        value: &Value,
    ) -> Result<String, RuntimeError> {
        let meta = &self.program.aggregate_meta[s.type_id as usize];
        if let Some(func_id) = meta.display_func {
            let result = self.call_function(registry, func_id.0 as usize, vec![value.clone()])?;
            match result {
                Value::String(s) => Ok((*s).clone()),
                _ => Err(RuntimeError::new("to_string must return a string")),
            }
        } else {
            let name = meta.name.clone();
            let field_names: Vec<String> = meta.fields.iter().map(|f| f.name.clone()).collect();
            let fields: Vec<Value> = s.fields.clone();
            let mut out = format!("{name}(");
            for (i, (fname, val)) in field_names.iter().zip(fields.iter()).enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                let formatted = self.format_value(registry, val)?;
                write!(out, "{fname}: {formatted}").unwrap();
            }
            out.push(')');
            Ok(out)
        }
    }

    fn format_sequence(
        &mut self,
        registry: &ExternRegistry,
        elements: &[Value],
    ) -> Result<String, RuntimeError> {
        let mut out = String::from("[");
        for (i, v) in elements.iter().enumerate() {
            if i > 0 {
                out.push_str(", ");
            }
            let formatted = self.format_value(registry, v)?;
            out.push_str(&formatted);
        }
        out.push(']');
        Ok(out)
    }

    fn format_value(
        &mut self,
        registry: &ExternRegistry,
        value: &Value,
    ) -> Result<String, RuntimeError> {
        match value {
            Value::Struct(s) | Value::DataRef(s) => self.format_struct_data(registry, s, value),
            Value::List(l) => {
                let elems: Vec<Value> = l.iter().cloned().collect();
                self.format_sequence(registry, &elems)
            }
            Value::Array(a) => {
                let elems: Vec<Value> = a.iter().cloned().collect();
                self.format_sequence(registry, &elems)
            }
            Value::Map(m) => {
                let mut out = String::from("[");
                let mut entries: Vec<(Value, Value)> =
                    m.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
                if !m.is_ordered() {
                    entries.sort_by_key(|(a, _)| a.to_string());
                }
                for (i, (k, v)) in entries.iter().enumerate() {
                    if i > 0 {
                        out.push_str(", ");
                    }
                    let fk = self.format_value(registry, k)?;
                    let fv = self.format_value(registry, v)?;
                    write!(out, "{fk}: {fv}").unwrap();
                }
                out.push(']');
                Ok(out)
            }
            Value::Tuple(t) => {
                let elems: Vec<Value> = t.iter().cloned().collect();
                let mut out = String::from("(");
                for (i, v) in elems.iter().enumerate() {
                    if i > 0 {
                        out.push_str(", ");
                    }
                    let formatted = self.format_value(registry, v)?;
                    out.push_str(&formatted);
                }
                out.push(')');
                Ok(out)
            }
            Value::Enum(e) => {
                if e.type_id == OPTION_TYPE_ID {
                    return match e.variant {
                        1 => {
                            let inner = self.format_value(registry, &e.fields[0])?;
                            Ok(format!(".Some({inner})"))
                        }
                        _ => Ok(".None".to_string()),
                    };
                }

                let program = self.program;
                let meta = &program.enum_meta[e.type_id as usize];
                let variant_idx = e.variant as usize;
                let variant_name = meta.variants[variant_idx].name.clone();
                let enum_name = meta.name.clone();
                let shape = meta.variants[variant_idx].shape.clone();
                let fields: Vec<Value> = e.fields.clone();
                match shape {
                    VariantShape::Unit => Ok(format!("{enum_name}.{variant_name}")),
                    VariantShape::Tuple(_) => {
                        let mut vals = vec![];
                        for v in &fields {
                            vals.push(self.format_value(registry, v)?);
                        }
                        Ok(format!("{enum_name}.{variant_name}({})", vals.join(", ")))
                    }
                    VariantShape::Struct(field_defs) => {
                        let mut pairs = vec![];
                        for (fmeta, val) in field_defs.iter().zip(fields.iter()) {
                            let formatted = self.format_value(registry, val)?;
                            pairs.push(format!("{}: {formatted}", fmeta.name));
                        }
                        Ok(format!("{enum_name}.{variant_name}({})", pairs.join(", ")))
                    }
                }
            }
            Value::ExternHandle(data) => Ok((data.to_string_fn)(data.id)),
            _ => Ok(format!("{value}")),
        }
    }

    fn format_value_with_spec(
        &mut self,
        registry: &ExternRegistry,
        value: &Value,
        spec: &FormatSpec,
    ) -> Result<String, RuntimeError> {
        let raw = self.format_raw(registry, value, spec)?;
        let is_numeric = matches!(value, Value::Int(_) | Value::Float(_) | Value::Double(_));
        let is_string = matches!(value, Value::String(_));
        let signed = spec.apply_sign(&raw, is_numeric);
        Ok(spec.apply_padding(&signed, is_string))
    }

    fn format_raw(
        &mut self,
        registry: &ExternRegistry,
        value: &Value,
        spec: &FormatSpec,
    ) -> Result<String, RuntimeError> {
        match spec.kind {
            FormatKind::Default => match (value, spec.precision) {
                (Value::Float(v), Some(prec)) => Ok(format!("{:.prec$}", v, prec = prec as usize)),
                (Value::Double(v), Some(prec)) => Ok(format!("{:.prec$}", v, prec = prec as usize)),
                (Value::String(s), Some(prec)) => Ok(s.chars().take(prec as usize).collect()),
                _ => self.format_value(registry, value),
            },
            FormatKind::Hex => {
                let Value::Int(n) = value else {
                    unreachable!("typechecker validated")
                };
                Ok(format!("{n:x}"))
            }
            FormatKind::HexUpper => {
                let Value::Int(n) = value else {
                    unreachable!("typechecker validated")
                };
                Ok(format!("{n:X}"))
            }
            FormatKind::Binary => {
                let Value::Int(n) = value else {
                    unreachable!("typechecker validated")
                };
                Ok(format!("{n:b}"))
            }
            FormatKind::Exp => match (value, spec.precision) {
                (Value::Float(v), Some(prec)) => Ok(format!("{:.prec$e}", v, prec = prec as usize)),
                (Value::Float(v), None) => Ok(format!("{v:e}")),
                (Value::Double(v), Some(prec)) => {
                    Ok(format!("{:.prec$e}", v, prec = prec as usize))
                }
                (Value::Double(v), None) => Ok(format!("{v:e}")),
                _ => unreachable!("typechecker validated"),
            },
            FormatKind::ExpUpper => match (value, spec.precision) {
                (Value::Float(v), Some(prec)) => Ok(format!("{:.prec$E}", v, prec = prec as usize)),
                (Value::Float(v), None) => Ok(format!("{v:E}")),
                (Value::Double(v), Some(prec)) => {
                    Ok(format!("{:.prec$E}", v, prec = prec as usize))
                }
                (Value::Double(v), None) => Ok(format!("{v:E}")),
                _ => unreachable!("typechecker validated"),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        Profile,
        vm::{bytecode::Chunk, managed_rc::ManagedRc},
    };

    fn empty_registry() -> ExternRegistry {
        ExternRegistry::new(0)
    }

    fn make_program(chunks: Vec<Chunk>, main_idx: usize) -> CompiledProgram {
        CompiledProgram {
            chunks,
            main_idx,
            extern_names: vec![],
            aggregate_meta: vec![],
            enum_meta: vec![],
            aggregate_vtables: vec![],
            profile: Profile::default(),
        }
    }

    fn simple_chunk(name: &str, ops: Vec<Op>, constants: Vec<Value>) -> Chunk {
        let mut chunk = Chunk::new(name, 0, 0);
        for op in ops {
            chunk.emit(op);
        }
        for val in constants {
            chunk.add_constant(val);
        }
        chunk
    }

    fn str_val(s: &str) -> Value {
        Value::String(ManagedRc::new(s.to_string()))
    }

    #[test]
    fn empty_main_runs_ok() {
        let chunk = simple_chunk("main", vec![Op::Nil, Op::Return], vec![]);
        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_ok());
        assert!(vm.stdout.is_empty());
    }

    #[test]
    fn return_constant() {
        let chunk = simple_chunk(
            "main",
            vec![Op::Constant(0), Op::Return],
            vec![Value::Int(42)],
        );
        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_ok());
    }

    #[test]
    fn set_and_get_local() {
        // local_count = 1; store 10, load, return
        let mut chunk = Chunk::new("main", 1, 0);
        let idx = chunk.add_constant(Value::Int(10));
        chunk.emit(Op::Constant(idx));
        chunk.emit(Op::SetLocal(0));
        chunk.emit(Op::GetLocal(0));
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_ok());
    }

    #[test]
    fn arithmetic_add() {
        let mut chunk = Chunk::new("main", 0, 0);
        let i1 = chunk.add_constant(Value::Int(3));
        let i2 = chunk.add_constant(Value::Int(4));
        chunk.emit(Op::Constant(i1));
        chunk.emit(Op::Constant(i2));
        chunk.emit(Op::Add);
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_ok());
    }

    #[test]
    fn jump_if_false_skips_when_false() {
        // push False; JumpIfFalse(1); Nil; Return(skipped via jump); Nil; Return
        let mut chunk = Chunk::new("main", 0, 0);
        chunk.emit(Op::False);
        chunk.emit(Op::JumpIfFalse(1)); // skip one op
        chunk.emit(Op::True); // skipped
        chunk.emit(Op::Nil);
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_ok());
    }

    #[test]
    fn jump_if_false_does_not_skip_when_true() {
        // push True; JumpIfFalse(1); True(not skipped); Return
        let mut chunk = Chunk::new("main", 0, 0);
        chunk.emit(Op::True);
        chunk.emit(Op::JumpIfFalse(1)); // would skip next op if false
        chunk.emit(Op::Nil); // NOT skipped — falls through
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_ok());
    }

    #[test]
    fn call_and_return() {
        // callee (chunk 0): returns Int(99)
        // main (chunk 1): calls callee, Pop, Nil, Return
        let mut callee = Chunk::new("callee", 0, 0);
        let ci = callee.add_constant(Value::Int(99));
        callee.emit(Op::Constant(ci));
        callee.emit(Op::Return);

        let mut main = Chunk::new("main", 0, 0);
        main.emit(Op::Call(0, 0)); // call chunk 0 with 0 args
        main.emit(Op::Pop);
        main.emit(Op::Nil);
        main.emit(Op::Return);

        let program = make_program(vec![callee, main], 1);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_ok());
    }

    #[test]
    fn call_with_args() {
        // fn add(a, b) -> a+b  (chunk 0: local_count=2, params_count=2)
        // main calls add(3, 4) and discards result
        let mut add_chunk = Chunk::new("add", 2, 2);
        add_chunk.emit(Op::GetLocal(0));
        add_chunk.emit(Op::GetLocal(1));
        add_chunk.emit(Op::Add);
        add_chunk.emit(Op::Return);

        let mut main_chunk = Chunk::new("main", 0, 0);
        let i3 = main_chunk.add_constant(Value::Int(3));
        let i4 = main_chunk.add_constant(Value::Int(4));
        main_chunk.emit(Op::Constant(i3));
        main_chunk.emit(Op::Constant(i4));
        main_chunk.emit(Op::Call(0, 2));
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::Nil);
        main_chunk.emit(Op::Return);

        let program = make_program(vec![add_chunk, main_chunk], 1);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_ok());
    }

    #[test]
    fn call_builtin_println_writes_stdout() {
        let mut chunk = Chunk::new("main", 0, 0);
        let si = chunk.add_constant(str_val("hello world"));
        chunk.emit(Op::Constant(si));
        chunk.emit(Op::CallBuiltin(0, 1)); // Println is index 0
        chunk.emit(Op::Pop);
        chunk.emit(Op::Nil);
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
        assert_eq!(vm.stdout, "hello world\n");
    }

    #[test]
    fn runtime_error_div_by_zero() {
        let mut chunk = Chunk::new("main", 0, 0);
        let i1 = chunk.add_constant(Value::Int(1));
        let i0 = chunk.add_constant(Value::Int(0));
        chunk.emit(Op::Constant(i1));
        chunk.emit(Op::Constant(i0));
        chunk.emit(Op::Div);
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        let result = vm.run(&empty_registry());
        assert!(result.is_err());
        assert!(result.unwrap_err().message.contains("zero"));
    }

    #[test]
    fn construct_struct_pushes_struct_value() {
        // push two Int constants, then ConstructStruct(42, 2)
        let mut chunk = Chunk::new("main", 0, 0);
        let i10 = chunk.add_constant(Value::Int(10));
        let i20 = chunk.add_constant(Value::Int(20));
        chunk.emit(Op::Constant(i10));
        chunk.emit(Op::Constant(i20));
        chunk.emit(Op::ConstructStruct(42, 2));
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
    }

    #[test]
    fn get_field_reads_struct_field() {
        // construct struct with fields [10, 20], then GetField(1) -> 20
        let mut chunk = Chunk::new("main", 1, 0);
        let i10 = chunk.add_constant(Value::Int(10));
        let i20 = chunk.add_constant(Value::Int(20));
        chunk.emit(Op::Constant(i10));
        chunk.emit(Op::Constant(i20));
        chunk.emit(Op::ConstructStruct(1, 2));
        chunk.emit(Op::SetLocal(0));
        chunk.emit(Op::GetLocal(0));
        chunk.emit(Op::GetField(1));
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
    }

    #[test]
    fn construct_tuple_and_get_field() {
        // tuple (true, 99), GetField(1) -> 99
        let mut chunk = Chunk::new("main", 0, 0);
        let i99 = chunk.add_constant(Value::Int(99));
        chunk.emit(Op::True);
        chunk.emit(Op::Constant(i99));
        chunk.emit(Op::ConstructTuple(2));
        chunk.emit(Op::GetField(1));
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
    }

    #[test]
    fn set_field_mutates_struct() {
        // local 0 = struct{10, 20}, SetField(0) with 99, GetField(0) -> 99
        let mut chunk = Chunk::new("main", 1, 0);
        let i10 = chunk.add_constant(Value::Int(10));
        let i20 = chunk.add_constant(Value::Int(20));
        let i99 = chunk.add_constant(Value::Int(99));
        // construct
        chunk.emit(Op::Constant(i10));
        chunk.emit(Op::Constant(i20));
        chunk.emit(Op::ConstructStruct(0, 2));
        chunk.emit(Op::SetLocal(0));
        // set field 0 to 99
        chunk.emit(Op::GetLocal(0));
        chunk.emit(Op::Constant(i99));
        chunk.emit(Op::SetField(0));
        chunk.emit(Op::SetLocal(0));
        // read field 0 and return
        chunk.emit(Op::GetLocal(0));
        chunk.emit(Op::GetField(0));
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
    }

    #[test]
    fn construct_enum_unit_variant() {
        // ConstructEnum(0, 1, 0) -> Value::Enum
        let mut chunk = Chunk::new("main", 0, 0);
        chunk.emit(Op::ConstructEnum(0, 1, 0));
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
    }

    #[test]
    fn construct_enum_with_field() {
        // Constant(42), ConstructEnum(0, 0, 1) -> Value::Enum with 1 field
        let mut chunk = Chunk::new("main", 0, 0);
        let i42 = chunk.add_constant(Value::Int(42));
        chunk.emit(Op::Constant(i42));
        chunk.emit(Op::ConstructEnum(0, 0, 1));
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
    }

    #[test]
    fn get_enum_variant_returns_variant_index() {
        // ConstructEnum(0, 2, 0), GetEnumVariant -> Int(2), Return
        let mut chunk = Chunk::new("main", 0, 0);
        chunk.emit(Op::ConstructEnum(0, 2, 0));
        chunk.emit(Op::GetEnumVariant);
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
    }

    #[test]
    fn get_field_on_enum_returns_payload() {
        // Constant(99), ConstructEnum(0, 0, 1), GetField(0) -> Int(99)
        let mut chunk = Chunk::new("main", 0, 0);
        let i99 = chunk.add_constant(Value::Int(99));
        chunk.emit(Op::Constant(i99));
        chunk.emit(Op::ConstructEnum(0, 0, 1));
        chunk.emit(Op::GetField(0));
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
    }

    #[test]
    fn get_enum_variant_on_raw_value_returns_one() {
        let mut chunk = Chunk::new("main", 0, 0);
        chunk.emit(Op::True);
        chunk.emit(Op::GetEnumVariant);
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
    }

    #[test]
    fn get_enum_variant_on_nil_returns_zero() {
        let mut chunk = Chunk::new("main", 0, 0);
        chunk.emit(Op::Nil);
        chunk.emit(Op::GetEnumVariant);
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
    }

    #[test]
    fn unwrap_optional_on_enum_extracts_field() {
        let mut chunk = Chunk::new("main", 0, 0);
        let i42 = chunk.add_constant(Value::Int(42));
        chunk.emit(Op::Constant(i42));
        chunk.emit(Op::ConstructEnum(0, 1, 1));
        chunk.emit(Op::UnwrapOptional);
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
    }

    #[test]
    fn unwrap_optional_on_flat_value_returns_same() {
        let mut chunk = Chunk::new("main", 0, 0);
        let i99 = chunk.add_constant(Value::Int(99));
        chunk.emit(Op::Constant(i99));
        chunk.emit(Op::UnwrapOptional);
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
    }

    #[test]
    fn construct_array_pushes_value() {
        let mut chunk = Chunk::new("main", 0, 0);
        let i1 = chunk.add_constant(Value::Int(1));
        let i2 = chunk.add_constant(Value::Int(2));
        chunk.emit(Op::Constant(i1));
        chunk.emit(Op::Constant(i2));
        chunk.emit(Op::ConstructArray(2));
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_ok());
    }

    #[test]
    fn construct_list_pushes_value() {
        let mut chunk = Chunk::new("main", 0, 0);
        let i1 = chunk.add_constant(Value::Int(10));
        let i2 = chunk.add_constant(Value::Int(20));
        chunk.emit(Op::Constant(i1));
        chunk.emit(Op::Constant(i2));
        chunk.emit(Op::ConstructList(2));
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_ok());
    }

    #[test]
    fn index_get_reads_element() {
        // [Int(10), Int(20)][1] -> Int(20)
        let mut chunk = Chunk::new("main", 0, 0);
        let i10 = chunk.add_constant(Value::Int(10));
        let i20 = chunk.add_constant(Value::Int(20));
        let i1 = chunk.add_constant(Value::Int(1));
        chunk.emit(Op::Constant(i10));
        chunk.emit(Op::Constant(i20));
        chunk.emit(Op::ConstructArray(2));
        chunk.emit(Op::Constant(i1));
        chunk.emit(Op::IndexGet);
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_ok());
    }

    #[test]
    fn index_get_out_of_bounds_error() {
        let mut chunk = Chunk::new("main", 0, 0);
        let i10 = chunk.add_constant(Value::Int(10));
        let i20 = chunk.add_constant(Value::Int(20));
        let i5 = chunk.add_constant(Value::Int(5));
        chunk.emit(Op::Constant(i10));
        chunk.emit(Op::Constant(i20));
        chunk.emit(Op::ConstructArray(2));
        chunk.emit(Op::Constant(i5));
        chunk.emit(Op::IndexGet);
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_err());
    }

    #[test]
    fn index_set_mutates_element() {
        // local 0 = [10, 20]; local 0[0] = 99; result = local 0[0] -> 99
        let mut chunk = Chunk::new("main", 1, 0);
        let i10 = chunk.add_constant(Value::Int(10));
        let i20 = chunk.add_constant(Value::Int(20));
        let i99 = chunk.add_constant(Value::Int(99));
        let i0 = chunk.add_constant(Value::Int(0));

        // construct [10, 20], store in local 0
        chunk.emit(Op::Constant(i10));
        chunk.emit(Op::Constant(i20));
        chunk.emit(Op::ConstructArray(2));
        chunk.emit(Op::SetLocal(0));

        // local 0[0] = 99
        chunk.emit(Op::GetLocal(0));
        chunk.emit(Op::Constant(i0));
        chunk.emit(Op::Constant(i99));
        chunk.emit(Op::IndexSet);
        chunk.emit(Op::SetLocal(0));

        // read local 0[0]
        chunk.emit(Op::GetLocal(0));
        chunk.emit(Op::Constant(i0));
        chunk.emit(Op::IndexGet);
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_ok());
    }

    #[test]
    fn index_set_cow_semantics() {
        // local 0 = [10, 20]; local 1 = local 0 (shared)
        // local 0[0] = 99  (COW: local 1 should remain [10, 20])
        // verify local 1[0] == 10
        let mut chunk = Chunk::new("main", 2, 0);
        let i10 = chunk.add_constant(Value::Int(10));
        let i20 = chunk.add_constant(Value::Int(20));
        let i99 = chunk.add_constant(Value::Int(99));
        let i0 = chunk.add_constant(Value::Int(0));

        // construct [10, 20], store in local 0
        chunk.emit(Op::Constant(i10));
        chunk.emit(Op::Constant(i20));
        chunk.emit(Op::ConstructArray(2));
        chunk.emit(Op::SetLocal(0));

        // copy to local 1 (shared reference)
        chunk.emit(Op::GetLocal(0));
        chunk.emit(Op::SetLocal(1));

        // mutate local 0[0] = 99
        chunk.emit(Op::GetLocal(0));
        chunk.emit(Op::Constant(i0));
        chunk.emit(Op::Constant(i99));
        chunk.emit(Op::IndexSet);
        chunk.emit(Op::SetLocal(0));

        // read local 1[0] — should still be 10 due to COW
        chunk.emit(Op::GetLocal(1));
        chunk.emit(Op::Constant(i0));
        chunk.emit(Op::IndexGet);
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_ok());
    }

    #[test]
    fn construct_map_basic() {
        let mut chunk = Chunk::new("main", 0, 0);
        let k = chunk.add_constant(str_val("a"));
        let v = chunk.add_constant(Value::Int(1));
        chunk.emit(Op::Constant(k));
        chunk.emit(Op::Constant(v));
        chunk.emit(Op::ConstructMap(1));
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_ok());
    }

    #[test]
    fn construct_empty_map() {
        let mut chunk = Chunk::new("main", 0, 0);
        chunk.emit(Op::ConstructMap(0));
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_ok());
    }

    #[test]
    fn index_get_map_hit() {
        let mut chunk = Chunk::new("main", 0, 0);
        let k = chunk.add_constant(str_val("a"));
        let v = chunk.add_constant(Value::Int(1));
        chunk.emit(Op::Constant(k));
        chunk.emit(Op::Constant(v));
        chunk.emit(Op::ConstructMap(1));
        chunk.emit(Op::Constant(k));
        chunk.emit(Op::IndexGet);
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_ok());
    }

    #[test]
    fn index_get_map_miss() {
        let mut chunk = Chunk::new("main", 0, 0);
        let k = chunk.add_constant(str_val("a"));
        let v = chunk.add_constant(Value::Int(1));
        let miss = chunk.add_constant(str_val("b"));
        chunk.emit(Op::Constant(k));
        chunk.emit(Op::Constant(v));
        chunk.emit(Op::ConstructMap(1));
        chunk.emit(Op::Constant(miss));
        chunk.emit(Op::IndexGet);
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_ok());
    }

    #[test]
    fn index_set_map_insert() {
        let mut chunk = Chunk::new("main", 1, 0);
        let k = chunk.add_constant(str_val("x"));
        let v = chunk.add_constant(Value::Int(42));
        chunk.emit(Op::ConstructMap(0));
        chunk.emit(Op::SetLocal(0));
        chunk.emit(Op::GetLocal(0));
        chunk.emit(Op::Constant(k));
        chunk.emit(Op::Constant(v));
        chunk.emit(Op::IndexSet);
        chunk.emit(Op::SetLocal(0));
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_ok());
    }

    #[test]
    fn index_set_map_update() {
        let mut chunk = Chunk::new("main", 1, 0);
        let k = chunk.add_constant(str_val("a"));
        let v1 = chunk.add_constant(Value::Int(1));
        let v2 = chunk.add_constant(Value::Int(99));
        chunk.emit(Op::Constant(k));
        chunk.emit(Op::Constant(v1));
        chunk.emit(Op::ConstructMap(1));
        chunk.emit(Op::SetLocal(0));
        chunk.emit(Op::GetLocal(0));
        chunk.emit(Op::Constant(k));
        chunk.emit(Op::Constant(v2));
        chunk.emit(Op::IndexSet);
        chunk.emit(Op::SetLocal(0));
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_ok());
    }

    #[test]
    fn map_cow_semantics() {
        // local 0 = {"a": 1}; local 1 = local 0 (shared)
        // local 0["a"] = 99 (COW: local 1 should remain {"a": 1})
        let mut chunk = Chunk::new("main", 2, 0);
        let k = chunk.add_constant(str_val("a"));
        let v1 = chunk.add_constant(Value::Int(1));
        let v2 = chunk.add_constant(Value::Int(99));

        chunk.emit(Op::Constant(k));
        chunk.emit(Op::Constant(v1));
        chunk.emit(Op::ConstructMap(1));
        chunk.emit(Op::SetLocal(0));

        chunk.emit(Op::GetLocal(0));
        chunk.emit(Op::SetLocal(1));

        chunk.emit(Op::GetLocal(0));
        chunk.emit(Op::Constant(k));
        chunk.emit(Op::Constant(v2));
        chunk.emit(Op::IndexSet);
        chunk.emit(Op::SetLocal(0));

        // read local 1["a"] — should still be 1 due to COW
        chunk.emit(Op::GetLocal(1));
        chunk.emit(Op::Constant(k));
        chunk.emit(Op::IndexGet);
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_ok());
    }

    #[test]
    fn list_push_basic() {
        let mut chunk = Chunk::new("main", 1, 0);
        let i1 = chunk.add_constant(Value::Int(1));
        let i2 = chunk.add_constant(Value::Int(2));
        let i3 = chunk.add_constant(Value::Int(3));
        let i99 = chunk.add_constant(Value::Int(99));
        chunk.emit(Op::Constant(i1));
        chunk.emit(Op::Constant(i2));
        chunk.emit(Op::Constant(i3));
        chunk.emit(Op::ConstructList(3));
        chunk.emit(Op::SetLocal(0));
        chunk.emit(Op::GetLocal(0));
        chunk.emit(Op::Constant(i99));
        chunk.emit(Op::ListPush);
        chunk.emit(Op::SetLocal(0));
        chunk.emit(Op::GetLocal(0));
        chunk.emit(Op::Constant(i3));
        chunk.emit(Op::IndexGet);
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_ok());
    }

    #[test]
    fn list_push_cow_semantics() {
        let mut chunk = Chunk::new("main", 2, 0);
        let i10 = chunk.add_constant(Value::Int(10));
        let i20 = chunk.add_constant(Value::Int(20));
        let i99 = chunk.add_constant(Value::Int(99));
        let i0 = chunk.add_constant(Value::Int(0));
        chunk.emit(Op::Constant(i10));
        chunk.emit(Op::Constant(i20));
        chunk.emit(Op::ConstructList(2));
        chunk.emit(Op::SetLocal(0));
        chunk.emit(Op::GetLocal(0));
        chunk.emit(Op::SetLocal(1));
        chunk.emit(Op::GetLocal(0));
        chunk.emit(Op::Constant(i99));
        chunk.emit(Op::ListPush);
        chunk.emit(Op::SetLocal(0));
        chunk.emit(Op::GetLocal(1));
        chunk.emit(Op::Constant(i0));
        chunk.emit(Op::IndexGet);
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_ok());
    }

    #[test]
    fn list_pop_basic() {
        let mut chunk = Chunk::new("main", 1, 0);
        let i1 = chunk.add_constant(Value::Int(1));
        let i2 = chunk.add_constant(Value::Int(2));
        let i3 = chunk.add_constant(Value::Int(3));
        let i0 = chunk.add_constant(Value::Int(0));
        chunk.emit(Op::Constant(i1));
        chunk.emit(Op::Constant(i2));
        chunk.emit(Op::Constant(i3));
        chunk.emit(Op::ConstructList(3));
        chunk.emit(Op::SetLocal(0));
        chunk.emit(Op::GetLocal(0));
        chunk.emit(Op::ListPop);
        chunk.emit(Op::SetLocal(0));
        chunk.emit(Op::Pop);
        chunk.emit(Op::GetLocal(0));
        chunk.emit(Op::Constant(i0));
        chunk.emit(Op::IndexGet);
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_ok());
    }

    #[test]
    fn map_insert_basic() {
        let mut chunk = Chunk::new("main", 1, 0);
        let kb = chunk.add_constant(str_val("b"));
        let v2 = chunk.add_constant(Value::Int(2));
        chunk.emit(Op::ConstructMap(0));
        chunk.emit(Op::SetLocal(0));
        chunk.emit(Op::GetLocal(0));
        chunk.emit(Op::Constant(kb));
        chunk.emit(Op::Constant(v2));
        chunk.emit(Op::MapInsert);
        chunk.emit(Op::SetLocal(0));
        chunk.emit(Op::GetLocal(0));
        chunk.emit(Op::Constant(kb));
        chunk.emit(Op::IndexGet);
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_ok());
    }

    #[test]
    fn map_remove_basic() {
        let mut chunk = Chunk::new("main", 1, 0);
        let ka = chunk.add_constant(str_val("a"));
        let kb = chunk.add_constant(str_val("b"));
        let v1 = chunk.add_constant(Value::Int(1));
        let v2 = chunk.add_constant(Value::Int(2));
        chunk.emit(Op::Constant(ka));
        chunk.emit(Op::Constant(v1));
        chunk.emit(Op::Constant(kb));
        chunk.emit(Op::Constant(v2));
        chunk.emit(Op::ConstructMap(2));
        chunk.emit(Op::SetLocal(0));
        chunk.emit(Op::GetLocal(0));
        chunk.emit(Op::Constant(ka));
        chunk.emit(Op::MapRemove);
        chunk.emit(Op::SetLocal(0));
        chunk.emit(Op::Pop);
        chunk.emit(Op::GetLocal(0));
        chunk.emit(Op::Constant(ka));
        chunk.emit(Op::IndexGet);
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_ok());
    }

    #[test]
    fn map_remove_cow_semantics() {
        let mut chunk = Chunk::new("main", 2, 0);
        let kx = chunk.add_constant(str_val("x"));
        let v10 = chunk.add_constant(Value::Int(10));
        chunk.emit(Op::Constant(kx));
        chunk.emit(Op::Constant(v10));
        chunk.emit(Op::ConstructMap(1));
        chunk.emit(Op::SetLocal(0));
        chunk.emit(Op::GetLocal(0));
        chunk.emit(Op::SetLocal(1));
        chunk.emit(Op::GetLocal(0));
        chunk.emit(Op::Constant(kx));
        chunk.emit(Op::MapRemove);
        chunk.emit(Op::SetLocal(0));
        chunk.emit(Op::Pop);
        chunk.emit(Op::GetLocal(1));
        chunk.emit(Op::Constant(kx));
        chunk.emit(Op::IndexGet);
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_ok());
    }

    #[test]
    fn push_ref_creates_correct_absolute_index() {
        // main: local_count=1, stack_base=0
        // store Int(42) in local 0, PushRef(0) → StackRef(0), println it
        // expected stdout: "<ref@0>"
        let mut chunk = Chunk::new("main", 1, 0);
        let i42 = chunk.add_constant(Value::Int(42));
        chunk.emit(Op::Constant(i42));
        chunk.emit(Op::SetLocal(0));
        chunk.emit(Op::PushRef(0));
        chunk.emit(Op::CallBuiltin(0, 1)); // Println
        chunk.emit(Op::Pop);
        chunk.emit(Op::Nil);
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
        assert_eq!(vm.stdout, "<ref@0>\n");
    }

    #[test]
    fn push_ref_in_callee_accounts_for_stack_base() {
        // main (chunk 1): local_count=1, stack_base=0; stores Int(42), calls callee
        // callee (chunk 0): local_count=1, stack_base=1; PushRef(0) → StackRef(1), println
        // expected stdout: "<ref@1>"
        let mut callee = Chunk::new("callee", 1, 0);
        callee.emit(Op::PushRef(0));
        callee.emit(Op::CallBuiltin(0, 1)); // Println
        callee.emit(Op::Pop);
        callee.emit(Op::Nil);
        callee.emit(Op::Return);

        let mut main_chunk = Chunk::new("main", 1, 0);
        let i42 = main_chunk.add_constant(Value::Int(42));
        main_chunk.emit(Op::Constant(i42));
        main_chunk.emit(Op::SetLocal(0));
        main_chunk.emit(Op::Call(0, 0));
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::Nil);
        main_chunk.emit(Op::Return);

        let program = make_program(vec![callee, main_chunk], 1);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
        assert_eq!(vm.stdout, "<ref@1>\n");
    }

    #[test]
    fn push_ref_arithmetic_errors() {
        // PushRef(0) + Int(1) → runtime error (StackRef is not an arithmetic value)
        let mut chunk = Chunk::new("main", 1, 0);
        let i1 = chunk.add_constant(Value::Int(1));
        chunk.emit(Op::PushRef(0));
        chunk.emit(Op::Constant(i1));
        chunk.emit(Op::Add);
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_err());
    }

    #[test]
    fn deref_read_retrieves_caller_value() {
        // main: local 0 = Int(42); PushRef(0); Call callee
        // callee: receives StackRef in local 0; DerefRead(0) → Int(42); println
        let mut callee = Chunk::new("callee", 1, 1);
        callee.emit(Op::DerefRead(0));
        callee.emit(Op::CallBuiltin(0, 1)); // println
        callee.emit(Op::Pop);
        callee.emit(Op::Nil);
        callee.emit(Op::Return);

        let mut main_chunk = Chunk::new("main", 1, 0);
        let i42 = main_chunk.add_constant(Value::Int(42));
        main_chunk.emit(Op::Constant(i42));
        main_chunk.emit(Op::SetLocal(0));
        main_chunk.emit(Op::PushRef(0));
        main_chunk.emit(Op::Call(0, 1));
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::Nil);
        main_chunk.emit(Op::Return);

        let program = make_program(vec![callee, main_chunk], 1);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
        assert_eq!(vm.stdout, "42\n");
    }

    #[test]
    fn deref_write_mutates_caller_local() {
        // main: local 0 = Int(42); PushRef(0); Call callee; println(local 0)
        // callee: DerefWrite(0) with Int(99) → main's local 0 becomes 99
        let mut callee = Chunk::new("callee", 1, 1);
        let i99 = callee.add_constant(Value::Int(99));
        callee.emit(Op::Constant(i99));
        callee.emit(Op::DerefWrite(0));
        callee.emit(Op::Nil);
        callee.emit(Op::Return);

        let mut main_chunk = Chunk::new("main", 1, 0);
        let i42 = main_chunk.add_constant(Value::Int(42));
        main_chunk.emit(Op::Constant(i42));
        main_chunk.emit(Op::SetLocal(0));
        main_chunk.emit(Op::PushRef(0));
        main_chunk.emit(Op::Call(0, 1));
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::GetLocal(0));
        main_chunk.emit(Op::CallBuiltin(0, 1)); // println
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::Nil);
        main_chunk.emit(Op::Return);

        let program = make_program(vec![callee, main_chunk], 1);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
        assert_eq!(vm.stdout, "99\n");
    }

    #[test]
    fn set_field_ref_mutates_struct_in_place() {
        // main: local 0 = Struct{[10, 20]}; PushRef(0); Call callee; println(local 0 field 1)
        // callee: SetFieldRef(0, 1) with Int(99) → struct field 1 becomes 99
        let mut callee = Chunk::new("callee", 1, 1);
        let i99 = callee.add_constant(Value::Int(99));
        callee.emit(Op::Constant(i99));
        callee.emit(Op::SetFieldRef(0, 1));
        callee.emit(Op::Nil);
        callee.emit(Op::Return);

        let mut main_chunk = Chunk::new("main", 1, 0);
        let i10 = main_chunk.add_constant(Value::Int(10));
        let i20 = main_chunk.add_constant(Value::Int(20));
        main_chunk.emit(Op::Constant(i10));
        main_chunk.emit(Op::Constant(i20));
        main_chunk.emit(Op::ConstructStruct(0, 2));
        main_chunk.emit(Op::SetLocal(0));
        main_chunk.emit(Op::PushRef(0));
        main_chunk.emit(Op::Call(0, 1));
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::GetLocal(0));
        main_chunk.emit(Op::GetField(1));
        main_chunk.emit(Op::CallBuiltin(0, 1)); // println
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::Nil);
        main_chunk.emit(Op::Return);

        let program = make_program(vec![callee, main_chunk], 1);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
        assert_eq!(vm.stdout, "99\n");
    }

    #[test]
    fn set_field_ref_cow_when_shared() {
        // main: local 0 = Struct{[10, 20]}; local 1 = clone (refcount=2)
        // PushRef(0); Call callee (sets field 1 to 99 through ref)
        // local 0 field 1 → 99 (COW on the ref's struct)
        // local 1 field 1 → 20 (untouched separate copy)
        let mut callee = Chunk::new("callee", 1, 1);
        let i99 = callee.add_constant(Value::Int(99));
        callee.emit(Op::Constant(i99));
        callee.emit(Op::SetFieldRef(0, 1));
        callee.emit(Op::Nil);
        callee.emit(Op::Return);

        let mut main_chunk = Chunk::new("main", 2, 0);
        let i10 = main_chunk.add_constant(Value::Int(10));
        let i20 = main_chunk.add_constant(Value::Int(20));
        main_chunk.emit(Op::Constant(i10));
        main_chunk.emit(Op::Constant(i20));
        main_chunk.emit(Op::ConstructStruct(0, 2));
        main_chunk.emit(Op::SetLocal(0));
        main_chunk.emit(Op::GetLocal(0)); // clone → refcount=2
        main_chunk.emit(Op::SetLocal(1));
        main_chunk.emit(Op::PushRef(0));
        main_chunk.emit(Op::Call(0, 1));
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::GetLocal(0));
        main_chunk.emit(Op::GetField(1));
        main_chunk.emit(Op::CallBuiltin(0, 1)); // println → "99"
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::GetLocal(1));
        main_chunk.emit(Op::GetField(1));
        main_chunk.emit(Op::CallBuiltin(0, 1)); // println → "20"
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::Nil);
        main_chunk.emit(Op::Return);

        let program = make_program(vec![callee, main_chunk], 1);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
        assert_eq!(vm.stdout, "99\n20\n");
    }

    #[test]
    fn set_field_ref_on_non_struct_errors() {
        // main: local 0 = Int(42); PushRef(0); Call callee
        // callee: SetFieldRef(0, 1) on an Int with field_idx > 0 → runtime error
        // (field_idx == 0 on a flat value is valid: it replaces the whole slot)
        let mut callee = Chunk::new("callee", 1, 1);
        let i0 = callee.add_constant(Value::Int(0));
        callee.emit(Op::Constant(i0));
        callee.emit(Op::SetFieldRef(0, 1));
        callee.emit(Op::Nil);
        callee.emit(Op::Return);

        let mut main_chunk = Chunk::new("main", 1, 0);
        let i42 = main_chunk.add_constant(Value::Int(42));
        main_chunk.emit(Op::Constant(i42));
        main_chunk.emit(Op::SetLocal(0));
        main_chunk.emit(Op::PushRef(0));
        main_chunk.emit(Op::Call(0, 1));
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::Nil);
        main_chunk.emit(Op::Return);

        let program = make_program(vec![callee, main_chunk], 1);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_err());
    }

    #[test]
    fn set_index_ref_mutates_array() {
        // main: local 0 = [10, 20, 30]; PushRef(0); Call callee; print local 0[1]
        // callee: push Int(1) (index), push Int(99) (value), SetIndexRef(0) → array[1] = 99
        let mut callee = Chunk::new("callee", 1, 1);
        let i1 = callee.add_constant(Value::Int(1));
        let i99 = callee.add_constant(Value::Int(99));
        callee.emit(Op::Constant(i1)); // index
        callee.emit(Op::Constant(i99)); // new_value
        callee.emit(Op::SetIndexRef(0));
        callee.emit(Op::Nil);
        callee.emit(Op::Return);

        let mut main_chunk = Chunk::new("main", 1, 0);
        let i10 = main_chunk.add_constant(Value::Int(10));
        let i20 = main_chunk.add_constant(Value::Int(20));
        let i30 = main_chunk.add_constant(Value::Int(30));
        let idx1 = main_chunk.add_constant(Value::Int(1));
        main_chunk.emit(Op::Constant(i10));
        main_chunk.emit(Op::Constant(i20));
        main_chunk.emit(Op::Constant(i30));
        main_chunk.emit(Op::ConstructArray(3));
        main_chunk.emit(Op::SetLocal(0));
        main_chunk.emit(Op::PushRef(0));
        main_chunk.emit(Op::Call(0, 1));
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::GetLocal(0));
        main_chunk.emit(Op::Constant(idx1));
        main_chunk.emit(Op::IndexGet);
        main_chunk.emit(Op::CallBuiltin(0, 1)); // println
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::Nil);
        main_chunk.emit(Op::Return);

        let program = make_program(vec![callee, main_chunk], 1);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
        assert_eq!(vm.stdout, "99\n");
    }

    #[test]
    fn set_index_ref_mutates_list() {
        // same as array test but with ConstructList
        let mut callee = Chunk::new("callee", 1, 1);
        let i1 = callee.add_constant(Value::Int(1));
        let i99 = callee.add_constant(Value::Int(99));
        callee.emit(Op::Constant(i1)); // index
        callee.emit(Op::Constant(i99)); // new_value
        callee.emit(Op::SetIndexRef(0));
        callee.emit(Op::Nil);
        callee.emit(Op::Return);

        let mut main_chunk = Chunk::new("main", 1, 0);
        let i10 = main_chunk.add_constant(Value::Int(10));
        let i20 = main_chunk.add_constant(Value::Int(20));
        let i30 = main_chunk.add_constant(Value::Int(30));
        let idx1 = main_chunk.add_constant(Value::Int(1));
        main_chunk.emit(Op::Constant(i10));
        main_chunk.emit(Op::Constant(i20));
        main_chunk.emit(Op::Constant(i30));
        main_chunk.emit(Op::ConstructList(3));
        main_chunk.emit(Op::SetLocal(0));
        main_chunk.emit(Op::PushRef(0));
        main_chunk.emit(Op::Call(0, 1));
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::GetLocal(0));
        main_chunk.emit(Op::Constant(idx1));
        main_chunk.emit(Op::IndexGet);
        main_chunk.emit(Op::CallBuiltin(0, 1)); // println
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::Nil);
        main_chunk.emit(Op::Return);

        let program = make_program(vec![callee, main_chunk], 1);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
        assert_eq!(vm.stdout, "99\n");
    }

    #[test]
    fn set_index_ref_mutates_map() {
        // main: local 0 = {"a": 1}; PushRef(0); Call callee; print local 0["a"]
        // callee: push Int(99), push "a", SetIndexRef(0) → map["a"] = 99
        let mut callee = Chunk::new("callee", 1, 1);
        let ka = callee.add_constant(str_val("a"));
        let i99 = callee.add_constant(Value::Int(99));
        callee.emit(Op::Constant(ka)); // index (key)
        callee.emit(Op::Constant(i99)); // new_value
        callee.emit(Op::SetIndexRef(0));
        callee.emit(Op::Nil);
        callee.emit(Op::Return);

        let mut main_chunk = Chunk::new("main", 1, 0);
        let ka = main_chunk.add_constant(str_val("a"));
        let i1 = main_chunk.add_constant(Value::Int(1));
        main_chunk.emit(Op::Constant(ka));
        main_chunk.emit(Op::Constant(i1));
        main_chunk.emit(Op::ConstructMap(1));
        main_chunk.emit(Op::SetLocal(0));
        main_chunk.emit(Op::PushRef(0));
        main_chunk.emit(Op::Call(0, 1));
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::GetLocal(0));
        main_chunk.emit(Op::Constant(ka));
        main_chunk.emit(Op::IndexGet);
        main_chunk.emit(Op::CallBuiltin(0, 1)); // println
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::Nil);
        main_chunk.emit(Op::Return);

        let program = make_program(vec![callee, main_chunk], 1);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
        assert_eq!(vm.stdout, "99\n");
    }

    #[test]
    fn set_index_ref_cow_array() {
        // main: local 0 = [10, 20]; local 1 = clone (refcount=2)
        // PushRef(0); call callee (sets index 0 to 99)
        // local 0[0] → 99 (COW on the ref's array)
        // local 1[0] → 10 (untouched)
        let mut callee = Chunk::new("callee", 1, 1);
        let i0 = callee.add_constant(Value::Int(0));
        let i99 = callee.add_constant(Value::Int(99));
        callee.emit(Op::Constant(i0)); // index
        callee.emit(Op::Constant(i99)); // new_value
        callee.emit(Op::SetIndexRef(0));
        callee.emit(Op::Nil);
        callee.emit(Op::Return);

        let mut main_chunk = Chunk::new("main", 2, 0);
        let i10 = main_chunk.add_constant(Value::Int(10));
        let i20 = main_chunk.add_constant(Value::Int(20));
        let idx0 = main_chunk.add_constant(Value::Int(0));
        main_chunk.emit(Op::Constant(i10));
        main_chunk.emit(Op::Constant(i20));
        main_chunk.emit(Op::ConstructArray(2));
        main_chunk.emit(Op::SetLocal(0));
        main_chunk.emit(Op::GetLocal(0)); // clone → refcount=2
        main_chunk.emit(Op::SetLocal(1));
        main_chunk.emit(Op::PushRef(0));
        main_chunk.emit(Op::Call(0, 1));
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::GetLocal(0));
        main_chunk.emit(Op::Constant(idx0));
        main_chunk.emit(Op::IndexGet);
        main_chunk.emit(Op::CallBuiltin(0, 1)); // println → "99"
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::GetLocal(1));
        main_chunk.emit(Op::Constant(idx0));
        main_chunk.emit(Op::IndexGet);
        main_chunk.emit(Op::CallBuiltin(0, 1)); // println → "10"
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::Nil);
        main_chunk.emit(Op::Return);

        let program = make_program(vec![callee, main_chunk], 1);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
        assert_eq!(vm.stdout, "99\n10\n");
    }

    #[test]
    fn set_index_ref_out_of_bounds_errors() {
        // main: local 0 = [10]; PushRef(0); call callee
        // callee: SetIndexRef(0) with index 5 → out of bounds error
        let mut callee = Chunk::new("callee", 1, 1);
        let i5 = callee.add_constant(Value::Int(5));
        let i0 = callee.add_constant(Value::Int(0));
        callee.emit(Op::Constant(i5)); // index (out of bounds)
        callee.emit(Op::Constant(i0)); // new_value
        callee.emit(Op::SetIndexRef(0));
        callee.emit(Op::Nil);
        callee.emit(Op::Return);

        let mut main_chunk = Chunk::new("main", 1, 0);
        let i10 = main_chunk.add_constant(Value::Int(10));
        main_chunk.emit(Op::Constant(i10));
        main_chunk.emit(Op::ConstructArray(1));
        main_chunk.emit(Op::SetLocal(0));
        main_chunk.emit(Op::PushRef(0));
        main_chunk.emit(Op::Call(0, 1));
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::Nil);
        main_chunk.emit(Op::Return);

        let program = make_program(vec![callee, main_chunk], 1);
        let mut vm = VM::new(&program);
        assert!(vm.run(&empty_registry()).is_err());
    }

    #[test]
    fn set_field_ref_on_tuple() {
        // main: local 0 = Tuple(Int(10), Int(20)); PushRef(0); Call callee; print field 1
        // callee: push Int(99), SetFieldRef(0, 1) → tuple field 1 = 99
        let mut callee = Chunk::new("callee", 1, 1);
        let i99 = callee.add_constant(Value::Int(99));
        callee.emit(Op::Constant(i99));
        callee.emit(Op::SetFieldRef(0, 1));
        callee.emit(Op::Nil);
        callee.emit(Op::Return);

        let mut main_chunk = Chunk::new("main", 1, 0);
        let i10 = main_chunk.add_constant(Value::Int(10));
        let i20 = main_chunk.add_constant(Value::Int(20));
        main_chunk.emit(Op::Constant(i10));
        main_chunk.emit(Op::Constant(i20));
        main_chunk.emit(Op::ConstructTuple(2));
        main_chunk.emit(Op::SetLocal(0));
        main_chunk.emit(Op::PushRef(0));
        main_chunk.emit(Op::Call(0, 1));
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::GetLocal(0));
        main_chunk.emit(Op::GetField(1));
        main_chunk.emit(Op::CallBuiltin(0, 1)); // println
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::Nil);
        main_chunk.emit(Op::Return);

        let program = make_program(vec![callee, main_chunk], 1);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
        assert_eq!(vm.stdout, "99\n");
    }

    #[test]
    fn set_field_ref_dataref_shared_mutation() {
        // DataRef uses force_mut (shared mutation), NOT make_mut (COW).
        // main: local 0 = DataRef{[10, 20]}; local 1 = clone (refcount=2)
        // PushRef(0); Call callee (sets field 1 to 99 through ref)
        // Both local 0 AND local 1 see the change (shared allocation).
        let mut callee = Chunk::new("callee", 1, 1);
        let i99 = callee.add_constant(Value::Int(99));
        callee.emit(Op::Constant(i99));
        callee.emit(Op::SetFieldRef(0, 1));
        callee.emit(Op::Nil);
        callee.emit(Op::Return);

        let mut main_chunk = Chunk::new("main", 2, 0);
        let data_ref = Value::DataRef(ManagedRc::new(StructData {
            type_id: 0,
            fields: vec![Value::Int(10), Value::Int(20)],
        }));
        let cdr = main_chunk.add_constant(data_ref);
        main_chunk.emit(Op::Constant(cdr));
        main_chunk.emit(Op::SetLocal(0));
        main_chunk.emit(Op::GetLocal(0)); // clone → refcount=2
        main_chunk.emit(Op::SetLocal(1));
        main_chunk.emit(Op::PushRef(0));
        main_chunk.emit(Op::Call(0, 1));
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::GetLocal(0));
        main_chunk.emit(Op::GetField(1));
        main_chunk.emit(Op::CallBuiltin(0, 1)); // println → "99"
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::GetLocal(1));
        main_chunk.emit(Op::GetField(1));
        main_chunk.emit(Op::CallBuiltin(0, 1)); // println → "99" (shared!)
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::Nil);
        main_chunk.emit(Op::Return);

        let program = make_program(vec![callee, main_chunk], 1);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
        assert_eq!(vm.stdout, "99\n99\n");
    }

    #[test]
    fn deref_write_full_reassignment() {
        // main: local 0 = Int(42); PushRef(0); Call callee; print local 0 field 0
        // callee: constructs Struct{[1, 2]}, DerefWrite(0) → main's local 0 is now the struct
        let mut callee = Chunk::new("callee", 1, 1);
        let i1 = callee.add_constant(Value::Int(1));
        let i2 = callee.add_constant(Value::Int(2));
        callee.emit(Op::Constant(i1));
        callee.emit(Op::Constant(i2));
        callee.emit(Op::ConstructStruct(0, 2));
        callee.emit(Op::DerefWrite(0));
        callee.emit(Op::Nil);
        callee.emit(Op::Return);

        let mut main_chunk = Chunk::new("main", 1, 0);
        let i42 = main_chunk.add_constant(Value::Int(42));
        main_chunk.emit(Op::Constant(i42));
        main_chunk.emit(Op::SetLocal(0));
        main_chunk.emit(Op::PushRef(0));
        main_chunk.emit(Op::Call(0, 1));
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::GetLocal(0));
        main_chunk.emit(Op::GetField(0));
        main_chunk.emit(Op::CallBuiltin(0, 1)); // println → "1"
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::Nil);
        main_chunk.emit(Op::Return);

        let program = make_program(vec![callee, main_chunk], 1);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
        assert_eq!(vm.stdout, "1\n");
    }

    // ── PathRef tests ──────────────────────────────────────────────────────────

    #[test]
    fn push_path_ref_creates_correct_value() {
        // main: local 0 = Struct{[Int(42), Int(99)]}
        //       PushPathRef(0, 1, [1,0,0,0]) → PathRef{root=0, depth=1}
        //       println → "<pathref@0[1]>"
        let mut chunk = Chunk::new("main", 1, 0);
        let i42 = chunk.add_constant(Value::Int(42));
        let i99 = chunk.add_constant(Value::Int(99));
        chunk.emit(Op::Constant(i42));
        chunk.emit(Op::Constant(i99));
        chunk.emit(Op::ConstructStruct(0, 2));
        chunk.emit(Op::SetLocal(0));
        chunk.emit(Op::PushPathRef(0, 1, [1, 0, 0, 0]));
        chunk.emit(Op::CallBuiltin(0, 1)); // println
        chunk.emit(Op::Pop);
        chunk.emit(Op::Nil);
        chunk.emit(Op::Return);

        let program = make_program(vec![chunk], 0);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
        assert_eq!(vm.stdout, "<pathref@0[1]>\n");
    }

    #[test]
    fn path_ref_deref_read_single_level() {
        // main: local 0 = Struct{[Int(42), Int(99)]}; PushPathRef(0, 1, [1,..]); Call callee
        // callee: DerefRead(0) → Int(99); println
        let mut callee = Chunk::new("callee", 1, 1);
        callee.emit(Op::DerefRead(0));
        callee.emit(Op::CallBuiltin(0, 1)); // println
        callee.emit(Op::Pop);
        callee.emit(Op::Nil);
        callee.emit(Op::Return);

        let mut main_chunk = Chunk::new("main", 1, 0);
        let i42 = main_chunk.add_constant(Value::Int(42));
        let i99 = main_chunk.add_constant(Value::Int(99));
        main_chunk.emit(Op::Constant(i42));
        main_chunk.emit(Op::Constant(i99));
        main_chunk.emit(Op::ConstructStruct(0, 2));
        main_chunk.emit(Op::SetLocal(0));
        main_chunk.emit(Op::PushPathRef(0, 1, [1, 0, 0, 0]));
        main_chunk.emit(Op::Call(0, 1));
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::Nil);
        main_chunk.emit(Op::Return);

        let program = make_program(vec![callee, main_chunk], 1);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
        assert_eq!(vm.stdout, "99\n");
    }

    #[test]
    fn path_ref_deref_read_two_levels() {
        // main: local 0 = Struct{[Struct{[Int(7), Int(8)]}]}
        //       PushPathRef(0, 2, [0, 1, 0, 0]); Call callee
        // callee: DerefRead(0) → Int(8); println
        let mut callee = Chunk::new("callee", 1, 1);
        callee.emit(Op::DerefRead(0));
        callee.emit(Op::CallBuiltin(0, 1)); // println
        callee.emit(Op::Pop);
        callee.emit(Op::Nil);
        callee.emit(Op::Return);

        let mut main_chunk = Chunk::new("main", 1, 0);
        let i7 = main_chunk.add_constant(Value::Int(7));
        let i8 = main_chunk.add_constant(Value::Int(8));
        main_chunk.emit(Op::Constant(i7));
        main_chunk.emit(Op::Constant(i8));
        main_chunk.emit(Op::ConstructStruct(0, 2)); // inner = Struct{[7, 8]}
        main_chunk.emit(Op::ConstructStruct(0, 1)); // outer = Struct{[inner]}
        main_chunk.emit(Op::SetLocal(0));
        main_chunk.emit(Op::PushPathRef(0, 2, [0, 1, 0, 0])); // outer.inner.field[1]
        main_chunk.emit(Op::Call(0, 1));
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::Nil);
        main_chunk.emit(Op::Return);

        let program = make_program(vec![callee, main_chunk], 1);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
        assert_eq!(vm.stdout, "8\n");
    }

    #[test]
    fn path_ref_deref_write() {
        // main: local 0 = Struct{[Int(10), Int(20)]}; PushPathRef(0, 1, [1,..]); Call callee
        //       After call: println(local 0 field 1) → "99"
        // callee: push Int(99); DerefWrite(0)
        let mut callee = Chunk::new("callee", 1, 1);
        let i99 = callee.add_constant(Value::Int(99));
        callee.emit(Op::Constant(i99));
        callee.emit(Op::DerefWrite(0));
        callee.emit(Op::Nil);
        callee.emit(Op::Return);

        let mut main_chunk = Chunk::new("main", 1, 0);
        let i10 = main_chunk.add_constant(Value::Int(10));
        let i20 = main_chunk.add_constant(Value::Int(20));
        main_chunk.emit(Op::Constant(i10));
        main_chunk.emit(Op::Constant(i20));
        main_chunk.emit(Op::ConstructStruct(0, 2));
        main_chunk.emit(Op::SetLocal(0));
        main_chunk.emit(Op::PushPathRef(0, 1, [1, 0, 0, 0]));
        main_chunk.emit(Op::Call(0, 1));
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::GetLocal(0));
        main_chunk.emit(Op::GetField(1));
        main_chunk.emit(Op::CallBuiltin(0, 1)); // println
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::Nil);
        main_chunk.emit(Op::Return);

        let program = make_program(vec![callee, main_chunk], 1);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
        assert_eq!(vm.stdout, "99\n");
    }

    #[test]
    fn path_ref_deref_write_nested() {
        // main: local 0 = Struct{[Struct{[Int(1), Int(2)]}]}
        //       PushPathRef(0, 2, [0, 0, ..]); Call callee
        //       After call: GetLocal(0) GetField(0) GetField(0) → println → "42"
        // callee: push Int(42); DerefWrite(0) — replaces outer.inner.field[0]
        let mut callee = Chunk::new("callee", 1, 1);
        let i42 = callee.add_constant(Value::Int(42));
        callee.emit(Op::Constant(i42));
        callee.emit(Op::DerefWrite(0));
        callee.emit(Op::Nil);
        callee.emit(Op::Return);

        let mut main_chunk = Chunk::new("main", 1, 0);
        let i1 = main_chunk.add_constant(Value::Int(1));
        let i2 = main_chunk.add_constant(Value::Int(2));
        main_chunk.emit(Op::Constant(i1));
        main_chunk.emit(Op::Constant(i2));
        main_chunk.emit(Op::ConstructStruct(0, 2)); // inner = Struct{[1, 2]}
        main_chunk.emit(Op::ConstructStruct(0, 1)); // outer = Struct{[inner]}
        main_chunk.emit(Op::SetLocal(0));
        main_chunk.emit(Op::PushPathRef(0, 2, [0, 0, 0, 0])); // outer.fields[0].fields[0]
        main_chunk.emit(Op::Call(0, 1));
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::GetLocal(0));
        main_chunk.emit(Op::GetField(0)); // get inner struct
        main_chunk.emit(Op::GetField(0)); // get field 0 → should be 42
        main_chunk.emit(Op::CallBuiltin(0, 1)); // println
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::Nil);
        main_chunk.emit(Op::Return);

        let program = make_program(vec![callee, main_chunk], 1);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
        assert_eq!(vm.stdout, "42\n");
    }

    #[test]
    fn path_ref_set_field_ref() {
        // main: local 0 = Struct{[Struct{[Int(1), Int(2)]}]}
        //       PushPathRef(0, 1, [0, ..]); Call callee
        //       After call: GetLocal(0) GetField(0) GetField(1) → println → "42"
        // callee: push Int(42); SetFieldRef(0, 1) — sets inner.fields[1] via PathRef path+extension
        let mut callee = Chunk::new("callee", 1, 1);
        let i42 = callee.add_constant(Value::Int(42));
        callee.emit(Op::Constant(i42));
        callee.emit(Op::SetFieldRef(0, 1)); // path=[0], extend with field_idx=1 → outer.fields[0].fields[1]
        callee.emit(Op::Nil);
        callee.emit(Op::Return);

        let mut main_chunk = Chunk::new("main", 1, 0);
        let i1 = main_chunk.add_constant(Value::Int(1));
        let i2 = main_chunk.add_constant(Value::Int(2));
        main_chunk.emit(Op::Constant(i1));
        main_chunk.emit(Op::Constant(i2));
        main_chunk.emit(Op::ConstructStruct(0, 2)); // inner = Struct{[1, 2]}
        main_chunk.emit(Op::ConstructStruct(0, 1)); // outer = Struct{[inner]}
        main_chunk.emit(Op::SetLocal(0));
        main_chunk.emit(Op::PushPathRef(0, 1, [0, 0, 0, 0])); // PathRef to outer.fields[0] (the inner struct)
        main_chunk.emit(Op::Call(0, 1));
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::GetLocal(0));
        main_chunk.emit(Op::GetField(0)); // get inner struct
        main_chunk.emit(Op::GetField(1)); // get field 1 → should be 42
        main_chunk.emit(Op::CallBuiltin(0, 1)); // println
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::Nil);
        main_chunk.emit(Op::Return);

        let program = make_program(vec![callee, main_chunk], 1);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
        assert_eq!(vm.stdout, "42\n");
    }

    #[test]
    fn path_ref_set_index_ref() {
        // main: local 0 = Struct{[Array([Int(10), Int(20), Int(30)])]}
        //       PushPathRef(0, 1, [0, ..]); Call callee
        //       After call: GetLocal(0) GetField(0) IndexGet(1) → println → "99"
        // callee: push Int(1) (index), push Int(99) (value), SetIndexRef(0)
        let mut callee = Chunk::new("callee", 1, 1);
        let i1 = callee.add_constant(Value::Int(1));
        let i99 = callee.add_constant(Value::Int(99));
        callee.emit(Op::Constant(i1)); // index
        callee.emit(Op::Constant(i99)); // new_value
        callee.emit(Op::SetIndexRef(0));
        callee.emit(Op::Nil);
        callee.emit(Op::Return);

        let mut main_chunk = Chunk::new("main", 1, 0);
        let i10 = main_chunk.add_constant(Value::Int(10));
        let i20 = main_chunk.add_constant(Value::Int(20));
        let i30 = main_chunk.add_constant(Value::Int(30));
        let idx1 = main_chunk.add_constant(Value::Int(1));
        main_chunk.emit(Op::Constant(i10));
        main_chunk.emit(Op::Constant(i20));
        main_chunk.emit(Op::Constant(i30));
        main_chunk.emit(Op::ConstructArray(3));
        main_chunk.emit(Op::ConstructStruct(0, 1)); // wrap array in a struct
        main_chunk.emit(Op::SetLocal(0));
        main_chunk.emit(Op::PushPathRef(0, 1, [0, 0, 0, 0])); // PathRef to struct.fields[0] (the array)
        main_chunk.emit(Op::Call(0, 1));
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::GetLocal(0));
        main_chunk.emit(Op::GetField(0)); // get the array
        main_chunk.emit(Op::Constant(idx1));
        main_chunk.emit(Op::IndexGet);
        main_chunk.emit(Op::CallBuiltin(0, 1)); // println
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::Nil);
        main_chunk.emit(Op::Return);

        let program = make_program(vec![callee, main_chunk], 1);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
        assert_eq!(vm.stdout, "99\n");
    }

    #[test]
    fn path_ref_cow_when_shared() {
        // main: local 0 = Struct{[Struct{[Int(10), Int(20)]}]}
        //       local 1 = clone of local 0 (refcount=2)
        //       PushPathRef(0, 1, [0,..]); Call callee (SetFieldRef(0,1) with 99)
        //       After call: local 0 inner field 1 → 99 (COW); local 1 inner field 1 → 20 (unchanged)
        let mut callee = Chunk::new("callee", 1, 1);
        let i99 = callee.add_constant(Value::Int(99));
        callee.emit(Op::Constant(i99));
        callee.emit(Op::SetFieldRef(0, 1)); // sets inner.fields[1] via PathRef
        callee.emit(Op::Nil);
        callee.emit(Op::Return);

        let mut main_chunk = Chunk::new("main", 2, 0);
        let i10 = main_chunk.add_constant(Value::Int(10));
        let i20 = main_chunk.add_constant(Value::Int(20));
        main_chunk.emit(Op::Constant(i10));
        main_chunk.emit(Op::Constant(i20));
        main_chunk.emit(Op::ConstructStruct(0, 2)); // inner = Struct{[10, 20]}
        main_chunk.emit(Op::ConstructStruct(0, 1)); // outer = Struct{[inner]}
        main_chunk.emit(Op::SetLocal(0));
        main_chunk.emit(Op::GetLocal(0)); // clone outer → refcount=2 on outer (inner still refcount=1)
        main_chunk.emit(Op::SetLocal(1));
        main_chunk.emit(Op::PushPathRef(0, 1, [0, 0, 0, 0])); // PathRef to local0.fields[0]
        main_chunk.emit(Op::Call(0, 1));
        main_chunk.emit(Op::Pop);
        // local 0 inner field 1 → 99
        main_chunk.emit(Op::GetLocal(0));
        main_chunk.emit(Op::GetField(0));
        main_chunk.emit(Op::GetField(1));
        main_chunk.emit(Op::CallBuiltin(0, 1)); // println → "99"
        main_chunk.emit(Op::Pop);
        // local 1 inner field 1 → 20 (COW ensured separate copy)
        main_chunk.emit(Op::GetLocal(1));
        main_chunk.emit(Op::GetField(0));
        main_chunk.emit(Op::GetField(1));
        main_chunk.emit(Op::CallBuiltin(0, 1)); // println → "20"
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::Nil);
        main_chunk.emit(Op::Return);

        let program = make_program(vec![callee, main_chunk], 1);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
        assert_eq!(vm.stdout, "99\n20\n");
    }

    #[test]
    fn path_ref_depth_4() {
        // main: 4 levels of nesting: Struct{[Struct{[Struct{[Struct{[Int(42)]}]}]}]}
        //       PushPathRef(0, 4, [0, 0, 0, 0]); Call callee
        // callee: DerefRead(0) → Int(42); println
        let mut callee = Chunk::new("callee", 1, 1);
        callee.emit(Op::DerefRead(0));
        callee.emit(Op::CallBuiltin(0, 1)); // println
        callee.emit(Op::Pop);
        callee.emit(Op::Nil);
        callee.emit(Op::Return);

        let mut main_chunk = Chunk::new("main", 1, 0);
        let i42 = main_chunk.add_constant(Value::Int(42));
        main_chunk.emit(Op::Constant(i42));
        main_chunk.emit(Op::ConstructStruct(0, 1)); // level 4: Struct{[42]}
        main_chunk.emit(Op::ConstructStruct(0, 1)); // level 3: Struct{[level4]}
        main_chunk.emit(Op::ConstructStruct(0, 1)); // level 2: Struct{[level3]}
        main_chunk.emit(Op::ConstructStruct(0, 1)); // level 1: Struct{[level2]}
        main_chunk.emit(Op::SetLocal(0));
        main_chunk.emit(Op::PushPathRef(0, 4, [0, 0, 0, 0]));
        main_chunk.emit(Op::Call(0, 1));
        main_chunk.emit(Op::Pop);
        main_chunk.emit(Op::Nil);
        main_chunk.emit(Op::Return);

        let program = make_program(vec![callee, main_chunk], 1);
        let mut vm = VM::new(&program);
        vm.run(&empty_registry()).unwrap();
        assert_eq!(vm.stdout, "42\n");
    }
}
