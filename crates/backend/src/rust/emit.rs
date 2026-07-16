use anvyx_frontend::air;

use super::{
    analysis,
    dataref_place::{DataRefPlaceDescriptor, DataRefPlaceDescriptors},
    native_call::{NativeArgAction, NativeCallPlan},
    place::RustPlaces,
    rep_policy::{LambdaTraceAction, LambdaVariantLayout, RirRustRepPolicy, RustTracePlan},
    retained_callbacks::{RetainedCallbackEmitter, RetainedCallbackSigPlan},
    rir::{
        RirCallArg, RirCallTarget, RirCellDecl, RirCellLifetime, RirCellRef, RirCellStorage,
        RirChild, RirCollectionAccess, RirCollectionFor, RirCollectionLoanScope,
        RirCollectionStorageKind, RirConstValue, RirDataRefId, RirDynCarrier, RirDynCarrierId,
        RirDynDispatchArm, RirDynReceiver, RirDynStorage, RirDynVariantId, RirEnum, RirEnumId,
        RirEnumRepr, RirExternKind, RirFlagStaticOp, RirFormatAlign, RirFormatKind, RirFormatSign,
        RirFormatSpec, RirFunction, RirIf, RirIterCountCheck, RirLambdaCapture,
        RirLambdaCaptureArg, RirLambdaCaptureKind, RirLambdaEnvFieldKind, RirLambdaEnvId,
        RirLambdaEnvLayout, RirLambdaId, RirLambdaSig, RirLambdaSigId, RirLambdaStorage,
        RirLocalId, RirLoop, RirLoopId, RirMapEntryMatch, RirMutPlaceAccess, RirMutPlaceArg,
        RirMutPlaceHandle, RirOperand, RirOptionMatch, RirOptionSubject, RirOrdinalAdapter,
        RirOrdinalPlan, RirParamAbi, RirParamSemantic, RirPatternAlternative, RirPatternBinding,
        RirPatternBindingMode, RirPatternMatch, RirPatternPath, RirPatternPathStep, RirPatternTest,
        RirPlace, RirPlaceRoot, RirProgram, RirProjection, RirRValue, RirRangeFor, RirRawEnumValue,
        RirResolvedCallTarget, RirScopedPlaceCellDecl, RirScopedPlaceCellRef, RirStmt,
        RirStringLiteralId, RirStructuredBlock, RirTerm, RirType, RirTypeId, RirVariant,
        RirVariantId, RirVariantKind, VerifiedRirProgram, native_arg_facts,
        native_dynamic_arg_facts, native_ty_is_resource_ref, stmt_child_blocks_any,
    },
    runtime_owner::RuntimeOwnerEmit,
    syntax::{
        FormatAlign, FormatKind, FormatSign, FormatSpec, binary_op, block_expr, comma, field_init,
        format_fragment, match_expr, rust_string, struct_lit, struct_variant,
        struct_variant_pattern, tuple_variant, tuple_variant_pattern, unary_op,
        unit_variant_pattern, variant_path,
    },
    target,
    value::RustValues,
    write::RustWriter,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RustSource {
    text: String,
}

impl RustSource {
    pub fn new(text: String) -> Self {
        Self { text }
    }

    pub fn as_str(&self) -> &str {
        &self.text
    }

    pub fn into_string(self) -> String {
        self.text
    }
}

pub fn emit(program: &VerifiedRirProgram<'_>) -> RustSource {
    let program = program.program();
    let mut cx = EmitCx {
        program,
        dataref_places: DataRefPlaceDescriptors::build(program),
        trace_plan: RustTracePlan::build(program),
        fallible_functions: analysis::fallible_functions(program),
        retained_callback_sigs: program.retained_callback_sigs(),
        provider_callback_sigs: program.provider_callback_sigs(),
        heap_callback_sigs: program.heap_callback_sigs(),
        w: RustWriter::default(),
        collection_loans: vec![],
    };
    cx.emit_program();
    RustSource::new(cx.w.finish())
}

struct ResolvedReceiver {
    expr: String,
    ty: RirTypeId,
    semantic: RirParamSemantic,
}

type PreparedNativeArgs = (Vec<String>, Vec<String>, Vec<(String, String, bool)>);

struct EmitCx<'a> {
    program: &'a RirProgram,
    dataref_places: DataRefPlaceDescriptors,
    trace_plan: RustTracePlan,
    fallible_functions: Vec<bool>,
    retained_callback_sigs: Vec<RirLambdaSigId>,
    provider_callback_sigs: Vec<RirLambdaSigId>,
    heap_callback_sigs: Vec<RirLambdaSigId>,
    w: RustWriter,
    collection_loans: Vec<ActiveCollectionLoan>,
}

struct ActiveCollectionLoan {
    root: RirCollectionAccess,
    version: String,
}

fn apply_ordinal_plan(iter: String, plan: &RirOrdinalPlan, values: &RustValues<'_>) -> String {
    plan.adapters
        .iter()
        .fold(iter, |iter, adapter| match adapter {
            RirOrdinalAdapter::Rev => target::iter_rev(&iter),
            RirOrdinalAdapter::Skip { count } => {
                target::iter_skip(&iter, &values.value_operand(count))
            }
            RirOrdinalAdapter::Take { count } => {
                target::iter_take(&iter, &values.value_operand(count))
            }
            RirOrdinalAdapter::StepBy { step } => {
                target::iter_step_by(&iter, &values.value_operand(step))
            }
        })
}

#[derive(Clone, Copy)]
enum IterFor<'a> {
    Range(&'a RirRangeFor),
    Collection(&'a RirCollectionFor),
}

impl EmitCx<'_> {
    fn collection_storage_heap_type(&self, value_ty: RirTypeId) -> String {
        let storage = self
            .program
            .collection_storage_for(value_ty)
            .expect("verified collection storage declaration");
        format!("statics.{}", storage.symbol.as_str())
    }

    fn emit_program(&mut self) {
        self.emit_ctx();
        for dataref in &self.program.datarefs {
            self.emit_dataref(dataref);
        }
        self.emit_dataref_place_descriptors();
        for env in &self.program.lambda_envs {
            self.emit_lambda_env(env);
        }
        for strukt in &self.program.structs {
            self.emit_struct(strukt);
        }
        for tuple in &self.program.tuples {
            self.emit_tuple(tuple);
        }
        for flag in &self.program.flags {
            self.emit_flag(flag);
        }
        for enm in &self.program.enums {
            self.emit_enum(enm);
        }
        for helper in &self.program.stringify_helpers {
            self.emit_stringify_helper(helper);
        }
        for sig in &self.program.lambda_sigs {
            self.emit_lambda_sig(sig);
        }
        self.emit_callback_registry();
        self.emit_callback_thunks();
        for function in &self.program.functions {
            self.emit_function(function);
        }
        if let Some(entry) = self.program.entry {
            self.emit_main(entry);
        }
    }

    fn emit_ctx(&mut self) {
        let statics = target::generated_statics_symbol(&self.program.ctx);
        let globals = target::generated_globals_symbol(&self.program.ctx);
        let owned_literals = self
            .program
            .string_literals
            .iter()
            .filter(|literal| literal.needs_owned)
            .collect::<Vec<_>>();
        let policy = RirRustRepPolicy::new(self.program);
        let retained_sigs = &self.retained_callback_sigs;
        let mut heap_types = self
            .program
            .datarefs
            .iter()
            .map(|dataref| {
                let register = target::heap_register(policy.dataref_storage_tracked(dataref));
                (
                    dataref.heap_type_symbol(),
                    policy.dataref_storage_ty(dataref),
                    register,
                )
            })
            .collect::<Vec<_>>();
        heap_types.extend(
            self.program
                .cells
                .iter()
                .filter(|cell| cell.storage == RirCellStorage::Heap)
                .map(|cell| {
                    let register = target::heap_register(policy.cell_storage_tracked(cell));
                    (
                        lambda_cell_heap_type_symbol(cell.id),
                        policy.cell_storage_ty(cell),
                        register,
                    )
                }),
        );
        heap_types.extend(self.program.lambda_envs.iter().map(|env| {
            let register = target::heap_register(policy.lambda_env_storage_tracked(env));
            (
                lambda_env_heap_type_symbol(env.id),
                policy.lambda_env_storage_ty(env),
                register,
            )
        }));
        heap_types.extend(self.program.collection_storages.iter().map(|storage| {
            let (storage_ty, register) = match storage.kind {
                RirCollectionStorageKind::List { elem_ty } => (
                    target::list_storage_ty(&policy.rust_storage_ty(elem_ty)),
                    target::heap_register(policy.list_storage_tracked(elem_ty)),
                ),
                RirCollectionStorageKind::Map { key_ty, value_ty } => (
                    target::map_storage_ty(
                        &policy.rust_storage_ty(key_ty),
                        &policy.rust_storage_ty(value_ty),
                    ),
                    target::heap_register(policy.map_storage_tracked(key_ty, value_ty)),
                ),
            };
            (storage.symbol.as_str().to_string(), storage_ty, register)
        }));
        heap_types.extend(retained_sigs.iter().map(|sig| {
            let plan = RetainedCallbackSigPlan::new(*sig);
            let record = plan.record_symbol();
            (
                plan.heap_type_field(),
                format!("{record}<'cx>"),
                target::heap_register(self.trace_plan.needs_lambda_sig_trace(*sig)),
            )
        }));

        self.w.block(format_args!("struct {statics}<'cx>"), |w| {
            for (heap_type, storage, _) in &heap_types {
                w.line(format_args!(
                    "{heap_type}: {},",
                    target::heap_type_ty(storage)
                ));
            }
            for literal in &owned_literals {
                w.line(target::string_literal_field_decl(literal.id));
            }
            w.line("_brand: std::marker::PhantomData<&'cx ()>,");
        });
        self.w.blank();
        self.w.block(format_args!("impl<'cx> {statics}<'cx>"), |w| {
            w.block(
                format_args!(
                    "fn new({}: &mut {}) -> Self",
                    if heap_types.is_empty() {
                        "_heap"
                    } else {
                        "heap"
                    },
                    target::heap_ty()
                ),
                |w| {
                    w.block("Self", |w| {
                        for (heap_type, storage, register) in &heap_types {
                            w.line(format_args!("{heap_type}: heap.{register}::<{storage}>(),"));
                        }
                        for literal in &owned_literals {
                            w.line(target::string_literal_init(
                                literal.id,
                                &rust_string(&literal.text),
                            ));
                        }
                        w.line("_brand: std::marker::PhantomData,");
                    });
                },
            );
        });
        self.w.blank();
        self.w.line(format_args!("struct {globals}<'cx> {{"));
        self.w.push_indent();
        for global in &self.program.globals {
            let slot_ty = target::global_slot_ty(&policy.rust_storage_ty(global.ty));
            self.w
                .line(format_args!("{}: {slot_ty},", global.slot_symbol.as_str()));
        }
        self.w.line("_brand: std::marker::PhantomData<&'cx ()>,");
        self.w.pop_indent();
        self.w.line("}");
        self.w.blank();
        let safepoint_param = if self.program.globals.is_empty() {
            "_safepoint"
        } else {
            "safepoint"
        };
        self.w.block(format_args!("impl<'cx> {globals}<'cx>"), |w| {
            w.block(
                format_args!(
                    "fn new({safepoint_param}: &{}) -> Self",
                    target::safepoint_state_ty()
                ),
                |w| {
                    w.block("Self", |w| {
                        for global in &self.program.globals {
                            w.line(format_args!(
                                "{}: {},",
                                global.slot_symbol.as_str(),
                                target::global_slot_new(global.name.as_str(), safepoint_param)
                            ));
                        }
                        w.line("_brand: std::marker::PhantomData,");
                    });
                },
            );
        });
        let traced_globals = self
            .program
            .globals
            .iter()
            .filter(|global| policy.type_owns_heap_edges(global.ty))
            .collect::<Vec<_>>();
        if !traced_globals.is_empty() {
            let ty = format!("{globals}<'cx>");
            self.w.block(
                format_args!("{}", target::trace_impl_header("<'cx>", &ty)),
                |w| {
                    w.line(format_args!("{} {{", target::trace_fn_header()));
                    w.indented(|w| {
                        for global in &traced_globals {
                            w.line(format_args!(
                                "{}::trace(&self.{}, visitor);",
                                target::trace_ty(),
                                global.slot_symbol.as_str()
                            ));
                        }
                    });
                    w.line("}");
                },
            );
            self.w.block(
                format_args!("impl<'cx> {}<'cx> for {ty}", target::trace_root_set_ty()),
                |w| {
                    w.block(
                        format_args!(
                            "fn validate_trace_roots(&self) -> {}",
                            target::result_ty("()")
                        ),
                        |w| {
                            for global in &traced_globals {
                                w.line(format_args!(
                                    "self.{}.validate_trace()?;",
                                    global.slot_symbol.as_str()
                                ));
                            }
                            w.line("Ok(())");
                        },
                    );
                },
            );
        }
        self.w.blank();
        let runtime = target::generated_runtime_symbol();
        let inner = target::generated_runtime_inner_symbol();
        self.w.block(format_args!("struct {runtime}<'cx>"), |w| {
            w.line(format_args!(
                "owner: {},",
                target::runtime_owner_handle_ty()
            ));
            w.line(format_args!(
                "inner: {},",
                target::pin_box_ty(&format!("{inner}<'cx>"))
            ));
        });
        self.w.blank();
        self.w.block(format_args!("struct {inner}<'cx>"), |w| {
            w.line(format_args!("statics: {statics}<'cx>,"));
            w.line(format_args!("globals: {globals}<'cx>,"));
            if !retained_sigs.is_empty() {
                w.line(format_args!(
                    "callbacks: {}<'cx>,",
                    target::generated_callback_registry_symbol()
                ));
            }
            w.line(format_args!("heap: {},", target::heap_ty()));
            w.line(format_args!("safepoint: {},", target::safepoint_state_ty()));
            w.line(format_args!("_pin: {},", target::phantom_pinned_ty()));
        });
        self.w.blank();
        self.w
            .block(format_args!("struct AnvEntry<'entry, 'cx>"), |w| {
                w.line(format_args!(
                    "heap: {},",
                    target::non_null_ty(&target::heap_ty())
                ));
                w.line(format_args!("statics: &'entry {statics}<'cx>,"));
                w.line(format_args!("globals: &'entry {globals}<'cx>,"));
                w.line(format_args!(
                    "safepoint: &'entry {},",
                    target::safepoint_state_ty()
                ));
                if !retained_sigs.is_empty() {
                    w.line(format_args!(
                        "owner: &'entry {},",
                        target::runtime_owner_handle_ty()
                    ));
                    w.line(format_args!(
                        "callbacks: {},",
                        target::non_null_ty(&format!(
                            "{}<'cx>",
                            target::generated_callback_registry_symbol()
                        ))
                    ));
                }
            });
        self.w.blank();
        self.w.block(format_args!("impl<'cx> {runtime}<'cx>"), |w| {
            w.block(
                format_args!("fn new(mut heap: {}) -> Self", target::heap_ty()),
                |w| {
                    w.line(format_args!(
                        "let owner = {}.expect(\"runtime owner id overflow\");",
                        target::runtime_owner_handle_new()
                    ));
                    w.line(format_args!("let statics = {statics}::new(&mut heap);"));
                    w.line(format_args!(
                        "let safepoint = {}::default();",
                        target::safepoint_state_ty()
                    ));
                    w.line(format_args!("let globals = {globals}::new(&safepoint);"));
                    if !retained_sigs.is_empty() {
                        w.line(format_args!(
                            "let callbacks = {}::default();",
                            target::generated_callback_registry_symbol()
                        ));
                    }
                    w.line("let mut runtime = Self {");
                    w.indented(|w| {
                        w.line("owner,");
                        w.line(format_args!("inner: {}", target::box_pin_struct_start(inner)));
                        w.indented(|w| {
                            w.line("statics,");
                            w.line("globals,");
                            if !retained_sigs.is_empty() {
                                w.line("callbacks,");
                            }
                            w.line("heap,");
                            w.line("safepoint,");
                            w.line(format_args!("_pin: {},", target::phantom_pinned_value()));
                        });
                        w.line("}),");
                    });
                    w.line("};");
                    w.block("unsafe", |w| {
                        w.line(format_args!(
                            "let inner = {};",
                            target::pin_get_unchecked_mut("runtime.inner.as_mut()")
                        ));
                        w.line(RuntimeOwnerEmit::attach_line(
                            "runtime.owner",
                            &format!("{}.cast()", target::non_null_from_mut("inner")),
                        ));
                    });
                    w.line("runtime");
                },
            );
            w.block(
                format_args!(
                    "fn with_entry<R>(&mut self, f: impl for<'entry> FnOnce(AnvEntry<'entry, 'cx>) -> {}) -> {}",
                    target::result_ty("R"),
                    target::result_ty("R")
                ),
                |w| {
                    w.line(RuntimeOwnerEmit::enter_current_line("owner_entry", "self.owner"));
                    w.line(format_args!(
                        "let inner = unsafe {{ {} }};",
                        target::non_null_cast_mut(
                            &target::owner_entry_ptr("owner_entry"),
                            &format!("{inner}<'cx>")
                        )
                    ));
                    w.block("f(AnvEntry", |w| {
                        w.line(format_args!(
                            "heap: {},",
                            target::non_null_from_mut("&mut inner.heap")
                        ));
                        w.line("statics: &inner.statics,");
                        w.line("globals: &inner.globals,");
                        w.line("safepoint: &inner.safepoint,");
                        if !retained_sigs.is_empty() {
                            w.line("owner: &self.owner,");
                            w.line(format_args!(
                                "callbacks: {},",
                                target::non_null_from_mut("&mut inner.callbacks")
                            ));
                        }
                    });
                    w.line(")");
                },
            );
        });
        self.w.blank();
        self.w
            .block(format_args!("impl<'cx> Drop for {runtime}<'cx>"), |w| {
                w.block("fn drop(&mut self)", |w| {
                    w.line(format_args!(
                        "if let Err(error) = {} {{",
                        target::owner_begin_shutdown("self.owner")
                    ));
                    w.indented(|w| {
                        w.line("panic!(\"runtime shutdown failed: {}\", error.message());");
                    });
                    w.line("}");
                });
            });
        self.w.blank();
    }

    fn has_retained_callbacks(&self) -> bool {
        !self.retained_callback_sigs.is_empty()
    }

    fn emit_callback_registry(&mut self) {
        RetainedCallbackEmitter::new(
            self.program,
            &self.trace_plan,
            &self.fallible_functions,
            &self.retained_callback_sigs,
            &self.provider_callback_sigs,
            &self.heap_callback_sigs,
            &mut self.w,
        )
        .emit_registry();
    }

    fn emit_callback_thunks(&mut self) {
        RetainedCallbackEmitter::new(
            self.program,
            &self.trace_plan,
            &self.fallible_functions,
            &self.retained_callback_sigs,
            &self.provider_callback_sigs,
            &self.heap_callback_sigs,
            &mut self.w,
        )
        .emit_thunks();
    }

    fn emit_main(&mut self, entry: super::rir::RirFunctionId) {
        let symbol = self.program.functions[entry.index()].symbol.as_str();
        let fallible = self.fallible_functions[entry.index()];
        let retained_callbacks = self.has_retained_callbacks();
        let ret = if fallible {
            format!(" -> {}", target::result_ty("()"))
        } else {
            String::new()
        };
        self.w.block(format_args!("fn main(){ret}"), |w| {
            w.line(format_args!("{}(|heap| {{", target::heap_scope_owned()));
            w.indented(|w| {
                w.line(format_args!(
                    "let mut runtime = {}::new(heap);",
                    target::generated_runtime_symbol()
                ));
                w.line("runtime.with_entry(|anv_entry| {");
                w.indented(|w| {
                    let rt = if self.program.globals.iter().any(|global| {
                        RirRustRepPolicy::new(self.program).type_owns_heap_edges(global.ty)
                    }) {
                        target::runtime_ctx_from_raw_with_trace_roots_and_safepoint(
                            "anv_entry.heap",
                            "anv_entry.globals",
                            "anv_entry.safepoint",
                        )
                    } else {
                        target::runtime_ctx_from_raw_with_safepoint(
                            "anv_entry.heap",
                            "anv_entry.safepoint",
                        )
                    };
                    w.line(format_args!("let mut rt = {rt};"));
                    let mut args = vec![
                        "&mut rt".to_string(),
                        "anv_entry.statics".to_string(),
                        "anv_entry.globals".to_string(),
                    ];
                    if retained_callbacks {
                        args.extend([
                            "anv_entry.owner".to_string(),
                            "anv_entry.callbacks".to_string(),
                        ]);
                    }
                    w.line(format_args!(
                        "let _ = {symbol}({}){};",
                        comma(args),
                        if fallible { "?" } else { "" }
                    ));
                    w.line("Ok(())");
                });
                if fallible {
                    w.line("})");
                } else {
                    w.line("}).expect(\"runtime entry failed\");");
                }
            });
            if fallible {
                w.line(format_args!("}})"));
            } else {
                w.line(format_args!("}});"));
            }
        });
    }

    fn emit_dataref(&mut self, dataref: &super::rir::RirDataRef) {
        let storage = dataref.storage_symbol();
        let policy = RirRustRepPolicy::new(self.program);
        let cx_dependent = policy.dataref_cx_dependent(dataref);
        let storage_lifetime = if cx_dependent { "<'cx>" } else { "" };
        let fields = dataref
            .fields
            .iter()
            .map(|field| (field.symbol.as_str(), self.ty(field.ty)))
            .collect::<Vec<_>>();
        if cx_dependent {
            self.w.line(target::trace_derive(&[]));
            self.w.line(target::trace_crate_attr(true));
        }
        self.w
            .block(format_args!("struct {storage}{storage_lifetime}"), |w| {
                for (field, ty) in &fields {
                    w.line(format_args!("{field}: {ty},"));
                }
            });
        self.w.blank();
        if policy.dataref_storage_tracked(dataref) && !cx_dependent {
            self.w.block(
                format_args!("unsafe impl<'cx> {}<'cx> for {storage}", target::trace_ty()),
                |w| {
                    w.line(format_args!(
                        "fn trace<D: {}<'cx>>(",
                        target::trace_driver_ty()
                    ));
                    w.indented(|w| {
                        w.line("&self,");
                        w.line(format_args!("_visitor: &mut {},", target::visitor_ty("D")));
                    });
                    w.line(format_args!(") {{"));
                    w.line(format_args!("}}"));
                },
            );
            self.w.blank();
        }
        self.w.line(format_args!(
            "type {}<'cx> = {};",
            dataref.symbol.as_str(),
            target::handle_ty(&policy.dataref_storage_ty(dataref))
        ));
        self.w.blank();
    }

    fn emit_dataref_place_descriptors(&mut self) {
        for descriptor in self.dataref_places.all() {
            Self::emit_dataref_place_descriptor(self.program, &mut self.w, descriptor);
        }
    }

    fn emit_dataref_place_descriptor(
        program: &RirProgram,
        w: &mut RustWriter,
        descriptor: &DataRefPlaceDescriptor,
    ) {
        let policy = RirRustRepPolicy::new(program);
        let dataref = &program.datarefs[descriptor.dataref.index()];
        let storage = policy.dataref_storage_ty(dataref);
        let payload = policy.rust_ty(descriptor.ty);
        let path = descriptor.storage_path(program);
        w.block(format_args!("struct {}<'cx>", descriptor.symbol), |w| {
            w.line(format_args!(
                "{}: {},",
                target::dataref_place_heap_type_field(),
                target::heap_type_ty(&storage)
            ));
        });
        w.blank();
        w.block(
            format_args!(
                "unsafe impl<'cx> {} for {}<'cx>",
                target::dataref_place_ops_ty(&payload),
                descriptor.symbol
            ),
            |w| {
                Self::emit_dataref_place_op(
                    w,
                    &target::DataRefPlaceOp::Access,
                    &payload,
                    &storage,
                    &path,
                );
                w.blank();
                Self::emit_dataref_place_op(
                    w,
                    &target::DataRefPlaceOp::Mutate,
                    &payload,
                    &storage,
                    &path,
                );
            },
        );
        w.blank();
    }

    fn emit_dataref_place_op(
        w: &mut RustWriter,
        op: &target::DataRefPlaceOp,
        payload: &str,
        storage: &str,
        path: &str,
    ) {
        let payload_ref = op.payload_ref(payload);
        w.line(format_args!("fn {}(", op.name()));
        w.indented(|w| {
            w.line("&self,");
            w.line(format_args!(
                "rt: &mut {},",
                target::runtime_ctx_ty_with("'_")
            ));
            w.line(format_args!("object: &{},", target::erased_handle_ty()));
            w.line(format_args!(
                "f: &mut dyn FnMut({payload_ref}) -> {},",
                target::result_ty("()")
            ));
        });
        w.line(format_args!(") -> {} {{", target::result_ty("()")));
        w.indented(|w| match op {
            target::DataRefPlaceOp::Access => {
                w.line(format_args!(
                    "let value = {}?;",
                    target::rt_heap_try_with_erased(
                        "rt",
                        "object",
                        &target::dataref_place_heap_type_access("self"),
                        "storage",
                        storage,
                        &format!("Ok({path}.clone())"),
                    )
                ));
                w.line("f(&value)");
            }
            target::DataRefPlaceOp::Mutate => {
                w.line(format_args!(
                    "let mut value = {}?;",
                    target::rt_heap_try_with_erased(
                        "rt",
                        "object",
                        &target::dataref_place_heap_type_access("self"),
                        "storage",
                        storage,
                        &format!("Ok({path}.clone())"),
                    )
                ));
                w.line("let result = f(&mut value);");
                w.line(format_args!(
                    "let writeback = {};",
                    target::rt_heap_try_with_erased_mut(
                        "rt",
                        "object",
                        &target::dataref_place_heap_type_access("self"),
                        "storage",
                        storage,
                        &format!("{{ {path} = value; Ok(()) }}"),
                    )
                ));
                w.line("match (result, writeback) {");
                w.indented(|w| {
                    w.line("(Ok(()), Ok(())) => Ok(()),");
                    w.line("(Err(error), _) | (_, Err(error)) => Err(error),");
                });
                w.line("}");
            }
        });
        w.line("}");
    }

    fn emit_lambda_env(&mut self, env: &RirLambdaEnvLayout) {
        let policy = RirRustRepPolicy::new(self.program);
        let lifetime = if policy.lambda_env_cx_dependent(env) {
            "<'cx>"
        } else {
            ""
        };
        if policy.lambda_env_storage_tracked(env) {
            self.w.line(target::trace_derive(&["Clone"]));
            self.w.line(target::trace_crate_attr(
                policy.lambda_env_cx_dependent(env),
            ));
        } else {
            self.w.line("#[derive(Clone)]");
        }
        self.w.block(
            format_args!("struct {}{lifetime}", env.symbol.as_str()),
            |w| {
                for field in &env.fields {
                    w.line(format_args!(
                        "{}: {},",
                        field.symbol.as_str(),
                        policy.lambda_env_field_ty(field)
                    ));
                }
            },
        );
        self.w.blank();
    }

    fn emit_struct(&mut self, strukt: &super::rir::RirStruct) {
        if let Some(path) = &strukt.native_path {
            if strukt.native_ref {
                self.w.line(target::anv_ref_alias(
                    strukt.symbol.as_str(),
                    &path.join("::"),
                ));
            } else {
                self.w.line(format_args!(
                    "type {} = {};",
                    strukt.symbol.as_str(),
                    path.join("::")
                ));
            }
            self.w.blank();
            return;
        }
        let policy = RirRustRepPolicy::new(self.program);
        self.emit_record_struct(
            strukt.symbol.as_str(),
            &strukt.fields,
            self.trace_plan.needs_struct_trace(strukt.id),
            policy.struct_cx_dependent(strukt),
            &policy.record_derives(&strukt.fields, strukt.copyable),
        );
    }

    fn emit_tuple(&mut self, tuple: &super::rir::RirTuple) {
        let policy = RirRustRepPolicy::new(self.program);
        self.emit_record_struct(
            tuple.symbol.as_str(),
            &tuple.fields,
            self.trace_plan.needs_tuple_trace(tuple.id),
            policy.tuple_cx_dependent(tuple),
            &policy.record_derives(&tuple.fields, tuple.copyable),
        );
    }

    fn emit_record_struct(
        &mut self,
        symbol: &str,
        fields: &[super::rir::RirField],
        trace: bool,
        cx_dependent: bool,
        derives: &[&str],
    ) {
        if trace {
            self.w.line(target::trace_derive(derives));
            self.w.line(target::trace_crate_attr(cx_dependent));
        } else {
            self.w.line(format_args!(
                "#[derive({})]",
                comma(derives.iter().map(|derive| (*derive).to_string()))
            ));
        }
        let policy = RirRustRepPolicy::new(self.program);
        let lifetime = if cx_dependent { "<'cx>" } else { "" };
        let fields = fields
            .iter()
            .map(|field| (field.symbol.as_str(), policy.rust_storage_ty(field.ty)))
            .collect::<Vec<_>>();
        self.w
            .block(format_args!("struct {symbol}{lifetime}"), |w| {
                for (field, ty) in &fields {
                    w.line(format_args!("{field}: {ty},"));
                }
            });
        self.w.blank();
    }

    fn emit_flag(&mut self, flag: &crate::rust::rir::RirFlag) {
        let derives = RirRustRepPolicy::new(self.program).flag_derives();
        self.w.line(target::trace_derive(&derives));
        self.w.line(target::trace_crate_attr(false));
        self.w.line(target::flag_type_decl(flag.symbol.as_str()));
        self.w
            .block(format_args!("impl {}", flag.symbol.as_str()), |w| {
                w.line(target::flag_known_bits_const(flag.known_bits));
                w.line(target::flag_bits_method());
                for member in &flag.members {
                    w.line(target::flag_member_const(
                        flag.symbol.as_str(),
                        member.symbol.as_str(),
                        member.value,
                    ));
                }
            });
        self.w.blank();
    }

    fn emit_enum(&mut self, enm: &RirEnum) {
        if let Some(path) = &enm.native_path {
            self.w.line(format_args!(
                "type {} = {};",
                enm.symbol.as_str(),
                path.join("::")
            ));
            self.w.blank();
            return;
        }
        let policy = RirRustRepPolicy::new(self.program);
        let cx_dependent = policy.enum_cx_dependent(enm);
        let needs_trace = self.trace_plan.needs_enum_trace(enm.id);
        let raw_repr = enm.repr == RirEnumRepr::RawInt && !enm.variants.is_empty();
        let derives = policy.enum_derives(enm);
        if needs_trace {
            self.w.line(target::trace_derive(&derives));
            self.w.line(target::trace_crate_attr(cx_dependent));
        } else if !derives.is_empty() {
            self.w.line(format_args!(
                "#[derive({})]",
                comma(derives.iter().map(|derive| (*derive).to_string()))
            ));
        }
        if raw_repr {
            self.w.line("#[repr(i64)]");
        }
        if self.program.dyn_carrier_for_enum(enm.id).is_some() {
            self.w.line(target::dynamic_carrier_repr_attr());
        }
        let lifetime = if cx_dependent { "<'cx>" } else { "" };
        let variants = enm
            .variants
            .iter()
            .map(|variant| match variant.kind {
                RirVariantKind::Unit => {
                    let raw = match &variant.raw_value {
                        Some(RirRawEnumValue::Int(value)) => format!(" = {value}"),
                        _ => String::new(),
                    };
                    format!("{}{raw},", variant.symbol.as_str())
                }
                RirVariantKind::Tuple => {
                    let dynamic = self
                        .program
                        .dyn_carrier_for_enum(enm.id)
                        .and_then(|carrier| carrier.variants.get(variant.id.index()));
                    let fields = comma(variant.fields.iter().map(|field| {
                        let ty = policy.rust_storage_ty(field.ty);
                        if dynamic.is_some_and(|variant| variant.storage == RirDynStorage::Boxed) {
                            format!("Box<{ty}>")
                        } else {
                            ty
                        }
                    }));
                    format!("{}({fields}),", variant.symbol.as_str())
                }
                RirVariantKind::Struct => {
                    let fields = variant.fields.iter().map(|field| {
                        field_init(field.symbol.as_str(), policy.rust_storage_ty(field.ty))
                    });
                    format!("{},", struct_lit(variant.symbol.as_str(), fields))
                }
            })
            .collect::<Vec<_>>();
        self.w.block(
            format_args!("enum {}{lifetime}", enm.symbol.as_str()),
            |w| {
                for variant in &variants {
                    w.line(format_args!("{variant}"));
                }
            },
        );
        if let Some(carrier) = self.program.dyn_carrier_for_enum(enm.id) {
            let function = &self.program.functions[0];
            let values = RustValues::new(self.program, function);
            let arms = carrier.variants.iter().map(|variant| {
                let rir_variant = &enm.variants[variant.id.index()];
                let path = variant_path(enm.symbol.as_str(), rir_variant.symbol.as_str());
                let source = if variant.storage == RirDynStorage::Boxed {
                    "payload.as_ref()"
                } else {
                    "payload"
                };
                let payload =
                    values.dyn_payload_from_ref(variant.concrete_ty, variant.payload, source);
                let payload = if variant.storage == RirDynStorage::Boxed {
                    format!("Box::new({payload})")
                } else {
                    payload
                };
                format!("{path}(payload) => {path}({payload})")
            });
            self.w.block(
                format_args!("impl{lifetime} Clone for {}{lifetime}", enm.symbol.as_str()),
                |w| {
                    w.block("fn clone(&self) -> Self", |w| {
                        w.line(match_expr("self", arms));
                    });
                },
            );
            if self.dyn_borrow_carrier_used(carrier) {
                self.emit_dyn_borrow_descriptor(carrier, enm);
            }
        }
        self.w.blank();
    }

    fn dyn_borrow_carrier_used(&self, carrier: &RirDynCarrier) -> bool {
        self.program.functions.iter().any(|function| {
            function
                .params
                .iter()
                .any(|param| param.abi == RirParamAbi::DynBorrow && param.ty == carrier.storage_ty)
        })
    }

    fn emit_dyn_borrow_descriptor(&mut self, carrier: &RirDynCarrier, enm: &RirEnum) {
        let policy = RirRustRepPolicy::new(self.program);
        let symbol = self.program.dyn_borrow_symbol(carrier.id);
        let weakenings = self
            .program
            .dyn_weakenings
            .iter()
            .filter(|w| w.target == carrier.id)
            .collect::<Vec<_>>();
        let mut variants = carrier
            .variants
            .iter()
            .map(|variant| {
                let name = enm.variants[variant.id.index()].symbol.as_str();
                let ty = policy.rust_ty(variant.concrete_ty);
                format!("{name}({}<'place, 'cx, {ty}>),", target::mut_place_ty())
            })
            .collect::<Vec<_>>();
        variants.push(format!(
            "Owned({}<'place, 'cx, {}>),",
            target::mut_place_ty(),
            policy.rust_ty(carrier.storage_ty)
        ));
        for weakening in &weakenings {
            let source = &self.program.dyn_carriers[weakening.source.index()];
            variants.push(format!(
                "OwnedFrom{}({}<'place, 'cx, {}>),",
                source.id.index(),
                target::mut_place_ty(),
                policy.rust_ty(source.storage_ty)
            ));
        }
        self.w.block(format!("enum {symbol}<'place, 'cx>"), |w| {
            for variant in &variants {
                w.line(variant);
            }
        });

        let mut reborrow_arms = carrier
            .variants
            .iter()
            .map(|variant| {
                let name = enm.variants[variant.id.index()].symbol.as_str();
                format!("Self::{name}(place) => {symbol}::{name}(place.reborrow())")
            })
            .collect::<Vec<_>>();
        reborrow_arms.push(format!(
            "Self::Owned(place) => {symbol}::Owned(place.reborrow())"
        ));
        for weakening in &weakenings {
            let source = weakening.source.index();
            reborrow_arms.push(format!(
                "Self::OwnedFrom{source}(place) => {symbol}::OwnedFrom{source}(place.reborrow())"
            ));
        }

        let methods = weakenings
            .iter()
            .filter(|weakening| {
                self.dyn_borrow_carrier_used(&self.program.dyn_carriers[weakening.source.index()])
            })
            .map(|weakening| self.dyn_borrow_weaken_method(weakening, enm, &symbol))
            .collect::<Vec<_>>();
        self.w
            .block(format!("impl<'place, 'cx> {symbol}<'place, 'cx>"), |w| {
                w.block(
                    format!("fn reborrow(&mut self) -> {symbol}<'_, 'cx>"),
                    |w| w.line(match_expr("self", reborrow_arms)),
                );
                for (header, body) in &methods {
                    w.block(header, |w| w.line(body));
                }
            });
    }

    fn dyn_borrow_weaken_method(
        &self,
        weakening: &super::rir::RirDynWeakening,
        target_enum: &RirEnum,
        target_symbol: &str,
    ) -> (String, String) {
        let source = &self.program.dyn_carriers[weakening.source.index()];
        let source_symbol = self.program.dyn_borrow_symbol(source.id);
        let RirType::Enum(source_enum) = self.program.types[source.storage_ty.index()] else {
            unreachable!("verified dynamic carrier storage")
        };
        let source_enum = &self.program.enums[source_enum.index()];
        let mut arms = weakening
            .arms
            .iter()
            .map(|arm| {
                let source_name = source_enum.variants[arm.source.index()].symbol.as_str();
                let target_name = target_enum.variants[arm.target.index()].symbol.as_str();
                format!(
                    "{source_symbol}::{source_name}(place) => {target_symbol}::{target_name}(place.reborrow())"
                )
            })
            .collect::<Vec<_>>();
        arms.push(format!(
            "{source_symbol}::Owned(place) => {target_symbol}::OwnedFrom{}(place.reborrow())",
            source.id.index()
        ));
        for ancestor in self
            .program
            .dyn_weakenings
            .iter()
            .filter(|candidate| candidate.target == source.id)
        {
            let ancestor = ancestor.source.index();
            arms.push(format!(
                "{source_symbol}::OwnedFrom{ancestor}(place) => {target_symbol}::OwnedFrom{ancestor}(place.reborrow())"
            ));
        }
        let header = format!(
            "fn weaken_from_{}<'borrow>(source: &'borrow mut {source_symbol}<'_, 'cx>) -> {target_symbol}<'borrow, 'cx>",
            source.id.index()
        );
        (header, match_expr("source", arms))
    }

    fn emit_stringify_helper(&mut self, helper: &super::rir::RirStringifyHelper) {
        match &helper.kind {
            super::rir::RirStringifyHelperKind::Struct(_) => {
                self.emit_struct_stringify_helper(helper);
            }
            super::rir::RirStringifyHelperKind::Enum { enm, variants } => {
                self.emit_enum_stringify_helper(helper, *enm, variants);
            }
            super::rir::RirStringifyHelperKind::Flag {
                flag,
                empty,
                members,
            } => self.emit_flag_stringify_helper(helper, *flag, *empty, members),
        }
    }

    fn stringify_helper_header(
        &self,
        helper: &super::rir::RirStringifyHelper,
    ) -> (String, analysis::ContextUse, bool) {
        let ctx_use = analysis::stringify_helper_context_use(self.program, helper);
        let fallible =
            analysis::stringify_helper_fallible(self.program, &self.fallible_functions, helper);
        let return_ty = if fallible {
            target::result_ty(&target::anv_string_ty())
        } else {
            target::anv_string_ty().clone()
        };
        let header = format!(
            "fn {}<'cx, 'rt>({}: {}, {}: {}, {}: {}, value: &{}) -> {return_ty}",
            helper.symbol.as_str(),
            target::runtime_param(ctx_use.rt),
            target::runtime_ctx_ref_ty(),
            target::statics_param(ctx_use.statics),
            target::statics_ref_ty(target::generated_statics_symbol(&self.program.ctx)),
            target::globals_param(ctx_use.globals),
            target::globals_ref_ty(target::generated_globals_symbol(&self.program.ctx)),
            self.ty(helper.ty),
        );
        (header, ctx_use, fallible)
    }

    fn emit_enum_stringify_helper(
        &mut self,
        helper: &super::rir::RirStringifyHelper,
        enum_id: RirEnumId,
        plans: &[super::rir::RirEnumStringifyVariant],
    ) {
        let enm = &self.program.enums[enum_id.index()];
        let (header, ctx_use, fallible) = self.stringify_helper_header(helper);
        let mut arms = vec![];
        for (variant, plan) in enm.variants.iter().zip(plans) {
            let names = (0..variant.fields.len())
                .map(|index| format!("f{index}"))
                .collect::<Vec<_>>();
            let path = variant_path(enm.symbol.as_str(), variant.symbol.as_str());
            let pattern = match variant.kind {
                RirVariantKind::Unit => path,
                RirVariantKind::Tuple => tuple_variant(&path, names.clone()),
                RirVariantKind::Struct => {
                    let fields = variant
                        .fields
                        .iter()
                        .zip(&names)
                        .map(|(field, name)| field_init(field.symbol.as_str(), name));
                    struct_variant(&path, fields)
                }
            };
            if variant.kind == RirVariantKind::Unit {
                let value = target::string_literal_share(target::statics_param_name(), plan.label);
                arms.push((
                    pattern,
                    vec![],
                    if fallible {
                        format!("Ok({value})")
                    } else {
                        value
                    },
                ));
                continue;
            }
            let mut lines = vec![
                format!("let mut out = {};", target::anv_string_builder()),
                format!(
                    "out.push_str({});",
                    target::string_literal_str(target::statics_param_name(), plan.label)
                ),
            ];
            lines.push(match variant.kind {
                RirVariantKind::Tuple => "out.push('(');".to_string(),
                RirVariantKind::Struct => "out.push_str(\" { \" );".to_string(),
                RirVariantKind::Unit => unreachable!(),
            });
            for (index, (field, name)) in variant.fields.iter().zip(&names).enumerate() {
                if index > 0 {
                    lines.push("out.push_str(\", \" );".to_string());
                }
                if variant.kind == RirVariantKind::Struct {
                    lines.push(format!(
                        "out.push_str({});",
                        target::string_literal_str(
                            target::statics_param_name(),
                            plan.field_labels[index],
                        )
                    ));
                }
                lines.push(self.stringify_push(field.ty, name, ctx_use, true));
            }
            lines.push(match variant.kind {
                RirVariantKind::Tuple => "out.push(')');".to_string(),
                RirVariantKind::Struct => "out.push_str(\" }\");".to_string(),
                RirVariantKind::Unit => unreachable!(),
            });
            arms.push((
                pattern,
                lines,
                if fallible { "Ok(out)" } else { "out" }.to_string(),
            ));
        }
        self.w.block(format_args!("{header}"), |w| {
            w.block("match value", |w| {
                for (pattern, lines, tail) in &arms {
                    w.block(format_args!("{pattern} =>"), |w| {
                        for line in lines {
                            w.line(line);
                        }
                        w.line(tail);
                    });
                }
            });
        });
        self.w.blank();
    }

    fn emit_flag_stringify_helper(
        &mut self,
        helper: &super::rir::RirStringifyHelper,
        flag_id: super::rir::RirFlagId,
        empty: RirStringLiteralId,
        names: &[RirStringLiteralId],
    ) {
        let flag = &self.program.flags[flag_id.index()];
        let (header, _, _) = self.stringify_helper_header(helper);
        let exact = flag
            .members
            .iter()
            .zip(names)
            .map(|(member, name)| {
                (
                    member.value,
                    target::string_literal_share(target::statics_param_name(), *name),
                )
            })
            .collect::<Vec<_>>();
        let has_named_zero = flag.members.iter().any(|member| member.value == 0);
        let atomic = flag
            .members
            .iter()
            .zip(names)
            .filter(|(member, _)| member.atomic)
            .map(|(member, name)| {
                (
                    member.value,
                    target::string_literal_str(target::statics_param_name(), *name),
                )
            })
            .collect::<Vec<_>>();
        let empty = target::string_literal_share(target::statics_param_name(), empty);
        self.w.block(format_args!("{header}"), |w| {
            w.block(format_args!("match {}", target::flag_bits("value")), |w| {
                for (bits, value) in &exact {
                    w.line(format_args!("{bits} => {value},"));
                }
                if !has_named_zero {
                    w.line(format_args!("0 => {empty},"));
                }
                w.block("bits =>", |w| {
                    w.line(format_args!(
                        "let mut out = {};",
                        target::anv_string_builder()
                    ));
                    let mut earlier = 0;
                    for (bits, name) in &atomic {
                        w.block(format_args!("if bits & {bits} != 0"), |w| {
                            if earlier != 0 {
                                w.block(format_args!("if bits & {earlier} != 0"), |w| {
                                    w.line("out.push_str(\" | \" );");
                                });
                            }
                            w.line(format_args!("out.push_str({name});"));
                        });
                        earlier |= *bits;
                    }
                    w.line("out");
                });
            });
        });
        self.w.blank();
    }

    fn emit_struct_stringify_helper(&mut self, helper: &super::rir::RirStringifyHelper) {
        let super::rir::RirStringifyHelperKind::Struct(struct_id) = &helper.kind else {
            unreachable!("verified struct stringify helper")
        };
        let strukt = &self.program.structs[struct_id.index()];
        let (header, ctx_use, fallible) = self.stringify_helper_header(helper);
        let display = rust_string(strukt.display.as_str());
        let fields = strukt
            .fields
            .iter()
            .map(|field| {
                let name = field.symbol.as_str().to_string();
                let push = self.stringify_push(field.ty, &format!("value.{name}"), ctx_use, false);
                (name, push)
            })
            .collect::<Vec<_>>();

        self.w.block(format_args!("{header}"), |w| {
            w.line(format_args!(
                "let mut out = {};",
                target::anv_string_builder()
            ));
            w.line(format_args!("out.push_str({display});"));
            w.line("out.push('(');");
            for (index, (field, push)) in fields.iter().enumerate() {
                if index > 0 {
                    w.line("out.push_str(\", \");");
                }
                w.line(format_args!(
                    "out.push_str({});",
                    rust_string(&format!("{field}: "))
                ));
                w.line(push);
            }
            w.line("out.push(')');");
            if fallible {
                w.line("Ok(out)");
            } else {
                w.line("out");
            }
        });
        self.w.blank();
    }

    fn stringify_nested_call(
        &self,
        kind: super::rir::RirStringifyReqKind,
        value: &str,
        ctx_use: analysis::ContextUse,
        borrowed: bool,
    ) -> String {
        match kind {
            super::rir::RirStringifyReqKind::Helper(helper) => {
                let helper = &self.program.stringify_helpers[helper.index()];
                let arg = if borrowed {
                    value.to_string()
                } else {
                    format!("&{value}")
                };
                let call = format!(
                    "{}({}, {}, {}, {arg})",
                    helper.symbol.as_str(),
                    target::runtime_param(ctx_use.rt),
                    target::statics_param(ctx_use.statics),
                    target::globals_param(ctx_use.globals),
                );
                if analysis::stringify_helper_fallible(
                    self.program,
                    &self.fallible_functions,
                    helper,
                ) {
                    format!("{call}?")
                } else {
                    call
                }
            }
            super::rir::RirStringifyReqKind::Override { function, mode } => {
                let symbol = self.program.functions[function.index()].symbol.as_str();
                let arg = match (mode, borrowed) {
                    (RirParamSemantic::Value, true) => format!("*{value}"),
                    (RirParamSemantic::Value, false) | (RirParamSemantic::SharedBorrow, true) => {
                        value.to_string()
                    }
                    (RirParamSemantic::SharedBorrow, false) => format!("&{value}"),
                    _ => unreachable!("verified stringify override mode"),
                };
                let call = if self.has_retained_callbacks() {
                    target::retained_generated_call(symbol, [arg])
                } else {
                    target::generated_call(symbol, [arg])
                };
                if self.fallible_functions[function.index()] {
                    format!("{call}?")
                } else {
                    call
                }
            }
        }
    }

    fn stringify_push(
        &self,
        ty: RirTypeId,
        value: &str,
        ctx_use: analysis::ContextUse,
        borrowed: bool,
    ) -> String {
        match self.program.types[ty.index()] {
            RirType::Float => {
                let value = if borrowed {
                    format!("*{value}")
                } else {
                    value.to_string()
                };
                format!("{};", target::anv_string_push_float("out", &value))
            }
            RirType::Int | RirType::Bool | RirType::Char => format!(
                "std::fmt::Write::write_fmt(&mut out, format_args!(\"{{}}\", {value})).unwrap();"
            ),
            RirType::String => format!("out.push_str({value}.as_str());"),
            RirType::Struct(_) | RirType::Enum(_) | RirType::Flag(_) => {
                let req = self
                    .program
                    .stringify_req(ty)
                    .expect("verified stringify field requirement");
                let call = self.stringify_nested_call(req.kind, value, ctx_use, borrowed);
                format!("out.push_str(({call}).as_str());")
            }
            RirType::Void
            | RirType::Tuple(_)
            | RirType::DataRef(_)
            | RirType::Array { .. }
            | RirType::List(_)
            | RirType::Map { .. }
            | RirType::Option(_)
            | RirType::Slice(_)
            | RirType::Lambda(_) => unreachable!("verified stringify helper field"),
        }
    }

    fn emit_lambda_sig(&mut self, sig: &RirLambdaSig) {
        let policy = RirRustRepPolicy::new(self.program);
        let layout = policy.lambda_sig_layout(sig.id);
        let symbol = policy.lambda_sig_symbol(sig.id);
        let retained_callbacks = self.has_retained_callbacks();
        let mut params = vec![
            format!("rt: {}", target::runtime_ctx_ref_ty()),
            format!(
                "statics: {}",
                target::statics_ref_ty(target::generated_statics_symbol(&self.program.ctx))
            ),
            format!(
                "globals: {}",
                target::globals_ref_ty(target::generated_globals_symbol(&self.program.ctx))
            ),
        ];
        if retained_callbacks {
            params.extend([
                format!(
                    "{}: &{}",
                    target::owner_param_name(),
                    target::runtime_owner_handle_ty()
                ),
                format!(
                    "{}: {}",
                    target::callbacks_param_name(),
                    target::non_null_ty(&format!(
                        "{}<'cx>",
                        target::generated_callback_registry_symbol()
                    ))
                ),
            ]);
        }
        params.extend(sig.params.iter().enumerate().map(|(index, param)| {
            format!(
                "arg_{index}: {}",
                policy.callable_param_ty(param.ty, param.abi, param.escape)
            )
        }));
        let fallible = layout
            .variants
            .iter()
            .any(|variant| self.fallible_functions[variant.function.index()]);
        let ret = self.lambda_sig_ret_ty(sig, fallible);
        let self_arg = layout.self_arg();
        let call_lifetimes = if policy.lambda_sig_needs_ctx_lifetime(sig.id) {
            "<'rt>"
        } else {
            "<'cx, 'rt>"
        };
        let header = if ret == "()" {
            format!("fn call{call_lifetimes}({self_arg}, {})", comma(params))
        } else {
            format!(
                "fn call{call_lifetimes}({self_arg}, {}) -> {ret}",
                comma(params)
            )
        };
        let arity = sig.params.len();
        let program = self.program;
        let lifetime = policy.lambda_sig_impl_generics(sig.id);

        let trace = self.trace_plan.needs_lambda_sig_trace(sig.id);
        if policy.lambda_sig_copyable(sig.id) {
            self.w.line("#[derive(Clone, Copy)]");
        } else if policy.lambda_sig_cloneable(sig.id) {
            self.w.line("#[derive(Clone)]");
        }
        self.w.block(format_args!("enum {symbol}{lifetime}"), |w| {
            for variant in &layout.variants {
                if let RirLambdaStorage::HeapEnv { env } = variant.storage {
                    let env = &self.program.lambda_envs[env.index()];
                    w.line(format_args!(
                        "{} {{ env: {} }},",
                        lambda_variant(variant.id),
                        target::handle_ty(&policy.lambda_env_storage_ty(env))
                    ));
                } else if variant.captures.is_empty() {
                    w.line(format_args!("{},", lambda_variant(variant.id)));
                } else {
                    let fields = variant
                        .captures
                        .iter()
                        .enumerate()
                        .map(|(index, capture)| {
                            format!(
                                "c{index}: {}",
                                policy.capture_field_ty(capture.ty, capture.abi)
                            )
                        })
                        .collect::<Vec<_>>();
                    w.line(format_args!(
                        "{} {{ {} }},",
                        lambda_variant(variant.id),
                        comma(fields)
                    ));
                }
            }
        });
        self.w.blank();
        if trace {
            self.emit_lambda_sig_trace_impl(sig.id, &layout.variants);
        }
        let fallible_functions = &self.fallible_functions;
        let body_call = |function: &RirFunction, capture_args: Vec<String>| {
            let args = capture_args
                .into_iter()
                .chain((0..arity).map(|index| format!("arg_{index}")));
            let call = if retained_callbacks {
                target::retained_generated_call(function.symbol.as_str(), args)
            } else {
                target::generated_call(function.symbol.as_str(), args)
            };
            if fallible && !fallible_functions[function.id.index()] {
                format!("Ok({call})")
            } else {
                call
            }
        };
        self.w
            .block(format_args!("impl{lifetime} {symbol}{lifetime}"), |w| {
                w.line(format_args!("{header} {{"));
                w.indented(|w| {
                    w.line("match self {");
                    w.indented(|w| {
                        for variant_layout in &layout.variants {
                            let function = &program.functions[variant_layout.function.index()];
                            let variant = lambda_variant(variant_layout.id);
                            match variant_layout.storage {
                                RirLambdaStorage::HeapEnv { env } => {
                                    let env = &program.lambda_envs[env.index()];
                                    w.line(format_args!("Self::{variant} {{ env }} => {{"));
                                    w.indented(|w| {
                                        let values = RustValues::new(program, function);
                                        for (index, field) in env.fields.iter().enumerate() {
                                            let capture = &variant_layout.captures[index];
                                            let source = format!("&env.{}", field.symbol.as_str());
                                            let value = match field.kind {
                                                RirLambdaEnvFieldKind::Value => {
                                                    values.value_from_ref(capture.ty, &source)
                                                }
                                                RirLambdaEnvFieldKind::HeapCell { .. } => {
                                                    format!("env.{}.clone()", field.symbol.as_str())
                                                }
                                            };
                                            w.line(format_args!(
                                                "let c{index} = {};",
                                                target::rt_heap_with("rt", "env", "env", &value)
                                            ));
                                        }
                                        let capture_args = (0..variant_layout.captures.len())
                                            .map(|index| format!("c{index}"))
                                            .collect();
                                        w.line(body_call(function, capture_args));
                                    });
                                    w.line("},");
                                }
                                _ if variant_layout.captures.is_empty() => {
                                    w.line(format_args!(
                                        "Self::{variant} => {},",
                                        body_call(function, vec![])
                                    ));
                                }
                                _ => {
                                    let fields = (0..variant_layout.captures.len())
                                        .map(|index| format!("c{index}"))
                                        .collect::<Vec<_>>();
                                    let capture_args = variant_layout
                                        .captures
                                        .iter()
                                        .enumerate()
                                        .map(|(index, capture)| {
                                            lambda_capture_call_arg(index, capture)
                                        })
                                        .collect();
                                    w.line(format_args!(
                                        "Self::{variant} {{ {} }} => {},",
                                        comma(fields),
                                        body_call(function, capture_args)
                                    ));
                                }
                            }
                        }
                    });
                    w.line("}");
                });
                w.line("}");
            });
        if self.lambda_sig_used_as_scoped_arg(sig.id) {
            self.emit_scoped_lambda_thunk(sig, fallible);
        }
        self.w.blank();
    }

    fn lambda_sig_used_as_scoped_arg(&self, sig: RirLambdaSigId) -> bool {
        self.program
            .functions
            .iter()
            .any(|function| block_uses_scoped_lambda_sig(&function.body, sig))
    }

    fn emit_lambda_sig_trace_impl(
        &mut self,
        sig: RirLambdaSigId,
        variants: &[LambdaVariantLayout<'_>],
    ) {
        let policy = RirRustRepPolicy::new(self.program);
        let symbol = policy.lambda_sig_symbol(sig);
        let lifetime = policy.lambda_sig_impl_generics(sig);
        let ty = format!("{symbol}{lifetime}");
        self.w.block(
            format_args!("{}", target::trace_impl_header(lifetime, &ty)),
            |w| {
                w.line(format_args!("{} {{", target::trace_fn_header()));
                w.indented(|w| {
                    w.line("match self {");
                    w.indented(|w| {
                        for variant_layout in variants {
                            let variant = lambda_variant(variant_layout.id);
                            match &variant_layout.trace_action {
                                LambdaTraceAction::HeapEnv => {
                                    debug_assert!(matches!(
                                        variant_layout.storage,
                                        RirLambdaStorage::HeapEnv { .. }
                                    ));
                                    w.line(format_args!(
                                        "Self::{variant} {{ env }} => {}::trace(env, visitor),",
                                        target::trace_ty()
                                    ));
                                }
                                LambdaTraceAction::HeapCellCaptures(cells) => {
                                    let fields = (0..variant_layout.captures.len())
                                        .map(|index| format!("c{index}"))
                                        .collect::<Vec<_>>();
                                    w.line(format_args!(
                                        "Self::{variant} {{ {} }} => {{",
                                        comma(fields)
                                    ));
                                    w.indented(|w| {
                                        for index in cells {
                                            w.line(format_args!(
                                                "{}::trace(c{index}, visitor);",
                                                target::trace_ty()
                                            ));
                                        }
                                    });
                                    w.line("},");
                                }
                                LambdaTraceAction::Noop if variant_layout.captures.is_empty() => {
                                    w.line(format_args!("Self::{variant} => {{}},"));
                                }
                                LambdaTraceAction::Noop => {
                                    w.line(format_args!("Self::{variant} {{ .. }} => {{}},"));
                                }
                            }
                        }
                    });
                    w.line("}");
                });
                w.line("}");
            },
        );
        self.w.blank();
    }

    fn emit_scoped_lambda_thunk(&mut self, sig: &RirLambdaSig, fallible: bool) {
        let policy = RirRustRepPolicy::new(self.program);
        let symbol = policy.lambda_sig_symbol(sig.id);
        let lifetime = policy.lambda_sig_impl_generics(sig.id);
        let (args_ty, ret_ty) = policy.scoped_lambda_sig_args_ret(sig.id);
        let result_ty = target::result_ty(&ret_ty);
        let args = (0..sig.params.len())
            .map(|index| format!("arg_{index}"))
            .collect::<Vec<_>>();
        let destructure = match args.as_slice() {
            [] => "let () = args;".to_string(),
            [arg] => format!("let ({arg},) = args;"),
            _ => format!("let ({}) = args;", comma(args.clone())),
        };
        let needs_ctx_lifetime = policy.lambda_sig_needs_ctx_lifetime(sig.id);
        let ctx_lifetime = if needs_ctx_lifetime {
            "'cx"
        } else {
            "'scoped_cx"
        };
        let rt_ty = target::runtime_ctx_ty_with_lifetimes(ctx_lifetime, "'_");
        let statics_ty = format!(
            "{}<{ctx_lifetime}>",
            target::generated_statics_symbol(&self.program.ctx)
        );
        let globals_ty = format!(
            "{}<{ctx_lifetime}>",
            target::generated_globals_symbol(&self.program.ctx)
        );
        let thunk_generics = if needs_ctx_lifetime {
            ""
        } else {
            "<'scoped_cx>"
        };
        let thunk = target::scoped_lambda_thunk();
        let state_ty = target::non_null_unit_ty();
        let retained_callbacks = self.has_retained_callbacks();
        let state_tuple = if retained_callbacks {
            format!("(Self, {state_ty}, {state_ty}, {state_ty}, {state_ty}, {state_ty})")
        } else {
            format!("(Self, {state_ty}, {state_ty}, {state_ty})")
        };
        self.w
            .block(format_args!("impl{lifetime} {symbol}{lifetime}"), |w| {
                w.block(
                    format_args!(
                        "unsafe fn {thunk}{thunk_generics}(state: {state_ty}, args: {args_ty}) -> {result_ty}"
                    ),
                    |w| {
                        w.line(format_args!("let state = unsafe {{ &mut *state.as_ptr().cast::<{state_tuple}>() }};"));
                        w.line(format_args!(
                            "let rt = unsafe {{ state.1.cast::<{rt_ty}>().as_mut() }};"
                        ));
                        w.line(format_args!(
                            "let statics = unsafe {{ state.2.cast::<{statics_ty}>().as_ref() }};"
                        ));
                        w.line(format_args!(
                            "let globals = unsafe {{ state.3.cast::<{globals_ty}>().as_ref() }};"
                        ));
                        w.line(format_args!("{};", target::runtime_validate_reentry("rt")));
                        if retained_callbacks {
                            let owner_ty = target::runtime_owner_handle_ty();
                            let callbacks_ty = format!(
                                "{}<{ctx_lifetime}>",
                                target::generated_callback_registry_symbol()
                            );
                            w.line(format_args!(
                                "let owner = unsafe {{ state.4.cast::<{owner_ty}>().as_ref() }};"
                            ));
                            w.line(format_args!(
                                "let callbacks = state.5.cast::<{callbacks_ty}>();"
                            ));
                            w.line(RuntimeOwnerEmit::enter_current_line(
                                "__anv_scoped_entry",
                                "owner",
                            ));
                        }
                        w.line(destructure);
                        let call_args = if retained_callbacks {
                            target::retained_generated_call_args(args)
                        } else {
                            target::generated_call_args(args)
                        };
                        let call = format!("state.0.call({})", comma(call_args));
                        if fallible {
                            if retained_callbacks {
                                w.line(format_args!("let __anv_scoped_result = {call};"));
                                w.line(RuntimeOwnerEmit::drop_line("__anv_scoped_entry"));
                                w.line("__anv_scoped_result");
                            } else {
                                w.line(call);
                            }
                        } else if ret_ty == "()" {
                            w.line(format_args!("{call};"));
                            if retained_callbacks {
                                w.line(RuntimeOwnerEmit::drop_line("__anv_scoped_entry"));
                            }
                            w.line("Ok(())");
                        } else if retained_callbacks {
                            w.line(format_args!("let __anv_scoped_result = {call};"));
                            w.line(RuntimeOwnerEmit::drop_line("__anv_scoped_entry"));
                            w.line("Ok(__anv_scoped_result)");
                        } else {
                            w.line(format_args!("Ok({call})"));
                        }
                    },
                );
            });
    }

    fn lambda_sig_fallible(&self, sig: &RirLambdaSig) -> bool {
        self.program
            .lambdas_for_sig(sig.id)
            .any(|lambda| self.fallible_functions[lambda.function.index()])
    }

    fn lambda_sig_ret_ty(&self, sig: &RirLambdaSig, fallible: bool) -> String {
        let ret = RirRustRepPolicy::new(self.program).callable_ret_ty(sig.ret);
        if !fallible {
            return ret;
        }
        if ret == "()" {
            target::result_ty("()")
        } else {
            target::result_ty(&ret)
        }
    }

    fn emit_function(&mut self, function: &RirFunction) {
        let ctx_use = analysis::function_context_use(self.program, function);
        let policy = RirRustRepPolicy::new(self.program);
        let retained_callbacks = self.has_retained_callbacks();
        let mut params = vec![
            format!(
                "{}: {}",
                target::runtime_param(ctx_use.rt),
                target::runtime_ctx_ref_ty()
            ),
            format!(
                "{}: {}",
                target::statics_param(ctx_use.statics),
                target::statics_ref_ty(target::generated_statics_symbol(&self.program.ctx))
            ),
            format!(
                "{}: {}",
                target::globals_param(ctx_use.globals),
                target::globals_ref_ty(target::generated_globals_symbol(&self.program.ctx))
            ),
        ];
        if retained_callbacks {
            params.extend([
                format!(
                    "{}: &{}",
                    target::owner_param_name(),
                    target::runtime_owner_handle_ty()
                ),
                format!(
                    "{}: {}",
                    target::callbacks_param_name(),
                    target::non_null_ty(&format!(
                        "{}<'cx>",
                        target::generated_callback_registry_symbol()
                    ))
                ),
            ]);
        }
        params.extend(function.params.iter().map(|param| {
            let local = &function.locals[param.local.index()];
            let mutability = if matches!(param.abi, RirParamAbi::MutPlace | RirParamAbi::DynBorrow)
                || self.local_needs_mut_binding(local.ty)
            {
                "mut "
            } else {
                ""
            };
            format!(
                "{mutability}{}: {}",
                local.symbol.as_str(),
                policy.callable_param_ty(param.ty, param.abi, param.escape)
            )
        }));
        let ret = self.function_ret_ty(function);
        let params = comma(params);
        let header = if ret == "()" {
            format!("fn {}<'cx, 'rt>({params})", function.symbol.as_str())
        } else {
            format!(
                "fn {}<'cx, 'rt>({params}) -> {ret}",
                function.symbol.as_str()
            )
        };
        self.w.line(format_args!("{header} {{"));
        self.indented(|this| {
            let block = &function.body;
            let predeclare = block.stmts.iter().any(|stmt| {
                matches!(
                    stmt,
                    RirStmt::If(_)
                        | RirStmt::Loop(_)
                        | RirStmt::RangeFor(_)
                        | RirStmt::CollectionFor(_)
                        | RirStmt::CollectionLoanScope(_)
                        | RirStmt::CollectionSlotScope(_)
                        | RirStmt::PatternMatch(_)
                        | RirStmt::DynMatch(_)
                        | RirStmt::OptionMatch(_)
                        | RirStmt::MapEntryMatch(_)
                )
            });
            if predeclare {
                this.emit_local_declarations(function);
            }
            this.emit_scoped_place_cells(function);
            this.emit_structured_block(function, block, predeclare);
            if this.fallible_functions[function.id.index()]
                && matches!(block.term, RirTerm::None)
                && matches!(this.program.types[function.ret.ty.index()], RirType::Void)
            {
                this.w.line("Ok(())");
            }
        });
        self.w.line("}");
        self.w.blank();
    }

    fn function_ret_ty(&self, function: &RirFunction) -> String {
        let ret = RirRustRepPolicy::new(self.program).callable_ret_ty(function.ret.ty);
        if !self.fallible_functions[function.id.index()] {
            return ret;
        }
        if ret == "()" {
            target::result_ty("()")
        } else {
            target::result_ty(&ret)
        }
    }

    fn local_needs_mut_binding(&self, ty: RirTypeId) -> bool {
        match self.program.types[ty.index()] {
            RirType::Lambda(sig) => self.lambda_sig_needs_mut_self(sig),
            _ => false,
        }
    }

    fn lambda_sig_needs_mut_self(&self, sig: RirLambdaSigId) -> bool {
        self.program.lambdas_for_sig(sig).any(|lambda| {
            lambda
                .captures
                .iter()
                .any(|capture| capture.semantic == RirParamSemantic::MutBorrow)
        })
    }

    fn emit_local_declarations(&mut self, function: &RirFunction) {
        let policy = RirRustRepPolicy::new(self.program);
        for cell in
            self.program.cells.iter().filter(|cell| {
                cell.owner == function.id && cell.lifetime == RirCellLifetime::Function
            })
        {
            let ty = match cell.storage {
                RirCellStorage::StackScoped => {
                    target::stack_lambda_cell_ty(&self.ty(cell.payload_ty))
                }
                RirCellStorage::Heap => target::handle_ty(&policy.cell_storage_ty(cell)),
            };
            self.w
                .line(format_args!("let {}: {ty};", cell.symbol.as_str()));
        }
        for local in &function.locals {
            if local.payload_ref
                || function.params.iter().any(|param| param.local == local.id)
                || self
                    .program
                    .cells
                    .iter()
                    .any(|cell| cell.owner == function.id && cell.source_local == local.id)
            {
                continue;
            }
            self.w.line(format_args!(
                "let mut {}: {};",
                local.symbol.as_str(),
                self.ty(local.ty)
            ));
        }
    }

    fn emit_scoped_place_cells(&mut self, function: &RirFunction) {
        let cells = self
            .program
            .scoped_place_cells
            .iter()
            .filter(|cell| {
                cell.owner == function.id && !Self::scoped_place_cell_needs_slot_init(cell)
            })
            .map(|cell| cell.id)
            .collect::<Vec<_>>();
        for cell in cells {
            let cell = self.program.scoped_place_cells[cell.index()].clone();
            self.emit_scoped_place_cell_init(function, &cell);
        }
    }

    fn emit_stmt_mode(
        &mut self,
        function: &RirFunction,
        index: usize,
        stmt: &RirStmt,
        predeclared: bool,
    ) {
        match stmt {
            RirStmt::Init { local, value } => {
                let local_data = &function.locals[local.index()];
                let value = match value {
                    RirRValue::MutPlaceGetCopy { place, .. } => {
                        self.mut_place_get_copy(function, index, place)
                    }
                    _ => self.rvalue(function, value),
                };
                if predeclared {
                    self.w
                        .line(format_args!("{} = {value};", local_data.symbol.as_str()));
                } else {
                    let mutability =
                        if local_data.mutable || self.local_needs_mut_binding(local_data.ty) {
                            "mut "
                        } else {
                            ""
                        };
                    self.w.line(format_args!(
                        "let {mutability}{}: {} = {value};",
                        local_data.symbol.as_str(),
                        self.ty(local_data.ty)
                    ));
                }
            }
            RirStmt::GlobalEnsure { global } => {
                self.w.line(format_args!(
                    "{};",
                    target::global_ensure(
                        &RustValues::global_slot_expr(self.program, *global),
                        &RustValues::global_init_call(self.program, *global),
                    )
                ));
            }
            RirStmt::GlobalSetRoot { global, value }
            | RirStmt::GlobalUpdateRoot { global, value } => {
                let value = self.rvalue(function, value);
                let slot = RustValues::global_slot_expr(self.program, *global);
                let set = if self
                    .program
                    .collection_replace_ty(self.program.globals[global.index()].ty)
                {
                    target::global_set_or_replace_collection(&slot, &value)
                } else {
                    target::global_set_without_init(&slot, &value)
                };
                self.w.line(format_args!("{set};"));
            }
            RirStmt::MutPlaceSet { place, value } => {
                let values = RustValues::new(self.program, function);
                let (root_ty, root) = values
                    .mut_place_access_arg(&place.access)
                    .expect("verified mutable-place set root");
                let slot_ty = place.ty;
                let (mut prelude, place) = if place.projections.is_empty() {
                    (vec![], root)
                } else {
                    self.prepared_projected_mut_place(function, index, place, root_ty, &root)
                };
                for line in prelude.drain(..) {
                    self.w.line(format_args!("{line}"));
                }
                let value_tmp = format!("__anv_value_{index}");
                let value = self.rvalue(function, value);
                self.w.line(format_args!("let {value_tmp} = {value};"));
                let set = if self.program.collection_replace_ty(slot_ty) {
                    target::mut_place_replace_collection(
                        &place,
                        target::runtime_param_name(),
                        &value_tmp,
                    )
                } else {
                    target::mut_place_set(&place, target::runtime_param_name(), &value_tmp)
                };
                self.w.line(format_args!("{set};"));
            }
            RirStmt::Assign { dst, value } => {
                let value = self.rvalue(function, value);
                let values = RustValues::new(self.program, function);
                self.w.line(format_args!("{};", values.assign(dst, &value)));
            }
            RirStmt::CellInit { cell, value } => {
                let init = self.cell_init(function, *cell, value);
                if predeclared && self.cell_decl(*cell).lifetime == RirCellLifetime::Function {
                    self.w
                        .line(format_args!("{} = {init};", self.cell_ref(function, *cell)));
                } else {
                    self.w.line(format_args!(
                        "let {}: {} = {init};",
                        self.cell_ref(function, *cell),
                        self.cell_ty(function, *cell)
                    ));
                }
            }
            RirStmt::ScopedPlaceCellInit { cell } => {
                let cell = &self.program.scoped_place_cells[cell.index()];
                self.emit_scoped_place_cell_init(function, cell);
            }
            RirStmt::CellSet { cell, value } => {
                let set = self.cell_set(function, *cell, value);
                self.w.line(format_args!("{set};"));
            }
            RirStmt::ScopedPlaceCellSet { cell, value } => {
                let ty = match *cell {
                    RirScopedPlaceCellRef::Owner(cell)
                    | RirScopedPlaceCellRef::Capture { cell, .. } => {
                        self.program.scoped_place_cells[cell.index()].payload_ty
                    }
                };
                let value = self.rvalue(function, value);
                self.w.line(format_args!(
                    "{};",
                    self.mut_place_set(ty, &self.scoped_place_cell_ref(function, *cell), &value)
                ));
            }
            RirStmt::DataRefSet {
                object,
                dataref,
                projections,
                value,
            } => self.w.line(format_args!(
                "{};",
                self.dataref_set(function, object, *dataref, projections, value)
            )),
            RirStmt::SequenceSlotSet {
                collection,
                index,
                value,
            } => {
                let value = RustValues::new(self.program, function).value_operand(value);
                let set = self.sequence_slot_set(function, collection, *index, &value);
                self.w.line(format_args!("{set};"));
            }
            RirStmt::MapValueSet { map, index, value } => self.w.line(format_args!(
                "{};",
                self.map_value_set(function, map, *index, value)
            )),
            RirStmt::Eval(value) => {
                let value = self.rvalue(function, value);
                self.w.line(format_args!("{value};"));
            }
            RirStmt::If(branch) => self.emit_if(function, branch, predeclared),
            RirStmt::Loop(loop_) => self.emit_loop(function, loop_, predeclared),
            RirStmt::RangeFor(range) => {
                self.emit_iter_for(function, &IterFor::Range(range), predeclared);
            }
            RirStmt::CollectionFor(for_) => {
                self.emit_iter_for(function, &IterFor::Collection(for_), predeclared);
            }
            RirStmt::CollectionLoanScope(scope) => {
                self.emit_collection_loan_scope(function, scope, predeclared);
            }
            RirStmt::CollectionSlotScope(block) => {
                self.emit_lexical_block(function, block, predeclared);
            }
            RirStmt::PatternMatch(match_) => {
                self.emit_pattern_match(function, index, match_, predeclared);
            }
            RirStmt::DynMatch(match_) => {
                self.emit_dyn_match(function, index, match_, predeclared);
            }
            RirStmt::OptionMatch(match_) => {
                self.emit_option_match(function, index, match_, predeclared);
            }
            RirStmt::MapEntryMatch(match_) => {
                self.emit_map_entry_match(function, index, match_, predeclared);
            }
        }
    }

    fn emit_loop(&mut self, function: &RirFunction, loop_: &RirLoop, predeclared: bool) {
        self.w
            .line(format_args!("{}: loop {{", loop_label(loop_.id)));
        self.indented(|this| this.emit_structured_block(function, &loop_.body, predeclared));
        self.w.line("}");
    }

    fn emit_iter_for(&mut self, function: &RirFunction, for_: &IterFor<'_>, predeclared: bool) {
        let values = RustValues::new(self.program, function);
        let (id, name, iter_new, item_name, item, ordinal, body) = match *for_ {
            IterFor::Range(range) => {
                let start = values.value_operand(&range.start);
                let end = values.value_operand(&range.end);
                (
                    range.id,
                    "range",
                    apply_ordinal_plan(
                        target::range_iter_new(&start, &end, range.inclusive),
                        &range.ordinal_plan,
                        &values,
                    ),
                    "__anv_item",
                    range.item,
                    range.ordinal,
                    &range.body,
                )
            }
            IterFor::Collection(for_) => {
                let len = function.locals[for_.len.index()].symbol.as_str();
                (
                    for_.id,
                    "collection",
                    apply_ordinal_plan(
                        target::collection_iter_new(len),
                        &for_.ordinal_plan,
                        &values,
                    ),
                    "__anv_index",
                    for_.index,
                    for_.ordinal,
                    &for_.body,
                )
            }
        };
        let iter = format!("__anv_{name}_{}", id.index());
        self.w.line("{");
        self.indented(|this| {
            this.w.line(format_args!("let mut {iter} = {iter_new};"));
            this.w.line(format_args!("{}: loop {{", loop_label(id)));
            this.indented(|this| {
                this.w.line(format_args!(
                    "let Some((__anv_ordinal, {item_name})) = {iter}.next() else {{ break; }};"
                ));
                this.w.line(format_args!(
                    "{} = {item_name};",
                    function.locals[item.index()].symbol.as_str()
                ));
                if let Some(ordinal) = ordinal {
                    this.w.line(format_args!(
                        "{} = __anv_ordinal;",
                        function.locals[ordinal.index()].symbol.as_str()
                    ));
                }
                this.emit_structured_block(function, body, predeclared);
            });
            this.w.line("}");
        });
        self.w.line("}");
    }

    fn emit_lexical_block(
        &mut self,
        function: &RirFunction,
        block: &RirStructuredBlock,
        predeclared: bool,
    ) {
        self.w.line("{");
        self.indented(|this| this.emit_structured_block(function, block, predeclared));
        self.w.line("}");
    }

    fn emit_collection_loan_scope(
        &mut self,
        function: &RirFunction,
        scope: &RirCollectionLoanScope,
        predeclared: bool,
    ) {
        self.w.line("{");
        self.indented(|this| {
            let track = scope.root_kind.tracks_shape_loan();
            if track {
                let depth = this.collection_loans.len();
                let loan_var = format!("__anv_collection_loan_{depth}");
                let version_var = format!("__anv_collection_version_{depth}");
                let (prelude, loan) = this.collection_shape_loan(function, &scope.root, depth);
                for line in prelude {
                    this.w.line(format_args!("{line}"));
                }
                this.w.line(format_args!("let {loan_var} = {loan};"));
                this.w.line(format_args!(
                    "let {version_var} = {};",
                    target::shape_loan_version(&loan_var)
                ));
                this.collection_loans.push(ActiveCollectionLoan {
                    root: scope.root.clone(),
                    version: version_var,
                });
            }
            this.emit_structured_block(function, &scope.body, predeclared);
            if track {
                this.collection_loans.pop();
            }
        });
        self.w.line("}");
    }

    fn emit_if(&mut self, function: &RirFunction, branch: &RirIf, predeclared: bool) {
        self.w.line(format_args!(
            "if {} {{",
            RustValues::new(self.program, function).operand(&branch.cond)
        ));
        self.indented(|this| {
            this.emit_structured_block(function, &branch.then_block, predeclared);
        });
        if let Some(else_block) = &branch.else_block {
            self.w.line("} else {");
            self.indented(|this| this.emit_structured_block(function, else_block, predeclared));
        }
        self.w.line("}");
    }

    fn emit_pattern_match(
        &mut self,
        function: &RirFunction,
        index: usize,
        match_: &RirPatternMatch,
        predeclared: bool,
    ) {
        let label = format!("'__anv_match_{index}");
        self.w.line(format_args!("{label}: {{"));
        self.indented(|this| {
            let mut alternative_index = 0;
            for arm in &match_.arms {
                for alternative in &arm.alternatives {
                    this.emit_pattern_alternative(
                        function,
                        match_,
                        alternative,
                        alternative_index,
                        &arm.block,
                        predeclared,
                        &label,
                    );
                    alternative_index += 1;
                }
            }
            this.w.line("unreachable!();");
        });
        self.w.line("}");
    }

    fn emit_pattern_alternative(
        &mut self,
        function: &RirFunction,
        match_: &RirPatternMatch,
        alternative: &RirPatternAlternative,
        alternative_index: usize,
        block: &RirStructuredBlock,
        predeclared: bool,
        label: &str,
    ) {
        if let Some((enum_id, variant)) = Self::root_enum_variant_test(alternative) {
            let enm = &self.program.enums[enum_id.index()];
            let variant = &enm.variants[variant.index()];
            let pattern = Self::variant_payload_pattern(enm, variant, alternative_index);
            let subject = RustPlaces::new(self.program, function).local_place(&match_.subject);
            let borrow = if Self::pattern_alternative_has_alias(alternative) {
                "&mut"
            } else {
                "&"
            };
            self.w
                .line(format_args!("if let {pattern} = {borrow} {subject} {{"));
            self.indented(|this| {
                this.emit_pattern_alternative_body(
                    function,
                    match_,
                    alternative,
                    alternative_index,
                    block,
                    predeclared,
                    label,
                    true,
                );
            });
            self.w.line("}");
            return;
        }
        self.emit_pattern_alternative_body(
            function,
            match_,
            alternative,
            alternative_index,
            block,
            predeclared,
            label,
            false,
        );
    }

    fn emit_pattern_alternative_body(
        &mut self,
        function: &RirFunction,
        match_: &RirPatternMatch,
        alternative: &RirPatternAlternative,
        alternative_index: usize,
        block: &RirStructuredBlock,
        predeclared: bool,
        label: &str,
        skip_root_variant_test: bool,
    ) {
        let condition = self.pattern_alternative_condition(
            function,
            match_,
            alternative,
            alternative_index,
            skip_root_variant_test,
        );
        if condition == "true" {
            self.emit_pattern_matched_body(
                function,
                match_,
                alternative,
                alternative_index,
                block,
                predeclared,
                label,
            );
            return;
        }
        self.w.line(format_args!("if {condition} {{"));
        self.indented(|this| {
            this.emit_pattern_matched_body(
                function,
                match_,
                alternative,
                alternative_index,
                block,
                predeclared,
                label,
            );
        });
        self.w.line("}");
    }

    fn emit_pattern_matched_body(
        &mut self,
        function: &RirFunction,
        match_: &RirPatternMatch,
        alternative: &RirPatternAlternative,
        alternative_index: usize,
        block: &RirStructuredBlock,
        predeclared: bool,
        label: &str,
    ) {
        self.emit_pattern_bindings(function, match_, alternative, alternative_index);
        self.emit_structured_block(function, block, predeclared);
        if matches!(block.term, RirTerm::None) {
            self.w.line(format_args!("break {label};"));
        }
    }

    fn pattern_alternative_has_alias(alternative: &RirPatternAlternative) -> bool {
        alternative
            .bindings
            .iter()
            .any(|binding| binding.mode == RirPatternBindingMode::Alias)
    }

    fn pattern_alternative_condition(
        &self,
        function: &RirFunction,
        match_: &RirPatternMatch,
        alternative: &RirPatternAlternative,
        alternative_index: usize,
        skip_root_variant_test: bool,
    ) -> String {
        let conditions = alternative
            .tests
            .iter()
            .filter(|test| {
                !(skip_root_variant_test
                    && matches!(test, RirPatternTest::EnumVariant { path, .. } if path.steps.is_empty()))
            })
            .map(|test| self.pattern_test_condition(function, match_, test, alternative_index))
            .collect::<Vec<_>>();
        if conditions.is_empty() {
            "true".into()
        } else {
            conditions.join(" && ")
        }
    }

    fn root_enum_variant_test(
        alternative: &RirPatternAlternative,
    ) -> Option<(RirEnumId, RirVariantId)> {
        alternative.tests.iter().find_map(|test| match test {
            RirPatternTest::EnumVariant {
                path,
                enum_id,
                variant,
            } if path.steps.is_empty() => Some((*enum_id, *variant)),
            _ => None,
        })
    }

    fn variant_payload_pattern(
        enm: &RirEnum,
        variant: &RirVariant,
        alternative_index: usize,
    ) -> String {
        let path = variant_path(enm.symbol.as_str(), variant.symbol.as_str());
        match variant.kind {
            RirVariantKind::Unit => unit_variant_pattern(&path),
            RirVariantKind::Tuple => {
                let fields = (0..variant.fields.len())
                    .map(|field| Self::pattern_payload_tmp(alternative_index, field))
                    .collect::<Vec<_>>();
                format!("{path}({})", comma(fields))
            }
            RirVariantKind::Struct => {
                let fields = variant
                    .fields
                    .iter()
                    .enumerate()
                    .map(|(index, field)| {
                        format!(
                            "{}: {}",
                            field.symbol.as_str(),
                            Self::pattern_payload_tmp(alternative_index, index)
                        )
                    })
                    .collect::<Vec<_>>();
                format!("{path} {{ {} }}", comma(fields))
            }
        }
    }

    fn pattern_payload_tmp(alternative_index: usize, field: usize) -> String {
        format!("__anv_pat_{alternative_index}_{field}")
    }

    fn pattern_test_condition(
        &self,
        function: &RirFunction,
        match_: &RirPatternMatch,
        test: &RirPatternTest,
        alternative_index: usize,
    ) -> String {
        match test {
            RirPatternTest::Any { branches } => format!(
                "({})",
                branches
                    .iter()
                    .map(|tests| {
                        let conditions = tests
                            .iter()
                            .map(|test| {
                                self.pattern_test_condition(
                                    function,
                                    match_,
                                    test,
                                    alternative_index,
                                )
                            })
                            .collect::<Vec<_>>();
                        format!(
                            "({})",
                            if conditions.is_empty() {
                                "true".into()
                            } else {
                                conditions.join(" && ")
                            }
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(" || ")
            ),
            RirPatternTest::Literal { path, value } => {
                let values = RustValues::new(self.program, function);
                let const_ty = self.program.consts[value.index()].ty;
                let lhs = if let Some(place) = self.subject_pattern_place_operand(match_, path) {
                    values.value_operand(&RirOperand::Place(place))
                } else {
                    let Some(source) = self.pattern_path_ref_source(
                        function,
                        match_,
                        path,
                        alternative_index,
                        false,
                    ) else {
                        return "false".into();
                    };
                    values.value_from_ref(const_ty, &source)
                };
                match &self.program.consts[value.index()].value {
                    RirConstValue::String(id) => format!(
                        "({lhs}).as_str() == {}",
                        rust_string(&self.program.string_literal(*id).text)
                    ),
                    _ => {
                        let rhs = values.value_operand(&RirOperand::Const(*value));
                        format!("{lhs} == {rhs}")
                    }
                }
            }
            RirPatternTest::FlagValue { path, flag, bits } => {
                let values = RustValues::new(self.program, function);
                let flag_ty = self
                    .program
                    .types
                    .iter()
                    .position(|ty| ty == &RirType::Flag(*flag))
                    .map(RirTypeId::from_index)
                    .expect("verified flag pattern type");
                let lhs = if let Some(place) = self.subject_pattern_place_operand(match_, path) {
                    values.value_operand(&RirOperand::Place(place))
                } else {
                    let Some(source) = self.pattern_path_ref_source(
                        function,
                        match_,
                        path,
                        alternative_index,
                        false,
                    ) else {
                        return "false".into();
                    };
                    values.value_from_ref(flag_ty, &source)
                };
                target::flag_pattern_eq(&lhs, *bits)
            }
            RirPatternTest::EnumVariant {
                path,
                enum_id,
                variant,
            } if path.steps.is_empty() => {
                let RirType::Enum(subject_enum) = self.program.types[match_.subject.ty.index()]
                else {
                    return "false".into();
                };
                if subject_enum != *enum_id {
                    return "false".into();
                }
                let enm = &self.program.enums[enum_id.index()];
                let variant = &enm.variants[variant.index()];
                let pattern = Self::variant_pattern(enm, variant);
                let subject = RustPlaces::new(self.program, function).local_place(&match_.subject);
                format!("matches!(&{subject}, {pattern})")
            }
            RirPatternTest::Nil { path } => self
                .pattern_optional_condition(function, match_, path, alternative_index, "is_none")
                .unwrap_or_else(|| "false".into()),
            RirPatternTest::OptionalSome { path } => self
                .pattern_optional_condition(function, match_, path, alternative_index, "is_some")
                .unwrap_or_else(|| "false".into()),
            RirPatternTest::EnumVariant {
                path,
                enum_id,
                variant,
            } => {
                let Some(source) =
                    self.pattern_path_ref_source(function, match_, path, alternative_index, false)
                else {
                    return "false".into();
                };
                let enm = &self.program.enums[enum_id.index()];
                let pattern = Self::variant_pattern(enm, &enm.variants[variant.index()]);
                format!("matches!({source}, {pattern})")
            }
        }
    }

    fn pattern_optional_condition(
        &self,
        function: &RirFunction,
        match_: &RirPatternMatch,
        path: &RirPatternPath,
        alternative_index: usize,
        method: &str,
    ) -> Option<String> {
        let source =
            self.pattern_path_ref_source(function, match_, path, alternative_index, false)?;
        Some(format!("({source}).{method}()"))
    }

    fn pattern_path_ref_source(
        &self,
        function: &RirFunction,
        match_: &RirPatternMatch,
        path: &RirPatternPath,
        alternative_index: usize,
        mutable: bool,
    ) -> Option<String> {
        self.enum_payload_binding_source(path, alternative_index, mutable)
            .or_else(|| self.subject_pattern_ref_source(function, match_, path, mutable))
    }

    fn subject_pattern_ref_source(
        &self,
        function: &RirFunction,
        match_: &RirPatternMatch,
        path: &RirPatternPath,
        mutable: bool,
    ) -> Option<String> {
        if !mutable && let Some(place) = self.subject_pattern_place_operand(match_, path) {
            return Some(
                RustValues::new(self.program, function).operand_ref(&RirOperand::Place(place)),
            );
        }
        let borrow = if mutable { "&mut" } else { "&" };
        let source = format!(
            "{borrow} {}",
            RustPlaces::new(self.program, function).local_place(&match_.subject)
        );
        self.pattern_path_ref_steps_source(match_.subject.ty, source, &path.steps, mutable)
    }

    fn pattern_path_ref_steps_source(
        &self,
        mut current_ty: RirTypeId,
        mut source: String,
        steps: &[RirPatternPathStep],
        mutable: bool,
    ) -> Option<String> {
        for step in steps {
            match *step {
                RirPatternPathStep::TupleField(field) => {
                    let RirType::Tuple(tuple) = self.program.types[current_ty.index()] else {
                        return None;
                    };
                    let field = &self.program.tuples[tuple.index()].fields[field as usize];
                    let borrow = if mutable { "&mut" } else { "&" };
                    source = format!("{borrow} ({source}).{}", field.symbol.as_str());
                    current_ty = field.ty;
                }
                RirPatternPathStep::Field(field) => {
                    let RirType::Struct(strukt) = self.program.types[current_ty.index()] else {
                        return None;
                    };
                    let field = &self.program.structs[strukt.index()].fields[field.index()];
                    let borrow = if mutable { "&mut" } else { "&" };
                    source = format!("{borrow} ({source}).{}", field.symbol.as_str());
                    current_ty = field.ty;
                }
                RirPatternPathStep::OptionalSome => {
                    let RirType::Option(inner) = self.program.types[current_ty.index()] else {
                        return None;
                    };
                    let method = if mutable { "as_mut" } else { "as_ref" };
                    source = format!("({source}).{method}().unwrap()");
                    current_ty = inner;
                }
                RirPatternPathStep::EnumTupleField {
                    enum_id,
                    variant,
                    field,
                }
                | RirPatternPathStep::EnumStructField {
                    enum_id,
                    variant,
                    field,
                } => {
                    if !matches!(
                        self.program.types[current_ty.index()],
                        RirType::Enum(found) if found == enum_id
                    ) {
                        return None;
                    }
                    let enm = &self.program.enums[enum_id.index()];
                    let variant = &enm.variants[variant.index()];
                    let selected = "__anv_nested_payload";
                    let path = variant_path(enm.symbol.as_str(), variant.symbol.as_str());
                    let pattern = match variant.kind {
                        RirVariantKind::Unit => return None,
                        RirVariantKind::Tuple => {
                            let fields = (0..variant.fields.len())
                                .map(|index| {
                                    if index == field as usize {
                                        selected.into()
                                    } else {
                                        "_".into()
                                    }
                                })
                                .collect::<Vec<String>>();
                            format!("{path}({})", comma(fields))
                        }
                        RirVariantKind::Struct => {
                            let field = &variant.fields[field as usize];
                            format!("{path} {{ {}: {selected}, .. }}", field.symbol.as_str())
                        }
                    };
                    source = format!(
                        "match ({source}) {{ {pattern} => {selected}, _ => unreachable!() }}"
                    );
                    current_ty = variant.fields[field as usize].ty;
                }
            }
        }
        Some(source)
    }

    fn subject_pattern_place_operand(
        &self,
        match_: &RirPatternMatch,
        path: &RirPatternPath,
    ) -> Option<RirPlace> {
        let mut projections = match_.subject.projections.clone();
        let mut ty = match_.subject.ty;
        for step in &path.steps {
            match *step {
                RirPatternPathStep::TupleField(field) => {
                    let RirType::Tuple(tuple) = self.program.types[ty.index()] else {
                        return None;
                    };
                    let field = &self.program.tuples[tuple.index()].fields[field as usize];
                    projections.push(RirProjection::TupleField(field.id));
                    ty = field.ty;
                }
                RirPatternPathStep::Field(field) => {
                    let RirType::Struct(strukt) = self.program.types[ty.index()] else {
                        return None;
                    };
                    projections.push(RirProjection::Field(field));
                    ty = self.program.structs[strukt.index()].fields[field.index()].ty;
                }
                RirPatternPathStep::OptionalSome
                | RirPatternPathStep::EnumTupleField { .. }
                | RirPatternPathStep::EnumStructField { .. } => return None,
            }
        }
        Some(RirPlace {
            root: match_.subject.root,
            projections,
            ty,
        })
    }

    fn emit_pattern_bindings(
        &mut self,
        function: &RirFunction,
        match_: &RirPatternMatch,
        alternative: &RirPatternAlternative,
        alternative_index: usize,
    ) {
        for binding in &alternative.bindings {
            self.emit_pattern_binding(function, match_, binding, alternative_index);
        }
    }

    fn emit_pattern_binding(
        &mut self,
        function: &RirFunction,
        match_: &RirPatternMatch,
        binding: &RirPatternBinding,
        alternative_index: usize,
    ) {
        let local = &function.locals[binding.local.index()];
        if binding.mode == RirPatternBindingMode::Owned
            && let Some(place) = self.subject_pattern_place_operand(match_, &binding.path)
        {
            let value =
                RustValues::new(self.program, function).value_operand(&RirOperand::Place(place));
            self.w
                .line(format_args!("{} = {value};", local.symbol.as_str()));
            return;
        }
        if let Some(source) = self.pattern_path_ref_source(
            function,
            match_,
            &binding.path,
            alternative_index,
            binding.mode == RirPatternBindingMode::Alias,
        ) {
            match binding.mode {
                RirPatternBindingMode::Owned => {
                    let value =
                        RustValues::new(self.program, function).value_from_ref(binding.ty, &source);
                    self.w
                        .line(format_args!("{} = {value};", local.symbol.as_str()));
                }
                RirPatternBindingMode::Alias => {
                    self.w
                        .line(format_args!("let {} = {source};", local.symbol.as_str()));
                }
            }
            return;
        }
        self.w.line("unreachable!();");
    }

    fn enum_payload_binding_source(
        &self,
        path: &RirPatternPath,
        alternative_index: usize,
        mutable: bool,
    ) -> Option<String> {
        let (first, rest) = path.steps.split_first()?;
        let (enum_id, variant, field) = match first {
            RirPatternPathStep::EnumTupleField {
                enum_id,
                variant,
                field,
            }
            | RirPatternPathStep::EnumStructField {
                enum_id,
                variant,
                field,
            } => (*enum_id, *variant, *field as usize),
            _ => return None,
        };
        let ty = self.program.enums[enum_id.index()].variants[variant.index()].fields[field].ty;
        self.pattern_path_ref_steps_source(
            ty,
            Self::pattern_payload_tmp(alternative_index, field),
            rest,
            mutable,
        )
    }

    fn emit_map_entry_match(
        &mut self,
        function: &RirFunction,
        index: usize,
        match_: &RirMapEntryMatch,
        predeclared: bool,
    ) {
        let key_tmp = format!("__anv_map_entry_key_{index}");
        let guard_tmp = format!("__anv_map_entry_guard_{index}");
        let (mut prelude, map) =
            self.prepared_escaping_payload_place_arg(function, index, &match_.map);
        for line in prelude.drain(..) {
            self.w.line(format_args!("{line}"));
        }
        let values = RustValues::new(self.program, function);
        let key = values.value_operand(&match_.key);
        self.w.line(format_args!("let {key_tmp} = {key};"));
        let is_some = target::mut_place_access_ctx(
            &map,
            target::runtime_param_name(),
            &target::map_contains_key_region(&key_tmp),
        );
        if match_.payload_escapes {
            self.w.line(format_args!("if !({is_some}) {{"));
            self.indented(|this| {
                this.emit_structured_block(function, &match_.none_block, predeclared);
            });
            self.w.line("}");
            self.emit_map_entry_alias(
                function,
                index,
                match_,
                match_.map.ty,
                &map,
                &key_tmp,
                &guard_tmp,
            );
            self.emit_structured_block(function, &match_.some_block, predeclared);
            return;
        }
        self.w.line(format_args!("if {is_some} {{"));
        self.indented(|this| {
            this.emit_map_entry_alias(
                function,
                index,
                match_,
                match_.map.ty,
                &map,
                &key_tmp,
                &guard_tmp,
            );
            this.emit_structured_block(function, &match_.some_block, predeclared);
        });
        self.w.line("} else {");
        self.indented(|this| {
            this.emit_structured_block(function, &match_.none_block, predeclared);
        });
        self.w.line("}");
    }

    fn emit_map_entry_alias(
        &mut self,
        function: &RirFunction,
        index: usize,
        match_: &RirMapEntryMatch,
        map_ty: RirTypeId,
        map: &str,
        key: &str,
        guard: &str,
    ) {
        let Some(payload) = match_.payload else {
            return;
        };
        let payload = &function.locals[payload.index()];
        let RirType::Map { key: key_ty, .. } = self.program.types[map_ty.index()] else {
            unreachable!("verified map-entry match")
        };
        let key_ty = self.ty(key_ty);
        let ops = format!("__anv_map_entry_ops_{index}");
        self.w.line(format_args!(
            "let {guard} = {};",
            target::mut_place_mutate_ctx(
                map,
                target::runtime_param_name(),
                &target::map_begin_value_loan_region(key),
            )
        ));
        self.w.line(format_args!(
            "let {ops}: {} = {};",
            target::map_value_ops_ty(&key_ty),
            target::map_value_ops_ctor(&format!("{key}.clone()"), guard)
        ));
        self.w.line(format_args!(
            "let {} = {};",
            payload.symbol.as_str(),
            target::scoped_mut_place_cell_new(
                &target::mut_place_projected(map, &format!("&{ops}")),
                &target::runtime_safepoint_state("rt"),
            )
        ));
    }

    fn emit_map_entry_alias_drop(
        &mut self,
        function: &RirFunction,
        index: usize,
        payload: RirLocalId,
        guard: &str,
    ) {
        self.w.line(format_args!(
            "drop({});",
            function.locals[payload.index()].symbol.as_str()
        ));
        self.w
            .line(format_args!("drop(__anv_map_entry_ops_{index});"));
        self.w.line(format_args!("drop({guard});"));
    }

    fn emit_dyn_match(
        &mut self,
        function: &RirFunction,
        index: usize,
        match_: &super::rir::RirDynMatch,
        predeclared: bool,
    ) {
        let match_source = match &match_.source {
            super::rir::RirDynMatchSource::MutPlace(source) => source,
            super::rir::RirDynMatchSource::Borrowed(borrow) => {
                self.emit_borrowed_dyn_match(function, match_, borrow, predeclared);
                return;
            }
            super::rir::RirDynMatchSource::Owned { .. } => {
                self.emit_owned_dyn_match(function, match_, predeclared);
                return;
            }
        };
        let (prelude, source) =
            self.prepared_escaping_payload_place_arg(function, index, match_source);
        for line in prelude {
            self.w.line(line);
        }
        let source_local = format!("__anv_dyn_match_source_{index}");
        self.w.line(format!("let mut {source_local} = {source};"));
        let source = format!("{source_local}.reborrow()");
        let carrier = &self.program.dyn_carriers[match_.carrier.index()];
        let RirType::Enum(id) = self.program.types[carrier.storage_ty.index()] else {
            unreachable!("verified dynamic match carrier")
        };
        let enm = &self.program.enums[id.index()];
        for (arm_index, arm) in match_.arms.iter().enumerate() {
            let patterns = arm.variants.iter().map(|variant| {
                let variant = &enm.variants[variant.index()];
                format!(
                    "{}(..)",
                    variant_path(enm.symbol.as_str(), variant.symbol.as_str())
                )
            });
            let patterns = patterns.collect::<Vec<_>>();
            let test_body = if patterns.is_empty() {
                "Ok(false)".to_string()
            } else {
                format!("Ok(matches!(value, {}))", patterns.join(" | "))
            };
            let test = target::mut_place_access(&source, target::runtime_param_name(), &test_body);
            let branch = if arm_index == 0 { "if" } else { "else if" };
            self.w.line(format!("{branch} {test} {{"));
            self.indented(|this| {
                if let super::rir::RirDynMatchBinding::Alias(binding) = arm.binding {
                    let ops = format!("__AnvDynPayloadOps_{index}_{arm_index}");
                    let descriptor = RustValues::new(this.program, function)
                        .dyn_payload_projection_descriptor(
                            &ops,
                            match_.carrier,
                            &arm.variants,
                            arm.target,
                        );
                    this.w.line(descriptor.struct_decl);
                    this.w.line(descriptor.impl_decl);
                    let binding = &function.locals[binding.index()];
                    this.w.line(format!(
                        "let {} = {};",
                        binding.symbol.as_str(),
                        target::scoped_mut_place_cell_new(
                            &target::mut_place_projected(&source, &format!("&{}", descriptor.ctor)),
                            &target::runtime_safepoint_state("rt"),
                        )
                    ));
                }
                this.emit_structured_block(function, &arm.block, predeclared);
            });
            self.w.line("}");
        }
        let emit_fallback = |this: &mut Self| {
            if let Some(binding) = match_.fallback_binding {
                let binding = &function.locals[binding.index()];
                this.w.line(format!(
                    "let {} = {};",
                    binding.symbol.as_str(),
                    target::scoped_mut_place_cell_new(
                        &format!("{source_local}.reborrow()"),
                        &target::runtime_safepoint_state("rt"),
                    )
                ));
            }
            this.emit_structured_block(function, &match_.fallback, predeclared);
        };
        if match_.arms.is_empty() {
            emit_fallback(self);
        } else {
            self.w.line("else {");
            self.indented(emit_fallback);
            self.w.line("}");
        }
    }

    fn emit_borrowed_dyn_match(
        &mut self,
        function: &RirFunction,
        match_: &super::rir::RirDynMatch,
        borrow: &super::rir::RirDynBorrow,
        predeclared: bool,
    ) {
        let super::rir::RirDynBorrowSource::Borrowed { local, carrier } = borrow.source else {
            unreachable!("dynamic match borrowed parameter root")
        };
        debug_assert_eq!(carrier, match_.carrier);
        let carrier = &self.program.dyn_carriers[carrier.index()];
        let RirType::Enum(id) = self.program.types[carrier.storage_ty.index()] else {
            unreachable!("verified dynamic match carrier")
        };
        let enm = &self.program.enums[id.index()];
        let symbol = self.program.dyn_borrow_symbol(carrier.id);
        let local = function.locals[local.index()].symbol.as_str();
        self.w.line(format!("match &mut {local} {{"));
        for variant in &carrier.variants {
            let name = enm.variants[variant.id.index()].symbol.as_str();
            self.indented(|this| {
                this.w.line(format!("{symbol}::{name}(place) => {{"));
                this.indented(|this| {
                    if let Some(arm) = match_
                        .arms
                        .iter()
                        .find(|arm| arm.variants.contains(&variant.id))
                    {
                        match arm.binding {
                            super::rir::RirDynMatchBinding::Alias(binding) => {
                                let binding = &function.locals[binding.index()];
                                this.w.line(format!(
                                    "let {} = {};",
                                    binding.symbol.as_str(),
                                    target::scoped_mut_place_cell_new(
                                        &target::mut_place_reborrow("place"),
                                        &target::runtime_safepoint_state("rt"),
                                    )
                                ));
                            }
                            super::rir::RirDynMatchBinding::Owned(binding) => {
                                let binding = &function.locals[binding.index()];
                                let payload = RustValues::new(this.program, function)
                                    .dyn_payload_from_ref(
                                        variant.concrete_ty,
                                        variant.payload,
                                        "value",
                                    );
                                let value = target::mut_place_access(
                                    &target::mut_place_reborrow("place"),
                                    target::runtime_param_name(),
                                    &format!("Ok({payload})"),
                                );
                                this.w
                                    .line(format!("{} = {value};", binding.symbol.as_str()));
                            }
                            super::rir::RirDynMatchBinding::Discard => {}
                        }
                        this.emit_structured_block(function, &arm.block, predeclared);
                    } else {
                        this.emit_borrowed_dyn_fallback(
                            function,
                            match_,
                            &format!("{symbol}::{name}(place.reborrow())"),
                            predeclared,
                        );
                    }
                });
                this.w.line("}");
            });
        }
        self.emit_borrowed_owned_match_arm(
            function,
            match_,
            carrier,
            &format!("{symbol}::Owned"),
            predeclared,
        );
        for weakening in self
            .program
            .dyn_weakenings
            .iter()
            .filter(|w| w.target == carrier.id)
        {
            let source = &self.program.dyn_carriers[weakening.source.index()];
            self.emit_borrowed_owned_match_arm(
                function,
                match_,
                source,
                &format!("{symbol}::OwnedFrom{}", source.id.index()),
                predeclared,
            );
        }
        self.w.line("}");
    }

    fn emit_borrowed_owned_match_arm(
        &mut self,
        function: &RirFunction,
        match_: &super::rir::RirDynMatch,
        source: &RirDynCarrier,
        pattern: &str,
        predeclared: bool,
    ) {
        self.indented(|this| {
            this.w.line(format!("{pattern}(place) => {{"));
            this.indented(|this| {
                let source_local = "__anv_borrowed_dyn_match_source";
                this.w
                    .line(format!("let mut {source_local} = place.reborrow();"));
                let RirType::Enum(id) = this.program.types[source.storage_ty.index()] else {
                    unreachable!("verified dynamic match source carrier")
                };
                let enm = &this.program.enums[id.index()];
                let supported = match_
                    .arms
                    .iter()
                    .filter_map(|arm| {
                        let variants = source
                            .variants
                            .iter()
                            .filter(|variant| variant.concrete_ty == arm.target)
                            .map(|variant| variant.id)
                            .collect::<Vec<_>>();
                        (!variants.is_empty()).then_some((arm, variants))
                    })
                    .collect::<Vec<_>>();
                for (index, (arm, variants)) in supported.iter().enumerate() {
                    let patterns = variants
                        .iter()
                        .map(|variant| {
                            let variant = &enm.variants[variant.index()];
                            format!(
                                "{}(..)",
                                variant_path(enm.symbol.as_str(), variant.symbol.as_str())
                            )
                        })
                        .collect::<Vec<_>>();
                    let test = target::mut_place_access(
                        &format!("{source_local}.reborrow()"),
                        target::runtime_param_name(),
                        &format!("Ok(matches!(value, {}))", patterns.join(" | ")),
                    );
                    let branch = if index == 0 { "if" } else { "else if" };
                    this.w.line(format!("{branch} {test} {{"));
                    this.indented(|this| {
                        if !matches!(arm.binding, super::rir::RirDynMatchBinding::Discard) {
                            let ops = format!(
                                "__AnvBorrowedDynPayloadOps_{}_{}",
                                source.id.index(),
                                index
                            );
                            let values = RustValues::new(this.program, function);
                            let descriptor = values.dyn_payload_projection_descriptor(
                                &ops, source.id, variants, arm.target,
                            );
                            this.w.line(descriptor.struct_decl);
                            this.w.line(descriptor.impl_decl);
                            let projected = target::mut_place_projected(
                                &target::mut_place_reborrow(source_local),
                                &format!("&{}", descriptor.ctor),
                            );
                            match arm.binding {
                                super::rir::RirDynMatchBinding::Alias(binding) => {
                                    let binding = &function.locals[binding.index()];
                                    this.w.line(format!(
                                        "let {} = {};",
                                        binding.symbol.as_str(),
                                        target::scoped_mut_place_cell_new(
                                            &projected,
                                            &target::runtime_safepoint_state("rt"),
                                        )
                                    ));
                                }
                                super::rir::RirDynMatchBinding::Owned(binding) => {
                                    let variant = &source.variants[variants[0].index()];
                                    debug_assert!(variants.iter().all(|id| {
                                        source.variants[id.index()].payload == variant.payload
                                    }));
                                    let binding = &function.locals[binding.index()];
                                    let payload = values.dyn_payload_from_ref(
                                        arm.target,
                                        variant.payload,
                                        "value",
                                    );
                                    let value = target::mut_place_access(
                                        &projected,
                                        target::runtime_param_name(),
                                        &format!("Ok({payload})"),
                                    );
                                    this.w
                                        .line(format!("{} = {value};", binding.symbol.as_str()));
                                }
                                super::rir::RirDynMatchBinding::Discard => unreachable!(),
                            }
                        }
                        this.emit_structured_block(function, &arm.block, predeclared);
                    });
                    this.w.line("}");
                }
                let emit_fallback = |this: &mut Self| {
                    let target_symbol = this.program.dyn_borrow_symbol(match_.carrier);
                    let constructor = if source.id == match_.carrier {
                        format!("{target_symbol}::Owned({source_local}.reborrow())")
                    } else {
                        format!(
                            "{target_symbol}::OwnedFrom{}({source_local}.reborrow())",
                            source.id.index()
                        )
                    };
                    this.emit_borrowed_dyn_fallback(function, match_, &constructor, predeclared);
                };
                if supported.is_empty() {
                    emit_fallback(this);
                } else {
                    this.w.line("else {");
                    this.indented(emit_fallback);
                    this.w.line("}");
                }
            });
            this.w.line("}");
        });
    }

    fn emit_borrowed_dyn_fallback(
        &mut self,
        function: &RirFunction,
        match_: &super::rir::RirDynMatch,
        descriptor: &str,
        predeclared: bool,
    ) {
        if let Some(binding) = match_.fallback_binding {
            let binding = &function.locals[binding.index()];
            self.w.line(format!(
                "let mut {} = {descriptor};",
                binding.symbol.as_str()
            ));
        }
        self.emit_structured_block(function, &match_.fallback, predeclared);
    }

    fn emit_owned_dyn_match(
        &mut self,
        function: &RirFunction,
        match_: &super::rir::RirDynMatch,
        predeclared: bool,
    ) {
        let super::rir::RirDynMatchSource::Owned { value, air_use, .. } = &match_.source else {
            unreachable!("checked by caller")
        };
        let carrier = &self.program.dyn_carriers[match_.carrier.index()];
        let RirType::Enum(id) = self.program.types[carrier.storage_ty.index()] else {
            unreachable!("verified dynamic match carrier")
        };
        let enm = &self.program.enums[id.index()];
        let values = RustValues::new(self.program, function);
        let source = values.operand(value);
        let source = if *air_use == air::DynOwnedUse::ConsumeTemporary {
            source
        } else {
            format!("&{source}")
        };
        self.w.line(format!("match {source} {{"));
        for variant in &carrier.variants {
            let rir_variant = &enm.variants[variant.id.index()];
            let path = variant_path(enm.symbol.as_str(), rir_variant.symbol.as_str());
            let arm = match_
                .arms
                .iter()
                .find(|arm| arm.variants.contains(&variant.id));
            self.indented(|this| {
                this.w.line(format!("{path}(payload) => {{"));
                this.indented(|this| {
                    if let Some(arm) = arm {
                        if let super::rir::RirDynMatchBinding::Owned(binding) = arm.binding {
                            let binding = &function.locals[binding.index()];
                            let payload = if *air_use == air::DynOwnedUse::ConsumeTemporary {
                                if variant.storage == RirDynStorage::Boxed {
                                    "*payload".to_string()
                                } else {
                                    "payload".to_string()
                                }
                            } else {
                                let payload = if variant.storage == RirDynStorage::Boxed {
                                    "payload.as_ref()"
                                } else {
                                    "payload"
                                };
                                values.dyn_payload_from_ref(
                                    variant.concrete_ty,
                                    variant.payload,
                                    payload,
                                )
                            };
                            this.w
                                .line(format!("let {} = {payload};", binding.symbol.as_str()));
                        }
                        this.emit_structured_block(function, &arm.block, predeclared);
                    } else {
                        if let Some(binding) = match_.fallback_binding {
                            let binding = &function.locals[binding.index()];
                            let payload = if *air_use == air::DynOwnedUse::ConsumeTemporary {
                                "payload".to_string()
                            } else {
                                let payload = if variant.storage == RirDynStorage::Boxed {
                                    "payload.as_ref()"
                                } else {
                                    "payload"
                                };
                                let payload = values.dyn_payload_from_ref(
                                    variant.concrete_ty,
                                    variant.payload,
                                    payload,
                                );
                                if variant.storage == RirDynStorage::Boxed {
                                    format!("Box::new({payload})")
                                } else {
                                    payload
                                }
                            };
                            this.w.line(format!(
                                "let {} = {};",
                                binding.symbol.as_str(),
                                tuple_variant(&path, [payload])
                            ));
                        }
                        this.emit_structured_block(function, &match_.fallback, predeclared);
                    }
                });
                this.w.line("}");
            });
        }
        self.w.line("}");
    }

    fn emit_option_match(
        &mut self,
        function: &RirFunction,
        index: usize,
        match_: &RirOptionMatch,
        predeclared: bool,
    ) {
        let RirOptionSubject::Place(subject) = &match_.subject else {
            self.emit_mut_place_option_match(function, index, match_, predeclared);
            return;
        };
        let RirType::Option(inner) = self.program.types[subject.ty.index()] else {
            unreachable!("verified option match")
        };
        let places = RustPlaces::new(self.program, function);
        let subject = if let RirPlaceRoot::Local(local) = subject.root
            && places.payload_ref_cell_local(local)
        {
            let tmp = format!("__anv_option_subject_{index}");
            let value = RustValues::new(self.program, function)
                .value_operand(&RirOperand::Place(subject.clone()));
            self.w.line(format_args!("let {tmp} = {value};"));
            tmp
        } else {
            places.local_place(subject)
        };
        let borrow = if match_.payload_ref { "&mut " } else { "&" };
        if match_.payload_escapes {
            let payload = match_.payload.expect("escaping option payload local");
            let local = &function.locals[payload.index()];
            self.w.line(format_args!(
                "let Some({}) = {borrow}{subject} else {{",
                local.symbol.as_str()
            ));
            self.indented(|this| {
                this.emit_structured_block(function, &match_.none_block, predeclared);
            });
            self.w.line("};");
            self.emit_structured_block(function, &match_.some_block, predeclared);
            return;
        }
        self.w.line(format_args!("match {borrow}{subject} {{"));
        let payload_ref = match match_.payload {
            Some(payload) if match_.payload_ref => {
                function.locals[payload.index()].symbol.as_str().to_string()
            }
            _ => Self::fresh_option_payload_ref(function),
        };
        self.indented(|this| {
            this.w.line(format_args!("Some({payload_ref}) => {{"));
            this.indented(|this| {
                if let Some(payload) = match_.payload
                    && !match_.payload_ref
                {
                    let local = &function.locals[payload.index()];
                    this.w.line(format_args!(
                        "{} = {};",
                        local.symbol.as_str(),
                        RustValues::new(this.program, function).value_from_ref(inner, &payload_ref)
                    ));
                }
                this.emit_structured_block(function, &match_.some_block, predeclared);
            });
            this.w.line("}");
            this.w.line("None => {");
            this.indented(|this| {
                this.emit_structured_block(function, &match_.none_block, predeclared);
            });
            this.w.line("}");
        });
        self.w.line("}");
    }

    fn emit_mut_place_option_match(
        &mut self,
        function: &RirFunction,
        index: usize,
        match_: &RirOptionMatch,
        predeclared: bool,
    ) {
        let RirOptionSubject::MutPlace(subject) = &match_.subject else {
            unreachable!("checked by caller")
        };
        debug_assert!(match_.payload_ref);
        let mut_place = self.prepared_escaping_payload_place_arg(function, index, subject);
        for line in mut_place.0 {
            self.w.line(format_args!("{line}"));
        }
        let is_some = target::mut_place_access(
            &mut_place.1,
            target::runtime_param_name(),
            "Ok(value.is_some())",
        );
        if match_.payload_escapes {
            self.w.line(format_args!("if !({is_some}) {{"));
            self.indented(|this| {
                this.emit_structured_block(function, &match_.none_block, predeclared);
            });
            self.emit_mut_place_option_some(function, index, match_, predeclared, &mut_place.1);
        } else {
            self.w.line(format_args!("if {is_some} {{"));
            self.indented(|this| {
                this.emit_mut_place_option_some(function, index, match_, predeclared, &mut_place.1);
            });
            self.w.line("} else {");
            self.indented(|this| {
                this.emit_structured_block(function, &match_.none_block, predeclared);
            });
            self.w.line("}");
        }
    }

    fn emit_mut_place_option_some(
        &mut self,
        function: &RirFunction,
        index: usize,
        match_: &RirOptionMatch,
        predeclared: bool,
        subject: &str,
    ) {
        let payload = match_
            .payload
            .expect("mutable-place option payload aliases have payload locals");
        self.emit_option_payload_alias(function, index, payload, subject);
        self.emit_structured_block(function, &match_.some_block, predeclared);
    }

    fn emit_option_payload_alias(
        &mut self,
        function: &RirFunction,
        index: usize,
        payload: RirLocalId,
        subject: &str,
    ) {
        let payload = &function.locals[payload.index()];
        let payload_ty = self.ty(payload.ty);
        let ops = format!("__anv_optional_payload_ops_{index}");
        self.w.line(format_args!(
            "let {ops}: {} = {};",
            target::optional_payload_ops_ty(&payload_ty),
            target::optional_payload_ops_ctor(&payload_ty)
        ));
        self.w.line(format_args!(
            "let {} = {};",
            payload.symbol.as_str(),
            target::scoped_mut_place_cell_new(
                &target::mut_place_projected(subject, &format!("&{ops}")),
                &target::runtime_safepoint_state("rt"),
            )
        ));
    }

    fn fresh_option_payload_ref(function: &RirFunction) -> String {
        let base = "__anv_option_payload";
        let mut candidate = base.to_string();
        let mut index = 0;
        while function
            .locals
            .iter()
            .any(|local| local.symbol.as_str() == candidate)
        {
            index += 1;
            candidate = format!("{base}_{index}");
        }
        candidate
    }

    fn emit_structured_block(
        &mut self,
        function: &RirFunction,
        block: &RirStructuredBlock,
        predeclared: bool,
    ) {
        for (index, stmt) in block.stmts.iter().enumerate() {
            self.emit_stmt_mode(function, index, stmt, predeclared);
            for local in self.slice_call_arg_drops(&block.stmts, index) {
                self.w.line(format_args!(
                    "drop({});",
                    function.locals[local.index()].symbol.as_str()
                ));
            }
            for (alias_index, payload) in escaping_map_entry_payloads(&block.stmts[..=index]) {
                let guard = format!("__anv_map_entry_guard_{alias_index}");
                let alias_used_here = stmt_uses_local(self.program, stmt, payload);
                let alias_declared_here = alias_index == index;
                if (alias_used_here || alias_declared_here)
                    && !block.stmts[index + 1..]
                        .iter()
                        .any(|stmt| stmt_uses_local(self.program, stmt, payload))
                    && !term_uses_local(self.program, &block.term, payload)
                {
                    self.emit_map_entry_alias_drop(function, alias_index, payload, &guard);
                }
            }
        }
        self.emit_term(function, &block.term);
    }

    fn slice_call_arg_drops(&self, stmts: &[RirStmt], index: usize) -> Vec<RirLocalId> {
        let Some(args) = stmt_call_args(&stmts[index]) else {
            return vec![];
        };
        let mut drops = vec![];
        for local in args.iter().filter_map(call_arg_root_local) {
            self.collect_slice_arg_drops(stmts, index, local, &mut drops);
        }
        drops
    }

    fn collect_slice_arg_drops(
        &self,
        stmts: &[RirStmt],
        index: usize,
        local: RirLocalId,
        drops: &mut Vec<RirLocalId>,
    ) {
        let Some(stmt) = stmts[..index]
            .iter()
            .rev()
            .find(|stmt| matches!(stmt, RirStmt::Init { local: initialized, .. } if *initialized == local))
        else {
            return;
        };
        match stmt {
            RirStmt::Init {
                value: RirRValue::SliceView { .. },
                ..
            } => drops.push(local),
            RirStmt::Init {
                value: RirRValue::Use(RirOperand::Place(source)),
                ..
            } if source.projections.is_empty()
                && matches!(self.program.types[source.ty.index()], RirType::Slice(_)) =>
            {
                let RirPlaceRoot::Local(source_local) = source.root else {
                    unreachable!("expected a local RIR place")
                };
                drops.push(local);
                self.collect_slice_arg_drops(stmts, index, source_local, drops);
            }
            _ => {}
        }
    }

    fn emit_term(&mut self, function: &RirFunction, term: &RirTerm) {
        match term {
            RirTerm::None => {}
            RirTerm::Return(None) => {
                if self.fallible_functions[function.id.index()] {
                    self.w.line("return Ok(());");
                } else {
                    self.w.line("return;");
                }
            }
            RirTerm::Return(Some(operand)) => {
                let value = RustValues::new(self.program, function).value_operand(operand);
                if self.fallible_functions[function.id.index()] {
                    self.w.line(format_args!("return Ok({value});"));
                } else {
                    self.w.line(format_args!("return {value};"));
                }
            }
            RirTerm::Break(id) => self.w.line(format_args!("break {};", loop_label(*id))),
            RirTerm::Continue(id) => self.w.line(format_args!("continue {};", loop_label(*id))),
            RirTerm::Unreachable => self.w.line("unreachable!();"),
        }
    }

    fn variant_pattern(enm: &RirEnum, variant: &RirVariant) -> String {
        let path = variant_path(enm.symbol.as_str(), variant.symbol.as_str());
        match variant.kind {
            RirVariantKind::Unit => unit_variant_pattern(&path),
            RirVariantKind::Tuple => tuple_variant_pattern(&path),
            RirVariantKind::Struct => struct_variant_pattern(&path),
        }
    }

    fn dyn_call(
        &self,
        function: &RirFunction,
        carrier: RirDynCarrierId,
        receiver: &RirDynReceiver,
        exact_variant: Option<RirDynVariantId>,
        args: &[RirCallArg],
        arms: &[RirDynDispatchArm],
    ) -> String {
        if let RirDynReceiver::MutPlace(place) = receiver {
            return self.dyn_mut_call(function, carrier, place, args, arms);
        }
        if let RirDynReceiver::Borrowed(borrow) = receiver {
            return self.dyn_borrowed_call(function, carrier, borrow, args, arms);
        }
        let carrier = &self.program.dyn_carriers[carrier.index()];
        let RirType::Enum(enum_id) = self.program.types[carrier.storage_ty.index()] else {
            unreachable!("verified dynamic carrier storage")
        };
        let enm = &self.program.enums[enum_id.index()];
        let values = RustValues::new(self.program, function);
        let (receiver, consume) = match receiver {
            RirDynReceiver::Owned { value, consume } => (values.operand(value), *consume),
            RirDynReceiver::Borrowed(_) => unreachable!(),
            RirDynReceiver::MutPlace(_) => {
                unreachable!("readonly dynamic dispatch excludes mutable receivers")
            }
        };
        let arms = arms
            .iter()
            .filter(|arm| exact_variant.is_none_or(|variant| arm.variant == variant))
            .map(|arm| {
                let variant = &carrier.variants[arm.variant.index()];
                let rir_variant = &enm.variants[arm.variant.index()];
                let path = variant_path(enm.symbol.as_str(), rir_variant.symbol.as_str());
                let payload = match (variant.storage, consume) {
                    (RirDynStorage::Boxed, true) => "(*payload)".to_string(),
                    (RirDynStorage::Boxed, false) => "payload.as_ref()".to_string(),
                    (RirDynStorage::Inline, _) => "payload".to_string(),
                };
                let (target, receiver, receiver_ty) =
                    self.dyn_projected_target(&arm.target, payload, variant.concrete_ty, !consume);
                let receiver = match (arm.receiver, consume) {
                    (RirParamAbi::Value, true) | (RirParamAbi::SharedBorrow, false) => receiver,
                    (RirParamAbi::Value, false) => values.value_from_ref(receiver_ty, &receiver),
                    (RirParamAbi::SharedBorrow, true) => format!("&{receiver}"),
                    _ => unreachable!("verified readonly dynamic receiver ABI"),
                };
                let call_args = self.dyn_call_args(target, args);
                let call = self.resolved_call_expr(
                    function,
                    target,
                    &call_args,
                    Some(ResolvedReceiver {
                        expr: receiver,
                        ty: receiver_ty,
                        semantic: Self::param_abi_semantic(arm.receiver),
                    }),
                );
                (path, call)
            })
            .collect::<Vec<_>>();
        let subject = if consume {
            receiver
        } else {
            format!("&{receiver}")
        };
        if exact_variant.is_some() {
            let (path, call) = arms
                .first()
                .expect("verified exact dynamic variant has a dispatch arm");
            return format!(
                "{{ let {path}(payload) = {subject} else {{ unreachable!(\"verified exact dynamic witness\") }}; {call} }}"
            );
        }
        match_expr(
            &subject,
            arms.into_iter()
                .map(|(path, call)| format!("{path}(payload) => {call}")),
        )
    }

    fn dyn_call_args(
        &self,
        target: &RirResolvedCallTarget,
        args: &[RirCallArg],
    ) -> Vec<RirCallArg> {
        let semantics = match target.base() {
            RirResolvedCallTarget::Function(id) => self.program.functions[id.index()]
                .params
                .iter()
                .skip(1)
                .map(|param| param.semantic)
                .collect::<Vec<_>>(),
            RirResolvedCallTarget::Extern(id) => self.program.externs[id.index()]
                .params
                .iter()
                .skip(1)
                .map(|param| param.semantic)
                .collect::<Vec<_>>(),
            RirResolvedCallTarget::Promoted { .. } => unreachable!(),
        };
        semantics
            .into_iter()
            .zip(args)
            .map(|(semantic, arg)| {
                arg.adapted_to(semantic, self.program)
                    .expect("verified dynamic call argument adaptation")
            })
            .collect()
    }

    fn dyn_borrowed_call(
        &self,
        function: &RirFunction,
        carrier_id: RirDynCarrierId,
        borrow: &super::rir::RirDynBorrow,
        args: &[RirCallArg],
        arms: &[RirDynDispatchArm],
    ) -> String {
        debug_assert_eq!(borrow.target, carrier_id);
        let (super::rir::RirDynBorrowSource::Borrowed { local, carrier }
        | super::rir::RirDynBorrowSource::Reborrowed { local, carrier }) = borrow.source
        else {
            unreachable!("dynamic borrowed receiver must be a descriptor root")
        };
        debug_assert_eq!(carrier, carrier_id);
        let readonly = arms
            .first()
            .expect("verified dynamic dispatch arm")
            .receiver
            .is_readonly_receiver();
        debug_assert!(
            arms.iter()
                .all(|arm| arm.receiver.is_readonly_receiver() == readonly)
        );
        self.dyn_borrowed_dispatch(function, carrier_id, local, args, arms, readonly)
    }

    fn dyn_borrowed_dispatch(
        &self,
        function: &RirFunction,
        carrier_id: RirDynCarrierId,
        local: RirLocalId,
        args: &[RirCallArg],
        arms: &[RirDynDispatchArm],
        readonly: bool,
    ) -> String {
        let carrier = &self.program.dyn_carriers[carrier_id.index()];
        let RirType::Enum(id) = self.program.types[carrier.storage_ty.index()] else {
            unreachable!("verified dynamic carrier storage")
        };
        let enm = &self.program.enums[id.index()];
        let symbol = self.program.dyn_borrow_symbol(carrier_id);
        let mut dispatch = arms
            .iter()
            .map(|arm| {
                let variant = &carrier.variants[arm.variant.index()];
                let name = enm.variants[arm.variant.index()].symbol.as_str();
                let receiver = target::mut_place_reborrow("place");
                let call = if readonly {
                    self.dyn_readonly_concrete_call(function, variant, arm, args, &receiver)
                } else {
                    self.dyn_concrete_descriptor_call(function, variant, arm, args, &receiver)
                };
                format!("{symbol}::{name}(place) => {call}")
            })
            .collect::<Vec<_>>();
        dispatch.push(format!(
            "{symbol}::Owned(place) => {}",
            self.dyn_borrowed_carrier_call(function, carrier_id, arms, args, "place", readonly)
        ));
        for weakening in self
            .program
            .dyn_weakenings
            .iter()
            .filter(|weakening| weakening.target == carrier_id)
        {
            let source_arms = weakening
                .arms
                .iter()
                .map(|map| {
                    let mut arm = arms
                        .iter()
                        .find(|arm| arm.variant == map.target)
                        .expect("verified weakened dispatch arm")
                        .clone();
                    arm.variant = map.source;
                    arm
                })
                .collect::<Vec<_>>();
            dispatch.push(format!(
                "{symbol}::OwnedFrom{}(place) => {}",
                weakening.source.index(),
                self.dyn_borrowed_carrier_call(
                    function,
                    weakening.source,
                    &source_arms,
                    args,
                    "place",
                    readonly,
                )
            ));
        }
        let local = function.locals[local.index()].symbol.as_str();
        match_expr(&format!("&mut {local}"), dispatch)
    }

    fn dyn_borrowed_carrier_call(
        &self,
        function: &RirFunction,
        carrier: RirDynCarrierId,
        arms: &[RirDynDispatchArm],
        args: &[RirCallArg],
        place: &str,
        readonly: bool,
    ) -> String {
        let carrier_decl = &self.program.dyn_carriers[carrier.index()];
        let values = RustValues::new(self.program, function);
        self.dyn_carrier_descriptor_dispatch(carrier, arms, place, |arm| {
            if !readonly {
                return self.dyn_mut_descriptor_call(function, carrier, arm, args);
            }
            let variant = &carrier_decl.variants[arm.variant.index()];
            let payload = values.dyn_payload_projection_descriptor(
                "__AnvDynPayloadOps",
                carrier,
                &[arm.variant],
                variant.concrete_ty,
            );
            let root = target::mut_place_reborrow("__anv_carrier_place");
            let receiver = target::mut_place_projected(&root, "&__anv_payload_ops");
            let call = self.dyn_readonly_concrete_call(function, variant, arm, args, &receiver);
            block_expr(
                [
                    payload.struct_decl,
                    payload.impl_decl,
                    format!("let __anv_payload_ops = {};", payload.ctor),
                ],
                Some(call),
            )
        })
    }

    fn dyn_carrier_descriptor_dispatch(
        &self,
        carrier: RirDynCarrierId,
        arms: &[RirDynDispatchArm],
        place: &str,
        mut render: impl FnMut(&RirDynDispatchArm) -> String,
    ) -> String {
        let carrier_decl = &self.program.dyn_carriers[carrier.index()];
        let RirType::Enum(id) = self.program.types[carrier_decl.storage_ty.index()] else {
            unreachable!("verified dynamic carrier storage")
        };
        let enm = &self.program.enums[id.index()];
        let tag = match_expr(
            "value",
            arms.iter().enumerate().map(|(index, arm)| {
                let variant = &enm.variants[arm.variant.index()];
                let path = variant_path(enm.symbol.as_str(), variant.symbol.as_str());
                format!("{path}(..) => {index}")
            }),
        );
        let probe = target::mut_place_reborrow("__anv_carrier_place");
        let stmts = [
            format!("let mut __anv_carrier_root = {place};"),
            format!(
                "let mut __anv_carrier_place = {};",
                target::mut_place_reborrow("__anv_carrier_root")
            ),
            format!(
                "let __anv_variant = {};",
                target::mut_place_access_ctx(
                    &probe,
                    target::runtime_param_name(),
                    &format!("{{ let _ = rt; Ok({tag}) }}"),
                )
            ),
        ];
        let dispatch = arms
            .iter()
            .enumerate()
            .map(|(index, arm)| format!("{index} => {}", render(arm)))
            .chain(std::iter::once(
                "_ => unreachable!(\"verified dynamic dispatch tag\")".to_string(),
            ));
        block_expr(stmts, Some(match_expr("__anv_variant", dispatch)))
    }

    fn dyn_concrete_receiver<'a>(
        &self,
        function: &RirFunction,
        target: &'a RirResolvedCallTarget,
        concrete_ty: RirTypeId,
        receiver: &str,
    ) -> (&'a RirResolvedCallTarget, RirTypeId, Vec<String>) {
        let (target, fields, receiver_ty) = self.dyn_mut_projected_target(target, concrete_ty);
        let mut stmts = vec![format!("let mut __anv_receiver = {receiver};")];
        if fields.is_empty() {
            return (target, receiver_ty, stmts);
        }
        let projections = fields
            .into_iter()
            .map(RirProjection::Field)
            .collect::<Vec<_>>();
        let descriptor = RustValues::new(self.program, function)
            .mut_place_projection_descriptor_for(
                "__AnvPromotedReceiverOps",
                concrete_ty,
                "__anv_receiver",
                receiver_ty,
                &projections,
            );
        stmts.extend([descriptor.struct_decl, descriptor.impl_decl]);
        stmts.push(format!("let __anv_promoted_ops = {};", descriptor.ctor));
        let root = target::mut_place_reborrow("__anv_receiver");
        stmts.push(format!(
            "let mut __anv_receiver = {};",
            target::mut_place_projected(&root, "&__anv_promoted_ops")
        ));
        (target, receiver_ty, stmts)
    }

    fn dyn_readonly_concrete_call(
        &self,
        function: &RirFunction,
        variant: &super::rir::RirDynVariant,
        arm: &RirDynDispatchArm,
        args: &[RirCallArg],
        receiver: &str,
    ) -> String {
        let (target, receiver_ty, mut stmts) =
            self.dyn_concrete_receiver(function, &arm.target, variant.concrete_ty, receiver);
        let values = RustValues::new(self.program, function);
        let materialized = values.value_from_ref(receiver_ty, "value");
        let place = target::mut_place_reborrow("__anv_receiver");
        stmts.push(format!(
            "let __anv_receiver_value = {};",
            target::mut_place_access_ctx(
                &place,
                target::runtime_param_name(),
                &format!("{{ Ok({materialized}) }}"),
            )
        ));
        let args = self.dyn_call_args(target, args);
        let receiver = match arm.receiver {
            RirParamAbi::Value => "__anv_receiver_value".to_string(),
            RirParamAbi::SharedBorrow => "&__anv_receiver_value".to_string(),
            _ => unreachable!("verified readonly dynamic receiver ABI"),
        };
        let call = self.resolved_call_expr(
            function,
            target,
            &args,
            Some(ResolvedReceiver {
                expr: receiver,
                ty: receiver_ty,
                semantic: Self::param_abi_semantic(arm.receiver),
            }),
        );
        block_expr(stmts, Some(call))
    }

    fn dyn_concrete_descriptor_call(
        &self,
        function: &RirFunction,
        variant: &super::rir::RirDynVariant,
        arm: &RirDynDispatchArm,
        args: &[RirCallArg],
        receiver: &str,
    ) -> String {
        let (target, receiver_ty, mut stmts) =
            self.dyn_concrete_receiver(function, &arm.target, variant.concrete_ty, receiver);
        let args = self.dyn_call_args(target, args);
        let call = match arm.receiver {
            RirParamAbi::MutPlace => self.resolved_call_expr(
                function,
                target,
                &args,
                Some(ResolvedReceiver {
                    expr: "__anv_receiver".to_string(),
                    ty: receiver_ty,
                    semantic: RirParamSemantic::MutPlace,
                }),
            ),
            RirParamAbi::MutBorrow => {
                debug_assert!(matches!(target, RirResolvedCallTarget::Extern(_)));
                debug_assert!(native_ty_is_resource_ref(self.program, receiver_ty));
                let snapshot =
                    RustValues::new(self.program, function).value_from_ref(receiver_ty, "value");
                let place = target::mut_place_reborrow("__anv_receiver");
                stmts.push(format!(
                    "let mut __anv_native_receiver = {};",
                    target::mut_place_access_ctx(
                        &place,
                        target::runtime_param_name(),
                        &format!("{{ Ok({snapshot}) }}"),
                    )
                ));
                self.resolved_call_expr(
                    function,
                    target,
                    &args,
                    Some(ResolvedReceiver {
                        expr: "&mut __anv_native_receiver".to_string(),
                        ty: receiver_ty,
                        semantic: RirParamSemantic::MutBorrow,
                    }),
                )
            }
            _ => unreachable!("verified mutable dynamic receiver ABI"),
        };
        block_expr(stmts, Some(call))
    }

    fn dyn_mut_call(
        &self,
        function: &RirFunction,
        carrier: RirDynCarrierId,
        place: &RirMutPlaceArg,
        args: &[RirCallArg],
        arms: &[RirDynDispatchArm],
    ) -> String {
        let (prelude, place) = self.prepared_escaping_payload_place_arg(function, 0, place);
        let call = self.dyn_carrier_descriptor_dispatch(carrier, arms, &place, |arm| {
            self.dyn_mut_descriptor_call(function, carrier, arm, args)
        });
        block_expr(prelude, Some(call))
    }

    fn dyn_mut_descriptor_call(
        &self,
        function: &RirFunction,
        carrier: RirDynCarrierId,
        arm: &RirDynDispatchArm,
        args: &[RirCallArg],
    ) -> String {
        let variant = &self.program.dyn_carriers[carrier.index()].variants[arm.variant.index()];
        let payload = RustValues::new(self.program, function).dyn_payload_projection_descriptor(
            "__AnvDynPayloadOps",
            carrier,
            &[arm.variant],
            variant.concrete_ty,
        );
        let stmts = [
            payload.struct_decl,
            payload.impl_decl,
            format!("let __anv_payload_ops = {};", payload.ctor),
        ];
        let root = target::mut_place_reborrow("__anv_carrier_place");
        let receiver = target::mut_place_projected(&root, "&__anv_payload_ops");
        let call = self.dyn_concrete_descriptor_call(function, variant, arm, args, &receiver);
        block_expr(stmts, Some(call))
    }

    fn dyn_mut_projected_target<'a>(
        &self,
        target: &'a RirResolvedCallTarget,
        mut ty: RirTypeId,
    ) -> (
        &'a RirResolvedCallTarget,
        Vec<super::rir::RirFieldId>,
        RirTypeId,
    ) {
        let mut target = target;
        let mut fields = vec![];
        while let RirResolvedCallTarget::Promoted {
            fields: promoted,
            target: next,
        } = target
        {
            for field in promoted {
                let RirType::Struct(id) = self.program.types[ty.index()] else {
                    unreachable!("verified promoted mutable dynamic receiver")
                };
                let field_decl = &self.program.structs[id.index()].fields[field.index()];
                fields.push(*field);
                ty = field_decl.ty;
            }
            target = next;
        }
        (target, fields, ty)
    }

    fn dyn_projected_target<'a>(
        &self,
        target: &'a RirResolvedCallTarget,
        mut receiver: String,
        mut ty: RirTypeId,
        borrowed: bool,
    ) -> (&'a RirResolvedCallTarget, String, RirTypeId) {
        let mut target = target;
        while let RirResolvedCallTarget::Promoted {
            fields,
            target: next,
        } = target
        {
            for field in fields {
                let RirType::Struct(id) = self.program.types[ty.index()] else {
                    unreachable!("verified promoted dynamic receiver")
                };
                let field = &self.program.structs[id.index()].fields[field.index()];
                receiver = if borrowed {
                    format!("&({receiver}).{}", field.symbol.as_str())
                } else {
                    format!("({receiver}).{}", field.symbol.as_str())
                };
                ty = field.ty;
            }
            target = next;
        }
        (target, receiver, ty)
    }

    fn rvalue(&mut self, function: &RirFunction, value: &RirRValue) -> String {
        let values = RustValues::new(self.program, function);
        let places = RustPlaces::new(self.program, function);
        match value {
            RirRValue::DynCopy { value, .. } => values.value_operand(value),
            RirRValue::DynPack {
                carrier,
                variant,
                value,
                action,
                ..
            } => {
                let carrier = &self.program.dyn_carriers[carrier.index()];
                let RirType::Enum(enum_id) = self.program.types[carrier.storage_ty.index()] else {
                    unreachable!("verified dynamic carrier storage")
                };
                let enm = &self.program.enums[enum_id.index()];
                let rir_variant = &enm.variants[variant.index()];
                let mut payload = values.dyn_payload(value, *action);
                if carrier.variants[variant.index()].storage == RirDynStorage::Boxed {
                    payload = format!("Box::new({payload})");
                }
                tuple_variant(
                    &variant_path(enm.symbol.as_str(), rir_variant.symbol.as_str()),
                    [payload],
                )
            }
            RirRValue::DynDowncast {
                carrier,
                air_use,
                value,
                variants,
                ..
            } => {
                let carrier = &self.program.dyn_carriers[carrier.index()];
                let RirType::Enum(id) = self.program.types[carrier.storage_ty.index()] else {
                    unreachable!("verified dynamic carrier storage")
                };
                let enm = &self.program.enums[id.index()];
                let arms = carrier.variants.iter().map(|variant| {
                    let rir_variant = &enm.variants[variant.id.index()];
                    let path = variant_path(enm.symbol.as_str(), rir_variant.symbol.as_str());
                    if !variants.contains(&variant.id) {
                        return format!("{path}(..) => None");
                    }
                    let payload = if *air_use == air::DynOwnedUse::ConsumeTemporary {
                        if variant.storage == RirDynStorage::Boxed {
                            "*payload".to_string()
                        } else {
                            "payload".to_string()
                        }
                    } else {
                        let payload = if variant.storage == RirDynStorage::Boxed {
                            "payload.as_ref()"
                        } else {
                            "payload"
                        };
                        values.dyn_payload_from_ref(variant.concrete_ty, variant.payload, payload)
                    };
                    format!("{path}(payload) => Some({payload})")
                });
                let source = values.operand(value);
                let source = if *air_use == air::DynOwnedUse::ConsumeTemporary {
                    source
                } else {
                    format!("&{source}")
                };
                match_expr(&source, arms)
            }
            RirRValue::DynCall {
                carrier,
                exact_variant,
                receiver,
                args,
                arms,
                ..
            } => self.dyn_call(function, *carrier, receiver, *exact_variant, args, arms),
            RirRValue::DynWeaken {
                source,
                target,
                air_use,
                value,
                arms,
                ..
            } => {
                let source = &self.program.dyn_carriers[source.index()];
                let target = &self.program.dyn_carriers[target.index()];
                let RirType::Enum(source_enum) = self.program.types[source.storage_ty.index()]
                else {
                    unreachable!("verified source dynamic storage")
                };
                let RirType::Enum(target_enum) = self.program.types[target.storage_ty.index()]
                else {
                    unreachable!("verified target dynamic storage")
                };
                let source_enum = &self.program.enums[source_enum.index()];
                let target_enum = &self.program.enums[target_enum.index()];
                let arms = arms.iter().map(|arm| {
                    let source_plan = &source.variants[arm.source.index()];
                    let source_variant = &source_enum.variants[arm.source.index()];
                    let target_variant = &target_enum.variants[arm.target.index()];
                    let source_path =
                        variant_path(source_enum.symbol.as_str(), source_variant.symbol.as_str());
                    let target_path =
                        variant_path(target_enum.symbol.as_str(), target_variant.symbol.as_str());
                    let payload = if *air_use == air::DynOwnedUse::ConsumeTemporary {
                        "payload".to_string()
                    } else {
                        let payload = if source_plan.storage == RirDynStorage::Boxed {
                            "payload.as_ref()"
                        } else {
                            "payload"
                        };
                        let payload = values.dyn_payload_from_ref(
                            source_plan.concrete_ty,
                            source_plan.payload,
                            payload,
                        );
                        if source_plan.storage == RirDynStorage::Boxed {
                            format!("Box::new({payload})")
                        } else {
                            payload
                        }
                    };
                    format!(
                        "{source_path}(payload) => {}",
                        tuple_variant(&target_path, [payload])
                    )
                });
                let source = values.operand(value);
                let source = if *air_use == air::DynOwnedUse::ConsumeTemporary {
                    source
                } else {
                    format!("&{source}")
                };
                match_expr(&source, arms)
            }
            RirRValue::Use(operand) | RirRValue::FunctionValue { value: operand, .. } => {
                values.value_operand(operand)
            }
            RirRValue::MoveValue { value, .. } => values.operand(value),
            RirRValue::Struct { ty, fields } => self.struct_literal(function, *ty, fields),
            RirRValue::Tuple { ty, fields } => self.tuple_literal(function, *ty, fields),
            RirRValue::DataRefAlloc { ty, fields } => self.dataref_alloc(function, *ty, fields),
            RirRValue::DataRefGet {
                object,
                dataref,
                projections,
                ty,
            } => self.dataref_get(function, object, *dataref, projections, *ty),
            RirRValue::CellGetCopy { cell, .. } => self.cell_get_copy(function, *cell),
            RirRValue::ScopedPlaceCellGet { cell, ty } => {
                values.scoped_place_cell_value(*cell, *ty)
            }
            RirRValue::MutPlaceGetCopy { .. } => {
                unreachable!("mutable-place reads need statement-local prelude")
            }
            RirRValue::Array { elems, .. } => {
                format!(
                    "[{}]",
                    comma(elems.iter().map(|elem| values.value_operand(elem)))
                )
            }
            RirRValue::List { ty, elems } => {
                let elems = comma(elems.iter().map(|elem| values.value_operand(elem)));
                let storage_ty = self.collection_storage_heap_type(*ty);
                target::anv_list_from_elems(target::runtime_param_name(), &storage_ty, &elems)
            }
            RirRValue::Map { ty, entries } => {
                let entries = comma(entries.iter().map(|(key, value)| {
                    format!(
                        "({}, {})",
                        values.value_operand(key),
                        values.value_operand(value)
                    )
                }));
                let storage_ty = self.collection_storage_heap_type(*ty);
                target::anv_map_from_entries(target::runtime_param_name(), &storage_ty, &entries)
            }
            RirRValue::EnumVariant {
                ty,
                variant,
                fields,
            } => self.enum_literal(function, *ty, *variant, fields),
            RirRValue::Unary { value, ty, .. }
                if matches!(self.program.types[ty.index()], RirType::Flag(_)) =>
            {
                target::flag_complement(self.flag_symbol(*ty), &values.operand(value))
            }
            RirRValue::Unary { op, value, .. } => {
                format!("{}{}", unary_op(*op), values.operand(value))
            }
            RirRValue::Binary { op, lhs, rhs, ty }
                if matches!(self.program.types[ty.index()], RirType::Flag(_)) =>
            {
                target::flag_bitwise(
                    self.flag_symbol(*ty),
                    &values.operand(lhs),
                    binary_op(*op),
                    &values.operand(rhs),
                )
            }
            RirRValue::Binary { op, lhs, rhs, .. } => {
                format!(
                    "{} {} {}",
                    values.operand(lhs),
                    binary_op(*op),
                    values.operand(rhs)
                )
            }
            RirRValue::SharedRefEq { lhs, rhs, negated } => {
                let eq = format!("{}.ptr_eq(&{})", values.operand(lhs), values.operand(rhs));
                if *negated { format!("!{eq}") } else { eq }
            }
            RirRValue::Cast { value, target } => self.cast(function, value, *target),
            RirRValue::RawProject { value, target } => self.raw_project(function, value, *target),
            RirRValue::RawTryConstruct { value, target, .. } => {
                self.raw_try_construct(function, value, *target)
            }
            RirRValue::FlagStatic { op, ty } => match op {
                RirFlagStaticOp::Empty => target::flag_empty(self.flag_symbol(*ty)),
                RirFlagStaticOp::All => target::flag_all(self.flag_symbol(*ty)),
            },
            RirRValue::OptionalSome { value, .. } => {
                format!("Some({})", values.value_operand(value))
            }
            RirRValue::Call { callee, args, .. } => match callee {
                RirCallTarget::Function(id) => self.resolved_call_expr(
                    function,
                    &RirResolvedCallTarget::Function(*id),
                    args,
                    None,
                ),
                RirCallTarget::Extern(id) => self.resolved_call_expr(
                    function,
                    &RirResolvedCallTarget::Extern(*id),
                    args,
                    None,
                ),
                RirCallTarget::LambdaValue { callee, sig } => {
                    let receiver = values.value_operand(callee);
                    let call =
                        self.call_expr(function, args, |args| format!("{receiver}.call({args})"));
                    if self.lambda_sig_fallible(&self.program.lambda_sigs[sig.index()]) {
                        format!("{call}?")
                    } else {
                        call
                    }
                }
            },
            RirRValue::Stringify { value, source_ty } => {
                match self.program.types[source_ty.index()] {
                    RirType::String => values.value_operand(value),
                    RirType::Float => target::anv_string_from_float(&values.operand(value)),
                    RirType::Int | RirType::Bool | RirType::Char => {
                        target::anv_string_format("\"{}\"", &values.operand(value))
                    }
                    RirType::Struct(_) | RirType::Enum(_) | RirType::Flag(_) => {
                        self.stringify_planned(function, value, *source_ty)
                    }
                    RirType::Void
                    | RirType::Tuple(_)
                    | RirType::DataRef(_)
                    | RirType::Array { .. }
                    | RirType::List(_)
                    | RirType::Map { .. }
                    | RirType::Option(_)
                    | RirType::Slice(_)
                    | RirType::Lambda(_) => {
                        unreachable!("verified structural stringify target gap")
                    }
                }
            }
            RirRValue::StringConcat { parts } => self.string_concat(function, parts),
            RirRValue::Format {
                value,
                source_ty,
                spec,
            } if *spec == RirFormatSpec::default()
                && matches!(self.program.types[source_ty.index()], RirType::Float) =>
            {
                target::anv_string_from_float(&values.operand(value))
            }
            RirRValue::Format {
                value,
                source_ty,
                spec,
            } => target::anv_string_format(
                &rust_string(&format_fragment(rust_format_spec(*spec))),
                &values.format_arg(value, *source_ty),
            ),
            RirRValue::Len { source } => format!("{}.len() as i64", places.local_place(source)),
            RirRValue::CollectionLen { source } => self.collection_len(function, source),
            RirRValue::SequenceSlotAt {
                collection,
                index,
                ty,
            } => self.sequence_slot_at(function, collection, *index, *ty),
            RirRValue::ListPush { list, value } => {
                let elem = values.value_operand(value);
                self.collection_mutation(
                    function,
                    list,
                    [("__anv_elem", elem)],
                    &target::list_push_ctx_region("__anv_elem"),
                )
            }
            RirRValue::SliceView {
                source,
                start,
                end,
                inclusive,
                mutable,
                ..
            } => self.slice_view(function, source, *start, *end, *inclusive, *mutable),
            RirRValue::RangeListCopy {
                source,
                start,
                end,
                inclusive,
                ty,
            } => self.range_list_copy(function, source, *start, *end, *inclusive, *ty),
            RirRValue::MapGet { map, key, ty } => self.map_get(function, map, key, *ty),
            RirRValue::MapInsert {
                map, key, value, ..
            } => {
                let key = values.value_operand(key);
                let value = values.value_operand(value);
                self.collection_mutation(
                    function,
                    map,
                    [("__anv_key", key), ("__anv_insert", value)],
                    &target::map_insert_region("__anv_key", "__anv_insert"),
                )
            }
            RirRValue::MapRemove { map, key, .. } => {
                let key = values.operand(key);
                self.collection_mutation(
                    function,
                    map,
                    [("__anv_key", key)],
                    &target::map_remove_region("__anv_key"),
                )
            }
            RirRValue::CheckedIterCount { count, check } => {
                let count = values.operand(count);
                match check {
                    RirIterCountCheck::SkipNonNegative => target::checked_iter_skip(&count),
                    RirIterCountCheck::TakeNonNegative => target::checked_iter_take(&count),
                    RirIterCountCheck::StepByPositive => target::checked_iter_step_by(&count),
                }
            }
            RirRValue::MapEntryAt { map, index, ty } => {
                self.map_entry_at(function, map, *index, *ty)
            }
            RirRValue::MapKeyAt { map, index, ty } => self.map_key_at(function, map, *index, *ty),
            RirRValue::MapValueAt { map, index, ty } => {
                self.map_value_at(function, map, *index, *ty)
            }
            RirRValue::Lambda {
                lambda, captures, ..
            } => {
                let lambda_decl = &self.program.lambdas[lambda.index()];
                let sig = RirRustRepPolicy::new(self.program).lambda_sig_symbol(lambda_decl.sig);
                let variant = lambda_variant(*lambda);
                match lambda_decl.storage {
                    RirLambdaStorage::ZeroEnv => format!("{sig}::{variant}"),
                    RirLambdaStorage::HeapEnv { env } => {
                        let env = &self.program.lambda_envs[env.index()];
                        let fields = env
                            .fields
                            .iter()
                            .enumerate()
                            .map(|(index, field)| {
                                let capture_decl = &lambda_decl.captures[index];
                                let capture = &captures[index];
                                field_init(
                                    field.symbol.as_str(),
                                    self.lambda_capture_arg(function, capture_decl, capture),
                                )
                            })
                            .collect::<Vec<_>>();
                        let heap_type = format!("statics.{}", lambda_env_heap_type_symbol(env.id));
                        let storage = format!("{} {{ {} }}", env.symbol.as_str(), comma(fields));
                        let alloc = target::rt_heap_alloc("rt", "heap_type", &storage);
                        format!(
                            "{{ let heap_type = {heap_type}; {sig}::{variant} {{ env: {alloc} }} }}"
                        )
                    }
                    RirLambdaStorage::ScopedCaptures => {
                        let fields = lambda_decl
                            .captures
                            .iter()
                            .zip(captures)
                            .enumerate()
                            .map(|(index, (decl, capture))| {
                                field_init(
                                    &format!("c{index}"),
                                    self.lambda_capture_arg(function, decl, capture),
                                )
                            })
                            .collect::<Vec<_>>();
                        format!("{sig}::{variant} {{ {} }}", comma(fields))
                    }
                }
            }
        }
    }

    fn mut_place_set(&self, ty: RirTypeId, place: &str, value: &str) -> String {
        if self.program.collection_replace_ty(ty) {
            target::mut_place_replace_collection(place, target::runtime_param_name(), value)
        } else {
            target::mut_place_set(place, target::runtime_param_name(), value)
        }
    }

    fn collection_len(&self, function: &RirFunction, source: &RirCollectionAccess) -> String {
        match source {
            RirCollectionAccess::Direct(source) => {
                let (prelude, collection) =
                    self.direct_collection(function, source, "__anv_collection", false);
                let len = target::collection_len(&collection);
                if prelude.is_empty() {
                    len
                } else {
                    block_expr(prelude, Some(len))
                }
            }
            RirCollectionAccess::MutPlace(source) => self.mut_place_access_block(
                function,
                source,
                &format!("Ok({})", target::collection_len("value")),
            ),
        }
    }

    fn sequence_slot_at(
        &self,
        function: &RirFunction,
        collection: &RirCollectionAccess,
        index: RirLocalId,
        ty: RirTypeId,
    ) -> String {
        match collection {
            RirCollectionAccess::Direct(collection) => match self.program.types
                [collection.ty.index()]
            {
                RirType::List(_) => self.list_slot_at(function, collection, index, ty),
                RirType::Array { .. } | RirType::Slice(_) => {
                    let place = Self::sequence_slot_place(collection, index, ty);
                    RustValues::new(self.program, function).value_operand(&RirOperand::Place(place))
                }
                _ => unreachable!("verified sequence slot collection"),
            },
            RirCollectionAccess::MutPlace(collection) => match self.program.types
                [collection.ty.index()]
            {
                RirType::Array { .. } => {
                    self.mut_place_array_slot_at(function, collection, index, ty)
                }
                RirType::List(_) => self.mut_place_list_slot_at(function, collection, index, ty),
                RirType::Slice(_) => self.mut_place_slice_slot_at(function, collection, index, ty),
                _ => unreachable!("RIR verifier rejects unsupported mut-place sequence slots"),
            },
        }
    }

    fn sequence_slot_set(
        &self,
        function: &RirFunction,
        collection: &RirCollectionAccess,
        index: RirLocalId,
        value: &str,
    ) -> String {
        match collection {
            RirCollectionAccess::Direct(collection) => {
                match self.program.types[collection.ty.index()] {
                    RirType::List(_) => self.list_slot_set(function, collection, index, value),
                    RirType::Array { .. } | RirType::Slice(_)
                        if matches!(collection.root, RirPlaceRoot::Global(_)) =>
                    {
                        self.direct_sequence_slot_set(function, collection, index, value)
                    }
                    RirType::Array { elem, .. } | RirType::Slice(elem) => {
                        let place = Self::sequence_slot_place(collection, index, elem);
                        RustValues::new(self.program, function).assign(&place, value)
                    }
                    _ => unreachable!("verified sequence slot collection"),
                }
            }
            RirCollectionAccess::MutPlace(collection) => {
                match self.program.types[collection.ty.index()] {
                    RirType::Array { elem, .. } => {
                        self.mut_place_array_slot_set(function, collection, index, value, elem)
                    }
                    RirType::List(_) => {
                        self.mut_place_list_slot_set(function, collection, index, value)
                    }
                    RirType::Slice(_) => {
                        self.mut_place_slice_slot_set(function, collection, index, value)
                    }
                    _ => unreachable!("RIR verifier rejects unsupported mut-place sequence slots"),
                }
            }
        }
    }

    fn direct_sequence_slot_set(
        &self,
        function: &RirFunction,
        collection: &RirPlace,
        index: RirLocalId,
        value: &str,
    ) -> String {
        let (mut prelude, collection_expr) =
            self.direct_collection(function, collection, "__anv_collection_guard", true);
        let index = function.locals[index.index()].symbol.as_str();
        prelude.extend([
            format!("let __anv_sequence = &mut ({collection_expr});"),
            format!(
                "let index = {};",
                target::checked_index_result(index, "__anv_sequence.len()", "sequence")
            ),
            format!("let __anv_sequence_value = {value};"),
        ]);
        let update = match self.program.types[collection.ty.index()] {
            RirType::Array { elem, .. } if self.program.collection_replace_ty(elem) => {
                format!(
                    "{}?",
                    target::replace_collection_result(
                        "__anv_sequence[index]",
                        "__anv_sequence_value"
                    )
                )
            }
            RirType::Array { .. } => "__anv_sequence[index] = __anv_sequence_value".to_string(),
            RirType::Slice(_) => format!(
                "{}?",
                target::slice_with_elem_mut_leaf(
                    "__anv_sequence",
                    target::runtime_param_name(),
                    "index",
                    "*value = __anv_sequence_value; Ok(())",
                )
            ),
            _ => unreachable!("verified sequence slot collection"),
        };
        block_expr(prelude, Some(update))
    }

    fn list_slot_at(
        &self,
        function: &RirFunction,
        collection: &RirPlace,
        index: RirLocalId,
        ty: RirTypeId,
    ) -> String {
        let (mut prelude, collection_expr) =
            self.direct_collection(function, collection, "__anv_collection_guard", false);
        let index = function.locals[index.index()].symbol.as_str();
        let version = self.collection_version(
            &RirCollectionAccess::Direct(collection.clone()),
            target::collection_structural_version("__anv_list"),
        );
        let value = RustValues::new(self.program, function).value_from_ref(ty, "value");
        prelude.extend([
            format!("let __anv_list = &({collection_expr});"),
            format!(
                "let index = {};",
                target::checked_index_result(index, "__anv_list.len()", "list")
            ),
        ]);
        block_expr(
            prelude,
            Some(format!(
                "{}?",
                target::list_with_elem_shared_short(
                    "__anv_list",
                    target::runtime_param_name(),
                    "index",
                    &version,
                    &format!("Ok({value})"),
                )
            )),
        )
    }

    fn list_slot_set(
        &self,
        function: &RirFunction,
        collection: &RirPlace,
        index: RirLocalId,
        value: &str,
    ) -> String {
        let (mut prelude, collection_expr) =
            self.direct_collection(function, collection, "__anv_collection_guard", true);
        let index = function.locals[index.index()].symbol.as_str();
        let version = self.collection_version(
            &RirCollectionAccess::Direct(collection.clone()),
            target::collection_structural_version("__anv_list"),
        );
        prelude.extend([
            format!("let __anv_list = &mut ({collection_expr});"),
            format!(
                "let index = {};",
                target::checked_index_result(index, "__anv_list.len()", "list")
            ),
            format!("let __anv_sequence_value = {value};"),
        ]);
        block_expr(
            prelude,
            Some(format!(
                "{}?",
                target::list_with_elem_mut_leaf(
                    "__anv_list",
                    target::runtime_param_name(),
                    "index",
                    &version,
                    "*value = __anv_sequence_value; Ok(())",
                )
            )),
        )
    }

    fn mut_place_access_block(
        &self,
        function: &RirFunction,
        collection: &RirMutPlaceArg,
        body: &str,
    ) -> String {
        let (prelude, place) = self.prepared_mut_place_arg(function, 0, collection);
        block_expr(
            prelude,
            Some(target::mut_place_access_ctx(
                &place,
                target::runtime_param_name(),
                body,
            )),
        )
    }

    fn mut_place_mutate_block(
        &self,
        function: &RirFunction,
        collection: &RirMutPlaceArg,
        body: &str,
    ) -> String {
        let (prelude, place) = self.prepared_mut_place_arg(function, 0, collection);
        block_expr(
            prelude,
            Some(target::mut_place_mutate_ctx(
                &place,
                target::runtime_param_name(),
                body,
            )),
        )
    }

    fn mut_place_array_slot_at(
        &self,
        function: &RirFunction,
        collection: &RirMutPlaceArg,
        index: RirLocalId,
        ty: RirTypeId,
    ) -> String {
        let index = function.locals[index.index()].symbol.as_str();
        let elem = RustValues::new(self.program, function).value_from_ref(ty, "elem");
        let body = block_expr(
            [format!(
                "let index = {};",
                target::checked_index_result(index, "value.len()", "array")
            )],
            Some(format!("let elem = &value[index]; Ok({elem})")),
        );
        self.mut_place_access_block(function, collection, &body)
    }

    fn mut_place_array_slot_set(
        &self,
        function: &RirFunction,
        collection: &RirMutPlaceArg,
        index: RirLocalId,
        new_value: &str,
        elem: RirTypeId,
    ) -> String {
        let index = function.locals[index.index()].symbol.as_str();
        let update = if self.program.collection_replace_ty(elem) {
            format!(
                "{}?; Ok(())",
                target::replace_collection_result("value[index]", "__anv_sequence_value")
            )
        } else {
            "value[index] = __anv_sequence_value; Ok(())".to_string()
        };
        let body = block_expr(
            [
                format!(
                    "let index = {};",
                    target::checked_index_result(index, "value.len()", "array")
                ),
                format!("let __anv_sequence_value = {new_value};"),
            ],
            Some(update),
        );
        self.mut_place_mutate_block(function, collection, &body)
    }

    fn mut_place_list_slot_at(
        &self,
        function: &RirFunction,
        collection: &RirMutPlaceArg,
        index: RirLocalId,
        ty: RirTypeId,
    ) -> String {
        let index = function.locals[index.index()].symbol.as_str();
        let version = self.collection_version(
            &RirCollectionAccess::MutPlace(collection.clone()),
            target::collection_structural_version("value"),
        );
        let value = RustValues::new(self.program, function).value_from_ref(ty, "value");
        let body = block_expr(
            [format!(
                "let index = {};",
                target::checked_index_result(index, "value.len()", "list")
            )],
            Some(format!(
                "Ok({}?)",
                target::list_with_elem_shared_short(
                    "value",
                    target::runtime_param_name(),
                    "index",
                    &version,
                    &format!("Ok({value})"),
                )
            )),
        );
        self.mut_place_access_block(function, collection, &body)
    }

    fn mut_place_list_slot_set(
        &self,
        function: &RirFunction,
        collection: &RirMutPlaceArg,
        index: RirLocalId,
        new_value: &str,
    ) -> String {
        let index = function.locals[index.index()].symbol.as_str();
        let version = self.collection_version(
            &RirCollectionAccess::MutPlace(collection.clone()),
            target::collection_structural_version("value"),
        );
        let body = block_expr(
            [
                format!(
                    "let index = {};",
                    target::checked_index_result(index, "value.len()", "list")
                ),
                format!("let __anv_sequence_value = {new_value};"),
            ],
            Some(format!(
                "{}?; Ok(())",
                target::list_with_elem_mut_leaf(
                    "value",
                    target::runtime_param_name(),
                    "index",
                    &version,
                    "*value = __anv_sequence_value; Ok(())",
                )
            )),
        );
        self.mut_place_mutate_block(function, collection, &body)
    }

    fn mut_place_slice_slot_at(
        &self,
        function: &RirFunction,
        collection: &RirMutPlaceArg,
        index: RirLocalId,
        _ty: RirTypeId,
    ) -> String {
        let index = function.locals[index.index()].symbol.as_str();
        let body = block_expr(
            [format!("let index = {index};")],
            Some(format!(
                "Ok({}?)",
                target::slice_elem_at_shared("value", target::runtime_param_name(), "index")
            )),
        );
        self.mut_place_access_block(function, collection, &body)
    }

    fn mut_place_slice_slot_set(
        &self,
        function: &RirFunction,
        collection: &RirMutPlaceArg,
        index: RirLocalId,
        new_value: &str,
    ) -> String {
        let index = function.locals[index.index()].symbol.as_str();
        let body = block_expr(
            [
                format!("let index = {index};"),
                format!("let __anv_sequence_value = {new_value};"),
            ],
            Some(format!(
                "{}?; Ok(())",
                target::slice_with_elem_mut_leaf(
                    "value",
                    target::runtime_param_name(),
                    "index",
                    "*value = __anv_sequence_value; Ok(())",
                )
            )),
        );
        self.mut_place_mutate_block(function, collection, &body)
    }

    fn sequence_slot_place(collection: &RirPlace, index: RirLocalId, ty: RirTypeId) -> RirPlace {
        let mut place = collection.clone();
        place.projections.push(RirProjection::Index(index));
        place.ty = ty;
        place
    }

    fn direct_collection(
        &self,
        function: &RirFunction,
        collection: &RirPlace,
        guard: &str,
        mutable: bool,
    ) -> (Vec<String>, String) {
        match collection.root {
            RirPlaceRoot::Local(_) => (
                vec![],
                RustPlaces::new(self.program, function).local_place(collection),
            ),
            RirPlaceRoot::Global(global) => {
                let root_ty = self.program.globals[global.index()].ty;
                let root = if mutable {
                    format!("(&mut *{guard})")
                } else {
                    format!("(&*{guard})")
                };
                let collection_expr = if collection.projections.is_empty() {
                    root
                } else {
                    RustPlaces::new(self.program, function)
                        .projected_expr(root_ty, &root, collection.ty, &collection.projections)
                        .expect("verified global collection projection")
                };
                let slot = RustValues::global_slot_expr(self.program, global);
                let init = RustValues::global_init_call(self.program, global);
                let access = if mutable {
                    target::global_write(&slot, &init)
                } else {
                    target::global_read(&slot, &init)
                };
                let binding = if mutable { "let mut" } else { "let" };
                (
                    vec![format!("{binding} {guard} = {access};")],
                    collection_expr,
                )
            }
        }
    }

    fn map_get(
        &self,
        function: &RirFunction,
        map: &RirCollectionAccess,
        key: &RirOperand,
        ty: RirTypeId,
    ) -> String {
        let RirType::Option(value_ty) = self.program.types[ty.index()] else {
            unreachable!("verified map get result")
        };
        let key = RustValues::new(self.program, function).operand(key);
        self.map_read(function, map, |map| {
            let value = RustValues::new(self.program, function).value_from_ref(value_ty, "value");
            format!(
                "{}?",
                target::map_with_value_shared_short(
                    map,
                    target::runtime_param_name(),
                    &format!("&{key}"),
                    &format!("Ok(value.map(|value| {value}))"),
                )
            )
        })
    }

    fn map_value_set(
        &self,
        function: &RirFunction,
        map: &RirCollectionAccess,
        index: RirLocalId,
        value: &RirOperand,
    ) -> String {
        let value = RustValues::new(self.program, function).value_operand(value);
        let set = self.map_index_update(function, map, index, |map, version| {
            target::map_with_value_mut_short(
                map,
                target::runtime_param_name(),
                "index",
                version,
                "*value = __anv_map_value; Ok(())",
            )
        });
        format!("{{ let __anv_map_value = {value}; {set}; }}")
    }

    fn map_key_at(
        &self,
        function: &RirFunction,
        map: &RirCollectionAccess,
        index: RirLocalId,
        _ty: RirTypeId,
    ) -> String {
        self.map_index_read(function, map, index, |map, version| {
            target::map_key_at_shared(map, target::runtime_param_name(), "index", version)
        })
    }

    fn map_value_at(
        &self,
        function: &RirFunction,
        map: &RirCollectionAccess,
        index: RirLocalId,
        _ty: RirTypeId,
    ) -> String {
        self.map_index_read(function, map, index, |map, version| {
            target::map_value_at_shared(map, target::runtime_param_name(), "index", version)
        })
    }

    fn map_entry_at(
        &self,
        function: &RirFunction,
        map: &RirCollectionAccess,
        index: RirLocalId,
        ty: RirTypeId,
    ) -> String {
        let tuple = match self.program.types[ty.index()] {
            RirType::Tuple(id) => &self.program.tuples[id.index()],
            _ => unreachable!("verified map entry tuple"),
        };
        self.map_index_read(function, map, index, |map, version| {
            let key =
                target::map_key_at_shared(map, target::runtime_param_name(), "index", version);
            let value =
                target::map_value_at_shared(map, target::runtime_param_name(), "index", version);
            Self::map_entry_tuple(tuple, &key, &value)
        })
    }

    fn map_read(
        &self,
        function: &RirFunction,
        map: &RirCollectionAccess,
        value: impl FnOnce(&str) -> String,
    ) -> String {
        match map {
            RirCollectionAccess::Direct(map) => {
                let (prelude, map_expr) =
                    self.direct_collection(function, map, "__anv_map_guard", false);
                block_expr(
                    prelude
                        .into_iter()
                        .chain([format!("let __anv_map = &({map_expr});")]),
                    Some(value("__anv_map")),
                )
            }
            RirCollectionAccess::MutPlace(map) => {
                self.mut_place_access_block(function, map, &format!("Ok({})", value("value")))
            }
        }
    }

    fn map_index_read(
        &self,
        function: &RirFunction,
        map: &RirCollectionAccess,
        index: RirLocalId,
        value: impl FnOnce(&str, &str) -> String,
    ) -> String {
        match map {
            RirCollectionAccess::Direct(place) => {
                let index = function.locals[index.index()].symbol.as_str();
                let checked = target::checked_index_result(index, "__anv_map.len()", "map entry");
                let version = self
                    .collection_version(map, target::collection_structural_version("__anv_map"));
                let (prelude, map_expr) =
                    self.direct_collection(function, place, "__anv_map_guard", false);
                block_expr(
                    prelude.into_iter().chain([
                        format!("let __anv_map = &({map_expr});"),
                        format!("let index = {checked};"),
                    ]),
                    Some(value("__anv_map", &version)),
                )
            }
            RirCollectionAccess::MutPlace(map) => {
                let index = function.locals[index.index()].symbol.as_str();
                let version = self.collection_version(
                    &RirCollectionAccess::MutPlace(map.clone()),
                    target::collection_structural_version("value"),
                );
                let body = block_expr(
                    [format!(
                        "let index = {};",
                        target::checked_index_result(index, "value.len()", "map entry")
                    )],
                    Some(format!("Ok({})", value("value", &version))),
                );
                self.mut_place_access_block(function, map, &body)
            }
        }
    }

    fn map_index_update(
        &self,
        function: &RirFunction,
        map: &RirCollectionAccess,
        index: RirLocalId,
        update: impl FnOnce(&str, &str) -> String,
    ) -> String {
        match map {
            RirCollectionAccess::Direct(place) => {
                let index = function.locals[index.index()].symbol.as_str();
                let checked = target::checked_index_result(index, "__anv_map.len()", "map entry");
                let version = self
                    .collection_version(map, target::collection_structural_version("__anv_map"));
                let (prelude, map_expr) =
                    self.direct_collection(function, place, "__anv_map_guard", true);
                format!(
                    "{}?",
                    block_expr(
                        prelude.into_iter().chain([
                            format!("let __anv_map = &mut ({map_expr});"),
                            format!("let index = {checked};"),
                        ]),
                        Some(update("__anv_map", &version)),
                    )
                )
            }
            RirCollectionAccess::MutPlace(map) => {
                let index = function.locals[index.index()].symbol.as_str();
                let version = self.collection_version(
                    &RirCollectionAccess::MutPlace(map.clone()),
                    target::collection_structural_version("value"),
                );
                let body = block_expr(
                    [format!(
                        "let index = {};",
                        target::checked_index_result(index, "value.len()", "map entry")
                    )],
                    Some(format!("{}?; Ok(())", update("value", &version))),
                );
                self.mut_place_mutate_block(function, map, &body)
            }
        }
    }

    fn map_entry_tuple(tuple: &super::rir::RirTuple, key: &str, value: &str) -> String {
        struct_lit(
            tuple.symbol.as_str(),
            [
                field_init(tuple.fields[0].symbol.as_str(), key.to_string()),
                field_init(tuple.fields[1].symbol.as_str(), value.to_string()),
            ],
        )
    }

    fn collection_shape_loan(
        &self,
        function: &RirFunction,
        root: &RirCollectionAccess,
        index: usize,
    ) -> (Vec<String>, String) {
        match root {
            RirCollectionAccess::Direct(root) => {
                let mut prelude = vec![];
                if let RirPlaceRoot::Global(global) = root.root
                    && !root.projections.is_empty()
                {
                    prelude.push(format!(
                        "let __anv_global_projected_loan_{index} = {};",
                        target::global_begin_projected_loan(&RustValues::global_slot_expr(
                            self.program,
                            global,
                        ))
                    ));
                }
                let loan = if let Some(loan) = self.global_collection_loan(function, root, index) {
                    loan
                } else {
                    target::begin_shape_loan(
                        &RustPlaces::new(self.program, function).local_place(root),
                    )
                };
                (prelude, loan)
            }
            RirCollectionAccess::MutPlace(root) => {
                let (prelude, place) = self.prepared_mut_place_arg(function, index, root);
                (
                    prelude,
                    target::mut_place_begin_shape_loan(&place, target::runtime_param_name()),
                )
            }
        }
    }

    fn global_collection_loan(
        &self,
        function: &RirFunction,
        root: &RirPlace,
        index: usize,
    ) -> Option<String> {
        let RirPlaceRoot::Global(global) = root.root else {
            return None;
        };
        let root_ty = self.program.globals[global.index()].ty;
        let guard = format!("__anv_global_{index}");
        let root_expr = format!("(&*{guard})");
        let collection = if root.projections.is_empty() {
            root_expr
        } else {
            RustPlaces::new(self.program, function).projected_expr(
                root_ty,
                &root_expr,
                root.ty,
                &root.projections,
            )?
        };
        Some(block_expr(
            [format!(
                "let {guard} = {};",
                target::global_read(
                    &RustValues::global_slot_expr(self.program, global),
                    &RustValues::global_init_call(self.program, global),
                )
            )],
            Some(target::begin_shape_loan(&collection)),
        ))
    }

    fn collection_mutation<const N: usize>(
        &self,
        function: &RirFunction,
        root: &RirCollectionAccess,
        bindings: [(&str, String); N],
        body: &str,
    ) -> String {
        match root {
            RirCollectionAccess::Direct(root) => {
                let (mut prelude, collection) =
                    self.direct_collection(function, root, "__anv_collection_guard", true);
                prelude.extend(
                    bindings
                        .into_iter()
                        .map(|(name, value)| format!("let {name} = {value};")),
                );
                prelude.push(format!("let value = &mut ({collection});"));
                block_expr(prelude, Some(format!("{body}?")))
            }
            RirCollectionAccess::MutPlace(root) => {
                let requires_detached_leaf = !root.projections.is_empty()
                    || matches!(
                        root.access,
                        RirMutPlaceAccess::DataRef { .. }
                            | RirMutPlaceAccess::Handle(RirMutPlaceHandle::HeapCell { .. })
                    );
                if !requires_detached_leaf {
                    let (mut prelude, place) = self.prepared_mut_place_arg(function, 0, root);
                    prelude.extend(
                        bindings
                            .into_iter()
                            .map(|(name, value)| format!("let {name} = {value};")),
                    );
                    return block_expr(
                        prelude,
                        Some(target::mut_place_mutate_ctx(
                            &place,
                            target::runtime_param_name(),
                            body,
                        )),
                    );
                }
                let values = RustValues::new(self.program, function);
                let (mut prelude, place) =
                    self.prepared_escaping_payload_place_arg(function, 0, root);
                prelude.push(format!("let mut __anv_collection_place = {place};"));
                let place = target::mut_place_reborrow("__anv_collection_place");
                prelude.extend(
                    bindings
                        .into_iter()
                        .map(|(name, value)| format!("let {name} = {value};")),
                );
                let snapshot = values.value_from_ref(root.ty, "value");
                prelude.push(format!(
                    "let mut __anv_collection = {};",
                    target::mut_place_access(
                        &place,
                        target::runtime_param_name(),
                        &format!("Ok({snapshot})"),
                    )
                ));
                prelude.push("let value = &mut __anv_collection;".to_string());
                prelude.push(format!("let __anv_mutation = {body}?;"));
                prelude.push(format!(
                    "{};",
                    target::mut_place_replace_collection(
                        &place,
                        target::runtime_param_name(),
                        "__anv_collection",
                    )
                ));
                block_expr(prelude, Some("__anv_mutation".to_string()))
            }
        }
    }

    fn collection_version(&self, root: &RirCollectionAccess, fallback: String) -> String {
        self.collection_loans
            .iter()
            .rev()
            .find(|loan| loan.root == *root)
            .map_or(fallback, |loan| loan.version.clone())
    }

    fn cell_decl(&self, cell: RirCellRef) -> &RirCellDecl {
        match cell {
            RirCellRef::Owner(cell) | RirCellRef::Capture { cell, .. } => {
                &self.program.cells[cell.index()]
            }
        }
    }

    fn cell_ref(&self, function: &RirFunction, cell: RirCellRef) -> String {
        match cell {
            RirCellRef::Owner(cell) => self.program.cells[cell.index()].symbol.as_str().to_string(),
            RirCellRef::Capture { local, .. } => {
                function.locals[local.index()].symbol.as_str().to_string()
            }
        }
    }

    fn scoped_place_cell_ref(&self, function: &RirFunction, cell: RirScopedPlaceCellRef) -> String {
        match cell {
            RirScopedPlaceCellRef::Owner(cell) => self.program.scoped_place_cells[cell.index()]
                .symbol
                .as_str()
                .to_string(),
            RirScopedPlaceCellRef::Capture { local, .. } => {
                function.locals[local.index()].symbol.as_str().to_string()
            }
        }
    }

    fn scoped_place_cell_capture_arg(
        &self,
        function: &RirFunction,
        cell: RirScopedPlaceCellRef,
    ) -> String {
        match cell {
            RirScopedPlaceCellRef::Owner(_) => {
                format!("&{}", self.scoped_place_cell_ref(function, cell))
            }
            RirScopedPlaceCellRef::Capture { .. } => self.scoped_place_cell_ref(function, cell),
        }
    }

    fn cell_payload_ty(&self, cell: RirCellRef) -> String {
        self.ty(self.cell_decl(cell).payload_ty)
    }

    fn cell_ty(&self, _function: &RirFunction, cell: RirCellRef) -> String {
        let payload = self.cell_payload_ty(cell);
        match self.cell_decl(cell).storage {
            RirCellStorage::StackScoped => target::stack_lambda_cell_ty(&payload),
            RirCellStorage::Heap => target::handle_ty(&target::lambda_cell_ty(&payload)),
        }
    }

    fn cell_init(&mut self, function: &RirFunction, cell: RirCellRef, value: &RirRValue) -> String {
        let payload = self.cell_payload_ty(cell);
        let value = self.rvalue(function, value);
        match self.cell_decl(cell).storage {
            RirCellStorage::StackScoped => {
                format!(
                    "{}::new_with_safepoint({value}, {})",
                    target::stack_lambda_cell_ctor(&payload),
                    target::runtime_safepoint_state("rt")
                )
            }
            RirCellStorage::Heap => {
                let heap_type = format!(
                    "statics.{}",
                    lambda_cell_heap_type_symbol(self.cell_decl(cell).id)
                );
                let storage = format!(
                    "{}::new_with_safepoint(value, safepoint)",
                    target::lambda_cell_ctor(&payload)
                );
                format!(
                    "{{ let value = {value}; let heap_type = {heap_type}; let safepoint = {}; {} }}",
                    target::runtime_safepoint_state("rt"),
                    target::rt_heap_alloc("rt", "heap_type", &storage)
                )
            }
        }
    }

    fn cell_set(&mut self, function: &RirFunction, cell: RirCellRef, value: &RirRValue) -> String {
        let value = self.rvalue(function, value);
        let decl = self.cell_decl(cell);
        let set =
            target::lambda_cell_set(&value, self.program.collection_replace_ty(decl.payload_ty));
        match decl.storage {
            RirCellStorage::StackScoped => {
                format!("{}.{}?", self.cell_ref(function, cell), set)
            }
            RirCellStorage::Heap => {
                target::rt_heap_with(
                    "rt",
                    &format!("&{}", self.cell_ref(function, cell)),
                    "cell",
                    &format!("cell.{set}"),
                ) + "?"
            }
        }
    }

    fn cell_get_copy(&self, function: &RirFunction, cell: RirCellRef) -> String {
        let decl = self.cell_decl(cell);
        let policy = RirRustRepPolicy::new(self.program);
        let access = if !policy.copyable(decl.payload_ty) && policy.shareable_value(decl.payload_ty)
        {
            let values = RustValues::new(self.program, function);
            let value = values.value_from_ref(decl.payload_ty, "value");
            format!("access(|value| Ok({value}))")
        } else {
            "get_copy()".to_string()
        };
        match decl.storage {
            RirCellStorage::StackScoped => format!("{}.{}?", self.cell_ref(function, cell), access),
            RirCellStorage::Heap => {
                target::rt_heap_with(
                    "rt",
                    &format!("&{}", self.cell_ref(function, cell)),
                    "cell",
                    &format!("cell.{access}"),
                ) + "?"
            }
        }
    }

    fn param_abi_semantic(abi: RirParamAbi) -> RirParamSemantic {
        match abi {
            RirParamAbi::Value => RirParamSemantic::Value,
            RirParamAbi::SharedBorrow => RirParamSemantic::SharedBorrow,
            RirParamAbi::MutBorrow => RirParamSemantic::MutBorrow,
            RirParamAbi::MutPlace => RirParamSemantic::MutPlace,
            RirParamAbi::DynBorrow => RirParamSemantic::DynBorrow,
            RirParamAbi::ScopedLambda => RirParamSemantic::ScopedLambda,
            RirParamAbi::EscapingLambda => RirParamSemantic::EscapingLambda,
            RirParamAbi::AnvCallback => RirParamSemantic::AnvCallback,
            RirParamAbi::StackCell => RirParamSemantic::StackCell,
            RirParamAbi::HeapCell => RirParamSemantic::HeapCell,
            RirParamAbi::ScopedPlaceCell => RirParamSemantic::ScopedPlaceCell,
        }
    }

    fn resolved_call_expr(
        &self,
        function: &RirFunction,
        target: &RirResolvedCallTarget,
        args: &[RirCallArg],
        receiver: Option<ResolvedReceiver>,
    ) -> String {
        match target {
            RirResolvedCallTarget::Function(id) => {
                let symbol = self.program.functions[id.index()].symbol.as_str();
                let call = match receiver {
                    Some(receiver) => {
                        let mut rendered = if self.has_retained_callbacks() {
                            target::retained_generated_call_args([])
                        } else {
                            target::generated_call_args([])
                        };
                        rendered.push(receiver.expr);
                        self.prepared_call_expr(function, args, rendered, |args| {
                            format!("{symbol}({args})")
                        })
                    }
                    None => self.call_expr(function, args, |args| format!("{symbol}({args})")),
                };
                if self.fallible_functions[id.index()] {
                    format!("{call}?")
                } else {
                    call
                }
            }
            RirResolvedCallTarget::Extern(id) => self.extern_call(function, *id, args, receiver),
            RirResolvedCallTarget::Promoted { .. } => {
                unreachable!("promoted paths must be projected before call emission")
            }
        }
    }

    fn call_expr(
        &self,
        function: &RirFunction,
        args: &[RirCallArg],
        render: impl FnOnce(String) -> String,
    ) -> String {
        let rendered = if self.has_retained_callbacks() {
            target::retained_generated_call_args([])
        } else {
            target::generated_call_args([])
        };
        self.prepared_call_expr(function, args, rendered, render)
    }

    fn prepared_call_expr(
        &self,
        function: &RirFunction,
        args: &[RirCallArg],
        mut rendered: Vec<String>,
        render: impl FnOnce(String) -> String,
    ) -> String {
        let mut prelude = vec![];
        for (index, arg) in args.iter().enumerate() {
            let (stmts, expr) = self.prepared_call_arg(function, index, arg);
            prelude.extend(stmts);
            rendered.push(expr);
        }
        let call = render(comma(rendered));
        if prelude.is_empty() {
            call
        } else {
            format!("{{ {} {call} }}", prelude.join(" "))
        }
    }

    fn prepared_native_call_expr(
        &self,
        function: &RirFunction,
        args: &[RirCallArg],
        native_plan: &NativeCallPlan,
        abis: &[anvyx_runtime::RustParamAbi],
        tys: &[RirTypeId],
        receiver: Option<ResolvedReceiver>,
        mut rendered: Vec<String>,
        render: impl FnOnce(String) -> String,
    ) -> String {
        let mut resource_borrows = vec![];
        let offset = if let Some(receiver) = receiver {
            let abi = &abis[0];
            let ty = tys[0];
            debug_assert_eq!(receiver.ty, ty);
            let facts = native_dynamic_arg_facts(self.program, ty, receiver.semantic);
            let action = native_plan.arg_action(0, facts);
            let mut expr = receiver.expr;
            if let Some(mutable) = action.native_ref_borrow_mutability() {
                let arg = "__anv_ref_arg_0".to_string();
                resource_borrows.push((expr, arg.clone(), mutable));
                expr = arg;
            } else {
                expr = self.native_arg_expr(abi, ty, expr);
            }
            rendered.push(expr);
            1
        } else {
            0
        };
        let (prelude, rendered, mut args_borrows) =
            self.prepared_native_args(function, args, native_plan, abis, tys, offset, rendered);
        resource_borrows.append(&mut args_borrows);
        let mut call = render(comma(rendered));
        for (resource, arg, mutable) in resource_borrows.into_iter().rev() {
            call = target::native_ref_borrow(&resource, &arg, mutable, &call);
        }
        if prelude.is_empty() {
            call
        } else {
            block_expr(prelude, Some(call))
        }
    }

    fn prepared_native_args(
        &self,
        function: &RirFunction,
        args: &[RirCallArg],
        native_plan: &NativeCallPlan,
        abis: &[anvyx_runtime::RustParamAbi],
        tys: &[RirTypeId],
        offset: usize,
        mut rendered: Vec<String>,
    ) -> PreparedNativeArgs {
        let mut prelude = vec![];
        let mut resource_borrows = vec![];
        let values = RustValues::new(self.program, function);
        for (arg_index, arg) in args.iter().enumerate() {
            let index = arg_index + offset;
            let (stmts, mut expr) = self.prepared_call_arg(function, index, arg);
            let (Some(abi), Some(ty)) = (abis.get(index), tys.get(index)) else {
                prelude.extend(stmts);
                rendered.push(expr);
                continue;
            };
            let action = native_plan.arg_action(index, native_arg_facts(self.program, *ty, arg));
            if action == NativeArgAction::SnapshotString {
                let tmp = format!("__anv_native_arg_{index}");
                let snapshot = format!("{}::from({expr})", target::anv_string_ty());
                let init = if stmts.is_empty() {
                    snapshot
                } else {
                    format!("{{ {} {snapshot} }}", stmts.join(" "))
                };
                prelude.push(format!("let {tmp} = {init};"));
                expr = values.borrow_temp_arg(*ty, &tmp);
            } else {
                prelude.extend(stmts);
            }
            if let anvyx_runtime::RustParamAbi::InitField(inner) = abi {
                expr = match arg {
                    RirCallArg::InitFieldProvided(_) => {
                        let value = self.native_arg_expr(inner, *ty, expr);
                        target::init_field_provided(&value)
                    }
                    RirCallArg::InitFieldOmitted => target::init_field_omitted(),
                    _ => unreachable!("verified init field ABI"),
                };
            } else if let Some(mutable) = action.native_ref_borrow_mutability() {
                let arg = format!("__anv_ref_arg_{index}");
                resource_borrows.push((expr, arg.clone(), mutable));
                expr = arg;
            } else {
                expr = self.native_arg_expr(abi, *ty, expr);
            }
            rendered.push(expr);
        }
        (prelude, rendered, resource_borrows)
    }

    fn native_array_map_expr(expr: &str, body: &str) -> String {
        if body == "value" {
            expr.to_string()
        } else {
            target::rust_array_map(expr, body)
        }
    }

    fn native_arg_expr(
        &self,
        abi: &anvyx_runtime::RustParamAbi,
        ty: RirTypeId,
        expr: String,
    ) -> String {
        match abi {
            anvyx_runtime::RustParamAbi::Value(native_ty)
            | anvyx_runtime::RustParamAbi::OwnedNamed(native_ty) => {
                self.native_value_arg_expr(native_ty, ty, expr)
            }
            anvyx_runtime::RustParamAbi::Option(inner) => {
                let RirType::Option(inner_ty) = self.program.types[ty.index()] else {
                    unreachable!("verified native option param type")
                };
                target::rust_option_map(
                    &expr,
                    &self.native_arg_expr(inner, inner_ty, "value".to_string()),
                )
            }
            anvyx_runtime::RustParamAbi::Result(ok, err) => {
                self.native_result_arg_expr(ty, ok, err, &expr)
            }
            anvyx_runtime::RustParamAbi::InitField(inner) => self.native_arg_expr(inner, ty, expr),
            anvyx_runtime::RustParamAbi::Slice(_)
            | anvyx_runtime::RustParamAbi::Borrow(_)
            | anvyx_runtime::RustParamAbi::MutBorrow(_)
            | anvyx_runtime::RustParamAbi::MutPlace(_)
            | anvyx_runtime::RustParamAbi::ScopedLambda(_)
            | anvyx_runtime::RustParamAbi::EscapingLambda(_)
            | anvyx_runtime::RustParamAbi::AnvCallback(_) => expr,
        }
    }

    fn native_value_arg_expr(
        &self,
        abi: &anvyx_runtime::ExternTypeExpr,
        ty: RirTypeId,
        expr: String,
    ) -> String {
        match abi {
            anvyx_runtime::ExternTypeExpr::Unit => target::rust_unit(),
            anvyx_runtime::ExternTypeExpr::Option(inner) => {
                let RirType::Option(inner_ty) = self.program.types[ty.index()] else {
                    unreachable!("verified native option param type")
                };
                target::rust_option_map(
                    &expr,
                    &self.native_value_arg_expr(inner, inner_ty, "value".to_string()),
                )
            }
            anvyx_runtime::ExternTypeExpr::Result(ok, err) => {
                self.native_result_value_arg_expr(ty, ok, err, &expr)
            }
            anvyx_runtime::ExternTypeExpr::Tuple(fields) => {
                self.native_tuple_arg_expr(ty, fields, &expr)
            }
            anvyx_runtime::ExternTypeExpr::Array { elem, .. } => {
                let RirType::Array { elem: elem_ty, .. } = self.program.types[ty.index()] else {
                    unreachable!("verified native array param type")
                };
                Self::native_array_map_expr(
                    &expr,
                    &self.native_value_arg_expr(elem, elem_ty, "value".to_string()),
                )
            }
            _ => expr,
        }
    }

    fn native_tuple_arg_expr(
        &self,
        ty: RirTypeId,
        fields: &[anvyx_runtime::ExternTypeExpr],
        expr: &str,
    ) -> String {
        let RirType::Tuple(tuple_id) = self.program.types[ty.index()] else {
            unreachable!("verified native tuple param type")
        };
        let tuple = &self.program.tuples[tuple_id.index()];
        let fields = tuple.fields.iter().zip(fields).map(|(field, abi)| {
            self.native_value_arg_expr(abi, field.ty, format!("{expr}.{}", field.symbol.as_str()))
        });
        target::rust_tuple(fields)
    }

    fn native_result_value_arg_expr(
        &self,
        ty: RirTypeId,
        ok: &anvyx_runtime::ExternTypeExpr,
        err: &anvyx_runtime::ExternTypeExpr,
        expr: &str,
    ) -> String {
        let (enm, ok_variant, err_variant) = self.result_enum(ty);
        let ok_path = variant_path(enm.symbol.as_str(), ok_variant.symbol.as_str());
        let err_path = variant_path(enm.symbol.as_str(), err_variant.symbol.as_str());
        target::rust_result_match(
            expr,
            &format!(
                "{ok_path}(value) => {}",
                target::rust_ok(&self.native_value_arg_expr(
                    ok,
                    ok_variant.fields[0].ty,
                    "value".to_string()
                ))
            ),
            &format!(
                "{err_path}(value) => {}",
                target::rust_err(&self.native_value_arg_expr(
                    err,
                    err_variant.fields[0].ty,
                    "value".to_string()
                ))
            ),
        )
    }

    fn native_result_arg_expr(
        &self,
        ty: RirTypeId,
        ok: &anvyx_runtime::RustParamAbi,
        err: &anvyx_runtime::RustParamAbi,
        expr: &str,
    ) -> String {
        let (enm, ok_variant, err_variant) = self.result_enum(ty);
        let ok_path = variant_path(enm.symbol.as_str(), ok_variant.symbol.as_str());
        let err_path = variant_path(enm.symbol.as_str(), err_variant.symbol.as_str());
        target::rust_result_match(
            expr,
            &format!(
                "{ok_path}(value) => {}",
                target::rust_ok(&self.native_arg_expr(
                    ok,
                    ok_variant.fields[0].ty,
                    "value".to_string()
                ))
            ),
            &format!(
                "{err_path}(value) => {}",
                target::rust_err(&self.native_arg_expr(
                    err,
                    err_variant.fields[0].ty,
                    "value".to_string()
                ))
            ),
        )
    }

    fn prepared_call_arg(
        &self,
        function: &RirFunction,
        index: usize,
        arg: &RirCallArg,
    ) -> (Vec<String>, String) {
        let values = RustValues::new(self.program, function);
        if let RirCallArg::DynBorrow(borrow) = arg {
            return self.prepared_dyn_borrow_arg(function, index, borrow);
        }
        if let RirCallArg::ScopedLambda { callee, sig } = arg {
            return self.prepared_scoped_lambda_call_arg(function, index, callee, *sig);
        }
        if let RirCallArg::EscapingLambda { callee, sig } = arg {
            return self.prepared_escaping_lambda_call_arg(function, index, callee, *sig);
        }
        if let RirCallArg::AnvCallback { callee, sig } = arg {
            return self.prepared_anv_callback_call_arg(function, index, callee, *sig);
        }
        if let RirCallArg::Value(operand @ RirOperand::Place(place))
        | RirCallArg::InitFieldProvided(operand @ RirOperand::Place(place)) = arg
        {
            let places = RustPlaces::new(self.program, function);
            let payload_ref_root = match place.root {
                RirPlaceRoot::Local(local) => places.payload_ref_cell_local(local),
                RirPlaceRoot::Global(_) => false,
            };
            if matches!(place.root, RirPlaceRoot::Global(_))
                || !place.projections.is_empty()
                || payload_ref_root
            {
                let tmp = format!("__anv_arg_{index}");
                return (
                    vec![format!("let {tmp} = {};", values.value_operand(operand))],
                    tmp,
                );
            }
        }
        if let RirCallArg::SharedBorrow(place) = arg
            && let RirPlaceRoot::Global(global) = place.root
        {
            let tmp = format!("__anv_global_borrow_{index}");
            return (
                vec![values.global_value_binding(global, &tmp)],
                values.borrow_temp_arg(place.ty, &tmp),
            );
        }
        let RirCallArg::MutPlace(mut_place) = arg else {
            return (vec![], values.call_arg(arg));
        };
        self.prepared_mut_place_arg(function, index, mut_place)
    }

    fn prepared_dyn_borrow_arg(
        &self,
        function: &RirFunction,
        index: usize,
        borrow: &super::rir::RirDynBorrow,
    ) -> (Vec<String>, String) {
        let symbol = self.program.dyn_borrow_symbol(borrow.target);
        let weakening = borrow.air_weakening.map(|id| {
            self.program
                .dyn_weakenings
                .iter()
                .find(|weakening| weakening.air_id == id)
                .expect("verified dynamic borrow weakening")
        });
        match &borrow.source {
            super::rir::RirDynBorrowSource::Concrete {
                place,
                carrier,
                air_witness,
            } => {
                let source = &self.program.dyn_carriers[carrier.index()];
                let source_variant = source
                    .variants
                    .iter()
                    .find(|variant| variant.air_witness == *air_witness)
                    .expect("verified dynamic borrow witness");
                let target_variant = weakening.map_or(source_variant.id, |weakening| {
                    weakening
                        .arms
                        .iter()
                        .find(|arm| arm.source == source_variant.id)
                        .expect("verified dynamic borrow weakening arm")
                        .target
                });
                let target = &self.program.dyn_carriers[borrow.target.index()];
                let RirType::Enum(id) = self.program.types[target.storage_ty.index()] else {
                    unreachable!("verified dynamic carrier storage")
                };
                let name = self.program.enums[id.index()].variants[target_variant.index()]
                    .symbol
                    .as_str();
                let (stmts, place) = self.prepared_mut_place_arg(function, index, place);
                (stmts, format!("{symbol}::{name}({place})"))
            }
            super::rir::RirDynBorrowSource::Owned { place, carrier } => {
                let (stmts, place) = self.prepared_mut_place_arg(function, index, place);
                let constructor = if *carrier == borrow.target {
                    "Owned".to_string()
                } else {
                    format!("OwnedFrom{}", carrier.index())
                };
                (stmts, format!("{symbol}::{constructor}({place})"))
            }
            super::rir::RirDynBorrowSource::Borrowed { local, carrier }
            | super::rir::RirDynBorrowSource::Reborrowed { local, carrier } => {
                let local = function.locals[local.index()].symbol.as_str();
                if *carrier == borrow.target {
                    (vec![], format!("{local}.reborrow()"))
                } else {
                    (
                        vec![],
                        format!("{symbol}::weaken_from_{}(&mut {local})", carrier.index()),
                    )
                }
            }
        }
    }

    fn mut_place_get_copy(
        &mut self,
        function: &RirFunction,
        index: usize,
        mut_place: &RirMutPlaceArg,
    ) -> String {
        let (prelude, place) = self.prepared_mut_place_arg(function, index, mut_place);
        for line in prelude {
            self.w.line(format_args!("{line}"));
        }
        target::mut_place_get_copy(&place, target::runtime_param_name())
    }

    fn scoped_place_cell_needs_slot_init(cell: &RirScopedPlaceCellDecl) -> bool {
        matches!(
            cell.source.place().access,
            RirMutPlaceAccess::Handle(RirMutPlaceHandle::Local { .. })
        )
    }

    fn emit_scoped_place_cell_init(
        &mut self,
        function: &RirFunction,
        cell: &RirScopedPlaceCellDecl,
    ) {
        let (prelude, source) =
            self.prepared_scoped_place_cell_source(function, cell.id.index(), cell.source.place());
        for line in prelude {
            self.w.line(format_args!("{line}"));
        }
        self.w.line(format_args!(
            "let {} = {};",
            cell.symbol.as_str(),
            target::scoped_mut_place_cell_new(&source, &target::runtime_safepoint_state("rt"))
        ));
    }

    fn prepared_scoped_place_cell_source(
        &self,
        function: &RirFunction,
        index: usize,
        mut_place: &RirMutPlaceArg,
    ) -> (Vec<String>, String) {
        let RirMutPlaceAccess::Handle(RirMutPlaceHandle::Local { local, ty }) = mut_place.access
        else {
            return self.prepared_mut_place_arg(function, index, mut_place);
        };
        if function.locals[local.index()].payload_ref {
            return self.prepared_mut_place_arg(function, index, mut_place);
        }
        let root = target::mut_place_local_raw(function.locals[local.index()].symbol.as_str());
        if mut_place.projections.is_empty() {
            return (vec![], root);
        }
        self.prepared_projected_mut_place(function, index, mut_place, ty, &root)
    }

    fn prepared_mut_place_arg(
        &self,
        function: &RirFunction,
        index: usize,
        mut_place: &RirMutPlaceArg,
    ) -> (Vec<String>, String) {
        let values = RustValues::new(self.program, function);
        if !mut_place.projections.is_empty()
            && let Some((root_ty, root)) = values.mut_place_access_arg(&mut_place.access)
        {
            return self.prepared_projected_mut_place(function, index, mut_place, root_ty, &root);
        }
        let RirMutPlaceAccess::DataRef { object, dataref } = &mut_place.access else {
            return (
                vec![],
                values
                    .mut_place_access_arg(&mut_place.access)
                    .expect("verified mut-place access")
                    .1,
            );
        };
        self.prepared_dataref_place(
            function,
            object,
            *dataref,
            &mut_place.projections,
            mut_place.ty,
            &format!("__anv_dataref_place_object_{index}"),
            &format!("__anv_dataref_place_ops_{index}"),
        )
    }

    fn prepared_dataref_place(
        &self,
        function: &RirFunction,
        object: &RirOperand,
        dataref: RirDataRefId,
        projections: &[RirProjection],
        ty: RirTypeId,
        object_tmp: &str,
        ops_tmp: &str,
    ) -> (Vec<String>, String) {
        let values = RustValues::new(self.program, function);
        let descriptor = self
            .dataref_places
            .find(dataref, projections, ty)
            .expect("verified dataref place descriptor");
        let object = values.operand_ref(object);
        let heap_type = descriptor.heap_type_field(self.program);
        (
            vec![
                format!(
                    "let {object_tmp} = {};",
                    target::rt_heap_erase("rt", &object)
                ),
                format!(
                    "let {ops_tmp} = {} {{ {}: {} }};",
                    descriptor.symbol,
                    target::dataref_place_heap_type_field(),
                    target::heap_type_access("statics", &heap_type)
                ),
            ],
            target::mut_place_dataref(object_tmp, &format!("&{ops_tmp}")),
        )
    }

    fn prepared_escaping_payload_place_arg(
        &self,
        function: &RirFunction,
        index: usize,
        mut_place: &RirMutPlaceArg,
    ) -> (Vec<String>, String) {
        if let RirMutPlaceAccess::Handle(RirMutPlaceHandle::Global { global, ty }) =
            mut_place.access
        {
            let init = format!("__anv_global_place_init_{index}");
            let root = format!("__anv_global_place_root_{index}");
            let prelude = vec![
                format!(
                    "let {init}: &dyn for<'__anv_rt> Fn(&mut {}) -> Result<{}, {}> = &|rt| {};",
                    target::runtime_ctx_ty_with("'__anv_rt"),
                    self.ty(ty),
                    target::runtime_error_ty(),
                    RustValues::global_init_call(self.program, global)
                ),
                format!(
                    "let mut {root} = {};",
                    target::mut_place_global_with_init(
                        &RustValues::global_slot_expr(self.program, global),
                        &init,
                    )
                ),
            ];
            if mut_place.projections.is_empty() {
                return (prelude, root);
            }
            let (projected_prelude, place) =
                self.prepared_projected_mut_place(function, index, mut_place, ty, &root);
            return (
                prelude.into_iter().chain(projected_prelude).collect(),
                place,
            );
        }
        let RirMutPlaceAccess::Handle(RirMutPlaceHandle::Local { local, ty }) = mut_place.access
        else {
            return self.prepared_mut_place_arg(function, index, mut_place);
        };
        if function.locals[local.index()].payload_ref {
            return self.prepared_mut_place_arg(function, index, mut_place);
        }
        let root = target::mut_place_local_raw(function.locals[local.index()].symbol.as_str());
        if mut_place.projections.is_empty() {
            return (vec![], root);
        }
        self.prepared_projected_mut_place(function, index, mut_place, ty, &root)
    }

    fn prepared_scoped_lambda_call_arg(
        &self,
        function: &RirFunction,
        index: usize,
        callee: &RirOperand,
        sig: RirLambdaSigId,
    ) -> (Vec<String>, String) {
        let values = RustValues::new(self.program, function);
        let policy = RirRustRepPolicy::new(self.program);
        let (args_ty, ret_ty) = policy.scoped_lambda_sig_args_ret(sig);
        let state = format!("__anv_scoped_lambda_state_{index}");
        let lambda = values.value_operand(callee);
        let rt_ptr = target::non_null_from_mut("&mut *rt");
        let statics_ptr = target::non_null_from_mut("statics");
        let globals_ptr = target::non_null_from_mut("globals");
        let retained_callbacks = self.has_retained_callbacks();
        let retained_ptrs = retained_callbacks.then(|| {
            format!(
                ", {}.cast::<()>(), callbacks.cast::<()>()",
                target::non_null_from_ref("owner")
            )
        });
        let ctor = target::scoped_lambda_ctor(&args_ty, &ret_ty);
        let thunk = format!(
            "{}::{}",
            policy.lambda_sig_assoc_path(sig),
            target::scoped_lambda_thunk()
        );
        (
            vec![format!(
                "let mut {state} = ({lambda}, {rt_ptr}.cast::<()>(), {statics_ptr}.cast::<()>(), {globals_ptr}.cast::<()>(){});",
                retained_ptrs.as_deref().unwrap_or("")
            )],
            target::scoped_lambda_from_raw(&ctor, &state, &thunk),
        )
    }

    fn prepared_callback_record_handle(
        &self,
        function: &RirFunction,
        index: usize,
        callee: &RirOperand,
        plan: RetainedCallbackSigPlan,
    ) -> (Vec<String>, String) {
        let lambda = RustValues::new(self.program, function).value_operand(callee);
        let handle = format!("__anv_callback_handle_{index}");
        let record_var = format!("__anv_callback_record_{index}");
        (
            vec![
                format!(
                    "let {record_var} = {} {{ lambda: {lambda}, _brand: std::marker::PhantomData }};",
                    plan.record_symbol()
                ),
                format!(
                    "let {handle} = {};",
                    target::rt_heap_alloc(
                        "rt",
                        &format!("statics.{}", plan.heap_type_field()),
                        &record_var
                    )
                ),
            ],
            handle,
        )
    }

    fn prepared_escaping_lambda_call_arg(
        &self,
        function: &RirFunction,
        index: usize,
        callee: &RirOperand,
        sig: RirLambdaSigId,
    ) -> (Vec<String>, String) {
        let plan = RetainedCallbackSigPlan::new(sig);
        let args_ty = plan.args_ty(self.program);
        let ret_ty = plan.ret_ty(self.program);
        let field = plan.table_field();
        let callback_ctor = target::escaping_lambda_ctor_ty(&args_ty, &ret_ty);
        let key = format!("__anv_callback_key_{index}");
        let (mut prelude, handle) =
            self.prepared_callback_record_handle(function, index, callee, plan);
        let index_var = format!("__anv_callback_index_{index}");
        let generation = format!("__anv_callback_generation_{index}");
        let arg = format!("__anv_callback_arg_{index}");
        let table_id = plan.table_id();
        let signature_id = plan.signature_id();
        prelude.extend([
            format!(
                "let ({index_var}, {generation}) = unsafe {{ &mut *callbacks.as_ptr() }}.insert_{field}({handle});"
            ),
            format!(
                "let {key} = {};",
                target::callback_key_new(
                    "owner.owner_id()",
                    "owner.shutdown_generation()",
                    table_id,
                    signature_id,
                    &index_var,
                    &generation
                )
            ),
            format!(
                "let {arg} = {};",
                target::escaping_lambda_new(
                    &callback_ctor,
                    "owner",
                    &key,
                    &plan.call_thunk_symbol(),
                    &plan.close_thunk_symbol()
                )
            ),
        ]);
        (prelude, arg)
    }

    fn prepared_anv_callback_call_arg(
        &self,
        function: &RirFunction,
        index: usize,
        callee: &RirOperand,
        sig: RirLambdaSigId,
    ) -> (Vec<String>, String) {
        let plan = RetainedCallbackSigPlan::new(sig);
        let args_ty = plan.args_ty(self.program);
        let ret_ty = plan.ret_ty(self.program);
        let callback_ctor = target::anv_callback_ctor_ty(&args_ty, &ret_ty);
        let (mut prelude, handle) =
            self.prepared_callback_record_handle(function, index, callee, plan);
        let erased = format!("__anv_callback_erased_{index}");
        let arg = format!("__anv_callback_arg_{index}");
        prelude.extend([
            format!(
                "let {erased} = {};",
                target::rt_heap_ref_erase("rt", &format!("&{handle}"))
            ),
            format!(
                "let {arg} = {};",
                target::anv_callback_new(
                    &callback_ctor,
                    "owner",
                    &erased,
                    &plan.anv_call_thunk_symbol()
                )
            ),
        ]);
        (prelude, arg)
    }

    fn prepared_projected_mut_place(
        &self,
        function: &RirFunction,
        index: usize,
        mut_place: &RirMutPlaceArg,
        root_ty_id: RirTypeId,
        root: &str,
    ) -> (Vec<String>, String) {
        let ops = format!("__AnvProjectedPlaceOps{index}");
        let ops_tmp = format!("__anv_projected_place_ops_{index}");
        let values = RustValues::new(self.program, function);
        let descriptor =
            if let RirMutPlaceAccess::Handle(RirMutPlaceHandle::Local { local, .. }) =
                mut_place.access
                && !RustPlaces::new(self.program, function).payload_ref_cell_local(local)
            {
                let raw_root = function.locals[local.index()].symbol.as_str();
                let projection = RustPlaces::new(self.program, function)
                    .projected_place(root_ty_id, raw_root, mut_place.ty, &mut_place.projections)
                    .expect("verified projected place descriptor");
                values.mut_place_projection_descriptor(&ops, &projection)
            } else {
                values.mut_place_projection_descriptor_for(
                    &ops,
                    root_ty_id,
                    root,
                    mut_place.ty,
                    &mut_place.projections,
                )
            };
        (
            vec![
                descriptor.struct_decl,
                descriptor.impl_decl,
                format!("let {ops_tmp} = {};", descriptor.ctor),
            ],
            target::mut_place_projected(root, &format!("&{ops_tmp}")),
        )
    }

    fn lambda_capture_arg(
        &self,
        function: &RirFunction,
        decl: &RirLambdaCapture,
        capture: &RirLambdaCaptureArg,
    ) -> String {
        let values = RustValues::new(self.program, function);
        match (decl.kind, decl.semantic, capture) {
            (
                RirLambdaCaptureKind::Param,
                RirParamSemantic::Value,
                RirLambdaCaptureArg::Readonly { value },
            ) => values.value_operand(value),
            (
                RirLambdaCaptureKind::Param,
                RirParamSemantic::SharedBorrow,
                RirLambdaCaptureArg::Readonly { value },
            ) => {
                let RirOperand::Place(place) = value else {
                    unreachable!("verified shared readonly capture place")
                };
                values.borrow_arg(place)
            }
            (
                RirLambdaCaptureKind::Param,
                RirParamSemantic::MutBorrow,
                RirLambdaCaptureArg::Scoped { place },
            ) => values.mut_borrow_arg(place),
            (
                RirLambdaCaptureKind::StackCell { .. },
                RirParamSemantic::StackCell,
                RirLambdaCaptureArg::StackCell { cell },
            ) => match cell {
                RirCellRef::Owner(_) => format!("&{}", self.cell_ref(function, *cell)),
                RirCellRef::Capture { .. } => self.cell_ref(function, *cell),
            },
            (
                RirLambdaCaptureKind::HeapCell { .. },
                RirParamSemantic::HeapCell,
                RirLambdaCaptureArg::HeapCell { cell },
            ) => format!("{}.clone()", self.cell_ref(function, *cell)),
            (
                RirLambdaCaptureKind::ScopedPlaceCell { .. },
                RirParamSemantic::ScopedPlaceCell,
                RirLambdaCaptureArg::ScopedPlaceCell { cell },
            ) => self.scoped_place_cell_capture_arg(function, *cell),
            _ => unreachable!("verified lambda capture arg mode"),
        }
    }

    fn cast(&self, function: &RirFunction, value: &RirOperand, target_ty: RirTypeId) -> String {
        let values = RustValues::new(self.program, function);
        let value_ty = values.operand_ty(value);
        let value = values.operand(value);
        match (
            &self.program.types[value_ty.index()],
            &self.program.types[target_ty.index()],
        ) {
            (RirType::Int, RirType::Float) => target::int_to_float(&value),
            (RirType::Float, RirType::Int) => format!("{}?", target::float_to_int(&value)),
            _ => unreachable!("verified numeric cast"),
        }
    }

    fn raw_project(
        &self,
        function: &RirFunction,
        value: &RirOperand,
        target_ty: RirTypeId,
    ) -> String {
        let values = RustValues::new(self.program, function);
        let value_ty = values.operand_ty(value);
        let value = values.operand(value);
        let enum_id = match self.program.types[value_ty.index()] {
            RirType::Flag(_) => return target::flag_bits(&value),
            RirType::Enum(enum_id) => enum_id,
            _ => unreachable!("verified raw projection source"),
        };
        let enm = &self.program.enums[enum_id.index()];
        match enm.repr {
            RirEnumRepr::RawInt => {
                if enm.variants.is_empty() {
                    Self::raw_enum_project_match(&value, String::new())
                } else {
                    target::raw_int_project(&value, &self.ty(target_ty))
                }
            }
            RirEnumRepr::RawString => {
                let arms = enm
                    .variants
                    .iter()
                    .map(|variant| {
                        let Some(RirRawEnumValue::String(id)) = variant.raw_value else {
                            unreachable!("verified raw string enum value")
                        };
                        format!(
                            "{} => {}",
                            variant_path(enm.symbol.as_str(), variant.symbol.as_str()),
                            target::string_literal_share(target::statics_param_name(), id)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                Self::raw_enum_project_match(&value, arms)
            }
            RirEnumRepr::Adt => unreachable!("verified raw projection enum"),
        }
    }

    fn raw_try_construct(
        &self,
        function: &RirFunction,
        value: &RirOperand,
        target_ty: RirTypeId,
    ) -> String {
        let values = RustValues::new(self.program, function);
        let enum_id = match self.program.types[target_ty.index()] {
            RirType::Flag(_) => {
                return target::flag_try_construct(
                    self.flag_symbol(target_ty),
                    &values.operand(value),
                );
            }
            RirType::Enum(enum_id) => enum_id,
            _ => unreachable!("verified raw construction target"),
        };
        let enm = &self.program.enums[enum_id.index()];
        if enm.variants.is_empty() {
            return target::option_none().to_string();
        }
        let (scrutinee, arms) = match enm.repr {
            RirEnumRepr::RawInt => {
                let arms = enm
                    .variants
                    .iter()
                    .map(|variant| {
                        let Some(RirRawEnumValue::Int(raw)) = variant.raw_value else {
                            unreachable!("verified raw int enum value")
                        };
                        let variant = variant_path(enm.symbol.as_str(), variant.symbol.as_str());
                        format!("{raw} => {}", target::option_some(&variant))
                    })
                    .collect::<Vec<_>>();
                (values.operand(value), arms)
            }
            RirEnumRepr::RawString => {
                let arms = enm
                    .variants
                    .iter()
                    .map(|variant| {
                        let Some(RirRawEnumValue::String(id)) = variant.raw_value else {
                            unreachable!("verified raw string enum value")
                        };
                        let raw = rust_string(&self.program.string_literal(id).text);
                        let variant = variant_path(enm.symbol.as_str(), variant.symbol.as_str());
                        format!("{raw} => {}", target::option_some(&variant))
                    })
                    .collect::<Vec<_>>();
                (values.string_arg(value), arms)
            }
            RirEnumRepr::Adt => unreachable!("verified raw construction target"),
        };
        match_expr(
            &scrutinee,
            arms.into_iter()
                .chain([format!("_ => {}", target::option_none())]),
        )
    }

    fn raw_enum_project_match(value: &str, arms: String) -> String {
        let scrutinee = format!("&{value}");
        if arms.is_empty() {
            match_expr(&scrutinee, ["_ => unreachable!()".to_string()])
        } else {
            match_expr(&scrutinee, [arms])
        }
    }

    fn slice_view(
        &self,
        function: &RirFunction,
        source: &RirPlace,
        start: RirLocalId,
        end: RirLocalId,
        inclusive: bool,
        mutable: bool,
    ) -> String {
        let places = RustPlaces::new(self.program, function);
        let source_expr = places.local_place(source);
        let start = function.locals[start.index()].symbol.as_str();
        let end = function.locals[end.index()].symbol.as_str();
        if places.mut_place_root_param(source)
            && matches!(
                self.program.types[source.ty.index()],
                RirType::Array { .. } | RirType::List(_)
            )
        {
            let raw = matches!(self.program.types[source.ty.index()], RirType::Array { .. });
            return target::mut_place_slice_view(
                &source_expr,
                target::runtime_param_name(),
                start,
                end,
                inclusive,
                mutable,
                raw,
            );
        }
        let range = target::checked_range(start, end, inclusive, &format!("{source_expr}.len()"));
        let lines = vec![format!("let __anv_range = {range};")];
        let view = match self.program.types[source.ty.index()] {
            RirType::Array { .. } if mutable => target::anv_slice_from_raw_parts_mut(
                &format!("{source_expr}.as_mut_ptr()"),
                &format!("{source_expr}.len()"),
                "__anv_range.start",
                "__anv_range.len()",
            ),
            RirType::Array { .. } => target::anv_slice_from_raw_parts(
                &format!("{source_expr}.as_ptr()"),
                &format!("{source_expr}.len()"),
                "__anv_range.start",
                "__anv_range.len()",
            ),
            RirType::List(_) => {
                if mutable {
                    target::anv_slice_from_list_mut(
                        target::runtime_param_name(),
                        &format!("&mut {source_expr}"),
                        "__anv_range.start",
                        "__anv_range.len()",
                    )
                } else {
                    target::anv_slice_from_list(
                        &format!("&{source_expr}"),
                        "__anv_range.start",
                        "__anv_range.len()",
                    )
                }
            }
            RirType::Slice(_) => {
                target::anv_slice_slice(&source_expr, "__anv_range.start", "__anv_range.len()")
            }
            _ => unreachable!("verified slice view source"),
        };
        block_expr(lines, Some(view))
    }

    fn range_list_copy(
        &self,
        function: &RirFunction,
        source: &RirPlace,
        start: RirLocalId,
        end: RirLocalId,
        inclusive: bool,
        ty: RirTypeId,
    ) -> String {
        let source_expr = RustPlaces::new(self.program, function).local_place(source);
        let start = function.locals[start.index()].symbol.as_str();
        let end = function.locals[end.index()].symbol.as_str();
        let range = target::checked_range(start, end, inclusive, &format!("{source_expr}.len()"));
        let (elem, source_kind) = match self.program.types[source.ty.index()] {
            RirType::List(elem) => (elem, RangeCopySource::List),
            RirType::Array { elem, .. } => (elem, RangeCopySource::Array),
            RirType::Slice(elem) => (elem, RangeCopySource::Slice),
            _ => unreachable!("verified range list copy source"),
        };
        let values = RustValues::new(self.program, function);
        let item = "item";
        let body = values.value_from_ref(elem, item);
        let storage_ty = self.collection_storage_heap_type(ty);
        let lines = vec![format!("let __anv_range = {range};")];
        let copy = match source_kind {
            RangeCopySource::Slice => format!(
                "{}?",
                target::anv_slice_copy_range_with(
                    &source_expr,
                    target::runtime_param_name(),
                    &storage_ty,
                    "__anv_range",
                    item,
                    &body,
                )
            ),
            RangeCopySource::List => target::anv_list_from_iter(
                target::runtime_param_name(),
                &storage_ty,
                &format!(
                    "({source_expr}.to_vec({})?)[__anv_range].iter().map(|{item}| {body})",
                    target::runtime_param_name()
                ),
            ),
            RangeCopySource::Array => target::anv_list_from_iter(
                target::runtime_param_name(),
                &storage_ty,
                &format!("{source_expr}[__anv_range].iter().map(|{item}| {body})"),
            ),
        };
        block_expr(lines, Some(copy))
    }

    fn string_concat(&self, function: &RirFunction, parts: &[RirOperand]) -> String {
        let values = RustValues::new(self.program, function);
        let mut rendered = vec![format!("let mut out = {};", target::anv_string_builder())];
        rendered.extend(
            parts
                .iter()
                .map(|part| format!("out.push_str({});", values.string_arg(part))),
        );
        block_expr(rendered, Some("out".to_string()))
    }

    fn native_return_call(
        &self,
        _function: &RirFunction,
        ret: RirTypeId,
        abi: &anvyx_runtime::RustReturnAbi,
        call: String,
    ) -> String {
        match abi {
            anvyx_runtime::RustReturnAbi::Void => call,
            _ => self.native_return_expr(ret, abi, &call),
        }
    }

    fn native_ref_adopt_return(&self, ret: RirTypeId, expr: &str) -> String {
        let RirType::Struct(id) = self.program.types[ret.index()] else {
            unreachable!("verified native resource return type")
        };
        let native = self.program.structs[id.index()]
            .native_path
            .as_ref()
            .expect("verified native resource path")
            .join("::");
        target::native_ref_adopt(&native, expr)
    }

    fn native_owned_named_return_expr(
        &self,
        ret: RirTypeId,
        abi: &anvyx_runtime::ExternTypeExpr,
        expr: &str,
    ) -> String {
        if native_ty_is_resource_ref(self.program, ret) {
            self.native_ref_adopt_return(ret, expr)
        } else {
            self.native_value_return_expr(ret, abi, expr)
        }
    }

    fn native_return_expr(
        &self,
        ret: RirTypeId,
        abi: &anvyx_runtime::RustReturnAbi,
        expr: &str,
    ) -> String {
        match abi {
            anvyx_runtime::RustReturnAbi::Value(ty) => self.native_value_return_expr(ret, ty, expr),
            anvyx_runtime::RustReturnAbi::OwnedNamed(ty) => {
                self.native_owned_named_return_expr(ret, ty, expr)
            }
            anvyx_runtime::RustReturnAbi::Option(inner) => {
                let RirType::Option(inner_ty) = self.program.types[ret.index()] else {
                    unreachable!("verified native option return type")
                };
                target::rust_option_map(expr, &self.native_return_expr(inner_ty, inner, "value"))
            }
            anvyx_runtime::RustReturnAbi::Result(ok, err) => {
                let (enm, ok_variant, err_variant) = self.result_enum(ret);
                let ok_path = variant_path(enm.symbol.as_str(), ok_variant.symbol.as_str());
                let err_path = variant_path(enm.symbol.as_str(), err_variant.symbol.as_str());
                target::rust_result_match(
                    expr,
                    &format!(
                        "Ok(value) => {ok_path}({})",
                        self.native_return_expr(ok_variant.fields[0].ty, ok, "value")
                    ),
                    &format!(
                        "Err(value) => {err_path}({})",
                        self.native_return_expr(err_variant.fields[0].ty, err, "value")
                    ),
                )
            }
            anvyx_runtime::RustReturnAbi::Void => {
                unreachable!("verified native result payload ABI")
            }
        }
    }

    fn native_value_return_expr(
        &self,
        ret: RirTypeId,
        abi: &anvyx_runtime::ExternTypeExpr,
        expr: &str,
    ) -> String {
        match abi {
            anvyx_runtime::ExternTypeExpr::Unit => self.native_empty_tuple_return_expr(ret, expr),
            anvyx_runtime::ExternTypeExpr::Option(inner) => {
                let RirType::Option(inner_ty) = self.program.types[ret.index()] else {
                    unreachable!("verified native option return type")
                };
                target::rust_option_map(
                    expr,
                    &self.native_value_return_expr(inner_ty, inner, "value"),
                )
            }
            anvyx_runtime::ExternTypeExpr::Result(ok, err) => {
                self.native_value_result_return_expr(ret, ok, err, expr)
            }
            anvyx_runtime::ExternTypeExpr::Tuple(fields) => {
                self.native_tuple_return_expr(ret, fields, expr)
            }
            anvyx_runtime::ExternTypeExpr::Array { elem, .. } => {
                let RirType::Array { elem: elem_ty, .. } = self.program.types[ret.index()] else {
                    unreachable!("verified native array return type")
                };
                Self::native_array_map_expr(
                    expr,
                    &self.native_value_return_expr(elem_ty, elem, "value"),
                )
            }
            _ => expr.to_string(),
        }
    }

    fn native_tuple_return_expr(
        &self,
        ret: RirTypeId,
        fields: &[anvyx_runtime::ExternTypeExpr],
        expr: &str,
    ) -> String {
        if fields.is_empty() {
            return self.native_empty_tuple_return_expr(ret, expr);
        }
        let RirType::Tuple(tuple_id) = self.program.types[ret.index()] else {
            unreachable!("verified native tuple return type")
        };
        let tuple = &self.program.tuples[tuple_id.index()];
        let tmp = "__anv_native_ret";
        let fields = tuple
            .fields
            .iter()
            .zip(fields)
            .enumerate()
            .map(|(index, (field, abi))| {
                field_init(
                    field.symbol.as_str(),
                    self.native_value_return_expr(
                        field.ty,
                        abi,
                        &target::rust_tuple_field(tmp, index),
                    ),
                )
            });
        format!(
            "{{ let {tmp} = {expr}; {} }}",
            struct_lit(tuple.symbol.as_str(), fields)
        )
    }

    fn native_empty_tuple_return_expr(&self, ret: RirTypeId, expr: &str) -> String {
        let RirType::Tuple(tuple_id) = self.program.types[ret.index()] else {
            unreachable!("verified native unit return type")
        };
        let tuple = &self.program.tuples[tuple_id.index()];
        target::rust_eval_then(
            expr,
            &struct_lit(tuple.symbol.as_str(), Vec::<String>::new()),
        )
    }

    fn native_value_result_return_expr(
        &self,
        ret: RirTypeId,
        ok: &anvyx_runtime::ExternTypeExpr,
        err: &anvyx_runtime::ExternTypeExpr,
        expr: &str,
    ) -> String {
        let (enm, ok_variant, err_variant) = self.result_enum(ret);
        let ok_path = variant_path(enm.symbol.as_str(), ok_variant.symbol.as_str());
        let err_path = variant_path(enm.symbol.as_str(), err_variant.symbol.as_str());
        target::rust_result_match(
            expr,
            &format!(
                "Ok(value) => {ok_path}({})",
                self.native_value_return_expr(ok_variant.fields[0].ty, ok, "value")
            ),
            &format!(
                "Err(value) => {err_path}({})",
                self.native_value_return_expr(err_variant.fields[0].ty, err, "value")
            ),
        )
    }

    fn result_enum(&self, ty: RirTypeId) -> (&RirEnum, &RirVariant, &RirVariant) {
        let RirType::Enum(enum_id) = self.program.types[ty.index()] else {
            unreachable!("verified native result type")
        };
        let enm = &self.program.enums[enum_id.index()];
        let [ok, err] = enm.variants.as_slice() else {
            unreachable!("verified native result variants")
        };
        (enm, ok, err)
    }

    fn extern_call(
        &self,
        function: &RirFunction,
        id: super::rir::RirExternId,
        args: &[RirCallArg],
        receiver: Option<ResolvedReceiver>,
    ) -> String {
        let ext = &self.program.externs[id.index()];
        let RirExternKind::Native(native) = &ext.kind;
        let rendered = match native.abi.ctx {
            anvyx_runtime::RustWrapperCtx::HiddenRuntime => target::native_call_args([]),
            anvyx_runtime::RustWrapperCtx::None => vec![],
        };
        let symbol = native.path.join("::");
        let native_plan = self.program.native_call_plan(id);
        let suspend_entry = native_plan.provider_entry().suspends_runtime_entry();
        let param_tys = ext.params.iter().map(|param| param.ty).collect::<Vec<_>>();
        let call = self.prepared_native_call_expr(
            function,
            args,
            &native_plan,
            native.abi.params.as_slice(),
            &param_tys,
            receiver,
            rendered,
            |rendered| {
                let call = format!("{symbol}({rendered})");
                let call = if suspend_entry {
                    RuntimeOwnerEmit::provider_suspended_call(
                        "owner",
                        "__anv_provider_entry",
                        "__anv_provider_result",
                        &call,
                    )
                } else {
                    call
                };
                if native.abi.fallible {
                    format!("{call}?")
                } else {
                    call
                }
            },
        );
        self.native_return_call(function, ext.ret, &native.abi.ret, call)
    }

    fn dataref_alloc(
        &self,
        function: &RirFunction,
        ty: RirTypeId,
        fields: &[RirOperand],
    ) -> String {
        let RirType::DataRef(id) = self.program.types[ty.index()] else {
            unreachable!("verified dataref allocation")
        };
        let dataref = &self.program.datarefs[id.index()];
        let values = RustValues::new(self.program, function);
        let fields = comma(dataref.fields.iter().zip(fields).map(|(field, value)| {
            format!("{}: {}", field.symbol.as_str(), values.value_operand(value))
        }));
        let heap_type = format!("statics.{}", dataref.heap_type_symbol());
        let storage = format!("{} {{ {} }}", dataref.storage_symbol(), fields);
        format!(
            "{{ let heap_type = {heap_type}; {} }}",
            target::rt_heap_alloc("rt", "heap_type", &storage)
        )
    }

    fn dataref_get(
        &self,
        function: &RirFunction,
        object: &RirOperand,
        dataref: RirDataRefId,
        projections: &[RirProjection],
        ty: RirTypeId,
    ) -> String {
        let values = RustValues::new(self.program, function);
        let object = values.operand_ref(object);
        let path = RustPlaces::new(self.program, function).storage_path(dataref, projections);
        target::rt_heap_with(
            "rt",
            &object,
            "storage",
            &values.value_from_place(ty, &path),
        )
    }

    fn dataref_set(
        &self,
        function: &RirFunction,
        object: &RirOperand,
        dataref: RirDataRefId,
        projections: &[RirProjection],
        value: &RirOperand,
    ) -> String {
        let values = RustValues::new(self.program, function);
        let object = values.operand_ref(object);
        let path = RustPlaces::new(self.program, function).storage_path(dataref, projections);
        let value_ty = values.operand_ty(value);
        let value = values.value_operand(value);
        if self.program.collection_replace_ty(value_ty) {
            target::rt_heap_with_mut(
                "rt",
                &object,
                "storage",
                &target::replace_collection_result(&path, &value),
            ) + "?"
        } else {
            target::rt_heap_with_mut("rt", &object, "storage", &format!("{path} = {value};"))
        }
    }

    fn struct_literal(
        &self,
        function: &RirFunction,
        ty: RirTypeId,
        operands: &[RirOperand],
    ) -> String {
        let RirType::Struct(struct_id) = self.program.types[ty.index()] else {
            unreachable!("verified struct literal type")
        };
        let strukt = &self.program.structs[struct_id.index()];
        self.record_literal(function, strukt.symbol.as_str(), &strukt.fields, operands)
    }

    fn tuple_literal(
        &self,
        function: &RirFunction,
        ty: RirTypeId,
        operands: &[RirOperand],
    ) -> String {
        let RirType::Tuple(tuple_id) = self.program.types[ty.index()] else {
            unreachable!("verified tuple literal type")
        };
        let tuple = &self.program.tuples[tuple_id.index()];
        self.record_literal(function, tuple.symbol.as_str(), &tuple.fields, operands)
    }

    fn record_literal(
        &self,
        function: &RirFunction,
        symbol: &str,
        fields: &[super::rir::RirField],
        operands: &[RirOperand],
    ) -> String {
        let values = RustValues::new(self.program, function);
        let fields = fields.iter().zip(operands).map(|(field, operand)| {
            field_init(field.symbol.as_str(), values.value_operand(operand))
        });
        struct_lit(symbol, fields)
    }

    fn flag_symbol(&self, ty: RirTypeId) -> &str {
        let RirType::Flag(flag) = self.program.types[ty.index()] else {
            unreachable!("verified flag operation has flag result")
        };
        self.program.flags[flag.index()].symbol.as_str()
    }

    fn enum_literal(
        &self,
        function: &RirFunction,
        ty: RirTypeId,
        variant_id: RirVariantId,
        fields: &[RirOperand],
    ) -> String {
        let RirType::Enum(enum_id) = self.program.types[ty.index()] else {
            unreachable!("verified enum literal type")
        };
        let enm = &self.program.enums[enum_id.index()];
        let variant = &enm.variants[variant_id.index()];
        let path = variant_path(enm.symbol.as_str(), variant.symbol.as_str());
        let values = RustValues::new(self.program, function);
        match variant.kind {
            RirVariantKind::Unit => path,
            RirVariantKind::Tuple => tuple_variant(
                &path,
                fields.iter().map(|field| values.value_operand(field)),
            ),
            RirVariantKind::Struct => {
                let fields = variant.fields.iter().zip(fields).map(|(field, operand)| {
                    field_init(field.symbol.as_str(), values.value_operand(operand))
                });
                struct_variant(&path, fields)
            }
        }
    }

    fn stringify_planned(
        &self,
        function: &RirFunction,
        value: &RirOperand,
        ty: RirTypeId,
    ) -> String {
        let req = self
            .program
            .stringify_reqs
            .iter()
            .find(|req| req.ty == ty)
            .expect("verified stringify req missing");
        match req.kind {
            super::rir::RirStringifyReqKind::Helper(helper) => {
                let helper = &self.program.stringify_helpers[helper.index()];
                let ctx_use = analysis::function_context_use(self.program, function);
                let call = format!(
                    "{}({}, {}, {}, &{})",
                    helper.symbol.as_str(),
                    target::runtime_param(ctx_use.rt),
                    target::statics_param(ctx_use.statics),
                    target::globals_param(ctx_use.globals),
                    RustValues::new(self.program, function).operand(value)
                );
                if analysis::stringify_helper_fallible(
                    self.program,
                    &self.fallible_functions,
                    helper,
                ) {
                    format!("{call}?")
                } else {
                    call
                }
            }
            super::rir::RirStringifyReqKind::Override {
                function: target,
                mode,
            } => {
                let symbol = self.program.functions[target.index()].symbol.as_str();
                let arg = RustValues::new(self.program, function).stringify_arg(mode, value);
                let call = if self.has_retained_callbacks() {
                    target::retained_generated_call(symbol, [arg])
                } else {
                    target::generated_call(symbol, [arg])
                };
                if self.fallible_functions[target.index()] {
                    format!("{call}?")
                } else {
                    call
                }
            }
        }
    }

    fn ty(&self, ty: RirTypeId) -> String {
        RirRustRepPolicy::new(self.program).rust_ty(ty)
    }

    fn indented(&mut self, f: impl FnOnce(&mut Self)) {
        self.w.push_indent();
        f(self);
        self.w.pop_indent();
    }
}

fn stmt_call_args(stmt: &RirStmt) -> Option<&[RirCallArg]> {
    match stmt {
        RirStmt::Init {
            value: RirRValue::Call { args, .. },
            ..
        }
        | RirStmt::Assign {
            value: RirRValue::Call { args, .. },
            ..
        }
        | RirStmt::Eval(RirRValue::Call { args, .. }) => Some(args),
        _ => None,
    }
}

fn escaping_map_entry_payloads(
    stmts: &[RirStmt],
) -> impl Iterator<Item = (usize, RirLocalId)> + '_ {
    stmts.iter().enumerate().filter_map(|(index, stmt)| {
        let RirStmt::MapEntryMatch(match_) = stmt else {
            return None;
        };
        match_.payload_escapes.then_some((index, match_.payload?))
    })
}

fn stmt_uses_local(program: &RirProgram, stmt: &RirStmt, local: RirLocalId) -> bool {
    stmt_directly_uses_local(program, stmt, local)
        || stmt_child_blocks_any(stmt, |block| block_uses_local(program, block, local))
}

fn stmt_directly_uses_local(program: &RirProgram, stmt: &RirStmt, local: RirLocalId) -> bool {
    match stmt {
        RirStmt::Init { value, .. }
        | RirStmt::GlobalSetRoot { value, .. }
        | RirStmt::GlobalUpdateRoot { value, .. }
        | RirStmt::Eval(value) => rvalue_uses_local(program, value, local),
        RirStmt::Assign { dst, value } => {
            place_uses_local(dst, local) || rvalue_uses_local(program, value, local)
        }
        RirStmt::CellInit { cell, value } | RirStmt::CellSet { cell, value } => {
            cell_uses_local(cell, local) || rvalue_uses_local(program, value, local)
        }
        RirStmt::ScopedPlaceCellInit { cell } => {
            program.scoped_place_cells[cell.index()]
                .source
                .place()
                .root_local()
                == Some(local)
        }
        RirStmt::ScopedPlaceCellSet { cell, value } => {
            scoped_cell_uses_local(program, cell, local) || rvalue_uses_local(program, value, local)
        }
        RirStmt::MutPlaceSet { place, value } => {
            mut_place_arg_uses_local(program, place, local)
                || rvalue_uses_local(program, value, local)
        }
        RirStmt::DataRefSet {
            object,
            projections,
            value,
            ..
        } => {
            operand_uses_local(program, object, local)
                || projections_use_local(projections, local)
                || operand_uses_local(program, value, local)
        }
        RirStmt::SequenceSlotSet {
            collection,
            index,
            value,
        } => {
            collection.uses_local(local)
                || *index == local
                || operand_uses_local(program, value, local)
        }
        RirStmt::MapValueSet { map, index, value } => {
            map.uses_local(local) || *index == local || operand_uses_local(program, value, local)
        }
        RirStmt::If(RirIf { cond, .. }) => operand_uses_local(program, cond, local),
        RirStmt::RangeFor(RirRangeFor {
            start,
            end,
            ordinal_plan,
            ..
        }) => {
            operand_uses_local(program, start, local)
                || operand_uses_local(program, end, local)
                || ordinal_uses_local(program, ordinal_plan, local)
        }
        RirStmt::CollectionFor(for_) => {
            for_.len == local || ordinal_uses_local(program, &for_.ordinal_plan, local)
        }
        RirStmt::CollectionLoanScope(RirCollectionLoanScope { root, .. }) => root.uses_local(local),
        RirStmt::PatternMatch(RirPatternMatch { subject, .. }) => place_uses_local(subject, local),
        RirStmt::DynMatch(match_) => match &match_.source {
            super::rir::RirDynMatchSource::Owned { value, .. } => {
                operand_uses_local(program, value, local)
            }
            super::rir::RirDynMatchSource::MutPlace(place) => {
                mut_place_arg_uses_local(program, place, local)
            }
            super::rir::RirDynMatchSource::Borrowed(borrow) => match &borrow.source {
                super::rir::RirDynBorrowSource::Concrete { place, .. }
                | super::rir::RirDynBorrowSource::Owned { place, .. } => {
                    mut_place_arg_uses_local(program, place, local)
                }
                super::rir::RirDynBorrowSource::Borrowed { local: source, .. }
                | super::rir::RirDynBorrowSource::Reborrowed { local: source, .. } => {
                    *source == local
                }
            },
        },
        RirStmt::OptionMatch(RirOptionMatch { subject, .. }) => {
            option_subject_uses_local(program, subject, local)
        }
        RirStmt::MapEntryMatch(RirMapEntryMatch { map, key, .. }) => {
            mut_place_arg_uses_local(program, map, local) || operand_uses_local(program, key, local)
        }
        RirStmt::GlobalEnsure { .. } | RirStmt::Loop(_) | RirStmt::CollectionSlotScope(_) => false,
    }
}

fn block_uses_local(program: &RirProgram, block: &RirStructuredBlock, local: RirLocalId) -> bool {
    block
        .stmts
        .iter()
        .any(|stmt| stmt_uses_local(program, stmt, local))
        || term_uses_local(program, &block.term, local)
}

fn term_uses_local(program: &RirProgram, term: &RirTerm, local: RirLocalId) -> bool {
    match term {
        RirTerm::Return(Some(operand)) => operand_uses_local(program, operand, local),
        RirTerm::None
        | RirTerm::Return(None)
        | RirTerm::Break(_)
        | RirTerm::Continue(_)
        | RirTerm::Unreachable => false,
    }
}

fn rvalue_uses_local(program: &RirProgram, value: &RirRValue, local: RirLocalId) -> bool {
    let mut uses = match value {
        RirRValue::CellGetCopy { cell, .. } => cell_uses_local(cell, local),
        RirRValue::ScopedPlaceCellGet { cell, .. } => scoped_cell_uses_local(program, cell, local),
        _ => false,
    };
    value.for_each_child(crate::rust::rir::RirValueUse::Read, &mut |child| {
        uses |= match child {
            RirChild::Operand { operand, .. } => operand_uses_local(program, operand, local),
            RirChild::Place { place, .. } => place_uses_local(place, local),
            RirChild::MutPlace { place, .. } => mut_place_arg_uses_local(program, place, local),
            RirChild::Collection { collection, .. } => collection.uses_local(local),
            RirChild::CallArg(arg) => call_arg_uses_local(program, arg, local),
            RirChild::CaptureArg(capture) => match capture {
                RirLambdaCaptureArg::Readonly { value } => {
                    operand_uses_local(program, value, local)
                }
                RirLambdaCaptureArg::Scoped { place } => place_uses_local(place, local),
                RirLambdaCaptureArg::StackCell { cell }
                | RirLambdaCaptureArg::HeapCell { cell } => cell_uses_local(cell, local),
                RirLambdaCaptureArg::ScopedPlaceCell { cell } => {
                    scoped_cell_uses_local(program, cell, local)
                }
            },
            RirChild::LocalRead(read) => read == local,
            RirChild::Block(_) | RirChild::Tail(_) => false,
        };
    });
    uses
}

fn call_arg_uses_local(program: &RirProgram, arg: &RirCallArg, local: RirLocalId) -> bool {
    match arg {
        RirCallArg::Value(operand)
        | RirCallArg::MovedValue { value: operand, .. }
        | RirCallArg::InitFieldProvided(operand) => operand_uses_local(program, operand, local),
        RirCallArg::SharedBorrow(place) | RirCallArg::MutBorrow(place) => {
            place_uses_local(place, local)
        }
        RirCallArg::MutPlace(place) => mut_place_arg_uses_local(program, place, local),
        RirCallArg::DynBorrow(borrow) => match &borrow.source {
            super::rir::RirDynBorrowSource::Concrete { place, .. }
            | super::rir::RirDynBorrowSource::Owned { place, .. } => {
                mut_place_arg_uses_local(program, place, local)
            }
            super::rir::RirDynBorrowSource::Borrowed { local: found, .. }
            | super::rir::RirDynBorrowSource::Reborrowed { local: found, .. } => *found == local,
        },
        RirCallArg::ScopedLambda { callee, .. }
        | RirCallArg::EscapingLambda { callee, .. }
        | RirCallArg::AnvCallback { callee, .. } => operand_uses_local(program, callee, local),
        RirCallArg::SharedStringConst(_) | RirCallArg::InitFieldOmitted => false,
    }
}

fn option_subject_uses_local(
    program: &RirProgram,
    subject: &RirOptionSubject,
    local: RirLocalId,
) -> bool {
    match subject {
        RirOptionSubject::Place(place) => place_uses_local(place, local),
        RirOptionSubject::MutPlace(place) => mut_place_arg_uses_local(program, place, local),
    }
}

fn mut_place_arg_uses_local(program: &RirProgram, arg: &RirMutPlaceArg, local: RirLocalId) -> bool {
    mut_place_access_uses_local(program, &arg.access, local)
        || projections_use_local(&arg.projections, local)
}

fn mut_place_access_uses_local(
    program: &RirProgram,
    access: &RirMutPlaceAccess,
    local: RirLocalId,
) -> bool {
    match access {
        RirMutPlaceAccess::Handle(handle) => mut_place_handle_uses_local(program, handle, local),
        RirMutPlaceAccess::DataRef { object, .. } => operand_uses_local(program, object, local),
    }
}

fn mut_place_handle_uses_local(
    program: &RirProgram,
    handle: &RirMutPlaceHandle,
    local: RirLocalId,
) -> bool {
    match handle {
        RirMutPlaceHandle::Local { local: root, .. }
        | RirMutPlaceHandle::Param { local: root, .. } => *root == local,
        RirMutPlaceHandle::StackCell { cell, .. } | RirMutPlaceHandle::HeapCell { cell, .. } => {
            cell_uses_local(cell, local)
        }
        RirMutPlaceHandle::ScopedPlaceCell { cell, .. } => {
            scoped_cell_uses_local(program, cell, local)
        }
        RirMutPlaceHandle::Global { .. } => false,
    }
}

fn cell_uses_local(cell: &RirCellRef, local: RirLocalId) -> bool {
    matches!(cell, RirCellRef::Capture { local: captured, .. } if *captured == local)
}

fn scoped_cell_uses_local(
    program: &RirProgram,
    cell: &RirScopedPlaceCellRef,
    local: RirLocalId,
) -> bool {
    if let RirScopedPlaceCellRef::Capture {
        local: captured, ..
    } = cell
        && *captured == local
    {
        return true;
    }
    let cell = match cell {
        RirScopedPlaceCellRef::Owner(cell) | RirScopedPlaceCellRef::Capture { cell, .. } => cell,
    };
    program.scoped_place_cells[cell.index()]
        .source
        .place()
        .uses_local(local)
}

fn operand_uses_local(_program: &RirProgram, operand: &RirOperand, local: RirLocalId) -> bool {
    match operand {
        RirOperand::Place(place) => place_uses_local(place, local),
        RirOperand::Const(_) => false,
    }
}

fn ordinal_uses_local(program: &RirProgram, plan: &RirOrdinalPlan, local: RirLocalId) -> bool {
    plan.operands()
        .any(|operand| operand_uses_local(program, operand, local))
}

fn place_uses_local(place: &RirPlace, local: RirLocalId) -> bool {
    matches!(place.root, RirPlaceRoot::Local(root) if root == local)
        || projections_use_local(&place.projections, local)
}

fn projections_use_local(projections: &[RirProjection], local: RirLocalId) -> bool {
    projections
        .iter()
        .any(|projection| matches!(projection, RirProjection::Index(index) if *index == local))
}

fn block_uses_scoped_lambda_sig(block: &RirStructuredBlock, sig: RirLambdaSigId) -> bool {
    block
        .stmts
        .iter()
        .any(|stmt| stmt_uses_scoped_lambda_sig(stmt, sig))
}

fn stmt_uses_scoped_lambda_sig(stmt: &RirStmt, sig: RirLambdaSigId) -> bool {
    stmt_directly_uses_scoped_lambda_sig(stmt, sig)
        || stmt_child_blocks_any(stmt, |block| block_uses_scoped_lambda_sig(block, sig))
}

fn stmt_directly_uses_scoped_lambda_sig(stmt: &RirStmt, sig: RirLambdaSigId) -> bool {
    match stmt {
        RirStmt::Init { value, .. }
        | RirStmt::Assign { value, .. }
        | RirStmt::GlobalSetRoot { value, .. }
        | RirStmt::GlobalUpdateRoot { value, .. }
        | RirStmt::MutPlaceSet { value, .. }
        | RirStmt::CellInit { value, .. }
        | RirStmt::CellSet { value, .. }
        | RirStmt::ScopedPlaceCellSet { value, .. }
        | RirStmt::Eval(value) => rvalue_uses_scoped_lambda_sig(value, sig),
        RirStmt::GlobalEnsure { .. }
        | RirStmt::ScopedPlaceCellInit { .. }
        | RirStmt::DataRefSet { .. }
        | RirStmt::SequenceSlotSet { .. }
        | RirStmt::MapValueSet { .. }
        | RirStmt::If(_)
        | RirStmt::Loop(_)
        | RirStmt::RangeFor(_)
        | RirStmt::CollectionFor(_)
        | RirStmt::CollectionLoanScope(_)
        | RirStmt::CollectionSlotScope(_)
        | RirStmt::PatternMatch(_)
        | RirStmt::DynMatch(_)
        | RirStmt::OptionMatch(_)
        | RirStmt::MapEntryMatch(_) => false,
    }
}

fn rvalue_uses_scoped_lambda_sig(value: &RirRValue, sig: RirLambdaSigId) -> bool {
    let RirRValue::Call { args, .. } = value else {
        return false;
    };
    args.iter()
        .any(|arg| matches!(arg, RirCallArg::ScopedLambda { sig: arg_sig, .. } if *arg_sig == sig))
}

fn call_arg_root_local(arg: &RirCallArg) -> Option<RirLocalId> {
    match arg {
        RirCallArg::Value(RirOperand::Place(place))
        | RirCallArg::MovedValue {
            value: RirOperand::Place(place),
            ..
        }
        | RirCallArg::InitFieldProvided(RirOperand::Place(place))
        | RirCallArg::ScopedLambda {
            callee: RirOperand::Place(place),
            ..
        }
        | RirCallArg::EscapingLambda {
            callee: RirOperand::Place(place),
            ..
        }
        | RirCallArg::AnvCallback {
            callee: RirOperand::Place(place),
            ..
        }
        | RirCallArg::SharedBorrow(place)
        | RirCallArg::MutBorrow(place) => {
            let RirPlaceRoot::Local(local) = place.root else {
                return None;
            };
            place.projections.is_empty().then_some(local)
        }
        RirCallArg::MutPlace(mut_place) => match &mut_place.access {
            RirMutPlaceAccess::Handle(RirMutPlaceHandle::Local { local, .. }) => {
                mut_place.projections.is_empty().then_some(*local)
            }
            _ => None,
        },
        RirCallArg::DynBorrow(borrow) => match &borrow.source {
            super::rir::RirDynBorrowSource::Concrete { place, .. }
            | super::rir::RirDynBorrowSource::Owned { place, .. } => match &place.access {
                RirMutPlaceAccess::Handle(RirMutPlaceHandle::Local { local, .. }) => {
                    place.projections.is_empty().then_some(*local)
                }
                _ => None,
            },
            super::rir::RirDynBorrowSource::Borrowed { local, .. }
            | super::rir::RirDynBorrowSource::Reborrowed { local, .. } => Some(*local),
        },
        RirCallArg::Value(RirOperand::Const(_))
        | RirCallArg::MovedValue {
            value: RirOperand::Const(_),
            ..
        }
        | RirCallArg::InitFieldProvided(RirOperand::Const(_))
        | RirCallArg::InitFieldOmitted
        | RirCallArg::ScopedLambda {
            callee: RirOperand::Const(_),
            ..
        }
        | RirCallArg::EscapingLambda {
            callee: RirOperand::Const(_),
            ..
        }
        | RirCallArg::AnvCallback {
            callee: RirOperand::Const(_),
            ..
        }
        | RirCallArg::SharedStringConst(_) => None,
    }
}

fn rust_format_spec(spec: RirFormatSpec) -> FormatSpec {
    FormatSpec {
        fill: spec.fill,
        align: spec.align.map(|align| match align {
            RirFormatAlign::Left => FormatAlign::Left,
            RirFormatAlign::Right => FormatAlign::Right,
            RirFormatAlign::Center => FormatAlign::Center,
        }),
        sign: match spec.sign {
            RirFormatSign::Default => FormatSign::Default,
            RirFormatSign::Always => FormatSign::Always,
        },
        zero_pad: spec.zero_pad,
        width: spec.width,
        precision: spec.precision,
        kind: match spec.kind {
            RirFormatKind::Default => FormatKind::Default,
            RirFormatKind::Hex => FormatKind::Hex,
            RirFormatKind::HexUpper => FormatKind::HexUpper,
            RirFormatKind::Binary => FormatKind::Binary,
            RirFormatKind::Exp => FormatKind::Exp,
            RirFormatKind::ExpUpper => FormatKind::ExpUpper,
        },
    }
}

enum RangeCopySource {
    List,
    Array,
    Slice,
}

fn loop_label(id: RirLoopId) -> String {
    format!("'loop_{}", id.index())
}

fn lambda_variant(id: RirLambdaId) -> String {
    format!("L{}", id.index())
}

fn lambda_env_heap_type_symbol(id: RirLambdaEnvId) -> String {
    format!("lambda_env{}", id.index())
}

fn lambda_cell_heap_type_symbol(id: super::rir::RirCellId) -> String {
    format!("lambda_cell{}", id.index())
}

fn lambda_capture_call_arg(index: usize, capture: &RirLambdaCapture) -> String {
    match capture.kind {
        RirLambdaCaptureKind::Param => match capture.semantic {
            RirParamSemantic::Value | RirParamSemantic::SharedBorrow => format!("*c{index}"),
            RirParamSemantic::MutBorrow => format!("&mut **c{index}"),
            RirParamSemantic::MutPlace
            | RirParamSemantic::DynBorrow
            | RirParamSemantic::ScopedLambda
            | RirParamSemantic::EscapingLambda
            | RirParamSemantic::AnvCallback
            | RirParamSemantic::StackCell
            | RirParamSemantic::HeapCell
            | RirParamSemantic::ScopedPlaceCell => unreachable!("verified non-param capture kind"),
        },
        RirLambdaCaptureKind::StackCell { .. } | RirLambdaCaptureKind::ScopedPlaceCell { .. } => {
            format!("*c{index}")
        }
        RirLambdaCaptureKind::HeapCell { .. } => format!("c{index}.clone()"),
    }
}
