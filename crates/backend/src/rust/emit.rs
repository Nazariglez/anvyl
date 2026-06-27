use super::{
    analysis,
    dataref_place::{DataRefPlaceDescriptor, DataRefPlaceDescriptors},
    place::RustPlaces,
    rep_policy::{LambdaTraceAction, RustRepPolicy, RustTracePlan},
    rir::{
        RirCallArg, RirCallTarget, RirCellDecl, RirCellLifetime, RirCellRef, RirCellStorage,
        RirCollectionAccess, RirCollectionLoanScope, RirCollectionStorageKind, RirCoreEnumKind,
        RirDataRefId, RirEnum, RirEnumId, RirEnumMatch, RirEnumRepr, RirExternKind, RirFormatAlign,
        RirFormatKind, RirFormatSign, RirFormatSpec, RirFunction, RirIf, RirLambdaCapture,
        RirLambdaCaptureArg, RirLambdaCaptureKind, RirLambdaEnvFieldKind, RirLambdaEnvId,
        RirLambdaEnvLayout, RirLambdaId, RirLambdaSig, RirLambdaSigId, RirLambdaStorage,
        RirLocalId, RirLoop, RirLoopId, RirMapEntryMatch, RirMutPlaceAccess, RirMutPlaceArg,
        RirMutPlaceHandle, RirOperand, RirOptionMatch, RirOptionSubject, RirParamAbi,
        RirParamSemantic, RirPlace, RirPlaceRoot, RirProgram, RirProjection, RirRValue,
        RirRawEnumValue, RirScopedPlaceCellRef, RirStmt, RirStructuredBlock, RirTerm, RirTupleId,
        RirType, RirTypeId, RirVariant, RirVariantId, RirVariantKind, VerifiedRirProgram,
        native_return_ty_is_resource, stmt_child_blocks_any,
    },
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
        w: RustWriter::default(),
        collection_loans: vec![],
    };
    cx.emit_program();
    RustSource::new(cx.w.finish())
}

struct EmitCx<'a> {
    program: &'a RirProgram,
    dataref_places: DataRefPlaceDescriptors,
    trace_plan: RustTracePlan,
    fallible_functions: Vec<bool>,
    retained_callback_sigs: Vec<RirLambdaSigId>,
    w: RustWriter,
    collection_loans: Vec<ActiveCollectionLoan>,
}

struct ActiveCollectionLoan {
    root: RirCollectionAccess,
    version: String,
}

impl EmitCx<'_> {
    fn collection_storage_heap_type(&self, value_ty: RirTypeId) -> String {
        let storage = self
            .program
            .collection_storage_for(value_ty)
            .expect("verified collection storage declaration");
        format!("types.{}", storage.symbol.as_str())
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
        let types = target::generated_types_symbol(&self.program.ctx);
        let globals = target::generated_globals_symbol(&self.program.ctx);
        let policy = RustRepPolicy::new(self.program);
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
            let record = target::callback_record_symbol(sig.index());
            (
                format!("callback_record_sig{}", sig.index()),
                format!("{record}<'cx>"),
                target::heap_register(self.trace_plan.needs_lambda_sig_trace(*sig)),
            )
        }));

        self.w.block(format_args!("struct {types}<'cx>"), |w| {
            for (heap_type, storage, _) in &heap_types {
                w.line(format_args!(
                    "{heap_type}: {},",
                    target::heap_type_ty(storage)
                ));
            }
            w.line("_brand: std::marker::PhantomData<&'cx ()>,");
        });
        self.w.blank();
        self.w.block(format_args!("impl<'cx> {types}<'cx>"), |w| {
            w.block(
                format_args!(
                    "fn register({}: &mut {}) -> Self",
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
        self.w.block(format_args!("impl<'cx> {globals}<'cx>"), |w| {
            w.block("fn new() -> Self", |w| {
                w.block("Self", |w| {
                    for global in &self.program.globals {
                        w.line(format_args!(
                            "{}: {},",
                            global.slot_symbol.as_str(),
                            target::global_slot_new(global.name.as_str())
                        ));
                    }
                    w.line("_brand: std::marker::PhantomData,");
                });
            });
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
                format_args!("impl<'cx> {}<'cx> for {ty}", target::ctx_roots_ty()),
                |w| {
                    w.block(
                        format_args!("fn validate_roots(&self) -> {}", target::result_ty("()")),
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
            w.line(format_args!("types: {types}<'cx>,"));
            w.line(format_args!("globals: {globals}<'cx>,"));
            if !retained_sigs.is_empty() {
                w.line(format_args!(
                    "callbacks: {}<'cx>,",
                    target::generated_callback_registry_symbol()
                ));
            }
            w.line(format_args!("heap: {},", target::heap_ty()));
            w.line(format_args!("_pin: {},", target::phantom_pinned_ty()));
        });
        self.w.blank();
        self.w
            .block(format_args!("struct AnvEntry<'entry, 'cx>"), |w| {
                w.line(format_args!(
                    "heap: {},",
                    target::non_null_ty(&target::heap_ty())
                ));
                w.line(format_args!("types: &'entry {types}<'cx>,"));
                w.line(format_args!("globals: &'entry {globals}<'cx>,"));
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
                    w.line(format_args!("let types = {types}::register(&mut heap);"));
                    w.line(format_args!("let globals = {globals}::new();"));
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
                            w.line("types,");
                            w.line("globals,");
                            if !retained_sigs.is_empty() {
                                w.line("callbacks,");
                            }
                            w.line("heap,");
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
                        w.line(format_args!(
                            "{}.expect(\"runtime owner attach failed\");",
                            target::owner_attach(
                                "runtime.owner",
                                &format!("{}.cast()", target::non_null_from_mut("inner"))
                            )
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
                    w.line(format_args!(
                        "let owner_entry = {}?;",
                        target::owner_enter_current("self.owner")
                    ));
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
                        w.line("types: &inner.types,");
                        w.line("globals: &inner.globals,");
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
                        "let _ = {};",
                        target::owner_detach("self.owner")
                    ));
                });
            });
        self.w.blank();
    }

    fn has_retained_callbacks(&self) -> bool {
        !self.retained_callback_sigs.is_empty()
    }

    fn emit_callback_registry(&mut self) {
        if self.retained_callback_sigs.is_empty() {
            return;
        }
        let sigs = self.retained_callback_sigs.clone();
        let policy = RustRepPolicy::new(self.program);
        for sig in &sigs {
            let record = target::callback_record_symbol(sig.index());
            let lambda = policy.lambda_sig_storage_ty(*sig);
            self.w.block(format_args!("struct {record}<'cx>"), |w| {
                w.line(format_args!("lambda: {lambda},"));
                w.line("_brand: std::marker::PhantomData<&'cx ()>,");
            });
            self.w.blank();
            if self.trace_plan.needs_lambda_sig_trace(*sig) {
                let ty = format!("{record}<'cx>");
                self.w.block(
                    format_args!("{}", target::trace_impl_header("<'cx>", &ty)),
                    |w| {
                        w.line(format_args!("{} {{", target::trace_fn_header()));
                        w.indented(|w| {
                            w.line(format_args!(
                                "{}::trace(&self.lambda, visitor);",
                                target::trace_ty()
                            ));
                        });
                        w.line("}");
                    },
                );
                self.w.blank();
            }
        }
        let registry = target::generated_callback_registry_symbol();
        self.w.line("#[derive(Default)]");
        self.w.block(format_args!("struct {registry}<'cx>"), |w| {
            for sig in &sigs {
                let field = target::callback_table_field(sig.index());
                let record = target::callback_record_symbol(sig.index());
                let root = target::root_id_ty(&format!("{record}<'cx>"));
                w.line(format_args!(
                    "{field}: Vec<{}>,",
                    target::callback_slot_ty(&root)
                ));
            }
            w.line("_brand: std::marker::PhantomData<&'cx ()>,");
        });
        self.w.blank();
        self.w
            .block(format_args!("impl<'cx> {registry}<'cx>"), |w| {
                for sig in &sigs {
                    let field = target::callback_table_field(sig.index());
                    let record = target::callback_record_symbol(sig.index());
                    let root = target::root_id_ty(&format!("{record}<'cx>"));
                    w.block(
                        format_args!(
                            "fn insert_{field}(&mut self, root: {root}) -> (usize, std::num::NonZeroU64)"
                        ),
                        |w| {
                            w.line(format_args!(
                                "if let Some((index, slot)) = self.{field}.iter_mut().enumerate().find(|(_, slot)| slot.is_free()) {{"
                            ));
                            w.indented(|w| {
                                w.line("let generation = slot.insert(root).expect(\"callback slot open failed\");");
                                w.line("return (index, generation);");
                            });
                            w.line("}");
                            w.line(format_args!("let index = self.{field}.len();"));
                            w.line(format_args!(
                                "let mut slot = {}::default();",
                                target::callback_slot_turbofish(&root)
                            ));
                            w.line("let generation = slot.insert(root).expect(\"callback slot open failed\");");
                            w.line(format_args!("self.{field}.push(slot);"));
                            w.line("(index, generation)");
                        },
                    );
                }
            });
        self.w.blank();
    }

    fn callback_args_ty(&self, sig: &RirLambdaSig) -> String {
        let policy = RustRepPolicy::new(self.program);
        match sig.params.as_slice() {
            [] => "()".to_string(),
            [param] => format!(
                "({},)",
                policy.callable_param_ty(param.ty, param.abi, param.escape)
            ),
            params => format!(
                "({})",
                comma(params.iter().map(|param| policy.callable_param_ty(
                    param.ty,
                    param.abi,
                    param.escape
                )))
            ),
        }
    }

    fn callback_ret_ty(&self, sig: &RirLambdaSig) -> String {
        RustRepPolicy::new(self.program).callable_ret_ty(sig.ret)
    }

    fn emit_callback_thunks(&mut self) {
        for sig_id in self.retained_callback_sigs.clone() {
            let sig = &self.program.lambda_sigs[sig_id.index()];
            let args_ty = self.callback_args_ty(sig);
            let ret_ty = self.callback_ret_ty(sig);
            let call_thunk = target::callback_call_thunk_symbol(sig_id.index());
            let close_thunk = target::callback_close_thunk_symbol(sig_id.index());
            let key_ty = target::callback_key_ty();
            let owner_ty = target::runtime_owner_handle_ty();
            let result_ret = target::result_ty(&ret_ty);
            let inner = target::generated_runtime_inner_symbol();
            let field = target::callback_table_field(sig_id.index());
            let expected_table = sig_id.index() + 1;
            let expected_signature = sig_id.index() + 1;
            let key_check = format!(
                "key.__anvyx_check_identity(std::num::NonZeroU64::new({expected_table}).unwrap(), std::num::NonZeroU64::new({expected_signature}).unwrap())?;"
            );
            let lambda_call_args = [
                "&mut rt".to_string(),
                "types".to_string(),
                "globals".to_string(),
                "owner".to_string(),
                "callbacks".to_string(),
            ]
            .into_iter()
            .chain((0..sig.params.len()).map(|index| format!("args.{index}")))
            .collect::<Vec<_>>();
            let lambda_fallible = self.lambda_sig_fallible(sig);
            let trace_globals = self
                .program
                .globals
                .iter()
                .any(|global| RustRepPolicy::new(self.program).type_owns_heap_edges(global.ty));
            self.w.block(
                format_args!(
                    "unsafe fn {call_thunk}(owner: &{owner_ty}, key: {key_ty}, args: {args_ty}) -> {result_ret}"
                ),
                |w| {
                    w.line(&key_check);
                    w.line("let owner_entry = owner.__anvyx_enter(key.owner_id(), key.shutdown_generation())?;");
                    w.line(format_args!(
                        "let mut inner_ptr = {}.cast::<{inner}<'_>>();",
                        target::owner_entry_ptr("owner_entry")
                    ));
                    w.line("let (lambda, guard) = {");
                    w.indented(|w| {
                        w.line("let inner = unsafe { inner_ptr.as_mut() };");
                        w.line(format_args!("let table = &mut inner.callbacks.{field};"));
                        w.line("let Some(slot) = table.get_mut(key.index()) else {");
                        w.indented(|w| {
                            w.line(format_args!(
                                "return Err({}::new(\"callback slot is closed\"));",
                                target::runtime_error_ty()
                            ));
                        });
                        w.line("};");
                        w.line("let (root, guard) = slot.begin_invocation(owner, key)?;");
                        w.line("let Some(lambda) = inner.heap.with_root(root, |record| record.lambda.clone()) else {");
                        w.indented(|w| {
                            w.line(format_args!(
                                "return Err({}::new(\"callback root is missing\"));",
                                target::runtime_error_ty()
                            ));
                        });
                        w.line("};");
                        w.line("(lambda, guard)");
                    });
                    w.line("};");
                    w.line("let (heap, types, globals, callbacks) = {");
                    w.indented(|w| {
                        w.line("let inner = unsafe { inner_ptr.as_mut() };");
                        w.line("let heap = std::ptr::NonNull::from(&mut inner.heap);");
                        w.line("let types = std::ptr::NonNull::from(&inner.types);");
                        w.line("let globals = std::ptr::NonNull::from(&inner.globals);");
                        w.line("let callbacks = std::ptr::NonNull::from(&mut inner.callbacks);");
                        w.line("(heap, types, globals, callbacks)");
                    });
                    w.line("};");
                    w.line("let types = unsafe { types.as_ref() };");
                    w.line("let globals = unsafe { globals.as_ref() };");
                    w.line(format_args!(
                        "let mut rt = {};",
                        if trace_globals {
                            target::runtime_ctx_from_raw_with_roots("heap", "globals")
                        } else {
                            target::runtime_ctx_from_raw("heap")
                        }
                    ));
                    let lambda_call = format!("lambda.call({})", comma(lambda_call_args));
                    let result = if lambda_fallible {
                        lambda_call
                    } else {
                        format!("Ok({lambda_call})")
                    };
                    w.line(format_args!("let __anv_callback_result = {result};"));
                    w.line("let __anv_callback_action = guard.finish();");
                    w.line("if __anv_callback_action == Ok(anvyx_runtime::CallbackCloseAction::RemoveRoot) {");
                    w.indented(|w| {
                        w.line("let inner = unsafe { inner_ptr.as_mut() };");
                        w.line(format_args!("let table = &mut inner.callbacks.{field};"));
                        w.line("if let Some(slot) = table.get_mut(key.index()) {");
                        w.indented(|w| {
                            w.line("if let Some(root) = slot.take_closed_root(key) {");
                            w.indented(|w| w.line("inner.heap.remove_root(&root);"));
                            w.line("}");
                        });
                        w.line("}");
                    });
                    w.line("}");
                    w.line("__anv_callback_action?;");
                    w.line("__anv_callback_result");
                },
            );
            self.w.blank();
            self.w.block(
                format_args!(
                    "unsafe fn {close_thunk}(owner: &{owner_ty}, key: {key_ty}) -> {}",
                    target::result_ty("bool")
                ),
                |w| {
                    w.line(&key_check);
                    w.line("let owner_entry = owner.__anvyx_enter(key.owner_id(), key.shutdown_generation())?;");
                    w.line(format_args!(
                        "let inner = unsafe {{ {} }};",
                        target::non_null_cast_mut(
                            &target::owner_entry_ptr("owner_entry"),
                            &format!("{inner}<'_>")
                        )
                    ));
                    w.line(format_args!("let table = &mut inner.callbacks.{field};"));
                    w.line("let Some(slot) = table.get_mut(key.index()) else { return Ok(false); };");
                    w.line("let (close, root) = slot.close(key);");
                    w.line("if let Some(root) = root {");
                    w.indented(|w| w.line("inner.heap.remove_root(&root);"));
                    w.line("}");
                    w.line("Ok(close.closed)");
                },
            );
            self.w.blank();
        }
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
                        RustRepPolicy::new(self.program).type_owns_heap_edges(global.ty)
                    }) {
                        target::runtime_ctx_from_raw_with_roots(
                            "anv_entry.heap",
                            "anv_entry.globals",
                        )
                    } else {
                        target::runtime_ctx_from_raw("anv_entry.heap")
                    };
                    w.line(format_args!("let mut rt = {rt};"));
                    let mut args = vec![
                        "&mut rt".to_string(),
                        "anv_entry.types".to_string(),
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
        let policy = RustRepPolicy::new(self.program);
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
        let policy = RustRepPolicy::new(program);
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
                "impl<'cx> {} for {}<'cx>",
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
        w.indented(|w| {
            w.line(op.heap_access(
                "rt",
                "object",
                &target::dataref_place_heap_type_access("self"),
                "storage",
                storage,
                &format!("f({}{path})", op.path_ref()),
            ));
        });
        w.line("}");
    }

    fn emit_lambda_env(&mut self, env: &RirLambdaEnvLayout) {
        let policy = RustRepPolicy::new(self.program);
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
        let policy = RustRepPolicy::new(self.program);
        self.emit_record_struct(
            strukt.symbol.as_str(),
            &strukt.fields,
            self.trace_plan.needs_struct_trace(strukt.id),
            policy.struct_cx_dependent(strukt),
            &policy.record_derives(&strukt.fields),
        );
    }

    fn emit_tuple(&mut self, tuple: &super::rir::RirTuple) {
        let policy = RustRepPolicy::new(self.program);
        self.emit_record_struct(
            tuple.symbol.as_str(),
            &tuple.fields,
            self.trace_plan.needs_tuple_trace(tuple.id),
            policy.tuple_cx_dependent(tuple),
            &policy.record_derives(&tuple.fields),
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
        let policy = RustRepPolicy::new(self.program);
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

    fn emit_enum(&mut self, enm: &RirEnum) {
        let policy = RustRepPolicy::new(self.program);
        let cx_dependent = policy.enum_cx_dependent(enm);
        let needs_trace = self.trace_plan.needs_enum_trace(enm.id);
        let copy = enm.repr == RirEnumRepr::RawInt && !enm.variants.is_empty();
        let derives = policy.enum_derives(enm);
        if needs_trace {
            self.w.line(target::trace_derive(&derives));
            self.w.line(target::trace_crate_attr(cx_dependent));
        } else {
            self.w.line(format_args!(
                "#[derive({})]",
                comma(derives.iter().map(|derive| (*derive).to_string()))
            ));
        }
        if copy {
            self.w.line("#[repr(i64)]");
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
                    let fields = comma(
                        variant
                            .fields
                            .iter()
                            .map(|field| policy.rust_storage_ty(field.ty)),
                    );
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
        self.w.blank();
    }

    fn emit_stringify_helper(&mut self, helper: &super::rir::RirStringifyHelper) {
        let RirType::Struct(struct_id) = self.program.types[helper.ty.index()] else {
            unreachable!("verified stringify helper target")
        };
        let strukt = &self.program.structs[struct_id.index()];
        let ctx_use = analysis::stringify_helper_context_use(self.program, strukt);
        let header = format!(
            "fn {}<'cx, 'rt>({}: {}, {}: {}, value: &{}) -> {}",
            helper.symbol.as_str(),
            target::runtime_param(ctx_use.rt),
            target::runtime_ctx_ref_ty(),
            target::types_param(ctx_use.types),
            target::types_ref_ty(target::generated_types_symbol(&self.program.ctx)),
            self.ty(helper.ty),
            target::anv_string_ty()
        );
        let display = rust_string(strukt.display.as_str());
        let fields = strukt
            .fields
            .iter()
            .map(|field| {
                let ty = self.program.types[field.ty.index()];
                let nested = matches!(ty, RirType::Struct(_))
                    .then(|| self.stringify_helper(field.ty).to_string());
                (field.symbol.as_str().to_string(), ty, nested)
            })
            .collect::<Vec<_>>();

        self.w.block(format_args!("{header}"), |w| {
            w.line("let mut out = String::new();");
            w.line(format_args!("out.push_str({display});"));
            w.line("out.push('(');");
            for (index, (field, ty, nested)) in fields.iter().enumerate() {
                if index > 0 {
                    w.line("out.push_str(\", \");");
                }
                w.line(format_args!(
                    "out.push_str({});",
                    rust_string(&format!("{field}: "))
                ));
                match ty {
                    RirType::Float => {
                        let text = Self::default_scalar_display(&format!("value.{field}"), ty);
                        w.line(format_args!("out.push_str({text}.as_str());"));
                    }
                    RirType::Int | RirType::Bool => {
                        w.line(format_args!(
                            "std::fmt::Write::write_fmt(&mut out, format_args!(\"{{}}\", value.{field})).unwrap();"
                        ));
                    }
                    RirType::String => {
                        w.line(format_args!("out.push_str(value.{field}.as_str());"));
                    }
                    RirType::Struct(_) => {
                        let nested = nested.as_deref().expect("struct field has helper");
                        w.line(format_args!(
                            "out.push_str({nested}(rt, types, &value.{field}).as_str());"
                        ));
                    }
                    RirType::Void
                    | RirType::Tuple(_)
                    | RirType::DataRef(_)
                    | RirType::Enum(_)
                    | RirType::Array { .. }
                    | RirType::List(_)
                    | RirType::Map { .. }
                    | RirType::Option(_)
                    | RirType::Slice(_)
                    | RirType::Lambda(_) => unreachable!("verified stringify helper field"),
                }
            }
            w.line("out.push(')');");
            w.line(target::anv_string_from("out"));
        });
        self.w.blank();
    }

    fn emit_lambda_sig(&mut self, sig: &RirLambdaSig) {
        let policy = RustRepPolicy::new(self.program);
        let symbol = policy.lambda_sig_symbol(sig.id);
        let variants = self
            .program
            .lambdas_for_sig(sig.id)
            .map(|lambda| {
                (
                    lambda.id,
                    lambda.function,
                    lambda.storage,
                    lambda.captures.as_slice(),
                )
            })
            .collect::<Vec<_>>();
        let retained_callbacks = self.has_retained_callbacks();
        let mut params = vec![
            format!("rt: {}", target::runtime_ctx_ref_ty()),
            format!(
                "types: {}",
                target::types_ref_ty(target::generated_types_symbol(&self.program.ctx))
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
        let fallible = variants
            .iter()
            .any(|(_, function, _, _)| self.fallible_functions[function.index()]);
        let ret = self.lambda_sig_ret_ty(sig, fallible);
        let captures_self = variants
            .iter()
            .any(|(_, _, _, captures)| !captures.is_empty());
        let mut_self = variants.iter().any(|(_, _, storage, captures)| {
            if matches!(storage, RirLambdaStorage::HeapEnv { .. }) {
                return false;
            }
            captures
                .iter()
                .any(|capture| capture.semantic == RirParamSemantic::MutBorrow)
        });
        let self_arg = match (captures_self, mut_self) {
            (_, true) => "&mut self",
            (true, false) => "&self",
            (false, false) => "self",
        };
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
            for (lambda, _, storage, captures) in &variants {
                if let RirLambdaStorage::HeapEnv { env } = storage {
                    let env = &self.program.lambda_envs[env.index()];
                    w.line(format_args!(
                        "{} {{ env: {} }},",
                        lambda_variant(*lambda),
                        target::handle_ty(&policy.lambda_env_storage_ty(env))
                    ));
                } else if captures.is_empty() {
                    w.line(format_args!("{},", lambda_variant(*lambda)));
                } else {
                    let fields = captures
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
                        lambda_variant(*lambda),
                        comma(fields)
                    ));
                }
            }
        });
        self.w.blank();
        if trace {
            self.emit_lambda_sig_trace_impl(sig.id, &variants);
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
                        for (lambda, function_id, storage, captures) in &variants {
                            let function = &program.functions[function_id.index()];
                            let variant = lambda_variant(*lambda);
                            match storage {
                                RirLambdaStorage::HeapEnv { env } => {
                                    let env = &program.lambda_envs[env.index()];
                                    w.line(format_args!("Self::{variant} {{ env }} => {{"));
                                    w.indented(|w| {
                                        let values = RustValues::new(program, function);
                                        for (index, field) in env.fields.iter().enumerate() {
                                            let capture = &captures[index];
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
                                        let capture_args = (0..captures.len())
                                            .map(|index| format!("c{index}"))
                                            .collect();
                                        w.line(body_call(function, capture_args));
                                    });
                                    w.line("},");
                                }
                                _ if captures.is_empty() => {
                                    w.line(format_args!(
                                        "Self::{variant} => {},",
                                        body_call(function, vec![])
                                    ));
                                }
                                _ => {
                                    let fields = (0..captures.len())
                                        .map(|index| format!("c{index}"))
                                        .collect::<Vec<_>>();
                                    let capture_args = captures
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
        variants: &[(
            RirLambdaId,
            super::rir::RirFunctionId,
            RirLambdaStorage,
            &[RirLambdaCapture],
        )],
    ) {
        let policy = RustRepPolicy::new(self.program);
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
                        for (lambda, _, storage, captures) in variants {
                            let variant = lambda_variant(*lambda);
                            match RustRepPolicy::lambda_trace_action(
                                &self.program.lambdas[lambda.index()],
                            ) {
                                LambdaTraceAction::HeapEnv => {
                                    debug_assert!(matches!(
                                        storage,
                                        RirLambdaStorage::HeapEnv { .. }
                                    ));
                                    w.line(format_args!(
                                        "Self::{variant} {{ env }} => {}::trace(env, visitor),",
                                        target::trace_ty()
                                    ));
                                }
                                LambdaTraceAction::HeapCellCaptures(cells) => {
                                    let fields = (0..captures.len())
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
                                LambdaTraceAction::Noop if captures.is_empty() => {
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
        let policy = RustRepPolicy::new(self.program);
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
        let rt_ty = format!("anvyx_runtime::Ctx<{ctx_lifetime}, '_>");
        let types_ty = format!(
            "{}<{ctx_lifetime}>",
            target::generated_types_symbol(&self.program.ctx)
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
                            "let types = unsafe {{ state.2.cast::<{types_ty}>().as_ref() }};"
                        ));
                        w.line(format_args!(
                            "let globals = unsafe {{ state.3.cast::<{globals_ty}>().as_ref() }};"
                        ));
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
                        }
                        w.line(destructure);
                        let call_args = if retained_callbacks {
                            target::retained_generated_call_args(args)
                        } else {
                            target::generated_call_args(args)
                        };
                        let call = format!("state.0.call({})", comma(call_args));
                        if fallible {
                            w.line(call);
                        } else if ret_ty == "()" {
                            w.line(format_args!("{call};"));
                            w.line("Ok(())");
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
        let ret = RustRepPolicy::new(self.program).callable_ret_ty(sig.ret);
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
        let policy = RustRepPolicy::new(self.program);
        let retained_callbacks = self.has_retained_callbacks();
        let mut params = vec![
            format!(
                "{}: {}",
                target::runtime_param(ctx_use.rt),
                target::runtime_ctx_ref_ty()
            ),
            format!(
                "{}: {}",
                target::types_param(ctx_use.types),
                target::types_ref_ty(target::generated_types_symbol(&self.program.ctx))
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
            let mutability =
                if param.abi == RirParamAbi::MutPlace || self.local_needs_mut_binding(local.ty) {
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
                        | RirStmt::CollectionLoanScope(_)
                        | RirStmt::CollectionSlotScope(_)
                        | RirStmt::EnumMatch(_)
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
        let ret = RustRepPolicy::new(self.program).callable_ret_ty(function.ret.ty);
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
        let policy = RustRepPolicy::new(self.program);
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
        for cell in self
            .program
            .scoped_place_cells
            .iter()
            .filter(|cell| cell.owner == function.id)
        {
            let (prelude, source) =
                self.prepared_mut_place_arg(function, cell.id.index(), cell.source.place());
            for line in prelude {
                self.w.line(format_args!("{line}"));
            }
            self.w.line(format_args!(
                "let {} = {};",
                cell.symbol.as_str(),
                target::scoped_mut_place_cell_new(&source)
            ));
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
            RirStmt::CollectionLoanScope(scope) => {
                self.emit_collection_loan_scope(function, scope, predeclared);
            }
            RirStmt::CollectionSlotScope(block) => {
                self.emit_lexical_block(function, block, predeclared);
            }
            RirStmt::EnumMatch(match_) => self.emit_match(function, match_, predeclared),
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

    fn emit_match(&mut self, function: &RirFunction, match_: &RirEnumMatch, predeclared: bool) {
        let RirType::Enum(enum_id) = self.program.types[match_.discr.ty.index()] else {
            unreachable!("verified enum match")
        };
        let enm = &self.program.enums[enum_id.index()];
        let patterns = match_
            .arms
            .iter()
            .map(|arm| {
                let variant = &enm.variants[arm.variant.index()];
                Self::variant_pattern(enm, variant)
            })
            .collect::<Vec<_>>();
        self.w.line(format_args!(
            "match &{} {{",
            RustPlaces::new(self.program, function).local_place(&match_.discr)
        ));
        self.indented(|this| {
            for (arm, pattern) in match_.arms.iter().zip(patterns.iter()) {
                this.w.line(format_args!("{pattern} => {{"));
                this.indented(|this| {
                    this.emit_structured_block(function, &arm.block, predeclared);
                });
                this.w.line("}");
            }
            if let Some(else_block) = &match_.else_block {
                this.w.line("_ => {");
                this.indented(|this| {
                    this.emit_structured_block(function, else_block, predeclared);
                });
                this.w.line("}");
            }
        });
        self.w.line("}");
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
            target::scoped_mut_place_cell_new(&target::mut_place_projected(
                map,
                &format!("&{ops}")
            ))
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
            self.w.line("}");
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
            target::scoped_mut_place_cell_new(&target::mut_place_projected(
                subject,
                &format!("&{ops}"),
            ))
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

    fn rvalue(&mut self, function: &RirFunction, value: &RirRValue) -> String {
        let values = RustValues::new(self.program, function);
        let places = RustPlaces::new(self.program, function);
        match value {
            RirRValue::Use(operand) | RirRValue::FunctionValue { value: operand, .. } => {
                values.value_operand(operand)
            }
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
            RirRValue::Unary { op, value, .. } => {
                format!("{}{}", unary_op(*op), values.operand(value))
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
            RirRValue::OptionalSome { value, .. } => {
                format!("Some({})", values.value_operand(value))
            }
            RirRValue::Call { callee, args, .. } => match callee {
                RirCallTarget::Function(id) => {
                    let symbol = self.program.functions[id.index()].symbol.as_str();
                    let call = self.call_expr(function, args, |args| format!("{symbol}({args})"));
                    if self.fallible_functions[id.index()] {
                        format!("{call}?")
                    } else {
                        call
                    }
                }
                RirCallTarget::Extern(id) => self.extern_call(function, *id, args),
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
                    RirType::Int | RirType::Float | RirType::Bool => {
                        target::anv_string_from(&Self::default_scalar_display(
                            &values.operand(value),
                            &self.program.types[source_ty.index()],
                        ))
                    }
                    RirType::Struct(_) => self.stringify_struct(function, value, *source_ty),
                    RirType::Void
                    | RirType::Tuple(_)
                    | RirType::DataRef(_)
                    | RirType::Enum(_)
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
                target::anv_string_from(&Self::default_scalar_display(
                    &values.operand(value),
                    &self.program.types[source_ty.index()],
                ))
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
            RirRValue::MapEntryAt { map, index, ty } => {
                self.map_entry_at(function, map, *index, *ty)
            }
            RirRValue::MapValueAt { map, index, ty } => {
                self.map_value_at(function, map, *index, *ty)
            }
            RirRValue::Lambda {
                lambda, captures, ..
            } => {
                let lambda_decl = &self.program.lambdas[lambda.index()];
                let sig = RustRepPolicy::new(self.program).lambda_sig_symbol(lambda_decl.sig);
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
                        let heap_type = format!("types.{}", lambda_env_heap_type_symbol(env.id));
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
                target::slice_with_elem_owned_mut_short(
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
                target::list_with_elem_owned_mut_short(
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
                target::list_with_elem_owned_mut_short(
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
                target::slice_with_elem_owned_mut_short(
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
            format!(
                "{map}.get({}, &{key})?.map(|value| {})",
                target::runtime_param_name(),
                RustValues::new(self.program, function).value_from_place(value_ty, "value")
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
                let (mut prelude, place) = self.prepared_mut_place_arg(function, 0, root);
                prelude.extend(
                    bindings
                        .into_iter()
                        .map(|(name, value)| format!("let {name} = {value};")),
                );
                block_expr(
                    prelude,
                    Some(target::mut_place_mutate_ctx(
                        &place,
                        target::runtime_param_name(),
                        body,
                    )),
                )
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
                format!("{}::new({value})", target::stack_lambda_cell_ctor(&payload))
            }
            RirCellStorage::Heap => {
                let heap_type = format!(
                    "types.{}",
                    lambda_cell_heap_type_symbol(self.cell_decl(cell).id)
                );
                let storage = format!("{}::new(value)", target::lambda_cell_ctor(&payload));
                format!(
                    "{{ let value = {value}; let heap_type = {heap_type}; {} }}",
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
        let policy = RustRepPolicy::new(self.program);
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
        rendered: Vec<String>,
        render: impl FnOnce(String) -> String,
    ) -> String {
        self.prepared_native_call_expr(function, args, &[], &[], rendered, render)
    }

    fn prepared_native_call_expr(
        &self,
        function: &RirFunction,
        args: &[RirCallArg],
        abis: &[anvyx_runtime::RustParamAbi],
        tys: &[RirTypeId],
        mut rendered: Vec<String>,
        render: impl FnOnce(String) -> String,
    ) -> String {
        let mut prelude = vec![];
        let mut resource_borrows = vec![];
        for (index, arg) in args.iter().enumerate() {
            let (mut stmts, mut expr) = self.prepared_call_arg(function, index, arg);
            prelude.append(&mut stmts);
            if let (Some(abi), Some(ty)) = (abis.get(index), tys.get(index)) {
                if let anvyx_runtime::RustParamAbi::InitField(inner) = abi {
                    expr = match arg {
                        RirCallArg::InitFieldProvided(_) => {
                            let value = self.native_arg_expr(inner, *ty, expr);
                            target::init_field_provided(&value)
                        }
                        RirCallArg::InitFieldOmitted => target::init_field_omitted(),
                        _ => unreachable!("verified init field ABI"),
                    };
                } else if let Some(mutable) = self.native_ref_borrow_arg(abi, *ty) {
                    let arg = format!("__anv_ref_arg_{index}");
                    resource_borrows.push((expr, arg.clone(), mutable));
                    expr = arg;
                } else {
                    expr = self.native_arg_expr(abi, *ty, expr);
                }
            }
            rendered.push(expr);
        }
        let mut call = render(comma(rendered));
        for (resource, arg, mutable) in resource_borrows.into_iter().rev() {
            call = target::native_ref_borrow(&resource, &arg, mutable, &call);
        }
        if prelude.is_empty() {
            call
        } else {
            format!("{{ {} {call} }}", prelude.join(" "))
        }
    }

    fn native_ref_borrow_arg(
        &self,
        abi: &anvyx_runtime::RustParamAbi,
        ty: RirTypeId,
    ) -> Option<bool> {
        let native_ref = matches!(
            self.program.types[ty.index()],
            RirType::Struct(id) if self.program.structs[id.index()].native_ref
        );
        match (native_ref, abi) {
            (true, anvyx_runtime::RustParamAbi::Borrow(_)) => Some(false),
            (true, anvyx_runtime::RustParamAbi::MutBorrow(_)) => Some(true),
            _ => None,
        }
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
            | anvyx_runtime::RustParamAbi::EscapingLambda(_) => expr,
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
            anvyx_runtime::ExternTypeExpr::Named { .. } => self.native_named_arg_expr(ty, expr),
            _ => expr,
        }
    }

    fn native_named_arg_expr(&self, ty: RirTypeId, expr: String) -> String {
        match self.program.types[ty.index()] {
            RirType::Enum(enum_id) if self.program.enums[enum_id.index()].native_path.is_some() => {
                self.native_enum_arg_expr(enum_id, &expr)
            }
            _ => expr,
        }
    }

    fn native_enum_arg_expr(&self, enum_id: RirEnumId, expr: &str) -> String {
        let enm = &self.program.enums[enum_id.index()];
        let native = enm
            .native_path
            .as_ref()
            .expect("verified native enum type")
            .join("::");
        let arms = enm.variants.iter().map(|variant| {
            let generated_path = variant_path(enm.symbol.as_str(), variant.symbol.as_str());
            let native_path = variant_path(&native, variant.symbol.as_str());
            match variant.kind {
                RirVariantKind::Unit => format!("{generated_path} => {native_path}"),
                RirVariantKind::Tuple => {
                    let bindings = (0..variant.fields.len())
                        .map(|index| format!("value{index}"))
                        .collect::<Vec<_>>();
                    let fields = variant
                        .fields
                        .iter()
                        .zip(&bindings)
                        .map(|(field, binding)| self.native_rir_arg_expr(field.ty, binding.clone()))
                        .collect::<Vec<_>>();
                    format!(
                        "{} => {}",
                        tuple_variant(&generated_path, bindings.iter().cloned()),
                        tuple_variant(&native_path, fields)
                    )
                }
                RirVariantKind::Struct => {
                    let bindings = variant
                        .fields
                        .iter()
                        .map(|field| field.symbol.as_str().to_string())
                        .collect::<Vec<_>>();
                    let fields = variant
                        .fields
                        .iter()
                        .zip(&bindings)
                        .map(|(field, binding)| {
                            field_init(
                                field.symbol.as_str(),
                                self.native_rir_arg_expr(field.ty, binding.clone()),
                            )
                        });
                    format!(
                        "{} => {}",
                        struct_variant(&generated_path, bindings.iter().cloned()),
                        struct_variant(&native_path, fields)
                    )
                }
            }
        });
        match_expr(expr, arms)
    }

    fn native_rir_arg_expr(&self, ty: RirTypeId, expr: String) -> String {
        match self.program.types[ty.index()] {
            RirType::Option(inner) => target::rust_option_map(
                &expr,
                &self.native_rir_arg_expr(inner, "value".to_string()),
            ),
            RirType::Array { elem, .. } => Self::native_array_map_expr(
                &expr,
                &self.native_rir_arg_expr(elem, "value".to_string()),
            ),
            RirType::Tuple(tuple_id) => self.native_rir_tuple_arg_expr(tuple_id, &expr),
            RirType::Enum(enum_id)
                if self.program.enums[enum_id.index()].core == Some(RirCoreEnumKind::Result) =>
            {
                self.native_rir_result_arg_expr(enum_id, &expr)
            }
            RirType::Enum(enum_id) if self.program.enums[enum_id.index()].native_path.is_some() => {
                self.native_enum_arg_expr(enum_id, &expr)
            }
            _ => expr,
        }
    }

    fn native_rir_tuple_arg_expr(&self, tuple_id: RirTupleId, expr: &str) -> String {
        let tuple = &self.program.tuples[tuple_id.index()];
        target::rust_tuple(tuple.fields.iter().map(|field| {
            self.native_rir_arg_expr(field.ty, format!("{expr}.{}", field.symbol.as_str()))
        }))
    }

    fn native_rir_result_arg_expr(&self, enum_id: RirEnumId, expr: &str) -> String {
        let enm = &self.program.enums[enum_id.index()];
        let [ok, err] = enm.variants.as_slice() else {
            unreachable!("verified result enum variants")
        };
        target::rust_result_match(
            expr,
            &format!(
                "{}(value) => {}",
                variant_path(enm.symbol.as_str(), ok.symbol.as_str()),
                target::rust_ok(&self.native_rir_arg_expr(ok.fields[0].ty, "value".to_string()))
            ),
            &format!(
                "{}(value) => {}",
                variant_path(enm.symbol.as_str(), err.symbol.as_str()),
                target::rust_err(&self.native_rir_arg_expr(err.fields[0].ty, "value".to_string()))
            ),
        )
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
        if let RirCallArg::ScopedLambda { callee, sig } = arg {
            return self.prepared_scoped_lambda_call_arg(function, index, callee, *sig);
        }
        if let RirCallArg::EscapingLambda { callee, sig } = arg {
            return self.prepared_escaping_lambda_call_arg(function, index, callee, *sig);
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
                    target::heap_type_access("types", &heap_type)
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
        let policy = RustRepPolicy::new(self.program);
        let (args_ty, ret_ty) = policy.scoped_lambda_sig_args_ret(sig);
        let state = format!("__anv_scoped_lambda_state_{index}");
        let lambda = values.value_operand(callee);
        let rt_ptr = target::non_null_from_mut("&mut *rt");
        let types_ptr = target::non_null_from_mut("types");
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
                "let mut {state} = ({lambda}, {rt_ptr}.cast::<()>(), {types_ptr}.cast::<()>(), {globals_ptr}.cast::<()>(){});",
                retained_ptrs.as_deref().unwrap_or("")
            )],
            format!("unsafe {{ {ctor}::__anvyx_from_raw(&mut {state}, {thunk}) }}"),
        )
    }

    fn prepared_escaping_lambda_call_arg(
        &self,
        function: &RirFunction,
        index: usize,
        callee: &RirOperand,
        sig: RirLambdaSigId,
    ) -> (Vec<String>, String) {
        let values = RustValues::new(self.program, function);
        let lambda = values.value_operand(callee);
        let sig_data = &self.program.lambda_sigs[sig.index()];
        let args_ty = self.callback_args_ty(sig_data);
        let ret_ty = self.callback_ret_ty(sig_data);
        let record = target::callback_record_symbol(sig.index());
        let field = target::callback_table_field(sig.index());
        let callback_ctor = target::escaping_lambda_ctor_ty(&args_ty, &ret_ty);
        let key = format!("__anv_callback_key_{index}");
        let root = format!("__anv_callback_root_{index}");
        let handle = format!("__anv_callback_handle_{index}");
        let record_var = format!("__anv_callback_record_{index}");
        let index_var = format!("__anv_callback_index_{index}");
        let generation = format!("__anv_callback_generation_{index}");
        let arg = format!("__anv_callback_arg_{index}");
        let table_id = sig.index() + 1;
        let signature_id = sig.index() + 1;
        (
            vec![
                format!(
                    "let {record_var} = {record} {{ lambda: {lambda}, _brand: std::marker::PhantomData }};"
                ),
                format!(
                    "let {handle} = rt.heap().alloc(types.callback_record_sig{}, {record_var});",
                    sig.index()
                ),
                format!("let {root} = rt.heap().root(&{handle});"),
                format!(
                    "let ({index_var}, {generation}) = unsafe {{ &mut *callbacks.as_ptr() }}.insert_{field}({root});"
                ),
                format!(
                    "let {key} = {}::new(owner.owner_id(), owner.shutdown_generation(), std::num::NonZeroU64::new({table_id}).unwrap(), std::num::NonZeroU64::new({signature_id}).unwrap(), {index_var}, {generation});",
                    target::callback_key_ty()
                ),
                format!(
                    "let {arg} = unsafe {{ {callback_ctor}::__anvyx_new(owner.clone(), {key}, {}, {}) }};",
                    target::callback_call_thunk_symbol(sig.index()),
                    target::callback_close_thunk_symbol(sig.index())
                ),
            ],
            arg,
        )
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

    fn cast(&self, function: &RirFunction, value: &RirOperand, target: RirTypeId) -> String {
        if self.program.types[target.index()] == RirType::Int
            && let Some((enm, value)) = self.raw_enum_place(function, value, RirEnumRepr::RawInt)
        {
            return if enm.variants.is_empty() {
                Self::raw_enum_cast_match(&value, String::new())
            } else {
                format!("{value} as {}", self.ty(target))
            };
        }
        if self.program.types[target.index()] == RirType::String
            && let Some((enm, value)) = self.raw_enum_place(function, value, RirEnumRepr::RawString)
        {
            let arms = enm
                .variants
                .iter()
                .map(|variant| {
                    let Some(RirRawEnumValue::String(raw)) = &variant.raw_value else {
                        unreachable!("verified raw string enum value")
                    };
                    format!(
                        "{} => {}",
                        variant_path(enm.symbol.as_str(), variant.symbol.as_str()),
                        target::anv_string_from(&rust_string(raw))
                    )
                })
                .collect::<Vec<_>>()
                .join(", ");
            return Self::raw_enum_cast_match(&value, arms);
        }
        format!(
            "{} as {}",
            RustValues::new(self.program, function).operand(value),
            self.ty(target)
        )
    }

    fn raw_enum_cast_match(value: &str, arms: String) -> String {
        let scrutinee = format!("&{value}");
        if arms.is_empty() {
            match_expr(&scrutinee, ["_ => unreachable!()".to_string()])
        } else {
            match_expr(&scrutinee, [arms])
        }
    }

    fn raw_enum_place(
        &self,
        function: &RirFunction,
        value: &RirOperand,
        repr: RirEnumRepr,
    ) -> Option<(&RirEnum, String)> {
        let RirOperand::Place(place) = value else {
            return None;
        };
        let RirType::Enum(enum_id) = self.program.types[place.ty.index()] else {
            return None;
        };
        let enm = &self.program.enums[enum_id.index()];
        (enm.repr == repr).then(|| {
            (
                enm,
                RustPlaces::new(self.program, function).local_place(place),
            )
        })
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
        match source_kind {
            RangeCopySource::Slice => format!(
                "{}?",
                target::anv_slice_copy_range_with(
                    &source_expr,
                    target::runtime_param_name(),
                    &storage_ty,
                    &range,
                    item,
                    &body,
                )
            ),
            RangeCopySource::List => target::anv_list_from_iter(
                target::runtime_param_name(),
                &storage_ty,
                &format!(
                    "({source_expr}.to_vec({})?)[{range}].iter().map(|{item}| {body})",
                    target::runtime_param_name()
                ),
            ),
            RangeCopySource::Array => target::anv_list_from_iter(
                target::runtime_param_name(),
                &storage_ty,
                &format!("{source_expr}[{range}].iter().map(|{item}| {body})"),
            ),
        }
    }

    fn string_concat(&self, function: &RirFunction, parts: &[RirOperand]) -> String {
        let values = RustValues::new(self.program, function);
        let mut rendered = vec!["let mut out = String::new();".to_string()];
        rendered.extend(
            parts
                .iter()
                .map(|part| format!("out.push_str({});", values.string_arg(part))),
        );
        block_expr(rendered, Some(target::anv_string_from("out")))
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
        if native_return_ty_is_resource(self.program, ret) {
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
            anvyx_runtime::ExternTypeExpr::Named { .. } => self.native_named_return_expr(ret, expr),
            _ => expr.to_string(),
        }
    }

    fn native_named_return_expr(&self, ret: RirTypeId, expr: &str) -> String {
        match self.program.types[ret.index()] {
            RirType::Enum(enum_id) if self.program.enums[enum_id.index()].native_path.is_some() => {
                self.native_enum_return_expr(enum_id, expr)
            }
            _ => expr.to_string(),
        }
    }

    fn native_enum_return_expr(&self, enum_id: RirEnumId, expr: &str) -> String {
        let enm = &self.program.enums[enum_id.index()];
        let native = enm
            .native_path
            .as_ref()
            .expect("verified native enum type")
            .join("::");
        let arms = enm.variants.iter().map(|variant| {
            let generated_path = variant_path(enm.symbol.as_str(), variant.symbol.as_str());
            let native_path = variant_path(&native, variant.symbol.as_str());
            match variant.kind {
                RirVariantKind::Unit => format!("{native_path} => {generated_path}"),
                RirVariantKind::Tuple => {
                    let bindings = (0..variant.fields.len())
                        .map(|index| format!("value{index}"))
                        .collect::<Vec<_>>();
                    let fields = variant
                        .fields
                        .iter()
                        .zip(&bindings)
                        .map(|(field, binding)| self.native_rir_return_expr(field.ty, binding))
                        .collect::<Vec<_>>();
                    format!(
                        "{} => {}",
                        tuple_variant(&native_path, bindings.iter().cloned()),
                        tuple_variant(&generated_path, fields)
                    )
                }
                RirVariantKind::Struct => {
                    let bindings = variant
                        .fields
                        .iter()
                        .map(|field| field.symbol.as_str().to_string())
                        .collect::<Vec<_>>();
                    let fields = variant
                        .fields
                        .iter()
                        .zip(&bindings)
                        .map(|(field, binding)| {
                            field_init(
                                field.symbol.as_str(),
                                self.native_rir_return_expr(field.ty, binding),
                            )
                        });
                    format!(
                        "{} => {}",
                        struct_variant(&native_path, bindings.iter().cloned()),
                        struct_variant(&generated_path, fields)
                    )
                }
            }
        });
        match_expr(expr, arms)
    }

    fn native_rir_return_expr(&self, ty: RirTypeId, expr: &str) -> String {
        match self.program.types[ty.index()] {
            RirType::Option(inner) => {
                target::rust_option_map(expr, &self.native_rir_return_expr(inner, "value"))
            }
            RirType::Array { elem, .. } => {
                Self::native_array_map_expr(expr, &self.native_rir_return_expr(elem, "value"))
            }
            RirType::Tuple(tuple_id) => self.native_rir_tuple_return_expr(tuple_id, expr),
            RirType::Enum(enum_id)
                if self.program.enums[enum_id.index()].core == Some(RirCoreEnumKind::Result) =>
            {
                self.native_rir_result_return_expr(enum_id, expr)
            }
            RirType::Enum(enum_id) if self.program.enums[enum_id.index()].native_path.is_some() => {
                self.native_enum_return_expr(enum_id, expr)
            }
            _ => expr.to_string(),
        }
    }

    fn native_rir_tuple_return_expr(&self, tuple_id: RirTupleId, expr: &str) -> String {
        let tuple = &self.program.tuples[tuple_id.index()];
        if tuple.fields.is_empty() {
            return target::rust_eval_then(
                expr,
                &struct_lit(tuple.symbol.as_str(), Vec::<String>::new()),
            );
        }
        let tmp = "__anv_native_payload";
        let fields = tuple.fields.iter().enumerate().map(|(index, field)| {
            field_init(
                field.symbol.as_str(),
                self.native_rir_return_expr(field.ty, &target::rust_tuple_field(tmp, index)),
            )
        });
        format!(
            "{{ let {tmp} = {expr}; {} }}",
            struct_lit(tuple.symbol.as_str(), fields)
        )
    }

    fn native_rir_result_return_expr(&self, enum_id: RirEnumId, expr: &str) -> String {
        let enm = &self.program.enums[enum_id.index()];
        let [ok, err] = enm.variants.as_slice() else {
            unreachable!("verified result enum variants")
        };
        target::rust_result_match(
            expr,
            &format!(
                "Ok(value) => {}({})",
                variant_path(enm.symbol.as_str(), ok.symbol.as_str()),
                self.native_rir_return_expr(ok.fields[0].ty, "value")
            ),
            &format!(
                "Err(value) => {}({})",
                variant_path(enm.symbol.as_str(), err.symbol.as_str()),
                self.native_rir_return_expr(err.fields[0].ty, "value")
            ),
        )
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
    ) -> String {
        let ext = &self.program.externs[id.index()];
        let (symbol, rendered, fallible, ret_abi, param_abis) = match &ext.kind {
            RirExternKind::Native(native) => {
                let rendered = match native.abi.ctx {
                    anvyx_runtime::RustWrapperCtx::HiddenRuntime => target::native_call_args([]),
                    anvyx_runtime::RustWrapperCtx::None => vec![],
                };
                (
                    native.path.join("::"),
                    rendered,
                    native.abi.fallible,
                    &native.abi.ret,
                    native.abi.params.as_slice(),
                )
            }
        };
        let suspend_entry = self.has_retained_callbacks();
        let param_tys = ext.params.iter().map(|param| param.ty).collect::<Vec<_>>();
        let call = self.prepared_native_call_expr(
            function,
            args,
            param_abis,
            &param_tys,
            rendered,
            |rendered| {
                let call = format!("{symbol}({rendered})");
                let call = if suspend_entry {
                    format!(
                        "{{ let __anv_provider_entry = owner.__anvyx_suspend_entry_for_provider()?; let __anv_provider_result = {call}; drop(__anv_provider_entry); __anv_provider_result }}"
                    )
                } else {
                    call
                };
                if fallible { format!("{call}?") } else { call }
            },
        );
        self.native_return_call(function, ext.ret, ret_abi, call)
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
        let heap_type = format!("types.{}", dataref.heap_type_symbol());
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

    fn default_scalar_display(value: &str, ty: &RirType) -> String {
        match ty {
            RirType::Float => target::display_float(value),
            RirType::Int | RirType::Bool => format!("format!(\"{{}}\", {value})"),
            _ => unreachable!("verified scalar display type"),
        }
    }

    fn stringify_struct(
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
            super::rir::RirStringifyReqKind::Structural(_) => {
                let RirOperand::Place(place) = value else {
                    unreachable!("verified place operand")
                };
                format!(
                    "{}(rt, types, &{})",
                    self.stringify_helper(ty),
                    RustPlaces::new(self.program, function).local_place(place)
                )
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

    fn stringify_helper(&self, ty: RirTypeId) -> &str {
        self.program
            .stringify_helpers
            .iter()
            .find(|helper| helper.ty == ty)
            .expect("verified stringify helper missing")
            .symbol
            .as_str()
    }

    fn ty(&self, ty: RirTypeId) -> String {
        RustRepPolicy::new(self.program).rust_ty(ty)
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
        RirStmt::CollectionLoanScope(RirCollectionLoanScope { root, .. }) => root.uses_local(local),
        RirStmt::EnumMatch(RirEnumMatch { discr, .. }) => place_uses_local(discr, local),
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
    match value {
        RirRValue::Use(operand)
        | RirRValue::FunctionValue { value: operand, .. }
        | RirRValue::Unary { value: operand, .. }
        | RirRValue::Cast { value: operand, .. }
        | RirRValue::OptionalSome { value: operand, .. }
        | RirRValue::Stringify { value: operand, .. }
        | RirRValue::Format { value: operand, .. } => operand_uses_local(program, operand, local),
        RirRValue::Struct { fields, .. }
        | RirRValue::Tuple { fields, .. }
        | RirRValue::Array { elems: fields, .. }
        | RirRValue::List { elems: fields, .. }
        | RirRValue::EnumVariant { fields, .. } => fields
            .iter()
            .any(|operand| operand_uses_local(program, operand, local)),
        RirRValue::Map { entries, .. } => entries.iter().any(|(key, value)| {
            operand_uses_local(program, key, local) || operand_uses_local(program, value, local)
        }),
        RirRValue::DataRefAlloc { fields, .. } => fields
            .iter()
            .any(|value| operand_uses_local(program, value, local)),
        RirRValue::DataRefGet {
            object,
            projections,
            ..
        } => {
            operand_uses_local(program, object, local) || projections_use_local(projections, local)
        }
        RirRValue::CellGetCopy { cell, .. } => cell_uses_local(cell, local),
        RirRValue::ScopedPlaceCellGet { cell, .. } => scoped_cell_uses_local(program, cell, local),
        RirRValue::MutPlaceGetCopy { place, .. } => mut_place_arg_uses_local(program, place, local),
        RirRValue::Binary { lhs, rhs, .. } | RirRValue::SharedRefEq { lhs, rhs, .. } => {
            operand_uses_local(program, lhs, local) || operand_uses_local(program, rhs, local)
        }
        RirRValue::Call { callee, args, .. } => {
            call_target_uses_local(program, callee, local)
                || args
                    .iter()
                    .any(|arg| call_arg_uses_local(program, arg, local))
        }
        RirRValue::StringConcat { parts } => parts
            .iter()
            .any(|part| operand_uses_local(program, part, local)),
        RirRValue::Len { source } => place_uses_local(source, local),
        RirRValue::CollectionLen { source }
        | RirRValue::SequenceSlotAt {
            collection: source, ..
        } => source.uses_local(local),
        RirRValue::SliceView {
            source, start, end, ..
        }
        | RirRValue::RangeListCopy {
            source, start, end, ..
        } => place_uses_local(source, local) || *start == local || *end == local,
        RirRValue::MapEntryAt {
            map: source, index, ..
        }
        | RirRValue::MapValueAt {
            map: source, index, ..
        } => source.uses_local(local) || *index == local,
        RirRValue::ListPush { list, value } => {
            list.uses_local(local) || operand_uses_local(program, value, local)
        }
        RirRValue::MapGet { map, key, .. } | RirRValue::MapRemove { map, key, .. } => {
            map.uses_local(local) || operand_uses_local(program, key, local)
        }
        RirRValue::MapInsert {
            map, key, value, ..
        } => {
            map.uses_local(local)
                || operand_uses_local(program, key, local)
                || operand_uses_local(program, value, local)
        }
        RirRValue::Lambda { captures, .. } => captures.iter().any(|capture| match capture {
            RirLambdaCaptureArg::Readonly { value } => operand_uses_local(program, value, local),
            RirLambdaCaptureArg::Scoped { place } => place_uses_local(place, local),
            RirLambdaCaptureArg::StackCell { cell } | RirLambdaCaptureArg::HeapCell { cell } => {
                cell_uses_local(cell, local)
            }
            RirLambdaCaptureArg::ScopedPlaceCell { cell } => {
                scoped_cell_uses_local(program, cell, local)
            }
        }),
    }
}

fn call_target_uses_local(program: &RirProgram, callee: &RirCallTarget, local: RirLocalId) -> bool {
    match callee {
        RirCallTarget::LambdaValue { callee, .. } => operand_uses_local(program, callee, local),
        RirCallTarget::Function(_) | RirCallTarget::Extern(_) => false,
    }
}

fn call_arg_uses_local(program: &RirProgram, arg: &RirCallArg, local: RirLocalId) -> bool {
    match arg {
        RirCallArg::Value(operand) | RirCallArg::InitFieldProvided(operand) => {
            operand_uses_local(program, operand, local)
        }
        RirCallArg::SharedBorrow(place) | RirCallArg::MutBorrow(place) => {
            place_uses_local(place, local)
        }
        RirCallArg::MutPlace(place) => mut_place_arg_uses_local(program, place, local),
        RirCallArg::ScopedLambda { callee, .. } | RirCallArg::EscapingLambda { callee, .. } => {
            operand_uses_local(program, callee, local)
        }
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
        | RirStmt::DataRefSet { .. }
        | RirStmt::SequenceSlotSet { .. }
        | RirStmt::MapValueSet { .. }
        | RirStmt::If(_)
        | RirStmt::Loop(_)
        | RirStmt::CollectionLoanScope(_)
        | RirStmt::CollectionSlotScope(_)
        | RirStmt::EnumMatch(_)
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
        | RirCallArg::InitFieldProvided(RirOperand::Place(place))
        | RirCallArg::ScopedLambda {
            callee: RirOperand::Place(place),
            ..
        }
        | RirCallArg::EscapingLambda {
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
        RirCallArg::Value(RirOperand::Const(_))
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
            | RirParamSemantic::ScopedLambda
            | RirParamSemantic::EscapingLambda
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
