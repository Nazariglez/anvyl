use super::{
    rep_policy::{RirRustRepPolicy, RustTracePlan},
    rir::{RirLambdaSig, RirLambdaSigId, RirProgram},
    runtime_owner::RuntimeOwnerEmit,
    syntax::comma,
    target,
    write::RustWriter,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct RetainedCallbackSigPlan {
    sig: RirLambdaSigId,
}

impl RetainedCallbackSigPlan {
    pub(super) fn new(sig: RirLambdaSigId) -> Self {
        Self { sig }
    }

    pub(super) fn sig(self, program: &RirProgram) -> &RirLambdaSig {
        &program.lambda_sigs[self.sig.index()]
    }

    pub(super) fn args_ty(self, program: &RirProgram) -> String {
        let policy = RirRustRepPolicy::new(program);
        match self.sig(program).params.as_slice() {
            [] => "()".to_string(),
            [param] => format!(
                "({},)",
                policy.callable_param_ty(param.ty, param.mode, param.escape)
            ),
            params => format!(
                "({})",
                comma(params.iter().map(|param| policy.callable_param_ty(
                    param.ty,
                    param.mode,
                    param.escape
                )))
            ),
        }
    }

    pub(super) fn ret_ty(self, program: &RirProgram) -> String {
        RirRustRepPolicy::new(program).callable_ret_ty(self.sig(program).ret)
    }

    pub(super) fn lambda_fallible(self, program: &RirProgram, fallible_functions: &[bool]) -> bool {
        program
            .lambdas_for_sig(self.sig)
            .any(|lambda| fallible_functions[lambda.function.index()])
    }

    pub(super) fn callback_id(self) -> usize {
        self.sig.index() + 1
    }

    pub(super) fn table_field(self) -> String {
        target::callback_table_field(self.sig.index())
    }

    pub(super) fn record_symbol(self) -> String {
        target::callback_record_symbol(self.sig.index())
    }

    pub(super) fn call_thunk_symbol(self) -> String {
        target::callback_call_thunk_symbol(self.sig.index())
    }

    pub(super) fn close_thunk_symbol(self) -> String {
        target::callback_close_thunk_symbol(self.sig.index())
    }

    pub(super) fn anv_call_thunk_symbol(self) -> String {
        target::callback_anv_call_thunk_symbol(self.sig.index())
    }

    pub(super) fn heap_type_field(self) -> String {
        target::callback_record_heap_type_field(self.sig.index())
    }
}

pub(super) struct RetainedCallbackEmitter<'a, 'w> {
    program: &'a RirProgram,
    trace_plan: &'a RustTracePlan,
    fallible_functions: &'a [bool],
    record_sigs: &'a [RirLambdaSigId],
    provider_sigs: &'a [RirLambdaSigId],
    heap_sigs: &'a [RirLambdaSigId],
    w: &'w mut RustWriter,
}

impl<'a, 'w> RetainedCallbackEmitter<'a, 'w> {
    pub(super) fn new(
        program: &'a RirProgram,
        trace_plan: &'a RustTracePlan,
        fallible_functions: &'a [bool],
        record_sigs: &'a [RirLambdaSigId],
        provider_sigs: &'a [RirLambdaSigId],
        heap_sigs: &'a [RirLambdaSigId],
        w: &'w mut RustWriter,
    ) -> Self {
        Self {
            program,
            trace_plan,
            fallible_functions,
            record_sigs,
            provider_sigs,
            heap_sigs,
            w,
        }
    }

    pub(super) fn emit_registry(&mut self) {
        if self.record_sigs.is_empty() {
            return;
        }
        let policy = RirRustRepPolicy::new(self.program);
        for sig in self.record_sigs {
            let plan = RetainedCallbackSigPlan::new(*sig);
            let record = plan.record_symbol();
            let lambda = policy.lambda_sig_storage_ty(*sig);
            self.w.block(format_args!("struct {record}<'cx>"), |w| {
                w.line(format_args!("lambda: {lambda},"));
                w.line("_brand: std::marker::PhantomData<&'cx ()>,");
            });
            self.w.blank();
            if self.trace_plan.needs_lambda_sig_trace(*sig) {
                self.emit_record_trace_impl(&record);
            }
        }
        self.emit_registry_struct();
        self.emit_registry_impl();
    }

    pub(super) fn emit_thunks(&mut self) {
        for sig in self.provider_sigs {
            let plan = RetainedCallbackSigPlan::new(*sig);
            self.emit_call_thunk(plan);
            self.emit_close_thunk(plan);
        }
        for sig in self.heap_sigs {
            self.emit_anv_call_thunk(RetainedCallbackSigPlan::new(*sig));
        }
    }

    fn emit_record_trace_impl(&mut self, record: &str) {
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

    fn emit_registry_struct(&mut self) {
        let registry = target::generated_callback_registry_symbol();
        self.w.line("#[derive(Default)]");
        self.w.block(format_args!("struct {registry}<'cx>"), |w| {
            for sig in self.provider_sigs {
                let plan = RetainedCallbackSigPlan::new(*sig);
                let field = plan.table_field();
                let record = plan.record_symbol();
                let handle = target::handle_ty(&format!("{record}<'cx>"));
                w.line(format_args!(
                    "{field}: Vec<{}>,",
                    target::callback_slot_ty(&handle)
                ));
            }
            w.line("_brand: std::marker::PhantomData<&'cx ()>,");
        });
        self.w.blank();
    }

    fn emit_registry_impl(&mut self) {
        let registry = target::generated_callback_registry_symbol();
        self.w.block(format_args!("impl<'cx> {registry}<'cx>"), |w| {
            for sig in self.provider_sigs {
                let plan = RetainedCallbackSigPlan::new(*sig);
                let field = plan.table_field();
                let record = plan.record_symbol();
                let handle = target::handle_ty(&format!("{record}<'cx>"));
                w.block(
                    format_args!(
                        "fn insert_{field}(&mut self, handle: {handle}) -> (usize, std::num::NonZeroU64)"
                    ),
                    |w| {
                        w.line(format_args!(
                            "if let Some((index, slot)) = self.{field}.iter_mut().enumerate().find(|(_, slot)| {}) {{",
                            target::callback_slot_is_free("slot")
                        ));
                        w.indented(|w| {
                            w.line(format_args!(
                                "let generation = {}.expect(\"callback slot open failed\");",
                                target::callback_slot_insert("slot", "handle")
                            ));
                            w.line("return (index, generation);");
                        });
                        w.line("}");
                        w.line(format_args!("let index = self.{field}.len();"));
                        w.line(format_args!(
                            "let mut slot = {}::default();",
                            target::callback_slot_turbofish(&handle)
                        ));
                        w.line(format_args!(
                            "let generation = {}.expect(\"callback slot open failed\");",
                            target::callback_slot_insert("slot", "handle")
                        ));
                        w.line(format_args!("self.{field}.push(slot);"));
                        w.line("(index, generation)");
                    },
                );
            }
        });
        self.w.blank();
    }

    fn emit_call_thunk(&mut self, plan: RetainedCallbackSigPlan) {
        let args_ty = plan.args_ty(self.program);
        let result_ret = target::result_ty(&plan.ret_ty(self.program));
        let call_thunk = plan.call_thunk_symbol();
        let key_ty = target::callback_key_ty();
        let owner_ty = target::runtime_owner_handle_ty();
        let inner = target::generated_runtime_inner_symbol();
        let field = plan.table_field();
        let key_check = Self::key_check(plan);
        let trace_globals = self.trace_globals();
        let result = self.lambda_call_result(plan);
        self.w.block(
            format_args!(
                "unsafe fn {call_thunk}(owner: &{owner_ty}, key: {key_ty}, args: {args_ty}) -> {result_ret}"
            ),
            |w| {
                w.line(&key_check);
                for line in RuntimeOwnerEmit::callback_entry_lines(
                    "owner_entry",
                    "inner_ptr",
                    &target::callback_key_owner_id("key"),
                    &target::callback_key_shutdown_generation("key"),
                    &format!("{inner}<'_>"),
                ) {
                    w.line(line);
                }
                w.line("let (lambda, guard) = {");
                w.indented(|w| {
                    w.line("let inner = unsafe { inner_ptr.as_mut() };");
                    w.line(format_args!("let table = &mut inner.callbacks.{field};"));
                    w.line(format_args!(
                        "let Some(slot) = table.get_mut({}) else {{",
                        target::callback_key_index("key")
                    ));
                    w.indented(|w| {
                        w.line(format_args!(
                            "return Err({}::new(\"callback slot is closed\"));",
                            target::runtime_error_ty()
                        ));
                    });
                    w.line("};");
                    w.line(format_args!(
                        "let (record_handle, guard) = {}?;",
                        target::callback_slot_begin_invocation("slot", "owner", "key")
                    ));
                    w.line(format_args!(
                        "let lambda = {};",
                        target::map_heap_access_error(&target::callback_heap_try_with(
                            "inner.heap",
                            "&record_handle",
                            "record",
                            "record.lambda.clone()",
                        ))
                    ));
                    w.line("(lambda, guard)");
                });
                w.line("};");
                Self::emit_runtime_context(w, trace_globals);
                w.line(format_args!("let __anv_callback_result = {result};"));
                w.line("guard.finish()?;");
                w.line("__anv_callback_result");
            },
        );
        self.w.blank();
    }

    fn emit_anv_call_thunk(&mut self, plan: RetainedCallbackSigPlan) {
        let args_ty = plan.args_ty(self.program);
        let result_ret = target::result_ty(&plan.ret_ty(self.program));
        let anv_call_thunk = plan.anv_call_thunk_symbol();
        let owner_ty = target::runtime_owner_handle_ty();
        let inner = target::generated_runtime_inner_symbol();
        let trace_globals = self.trace_globals();
        let result = self.lambda_call_result(plan);
        self.w.block(
            format_args!(
                "unsafe fn {anv_call_thunk}<'cx>(owner: &{owner_ty}, handle: &{}, args: {args_ty}) -> {result_ret}",
                target::erased_handle_ty()
            ),
            |w| {
                for line in RuntimeOwnerEmit::callback_entry_lines(
                    "owner_entry",
                    "inner_ptr",
                    &target::owner_id("owner"),
                    &target::owner_shutdown_generation("owner"),
                    &format!("{inner}<'cx>"),
                ) {
                    w.line(line);
                }
                w.line("let lambda = {");
                w.indented(|w| {
                    w.line("let inner = unsafe { inner_ptr.as_mut() };");
                    w.line(format_args!(
                        "let lambda = {};",
                        target::map_heap_access_error(&target::callback_heap_try_with_erased(
                            "inner.heap",
                            "handle",
                            &format!("inner.statics.{}", plan.heap_type_field()),
                            "record",
                            "record.lambda.clone()",
                        ))
                    ));
                    w.line("lambda");
                });
                w.line("};");
                Self::emit_runtime_context(w, trace_globals);
                w.line(&result);
            },
        );
        self.w.blank();
    }

    fn emit_close_thunk(&mut self, plan: RetainedCallbackSigPlan) {
        let close_thunk = plan.close_thunk_symbol();
        let key_ty = target::callback_key_ty();
        let owner_ty = target::runtime_owner_handle_ty();
        let inner = target::generated_runtime_inner_symbol();
        let field = plan.table_field();
        let key_check = Self::key_check(plan);
        self.w.block(
            format_args!(
                "unsafe fn {close_thunk}(owner: &{owner_ty}, key: {key_ty}) -> {}",
                target::result_ty("bool")
            ),
            |w| {
                w.line(&key_check);
                for line in RuntimeOwnerEmit::callback_entry_lines(
                    "owner_entry",
                    "inner_ptr",
                    &target::callback_key_owner_id("key"),
                    &target::callback_key_shutdown_generation("key"),
                    &format!("{inner}<'_>"),
                ) {
                    w.line(line);
                }
                w.line("let inner = unsafe { inner_ptr.as_mut() };");
                w.line(format_args!("let table = &mut inner.callbacks.{field};"));
                w.line(format_args!(
                    "let Some(slot) = table.get_mut({}) else {{ return Ok(false); }};",
                    target::callback_key_index("key")
                ));
                w.line(format_args!(
                    "let close = {};",
                    target::callback_slot_close("slot", "key")
                ));
                w.line("Ok(close.closed)");
            },
        );
        self.w.blank();
    }

    fn emit_runtime_context(w: &mut RustWriter, trace_globals: bool) {
        w.line("let (heap, statics, globals, safepoint, callbacks) = {");
        w.indented(|w| {
            w.line("let inner = unsafe { inner_ptr.as_mut() };");
            w.line("let heap = std::ptr::NonNull::from(&mut inner.heap);");
            w.line("let statics = std::ptr::NonNull::from(&inner.statics);");
            w.line("let globals = std::ptr::NonNull::from(&inner.globals);");
            w.line("let safepoint = std::ptr::NonNull::from(&inner.safepoint);");
            w.line("let callbacks = std::ptr::NonNull::from(&mut inner.callbacks);");
            w.line("(heap, statics, globals, safepoint, callbacks)");
        });
        w.line("};");
        w.line(format_args!(
            "let {} = unsafe {{ statics.as_ref() }};",
            target::statics_param_name()
        ));
        w.line(format_args!(
            "let {} = unsafe {{ globals.as_ref() }};",
            target::globals_param_name()
        ));
        w.line("let safepoint = unsafe { safepoint.as_ref() };");
        w.line(format_args!(
            "let mut {} = {};",
            target::runtime_param_name(),
            if trace_globals {
                target::runtime_ctx_from_raw_with_trace_roots_and_safepoint(
                    "heap",
                    target::globals_param_name(),
                    "safepoint",
                )
            } else {
                target::runtime_ctx_from_raw_with_safepoint("heap", "safepoint")
            }
        ));
    }

    fn trace_globals(&self) -> bool {
        self.program
            .globals
            .iter()
            .any(|global| RirRustRepPolicy::new(self.program).type_owns_heap_edges(global.ty))
    }

    fn lambda_call_result(&self, plan: RetainedCallbackSigPlan) -> String {
        let lambda_call = format!("lambda.call({})", comma(self.lambda_call_args(plan)));
        if plan.lambda_fallible(self.program, self.fallible_functions) {
            lambda_call
        } else {
            format!("Ok({lambda_call})")
        }
    }

    fn lambda_call_args(&self, plan: RetainedCallbackSigPlan) -> Vec<String> {
        let sig = plan.sig(self.program);
        [
            format!("&mut {}", target::runtime_param_name()),
            target::statics_param_name().to_string(),
            target::globals_param_name().to_string(),
            "owner".to_string(),
            "callbacks".to_string(),
        ]
        .into_iter()
        .chain((0..sig.params.len()).map(|index| format!("args.{index}")))
        .collect()
    }

    fn key_check(plan: RetainedCallbackSigPlan) -> String {
        let id = plan.callback_id();
        let check = target::callback_check_identity("key", id, id);
        format!("{check}?;")
    }
}
