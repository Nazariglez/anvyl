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

    pub(super) fn sig_index(self) -> usize {
        self.sig.index()
    }

    pub(super) fn sig(self, program: &RirProgram) -> &RirLambdaSig {
        &program.lambda_sigs[self.sig_index()]
    }

    pub(super) fn args_ty(self, program: &RirProgram) -> String {
        let policy = RirRustRepPolicy::new(program);
        match self.sig(program).params.as_slice() {
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

    pub(super) fn ret_ty(self, program: &RirProgram) -> String {
        RirRustRepPolicy::new(program).callable_ret_ty(self.sig(program).ret)
    }

    pub(super) fn lambda_fallible(self, program: &RirProgram, fallible_functions: &[bool]) -> bool {
        program
            .lambdas_for_sig(self.sig)
            .any(|lambda| fallible_functions[lambda.function.index()])
    }

    pub(super) fn table_id(self) -> usize {
        self.sig.index() + 1
    }

    pub(super) fn signature_id(self) -> usize {
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
                            "if let Some((index, slot)) = self.{field}.iter_mut().enumerate().find(|(_, slot)| slot.is_free()) {{"
                        ));
                        w.indented(|w| {
                            w.line("let generation = slot.insert(handle).expect(\"callback slot open failed\");");
                            w.line("return (index, generation);");
                        });
                        w.line("}");
                        w.line(format_args!("let index = self.{field}.len();"));
                        w.line(format_args!(
                            "let mut slot = {}::default();",
                            target::callback_slot_turbofish(&handle)
                        ));
                        w.line("let generation = slot.insert(handle).expect(\"callback slot open failed\");");
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
                    "key.owner_id()",
                    "key.shutdown_generation()",
                    &format!("{inner}<'_>"),
                ) {
                    w.line(line);
                }
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
                    w.line("let (record_handle, guard) = unsafe { slot.begin_invocation(owner, key) }?;");
                    w.line(format_args!(
                        "let lambda = {};",
                        target::map_heap_access_error(
                            "inner.heap.try_with(&record_handle, |record| record.lambda.clone())"
                        )
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
                    "owner.owner_id()",
                    "owner.shutdown_generation()",
                    &format!("{inner}<'cx>"),
                ) {
                    w.line(line);
                }
                w.line("let lambda = {");
                w.indented(|w| {
                    w.line("let inner = unsafe { inner_ptr.as_mut() };");
                    w.line(format_args!(
                        "let lambda = {};",
                        target::map_heap_access_error(&format!(
                            "inner.heap.try_with_erased(handle, inner.types.{}, |record| record.lambda.clone())",
                            plan.heap_type_field()
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
                    "key.owner_id()",
                    "key.shutdown_generation()",
                    &format!("{inner}<'_>"),
                ) {
                    w.line(line);
                }
                w.line("let inner = unsafe { inner_ptr.as_mut() };");
                w.line(format_args!("let table = &mut inner.callbacks.{field};"));
                w.line("let Some(slot) = table.get_mut(key.index()) else { return Ok(false); };");
                w.line("let close = slot.close(key);");
                w.line("Ok(close.closed)");
            },
        );
        self.w.blank();
    }

    fn emit_runtime_context(w: &mut RustWriter, trace_globals: bool) {
        w.line("let (heap, types, globals, safepoint, callbacks) = {");
        w.indented(|w| {
            w.line("let inner = unsafe { inner_ptr.as_mut() };");
            w.line("let heap = std::ptr::NonNull::from(&mut inner.heap);");
            w.line("let types = std::ptr::NonNull::from(&inner.types);");
            w.line("let globals = std::ptr::NonNull::from(&inner.globals);");
            w.line("let safepoint = std::ptr::NonNull::from(&inner.safepoint);");
            w.line("let callbacks = std::ptr::NonNull::from(&mut inner.callbacks);");
            w.line("(heap, types, globals, safepoint, callbacks)");
        });
        w.line("};");
        w.line("let types = unsafe { types.as_ref() };");
        w.line("let globals = unsafe { globals.as_ref() };");
        w.line("let safepoint = unsafe { safepoint.as_ref() };");
        w.line(format_args!(
            "let mut rt = {};",
            if trace_globals {
                target::runtime_ctx_from_raw_with_trace_roots_and_safepoint(
                    "heap",
                    "globals",
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
            "&mut rt".to_string(),
            "types".to_string(),
            "globals".to_string(),
            "owner".to_string(),
            "callbacks".to_string(),
        ]
        .into_iter()
        .chain((0..sig.params.len()).map(|index| format!("args.{index}")))
        .collect()
    }

    fn key_check(plan: RetainedCallbackSigPlan) -> String {
        format!(
            "{}?;",
            target::callback_check_identity("key", plan.table_id(), plan.signature_id())
        )
    }
}
