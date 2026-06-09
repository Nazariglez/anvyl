use super::{
    analysis,
    place::RustPlaces,
    rep_policy::{RustRepPolicy, RustTracePlan},
    rir::{
        RirCallArg, RirCallTarget, RirEnum, RirEnumMatch, RirEnumRepr, RirExternKind,
        RirFormatAlign, RirFormatKind, RirFormatSign, RirFormatSpec, RirFunction, RirIf,
        RirLambdaCapture, RirLambdaCaptureArg, RirLambdaCaptureKind, RirLambdaEnvId,
        RirLambdaEnvLayout, RirLambdaId, RirLambdaSig, RirLambdaStorage, RirLoop, RirLoopId,
        RirOperand, RirOptionMatch, RirParamAbi, RirParamSemantic, RirPlace, RirProgram, RirRValue,
        RirRawEnumValue, RirScopedPlaceCellRef, RirStmt, RirStructuredBlock, RirTerm, RirType,
        RirTypeId, RirVariant, RirVariantId, RirVariantKind, VerifiedRirProgram,
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
        trace_plan: RustTracePlan::build(program),
        fallible_functions: analysis::fallible_functions(program),
        w: RustWriter::default(),
    };
    cx.emit_program();
    RustSource::new(cx.w.finish())
}

struct EmitCx<'a> {
    program: &'a RirProgram,
    trace_plan: RustTracePlan,
    fallible_functions: Vec<bool>,
    w: RustWriter,
}

impl EmitCx<'_> {
    fn emit_program(&mut self) {
        if !self.program.stringify_helpers.is_empty() {
            self.w.line("use std::fmt::Write;");
            self.w.blank();
        }
        self.emit_ctx();
        for dataref in &self.program.datarefs {
            self.emit_dataref(dataref);
        }
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
        for function in &self.program.functions {
            self.emit_function(function);
        }
        if let Some(entry) = self.program.entry {
            self.emit_main(entry);
        }
    }

    fn emit_ctx(&mut self) {
        let ctx = self.program.ctx.symbol.as_str();
        let policy = RustRepPolicy::new(self.program);
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
        heap_types.extend(self.program.lambda_envs.iter().map(|env| {
            let register = target::heap_register(policy.lambda_env_storage_tracked(env));
            (
                lambda_env_heap_type_symbol(env.id),
                policy.lambda_env_storage_ty(env),
                register,
            )
        }));

        self.w.block("struct AnvTypes<'cx>", |w| {
            for (heap_type, storage, _) in &heap_types {
                w.line(format_args!(
                    "{heap_type}: {},",
                    target::heap_type_ty(storage)
                ));
            }
            w.line("_brand: std::marker::PhantomData<&'cx ()>,");
        });
        self.w.blank();
        self.w.block("impl<'cx> AnvTypes<'cx>", |w| {
            w.block(
                format_args!("fn register(heap: &mut {}) -> Self", target::heap_ty()),
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
        self.w.block(format_args!("struct {ctx}<'cx, 'rt>"), |w| {
            w.line(format_args!("rt: {},", target::runtime_ctx_ty()));
            w.line("_types: AnvTypes<'cx>,");
        });
        self.w.blank();
        self.w
            .block(format_args!("impl<'cx, 'rt> {ctx}<'cx, 'rt>"), |w| {
                w.block(
                    format_args!(
                        "fn new(rt: {}, types: AnvTypes<'cx>) -> Self",
                        target::runtime_ctx_ty()
                    ),
                    |w| w.line(format_args!("Self {{ rt, _types: types }}")),
                );
                w.blank();
                w.block(
                    format_args!("fn heap(&mut self) -> &mut {}", target::heap_ty()),
                    |w| w.line("self.rt.heap()"),
                );
                w.blank();
                w.block(
                    format_args!("fn runtime(&mut self) -> &mut {}", target::runtime_ctx_ty()),
                    |w| w.line("&mut self.rt"),
                );
            });
        self.w.blank();
    }

    fn emit_main(&mut self, entry: super::rir::RirFunctionId) {
        let symbol = self.program.functions[entry.index()].symbol.as_str();
        let fallible = self.fallible_functions[entry.index()];
        let ret = if fallible {
            format!(" -> {}", target::result_ty("()"))
        } else {
            String::new()
        };
        let ctx = self.program.ctx.symbol.as_str();
        self.w.block(format_args!("fn main(){ret}"), |w| {
            w.line(format_args!("{}(|heap| {{", target::heap_scope()));
            w.indented(|w| {
                w.line("let types = AnvTypes::register(heap);");
                w.line(format_args!(
                    "let rt = {};",
                    target::runtime_ctx_new("heap")
                ));
                w.line(format_args!("let mut ctx = {ctx}::new(rt, types);"));
                w.line(format_args!(
                    "let _ = {symbol}(&mut ctx){};",
                    if fallible { "?" } else { "" }
                ));
                if fallible {
                    w.line("Ok(())");
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
                        policy.rust_ty(field.ty)
                    ));
                }
            },
        );
        self.w.blank();
    }

    fn emit_struct(&mut self, strukt: &super::rir::RirStruct) {
        if let Some(path) = &strukt.native_path {
            self.w.line(format_args!(
                "type {} = {};",
                strukt.symbol.as_str(),
                path.join("::")
            ));
            self.w.blank();
            return;
        }
        self.emit_record_struct(
            strukt.symbol.as_str(),
            &strukt.fields,
            self.trace_plan.needs_struct_trace(strukt.id),
            RustRepPolicy::new(self.program).struct_cx_dependent(strukt),
        );
    }

    fn emit_tuple(&mut self, tuple: &super::rir::RirTuple) {
        self.emit_record_struct(
            tuple.symbol.as_str(),
            &tuple.fields,
            self.trace_plan.needs_tuple_trace(tuple.id),
            RustRepPolicy::new(self.program).tuple_cx_dependent(tuple),
        );
    }

    fn emit_record_struct(
        &mut self,
        symbol: &str,
        fields: &[super::rir::RirField],
        trace: bool,
        cx_dependent: bool,
    ) {
        if trace {
            self.w.line(target::trace_derive(&["Clone"]));
            self.w.line(target::trace_crate_attr(cx_dependent));
        } else {
            self.w.line("#[derive(Clone)]");
        }
        let lifetime = if cx_dependent { "<'cx>" } else { "" };
        let fields = fields
            .iter()
            .map(|field| (field.symbol.as_str(), self.ty(field.ty)))
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
        let cx_dependent = RustRepPolicy::new(self.program).enum_cx_dependent(enm);
        let needs_trace = self.trace_plan.needs_enum_trace(enm.id);
        if needs_trace {
            if enm.repr == RirEnumRepr::RawInt && !enm.variants.is_empty() {
                self.w.line(target::trace_derive(&["Clone", "Copy"]));
                self.w.line("#[repr(i64)]");
            } else {
                self.w.line(target::trace_derive(&["Clone"]));
            }
            self.w.line(target::trace_crate_attr(cx_dependent));
        } else if enm.repr == RirEnumRepr::RawInt && !enm.variants.is_empty() {
            self.w.line("#[derive(Clone, Copy)]");
            self.w.line("#[repr(i64)]");
        } else {
            self.w.line("#[derive(Clone)]");
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
                    let fields = comma(variant.fields.iter().map(|field| self.ty(field.ty)));
                    format!("{}({fields}),", variant.symbol.as_str())
                }
                RirVariantKind::Struct => {
                    let fields = variant
                        .fields
                        .iter()
                        .map(|field| field_init(field.symbol.as_str(), self.ty(field.ty)));
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
        let ctx = self.stringify_helper_ctx_name(strukt);
        let header = format!(
            "fn {}<'cx, 'rt>({ctx}: &mut {}, value: &{}) -> {}",
            helper.symbol.as_str(),
            self.ctx_ty(),
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
                    RirType::Int | RirType::Float | RirType::Bool => {
                        w.line(format_args!(
                            "let _ = write!(out, \"{{}}\", value.{field});"
                        ));
                    }
                    RirType::String => {
                        w.line(format_args!("out.push_str(value.{field}.as_str());"));
                    }
                    RirType::Struct(_) => {
                        let nested = nested.as_deref().expect("struct field has helper");
                        w.line(format_args!(
                            "out.push_str({nested}(ctx, &value.{field}).as_str());"
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
        let params = std::iter::once(format!("ctx: &mut {}", self.ctx_ty()))
            .chain(sig.params.iter().enumerate().map(|(index, param)| {
                format!("arg_{index}: {}", policy.param_ty(param.ty, param.abi))
            }))
            .collect::<Vec<_>>();
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
        let fallible_functions = &self.fallible_functions;
        let lifetime = match (
            policy.lambda_sig_needs_lifetime(sig.id),
            policy.lambda_sig_needs_ctx_lifetime(sig.id),
        ) {
            (true, true) => "<'env, 'cx>",
            (true, false) => "<'env>",
            (false, true) => "<'cx>",
            (false, false) => "",
        };

        let trace = self.trace_plan.needs_lambda_sig_trace(sig.id);
        if trace {
            let derives = if policy.lambda_sig_copyable(sig.id) {
                ["Clone", "Copy"].as_slice()
            } else {
                ["Clone"].as_slice()
            };
            self.w.line(target::trace_derive(derives));
            self.w.line(target::trace_crate_attr(
                policy.lambda_sig_needs_ctx_lifetime(sig.id),
            ));
        } else if policy.lambda_sig_copyable(sig.id) {
            self.w.line("#[derive(Clone, Copy)]");
        } else if policy.lambda_sig_has_heap_env(sig.id) {
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
        let body_call = |function: &RirFunction, capture_args: Vec<String>| {
            let args = std::iter::once("ctx".to_string())
                .chain(capture_args)
                .chain((0..arity).map(|index| format!("arg_{index}")));
            let call = format!("{}({})", function.symbol.as_str(), comma(args));
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
                                            w.line(format_args!(
                                                "let c{index} = ctx.heap().with(env, |env| {});",
                                                values.value_from_ref(capture.ty, &source)
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
        self.w.blank();
    }

    fn lambda_sig_fallible(&self, sig: &RirLambdaSig) -> bool {
        self.program
            .lambdas_for_sig(sig.id)
            .any(|lambda| self.fallible_functions[lambda.function.index()])
    }

    fn lambda_sig_ret_ty(&self, sig: &RirLambdaSig, fallible: bool) -> String {
        let ret = self.ty(sig.ret);
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
        let ctx = if analysis::function_uses_ctx(self.program, function) {
            "ctx"
        } else {
            "_ctx"
        };
        let mut params = vec![format!("{ctx}: &mut {}", self.ctx_ty())];
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
                RustRepPolicy::new(self.program).param_ty(param.ty, param.abi)
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
                        | RirStmt::EnumMatch(_)
                        | RirStmt::OptionMatch(_)
                )
            });
            if predeclare {
                this.emit_local_declarations(function);
            }
            this.emit_scoped_place_cells(function);
            for stmt in &block.stmts {
                this.emit_stmt_mode(function, stmt, predeclare);
            }
            this.emit_term(function, &block.term);
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
        let ret = self.ty(function.ret.ty);
        if !self.fallible_functions[function.id.index()] {
            return ret;
        }
        if ret == "()" {
            target::result_ty("()")
        } else {
            target::result_ty(&ret)
        }
    }

    fn ctx_ty(&self) -> String {
        format!("{}<'cx, 'rt>", self.program.ctx.symbol.as_str())
    }

    fn local_needs_mut_binding(&self, ty: RirTypeId) -> bool {
        match self.program.types[ty.index()] {
            RirType::Lambda(sig) => self.lambda_sig_needs_mut_self(sig),
            _ => false,
        }
    }

    fn lambda_sig_needs_mut_self(&self, sig: super::rir::RirLambdaSigId) -> bool {
        self.program.lambdas_for_sig(sig).any(|lambda| {
            lambda
                .captures
                .iter()
                .any(|capture| capture.semantic == RirParamSemantic::MutBorrow)
        })
    }

    fn emit_local_declarations(&mut self, function: &RirFunction) {
        for cell in self
            .program
            .cells
            .iter()
            .filter(|cell| cell.owner == function.id)
        {
            self.w.line(format_args!(
                "let {}: {};",
                cell.symbol.as_str(),
                target::stack_lambda_cell_ty(&self.ty(cell.payload_ty))
            ));
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
            let source = function.locals[cell.source_local.index()].symbol.as_str();
            self.w.line(format_args!(
                "let {} = anvyx_runtime::ScopedMutPlaceCell::new({source});",
                cell.symbol.as_str()
            ));
        }
    }

    fn stringify_helper_ctx_name(&self, strukt: &super::rir::RirStruct) -> &'static str {
        if analysis::stringify_helper_uses_ctx(self.program, strukt) {
            "ctx"
        } else {
            "_ctx"
        }
    }

    fn emit_stmt_mode(&mut self, function: &RirFunction, stmt: &RirStmt, predeclared: bool) {
        match stmt {
            RirStmt::Init { local, value } => {
                let local_data = &function.locals[local.index()];
                if predeclared {
                    self.w.line(format_args!(
                        "{} = {};",
                        local_data.symbol.as_str(),
                        self.rvalue(function, value)
                    ));
                } else {
                    let mutability =
                        if local_data.mutable || self.local_needs_mut_binding(local_data.ty) {
                            "mut "
                        } else {
                            ""
                        };
                    self.w.line(format_args!(
                        "let {mutability}{}: {} = {};",
                        local_data.symbol.as_str(),
                        self.ty(local_data.ty),
                        self.rvalue(function, value)
                    ));
                }
            }
            RirStmt::Assign { dst, value } => {
                let places = RustPlaces::new(self.program, function);
                if places.mut_place_root_param(dst) {
                    self.w.line(format_args!(
                        "{}.set({})?;",
                        places.local_place(dst),
                        self.rvalue(function, value)
                    ));
                } else {
                    self.w.line(format_args!(
                        "{} = {};",
                        places.local_place(dst),
                        self.rvalue(function, value)
                    ));
                }
            }
            RirStmt::CellInit { cell, value } => {
                let payload_ty = self.cell_payload_ty(function, *cell);
                if predeclared {
                    self.w.line(format_args!(
                        "{} = {}::new({});",
                        self.cell_ref(function, *cell),
                        target::stack_lambda_cell_ctor(&payload_ty),
                        self.rvalue(function, value)
                    ));
                } else {
                    self.w.line(format_args!(
                        "let {}: {} = {}::new({});",
                        self.cell_ref(function, *cell),
                        target::stack_lambda_cell_ty(&payload_ty),
                        target::stack_lambda_cell_ctor(&payload_ty),
                        self.rvalue(function, value)
                    ));
                }
            }
            RirStmt::CellSet { cell, value } => self.w.line(format_args!(
                "{}.set({})?;",
                self.cell_ref(function, *cell),
                self.rvalue(function, value)
            )),
            RirStmt::ScopedPlaceCellSet { cell, value } => self.w.line(format_args!(
                "{}.set({})?;",
                self.scoped_place_cell_ref(function, *cell),
                self.rvalue(function, value)
            )),
            RirStmt::DataRefSet {
                object,
                dataref,
                projections,
                value,
            } => self.w.line(format_args!(
                "{};",
                self.dataref_set(function, object, *dataref, projections, value)
            )),
            RirStmt::Eval(value) => {
                self.w
                    .line(format_args!("{};", self.rvalue(function, value)));
            }
            RirStmt::If(branch) => self.emit_if(function, branch, predeclared),
            RirStmt::Loop(loop_) => self.emit_loop(function, loop_, predeclared),
            RirStmt::EnumMatch(match_) => self.emit_match(function, match_, predeclared),
            RirStmt::OptionMatch(match_) => {
                self.emit_option_match(function, match_, predeclared);
            }
        }
    }

    fn emit_loop(&mut self, function: &RirFunction, loop_: &RirLoop, predeclared: bool) {
        self.w
            .line(format_args!("{}: loop {{", loop_label(loop_.id)));
        self.indented(|this| this.emit_structured_block(function, &loop_.body, predeclared));
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
                self.variant_pattern(enm, variant)
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

    fn emit_option_match(
        &mut self,
        function: &RirFunction,
        match_: &RirOptionMatch,
        predeclared: bool,
    ) {
        let RirType::Option(inner) = self.program.types[match_.discr.ty.index()] else {
            unreachable!("verified option match")
        };
        let borrow = if match_.payload_ref { "&mut " } else { "&" };
        if match_.payload_escapes {
            let payload = match_.payload.expect("escaping option payload local");
            let local = &function.locals[payload.index()];
            self.w.line(format_args!(
                "let Some({}) = {borrow}{} else {{",
                local.symbol.as_str(),
                RustPlaces::new(self.program, function).local_place(&match_.discr)
            ));
            self.indented(|this| {
                this.emit_structured_block(function, &match_.none_block, predeclared);
            });
            self.w.line("};");
            self.emit_structured_block(function, &match_.some_block, predeclared);
            return;
        }
        self.w.line(format_args!(
            "match {borrow}{} {{",
            RustPlaces::new(self.program, function).local_place(&match_.discr)
        ));
        let payload_ref = match match_.payload {
            Some(payload) if match_.payload_ref => {
                function.locals[payload.index()].symbol.as_str().to_string()
            }
            _ => self.fresh_option_payload_ref(function),
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

    fn fresh_option_payload_ref(&self, function: &RirFunction) -> String {
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
        for stmt in &block.stmts {
            self.emit_stmt_mode(function, stmt, predeclared);
        }
        self.emit_term(function, &block.term);
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

    fn variant_pattern(&self, enm: &RirEnum, variant: &RirVariant) -> String {
        let path = variant_path(enm.symbol.as_str(), variant.symbol.as_str());
        match variant.kind {
            RirVariantKind::Unit => unit_variant_pattern(&path),
            RirVariantKind::Tuple => tuple_variant_pattern(&path),
            RirVariantKind::Struct => struct_variant_pattern(&path),
        }
    }

    fn rvalue(&self, function: &RirFunction, value: &RirRValue) -> String {
        let values = RustValues::new(self.program, function);
        let places = RustPlaces::new(self.program, function);
        match value {
            RirRValue::Use(operand) => values.value_operand(operand),
            RirRValue::Struct { ty, fields } => self.struct_literal(function, *ty, fields),
            RirRValue::Tuple { ty, fields } => self.tuple_literal(function, *ty, fields),
            RirRValue::DataRefAlloc { ty, fields } => self.dataref_alloc(function, *ty, fields),
            RirRValue::DataRefGet {
                object,
                dataref,
                projections,
                ty,
            } => self.dataref_get(function, object, *dataref, projections, *ty),
            RirRValue::CellGetCopy { cell, .. } => {
                format!("{}.get_copy()?", self.cell_ref(function, *cell))
            }
            RirRValue::ScopedPlaceCellGet { cell, ty } => {
                values.scoped_place_cell_value(*cell, *ty)
            }
            RirRValue::Array { elems, .. } => {
                format!(
                    "[{}]",
                    comma(elems.iter().map(|elem| values.value_operand(elem)))
                )
            }
            RirRValue::List { elems, .. } => {
                let elems = comma(elems.iter().map(|elem| values.value_operand(elem)));
                target::anv_list_from_elems(&elems)
            }
            RirRValue::Map { entries, .. } => {
                let entries = comma(entries.iter().map(|(key, value)| {
                    format!(
                        "({}, {})",
                        values.value_operand(key),
                        values.value_operand(value)
                    )
                }));
                target::anv_map_from_entries(&entries)
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
                    let mut rendered = vec!["ctx".to_string()];
                    rendered.extend(args.iter().map(|arg| values.call_arg(arg)));
                    let call = format!("{symbol}({})", comma(rendered));
                    if self.fallible_functions[id.index()] {
                        format!("{call}?")
                    } else {
                        call
                    }
                }
                RirCallTarget::Extern(id) => self.extern_call(function, *id, args),
                RirCallTarget::LambdaValue { callee, sig } => {
                    let rendered = std::iter::once("ctx".to_string())
                        .chain(args.iter().map(|arg| values.call_arg(arg)));
                    let call =
                        format!("{}.call({})", values.value_operand(callee), comma(rendered));
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
                        target::anv_string_format("\"{}\"", &values.operand(value))
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
            } => target::anv_string_format(
                &rust_string(&format_fragment(rust_format_spec(*spec))),
                &values.format_arg(value, *source_ty),
            ),
            RirRValue::Len { source } if places.mut_place_root_param(source) => {
                self.mut_place_region(function, source, "access", "Ok(value.len() as i64)".into())
            }
            RirRValue::Len { source } => format!("{}.len() as i64", places.local_place(source)),
            RirRValue::ListPush { list, value } if places.mut_place_root_param(list) => self
                .mut_place_region(
                    function,
                    list,
                    "mutate",
                    format!("{{ value.push({}); Ok(()) }}", values.value_operand(value)),
                ),
            RirRValue::ListPush { list, value } => format!(
                "{}.push({})",
                places.local_place(list),
                values.value_operand(value)
            ),
            RirRValue::SliceView {
                source,
                start,
                end,
                inclusive,
                ..
            } => {
                let source = places.local_place(source);
                let start = function.locals[start.index()].symbol.as_str();
                let end = function.locals[end.index()].symbol.as_str();
                let range =
                    target::checked_range(start, end, *inclusive, &format!("{source}.len()"));
                format!("&{source}[{range}]")
            }
            RirRValue::ListSlice {
                source,
                start,
                end,
                inclusive,
                ..
            } => self.list_slice(function, source, *start, *end, *inclusive),
            RirRValue::MapGet { map, key, ty } => {
                let RirType::Option(value_ty) = self.program.types[ty.index()] else {
                    unreachable!("verified map get result")
                };
                let key = values.operand(key);
                let body = format!(
                    "value.get(&{key}).map(|value| {})",
                    values.value_from_ref(value_ty, "value")
                );
                if places.mut_place_root_param(map) {
                    self.mut_place_region(function, map, "access", format!("Ok({body})"))
                } else {
                    format!(
                        "{}.get(&{key}).map(|value| {})",
                        places.local_place(map),
                        values.value_from_ref(value_ty, "value")
                    )
                }
            }
            RirRValue::MapInsert { map, key, value } if places.mut_place_root_param(map) => self
                .mut_place_region(
                    function,
                    map,
                    "mutate",
                    format!(
                        "{{ value.insert({}, {}); Ok(()) }}",
                        values.value_operand(key),
                        values.value_operand(value)
                    ),
                ),
            RirRValue::MapInsert { map, key, value } => block_expr(
                [format!(
                    "{}.insert({}, {});",
                    places.local_place(map),
                    values.value_operand(key),
                    values.value_operand(value)
                )],
                None,
            ),
            RirRValue::MapRemove { map, key, .. } if places.mut_place_root_param(map) => self
                .mut_place_region(
                    function,
                    map,
                    "mutate",
                    format!("Ok(value.remove(&{}))", values.operand(key)),
                ),
            RirRValue::MapRemove { map, key, .. } => {
                format!(
                    "{}.remove(&{})",
                    places.local_place(map),
                    values.operand(key)
                )
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
                        let heap_type =
                            format!("ctx._types.{}", lambda_env_heap_type_symbol(env.id));
                        let storage = format!("{} {{ {} }}", env.symbol.as_str(), comma(fields));
                        let alloc = target::ctx_heap_alloc("ctx", "heap_type", &storage);
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

    fn mut_place_region(
        &self,
        function: &RirFunction,
        place: &RirPlace,
        op: &str,
        body: String,
    ) -> String {
        format!(
            "{}.{}(|value| {})?",
            RustPlaces::new(self.program, function).local_place(place),
            op,
            body
        )
    }

    fn cell_ref(&self, function: &RirFunction, cell: super::rir::RirCellRef) -> String {
        match cell {
            super::rir::RirCellRef::Owner(cell) => {
                self.program.cells[cell.index()].symbol.as_str().to_string()
            }
            super::rir::RirCellRef::Capture { local, .. } => {
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

    fn cell_payload_ty(&self, function: &RirFunction, cell: super::rir::RirCellRef) -> String {
        let ty = match cell {
            super::rir::RirCellRef::Owner(cell) => self.program.cells[cell.index()].payload_ty,
            super::rir::RirCellRef::Capture { local, .. } => function.locals[local.index()].ty,
        };
        self.ty(ty)
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
                super::rir::RirCellRef::Owner(_) => format!("&{}", self.cell_ref(function, *cell)),
                super::rir::RirCellRef::Capture { .. } => self.cell_ref(function, *cell),
            },
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
                self.raw_enum_cast_match(&value, String::new())
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
            return self.raw_enum_cast_match(&value, arms);
        }
        format!(
            "{} as {}",
            RustValues::new(self.program, function).operand(value),
            self.ty(target)
        )
    }

    fn raw_enum_cast_match(&self, value: &str, arms: String) -> String {
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

    fn list_slice(
        &self,
        function: &RirFunction,
        source: &RirPlace,
        start: super::rir::RirLocalId,
        end: super::rir::RirLocalId,
        inclusive: bool,
    ) -> String {
        let source_expr = RustPlaces::new(self.program, function).local_place(source);
        let start = function.locals[start.index()].symbol.as_str();
        let end = function.locals[end.index()].symbol.as_str();
        let range = target::checked_range(start, end, inclusive, &format!("{source_expr}.len()"));
        let RirType::List(elem) = self.program.types[source.ty.index()] else {
            unreachable!("verified list slice source")
        };
        let values = RustValues::new(self.program, function);
        block_expr(
            [
                format!("let mut out = {};", target::anv_list_default()),
                format!(
                    "for item in &{source_expr}[{range}] {{ out.push({}); }}",
                    values.value_from_ref(elem, "item")
                ),
            ],
            Some("out".to_string()),
        )
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

    fn extern_call(
        &self,
        function: &RirFunction,
        id: super::rir::RirExternId,
        args: &[RirCallArg],
    ) -> String {
        let ext = &self.program.externs[id.index()];
        let values = RustValues::new(self.program, function);
        let (symbol, mut rendered, fallible, ret_abi) = match &ext.kind {
            RirExternKind::Native(native) => (
                native.path.join("::"),
                vec![target::ctx_runtime("ctx")],
                native.abi.fallible,
                &native.abi.ret,
            ),
        };
        rendered.extend(args.iter().map(|arg| values.call_arg(arg)));
        let call = format!("{symbol}({})", comma(rendered));
        let call = if fallible { format!("{call}?") } else { call };
        values.native_return_call(ext.ret, ret_abi, call)
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
        let heap_type = format!("ctx._types.{}", dataref.heap_type_symbol());
        let storage = format!("{} {{ {} }}", dataref.storage_symbol(), fields);
        format!(
            "{{ let heap_type = {heap_type}; {} }}",
            target::ctx_heap_alloc("ctx", "heap_type", &storage)
        )
    }

    fn dataref_get(
        &self,
        function: &RirFunction,
        object: &RirOperand,
        dataref: super::rir::RirDataRefId,
        projections: &[super::rir::RirProjection],
        ty: RirTypeId,
    ) -> String {
        let values = RustValues::new(self.program, function);
        let object = values.operand_ref(object);
        let path = RustPlaces::new(self.program, function).storage_path(dataref, projections);
        target::ctx_heap_with(
            "ctx",
            &object,
            "storage",
            &values.value_from_place(ty, &path),
        )
    }

    fn dataref_set(
        &self,
        function: &RirFunction,
        object: &RirOperand,
        dataref: super::rir::RirDataRefId,
        projections: &[super::rir::RirProjection],
        value: &RirOperand,
    ) -> String {
        let values = RustValues::new(self.program, function);
        let object = values.operand_ref(object);
        let path = RustPlaces::new(self.program, function).storage_path(dataref, projections);
        target::ctx_heap_with_mut(
            "ctx",
            &object,
            "storage",
            &format!("{} = {};", path, values.value_operand(value)),
        )
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
                    "{}(ctx, &{})",
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
                let call = format!("{symbol}(ctx, {arg})");
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

fn loop_label(id: RirLoopId) -> String {
    format!("'loop_{}", id.index())
}

fn lambda_variant(id: RirLambdaId) -> String {
    format!("L{}", id.index())
}

fn lambda_env_heap_type_symbol(id: RirLambdaEnvId) -> String {
    format!("lambda_env{}", id.index())
}

fn lambda_capture_call_arg(index: usize, capture: &RirLambdaCapture) -> String {
    match capture.kind {
        RirLambdaCaptureKind::Param => match capture.semantic {
            RirParamSemantic::Value | RirParamSemantic::SharedBorrow => format!("*c{index}"),
            RirParamSemantic::MutBorrow => format!("&mut **c{index}"),
            RirParamSemantic::MutPlace
            | RirParamSemantic::StackCell
            | RirParamSemantic::ScopedPlaceCell => unreachable!("verified non-param capture kind"),
        },
        RirLambdaCaptureKind::StackCell { .. } => format!("*c{index}"),
        RirLambdaCaptureKind::ScopedPlaceCell { .. } => format!("*c{index}"),
    }
}
