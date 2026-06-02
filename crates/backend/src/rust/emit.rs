use std::fmt::Write;

use anvyx_frontend::ast::{BinaryOp, UnaryOp};

use super::{
    rep_policy::{RustBorrowView, RustRepPolicy},
    rir::{
        RirCallArg, RirCallTarget, RirConst, RirConstValue, RirEnum, RirEnumMatch, RirEnumRepr,
        RirExternKind, RirFormatAlign, RirFormatKind, RirFormatSign, RirFormatSpec, RirFunction,
        RirIf, RirLoop, RirLoopId, RirOperand, RirParamAbi, RirParamSemantic, RirPlace, RirProgram,
        RirRValue, RirRawEnumValue, RirStmt, RirStructuredBlock, RirTerm, RirType, RirTypeId,
        RirVariant, RirVariantId, RirVariantKind, VerifiedRirProgram,
    },
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
        fallible_functions: fallible_functions(program),
        out: String::new(),
    };
    cx.emit_program();
    RustSource::new(cx.out)
}

struct EmitCx<'a> {
    program: &'a RirProgram,
    fallible_functions: Vec<bool>,
    out: String,
}

fn fallible_functions(program: &RirProgram) -> Vec<bool> {
    let mut fallible = vec![false; program.functions.len()];
    loop {
        let mut changed = false;
        for function in &program.functions {
            let is_fallible = block_calls_fallible(program, &fallible, &function.body);
            let slot = &mut fallible[function.id.index()];
            if is_fallible && !*slot {
                *slot = true;
                changed = true;
            }
        }
        if !changed {
            return fallible;
        }
    }
}

fn block_calls_fallible(
    program: &RirProgram,
    fallible: &[bool],
    block: &RirStructuredBlock,
) -> bool {
    block
        .stmts
        .iter()
        .any(|stmt| stmt_calls_fallible(program, fallible, stmt))
}

fn stmt_calls_fallible(program: &RirProgram, fallible: &[bool], stmt: &RirStmt) -> bool {
    match stmt {
        RirStmt::Init { value, .. } | RirStmt::Assign { value, .. } | RirStmt::Eval(value) => {
            rvalue_calls_fallible(program, fallible, value)
        }
        RirStmt::DataRefSet { .. } => false,
        RirStmt::If(branch) => {
            block_calls_fallible(program, fallible, &branch.then_block)
                || branch
                    .else_block
                    .as_ref()
                    .is_some_and(|block| block_calls_fallible(program, fallible, block))
        }
        RirStmt::Loop(loop_) => block_calls_fallible(program, fallible, &loop_.body),
        RirStmt::EnumMatch(match_) => {
            match_
                .arms
                .iter()
                .any(|arm| block_calls_fallible(program, fallible, &arm.block))
                || match_
                    .else_block
                    .as_ref()
                    .is_some_and(|block| block_calls_fallible(program, fallible, block))
        }
    }
}

fn rvalue_calls_fallible(program: &RirProgram, fallible: &[bool], value: &RirRValue) -> bool {
    match value {
        RirRValue::Call { callee, .. } => match callee {
            RirCallTarget::Function(id) => fallible[id.index()],
            RirCallTarget::Extern(id) => match &program.externs[id.index()].kind {
                RirExternKind::Native(native) => native.abi.fallible,
            },
        },
        _ => false,
    }
}

impl EmitCx<'_> {
    fn emit_program(&mut self) {
        if !self.program.stringify_helpers.is_empty() {
            self.line("use std::fmt::Write;");
            self.line("");
        }
        self.emit_ctx();
        for dataref in &self.program.datarefs {
            self.emit_dataref(dataref);
        }
        for strukt in &self.program.structs {
            self.emit_struct(strukt);
        }
        for enm in &self.program.enums {
            self.emit_enum(enm);
        }
        for helper in &self.program.stringify_helpers {
            self.emit_stringify_helper(helper);
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
        self.line("struct AnvTypes<'cx> {");
        for dataref in &self.program.datarefs {
            self.line(&format!(
                "    {}: anvyx_runtime::HeapType<'cx, {}>,",
                dataref.heap_type_symbol(),
                self.dataref_storage_ty(dataref)
            ));
        }
        self.line("    _brand: std::marker::PhantomData<&'cx ()>,");
        self.line("}");
        self.line("");
        self.line("impl<'cx> AnvTypes<'cx> {");
        self.line("    fn register(heap: &mut anvyx_runtime::Heap<'cx>) -> Self {");
        self.line("        Self {");
        for dataref in &self.program.datarefs {
            let register = if dataref.cycle_capable || self.dataref_cx_dependent(dataref) {
                "register_tracked"
            } else {
                "register_untracked"
            };
            self.line(&format!(
                "            {}: heap.{register}::<{}>(),",
                dataref.heap_type_symbol(),
                self.dataref_storage_ty(dataref)
            ));
        }
        self.line("            _brand: std::marker::PhantomData,");
        self.line("        }");
        self.line("    }");
        self.line("}");
        self.line("");
        self.line(&format!("struct {ctx}<'cx, 'rt> {{"));
        self.line("    rt: anvyx_runtime::Ctx<'cx, 'rt>,");
        self.line("    _types: AnvTypes<'cx>,");
        self.line("}");
        self.line("");
        self.line(&format!("impl<'cx, 'rt> {ctx}<'cx, 'rt> {{"));
        self.line("    fn new(rt: anvyx_runtime::Ctx<'cx, 'rt>, types: AnvTypes<'cx>) -> Self {");
        self.line("        Self { rt, _types: types }");
        self.line("    }");
        self.line("");
        self.line("    fn heap(&mut self) -> &mut anvyx_runtime::Heap<'cx> {");
        self.line("        self.rt.heap()");
        self.line("    }");
        self.line("");
        self.line("    fn runtime(&mut self) -> &mut anvyx_runtime::Ctx<'cx, 'rt> {");
        self.line("        &mut self.rt");
        self.line("    }");
        self.line("}");
        self.line("");
    }

    fn emit_main(&mut self, entry: super::rir::RirFunctionId) {
        let symbol = self.program.functions[entry.index()].symbol.as_str();
        let fallible = self.fallible_functions[entry.index()];
        let ret = if fallible {
            " -> Result<(), anvyx_runtime::RuntimeError>"
        } else {
            ""
        };
        self.line(&format!("fn main(){ret} {{"));
        self.line("    anvyx_runtime::Heap::scope(|heap| {");
        self.line("        let types = AnvTypes::register(heap);");
        self.line("        let rt = anvyx_runtime::Ctx::new(heap);");
        self.line(&format!(
            "        let mut ctx = {}::new(rt, types);",
            self.program.ctx.symbol.as_str()
        ));
        self.line(&format!(
            "        let _ = {symbol}(&mut ctx){};",
            if fallible { "?" } else { "" }
        ));
        if fallible {
            self.line("        Ok(())");
            self.line("    })");
        } else {
            self.line("    });");
        }
        self.line("}");
    }

    fn emit_dataref(&mut self, dataref: &super::rir::RirDataRef) {
        let storage = dataref.storage_symbol();
        let cx_dependent = self.dataref_cx_dependent(dataref);
        let storage_lifetime = if cx_dependent { "<'cx>" } else { "" };
        if cx_dependent {
            self.line("#[derive(anvyx_runtime::Trace)]");
            self.line("#[trace(crate = anvyx_runtime, ctx = 'cx)]");
        }
        self.line(&format!("struct {storage}{storage_lifetime} {{"));
        for field in &dataref.fields {
            self.line(&format!(
                "    {}: {},",
                field.symbol.as_str(),
                self.ty(field.ty)
            ));
        }
        self.line("}");
        self.line("");
        if dataref.cycle_capable && !cx_dependent {
            self.line(&format!(
                "unsafe impl<'cx> anvyx_runtime::Trace<'cx> for {storage} {{"
            ));
            self.line("    fn trace<D: anvyx_runtime::TraceDriver<'cx>>(");
            self.line("        &self,");
            self.line("        _visitor: &mut anvyx_runtime::Visitor<'cx, '_, D>,");
            self.line("    ) {");
            self.line("    }");
            self.line("}");
            self.line("");
        }
        self.line(&format!(
            "type {}<'cx> = anvyx_runtime::Handle<'cx, {}>;",
            dataref.symbol.as_str(),
            self.dataref_storage_ty(dataref)
        ));
        self.line("");
    }

    fn dataref_storage_ty(&self, dataref: &super::rir::RirDataRef) -> String {
        let storage = dataref.storage_symbol();
        if self.dataref_cx_dependent(dataref) {
            format!("{storage}<'cx>")
        } else {
            storage
        }
    }

    fn dataref_cx_dependent(&self, dataref: &super::rir::RirDataRef) -> bool {
        let policy = RustRepPolicy::new(self.program);
        dataref
            .fields
            .iter()
            .any(|field| policy.type_cx_dependent(field.ty))
    }

    fn emit_struct(&mut self, strukt: &super::rir::RirStruct) {
        if let Some(path) = &strukt.native_path {
            self.line(&format!(
                "type {} = {};",
                strukt.symbol.as_str(),
                path.join("::")
            ));
            self.line("");
            return;
        }
        let cx_dependent = self.struct_cx_dependent(strukt);
        if cx_dependent {
            self.line("#[derive(Clone, anvyx_runtime::Trace)]");
            self.line("#[trace(crate = anvyx_runtime, ctx = 'cx)]");
        } else {
            self.line("#[derive(Clone)]");
        }
        let lifetime = if cx_dependent { "<'cx>" } else { "" };
        self.line(&format!("struct {}{lifetime} {{", strukt.symbol.as_str()));
        for field in &strukt.fields {
            self.line(&format!(
                "    {}: {},",
                field.symbol.as_str(),
                self.ty(field.ty)
            ));
        }
        self.line("}");
        self.line("");
    }

    fn emit_enum(&mut self, enm: &RirEnum) {
        let cx_dependent = self.enum_cx_dependent(enm);
        if cx_dependent {
            self.line("#[derive(Clone, anvyx_runtime::Trace)]");
            self.line("#[trace(crate = anvyx_runtime, ctx = 'cx)]");
        } else if enm.repr == RirEnumRepr::RawInt && !enm.variants.is_empty() {
            self.line("#[derive(Clone, Copy)]");
            self.line("#[repr(i64)]");
        } else {
            self.line("#[derive(Clone)]");
        }
        let lifetime = if cx_dependent { "<'cx>" } else { "" };
        self.line(&format!("enum {}{lifetime} {{", enm.symbol.as_str()));
        for variant in &enm.variants {
            match variant.kind {
                RirVariantKind::Unit => {
                    let raw = match &variant.raw_value {
                        Some(RirRawEnumValue::Int(value)) => format!(" = {value}"),
                        _ => String::new(),
                    };
                    self.line(&format!("    {}{raw},", variant.symbol.as_str()));
                }
                RirVariantKind::Tuple => {
                    let fields = variant
                        .fields
                        .iter()
                        .map(|field| self.ty(field.ty))
                        .collect::<Vec<_>>()
                        .join(", ");
                    self.line(&format!("    {}({}),", variant.symbol.as_str(), fields));
                }
                RirVariantKind::Struct => {
                    let fields = variant
                        .fields
                        .iter()
                        .map(|field| format!("{}: {}", field.symbol.as_str(), self.ty(field.ty)))
                        .collect::<Vec<_>>()
                        .join(", ");
                    self.line(&format!(
                        "    {} {{ {} }},",
                        variant.symbol.as_str(),
                        fields
                    ));
                }
            }
        }
        self.line("}");
        self.line("");
    }

    fn struct_cx_dependent(&self, strukt: &super::rir::RirStruct) -> bool {
        let policy = RustRepPolicy::new(self.program);
        strukt
            .fields
            .iter()
            .any(|field| policy.type_cx_dependent(field.ty))
    }

    fn enum_cx_dependent(&self, enm: &RirEnum) -> bool {
        let policy = RustRepPolicy::new(self.program);
        enm.variants.iter().any(|variant| {
            variant
                .fields
                .iter()
                .any(|field| policy.type_cx_dependent(field.ty))
        })
    }

    fn emit_stringify_helper(&mut self, helper: &super::rir::RirStringifyHelper) {
        let RirType::Struct(struct_id) = self.program.types[helper.ty.index()] else {
            unreachable!("verified stringify helper target")
        };
        let strukt = &self.program.structs[struct_id.index()];
        let ctx = self.stringify_helper_ctx_name(strukt);
        self.line(&format!(
            "fn {}<'cx, 'rt>({ctx}: &mut {}, value: &{}) -> anvyx_runtime::AnvString {{",
            helper.symbol.as_str(),
            self.ctx_ty(),
            self.ty(helper.ty)
        ));
        self.line("    let mut out = String::new();");
        self.line(&format!(
            "    out.push_str({});",
            rust_string(strukt.display.as_str())
        ));
        self.line("    out.push('(');");
        for (index, field) in strukt.fields.iter().enumerate() {
            if index > 0 {
                self.line("    out.push_str(\", \");");
            }
            self.line(&format!(
                "    out.push_str({});",
                rust_string(&format!("{}: ", field.symbol.as_str()))
            ));
            match self.program.types[field.ty.index()] {
                RirType::Int | RirType::Float | RirType::Bool => {
                    self.line(&format!(
                        "    let _ = write!(out, \"{{}}\", value.{});",
                        field.symbol.as_str()
                    ));
                }
                RirType::String => {
                    self.line(&format!(
                        "    out.push_str(value.{}.as_str());",
                        field.symbol.as_str()
                    ));
                }
                RirType::Struct(_) => {
                    let nested = self.stringify_helper(field.ty);
                    self.line(&format!(
                        "    out.push_str({}(ctx, &value.{}).as_str());",
                        nested,
                        field.symbol.as_str()
                    ));
                }
                RirType::Void
                | RirType::DataRef(_)
                | RirType::Enum(_)
                | RirType::Array { .. }
                | RirType::List(_)
                | RirType::Map { .. }
                | RirType::Option(_)
                | RirType::Slice(_) => unreachable!("verified stringify helper field"),
            }
        }
        self.line("    out.push(')');");
        self.line("    anvyx_runtime::AnvString::from(out)");
        self.line("}");
        self.line("");
    }

    fn emit_function(&mut self, function: &RirFunction) {
        let ctx = if self.function_uses_ctx(function) {
            "ctx"
        } else {
            "_ctx"
        };
        let mut params = vec![format!("{ctx}: &mut {}", self.ctx_ty())];
        params.extend(function.params.iter().map(|param| {
            let local = &function.locals[param.local.index()];
            format!(
                "{}: {}",
                local.symbol.as_str(),
                self.param_ty(param.ty, param.abi)
            )
        }));
        let ret = self.function_ret_ty(function);
        if ret == "()" {
            self.line(&format!(
                "fn {}<'cx, 'rt>({}) {{",
                function.symbol.as_str(),
                params.join(", ")
            ));
        } else {
            self.line(&format!(
                "fn {}<'cx, 'rt>({}) -> {ret} {{",
                function.symbol.as_str(),
                params.join(", ")
            ));
        }
        let block = &function.body;
        let predeclare = block.stmts.iter().any(|stmt| {
            matches!(
                stmt,
                RirStmt::If(_) | RirStmt::Loop(_) | RirStmt::EnumMatch(_)
            )
        });
        if predeclare {
            self.emit_local_declarations(function);
        }
        for stmt in &block.stmts {
            self.emit_stmt_mode(function, stmt, "    ", predeclare);
        }
        self.emit_term(function, &block.term, "    ");
        self.line("}");
        self.line("");
    }

    fn function_ret_ty(&self, function: &RirFunction) -> String {
        let ret = self.ty(function.ret.ty);
        if !self.fallible_functions[function.id.index()] {
            return ret;
        }
        if ret == "()" {
            "Result<(), anvyx_runtime::RuntimeError>".to_string()
        } else {
            format!("Result<{ret}, anvyx_runtime::RuntimeError>")
        }
    }

    fn ctx_ty(&self) -> String {
        format!("{}<'cx, 'rt>", self.program.ctx.symbol.as_str())
    }

    fn emit_local_declarations(&mut self, function: &RirFunction) {
        for local in &function.locals {
            if function.params.iter().any(|param| param.local == local.id) {
                continue;
            }
            self.line(&format!(
                "    let mut {}: {};",
                local.symbol.as_str(),
                self.ty(local.ty)
            ));
        }
    }

    fn function_uses_ctx(&self, function: &RirFunction) -> bool {
        self.block_uses_ctx(&function.body)
    }

    fn stmt_uses_ctx(&self, stmt: &RirStmt) -> bool {
        match stmt {
            RirStmt::Init { value, .. } | RirStmt::Assign { value, .. } | RirStmt::Eval(value) => {
                self.rvalue_uses_ctx(value)
            }
            RirStmt::DataRefSet { .. } => true,
            RirStmt::If(branch) => {
                self.block_uses_ctx(&branch.then_block)
                    || branch
                        .else_block
                        .as_ref()
                        .is_some_and(|block| self.block_uses_ctx(block))
            }
            RirStmt::Loop(loop_) => self.block_uses_ctx(&loop_.body),
            RirStmt::EnumMatch(match_) => {
                match_
                    .arms
                    .iter()
                    .any(|arm| self.block_uses_ctx(&arm.block))
                    || match_
                        .else_block
                        .as_ref()
                        .is_some_and(|block| self.block_uses_ctx(block))
            }
        }
    }

    fn block_uses_ctx(&self, block: &RirStructuredBlock) -> bool {
        block.stmts.iter().any(|stmt| self.stmt_uses_ctx(stmt))
    }

    fn rvalue_uses_ctx(&self, value: &RirRValue) -> bool {
        match value {
            RirRValue::Call { .. } => true,
            RirRValue::Stringify { source_ty, .. } => {
                matches!(self.program.types[source_ty.index()], RirType::Struct(_))
            }
            RirRValue::Use(_)
            | RirRValue::Unary { .. }
            | RirRValue::Binary { .. }
            | RirRValue::SharedRefEq { .. }
            | RirRValue::Cast { .. }
            | RirRValue::OptionalSome { .. }
            | RirRValue::StringConcat { .. }
            | RirRValue::Format { .. }
            | RirRValue::Len { .. }
            | RirRValue::ListPush { .. }
            | RirRValue::SliceView { .. }
            | RirRValue::Array { .. }
            | RirRValue::List { .. }
            | RirRValue::Map { .. }
            | RirRValue::MapGet { .. }
            | RirRValue::MapInsert { .. }
            | RirRValue::MapRemove { .. }
            | RirRValue::ListSlice { .. }
            | RirRValue::Struct { .. }
            | RirRValue::EnumVariant { .. } => false,
            RirRValue::DataRefAlloc { .. } | RirRValue::DataRefGet { .. } => true,
        }
    }

    fn stringify_helper_ctx_name(&self, strukt: &super::rir::RirStruct) -> &'static str {
        if strukt
            .fields
            .iter()
            .any(|field| matches!(self.program.types[field.ty.index()], RirType::Struct(_)))
        {
            "ctx"
        } else {
            "_ctx"
        }
    }

    fn emit_stmt_mode(
        &mut self,
        function: &RirFunction,
        stmt: &RirStmt,
        indent: &str,
        predeclared: bool,
    ) {
        match stmt {
            RirStmt::Init { local, value } => {
                let local_data = &function.locals[local.index()];
                if predeclared {
                    self.line(&format!(
                        "{indent}{} = {};",
                        local_data.symbol.as_str(),
                        self.rvalue(function, value)
                    ));
                } else {
                    let mutability = if local_data.mutable { "mut " } else { "" };
                    self.line(&format!(
                        "{indent}let {mutability}{}: {} = {};",
                        local_data.symbol.as_str(),
                        self.ty(local_data.ty),
                        self.rvalue(function, value)
                    ));
                }
            }
            RirStmt::Assign { dst, value } => self.line(&format!(
                "{indent}{} = {};",
                self.place(function, dst),
                self.rvalue(function, value)
            )),
            RirStmt::DataRefSet {
                object,
                dataref,
                projections,
                value,
            } => self.line(&format!(
                "{indent}{};",
                self.dataref_set(function, object, *dataref, projections, value)
            )),
            RirStmt::Eval(value) => {
                self.line(&format!("{indent}{};", self.rvalue(function, value)));
            }
            RirStmt::If(branch) => self.emit_if(function, branch, indent, predeclared),
            RirStmt::Loop(loop_) => self.emit_loop(function, loop_, indent, predeclared),
            RirStmt::EnumMatch(match_) => self.emit_match(function, match_, indent, predeclared),
        }
    }

    fn emit_loop(
        &mut self,
        function: &RirFunction,
        loop_: &RirLoop,
        indent: &str,
        predeclared: bool,
    ) {
        self.line(&format!("{indent}{}: loop {{", loop_label(loop_.id)));
        self.emit_structured_block(function, &loop_.body, &format!("{indent}    "), predeclared);
        self.line(&format!("{indent}}}"));
    }

    fn emit_if(&mut self, function: &RirFunction, branch: &RirIf, indent: &str, predeclared: bool) {
        self.line(&format!(
            "{indent}if {} {{",
            self.operand(function, &branch.cond)
        ));
        self.emit_structured_block(
            function,
            &branch.then_block,
            &format!("{indent}    "),
            predeclared,
        );
        if let Some(else_block) = &branch.else_block {
            self.line(&format!("{indent}}} else {{"));
            self.emit_structured_block(function, else_block, &format!("{indent}    "), predeclared);
        }
        self.line(&format!("{indent}}}"));
    }

    fn emit_match(
        &mut self,
        function: &RirFunction,
        match_: &RirEnumMatch,
        indent: &str,
        predeclared: bool,
    ) {
        let RirType::Enum(enum_id) = self.program.types[match_.discr.ty.index()] else {
            unreachable!("verified enum match")
        };
        let enm = &self.program.enums[enum_id.index()];
        self.line(&format!(
            "{indent}match &{} {{",
            self.place(function, &match_.discr)
        ));
        for arm in &match_.arms {
            let variant = &enm.variants[arm.variant.index()];
            let pattern = self.variant_pattern(enm, variant);
            self.line(&format!("{indent}    {pattern} => {{"));
            self.emit_structured_block(
                function,
                &arm.block,
                &format!("{indent}        "),
                predeclared,
            );
            self.line(&format!("{indent}    }}"));
        }
        if let Some(else_block) = &match_.else_block {
            self.line(&format!("{indent}    _ => {{"));
            self.emit_structured_block(
                function,
                else_block,
                &format!("{indent}        "),
                predeclared,
            );
            self.line(&format!("{indent}    }}"));
        }
        self.line(&format!("{indent}}}"));
    }

    fn emit_structured_block(
        &mut self,
        function: &RirFunction,
        block: &RirStructuredBlock,
        indent: &str,
        predeclared: bool,
    ) {
        for stmt in &block.stmts {
            self.emit_stmt_mode(function, stmt, indent, predeclared);
        }
        self.emit_term(function, &block.term, indent);
    }

    fn emit_term(&mut self, function: &RirFunction, term: &RirTerm, indent: &str) {
        match term {
            RirTerm::None => {}
            RirTerm::Return(None) => {
                if self.fallible_functions[function.id.index()] {
                    self.line(&format!("{indent}return Ok(());"));
                } else {
                    self.line(&format!("{indent}return;"));
                }
            }
            RirTerm::Return(Some(operand)) => {
                let value = self.value_operand(function, operand);
                if self.fallible_functions[function.id.index()] {
                    self.line(&format!("{indent}return Ok({value});"));
                } else {
                    self.line(&format!("{indent}return {value};"));
                }
            }
            RirTerm::Break(id) => self.line(&format!("{indent}break {};", loop_label(*id))),
            RirTerm::Continue(id) => self.line(&format!("{indent}continue {};", loop_label(*id))),
            RirTerm::Unreachable => self.line(&format!("{indent}unreachable!();")),
        }
    }

    fn variant_pattern(&self, enm: &RirEnum, variant: &RirVariant) -> String {
        match variant.kind {
            RirVariantKind::Unit => format!("{}::{}", enm.symbol.as_str(), variant.symbol.as_str()),
            RirVariantKind::Tuple => {
                format!("{}::{}(..)", enm.symbol.as_str(), variant.symbol.as_str())
            }
            RirVariantKind::Struct => format!(
                "{}::{} {{ .. }}",
                enm.symbol.as_str(),
                variant.symbol.as_str()
            ),
        }
    }

    fn rvalue(&self, function: &RirFunction, value: &RirRValue) -> String {
        match value {
            RirRValue::Use(operand) => self.value_operand(function, operand),
            RirRValue::Struct { ty, fields } => self.struct_literal(function, *ty, fields),
            RirRValue::DataRefAlloc { ty, fields } => self.dataref_alloc(function, *ty, fields),
            RirRValue::DataRefGet {
                object,
                dataref,
                projections,
                ty,
            } => self.dataref_get(function, object, *dataref, projections, *ty),
            RirRValue::Array { elems, .. } => format!(
                "[{}]",
                elems
                    .iter()
                    .map(|elem| self.value_operand(function, elem))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            RirRValue::List { elems, .. } => format!(
                "anvyx_runtime::AnvList::from_elems([{}])",
                elems
                    .iter()
                    .map(|elem| self.value_operand(function, elem))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            RirRValue::Map { entries, .. } => format!(
                "anvyx_runtime::AnvMap::from_entries([{}])",
                entries
                    .iter()
                    .map(|(key, value)| format!(
                        "({}, {})",
                        self.value_operand(function, key),
                        self.value_operand(function, value)
                    ))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            RirRValue::EnumVariant {
                ty,
                variant,
                fields,
            } => self.enum_literal(function, *ty, *variant, fields),
            RirRValue::Unary { op, value, .. } => {
                format!("{}{}", unary_op(*op), self.operand(function, value))
            }
            RirRValue::Binary { op, lhs, rhs, .. } => format!(
                "{} {} {}",
                self.operand(function, lhs),
                binary_op(*op),
                self.operand(function, rhs)
            ),
            RirRValue::SharedRefEq { lhs, rhs, negated } => {
                let eq = format!(
                    "{}.ptr_eq(&{})",
                    self.operand(function, lhs),
                    self.operand(function, rhs)
                );
                if *negated { format!("!{eq}") } else { eq }
            }
            RirRValue::Cast { value, target } => self.cast(function, value, *target),
            RirRValue::OptionalSome { value, .. } => {
                format!("Some({})", self.value_operand(function, value))
            }
            RirRValue::Call { callee, args, .. } => match callee {
                RirCallTarget::Function(id) => {
                    let symbol = self.program.functions[id.index()].symbol.as_str();
                    let mut rendered = vec!["ctx".to_string()];
                    rendered.extend(args.iter().map(|arg| self.call_arg(function, arg)));
                    let call = format!("{symbol}({})", rendered.join(", "));
                    if self.fallible_functions[id.index()] {
                        format!("{call}?")
                    } else {
                        call
                    }
                }
                RirCallTarget::Extern(id) => self.extern_call(function, *id, args),
            },
            RirRValue::Stringify { value, source_ty } => {
                match self.program.types[source_ty.index()] {
                    RirType::String => self.value_operand(function, value),
                    RirType::Int | RirType::Float | RirType::Bool => format!(
                        "anvyx_runtime::AnvString::from(format!(\"{{}}\", {}))",
                        self.operand(function, value)
                    ),
                    RirType::Struct(_) => self.stringify_struct(function, value, *source_ty),
                    RirType::Void
                    | RirType::DataRef(_)
                    | RirType::Enum(_)
                    | RirType::Array { .. }
                    | RirType::List(_)
                    | RirType::Map { .. }
                    | RirType::Option(_)
                    | RirType::Slice(_) => {
                        unreachable!("verified structural stringify target gap")
                    }
                }
            }
            RirRValue::StringConcat { parts } => self.string_concat(function, parts),
            RirRValue::Format {
                value,
                source_ty,
                spec,
            } => format!(
                "anvyx_runtime::AnvString::from(format!({}, {}))",
                rust_string(&format_fragment(*spec)),
                self.format_operand(function, value, *source_ty)
            ),
            RirRValue::Len { source } => format!("{}.len() as i64", self.place(function, source)),
            RirRValue::ListPush { list, value } => format!(
                "{}.push({})",
                self.place(function, list),
                self.value_operand(function, value)
            ),
            RirRValue::SliceView {
                source,
                start,
                end,
                inclusive,
                ..
            } => {
                let source = self.place(function, source);
                let start = function.locals[start.index()].symbol.as_str();
                let end = function.locals[end.index()].symbol.as_str();
                let range = checked_range(start, end, *inclusive, &format!("{source}.len()"));
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
                format!(
                    "{}.get(&{}).map(|value| {})",
                    self.place(function, map),
                    self.operand(function, key),
                    self.value_from_ref(value_ty, "value")
                )
            }
            RirRValue::MapInsert { map, key, value } => format!(
                "{{ {}.insert({}, {}); }}",
                self.place(function, map),
                self.value_operand(function, key),
                self.value_operand(function, value)
            ),
            RirRValue::MapRemove { map, key, .. } => format!(
                "{}.remove(&{})",
                self.place(function, map),
                self.operand(function, key)
            ),
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
                        "{}::{} => anvyx_runtime::AnvString::from({})",
                        enm.symbol.as_str(),
                        variant.symbol.as_str(),
                        rust_string(raw)
                    )
                })
                .collect::<Vec<_>>()
                .join(", ");
            return self.raw_enum_cast_match(&value, arms);
        }
        format!("{} as {}", self.operand(function, value), self.ty(target))
    }

    fn raw_enum_cast_match(&self, value: &str, arms: String) -> String {
        if arms.is_empty() {
            format!("match &{value} {{ _ => unreachable!() }}")
        } else {
            format!("match &{value} {{ {arms} }}")
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
        (enm.repr == repr).then(|| (enm, self.place(function, place)))
    }

    fn list_slice(
        &self,
        function: &RirFunction,
        source: &RirPlace,
        start: super::rir::RirLocalId,
        end: super::rir::RirLocalId,
        inclusive: bool,
    ) -> String {
        let source_expr = self.place(function, source);
        let start = function.locals[start.index()].symbol.as_str();
        let end = function.locals[end.index()].symbol.as_str();
        let range = checked_range(start, end, inclusive, &format!("{source_expr}.len()"));
        let RirType::List(elem) = self.program.types[source.ty.index()] else {
            unreachable!("verified list slice source")
        };
        format!(
            "{{ let mut out = anvyx_runtime::AnvList::default(); for item in &{source_expr}[{range}] {{ out.push({}); }} out }}",
            self.value_from_ref(elem, "item")
        )
    }

    fn string_concat(&self, function: &RirFunction, parts: &[RirOperand]) -> String {
        let mut rendered = vec!["{".to_string(), "let mut out = String::new();".to_string()];
        for part in parts {
            match part {
                RirOperand::Const(id) => {
                    let konst = &self.program.consts[id.index()];
                    match &konst.value {
                        RirConstValue::String(value) => {
                            rendered.push(format!("out.push_str({});", rust_string(value)));
                        }
                        _ => rendered.push(format!(
                            "out.push_str({}.as_str());",
                            self.const_value(konst)
                        )),
                    }
                }
                RirOperand::Place(place) => {
                    rendered.push(format!(
                        "out.push_str({});",
                        self.borrow_expr(function, place)
                    ));
                }
            }
        }
        rendered.push("anvyx_runtime::AnvString::from(out)".into());
        rendered.push("}".into());
        rendered.join(" ")
    }

    fn format_operand(
        &self,
        function: &RirFunction,
        operand: &RirOperand,
        source_ty: RirTypeId,
    ) -> String {
        match (self.program.types[source_ty.index()], operand) {
            (RirType::String, RirOperand::Place(place)) => self.borrow_expr(function, place),
            (RirType::String, RirOperand::Const(id)) => {
                let konst = &self.program.consts[id.index()];
                match &konst.value {
                    RirConstValue::String(value) => rust_string(value),
                    _ => self.const_value(konst),
                }
            }
            _ => self.operand(function, operand),
        }
    }

    fn extern_call(
        &self,
        function: &RirFunction,
        id: super::rir::RirExternId,
        args: &[RirCallArg],
    ) -> String {
        let ext = &self.program.externs[id.index()];
        let (symbol, mut rendered, fallible, ret_abi) = match &ext.kind {
            RirExternKind::Native(native) => (
                native.path.join("::"),
                vec!["ctx.runtime()".to_string()],
                native.abi.fallible,
                &native.abi.ret,
            ),
        };
        rendered.extend(args.iter().map(|arg| self.call_arg(function, arg)));
        let call = format!("{symbol}({})", rendered.join(", "));
        let call = if fallible { format!("{call}?") } else { call };
        self.native_return_call(ext.ret, ret_abi, call)
    }

    fn native_return_call(
        &self,
        ret: RirTypeId,
        abi: &anvyx_runtime::RustReturnAbi,
        call: String,
    ) -> String {
        match abi {
            anvyx_runtime::RustReturnAbi::Value(anvyx_runtime::ExternTypeExpr::String) => {
                format!("anvyx_runtime::AnvString::from({call})")
            }
            anvyx_runtime::RustReturnAbi::Option(inner) => {
                self.option_return_call(ret, inner, call)
            }
            _ => call,
        }
    }

    fn option_return_call(
        &self,
        ret: RirTypeId,
        inner: &anvyx_runtime::RustReturnAbi,
        call: String,
    ) -> String {
        let RirType::Option(_) = self.program.types[ret.index()] else {
            unreachable!("verified native option return type")
        };
        let value = match inner {
            anvyx_runtime::RustReturnAbi::Value(anvyx_runtime::ExternTypeExpr::String) => {
                "anvyx_runtime::AnvString::from(value)".to_string()
            }
            anvyx_runtime::RustReturnAbi::Value(_) => "value".to_string(),
            _ => unreachable!("verified native option return inner"),
        };
        format!("match {call} {{ Some(value) => Some({value}), None => None }}")
    }

    fn call_arg(&self, function: &RirFunction, arg: &RirCallArg) -> String {
        match arg {
            RirCallArg::Value(operand) => self.value_operand(function, operand),
            RirCallArg::SharedBorrow(place) => self.borrow_expr(function, place),
            RirCallArg::SharedStringConst(id) => match &self.program.consts[id.index()].value {
                RirConstValue::String(value) => rust_string(value),
                _ => unreachable!("verified shared string const"),
            },
            RirCallArg::MutBorrow(place) => format!("&mut {}", self.place(function, place)),
        }
    }

    fn borrow_expr(&self, function: &RirFunction, place: &RirPlace) -> String {
        RustRepPolicy::new(self.program)
            .borrow_view(place.ty)
            .render(
                self.place(function, place),
                self.borrowed_root_param(function, place),
            )
    }

    fn borrowed_root_param(&self, function: &RirFunction, place: &RirPlace) -> bool {
        place.projections.is_empty()
            && function
                .params
                .iter()
                .any(|param| param.local == place.local && param.abi == RirParamAbi::SharedBorrow)
    }

    fn mut_borrowed_root_param(&self, function: &RirFunction, place: &RirPlace) -> bool {
        function
            .params
            .iter()
            .any(|param| param.local == place.local && param.abi == RirParamAbi::MutBorrow)
    }

    fn value_operand(&self, function: &RirFunction, operand: &RirOperand) -> String {
        let RirOperand::Place(place) = operand else {
            return self.operand(function, operand);
        };
        let policy = RustRepPolicy::new(self.program);
        if policy.cow_value(place.ty) {
            if self.borrowed_root_param(function, place)
                && matches!(self.program.types[place.ty.index()], RirType::String)
            {
                return format!(
                    "anvyx_runtime::AnvString::from({})",
                    self.place(function, place)
                );
            }
            return format!("{}.share()", self.place(function, place));
        }
        if !policy.copyable(place.ty) && policy.shareable_value(place.ty) {
            return self.value_from_place(place.ty, &self.place(function, place));
        }
        self.operand(function, operand)
    }

    fn operand(&self, function: &RirFunction, operand: &RirOperand) -> String {
        match operand {
            RirOperand::Place(place) => match self.program.types[place.ty.index()] {
                RirType::Struct(id) if self.program.structs[id.index()].copyable => {
                    self.copy_struct_place(function, place)
                }
                RirType::Enum(id) if self.program.enums[id.index()].copyable => {
                    self.copy_enum_place(function, place)
                }
                RirType::Array { .. } => self.copy_array_place(function, place),
                _ => self.place(function, place),
            },
            RirOperand::Const(id) => self.const_value(&self.program.consts[id.index()]),
        }
    }

    fn place(&self, function: &RirFunction, place: &RirPlace) -> String {
        let symbol = function.locals[place.local.index()].symbol.as_str();
        let mut out = if self.mut_borrowed_root_param(function, place) {
            format!("(*{symbol})")
        } else {
            symbol.to_string()
        };
        let mut ty = function.locals[place.local.index()].ty;
        for projection in &place.projections {
            match projection {
                super::rir::RirProjection::Field(field_id) => {
                    let RirType::Struct(struct_id) = self.program.types[ty.index()] else {
                        unreachable!("verified field projection")
                    };
                    let field = &self.program.structs[struct_id.index()].fields[field_id.index()];
                    out.push('.');
                    out.push_str(field.symbol.as_str());
                    ty = field.ty;
                }
                super::rir::RirProjection::Index(index) => {
                    let (elem, len) = match self.program.types[ty.index()] {
                        RirType::Array { elem, len } => (elem, Some(len)),
                        RirType::List(elem) => (elem, None),
                        _ => unreachable!("verified index projection"),
                    };
                    let index = function.locals[index.index()].symbol.as_str();
                    let index = checked_index(index, len);
                    out.push('[');
                    out.push_str(&index);
                    out.push(']');
                    ty = elem;
                }
            }
        }
        out
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
        let fields = dataref
            .fields
            .iter()
            .zip(fields)
            .map(|(field, value)| {
                format!(
                    "{}: {}",
                    field.symbol.as_str(),
                    self.value_operand(function, value)
                )
            })
            .collect::<Vec<_>>()
            .join(", ");
        format!(
            "{{ let heap_type = ctx._types.{}; ctx.heap().alloc(heap_type, {} {{ {} }}) }}",
            dataref.heap_type_symbol(),
            dataref.storage_symbol(),
            fields
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
        let object = self.operand_ref(function, object);
        let (path, _) = self.storage_path(function, dataref, projections);
        format!(
            "ctx.heap().with({object}, |storage| {})",
            self.value_from_place(ty, &path)
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
        let object = self.operand_ref(function, object);
        let (path, _) = self.storage_path(function, dataref, projections);
        format!(
            "ctx.heap().with_mut({object}, |storage| {{ {path} = {}; }})",
            self.value_operand(function, value)
        )
    }

    fn operand_ref(&self, function: &RirFunction, operand: &RirOperand) -> String {
        match operand {
            RirOperand::Place(place) => format!("&{}", self.place(function, place)),
            RirOperand::Const(_) => format!("&{}", self.operand(function, operand)),
        }
    }

    fn storage_path(
        &self,
        function: &RirFunction,
        dataref: super::rir::RirDataRefId,
        projections: &[super::rir::RirProjection],
    ) -> (String, RirTypeId) {
        let dataref = &self.program.datarefs[dataref.index()];
        let Some((first, rest)) = projections.split_first() else {
            unreachable!("verified dataref projection")
        };
        let super::rir::RirProjection::Field(field_id) = first else {
            unreachable!("verified dataref field projection")
        };
        let field = &dataref.fields[field_id.index()];
        let mut out = format!("storage.{}", field.symbol.as_str());
        let mut ty = field.ty;
        for projection in rest {
            match projection {
                super::rir::RirProjection::Field(field_id) => {
                    let RirType::Struct(struct_id) = self.program.types[ty.index()] else {
                        unreachable!("verified storage field projection")
                    };
                    let field = &self.program.structs[struct_id.index()].fields[field_id.index()];
                    out.push('.');
                    out.push_str(field.symbol.as_str());
                    ty = field.ty;
                }
                super::rir::RirProjection::Index(index) => {
                    let RirType::Array { elem, len } = self.program.types[ty.index()] else {
                        unreachable!("verified storage index projection")
                    };
                    let index = function.locals[index.index()].symbol.as_str();
                    out.push('[');
                    out.push_str(&checked_index(index, Some(len)));
                    out.push(']');
                    ty = elem;
                }
            }
        }
        (out, ty)
    }

    fn struct_literal(
        &self,
        function: &RirFunction,
        ty: RirTypeId,
        fields: &[RirOperand],
    ) -> String {
        let RirType::Struct(struct_id) = self.program.types[ty.index()] else {
            unreachable!("verified struct literal type")
        };
        let strukt = &self.program.structs[struct_id.index()];
        let fields = strukt
            .fields
            .iter()
            .zip(fields)
            .map(|(field, operand)| {
                format!(
                    "{}: {}",
                    field.symbol.as_str(),
                    self.value_operand(function, operand)
                )
            })
            .collect::<Vec<_>>()
            .join(", ");
        format!("{} {{ {} }}", strukt.symbol.as_str(), fields)
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
        let path = format!("{}::{}", enm.symbol.as_str(), variant.symbol.as_str());
        match variant.kind {
            RirVariantKind::Unit => path,
            RirVariantKind::Tuple => format!(
                "{}({})",
                path,
                fields
                    .iter()
                    .map(|field| self.value_operand(function, field))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            RirVariantKind::Struct => {
                let fields = variant
                    .fields
                    .iter()
                    .zip(fields)
                    .map(|(field, operand)| {
                        format!(
                            "{}: {}",
                            field.symbol.as_str(),
                            self.value_operand(function, operand)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{path} {{ {fields} }}")
            }
        }
    }

    fn copy_struct_place(&self, function: &RirFunction, place: &RirPlace) -> String {
        let RirType::Struct(struct_id) = self.program.types[place.ty.index()] else {
            unreachable!("verified struct copy place")
        };
        let strukt = &self.program.structs[struct_id.index()];
        let fields = strukt
            .fields
            .iter()
            .map(|field| {
                let mut field_place = place.clone();
                field_place
                    .projections
                    .push(super::rir::RirProjection::Field(field.id));
                field_place.ty = field.ty;
                format!(
                    "{}: {}",
                    field.symbol.as_str(),
                    self.operand(function, &RirOperand::Place(field_place))
                )
            })
            .collect::<Vec<_>>()
            .join(", ");
        format!("{} {{ {} }}", strukt.symbol.as_str(), fields)
    }

    fn copy_array_place(&self, function: &RirFunction, place: &RirPlace) -> String {
        let RirType::Array { elem, len } = self.program.types[place.ty.index()] else {
            unreachable!("verified array copy place")
        };
        let source = self.place(function, place);
        let elems = (0..len)
            .map(|index| self.copy_from_ref(elem, &format!("&{source}[{index}]")))
            .collect::<Vec<_>>()
            .join(", ");
        format!("[{elems}]")
    }

    fn copy_enum_place(&self, function: &RirFunction, place: &RirPlace) -> String {
        let source = self.place(function, place);
        self.copy_enum_expr(&source, place.ty)
    }

    fn copy_enum_expr(&self, source: &str, ty: RirTypeId) -> String {
        self.copy_enum_ref_expr(&format!("&{source}"), ty)
    }

    fn copy_enum_ref_expr(&self, source: &str, ty: RirTypeId) -> String {
        let RirType::Enum(enum_id) = self.program.types[ty.index()] else {
            unreachable!("verified enum copy expression")
        };
        let enm = &self.program.enums[enum_id.index()];
        if enm.variants.is_empty() {
            return format!("match {source} {{}}");
        }
        let arms = enm
            .variants
            .iter()
            .map(|variant| self.copy_enum_variant_arm(enm, variant))
            .collect::<Vec<_>>()
            .join(", ");
        format!("match {source} {{ {arms} }}")
    }

    fn copy_enum_variant_arm(&self, enm: &RirEnum, variant: &RirVariant) -> String {
        let path = format!("{}::{}", enm.symbol.as_str(), variant.symbol.as_str());
        match variant.kind {
            RirVariantKind::Unit => format!("{path} => {path}"),
            RirVariantKind::Tuple => {
                let vars = (0..variant.fields.len())
                    .map(|index| format!("f{index}"))
                    .collect::<Vec<_>>();
                let values = variant
                    .fields
                    .iter()
                    .zip(&vars)
                    .map(|(field, var)| self.value_from_ref(field.ty, var))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{path}({}) => {path}({values})", vars.join(", "))
            }
            RirVariantKind::Struct => {
                let vars = variant
                    .fields
                    .iter()
                    .map(|field| field.symbol.as_str().to_string())
                    .collect::<Vec<_>>();
                let values = variant
                    .fields
                    .iter()
                    .zip(&vars)
                    .map(|(field, var)| {
                        format!(
                            "{}: {}",
                            field.symbol.as_str(),
                            self.value_from_ref(field.ty, var)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{path} {{ {} }} => {path} {{ {values} }}", vars.join(", "))
            }
        }
    }

    fn value_from_ref(&self, ty: RirTypeId, expr: &str) -> String {
        match self.program.types[ty.index()] {
            RirType::String | RirType::List(_) | RirType::Map { .. } => {
                format!("(*({expr})).share()")
            }
            RirType::DataRef(_) => format!("(*({expr})).clone()"),
            _ => self.copy_from_ref(ty, expr),
        }
    }

    fn value_from_place(&self, ty: RirTypeId, expr: &str) -> String {
        match self.program.types[ty.index()] {
            RirType::Int | RirType::Float | RirType::Bool => expr.to_string(),
            RirType::String | RirType::List(_) | RirType::Map { .. } => format!("{expr}.share()"),
            RirType::DataRef(_) => format!("{expr}.clone()"),
            RirType::Struct(_) | RirType::Array { .. } | RirType::Enum(_) => {
                self.copy_from_ref(ty, &format!("&{expr}"))
            }
            RirType::Option(_) => self.copy_from_ref(ty, &format!("&{expr}")),
            RirType::Slice(_) | RirType::Void => unreachable!("verified dataref field value"),
        }
    }

    fn copy_from_ref(&self, ty: RirTypeId, expr: &str) -> String {
        match self.program.types[ty.index()] {
            RirType::Int | RirType::Float | RirType::Bool => format!("*({expr})"),
            RirType::Struct(id) => {
                let strukt = &self.program.structs[id.index()];
                let fields = strukt
                    .fields
                    .iter()
                    .map(|field| {
                        let field_expr = format!("&({expr}).{}", field.symbol.as_str());
                        format!(
                            "{}: {}",
                            field.symbol.as_str(),
                            self.value_from_ref(field.ty, &field_expr)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{} {{ {} }}", strukt.symbol.as_str(), fields)
            }
            RirType::Array { elem, len } => {
                let elems = (0..len)
                    .map(|index| self.value_from_ref(elem, &format!("&({expr})[{index}]")))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{elems}]")
            }
            RirType::Enum(id) if self.program.enums[id.index()].variants.is_empty() => {
                format!("match *({expr}) {{}}")
            }
            RirType::Enum(_) => self.copy_enum_ref_expr(expr, ty),
            RirType::Option(inner) => format!(
                "({expr}).as_ref().map(|value| {})",
                self.value_from_ref(inner, "value")
            ),
            RirType::DataRef(_) => format!("(*({expr})).clone()"),
            RirType::String | RirType::List(_) | RirType::Map { .. } => {
                format!("(*({expr})).share()")
            }
            RirType::Slice(_) | RirType::Void => unreachable!("verified copy enum payload"),
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
                format!(
                    "{}(ctx, &{})",
                    self.stringify_helper(ty),
                    self.place_operand(function, value)
                )
            }
            super::rir::RirStringifyReqKind::Override {
                function: target,
                mode,
            } => {
                let symbol = self.program.functions[target.index()].symbol.as_str();
                let arg = match mode {
                    RirParamSemantic::Value => self.operand(function, value),
                    RirParamSemantic::SharedBorrow => {
                        let RirOperand::Place(place) = value else {
                            unreachable!("verified stringify override place")
                        };
                        self.borrow_expr(function, place)
                    }
                    RirParamSemantic::MutBorrow => unreachable!("verified stringify override mode"),
                };
                format!("{symbol}(ctx, {arg})")
            }
        }
    }

    fn place_operand(&self, function: &RirFunction, value: &RirOperand) -> String {
        let RirOperand::Place(place) = value else {
            unreachable!("verified place operand")
        };
        self.place(function, place)
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

    fn const_value(&self, konst: &RirConst) -> String {
        match &konst.value {
            RirConstValue::Int(value) => value.to_string(),
            RirConstValue::Float(value) => {
                let text = value.to_string();
                if text.contains('.') {
                    text
                } else {
                    format!("{text}.0")
                }
            }
            RirConstValue::Bool(value) => value.to_string(),
            RirConstValue::String(value) => {
                format!("anvyx_runtime::AnvString::from({})", rust_string(value))
            }
            RirConstValue::Nil => "None".into(),
        }
    }

    fn param_ty(&self, ty: RirTypeId, abi: RirParamAbi) -> String {
        match abi {
            RirParamAbi::Value => self.ty(ty),
            RirParamAbi::SharedBorrow => match RustRepPolicy::new(self.program).borrow_view(ty) {
                RustBorrowView::Str => "&str".into(),
                _ => format!("&{}", self.ty(ty)),
            },
            RirParamAbi::MutBorrow => format!("&mut {}", self.ty(ty)),
        }
    }

    fn ty(&self, ty: RirTypeId) -> String {
        RustRepPolicy::new(self.program).rust_ty(ty)
    }

    fn line(&mut self, text: &str) {
        writeln!(self.out, "{text}").expect("write to string");
    }
}

fn unary_op(op: UnaryOp) -> &'static str {
    match op {
        UnaryOp::Neg => "-",
        UnaryOp::Not => "!",
        UnaryOp::BitNot => "!",
    }
}

fn format_fragment(spec: RirFormatSpec) -> String {
    let mut fragment = String::from("{");
    if spec != RirFormatSpec::default() {
        fragment.push(':');
        if let Some(align) = spec.align {
            let implicit_zero_align =
                spec.zero_pad && spec.fill == '0' && align == RirFormatAlign::Right;
            if !implicit_zero_align {
                if spec.fill != '\0' {
                    push_format_char(&mut fragment, spec.fill);
                }
                fragment.push(match align {
                    RirFormatAlign::Left => '<',
                    RirFormatAlign::Right => '>',
                    RirFormatAlign::Center => '^',
                });
            }
        }
        if spec.sign == RirFormatSign::Always {
            fragment.push('+');
        }
        if spec.zero_pad {
            fragment.push('0');
        }
        if let Some(width) = spec.width {
            write!(fragment, "{width}").expect("write to string failed");
        }
        if let Some(precision) = spec.precision {
            write!(fragment, ".{precision}").expect("write to string failed");
        }
        fragment.push_str(match spec.kind {
            RirFormatKind::Default => "",
            RirFormatKind::Hex => "x",
            RirFormatKind::HexUpper => "X",
            RirFormatKind::Binary => "b",
            RirFormatKind::Exp => "e",
            RirFormatKind::ExpUpper => "E",
        });
    }
    fragment.push('}');
    fragment
}

fn push_format_char(out: &mut String, ch: char) {
    match ch {
        '{' => out.push_str("{{"),
        '}' => out.push_str("}}"),
        ch => out.push(ch),
    }
}

fn binary_op(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "+",
        BinaryOp::Sub => "-",
        BinaryOp::Mul => "*",
        BinaryOp::Div => "/",
        BinaryOp::Rem => "%",
        BinaryOp::Eq => "==",
        BinaryOp::NotEq => "!=",
        BinaryOp::LessThan => "<",
        BinaryOp::GreaterThan => ">",
        BinaryOp::LessThanEq => "<=",
        BinaryOp::GreaterThanEq => ">=",
        BinaryOp::And => "&&",
        BinaryOp::Or => "||",
        BinaryOp::Xor => "^",
        BinaryOp::BitAnd => "&",
        BinaryOp::BitOr => "|",
        BinaryOp::Shl => "<<",
        BinaryOp::Shr => ">>",
        BinaryOp::Coalesce => panic!("verified RIR excludes coalesce"),
    }
}

fn checked_index(index: &str, len: Option<u64>) -> String {
    match len {
        Some(len) => format!(
            "{{ let i = {index}; if i < 0 {{ panic!(\"negative index\"); }} let u = i as usize; if u >= {len} {{ panic!(\"index out of bounds\"); }} u }}"
        ),
        None => {
            format!("{{ let i = {index}; if i < 0 {{ panic!(\"negative index\"); }} i as usize }}")
        }
    }
}

fn loop_label(id: RirLoopId) -> String {
    format!("'loop_{}", id.index())
}

fn checked_range(start: &str, end: &str, inclusive: bool, len: &str) -> String {
    let end_expr = if inclusive {
        "{ if e0 == i64::MAX { panic!(\"range end overflow\"); } e0 + 1 }".to_string()
    } else {
        "e0".into()
    };
    format!(
        "{{ let s = {start}; let e0 = {end}; if s < 0 || e0 < 0 {{ panic!(\"negative range bound\"); }}; if s > e0 {{ panic!(\"invalid range\"); }}; let e = {end_expr}; let su = s as usize; let eu = e as usize; if eu > {len} {{ panic!(\"range out of bounds\"); }}; su..eu }}"
    )
}

fn rust_string(text: &str) -> String {
    let mut out = String::from("\"");
    for ch in text.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            ch if ch.is_control() => {
                write!(out, "\\u{{{:x}}}", ch as u32).expect("write to string");
            }
            ch => out.push(ch),
        }
    }
    out.push('"');
    out
}
