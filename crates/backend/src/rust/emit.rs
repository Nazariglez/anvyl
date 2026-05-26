use std::fmt::Write;

use anvyx_frontend::ast::{BinaryOp, UnaryOp};

use super::rir::{
    RirCallArg, RirCallTarget, RirConst, RirConstValue, RirEnum, RirEnumMatch, RirExternKind,
    RirFormatAlign, RirFormatKind, RirFormatSign, RirFormatSpec, RirFunction, RirIf, RirOperand,
    RirParamAbi, RirParamSemantic, RirPlace, RirProgram, RirRValue, RirStmt, RirStructuredBlock,
    RirTerm, RirType, RirTypeId, RirVariant, RirVariantId, RirVariantKind, VerifiedRirProgram,
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
    let mut cx = EmitCx {
        program: program.program(),
        out: String::new(),
    };
    cx.emit_program();
    RustSource::new(cx.out)
}

struct EmitCx<'a> {
    program: &'a RirProgram,
    out: String,
}

impl EmitCx<'_> {
    fn emit_program(&mut self) {
        if !self.program.stringify_helpers.is_empty() {
            self.line("use std::fmt::Write;");
            self.line("");
        }
        self.line("#[derive(Default)]");
        self.line(&format!("struct {} {{}}", self.program.ctx.symbol.as_str()));
        self.line("");
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
            self.line("fn main() {");
            self.line(&format!(
                "    let mut ctx = {}::default();",
                self.program.ctx.symbol.as_str()
            ));
            let symbol = self.program.functions[entry.index()].symbol.as_str();
            self.line(&format!("    let _ = {symbol}(&mut ctx);"));
            self.line("}");
        }
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
        self.line(&format!("struct {} {{", strukt.symbol.as_str()));
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
        self.line(&format!("enum {} {{", enm.symbol.as_str()));
        for variant in &enm.variants {
            match variant.kind {
                RirVariantKind::Unit => {
                    self.line(&format!("    {},", variant.symbol.as_str()));
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

    fn emit_stringify_helper(&mut self, helper: &super::rir::RirStringifyHelper) {
        let RirType::Struct(struct_id) = self.program.types[helper.ty.index()] else {
            unreachable!("verified stringify helper target")
        };
        let strukt = &self.program.structs[struct_id.index()];
        let ctx = self.stringify_helper_ctx_name(strukt);
        self.line(&format!(
            "fn {}({ctx}: &mut {}, value: &{}) -> String {{",
            helper.symbol.as_str(),
            self.program.ctx.symbol.as_str(),
            strukt.symbol.as_str()
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
                | RirType::Enum(_)
                | RirType::Array { .. }
                | RirType::List(_)
                | RirType::Slice(_) => unreachable!("verified stringify helper field"),
            }
        }
        self.line("    out.push(')');");
        self.line("    out");
        self.line("}");
        self.line("");
    }

    fn emit_function(&mut self, function: &RirFunction) {
        let ctx = if self.function_needs_context(function) {
            "ctx"
        } else {
            "_ctx"
        };
        let mut params = vec![format!("{ctx}: &mut {}", self.program.ctx.symbol.as_str())];
        params.extend(function.params.iter().map(|param| {
            let local = &function.locals[param.local.index()];
            format!(
                "{}: {}",
                local.symbol.as_str(),
                self.param_ty(param.ty, param.abi)
            )
        }));
        let ret = self.ty(function.ret.ty);
        if ret == "()" {
            self.line(&format!(
                "fn {}({}) {{",
                function.symbol.as_str(),
                params.join(", ")
            ));
        } else {
            self.line(&format!(
                "fn {}({}) -> {ret} {{",
                function.symbol.as_str(),
                params.join(", ")
            ));
        }
        let block = &function.body;
        let predeclare = block
            .stmts
            .iter()
            .any(|stmt| matches!(stmt, RirStmt::If(_) | RirStmt::EnumMatch(_)));
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

    fn emit_local_declarations(&mut self, function: &RirFunction) {
        for local in &function.locals {
            if function.params.iter().any(|param| param.local == local.id) {
                continue;
            }
            let mutability = if local.mutable { "mut " } else { "" };
            self.line(&format!(
                "    let {mutability}{}: {};",
                local.symbol.as_str(),
                self.ty(local.ty)
            ));
        }
    }

    fn function_needs_context(&self, function: &RirFunction) -> bool {
        self.block_needs_context(&function.body)
    }

    fn stmt_needs_context(&self, stmt: &RirStmt) -> bool {
        match stmt {
            RirStmt::Init { value, .. } | RirStmt::Assign { value, .. } | RirStmt::Eval(value) => {
                self.rvalue_needs_context(value)
            }
            RirStmt::If(branch) => {
                self.block_needs_context(&branch.then_block)
                    || branch
                        .else_block
                        .as_ref()
                        .is_some_and(|block| self.block_needs_context(block))
            }
            RirStmt::EnumMatch(match_) => {
                match_
                    .arms
                    .iter()
                    .any(|arm| self.block_needs_context(&arm.block))
                    || match_
                        .else_block
                        .as_ref()
                        .is_some_and(|block| self.block_needs_context(block))
            }
        }
    }

    fn block_needs_context(&self, block: &RirStructuredBlock) -> bool {
        block.stmts.iter().any(|stmt| self.stmt_needs_context(stmt))
    }

    fn rvalue_needs_context(&self, value: &RirRValue) -> bool {
        match value {
            RirRValue::Call { .. } => true,
            RirRValue::Stringify { source_ty, .. } => {
                matches!(self.program.types[source_ty.index()], RirType::Struct(_))
            }
            RirRValue::Use(_)
            | RirRValue::Unary { .. }
            | RirRValue::Binary { .. }
            | RirRValue::Cast { .. }
            | RirRValue::StringConcat { .. }
            | RirRValue::Format { .. }
            | RirRValue::Len { .. }
            | RirRValue::ListPush { .. }
            | RirRValue::SliceView { .. }
            | RirRValue::Array { .. }
            | RirRValue::List { .. }
            | RirRValue::ListSlice { .. }
            | RirRValue::Struct { .. }
            | RirRValue::EnumVariant { .. } => false,
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
            RirStmt::Eval(value) => {
                self.line(&format!("{indent}{};", self.rvalue(function, value)));
            }
            RirStmt::If(branch) => self.emit_if(function, branch, indent, predeclared),
            RirStmt::EnumMatch(match_) => self.emit_match(function, match_, indent, predeclared),
        }
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
            RirTerm::Return(None) => self.line(&format!("{indent}return;")),
            RirTerm::Return(Some(operand)) => {
                self.line(&format!(
                    "{indent}return {};",
                    self.operand(function, operand)
                ));
            }
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
            RirRValue::Use(operand) => self.operand(function, operand),
            RirRValue::Struct { ty, fields } => self.struct_literal(function, *ty, fields),
            RirRValue::Array { elems, .. } => format!(
                "[{}]",
                elems
                    .iter()
                    .map(|elem| self.operand(function, elem))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            RirRValue::List { elems, .. } => format!(
                "vec![{}]",
                elems
                    .iter()
                    .map(|elem| self.operand(function, elem))
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
            RirRValue::Cast { value, target } => {
                format!("{} as {}", self.operand(function, value), self.ty(*target))
            }
            RirRValue::Call { callee, args, .. } => match callee {
                RirCallTarget::Function(id) => {
                    let symbol = self.program.functions[id.index()].symbol.as_str();
                    let mut rendered = vec!["ctx".to_string()];
                    rendered.extend(args.iter().map(|arg| self.call_arg(function, arg)));
                    format!("{symbol}({})", rendered.join(", "))
                }
                RirCallTarget::Extern(id) => self.extern_call(function, *id, args),
            },
            RirRValue::Stringify { value, source_ty } => {
                match self.program.types[source_ty.index()] {
                    RirType::String => self.operand(function, value),
                    RirType::Int | RirType::Float | RirType::Bool => {
                        format!("format!(\"{{}}\", {})", self.operand(function, value))
                    }
                    RirType::Struct(_) => self.stringify_struct(function, value, *source_ty),
                    RirType::Void
                    | RirType::Enum(_)
                    | RirType::Array { .. }
                    | RirType::List(_)
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
                "format!({}, {})",
                rust_string(&format_fragment(*spec)),
                self.format_operand(function, value, *source_ty)
            ),
            RirRValue::Len { source } => format!("{}.len() as i64", self.place(function, source)),
            RirRValue::ListPush { list, value } => format!(
                "{}.push({})",
                self.place(function, list),
                self.operand(function, value)
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
        }
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
            "{{ let mut out = Vec::new(); for item in &{source_expr}[{range}] {{ out.push({}); }} out }}",
            self.copy_from_ref(elem, "item")
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
                        self.string_slice_expr(function, place)
                    ));
                }
            }
        }
        rendered.push("out".into());
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
            (RirType::String, RirOperand::Place(place)) => self.string_slice_expr(function, place),
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
            RirExternKind::Native(native) => {
                let mut rendered = vec![];
                if native.abi.needs_context {
                    rendered.push("ctx".to_string());
                }
                (
                    native.path.join("::"),
                    rendered,
                    native.abi.fallible,
                    &native.abi.ret,
                )
            }
        };
        rendered.extend(args.iter().map(|arg| self.call_arg(function, arg)));
        let call = format!("{symbol}({})", rendered.join(", "));
        let call = if fallible {
            format!("{call}.unwrap()")
        } else {
            call
        };
        self.native_return_call(ext.ret, ret_abi, call)
    }

    fn native_return_call(
        &self,
        ret: RirTypeId,
        abi: &anvyx_runtime::RustReturnAbi,
        call: String,
    ) -> String {
        match abi {
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
        let RirType::Enum(enum_id) = self.program.types[ret.index()] else {
            unreachable!("verified native option return type")
        };
        let enm = &self.program.enums[enum_id.index()];
        let none = &enm.variants[0];
        let some = &enm.variants[1];
        let value = match inner {
            anvyx_runtime::RustReturnAbi::Value(_) => "value".to_string(),
            _ => unreachable!("verified native option return inner"),
        };
        format!(
            "match {call} {{ Some(value) => {}::{}, None => {}::{} }}",
            enm.symbol.as_str(),
            self.option_some_expr(some, &value),
            enm.symbol.as_str(),
            none.symbol.as_str()
        )
    }

    fn option_some_expr(&self, variant: &RirVariant, value: &str) -> String {
        match variant.kind {
            RirVariantKind::Tuple => format!("{}({value})", variant.symbol.as_str()),
            _ => unreachable!("verified native option some variant"),
        }
    }

    fn call_arg(&self, function: &RirFunction, arg: &RirCallArg) -> String {
        match arg {
            RirCallArg::Value(operand) => self.operand(function, operand),
            RirCallArg::SharedBorrow(place) => self.shared_borrow_arg(function, place),
            RirCallArg::SharedStringConst(id) => match &self.program.consts[id.index()].value {
                RirConstValue::String(value) => rust_string(value),
                _ => unreachable!("verified shared string const"),
            },
            RirCallArg::MutBorrow(place) => format!("&mut {}", self.place(function, place)),
        }
    }

    fn shared_borrow_arg(&self, function: &RirFunction, place: &RirPlace) -> String {
        if matches!(self.program.types[place.ty.index()], RirType::String) {
            self.string_slice_expr(function, place)
        } else if self.borrowed_root_param(function, place) {
            self.place(function, place)
        } else {
            format!("&{}", self.place(function, place))
        }
    }

    fn string_slice_expr(&self, function: &RirFunction, place: &RirPlace) -> String {
        if self.borrowed_root_param(function, place) {
            self.place(function, place)
        } else {
            format!("{}.as_str()", self.place(function, place))
        }
    }

    fn borrowed_root_param(&self, function: &RirFunction, place: &RirPlace) -> bool {
        place.projections.is_empty()
            && function
                .params
                .iter()
                .any(|param| param.local == place.local && param.abi == RirParamAbi::SharedBorrow)
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
        let mut out = function.locals[place.local.index()]
            .symbol
            .as_str()
            .to_string();
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
                    self.operand(function, operand)
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
                    .map(|field| self.operand(function, field))
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
                            self.operand(function, operand)
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
        let RirType::Enum(enum_id) = self.program.types[ty.index()] else {
            unreachable!("verified enum copy expression")
        };
        let enm = &self.program.enums[enum_id.index()];
        let arms = enm
            .variants
            .iter()
            .map(|variant| self.copy_enum_variant_arm(enm, variant))
            .collect::<Vec<_>>()
            .join(", ");
        format!("match &{source} {{ {arms} }}")
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
                    .map(|(field, var)| self.copy_from_ref(field.ty, var))
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
                            self.copy_from_ref(field.ty, var)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{path} {{ {} }} => {path} {{ {values} }}", vars.join(", "))
            }
        }
    }

    fn copy_from_ref(&self, ty: RirTypeId, expr: &str) -> String {
        match self.program.types[ty.index()] {
            RirType::Int | RirType::Float | RirType::Bool => format!("*{expr}"),
            RirType::Struct(id) => {
                let strukt = &self.program.structs[id.index()];
                let fields = strukt
                    .fields
                    .iter()
                    .map(|field| {
                        let field_expr = format!("{expr}.{}", field.symbol.as_str());
                        format!(
                            "{}: {}",
                            field.symbol.as_str(),
                            self.copy_from_ref(field.ty, &field_expr)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{} {{ {} }}", strukt.symbol.as_str(), fields)
            }
            RirType::Array { elem, len } => {
                let elems = (0..len)
                    .map(|index| self.copy_from_ref(elem, &format!("{expr}[{index}]")))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{elems}]")
            }
            RirType::Enum(_) => self.copy_enum_expr(expr, ty),
            RirType::List(_) | RirType::Slice(_) | RirType::String | RirType::Void => {
                unreachable!("verified copy enum payload")
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
                        self.shared_borrow_arg(function, place)
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
            RirConstValue::String(value) => format!("String::from({})", rust_string(value)),
        }
    }

    fn param_ty(&self, ty: RirTypeId, abi: RirParamAbi) -> String {
        match abi {
            RirParamAbi::Value => self.ty(ty),
            RirParamAbi::SharedBorrow
                if matches!(self.program.types[ty.index()], RirType::String) =>
            {
                "&str".into()
            }
            RirParamAbi::SharedBorrow => format!("&{}", self.ty(ty)),
            RirParamAbi::MutBorrow => format!("&mut {}", self.ty(ty)),
            RirParamAbi::PlaceHandle => self.ty(ty),
        }
    }

    fn ty(&self, ty: RirTypeId) -> String {
        match self.program.types[ty.index()] {
            RirType::Int => "i64".into(),
            RirType::Float => "f64".into(),
            RirType::Bool => "bool".into(),
            RirType::String => "String".into(),
            RirType::Void => "()".into(),
            RirType::Struct(id) => self.program.structs[id.index()].symbol.as_str().into(),
            RirType::Enum(id) => self.program.enums[id.index()].symbol.as_str().into(),
            RirType::Array { elem, len } => format!("[{}; {}]", self.ty(elem), len),
            RirType::List(elem) => format!("Vec<{}>", self.ty(elem)),
            RirType::Slice(elem) => format!("&[{}]", self.ty(elem)),
        }
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
