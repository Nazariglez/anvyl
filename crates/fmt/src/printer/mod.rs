use std::{collections::HashMap, fmt::Write};

use anvyx_frontend::ast;

use super::trivia::{TriviaItem, TriviaKind};

mod decl;
mod expr;
mod pattern;
mod stmt;
mod types;

const MAX_WIDTH: usize = 100;

#[derive(Clone, Copy)]
struct Snapshot {
    buf_len: usize,
    trivia_cursor: usize,
    indent: u32,
}

fn expr_has_block(expr: &ast::Expr) -> bool {
    match &expr.kind {
        ast::ExprKind::If(_) | ast::ExprKind::Match(_) | ast::ExprKind::Block(_) => true,
        ast::ExprKind::Lambda(l) => matches!(l.node.body.node.kind, ast::ExprKind::Block(_)),
        _ => false,
    }
}

fn escape_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for ch in s.chars() {
        match ch {
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\0' => out.push_str("\\0"),
            other => out.push(other),
        }
    }
    out
}

// keep imports and consts grouped and separate everything else
fn needs_blank_line_between(a: &ast::Stmt, b: &ast::Stmt) -> bool {
    let same_group = (matches!(a, ast::Stmt::Import(_)) && matches!(b, ast::Stmt::Import(_)))
        || (matches!(a, ast::Stmt::Const(_)) && matches!(b, ast::Stmt::Const(_)));
    !same_group
}

pub struct Printer<'a> {
    source: &'a str,
    trivia: &'a [TriviaItem],
    trivia_cursor: usize,
    buf: String,
    indent: u32,
    type_var_names: HashMap<ast::TypeVarId, String>,
    const_param_names: HashMap<ast::ConstParamId, String>,
}

impl<'a> Printer<'a> {
    pub fn new(source: &'a str, trivia: &'a [TriviaItem]) -> Self {
        let estimated_capacity = source.len() + source.len() / 4;
        Self {
            source,
            trivia,
            trivia_cursor: 0,
            buf: String::with_capacity(estimated_capacity),
            indent: 0,
            type_var_names: HashMap::new(),
            const_param_names: HashMap::new(),
        }
    }

    pub fn finish(self) -> String {
        let mut result = self.buf;
        if !result.ends_with('\n') {
            result.push('\n');
        }
        result
    }

    fn write(&mut self, s: &str) {
        self.buf.push_str(s);
    }

    fn write_fmt(&mut self, val: impl std::fmt::Display) {
        write!(self.buf, "{val}").unwrap();
    }

    fn writeln(&mut self) {
        self.buf.push('\n');
    }

    fn write_indent(&mut self) {
        const SPACES: &str = "                                ";
        let n = (self.indent * 4) as usize;
        if n <= SPACES.len() {
            self.buf.push_str(&SPACES[..n]);
        } else {
            for _ in 0..n {
                self.buf.push(' ');
            }
        }
    }

    fn format_pattern_head(&mut self, head: ast::PatternHead) {
        self.write(match head {
            ast::PatternHead::Let => "let",
            ast::PatternHead::Var => "var",
        });
    }

    fn indent(&mut self) {
        self.indent += 1;
    }

    fn dedent(&mut self) {
        assert!(self.indent > 0, "dedent below zero");
        self.indent -= 1;
    }

    fn current_column(&self) -> usize {
        match self.buf.rfind('\n') {
            Some(pos) => self.buf.len() - pos - 1,
            None => self.buf.len(),
        }
    }

    fn snapshot(&self) -> Snapshot {
        Snapshot {
            buf_len: self.buf.len(),
            trivia_cursor: self.trivia_cursor,
            indent: self.indent,
        }
    }

    fn restore(&mut self, snap: Snapshot) {
        self.buf.truncate(snap.buf_len);
        self.trivia_cursor = snap.trivia_cursor;
        self.indent = snap.indent;
    }

    // speculatively render the given function and keep the result if it fits
    // within MAX_WIDTH, otherwise roll back
    fn try_single_line(&mut self, f: impl FnOnce(&mut Self)) -> bool {
        let snap = self.snapshot();
        f(self);
        let rendered = &self.buf[snap.buf_len..];
        if !rendered.contains('\n') && self.current_column() <= MAX_WIDTH {
            true
        } else {
            self.restore(snap);
            false
        }
    }

    fn format_comma_list<T>(
        &mut self,
        open: &str,
        close: &str,
        items: &[T],
        format_item: impl Fn(&mut Self, &T),
    ) {
        let fits = self.try_single_line(|p| {
            p.write(open);
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    p.write(", ");
                }
                format_item(p, item);
            }
            p.write(close);
        });
        if !fits {
            self.write(open);
            self.writeln();
            self.indent();
            for item in items {
                self.write_indent();
                format_item(self, item);
                self.write(",");
                self.writeln();
            }
            self.dedent();
            self.write_indent();
            self.write(close);
        }
    }

    fn format_brace_list<T>(&mut self, items: &[T], format_item: impl Fn(&mut Self, &T)) {
        let fits = self.try_single_line(|p| {
            p.write(" { ");
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    p.write(", ");
                }
                format_item(p, item);
            }
            p.write(" }");
        });
        if !fits {
            self.write(" {");
            self.writeln();
            self.indent();
            for item in items {
                self.write_indent();
                format_item(self, item);
                self.write(",");
                self.writeln();
            }
            self.dedent();
            self.write_indent();
            self.write("}");
        }
    }

    fn format_return_type(&mut self, ret: &ast::Type) {
        if !matches!(ret, ast::Type::Void) {
            self.write(" -> ");
            self.format_type(ret);
        }
    }

    fn format_visibility(&mut self, vis: ast::Visibility) {
        if matches!(vis, ast::Visibility::Public) {
            self.write("pub ");
        }
    }

    fn populate_type_param_names(
        &mut self,
        type_params: &[ast::TypeParam],
        const_params: &[ast::ConstParam],
    ) {
        self.type_var_names = type_params
            .iter()
            .map(|tp| (tp.id, tp.name.to_string()))
            .collect();
        self.const_param_names = const_params
            .iter()
            .map(|cp| (cp.id, cp.name.to_string()))
            .collect();
    }

    fn extend_type_param_names(
        &mut self,
        type_params: &[ast::TypeParam],
        const_params: &[ast::ConstParam],
    ) {
        for tp in type_params {
            self.type_var_names.insert(tp.id, tp.name.to_string());
        }
        for cp in const_params {
            self.const_param_names.insert(cp.id, cp.name.to_string());
        }
    }

    fn with_extended_type_param_names(
        &mut self,
        type_params: &[ast::TypeParam],
        const_params: &[ast::ConstParam],
        f: impl FnOnce(&mut Self),
    ) {
        let saved_type_vars = self.type_var_names.clone();
        let saved_const_params = self.const_param_names.clone();
        self.extend_type_param_names(type_params, const_params);
        f(self);
        self.type_var_names = saved_type_vars;
        self.const_param_names = saved_const_params;
    }

    fn emit_trivia_before(&mut self, pos: usize) {
        while self.trivia_cursor < self.trivia.len() {
            let item = &self.trivia[self.trivia_cursor];
            if item.span.start >= pos {
                break;
            }
            match item.kind {
                TriviaKind::LineComment => {
                    self.write_indent();
                    self.write(&item.text);
                    self.writeln();
                }
                TriviaKind::BlankLine => {
                    if !self.buf.ends_with("\n\n") {
                        self.writeln();
                    }
                }
            }
            self.trivia_cursor += 1;
        }
    }

    fn emit_trailing_trivia(&mut self, prev_end: usize, next_start: usize) {
        while self.trivia_cursor < self.trivia.len() {
            let item = &self.trivia[self.trivia_cursor];
            if item.span.start >= next_start {
                break;
            }
            // struct/enum/extend bodies don't consume their own trivia so skip anything before prev_end
            if item.span.start < prev_end {
                self.trivia_cursor += 1;
                continue;
            }
            let between = &self.source[prev_end..item.span.start];
            let on_same_line = !between.contains('\n');
            if on_same_line {
                // remove the trailing newline that format_stmt wrote so
                // the comment lands on the same line
                if self.buf.ends_with('\n') {
                    self.buf.pop();
                }
                self.write(" ");
                self.write(&item.text);
                self.writeln();
                self.trivia_cursor += 1;
            } else {
                break;
            }
        }
    }

    fn format_lit(&mut self, lit: &ast::Lit) {
        match lit {
            ast::Lit::Int(n) => self.write_fmt(n),
            ast::Lit::Float(value) => {
                let s = value.to_string();
                if value.is_finite() && !s.contains('.') {
                    self.write(&s);
                    self.write(".0");
                } else {
                    self.write(&s);
                }
            }
            ast::Lit::Bool(b) => self.write(if *b { "true" } else { "false" }),
            ast::Lit::String(s) => {
                self.write("\"");
                self.write(&escape_string(s));
                self.write("\"");
            }
            ast::Lit::Nil => self.write("nil"),
        }
    }

    fn format_block_expanded(&mut self, block: &ast::BlockNode) {
        self.format_block_inner(block, false);
    }

    fn format_block(&mut self, block: &ast::BlockNode) {
        self.format_block_inner(block, true);
    }

    fn format_block_inner(&mut self, block: &ast::BlockNode, allow_compact: bool) {
        debug_assert!(block.span.start < block.span.end);
        debug_assert!(block.span.end <= self.source.len());
        debug_assert_eq!(self.source.as_bytes().get(block.span.start), Some(&b'{'));
        debug_assert_eq!(self.source.as_bytes().get(block.span.end - 1), Some(&b'}'));
        let close_brace_byte_start = block.span.end - 1;
        let open_brace_byte_end = block.span.start + 1;

        // skip trivia before `{` so outer scope items don't leak in (like blank lines in struct bodies)
        while self.trivia_cursor < self.trivia.len()
            && self.trivia[self.trivia_cursor].span.start < open_brace_byte_end
        {
            self.trivia_cursor += 1;
        }

        let has_inner_trivia = self.trivia_cursor < self.trivia.len()
            && self.trivia[self.trivia_cursor].span.start >= open_brace_byte_end
            && self.trivia[self.trivia_cursor].span.start < close_brace_byte_start;

        if block.node.stmts.is_empty() && block.node.tail.is_none() {
            if has_inner_trivia {
                self.write("{");
                self.writeln();
                self.indent();
                self.emit_trivia_before(close_brace_byte_start);
                self.dedent();
                self.write_indent();
                self.write("}");
            } else {
                self.write("{}");
            }
            return;
        }

        if allow_compact
            && block.node.stmts.is_empty()
            && let Some(tail) = &block.node.tail
            && !has_inner_trivia
            && !expr_has_block(&tail.node)
        {
            let compact = self.try_single_line(|p| {
                p.write("{ ");
                p.format_expr(&tail.node);
                p.write(" }");
            });
            if compact {
                return;
            }
        }

        self.write("{");
        self.writeln();
        self.indent();

        let stmts_len = block.node.stmts.len();
        for (i, stmt_node) in block.node.stmts.iter().enumerate() {
            self.emit_trivia_before(stmt_node.span.start);
            self.format_stmt(stmt_node);
            let next_byte_start = if i + 1 < stmts_len {
                block.node.stmts[i + 1].span.start
            } else if let Some(tail) = &block.node.tail {
                tail.span.start
            } else {
                close_brace_byte_start
            };
            self.emit_trailing_trivia(stmt_node.span.end, next_byte_start);
        }

        if let Some(tail) = &block.node.tail {
            self.emit_trivia_before(tail.span.start);
            self.write_indent();
            self.format_expr(&tail.node);
            self.writeln();
            self.emit_trailing_trivia(tail.span.end, close_brace_byte_start);
        }

        self.emit_trivia_before(close_brace_byte_start);
        self.dedent();
        self.write_indent();
        self.write("}");
    }

    pub fn format_program(&mut self, program: &ast::Program) {
        for (i, stmt) in program.stmts.iter().enumerate() {
            self.emit_trivia_before(stmt.span.start);

            self.format_stmt(stmt);

            let next_byte_start = if i + 1 < program.stmts.len() {
                program.stmts[i + 1].span.start
            } else {
                self.source.len()
            };
            self.emit_trailing_trivia(stmt.span.end, next_byte_start);

            if i + 1 < program.stmts.len()
                && needs_blank_line_between(&stmt.node, &program.stmts[i + 1].node)
                && !self.buf.ends_with("\n\n")
            {
                self.writeln();
            }
        }

        self.emit_trivia_before(self.source.len());
    }
}
