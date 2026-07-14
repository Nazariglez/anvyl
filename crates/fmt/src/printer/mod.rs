use std::{collections::HashMap, fmt::Write};

use anvyx_frontend::{
    ast,
    lexer::{LexedToken, Token},
};

use super::trivia::{TriviaItem, TriviaKind};

mod decl;
mod expr;
mod pattern;
mod stmt;
mod types;

const MAX_WIDTH: usize = 100;
const MISPLACED_DIRECTIVE: &str =
    "skip directive must precede a statement in the same statement list";

#[derive(Clone, Copy)]
struct Snapshot {
    buf_len: usize,
    trivia_cursor: usize,
    indent: u32,
    pending_directive: Option<usize>,
    directive_error: Option<DirectiveError>,
}

#[derive(Clone, Copy)]
pub(super) struct DirectiveError {
    pos: usize,
    message: &'static str,
}

impl std::fmt::Display for DirectiveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "byte {}: {}", self.pos, self.message)
    }
}

fn expr_has_block(expr: &ast::Expr) -> bool {
    match &expr.kind {
        ast::ExprKind::If(_) | ast::ExprKind::Match(_) | ast::ExprKind::Block(_) => true,
        ast::ExprKind::Lambda(l) => matches!(l.node.body.node.kind, ast::ExprKind::Block(_)),
        _ => false,
    }
}

fn push_escaped_lit_char(out: &mut String, ch: char, quote: char) {
    match ch {
        '\n' => out.push_str("\\n"),
        '\t' => out.push_str("\\t"),
        '\r' => out.push_str("\\r"),
        '\\' => out.push_str("\\\\"),
        '"' if quote == '"' => out.push_str("\\\""),
        '\'' if quote == '\'' => out.push_str("\\'"),
        '\0' if quote == '"' => out.push_str("\\0"),
        other => out.push(other),
    }
}

fn escape_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for ch in s.chars() {
        push_escaped_lit_char(&mut out, ch, '"');
    }
    out
}

fn escape_char(ch: char) -> String {
    let mut out = String::new();
    push_escaped_lit_char(&mut out, ch, '\'');
    out
}

// keep imports and consts grouped and separate everything else
fn needs_blank_line_between(a: &ast::Stmt, b: &ast::Stmt) -> bool {
    let same_group = (matches!(a, ast::Stmt::Import(_)) && matches!(b, ast::Stmt::Import(_)))
        || (matches!(a, ast::Stmt::Const(_)) && matches!(b, ast::Stmt::Const(_)));
    !same_group
}

pub(super) struct Printer<'a> {
    source: &'a str,
    trivia: &'a [TriviaItem],
    tokens: &'a [LexedToken],
    trivia_cursor: usize,
    buf: String,
    indent: u32,
    type_var_names: HashMap<ast::TypeVarId, String>,
    const_param_names: HashMap<ast::ConstParamId, String>,
    pending_directive: Option<usize>,
    directive_error: Option<DirectiveError>,
}

impl<'a> Printer<'a> {
    pub(super) fn new(source: &'a str, trivia: &'a [TriviaItem], tokens: &'a [LexedToken]) -> Self {
        let estimated_capacity = source.len() + source.len() / 4;
        Self {
            source,
            trivia,
            tokens,
            trivia_cursor: 0,
            buf: String::with_capacity(estimated_capacity),
            indent: 0,
            type_var_names: HashMap::new(),
            const_param_names: HashMap::new(),
            pending_directive: None,
            directive_error: None,
        }
    }

    pub(super) fn finish(self) -> String {
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

    fn format_param_prefix(&mut self, escape: ast::EscapeMode, cast_accept: bool) {
        if escape.is_escaping() {
            self.write("escaping ");
        }
        if cast_accept {
            self.write("as ");
        }
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

    fn format_conditional_pattern_access(&mut self, access: ast::ConditionalPatternAccess) {
        self.write(access.keyword());
    }

    fn format_mutability(&mut self, mutability: ast::Mutability) {
        self.write(mutability.keyword());
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
            pending_directive: self.pending_directive,
            directive_error: self.directive_error,
        }
    }

    fn restore(&mut self, snap: Snapshot) {
        self.buf.truncate(snap.buf_len);
        self.trivia_cursor = snap.trivia_cursor;
        self.indent = snap.indent;
        self.pending_directive = snap.pending_directive;
        self.directive_error = snap.directive_error;
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

    fn format_return_type(&mut self, ret: &ast::ReturnSpec) {
        if !ret.is_implicit_void() {
            self.write(" -> ");
            self.format_return_spec_tail(ret);
        }
    }

    fn format_return_spec_tail(&mut self, ret: &ast::ReturnSpec) {
        if ret.is_place() {
            self.write("ref ");
        }
        if ret.is_iter() {
            self.write("iter");
        } else {
            self.format_type(&ret.ty());
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

    fn with_type_param_names(
        &mut self,
        type_params: &[ast::TypeParam],
        const_params: &[ast::ConstParam],
        f: impl FnOnce(&mut Self),
    ) {
        let saved_type_vars = std::mem::take(&mut self.type_var_names);
        let saved_const_params = std::mem::take(&mut self.const_param_names);
        self.populate_type_param_names(type_params, const_params);
        f(self);
        self.type_var_names = saved_type_vars;
        self.const_param_names = saved_const_params;
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
                TriviaKind::SkipDirective => {
                    let directive_pos = item.span.start;
                    self.write_indent();
                    self.write(&item.text);
                    self.writeln();
                    if self.pending_directive.is_some() {
                        self.record_directive_error(
                            directive_pos,
                            "duplicate skip directive before statement",
                        );
                    } else {
                        self.pending_directive = Some(directive_pos);
                    }
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
        // struct/enum/extend bodies don't consume their own trivia
        self.discard_trivia_before(prev_end);

        while self.trivia_cursor < self.trivia.len() {
            let item = &self.trivia[self.trivia_cursor];
            if item.span.start >= next_start {
                break;
            }
            if self.source[prev_end..item.span.start].contains('\n') {
                break;
            }

            // remove the trailing newline that format_stmt wrote so
            // the comment lands on the same line
            if self.buf.ends_with('\n') {
                self.buf.pop();
            }
            self.write(" ");
            self.write(&item.text);
            self.writeln();
            self.trivia_cursor += 1;
        }
    }

    fn discard_trivia_before(&mut self, end: usize) {
        while self.trivia_cursor < self.trivia.len()
            && self.trivia[self.trivia_cursor].span.start < end
        {
            let item = &self.trivia[self.trivia_cursor];
            let kind = item.kind;
            let pos = item.span.start;
            self.trivia_cursor += 1;
            if kind == TriviaKind::SkipDirective {
                self.record_directive_error(pos, MISPLACED_DIRECTIVE);
            }
        }
    }

    fn record_directive_error(&mut self, pos: usize, message: &'static str) {
        if self.directive_error.is_none() {
            self.directive_error = Some(DirectiveError { pos, message });
        }
    }

    fn reject_pending_directive(&mut self) {
        if let Some(pos) = self.pending_directive.take() {
            self.record_directive_error(pos, MISPLACED_DIRECTIVE);
        }
    }

    fn statement_source_start(&self, stmt: &ast::StmtNode, lower_bound: usize) -> usize {
        let first = self
            .tokens
            .partition_point(|(_, span)| span.start() < lower_bound);
        self.tokens[first..]
            .iter()
            .take_while(|(_, span)| span.start() <= stmt.span.start)
            .find(|(token, _)| !matches!(token, Token::Semicolon))
            .map_or(stmt.span.start, |(_, span)| span.start())
    }

    fn statement_source_end(&self, stmt: &ast::StmtNode) -> usize {
        let next = self
            .tokens
            .partition_point(|(_, span)| span.start() < stmt.span.end);
        self.tokens
            .get(next)
            .filter(|(token, _)| matches!(token, Token::Semicolon))
            .map_or(stmt.span.end, |(_, span)| span.end())
    }

    fn reject_directives_before(&mut self, end: usize) {
        let pos = self.trivia[self.trivia_cursor..]
            .iter()
            .take_while(|item| item.span.start < end)
            .find(|item| item.kind == TriviaKind::SkipDirective)
            .map(|item| item.span.start);
        if let Some(pos) = pos {
            self.record_directive_error(pos, MISPLACED_DIRECTIVE);
        }
    }

    fn format_statement(&mut self, stmt: &ast::StmtNode, source_start: usize, next_start: usize) {
        self.emit_trivia_before(source_start);

        if let Some(directive_pos) = self.pending_directive.take()
            && self.format_raw_statement(stmt, source_start, next_start, directive_pos)
        {
            return;
        }

        self.emit_trivia_before(stmt.span.start);
        self.reject_pending_directive();
        self.format_stmt(stmt);
        self.emit_trailing_trivia(stmt.span.end, next_start);
        self.reject_directives_before(self.statement_source_end(stmt));
    }

    fn format_raw_statement(
        &mut self,
        stmt: &ast::StmtNode,
        source_start: usize,
        next_start: usize,
        directive_pos: usize,
    ) -> bool {
        let line_start = self.source[..source_start]
            .rfind('\n')
            .map_or(0, |pos| pos + 1);
        let source_end = self.statement_source_end(stmt);
        let line_end = self.source[source_end..]
            .find('\n')
            .map_or(self.source.len(), |offset| source_end + offset + 1);

        if next_start < line_end {
            self.record_directive_error(
                directive_pos,
                "skipped statement must own its final physical line",
            );
            return false;
        }

        self.validate_raw_statement(stmt, line_end);
        self.trivia_cursor +=
            self.trivia[self.trivia_cursor..].partition_point(|item| item.span.start < line_end);
        self.write(&self.source[line_start..line_end]);
        true
    }

    fn validate_raw_statement(&mut self, stmt: &ast::StmtNode, line_end: usize) {
        let snap = self.snapshot();
        self.format_stmt(stmt);
        self.emit_trailing_trivia(stmt.span.end, line_end);
        self.discard_trivia_before(line_end);
        let error = self.directive_error;
        self.restore(snap);
        self.directive_error = self.directive_error.or(error);
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
            ast::Lit::Char(ch) => {
                self.write("'");
                self.write(&escape_char(*ch));
                self.write("'");
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
        let close_start = block.span.end - 1;
        let open_end = block.span.start + 1;

        // Discard outer-scope trivia before `{` so it cannot leak into the block.
        self.discard_trivia_before(open_end);

        let has_inner_trivia = self.trivia_cursor < self.trivia.len()
            && self.trivia[self.trivia_cursor].span.start >= open_end
            && self.trivia[self.trivia_cursor].span.start < close_start;

        if block.node.stmts.is_empty() && block.node.tail.is_none() {
            if has_inner_trivia {
                self.write("{");
                self.writeln();
                self.indent();
                self.emit_trivia_before(close_start);
                self.reject_pending_directive();
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

        for (i, stmt) in block.node.stmts.iter().enumerate() {
            let lower = if i == 0 {
                open_end
            } else {
                block.node.stmts[i - 1].span.end
            };
            let start = self.statement_source_start(stmt, lower);
            let next = if i + 1 < block.node.stmts.len() {
                self.statement_source_start(&block.node.stmts[i + 1], stmt.span.end)
            } else if let Some(tail) = &block.node.tail {
                tail.span.start
            } else {
                close_start
            };
            self.format_statement(stmt, start, next);
        }

        if let Some(tail) = &block.node.tail {
            self.emit_trivia_before(tail.span.start);
            self.reject_pending_directive();
            self.write_indent();
            self.format_expr(&tail.node);
            self.writeln();
            self.emit_trailing_trivia(tail.span.end, close_start);
        }

        self.emit_trivia_before(close_start);
        self.reject_pending_directive();
        self.dedent();
        self.write_indent();
        self.write("}");
    }

    pub(super) fn format_program(&mut self, program: &ast::Program) -> Result<(), DirectiveError> {
        for (i, stmt) in program.stmts.iter().enumerate() {
            let lower = if i == 0 {
                0
            } else {
                program.stmts[i - 1].span.end
            };
            let start = self.statement_source_start(stmt, lower);
            let next = if i + 1 < program.stmts.len() {
                self.statement_source_start(&program.stmts[i + 1], stmt.span.end)
            } else {
                self.source.len()
            };
            self.format_statement(stmt, start, next);

            if i + 1 < program.stmts.len()
                && needs_blank_line_between(&stmt.node, &program.stmts[i + 1].node)
                && !self.buf.ends_with("\n\n")
            {
                self.writeln();
            }
        }

        self.emit_trivia_before(self.source.len());
        self.reject_pending_directive();
        self.directive_error.map_or(Ok(()), Err)
    }
}
