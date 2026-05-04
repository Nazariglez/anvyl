use anvyx_frontend::ast;

use super::{Printer, escape_string};

const ASSIGN_PREC: u8 = 1;
const TERNARY_PREC: u8 = 2;
const LOGICAL_OR_PREC: u8 = 3;
const POSTFIX_PREC: u8 = 17;

fn binary_op_precedence(op: ast::BinaryOp) -> u8 {
    match op {
        ast::BinaryOp::Mul | ast::BinaryOp::Div | ast::BinaryOp::Rem => 14,
        ast::BinaryOp::Add | ast::BinaryOp::Sub => 13,
        ast::BinaryOp::Shl | ast::BinaryOp::Shr => 11,
        ast::BinaryOp::LessThan
        | ast::BinaryOp::GreaterThan
        | ast::BinaryOp::LessThanEq
        | ast::BinaryOp::GreaterThanEq => 10,
        ast::BinaryOp::Eq | ast::BinaryOp::NotEq => 9,
        ast::BinaryOp::BitAnd => 8,
        ast::BinaryOp::Xor => 7,
        ast::BinaryOp::BitOr => 6,
        ast::BinaryOp::And => 5,
        ast::BinaryOp::Coalesce => 4,
        ast::BinaryOp::Or => LOGICAL_OR_PREC,
    }
}

fn expr_precedence(expr: &ast::Expr) -> Option<u8> {
    match &expr.kind {
        ast::ExprKind::Binary(node) => Some(binary_op_precedence(node.node.op)),
        ast::ExprKind::Range(_) => Some(12),
        ast::ExprKind::Cast(_) => Some(15),
        ast::ExprKind::Unary(_) => Some(16),
        ast::ExprKind::Ternary(_) => Some(TERNARY_PREC),
        ast::ExprKind::Assign(_) => Some(ASSIGN_PREC),
        _ => None,
    }
}

impl Printer<'_> {
    pub(super) fn format_expr(&mut self, expr: &ast::Expr) {
        self.format_expr_prec(expr, 0, false);
    }

    fn format_expr_prec(&mut self, expr: &ast::Expr, parent_prec: u8, is_right: bool) {
        let needs_parens = match expr_precedence(expr) {
            None => false,
            Some(child_prec) => {
                if is_right {
                    child_prec <= parent_prec
                } else {
                    child_prec < parent_prec
                }
            }
        };
        if needs_parens {
            self.write("(");
            self.format_expr_inner(expr);
            self.write(")");
        } else {
            self.format_expr_inner(expr);
        }
    }

    fn format_expr_inner(&mut self, expr: &ast::Expr) {
        match &expr.kind {
            ast::ExprKind::Ident(id) => self.write_fmt(id),
            ast::ExprKind::Lit(lit) => self.format_lit(lit),
            ast::ExprKind::Block(block) => self.format_block(block),

            ast::ExprKind::Binary(node) => {
                let prec = binary_op_precedence(node.node.op);
                let op = node.node.op;
                let fits = self.try_single_line(|p| {
                    p.format_expr_prec(&node.node.left.node, prec, false);
                    p.write(" ");
                    p.write_fmt(op);
                    p.write(" ");
                    p.format_expr_prec(&node.node.right.node, prec, true);
                });
                if !fits {
                    self.format_expr_prec(&node.node.left.node, prec, false);
                    self.writeln();
                    self.indent();
                    self.write_indent();
                    self.write_fmt(op);
                    self.write(" ");
                    self.format_expr_prec(&node.node.right.node, prec, true);
                    self.dedent();
                }
            }

            ast::ExprKind::Unary(node) => {
                self.write_fmt(node.node.op);
                self.format_expr_prec(&node.node.expr.node, 16, false);
            }

            ast::ExprKind::Assign(node) => {
                self.format_expr(&node.node.target.node);
                self.write(" ");
                self.write_fmt(node.node.op);
                self.write(" ");
                self.format_expr(&node.node.value.node);
            }

            ast::ExprKind::Call(node) => self.format_call(&node.node),

            ast::ExprKind::Field(node) => {
                self.format_expr_prec(&node.node.target.node, POSTFIX_PREC, false);
                self.write(if node.node.safe { "?." } else { "." });
                self.write_fmt(node.node.field);
            }

            ast::ExprKind::TupleIndex(node) => {
                self.format_expr_prec(&node.node.target.node, POSTFIX_PREC, false);
                self.write(".");
                self.write_fmt(node.node.index);
            }

            ast::ExprKind::Index(node) => {
                self.format_expr_prec(&node.node.target.node, POSTFIX_PREC, false);
                self.write(if node.node.safe { "?[" } else { "[" });
                self.format_expr(&node.node.index.node);
                self.write("]");
            }

            ast::ExprKind::If(node) => self.format_if(&node.node),
            ast::ExprKind::Ternary(node) => self.format_ternary(&node.node),
            ast::ExprKind::IfLet(node) => self.format_if_let(&node.node),
            ast::ExprKind::Match(node) => self.format_match(&node.node),

            ast::ExprKind::Tuple(exprs) => {
                self.format_comma_list("(", ")", exprs, |p, e| p.format_expr(&e.node));
            }

            ast::ExprKind::StructLiteral(node) => self.format_struct_literal_expr(&node.node),

            ast::ExprKind::ArrayLiteral(node) => {
                if node.node.elements.is_empty() {
                    self.write("[]");
                } else {
                    self.format_comma_list("[", "]", &node.node.elements, |p, e| {
                        p.format_expr(&e.node);
                    });
                }
            }

            ast::ExprKind::ArrayFill(node) => {
                self.write("[");
                self.format_expr(&node.node.value.node);
                self.write("; ");
                self.format_expr(&node.node.len.node);
                self.write("]");
            }

            ast::ExprKind::MapLiteral(node) => {
                if node.node.entries.is_empty() {
                    self.write("[:]");
                } else {
                    self.format_comma_list("[", "]", &node.node.entries, |p, (key, val)| {
                        p.format_expr(&key.node);
                        p.write(": ");
                        p.format_expr(&val.node);
                    });
                }
            }

            ast::ExprKind::Range(node) => match &node.node {
                ast::Range::Bounded {
                    start,
                    end,
                    inclusive,
                } => {
                    self.format_expr_prec(&start.node, 12, false);
                    self.write(if *inclusive { "..=" } else { ".." });
                    self.format_expr_prec(&end.node, 12, true);
                }
                ast::Range::From { start } => {
                    self.format_expr_prec(&start.node, 12, false);
                    self.write("..");
                }
                ast::Range::To { end, inclusive } => {
                    self.write(if *inclusive { "..=" } else { ".." });
                    self.format_expr_prec(&end.node, 12, true);
                }
            },

            ast::ExprKind::StringInterp(parts) => self.format_string_interp(parts),

            ast::ExprKind::Cast(node) => {
                self.format_expr_prec(&node.node.expr.node, 15, false);
                self.write(" as ");
                self.format_type(&node.node.target);
            }

            ast::ExprKind::Lambda(node) => self.format_lambda(&node.node),
            ast::ExprKind::InferredEnum(node) => self.format_inferred_enum_expr(&node.node),
            ast::ExprKind::IntrinsicCall(node) => {
                self.write("#");
                self.write_fmt(node.node.name);
                self.format_comma_list("(", ")", &node.node.args, |p, e| {
                    p.format_expr(&e.node);
                });
            }
        }
    }

    fn format_call(&mut self, call: &ast::Call) {
        self.format_expr_prec(&call.func.node, POSTFIX_PREC, false);
        if call.safe {
            self.write("?");
        }
        if !call.generic_args.is_empty() {
            self.format_generic_args(&call.generic_args);
        }
        if call.args.is_empty() {
            self.write("()");
            return;
        }
        self.format_comma_list("(", ")", &call.args, |p, arg| p.format_expr(&arg.node));
    }

    fn format_if(&mut self, if_node: &ast::If) {
        self.write("if ");
        self.format_expr(&if_node.cond.node);
        self.write(" ");
        self.format_block_expanded(&if_node.then_block);
        if let Some(else_block) = &if_node.else_block {
            self.format_else_branch(else_block);
        }
    }

    fn format_ternary_part(&mut self, expr: &ast::Expr, force_parens: bool) {
        if force_parens {
            self.write("(");
            self.format_expr(expr);
            self.write(")");
        } else {
            self.format_expr_prec(expr, TERNARY_PREC, false);
        }
    }

    fn format_ternary(&mut self, ternary: &ast::Ternary) {
        self.format_ternary_part(
            &ternary.cond.node,
            matches!(ternary.cond.node.kind, ast::ExprKind::Ternary(_)),
        );
        self.write(" ? ");
        self.format_ternary_part(
            &ternary.then_expr.node,
            matches!(ternary.then_expr.node.kind, ast::ExprKind::Ternary(_)),
        );
        self.write(" : ");
        if matches!(ternary.else_expr.node.kind, ast::ExprKind::Ternary(_)) {
            self.format_expr(&ternary.else_expr.node);
        } else {
            self.format_expr_prec(&ternary.else_expr.node, TERNARY_PREC, true);
        }
    }

    fn format_if_let(&mut self, il: &ast::IfLet) {
        self.write("if let ");
        self.format_pattern(&il.pattern.node);
        self.write(" = ");
        self.format_expr(&il.value.node);
        self.write(" ");
        self.format_block_expanded(&il.then_block);
        if let Some(else_block) = &il.else_block {
            self.format_else_branch(else_block);
        }
    }

    fn format_else_branch(&mut self, else_block: &ast::BlockNode) {
        if else_block.node.stmts.is_empty()
            && let Some(tail) = &else_block.node.tail
        {
            match &tail.node.kind {
                ast::ExprKind::If(nested) => {
                    self.write(" else ");
                    self.format_if(&nested.node);
                    return;
                }
                ast::ExprKind::IfLet(nested) => {
                    self.write(" else ");
                    self.format_if_let(&nested.node);
                    return;
                }
                _ => {}
            }
        }
        self.write(" else ");
        self.format_block_expanded(else_block);
    }

    fn format_match(&mut self, m: &ast::Match) {
        self.write("match ");
        self.format_expr(&m.scrutinee.node);
        self.write(" {");
        self.writeln();
        self.indent();
        for arm in &m.arms {
            self.write_indent();
            self.format_pattern(&arm.node.pattern.node);
            self.write(" => ");
            self.format_expr(&arm.node.body.node);
            self.write(",");
            self.writeln();
        }
        self.dedent();
        self.write_indent();
        self.write("}");
    }

    fn format_lambda_param(&mut self, param: &ast::LambdaParam) {
        if param.mutable {
            self.write("var ");
        }
        self.write_fmt(param.name);
        if let Some(ty) = &param.ty {
            self.write(": ");
            if param.cast_accept {
                self.write("as ");
            }
            self.format_type(ty);
        }
    }

    fn format_lambda_params(&mut self, params: &[ast::LambdaParam]) {
        for (i, p) in params.iter().enumerate() {
            if i > 0 {
                self.write(", ");
            }
            self.format_lambda_param(p);
        }
    }

    fn format_lambda(&mut self, lambda: &ast::Lambda) {
        if lambda.params.is_empty() {
            self.write("||");
            if let Some(ret) = &lambda.ret_type {
                self.write(" -> ");
                self.format_type(ret);
            }
            self.write(" ");
            self.format_expr(&lambda.body.node);
            return;
        }
        let fits = self.try_single_line(|p| {
            p.write("|");
            p.format_lambda_params(&lambda.params);
            p.write("|");
            if let Some(ret) = &lambda.ret_type {
                p.write(" -> ");
                p.format_type(ret);
            }
            p.write(" ");
            p.format_expr(&lambda.body.node);
        });
        if fits {
            return;
        }
        let params_fit = self.try_single_line(|p| {
            p.write("|");
            p.format_lambda_params(&lambda.params);
            p.write("|");
        });
        if !params_fit {
            self.write("|");
            self.writeln();
            self.indent();
            for param in &lambda.params {
                self.write_indent();
                self.format_lambda_param(param);
                self.write(",");
                self.writeln();
            }
            self.dedent();
            self.write_indent();
            self.write("|");
        }
        if let Some(ret) = &lambda.ret_type {
            self.write(" -> ");
            self.format_type(ret);
        }
        self.write(" ");
        self.format_expr(&lambda.body.node);
    }

    fn format_string_interp(&mut self, parts: &[ast::StringPart]) {
        self.write("f\"");
        for part in parts {
            match part {
                ast::StringPart::Text(s) => self.write(&escape_string(s)),
                ast::StringPart::Expr(expr, spec) => {
                    self.write("{");
                    let parens = matches!(expr.node.kind, ast::ExprKind::Ternary(_));
                    if parens {
                        self.write("(");
                    }
                    self.format_expr(&expr.node);
                    if parens {
                        self.write(")");
                    }
                    if let Some(spec) = spec {
                        self.write(":");
                        self.format_format_spec(&spec.node);
                    }
                    self.write("}");
                }
            }
        }
        self.write("\"");
    }

    fn format_format_spec(&mut self, spec: &ast::FormatSpec) {
        // `zero_pad` already means zero fill + right align, so skip both to avoid `0>04`.
        let align_from_zero_pad =
            spec.zero_pad && spec.fill == '0' && spec.align == Some(ast::FormatAlign::Right);

        if let Some(align) = spec.align
            && !align_from_zero_pad
        {
            if spec.fill != ' ' {
                self.buf.push(spec.fill);
            }
            match align {
                ast::FormatAlign::Left => self.write("<"),
                ast::FormatAlign::Right => self.write(">"),
                ast::FormatAlign::Center => self.write("^"),
            }
        }

        if matches!(spec.sign, ast::FormatSign::Always) {
            self.write("+");
        }

        if spec.zero_pad {
            self.write("0");
        }

        if let Some(w) = spec.width {
            self.write_fmt(w);
        }

        if let Some(p) = spec.precision {
            self.write(".");
            self.write_fmt(p);
        }

        match spec.kind {
            ast::FormatKind::Default => {}
            ast::FormatKind::Hex => self.write("x"),
            ast::FormatKind::HexUpper => self.write("X"),
            ast::FormatKind::Binary => self.write("b"),
            ast::FormatKind::Exp => self.write("e"),
            ast::FormatKind::ExpUpper => self.write("E"),
        }
    }

    fn write_field_value(&mut self, name: ast::Ident, value: &ast::Expr) {
        let is_shorthand = matches!(&value.kind, ast::ExprKind::Ident(id) if *id == name);
        self.write_fmt(name);
        if !is_shorthand {
            self.write(": ");
            self.format_expr(value);
        }
    }

    fn format_struct_literal_expr(&mut self, sl: &ast::StructLiteral) {
        if let Some(qualifier) = &sl.qualifier {
            self.write_fmt(qualifier);
            self.write(".");
        }
        self.write_fmt(sl.name);
        if !sl.generic_args.is_empty() {
            self.format_generic_args(&sl.generic_args);
        }
        if sl.fields.is_empty() {
            self.write(" {}");
            return;
        }
        self.format_brace_list(&sl.fields, |p, (name, value)| {
            p.write_field_value(*name, &value.node);
        });
    }

    fn format_inferred_enum_expr(&mut self, ie: &ast::InferredEnum) {
        self.write(".");
        self.write_fmt(ie.variant);
        match &ie.args {
            ast::InferredEnumArgs::Unit => {}
            ast::InferredEnumArgs::Tuple(args) => {
                if args.is_empty() {
                    self.write("()");
                } else {
                    self.format_comma_list("(", ")", args, |p, arg| p.format_expr(&arg.node));
                }
            }
            ast::InferredEnumArgs::Struct(fields) => {
                if fields.is_empty() {
                    self.write(" {}");
                } else {
                    self.format_brace_list(fields, |p, (name, value)| {
                        p.write_field_value(*name, &value.node);
                    });
                }
            }
        }
    }
}
