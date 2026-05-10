use anvyx_frontend::ast;

use super::Printer;

impl Printer<'_> {
    fn format_binding(&mut self, b: &ast::Binding) {
        self.write_indent();
        match b.mutability {
            ast::Mutability::Immutable => self.write("let "),
            ast::Mutability::Mutable => self.write("var "),
        }
        self.format_pattern(&b.pattern.node);
        if let Some(ty) = &b.ty {
            self.write(": ");
            self.format_type(ty);
        }
        self.write(" = ");
        self.format_expr(&b.value.node);
        self.write(";");
        self.writeln();
    }

    fn format_let_else(&mut self, le: &ast::LetElse) {
        self.write_indent();
        self.format_pattern_head(le.head);
        self.write(" ");
        self.format_pattern(&le.pattern.node);
        self.write(" = ");
        self.format_expr(&le.value.node);
        self.write(" else ");
        self.format_block(&le.else_block);
        self.writeln();
    }

    fn format_return(&mut self, r: &ast::Return) {
        self.write_indent();
        self.write("return");
        if let Some(value) = &r.value {
            self.write(" ");
            self.format_expr(&value.node);
        }
        self.write(";");
        self.writeln();
    }

    fn format_while(&mut self, w: &ast::While) {
        self.write_indent();
        self.write("while ");
        self.format_expr(&w.cond.node);
        self.write(" ");
        self.format_block(&w.body);
        self.writeln();
    }

    fn format_while_let(&mut self, wl: &ast::WhileLet) {
        self.write_indent();
        self.write("while ");
        self.format_pattern_head(wl.head);
        self.write(" ");
        self.format_pattern(&wl.pattern.node);
        self.write(" = ");
        self.format_expr(&wl.value.node);
        self.write(" ");
        self.format_block(&wl.body);
        self.writeln();
    }

    fn format_for(&mut self, f: &ast::For) {
        self.write_indent();
        self.write("for ");
        self.format_pattern(&f.pattern.node);
        self.write(" in ");
        if f.reversed {
            self.write("rev ");
        }

        // TODO: ranges should be able to be used witouth parens
        // `x..` needs parens in `for ... in` so the body block doesn't get parsed as its end
        let needs_parens = matches!(
            f.iterable.node.kind,
            ast::ExprKind::Range(ref r) if matches!(r.node, ast::Range::From { .. })
        );
        if needs_parens {
            self.write("(");
            self.format_expr(&f.iterable.node);
            self.write(")");
        } else {
            self.format_expr(&f.iterable.node);
        }
        if let Some(step) = &f.step {
            self.write(" step ");
            self.format_expr(&step.node);
        }
        self.write(" ");
        self.format_block(&f.body);
        self.writeln();
    }

    fn format_defer(&mut self, d: &ast::Defer) {
        self.write_indent();
        self.write("defer ");
        match &d.body {
            ast::DeferBody::Expr(e) => {
                self.format_expr(&e.node);
                self.write(";");
            }
            ast::DeferBody::Block(b) => {
                self.format_block(b);
            }
        }
        self.writeln();
    }

    pub(super) fn format_stmt(&mut self, stmt: &ast::StmtNode) {
        match &stmt.node {
            ast::Stmt::Func(f) => self.format_func(&f.node),
            ast::Stmt::ExternFunc(ef) => self.format_extern_func(&ef.node),
            ast::Stmt::ExternType(et) => self.format_extern_type(&et.node),
            ast::Stmt::Import(imp) => self.format_import(&imp.node),
            ast::Stmt::Const(c) => self.format_const(&c.node),
            ast::Stmt::TypeAlias(alias) => self.format_type_alias(&alias.node),
            ast::Stmt::Aggregate(s) => self.format_aggregate(&s.node),
            ast::Stmt::Enum(e) => self.format_enum(&e.node),
            ast::Stmt::Extend(ext) => self.format_extend(&ext.node),
            ast::Stmt::Binding(b) => self.format_binding(&b.node),
            ast::Stmt::LetElse(le) => self.format_let_else(&le.node),
            ast::Stmt::Return(r) => self.format_return(&r.node),
            ast::Stmt::While(w) => self.format_while(&w.node),
            ast::Stmt::WhileLet(wl) => self.format_while_let(&wl.node),
            ast::Stmt::For(f) => self.format_for(&f.node),
            ast::Stmt::Defer(d) => self.format_defer(&d.node),
            ast::Stmt::Break => {
                self.write_indent();
                self.write("break;");
                self.writeln();
            }
            ast::Stmt::Continue => {
                self.write_indent();
                self.write("continue;");
                self.writeln();
            }
            ast::Stmt::Expr(e) => {
                self.write_indent();
                self.format_expr(&e.node);
                self.write(";");
                self.writeln();
            }
        }
    }
}
