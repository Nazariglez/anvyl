use anvyx_frontend::ast;

use super::Printer;

impl Printer<'_> {
    pub(super) fn format_pattern(&mut self, pat: &ast::Pattern) {
        match pat {
            ast::Pattern::Ident(id) => self.write_fmt(id),
            ast::Pattern::VarIdent(id) => {
                self.write("var ");
                self.write_fmt(id);
            }
            ast::Pattern::Wildcard => self.write("_"),
            ast::Pattern::Nil => self.write("nil"),
            ast::Pattern::Rest => self.write(".."),
            ast::Pattern::Lit(lit) => self.format_lit(lit),
            ast::Pattern::Optional(inner) => {
                self.format_pattern(&inner.node);
                self.write("?");
            }
            ast::Pattern::Or(pats) => {
                for (i, p) in pats.iter().enumerate() {
                    if i > 0 {
                        self.write(" | ");
                    }
                    self.format_pattern(&p.node);
                }
            }
            ast::Pattern::Tuple(pats) => self.format_tuple_pattern_args(pats),
            ast::Pattern::Struct { name, fields } => {
                self.write_fmt(name);
                self.format_struct_pattern_args(fields, false);
            }
            ast::Pattern::EnumUnit { qualifier, variant } => {
                self.write_fmt(qualifier);
                self.write(".");
                self.write_fmt(variant);
            }
            ast::Pattern::EnumTuple {
                qualifier,
                variant,
                fields,
            } => {
                self.write_fmt(qualifier);
                self.write(".");
                self.write_fmt(variant);
                self.format_tuple_pattern_args(fields);
            }
            ast::Pattern::EnumStruct {
                qualifier,
                variant,
                fields,
                has_rest,
            } => {
                self.write_fmt(qualifier);
                self.write(".");
                self.write_fmt(variant);
                self.format_struct_pattern_args(fields, *has_rest);
            }
            ast::Pattern::InferredEnumUnit { variant } => {
                self.write(".");
                self.write_fmt(variant);
            }
            ast::Pattern::InferredEnumTuple { variant, fields } => {
                self.write(".");
                self.write_fmt(variant);
                self.format_tuple_pattern_args(fields);
            }
            ast::Pattern::InferredEnumStruct {
                variant,
                fields,
                has_rest,
            } => {
                self.write(".");
                self.write_fmt(variant);
                self.format_struct_pattern_args(fields, *has_rest);
            }
            ast::Pattern::Range {
                start,
                end,
                inclusive,
            } => {
                if let Some(s) = start {
                    self.format_lit(s);
                }
                self.write(if *inclusive { "..=" } else { ".." });
                if let Some(e) = end {
                    self.format_lit(e);
                }
            }
        }
    }

    fn format_tuple_pattern_args(&mut self, fields: &[ast::PatternNode]) {
        self.write("(");
        for (i, p) in fields.iter().enumerate() {
            if i > 0 {
                self.write(", ");
            }
            self.format_pattern(&p.node);
        }
        self.write(")");
    }

    fn format_struct_pattern_args(
        &mut self,
        fields: &[(ast::Ident, ast::PatternNode)],
        has_rest: bool,
    ) {
        self.write(" { ");
        for (i, (name, p)) in fields.iter().enumerate() {
            if i > 0 {
                self.write(", ");
            }
            self.write_fmt(name);
            self.write(": ");
            self.format_pattern(&p.node);
        }
        if has_rest {
            if !fields.is_empty() {
                self.write(", ");
            }
            self.write("..");
        }
        self.write(" }");
    }
}
