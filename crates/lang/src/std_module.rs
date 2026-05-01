use std::collections::HashMap;

use crate::{ExternDecl, ExternHandler, ExternTypeDecl};

fn format_params(out: &mut String, params: &[(&str, &str)], clean: bool) {
    for (i, (pname, pty)) in params.iter().enumerate() {
        if i > 0 {
            out.push_str(", ");
        }
        out.push_str(pname);
        out.push_str(": ");
        format_type(out, pty, clean);
    }
}

fn format_type(out: &mut String, ty: &str, clean: bool) {
    if clean {
        out.push_str(&clean_type(ty));
    } else {
        out.push_str(ty);
    }
}

fn clean_type(ty: &str) -> String {
    let ty = ty.trim();
    if let Some(inner) = ty
        .strip_prefix("Option<")
        .and_then(|rest| rest.strip_suffix('>'))
    {
        let inner = clean_type(inner);
        if matches!(
            inner.trim_end_matches('?'),
            "any" | "bool" | "float" | "int" | "string"
        ) {
            return format!("{inner}?");
        }
    }
    ty.to_string()
}

fn emit_doc(out: &mut String, doc: Option<&str>) {
    if let Some(doc) = doc {
        for line in doc.lines() {
            if line.is_empty() {
                out.push_str("///\n");
            } else {
                out.push_str("/// ");
                out.push_str(line);
                out.push('\n');
            }
        }
    }
}

pub struct StdModule {
    pub name: &'static str,
    pub anv_source: &'static str,
    pub exports: fn() -> Vec<ExternDecl>,
    pub type_exports: fn() -> Vec<ExternTypeDecl>,
    pub handlers: fn() -> HashMap<String, ExternHandler>,
    pub init: Option<fn()>,
}

impl StdModule {
    pub fn full_anv_source(&self) -> String {
        self.full_anv_source_inner(false)
    }

    pub fn full_clean_anv_source(&self) -> String {
        self.full_anv_source_inner(true)
    }

    fn full_anv_source_inner(&self, clean: bool) -> String {
        let mut out = String::new();
        for ty in (self.type_exports)() {
            let has_members = ty.has_init
                || !ty.fields.is_empty()
                || !ty.methods.is_empty()
                || !ty.statics.is_empty()
                || !ty.operators.is_empty();
            emit_doc(&mut out, ty.doc);
            if has_members {
                out.push_str("extern type ");
                out.push_str(ty.name);
                out.push_str(" {\n");
                if ty.has_init {
                    out.push_str("    init;\n");
                }
                for field in &ty.fields {
                    emit_doc(&mut out, field.doc);
                    out.push_str("    ");
                    out.push_str(field.name);
                    out.push_str(": ");
                    format_type(&mut out, field.ty, clean);
                    out.push_str(";\n");
                }
                for method in &ty.methods {
                    emit_doc(&mut out, method.doc);
                    out.push_str("    fn ");
                    out.push_str(method.name);
                    out.push('(');
                    let receiver_str = match method.receiver {
                        "var" => "var self",
                        _ => "self",
                    };
                    out.push_str(receiver_str);
                    for (pname, pty) in &method.params {
                        out.push_str(", ");
                        out.push_str(pname);
                        out.push_str(": ");
                        format_type(&mut out, pty, clean);
                    }
                    out.push(')');
                    if method.ret != "void" {
                        out.push_str(" -> ");
                        format_type(&mut out, method.ret, clean);
                    }
                    out.push_str(";\n");
                }
                for s in &ty.statics {
                    emit_doc(&mut out, s.doc);
                    out.push_str("    fn ");
                    out.push_str(s.name);
                    out.push('(');
                    format_params(&mut out, &s.params, clean);
                    out.push(')');
                    if s.ret != "void" {
                        out.push_str(" -> ");
                        format_type(&mut out, s.ret, clean);
                    }
                    out.push_str(";\n");
                }
                for op in &ty.operators {
                    let sym = match op.op {
                        "Add" => "+",
                        "Sub" | "Neg" => "-",
                        "Mul" => "*",
                        "Div" => "/",
                        "Rem" => "%",
                        "Eq" => "==",
                        other => panic!("unknown operator: {other}"),
                    };
                    let as_self =
                        |s: &'static str| -> &'static str { if s == ty.name { "Self" } else { s } };
                    let ret_str = as_self(op.ret);
                    match (op.rhs, op.lhs) {
                        (None, None) => {
                            // unary
                            out.push_str("    op -Self -> ");
                            out.push_str(ret_str);
                            out.push_str(";\n");
                        }
                        (Some(rhs), None) => {
                            // Self op rhs -> ret
                            out.push_str("    op Self ");
                            out.push_str(sym);
                            out.push(' ');
                            out.push_str(as_self(rhs));
                            out.push_str(" -> ");
                            out.push_str(ret_str);
                            out.push_str(";\n");
                        }
                        (None, Some(lhs)) => {
                            // lhs op Self -> ret
                            out.push_str("    op ");
                            out.push_str(as_self(lhs));
                            out.push(' ');
                            out.push_str(sym);
                            out.push_str(" Self -> ");
                            out.push_str(ret_str);
                            out.push_str(";\n");
                        }
                        (Some(_), Some(_)) => {
                            panic!("ExternOpDecl cannot have both rhs and lhs set");
                        }
                    }
                }
                out.push_str("}\n");
            } else {
                out.push_str("extern type ");
                out.push_str(ty.name);
                out.push_str(";\n");
            }
        }
        let exports = (self.exports)();
        for decl in &exports {
            emit_doc(&mut out, decl.doc);
            out.push_str("extern fn ");
            out.push_str(decl.name);
            out.push('(');
            format_params(&mut out, &decl.params, clean);
            out.push(')');
            if decl.ret != "void" {
                out.push_str(" -> ");
                format_type(&mut out, decl.ret, clean);
            }
            out.push_str(";\n");
        }
        if !exports.is_empty() && !self.anv_source.is_empty() {
            out.push('\n');
        }
        out.push_str(self.anv_source);
        out
    }
}

pub fn init_std_modules(modules: &[StdModule]) {
    for module in modules {
        if let Some(init) = module.init {
            init();
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{
        ExternDecl, ExternFieldDecl, ExternMethodDecl, ExternStaticMethodDecl, ExternTypeDecl, ast,
        lexer, parser,
    };

    fn parse_program(src: &str) -> ast::Program {
        let tokens = lexer::tokenize(src).unwrap_or_else(|e| panic!("tokenize failed: {e:?}"));
        parser::parse_ast(&tokens).unwrap_or_else(|e| panic!("parse failed: {e:?}"))
    }

    #[test]
    fn doc_round_trip_extern_type() {
        let module = super::StdModule {
            name: "test",
            anv_source: "",
            exports: || {
                vec![ExternDecl {
                    name: "free_fn",
                    params: vec![],
                    ret: "void",
                    doc: Some("A free function."),
                }]
            },
            type_exports: || {
                vec![ExternTypeDecl {
                    name: "Widget",
                    doc: Some("A widget type.\n\nHas multiple uses."),
                    has_init: false,
                    fields: vec![ExternFieldDecl {
                        name: "size",
                        ty: "int",
                        computed: false,
                        doc: Some("The widget size."),
                    }],
                    methods: vec![ExternMethodDecl {
                        name: "resize",
                        doc: Some("Resizes the widget."),
                        receiver: "var",
                        params: vec![("new_size", "int")],
                        ret: "void",
                    }],
                    statics: vec![ExternStaticMethodDecl {
                        name: "create",
                        doc: Some("Creates a new widget."),
                        params: vec![],
                        ret: "Widget",
                    }],
                    operators: vec![],
                }]
            },
            handlers: || HashMap::new(),
            init: None,
        };

        let source = module.full_anv_source();
        let prog = parse_program(&source);

        //statements: extern type + extern fn
        assert_eq!(prog.stmts.len(), 2);

        // Extern type
        let ast::Stmt::ExternType(ty) = &prog.stmts[0].node else {
            panic!("expected ExternType, got {:?}", prog.stmts[0].node);
        };
        assert_eq!(
            ty.node.doc.as_deref(),
            Some("A widget type.\n\nHas multiple uses.")
        );
        assert_eq!(ty.node.name.0.as_ref(), "Widget");

        // Field
        let ast::ExternTypeMember::Field { doc, name, .. } = &ty.node.members[0] else {
            panic!("expected Field");
        };
        assert_eq!(doc.as_deref(), Some("The widget size."));
        assert_eq!(name.0.as_ref(), "size");

        // Method
        let ast::ExternTypeMember::Method { doc, name, .. } = &ty.node.members[1] else {
            panic!("expected Method");
        };
        assert_eq!(doc.as_deref(), Some("Resizes the widget."));
        assert_eq!(name.0.as_ref(), "resize");

        // Static method
        let ast::ExternTypeMember::StaticMethod { doc, name, .. } = &ty.node.members[2] else {
            panic!("expected StaticMethod");
        };
        assert_eq!(doc.as_deref(), Some("Creates a new widget."));
        assert_eq!(name.0.as_ref(), "create");

        // Free function
        let ast::Stmt::ExternFunc(fn_decl) = &prog.stmts[1].node else {
            panic!("expected ExternFunc, got {:?}", prog.stmts[1].node);
        };
        assert_eq!(fn_decl.node.doc.as_deref(), Some("A free function."));
        assert_eq!(fn_decl.node.name.0.as_ref(), "free_fn");
    }

    #[test]
    fn doc_round_trip_absent() {
        let module = super::StdModule {
            name: "test",
            anv_source: "",
            exports: || vec![],
            type_exports: || {
                vec![ExternTypeDecl {
                    name: "Plain",
                    doc: None,
                    has_init: false,
                    fields: vec![ExternFieldDecl {
                        name: "x",
                        ty: "int",
                        computed: false,
                        doc: None,
                    }],
                    methods: vec![],
                    statics: vec![],
                    operators: vec![],
                }]
            },
            handlers: || HashMap::new(),
            init: None,
        };

        let source = module.full_anv_source();
        let prog = parse_program(&source);

        let ast::Stmt::ExternType(ty) = &prog.stmts[0].node else {
            panic!("expected ExternType");
        };
        assert_eq!(ty.node.doc, None);
        let ast::ExternTypeMember::Field { doc, .. } = &ty.node.members[0] else {
            panic!("expected Field");
        };
        assert_eq!(*doc, None);
    }

    #[test]
    fn doc_emit_snapshot() {
        let module = super::StdModule {
            name: "test",
            anv_source: "",
            exports: || {
                vec![ExternDecl {
                    name: "greet",
                    params: vec![("name", "string")],
                    ret: "void",
                    doc: Some("Says hello."),
                }]
            },
            type_exports: || {
                vec![ExternTypeDecl {
                    name: "Vec2",
                    doc: Some("A 2D vector."),
                    has_init: false,
                    fields: vec![
                        ExternFieldDecl {
                            name: "x",
                            ty: "float",
                            computed: false,
                            doc: Some("X component."),
                        },
                        ExternFieldDecl {
                            name: "y",
                            ty: "float",
                            computed: false,
                            doc: None,
                        },
                    ],
                    methods: vec![ExternMethodDecl {
                        name: "length",
                        doc: Some("Returns the magnitude."),
                        receiver: "self",
                        params: vec![],
                        ret: "float",
                    }],
                    statics: vec![],
                    operators: vec![],
                }]
            },
            handlers: || HashMap::new(),
            init: None,
        };

        let expected = "\
/// A 2D vector.
extern type Vec2 {
/// X component.
    x: float;
    y: float;
/// Returns the magnitude.
    fn length(self) -> float;
}
/// Says hello.
extern fn greet(name: string);
";
        assert_eq!(module.full_anv_source(), expected);
    }

    #[test]
    fn clean_source_uses_postfix_option_boundary_types() {
        let module = super::StdModule {
            name: "test",
            anv_source: "",
            exports: || {
                vec![ExternDecl {
                    name: "get",
                    params: vec![("key", "Option<string>")],
                    ret: "Option<Option<int>>",
                    doc: None,
                }]
            },
            type_exports: || vec![],
            handlers: || HashMap::new(),
            init: None,
        };

        assert_eq!(
            module.full_clean_anv_source(),
            "extern fn get(key: string?) -> int??;\n"
        );
    }
}
