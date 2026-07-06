use super::helpers::{parse_program, parse_program_err};
use crate::ast;

#[test]
fn doc_on_func() {
    let prog = parse_program(
        r"
        /// A function.
        fn foo() {}
        ",
    );
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    assert_eq!(func_node.node.doc.as_deref(), Some("A function."));
}

#[test]
fn doc_on_struct() {
    let prog = parse_program(
        r"
        /// A struct.
        struct S { x: int }
        ",
    );
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Aggregate(struct_node) = &prog.stmts[0].node else {
        panic!("expected Aggregate");
    };
    assert_eq!(struct_node.node.doc.as_deref(), Some("A struct."));
}

#[test]
fn doc_on_struct_field() {
    let prog = parse_program("struct S { /// A field.\n x: int }");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Aggregate(struct_node) = &prog.stmts[0].node else {
        panic!("expected Aggregate");
    };
    assert_eq!(struct_node.node.fields.len(), 1);
    assert_eq!(struct_node.node.fields[0].doc.as_deref(), Some("A field."));
}

#[test]
fn doc_on_struct_method() {
    let prog = parse_program("struct S { /// A method.\n fn m(self) {} }");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Aggregate(struct_node) = &prog.stmts[0].node else {
        panic!("expected Aggregate");
    };
    assert_eq!(struct_node.node.methods.len(), 1);
    assert_eq!(
        struct_node.node.methods[0].doc.as_deref(),
        Some("A method.")
    );
}

#[test]
fn doc_on_enum() {
    let prog = parse_program(
        r"
        /// An enum.
        enum E { V }
        ",
    );
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Enum(enum_node) = &prog.stmts[0].node else {
        panic!("expected Enum");
    };
    assert_eq!(enum_node.node.doc.as_deref(), Some("An enum."));
}

#[test]
fn doc_on_enum_variant() {
    let prog = parse_program("enum E { /// A variant.\n V }");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Enum(enum_node) = &prog.stmts[0].node else {
        panic!("expected Enum");
    };
    assert_eq!(enum_node.node.variants.len(), 1);
    assert_eq!(
        enum_node.node.variants[0].doc.as_deref(),
        Some("A variant.")
    );
}

#[test]
fn doc_on_enum_variant_tuple() {
    let prog = parse_program("enum E { /// A tuple variant.\n V(int) }");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Enum(enum_node) = &prog.stmts[0].node else {
        panic!("expected Enum");
    };
    assert_eq!(enum_node.node.variants.len(), 1);
    assert_eq!(
        enum_node.node.variants[0].doc.as_deref(),
        Some("A tuple variant.")
    );
}

#[test]
fn doc_on_enum_variant_struct() {
    let prog = parse_program("enum E { /// A struct variant.\n V { x: int } }");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Enum(enum_node) = &prog.stmts[0].node else {
        panic!("expected Enum");
    };
    assert_eq!(enum_node.node.variants.len(), 1);
    assert_eq!(
        enum_node.node.variants[0].doc.as_deref(),
        Some("A struct variant.")
    );
}

#[test]
fn doc_on_const() {
    let prog = parse_program(
        r"
        /// A constant.
        const N: int = 1;
        ",
    );
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Const(const_node) = &prog.stmts[0].node else {
        panic!("expected Const");
    };
    assert_eq!(const_node.node.doc.as_deref(), Some("A constant."));
}

#[test]
fn doc_on_extern_fn() {
    let prog = parse_program(
        r"
        /// An extern function.
        extern fn f();
        ",
    );
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::ExternFunc(extern_fn_node) = &prog.stmts[0].node else {
        panic!("expected ExternFunc");
    };
    assert_eq!(
        extern_fn_node.node.doc.as_deref(),
        Some("An extern function.")
    );
}

#[test]
fn doc_on_extern_type() {
    let prog = parse_program(
        r"
        /// An extern type.
        extern type T;
        ",
    );
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::ExternType(extern_type_node) = &prog.stmts[0].node else {
        panic!("expected ExternType");
    };
    assert_eq!(
        extern_type_node.node.doc.as_deref(),
        Some("An extern type.")
    );
}

#[test]
fn doc_on_extern_type_field() {
    let prog = parse_program("extern type T { /// A field.\n x: int; }");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::ExternType(extern_type_node) = &prog.stmts[0].node else {
        panic!("expected ExternType");
    };
    assert_eq!(extern_type_node.node.members.len(), 1);
    let ast::ExternTypeMember::Field { doc, name, .. } = &extern_type_node.node.members[0] else {
        panic!("expected Field");
    };
    assert_eq!(doc.as_deref(), Some("A field."));
    assert_eq!(name.0.as_ref(), "x");
}

#[test]
fn doc_on_extern_type_method() {
    let prog = parse_program("extern type T { /// A method.\n fn m(self); }");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::ExternType(extern_type_node) = &prog.stmts[0].node else {
        panic!("expected ExternType");
    };
    assert_eq!(extern_type_node.node.members.len(), 1);
    let ast::ExternTypeMember::Method { doc, name, .. } = &extern_type_node.node.members[0] else {
        panic!("expected Method");
    };
    assert_eq!(doc.as_deref(), Some("A method."));
    assert_eq!(name.0.as_ref(), "m");
}

#[test]
fn doc_on_extern_type_static() {
    let prog = parse_program("extern type T { /// A static method.\n fn new() -> T; }");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::ExternType(extern_type_node) = &prog.stmts[0].node else {
        panic!("expected ExternType");
    };
    assert_eq!(extern_type_node.node.members.len(), 1);
    let ast::ExternTypeMember::StaticMethod { doc, name, .. } = &extern_type_node.node.members[0]
    else {
        panic!("expected StaticMethod");
    };
    assert_eq!(doc.as_deref(), Some("A static method."));
    assert_eq!(name.0.as_ref(), "new");
}

#[test]
fn doc_on_extend_method() {
    let prog = parse_program("extend int { /// An extend method.\n fn m(self) {} }");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Extend(extend_node) = &prog.stmts[0].node else {
        panic!("expected Extend");
    };
    assert_eq!(extend_node.node.methods.len(), 1);
    assert_eq!(
        extend_node.node.methods[0].node.doc.as_deref(),
        Some("An extend method.")
    );
}

#[test]
fn doc_multiline() {
    let prog = parse_program(
        r"
        /// Line 1.
        /// Line 2.
        fn f() {}
        ",
    );
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    assert_eq!(func_node.node.doc.as_deref(), Some("Line 1.\nLine 2."));
}

#[test]
fn doc_multiline_with_empty() {
    let prog = parse_program(
        r"
        /// Line 1.
        ///
        /// Line 3.
        fn f() {}
        ",
    );
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    assert_eq!(func_node.node.doc.as_deref(), Some("Line 1.\n\nLine 3."));
}

#[test]
fn doc_absent() {
    let prog = parse_program("fn f() {}");
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    assert_eq!(func_node.node.doc, None);
}

#[test]
fn four_slashes_not_doc() {
    let prog = parse_program(
        r"
        //// not a doc comment
        fn f() {}
        ",
    );
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    assert_eq!(func_node.node.doc, None);
}

#[test]
fn blank_line_does_not_break_doc() {
    let prog = parse_program(
        r"
        /// A doc.

        fn f() {}
        ",
    );
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    // Blank line between doc and declaration does NOT break doc attachment
    assert_eq!(func_node.node.doc.as_deref(), Some("A doc."));
}

#[test]
fn regular_comment_does_not_break_doc() {
    let prog = parse_program(
        r"
        /// A doc.
        // regular comment
        fn f() {}
        ",
    );
    assert_eq!(prog.stmts.len(), 1);
    let ast::Stmt::Func(func_node) = &prog.stmts[0].node else {
        panic!("expected Func");
    };
    // Regular comment does NOT break doc attachment
    assert_eq!(func_node.node.doc.as_deref(), Some("A doc."));
}
