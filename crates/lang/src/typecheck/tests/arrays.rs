use internment::Intern;

use super::helpers::{
    array_fill, array_literal, assert_expr_type, assign_expr, call_expr, dummy_span, func_decl,
    get_expr_id, ident_expr, index_expr, let_binding, lit_float, lit_int, lit_nil, lit_string,
    map_literal_expr, opt_type, program, reset_expr_ids, run_err, run_ok, safe_index_expr,
    slice_type, var_binding,
};
use crate::{
    ast::{
        ArrayLen, AssignOp, Block, BlockNode, Func, FuncNode, Ident, Mutability, Param, Stmt,
        StmtNode, Type, Visibility,
    },
    typecheck::error::DiagnosticKind,
};

fn ident(s: &str) -> Ident {
    Ident(Intern::new(s.to_string()))
}

// ---- array literal tests ----

#[test]
fn test_array_literal_unannotated_int() {
    reset_expr_ids();

    // let a = [1, 2, 3];
    let arr = array_literal(vec![lit_int(1), lit_int(2), lit_int(3)]);
    let arr_id = get_expr_id(&arr);
    let prog = program(vec![let_binding("a", None, arr)]);

    let tcx = run_ok(prog);
    assert_expr_type(
        &tcx,
        arr_id,
        &Type::Array {
            elem: Type::Int.boxed(),
            len: ArrayLen::Fixed(3),
        },
    );
}

#[test]
fn test_array_literal_unannotated_string() {
    reset_expr_ids();

    // let b = ["x", "y"];
    let arr = array_literal(vec![lit_string("x"), lit_string("y")]);
    let arr_id = get_expr_id(&arr);
    let prog = program(vec![let_binding("b", None, arr)]);

    let tcx = run_ok(prog);
    assert_expr_type(
        &tcx,
        arr_id,
        &Type::Array {
            elem: Type::String.boxed(),
            len: ArrayLen::Fixed(2),
        },
    );
}

#[test]
fn test_array_literal_empty() {
    reset_expr_ids();

    // let c: int[0] = [];
    let arr = array_literal(vec![]);
    let arr_id = get_expr_id(&arr);
    let annot = Type::Array {
        elem: Type::Int.boxed(),
        len: ArrayLen::Fixed(0),
    };
    let prog = program(vec![let_binding("c", Some(annot.clone()), arr)]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, arr_id, &annot);
}

#[test]
fn test_array_literal_annotated_fixed_length_ok() {
    reset_expr_ids();

    // let d: int[3] = [1, 2, 3];
    let arr = array_literal(vec![lit_int(1), lit_int(2), lit_int(3)]);
    let arr_id = get_expr_id(&arr);
    let annot = Type::Array {
        elem: Type::Int.boxed(),
        len: ArrayLen::Fixed(3),
    };
    let prog = program(vec![let_binding("d", Some(annot.clone()), arr)]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, arr_id, &annot);
}

#[test]
fn test_array_literal_annotated_length_mismatch() {
    reset_expr_ids();

    // let e: int[2] = [1, 2, 3];
    let arr = array_literal(vec![lit_int(1), lit_int(2), lit_int(3)]);
    let annot = Type::Array {
        elem: Type::Int.boxed(),
        len: ArrayLen::Fixed(2),
    };
    let prog = program(vec![let_binding("e", Some(annot), arr)]);

    let errors = run_err(prog);
    assert!(!errors.is_empty());
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::MismatchedTypes { .. }))
    );
}

#[test]
fn test_array_literal_annotated_infer_length() {
    reset_expr_ids();

    // let f: int[_] = [1, 2, 3, 4];
    let arr = array_literal(vec![lit_int(1), lit_int(2), lit_int(3), lit_int(4)]);
    let arr_id = get_expr_id(&arr);
    let annot = Type::Array {
        elem: Type::Int.boxed(),
        len: ArrayLen::Infer,
    };
    let prog = program(vec![let_binding("f", Some(annot), arr)]);

    let tcx = run_ok(prog);
    assert_expr_type(
        &tcx,
        arr_id,
        &Type::Array {
            elem: Type::Int.boxed(),
            len: ArrayLen::Fixed(4),
        },
    );
}

#[test]
fn test_array_literal_annotated_list() {
    reset_expr_ids();

    // let g: [int] = [1, 2];
    // under [int] annotation, the literal [1, 2] is instantiated directly as [int]
    let arr = array_literal(vec![lit_int(1), lit_int(2)]);
    let arr_id = get_expr_id(&arr);
    let annot = Type::List {
        elem: Type::Int.boxed(),
    };
    let prog = program(vec![let_binding("g", Some(annot), arr)]);

    let tcx = run_ok(prog);
    assert_expr_type(
        &tcx,
        arr_id,
        &Type::List {
            elem: Type::Int.boxed(),
        },
    );
}

#[test]
fn test_array_literal_all_nil_ambiguous() {
    reset_expr_ids();

    // let h = [nil, nil];
    let arr = array_literal(vec![lit_nil(), lit_nil()]);
    let prog = program(vec![let_binding("h", None, arr)]);

    let errors = run_err(prog);
    assert!(!errors.is_empty());
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::ArrayAllNilAmbiguous))
    );
}

#[test]
fn test_array_literal_all_nil_annotated_ok() {
    reset_expr_ids();

    // let i: int?[_] = [nil, nil];
    let arr = array_literal(vec![lit_nil(), lit_nil()]);
    let arr_id = get_expr_id(&arr);
    let annot = Type::Array {
        elem: opt_type(Type::Int).boxed(),
        len: ArrayLen::Infer,
    };
    let prog = program(vec![let_binding("i", Some(annot), arr)]);

    let tcx = run_ok(prog);
    assert_expr_type(
        &tcx,
        arr_id,
        &Type::Array {
            elem: opt_type(Type::Int).boxed(),
            len: ArrayLen::Fixed(2),
        },
    );
}

#[test]
fn test_array_literal_optional_elements_mixed_order() {
    reset_expr_ids();

    // let a: [int?; _] = [1, nil];
    let arr_tail_nil = array_literal(vec![lit_int(1), lit_nil()]);
    let arr_tail_id = get_expr_id(&arr_tail_nil);
    let annot = Type::Array {
        elem: opt_type(Type::Int).boxed(),
        len: ArrayLen::Infer,
    };
    let prog = program(vec![let_binding("a", Some(annot.clone()), arr_tail_nil)]);
    let tcx_tail = run_ok(prog);
    assert_expr_type(
        &tcx_tail,
        arr_tail_id,
        &Type::Array {
            elem: opt_type(Type::Int).boxed(),
            len: ArrayLen::Fixed(2),
        },
    );

    // let b: [int?; _] = [nil, 1];
    let arr_head_nil = array_literal(vec![lit_nil(), lit_int(1)]);
    let arr_head_id = get_expr_id(&arr_head_nil);
    let prog = program(vec![let_binding("b", Some(annot), arr_head_nil)]);
    let tcx_head = run_ok(prog);
    assert_expr_type(
        &tcx_head,
        arr_head_id,
        &Type::Array {
            elem: opt_type(Type::Int).boxed(),
            len: ArrayLen::Fixed(2),
        },
    );
}

#[test]
fn test_array_literal_optional_string_elements() {
    reset_expr_ids();

    // let c: [string?; _] = [nil, "hola", nil];
    let arr = array_literal(vec![lit_nil(), lit_string("hola"), lit_nil()]);
    let arr_id = get_expr_id(&arr);
    let annot = Type::Array {
        elem: opt_type(Type::String).boxed(),
        len: ArrayLen::Infer,
    };
    let prog = program(vec![let_binding("c", Some(annot.clone()), arr)]);
    let tcx_annot = run_ok(prog);
    assert_expr_type(
        &tcx_annot,
        arr_id,
        &Type::Array {
            elem: opt_type(Type::String).boxed(),
            len: ArrayLen::Fixed(3),
        },
    );

    // let d = [nil, "hola", nil];
    let arr_inferred = array_literal(vec![lit_nil(), lit_string("hola"), lit_nil()]);
    let arr_inferred_id = get_expr_id(&arr_inferred);
    let prog = program(vec![let_binding("d", None, arr_inferred)]);
    let tcx_inferred = run_ok(prog);
    assert_expr_type(
        &tcx_inferred,
        arr_inferred_id,
        &Type::Array {
            elem: opt_type(Type::String).boxed(),
            len: ArrayLen::Fixed(3),
        },
    );
}

#[test]
fn test_array_literal_optional_mixed_error() {
    reset_expr_ids();

    // let e: [string?; _] = [nil, "hola", 1];
    let arr = array_literal(vec![lit_nil(), lit_string("hola"), lit_int(1)]);
    let annot = Type::Array {
        elem: opt_type(Type::String).boxed(),
        len: ArrayLen::Infer,
    };
    let prog = program(vec![let_binding("e", Some(annot), arr)]);

    let errors = run_err(prog);
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::MismatchedTypes { .. })),
        "expected mismatched types error for incompatible element"
    );
}

#[test]
fn test_array_literal_element_type_mismatch() {
    reset_expr_ids();

    // let j = [1, "x"];
    let arr = array_literal(vec![lit_int(1), lit_string("x")]);
    let prog = program(vec![let_binding("j", None, arr)]);

    let errors = run_err(prog);
    assert!(!errors.is_empty());
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::MismatchedTypes { .. }))
    );
}

// ---- array fill tests ----

#[test]
fn test_array_fill_unannotated() {
    reset_expr_ids();

    // let k = [0; 5];
    let arr = array_fill(lit_int(0), lit_int(5));
    let arr_id = get_expr_id(&arr);
    let prog = program(vec![let_binding("k", None, arr)]);

    let tcx = run_ok(prog);
    assert_expr_type(
        &tcx,
        arr_id,
        &Type::Array {
            elem: Type::Int.boxed(),
            len: ArrayLen::Fixed(5),
        },
    );
}

#[test]
fn test_array_fill_annotated_ok() {
    reset_expr_ids();

    // let l: int[3] = [0; 3];
    let arr = array_fill(lit_int(0), lit_int(3));
    let arr_id = get_expr_id(&arr);
    let annot = Type::Array {
        elem: Type::Int.boxed(),
        len: ArrayLen::Fixed(3),
    };
    let prog = program(vec![let_binding("l", Some(annot.clone()), arr)]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, arr_id, &annot);
}

#[test]
fn test_array_fill_length_mismatch() {
    reset_expr_ids();

    // let m: int[2] = [0; 3];
    let arr = array_fill(lit_int(0), lit_int(3));
    let annot = Type::Array {
        elem: Type::Int.boxed(),
        len: ArrayLen::Fixed(2),
    };
    let prog = program(vec![let_binding("m", Some(annot), arr)]);

    let errors = run_err(prog);
    assert!(!errors.is_empty());
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::MismatchedTypes { .. }))
    );
}

#[test]
fn test_array_fill_infer_length() {
    reset_expr_ids();

    // let n: int[_] = [0; 4];
    let arr = array_fill(lit_int(0), lit_int(4));
    let arr_id = get_expr_id(&arr);
    let annot = Type::Array {
        elem: Type::Int.boxed(),
        len: ArrayLen::Infer,
    };
    let prog = program(vec![let_binding("n", Some(annot), arr)]);

    let tcx = run_ok(prog);
    assert_expr_type(
        &tcx,
        arr_id,
        &Type::Array {
            elem: Type::Int.boxed(),
            len: ArrayLen::Fixed(4),
        },
    );
}

#[test]
fn test_array_fill_length_not_literal() {
    reset_expr_ids();

    // let x = 3;
    // let o = [0; x];
    let x_binding = let_binding("x", None, lit_int(3));
    let arr = array_fill(lit_int(0), ident_expr("x"));
    let prog = program(vec![x_binding, let_binding("o", None, arr)]);

    let errors = run_err(prog);
    assert!(!errors.is_empty());
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::ArrayFillLengthNotLiteral))
    );
}

#[test]
fn test_array_fill_list_annotated_ok() {
    reset_expr_ids();

    // let xs: [int] = [0; 3];
    let arr = array_fill(lit_int(0), lit_int(3));
    let arr_id = get_expr_id(&arr);
    let annot = Type::List {
        elem: Type::Int.boxed(),
    };
    let prog = program(vec![let_binding("xs", Some(annot.clone()), arr)]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, arr_id, &annot);
}

#[test]
fn test_array_fill_optional_list_ok() {
    reset_expr_ids();

    // var xs: [int?] = [nil; 3];
    let arr = array_fill(lit_nil(), lit_int(3));
    let arr_id = get_expr_id(&arr);
    let annot = Type::List {
        elem: opt_type(Type::Int).boxed(),
    };
    let prog = program(vec![var_binding("xs", Some(annot.clone()), arr)]);

    let tcx = run_ok(prog);
    assert_expr_type(&tcx, arr_id, &annot);
}

#[test]
fn test_array_fill_list_len_not_literal_err() {
    reset_expr_ids();

    // let n = 3;
    // let xs: [int] = [0; n];
    let n_binding = let_binding("n", None, lit_int(3));
    let arr = array_fill(lit_int(0), ident_expr("n"));
    let annot = Type::List {
        elem: Type::Int.boxed(),
    };
    let xs_binding = let_binding("xs", Some(annot), arr);
    let prog = program(vec![n_binding, xs_binding]);

    let errors = run_err(prog);
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::ArrayFillLengthNotLiteral)),
        "Expected ArrayFillLengthNotLiteral error, got: {errors:?}"
    );
}

// ---- array/list assignability tests ----

#[test]
fn test_fixed_array_not_assignable_to_list() {
    reset_expr_ids();

    // let arr: [int; 2] = [1, 2];
    // let x: [int] = arr;   // must fail
    let arr_lit = array_literal(vec![lit_int(1), lit_int(2)]);
    let arr_binding = let_binding(
        "arr",
        Some(Type::Array {
            elem: Type::Int.boxed(),
            len: ArrayLen::Fixed(2),
        }),
        arr_lit,
    );

    let x_binding = let_binding(
        "x",
        Some(Type::List {
            elem: Type::Int.boxed(),
        }),
        ident_expr("arr"),
    );

    let prog = program(vec![arr_binding, x_binding]);
    let errors = run_err(prog);

    assert!(
        errors.iter().any(|e| matches!(
            &e.kind,
            DiagnosticKind::MismatchedTypes { expected, found }
            if *expected == Type::List { elem: Type::Int.boxed() }
            && *found == Type::Array { elem: Type::Int.boxed(), len: ArrayLen::Fixed(2) }
        )),
        "Expected MismatchedTypes error ([int; 2] not assignable to [int]), got: {errors:?}"
    );
}

#[test]
fn test_list_not_assignable_to_fixed_array() {
    reset_expr_ids();

    // let lst: [int] = [1, 2];
    // let y: [int; 2] = lst;  // must fail
    let lst_lit = array_literal(vec![lit_int(1), lit_int(2)]);
    let lst_binding = let_binding(
        "lst",
        Some(Type::List {
            elem: Type::Int.boxed(),
        }),
        lst_lit,
    );

    let y_binding = let_binding(
        "y",
        Some(Type::Array {
            elem: Type::Int.boxed(),
            len: ArrayLen::Fixed(2),
        }),
        ident_expr("lst"),
    );

    let prog = program(vec![lst_binding, y_binding]);
    let errors = run_err(prog);

    assert!(
        errors.iter().any(|e| matches!(
            &e.kind,
            DiagnosticKind::MismatchedTypes { expected, found }
            if *expected == Type::Array { elem: Type::Int.boxed(), len: ArrayLen::Fixed(2) }
            && *found == Type::List { elem: Type::Int.boxed() }
        )),
        "Expected MismatchedTypes error ([int] not assignable to [int; 2]), got: {errors:?}"
    );
}

#[test]
fn test_all_nil_annotated_list_ok() {
    reset_expr_ids();

    // var a: [int?] = [nil, nil];
    let arr = array_literal(vec![lit_nil(), lit_nil()]);
    let arr_id = get_expr_id(&arr);
    let annot = Type::List {
        elem: opt_type(Type::Int).boxed(),
    };
    let prog = program(vec![var_binding("a", Some(annot), arr)]);

    let tcx = run_ok(prog);
    assert_expr_type(
        &tcx,
        arr_id,
        &Type::List {
            elem: opt_type(Type::Int).boxed(),
        },
    );
}

// ---- array index tests ----

#[test]
fn test_array_index_fixed_ok() {
    reset_expr_ids();

    // let a: int[3] = [1, 2, 3];
    // let x = a[0];
    let arr = array_literal(vec![lit_int(1), lit_int(2), lit_int(3)]);
    let arr_annot = Type::Array {
        elem: Type::Int.boxed(),
        len: ArrayLen::Fixed(3),
    };
    let arr_binding = let_binding("a", Some(arr_annot), arr);

    let idx = index_expr(ident_expr("a"), lit_int(0));
    let idx_id = get_expr_id(&idx);
    let x_binding = let_binding("x", None, idx);

    let prog = program(vec![arr_binding, x_binding]);
    let tcx = run_ok(prog);

    assert_expr_type(&tcx, idx_id, &Type::Int);
}

#[test]
fn test_list_index_ok() {
    reset_expr_ids();

    // let b: [float] = [1.0, 2.0];
    // let y = b[1];
    let arr = array_literal(vec![lit_float(1.0), lit_float(2.0)]);
    let arr_annot = Type::List {
        elem: Type::Float.boxed(),
    };
    let arr_binding = let_binding("b", Some(arr_annot), arr);

    let idx = index_expr(ident_expr("b"), lit_int(1));
    let idx_id = get_expr_id(&idx);
    let y_binding = let_binding("y", None, idx);

    let prog = program(vec![arr_binding, y_binding]);
    let tcx = run_ok(prog);

    assert_expr_type(&tcx, idx_id, &Type::Float);
}

#[test]
fn test_array_index_inferred_ok() {
    reset_expr_ids();

    // let a = [1, 2, 3];
    // let x = a[2];
    let arr = array_literal(vec![lit_int(1), lit_int(2), lit_int(3)]);
    let arr_binding = let_binding("a", None, arr);

    let idx = index_expr(ident_expr("a"), lit_int(2));
    let idx_id = get_expr_id(&idx);
    let x_binding = let_binding("x", None, idx);

    let prog = program(vec![arr_binding, x_binding]);
    let tcx = run_ok(prog);

    assert_expr_type(&tcx, idx_id, &Type::Int);
}

#[test]
fn test_array_index_non_int_index_error() {
    reset_expr_ids();

    // let a = [1, 2, 3];
    // let i = 1.0;
    // let x = a[i];
    let arr = array_literal(vec![lit_int(1), lit_int(2), lit_int(3)]);
    let arr_binding = let_binding("a", None, arr);
    let i_binding = let_binding("i", None, lit_float(1.0));
    let idx = index_expr(ident_expr("a"), ident_expr("i"));
    let x_binding = let_binding("x", None, idx);

    let prog = program(vec![arr_binding, i_binding, x_binding]);
    let errors = run_err(prog);

    assert!(!errors.is_empty(), "Expected type error for non-int index");
    assert!(
        errors.iter().any(
            |e| matches!(&e.kind, DiagnosticKind::IndexNotInt { found } if *found == Type::Float)
        ),
        "Expected IndexNotInt error with found=float, got: {errors:?}"
    );
}

#[test]
fn test_index_on_non_array_error() {
    reset_expr_ids();

    // let x = 10;
    // let y = x[0];
    let x_binding = let_binding("x", None, lit_int(10));
    let idx = index_expr(ident_expr("x"), lit_int(0));
    let y_binding = let_binding("y", None, idx);

    let prog = program(vec![x_binding, y_binding]);
    let errors = run_err(prog);

    assert!(
        !errors.is_empty(),
        "Expected type error for indexing non-array"
    );
    assert!(
        errors.iter().any(
            |e| matches!(&e.kind, DiagnosticKind::IndexOnNonArray { found } if *found == Type::Int)
        ),
        "Expected IndexOnNonArray error with found=int, got: {errors:?}"
    );
}

#[test]
fn test_nested_array_index_ok() {
    reset_expr_ids();

    // let a: int[3][2] = [[1, 2, 3], [4, 5, 6]];
    // let x = a[0][1];
    let inner1 = array_literal(vec![lit_int(1), lit_int(2), lit_int(3)]);
    let inner2 = array_literal(vec![lit_int(4), lit_int(5), lit_int(6)]);
    let outer = array_literal(vec![inner1, inner2]);

    let arr_annot = Type::Array {
        elem: Type::Array {
            elem: Type::Int.boxed(),
            len: ArrayLen::Fixed(3),
        }
        .boxed(),
        len: ArrayLen::Fixed(2),
    };
    let arr_binding = let_binding("a", Some(arr_annot), outer);

    let idx1 = index_expr(ident_expr("a"), lit_int(0));
    let idx2 = index_expr(idx1, lit_int(1));
    let idx2_id = get_expr_id(&idx2);
    let x_binding = let_binding("x", None, idx2);

    let prog = program(vec![arr_binding, x_binding]);
    let tcx = run_ok(prog);

    assert_expr_type(&tcx, idx2_id, &Type::Int);
}

#[test]
fn test_array_index_assignment_ok() {
    reset_expr_ids();

    // var a: int[3] = [1, 2, 3];
    // a[0] = 10;
    let arr = array_literal(vec![lit_int(1), lit_int(2), lit_int(3)]);
    let arr_annot = Type::Array {
        elem: Type::Int.boxed(),
        len: ArrayLen::Fixed(3),
    };
    let arr_binding = var_binding("a", Some(arr_annot), arr);

    let idx = index_expr(ident_expr("a"), lit_int(0));
    let assign = assign_expr(idx, AssignOp::Assign, lit_int(10));
    let assign_stmt = StmtNode {
        node: Stmt::Expr(assign),
        span: dummy_span(),
    };

    let prog = program(vec![arr_binding, assign_stmt]);
    let _ = run_ok(prog);
}

// ---- slice tests ----

#[test]
fn test_view_param_accepts_fixed_array() {
    reset_expr_ids();

    // fn sum(xs: [int; ..]) -> int { xs[0] }
    // sum([1, 2, 3])
    let view_param = slice_type(&Type::Int);
    let func = func_decl("sum", vec![("xs", view_param)], Type::Int, vec![]);

    let arr = array_literal(vec![lit_int(1), lit_int(2), lit_int(3)]);
    let call = call_expr(ident_expr("sum"), vec![arr]);
    let call_stmt = StmtNode {
        node: Stmt::Expr(call),
        span: dummy_span(),
    };

    let prog = program(vec![func, call_stmt]);
    let _ = run_ok(prog);
}

#[test]
fn test_view_param_accepts_list() {
    reset_expr_ids();

    // fn sum(xs: [int; ..]) -> int { xs[0] }
    // let lst: [int] = [1, 2, 3];
    // sum(lst)
    let view_param = slice_type(&Type::Int);
    let func = func_decl("sum", vec![("xs", view_param)], Type::Int, vec![]);

    let lst = array_literal(vec![lit_int(1), lit_int(2), lit_int(3)]);
    let lst_annot = Type::List {
        elem: Type::Int.boxed(),
    };
    let lst_binding = let_binding("lst", Some(lst_annot), lst);

    let call = call_expr(ident_expr("sum"), vec![ident_expr("lst")]);
    let call_stmt = StmtNode {
        node: Stmt::Expr(call),
        span: dummy_span(),
    };

    let prog = program(vec![func, lst_binding, call_stmt]);
    let _ = run_ok(prog);
}

#[test]
fn test_view_indexing_ok() {
    reset_expr_ids();

    // fn head(xs: [int; ..]) -> int { xs[0] }
    let view_param = slice_type(&Type::Int);
    let idx = index_expr(ident_expr("xs"), lit_int(0));
    let idx_id = get_expr_id(&idx);
    let func = FuncNode {
        node: Func {
            annotations: vec![],
            doc: None,
            name: ident("head"),
            visibility: Visibility::Private,
            type_params: vec![],
            const_params: vec![],
            params: vec![Param {
                mutability: Mutability::Immutable,
                name: ident("xs"),
                ty: view_param,
                default: None,
                cast_accept: false,
            }],
            ret: Type::Int,
            body: BlockNode {
                node: Block {
                    stmts: vec![],
                    tail: Some(Box::new(idx)),
                },
                span: dummy_span(),
            },
        },
        span: dummy_span(),
    };
    let func_stmt = StmtNode {
        node: Stmt::Func(func),
        span: dummy_span(),
    };

    let prog = program(vec![func_stmt]);
    let tcx = run_ok(prog);
    assert_expr_type(&tcx, idx_id, &Type::Int);
}

#[test]
fn test_view_mismatched_element_type_err() {
    reset_expr_ids();

    // fn f(xs: [int; ..]) {}
    // f(["a", "b"])
    let view_param = slice_type(&Type::Int);
    let func = func_decl("f", vec![("xs", view_param)], Type::Void, vec![]);

    let arr = array_literal(vec![lit_string("a"), lit_string("b")]);
    let call = call_expr(ident_expr("f"), vec![arr]);
    let call_stmt = StmtNode {
        node: Stmt::Expr(call),
        span: dummy_span(),
    };

    let prog = program(vec![func, call_stmt]);
    let errors = run_err(prog);
    assert!(
        errors
            .iter()
            .any(|e| matches!(&e.kind, DiagnosticKind::MismatchedTypes { .. })),
        "Expected MismatchedTypes error, got: {errors:?}"
    );
}

// ---- safe map indexing (map?[key]) ----

#[test]
fn test_safe_map_index_returns_optional_value() {
    reset_expr_ids();

    // let m: [string: int]? = ["a": 1];
    // let v = m?["a"];   -- optional map indexed safely; should type as int?
    let map = map_literal_expr(vec![(lit_string("a"), lit_int(1))]);
    let map_annot = opt_type(Type::Map {
        key: Box::new(Type::String),
        value: Box::new(Type::Int),
    });
    let m_binding = let_binding("m", Some(map_annot), map);

    let idx = safe_index_expr(ident_expr("m"), lit_string("a"));
    let idx_id = get_expr_id(&idx);
    let v_binding = let_binding("v", None, idx);

    let prog = program(vec![m_binding, v_binding]);
    let tcx = run_ok(prog);

    let expected = opt_type(Type::Int);
    assert_expr_type(&tcx, idx_id, &expected);
}

#[test]
fn test_map_index_ok() {
    reset_expr_ids();

    // let m: [string: int] = ["a": 1];
    // let v = m["a"];   -- non-safe, returns int?
    let map = map_literal_expr(vec![(lit_string("a"), lit_int(1))]);
    let map_annot = Type::Map {
        key: Box::new(Type::String),
        value: Box::new(Type::Int),
    };
    let m_binding = let_binding("m", Some(map_annot), map);

    let idx = index_expr(ident_expr("m"), lit_string("a"));
    let idx_id = get_expr_id(&idx);
    let v_binding = let_binding("v", None, idx);

    let prog = program(vec![m_binding, v_binding]);
    let tcx = run_ok(prog);

    let expected = opt_type(Type::Int);
    assert_expr_type(&tcx, idx_id, &expected);
}
