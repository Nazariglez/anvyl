use super::helpers::{expect_nominal, parse_param_type, parse_program, parse_type};
use crate::ast;

#[test]
fn array_fixed_len() {
    let ty = parse_type("[int; 3]");
    match ty {
        ast::Type::Array { elem, len } => {
            assert_eq!(*elem, ast::Type::Int);
            assert_eq!(len, ast::ArrayLen::Fixed(3));
        }
        other => panic!("expected array type, found {other:?}"),
    }
}

#[test]
fn list() {
    let ty = parse_type("[string]");
    match ty {
        ast::Type::List { elem } => {
            assert_eq!(*elem, ast::Type::String);
        }
        other => panic!("expected list type, found {other:?}"),
    }
}

#[test]
fn array_infer_len() {
    let ty = parse_type("[float; _]");
    match ty {
        ast::Type::Array { elem, len } => {
            assert_eq!(*elem, ast::Type::Float);
            assert_eq!(len, ast::ArrayLen::Infer);
        }
        other => panic!("expected infer-length array type, found {other:?}"),
    }
}

#[test]
fn array_opt() {
    let ty = parse_type("[int; 3]?");
    assert!(ty.is_option(), "expected optional array type, found {ty:?}");
    let inner = ty.option_inner().expect("is_option guarantees inner");
    match inner {
        ast::Type::Array { elem, len } => {
            assert_eq!(**elem, ast::Type::Int);
            assert_eq!(*len, ast::ArrayLen::Fixed(3));
        }
        other => panic!("expected inner array type, found {other:?}"),
    }
}

#[test]
fn array_struct_elem() {
    let ty = parse_type("[MyStruct; 5]");
    match ty {
        ast::Type::Array { elem, len } => {
            assert_eq!(len, ast::ArrayLen::Fixed(5));
            match *elem {
                ast::Type::UnresolvedNominal {
                    qualifier,
                    name,
                    generic_args,
                } => {
                    assert!(qualifier.is_none());
                    assert_eq!(name.0.as_ref(), "MyStruct");
                    assert!(generic_args.is_empty());
                }
                other => panic!("expected UnresolvedNominal, found {other:?}"),
            }
        }
        other => panic!("expected array type, found {other:?}"),
    }
}

#[test]
fn map() {
    let ty = parse_type("[string: int]");
    match ty {
        ast::Type::Map { key, value } => {
            assert_eq!(*key, ast::Type::String);
            assert_eq!(*value, ast::Type::Int);
        }
        other => panic!("expected map type, found {other:?}"),
    }
}

#[test]
fn fn_type() {
    let ty = parse_type("fn(int, string) -> bool");
    match ty {
        ast::Type::Func { params, ret } => {
            assert_eq!(
                params,
                vec![
                    ast::FuncParam::immut(ast::Type::Int),
                    ast::FuncParam::immut(ast::Type::String),
                ]
            );
            assert_eq!(*ret, ast::Type::Bool);
        }
        other => panic!("expected function type, found {other:?}"),
    }
}

#[test]
fn fn_type_opt() {
    let ty = parse_type("(fn(float) -> int)?");
    assert!(
        ty.is_option(),
        "expected optional function type, found {ty:?}"
    );
    let inner = ty.option_inner().expect("is_option guarantees inner");
    match inner {
        ast::Type::Func { params, ret } => {
            assert_eq!(*params, vec![ast::FuncParam::immut(ast::Type::Float)]);
            assert_eq!(**ret, ast::Type::Int);
        }
        other => panic!("expected function type inside optional, found {other:?}"),
    }
}

#[test]
fn nested_array() {
    let ty = parse_type("[[int; 3]; 2]");
    match ty {
        ast::Type::Array { elem, len } => {
            assert_eq!(len, ast::ArrayLen::Fixed(2));
            match *elem {
                ast::Type::Array {
                    elem: inner_elem,
                    len: inner_len,
                } => {
                    assert_eq!(*inner_elem, ast::Type::Int);
                    assert_eq!(inner_len, ast::ArrayLen::Fixed(3));
                }
                other => panic!("expected inner array type, found {other:?}"),
            }
        }
        other => panic!("expected nested array type, found {other:?}"),
    }
}

#[test]
fn nested_opt() {
    let ty = parse_type("(int?)?");
    assert!(ty.is_option(), "expected optional type, found {ty:?}");
    let inner = ty.option_inner().expect("is_option guarantees inner");
    assert!(
        inner.is_option(),
        "expected Optional(Optional(Int)), found {inner:?}"
    );
    let inner2 = inner.option_inner().expect("is_option guarantees inner");
    assert_eq!(*inner2, ast::Type::Int);
}

#[test]
fn opt_array_infer() {
    let ty = parse_type("[int?; _]");
    match ty {
        ast::Type::Array { ref elem, len } => {
            assert_eq!(len, ast::ArrayLen::Infer);
            assert!(elem.is_option(), "expected Optional(Int), found {elem:?}");
            let inner = elem.option_inner().expect("is_option guarantees inner");
            assert_eq!(*inner, ast::Type::Int);
        }
        other => panic!("expected Array(Optional(Int), Infer), found {other:?}"),
    }
}

#[test]
fn opt_list() {
    let ty = parse_type("[int?]");
    match ty {
        ast::Type::List { ref elem } => {
            assert!(elem.is_option(), "expected Optional(Int), found {elem:?}");
            let inner = elem.option_inner().expect("is_option guarantees inner");
            assert_eq!(*inner, ast::Type::Int);
        }
        other => panic!("expected List(Optional(Int)), found {other:?}"),
    }
}

#[test]
fn list_opt() {
    let ty = parse_type("[int]?");
    assert!(ty.is_option(), "expected Optional(List(Int)), found {ty:?}");
    let inner = ty.option_inner().expect("is_option guarantees inner");
    match inner {
        ast::Type::List { elem } => {
            assert_eq!(**elem, ast::Type::Int);
        }
        other => panic!("expected List(Int), found {other:?}"),
    }
}

#[test]
fn opt_array_fixed() {
    let ty = parse_type("[int?; 3]");
    match ty {
        ast::Type::Array { ref elem, len } => {
            assert_eq!(len, ast::ArrayLen::Fixed(3));
            assert!(elem.is_option(), "expected Optional(Int), found {elem:?}");
            let inner = elem.option_inner().expect("is_option guarantees inner");
            assert_eq!(*inner, ast::Type::Int);
        }
        other => panic!("expected Array(Optional(Int), Fixed(3)), found {other:?}"),
    }
}

#[test]
fn slice_int() {
    let ty = parse_param_type("slice[int]");
    match ty {
        ast::Type::Slice { elem } => {
            assert_eq!(*elem, ast::Type::Int);
        }
        other => panic!("expected Slice(Int), found {other:?}"),
    }
}

#[test]
fn slice_float() {
    let ty = parse_param_type("slice[float]");
    match ty {
        ast::Type::Slice { elem } => {
            assert_eq!(*elem, ast::Type::Float);
        }
        other => panic!("expected Slice(Float), found {other:?}"),
    }
}

#[test]
fn slice_array() {
    let ty = parse_param_type("slice[[int; 3]]");
    match ty {
        ast::Type::Slice { elem } => match *elem {
            ast::Type::Array { elem: inner, len } => {
                assert_eq!(*inner, ast::Type::Int);
                assert_eq!(len, ast::ArrayLen::Fixed(3));
            }
            other => panic!("expected Array(Int, Fixed(3)), found {other:?}"),
        },
        other => panic!("expected Slice(Array(...)), found {other:?}"),
    }
}

#[test]
fn slice_list() {
    let ty = parse_param_type("slice[[string]]");
    match ty {
        ast::Type::Slice { elem } => match *elem {
            ast::Type::List { elem: inner } => {
                assert_eq!(*inner, ast::Type::String);
            }
            other => panic!("expected List(String), found {other:?}"),
        },
        other => panic!("expected Slice(List(...)), found {other:?}"),
    }
}

#[test]
fn slice_opt() {
    let ty = parse_param_type("slice[int]?");
    assert!(
        ty.is_option(),
        "expected Optional(Slice(Int)), found {ty:?}"
    );
    let inner = ty.option_inner().expect("is_option guarantees inner");
    match inner {
        ast::Type::Slice { elem } => {
            assert_eq!(**elem, ast::Type::Int);
        }
        other => panic!("expected Slice(Int), found {other:?}"),
    }
}

#[test]
fn fn_type_var_param() {
    let ty = parse_type("fn(var int) -> void");
    match ty {
        ast::Type::Func { params, ret } => {
            assert_eq!(params, vec![ast::FuncParam::new(ast::Type::Int, true)]);
            assert_eq!(*ret, ast::Type::Void);
        }
        other => panic!("expected function type with var param, found {other:?}"),
    }
}

#[test]
fn fn_type_mixed_var() {
    let ty = parse_type("fn(var int, string) -> bool");
    match ty {
        ast::Type::Func { params, ret } => {
            assert_eq!(
                params,
                vec![
                    ast::FuncParam::new(ast::Type::Int, true),
                    ast::FuncParam::immut(ast::Type::String),
                ]
            );
            assert_eq!(*ret, ast::Type::Bool);
        }
        other => panic!("expected function type with mixed params, found {other:?}"),
    }
}

#[test]
fn qualified_nominal() {
    let ty = parse_type("gk.GameKit");
    match ty {
        ast::Type::UnresolvedNominal {
            qualifier,
            name,
            generic_args,
        } => {
            assert!(qualifier.is_some(), "expected qualifier Some(gk), got None");
            assert_eq!(qualifier.as_ref().unwrap().0.as_ref(), "gk");
            assert_eq!(name.0.as_ref(), "GameKit");
            assert!(generic_args.is_empty());
        }
        other => panic!("expected UnresolvedNominal, found {other:?}"),
    }
}

#[test]
fn qualified_generic() {
    let ty = parse_type("gk.GameKit<int>");
    match ty {
        ast::Type::UnresolvedNominal {
            qualifier,
            name,
            generic_args,
        } => {
            assert!(qualifier.is_some());
            assert_eq!(qualifier.as_ref().unwrap().0.as_ref(), "gk");
            assert_eq!(name.0.as_ref(), "GameKit");
            assert_eq!(generic_args, &[ast::GenericArg::Type(ast::Type::Int)]);
        }
        other => panic!("expected UnresolvedNominal, found {other:?}"),
    }
}

#[test]
fn bare_generic() {
    let ty = parse_type("Foo<T>");
    match ty {
        ast::Type::UnresolvedNominal {
            qualifier,
            name,
            generic_args,
        } => {
            assert!(qualifier.is_none());
            assert_eq!(name.0.as_ref(), "Foo");
            assert_eq!(generic_args.len(), 1);
            let ast::GenericArg::Type(ast::Type::UnresolvedNominal {
                qualifier: None,
                name,
                generic_args: inner_args,
            }) = &generic_args[0]
            else {
                panic!("expected bare nominal T, found {:?}", generic_args[0]);
            };
            assert_eq!(name.0.as_ref(), "T");
            assert!(inner_args.is_empty());
        }
        other => panic!("expected UnresolvedNominal, found {other:?}"),
    }
}

#[test]
fn mixed_params() {
    let prog = parse_program("struct Use<T, N: int> { buf: FixedBuf<T, N> }");
    let ast::Stmt::Aggregate(node) = &prog.stmts[0].node else {
        panic!("expected aggregate");
    };
    let args = expect_nominal(&node.node.fields[0].ty, "FixedBuf");
    assert_eq!(
        args[0],
        ast::GenericArg::Type(ast::Type::Var(node.node.type_params[0].id))
    );
    assert_eq!(
        args[1],
        ast::GenericArg::Const(ast::ConstArg::Param(node.node.const_params[0].id))
    );
}

#[test]
fn arg_order() {
    let ty = parse_type("FixedBuf<3, int>");
    let args = expect_nominal(&ty, "FixedBuf");
    assert_eq!(
        args[0],
        ast::GenericArg::Const(ast::ConstArg::Value(ast::ConstValue::Int(3)))
    );
    assert_eq!(args[1], ast::GenericArg::Type(ast::Type::Int));
}

#[test]
fn nested_args() {
    let ty = parse_type("Outer<FixedBuf<int, 3>, 2>");
    let args = expect_nominal(&ty, "Outer");
    assert_eq!(args.len(), 2);
    assert_eq!(
        args[1],
        ast::GenericArg::Const(ast::ConstArg::Value(ast::ConstValue::Int(2)))
    );
    let ast::GenericArg::Type(inner) = &args[0] else {
        panic!("expected type arg");
    };
    let inner_args = expect_nominal(inner, "FixedBuf");
    assert_eq!(inner_args[0], ast::GenericArg::Type(ast::Type::Int));
    assert_eq!(
        inner_args[1],
        ast::GenericArg::Const(ast::ConstArg::Value(ast::ConstValue::Int(3)))
    );
}
