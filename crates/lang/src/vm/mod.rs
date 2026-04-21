mod builtins;
mod bytecode;
pub mod callback;
mod compiler;
pub(crate) mod cycle_collector;
pub mod extern_type;
pub mod handle_store;
pub mod managed_rc;
mod runtime;
mod value;

use std::{collections::HashMap, fmt::Write};

pub use callback::{AnvyxFn, VmContext, with_callback_ctx};
pub use compiler::CompiledProgram;
pub use extern_type::{AnvyxConvert, AnvyxExternType, ExternHandle, extern_handle};
pub use handle_store::HandleStore;
pub use managed_rc::ManagedRc;
pub use runtime::{ExternHandler, ExternRegistry, VM};
pub use value::{
    DisplayDetect, DisplayDetectFallback, EnumData, ExternHandleData, MapStorage, RuntimeError,
    StructData, Value,
};

use crate::{Profile, hir};

pub fn compile_with_externs(
    hir_prog: &hir::Program,
    externs: HashMap<String, ExternHandler>,
    profile: Profile,
) -> Result<(CompiledProgram, ExternRegistry), String> {
    let compiled =
        compiler::compile(hir_prog, profile).map_err(|e| format!("Compile error: {e}"))?;

    let mut registry = ExternRegistry::new(compiled.extern_names.len());
    for (name, handler) in externs {
        let idx = compiled.extern_names.iter().position(|n| n == &name);
        match idx {
            Some(i) => registry.register(i, handler),
            None => {
                return Err(format!(
                    "Registered extern '{name}' was not declared in the program"
                ));
            }
        }
    }

    Ok((compiled, registry))
}

pub fn run_with_externs(
    hir_prog: &hir::Program,
    externs: HashMap<String, ExternHandler>,
    profile: Profile,
) -> Result<String, String> {
    let (compiled, registry) = compile_with_externs(hir_prog, externs, profile)?;

    let mut vm = VM::new(&compiled);
    vm.run(&registry)
        .map_err(|e| format!("Runtime error: {e}"))?;

    // we need to take the stdout before dropping the VM because the VM drops the stack
    let stdout = std::mem::take(&mut vm.stdout);
    drop(vm);

    // free any remaining suspects
    cycle_collector::collect_cycles();

    // check for memory leaks and output the details if any are found
    // the user should be able to see this if we do thing right and the cycle detector works
    let live = managed_rc::managed_alloc_count();
    if live > 0 {
        let mut msg = format!(
            "memory leak: {live} managed object(s) still alive after final cycle collection"
        );
        let mut details = managed_rc::managed_alloc_details();
        details.sort_by(|a, b| a.0.cmp(b.0));
        for (name, count) in details {
            let _ = write!(msg, "\n  {name}: {count}");
        }
        return Err(msg);
    }

    Ok(stdout)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::{
        ExternHandler, Value, managed_rc::ManagedRc, run_with_externs, value::ExternHandleData,
    };
    use crate::{Profile, test_helpers::TestCtx};

    fn noop_drop(_id: u64) {}

    fn extern_handle(id: u64) -> Value {
        Value::ExternHandle(ManagedRc::new(ExternHandleData {
            id,
            drop_fn: noop_drop,
            type_name: "Extern",
            to_string_fn: |_| "<Extern>".to_string(),
        }))
    }

    // extern type fixture for callback and handle integration tests
    struct Counter {
        value: i64,
    }

    thread_local! {
        static COUNTER_STORE: std::cell::RefCell<super::handle_store::HandleStore<Counter>> =
            std::cell::RefCell::new(super::handle_store::HandleStore::new());
    }

    impl super::extern_type::AnvyxExternType for Counter {
        const TYPE_NAME: &'static str = "Counter";
        fn with_store<R>(
            f: impl FnOnce(&std::cell::RefCell<super::handle_store::HandleStore<Self>>) -> R,
        ) -> R {
            COUNTER_STORE.with(f)
        }
        fn cleanup(id: u64) {
            Self::with_store(|s| {
                let _ = s.borrow_mut().remove(id);
            });
        }
        fn to_display(id: u64) -> String {
            Self::with_store(|s| {
                s.borrow().borrow(id).map_or_else(
                    |_| "<invalid>".to_string(),
                    |c| format!("Counter({})", c.value),
                )
            })
        }
    }

    fn vm_ok(source: &str) -> String {
        TestCtx::vm_ok(source)
    }

    fn vm_err(source: &str) -> String {
        TestCtx::vm_err(source)
    }

    fn vm_ok_with_externs(source: &str, externs: HashMap<String, ExternHandler>) -> String {
        let hir = crate::test_helpers::generate_hir(source, "<test>").expect("generate_hir failed");
        run_with_externs(&hir, externs, Profile::default()).expect("vm run failed")
    }

    fn vm_err_with_externs(source: &str, externs: HashMap<String, ExternHandler>) -> String {
        let hir = crate::test_helpers::generate_hir(source, "<test>").expect("generate_hir failed");
        run_with_externs(&hir, externs, Profile::default()).expect_err("expected vm error")
    }

    fn native_apply_externs() -> HashMap<String, ExternHandler> {
        use super::callback::with_callback_ctx;
        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "native_apply".to_string(),
            Box::new(|args| {
                let Value::Int(value) = args[0] else {
                    panic!("expected int");
                };
                let closure = args[1].clone();
                with_callback_ctx(|ctx| ctx.call_closure(&closure, vec![Value::Int(value)]))
            }),
        );
        externs
    }

    fn compile_with_store_callback(
        src: &str,
        extern_name: &str,
    ) -> (
        super::compiler::CompiledProgram,
        super::ExternRegistry,
        std::rc::Rc<std::cell::RefCell<Option<Value>>>,
    ) {
        use std::{cell::RefCell, rc::Rc};
        let stored: Rc<RefCell<Option<Value>>> = Rc::new(RefCell::new(None));
        let stored_clone = stored.clone();
        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            extern_name.to_string(),
            Box::new(move |args| {
                *stored_clone.borrow_mut() = Some(args[0].clone());
                Ok(Value::Nil)
            }),
        );
        let hir = crate::test_helpers::generate_hir(src, "<test>").expect("generate_hir failed");
        let (compiled, registry) =
            super::compile_with_externs(&hir, externs, Profile::default()).expect("compile failed");
        (compiled, registry, stored)
    }

    #[test]
    fn empty_main() {
        let out = vm_ok("fn main() {}");
        assert!(out.is_empty());
    }

    #[test]
    fn let_binding() {
        let out = vm_ok("fn main() { let x = 42; }");
        assert!(out.is_empty());
    }

    #[test]
    fn return_int() {
        let out = vm_ok("fn main() -> int { 42 }");
        assert!(out.is_empty());
    }

    #[test]
    fn println_hello() {
        let out = vm_ok(r#"fn main() { println("hello"); }"#);
        assert_eq!(out, "hello\n");
    }

    #[test]
    fn println_multiple_lines() {
        let out = vm_ok(r#"fn main() { println("a"); println("b"); }"#);
        assert_eq!(out, "a\nb\n");
    }

    #[test]
    fn assert_true_succeeds() {
        let out = vm_ok("fn main() { assert(true); }");
        assert!(out.is_empty());
    }

    #[test]
    fn assert_false_fails() {
        let err = vm_err("fn main() { assert(false); }");
        assert!(err.contains("assertion failed"));
    }

    #[test]
    fn assert_msg_false_fails_with_message() {
        let err = vm_err(r#"fn main() { assert_msg(false, "oops"); }"#);
        assert!(err.contains("oops"));
    }

    #[test]
    fn if_true_branch() {
        let out = vm_ok(r#"fn main() { if true { println("yes"); } }"#);
        assert_eq!(out, "yes\n");
    }

    #[test]
    fn if_false_skips_branch() {
        let out = vm_ok(r#"fn main() { if false { println("no"); } }"#);
        assert!(out.is_empty());
    }

    #[test]
    fn if_else() {
        let out = vm_ok(r#"fn main() { if false { println("a"); } else { println("b"); } }"#);
        assert_eq!(out, "b\n");
    }

    #[test]
    fn while_false_does_not_execute_body() {
        let out = vm_ok(r#"fn main() { while false { println("x"); } }"#);
        assert!(out.is_empty());
    }

    #[test]
    fn while_with_break() {
        let out = vm_ok(
            r#"
            fn main() {
                var i = 0;
                while true {
                    i = i + 1;
                    if i == 3 { break; }
                }
                println("done");
            }
        "#,
        );
        assert_eq!(out, "done\n");
    }

    #[test]
    fn while_with_continue() {
        let out = vm_ok(
            r#"
            fn main() {
                var i = 0;
                var sum = 0;
                while i < 5 {
                    i = i + 1;
                    if i == 3 { continue; }
                    sum = sum + i;
                }
                println("ok");
            }
        "#,
        );
        assert_eq!(out, "ok\n");
    }

    #[test]
    fn call_empty_function() {
        let out = vm_ok("fn foo() {} fn main() { foo(); }");
        assert!(out.is_empty());
    }

    #[test]
    fn call_function_with_return_value() {
        let out = vm_ok(
            r#"
            fn answer() -> int { 42 }
            fn main() { let x = answer(); println("ok"); }
        "#,
        );
        assert_eq!(out, "ok\n");
    }

    #[test]
    fn call_function_with_args() {
        let out = vm_ok(
            r"
            fn add(a: int, b: int) -> int { a + b }
            fn main() { add(1, 2); }
        ",
        );
        assert!(out.is_empty());
    }

    #[test]
    fn arithmetic_ops() {
        let out = vm_ok(
            r#"
            fn main() {
                let a = 10;
                let b = 3;
                let add = a + b;
                let sub = a - b;
                let mul = a * b;
                let div = a / b;
                let rem = a % b;
                println("ok");
            }
        "#,
        );
        assert_eq!(out, "ok\n");
    }

    #[test]
    fn string_concat() {
        let out = vm_ok(
            r#"
            fn main() {
                let s = "hello" + " " + "world";
                println(s);
            }
        "#,
        );
        assert_eq!(out, "hello world\n");
    }

    #[test]
    fn string_interp_int() {
        let out = vm_ok(r#"fn main() { let x = 42; println(f"val: {x}"); }"#);
        assert_eq!(out, "val: 42\n");
    }

    #[test]
    fn string_interp_multiple_values() {
        let out = vm_ok(r#"fn main() { let a = "hi"; let b = 3; println(f"{a} {b}"); }"#);
        assert_eq!(out, "hi 3\n");
    }

    #[test]
    fn nested_function_calls() {
        let out = vm_ok(
            r#"
            fn twice(x: int) -> int { x * 2 }
            fn quadruple(x: int) -> int { twice(twice(x)) }
            fn main() {
                quadruple(3);
                println("ok");
            }
        "#,
        );
        assert_eq!(out, "ok\n");
    }

    #[test]
    fn if_expr_returns_value() {
        let out = vm_ok(
            r#"
            fn choose(x: bool) -> int { if x { 1 } else { 2 } }
            fn main() {
                let a = choose(true);
                let b = choose(false);
                if a == 1 {
                    println("ok");
                }
                if b == 2 {
                    println("ok");
                }
            }
        "#,
        );
        assert_eq!(out, "ok\nok\n");
    }

    #[test]
    fn fib_recursive() {
        let out = vm_ok(
            r#"
            fn fib(n: int) -> int {
                if n <= 1 {
                    n
                } else {
                    fib(n - 1) + fib(n - 2)
                }
            }
            fn main() {
                let result = fib(10);
                if result == 55 {
                    println("fib ok");
                } else {
                    println("fib failed");
                }
            }
        "#,
        );
        assert_eq!(out, "fib ok\n");
    }

    #[test]
    fn bool_operations() {
        let out = vm_ok(
            r#"
            fn main() {
                let t = true;
                let f = false;
                let a = t && f;
                let b = t || f;
                let c = !t;
                println("ok");
            }
        "#,
        );
        assert_eq!(out, "ok\n");
    }

    #[test]
    fn comparison_ops() {
        let out = vm_ok(
            r#"
            fn main() {
                let x = 5;
                let eq = x == 5;
                let ne = x != 4;
                let lt = x < 10;
                let gt = x > 1;
                let le = x <= 5;
                let ge = x >= 5;
                println("ok");
            }
        "#,
        );
        assert_eq!(out, "ok\n");
    }

    // ---- extern fn vm ----

    #[test]
    fn extern_fn_returns_value() {
        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "add".to_string(),
            Box::new(|args| {
                let Value::Int(a) = args[0] else {
                    panic!("expected int")
                };
                let Value::Int(b) = args[1] else {
                    panic!("expected int")
                };
                Ok(Value::Int(a + b))
            }),
        );
        let src = r#"
            extern fn add(a: int, b: int) -> int;
            fn main() {
                let result = add(3, 4);
                assert(result == 7);
                println("ok");
            }
        "#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "ok\n");
    }

    #[test]
    fn extern_fn_zero_args() {
        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "get_answer".to_string(),
            Box::new(|_args| Ok(Value::Int(42))),
        );
        let src = r#"
            extern fn get_answer() -> int;
            fn main() {
                let x = get_answer();
                assert(x == 42);
                println("ok");
            }
        "#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "ok\n");
    }

    #[test]
    fn extern_fn_void_with_side_effect() {
        use std::{cell::RefCell, rc::Rc};
        let counter = Rc::new(RefCell::new(0i64));
        let counter_clone = counter.clone();
        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "increment".to_string(),
            Box::new(move |_args| {
                *counter_clone.borrow_mut() += 1;
                Ok(Value::Nil)
            }),
        );
        let src = r#"
            extern fn increment();
            fn main() {
                increment();
                increment();
                println("ok");
            }
        "#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "ok\n");
        assert_eq!(*counter.borrow(), 2);
    }

    #[test]
    fn extern_fn_missing_handler_errors() {
        let externs: HashMap<String, ExternHandler> = HashMap::new();
        let src = r"
            extern fn add(a: int, b: int) -> int;
            fn main() { let x = add(1, 2); }
        ";
        let err = vm_err_with_externs(src, externs);
        assert!(
            err.contains("missing extern") && err.contains("add"),
            "got: {err}"
        );
    }

    #[test]
    fn extern_fn_multiple_externs() {
        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "twice".to_string(),
            Box::new(|args| {
                let Value::Int(n) = args[0] else {
                    panic!("expected int")
                };
                Ok(Value::Int(n * 2))
            }),
        );
        externs.insert(
            "negate".to_string(),
            Box::new(|args| {
                let Value::Int(n) = args[0] else {
                    panic!("expected int")
                };
                Ok(Value::Int(-n))
            }),
        );
        let src = r#"
            extern fn twice(n: int) -> int;
            extern fn negate(n: int) -> int;
            fn main() {
                let x = twice(5);
                let y = negate(x);
                assert(y == -10);
                println("ok");
            }
        "#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "ok\n");
    }

    #[test]
    fn extern_handle_round_trip() {
        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "make_handle".to_string(),
            Box::new(|_args| Ok(extern_handle(42))),
        );
        externs.insert(
            "use_handle".to_string(),
            Box::new(|args| {
                let Value::ExternHandle(ref data) = args[0] else {
                    panic!("expected ExternHandle");
                };
                let id = data.id;
                assert_eq!(id, 42);
                Ok(Value::Nil)
            }),
        );
        let src = "
extern type Foo;
extern fn make_handle() -> Foo;
extern fn use_handle(h: Foo);
fn main() {
    let h = make_handle();
    use_handle(h);
    println(\"ok\");
}";
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "ok\n");
    }

    #[test]
    fn extern_handle_display() {
        assert_eq!(extern_handle(99).to_string(), "<extern:99>");
    }

    #[test]
    fn extern_type_field_get() {
        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "make_point".to_string(),
            Box::new(|_args| Ok(extern_handle(1))),
        );
        externs.insert(
            "Point::__get_x".to_string(),
            Box::new(|args| {
                let Value::ExternHandle(ref data) = args[0] else {
                    panic!("expected ExternHandle");
                };
                let id = data.id;
                assert_eq!(id, 1);
                Ok(Value::Float(3.5_f32))
            }),
        );
        let src = r#"
extern type Point {
    x: float;
}
extern fn make_point() -> Point;
fn main() {
    let p = make_point();
    let v = p.x;
    assert(v == 3.5);
    println("ok");
}
"#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "ok\n");
    }

    #[test]
    fn extern_type_field_set() {
        use std::{cell::RefCell, rc::Rc};
        let captured = Rc::new(RefCell::new(0.0f64));
        let captured_clone = captured.clone();
        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "make_point".to_string(),
            Box::new(|_args| Ok(extern_handle(1))),
        );
        externs.insert(
            "Point::__set_x".to_string(),
            Box::new(move |args| {
                let Value::ExternHandle(ref data) = args[0] else {
                    panic!("expected ExternHandle");
                };
                let id = data.id;
                assert_eq!(id, 1);
                let Value::Float(val) = args[1] else {
                    panic!("expected Float");
                };
                *captured_clone.borrow_mut() = f64::from(val);
                Ok(Value::Nil)
            }),
        );
        let src = r#"
extern type Point {
    x: float;
}
extern fn make_point() -> Point;
fn main() {
    var p = make_point();
    p.x = 42.0;
    println("ok");
}
"#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "ok\n");
        assert_eq!(*captured.borrow(), 42.0);
    }

    #[test]
    fn extern_type_instance_method() {
        use std::{cell::RefCell, rc::Rc};
        let captured = Rc::new(RefCell::new((0u64, 0.0f64, 0.0f64)));
        let captured_clone = captured.clone();
        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "make_point".to_string(),
            Box::new(|_args| Ok(extern_handle(7))),
        );
        externs.insert(
            "Point::move_by".to_string(),
            Box::new(move |args| {
                let Value::ExternHandle(ref data) = args[0] else {
                    panic!("expected ExternHandle");
                };
                let id = data.id;
                let Value::Float(dx) = args[1] else {
                    panic!("expected Float dx");
                };
                let Value::Float(dy) = args[2] else {
                    panic!("expected Float dy");
                };
                *captured_clone.borrow_mut() = (id, f64::from(dx), f64::from(dy));
                Ok(Value::Nil)
            }),
        );
        let src = r#"
extern type Point {
    fn move_by(var self, dx: float, dy: float);
}
extern fn make_point() -> Point;
fn main() {
    var p = make_point();
    p.move_by(5.0, -3.0);
    println("ok");
}
"#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "ok\n");
        let vals = *captured.borrow();
        assert_eq!(vals, (7, 5.0, -3.0));
    }

    #[test]
    fn extern_type_method_returns_value() {
        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "make_point".to_string(),
            Box::new(|_args| Ok(extern_handle(1))),
        );
        externs.insert(
            "Point::length".to_string(),
            Box::new(|args| {
                let Value::ExternHandle(_) = args[0] else {
                    panic!("expected ExternHandle");
                };
                Ok(Value::Float(5.0_f32))
            }),
        );
        let src = r#"
extern type Point {
    fn length(self) -> float;
}
extern fn make_point() -> Point;
fn main() {
    let p = make_point();
    let d = p.length();
    assert(d == 5.0);
    println("ok");
}
"#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "ok\n");
    }

    #[test]
    fn extern_type_combined() {
        use std::{cell::RefCell, rc::Rc};
        let set_val = Rc::new(RefCell::new(0.0f64));
        let set_val_clone = set_val.clone();
        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "make_point".to_string(),
            Box::new(|_args| Ok(extern_handle(1))),
        );
        externs.insert(
            "Point::__get_x".to_string(),
            Box::new(|args| {
                let Value::ExternHandle(_) = args[0] else {
                    panic!("expected ExternHandle");
                };
                Ok(Value::Float(10.0_f32))
            }),
        );
        externs.insert(
            "Point::__set_x".to_string(),
            Box::new(move |args| {
                let Value::ExternHandle(_) = args[0] else {
                    panic!("expected ExternHandle");
                };
                let Value::Float(val) = args[1] else {
                    panic!("expected Float");
                };
                *set_val_clone.borrow_mut() = f64::from(val);
                Ok(Value::Nil)
            }),
        );
        externs.insert(
            "Point::length".to_string(),
            Box::new(|args| {
                let Value::ExternHandle(_) = args[0] else {
                    panic!("expected ExternHandle");
                };
                Ok(Value::Float(5.0_f32))
            }),
        );
        let src = r#"
extern type Point {
    x: float;
    fn length(self) -> float;
}
extern fn make_point() -> Point;
fn main() {
    var p = make_point();
    let v = p.x;
    assert(v == 10.0);
    let d = p.length();
    assert(d == 5.0);
    p.x = 99.0;
    println("ok");
}
"#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "ok\n");
        assert_eq!(*set_val.borrow(), 99.0);
    }

    #[test]
    fn extern_type_static_method() {
        use std::{cell::RefCell, rc::Rc};
        let called_args = Rc::new(RefCell::new(vec![]));
        let called_args_clone = called_args.clone();
        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "Point::new".to_string(),
            Box::new(move |args| {
                *called_args_clone.borrow_mut() = args.clone();
                Ok(extern_handle(42))
            }),
        );
        let src = r#"
extern type Point {
    fn new(x: float, y: float) -> Self;
}
fn main() {
    let p = Point.new(1.0, 2.0);
    println("ok");
}
"#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "ok\n");
        let args = called_args.borrow();
        assert_eq!(args.len(), 2);
        assert_eq!(args[0], Value::Float(1.0_f32));
        assert_eq!(args[1], Value::Float(2.0_f32));
    }

    #[test]
    fn extern_type_static_with_instance() {
        use std::{cell::RefCell, rc::Rc};
        let move_args = Rc::new(RefCell::new((0u64, 0.0f64, 0.0f64)));
        let move_args_clone = move_args.clone();
        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "Point::new".to_string(),
            Box::new(|_args| Ok(extern_handle(7))),
        );
        externs.insert(
            "Point::__get_x".to_string(),
            Box::new(|args| {
                let Value::ExternHandle(_) = args[0] else {
                    panic!("expected ExternHandle");
                };
                Ok(Value::Float(3.0_f32))
            }),
        );
        externs.insert(
            "Point::move_by".to_string(),
            Box::new(move |args| {
                let Value::ExternHandle(ref data) = args[0] else {
                    panic!("expected ExternHandle");
                };
                let id = data.id;
                let Value::Float(dx) = args[1] else {
                    panic!("expected Float dx");
                };
                let Value::Float(dy) = args[2] else {
                    panic!("expected Float dy");
                };
                *move_args_clone.borrow_mut() = (id, f64::from(dx), f64::from(dy));
                Ok(Value::Nil)
            }),
        );
        let src = r#"
extern type Point {
    x: float;
    fn new(x: float, y: float) -> Self;
    fn move_by(var self, dx: float, dy: float);
}
fn main() {
    var p = Point.new(1.0, 2.0);
    let v = p.x;
    assert(v == 3.0);
    p.move_by(5.0, -3.0);
    println("ok");
}
"#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "ok\n");
        let vals = *move_args.borrow();
        assert_eq!(vals, (7, 5.0, -3.0));
    }

    #[test]
    fn extern_type_static_void() {
        use std::{cell::RefCell, rc::Rc};
        let called = Rc::new(RefCell::new(false));
        let called_clone = called.clone();
        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "Logger::reset".to_string(),
            Box::new(move |_args| {
                *called_clone.borrow_mut() = true;
                Ok(Value::Nil)
            }),
        );
        let src = r#"
extern type Logger {
    fn reset();
}
fn main() {
    Logger.reset();
    println("ok");
}
"#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "ok\n");
        assert!(*called.borrow());
    }

    #[test]
    fn extern_type_struct_literal_init() {
        use std::{cell::RefCell, rc::Rc};
        let init_args = Rc::new(RefCell::new(vec![]));
        let init_args_clone = init_args.clone();
        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "Point::__init__".to_string(),
            Box::new(move |args| {
                *init_args_clone.borrow_mut() = args.clone();
                Ok(extern_handle(10))
            }),
        );
        externs.insert(
            "Point::__get_x".to_string(),
            Box::new(|args| {
                let Value::ExternHandle(_) = args[0] else {
                    panic!("expected ExternHandle")
                };
                Ok(Value::Float(1.0_f32))
            }),
        );
        externs.insert(
            "Point::__get_y".to_string(),
            Box::new(|args| {
                let Value::ExternHandle(_) = args[0] else {
                    panic!("expected ExternHandle")
                };
                Ok(Value::Float(2.0_f32))
            }),
        );
        let src = r#"
extern type Point { init; x: float; y: float; }
fn main() {
    let p = Point { x: 1.0, y: 2.0 };
    assert(p.x == 1.0);
    assert(p.y == 2.0);
    println("ok");
}
"#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "ok\n");
        let args = init_args.borrow();
        assert_eq!(args.len(), 2);
        assert_eq!(args[0], Value::Float(1.0_f32));
        assert_eq!(args[1], Value::Float(2.0_f32));
    }

    #[test]
    fn extern_type_destructure() {
        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert("make_point".to_string(), Box::new(|_| Ok(extern_handle(5))));
        externs.insert(
            "Point::__get_x".to_string(),
            Box::new(|args| {
                let Value::ExternHandle(_) = args[0] else {
                    panic!("expected ExternHandle")
                };
                Ok(Value::Float(7.5_f32))
            }),
        );
        externs.insert(
            "Point::__get_y".to_string(),
            Box::new(|args| {
                let Value::ExternHandle(_) = args[0] else {
                    panic!("expected ExternHandle")
                };
                Ok(Value::Float(3.0_f32))
            }),
        );
        let src = r#"
extern type Point { x: float; y: float; }
extern fn make_point() -> Point;
fn main() {
    let p = make_point();
    let Point { x, y } = p;
    assert(x == 7.5);
    assert(y == 3.0);
    println("ok");
}
"#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "ok\n");
    }

    #[test]
    fn extern_type_init_destructure_round_trip() {
        use std::{cell::RefCell, rc::Rc};
        let state = Rc::new(RefCell::new((0.0f64, 0.0f64)));
        let state_init = state.clone();
        let state_move = state.clone();
        let state_get_x = state.clone();
        let state_get_y = state.clone();
        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "Point::__init__".to_string(),
            Box::new(move |args| {
                let Value::Float(x) = args[0] else {
                    panic!("expected Float")
                };
                let Value::Float(y) = args[1] else {
                    panic!("expected Float")
                };
                *state_init.borrow_mut() = (f64::from(x), f64::from(y));
                Ok(extern_handle(1))
            }),
        );
        externs.insert(
            "Point::move_by".to_string(),
            Box::new(move |args| {
                let Value::ExternHandle(_) = args[0] else {
                    panic!("expected ExternHandle")
                };
                let Value::Float(dx) = args[1] else {
                    panic!("expected Float")
                };
                let Value::Float(dy) = args[2] else {
                    panic!("expected Float")
                };
                let mut s = state_move.borrow_mut();
                s.0 += f64::from(dx);
                s.1 += f64::from(dy);
                Ok(Value::Nil)
            }),
        );
        externs.insert(
            "Point::__get_x".to_string(),
            Box::new(move |_| Ok(Value::Float(state_get_x.borrow().0 as f32))),
        );
        externs.insert(
            "Point::__get_y".to_string(),
            Box::new(move |_| Ok(Value::Float(state_get_y.borrow().1 as f32))),
        );
        let src = r#"
extern type Point {
    init;
    x: float;
    y: float;
    fn move_by(var self, dx: float, dy: float);
}
fn main() {
    var p = Point { x: 10.0, y: 20.0 };
    p.move_by(5.0, -3.0);
    let Point { x, y } = p;
    assert(x == 15.0);
    assert(y == 17.0);
    println("ok");
}
"#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "ok\n");
    }

    #[test]
    fn extern_handle_to_string_custom() {
        fn custom_fmt(_id: u64) -> String {
            "MyWidget".to_string()
        }
        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "make_widget".to_string(),
            Box::new(|_args| {
                Ok(Value::ExternHandle(ManagedRc::new(ExternHandleData {
                    id: 1,
                    drop_fn: noop_drop,
                    type_name: "Widget",
                    to_string_fn: custom_fmt,
                })))
            }),
        );
        let src = r"
extern type Widget;
extern fn make_widget() -> Widget;
fn main() {
    let w = make_widget();
    println(w);
}";
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "MyWidget\n");
    }

    #[test]
    fn extern_handle_to_string_fallback() {
        fn fallback_fmt(_id: u64) -> String {
            "<Window>".to_string()
        }
        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "make_window".to_string(),
            Box::new(|_args| {
                Ok(Value::ExternHandle(ManagedRc::new(ExternHandleData {
                    id: 1,
                    drop_fn: noop_drop,
                    type_name: "Window",
                    to_string_fn: fallback_fmt,
                })))
            }),
        );
        let src = r"
extern type Window;
extern fn make_window() -> Window;
fn main() {
    let w = make_window();
    println(w);
}";
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "<Window>\n");
    }

    #[test]
    fn ownership_rc_reduction_benchmark() {
        use super::managed_rc::{rc_dec_count, rc_inc_count, reset_rc_counts};

        let source = r"
            struct Point { x: int, y: int }

            fn make_point(x: int, y: int) -> Point {
                Point { x: x, y: y }
            }

            fn sum_point(p: Point) -> int {
                p.x + p.y
            }

            fn transform(p: Point) -> Point {
                let x = p.x * 2;
                let y = p.y + 1;
                make_point(x, y)
            }

            fn main() {
                var total = 0;
                var i = 0;
                while i < 100 {
                    let p = make_point(i, i + 1);
                    let q = transform(p);
                    total = total + sum_point(q);
                    i = i + 1;
                }
                println(total);
            }
        ";

        reset_rc_counts();
        let out = vm_ok(source);
        let incs = rc_inc_count();
        let decs = rc_dec_count();

        eprintln!("RC increments: {incs}, RC decrements: {decs}");

        assert!(!out.is_empty());
    }

    #[test]
    fn extern_handle_to_string_in_list() {
        fn item_fmt(_id: u64) -> String {
            "Item".to_string()
        }
        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "make_item".to_string(),
            Box::new(|_args| {
                Ok(Value::ExternHandle(ManagedRc::new(ExternHandleData {
                    id: 1,
                    drop_fn: noop_drop,
                    type_name: "Item",
                    to_string_fn: item_fmt,
                })))
            }),
        );
        let src = r"
extern type Item;
extern fn make_item() -> Item;
fn main() {
    let items = [make_item(), make_item()];
    println(items);
}";
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "[Item, Item]\n");
    }

    #[test]
    fn extern_callback_named_fn() {
        let externs = native_apply_externs();
        let src = r#"
            extern fn native_apply(value: int, cb: fn(int) -> int) -> int;
            fn mul2(x: int) -> int {
                return x * 2;
            }
            fn main() {
                let result = native_apply(5, mul2);
                assert(result == 10);
                println("ok");
            }
        "#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "ok\n");
    }

    #[test]
    fn extern_callback_closure_with_capture() {
        let externs = native_apply_externs();
        let src = r#"
            extern fn native_apply(value: int, cb: fn(int) -> int) -> int;
            fn main() {
                let offset = 100;
                let result = native_apply(5, |x| x + offset);
                assert(result == 105);
                println("ok");
            }
        "#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "ok\n");
    }

    #[test]
    fn extern_callback_closure_with_builtin() {
        let externs = native_apply_externs();
        let src = r#"
            extern fn native_apply(value: int, cb: fn(int) -> int) -> int;
            fn main() {
                let result = native_apply(5, |x| {
                    println(x);
                    x * 2
                });
                assert(result == 10);
                println("builtin ok");
            }
        "#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "5\nbuiltin ok\n");
    }

    #[test]
    fn host_driven_callback() {
        let src = r"
            extern fn store_callback(cb: fn(int) -> int);
            fn main() {
                store_callback(|x| x * 3);
            }
        ";
        let (compiled, registry, stored) = compile_with_store_callback(src, "store_callback");
        let mut vm = super::runtime::VM::new(&compiled);
        vm.run(&registry).expect("vm run failed");
        let closure_val = stored.borrow().clone().expect("closure not stored");
        let result = vm
            .call_closure(&registry, &closure_val, vec![Value::Int(7)])
            .expect("callback failed");
        assert_eq!(result, Value::Int(21));
    }

    #[test]
    fn anvyx_fn_typed_callback() {
        use super::{callback::AnvyxFn, extern_type::AnvyxConvert};

        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "native_apply".to_string(),
            Box::new(|args| {
                let value = <i64 as AnvyxConvert>::from_anvyx(&args[0])?;
                let cb = <AnvyxFn<(i64,), i64> as AnvyxConvert>::from_anvyx(&args[1])?;
                let result = cb.call(value)?;
                Ok(result.into_anvyx())
            }),
        );
        let src = r#"
            extern fn native_apply(value: int, cb: fn(int) -> int) -> int;
            fn main() {
                let result = native_apply(5, |x| x * 2);
                assert(result == 10);
                println("typed ok");
            }
        "#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "typed ok\n");
    }

    #[test]
    fn anvyx_fn_void_callback() {
        // Note: closures passed to non-inline extern functions cannot call builtin functions
        // (println, assert, etc.) — those names end up in the capture list because register_builtins
        // pushes them at scope 0 (below any lambda boundary). The empty-block body avoids this.
        use super::{callback::AnvyxFn, extern_type::AnvyxConvert};

        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "native_each".to_string(),
            Box::new(|args| {
                let cb = <AnvyxFn<(i64,), ()> as AnvyxConvert>::from_anvyx(&args[0])?;
                for i in 1..=3_i64 {
                    cb.call(i)?;
                }
                Ok(Value::Nil)
            }),
        );
        let src = r#"
            extern fn native_each(cb: fn(int) -> void);
            fn main() {
                native_each(|x| { });
                println("void ok");
            }
        "#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "void ok\n");
    }

    #[test]
    fn anvyx_fn_multi_arg_callback() {
        use super::{callback::AnvyxFn, extern_type::AnvyxConvert};

        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "native_fold".to_string(),
            Box::new(|args| {
                let init = <i64 as AnvyxConvert>::from_anvyx(&args[0])?;
                let cb = <AnvyxFn<(i64, i64), i64> as AnvyxConvert>::from_anvyx(&args[1])?;
                let mut acc = init;
                for i in 1..=3_i64 {
                    acc = cb.call(acc, i)?;
                }
                Ok(acc.into_anvyx())
            }),
        );
        let src = r#"
            extern fn native_fold(init: int, cb: fn(int, int) -> int) -> int;
            fn main() {
                let result = native_fold(0, |acc, x| acc + x);
                assert(result == 6);
                println("fold ok");
            }
        "#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "fold ok\n");
    }

    #[test]
    fn anvyx_fn_named_fn_callback() {
        use super::{callback::AnvyxFn, extern_type::AnvyxConvert};

        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "native_apply".to_string(),
            Box::new(|args| {
                let value = <i64 as AnvyxConvert>::from_anvyx(&args[0])?;
                let cb = <AnvyxFn<(i64,), i64> as AnvyxConvert>::from_anvyx(&args[1])?;
                let result = cb.call(value)?;
                Ok(result.into_anvyx())
            }),
        );
        let src = r#"
            extern fn native_apply(value: int, cb: fn(int) -> int) -> int;
            fn mul3(x: int) -> int {
                x * 3
            }
            fn main() {
                let result = native_apply(4, mul3);
                assert(result == 12);
                println("named ok");
            }
        "#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "named ok\n");
    }

    #[test]
    fn anvyx_fn_error_propagation() {
        use super::{callback::AnvyxFn, extern_type::AnvyxConvert};

        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "native_apply".to_string(),
            Box::new(|args| {
                let value = <i64 as AnvyxConvert>::from_anvyx(&args[0])?;
                let cb = <AnvyxFn<(i64,), i64> as AnvyxConvert>::from_anvyx(&args[1])?;
                let result = cb.call(value)?;
                Ok(result.into_anvyx())
            }),
        );
        let src = r"
            extern fn native_apply(value: int, cb: fn(int) -> int) -> int;
            fn fail_fn(x: int) -> int {
                assert(false);
                x
            }
            fn main() {
                native_apply(1, fail_fn);
            }
        ";
        let err = vm_err_with_externs(src, externs);
        assert!(err.contains("assertion failed"), "got: {err}");
    }

    #[test]
    fn anvyx_fn_multiple_callback_params() {
        use super::{callback::AnvyxFn, extern_type::AnvyxConvert};

        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "apply_both".to_string(),
            Box::new(|args| {
                let value = <i64 as AnvyxConvert>::from_anvyx(&args[0])?;
                let cb_a = <AnvyxFn<(i64,), i64> as AnvyxConvert>::from_anvyx(&args[1])?;
                let cb_b = <AnvyxFn<(i64,), i64> as AnvyxConvert>::from_anvyx(&args[2])?;
                let a = cb_a.call(value)?;
                let b = cb_b.call(value)?;
                Ok((a + b).into_anvyx())
            }),
        );
        let src = r#"
            extern fn apply_both(value: int, f: fn(int) -> int, g: fn(int) -> int) -> int;
            fn mul2(x: int) -> int { x * 2 }
            fn mul3(x: int) -> int { x * 3 }
            fn main() {
                let result = apply_both(5, mul2, mul3);
                assert(result == 25);
                println("both ok");
            }
        "#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "both ok\n");
    }

    #[test]
    fn anvyx_fn_zero_arg_callback() {
        use super::{callback::AnvyxFn, extern_type::AnvyxConvert};

        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "native_get".to_string(),
            Box::new(|args| {
                let cb = <AnvyxFn<(), i64> as AnvyxConvert>::from_anvyx(&args[0])?;
                let result = cb.call()?;
                Ok(result.into_anvyx())
            }),
        );
        let src = r#"
            extern fn native_get(cb: fn() -> int) -> int;
            fn answer() -> int { 42 }
            fn main() {
                let result = native_get(answer);
                assert(result == 42);
                println("zero arg ok");
            }
        "#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "zero arg ok\n");
    }

    #[test]
    fn anvyx_fn_callback_called_multiple_times() {
        use super::{callback::AnvyxFn, extern_type::AnvyxConvert};

        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "native_sum_squares".to_string(),
            Box::new(|args| {
                let cb = <AnvyxFn<(i64,), i64> as AnvyxConvert>::from_anvyx(&args[0])?;
                let mut total = 0_i64;
                for i in 1..=4_i64 {
                    total += cb.call(i)?;
                }
                Ok(total.into_anvyx())
            }),
        );
        // sum of squares 1..=4: 1+4+9+16=30
        let src = r#"
            extern fn native_sum_squares(cb: fn(int) -> int) -> int;
            fn main() {
                let total = native_sum_squares(|x| x * x);
                assert(total == 30);
                println("multi call ok");
            }
        "#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "multi call ok\n");
    }

    #[test]
    fn anvyx_fn_nested_callback() {
        // Tests that AnvyxFn callbacks can be nested at the host level:
        // native_compose(f, g)(x) == f(g(x))
        // f = |x| x * 3, g = |x| x + 1, x = 4 => g(4)=5, f(5)=15
        use super::{callback::AnvyxFn, extern_type::AnvyxConvert};

        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "native_compose".to_string(),
            Box::new(|args| {
                let value = <i64 as AnvyxConvert>::from_anvyx(&args[0])?;
                let f = <AnvyxFn<(i64,), i64> as AnvyxConvert>::from_anvyx(&args[1])?;
                let g = <AnvyxFn<(i64,), i64> as AnvyxConvert>::from_anvyx(&args[2])?;
                let mid = g.call(value)?;
                let result = f.call(mid)?;
                Ok(result.into_anvyx())
            }),
        );
        let src = r#"
            extern fn native_compose(value: int, f: fn(int) -> int, g: fn(int) -> int) -> int;
            fn main() {
                let result = native_compose(4, |x| x * 3, |x| x + 1);
                assert(result == 15);
                println("nested ok");
            }
        "#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "nested ok\n");
    }

    #[test]
    fn anvyx_fn_bool_returning_callback() {
        use super::{callback::AnvyxFn, extern_type::AnvyxConvert};

        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "native_count_if".to_string(),
            Box::new(|args| {
                let cb = <AnvyxFn<(i64,), bool> as AnvyxConvert>::from_anvyx(&args[0])?;
                let mut count = 0_i64;
                for i in 1..=10_i64 {
                    if cb.call(i)? {
                        count += 1;
                    }
                }
                Ok(count.into_anvyx())
            }),
        );
        let src = r#"
            extern fn native_count_if(cb: fn(int) -> bool) -> int;
            fn main() {
                let evens = native_count_if(|x| x % 2 == 0);
                assert(evens == 5);
                println("bool cb ok");
            }
        "#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "bool cb ok\n");
    }

    #[test]
    fn anvyx_fn_closure_with_capture_typed() {
        use super::{callback::AnvyxFn, extern_type::AnvyxConvert};

        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "native_apply".to_string(),
            Box::new(|args| {
                let value = <i64 as AnvyxConvert>::from_anvyx(&args[0])?;
                let cb = <AnvyxFn<(i64,), i64> as AnvyxConvert>::from_anvyx(&args[1])?;
                let result = cb.call(value)?;
                Ok(result.into_anvyx())
            }),
        );
        let src = r#"
            extern fn native_apply(value: int, cb: fn(int) -> int) -> int;
            fn main() {
                let offset = 100;
                let result = native_apply(5, |x| x + offset);
                assert(result == 105);
                println("capture typed ok");
            }
        "#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "capture typed ok\n");
    }

    #[test]
    fn anvyx_fn_callback_with_extern_handle() {
        // Proves ExternHandle<T> flows through AnvyxFn: Rust creates the handle,
        // passes it to the closure, and the closure reads a field via the getter.
        use super::{
            callback::AnvyxFn,
            extern_type::{AnvyxConvert, ExternHandle},
        };

        let mut externs: HashMap<String, ExternHandler> = HashMap::new();

        externs.insert(
            "run_with_counter".to_string(),
            Box::new(|args| {
                let cb =
                    <AnvyxFn<(ExternHandle<Counter>,), i64> as AnvyxConvert>::from_anvyx(&args[0])?;
                let handle = ExternHandle::new(Counter { value: 99 });
                let result = cb.call(handle)?;
                Ok(result.into_anvyx())
            }),
        );

        externs.insert(
            "Counter::__get_value".to_string(),
            Box::new(|args| {
                let handle = <ExternHandle<Counter> as AnvyxConvert>::from_anvyx(&args[0])?;
                let v = handle.with_borrow(|c| c.value)?;
                Ok(v.into_anvyx())
            }),
        );

        let src = r#"
            extern type Counter {
                value: int;
            }
            extern fn run_with_counter(cb: fn(Counter) -> int) -> int;
            fn main() {
                let result = run_with_counter(|c| c.value);
                assert(result == 99);
                println("handle callback ok");
            }
        "#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "handle callback ok\n");
    }

    #[test]
    fn anvyx_fn_callback_with_extern_handle_method() {
        // Proves ExternHandle<T> flows through AnvyxFn: Rust creates the handle,
        // passes it to the closure, and the closure calls an immutable method on it.
        use super::{
            callback::AnvyxFn,
            extern_type::{AnvyxConvert, ExternHandle},
        };

        let mut externs: HashMap<String, ExternHandler> = HashMap::new();

        externs.insert(
            "run_with_counter".to_string(),
            Box::new(|args| {
                let cb =
                    <AnvyxFn<(ExternHandle<Counter>,), i64> as AnvyxConvert>::from_anvyx(&args[0])?;
                let handle = ExternHandle::new(Counter { value: 42 });
                let result = cb.call(handle)?;
                Ok(result.into_anvyx())
            }),
        );

        externs.insert(
            "Counter::current".to_string(),
            Box::new(|args| {
                let handle = <ExternHandle<Counter> as AnvyxConvert>::from_anvyx(&args[0])?;
                let v = handle.with_borrow(|c| c.value)?;
                Ok(v.into_anvyx())
            }),
        );

        let src = r#"
            extern type Counter {
                fn current(self) -> int;
            }
            extern fn run_with_counter(cb: fn(Counter) -> int) -> int;
            fn main() {
                let result = run_with_counter(|c| c.current());
                assert(result == 42);
                println("handle method ok");
            }
        "#;
        let out = vm_ok_with_externs(src, externs);
        assert_eq!(out, "handle method ok\n");
    }

    #[test]
    fn host_driven_multiple_calls() {
        let src = r"
            extern fn store_callback(cb: fn(int) -> int);
            fn main() {
                store_callback(|x| x * x);
            }
        ";
        let (compiled, registry, stored) = compile_with_store_callback(src, "store_callback");
        let mut vm = super::runtime::VM::new(&compiled);
        vm.run(&registry).expect("vm run failed");
        let closure_val = stored.borrow().clone().expect("closure not stored");
        for i in 1..=5_i64 {
            let result = vm
                .call_closure(&registry, &closure_val, vec![Value::Int(i)])
                .expect("callback failed");
            assert_eq!(result, Value::Int(i * i));
        }
    }

    #[test]
    fn host_driven_multiple_closures() {
        use std::{cell::RefCell, rc::Rc};

        let stored_update: Rc<RefCell<Option<Value>>> = Rc::new(RefCell::new(None));
        let stored_draw: Rc<RefCell<Option<Value>>> = Rc::new(RefCell::new(None));
        let update_clone = stored_update.clone();
        let draw_clone = stored_draw.clone();

        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "register_update".to_string(),
            Box::new(move |args| {
                *update_clone.borrow_mut() = Some(args[0].clone());
                Ok(Value::Nil)
            }),
        );
        externs.insert(
            "register_draw".to_string(),
            Box::new(move |args| {
                *draw_clone.borrow_mut() = Some(args[0].clone());
                Ok(Value::Nil)
            }),
        );

        let src = r"
            extern fn register_update(cb: fn(int) -> int);
            extern fn register_draw(cb: fn() -> int);
            fn main() {
                register_update(|dt| dt * 2);
                register_draw(|| 42);
            }
        ";

        let hir = crate::test_helpers::generate_hir(src, "<test>").expect("generate_hir failed");
        let (compiled, registry) =
            super::compile_with_externs(&hir, externs, Profile::default()).expect("compile failed");

        let mut vm = super::runtime::VM::new(&compiled);
        vm.run(&registry).expect("vm run failed");

        let update_val = stored_update.borrow().clone().expect("update not stored");
        let draw_val = stored_draw.borrow().clone().expect("draw not stored");

        let result = vm
            .call_closure(&registry, &update_val, vec![Value::Int(10)])
            .expect("update callback failed");
        assert_eq!(result, Value::Int(20));

        let result = vm
            .call_closure(&registry, &draw_val, vec![])
            .expect("draw callback failed");
        assert_eq!(result, Value::Int(42));
    }

    #[test]
    fn host_driven_named_fn() {
        let src = r"
            extern fn store_callback(cb: fn(int) -> int);
            fn triple(x: int) -> int { x * 3 }
            fn main() {
                store_callback(triple);
            }
        ";
        let (compiled, registry, stored) = compile_with_store_callback(src, "store_callback");
        let mut vm = super::runtime::VM::new(&compiled);
        vm.run(&registry).expect("vm run failed");
        let closure_val = stored.borrow().clone().expect("closure not stored");
        let result = vm
            .call_closure(&registry, &closure_val, vec![Value::Int(5)])
            .expect("callback failed");
        assert_eq!(result, Value::Int(15));
    }

    #[test]
    fn host_driven_callback_with_stdout() {
        let src = r"
            extern fn store_callback(cb: fn(int) -> int);
            fn process(x: int) -> int {
                println(x);
                x + 1
            }
            fn main() {
                store_callback(process);
            }
        ";
        let (compiled, registry, stored) = compile_with_store_callback(src, "store_callback");
        let mut vm = super::runtime::VM::new(&compiled);
        vm.run(&registry).expect("vm run failed");
        let closure_val = stored.borrow().clone().expect("closure not stored");

        let result = vm
            .call_closure(&registry, &closure_val, vec![Value::Int(10)])
            .expect("callback failed");
        assert_eq!(result, Value::Int(11));
        assert_eq!(vm.stdout, "10\n");

        let result = vm
            .call_closure(&registry, &closure_val, vec![Value::Int(20)])
            .expect("callback failed");
        assert_eq!(result, Value::Int(21));
        assert_eq!(vm.stdout, "10\n20\n");
    }

    #[test]
    fn host_driven_callback_error() {
        let src = r"
            extern fn store_callback(cb: fn(int) -> int);
            fn guarded(x: int) -> int {
                assert(x > 0);
                x
            }
            fn main() {
                store_callback(guarded);
            }
        ";
        let (compiled, registry, stored) = compile_with_store_callback(src, "store_callback");
        let mut vm = super::runtime::VM::new(&compiled);
        vm.run(&registry).expect("vm run failed");
        let closure_val = stored.borrow().clone().expect("closure not stored");

        let result = vm
            .call_closure(&registry, &closure_val, vec![Value::Int(5)])
            .expect("callback should succeed");
        assert_eq!(result, Value::Int(5));

        let err = vm
            .call_closure(&registry, &closure_val, vec![Value::Int(-1)])
            .expect_err("callback should fail");
        assert!(err.to_string().contains("assertion failed"), "got: {err}");
    }

    #[test]
    fn host_driven_closure_with_capture() {
        let src = r"
            extern fn store_callback(cb: fn(int) -> int);
            fn main() {
                let factor = 7;
                store_callback(|x| x * factor);
            }
        ";
        let (compiled, registry, stored) = compile_with_store_callback(src, "store_callback");
        let mut vm = super::runtime::VM::new(&compiled);
        vm.run(&registry).expect("vm run failed");
        let closure_val = stored.borrow().clone().expect("closure not stored");
        let result = vm
            .call_closure(&registry, &closure_val, vec![Value::Int(3)])
            .expect("callback failed");
        assert_eq!(result, Value::Int(21));
    }

    #[test]
    fn host_driven_simulate_game_loop() {
        use std::{cell::RefCell, rc::Rc};

        let stored_update: Rc<RefCell<Option<Value>>> = Rc::new(RefCell::new(None));
        let stored_draw: Rc<RefCell<Option<Value>>> = Rc::new(RefCell::new(None));
        let update_clone = stored_update.clone();
        let draw_clone = stored_draw.clone();

        let mut externs: HashMap<String, ExternHandler> = HashMap::new();
        externs.insert(
            "app_run".to_string(),
            Box::new(move |args| {
                *update_clone.borrow_mut() = Some(args[0].clone());
                *draw_clone.borrow_mut() = Some(args[1].clone());
                Ok(Value::Nil)
            }),
        );

        let src = r"
            extern fn app_run(update: fn(int) -> int, draw: fn() -> int);
            fn main() {
                app_run(|dt| dt + 1, || 99);
            }
        ";

        let hir = crate::test_helpers::generate_hir(src, "<test>").expect("generate_hir failed");
        let (compiled, registry) =
            super::compile_with_externs(&hir, externs, Profile::default()).expect("compile failed");

        let mut vm = super::runtime::VM::new(&compiled);
        vm.run(&registry).expect("vm run failed");

        let update_val = stored_update.borrow().clone().expect("update not stored");
        let draw_val = stored_draw.borrow().clone().expect("draw not stored");

        for frame in 0..3_i64 {
            let dt = frame + 1;
            let update_result = vm
                .call_closure(&registry, &update_val, vec![Value::Int(dt)])
                .expect("update failed");
            assert_eq!(update_result, Value::Int(dt + 1));

            let draw_result = vm
                .call_closure(&registry, &draw_val, vec![])
                .expect("draw failed");
            assert_eq!(draw_result, Value::Int(99));
        }
    }
}
