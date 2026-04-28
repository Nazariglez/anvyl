use super::support::{
    assert_type_with_modules, assert_type_with_named_modules, typecheck_with_always_active_modules,
    typecheck_with_named_modules,
};
use crate::{
    ast::{Ident, Type},
    resolve::ModulePath,
    typecheck::{
        GenericArgs,
        call_map::CallTarget,
        decls::{CallableId, ModuleScope},
    },
};

const FIXED_BUF_MOD: &str = "pub struct FixedBuf<T, N: int> { data: [T; N] }";
const LEN_EXT_MOD: &str = "
    import bufs { FixedBuf };
    pub fn keep() {}
    pub extend<T, N: int> FixedBuf<T, N> { fn len(self) -> int { N } }
";
const LEN_EXT_ROOT: &str = "
    import bufs { FixedBuf };
    import exts { keep };
    fn use_it() -> int { keep(); FixedBuf { data: [1, 2] }.len() }
";

#[test]
fn qualified_type() {
    let dep = "pub struct GameKit {}";
    let root = "
        import gamekit as gk;
        fn use_it(x: gk.GameKit) {}
    ";
    assert_type_with_modules(root, dep, Type::Void);
}

#[test]
fn selective_self_type() {
    let dep = "pub struct GameKit {}";
    let root = "
        import gamekit { self };
        fn use_it(x: gamekit.GameKit) {}
    ";
    assert_type_with_modules(root, dep, Type::Void);
}

#[test]
fn selective_self_alias_type() {
    let dep = "pub struct GameKit {}";
    let root = "
        import gamekit { self as gk };
        fn use_it(x: gk.GameKit) {}
    ";
    assert_type_with_modules(root, dep, Type::Void);
}

#[test]
fn selective_imported_type() {
    let dep = "pub struct GameKit {}";
    let root = "
        import gamekit { GameKit };
        fn use_it(x: GameKit) {}
    ";
    assert_type_with_modules(root, dep, Type::Void);
}

#[test]
fn selective_imported_enum() {
    let dep = "pub enum Color { Red, Green, Blue }";
    let root = "
        import gamekit { Color };
        fn use_it(x: Color) {}
    ";
    assert_type_with_modules(root, dep, Type::Void);
}

#[test]
fn selective_imported_dataref() {
    let dep = "pub dataref Vec2 { x: float, y: float }";
    let root = "
        import gamekit { Vec2 };
        fn use_it(x: Vec2) {}
    ";
    assert_type_with_modules(root, dep, Type::Void);
}

#[test]
fn selective_imported_func() {
    let dep = "pub fn init() -> int { 0 }";
    let root = "
        import gamekit { init };
        fn use_it() -> int { init() }
    ";
    assert_type_with_modules(root, dep, Type::Int);
}

#[test]
fn wildcard_import_type() {
    let dep = "pub struct GameKit {}";
    let root = "
        import gamekit { * };
        fn use_it(x: GameKit) {}
    ";
    assert_type_with_modules(root, dep, Type::Void);
}

#[test]
fn wildcard_import_value() {
    let dep = "pub fn init() -> int { 0 }";
    let root = "
        import gamekit { * };
        fn use_it() -> int { init() }
    ";
    assert_type_with_modules(root, dep, Type::Int);
}

#[test]
fn module_import_resolves_struct() {
    let dep = "pub struct Point { x: int, y: int }";
    let root = "
        import geom;
        fn use_it(x: geom.Point) {}
    ";
    assert_type_with_named_modules(root, &[("geom", dep)], Type::Void);
}

#[test]
fn qualified_generic_annotation() {
    let dep = "pub struct Wrapper<T> { value: T }";
    let root = "
        import shapes;
        fn use_it(x: shapes.Wrapper<int>) -> int { x.value }
    ";
    assert_type_with_named_modules(root, &[("shapes", dep)], Type::Int);
}

#[test]
fn qualified_struct_literal() {
    let dep = "pub struct Wrapper<T> { value: T }";
    let root = "
        import shapes;
        fn use_it() -> int {
            let w = shapes.Wrapper { value: 42 };
            w.value
        }
    ";
    assert_type_with_named_modules(root, &[("shapes", dep)], Type::Int);
}

#[test]
fn qualified_dataref_literal() {
    let dep = "pub dataref Box<T> { value: T }";
    let root = "
        import boxes;
        fn use_it() -> bool {
            let b = boxes.Box { value: true };
            b.value
        }
    ";
    assert_type_with_named_modules(root, &[("boxes", dep)], Type::Bool);
}

#[test]
fn reexport_generic_fn() {
    let root = "
        import facade { id };
        fn use_it() -> int { id(1) }
    ";
    let modules = [
        ("tools", "pub fn id<T>(x: T) -> T { x }"),
        ("facade", "pub import tools { id };"),
    ];
    assert_type_with_named_modules(root, &modules, Type::Int);
}

#[test]
fn reexport_alias_call_target() {
    let root = "
        import facade { dup };
        fn use_it() -> int { dup(1) }
    ";
    let modules = [
        ("tools", "pub fn id<T>(x: T) -> T { x }"),
        ("facade", "pub import tools { id as dup };"),
    ];
    let result = typecheck_with_named_modules(root, &modules).expect("typecheck failed");
    let target = result.calls().values().next().expect("missing call target");
    assert_eq!(
        target,
        &CallTarget::Callable {
            id: CallableId::function(
                ModuleScope::Named(ModulePath::new(vec!["tools".to_string()]).unwrap()),
                Ident::new("id"),
            ),
            args: GenericArgs {
                type_args: vec![Type::Int],
                const_args: vec![],
            },
        }
    );
}

#[test]
fn reexport_type_alias_origin() {
    let root = "
        import facade { P };
        fn use_it() -> int {
            let p = P { x: 1, y: 2 };
            p.x
        }
    ";
    let modules = [
        ("tools", "pub struct Point { x: int, y: int }"),
        ("facade", "pub import tools { Point as P };"),
    ];
    assert_type_with_named_modules(root, &modules, Type::Int);
}

#[test]
fn qualified_reexport_type_alias() {
    let root = "
        import facade;
        fn use_it(x: facade.P) -> int { x.x }
    ";
    let modules = [
        ("tools", "pub struct Point { x: int }"),
        ("facade", "pub import tools { Point as P };"),
    ];
    assert_type_with_named_modules(root, &modules, Type::Int);
}

#[test]
fn reexport_const_alias_origin() {
    let root = "
        import facade { N };
        fn use_it() -> int { N }
    ";
    let modules = [
        ("tools", "pub const SIZE = 4;"),
        ("facade", "pub import tools { SIZE as N };"),
    ];
    assert_type_with_named_modules(root, &modules, Type::Int);
}

#[test]
fn lexical_value_shadows_imported_value() {
    let root = "
        import tools { id };
        fn use_it() -> int {
            let id = 3;
            id
        }
    ";
    let modules = [("tools", "pub fn id(x: int) -> int { x }")];
    assert_type_with_named_modules(root, &modules, Type::Int);
}

#[test]
fn imported_generic_source_scope() {
    let dep = "
        fn mul2(x: int) -> int { x * 2 }
        pub fn duplicate<T>(x: T) -> T { mul2(x) }
    ";
    let root = "
        import tools { duplicate };
        fn use_it() -> int { duplicate(4) }
    ";
    assert_type_with_named_modules(root, &[("tools", dep)], Type::Int);
}

#[test]
fn module_qualified_imported_generic_call() {
    let dep = "
        fn mul2(x: int) -> int { x * 2 }
        pub fn duplicate<T>(x: T) -> T { mul2(x) }
    ";
    let root = "
        import tools;
        fn use_it() -> int { tools.duplicate(5) }
    ";
    assert_type_with_named_modules(root, &[("tools", dep)], Type::Int);
}

#[test]
fn selective_extend() {
    let dep = "
        pub fn keep_alive() {}
        pub extend<T> T { fn id(self) -> T { self } }
    ";
    let root = "
        import tools { keep_alive };
        fn use_it() -> int { keep_alive(); 1.id() }
    ";
    assert_type_with_named_modules(root, &[("tools", dep)], Type::Int);
}

#[test]
fn extend_requires_import() {
    let dep = "pub extend<T> T { fn id(self) -> T { self } }";
    let root = "fn use_it() -> int { 1.id() }";
    assert!(typecheck_with_named_modules(root, &[("tools", dep)]).is_err());
}

#[test]
fn private_extend_index() {
    let dep = "
        extend bool { fn hidden(self) -> bool { self } }
        pub fn keep_alive() {}
        pub extend<T> T { fn bad(self) -> T { true } }
    ";
    let root = "
        import tools { keep_alive };
        fn use_it() { keep_alive(); 1.bad(); }
    ";
    assert!(typecheck_with_named_modules(root, &[("tools", dep)]).is_err());
}

#[test]
fn reexport_facade_extend() {
    let root = "
        import facade { Box };
        fn use_it() -> int { let b = Box { value: 1 }; b.get() }
    ";
    let modules = [
        ("tools", "pub struct Box<T> { value: T }"),
        (
            "facade",
            "pub import tools { Box }; pub extend<T> Box<T> { fn get(self) -> T { self.value } }",
        ),
    ];
    assert_type_with_named_modules(root, &modules, Type::Int);
}

#[test]
fn const_extend_requires_import() {
    let root = "
        import bufs { FixedBuf };
        fn use_it() -> int { FixedBuf { data: [1, 2] }.len() }
    ";
    assert!(
        typecheck_with_named_modules(root, &[("bufs", FIXED_BUF_MOD), ("exts", LEN_EXT_MOD)])
            .is_err()
    );
}

#[test]
fn selective_const_extend() {
    assert_type_with_named_modules(
        LEN_EXT_ROOT,
        &[("bufs", FIXED_BUF_MOD), ("exts", LEN_EXT_MOD)],
        Type::Int,
    );
}

#[test]
fn private_const_extend() {
    let ext = LEN_EXT_MOD.replace("pub extend", "extend");
    assert!(
        typecheck_with_named_modules(LEN_EXT_ROOT, &[("bufs", FIXED_BUF_MOD), ("exts", &ext)])
            .is_err()
    );
}

#[test]
fn facade_const_extend_origin() {
    let root = "
        import facade { FixedBuf };
        fn use_it() -> int { FixedBuf { data: [1, 2] }.len() }
    ";
    let facade = "
        pub import bufs { FixedBuf };
        const ADD: int = 1;
        pub extend<T, N: int> FixedBuf<T, N> { fn len(self) -> int { N + ADD } }
    ";
    assert_type_with_named_modules(
        root,
        &[("bufs", FIXED_BUF_MOD), ("facade", facade)],
        Type::Int,
    );
}

#[test]
fn qualified_reexport_alias_call_target() {
    let root = "
        import facade;
        fn use_it() -> int { facade.dup(1) }
    ";
    let modules = [
        ("tools", "pub fn id<T>(x: T) -> T { x }"),
        ("facade", "pub import tools { id as dup };"),
    ];
    let result = typecheck_with_named_modules(root, &modules).expect("typecheck failed");
    let target = result.calls().values().next().expect("missing call target");
    assert_eq!(
        target,
        &CallTarget::Callable {
            id: CallableId::function(
                ModuleScope::Named(ModulePath::new(vec!["tools".to_string()]).unwrap()),
                Ident::new("id"),
            ),
            args: GenericArgs {
                type_args: vec![Type::Int],
                const_args: vec![],
            },
        }
    );
}

#[test]
fn always_active_extend_is_visible_without_import() {
    let root = "fn use_it() -> int { 1.plus_one() }";
    let modules = [(
        "core_int",
        "pub extend int { fn plus_one(self) -> int { self + 1 } }",
    )];

    typecheck_with_always_active_modules(root, &modules, &["core_int"]).unwrap();
}

#[test]
fn ordinary_module_extend_is_not_visible_without_import() {
    let root = "fn use_it() -> int { 1.plus_one() }";
    let modules = [(
        "core_int",
        "pub extend int { fn plus_one(self) -> int { self + 1 } }",
    )];

    assert!(typecheck_with_always_active_modules(root, &modules, &[]).is_err());
}

#[test]
fn always_active_module_names_are_not_imported() {
    let root = "fn use_it() -> int { hidden() }";
    let modules = [("helpers", "pub fn hidden() -> int { 1 }")];

    assert!(typecheck_with_always_active_modules(root, &modules, &["helpers"]).is_err());
}

#[test]
fn explicit_import_keeps_extend_visible() {
    let root = "import core_int; fn use_it() -> int { 1.plus_one() }";
    let modules = [(
        "core_int",
        "pub extend int { fn plus_one(self) -> int { self + 1 } }",
    )];

    typecheck_with_always_active_modules(root, &modules, &[]).unwrap();
}
