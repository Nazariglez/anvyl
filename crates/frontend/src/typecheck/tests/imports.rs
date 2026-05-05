use super::support::{assert_ty_mods, assert_ty_named, check, check_named};
use crate::{
    ast::{Ident, NominalKind, Type},
    resolve::ModulePath,
    typecheck::{
        CallTarget, GenericArgs, TypeError, TypecheckResult,
        decls::{BindingNamespace, BindingOrigin, CallableId, DeclError, ExtendId, ModuleScope},
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

fn enum_ty(module: &str, name: &str) -> Type {
    Type::nominal(
        NominalKind::Enum,
        Ident::new(name),
        vec![],
        vec![],
        Some(vec![module.to_string()].into()),
    )
}

fn expect_errors(result: Result<TypecheckResult, Vec<TypeError>>, message: &str) -> Vec<TypeError> {
    let Err(errors) = result else {
        panic!("{message}");
    };
    errors
}

fn expect_single_error(
    result: Result<TypecheckResult, Vec<TypeError>>,
    matches: impl FnOnce(&TypeError) -> bool,
) {
    let errors = expect_errors(result, "expected one typecheck error");
    assert_eq!(errors.len(), 1, "unexpected errors: {errors:?}");
    assert!(matches(&errors[0]), "unexpected error: {:?}", errors[0]);
}

fn expect_decl_error_count(
    result: Result<TypecheckResult, Vec<TypeError>>,
    expected: usize,
    matches: impl Fn(&DeclError) -> bool,
) {
    let errors = expect_errors(result, "expected typecheck errors");
    let count = errors
        .iter()
        .filter(|err| matches!(err, TypeError::Decl(decl) if matches(decl)))
        .count();
    assert_eq!(count, expected, "unexpected errors: {errors:?}");
}

#[derive(Clone, Copy)]
enum ExpectedDecl {
    DuplicateType(&'static str),
    DuplicateModule(&'static str),
    MissingMember(&'static str),
    PrivateMember(&'static str),
    ImportConflict(BindingNamespace, &'static str),
    ReexportConflict(BindingNamespace, &'static str),
}

fn assert_decl_error(result: Result<TypecheckResult, Vec<TypeError>>, expected: ExpectedDecl) {
    let errors = expect_errors(result, "expected declaration error");
    assert!(
        errors.iter().any(|error| matches_decl(error, expected)),
        "unexpected errors: {errors:?}"
    );
}

fn matches_decl(error: &TypeError, expected: ExpectedDecl) -> bool {
    let TypeError::Decl(error) = error else {
        return false;
    };
    match (error, expected) {
        (DeclError::DuplicateType { name, .. }, ExpectedDecl::DuplicateType(expected))
        | (
            DeclError::DuplicateModuleBinding { name, .. },
            ExpectedDecl::DuplicateModule(expected),
        )
        | (DeclError::MissingImportMember { name, .. }, ExpectedDecl::MissingMember(expected))
        | (DeclError::PrivateImportMember { name, .. }, ExpectedDecl::PrivateMember(expected)) => {
            *name == Ident::new(expected)
        }
        (
            DeclError::ImportConflict {
                name, namespace, ..
            },
            ExpectedDecl::ImportConflict(expected_namespace, expected),
        )
        | (
            DeclError::ReexportConflict {
                name, namespace, ..
            },
            ExpectedDecl::ReexportConflict(expected_namespace, expected),
        ) => *namespace == expected_namespace && *name == Ident::new(expected),
        _ => false,
    }
}

#[test]
fn qualified_type() {
    let dep = "pub struct GameKit {}";
    let root = "
        import gamekit as gk;
        fn use_it(x: gk.GameKit) {}
    ";
    assert_ty_mods(root, dep, Type::Void);
}

#[test]
fn selective_self_type() {
    let dep = "pub struct GameKit {}";
    let root = "
        import gamekit { self };
        fn use_it(x: gamekit.GameKit) {}
    ";
    assert_ty_mods(root, dep, Type::Void);
}

#[test]
fn selective_self_alias_type() {
    let dep = "pub struct GameKit {}";
    let root = "
        import gamekit { self as gk };
        fn use_it(x: gk.GameKit) {}
    ";
    assert_ty_mods(root, dep, Type::Void);
}

#[test]
fn selective_imported_type() {
    let dep = "pub struct GameKit {}";
    let root = "
        import gamekit { GameKit };
        fn use_it(x: GameKit) {}
    ";
    assert_ty_mods(root, dep, Type::Void);
}

#[test]
fn selective_imported_enum() {
    let dep = "pub enum Color { Red, Green, Blue }";
    let root = "
        import gamekit { Color };
        fn use_it(x: Color) {}
    ";
    assert_ty_mods(root, dep, Type::Void);
}

#[test]
fn enum_variant_origin() {
    let root = "
        import colors { Color };
        fn use_it() -> Color { Color.Red }
    ";
    let modules = [("colors", "pub enum Color { Red, Green, Blue }")];

    assert_ty_named(root, &modules, enum_ty("colors", "Color"));
}

#[test]
fn reexported_enum_variant_origin() {
    let root = "
        import facade { Color };
        fn use_it() -> Color { Color.Red }
    ";
    let modules = [
        ("colors", "pub enum Color { Red, Green, Blue }"),
        ("facade", "pub import colors { Color };"),
    ];

    assert_ty_named(root, &modules, enum_ty("colors", "Color"));
}

#[test]
fn function_enum_return_origin() {
    let root = "
        import lib;
        import colors;
        fn use_it() -> colors.Color { lib.red() }
    ";
    let modules = [
        ("colors", "pub enum Color { Red, Green, Blue }"),
        (
            "lib",
            "import colors { Color }; pub fn red() -> Color { Color.Red }",
        ),
    ];

    assert_ty_named(root, &modules, enum_ty("colors", "Color"));
}

#[test]
fn imported_enum_pattern_origin() {
    let root = "
        import colors { Color };
        fn use_it(c: Color) -> int { match c { Color.Red => 1, Color.Green => 2 } }
    ";
    let modules = [("colors", "pub enum Color { Red, Green }")];

    assert_ty_named(root, &modules, Type::Int);
}

#[test]
fn reexported_enum_pattern_origin() {
    let root = "
        import facade { Color };
        fn use_it(c: Color) -> int { match c { Color.Red => 1, Color.Green => 2 } }
    ";
    let modules = [
        ("colors", "pub enum Color { Red, Green }"),
        ("facade", "pub import colors { Color };"),
    ];

    assert_ty_named(root, &modules, Type::Int);
}

#[test]
fn transitive_return_enum_pattern_origin() {
    let root = "
        import lib { color };
        fn use_it() -> int { match color() { Color.Red => 1, Color.Green => 2 } }
    ";
    let modules = [
        ("colors", "pub enum Color { Red, Green }"),
        (
            "lib",
            "import colors { Color }; pub fn color() -> Color { Color.Red }",
        ),
    ];

    assert_ty_named(root, &modules, Type::Int);
}

#[test]
fn imported_enum_payload_pattern_origin() {
    let root = "
        import messages { Message };
        fn use_it(m: Message) -> int { match m { Message.Data(x) => x, Message.Empty => 0 } }
    ";
    let modules = [("messages", "pub enum Message { Data(int), Empty }")];

    assert_ty_named(root, &modules, Type::Int);
}

#[test]
fn selective_imported_dataref() {
    let dep = "pub dataref Vec2 { x: float, y: float }";
    let root = "
        import gamekit { Vec2 };
        fn use_it(x: Vec2) {}
    ";
    assert_ty_mods(root, dep, Type::Void);
}

#[test]
fn selective_imported_func() {
    let dep = "pub fn init() -> int { 0 }";
    let root = "
        import gamekit { init };
        fn use_it() -> int { init() }
    ";
    assert_ty_mods(root, dep, Type::Int);
}

#[test]
fn annotation_alias_key() {
    let root = "
        import alpha { Item as A };
        fn use_it(x: A) -> int { x.value }
    ";
    let modules = [
        ("alpha", "pub struct Item { value: int }"),
        ("beta", "pub struct Item { label: string }"),
    ];

    assert_ty_named(root, &modules, Type::Int);
}

#[test]
fn annotation_no_export_fallback() {
    let root = "fn use_it(x: Item) {}";
    let modules = [("alpha", "pub struct Item { value: int }")];

    expect_single_error(
        check_named(root, &modules),
        |err| matches!(err, TypeError::Decl(DeclError::UnknownType { qualifier: None, name, .. }) if *name == Ident::new("Item")),
    );
}

#[test]
fn unknown_module_type_member() {
    let root = "import shapes; fn use_it(x: shapes.Missing) {}";
    let modules = [("shapes", "pub struct Point { x: int }")];

    expect_single_error(
        check_named(root, &modules),
        |err| matches!(err, TypeError::Decl(DeclError::UnknownType { qualifier: Some(module), name, .. }) if *module == Ident::new("shapes") && *name == Ident::new("Missing")),
    );
}

#[test]
fn qualified_annotation_binding() {
    let root = "
        import shapes;
        fn use_it(x: shapes.Point) -> int { x.x }
    ";
    let modules = [("shapes", "pub struct Point { x: int }")];

    assert_ty_named(root, &modules, Type::Int);
}

#[test]
fn qualified_annotation_unbound() {
    let root = "fn use_it(x: shapes.Point) {}";
    let modules = [("shapes", "pub struct Point { x: int }")];

    expect_single_error(
        check_named(root, &modules),
        |err| matches!(err, TypeError::Decl(DeclError::UnknownType { qualifier: Some(module), name, .. }) if *module == Ident::new("shapes") && *name == Ident::new("Point")),
    );
}

#[test]
fn nested_arg_visible_namespace() {
    let root = "
        import boxes { Box };
        import alpha { Item };
        fn use_it(x: Box<Item>) -> int { x.value.value }
    ";
    let modules = [
        ("boxes", "pub struct Box<T> { value: T }"),
        ("alpha", "pub struct Item { value: int }"),
    ];

    assert_ty_named(root, &modules, Type::Int);
}

#[test]
fn literal_alias_key() {
    let root = "
        import alpha { Item as A };
        fn use_it() -> int { A { value: 1 }.value }
    ";
    let modules = [
        ("alpha", "pub struct Item { value: int }"),
        ("beta", "pub struct Item { label: string }"),
    ];

    assert_ty_named(root, &modules, Type::Int);
}

#[test]
fn literal_not_bare_from_module() {
    let root = "
        import shapes;
        fn use_it() { Point { x: 1 }; }
    ";
    let modules = [("shapes", "pub struct Point { x: int }")];

    expect_single_error(check_named(root, &modules), |err| {
        matches!(
            err,
            TypeError::UnknownStructLiteral {
                qualifier: None,
                name,
                ..
            } if *name == Ident::new("Point")
        )
    });
}

#[test]
fn literal_not_bare_from_signature() {
    let root = "
        import lib;
        fn use_it() { Point { x: 1 }; }
    ";
    let modules = [
        (
            "lib",
            "import shapes { Point }; pub fn origin() -> Point { Point { x: 0 } }",
        ),
        ("shapes", "pub struct Point { x: int }"),
    ];

    expect_single_error(check_named(root, &modules), |err| {
        matches!(
            err,
            TypeError::UnknownStructLiteral {
                qualifier: None,
                name,
                ..
            } if *name == Ident::new("Point")
        )
    });
}

#[test]
fn literal_requires_module_binding() {
    let root = "fn use_it() { shapes.Point { x: 1 }; }";
    let modules = [("shapes", "pub struct Point { x: int }")];

    expect_single_error(check_named(root, &modules), |err| {
        matches!(
            err,
            TypeError::UnknownStructLiteral {
                qualifier: Some(module),
                name,
                ..
            } if *module == Ident::new("shapes") && *name == Ident::new("Point")
        )
    });
}

#[test]
fn literal_requires_exported_type() {
    let root = "
        import shapes;
        fn use_it() { shapes.Point { x: 1 }; }
    ";
    let modules = [("shapes", "struct Point { x: int }")];

    expect_single_error(check_named(root, &modules), |err| {
        matches!(
            err,
            TypeError::UnknownStructLiteral {
                qualifier: Some(module),
                name,
                ..
            } if *module == Ident::new("shapes") && *name == Ident::new("Point")
        )
    });
}

#[test]
fn wildcard_import_type() {
    let dep = "pub struct GameKit {}";
    let root = "
        import gamekit { * };
        fn use_it(x: GameKit) {}
    ";
    assert_ty_mods(root, dep, Type::Void);
}

#[test]
fn wildcard_import_value() {
    let dep = "pub fn init() -> int { 0 }";
    let root = "
        import gamekit { * };
        fn use_it() -> int { init() }
    ";
    assert_ty_mods(root, dep, Type::Int);
}

#[test]
fn module_import_resolves_struct() {
    let dep = "pub struct Point { x: int, y: int }";
    let root = "
        import geom;
        fn use_it(x: geom.Point) {}
    ";
    assert_ty_named(root, &[("geom", dep)], Type::Void);
}

#[test]
fn missing_qualified_member() {
    let root = "
        import helpers;
        fn use_it() { helpers.missing(); }
    ";
    let modules = [("helpers", "pub fn present() -> int { 1 }")];
    let helpers = ModuleScope::Named(ModulePath::new(vec!["helpers".to_string()]).unwrap());

    expect_single_error(check_named(root, &modules), |err| {
        matches!(
            err,
            TypeError::UndefinedModuleMember { module, name, .. }
                if *module == helpers && *name == Ident::new("missing")
        )
    });
}

#[test]
fn private_qualified_member() {
    let root = "
        import helpers;
        fn use_it() { helpers.secret(1); }
    ";
    let modules = [("helpers", "fn secret(x: int) -> int { x }")];
    let helpers = ModuleScope::Named(ModulePath::new(vec!["helpers".to_string()]).unwrap());

    expect_single_error(check_named(root, &modules), |err| {
        matches!(
            err,
            TypeError::PrivateModuleMember { module, name, .. }
                if *module == helpers && *name == Ident::new("secret")
        )
    });
}

#[test]
fn qualified_generic_annotation() {
    let dep = "pub struct Wrapper<T> { value: T }";
    let root = "
        import shapes;
        fn use_it(x: shapes.Wrapper<int>) -> int { x.value }
    ";
    assert_ty_named(root, &[("shapes", dep)], Type::Int);
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
    assert_ty_named(root, &[("shapes", dep)], Type::Int);
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
    assert_ty_named(root, &[("boxes", dep)], Type::Bool);
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
    assert_ty_named(root, &modules, Type::Int);
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
    let result = check_named(root, &modules).expect("typecheck failed");
    let target = result.calls().values().next().expect("missing call target");
    assert_eq!(
        target,
        &CallTarget::new(
            CallableId::function(
                ModuleScope::Named(ModulePath::new(vec!["tools".to_string()]).unwrap()),
                Ident::new("id"),
            ),
            GenericArgs {
                type_args: vec![Type::Int],
                const_args: vec![],
            }
        )
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
    assert_ty_named(root, &modules, Type::Int);
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
    assert_ty_named(root, &modules, Type::Int);
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
    assert_ty_named(root, &modules, Type::Int);
}

#[test]
fn value_shadows_import() {
    let root = "
        import tools { id };
        fn use_it() -> int {
            let id = 3;
            id
        }
    ";
    let modules = [("tools", "pub fn id(x: int) -> int { x }")];
    assert_ty_named(root, &modules, Type::Int);
}

#[test]
fn duplicate_nominals() {
    for (source, name) in [
        ("struct Point { x: int } struct Point { y: int }", "Point"),
        ("enum Color { Red } enum Color { Blue }", "Color"),
        (
            "dataref Vec2 { x: float } dataref Vec2 { y: float }",
            "Vec2",
        ),
        ("struct Entity { id: int } enum Entity { Player }", "Entity"),
    ] {
        assert_decl_error(check(source), ExpectedDecl::DuplicateType(name));
    }
}

#[test]
fn selective_member_errors() {
    for (root, dep, expected) in [
        (
            "import tools { missing };",
            "pub fn visible() -> int { 1 }",
            ExpectedDecl::MissingMember("missing"),
        ),
        (
            "import tools { hidden };",
            "fn hidden() -> int { 1 }",
            ExpectedDecl::PrivateMember("hidden"),
        ),
        (
            "import tools { Hidden };",
            "struct Hidden { value: int }",
            ExpectedDecl::PrivateMember("Hidden"),
        ),
    ] {
        assert_decl_error(check_named(root, &[("tools", dep)]), expected);
    }
}

#[test]
fn public_missing_reports_once() {
    let root = "pub import tools { missing };";
    let modules = [("tools", "pub fn visible() -> int { 1 }")];

    expect_decl_error_count(check_named(root, &modules), 1, |error| {
        matches!(
            error,
            DeclError::MissingImportMember { name, .. } if *name == Ident::new("missing")
        )
    });
}

#[test]
fn public_private_reports_once() {
    let root = "pub import tools { hidden };";
    let modules = [("tools", "fn hidden() -> int { 1 }")];

    expect_decl_error_count(check_named(root, &modules), 1, |error| {
        matches!(
            error,
            DeclError::PrivateImportMember { name, .. } if *name == Ident::new("hidden")
        )
    });
}

#[test]
fn duplicate_exported_values() {
    let root = "import tools { dup }; fn main() { dup(); }";
    let modules = [(
        "tools",
        "pub fn dup() -> int { 1 } pub fn dup() -> int { 2 }",
    )];

    expect_errors(
        check_named(root, &modules),
        "expected duplicate exported value error",
    );
}

#[test]
fn duplicate_exported_const_fn() {
    let root = "import tools { dup }; fn main() { dup; }";
    let modules = [("tools", "pub fn dup() -> int { 1 } pub const dup = 2;")];

    expect_errors(
        check_named(root, &modules),
        "expected duplicate exported value error",
    );
}

#[test]
fn module_binding_collisions() {
    let cases = [
        (
            "import tools; import tools;",
            vec![("tools", "pub fn visible() -> int { 1 }")],
            "tools",
        ),
        (
            "import alpha as tools; import beta as tools;",
            vec![
                ("alpha", "pub fn a() -> int { 1 }"),
                ("beta", "pub fn b() -> int { 2 }"),
            ],
            "tools",
        ),
        (
            "import foo.utils; import bar.utils;",
            vec![
                ("foo.utils", "pub fn a() -> int { 1 }"),
                ("bar.utils", "pub fn b() -> int { 2 }"),
            ],
            "utils",
        ),
    ];

    for (root, modules, name) in cases {
        assert_decl_error(
            check_named(root, &modules),
            ExpectedDecl::DuplicateModule(name),
        );
    }
}

#[test]
fn import_conflicts() {
    let cases = [
        (
            "struct Point { x: int } import tools { Point };",
            vec![("tools", "pub struct Point { y: int }")],
            ExpectedDecl::ImportConflict(BindingNamespace::Type, "Point"),
        ),
        (
            "struct Local { x: int } import tools { Point as Local };",
            vec![("tools", "pub struct Point { y: int }")],
            ExpectedDecl::ImportConflict(BindingNamespace::Type, "Local"),
        ),
        (
            "struct Point { x: int } import tools { * };",
            vec![("tools", "pub struct Point { y: int }")],
            ExpectedDecl::ImportConflict(BindingNamespace::Type, "Point"),
        ),
        (
            "import alpha { Point }; import beta { Point };",
            vec![
                ("alpha", "pub struct Point { x: int }"),
                ("beta", "pub struct Point { x: float }"),
            ],
            ExpectedDecl::ImportConflict(BindingNamespace::Type, "Point"),
        ),
        (
            "import alpha { * }; import beta { * };",
            vec![
                ("alpha", "pub fn dup() -> int { 1 }"),
                ("beta", "pub fn dup() -> int { 2 }"),
            ],
            ExpectedDecl::ImportConflict(BindingNamespace::Value, "dup"),
        ),
        (
            "import alpha { dup }; import beta { * };",
            vec![
                ("alpha", "pub fn dup() -> int { 1 }"),
                ("beta", "pub fn dup() -> int { 2 }"),
            ],
            ExpectedDecl::ImportConflict(BindingNamespace::Value, "dup"),
        ),
        (
            "import facade { dup };",
            vec![
                ("alpha", "pub fn dup() -> int { 1 }"),
                ("beta", "pub fn dup() -> int { 2 }"),
                ("facade", "pub import alpha { * }; pub import beta { * };"),
            ],
            ExpectedDecl::ReexportConflict(BindingNamespace::Value, "dup"),
        ),
    ];

    for (root, modules, expected) in cases {
        assert_decl_error(check_named(root, &modules), expected);
    }
}

#[test]
fn reexport_private_type_collision() {
    let root = "import facade;";
    let modules = [
        ("tools", "pub struct Point { x: int }"),
        (
            "facade",
            "struct Point { y: int } pub import tools { Point };",
        ),
    ];
    let errors = expect_errors(check_named(root, &modules), "expected import conflict");

    assert!(
        errors.iter().any(|error| matches!(
            error,
            TypeError::Decl(DeclError::ImportConflict {
                module: ModuleScope::Named(path),
                name,
                namespace: BindingNamespace::Type,
                first: BindingOrigin::Local,
                ..
            }) if path == &ModulePath::new(vec!["facade".to_string()]).unwrap()
                && *name == Ident::new("Point")
        )),
        "unexpected errors: {errors:?}"
    );
}

#[test]
fn private_then_public_type_collision() {
    let root = "import alpha { Point }; pub import beta { Point };";
    let modules = [
        ("alpha", "pub struct Point { x: int }"),
        ("beta", "pub struct Point { y: int }"),
    ];
    let errors = expect_errors(check_named(root, &modules), "expected import conflict");

    assert!(
        errors.iter().any(|error| matches!(
            error,
            TypeError::Decl(DeclError::ImportConflict {
                name,
                namespace: BindingNamespace::Type,
                ..
            }) if *name == Ident::new("Point")
        )),
        "unexpected errors: {errors:?}"
    );
}

#[test]
fn public_then_private_type_collision() {
    let root = "pub import beta { Point }; import alpha { Point };";
    let modules = [
        ("alpha", "pub struct Point { x: int }"),
        ("beta", "pub struct Point { y: int }"),
    ];
    let errors = expect_errors(check_named(root, &modules), "expected import conflict");

    assert!(
        errors.iter().any(|error| matches!(
            error,
            TypeError::Decl(DeclError::ImportConflict {
                name,
                namespace: BindingNamespace::Type,
                ..
            }) if *name == Ident::new("Point")
        )),
        "unexpected errors: {errors:?}"
    );
}

#[test]
fn rejects_duplicate_extern_type_name() {
    let errors = expect_errors(
        check("extern type Handle; extern type Handle;"),
        "expected duplicate type error",
    );

    assert!(
        errors.iter().any(|error| matches!(
            error,
            TypeError::Decl(DeclError::DuplicateType { name, .. }) if *name == Ident::new("Handle")
        )),
        "unexpected errors: {errors:?}"
    );
}

#[test]
fn struct_then_extern_type() {
    let errors = expect_errors(
        check("struct Handle { id: int } extern type Handle;"),
        "expected duplicate type error",
    );

    assert!(
        errors.iter().any(|error| matches!(
            error,
            TypeError::Decl(DeclError::DuplicateType { name, .. }) if *name == Ident::new("Handle")
        )),
        "unexpected errors: {errors:?}"
    );
}

#[test]
fn extern_then_struct_type() {
    let errors = expect_errors(
        check("extern type Handle; struct Handle { id: int }"),
        "expected duplicate type error",
    );

    assert!(
        errors.iter().any(|error| matches!(
            error,
            TypeError::Decl(DeclError::DuplicateType { name, .. }) if *name == Ident::new("Handle")
        )),
        "unexpected errors: {errors:?}"
    );
}

#[test]
fn rejects_enum_then_extern_type_name() {
    let errors = expect_errors(
        check("enum Handle { One } extern type Handle;"),
        "expected duplicate type error",
    );

    assert!(
        errors.iter().any(|error| matches!(
            error,
            TypeError::Decl(DeclError::DuplicateType { name, .. }) if *name == Ident::new("Handle")
        )),
        "unexpected errors: {errors:?}"
    );
}

#[test]
fn nested_imported_extend_visible() {
    let root = "import core.int_ext; fn use_it() -> int { 1.plus_one() }";
    let modules = [(
        "core.int_ext",
        "pub extend int { fn plus_one(self) -> int { self + 1 } }",
    )];

    check_named(root, &modules).unwrap();
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
    assert_ty_named(root, &[("tools", dep)], Type::Int);
}

#[test]
fn qualified_imported_generic_call() {
    let dep = "
        fn mul2(x: int) -> int { x * 2 }
        pub fn duplicate<T>(x: T) -> T { mul2(x) }
    ";
    let root = "
        import tools;
        fn use_it() -> int { tools.duplicate(5) }
    ";
    assert_ty_named(root, &[("tools", dep)], Type::Int);
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
    assert_ty_named(root, &[("tools", dep)], Type::Int);
}

#[test]
fn extend_requires_import() {
    let dep = "pub extend<T> T { fn id(self) -> T { self } }";
    let root = "fn use_it() -> int { 1.id() }";
    assert!(check_named(root, &[("tools", dep)]).is_err());
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
    assert!(check_named(root, &[("tools", dep)]).is_err());
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
    assert_ty_named(root, &modules, Type::Int);
}

#[test]
fn const_extend_requires_import() {
    let root = "
        import bufs { FixedBuf };
        fn use_it() -> int { FixedBuf { data: [1, 2] }.len() }
    ";
    assert!(check_named(root, &[("bufs", FIXED_BUF_MOD), ("exts", LEN_EXT_MOD)]).is_err());
}

#[test]
fn selective_const_extend() {
    assert_ty_named(
        LEN_EXT_ROOT,
        &[("bufs", FIXED_BUF_MOD), ("exts", LEN_EXT_MOD)],
        Type::Int,
    );
}

#[test]
fn private_const_extend() {
    let ext = LEN_EXT_MOD.replace("pub extend", "extend");
    assert!(check_named(LEN_EXT_ROOT, &[("bufs", FIXED_BUF_MOD), ("exts", &ext)]).is_err());
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
    assert_ty_named(
        root,
        &[("bufs", FIXED_BUF_MOD), ("facade", facade)],
        Type::Int,
    );
}

#[test]
fn qualified_reexport_call_target() {
    let root = "
        import facade;
        fn use_it() -> int { facade.dup(1) }
    ";
    let modules = [
        ("tools", "pub fn id<T>(x: T) -> T { x }"),
        ("facade", "pub import tools { id as dup };"),
    ];
    let result = check_named(root, &modules).expect("typecheck failed");
    let target = result.calls().values().next().expect("missing call target");
    assert_eq!(
        target,
        &CallTarget::new(
            CallableId::function(
                ModuleScope::Named(ModulePath::new(vec!["tools".to_string()]).unwrap()),
                Ident::new("id"),
            ),
            GenericArgs {
                type_args: vec![Type::Int],
                const_args: vec![],
            }
        )
    );
}

#[test]
fn imported_extend_visible() {
    let root = "import core_int; fn use_it() -> int { 1.plus_one() }";
    let modules = [(
        "core_int",
        "pub extend int { fn plus_one(self) -> int { self + 1 } }",
    )];

    check_named(root, &modules).unwrap();
}

#[test]
fn ordinary_extend_not_visible() {
    let root = "fn use_it() -> int { 1.plus_one() }";
    let modules = [(
        "core_int",
        "pub extend int { fn plus_one(self) -> int { self + 1 } }",
    )];

    assert!(check_named(root, &modules).is_err());
}

#[test]
fn import_keeps_extend_visible() {
    let root = "import core_int; fn use_it() -> int { 1.plus_one() }";
    let modules = [(
        "core_int",
        "pub extend int { fn plus_one(self) -> int { self + 1 } }",
    )];

    check_named(root, &modules).unwrap();
}

#[test]
fn reexport_module_forwards_extend() {
    let root = "
        import facade;
        fn use_it() -> int { 1.plus_one() }
    ";
    let modules = [
        ("facade", "pub import ints;"),
        (
            "ints",
            "pub extend int { fn plus_one(self) -> int { self + 1 } }",
        ),
    ];

    assert_ty_named(root, &modules, Type::Int);
}

#[test]
fn reexport_selective_forwards_extend() {
    let root = "
        import facade { keep };
        fn use_it() -> int { keep(); 1.plus_one() }
    ";
    let modules = [
        ("facade", "pub import ints { keep };"),
        (
            "ints",
            "pub fn keep() {} pub extend int { fn plus_one(self) -> int { self + 1 } }",
        ),
    ];

    assert_ty_named(root, &modules, Type::Int);
}

#[test]
fn reexport_wildcard_forwards_extension_only_module() {
    let root = "
        import facade;
        fn use_it() -> int { 1.plus_one() }
    ";
    let modules = [
        ("facade", "pub import ints { * };"),
        (
            "ints",
            "pub extend int { fn plus_one(self) -> int { self + 1 } }",
        ),
    ];

    assert_ty_named(root, &modules, Type::Int);
}

#[test]
fn private_import_does_not_forward_extend() {
    let root = "
        import facade;
        fn use_it() -> int { 1.plus_one() }
    ";
    let modules = [
        ("facade", "import ints;"),
        (
            "ints",
            "pub extend int { fn plus_one(self) -> int { self + 1 } }",
        ),
    ];

    assert!(check_named(root, &modules).is_err());
}

#[test]
fn transitive_reexport_forwards_extend() {
    let root = "
        import api;
        fn use_it() -> int { 1.plus_one() }
    ";
    let modules = [
        ("api", "pub import prelude;"),
        ("prelude", "pub import ints;"),
        (
            "ints",
            "pub extend int { fn plus_one(self) -> int { self + 1 } }",
        ),
    ];

    assert_ty_named(root, &modules, Type::Int);
}

#[test]
fn public_import_cycle_closes_extension_surface() {
    let root = "
        import a;
        fn use_it() -> int { 1.from_b() }
    ";
    let modules = [
        ("a", "pub import b;"),
        (
            "b",
            "pub import a; pub extend int { fn from_b(self) -> int { self } }",
        ),
    ];

    assert_ty_named(root, &modules, Type::Int);
}

#[test]
fn qualified_extend_call_disambiguates_reexported_modules() {
    let root = "
        import facade;
        fn use_it() -> int { facade.a.pick(1) + facade.b.pick(1) }
    ";
    let modules = [
        ("facade", "pub import a; pub import b;"),
        ("a", "pub extend int { fn pick(self) -> int { 1 } }"),
        ("b", "pub extend int { fn pick(self) -> int { 2 } }"),
    ];

    assert_ty_named(root, &modules, Type::Int);
}

#[test]
fn qualified_extend_call_uses_alias() {
    let root = "
        import facade;
        fn use_it() -> int { facade.left.pick(1) }
    ";
    let modules = [
        ("facade", "pub import a as left;"),
        ("a", "pub extend int { fn pick(self) -> int { 1 } }"),
    ];

    assert_ty_named(root, &modules, Type::Int);
}

#[test]
fn hidden_provider_conflict_stays_ambiguous() {
    let root = "
        import facade;
        fn use_it() -> int { 1.pick() }
    ";
    let modules = [
        ("facade", "pub import a { * }; pub import b { * };"),
        ("a", "pub extend int { fn pick(self) -> int { 1 } }"),
        ("b", "pub extend int { fn pick(self) -> int { 2 } }"),
    ];

    expect_single_error(
        check_named(root, &modules),
        |error| matches!(error, TypeError::AmbiguousExtendMethod { name, .. } if *name == Ident::new("pick")),
    );
}

#[test]
fn qualified_extend_call_target_preserves_provider() {
    let root = "
        import facade;
        fn use_it() -> int { facade.a.pick(1) }
    ";
    let modules = [
        ("facade", "pub import a;"),
        ("a", "pub extend int { fn pick(self) -> int { 1 } }"),
    ];

    let result = check_named(root, &modules).expect("typecheck failed");
    let target = result.calls().values().next().expect("missing call target");
    assert_qualified_pick_target(target, "a");
}

#[test]
fn barrel_qualified_extend_call_target_preserves_provider() {
    let root = "
        import facade;
        fn use_it() -> int { facade.pick(1) }
    ";
    let modules = [
        ("facade", "pub import a { * };"),
        ("a", "pub extend int { fn pick(self) -> int { 1 } }"),
    ];

    let result = check_named(root, &modules).expect("typecheck failed");
    let target = result.calls().values().next().expect("missing call target");
    assert_qualified_pick_target(target, "a");
}

#[test]
fn reexported_extend_ambiguity_is_call_site_error() {
    let root = "
        import facade;
        fn use_it() -> int { 1.pick() }
    ";
    let modules = [
        ("facade", "pub import a; pub import b;"),
        ("a", "pub extend int { fn pick(self) -> int { 1 } }"),
        ("b", "pub extend int { fn pick(self) -> int { 2 } }"),
    ];

    expect_single_error(
        check_named(root, &modules),
        |error| matches!(error, TypeError::AmbiguousExtendMethod { name, .. } if *name == Ident::new("pick")),
    );
}

fn assert_qualified_pick_target(target: &CallTarget, module: &str) {
    assert_eq!(target.id, pick_id(module));
    assert_eq!(target.args, GenericArgs::default());
    assert!(matches!(
        target.form,
        crate::typecheck::CallForm::QualifiedExtend { .. }
    ));
}

fn pick_id(module: &str) -> CallableId {
    CallableId::extend_method(
        ExtendId {
            module: ModuleScope::Named(ModulePath::new(vec![module.to_string()]).unwrap()),
            index: 0,
        },
        Ident::new("pick"),
    )
}
