use super::support::{TypecheckTestResult, assert_typecheck_closed, check};
use crate::{
    ast::{Ident, NominalKind, Type},
    typecheck::{
        CallTarget, GenericArgs, MemberPathKind,
        decls::{CallableId, ExtendId, MethodSurface, ModuleScope, NominalKey},
    },
};

fn root_key(kind: NominalKind, name: &str) -> NominalKey {
    NominalKey {
        module: ModuleScope::Root,
        kind,
        name: Ident::new(name),
    }
}

fn nominal(kind: NominalKind, name: &str, type_args: Vec<Type>) -> Type {
    Type::nominal(kind, Ident::new(name), type_args, vec![], None)
}

#[test]
fn promoted_field_records_canonical_path() {
    let result = check(
        "struct Health { hp: int }
        struct Enemy { embed health: Health }
        fn main(enemy: Enemy) { enemy.hp; }",
    )
    .expect("typecheck failed");
    let fact = result
        .member_paths()
        .values()
        .next()
        .expect("missing member path fact");

    assert_eq!(fact.kind, MemberPathKind::Field);
    assert_eq!(fact.path, vec![Ident::new("health"), Ident::new("hp")]);
    assert_eq!(
        fact.origin_owner,
        nominal(NominalKind::Struct, "Health", vec![])
    );
    assert_eq!(fact.origin_member, Ident::new("hp"));
}

#[test]
fn promoted_method_records_origin_and_receiver_path() {
    let result = check(
        "struct Health { fn damage(self, amount: int) {} }
        struct Enemy { embed health: Health }
        fn main(enemy: Enemy) { enemy.damage(1); }",
    )
    .expect("typecheck failed");

    assert_method_target(&result, "Health", "damage", MethodSurface::Instance, vec![]);
    let fact = result
        .member_paths()
        .values()
        .next()
        .expect("missing member path fact");
    assert_eq!(fact.kind, MemberPathKind::MethodReceiver);
    assert_eq!(fact.path, vec![Ident::new("health")]);
    assert_eq!(
        fact.origin_owner,
        nominal(NominalKind::Struct, "Health", vec![])
    );
    assert_eq!(fact.origin_member, Ident::new("damage"));
}

#[test]
fn native_method_call_target() {
    let result = check(
        "struct Point { x: int, fn len(self) -> int { 0 } }
        fn main() { let p = Point { x: 1 }; p.len(); }",
    )
    .expect("typecheck failed");
    assert_method_target(&result, "Point", "len", MethodSurface::Instance, vec![]);
}

#[test]
fn static_nil_inference_does_not_leak() {
    let checked = check(
        "struct Foo { fn id<T>(x: T) -> T { x } }
        fn main() { let x: int? = Foo.id(nil); x; }",
    )
    .expect("typecheck failed");
    assert_typecheck_closed(&checked);
}

#[test]
fn extension_call_target_records_specialization() {
    let result = check("extend<T> T { fn id(self) -> T { self } } fn main() { 1.id(); }")
        .expect("typecheck failed");
    assert_extend_target(&result, 0, "id", MethodSurface::Instance, vec![Type::Int]);
}

#[test]
fn enum_tuple_nil_inference_does_not_leak() {
    let checked = check(
        "enum Option<T> { Some(T), None }
        fn main() { let x: Option<int?> = Option.Some(nil); x; }",
    )
    .expect("typecheck failed");
    assert_typecheck_closed(&checked);
}

fn assert_method_target(
    result: &TypecheckTestResult,
    owner: &str,
    name: &str,
    surface: MethodSurface,
    type_args: Vec<Type>,
) {
    let target = result.calls().values().next().expect("missing call target");
    assert_eq!(
        target,
        &CallTarget::new(
            CallableId::aggregate_method(
                root_key(NominalKind::Struct, owner),
                Ident::new(name),
                surface
            ),
            GenericArgs {
                type_args,
                const_args: vec![],
            },
        ),
    );
}

fn assert_extend_target(
    result: &TypecheckTestResult,
    index: usize,
    name: &str,
    surface: MethodSurface,
    type_args: Vec<Type>,
) {
    let target = result.calls().values().next().expect("missing call target");
    assert_eq!(
        target,
        &CallTarget::new(
            CallableId::extend_method(
                ExtendId {
                    module: ModuleScope::Root,
                    index,
                },
                Ident::new(name),
                surface,
            ),
            GenericArgs {
                type_args,
                const_args: vec![],
            },
        ),
    );
}
