use super::support::{TypecheckTestResult, check};
use crate::{
    ast::{ArrayLen, Ident, Type},
    typecheck::{CallableParent, NominalPlacement, TypeError, decls::DeclError},
};

fn expect_errors(
    result: Result<TypecheckTestResult, Vec<TypeError>>,
    message: &str,
) -> Vec<TypeError> {
    match result {
        Ok(_) => panic!("{message}"),
        Err(errors) => errors,
    }
}

fn function_param_types<'a>(
    result: &'a TypecheckTestResult,
    name: &str,
) -> impl Iterator<Item = &'a Type> {
    let name = Ident::new(name);
    result
        .function_facts()
        .iter()
        .filter(move |fact| fact.name == name)
        .map(|fact| &fact.params[0].ty)
}

#[test]
fn schema_separates_owner_and_declaration_generics() {
    let result = check(
        "fn outer<Outer>(value: Outer) {
            struct Box<Inner> { owner: Outer, item: Inner }
            fn take(item: Box<int>) {}
            take(Box<int> { owner: value, item: 1 });
        }
        fn main() { outer(1); }",
    )
    .expect("generic local schema should typecheck");
    let (key, schema) = result
        .decls()
        .aggregates()
        .find(|(key, _)| key.name == Ident::new("Box"))
        .expect("missing local schema");
    assert!(matches!(key.placement, NominalPlacement::Lexical));
    assert_eq!(schema.owner_generics.type_params.len(), 1);
    assert_eq!(schema.generics.type_params.len(), 1);
    assert_ne!(
        schema.owner_generics.type_params[0].id,
        schema.generics.type_params[0].id
    );
    assert_eq!(
        schema
            .fields
            .get(Ident::new("owner"))
            .expect("owner field")
            .ty,
        Type::Var(schema.owner_generics.type_params[0].id)
    );
    assert_eq!(
        schema
            .fields
            .get(Ident::new("item"))
            .expect("item field")
            .ty,
        Type::Var(schema.generics.type_params[0].id)
    );
}

#[test]
fn unused_owner_parameter_distinguishes_instances() {
    let result = check(
        "fn outer<T>(value: T) {
            struct Marker { tag: int }
            fn take(item: Marker) {}
            take(Marker { tag: 0 });
        }
        fn main() { outer(1); outer(\"x\"); }",
    )
    .expect("owner specializations should typecheck");
    let mut args = function_param_types(&result, "take")
        .map(|ty| match ty {
            Type::Nominal(nominal) => nominal.type_args.clone(),
            _ => panic!("expected local nominal parameter"),
        })
        .collect::<Vec<_>>();
    args.sort_by_key(|args| format!("{args:?}"));
    args.dedup();
    assert_eq!(args.len(), 2);
    assert!(args.contains(&vec![Type::Int]));
    assert!(args.contains(&vec![Type::String]));
}

#[test]
fn owner_const_remains_canonical_across_specializations() {
    let result = check(
        "fn outer<N: int>() {
            const SIZE = N;
            struct Buffer { values: [int; SIZE] }
            fn take(item: Buffer) {}
            take(Buffer { values: [0; SIZE] });
        }
        fn main() { outer<2>(); outer<3>(); }",
    )
    .expect("owner-dependent local const should remain canonical");
    let (_, schema) = result
        .decls()
        .aggregates()
        .find(|(key, _)| key.name == Ident::new("Buffer"))
        .expect("missing Buffer schema");
    let Type::Array { len, .. } = &schema
        .fields
        .get(Ident::new("values"))
        .expect("missing values field")
        .ty
    else {
        panic!("expected array field");
    };
    assert_eq!(
        *len,
        ArrayLen::Param(schema.owner_generics.const_params[0].id)
    );
}

#[test]
fn methods_use_nominal_callable_identity() {
    let result = check(
        "const GLOBAL = 7;
        fn main() {
            struct Tools {
                value: int = GLOBAL,
                fn make() -> Self { Tools {} }
                fn add(self, amount: int = GLOBAL) -> int { self.value + amount }
                fn to_string(self) -> string { \"tools\" }
            }
            let tool = Tools.make();
            tool.add();
            tool.to_string();
        }",
    )
    .expect("local methods and defaults should use ordinary checking");
    let methods = result
        .function_facts()
        .iter()
        .filter(|fact| matches!(fact.name.as_str(), "make" | "add" | "to_string"))
        .collect::<Vec<_>>();
    assert!(!methods.is_empty());
    assert!(
        methods
            .iter()
            .all(|fact| { matches!(fact.id.parent, Some(CallableParent::Nominal(_))) })
    );
}

#[test]
fn raw_enum_transitive_const_dependency_uses_declaration_environments() {
    let errors = expect_errors(
        check(
            "fn outer<N: int>() {
                const SOURCE = N;
                const CHAIN = SOURCE;
                {
                    let SOURCE = 4;
                    enum State: int { A = CHAIN, B }
                };
            }
            fn main() { outer<2>(); }",
        ),
        "transitive owner-const dependency must not bind to a shadow",
    );
    assert!(errors.iter().any(|error| matches!(
        error,
        TypeError::Decl(DeclError::RawEnumOwnerDependency { .. })
    )));
}

#[test]
fn contract_witness_specializes_generic_owner_local_method() {
    let result = check(
        "contract Scored { fn score(self) -> int; }
        fn make<T>(marker: T) -> dyn Scored {
            struct Local { fn score(self) -> int { 7 } }
            Local {}
        }
        fn main() { make(false).score(); }",
    )
    .expect("contract witness should specialize the local method");

    let fact = result
        .function_facts()
        .iter()
        .find(|fact| {
            fact.name == Ident::new("score")
                && matches!(fact.id.parent, Some(CallableParent::Nominal(_)))
        })
        .expect("missing specialized local method fact");
    assert_eq!(fact.args.type_args, vec![Type::Bool]);
}
