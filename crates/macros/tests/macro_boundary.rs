use anvyx_runtime::function;

mod r#type {
    use super::function;

    #[function]
    pub fn ping() -> i64 {
        1
    }

    anvyx_runtime::builtin_module! {
        name: "raw",
    root: false,
        exports: [ping],
    }
}

anvyx_runtime::provider_package! { modules: [r#type] }

#[test]
fn provider_package_normalizes_raw_identifiers() {
    let package = format!("{:#?}", rust_providers());
    assert!(package.contains("raw"));
    assert!(package.contains("r#type"));
}

#[test]
fn macro_boundary_contracts() {
    let cases = trybuild::TestCases::new();
    cases.pass("tests/ui/pass/*.rs");
    cases.compile_fail("tests/ui/fail/*.rs");
}
