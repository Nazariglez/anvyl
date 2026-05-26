#[test]
fn function_compile_failures() {
    let cases = trybuild::TestCases::new();
    cases.compile_fail("tests/ui/function/*.rs");
    cases.compile_fail("tests/ui/module/*.rs");
}
