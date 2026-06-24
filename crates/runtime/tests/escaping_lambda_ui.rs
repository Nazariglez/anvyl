#[test]
fn escaping_lambda_compile_failures() {
    let cases = trybuild::TestCases::new();
    cases.compile_fail("tests/ui/escaping_lambda/*.rs");
}
