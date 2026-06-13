#[test]
fn scoped_lambda_compile_failures() {
    let cases = trybuild::TestCases::new();
    cases.compile_fail("tests/ui/scoped_lambda/*.rs");
}
