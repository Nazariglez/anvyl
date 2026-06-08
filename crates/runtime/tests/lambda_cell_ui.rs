#[test]
fn lambda_cell_compile_failures() {
    let cases = trybuild::TestCases::new();
    cases.compile_fail("tests/ui/lambda_cell/*.rs");
}
