#[test]
fn methods_compile_failures() {
    let cases = trybuild::TestCases::new();
    cases.compile_fail("tests/ui/methods/*.rs");
}
