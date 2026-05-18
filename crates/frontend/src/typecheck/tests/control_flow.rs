use super::support::check;

fn step_checks(source: &str) -> usize {
    check(source)
        .expect("typecheck failed")
        .for_step_runtime_checks()
        .len()
}

#[test]
fn records_dynamic_step() {
    assert_eq!(
        step_checks(
            "
            fn stride() -> int { 1 }
            fn main() { for n in [1, 2] step stride() {} }
            ",
        ),
        1,
    );
}

#[test]
fn ignores_static_positive_step() {
    assert_eq!(step_checks("fn main() { for n in [1, 2] step 2 {} }"), 0);
}

#[test]
fn dynamic_step_fact_is_not_duplicated_by_specializations() {
    assert_eq!(
        step_checks(
            "
            fn each<T>(stride: int) {
                for n in [1, 2] step stride {}
            }

            fn main() {
                each<int>(1);
                each<bool>(1);
            }
            ",
        ),
        1,
    );
}
