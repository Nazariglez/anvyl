use super::helpers::{parse_program, parse_program_err};

const EMPTY_OK: &str = include_str!("../../../../../tests/syntax/for/empty_ok.anv");
const PAREN_ITER_OK: &str = include_str!("../../../../../tests/syntax/for/paren_iter_ok.anv");
const PAREN_STEP_OK: &str = include_str!("../../../../../tests/syntax/for/paren_step_ok.anv");
const STRUCT_ITER_ERR: &str = include_str!("../../../../../tests/syntax/for/struct_iter_err.anv");
const STRUCT_STEP_ERR: &str = include_str!("../../../../../tests/syntax/for/struct_step_err.anv");

#[test]
fn empty_ok() {
    parse_program(EMPTY_OK);
}

#[test]
fn paren_iter_ok() {
    parse_program(PAREN_ITER_OK);
}

#[test]
fn paren_step_ok() {
    parse_program(PAREN_STEP_OK);
}

#[test]
fn struct_iter_err() {
    parse_program_err(STRUCT_ITER_ERR);
}

#[test]
fn struct_step_err() {
    parse_program_err(STRUCT_STEP_ERR);
}
