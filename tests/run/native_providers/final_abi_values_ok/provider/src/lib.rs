use anvyx_runtime::{AnvString, AnvyxEnum, function};

#[derive(Clone, AnvyxEnum)]
#[anvyx(name = "HostError")]
pub enum HostError {
    Bad(AnvString),
}

#[function]
pub fn maybe_add(value: Option<i64>, delta: i64) -> Option<i64> {
    value.map(|value| value + delta)
}

#[function]
pub fn swap_pair(pair: (i64, AnvString)) -> (AnvString, i64) {
    (pair.1, pair.0)
}

#[function]
pub fn counted_pair() -> (i64, i64) {
    (1, 1)
}

#[function]
pub fn first_array(values: [i64; 3]) -> i64 {
    values[0]
}

#[function]
pub fn echo_char(value: char) -> char {
    value
}

#[function]
pub fn maybe_char(value: Option<char>) -> Option<char> {
    value
}

#[function]
pub fn first_char_array(values: [char; 3]) -> char {
    values[0]
}

#[function]
pub fn visible_result(ok: bool) -> Result<i64, AnvString> {
    if ok { Ok(7) } else { Err(AnvString::from("bad")) }
}

#[function]
pub fn visible_enum_result(ok: bool) -> Result<i64, HostError> {
    if ok {
        Ok(11)
    } else {
        Err(HostError::Bad(AnvString::from("enum")))
    }
}

#[function]
pub fn describe_result(value: Result<i64, AnvString>) -> AnvString {
    match value {
        Ok(value) => AnvString::from(value.to_string()),
        Err(message) => message,
    }
}

#[function]
pub fn describe_enum_result(value: Result<i64, HostError>) -> AnvString {
    match value {
        Ok(value) => AnvString::from(value.to_string()),
        Err(HostError::Bad(message)) => message,
    }
}

#[function]
pub fn unit_result() -> Result<(), AnvString> {
    Ok(())
}

#[function]
pub fn describe_unit_result(value: Result<(), AnvString>) -> i64 {
    match value {
        Ok(()) => 21,
        Err(_) => 0,
    }
}

anvyx_runtime::builtin_module! {
    name: "host",
    source: "",
    exports: [
        HostError,
        maybe_add,
        swap_pair,
        counted_pair,
        first_array,
        echo_char,
        maybe_char,
        first_char_array,
        visible_result,
        visible_enum_result,
        describe_result,
        describe_enum_result,
        unit_result,
        describe_unit_result,
    ],
}
