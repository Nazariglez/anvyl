use anvyx_runtime::{function, methods, AnvString, AnvyxEnum};

#[derive(Clone, PartialEq, Eq, Hash, AnvyxEnum)]
#[anvyx(name = "HostError")]
pub enum HostError {
    Bad(anvyx_runtime::AnvString),
}

#[methods]
impl HostError {
    pub fn label(&self) -> AnvString {
        match self {
            Self::Bad(label) => label.clone(),
        }
    }
}

#[function]
pub fn make_host_error() -> HostError {
    HostError::Bad(AnvString::from("dynamic enum"))
}

#[derive(Clone, PartialEq, Eq, Hash, AnvyxEnum)]
pub enum HostValue {
    Pair((i64, i64)),
    Outcome(std::result::Result<i64, anvyx_runtime::AnvString>),
}

#[methods]
impl HostValue {
    pub fn score(&self) -> i64 {
        match self {
            Self::Pair((left, right)) => left + right,
            Self::Outcome(Ok(value)) => *value,
            Self::Outcome(Err(_)) => 0,
        }
    }
}

#[function]
pub fn make_host_pair() -> HostValue {
    HostValue::Pair((3, 4))
}

#[function]
pub fn make_host_outcome() -> HostValue {
    HostValue::Outcome(Ok(9))
}

#[function]
pub fn echo_host_value(value: HostValue) -> HostValue {
    value
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
pub fn host_inf() -> f64 {
    f64::INFINITY
}

#[function]
pub fn host_neg_inf() -> f64 {
    f64::NEG_INFINITY
}

#[function]
pub fn host_payload_nan() -> f64 {
    f64::from_bits(0x7ff8_0000_0000_0042)
}

#[function]
pub fn host_neg_zero() -> f64 {
    -0.0
}

#[function]
pub fn host_accepts_nonfinite(nan: f64, inf: f64, neg_inf: f64) -> bool {
    nan.is_nan() && inf == f64::INFINITY && neg_inf == f64::NEG_INFINITY
}

#[function]
pub fn host_preserves_float_bits(payload_nan: f64, neg_zero: f64) -> bool {
    payload_nan.to_bits() == 0x7ff8_0000_0000_0042
        && neg_zero.to_bits() == (-0.0_f64).to_bits()
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
    if ok {
        Ok(7)
    } else {
        Err(AnvString::from("bad"))
    }
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
        make_host_error,
        HostValue,
        make_host_pair,
        make_host_outcome,
        echo_host_value,
        maybe_add,
        swap_pair,
        counted_pair,
        first_array,
        echo_char,
        host_inf,
        host_neg_inf,
        host_payload_nan,
        host_neg_zero,
        host_accepts_nonfinite,
        host_preserves_float_bits,
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
