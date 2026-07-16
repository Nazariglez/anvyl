#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FloatToIntError {
    NonFinite,
    OutOfRange,
}

pub fn display_float(float: f64) -> String {
    let mut text = float.to_string();
    if display_float_needs_decimal(float, &text) {
        text.push_str(".0");
    }
    text
}

pub fn display_float_needs_decimal(float: f64, text: &str) -> bool {
    float.is_finite() && !text.contains(['.', 'e', 'E'])
}

pub fn int_to_float(int: i64) -> f64 {
    int as f64
}

pub fn float_to_int(float: f64) -> Result<i64, FloatToIntError> {
    if !float.is_finite() {
        return Err(FloatToIntError::NonFinite);
    }
    if !(-(2.0_f64.powi(63))..2.0_f64.powi(63)).contains(&float) {
        return Err(FloatToIntError::OutOfRange);
    }
    Ok(float as i64)
}
