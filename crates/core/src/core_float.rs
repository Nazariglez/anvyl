use anvyx_runtime::function;

#[inline]
#[function]
pub fn float_sin(x: f64) -> f64 {
    x.sin()
}

#[inline]
#[function]
pub fn float_cos(x: f64) -> f64 {
    x.cos()
}

#[inline]
#[function]
pub fn float_tan(x: f64) -> f64 {
    x.tan()
}

#[inline]
#[function]
pub fn float_asin(x: f64) -> f64 {
    x.asin()
}

#[inline]
#[function]
pub fn float_acos(x: f64) -> f64 {
    x.acos()
}

#[inline]
#[function]
pub fn float_atan(x: f64) -> f64 {
    x.atan()
}

#[inline]
#[function]
pub fn float_atan2(y: f64, x: f64) -> f64 {
    y.atan2(x)
}

#[inline]
#[function]
pub fn float_floor(x: f64) -> f64 {
    x.floor()
}

#[inline]
#[function]
pub fn float_ceil(x: f64) -> f64 {
    x.ceil()
}

#[inline]
#[function]
pub fn float_round(x: f64) -> f64 {
    x.round()
}

#[inline]
#[function]
pub fn float_trunc(x: f64) -> f64 {
    x.trunc()
}

#[inline]
#[function]
pub fn float_sqrt(x: f64) -> f64 {
    x.sqrt()
}

#[inline]
#[function]
pub fn float_cbrt(x: f64) -> f64 {
    x.cbrt()
}

#[inline]
#[function]
pub fn float_pow(x: f64, exp: f64) -> f64 {
    x.powf(exp)
}

#[inline]
#[function]
pub fn float_exp(x: f64) -> f64 {
    x.exp()
}

#[inline]
#[function]
pub fn float_ln(x: f64) -> f64 {
    x.ln()
}

#[inline]
#[function]
pub fn float_abs(x: f64) -> f64 {
    x.abs()
}

#[inline]
#[function]
pub fn float_lerp(x: f64, target: f64, t: f64) -> f64 {
    x + (target - x) * t
}

#[inline]
#[function]
pub fn float_to_radians(x: f64) -> f64 {
    x.to_radians()
}

#[inline]
#[function]
pub fn float_to_degrees(x: f64) -> f64 {
    x.to_degrees()
}

anvyx_runtime::builtin_module! {
name: "core_float",
root: false,
exports: [
    float_sin, float_cos, float_tan, float_asin, float_acos, float_atan, float_atan2,
    float_floor, float_ceil, float_round, float_trunc, float_sqrt, float_cbrt, float_pow,
    float_exp, float_ln, float_abs, float_lerp,
    float_to_radians, float_to_degrees
],}
