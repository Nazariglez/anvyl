use anvyx_runtime::function;

#[inline(always)]
#[function]
pub fn float_sin(x: f64) -> f64 {
    x.sin()
}

#[inline(always)]
#[function]
pub fn float_cos(x: f64) -> f64 {
    x.cos()
}

#[inline(always)]
#[function]
pub fn float_tan(x: f64) -> f64 {
    x.tan()
}

#[inline(always)]
#[function]
pub fn float_asin(x: f64) -> f64 {
    x.asin()
}

#[inline(always)]
#[function]
pub fn float_acos(x: f64) -> f64 {
    x.acos()
}

#[inline(always)]
#[function]
pub fn float_atan(x: f64) -> f64 {
    x.atan()
}

#[inline(always)]
#[function]
pub fn float_atan2(y: f64, x: f64) -> f64 {
    y.atan2(x)
}

#[inline(always)]
#[function]
pub fn float_floor(x: f64) -> f64 {
    x.floor()
}

#[inline(always)]
#[function]
pub fn float_ceil(x: f64) -> f64 {
    x.ceil()
}

#[inline(always)]
#[function]
pub fn float_round(x: f64) -> f64 {
    x.round()
}

#[inline(always)]
#[function]
pub fn float_trunc(x: f64) -> f64 {
    x.trunc()
}

#[inline(always)]
#[function]
pub fn float_sqrt(x: f64) -> f64 {
    x.sqrt()
}

#[inline(always)]
#[function]
pub fn float_cbrt(x: f64) -> f64 {
    x.cbrt()
}

#[inline(always)]
#[function]
pub fn float_pow(x: f64, exp: f64) -> f64 {
    x.powf(exp)
}

#[inline(always)]
#[function]
pub fn float_exp(x: f64) -> f64 {
    x.exp()
}

#[inline(always)]
#[function]
pub fn float_ln(x: f64) -> f64 {
    x.ln()
}

#[inline(always)]
#[function]
pub fn float_abs(x: f64) -> f64 {
    x.abs()
}

#[inline(always)]
#[function]
pub fn float_min(a: f64, b: f64) -> f64 {
    a.min(b)
}

#[inline(always)]
#[function]
pub fn float_max(a: f64, b: f64) -> f64 {
    a.max(b)
}

#[inline(always)]
#[function]
pub fn float_clamp(x: f64, lo: f64, hi: f64) -> f64 {
    x.clamp(lo, hi)
}

#[inline(always)]
#[function]
pub fn float_lerp(x: f64, target: f64, t: f64) -> f64 {
    x + (target - x) * t
}

#[inline(always)]
#[function]
pub fn float_to_radians(x: f64) -> f64 {
    x.to_radians()
}

#[inline(always)]
#[function]
pub fn float_to_degrees(x: f64) -> f64 {
    x.to_degrees()
}

anvyx_runtime::builtin_module! {
    name: "core_float",
    source: include_str!("core_float.anv"),
    exports: [
        float_sin, float_cos, float_tan, float_asin, float_acos, float_atan, float_atan2,
        float_floor, float_ceil, float_round, float_trunc, float_sqrt, float_cbrt, float_pow,
        float_exp, float_ln, float_abs, float_min, float_max, float_clamp, float_lerp,
        float_to_radians, float_to_degrees
    ],
}
