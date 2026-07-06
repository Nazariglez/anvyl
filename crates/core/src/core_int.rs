use anvyx_runtime::function;

#[inline]
#[function]
pub fn int_abs(x: i64) -> i64 {
    x.abs()
}

#[inline]
#[function]
pub fn int_min(a: i64, b: i64) -> i64 {
    a.min(b)
}

#[inline]
#[function]
pub fn int_max(a: i64, b: i64) -> i64 {
    a.max(b)
}

#[inline]
#[function]
pub fn int_clamp(x: i64, lo: i64, hi: i64) -> i64 {
    x.clamp(lo, hi)
}

anvyx_runtime::builtin_module! {
    name: "core_int",
    source: include_str!("core_int.anv"),
    exports: [int_abs, int_min, int_max, int_clamp],
}
