use anvyx_runtime::function;

#[inline(always)]
#[function]
pub fn _println(message: &str) {
    println!("{message}");
}

#[inline(always)]
#[function]
pub fn _assert(condition: bool, message: &str) {
    assert!(condition, "{message}");
}

anvyx_runtime::builtin_module! {
    name: "core_runtime",
    source: include_str!("runtime.anv"),
    exports: [_println, _assert],
}
