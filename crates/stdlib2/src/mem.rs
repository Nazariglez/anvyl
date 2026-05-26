use anvyx_runtime::function;

#[function]
pub fn collect_cycles() {
    anvyx_runtime::collect_cycles();
}

#[function]
pub fn auto_collect(enabled: bool) {
    anvyx_runtime::set_auto_collect(enabled);
}

#[function]
pub fn managed_count() -> i64 {
    anvyx_runtime::managed_alloc_count() as i64
}

anvyx_runtime::builtin_module! {
    provider: "stdlib_mem",
    name: "mem",
    source: include_str!("mem.anv"),
    exports: [collect_cycles, auto_collect, managed_count],
}
