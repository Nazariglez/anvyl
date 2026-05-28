use anvyx_runtime::function;

#[function]
pub fn collect_cycles() {
    unimplemented!("std:mem heap support is not implemented")
}

#[function]
pub fn auto_collect(_enabled: bool) {
    unimplemented!("std:mem heap support is not implemented")
}

#[function]
pub fn managed_count() -> i64 {
    unimplemented!("std:mem heap support is not implemented")
}

anvyx_runtime::builtin_module! {
    provider: "stdlib_mem",
    name: "mem",
    source: include_str!("mem.anv"),
    exports: [collect_cycles, auto_collect, managed_count],
}
