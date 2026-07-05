use anvyx_lang::{export_fn, provider};

use super::StdModule;

#[export_fn]
pub fn collect_cycles() {
    anvyx_runtime::legacy_gc::collect_cycles();
}

#[export_fn]
pub fn auto_collect(enabled: bool) {
    anvyx_runtime::legacy_gc::set_auto_collect(enabled);
}

#[export_fn]
pub fn managed_count() -> i64 {
    anvyx_runtime::legacy_gc::managed_alloc_count() as i64
}

provider!(collect_cycles, auto_collect, managed_count);

pub fn module() -> StdModule {
    StdModule {
        name: "mem",
        anv_source: include_str!("./mem.anv"),
        exports: anvyx_exports,
        type_exports: anvyx_type_exports,
        handlers: anvyx_externs,
        init: None,
    }
}
