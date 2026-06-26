use anvyx_runtime::{function, RuntimeError, ScopedLambda};

#[function(trap)]
pub fn host_apply(f: ScopedLambda<'_, '_, (i64,), i64>) -> Result<i64, RuntimeError> {
    f.call(99)
}

anvyx_runtime::builtin_module! {
    name: "host",
    source: "",
    exports: [host_apply],
}
