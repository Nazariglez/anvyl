use anvyx_runtime::{function, RuntimeResult, ScopedLambda};

#[function]
pub fn host_apply(f: ScopedLambda<'_, '_, (i64,), i64>) -> RuntimeResult<i64> {
    f.call(99)
}

anvyx_runtime::builtin_module! {
    name: "host",
    source: "",
    exports: [host_apply],
}
