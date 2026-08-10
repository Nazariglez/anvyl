use std::sync::atomic::{AtomicUsize, Ordering};

use anvyx_runtime::{
    AnvyxEnum, Ctx, RuntimeResult, Trace, TraceDriver, Visitor, function,
};

static MATERIALIZATIONS: AtomicUsize = AtomicUsize::new(0);
static CREATED: AtomicUsize = AtomicUsize::new(0);
static DROPPED: AtomicUsize = AtomicUsize::new(0);
static TRACE_VISITS: AtomicUsize = AtomicUsize::new(0);

#[derive(AnvyxEnum)]
pub enum HostValue {
    Value(i64),
}

impl Clone for HostValue {
    fn clone(&self) -> Self {
        MATERIALIZATIONS.fetch_add(1, Ordering::Relaxed);
        CREATED.fetch_add(1, Ordering::Relaxed);
        match self {
            Self::Value(value) => Self::Value(*value),
        }
    }
}

impl Drop for HostValue {
    fn drop(&mut self) {
        DROPPED.fetch_add(1, Ordering::Relaxed);
    }
}

unsafe impl<'cx> Trace<'cx> for HostValue {
    fn trace<D: TraceDriver<'cx>>(&self, _visitor: &mut Visitor<'cx, '_, D>) {
        TRACE_VISITS.fetch_add(1, Ordering::Relaxed);
    }
}

#[function(ret = "HostValue")]
pub fn make_value(value: i64) -> HostValue {
    CREATED.fetch_add(1, Ordering::Relaxed);
    HostValue::Value(value)
}

#[function(params(value = "HostValue"))]
pub fn read_value(value: HostValue) -> i64 {
    match value {
        HostValue::Value(value) => value,
    }
}

#[function]
pub fn reset_materializations() {
    MATERIALIZATIONS.store(0, Ordering::Relaxed);
}

#[function]
pub fn materializations() -> i64 {
    MATERIALIZATIONS.load(Ordering::Relaxed) as i64
}

#[function]
pub fn live_values() -> i64 {
    CREATED.load(Ordering::Relaxed) as i64 - DROPPED.load(Ordering::Relaxed) as i64
}

#[function]
pub fn reset_trace_visits() {
    TRACE_VISITS.store(0, Ordering::Relaxed);
}

#[function]
pub fn trace_visits() -> i64 {
    TRACE_VISITS.load(Ordering::Relaxed) as i64
}

#[function(ctx)]
pub fn collect_now<'cx>(ctx: &mut Ctx<'cx, '_>) -> RuntimeResult<()> {
    ctx.collect_all().map(|_| ())
}

anvyx_runtime::builtin_module! {
    name: "host",
    exports: [
        HostValue,
        collect_now,
        live_values,
        make_value,
        materializations,
        read_value,
        reset_materializations,
        reset_trace_visits,
        trace_visits,
    ],
}
