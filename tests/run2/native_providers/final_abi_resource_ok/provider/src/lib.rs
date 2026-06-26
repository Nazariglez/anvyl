use anvyx_runtime::{function, methods, AnvRef, AnvRefType, AnvyxRef, Ctx};

#[derive(AnvyxRef)]
#[anvyx(name = "Counter")]
pub struct Counter {
    value: i64,
}

#[methods]
impl Counter {
    #[anvyx(init)]
    pub fn new(value: i64) -> Self {
        Self { value }
    }

    #[anvyx(getter)]
    pub fn value(&self) -> i64 {
        self.value
    }

    #[anvyx(setter)]
    pub fn set_value(&mut self, value: i64) {
        self.value = value;
    }

    pub fn bump(&mut self, delta: i64) {
        self.value += delta;
    }
}

#[function(ctx)]
pub fn make_counter<'cx>(ctx: &mut Ctx<'cx, '_>, value: i64) -> AnvRef<'cx, Counter> {
    AnvRefType::<Counter>::register_untracked(ctx.heap()).alloc_in(ctx, Counter { value })
}

#[function(ctx)]
pub fn counter_value<'cx>(ctx: &mut Ctx<'cx, '_>, counter: AnvRef<'cx, Counter>) -> i64 {
    counter.with(ctx.heap_ref(), |counter| counter.value).unwrap()
}

anvyx_runtime::builtin_module! {
    name: "host",
    source: "",
    exports: [Counter, make_counter, counter_value],
}
