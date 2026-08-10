use anvyx_runtime::{function, methods, AnvRef, AnvRefType, AnvString, AnvyxRef, Ctx};

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

    #[anvyx(ctx)]
    pub fn inspect<'cx>(&self, ctx: &mut Ctx<'cx, '_>) -> i64 {
        let _ = ctx;
        self.value
    }

    #[anvyx(setter)]
    pub fn set_value(&mut self, value: i64) {
        self.value = value;
    }

    pub fn bump(&mut self, delta: i64) {
        self.value += delta;
    }

    pub fn duplicate(&self, delta: i64) -> Self {
        Self {
            value: self.value + delta,
        }
    }

    pub fn from_static(value: i64) -> Self {
        Self { value }
    }
}

#[function(ctx)]
pub fn make_counter<'cx>(ctx: &mut Ctx<'cx, '_>, value: i64) -> AnvRef<'cx, Counter> {
    AnvRefType::<Counter>::register_untracked_in(ctx).alloc_in(ctx, Counter { value })
}

#[function]
pub fn make_owned_counter(value: i64) -> Counter {
    Counter { value }
}

#[function]
pub fn maybe_owned_counter(ok: bool, value: i64) -> Option<Counter> {
    ok.then_some(Counter { value })
}

#[function]
pub fn result_owned_counter(ok: bool, value: i64) -> Result<Counter, AnvString> {
    if ok {
        Ok(Counter { value })
    } else {
        Err(AnvString::from("missing"))
    }
}

#[function(ctx)]
pub fn counter_value<'cx>(ctx: &mut Ctx<'cx, '_>, counter: AnvRef<'cx, Counter>) -> i64 {
    counter.with_in(ctx, |counter| counter.value).unwrap()
}

#[function(ctx)]
pub fn maybe_counter<'cx>(
    ctx: &mut Ctx<'cx, '_>,
    present: bool,
    value: i64,
) -> Option<AnvRef<'cx, Counter>> {
    present.then(|| AnvRefType::<Counter>::register_untracked_in(ctx).alloc_in(ctx, Counter { value }))
}

#[function(ctx)]
pub fn maybe_counter_value<'cx>(
    ctx: &mut Ctx<'cx, '_>,
    counter: Option<AnvRef<'cx, Counter>>,
) -> i64 {
    counter.map_or(-1, |counter| counter.with_in(ctx, |counter| counter.value).unwrap())
}

#[function(ctx)]
pub fn counter_result_value<'cx>(
    ctx: &mut Ctx<'cx, '_>,
    result: Result<AnvRef<'cx, Counter>, AnvString>,
) -> AnvString {
    match result {
        Ok(counter) => AnvString::from(counter.with_in(ctx, |counter| counter.value).unwrap().to_string()),
        Err(message) => message,
    }
}

anvyx_runtime::builtin_module! {
    name: "host",
    exports: [Counter, make_counter, make_owned_counter, maybe_owned_counter, result_owned_counter, counter_value, maybe_counter, maybe_counter_value, counter_result_value],
}
