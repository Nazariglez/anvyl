use anvyx_runtime::{methods, AnvyxInline, Ctx, MutPlace, RuntimeError, RuntimeResult};

#[derive(Clone, Copy, AnvyxInline)]
pub struct NativeScore {
    value: i64,
}

#[methods]
impl NativeScore {
    #[anvyx(init)]
    pub fn new(value: i64) -> Self {
        Self { value }
    }

    #[anvyx(place, ctx)]
    pub fn bump<'cx>(
        ctx: &mut Ctx<'cx, '_>,
        mut place: MutPlace<'_, 'cx, Self>,
    ) -> RuntimeResult<i64> {
        place.update_copy(ctx, |score| Self {
            value: score.value + 1,
        })?;
        Ok(place.get_copy(ctx)?.value)
    }

    #[anvyx(place, ctx)]
    pub fn fail<'cx>(
        ctx: &mut Ctx<'cx, '_>,
        mut place: MutPlace<'_, 'cx, Self>,
    ) -> RuntimeResult<i64> {
        place.update_copy(ctx, |score| Self {
            value: score.value + 1,
        })?;
        let value = place.get_copy(ctx)?.value;
        Err(RuntimeError::new(format!(
            "projected provider failure after {value}"
        )))
    }

    #[anvyx(getter)]
    pub fn value(&self) -> i64 {
        self.value
    }

    #[anvyx(setter)]
    pub fn set_value(&mut self, value: i64) {
        self.value = value;
    }

    pub fn number(&self) -> i64 {
        self.value
    }
}

anvyx_runtime::builtin_module! {
    name: "host",
    source: "",
    exports: [NativeScore],
}
