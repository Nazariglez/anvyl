use anvyx_runtime::{function, AnvSlice, AnvString, Ctx};

#[function(ctx)]
pub fn option_slice<'cx>(
    ctx: &mut Ctx<'cx, '_>,
    values: Option<AnvSlice<'cx, i64>>,
) -> i64 {
    let _ = ctx;
    values.map_or(-1, |values| values.len() as i64)
}

#[function(ctx)]
pub fn result_slice<'cx>(
    ctx: &mut Ctx<'cx, '_>,
    values: Result<AnvSlice<'cx, i64>, AnvString>,
) -> i64 {
    let _ = ctx;
    match values {
        Ok(values) => values.len() as i64,
        Err(error) => -(error.len() as i64),
    }
}

#[function(ctx)]
pub fn nested<'cx>(
    ctx: &mut Ctx<'cx, '_>,
    values: Option<Result<AnvSlice<'cx, i64>, AnvString>>,
) -> i64 {
    let _ = ctx;
    match values {
        None => -1,
        Some(Ok(values)) => values.len() as i64,
        Some(Err(error)) => -(error.len() as i64),
    }
}

anvyx_runtime::builtin_module! {
    name: "host",
    exports: [option_slice, result_slice, nested],
}
