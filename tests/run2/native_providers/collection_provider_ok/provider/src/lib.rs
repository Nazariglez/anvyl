use anvyx_runtime::{
    function, AnvList, AnvMap, AnvSlice, AnvString, Ctx, ListStorage, MapStorage, RuntimeResult,
};

#[function(ctx)]
pub fn host_direct_first<'cx>(
    ctx: &mut Ctx<'cx, '_>,
    xs: AnvList<'cx, i64>,
) -> RuntimeResult<i64> {
    xs.checked_index(ctx, 0)
}

#[function(ctx)]
pub fn host_direct_make<'cx>(ctx: &mut Ctx<'cx, '_>) -> AnvList<'cx, i64> {
    let storage = ctx.heap().register_untracked::<ListStorage<'_, i64>>();
    AnvList::from_elems(ctx, storage, [11, 13])
}

#[function(ctx)]
pub fn host_direct_lookup<'cx>(
    ctx: &mut Ctx<'cx, '_>,
    scores: AnvMap<'cx, AnvString, i64>,
    key: AnvString,
) -> RuntimeResult<Option<i64>> {
    scores.get(ctx, &key)
}

#[function(ctx)]
pub fn host_direct_map<'cx>(ctx: &mut Ctx<'cx, '_>) -> AnvMap<'cx, AnvString, i64> {
    let storage = ctx
        .heap()
        .register_untracked::<MapStorage<'_, AnvString, i64>>();
    AnvMap::from_entries(ctx, storage, [(AnvString::from("hp"), 99)])
}

#[function(ctx)]
pub fn host_slice_first<'cx>(
    ctx: &mut Ctx<'cx, '_>,
    values: AnvSlice<'cx, i64>,
) -> RuntimeResult<i64> {
    values.elem_at_shared(ctx, 0)
}

anvyx_runtime::builtin_module! {
    name: "host",
    source: "",
    exports: [
        host_direct_first, host_direct_make, host_direct_lookup, host_direct_map, host_slice_first,
    ],
}
